import gleam/bool
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set
import starfish/internal/board
import starfish/internal/game.{type Game, Game}
import starfish/internal/hash
import starfish/internal/move/attack
import starfish/internal/move/direction.{type Direction}

pub type Move {
  Castle(from: Int, to: Int)
  Move(from: Int, to: Int)
  Capture(from: Int, to: Int)
  EnPassant(from: Int, to: Int)
  Promotion(from: Int, to: Int, piece: board.Piece)
}

pub fn legal(game: Game) -> List(Move) {
  use moves, position, #(piece, colour) <- dict.fold(game.board, [])

  case colour == game.to_move {
    True -> moves_for_piece(game, position, piece, moves)
    False -> moves
  }
}

/// Checks whether any legal moves are possible without calculating every legal
/// move beforehand.
pub fn any_legal(game: Game) -> Bool {
  use any, position, #(piece, colour) <- dict.fold(game.board, False)
  // If we've found legal moves already, we already know  that there are some,
  // so no need to check for more.
  use <- bool.guard(any, any)

  case colour == game.to_move {
    True -> moves_for_piece(game, position, piece, []) != []
    False -> any
  }
}

fn move_is_valid_with_pins(
  from: Int,
  to: Int,
  attack_information: attack.AttackInformation,
) -> Bool {
  case dict.get(attack_information.pin_lines, from) {
    Error(_) -> True
    Ok(line) -> list.contains(line, to)
  }
}

fn can_move(
  from: Int,
  to: Int,
  attack_information: attack.AttackInformation,
) -> Bool {
  case attack_information.in_check {
    False -> move_is_valid_with_pins(from, to, attack_information)
    True ->
      list.contains(attack_information.check_block_line, to)
      && move_is_valid_with_pins(from, to, attack_information)
  }
}

fn king_can_move(to: Int, attack_information: attack.AttackInformation) -> Bool {
  case attack_information.in_check {
    False -> !list.contains(attack_information.attacks, to)
    True ->
      !list.contains(attack_information.check_attack_squares, to)
      && !list.contains(attack_information.attacks, to)
  }
}

fn moves_for_piece(
  game: Game,
  position: Int,
  piece: board.Piece,
  moves: List(Move),
) -> List(Move) {
  case piece {
    board.Bishop ->
      sliding_moves(game, position, moves, direction.bishop_directions)
    board.Rook ->
      sliding_moves(game, position, moves, direction.rook_directions)
    board.Queen ->
      sliding_moves(game, position, moves, direction.queen_directions)
    board.King -> king_moves(game, position, moves, direction.queen_directions)
    board.Knight ->
      knight_moves(game, position, moves, direction.knight_directions)
    board.Pawn -> pawn_moves(game, position, moves)
  }
}

fn pawn_moves(game: Game, position: Int, moves: List(Move)) -> List(Move) {
  let #(forward, left, right, promotion_rank) = case game.to_move {
    board.Black -> #(
      direction.down,
      direction.down_left,
      direction.down_right,
      0,
    )
    board.White -> #(direction.up, direction.up_left, direction.up_right, 7)
  }

  let forward_one = direction.in_direction(position, forward)
  // If moving forward on square is a promotion, then captured are also
  // promotions, because they must move to the same rank. A double-move can
  // never be a promotion because a pawn cannot double move from a position that
  // ends on the promotion rank.
  let is_promotion = board.rank(forward_one) == promotion_rank

  let moves = case board.get(game.board, forward_one) {
    board.Empty -> {
      let moves = case
        can_move(position, forward_one, game.attack_information)
      {
        False -> moves
        True if is_promotion ->
          add_promotions(position, forward_one, moves, board.pawn_promotions)
        True -> [Move(from: position, to: forward_one), ..moves]
      }

      let can_double_move = case game.to_move, board.rank(position) {
        board.Black, 6 | board.White, 1 -> True
        _, _ -> False
      }

      use <- bool.guard(!can_double_move, moves)

      let forward_two = direction.in_direction(forward_one, forward)
      case board.get(game.board, forward_two) {
        board.Empty ->
          case can_move(position, forward_two, game.attack_information) {
            False -> moves
            True -> [Move(from: position, to: forward_two), ..moves]
          }
        board.Occupied(_, _) | board.OffBoard -> moves
      }
    }
    board.Occupied(_, _) | board.OffBoard -> moves
  }

  let new_position = direction.in_direction(position, left)
  let moves = case board.get(game.board, new_position) {
    board.Occupied(colour:, ..) if colour != game.to_move ->
      case can_move(position, new_position, game.attack_information) {
        False -> moves
        True if is_promotion ->
          add_promotions(position, new_position, moves, board.pawn_promotions)
        True -> [Capture(from: position, to: new_position), ..moves]
      }
    board.Empty if game.en_passant_square == Some(new_position) ->
      case en_passant_is_valid(game, position, new_position) {
        False -> moves
        True -> [EnPassant(from: position, to: new_position), ..moves]
      }
    board.Empty | board.Occupied(_, _) | board.OffBoard -> moves
  }

  let new_position = direction.in_direction(position, right)
  case board.get(game.board, new_position) {
    board.Occupied(colour:, ..) if colour != game.to_move ->
      case can_move(position, new_position, game.attack_information) {
        False -> moves
        True if is_promotion ->
          add_promotions(position, new_position, moves, board.pawn_promotions)
        True -> [Capture(from: position, to: new_position), ..moves]
      }
    board.Empty if game.en_passant_square == Some(new_position) ->
      case en_passant_is_valid(game, position, new_position) {
        False -> moves
        True -> [EnPassant(from: position, to: new_position), ..moves]
      }
    board.Empty | board.Occupied(_, _) | board.OffBoard -> moves
  }
}

fn en_passant_is_valid(game: Game, position: Int, new_position: Int) -> Bool {
  let captured_pawn_position =
    board.position(file: board.file(new_position), rank: board.rank(position))

  case game.attack_information.in_check {
    False ->
      move_is_valid_with_pins(position, new_position, game.attack_information)
    True ->
      // If the king is in check by the pawn we are capturing, then en passant
      // is valid. It is also valid if moving the pawn to its new location blocks
      // the existing check.
      {
        list.contains(
          game.attack_information.check_block_line,
          captured_pawn_position,
        )
        || list.contains(game.attack_information.check_block_line, new_position)
      }
      && move_is_valid_with_pins(
        position,
        new_position,
        game.attack_information,
      )
  }
  && !in_check_after_en_passant(game, position, captured_pawn_position)
}

/// En passant needs to be checked slightly different to other moves. For example,
/// if a row of the board looks something like this, after the black pawn having
/// moved two squares:
///
/// ```txt
/// | K |   | p | P |   |   | r |   |
/// ```
///
/// Here, the white pawn is not pinned by the black rook, so it can safely move
/// forwards. However, if it were to perform en passant and capture the black
/// pawn, the king would be in check. Here, we check for this case.
///
/// To perform the check, we cast out rays on either side of the en passant pair.
/// If we hit the king, as well as a rook or a queen of the opposite colour, then
/// en passant is not valid.
///
fn in_check_after_en_passant(
  game: Game,
  position: Int,
  captured_pawn_position: Int,
) -> Bool {
  let #(left_position, right_position) = sort(position, captured_pawn_position)

  case
    in_check_after_en_passant_loop(game, left_position, direction.left, NoPiece)
  {
    NoPiece | Both -> False
    found_piece ->
      in_check_after_en_passant_loop(
        game,
        right_position,
        direction.right,
        found_piece,
      )
      == Both
  }
}

type FoundPiece {
  NoPiece
  KingPiece
  EnemyPiece
  Both
}

fn sort(a: Int, b: Int) -> #(Int, Int) {
  case a < b {
    True -> #(a, b)
    False -> #(b, a)
  }
}

fn in_check_after_en_passant_loop(
  game: Game,
  position: Int,
  direction: Direction,
  found_piece: FoundPiece,
) -> FoundPiece {
  let new_position = direction.in_direction(position, direction)
  case board.get(game.board, new_position), found_piece {
    board.Empty, _ ->
      in_check_after_en_passant_loop(game, new_position, direction, found_piece)
    board.Occupied(piece: board.King, colour:), NoPiece
      if colour == game.to_move
    -> KingPiece
    board.Occupied(piece: board.King, colour:), EnemyPiece
      if colour == game.to_move
    -> Both
    board.Occupied(piece: board.Rook, colour:), NoPiece
    | board.Occupied(piece: board.Queen, colour:), NoPiece
      if colour != game.to_move
    -> EnemyPiece
    board.Occupied(piece: board.Rook, colour:), KingPiece
    | board.Occupied(piece: board.Queen, colour:), KingPiece
      if colour != game.to_move
    -> Both
    board.Occupied(_, _), _ | board.OffBoard, _ -> found_piece
  }
}

fn add_promotions(
  from: Int,
  to: Int,
  moves: List(Move),
  pieces: List(board.Piece),
) -> List(Move) {
  case pieces {
    [] -> moves
    [piece, ..pieces] ->
      add_promotions(from, to, [Promotion(from:, to:, piece:), ..moves], pieces)
  }
}

fn knight_moves(
  game: Game,
  position: Int,
  moves: List(Move),
  directions: List(Direction),
) -> List(Move) {
  case directions {
    [] -> moves
    [direction, ..directions] -> {
      let new_position = direction.in_direction(position, direction)
      let moves = case board.get(game.board, new_position) {
        board.Empty ->
          case can_move(position, new_position, game.attack_information) {
            False -> moves
            True -> [Move(from: position, to: new_position), ..moves]
          }
        board.Occupied(colour:, ..) if colour != game.to_move ->
          case can_move(position, new_position, game.attack_information) {
            False -> moves
            True -> [Capture(from: position, to: new_position), ..moves]
          }
        board.Occupied(_, _) | board.OffBoard -> moves
      }

      knight_moves(game, position, moves, directions)
    }
  }
}

fn king_moves(
  game: Game,
  position: Int,
  moves: List(Move),
  directions: List(Direction),
) -> List(Move) {
  let moves = regular_king_moves(game, position, moves, directions)

  // If we're in check, castling is not valid.
  use <- bool.guard(game.attack_information.in_check, moves)

  let is_empty = fn(position) { board.get(game.board, position) == board.Empty }

  let can_move_through = fn(position) {
    board.get(game.board, position) == board.Empty
    && !list.contains(game.attack_information.attacks, position)
  }

  let moves = case game.to_move {
    board.Black if game.castling.black_kingside ->
      case can_move_through(61) && can_move_through(62) {
        True -> [Castle(from: position, to: 62), ..moves]
        False -> moves
      }
    board.White if game.castling.white_kingside ->
      case can_move_through(5) && can_move_through(6) {
        True -> [Castle(from: position, to: 6), ..moves]
        False -> moves
      }
    _ -> moves
  }

  case game.to_move {
    board.Black if game.castling.black_queenside ->
      case can_move_through(59) && can_move_through(58) && is_empty(57) {
        True -> [Castle(from: position, to: 58), ..moves]
        False -> moves
      }
    board.White if game.castling.white_queenside ->
      case can_move_through(3) && can_move_through(2) && is_empty(1) {
        True -> [Castle(from: position, to: 2), ..moves]
        False -> moves
      }
    _ -> moves
  }
}

fn regular_king_moves(
  game: Game,
  position: Int,
  moves: List(Move),
  directions: List(Direction),
) -> List(Move) {
  case directions {
    [] -> moves
    [direction, ..directions] -> {
      let new_position = direction.in_direction(position, direction)
      let moves = case board.get(game.board, new_position) {
        board.Empty ->
          case king_can_move(new_position, game.attack_information) {
            False -> moves
            True -> [Move(from: position, to: new_position), ..moves]
          }
        board.Occupied(colour:, ..) if colour != game.to_move ->
          case king_can_move(new_position, game.attack_information) {
            False -> moves
            True -> [Capture(from: position, to: new_position), ..moves]
          }
        board.Occupied(_, _) | board.OffBoard -> moves
      }

      regular_king_moves(game, position, moves, directions)
    }
  }
}

fn sliding_moves(
  game: Game,
  position: Int,
  moves: List(Move),
  directions: List(Direction),
) -> List(Move) {
  case directions {
    [] -> moves
    [direction, ..directions] ->
      sliding_moves(
        game,
        position,
        sliding_moves_in_direction(game, position, position, direction, moves),
        directions,
      )
  }
}

fn sliding_moves_in_direction(
  game: Game,
  start_position: Int,
  position: Int,
  direction: Direction,
  moves: List(Move),
) -> List(Move) {
  let new_position = direction.in_direction(position, direction)
  case board.get(game.board, new_position) {
    board.Empty ->
      sliding_moves_in_direction(
        game,
        start_position,
        new_position,
        direction,
        case can_move(start_position, new_position, game.attack_information) {
          False -> moves
          True -> [Move(from: start_position, to: new_position), ..moves]
        },
      )
    board.Occupied(colour:, ..) if colour != game.to_move ->
      case can_move(start_position, new_position, game.attack_information) {
        False -> moves
        True -> [Capture(from: start_position, to: new_position), ..moves]
      }
    board.Occupied(_, _) | board.OffBoard -> moves
  }
}

pub fn apply(game: Game, move: Move) -> game.Game {
  case move {
    Capture(from:, to:) -> do_apply(game, from, to, False, None, True)
    Castle(from:, to:) -> apply_castle(game, from, to, board.file(to) == 2)
    EnPassant(from:, to:) -> do_apply(game, from, to, True, None, True)
    Move(from:, to:) -> do_apply(game, from, to, False, None, False)
    Promotion(from:, to:, piece:) ->
      do_apply(game, from, to, False, Some(piece), False)
  }
}

fn apply_castle(game: Game, from: Int, to: Int, long: Bool) -> Game {
  let Game(
    board:,
    to_move:,
    castling:,
    en_passant_square: _,
    half_moves:,
    full_moves:,
    zobrist_hash:,
    previous_positions:,
    attack_information: _,
  ) = game

  let assert board.Occupied(piece:, colour:) = board.get(board, from)
    as "Tried to apply castle move from invalid position"

  let castling = case colour {
    board.Black ->
      game.Castling(..castling, black_kingside: False, black_queenside: False)
    board.White ->
      game.Castling(..castling, white_kingside: False, white_queenside: False)
  }

  let rook_rank = board.rank(from)
  let #(rook_file_from, rook_file_to) = case long {
    True -> #(0, 3)
    False -> #(7, 5)
  }

  let board =
    board
    |> dict.delete(from)
    |> dict.delete(board.position(file: rook_file_from, rank: rook_rank))
    |> dict.insert(to, #(piece, colour))
    |> dict.insert(board.position(file: rook_file_to, rank: rook_rank), #(
      board.Rook,
      colour,
    ))

  let en_passant_square = None

  let full_moves = case to_move {
    board.Black -> full_moves + 1
    board.White -> full_moves
  }

  let to_move = case to_move {
    board.Black -> board.White
    board.White -> board.Black
  }

  let half_moves = half_moves + 1
  let previous_positions = set.insert(previous_positions, zobrist_hash)

  // TODO: Update incrementally
  let zobrist_hash = hash.hash(board, to_move)

  // TODO: Maybe we can update this incrementally too?
  let attack_information = attack.calculate(board, to_move)

  Game(
    board:,
    to_move:,
    castling:,
    en_passant_square:,
    half_moves:,
    full_moves:,
    zobrist_hash:,
    previous_positions:,
    attack_information:,
  )
}

fn do_apply(
  game: Game,
  from: Int,
  to: Int,
  en_passant: Bool,
  promotion: Option(board.Piece),
  capture: Bool,
) -> Game {
  let Game(
    board:,
    to_move:,
    castling:,
    en_passant_square:,
    half_moves:,
    full_moves:,
    zobrist_hash:,
    previous_positions:,
    attack_information: _,
  ) = game

  let assert board.Occupied(piece:, colour:) = board.get(board, from)
    as "Tried to apply move from invalid position"

  let castling =
    castling
    |> remove_castling(from)
    |> remove_castling(to)

  let one_way_move = capture || piece == board.Pawn

  let piece = case promotion {
    None -> piece
    Some(piece) -> piece
  }

  let board =
    board
    |> dict.delete(from)
    |> dict.insert(to, #(piece, colour))

  let board = case en_passant, en_passant_square, colour {
    True, Some(square), board.White -> dict.delete(board, square - 8)
    True, Some(square), board.Black -> dict.delete(board, square + 8)
    _, _, _ -> board
  }

  let en_passant_square = case piece, to - from {
    board.Pawn, 16 -> Some(from + 8)
    board.Pawn, -16 -> Some(from - 8)
    _, _ -> None
  }

  let full_moves = case to_move {
    board.Black -> full_moves + 1
    board.White -> full_moves
  }

  let to_move = case to_move {
    board.Black -> board.White
    board.White -> board.Black
  }

  let #(half_moves, previous_positions) = case one_way_move {
    True -> #(0, set.new())
    False -> #(half_moves + 1, set.insert(previous_positions, zobrist_hash))
  }

  // TODO: Update incrementally
  let zobrist_hash = hash.hash(board, to_move)

  // TODO: Maybe we can update this incrementally too?
  let attack_information = attack.calculate(board, to_move)

  Game(
    board:,
    to_move:,
    castling:,
    en_passant_square:,
    half_moves:,
    full_moves:,
    zobrist_hash:,
    previous_positions:,
    attack_information:,
  )
}

/// Removes castling rights based on a specific square changing. This works if
/// the piece (such as a rook or king) moves, or is captured by another piece.
fn remove_castling(castling: game.Castling, position: Int) -> game.Castling {
  case position {
    4 ->
      game.Castling(..castling, white_kingside: False, white_queenside: False)
    60 ->
      game.Castling(..castling, black_kingside: False, black_queenside: False)
    7 -> game.Castling(..castling, white_kingside: False)
    63 -> game.Castling(..castling, black_kingside: False)
    0 -> game.Castling(..castling, white_queenside: False)
    56 -> game.Castling(..castling, black_queenside: False)
    _ -> castling
  }
}

pub fn to_long_algebraic_notation(move: Move) -> String {
  let from = board.position_to_string(move.from)
  let to = board.position_to_string(move.to)
  let extra = case move {
    Capture(..) | Castle(..) | EnPassant(..) | Move(..) -> ""
    Promotion(piece:, ..) ->
      case piece {
        board.Bishop -> "b"
        board.King -> "k"
        board.Knight -> "n"
        board.Pawn -> "p"
        board.Queen -> "q"
        board.Rook -> "r"
      }
  }

  from <> to <> extra
}

// TODO: can we do this without calculating every legal move? We can probably just
// calculate legal moves for the specific square we need.
pub fn from_long_algebraic_notation(
  string: String,
  game: Game,
) -> Result(Move, Nil) {
  use #(from, string) <- result.try(board.parse_position(string))
  use #(to, string) <- result.try(board.parse_position(string))
  use promotion_piece <- result.try(case string {
    "" -> Ok(None)
    "b" | "B" -> Ok(Some(board.Bishop))
    "n" | "N" -> Ok(Some(board.Knight))
    "q" | "Q" -> Ok(Some(board.Queen))
    "r" | "R" -> Ok(Some(board.Rook))
    _ -> Error(Nil)
  })

  let legal_moves = legal(game)

  let valid_moves =
    list.filter(legal_moves, fn(move) {
      move.from == from
      && move.to == to
      && case move, promotion_piece {
        Promotion(..), None -> False
        Promotion(piece:, ..), Some(promotion) -> piece == promotion
        _, _ -> promotion_piece == None
      }
    })

  case valid_moves {
    [move] -> Ok(move)
    _ -> Error(Nil)
  }
}
