import gleam/bool
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import starfish/internal/board
import starfish/internal/game.{type Game, Game}
import starfish/internal/hash
import starfish/internal/move/attack
import starfish/internal/move/direction.{type Direction}
import starfish/internal/piece_table

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
  let is_promotion = forward_one / 8 == promotion_rank

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

      let can_double_move = case game.to_move, position / 8 {
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
  let captured_pawn_position = new_position % 8 + position / 8 * 8

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
    Castle(from:, to:) -> apply_castle(game, from, to, to % 8 == 2)
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
    zobrist_hash: previous_hash,
    previous_positions:,
    attack_information: _,
    white_pieces: game.PieceInfo(
      king_position: white_king_position,
      non_pawn_material: white_non_pawn_material,
      pawn_material: white_pawn_material,
      piece_square_score: white_piece_square_score,
    ),
    black_pieces: game.PieceInfo(
      king_position: black_king_position,
      non_pawn_material: black_non_pawn_material,
      pawn_material: black_pawn_material,
      piece_square_score: black_piece_square_score,
    ),
  ) = game

  let castling = case to_move {
    board.Black ->
      game.Castling(..castling, black_kingside: False, black_queenside: False)
    board.White ->
      game.Castling(..castling, white_kingside: False, white_queenside: False)
  }

  let rook_rank = from / 8
  let #(rook_file_from, rook_file_to) = case long {
    True -> #(0, 3)
    False -> #(7, 5)
  }

  let rook_from = rook_rank * 8 + rook_file_from
  let rook_to = rook_rank * 8 + rook_file_to

  let board =
    board
    |> dict.delete(from)
    |> dict.delete(rook_from)
    |> dict.insert(to, #(board.King, to_move))
    |> dict.insert(rook_to, #(board.Rook, to_move))

  let zobrist_hash =
    previous_hash
    |> hash.toggle_to_move
    |> hash.toggle_piece(from, board.King, to_move)
    |> hash.toggle_piece(to, board.King, to_move)
    |> hash.toggle_piece(rook_from, board.Rook, to_move)
    |> hash.toggle_piece(rook_to, board.Rook, to_move)

  let phase = game.phase(white_non_pawn_material, black_non_pawn_material)

  let #(white_piece_square_score, black_piece_square_score) = case to_move {
    board.Black -> #(
      white_piece_square_score,
      black_piece_square_score
        - piece_table.piece_score(board.King, to_move, from, phase)
        - piece_table.piece_score(board.Rook, to_move, rook_from, phase)
        + piece_table.piece_score(board.King, to_move, to, phase)
        + piece_table.piece_score(board.Rook, to_move, rook_to, phase),
    )
    board.White -> #(
      white_piece_square_score
        - piece_table.piece_score(board.King, to_move, from, phase)
        - piece_table.piece_score(board.Rook, to_move, rook_from, phase)
        + piece_table.piece_score(board.King, to_move, to, phase)
        + piece_table.piece_score(board.Rook, to_move, rook_to, phase),
      black_piece_square_score,
    )
  }

  let en_passant_square = None

  let white_king_position = case to_move {
    board.White -> to
    board.Black -> white_king_position
  }

  let black_king_position = case to_move {
    board.Black -> to
    board.White -> black_king_position
  }

  let full_moves = case to_move {
    board.Black -> full_moves + 1
    board.White -> full_moves
  }

  let to_move = case to_move {
    board.Black -> board.White
    board.White -> board.Black
  }

  let half_moves = half_moves + 1
  let previous_positions = [previous_hash, ..previous_positions]

  let king_position = case to_move {
    board.Black -> black_king_position
    board.White -> white_king_position
  }
  // TODO: Maybe we can update this incrementally too?
  let attack_information = attack.calculate(board, king_position, to_move)

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
    white_pieces: game.PieceInfo(
      king_position: white_king_position,
      non_pawn_material: white_non_pawn_material,
      pawn_material: white_pawn_material,
      piece_square_score: white_piece_square_score,
    ),
    black_pieces: game.PieceInfo(
      king_position: black_king_position,
      non_pawn_material: black_non_pawn_material,
      pawn_material: black_pawn_material,
      piece_square_score: black_piece_square_score,
    ),
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
    zobrist_hash: previous_hash,
    previous_positions:,
    attack_information: _,
    white_pieces:,
    black_pieces:,
  ) = game

  let #(
    game.PieceInfo(
      king_position: our_king_position,
      non_pawn_material: our_non_pawn_material,
      pawn_material: our_pawn_material,
      piece_square_score: our_piece_square_score,
    ),
    game.PieceInfo(
      king_position: opposing_king_position,
      non_pawn_material: opposing_non_pawn_material,
      pawn_material: opposing_pawn_material,
      piece_square_score: opposing_piece_square_score,
    ),
  ) = case to_move {
    board.Black -> #(black_pieces, white_pieces)
    board.White -> #(white_pieces, black_pieces)
  }

  let assert board.Occupied(piece:, colour:) = board.get(board, from)
    as "Tried to apply move from invalid position"

  let castling =
    castling
    |> remove_castling(from)
    |> remove_castling(to)

  let one_way_move = capture || piece == board.Pawn

  let zobrist_hash =
    previous_hash
    |> hash.toggle_to_move
    |> hash.toggle_piece(from, piece, colour)

  let phase =
    game.phase(white_pieces.non_pawn_material, black_pieces.non_pawn_material)

  let our_piece_square_score =
    our_piece_square_score - piece_table.piece_score(piece, colour, from, phase)

  let #(piece, our_pawn_material, our_non_pawn_material) = case promotion {
    None -> #(piece, our_pawn_material, our_non_pawn_material)
    Some(piece) -> #(
      piece,
      our_pawn_material - board.pawn_value,
      our_non_pawn_material + board.piece_value(piece),
    )
  }

  let our_piece_square_score =
    our_piece_square_score + piece_table.piece_score(piece, colour, to, phase)

  let zobrist_hash = hash.toggle_piece(zobrist_hash, to, piece, colour)

  let #(
    zobrist_hash,
    opposing_pawn_material,
    opposing_non_pawn_material,
    opposing_piece_square_score,
  ) = case board.get(board, to) {
    board.Occupied(piece: board.Pawn, colour:) -> #(
      hash.toggle_piece(zobrist_hash, to, board.Pawn, colour),
      opposing_pawn_material - board.pawn_value,
      opposing_non_pawn_material,
      opposing_piece_square_score
        - piece_table.piece_score(board.Pawn, colour, to, phase),
    )
    board.Occupied(piece:, colour:) -> #(
      hash.toggle_piece(zobrist_hash, to, piece, colour),
      opposing_pawn_material,
      opposing_non_pawn_material - board.piece_value(piece),
      opposing_piece_square_score
        - piece_table.piece_score(piece, colour, to, phase),
    )
    board.Empty | board.OffBoard -> #(
      zobrist_hash,
      opposing_pawn_material,
      opposing_non_pawn_material,
      opposing_piece_square_score,
    )
  }

  let board =
    board
    |> dict.delete(from)
    |> dict.insert(to, #(piece, colour))

  let #(board, zobrist_hash) = case en_passant, en_passant_square, colour {
    True, Some(square), board.White -> {
      let ep_square = square - 8
      #(
        dict.delete(board, ep_square),
        hash.toggle_piece(zobrist_hash, ep_square, board.Pawn, board.Black),
      )
    }
    True, Some(square), board.Black -> {
      let ep_square = square + 8
      #(
        dict.delete(board, ep_square),
        hash.toggle_piece(zobrist_hash, ep_square, board.Pawn, board.White),
      )
    }
    _, _, _ -> #(board, zobrist_hash)
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

  let our_king_position = case from == our_king_position {
    True -> to
    False -> our_king_position
  }

  let our_pieces =
    game.PieceInfo(
      king_position: our_king_position,
      non_pawn_material: our_non_pawn_material,
      pawn_material: our_pawn_material,
      piece_square_score: our_piece_square_score,
    )

  let opposing_pieces =
    game.PieceInfo(
      king_position: opposing_king_position,
      non_pawn_material: opposing_non_pawn_material,
      pawn_material: opposing_pawn_material,
      piece_square_score: opposing_piece_square_score,
    )

  let #(white_pieces, black_pieces) = case to_move {
    board.White -> #(our_pieces, opposing_pieces)
    board.Black -> #(opposing_pieces, our_pieces)
  }

  let to_move = case to_move {
    board.Black -> board.White
    board.White -> board.Black
  }

  let #(half_moves, previous_positions) = case one_way_move {
    True -> #(0, [])
    False -> #(half_moves + 1, [previous_hash, ..previous_positions])
  }

  // TODO: Maybe we can update this incrementally too?
  let attack_information =
    attack.calculate(board, opposing_king_position, to_move)

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
    white_pieces:,
    black_pieces:,
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
  legal_moves: List(Move),
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

pub fn from_standard_algebraic_notation(
  move: String,
  game: Game,
  legal_moves: List(Move),
) -> Result(Move, Nil) {
  case parse_special_move(move, game, legal_moves) {
    Illegal -> Error(Nil)
    Legal(move) -> Ok(move)
    Invalid -> {
      let #(piece_kind, move) = case move {
        "R" <> move -> #(board.Rook, move)
        "B" <> move -> #(board.Bishop, move)
        "N" <> move -> #(board.Knight, move)
        "K" <> move -> #(board.King, move)
        "Q" <> move -> #(board.Queen, move)
        _ -> #(board.Pawn, move)
      }

      // Pawn moves have entirely different syntax than non-pawn moves
      use <- bool.lazy_guard(piece_kind == board.Pawn, fn() {
        parse_pawn_move(move, game, legal_moves)
      })

      // In order to determine which move syntax this is, we need to parse the
      // first two characters and match on what they are.
      use #(first, move) <- result.try(parse_move_part(move))
      use #(second, move) <- result.try(parse_move_part(move))

      use #(from_file, from_rank, capture, to_file, to_rank, move) <- result.try(
        case first, second {
          // `xx` is not an allowed move
          CaptureSpecifier, CaptureSpecifier -> Error(Nil)
          // We disambiguate the file and it's a capture (e.g. `Baxc6`)
          File(file), CaptureSpecifier -> {
            let from_file = Some(file)
            let from_rank = None
            let capture = True
            use #(to_file, move) <- result.try(parse_file(move))
            use #(to_rank, move) <- result.try(parse_rank(move))

            Ok(#(from_file, from_rank, capture, to_file, to_rank, move))
          }
          // We disambiguate the rank and it's a capture (e.g. `R5xc4`)
          Rank(rank), CaptureSpecifier -> {
            let from_file = None
            let from_rank = Some(rank)
            let capture = True
            use #(to_file, move) <- result.try(parse_file(move))
            use #(to_rank, move) <- result.try(parse_rank(move))

            Ok(#(from_file, from_rank, capture, to_file, to_rank, move))
          }
          // It's a capture, and we've parsed the file of the destination (e.g.
          // `Bxa5`)
          CaptureSpecifier, File(to_file) -> {
            let from_file = None
            let from_rank = None
            let capture = True
            use #(to_rank, move) <- result.try(parse_rank(move))

            Ok(#(from_file, from_rank, capture, to_file, to_rank, move))
          }
          // We disambiguate the file and we've parsed the file of the destination
          // (e.g. `Qhd4`)
          File(from_file), File(to_file) -> {
            let from_file = Some(from_file)
            let from_rank = None
            let capture = False
            use #(to_rank, move) <- result.try(parse_rank(move))

            Ok(#(from_file, from_rank, capture, to_file, to_rank, move))
          }
          // We disambiguate the rank and we've parsed the file of the destination
          // (e.g. `R7d2`)
          Rank(rank), File(to_file) -> {
            let from_file = None
            let from_rank = Some(rank)
            let capture = False
            use #(to_rank, move) <- result.try(parse_rank(move))

            Ok(#(from_file, from_rank, capture, to_file, to_rank, move))
          }
          // Capture followed by a rank is not allowed, e.g. `Rx1`
          CaptureSpecifier, Rank(_) -> Error(Nil)
          // We've parsed the file and rank, and there's no more move to parse,
          // so we're done. (e.g. `Nf3`)
          File(file), Rank(rank) if move == "" ->
            Ok(#(None, None, False, file, rank, move))
          // We've disambiguated the rank and file, and we still need to parse
          // the rest of the move. (e.g. `Qh4xe1`)
          File(from_file), Rank(from_rank) ->
            case parse_move_part(move) {
              Ok(#(CaptureSpecifier, move)) -> {
                let from_file = Some(from_file)
                let from_rank = Some(from_rank)
                let capture = True
                use #(to_file, move) <- result.try(parse_file(move))
                use #(to_rank, move) <- result.try(parse_rank(move))

                Ok(#(from_file, from_rank, capture, to_file, to_rank, move))
              }
              Ok(#(File(to_file), _)) -> {
                let from_file = Some(from_file)
                let from_rank = Some(from_rank)
                let capture = False
                use #(to_rank, move) <- result.try(parse_rank(move))

                Ok(#(from_file, from_rank, capture, to_file, to_rank, move))
              }
              Ok(#(Rank(_), _)) | Error(_) -> Error(Nil)
            }
          // Two ranks are not allowed, e.g. `R15`
          Rank(_), Rank(_) -> Error(Nil)
        },
      )

      use <- bool.guard(move != "", Error(Nil))

      let to = to_rank * 8 + to_file

      case get_pieces(game, piece_kind, legal_moves, from_file, from_rank, to) {
        [from] if capture -> Ok(Capture(from:, to:))
        [from] -> Ok(Move(from:, to:))
        // If there is more than one valid move, the notation is ambiguous, and
        // so we error. If there are no valid moves, we also error.
        _ -> Error(Nil)
      }
    }
  }
}

type MovePart {
  CaptureSpecifier
  File(Int)
  Rank(Int)
}

fn parse_move_part(move: String) -> Result(#(MovePart, String), Nil) {
  case move {
    "x" <> move -> Ok(#(CaptureSpecifier, move))
    _ ->
      case parse_file(move) {
        Ok(#(file, move)) -> Ok(#(File(file), move))
        Error(_) ->
          case parse_rank(move) {
            Ok(#(rank, move)) -> Ok(#(Rank(rank), move))
            Error(Nil) -> Error(Nil)
          }
      }
  }
}

type SpecialMove {
  Invalid
  Illegal
  Legal(Move)
}

fn parse_special_move(
  move: String,
  game: Game,
  legal_moves: List(Move),
) -> SpecialMove {
  let move = case move {
    "O-O" | "0-0" if game.to_move == board.White -> Ok(Castle(4, 6))
    "O-O" | "0-0" -> Ok(Castle(60, 62))
    "O-O-O" | "0-0-0" if game.to_move == board.White -> Ok(Castle(4, 2))
    "O-O-O" | "0-0-0" -> Ok(Castle(60, 58))
    _ -> Error(Nil)
  }
  case move {
    Error(_) -> Invalid
    Ok(move) ->
      case list.contains(legal_moves, move) {
        False -> Illegal
        True -> Legal(move)
      }
  }
}

fn parse_pawn_move(
  move: String,
  game: Game,
  legal_moves: List(Move),
) -> Result(Move, Nil) {
  use #(file, move) <- result.try(parse_file(move))

  use #(from_file, is_capture, to_file, move) <- result.try(case move {
    "x" <> move ->
      parse_file(move)
      |> result.map(fn(pair) {
        let #(to_file, move) = pair
        #(Some(file), True, to_file, move)
      })
    _ -> Ok(#(None, False, file, move))
  })

  use #(rank, move) <- result.try(parse_rank(move))

  use promotion <- result.try(case move {
    "" -> Ok(None)
    "n" | "N" | "=n" | "=N" -> Ok(Some(board.Knight))
    "q" | "Q" | "=q" | "=Q" -> Ok(Some(board.Queen))
    "b" | "B" | "=b" | "=B" -> Ok(Some(board.Bishop))
    "r" | "R" | "=r" | "=R" -> Ok(Some(board.Rook))
    _ -> Error(Nil)
  })

  let to = rank * 8 + to_file

  case
    get_pieces(game, board.Pawn, legal_moves, from_file, None, to),
    promotion
  {
    [from], Some(piece) -> Ok(Promotion(from:, to:, piece:))
    [from], _ if game.en_passant_square == Some(to) -> Ok(EnPassant(from:, to:))
    [from], _ if is_capture -> Ok(Capture(from:, to:))
    [from], _ -> Ok(Move(from:, to:))
    _, _ -> Error(Nil)
  }
}

/// Gets the possible destination squares for a move, based on the information
/// we know.
fn get_pieces(
  game: Game,
  find_piece: board.Piece,
  legal_moves: List(Move),
  from_file: option.Option(Int),
  from_rank: option.Option(Int),
  to: Int,
) -> List(Int) {
  use pieces, position, #(piece, colour) <- dict.fold(game.board, [])
  let is_valid =
    colour == game.to_move
    && piece == find_piece
    && case from_file {
      None -> True
      Some(file) -> file == position % 8
    }
    && case from_rank {
      None -> True
      Some(rank) -> rank == position / 8
    }
    && list.any(legal_moves, fn(move) { move.to == to && move.from == position })

  case is_valid {
    False -> pieces
    True -> [position, ..pieces]
  }
}

fn parse_file(move: String) -> Result(#(Int, String), Nil) {
  case move {
    "a" <> move | "A" <> move -> Ok(#(0, move))
    "b" <> move | "B" <> move -> Ok(#(1, move))
    "c" <> move | "C" <> move -> Ok(#(2, move))
    "d" <> move | "D" <> move -> Ok(#(3, move))
    "e" <> move | "E" <> move -> Ok(#(4, move))
    "f" <> move | "F" <> move -> Ok(#(5, move))
    "g" <> move | "G" <> move -> Ok(#(6, move))
    "h" <> move | "H" <> move -> Ok(#(7, move))
    _ -> Error(Nil)
  }
}

fn parse_rank(move: String) -> Result(#(Int, String), Nil) {
  case move {
    "1" <> move -> Ok(#(0, move))
    "2" <> move -> Ok(#(1, move))
    "3" <> move -> Ok(#(2, move))
    "4" <> move -> Ok(#(3, move))
    "5" <> move -> Ok(#(4, move))
    "6" <> move -> Ok(#(5, move))
    "7" <> move -> Ok(#(6, move))
    "8" <> move -> Ok(#(7, move))
    _ -> Error(Nil)
  }
}
