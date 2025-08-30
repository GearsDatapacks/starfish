import gleam/bool
import gleam/option.{type Option, None, Some}
import gleam/set
import iv
import starfish/internal/board
import starfish/internal/game.{type Game, Game}
import starfish/internal/hash
import starfish/internal/move/attack
import starfish/internal/move/direction.{type Direction}

pub type Valid

pub type Legal

pub type Move(validity) {
  Castle(from: Int, to: Int)
  Move(from: Int, to: Int)
  Capture(from: Int, to: Int)
  EnPassant(from: Int, to: Int)
  Promotion(from: Int, to: Int, piece: board.Piece)
}

pub fn legal(game: Game) -> List(Move(Legal)) {
  use moves, square, position <- iv.index_fold(game.board, [])

  case square {
    board.Occupied(piece:, colour:) if colour == game.to_move ->
      moves_for_piece(game, position, piece, moves)
    board.Occupied(_, _) | board.Empty -> moves
  }
}

fn moves_for_piece(
  game: Game,
  position: Int,
  piece: board.Piece,
  moves: List(Move(Legal)),
) -> List(Move(Legal)) {
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

fn pawn_moves(
  game: Game,
  position: Int,
  moves: List(Move(Legal)),
) -> List(Move(Legal)) {
  let #(forward, left, right) = case game.to_move {
    board.Black -> #(direction.down, direction.down_left, direction.down_right)
    board.White -> #(direction.up, direction.up_left, direction.up_right)
  }

  let forward_one = direction.in_direction(position, forward)
  let moves = case iv.get(game.board, forward_one) {
    Ok(board.Empty) -> {
      let moves = [Move(from: position, to: forward_one), ..moves]

      let can_double_move = case game.to_move, board.rank(position) {
        board.Black, 6 | board.White, 1 -> True
        _, _ -> False
      }

      use <- bool.guard(!can_double_move, moves)

      let forward_two = direction.in_direction(forward_one, forward)
      case iv.get(game.board, forward_two) {
        Ok(board.Empty) -> [Move(from: position, to: forward_two), ..moves]
        Ok(board.Occupied(_, _)) | Error(_) -> moves
      }
    }
    Ok(board.Occupied(_, _)) | Error(_) -> moves
  }

  let new_position = direction.in_direction(position, left)
  let moves = case iv.get(game.board, new_position) {
    Ok(board.Occupied(colour:, ..)) if colour != game.to_move -> [
      Capture(from: position, to: new_position),
      ..moves
    ]
    Ok(board.Empty) if game.en_passant_square == Some(new_position) -> [
      EnPassant(from: position, to: new_position),
      ..moves
    ]
    Ok(board.Empty) | Ok(board.Occupied(_, _)) | Error(_) -> moves
  }

  let new_position = direction.in_direction(position, right)
  case iv.get(game.board, new_position) {
    Ok(board.Occupied(colour:, ..)) if colour != game.to_move -> [
      Capture(from: position, to: new_position),
      ..moves
    ]
    Ok(board.Empty) if game.en_passant_square == Some(new_position) -> [
      EnPassant(from: position, to: new_position),
      ..moves
    ]
    Ok(board.Empty) | Ok(board.Occupied(_, _)) | Error(_) -> moves
  }
}

fn knight_moves(
  game: Game,
  position: Int,
  moves: List(Move(Legal)),
  directions: List(Direction),
) -> List(Move(Legal)) {
  case directions {
    [] -> moves
    [direction, ..directions] -> {
      let new_position = direction.in_direction(position, direction)
      let moves = case iv.get(game.board, new_position) {
        Ok(board.Empty) -> [Move(from: position, to: new_position), ..moves]
        Ok(board.Occupied(colour:, ..)) if colour != game.to_move -> [
          Capture(from: position, to: new_position),
          ..moves
        ]
        Ok(board.Occupied(_, _)) | Error(_) -> moves
      }

      knight_moves(game, position, moves, directions)
    }
  }
}

fn king_moves(
  game: Game,
  position: Int,
  moves: List(Move(Legal)),
  directions: List(Direction),
) -> List(Move(Legal)) {
  let moves = regular_king_moves(game, position, moves, directions)

  let is_empty = fn(position) {
    iv.get_or_default(game.board, position, board.Empty) == board.Empty
  }

  let moves = case game.to_move {
    board.Black if game.castling.black_kingside ->
      case is_empty(61) && is_empty(62) {
        True -> [Castle(from: position, to: 62), ..moves]
        False -> moves
      }
    board.White if game.castling.white_kingside ->
      case is_empty(5) && is_empty(6) {
        True -> [Castle(from: position, to: 6), ..moves]
        False -> moves
      }
    _ -> moves
  }

  case game.to_move {
    board.Black if game.castling.black_queenside ->
      case is_empty(59) && is_empty(58) && is_empty(57) {
        True -> [Castle(from: position, to: 58), ..moves]
        False -> moves
      }
    board.White if game.castling.white_queenside ->
      case is_empty(3) && is_empty(2) && is_empty(1) {
        True -> [Castle(from: position, to: 2), ..moves]
        False -> moves
      }
    _ -> moves
  }
}

fn regular_king_moves(
  game: Game,
  position: Int,
  moves: List(Move(Legal)),
  directions: List(Direction),
) -> List(Move(Legal)) {
  case directions {
    [] -> moves
    [direction, ..directions] -> {
      let new_position = direction.in_direction(position, direction)
      let moves = case iv.get(game.board, new_position) {
        Ok(board.Empty) -> [Move(from: position, to: new_position), ..moves]
        Ok(board.Occupied(colour:, ..)) if colour != game.to_move -> [
          Capture(from: position, to: new_position),
          ..moves
        ]
        Ok(board.Occupied(_, _)) | Error(_) -> moves
      }

      regular_king_moves(game, position, moves, directions)
    }
  }
}

fn sliding_moves(
  game: Game,
  position: Int,
  moves: List(Move(Legal)),
  directions: List(Direction),
) -> List(Move(Legal)) {
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
  moves: List(Move(Legal)),
) -> List(Move(Legal)) {
  let new_position = direction.in_direction(position, direction)
  case iv.get(game.board, new_position) {
    Ok(board.Empty) ->
      sliding_moves_in_direction(game, start_position, new_position, direction, [
        Move(from: start_position, to: new_position),
        ..moves
      ])
    Ok(board.Occupied(colour:, ..)) if colour != game.to_move -> [
      Capture(from: start_position, to: new_position),
      ..moves
    ]
    Ok(board.Occupied(_, _)) | Error(_) -> moves
  }
}

pub fn apply(game: Game, move: Move(Legal)) -> game.Game {
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
    hash_data:,
    piece_tables:,
    previous_positions:,
    attack_information: _,
  ) = game

  let assert board.Occupied(piece:, colour:) =
    iv.get_or_default(board, from, board.Empty)
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
    |> iv.try_set(from, board.Empty)
    |> iv.try_set(
      board.position(file: rook_file_from, rank: rook_rank),
      board.Empty,
    )
    |> iv.try_set(to, board.Occupied(piece:, colour:))
    |> iv.try_set(
      board.position(file: rook_file_to, rank: rook_rank),
      board.Occupied(piece: board.Rook, colour:),
    )

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
  let zobrist_hash = hash.hash(hash_data, board, to_move)

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
    hash_data:,
    piece_tables:,
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
    hash_data:,
    piece_tables:,
    previous_positions:,
    attack_information: _,
  ) = game

  let assert board.Occupied(piece:, colour:) =
    iv.get_or_default(board, from, board.Empty)
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
    |> iv.try_set(from, board.Empty)
    |> iv.try_set(to, board.Occupied(piece:, colour:))

  let board = case en_passant, en_passant_square, colour {
    True, Some(square), board.White ->
      iv.try_set(board, square - 8, board.Empty)
    True, Some(square), board.Black ->
      iv.try_set(board, square + 8, board.Empty)
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
  let zobrist_hash = hash.hash(hash_data, board, to_move)

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
    hash_data:,
    piece_tables:,
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
