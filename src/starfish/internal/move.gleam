import gleam/bool
import iv
import starfish/internal/board
import starfish/internal/direction.{type Direction}
import starfish/internal/game.{type Game}

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
    board.Occupied(piece) if piece.colour == game.to_move ->
      moves_for_piece(game, position, piece, moves)
    board.Occupied(_) | board.Empty -> moves
  }
}

fn moves_for_piece(
  game: Game,
  position: Int,
  piece: board.Piece,
  moves: List(Move(Legal)),
) -> List(Move(Legal)) {
  case piece {
    board.Bishop(_) ->
      sliding_moves(game, position, moves, direction.bishop_directions)
    board.Rook(_) ->
      sliding_moves(game, position, moves, direction.rook_directions)
    board.Queen(_) ->
      sliding_moves(game, position, moves, direction.queen_directions)
    board.King(_) ->
      king_moves(game, position, moves, direction.queen_directions)
    board.Knight(_) ->
      knight_moves(game, position, moves, direction.knight_directions)
    board.Pawn(_) -> pawn_moves(game, position, moves)
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
        Ok(board.Occupied(_)) | Error(_) -> moves
      }
    }
    Ok(board.Occupied(_)) | Error(_) -> moves
  }

  let new_position = direction.in_direction(position, left)
  let moves = case iv.get(game.board, new_position) {
    Ok(board.Occupied(piece)) if piece.colour != game.to_move -> [
      Capture(from: position, to: new_position),
      ..moves
    ]
    Ok(board.Empty) | Ok(board.Occupied(_)) | Error(_) -> moves
  }

  let new_position = direction.in_direction(position, right)
  case iv.get(game.board, new_position) {
    Ok(board.Occupied(piece)) if piece.colour != game.to_move -> [
      Capture(from: position, to: new_position),
      ..moves
    ]
    Ok(board.Empty) | Ok(board.Occupied(_)) | Error(_) -> moves
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
        Ok(board.Occupied(piece)) if piece.colour != game.to_move -> [
          Capture(from: position, to: new_position),
          ..moves
        ]
        Ok(board.Occupied(_)) | Error(_) -> moves
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
  case directions {
    [] -> moves
    [direction, ..directions] -> {
      let new_position = direction.in_direction(position, direction)
      let moves = case iv.get(game.board, new_position) {
        Ok(board.Empty) -> [Move(from: position, to: new_position), ..moves]
        Ok(board.Occupied(piece)) if piece.colour != game.to_move -> [
          Capture(from: position, to: new_position),
          ..moves
        ]
        Ok(board.Occupied(_)) | Error(_) -> moves
      }

      king_moves(game, position, moves, directions)
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
        sliding_moves_in_direction(game, position, direction, moves),
        directions,
      )
  }
}

fn sliding_moves_in_direction(
  game: Game,
  position: Int,
  direction: Direction,
  moves: List(Move(Legal)),
) -> List(Move(Legal)) {
  let new_position = direction.in_direction(position, direction)
  case iv.get(game.board, new_position) {
    Ok(board.Empty) ->
      sliding_moves_in_direction(game, new_position, direction, [
        Move(from: position, to: new_position),
        ..moves
      ])
    Ok(board.Occupied(piece)) if piece.colour != game.to_move -> [
      Capture(from: position, to: new_position),
      ..moves
    ]
    Ok(board.Occupied(_)) | Error(_) -> moves
  }
}
