import gleam/dict
import gleam/list
import starfish/internal/board
import starfish/internal/game
import starfish/internal/move
import starfish/internal/piece_table

/// Statically evaluates a position. Does not take into account checkmate or
/// stalemate, those must be accounted for beforehand.
pub fn evaluate(game: game.Game, legal_moves: List(#(move.Move, Int))) -> Int {
  evaluate_position(game) + list.length(legal_moves)
}

fn evaluate_position(game: game.Game) -> Int {
  use eval, position, #(piece, colour) <- dict.fold(game.board, 0)
  let score =
    board.piece_value(piece) + piece_table.piece_score(piece, colour, position)
  case colour == game.to_move {
    True -> eval + score
    False -> eval - score
  }
}
