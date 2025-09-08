import gleam/list
import starfish/internal/board
import starfish/internal/game

/// Statically evaluates a position. Does not take into account checkmate or
/// stalemate, those must be accounted for beforehand.
pub fn evaluate(game: game.Game, legal_moves: List(move)) -> Int {
  let black_position_score = position_score(game.black_pieces)
  let white_position_score = position_score(game.white_pieces)

  let position_score = case game.to_move {
    board.Black -> black_position_score - white_position_score
    board.White -> white_position_score - black_position_score
  }

  position_score + list.length(legal_moves)
}

fn position_score(pieces: game.PieceInfo) -> Int {
  let game.PieceInfo(
    king_position: _,
    non_pawn_material:,
    pawn_material:,
    piece_square_score:,
  ) = pieces
  non_pawn_material + pawn_material + piece_square_score
}
