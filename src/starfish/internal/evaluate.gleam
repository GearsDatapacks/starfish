import gleam/dict
import gleam/list
import starfish/internal/board
import starfish/internal/game
import starfish/internal/move
import starfish/internal/piece_table

const phase_multiplier = 128

/// About queen + rook, so one major piece per side
const endgame_material = 1400

/// Below this material limit, the endgame weight is zero. this is about enough
/// for three minor pieces to be captured.
const middlegame_material = 3000

pub fn phase(game: game.Game) -> Int {
  let non_pawn_material =
    game.black_pieces.non_pawn_material + game.white_pieces.non_pawn_material
  let clamped_material = case non_pawn_material > middlegame_material {
    True -> middlegame_material
    False ->
      case non_pawn_material < endgame_material {
        True -> endgame_material
        False -> non_pawn_material
      }
  }

  { middlegame_material - clamped_material }
  * phase_multiplier
  / { middlegame_material - endgame_material }
}

/// Statically evaluates a position. Does not take into account checkmate or
/// stalemate, those must be accounted for beforehand.
pub fn evaluate(game: game.Game, legal_moves: List(#(move.Move, Int))) -> Int {
  let phase = phase(game)
  evaluate_position(game, phase) + list.length(legal_moves)
}

fn evaluate_position(game: game.Game, phase: Int) -> Int {
  use eval, position, #(piece, colour) <- dict.fold(game.board, 0)
  let score =
    board.piece_value(piece)
    + piece_table.piece_score(piece, colour, position, phase)
  case colour == game.to_move {
    True -> eval + score
    False -> eval - score
  }
}
