import gleam/list
import starfish/internal/board
import starfish/internal/game

const phase_multiplier = 128

/// About queen + rook, so one major piece per side
const endgame_material = 1400

/// Below this material limit, the endgame weight is zero. this is about enough
/// for three minor pieces to be captured.
const middlegame_material = 3000

pub fn phase(game: game.Game) -> Int {
  let non_pawn_material =
    game.white_pieces.non_pawn_material + game.black_pieces.non_pawn_material

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

pub fn interpolate_phase(
  middlegame_value: Int,
  endgame_value: Int,
  phase: Int,
) -> Int {
  { middlegame_value * { phase_multiplier - phase } + endgame_value * phase }
  / phase_multiplier
}

/// Statically evaluates a position. Does not take into account checkmate or
/// stalemate, those must be accounted for beforehand.
pub fn evaluate(game: game.Game, legal_moves: List(move)) -> Int {
  let phase = phase(game)

  let black_position_score = position_score(game.black_pieces, phase)
  let white_position_score = position_score(game.white_pieces, phase)

  let position_score = case game.to_move {
    board.Black -> black_position_score - white_position_score
    board.White -> white_position_score - black_position_score
  }

  position_score + list.length(legal_moves)
}

fn position_score(pieces: game.PieceInfo, phase: Int) -> Int {
  let game.PieceInfo(
    king_position: _,
    non_pawn_material:,
    pawn_material:,
    piece_square_score_midgame:,
    piece_square_score_endgame:,
  ) = pieces
  non_pawn_material
  + pawn_material
  + interpolate_phase(
    piece_square_score_midgame,
    piece_square_score_endgame,
    phase,
  )
}
