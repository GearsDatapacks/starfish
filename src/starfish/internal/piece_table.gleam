//// Piece tables indicating stronger and weaker positions for each piece type.
//// For example, knights are stronger in the centre of the board and weaker near
//// the edge.
//// Each table is made from white's perspective. To get scores for black, simply
//// reverse the squares.

import iv

const pawn = []

const knight = []

const bishop = []

const rook = []

const queen = []

const king = []

/// In the beginning and middle of the game, the king must be kept safe, however
/// as the game progresses towards the end, the king should become more aggressive
/// so we use a different set of scores for kings in the endgame.
const king_endgame = []

/// A type which keeps track of the table for each piece
pub opaque type PieceTables {
  PieceTables(
    pawn: iv.Array(Int),
    knight: iv.Array(Int),
    bishop: iv.Array(Int),
    rook: iv.Array(Int),
    queen: iv.Array(Int),
    king: iv.Array(Int),
    king_endgame: iv.Array(Int),
  )
}

/// Turn the `List`s which are in the constants into `iv.Arrays` for faster
/// accessing
pub fn construct_tables() -> PieceTables {
  PieceTables(
    pawn: iv.from_list(pawn),
    knight: iv.from_list(knight),
    bishop: iv.from_list(bishop),
    rook: iv.from_list(rook),
    queen: iv.from_list(queen),
    king: iv.from_list(king),
    king_endgame: iv.from_list(king_endgame),
  )
}
