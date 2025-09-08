//// Piece tables indicating stronger and weaker positions for each piece type.
//// For example, knights are stronger in the centre of the board and weaker near
//// the edge.
//// Each table is made from white's perspective. To get scores for black, simply
//// reverse the squares.

import starfish/internal/board

// Values taken from https://www.chessprogramming.org/Simplified_Evaluation_Function

// We use tuples for constant time accessing. The values are constant so we don't
// need to worry about the cost of updating.

const pawn = #(
  #(0, 0, 0, 0, 0, 0, 0, 0),
  #(30, 30, 30, 30, 30, 30, 30, 30),
  #(10, 10, 20, 30, 30, 20, 10, 10),
  #(5, 5, 10, 25, 25, 10, 5, 5),
  #(0, 0, 0, 20, 20, 0, 0, 0),
  #(5, -5, -10, 0, 0, -10, -5, 5),
  #(5, 10, 10, -20, -20, 10, 10, 5),
  #(0, 0, 0, 0, 0, 0, 0, 0),
)

const knight = #(
  #(-50, -40, -30, -30, -30, -30, -40, -50),
  #(-40, -20, 0, 0, 0, 0, -20, -40),
  #(-30, 0, 10, 15, 15, 10, 0, -30),
  #(-30, 5, 15, 20, 20, 15, 5, -30),
  #(-30, 0, 15, 20, 20, 15, 0, -30),
  #(-30, 5, 10, 15, 15, 10, 5, -30),
  #(-40, -20, 0, 5, 5, 0, -20, -40),
  #(-50, -40, -30, -30, -30, -30, -40, -50),
)

const bishop = #(
  #(-20, -10, -10, -10, -10, -10, -10, -20),
  #(-10, 0, 0, 0, 0, 0, 0, -10),
  #(-10, 0, 5, 10, 10, 5, 0, -10),
  #(-10, 5, 5, 10, 10, 5, 5, -10),
  #(-10, 0, 10, 10, 10, 10, 0, -10),
  #(-10, 10, 10, 10, 10, 10, 10, -10),
  #(-10, 5, 0, 0, 0, 0, 5, -10),
  #(-20, -10, -10, -10, -10, -10, -10, -20),
)

const rook = #(
  #(0, 0, 0, 0, 0, 0, 0, 0),
  #(5, 10, 10, 10, 10, 10, 10, 5),
  #(-5, 0, 0, 0, 0, 0, 0, -5),
  #(-5, 0, 0, 0, 0, 0, 0, -5),
  #(-5, 0, 0, 0, 0, 0, 0, -5),
  #(-5, 0, 0, 0, 0, 0, 0, -5),
  #(-5, 0, 0, 0, 0, 0, 0, -5),
  #(0, 0, 0, 5, 5, 0, 0, 0),
)

const queen = #(
  #(-20, -10, -10, -5, -5, -10, -10, -20),
  #(-10, 0, 0, 0, 0, 0, 0, -10),
  #(-10, 0, 5, 5, 5, 5, 0, -10),
  #(-5, 0, 5, 5, 5, 5, 0, -5),
  #(0, 0, 5, 5, 5, 5, 0, -5),
  #(-10, 5, 5, 5, 5, 5, 0, -10),
  #(-10, 0, 5, 0, 0, 0, 0, -10),
  #(-20, -10, -10, -5, -5, -10, -10, -20),
)

const king = #(
  #(-30, -40, -40, -50, -50, -40, -40, -30),
  #(-30, -40, -40, -50, -50, -40, -40, -30),
  #(-30, -40, -40, -50, -50, -40, -40, -30),
  #(-30, -40, -40, -50, -50, -40, -40, -30),
  #(-20, -30, -30, -40, -40, -30, -30, -20),
  #(-10, -20, -20, -20, -20, -20, -20, -10),
  #(20, 20, 0, 0, 0, 0, 20, 20),
  #(20, 30, 10, 0, 0, 10, 30, 20),
)

/// In the beginning and middle of the game, the king must be kept safe. However
/// as the game progresses towards the end, the king should become more aggressive
/// so we use a different set of scores for kings in the endgame.
const king_endgame = #(
  #(-50, -40, -30, -20, -20, -30, -40, -50),
  #(-30, -20, -10, 0, 0, -10, -20, -30),
  #(-30, -10, 20, 30, 30, 20, -10, -30),
  #(-30, -10, 30, 40, 40, 30, -10, -30),
  #(-30, -10, 30, 40, 40, 30, -10, -30),
  #(-30, -10, 20, 30, 30, 20, -10, -30),
  #(-30, -30, 0, 0, 0, 0, -30, -30),
  #(-50, -30, -30, -30, -30, -30, -30, -50),
)

/// In the middlegame, pawns are encouraged to protect the king's castling
/// squares. In the endgame though, they no longer need to protect the king and
/// instead should promote. Therefore, we use a different table to encourage this.
const pawn_endgame = #(
  #(100, 100, 100, 100, 100, 100, 100, 100),
  #(80, 80, 80, 80, 80, 80, 80, 80),
  #(50, 50, 50, 50, 50, 50, 50, 50),
  #(30, 30, 30, 30, 30, 30, 30, 30),
  #(10, 10, 10, 10, 10, 10, 10, 10),
  // Since pawns can double-move, the first two ranks are equivalent from the
  // pawn's perspective.
  #(-10, -10, -10, -10, -10, -10, -10, -10),
  #(-10, -10, -10, -10, -10, -10, -10, -10),
  #(-10, -10, -10, -10, -10, -10, -10, -10),
)

type Table =
  #(
    #(Int, Int, Int, Int, Int, Int, Int, Int),
    #(Int, Int, Int, Int, Int, Int, Int, Int),
    #(Int, Int, Int, Int, Int, Int, Int, Int),
    #(Int, Int, Int, Int, Int, Int, Int, Int),
    #(Int, Int, Int, Int, Int, Int, Int, Int),
    #(Int, Int, Int, Int, Int, Int, Int, Int),
    #(Int, Int, Int, Int, Int, Int, Int, Int),
    #(Int, Int, Int, Int, Int, Int, Int, Int),
  )

fn get(table: Table, position: Int, colour: board.Colour) -> Int {
  // If the piece is black, the ranks must be reversed to we index from the end
  // instead.
  let rank = case colour {
    board.White -> position / 8
    board.Black -> 7 - position / 8
  }
  let file = position % 8

  let row = case rank {
    0 -> table.0
    1 -> table.1
    2 -> table.2
    3 -> table.3
    4 -> table.4
    5 -> table.5
    6 -> table.6
    _ -> table.7
  }

  case file {
    0 -> row.0
    1 -> row.1
    2 -> row.2
    3 -> row.3
    4 -> row.4
    5 -> row.5
    6 -> row.6
    _ -> row.7
  }
}

/// Calculate the score for a given piece at a position at some point in the game
pub fn piece_score(
  piece: board.Piece,
  colour: board.Colour,
  position: Int,
  phase: Int,
) -> Int {
  let table = case piece {
    board.Pawn -> pawn
    board.Bishop -> bishop
    board.Knight -> knight
    board.Queen -> queen
    board.Rook -> rook
    board.King -> king
  }

  let middlegame_value = get(table, position, colour)

  case piece {
    board.King if phase > 0 ->
      interpolate(middlegame_value, get(king_endgame, position, colour), phase)
    board.Pawn if phase > 0 ->
      interpolate(middlegame_value, get(pawn_endgame, position, colour), phase)
    _ -> middlegame_value
  }
}

fn interpolate(middlegame_value: Int, endgame_value: Int, phase: Int) -> Int {
  { middlegame_value * { 128 - phase } + endgame_value * phase } / 128
}
