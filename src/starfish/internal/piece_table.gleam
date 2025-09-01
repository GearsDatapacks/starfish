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
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  30,
  30,
  30,
  30,
  30,
  30,
  30,
  30,
  10,
  10,
  20,
  30,
  30,
  20,
  10,
  10,
  5,
  5,
  10,
  25,
  25,
  10,
  5,
  5,
  0,
  0,
  0,
  20,
  20,
  0,
  0,
  0,
  5,
  -5,
  -10,
  0,
  0,
  -10,
  -5,
  5,
  5,
  10,
  10,
  -20,
  -20,
  10,
  10,
  5,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
)

const knight = #(
  -50,
  -40,
  -30,
  -30,
  -30,
  -30,
  -40,
  -50,
  -40,
  -20,
  0,
  0,
  0,
  0,
  -20,
  -40,
  -30,
  0,
  10,
  15,
  15,
  10,
  0,
  -30,
  -30,
  5,
  15,
  20,
  20,
  15,
  5,
  -30,
  -30,
  0,
  15,
  20,
  20,
  15,
  0,
  -30,
  -30,
  5,
  10,
  15,
  15,
  10,
  5,
  -30,
  -40,
  -20,
  0,
  5,
  5,
  0,
  -20,
  -40,
  -50,
  -40,
  -30,
  -30,
  -30,
  -30,
  -40,
  -50,
)

const bishop = #(
  -20,
  -10,
  -10,
  -10,
  -10,
  -10,
  -10,
  -20,
  -10,
  0,
  0,
  0,
  0,
  0,
  0,
  -10,
  -10,
  0,
  5,
  10,
  10,
  5,
  0,
  -10,
  -10,
  5,
  5,
  10,
  10,
  5,
  5,
  -10,
  -10,
  0,
  10,
  10,
  10,
  10,
  0,
  -10,
  -10,
  10,
  10,
  10,
  10,
  10,
  10,
  -10,
  -10,
  5,
  0,
  0,
  0,
  0,
  5,
  -10,
  -20,
  -10,
  -10,
  -10,
  -10,
  -10,
  -10,
  -20,
)

const rook = #(
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  5,
  10,
  10,
  10,
  10,
  10,
  10,
  5,
  -5,
  0,
  0,
  0,
  0,
  0,
  0,
  -5,
  -5,
  0,
  0,
  0,
  0,
  0,
  0,
  -5,
  -5,
  0,
  0,
  0,
  0,
  0,
  0,
  -5,
  -5,
  0,
  0,
  0,
  0,
  0,
  0,
  -5,
  -5,
  0,
  0,
  0,
  0,
  0,
  0,
  -5,
  0,
  0,
  0,
  5,
  5,
  0,
  0,
  0,
)

const queen = #(
  -20,
  -10,
  -10,
  -5,
  -5,
  -10,
  -10,
  -20,
  -10,
  0,
  0,
  0,
  0,
  0,
  0,
  -10,
  -10,
  0,
  5,
  5,
  5,
  5,
  0,
  -10,
  -5,
  0,
  5,
  5,
  5,
  5,
  0,
  -5,
  0,
  0,
  5,
  5,
  5,
  5,
  0,
  -5,
  -10,
  5,
  5,
  5,
  5,
  5,
  0,
  -10,
  -10,
  0,
  5,
  0,
  0,
  0,
  0,
  -10,
  -20,
  -10,
  -10,
  -5,
  -5,
  -10,
  -10,
  -20,
)

const king = #(
  -30,
  -40,
  -40,
  -50,
  -50,
  -40,
  -40,
  -30,
  -30,
  -40,
  -40,
  -50,
  -50,
  -40,
  -40,
  -30,
  -30,
  -40,
  -40,
  -50,
  -50,
  -40,
  -40,
  -30,
  -30,
  -40,
  -40,
  -50,
  -50,
  -40,
  -40,
  -30,
  -20,
  -30,
  -30,
  -40,
  -40,
  -30,
  -30,
  -20,
  -10,
  -20,
  -20,
  -20,
  -20,
  -20,
  -20,
  -10,
  20,
  20,
  0,
  0,
  0,
  0,
  20,
  20,
  20,
  30,
  10,
  0,
  0,
  10,
  30,
  20,
)

/// In the beginning and middle of the game, the king must be kept safe, however
/// as the game progresses towards the end, the king should become more aggressive
/// so we use a different set of scores for kings in the endgame.
const king_endgame = #(
  -50,
  -40,
  -30,
  -20,
  -20,
  -30,
  -40,
  -50,
  -30,
  -20,
  -10,
  0,
  0,
  -10,
  -20,
  -30,
  -30,
  -10,
  20,
  30,
  30,
  20,
  -10,
  -30,
  -30,
  -10,
  30,
  40,
  40,
  30,
  -10,
  -30,
  -30,
  -10,
  30,
  40,
  40,
  30,
  -10,
  -30,
  -30,
  -10,
  20,
  30,
  30,
  20,
  -10,
  -30,
  -30,
  -30,
  0,
  0,
  0,
  0,
  -30,
  -30,
  -50,
  -30,
  -30,
  -30,
  -30,
  -30,
  -30,
  -50,
)

type Table =
  #(
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
    Int,
  )

fn get(table: Table, index: Int, default: Int) -> Int {
  case index {
    0 -> table.0
    1 -> table.1
    2 -> table.2
    3 -> table.3
    4 -> table.4
    5 -> table.5
    6 -> table.6
    7 -> table.7
    8 -> table.8
    9 -> table.9
    10 -> table.10
    11 -> table.11
    12 -> table.12
    13 -> table.13
    14 -> table.14
    15 -> table.15
    16 -> table.16
    17 -> table.17
    18 -> table.18
    19 -> table.19
    20 -> table.20
    21 -> table.21
    22 -> table.22
    23 -> table.23
    24 -> table.24
    25 -> table.25
    26 -> table.26
    27 -> table.27
    28 -> table.28
    29 -> table.29
    30 -> table.30
    31 -> table.31
    32 -> table.32
    33 -> table.33
    34 -> table.34
    35 -> table.35
    36 -> table.36
    37 -> table.37
    38 -> table.38
    39 -> table.39
    40 -> table.40
    41 -> table.41
    42 -> table.42
    43 -> table.43
    44 -> table.44
    45 -> table.45
    46 -> table.46
    47 -> table.47
    48 -> table.48
    49 -> table.49
    50 -> table.50
    51 -> table.51
    52 -> table.52
    53 -> table.53
    54 -> table.54
    55 -> table.55
    56 -> table.56
    57 -> table.57
    58 -> table.58
    59 -> table.59
    60 -> table.60
    61 -> table.61
    62 -> table.62
    63 -> table.63
    _ -> default
  }
}

/// Calculate the score for a given piece at a position at some point in the game
pub fn piece_score(
  piece: board.Piece,
  colour: board.Colour,
  position: Int,
) -> Int {
  let table = case piece {
    board.Pawn -> pawn
    board.Bishop -> bishop
    board.Knight -> knight
    board.Queen -> queen
    board.Rook -> rook
    board.King -> king
  }

  // If the piece is black, the table must be reversed to we index from the end
  // instead.
  let index = case colour {
    board.White -> position
    board.Black -> 63 - position
  }

  // TODO: take into account endgame for kings
  get(table, index, 0)
}
