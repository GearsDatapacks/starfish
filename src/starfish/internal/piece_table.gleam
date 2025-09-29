//// Piece tables indicating stronger and weaker positions for each piece type.
//// For example, knights are stronger in the centre of the board and weaker near
//// the edge.
//// Each table is made from white's perspective. To get scores for black, simply
//// reverse the squares.

import starfish/internal/board

// Values taken from https://hxim.github.io/Stockfish-Evaluation-Guide/

// We use tuples for constant time accessing. The values are constant so we don't
// need to worry about the cost of updating.

const pawn = #(
  #(000, 000, 000, 000, 000, 000, 000, 000),
  #(003, 003, 010, 019, 016, 019, 007, -005),
  #(-009, -015, 011, 015, 032, 022, 005, -022),
  #(-004, -023, 006, 020, 040, 017, 004, -008),
  #(013, 000, -013, 001, 011, -002, -013, 005),
  #(005, -012, -007, 022, -008, -005, -015, -008),
  #(-007, 007, -003, -013, 005, -016, 010, -008),
  #(000, 000, 000, 000, 000, 000, 000, 000),
)

const knight = #(
  #(-175, -092, -074, -073, -073, -074, -092, -175),
  #(-077, -041, -027, -015, -015, -027, -041, -077),
  #(-061, -017, 006, 012, 012, 006, -017, -061),
  #(-035, 008, 040, 049, 049, 040, 008, -035),
  #(-034, 013, 044, 051, 051, 044, 013, -034),
  #(-009, 022, 058, 053, 053, 058, 022, -009),
  #(-067, -027, 004, 037, 037, 004, -027, -067),
  #(-201, -083, -056, -026, -026, -056, -083, -201),
)

const bishop = #(
  #(-053, -005, -008, -023, -023, -008, -005, -053),
  #(-015, 008, 019, 004, 004, 019, 008, -015),
  #(-007, 021, -005, 017, 017, -005, 021, -007),
  #(-005, 011, 025, 039, 039, 025, 011, -005),
  #(-012, 029, 022, 031, 031, 022, 029, -012),
  #(-016, 006, 001, 011, 011, 001, 006, -016),
  #(-017, -014, 005, 000, 000, 005, -014, -017),
  #(-048, 001, -014, -023, -023, -014, 001, -048),
)

const rook = #(
  #(-031, -020, -014, -005, -005, -014, -020, -031),
  #(-021, -013, -008, 006, 006, -008, -013, -021),
  #(-025, -011, -001, 003, 003, -001, -011, -025),
  #(-013, -005, -004, -006, -006, -004, -005, -013),
  #(-027, -015, -004, 003, 003, -004, -015, -027),
  #(-022, -002, 006, 012, 012, 006, -002, -022),
  #(-002, 012, 016, 018, 018, 016, 012, -002),
  #(-017, -019, -001, 009, 009, -001, -019, -017),
)

const queen = #(
  #(003, -005, -005, 004, 004, -005, -005, 003),
  #(-003, 005, 008, 012, 012, 008, 005, -003),
  #(-003, 006, 013, 007, 007, 013, 006, -003),
  #(004, 005, 009, 008, 008, 009, 005, 004),
  #(000, 014, 012, 005, 005, 012, 014, 000),
  #(-004, 010, 006, 008, 008, 006, 010, -004),
  #(-005, 006, 010, 008, 008, 010, 006, -005),
  #(-002, -002, 001, -002, -002, 001, -002, -002),
)

const king = #(
  #(271, 327, 271, 198, 198, 271, 327, 271),
  #(278, 303, 234, 179, 179, 234, 303, 278),
  #(195, 258, 169, 120, 120, 169, 258, 195),
  #(164, 190, 138, 098, 098, 138, 190, 164),
  #(154, 179, 105, 070, 070, 105, 179, 154),
  #(123, 145, 081, 031, 031, 081, 145, 123),
  #(088, 120, 065, 033, 033, 065, 120, 088),
  #(059, 089, 045, -001, -001, 045, 089, 059),
)

/// In the middlegame, pawns are encouraged to protect the king's castling
/// squares. In the endgame though, they no longer need to protect the king and
/// instead should promote. Therefore, we use a different table to encourage this.
const pawn_endgame = #(
  #(000, 000, 000, 000, 000, 000, 000, 000),
  #(-010, -006, 010, 000, 014, 007, -005, -019),
  #(-010, -010, -010, 004, 004, 003, -006, -004),
  #(006, -002, -008, -004, -013, -012, -010, -009),
  #(010, 005, 004, -005, -005, -005, 014, 009),
  #(028, 020, 021, 028, 030, 007, 006, 013),
  #(000, -011, 012, 021, 025, 019, 004, 007),
  #(000, 000, 000, 000, 000, 000, 000, 000),
)

const knight_endgame = #(
  #(-096, -065, -049, -021, -021, -049, -065, -096),
  #(-067, -054, -018, 008, 008, -018, -054, -067),
  #(-040, -027, -008, 029, 029, -008, -027, -040),
  #(-035, -002, 013, 028, 028, 013, -002, -035),
  #(-045, -016, 009, 039, 039, 009, -016, -045),
  #(-051, -044, -016, 017, 017, -016, -044, -051),
  #(-069, -050, -051, 012, 012, -051, -050, -069),
  #(-100, -088, -056, -017, -017, -056, -088, -100),
)

const bishop_endgame = #(
  #(-057, -030, -037, -012, -012, -037, -030, -057),
  #(-037, -013, -017, 001, 001, -017, -013, -037),
  #(-016, -001, -002, 010, 010, -002, -001, -016),
  #(-020, -006, 000, 017, 017, 000, -006, -020),
  #(-017, -001, -014, 015, 015, -014, -001, -017),
  #(-030, 006, 004, 006, 006, 004, 006, -030),
  #(-031, -020, -001, 001, 001, -001, -020, -031),
  #(-046, -042, -037, -024, -024, -037, -042, -046),
)

const rook_endgame = #(
  #(-009, -013, -010, -009, -009, -010, -013, -009),
  #(-012, -009, -001, -002, -002, -001, -009, -012),
  #(006, -008, -002, -006, -006, -002, -008, 006),
  #(-006, 001, -009, 007, 007, -009, 001, -006),
  #(-005, 008, 007, -006, -006, 007, 008, -005),
  #(006, 001, -007, 010, 010, -007, 001, 006),
  #(004, 005, 020, -005, -005, 020, 005, 004),
  #(018, 000, 019, 013, 013, 019, 000, 018),
)

const queen_endgame = #(
  #(-069, -057, -047, -026, -026, -047, -057, -069),
  #(-055, -031, -022, -004, -004, -022, -031, -055),
  #(-039, -018, -009, 003, 003, -009, -018, -039),
  #(-023, -003, 013, 024, 024, 013, -003, -023),
  #(-029, -006, 009, 021, 021, 009, -006, -029),
  #(-038, -018, -012, 001, 001, -012, -018, -038),
  #(-050, -027, -024, -008, -008, -024, -027, -050),
  #(-075, -052, -043, -036, -036, -043, -052, -075),
)

/// In the beginning and middle of the game, the king must be kept safe. However
/// as the game progresses towards the end, the king should become more aggressive
/// so we use a different set of scores for kings in the endgame.
const king_endgame = #(
  #(001, 045, 085, 076, 076, 085, 045, 001),
  #(053, 100, 133, 135, 135, 133, 100, 053),
  #(088, 130, 169, 175, 175, 169, 130, 088),
  #(103, 156, 172, 172, 172, 172, 156, 103),
  #(096, 166, 199, 199, 199, 199, 166, 096),
  #(092, 172, 184, 191, 191, 184, 172, 092),
  #(047, 121, 116, 131, 131, 116, 121, 047),
  #(011, 059, 073, 078, 078, 073, 059, 011),
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

/// Calculate the score for a given piece at a position during the middlegame
pub fn piece_score_midgame(
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

  get(table, position, colour)
}

/// Calculate the score for a given piece at a position in the endgame
pub fn piece_score_endgame(
  piece: board.Piece,
  colour: board.Colour,
  position: Int,
) -> Int {
  let table = case piece {
    board.Pawn -> pawn_endgame
    board.King -> king_endgame
    board.Bishop -> bishop_endgame
    board.Knight -> knight_endgame
    board.Queen -> queen_endgame
    board.Rook -> rook_endgame
  }

  get(table, position, colour)
}
