import gleam/bool
import gleam/dict
import gleam/int
import gleam/result

pub const side_length = 8

pub const size = 64

pub type Board =
  dict.Dict(Int, #(Piece, Colour))

pub type Square {
  Empty
  OffBoard
  Occupied(piece: Piece, colour: Colour)
}

pub fn get(board: Board, position: Int) -> Square {
  use <- bool.guard(position == -1, OffBoard)
  case dict.get(board, position) {
    Ok(#(piece, colour)) -> Occupied(piece:, colour:)
    Error(_) -> Empty
  }
}

pub type Piece {
  Pawn
  Bishop
  Knight
  Rook
  Queen
  King
}

pub const pawn_promotions = [Bishop, Knight, Rook, Queen]

pub type Colour {
  White
  Black
}

pub fn position(file file: Int, rank rank: Int) -> Int {
  rank * 8 + file
}

pub fn file(position: Int) -> Int {
  position % 8
}

pub fn rank(position: Int) -> Int {
  position / 8
}

pub fn position_to_string(position: Int) -> String {
  // Add one, because file/rank is zero-indexed
  let rank = int.to_string(rank(position) + 1)

  let file = case file(position) {
    0 -> "a"
    1 -> "b"
    2 -> "c"
    3 -> "d"
    4 -> "e"
    5 -> "f"
    6 -> "g"
    _ -> "h"
  }

  file <> rank
}

pub fn parse_position(fen: String) -> Result(#(Int, String), Nil) {
  use #(file, fen) <- result.try(case fen {
    "a" <> fen | "A" <> fen -> Ok(#(0, fen))
    "b" <> fen | "B" <> fen -> Ok(#(1, fen))
    "c" <> fen | "C" <> fen -> Ok(#(2, fen))
    "d" <> fen | "D" <> fen -> Ok(#(3, fen))
    "e" <> fen | "E" <> fen -> Ok(#(4, fen))
    "f" <> fen | "F" <> fen -> Ok(#(5, fen))
    "g" <> fen | "G" <> fen -> Ok(#(6, fen))
    "h" <> fen | "H" <> fen -> Ok(#(7, fen))
    _ -> Error(Nil)
  })

  use #(rank, fen) <- result.try(case fen {
    // We subtract one from the digit because we use zero-indexed positions
    "1" <> fen -> Ok(#(0, fen))
    "2" <> fen -> Ok(#(1, fen))
    "3" <> fen -> Ok(#(2, fen))
    "4" <> fen -> Ok(#(3, fen))
    "5" <> fen -> Ok(#(4, fen))
    "6" <> fen -> Ok(#(5, fen))
    "7" <> fen -> Ok(#(6, fen))
    "8" <> fen -> Ok(#(7, fen))
    _ -> Error(Nil)
  })

  Ok(#(position(file:, rank:), fen))
}

pub fn from_fen(fen: String) -> #(Board, String, Bool) {
  // FEN starts from black's size, which means that `rank` needs to start at the
  // end of the board.
  from_fen_loop(fen, 0, side_length - 1, dict.new())
}

fn from_fen_loop(
  fen: String,
  file: Int,
  rank: Int,
  board: Board,
) -> #(Board, String, Bool) {
  let position = position(file:, rank:)

  case fen {
    // When we hit a `/`, we start from file 0 again, and move down the rank,
    // since we are starting from black and ending on white.
    "/" <> fen -> from_fen_loop(fen, 0, rank - 1, board)
    "0" <> fen -> from_fen_loop(fen, file, rank, board)
    "1" <> fen -> from_fen_loop(fen, file + 1, rank, board)
    "2" <> fen -> from_fen_loop(fen, file + 2, rank, board)
    "3" <> fen -> from_fen_loop(fen, file + 3, rank, board)
    "4" <> fen -> from_fen_loop(fen, file + 4, rank, board)
    "5" <> fen -> from_fen_loop(fen, file + 5, rank, board)
    "6" <> fen -> from_fen_loop(fen, file + 6, rank, board)
    "7" <> fen -> from_fen_loop(fen, file + 7, rank, board)
    "8" <> fen -> from_fen_loop(fen, file + 8, rank, board)
    "9" <> fen -> from_fen_loop(fen, file + 9, rank, board)
    "K" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        dict.insert(board, position, #(King, White)),
      )
    "Q" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        dict.insert(board, position, #(Queen, White)),
      )
    "B" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        dict.insert(board, position, #(Bishop, White)),
      )
    "N" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        dict.insert(board, position, #(Knight, White)),
      )
    "R" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        dict.insert(board, position, #(Rook, White)),
      )
    "P" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        dict.insert(board, position, #(Pawn, White)),
      )
    "k" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        dict.insert(board, position, #(King, Black)),
      )
    "q" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        dict.insert(board, position, #(Queen, Black)),
      )
    "b" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        dict.insert(board, position, #(Bishop, Black)),
      )
    "n" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        dict.insert(board, position, #(Knight, Black)),
      )
    "r" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        dict.insert(board, position, #(Rook, Black)),
      )
    "p" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        dict.insert(board, position, #(Pawn, Black)),
      )
    // Since we iterate the rank in reverse order, but we iterate the file in
    // ascending order, the final position should equal to `side_length`
    _ -> #(board, fen, position == side_length)
  }
}

pub fn to_fen(board: Board) -> String {
  do_to_fen(board, 0, side_length - 1, 0, "")
}

fn do_to_fen(
  board: Board,
  file: Int,
  rank: Int,
  empty: Int,
  fen: String,
) -> String {
  use <- bool.lazy_guard(file >= side_length, fn() {
    case rank == 0 {
      True -> maybe_add_empty(fen, empty)
      False ->
        do_to_fen(board, 0, rank - 1, 0, maybe_add_empty(fen, empty) <> "/")
    }
  })

  let position = position(file:, rank:)

  case get(board, position) {
    Empty -> do_to_fen(board, file + 1, rank, empty + 1, fen)
    Occupied(piece:, colour:) -> {
      let fen = maybe_add_empty(fen, empty)

      let fen = case piece, colour {
        Bishop, White -> fen <> "B"
        King, White -> fen <> "K"
        Knight, White -> fen <> "N"
        Pawn, White -> fen <> "P"
        Queen, White -> fen <> "Q"
        Rook, White -> fen <> "R"
        Bishop, Black -> fen <> "b"
        King, Black -> fen <> "k"
        Knight, Black -> fen <> "n"
        Pawn, Black -> fen <> "p"
        Queen, Black -> fen <> "q"
        Rook, Black -> fen <> "r"
      }

      do_to_fen(board, file + 1, rank, 0, fen)
    }
    OffBoard -> maybe_add_empty(fen, empty)
  }
}

fn maybe_add_empty(fen: String, empty: Int) -> String {
  case empty {
    0 -> fen
    _ -> fen <> int.to_string(empty)
  }
}

pub fn initial_position() -> Board {
  dict.from_list(initial_squares)
}

const initial_squares = [
  #(0, #(Rook, White)),
  #(1, #(Knight, White)),
  #(2, #(Bishop, White)),
  #(3, #(Queen, White)),
  #(4, #(King, White)),
  #(5, #(Bishop, White)),
  #(6, #(Knight, White)),
  #(7, #(Rook, White)),
  #(8, #(Pawn, White)),
  #(9, #(Pawn, White)),
  #(10, #(Pawn, White)),
  #(11, #(Pawn, White)),
  #(12, #(Pawn, White)),
  #(13, #(Pawn, White)),
  #(14, #(Pawn, White)),
  #(15, #(Pawn, White)),
  #(48, #(Pawn, Black)),
  #(49, #(Pawn, Black)),
  #(50, #(Pawn, Black)),
  #(51, #(Pawn, Black)),
  #(52, #(Pawn, Black)),
  #(53, #(Pawn, Black)),
  #(54, #(Pawn, Black)),
  #(55, #(Pawn, Black)),
  #(56, #(Rook, Black)),
  #(57, #(Knight, Black)),
  #(58, #(Bishop, Black)),
  #(59, #(Queen, Black)),
  #(60, #(King, Black)),
  #(61, #(Bishop, Black)),
  #(62, #(Knight, Black)),
  #(63, #(Rook, Black)),
]
