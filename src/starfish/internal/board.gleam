import gleam/bool
import gleam/int
import iv

pub const side_length = 8

pub const size = 64

pub type Board =
  iv.Array(Square)

pub type Square {
  Empty
  Occupied(piece: Piece, colour: Colour)
}

pub type Piece {
  Pawn
  Bishop
  Knight
  Rook
  Queen
  King
}

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

pub fn from_fen(fen: String) -> #(Board, String, Bool) {
  // FEN starts from black's size, which means that `rank` needs to start at the
  // end of the board.
  from_fen_loop(fen, 0, side_length - 1, iv.repeat(Empty, size))
}

fn from_fen_loop(
  fen: String,
  file: Int,
  rank: Int,
  board: iv.Array(Square),
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
        iv.try_set(board, position, Occupied(King, White)),
      )
    "Q" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        iv.try_set(board, position, Occupied(Queen, White)),
      )
    "B" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        iv.try_set(board, position, Occupied(Bishop, White)),
      )
    "N" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        iv.try_set(board, position, Occupied(Knight, White)),
      )
    "R" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        iv.try_set(board, position, Occupied(Rook, White)),
      )
    "P" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        iv.try_set(board, position, Occupied(Pawn, White)),
      )
    "k" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        iv.try_set(board, position, Occupied(King, Black)),
      )
    "q" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        iv.try_set(board, position, Occupied(Queen, Black)),
      )
    "b" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        iv.try_set(board, position, Occupied(Bishop, Black)),
      )
    "n" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        iv.try_set(board, position, Occupied(Knight, Black)),
      )
    "r" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        iv.try_set(board, position, Occupied(Rook, Black)),
      )
    "p" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        iv.try_set(board, position, Occupied(Pawn, Black)),
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
  board: iv.Array(Square),
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

  case iv.get(board, position) {
    Ok(Empty) -> do_to_fen(board, file + 1, rank, empty + 1, fen)
    Ok(Occupied(piece:, colour:)) -> {
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
    Error(Nil) -> maybe_add_empty(fen, empty)
  }
}

fn maybe_add_empty(fen: String, empty: Int) -> String {
  case empty {
    0 -> fen
    _ -> fen <> int.to_string(empty)
  }
}

pub fn initial_position() -> Board {
  iv.from_list(initial_squares)
}

const initial_squares = [
  Occupied(Rook, White),
  Occupied(Knight, White),
  Occupied(Bishop, White),
  Occupied(Queen, White),
  Occupied(King, White),
  Occupied(Bishop, White),
  Occupied(Knight, White),
  Occupied(Rook, White),
  Occupied(Pawn, White),
  Occupied(Pawn, White),
  Occupied(Pawn, White),
  Occupied(Pawn, White),
  Occupied(Pawn, White),
  Occupied(Pawn, White),
  Occupied(Pawn, White),
  Occupied(Pawn, White),
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Occupied(Pawn, Black),
  Occupied(Pawn, Black),
  Occupied(Pawn, Black),
  Occupied(Pawn, Black),
  Occupied(Pawn, Black),
  Occupied(Pawn, Black),
  Occupied(Pawn, Black),
  Occupied(Pawn, Black),
  Occupied(Rook, Black),
  Occupied(Knight, Black),
  Occupied(Bishop, Black),
  Occupied(Queen, Black),
  Occupied(King, Black),
  Occupied(Bishop, Black),
  Occupied(Knight, Black),
  Occupied(Rook, Black),
]
