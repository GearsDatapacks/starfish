import gleam/bool
import gleam/dict
import gleam/int
import gleam/option
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

pub fn piece_value(piece: Piece) -> Int {
  case piece {
    Pawn -> 100
    Knight -> 300
    Bishop -> 300
    Rook -> 500
    Queen -> 900
    King -> 1000
  }
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

pub type FenParseResult {
  FenParseResult(
    board: Board,
    remaining: String,
    board_is_complete: Bool,
    white_king_position: option.Option(Int),
    black_king_position: option.Option(Int),
  )
}

pub fn from_fen(fen: String) -> FenParseResult {
  // FEN starts from black's size, which means that `rank` needs to start at the
  // end of the board.
  from_fen_loop(fen, 0, side_length - 1, dict.new(), option.None, option.None)
}

fn from_fen_loop(
  fen: String,
  file: Int,
  rank: Int,
  board: Board,
  white_king_position: option.Option(Int),
  black_king_position: option.Option(Int),
) -> FenParseResult {
  let position = position(file:, rank:)

  case fen {
    // When we hit a `/`, we start from file 0 again, and move down the rank,
    // since we are starting from black and ending on white.
    "/" <> fen ->
      from_fen_loop(
        fen,
        0,
        rank - 1,
        board,
        white_king_position,
        black_king_position,
      )
    "0" <> fen ->
      from_fen_loop(
        fen,
        file,
        rank,
        board,
        white_king_position,
        black_king_position,
      )
    "1" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        board,
        white_king_position,
        black_king_position,
      )
    "2" <> fen ->
      from_fen_loop(
        fen,
        file + 2,
        rank,
        board,
        white_king_position,
        black_king_position,
      )
    "3" <> fen ->
      from_fen_loop(
        fen,
        file + 3,
        rank,
        board,
        white_king_position,
        black_king_position,
      )
    "4" <> fen ->
      from_fen_loop(
        fen,
        file + 4,
        rank,
        board,
        white_king_position,
        black_king_position,
      )
    "5" <> fen ->
      from_fen_loop(
        fen,
        file + 5,
        rank,
        board,
        white_king_position,
        black_king_position,
      )
    "6" <> fen ->
      from_fen_loop(
        fen,
        file + 6,
        rank,
        board,
        white_king_position,
        black_king_position,
      )
    "7" <> fen ->
      from_fen_loop(
        fen,
        file + 7,
        rank,
        board,
        white_king_position,
        black_king_position,
      )
    "8" <> fen ->
      from_fen_loop(
        fen,
        file + 8,
        rank,
        board,
        white_king_position,
        black_king_position,
      )
    "9" <> fen ->
      from_fen_loop(
        fen,
        file + 9,
        rank,
        board,
        white_king_position,
        black_king_position,
      )
    "K" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        dict.insert(board, position, #(King, White)),
        option.Some(position),
        black_king_position,
      )
    "Q" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        dict.insert(board, position, #(Queen, White)),
        white_king_position,
        black_king_position,
      )
    "B" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        dict.insert(board, position, #(Bishop, White)),
        white_king_position,
        black_king_position,
      )
    "N" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        dict.insert(board, position, #(Knight, White)),
        white_king_position,
        black_king_position,
      )
    "R" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        dict.insert(board, position, #(Rook, White)),
        white_king_position,
        black_king_position,
      )
    "P" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        dict.insert(board, position, #(Pawn, White)),
        white_king_position,
        black_king_position,
      )
    "k" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        dict.insert(board, position, #(King, Black)),
        white_king_position,
        option.Some(position),
      )
    "q" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        dict.insert(board, position, #(Queen, Black)),
        white_king_position,
        black_king_position,
      )
    "b" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        dict.insert(board, position, #(Bishop, Black)),
        white_king_position,
        black_king_position,
      )
    "n" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        dict.insert(board, position, #(Knight, Black)),
        white_king_position,
        black_king_position,
      )
    "r" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        dict.insert(board, position, #(Rook, Black)),
        white_king_position,
        black_king_position,
      )
    "p" <> fen ->
      from_fen_loop(
        fen,
        file + 1,
        rank,
        dict.insert(board, position, #(Pawn, Black)),
        white_king_position,
        black_king_position,
      )
    // Since we iterate the rank in reverse order, but we iterate the file in
    // ascending order, the final position should equal to `side_length`
    _ ->
      FenParseResult(
        board:,
        remaining: fen,
        board_is_complete: position == side_length,
        white_king_position:,
        black_king_position:,
      )
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
