import gleam/dict.{type Dict}
import gleam/option.{type Option, None, Some}
import gleam/result
import starfish/internal/board.{Black, White}
import starfish/internal/hash
import starfish/internal/piece_table

pub type Castling {
  Castling(
    white_kingside: Bool,
    white_queenside: Bool,
    black_kingside: Bool,
    black_queenside: Bool,
  )
}

pub type Game {
  Game(
    // Game state
    board: board.Board,
    to_move: board.Colour,
    castling: Castling,
    en_passant_square: Option(Int),
    half_moves: Int,
    full_moves: Int,
    // Extra information
    zobrist_hash: Int,
    hash_data: hash.HashData,
    piece_tables: piece_table.PieceTables,
    previous_positions: Dict(Int, Int),
  )
}

const all_castling = Castling(True, True, True, True)

pub fn initial_position() -> Game {
  let to_move = White
  let hash_data = hash.generate_data()
  let piece_tables = piece_table.make_tables()
  let board = board.initial_position()
  let zobrist_hash = hash.hash(hash_data, board, to_move)

  Game(
    board:,
    to_move:,
    castling: all_castling,
    en_passant_square: None,
    half_moves: 0,
    full_moves: 1,
    zobrist_hash:,
    hash_data:,
    piece_tables:,
    previous_positions: dict.new(),
  )
}

pub fn from_fen(fen: String) -> Game {
  let fen = strip_spaces(fen)

  let #(board, fen) = board.from_fen(fen)
  let fen = strip_spaces(fen)

  let #(to_move, fen) = case fen {
    "w" <> fen | "W" <> fen -> #(White, fen)
    "b" <> fen | "B" <> fen -> #(Black, fen)
    _ -> #(White, fen)
  }
  let fen = strip_spaces(fen)

  let #(castling, fen) = parse_castling(fen)
  let fen = strip_spaces(fen)

  let #(en_passant_square, fen) = case fen {
    "-" <> fen -> #(None, fen)
    _ -> {
      case parse_position(fen) {
        Ok(#(position, fen)) -> #(Some(position), fen)
        Error(_) -> #(None, fen)
      }
    }
  }
  let fen = strip_spaces(fen)

  let #(half_moves, fen) = case parse_int(fen) {
    Error(Nil) -> #(0, fen)
    Ok(pair) -> pair
  }
  let fen = strip_spaces(fen)

  let full_moves = case parse_int(fen) {
    Error(Nil) -> 1
    Ok(#(moves, _)) -> moves
  }

  let hash_data = hash.generate_data()
  let piece_tables = piece_table.make_tables()
  let zobrist_hash = hash.hash(hash_data, board, to_move)

  Game(
    board:,
    to_move:,
    castling:,
    en_passant_square:,
    half_moves:,
    full_moves:,
    zobrist_hash:,
    hash_data:,
    piece_tables:,
    previous_positions: dict.new(),
  )
}

fn parse_int(fen: String) -> Result(#(Int, String), Nil) {
  case fen {
    "0" <> _
    | "1" <> _
    | "2" <> _
    | "3" <> _
    | "4" <> _
    | "5" <> _
    | "6" <> _
    | "7" <> _
    | "8" <> _
    | "9" <> _ -> Ok(do_parse_int(fen, 0))
    _ -> Error(Nil)
  }
}

fn do_parse_int(fen: String, parsed: Int) -> #(Int, String) {
  case fen {
    "0" <> fen -> do_parse_int(fen, parsed * 10)
    "1" <> fen -> do_parse_int(fen, parsed * 10 + 1)
    "2" <> fen -> do_parse_int(fen, parsed * 10 + 2)
    "3" <> fen -> do_parse_int(fen, parsed * 10 + 3)
    "4" <> fen -> do_parse_int(fen, parsed * 10 + 4)
    "5" <> fen -> do_parse_int(fen, parsed * 10 + 5)
    "6" <> fen -> do_parse_int(fen, parsed * 10 + 6)
    "7" <> fen -> do_parse_int(fen, parsed * 10 + 7)
    "8" <> fen -> do_parse_int(fen, parsed * 10 + 8)
    "9" <> fen -> do_parse_int(fen, parsed * 10 + 9)
    _ -> #(parsed, fen)
  }
}

fn parse_position(fen: String) -> Result(#(Int, String), Nil) {
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
    "0" <> fen -> Ok(#(0, fen))
    "1" <> fen -> Ok(#(1, fen))
    "2" <> fen -> Ok(#(2, fen))
    "3" <> fen -> Ok(#(3, fen))
    "4" <> fen -> Ok(#(4, fen))
    "5" <> fen -> Ok(#(5, fen))
    "6" <> fen -> Ok(#(6, fen))
    "7" <> fen -> Ok(#(7, fen))
    _ -> Error(Nil)
  })

  Ok(#(board.position(file:, rank:), fen))
}

fn parse_castling(fen: String) -> #(Castling, String) {
  let castling = Castling(False, False, False, False)
  case fen {
    "-" <> fen -> #(castling, fen)
    _ -> parse_castling_loop(fen, castling)
  }
}

fn parse_castling_loop(fen: String, castling: Castling) -> #(Castling, String) {
  case fen {
    "K" <> fen ->
      parse_castling_loop(fen, Castling(..castling, white_kingside: True))
    "k" <> fen ->
      parse_castling_loop(fen, Castling(..castling, black_kingside: True))
    "Q" <> fen ->
      parse_castling_loop(fen, Castling(..castling, white_queenside: True))
    "q" <> fen ->
      parse_castling_loop(fen, Castling(..castling, black_queenside: True))
    _ -> #(castling, fen)
  }
}

fn strip_spaces(fen: String) -> String {
  case fen {
    " " <> fen -> strip_spaces(fen)
    _ -> fen
  }
}
