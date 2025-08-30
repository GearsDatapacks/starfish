import gleam/bool
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string
import starfish/internal/board.{Black, White}
import starfish/internal/hash
import starfish/internal/move/attack
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
    previous_positions: Set(Int),
    attack_information: attack.AttackInformation,
  )
}

const all_castling = Castling(True, True, True, True)

pub fn initial_position() -> Game {
  let to_move = White
  let hash_data = hash.generate_data()
  let piece_tables = piece_table.construct_tables()
  let board = board.initial_position()
  let zobrist_hash = hash.hash(hash_data, board, to_move)
  let attack_information = attack.calculate(board, to_move)

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
    previous_positions: set.new(),
    attack_information:,
  )
}

pub fn from_fen(fen: String) -> Game {
  let fen = strip_spaces(fen)

  let #(board, fen, _) = board.from_fen(fen)
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
  let piece_tables = piece_table.construct_tables()
  let zobrist_hash = hash.hash(hash_data, board, to_move)
  let attack_information = attack.calculate(board, to_move)

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
    previous_positions: set.new(),
    attack_information:,
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

  Ok(#(board.position(file:, rank:), fen))
}

fn parse_castling(fen: String) -> #(Castling, String) {
  let castling = Castling(False, False, False, False)
  case fen {
    "-" <> fen -> #(castling, fen)
    _ -> parse_castling_loop(fen, castling, False)
  }
}

fn parse_castling_loop(
  fen: String,
  castling: Castling,
  parsed_any: Bool,
) -> #(Castling, String) {
  case fen {
    "K" <> fen ->
      parse_castling_loop(fen, Castling(..castling, white_kingside: True), True)
    "k" <> fen ->
      parse_castling_loop(fen, Castling(..castling, black_kingside: True), True)
    "Q" <> fen ->
      parse_castling_loop(
        fen,
        Castling(..castling, white_queenside: True),
        True,
      )
    "q" <> fen ->
      parse_castling_loop(
        fen,
        Castling(..castling, black_queenside: True),
        True,
      )
    _ if parsed_any -> #(castling, fen)
    _ -> #(all_castling, fen)
  }
}

fn strip_spaces(fen: String) -> String {
  case fen {
    " " <> fen -> strip_spaces(fen)
    _ -> fen
  }
}

pub type FenParseError {
  PiecePositionsIncomplete
  ExpectedActiveColour
  ExpectedSpaceAfterSegment
  TrailingData(String)
  ExpectedEnPassantPosition
  ExpectedHalfMoveCount
  ExpectedFullMoveCount
  DuplicateCastlingIndicator
}

pub fn try_from_fen(fen: String) -> Result(Game, FenParseError) {
  let fen = strip_spaces(fen)

  let #(board, fen, completed) = board.from_fen(fen)
  use <- bool.guard(!completed, Error(PiecePositionsIncomplete))
  use fen <- result.try(expect_spaces(fen))

  use #(to_move, fen) <- result.try(case fen {
    "w" <> fen | "W" <> fen -> Ok(#(White, fen))
    "b" <> fen | "B" <> fen -> Ok(#(Black, fen))
    _ -> Error(ExpectedActiveColour)
  })
  use fen <- result.try(expect_spaces(fen))

  use #(castling, fen) <- result.try(try_parse_castling(fen))
  use fen <- result.try(expect_spaces(fen))

  use #(en_passant_square, fen) <- result.try(case fen {
    "-" <> fen -> Ok(#(None, fen))
    _ -> {
      case parse_position(fen) {
        Ok(#(position, fen)) -> Ok(#(Some(position), fen))
        Error(Nil) -> Error(ExpectedEnPassantPosition)
      }
    }
  })
  use fen <- result.try(expect_spaces(fen))

  use #(half_moves, fen) <- result.try(
    parse_int(fen) |> result.replace_error(ExpectedHalfMoveCount),
  )
  use fen <- result.try(expect_spaces(fen))

  use #(full_moves, fen) <- result.try(
    parse_int(fen) |> result.replace_error(ExpectedFullMoveCount),
  )

  let fen = strip_spaces(fen)
  use <- bool.guard(fen != "", Error(TrailingData(fen)))

  let hash_data = hash.generate_data()
  let piece_tables = piece_table.construct_tables()
  let zobrist_hash = hash.hash(hash_data, board, to_move)
  let attack_information = attack.calculate(board, to_move)

  Ok(Game(
    board:,
    to_move:,
    castling:,
    en_passant_square:,
    half_moves:,
    full_moves:,
    zobrist_hash:,
    hash_data:,
    piece_tables:,
    previous_positions: set.new(),
    attack_information:,
  ))
}

fn expect_spaces(fen: String) -> Result(String, FenParseError) {
  case fen {
    " " <> fen -> Ok(strip_spaces(fen))
    _ -> Error(ExpectedSpaceAfterSegment)
  }
}

fn try_parse_castling(fen: String) -> Result(#(Castling, String), FenParseError) {
  let castling = Castling(False, False, False, False)
  case fen {
    "-" <> fen -> Ok(#(castling, fen))
    _ -> try_parse_castling_loop(fen, castling)
  }
}

fn try_parse_castling_loop(
  fen: String,
  castling: Castling,
) -> Result(#(Castling, String), FenParseError) {
  case fen {
    "K" <> _ if castling.white_kingside -> Error(DuplicateCastlingIndicator)
    "k" <> _ if castling.black_kingside -> Error(DuplicateCastlingIndicator)
    "Q" <> _ if castling.white_queenside -> Error(DuplicateCastlingIndicator)
    "q" <> _ if castling.black_queenside -> Error(DuplicateCastlingIndicator)

    "K" <> fen ->
      try_parse_castling_loop(fen, Castling(..castling, white_kingside: True))
    "k" <> fen ->
      try_parse_castling_loop(fen, Castling(..castling, black_kingside: True))
    "Q" <> fen ->
      try_parse_castling_loop(fen, Castling(..castling, white_queenside: True))
    "q" <> fen ->
      try_parse_castling_loop(fen, Castling(..castling, black_queenside: True))
    _ -> Ok(#(castling, fen))
  }
}

pub fn to_fen(game: Game) -> String {
  let board_fen = board.to_fen(game.board)

  let active_colour = case game.to_move {
    Black -> "b"
    White -> "w"
  }
  let castling = castling_to_string(game.castling)
  let en_passant = case game.en_passant_square {
    None -> "-"
    Some(position) -> position_to_string(position)
  }
  let half_moves = int.to_string(game.half_moves)
  let full_moves = int.to_string(game.full_moves)

  string.join(
    [board_fen, active_colour, castling, en_passant, half_moves, full_moves],
    " ",
  )
}

fn position_to_string(position: Int) -> String {
  // Add one, because file/rank is zero-indexed
  let rank = int.to_string(board.rank(position) + 1)

  let file = case board.file(position) {
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

fn castling_to_string(castling: Castling) -> String {
  let string = case castling.white_kingside {
    True -> "K"
    False -> ""
  }
  let string = case castling.white_queenside {
    True -> string <> "Q"
    False -> string
  }
  let string = case castling.black_kingside {
    True -> string <> "k"
    False -> string
  }
  let string = case castling.black_queenside {
    True -> string <> "q"
    False -> string
  }

  case string {
    "" -> "-"
    _ -> string
  }
}
