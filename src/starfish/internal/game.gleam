import gleam/bool
import gleam/dict
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/result
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
    previous_positions: List(Int),
    attack_information: attack.AttackInformation,
    black_pieces: PieceInfo,
    white_pieces: PieceInfo,
  )
}

pub type PieceInfo {
  PieceInfo(
    king_position: Int,
    non_pawn_material: Int,
    pawn_material: Int,
    piece_square_score: Int,
  )
}

const all_castling = Castling(True, True, True, True)

pub fn initial_position() -> Game {
  let to_move = White
  let board = board.initial_position()
  let zobrist_hash = hash.hash(board, to_move)

  let white_king_position = 4
  let black_king_position = 60

  let non_pawn_material =
    board.queen_value
    + board.rook_value
    * 2
    + board.bishop_value
    * 2
    + board.knight_value
    * 2

  let pawn_material = board.pawn_value * 8

  let attack_information = attack.calculate(board, white_king_position, to_move)

  let phase = phase(non_pawn_material, non_pawn_material)
  let #(white_piece_scores, black_piece_scores) =
    piece_square_scores(board, phase)

  Game(
    board:,
    to_move:,
    castling: all_castling,
    en_passant_square: None,
    half_moves: 0,
    full_moves: 1,
    zobrist_hash:,
    previous_positions: [],
    attack_information:,
    white_pieces: PieceInfo(
      king_position: white_king_position,
      non_pawn_material:,
      pawn_material:,
      piece_square_score: white_piece_scores,
    ),
    black_pieces: PieceInfo(
      king_position: black_king_position,
      non_pawn_material:,
      pawn_material:,
      piece_square_score: black_piece_scores,
    ),
  )
}

fn piece_square_scores(board: board.Board, phase: Int) -> #(Int, Int) {
  use #(white_score, black_score), position, #(piece, colour) <- dict.fold(
    board,
    #(0, 0),
  )
  let score = piece_table.piece_score(piece, colour, position, phase)
  case colour {
    Black -> #(white_score, black_score + score)
    White -> #(white_score + score, black_score)
  }
}

pub fn from_fen(fen: String) -> Game {
  let fen = strip_spaces(fen)

  let board.FenParseResult(
    board:,
    remaining: fen,
    board_is_complete: _,
    white_king_position:,
    black_king_position:,
    white_pawn_material:,
    black_pawn_material:,
    white_non_pawn_material:,
    black_non_pawn_material:,
  ) = board.from_fen(fen)
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
      case board.parse_position(fen) {
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

  let zobrist_hash = hash.hash(board, to_move)

  let white_king_position = option.unwrap(white_king_position, 0)
  let black_king_position = option.unwrap(black_king_position, 0)

  let king_position = case to_move {
    Black -> black_king_position
    White -> white_king_position
  }
  let attack_information = attack.calculate(board, king_position, to_move)

  let phase = phase(white_non_pawn_material, black_non_pawn_material)
  let #(white_piece_scores, black_piece_scores) =
    piece_square_scores(board, phase)

  Game(
    board:,
    to_move:,
    castling:,
    en_passant_square:,
    half_moves:,
    full_moves:,
    zobrist_hash:,
    previous_positions: [],
    attack_information:,
    white_pieces: PieceInfo(
      king_position: white_king_position,
      non_pawn_material: white_non_pawn_material,
      pawn_material: white_pawn_material,
      piece_square_score: white_piece_scores,
    ),
    black_pieces: PieceInfo(
      king_position: black_king_position,
      non_pawn_material: black_non_pawn_material,
      pawn_material: black_pawn_material,
      piece_square_score: black_piece_scores,
    ),
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
  MissingWhiteKing
  MissingBlackKing
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

  let board.FenParseResult(
    board:,
    remaining: fen,
    board_is_complete:,
    white_king_position:,
    black_king_position:,
    white_pawn_material:,
    black_pawn_material:,
    white_non_pawn_material:,
    black_non_pawn_material:,
  ) = board.from_fen(fen)
  use <- bool.guard(!board_is_complete, Error(PiecePositionsIncomplete))
  use white_king_position <- result.try(option.to_result(
    white_king_position,
    MissingWhiteKing,
  ))
  use black_king_position <- result.try(option.to_result(
    black_king_position,
    MissingBlackKing,
  ))

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
      case board.parse_position(fen) {
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

  let zobrist_hash = hash.hash(board, to_move)

  let king_position = case to_move {
    Black -> black_king_position
    White -> white_king_position
  }

  let attack_information = attack.calculate(board, king_position, to_move)

  let phase = phase(white_non_pawn_material, black_non_pawn_material)
  let #(white_piece_scores, black_piece_scores) =
    piece_square_scores(board, phase)

  Ok(Game(
    board:,
    to_move:,
    castling:,
    en_passant_square:,
    half_moves:,
    full_moves:,
    zobrist_hash:,
    previous_positions: [],
    attack_information:,
    white_pieces: PieceInfo(
      king_position: white_king_position,
      non_pawn_material: white_non_pawn_material,
      pawn_material: white_pawn_material,
      piece_square_score: white_piece_scores,
    ),
    black_pieces: PieceInfo(
      king_position: black_king_position,
      non_pawn_material: black_non_pawn_material,
      pawn_material: black_pawn_material,
      piece_square_score: black_piece_scores,
    ),
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
    Some(position) -> board.position_to_string(position)
  }
  let half_moves = int.to_string(game.half_moves)
  let full_moves = int.to_string(game.full_moves)

  string.join(
    [board_fen, active_colour, castling, en_passant, half_moves, full_moves],
    " ",
  )
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

pub fn is_insufficient_material(game: Game) -> Bool {
  {
    game.black_pieces.pawn_material == 0 && game.white_pieces.pawn_material == 0
  }
  && {
    game.black_pieces.non_pawn_material == board.bishop_value
    || game.black_pieces.non_pawn_material == board.knight_value
    || game.white_pieces.non_pawn_material == board.bishop_value
    || game.white_pieces.non_pawn_material == board.knight_value
  }
}

pub fn is_threefold_repetition(game: Game) -> Bool {
  is_threefold_repetition_loop(
    game.previous_positions,
    game.zobrist_hash,
    False,
  )
}

fn is_threefold_repetition_loop(
  positions: List(Int),
  position: Int,
  found: Bool,
) -> Bool {
  case positions {
    [] -> False
    [first, ..rest] if first == position ->
      case found {
        False -> is_threefold_repetition_loop(rest, position, True)
        True -> True
      }
    [_, ..rest] -> is_threefold_repetition_loop(rest, position, found)
  }
}

const phase_multiplier = 128

/// About queen + rook, so one major piece per side
const endgame_material = 1400

/// Below this material limit, the endgame weight is zero. this is about enough
/// for three minor pieces to be captured.
const middlegame_material = 3000

pub fn phase(white_material: Int, black_material: Int) -> Int {
  let non_pawn_material = white_material + black_material

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
