import birl
import gleam/bool
import gleam/list
import gleam/result
import starfish/internal/board
import starfish/internal/game
import starfish/internal/move
import starfish/internal/search

pub opaque type Game {
  Game(game: game.Game)
}

/// A single legal move on the chess board.
pub opaque type Move {
  Move(move: move.Move)
}

@internal
pub fn get_move(move: Move) -> move.Move {
  move.move
}

/// The [FEN string](https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation)
/// representing the initial position of a chess game.
pub const starting_fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

/// Parses a game from a FEN string. This function does a best-effort parsing of
/// the input, meaning if a FEN string is partially incomplete (e.g. missing the
/// half-move and full-move counters at the end), it will fill it in with the
/// default values of the starting position.
///
/// For strict parsing, see [`try_from_fen`](#try_from_fen).
///
/// ## Examples
///
/// The following expressions are all equivalent:
///
/// ```gleam
/// starfish.new()
/// starfish.from_fen(starfish.starting_fen)
/// // Here, we provide the board position and the rest of the information is
/// // filled in.
/// starfish.from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
/// ```
pub fn from_fen(fen: String) -> Game {
  Game(game.from_fen(fen))
}

pub type FenParseError {
  /// The field specifying the positions of piece on the board is incomplete and
  /// doesn't cover every square on the chessboard. For example, in the string
  /// `rnbqkbnr/8/8/8/8/RNBQKBNR w - - 0 1`, only 6 ranks are specified, which
  /// would cause this error.
  PiecePositionsIncomplete
  /// The board is missing the white king, meaning the position is invalid.
  MissingWhiteKing
  /// The board is missing the black king, meaning the position is invalid.
  MissingBlackKing
  /// The field specifying which player's turn is next is wrong or missing. For
  /// example in the string `8/8/8/8/8/8/8/8 - - 0 1`, the active colour specifier
  /// is missing.
  ExpectedActiveColour
  /// If a segment is not followed by a space. For example in the string:
  /// `8/8/8/8/8/8/8/8w--01`
  ExpectedSpaceAfterSegment
  /// After the FEN string is successfully parsed, there is extra data at the
  /// end of the string (whitespace doesn't count).
  TrailingData(String)
  /// The field specifying the en passant square is missing.
  ExpectedEnPassantPosition
  /// The field specifying the half-move count is missing.
  ExpectedHalfMoveCount
  /// The field specifying the full-move count is missing.
  ExpectedFullMoveCount
  /// When specifying castling rights, one of the characters is duplicated. For
  /// example, in the string `8/8/8/8/8/8/8/8 w KKKQQQkkqq - 0 1`.
  DuplicateCastlingIndicator
}

/// Tries to parse a game from a FEN string, returning an error if it doesn't
/// follow standard FEN notation. For more lenient parsing, see [`from_fen`](
/// #from_fen).
///
/// ## Examples
///
/// ```gleam
/// let assert Ok(start_pos) = starfish.try_from_fen(starfish.starting_fen)
/// assert start_pos == starfish.new()
///
/// let assert Error(starfish.ExpectedSpaceAfterSegment) =
///   starfish.try_from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
/// ```
pub fn try_from_fen(fen: String) -> Result(Game, FenParseError) {
  game.try_from_fen(fen)
  |> result.map_error(convert_fen_parse_error)
  |> result.map(Game)
}

// Since circular imports are not allowed, but we want the user to be able to
// pattern match on the type from the internal module, we convert it here to an
// identical type which is part of the public API.
fn convert_fen_parse_error(error: game.FenParseError) -> FenParseError {
  case error {
    game.DuplicateCastlingIndicator -> DuplicateCastlingIndicator
    game.ExpectedActiveColour -> ExpectedActiveColour
    game.ExpectedEnPassantPosition -> ExpectedEnPassantPosition
    game.ExpectedFullMoveCount -> ExpectedFullMoveCount
    game.ExpectedHalfMoveCount -> ExpectedHalfMoveCount
    game.ExpectedSpaceAfterSegment -> ExpectedSpaceAfterSegment
    game.PiecePositionsIncomplete -> PiecePositionsIncomplete
    game.TrailingData(value) -> TrailingData(value)
    game.MissingBlackKing -> MissingBlackKing
    game.MissingWhiteKing -> MissingWhiteKing
  }
}

/// Returns a game representing the initial position.
pub fn new() -> Game {
  Game(game.initial_position())
}

/// Convert a game into its FEN string representation.
///
/// ## Examples
///
/// ```gleam
/// assert starfish.to_fen(starfish.new()) == starfish.starting_fen
/// ```
pub fn to_fen(game: Game) -> String {
  game.to_fen(game.game)
}

pub fn legal_moves(game: Game) -> List(Move) {
  list.map(move.legal(game.game), Move)
}

/// Used to determine how long to search positions
pub type SearchCutoff {
  /// Search to a specific depth
  Depth(depth: Int)
  /// Search for a given number of milliseconds.
  /// 
  /// NOTE: The process will usually take slightly longer than the specified time.
  /// It would be expensive to check the time every millisecond, so it is checked
  /// periodically. This is usually less than 10ms, but it can be higher than that.
  Time(milliseconds: Int)
}

/// Finds the best move for a given position, or returns an error if no moves are
/// legal (If it's checkmate or stalemate)
pub fn search(game: Game, until cutoff: SearchCutoff) -> Result(Move, Nil) {
  let until = case cutoff {
    Depth(depth:) -> fn(current_depth) { current_depth > depth }
    Time(milliseconds:) -> {
      let end_time = birl.monotonic_now() + milliseconds * 1000
      fn(_) { birl.monotonic_now() >= end_time }
    }
  }

  result.map(search.best_move(game.game, until), Move)
}

pub fn apply_move(game: Game, move: Move) -> Game {
  Game(move.apply(game.game, move.move))
}

pub fn to_standard_algebraic_notation(move: Move, game: Game) -> String {
  move.to_standard_algebraic_notation(move.move, game.game)
}

/// Convert a move to [long algebraic notation](
/// https://en.wikipedia.org/wiki/Algebraic_notation_(chess)#Long_algebraic_notation),
/// specifically the UCI format, containing the start and end positions. For
/// example, `e2e4` or `c7d8q`.
pub fn to_long_algebraic_notation(move: Move) -> String {
  move.to_long_algebraic_notation(move.move)
}

/// Parses a move from either long algebraic notation, in the same format as
/// [`to_long_algebraic_notation`](#to_long_algebraic_notation), or from [Standard
/// Algebraic Notation](https://en.wikipedia.org/wiki/Algebraic_notation_(chess)).
/// Returns an error if the syntax is invalid or the move is not legal on the
/// board.
pub fn parse_move(move: String, game: Game) -> Result(Move, Nil) {
  let legal_moves = move.legal(game.game)
  case move.from_long_algebraic_notation(move, legal_moves) {
    Ok(move) -> Ok(Move(move))
    Error(_) ->
      move.from_standard_algebraic_notation(move, game.game, legal_moves)
      |> result.map(Move)
  }
}

pub type GameState {
  Continue
  Draw(DrawReason)
  WhiteWin
  BlackWin
}

pub type DrawReason {
  ThreefoldRepetition
  InsufficientMaterial
  Stalemate
  FiftyMoves
}

/// Returns the current game state: A win, draw or neither.
pub fn state(game: Game) -> GameState {
  let game = game.game
  use <- bool.guard(game.half_moves >= 50, Draw(FiftyMoves))
  use <- bool.guard(
    game.is_insufficient_material(game),
    Draw(InsufficientMaterial),
  )
  use <- bool.guard(
    game.is_threefold_repetition(game),
    Draw(ThreefoldRepetition),
  )
  use <- bool.guard(move.any_legal(game), Continue)
  use <- bool.guard(!game.attack_information.in_check, Draw(Stalemate))
  case game.to_move {
    board.Black -> WhiteWin
    board.White -> BlackWin
  }
}
