import gleam/result
import starfish/internal/game

pub type Game =
  game.Game

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
/// ```
/// starfish.new()
/// starfish.from_fen(starfish.starting_fen)
/// // Here, we provide the board position and the rest of the information is
/// // filled in.
/// starfish.from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
/// ```
pub fn from_fen(fen: String) -> Game {
  game.from_fen(fen)
}

pub type FenParseError {
  /// The field specifying the positions of piece on the board is incomplete and
  /// doesn't cover every square on the chessboard. For example, in the string
  /// `rnbqkbnr/8/8/8/8/RNBQKBNR w - - 0 1`, only 6 ranks are specified, which
  /// would cause this error.
  PiecePositionsIncomplete
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
/// ```
/// let assert Ok(start_pos) = starfish.try_from_fen(starfish.starting_fen)
/// assert start_pos == starfish.new()
/// 
/// let assert Error(starfish.ExpectedSpaceAfterSegment) =
///   starfish.try_from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
/// ```
pub fn try_from_fen(fen: String) -> Result(Game, FenParseError) {
  result.map_error(game.try_from_fen(fen), convert_fen_parse_error)
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
  }
}

/// Returns a game representing the initial position.
pub fn new() -> Game {
  game.initial_position()
}

pub type Valid

pub type Legal

pub type Move(validity)

pub fn legal_moves(game: Game) -> List(Move(Legal)) {
  todo
}

pub fn search(game: Game, to_depth depth: Int) -> Result(Move(Legal), Nil) {
  todo
}

pub fn apply(game: Game, move: Move(Legal)) -> Game {
  todo
}

pub fn to_fen(game: Game) -> String {
  todo
}

pub fn to_standard_algebraic_notation(move: Move(a)) -> String {
  todo
}

pub fn to_long_algebraic_notation(move: Move(a)) -> String {
  todo
}

pub fn parse_move(string: String) -> Result(Move(Valid), Nil) {
  todo
}

pub fn parse_legal_move(string: String, game: Game) -> Result(Move(Legal), Nil) {
  string |> parse_move |> result.try(validate_move(_, game))
}

pub fn validate_move(move: Move(a), game: Game) -> Result(Move(Legal), Nil) {
  todo
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

pub fn state(game: Game) -> GameState {
  todo
}
