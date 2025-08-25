import gleam/result
import starfish/internal/game

pub type Game =
  game.Game

pub const starting_fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

/// Parses a game from a FEN string. This function does a best-effort parsing of
/// the input, meaning if a FEN string is partially incomplete (e.g. missing the
/// half-move and full-move counters at the end), it will fill it in with the
/// default values of the starting position. For example, the following
/// expressions are all equivalent:
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
