import gleam/result
import starfish/internal/board

pub type Game =
  board.Game

pub const starting_fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

pub fn new(fen: String) -> Result(Game, Nil) {
  todo
}

pub fn initial_position() -> Game {
  todo
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
