import gleam/int
import gleam/io
import gleam/list
import gleeunit
import pocket_watch
import starfish
import starfish/internal/game

pub fn main() -> Nil {
  gleeunit.main()
}

// Ignore `HashData` and `PieceTables` as they are not relevant to the state of
// the board.
fn game_equal(a: game.Game, b: game.Game) -> Bool {
  a.board == b.board
  && a.to_move == b.to_move
  && a.castling == b.castling
  && a.en_passant_square == b.en_passant_square
  && a.half_moves == b.half_moves
  && a.full_moves == b.full_moves
}

pub fn from_fen_test() {
  let initial = starfish.new()
  let parsed = starfish.from_fen(starfish.starting_fen)
  assert game_equal(initial, parsed)

  let initial_with_only_position = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
  let parsed = starfish.from_fen(initial_with_only_position)
  assert game_equal(initial, parsed)
}

pub fn try_from_fen_test() {
  let initial = starfish.new()
  let assert Ok(parsed) = starfish.try_from_fen(starfish.starting_fen)
  assert game_equal(parsed, initial)

  let initial_with_only_position = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
  let assert Error(error) = starfish.try_from_fen(initial_with_only_position)
  assert error == starfish.ExpectedSpaceAfterSegment
}

pub fn to_fen_test() {
  let fen = game.to_fen(starfish.new())
  assert fen == starfish.starting_fen

  let fen = "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1"
  assert fen |> starfish.from_fen |> starfish.to_fen == fen
}

fn perft_all(fen: String, expected: List(Int)) {
  expected
  |> list.index_map(fn(expected, index) { perft(fen, index + 1, expected) })
  Nil
}

fn perft(fen: String, depth: Int, expected_moves: Int) {
  assert do_perft(game.from_fen(fen), depth - 1) == expected_moves
}

fn do_perft(game: game.Game, depth: Int) -> Int {
  let legal_moves = starfish.legal_moves(game)
  case depth {
    0 -> list.length(legal_moves)
    _ ->
      list.map(legal_moves, fn(move) {
        game
        |> starfish.apply(move)
        |> do_perft(depth - 1)
      })
      |> int.sum
  }
}

pub opaque type TestOptions(a) {
  Timeout(timeout: Int, function: fn() -> a)
}

fn print_time(name: String, time: String) -> Nil {
  io.println_error("\nPerft test of " <> name <> " took " <> time <> ".")
}

pub fn perft_initial_position_test_() {
  use <- Timeout(1_000_000)
  use <- pocket_watch.callback("initial position", print_time)
  perft_all(starfish.starting_fen, [
    20,
    // 400, 8902, 197_281, 4_865_609, 119_060_324,
  ])
}
