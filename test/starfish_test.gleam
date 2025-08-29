import gleam/int
import gleam/io
import gleam/list
import gleeunit
import pocket_watch
import starfish
import starfish/internal/board
import starfish/internal/game
import starfish/internal/move

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
        |> starfish.apply_move(move)
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

fn test_apply_move(
  starting_fen: String,
  moves: List(move.Move(move.Legal)),
  expected_fen: String,
) {
  let final_fen =
    starting_fen
    |> game.from_fen
    |> list.fold(moves, _, starfish.apply_move)
    |> game.to_fen

  assert final_fen == expected_fen
}

pub fn apply_move_test() {
  test_apply_move(
    starfish.starting_fen,
    // a2a4
    [move.Move(from: 8, to: 24)],
    "rnbqkbnr/pppppppp/8/8/P7/8/1PPPPPPP/RNBQKBNR b KQkq a3 0 1",
  )

  test_apply_move(
    starfish.starting_fen,
    // g1f3
    [move.Move(from: 6, to: 21)],
    "rnbqkbnr/pppppppp/8/8/8/5N2/PPPPPPPP/RNBQKB1R b KQkq - 1 1",
  )

  test_apply_move(
    starfish.starting_fen,
    // a2a4, b8c6
    [move.Move(from: 8, to: 24), move.Move(from: 57, to: 42)],
    "r1bqkbnr/pppppppp/2n5/8/P7/8/1PPPPPPP/RNBQKBNR w KQkq - 1 2",
  )

  test_apply_move(
    starfish.starting_fen,
    // a2a4, b7b5
    [move.Move(from: 8, to: 24), move.Move(from: 49, to: 33)],
    "rnbqkbnr/p1pppppp/8/1p6/P7/8/1PPPPPPP/RNBQKBNR w KQkq b6 0 2",
  )

  test_apply_move(
    "rnbqkbnr/p1p1pppp/8/Pp1p4/8/8/1PPPPPPP/RNBQKBNR w KQkq b6 0 3",
    // a5b6
    [move.EnPassant(from: 32, to: 41)],
    "rnbqkbnr/p1p1pppp/1P6/3p4/8/8/1PPPPPPP/RNBQKBNR b KQkq - 0 3",
  )

  test_apply_move(
    "rnbqkbnr/pp3ppp/8/2ppp3/4P3/5N2/PPPPBPPP/RNBQK2R w KQkq - 0 4",
    // O-O
    [move.Castle(from: 4, to: 6)],
    "rnbqkbnr/pp3ppp/8/2ppp3/4P3/5N2/PPPPBPPP/RNBQ1RK1 b kq - 1 4",
  )

  test_apply_move(
    "rnbqkbnr/ppp2ppp/8/3pp3/3P4/2N1B3/PPPQPPPP/R3KBNR w KQkq - 0 5",
    // O-O-O
    [move.Castle(from: 4, to: 2)],
    "rnbqkbnr/ppp2ppp/8/3pp3/3P4/2N1B3/PPPQPPPP/2KR1BNR b kq - 1 5",
  )

  test_apply_move(
    "rnbqk2r/ppppbppp/5n2/4p3/2PPP3/8/PP3PPP/RNBQKBNR b KQkq - 0 4",
    // O-O
    [move.Castle(from: 60, to: 62)],
    "rnbq1rk1/ppppbppp/5n2/4p3/2PPP3/8/PP3PPP/RNBQKBNR w KQ - 1 5",
  )

  test_apply_move(
    "r3kbnr/pppqpppp/2n1b3/3p4/3PP3/8/PPP2PPP/RNBQKBNR b KQkq - 0 5",
    // O-O-O
    [move.Castle(from: 60, to: 58)],
    "2kr1bnr/pppqpppp/2n1b3/3p4/3PP3/8/PPP2PPP/RNBQKBNR w KQ - 1 6",
  )

  test_apply_move(
    "rnbq1bnr/pppPkpp1/4p2p/8/8/8/PPPP1PPP/RNBQKBNR w KQ - 1 5",
    // d7c8q
    [move.Promotion(from: 51, to: 58, piece: board.Queen)],
    "rnQq1bnr/ppp1kpp1/4p2p/8/8/8/PPPP1PPP/RNBQKBNR b KQ - 0 5",
  )

  test_apply_move(
    "rnbqkbnr/pppp1ppp/8/8/8/4P2P/PPPpKPP1/RNBQ1BNR b kq - 1 5",
    // d2c1n
    [move.Promotion(from: 11, to: 2, piece: board.Knight)],
    "rnbqkbnr/pppp1ppp/8/8/8/4P2P/PPP1KPP1/RNnQ1BNR w kq - 0 6",
  )
}
