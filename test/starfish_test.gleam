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

pub fn to_long_algebraic_notation_test() {
  assert move.Move(from: 8, to: 24) |> starfish.to_long_algebraic_notation
    == "a2a4"
  assert move.Move(from: 6, to: 21) |> starfish.to_long_algebraic_notation
    == "g1f3"
  assert move.Move(from: 57, to: 42) |> starfish.to_long_algebraic_notation
    == "b8c6"
  assert move.Move(from: 49, to: 33) |> starfish.to_long_algebraic_notation
    == "b7b5"
  assert move.EnPassant(from: 32, to: 41) |> starfish.to_long_algebraic_notation
    == "a5b6"
  assert move.Castle(from: 4, to: 6) |> starfish.to_long_algebraic_notation
    == "e1g1"
  assert move.Castle(from: 4, to: 2) |> starfish.to_long_algebraic_notation
    == "e1c1"
  assert move.Castle(from: 60, to: 62) |> starfish.to_long_algebraic_notation
    == "e8g8"
  assert move.Castle(from: 60, to: 58) |> starfish.to_long_algebraic_notation
    == "e8c8"
  assert move.Promotion(from: 51, to: 58, piece: board.Queen)
    |> starfish.to_long_algebraic_notation
    == "d7c8q"
  assert move.Promotion(from: 11, to: 2, piece: board.Knight)
    |> starfish.to_long_algebraic_notation
    == "d2c1n"
  assert move.Capture(from: 49, to: 7) |> starfish.to_long_algebraic_notation
    == "b7h1"
}

pub fn parse_long_algebraic_notation_test() {
  let assert Ok(move) = starfish.parse_long_algebraic_notation("a2a4")
  assert move == move.Move(from: 8, to: 24)
  let assert Ok(move) = starfish.parse_long_algebraic_notation("g1f3")
  assert move == move.Move(from: 6, to: 21)
  let assert Ok(move) = starfish.parse_long_algebraic_notation("b8c6")
  assert move == move.Move(from: 57, to: 42)
  let assert Ok(move) = starfish.parse_long_algebraic_notation("B7b5")
  assert move == move.Move(from: 49, to: 33)
  let assert Ok(move) = starfish.parse_long_algebraic_notation("a5b6")
  assert move == move.Move(from: 32, to: 41)
  let assert Ok(move) = starfish.parse_long_algebraic_notation("e1G1")
  assert move == move.Move(from: 4, to: 6)
  let assert Ok(move) = starfish.parse_long_algebraic_notation("e1c1")
  assert move == move.Move(from: 4, to: 2)
  let assert Ok(move) = starfish.parse_long_algebraic_notation("e8g8")
  assert move == move.Move(from: 60, to: 62)
  let assert Ok(move) = starfish.parse_long_algebraic_notation("E8C8")
  assert move == move.Move(from: 60, to: 58)
  let assert Ok(move) = starfish.parse_long_algebraic_notation("d7c8q")
  assert move == move.Promotion(from: 51, to: 58, piece: board.Queen)
  let assert Ok(move) = starfish.parse_long_algebraic_notation("d2c1N")
  assert move == move.Promotion(from: 11, to: 2, piece: board.Knight)
  let assert Ok(move) = starfish.parse_long_algebraic_notation("b7h1")
  assert move == move.Move(from: 49, to: 7)

  let assert Error(Nil) = starfish.parse_long_algebraic_notation("abcd")
  let assert Error(Nil) = starfish.parse_long_algebraic_notation("e2e4extra")
  let assert Error(Nil) = starfish.parse_long_algebraic_notation("e2")
  let assert Error(Nil) = starfish.parse_long_algebraic_notation("Bxe4")
}

pub fn state_test() {
  assert "8/8/8/1q6/1K6/3q4/8/8 w - - 0 1"
    |> starfish.from_fen
    |> starfish.state
    == starfish.BlackWin

  assert "8/1B6/3np3/4k3/8/2B5/5R2/8 b - - 0 1"
    |> starfish.from_fen
    |> starfish.state
    == starfish.WhiteWin

  assert "8/8/8/1q1p2pp/8/KP3r2/5r2/8 w - - 0 1"
    |> starfish.from_fen
    |> starfish.state
    == starfish.Draw(starfish.Stalemate)

  assert "8/8/1RK5/8/7N/3rp3/3kb3/8 w - - 50 82"
    |> starfish.from_fen
    |> starfish.state
    == starfish.Draw(starfish.FiftyMoves)
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
    20, 400, 8902, 197_281, 4_865_609,
    //119_060_324,
  ])
}

pub fn perft_position2_test_() {
  use <- Timeout(1_000_000)
  use <- pocket_watch.callback("position 2", print_time)
  perft_all(
    "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
    [
      48, 2039, 97_862, 4_085_603,
      //193_690_690
    ],
  )
}

pub fn perft_position3_test_() {
  use <- Timeout(1_000_000)
  use <- pocket_watch.callback("position 3", print_time)
  perft_all("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1", [
    14, 191, 2812, 43_238, 674_624, 11_030_083,
    //178_633_661,
  ])
}

pub fn perft_position4_test_() {
  use <- Timeout(1_000_000)
  use <- pocket_watch.callback("position 4", print_time)
  perft_all("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1", [
    6, 264, 9467, 422_333, 15_833_292,
  ])
}

pub fn perft_position5_test_() {
  use <- Timeout(1_000_000)
  use <- pocket_watch.callback("position 5", print_time)
  perft_all("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8", [
    44, 1486, 62_379, 2_103_487,
    //89_941_194,
  ])
}

pub fn perft_position6_test_() {
  use <- Timeout(1_000_000)
  use <- pocket_watch.callback("position 6", print_time)
  perft_all(
    "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10",
    [
      46, 2079, 89_890, 3_894_594,
      //164_075_551
    ],
  )
}

// Ensure en passant blocking check is valid
pub fn perft_extra_position0_test() {
  use <- pocket_watch.callback("extra position 0", print_time)
  perft_all("r1b1kbnr/p1p1pppp/p1K3q1/3pP3/8/8/PPP1PPPP/RNBQ1BNR w kq d6 0 1", [
    5, 157, 4146, 128_984, 3_660_806,
  ])
}

// Extra tests from:
// https://gist.github.com/peterellisjones/8c46c28141c162d1d8a0f0badbc9cff9

pub fn perft_extra_position_1_test() {
  use <- pocket_watch.callback("extra position 1", print_time)
  perft("r6r/1b2k1bq/8/8/7B/8/8/R3K2R b KQ - 3 2", 1, 8)
}

pub fn perft_extra_position_2_test() {
  use <- pocket_watch.callback("extra 2osition 1", print_time)
  perft("8/8/8/2k5/2pP4/8/B7/4K3 b - d3 0 3", 1, 8)
}

pub fn perft_extra_position_3_test() {
  use <- pocket_watch.callback("extra 3osition 1", print_time)
  perft("r1bqkbnr/pppppppp/n7/8/8/P7/1PPPPPPP/RNBQKBNR w KQkq - 2 2", 1, 19)
}

pub fn perft_extra_position_4_test() {
  use <- pocket_watch.callback("extra 4osition 1", print_time)
  perft(
    "r3k2r/p1pp1pb1/bn2Qnp1/2qPN3/1p2P3/2N5/PPPBBPPP/R3K2R b KQkq - 3 2",
    1,
    5,
  )
}

pub fn perft_extra_position_5_test() {
  use <- pocket_watch.callback("extra 5osition 1", print_time)
  perft(
    "2kr3r/p1ppqpb1/bn2Qnp1/3PN3/1p2P3/2N5/PPPBBPPP/R3K2R b KQ - 3 2",
    1,
    44,
  )
}

pub fn perft_extra_position_6_test() {
  use <- pocket_watch.callback("extra 6osition 1", print_time)
  perft("rnb2k1r/pp1Pbppp/2p5/q7/2B5/8/PPPQNnPP/RNB1K2R w KQ - 3 9", 1, 39)
}

pub fn perft_extra_position_7_test() {
  use <- pocket_watch.callback("extra 7osition 1", print_time)
  perft("2r5/3pk3/8/2P5/8/2K5/8/8 w - - 5 4", 1, 9)
}

pub fn perft_extra_position_8_test() {
  use <- pocket_watch.callback("extra 8osition 1", print_time)
  perft("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8", 3, 62_379)
}

pub fn perft_extra_position_9_test() {
  use <- pocket_watch.callback("extra 9osition 1", print_time)
  perft(
    "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10",
    3,
    89_890,
  )
}

pub fn perft_extra_position_10_test() {
  use <- pocket_watch.callback("extra position 10", print_time)
  perft("3k4/3p4/8/K1P4r/8/8/8/8 b - - 0 1", 6, 1_134_888)
}

pub fn perft_extra_position_11_test() {
  use <- pocket_watch.callback("extra position 11", print_time)
  perft("8/8/4k3/8/2p5/8/B2P2K1/8 w - - 0 1", 6, 1_015_133)
}

pub fn perft_extra_position_12_test() {
  use <- pocket_watch.callback("extra position 12", print_time)
  perft("8/8/1k6/2b5/2pP4/8/5K2/8 b - d3 0 1", 6, 1_440_467)
}

pub fn perft_extra_position_13_test() {
  use <- pocket_watch.callback("extra position 13", print_time)
  perft("5k2/8/8/8/8/8/8/4K2R w K - 0 1", 6, 661_072)
}

pub fn perft_extra_position_14_test() {
  use <- pocket_watch.callback("extra position 14", print_time)
  perft("3k4/8/8/8/8/8/8/R3K3 w Q - 0 1", 6, 803_711)
}

pub fn perft_extra_position_15_test() {
  use <- pocket_watch.callback("extra position 15", print_time)
  perft("r3k2r/1b4bq/8/8/8/8/7B/R3K2R w KQkq - 0 1", 4, 1_274_206)
}

pub fn perft_extra_position_16_test() {
  use <- pocket_watch.callback("extra position 16", print_time)
  perft("r3k2r/8/3Q4/8/8/5q2/8/R3K2R b KQkq - 0 1", 4, 1_720_476)
}

pub fn perft_extra_position_17_test() {
  use <- pocket_watch.callback("extra position 17", print_time)
  perft("2K2r2/4P3/8/8/8/8/8/3k4 w - - 0 1", 6, 3_821_001)
}

pub fn perft_extra_position_18_test() {
  use <- pocket_watch.callback("extra position 18", print_time)
  perft("8/8/1P2K3/8/2n5/1q6/8/5k2 b - - 0 1", 5, 1_004_658)
}

pub fn perft_extra_position_19_test() {
  use <- pocket_watch.callback("extra position 19", print_time)
  perft("4k3/1P6/8/8/8/8/K7/8 w - - 0 1", 6, 217_342)
}

pub fn perft_extra_position_20_test() {
  use <- pocket_watch.callback("extra position 20", print_time)
  perft("8/P1k5/K7/8/8/8/8/8 w - - 0 1", 6, 92_683)
}

pub fn perft_extra_position_21_test() {
  use <- pocket_watch.callback("extra position 21", print_time)
  perft("K1k5/8/P7/8/8/8/8/8 w - - 0 1", 6, 2217)
}

pub fn perft_extra_position_22_test() {
  use <- pocket_watch.callback("extra position 22", print_time)
  perft("8/k1P5/8/1K6/8/8/8/8 w - - 0 1", 7, 567_584)
}

pub fn perft_extra_position_23_test() {
  use <- pocket_watch.callback("extra position 23", print_time)
  perft("8/8/2k5/5q2/5n2/8/5K2/8 b - - 0 1", 4, 23_527)
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

  // Castling rights are removed if the rook is taken
  test_apply_move(
    "rn1qkbnr/pbpppppp/1p6/6P1/8/8/PPPPPP1P/RNBQKBNR b KQkq - 0 3",
    // b7h1
    [move.Capture(from: 49, to: 7)],
    "rn1qkbnr/p1pppppp/1p6/6P1/8/8/PPPPPP1P/RNBQKBNb w Qkq - 0 4",
  )
}
