import gleeunit
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
