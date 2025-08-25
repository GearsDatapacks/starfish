import gleeunit
import starfish

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn from_fen_test() {
  let initial = starfish.new()
  let parsed = starfish.from_fen(starfish.starting_fen)
  assert initial == parsed

  let initial_with_only_position = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
  let parsed = starfish.from_fen(initial_with_only_position)
  assert initial == parsed
}
