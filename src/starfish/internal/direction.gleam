import starfish/internal/board

/// A direction along the board. This could be a cardinal direction such as left
/// or up, or even a direction which jumps squares, such as how a knight moves.
pub type Direction {
  Direction(file_change: Int, rank_change: Int)
}

/// Returns a position moved in a given direction, checking for it being within
/// the bounds of the board.
pub fn in_direction(position: Int, direction: Direction) -> Int {
  let file = board.file(position) + direction.file_change
  let rank = board.rank(position) + direction.rank_change

  case file >= board.side_length || rank >= board.side_length {
    True -> -1
    False -> board.position(file:, rank:)
  }
}

pub const left = Direction(-1, 0)

pub const right = Direction(1, 0)

pub const up = Direction(0, 1)

pub const down = Direction(0, -1)

pub const up_left = Direction(-1, 1)

pub const down_left = Direction(-1, -1)

pub const up_right = Direction(1, 1)

pub const down_right = Direction(1, -1)

pub const rook_directions = [left, right, up, down]

pub const bishop_directions = [up_left, up_right, down_left, down_right]

pub const queen_directions = [
  left,
  right,
  up,
  down,
  up_left,
  up_right,
  down_left,
  down_right,
]

pub const knight_directions = [
  Direction(-1, -2),
  Direction(1, -2),
  Direction(-1, 2),
  Direction(1, 2),
  Direction(2, -1),
  Direction(2, 1),
  Direction(-2, -1),
  Direction(-2, 1),
]
