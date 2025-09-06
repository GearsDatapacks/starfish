import gleam/bool
import gleam/dict.{type Dict}
import gleam/list
import starfish/internal/board.{type Board}
import starfish/internal/move/direction.{type Direction}

/// Information about which squares on the board are attacked.
pub type AttackInformation {
  AttackInformation(
    /// All squares which are immediately attacked by pieces of the specified
    /// colour. The king cannot move here, as that would be moving into check.
    attacks: List(Int),
    /// Whether the king is currently in check.
    in_check: Bool,
    // TODO: Maybe just merge this with `attacks`?
    /// Squares which would become attacked if the king were to move. The king
    /// also can't move here.
    check_attack_squares: List(Int),
    /// Squares which another piece can move to in order to block check. If
    /// multiple pieces are delivering check, or the king is not in check, this
    /// is empty.
    check_block_line: List(Int),
    /// A map of pieces to pin lines. To check if a piece can move, first look
    /// it up in this map. If it's not present, it is not pinned at all. If it
    /// is, the piece can only move to one of the specified squares in the list.
    pin_lines: Dict(Int, List(Int)),
  )
}

pub fn calculate(
  board: Board,
  king_position: Int,
  to_move: board.Colour,
) -> AttackInformation {
  let attacking = case to_move {
    board.Black -> board.White
    board.White -> board.Black
  }

  let attacks = get_attacks(board, attacking)

  let in_check = list.contains(attacks, king_position)

  let #(check_attack_squares, check_block_line) = case in_check {
    False -> #([], [])
    True -> #(
      get_check_attack_squares(board, attacking, king_position),
      get_check_block_line(board, attacking, king_position),
    )
  }

  let pin_lines = get_pin_lines(board, attacking, king_position)

  AttackInformation(
    attacks:,
    in_check:,
    check_attack_squares:,
    check_block_line:,
    pin_lines:,
  )
}

fn get_pin_lines(
  board: Board,
  attacking: board.Colour,
  king_position: Int,
) -> Dict(Int, List(Int)) {
  use lines, position, #(piece, colour) <- dict.fold(board, dict.new())
  use <- bool.guard(colour != attacking, lines)

  case piece {
    board.Bishop ->
      get_sliding_pin_lines(
        board,
        attacking,
        position,
        king_position,
        direction.bishop_directions,
        lines,
      )
    board.Queen ->
      get_sliding_pin_lines(
        board,
        attacking,
        position,
        king_position,
        direction.queen_directions,
        lines,
      )
    board.Rook ->
      get_sliding_pin_lines(
        board,
        attacking,
        position,
        king_position,
        direction.rook_directions,
        lines,
      )
    _ -> lines
  }
}

fn get_sliding_pin_lines(
  board: Board,
  attacking: board.Colour,
  position: Int,
  king_position: Int,
  directions: List(Direction),
  lines: Dict(Int, List(Int)),
) -> Dict(Int, List(Int)) {
  case directions {
    [] -> lines
    [direction, ..directions] ->
      get_sliding_pin_lines(
        board,
        attacking,
        position,
        king_position,
        directions,
        get_sliding_pin_lines_loop(
          board,
          attacking,
          position,
          king_position,
          direction,
          lines,
          [position],
          -1,
        ),
      )
  }
}

fn get_sliding_pin_lines_loop(
  board: Board,
  attacking: board.Colour,
  position: Int,
  king_position: Int,
  direction: Direction,
  lines: Dict(Int, List(Int)),
  line: List(Int),
  pinned_piece: Int,
) -> Dict(Int, List(Int)) {
  let position = direction.in_direction(position, direction)

  // If we hit the king and we have already passed a piece, that means that piece
  // is pinned to the king.
  use <- bool.lazy_guard(pinned_piece != -1 && position == king_position, fn() {
    dict.insert(lines, pinned_piece, line)
  })

  case board.get(board, position) {
    board.Empty ->
      get_sliding_pin_lines_loop(
        board,
        attacking,
        position,
        king_position,
        direction,
        lines,
        [position, ..line],
        pinned_piece,
      )
    // If we hit a piece of the opposite colour, and haven't yet encountered any
    // pieces, this could be a pin, so we remember the position and continue.
    board.Occupied(colour:, ..) if colour != attacking && pinned_piece == -1 ->
      get_sliding_pin_lines_loop(
        board,
        attacking,
        position,
        king_position,
        direction,
        lines,
        line,
        position,
      )
    // Otherwise, we have either hit a piece of the same colour, in which case
    // it isn't a pin, or we have hit the edge of the board, meaning there is
    // also no pin, or we have hit more than one piece of the opposite colour,
    // also meaning there is no pin.
    _ -> lines
  }
}

type Line {
  NoLine
  Single(List(Int))
  Multiple
}

fn get_check_block_line(
  board: Board,
  attacking: board.Colour,
  king_position: Int,
) -> List(Int) {
  // This cursed piece of code just lets us map the final value after all the
  // `use` statements. Because of function inlining, this is equivalent to using
  // a block, then mapping at the end.
  use <-
    fn(f) {
      let line = f()
      case line {
        Single(line) -> line
        Multiple | NoLine -> []
      }
    }

  use line, position, #(piece, colour) <- dict.fold(board, NoLine)

  // If multiple difference pieces are putting the king in check, it cannot be
  // blocked, and the king must move instead. In that case, there's no point
  // computing any more attacks, since no matter what, they cannot be blocked.
  use <- bool.guard(line == Multiple, line)
  use <- bool.guard(colour != attacking, line)

  case piece {
    // If a sliding piece is causing check, moving anywhere in the line
    // between that piece and the king (including taking the piece) prevents
    // check.
    board.Rook ->
      case
        sliding_check_block_line(
          board,
          position,
          king_position,
          direction.rook_directions,
        ),
        line
      {
        [], _ -> line
        line, NoLine -> Single(line)
        _, _ -> Multiple
      }
    board.Bishop ->
      case
        sliding_check_block_line(
          board,
          position,
          king_position,
          direction.bishop_directions,
        ),
        line
      {
        [], _ -> line
        line, NoLine -> Single(line)
        _, _ -> Multiple
      }
    board.Queen ->
      case
        sliding_check_block_line(
          board,
          position,
          king_position,
          direction.queen_directions,
        ),
        line
      {
        [], _ -> line
        line, NoLine -> Single(line)
        _, _ -> Multiple
      }
    // For pieces which only move in a set number of directions, the only way
    // to prevent check is to take that piece.
    board.King ->
      case
        piece_attacks_square(
          position,
          king_position,
          direction.queen_directions,
        ),
        line
      {
        False, _ -> line
        True, NoLine -> Single([position])
        _, _ -> Multiple
      }
    board.Knight ->
      case
        piece_attacks_square(
          position,
          king_position,
          direction.knight_directions,
        ),
        line
      {
        False, _ -> line
        True, NoLine -> Single([position])
        _, _ -> Multiple
      }
    board.Pawn if attacking == board.Black ->
      case
        piece_attacks_square(
          position,
          king_position,
          direction.black_pawn_captures,
        ),
        line
      {
        False, _ -> line
        True, NoLine -> Single([position])
        _, _ -> Multiple
      }
    board.Pawn ->
      case
        piece_attacks_square(
          position,
          king_position,
          direction.white_pawn_captures,
        ),
        line
      {
        False, _ -> line
        True, NoLine -> Single([position])
        _, _ -> Multiple
      }
  }
}

fn piece_attacks_square(
  position: Int,
  target: Int,
  directions: List(Direction),
) -> Bool {
  case directions {
    [] -> False
    [direction, ..directions] ->
      case direction.in_direction(position, direction) == target {
        True -> True
        False -> piece_attacks_square(position, target, directions)
      }
  }
}

fn sliding_check_block_line(
  board: Board,
  position: Int,
  king_position: Int,
  directions: List(Direction),
) -> List(Int) {
  case directions {
    [] -> []
    [direction, ..directions] ->
      case
        sliding_check_block_line_loop(
          board,
          position,
          king_position,
          False,
          direction,
          [position],
        )
      {
        [] ->
          sliding_check_block_line(board, position, king_position, directions)
        line -> line
      }
  }
}

fn sliding_check_block_line_loop(
  board: Board,
  position: Int,
  king_position: Int,
  found_king: Bool,
  direction: Direction,
  line: List(Int),
) -> List(Int) {
  let new_position = direction.in_direction(position, direction)
  use <- bool.guard(new_position == king_position, line)

  case board.get(board, new_position) {
    board.Empty ->
      sliding_check_block_line_loop(
        board,
        new_position,
        king_position,
        found_king,
        direction,
        [new_position, ..line],
      )
    _ -> []
  }
}

// TODO: We can probably merge this with `get_check_block_line`, to avoid
// iterating the board twice.
/// When the king is in check, it cannot move into any squares that are attacked
/// by other pieces. However, there's another case too. Imagine one row of the
/// board looks like this:
///
/// ```txt
/// | R |   |   |   | k | B |   |   |
/// ```
///
/// Here, the white bishop isn't protected by the rook. However, if the king were
/// to capture the bishop, the rook now is attacking that square, so the king is
/// still in check. For this reason, we need to calculate all the squares just
/// past the king in a check line, and prevent the king from moving there.
fn get_check_attack_squares(
  board: Board,
  attacking: board.Colour,
  king_position: Int,
) -> List(Int) {
  use squares, position, #(piece, colour) <- dict.fold(board, [])
  use <- bool.guard(colour != attacking, squares)

  case piece {
    board.Bishop ->
      get_sliding_check_attack_squares(
        board,
        position,
        king_position,
        direction.bishop_directions,
        squares,
      )
    board.Queen ->
      get_sliding_check_attack_squares(
        board,
        position,
        king_position,
        direction.queen_directions,
        squares,
      )
    board.Rook ->
      get_sliding_check_attack_squares(
        board,
        position,
        king_position,
        direction.rook_directions,
        squares,
      )
    _ -> squares
  }
}

fn get_sliding_check_attack_squares(
  board: Board,
  position: Int,
  king_position: Int,
  directions: List(Direction),
  squares: List(Int),
) -> List(Int) {
  case directions {
    [] -> squares
    [direction, ..directions] ->
      get_sliding_check_attack_squares(
        board,
        position,
        king_position,
        directions,
        get_sliding_check_attack_squares_loop(
          board,
          position,
          king_position,
          direction,
          squares,
        ),
      )
  }
}

fn get_sliding_check_attack_squares_loop(
  board: Board,
  position: Int,
  king_position: Int,
  direction: Direction,
  squares: List(Int),
) -> List(Int) {
  let new_position = direction.in_direction(position, direction)

  case new_position == king_position {
    // We've encountered the king. The next square would be attacked if the king
    // were to move there.
    True ->
      case direction.in_direction(new_position, direction) {
        // If it's off the board, we don't need to track it
        -1 -> squares
        position -> [position, ..squares]
      }
    False ->
      case board.get(board, new_position) {
        board.Empty ->
          get_sliding_check_attack_squares_loop(
            board,
            new_position,
            king_position,
            direction,
            squares,
          )
        // If we hit another piece (or the edge of the board) before the king,
        // we can stop.
        _ -> squares
      }
  }
}

fn get_attacks(board: Board, attacking: board.Colour) -> List(Int) {
  use attacks, position, #(piece, colour) <- dict.fold(board, [])
  case colour == attacking {
    True -> get_attacks_for_piece(board, piece, position, attacking, attacks)
    False -> attacks
  }
}

fn get_attacks_for_piece(
  board: Board,
  piece: board.Piece,
  position: Int,
  colour: board.Colour,
  positions: List(Int),
) -> List(Int) {
  case piece {
    board.Bishop ->
      get_sliding_attacks(
        board,
        position,
        direction.bishop_directions,
        positions,
      )
    board.Queen ->
      get_sliding_attacks(
        board,
        position,
        direction.queen_directions,
        positions,
      )
    board.Rook ->
      get_sliding_attacks(board, position, direction.rook_directions, positions)
    board.King ->
      get_single_move_attacks(position, positions, direction.queen_directions)
    board.Pawn if colour == board.Black ->
      get_single_move_attacks(
        position,
        positions,
        direction.black_pawn_captures,
      )
    board.Pawn ->
      get_single_move_attacks(
        position,
        positions,
        direction.white_pawn_captures,
      )
    board.Knight ->
      get_single_move_attacks(position, positions, direction.knight_directions)
  }
}

fn get_sliding_attacks(
  board: Board,
  position: Int,
  directions: List(Direction),
  positions: List(Int),
) -> List(Int) {
  case directions {
    [] -> positions
    [direction, ..directions] ->
      get_sliding_attacks(
        board,
        position,
        directions,
        get_sliding_attacks_loop(board, position, direction, positions),
      )
  }
}

fn get_sliding_attacks_loop(
  board: Board,
  position: Int,
  direction: Direction,
  positions: List(Int),
) -> List(Int) {
  let new_position = direction.in_direction(position, direction)

  case board.get(board, new_position) {
    board.OffBoard -> positions
    board.Empty ->
      get_sliding_attacks_loop(board, new_position, direction, [
        new_position,
        ..positions
      ])
    _ -> [new_position, ..positions]
  }
}

fn get_single_move_attacks(
  position: Int,
  positions: List(Int),
  directions: List(Direction),
) -> List(Int) {
  case directions {
    [] -> positions
    [direction, ..directions] -> {
      let positions = case direction.in_direction(position, direction) {
        -1 -> positions
        position -> [position, ..positions]
      }
      get_single_move_attacks(position, positions, directions)
    }
  }
}
