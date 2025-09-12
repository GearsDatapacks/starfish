//// An implementation of [Zobrist Hashing](https://www.chessprogramming.org/Zobrist_Hashing).

import gleam/bool
import gleam/dict
import gleam/int
import iv
import starfish/internal/board
import starfish/internal/generated

pub fn hash(board: board.Board, to_move: board.Colour) -> Int {
  let hash = case to_move {
    board.Black -> generated.black_to_move_hash
    board.White -> 0
  }

  dict.fold(board, hash, fn(hash, position, square) {
    let #(piece, colour) = square
    int.bitwise_exclusive_or(
      generated.hash_for_piece(piece, colour, position),
      hash,
    )
  })
}

pub fn toggle_piece(
  hash: Int,
  position: Int,
  piece: board.Piece,
  colour: board.Colour,
) -> Int {
  int.bitwise_exclusive_or(
    generated.hash_for_piece(piece, colour, position),
    hash,
  )
}

pub fn toggle_to_move(hash: Int) -> Int {
  int.bitwise_exclusive_or(hash, generated.black_to_move_hash)
}

pub type Table =
  iv.Array(Entry)

/// Information about a particular position that has been cached
pub type Entry {
  Entry(
    /// The depth to which the search has been performed to get this evaluation.
    /// We don't want to end up using caches for searches done to lower depths
    /// than we are currently at, because that loses precision.
    depth: Int,
    kind: CacheKind,
    eval: Int,
    hash: Int,
  )
}

pub type CacheKind {
  /// This position was evaluated fully, so we know its exact evaluation
  Exact
  /// This move was too good, meaning our opponent would never have let us get
  /// here (beta-pruning). The evaluation of this move is as least as good as
  /// this.
  Floor
  /// This position didn't evaluate higher than the previous best evaluation, so
  /// it is a best as high as this.
  Ceiling
}

// Zobrist hashes can't be negative, so a negative hash indicates that an entry
// is not present.
const missing_entry = Entry(depth: -1, kind: Exact, eval: 0, hash: -1)

pub fn new_table() {
  iv.repeat(missing_entry, table_size)
}

const table_size = 100_000

pub fn cache(
  table: Table,
  hash: Int,
  depth: Int,
  depth_searched: Int,
  kind: CacheKind,
  eval: Int,
) -> Table {
  let key = hash % table_size

  let eval = correct_mate_score(eval, -depth_searched)

  let position = Entry(depth:, kind:, eval:, hash:)

  iv.try_set(table, key, position)
}

fn correct_mate_score(eval: Int, depth_searched: Int) -> Int {
  case eval < 0 {
    True ->
      case eval - max_depth <= -checkmate {
        True -> eval + depth_searched
        False -> eval
      }
    False ->
      case eval + max_depth >= checkmate {
        True -> eval - depth_searched
        False -> eval
      }
  }
}

pub fn get(
  table: Table,
  hash: Int,
  depth: Int,
  depth_searched: Int,
  best_eval: Int,
  best_opponent_move: Int,
) -> Result(#(Int, CacheKind), Nil) {
  let key = hash % table_size
  let entry = iv.get_or_default(table, key, missing_entry)
  use <- bool.guard(entry.hash != hash || entry.depth < depth, Error(Nil))

  let eval = correct_mate_score(entry.eval, depth_searched)

  case entry.kind {
    Exact -> Ok(#(eval, Exact))
    Ceiling if eval >= best_opponent_move -> Ok(#(eval, Ceiling))
    Floor if eval <= best_eval -> Ok(#(eval, Floor))
    _ -> Error(Nil)
  }
}

const checkmate = 1_000_000

const max_depth = 1000
