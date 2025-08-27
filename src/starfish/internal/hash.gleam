//// An implementation of [Zobrist Hashing](https://www.chessprogramming.org/Zobrist_Hashing).

import gleam/int
import iv
import starfish/internal/board

const white_pawn = 0

const black_pawn = 1

const white_knight = 2

const black_knight = 3

const white_bishop = 4

const black_bishop = 5

const white_rook = 6

const black_rook = 7

const white_queen = 8

const black_queen = 9

const white_king = 10

const black_king = 11

const num_pieces = 12

/// Data structure for storing randomly generated data used for hashing positions
pub opaque type HashData {
  HashData(table: iv.Array(Int), black_to_move: Int)
}

const max_64_bit_int = 18_446_744_073_709_552_000

pub fn generate_data() -> HashData {
  let table =
    iv.initialise(num_pieces * board.side_length * board.side_length, fn(_) {
      int.random(max_64_bit_int)
    })
  let black_to_move = int.random(max_64_bit_int)
  HashData(table:, black_to_move:)
}

pub fn hash(
  data: HashData,
  board: iv.Array(board.Square),
  to_move: board.Colour,
) -> Int {
  let hash = case to_move {
    board.Black -> data.black_to_move
    board.White -> 0
  }

  iv.index_fold(board, hash, fn(hash, square, position) {
    case square {
      board.Empty -> hash
      board.Occupied(piece) -> toggle_hash_square(hash, position, piece, data)
    }
  })
}

fn toggle_hash_square(
  hash: Int,
  position: Int,
  piece: board.Piece,
  data: HashData,
) -> Int {
  let piece_index = case piece {
    board.Pawn(board.White) -> white_pawn
    board.Bishop(board.White) -> white_bishop
    board.King(board.White) -> white_king
    board.Knight(board.White) -> white_knight
    board.Queen(board.White) -> white_queen
    board.Rook(board.White) -> white_rook
    board.Bishop(board.Black) -> black_bishop
    board.King(board.Black) -> black_king
    board.Knight(board.Black) -> black_knight
    board.Pawn(board.Black) -> black_pawn
    board.Queen(board.Black) -> black_queen
    board.Rook(board.Black) -> black_rook
  }
  let index = position * num_pieces + piece_index
  int.bitwise_exclusive_or(iv.get_or_default(data.table, index, 0), hash)
}
