import gleam/dict.{type Dict}
import gleam/option.{type Option, None}
import starfish/internal/board
import starfish/internal/hash
import starfish/internal/piece_table

pub type Castling {
  Castling(
    white_kingside: Bool,
    white_queenside: Bool,
    black_kingside: Bool,
    black_queenside: Bool,
  )
}

pub type Game {
  Game(
    // Game state
    board: board.Board,
    to_move: board.Colour,
    castling: Castling,
    en_passant_square: Option(Int),
    half_moves: Int,
    full_moves: Int,
    // Extra information
    zobrist_hash: Int,
    hash_data: hash.HashData,
    piece_tables: piece_table.PieceTables,
    previous_positions: Dict(Int, Int),
  )
}

pub fn initial_position() -> Game {
  let to_move = board.White
  let hash_data = hash.generate_data()
  let piece_tables = piece_table.make_tables()
  let board = board.initial_position()
  let zobrist_hash = hash.hash(hash_data, board, to_move)

  Game(
    board:,
    to_move:,
    castling: Castling(True, True, True, True),
    en_passant_square: None,
    half_moves: 0,
    full_moves: 0,
    zobrist_hash:,
    hash_data:,
    piece_tables:,
    previous_positions: dict.new(),
  )
}
