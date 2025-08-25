import gleam/dict.{type Dict}
import gleam/option.{type Option}
import iv
import starfish/internal/hash
import starfish/internal/piece_table

pub type Board {
  Board(iv.Array(Square))
}

pub type Square {
  Empty
  Occupied(Piece)
}

pub type Piece {
  Pawn(colour: Colour)
  Bishop(colour: Colour)
  Knight(colour: Colour)
  Rook(colour: Colour)
  Queen(colour: Colour)
  King(colour: Colour)
}

pub type Colour {
  White
  Black
}

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
    board: Board,
    to_move: Colour,
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
