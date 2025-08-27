import starfish/internal/board

pub type Valid

pub type Legal

pub type Move(validity) {
  Castle(from: Int, to: Int)
  Standard(from: Int, to: Int)
  EnPassant(from: Int, to: Int)
  Promotion(from: Int, to: Int, piece: board.Piece)
}
