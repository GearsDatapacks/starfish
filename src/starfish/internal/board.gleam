import iv

pub type Board {
  Board(squares: iv.Array(Square))
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

pub fn initial_position() -> Board {
  Board(iv.from_list(initial_squares))
}

const initial_squares = [
  Occupied(Rook(White)),
  Occupied(Knight(White)),
  Occupied(Bishop(White)),
  Occupied(Queen(White)),
  Occupied(King(White)),
  Occupied(Bishop(White)),
  Occupied(Knight(White)),
  Occupied(Rook(White)),
  Occupied(Pawn(White)),
  Occupied(Pawn(White)),
  Occupied(Pawn(White)),
  Occupied(Pawn(White)),
  Occupied(Pawn(White)),
  Occupied(Pawn(White)),
  Occupied(Pawn(White)),
  Occupied(Pawn(White)),
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Empty,
  Occupied(Pawn(Black)),
  Occupied(Pawn(Black)),
  Occupied(Pawn(Black)),
  Occupied(Pawn(Black)),
  Occupied(Pawn(Black)),
  Occupied(Pawn(Black)),
  Occupied(Pawn(Black)),
  Occupied(Pawn(Black)),
  Occupied(Rook(Black)),
  Occupied(Knight(Black)),
  Occupied(Bishop(Black)),
  Occupied(Queen(Black)),
  Occupied(King(Black)),
  Occupied(Bishop(Black)),
  Occupied(Knight(Black)),
  Occupied(Rook(Black)),
]
