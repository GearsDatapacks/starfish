//// Code generation for various parts of the engine

import gleam/int
import simplifile as file
import starfish/internal/board

const generated_file_path = "src/starfish/internal/generated.gleam"

pub fn main() {
  let imports =
    "import starfish/internal/board.{Pawn, Knight, Bishop, Rook, Queen, King, White, Black}"

  let generated_code = imports <> "\n\n" <> generate_hash_data() <> "\n"
  let assert Ok(Nil) =
    file.write(to: generated_file_path, contents: generated_code)
}

/// 60 bits is the highest we can go on Erlang where bitwise operations are fast,
/// so we limit hashes to 60 bits to ensure computing hashes is as fast as possible.
const max_60_bit_int = 576_460_752_303_423_500

const piece_count = 12

fn generate_hash_data() -> String {
  let function_head =
    "pub fn hash_for_piece(piece: board.Piece, colour: board.Colour, position: Int) -> Int {
  case piece, colour, position {"
  let length = piece_count * board.size
  let hash_for_piece = generate_hash_data_loop(function_head, 0, length)
  // Generate a random hash to indicate that it's black's turn to move
  let black_to_move =
    "pub const black_to_move_hash = 0x"
    <> int.to_base16(int.random(max_60_bit_int))

  hash_for_piece <> "\n\n" <> black_to_move
}

fn generate_hash_data_loop(acc: String, generated: Int, length: Int) -> String {
  case generated >= length {
    True -> acc <> "\n  }\n}"
    False -> {
      let position = generated / piece_count
      let position = case position == board.size - 1 {
        // The last square needs to be a catch-all to ensure the `case` expression
        // compiles.
        True -> "_"
        False -> int.to_string(position)
      }
      let piece = case generated % piece_count {
        0 -> "Pawn, White"
        1 -> "Pawn, Black"
        2 -> "Knight, White"
        3 -> "Knight, Black"
        4 -> "Bishop, White"
        5 -> "Bishop, Black"
        6 -> "Rook, White"
        7 -> "Rook, Black"
        8 -> "Queen, White"
        9 -> "Queen, Black"
        10 -> "King, White"
        _11 -> "King, Black"
      }
      let acc =
        acc
        <> "\n    "
        <> piece
        <> ", "
        <> position
        <> " -> 0x"
        // Generate a random 60-bit hash for this combination of piece and position
        <> int.to_base16(int.random(max_60_bit_int))
      generate_hash_data_loop(acc, generated + 1, length)
    }
  }
}
