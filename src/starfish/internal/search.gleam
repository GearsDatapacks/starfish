import gleam/bool
import gleam/option.{type Option, None, Some}
import starfish/internal/evaluate
import starfish/internal/game.{type Game}
import starfish/internal/move

/// Not really infinity, but a high enough number that nothing but explicit 
/// references to it will reach it.
const infinity = 1_000_000_000

type Move =
  move.Move(move.Legal)

pub fn best_move(game: Game, depth: Int) -> Result(Move, Nil) {
  use <- bool.guard(depth < 1, Error(Nil))
  let result = search_top_level(game, depth, move.legal(game), None, -infinity)

  case result {
    Ok(SearchResult(eval: _, move:)) -> Ok(move)
    Error(Nil) -> Error(Nil)
  }
}

type SearchResult {
  SearchResult(eval: Int, move: Move)
}

/// Standard minimax search, with alpha-beta pruning. For each position searched,
/// we recursively search all possible positions resulting from it, then negate
/// the result. This is because the evaluation for one side is the negative value
/// of the evaluation for the other side.
/// Once we reach depth 0, we perform a static evaluation of the board. (See the
/// `evaluate` module)
fn search_top_level(
  game: Game,
  depth: Int,
  legal_moves: List(Move),
  best_move: Option(Move),
  best_eval: Int,
) -> Result(SearchResult, Nil) {
  case legal_moves {
    [] ->
      case best_move {
        None -> Error(Nil)
        Some(best_move) -> Ok(SearchResult(move: best_move, eval: best_eval))
      }
    [move, ..moves] -> {
      let eval =
        -search(move.apply(game, move), depth - 1, -infinity, -best_eval, 0)

      let #(best_move, best_eval) = case eval > best_eval {
        True -> #(Some(move), eval)
        False -> #(best_move, best_eval)
      }
      search_top_level(game, depth, moves, best_move, best_eval)
    }
  }
}

fn search(
  game: Game,
  depth: Int,
  best_eval: Int,
  best_opponent_move: Int,
  depth_searched: Int,
) -> Int {
  // If we have reached fifty moves, the game is already a draw, so there's no
  // point searching further.
  use <- bool.guard(game.half_moves >= 50, 0)

  case move.legal(game) {
    // If the game is in a checkmate or stalemate position, the game is over, so
    // we stop searching.
    // Sooner checkmate is better. Or from the perspective of the side being
    // mated, later checkmate is better.
    [] if game.attack_information.in_check -> -infinity + depth_searched
    [] -> 0
    moves ->
      case depth {
        // Once we reach the limit of our depth, we statically evaluate the position.
        0 -> evaluate.evaluate(game, moves)
        _ -> {
          search_loop(
            game,
            moves,
            depth,
            best_eval,
            best_opponent_move,
            depth_searched,
          )
        }
      }
  }
}

fn search_loop(
  game: Game,
  moves: List(Move),
  depth: Int,
  // The best evaluation we've encountered so far.
  best_eval: Int,
  // The best evaluation our opponent can get. If we find a position which scores
  // higher than this, we can discard it. That position is too good and we can
  // assume that our opponent would never let us get there.
  best_opponent_move: Int,
  depth_searched: Int,
) -> Int {
  case moves {
    [] -> best_eval
    [move, ..moves] -> {
      // Evaluate the position for the opponent. The negative of the opponent's
      // eval is our eval.
      let eval =
        -search(
          move.apply(game, move),
          depth - 1,
          -best_opponent_move,
          -best_eval,
          depth_searched + 1,
        )

      use <- bool.guard(eval >= best_opponent_move, best_opponent_move)

      let best_eval = case eval > best_eval {
        True -> eval
        False -> best_eval
      }

      search_loop(
        game,
        moves,
        depth,
        best_eval,
        best_opponent_move,
        depth_searched,
      )
    }
  }
}
