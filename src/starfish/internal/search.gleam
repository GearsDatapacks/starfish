import gleam/bool
import gleam/option.{type Option, None, Some}
import starfish/internal/evaluate
import starfish/internal/game.{type Game}
import starfish/internal/hash
import starfish/internal/move.{type Move}

/// Not really infinity, but a high enough number that nothing but explicit
/// references to it will reach it.
const infinity = 1_000_000_000

const checkmate = -1_000_000

pub fn best_move(game: Game, depth: Int) -> Result(Move, Nil) {
  use <- bool.guard(depth < 1, Error(Nil))
  search_top_level(
    game,
    hash.new_table(),
    depth,
    move.legal(game),
    None,
    -infinity,
  )
}

type SearchResult {
  SearchResult(
    eval: Int,
    cached_positions: hash.Table,
    eval_kind: hash.CacheKind,
  )
}

/// Standard minimax search, with alpha-beta pruning. For each position searched,
/// we recursively search all possible positions resulting from it, then negate
/// the result. This is because the evaluation for one side is the negative value
/// of the evaluation for the other side.
/// Once we reach depth 0, we perform a static evaluation of the board. (See the
/// `evaluate` module)
fn search_top_level(
  game: Game,
  cached_positions: hash.Table,
  depth: Int,
  legal_moves: List(Move),
  best_move: Option(Move),
  best_eval: Int,
) -> Result(Move, Nil) {
  case legal_moves {
    [] ->
      case best_move {
        None -> Error(Nil)
        Some(best_move) -> Ok(best_move)
      }
    [move, ..moves] -> {
      let SearchResult(eval:, cached_positions:, ..) =
        search(
          move.apply(game, move),
          cached_positions,
          depth - 1,
          -infinity,
          -best_eval,
          0,
        )

      let eval = -eval

      let #(best_move, best_eval) = case eval > best_eval {
        True -> #(Some(move), eval)
        False -> #(best_move, best_eval)
      }
      search_top_level(
        game,
        cached_positions,
        depth,
        moves,
        best_move,
        best_eval,
      )
    }
  }
}

fn search(
  game: Game,
  cached_positions: hash.Table,
  depth: Int,
  best_eval: Int,
  best_opponent_move: Int,
  depth_searched: Int,
) -> SearchResult {
  // If we have reached fifty moves, the game is already a draw, so there's no
  // point searching further.
  use <- bool.guard(
    game.half_moves >= 50,
    SearchResult(0, cached_positions, hash.Exact),
  )

  case
    hash.get(
      cached_positions,
      game.zobrist_hash,
      depth,
      depth_searched,
      best_eval,
      best_opponent_move,
    )
  {
    Ok(#(eval, eval_kind)) -> SearchResult(eval:, cached_positions:, eval_kind:)
    Error(_) ->
      case move.legal(game) {
        // If the game is in a checkmate or stalemate position, the game is over, so
        // we stop searching.
        [] -> {
          let eval = case game.attack_information.in_check {
            // Sooner checkmate is better. Or from the perspective of the side being
            // mated, later checkmate is better.
            True -> checkmate + depth_searched
            False -> 0
          }
          let cached_positions =
            hash.cache(
              cached_positions,
              game.zobrist_hash,
              depth,
              depth_searched,
              hash.Exact,
              eval,
            )
          SearchResult(eval:, cached_positions:, eval_kind: hash.Exact)
        }
        moves ->
          case depth {
            // Once we reach the limit of our depth, we statically evaluate the position.
            0 -> {
              let eval = evaluate.evaluate(game, moves)
              let cached_positions =
                hash.cache(
                  cached_positions,
                  game.zobrist_hash,
                  depth,
                  depth_searched,
                  hash.Exact,
                  eval,
                )
              SearchResult(eval:, cached_positions:, eval_kind: hash.Exact)
            }
            _ -> {
              let SearchResult(eval:, cached_positions:, eval_kind:) =
                search_loop(
                  game,
                  cached_positions,
                  moves,
                  depth,
                  best_eval,
                  best_opponent_move,
                  depth_searched,
                  hash.Ceiling,
                )

              let cached_positions =
                hash.cache(
                  cached_positions,
                  game.zobrist_hash,
                  depth,
                  depth_searched,
                  eval_kind,
                  eval,
                )

              SearchResult(eval:, cached_positions:, eval_kind:)
            }
          }
      }
  }
}

fn search_loop(
  game: Game,
  cached_positions: hash.Table,
  moves: List(Move),
  depth: Int,
  // The best evaluation we've encountered so far.
  best_eval: Int,
  // The best evaluation our opponent can get. If we find a position which scores
  // higher than this, we can discard it. That position is too good and we can
  // assume that our opponent would never let us get there.
  best_opponent_move: Int,
  depth_searched: Int,
  eval_kind: hash.CacheKind,
) -> SearchResult {
  case moves {
    [] -> SearchResult(eval: best_eval, cached_positions:, eval_kind:)
    [move, ..moves] -> {
      // Evaluate the position for the opponent. The negative of the opponent's
      // eval is our eval.
      let SearchResult(eval:, cached_positions:, eval_kind: search_kind) =
        search(
          move.apply(game, move),
          cached_positions,
          depth - 1,
          -best_opponent_move,
          -best_eval,
          depth_searched + 1,
        )

      let eval = -eval

      use <- bool.guard(
        eval >= best_opponent_move,
        SearchResult(best_opponent_move, cached_positions, hash.Floor),
      )

      let #(best_eval, eval_kind) = case eval > best_eval {
        True -> #(eval, search_kind)
        False -> #(best_eval, eval_kind)
      }

      search_loop(
        game,
        cached_positions,
        moves,
        depth,
        best_eval,
        best_opponent_move,
        depth_searched,
        eval_kind,
      )
    }
  }
}
