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

type Until =
  fn(Int) -> Bool

pub fn best_move(game: Game, until: Until) -> Result(Move, Nil) {
  use <- bool.guard(until(0), Error(Nil))
  let legal_moves = move.legal(game)
  use <- bool.guard(legal_moves == [], Error(Nil))
  iterative_deepening(game, 1, None, legal_moves, hash.new_table(), until)
}

fn iterative_deepening(
  game: Game,
  depth: Int,
  best_move: Option(Move),
  legal_moves: List(Move),
  cached_positions: hash.Table,
  until: Until,
) -> Result(Move, Nil) {
  use <- bool.guard(until(depth), option.to_result(best_move, Nil))

  let move_result =
    search_top_level(
      game,
      cached_positions,
      depth,
      legal_moves,
      None,
      -infinity,
      until,
    )

  case move_result {
    Error(_) -> option.to_result(best_move, Nil)
    Ok(TopLevelSearchResult(best_move:, cached_positions:)) ->
      iterative_deepening(
        game,
        depth + 1,
        Some(best_move),
        legal_moves,
        cached_positions,
        until,
      )
  }
}

type TopLevelSearchResult {
  TopLevelSearchResult(best_move: Move, cached_positions: hash.Table)
}

type SearchResult {
  SearchResult(
    eval: Int,
    cached_positions: hash.Table,
    eval_kind: hash.CacheKind,
    finished: Bool,
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
  until: Until,
) -> Result(TopLevelSearchResult, Nil) {
  case legal_moves {
    [] ->
      case best_move {
        None -> Error(Nil)
        Some(best_move) ->
          Ok(TopLevelSearchResult(best_move:, cached_positions:))
      }
    [move, ..moves] -> {
      let SearchResult(eval:, cached_positions:, eval_kind: _, finished:) =
        search(
          move.apply(game, move),
          cached_positions,
          depth - 1,
          -infinity,
          -best_eval,
          0,
          until,
        )

      use <- bool.guard(!finished, case best_move {
        None -> Error(Nil)
        Some(best_move) ->
          Ok(TopLevelSearchResult(best_move:, cached_positions:))
      })

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
        until,
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
  until: Until,
) -> SearchResult {
  use <- bool.guard(
    until(depth),
    SearchResult(0, cached_positions, hash.Exact, False),
  )

  // If we have reached fifty moves, the game is already a draw, so there's no
  // point searching further.
  use <- bool.guard(
    game.half_moves >= 50,
    SearchResult(0, cached_positions, hash.Exact, True),
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
    Ok(#(eval, eval_kind)) ->
      SearchResult(eval:, cached_positions:, eval_kind:, finished: True)
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
          SearchResult(
            eval:,
            cached_positions:,
            eval_kind: hash.Exact,
            finished: True,
          )
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
              SearchResult(
                eval:,
                cached_positions:,
                eval_kind: hash.Exact,
                finished: True,
              )
            }
            _ -> {
              let SearchResult(eval:, cached_positions:, eval_kind:, finished:) as result =
                search_loop(
                  game,
                  cached_positions,
                  moves,
                  depth,
                  best_eval,
                  best_opponent_move,
                  depth_searched,
                  hash.Ceiling,
                  until,
                )

              use <- bool.guard(!finished, result)

              let cached_positions =
                hash.cache(
                  cached_positions,
                  game.zobrist_hash,
                  depth,
                  depth_searched,
                  eval_kind,
                  eval,
                )

              SearchResult(eval:, cached_positions:, eval_kind:, finished:)
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
  until: Until,
) -> SearchResult {
  case moves {
    [] ->
      SearchResult(
        eval: best_eval,
        cached_positions:,
        eval_kind:,
        finished: True,
      )
    [move, ..moves] -> {
      // Evaluate the position for the opponent. The negative of the opponent's
      // eval is our eval.
      let SearchResult(
        eval:,
        cached_positions:,
        eval_kind: search_kind,
        finished:,
      ) as result =
        search(
          move.apply(game, move),
          cached_positions,
          depth - 1,
          -best_opponent_move,
          -best_eval,
          depth_searched + 1,
          until,
        )

      use <- bool.guard(!finished, result)

      let eval = -eval

      use <- bool.guard(
        eval >= best_opponent_move,
        SearchResult(
          best_opponent_move,
          cached_positions,
          hash.Floor,
          finished: True,
        ),
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
        until,
      )
    }
  }
}
