import gleam/bool
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import starfish/internal/board
import starfish/internal/evaluate
import starfish/internal/game.{type Game}
import starfish/internal/hash
import starfish/internal/move.{type Move}
import starfish/internal/piece_table

/// Not really infinity, but a high enough number that nothing but explicit
/// references to it will reach it.
const infinity = 1_000_000_000

const checkmate = -1_000_000

type Until =
  fn(Int) -> Bool

pub fn best_move(game: Game, until: Until) -> Result(Move, Nil) {
  use <- bool.guard(until(0), Error(Nil))
  let legal_moves = order_moves(game)
  use <- bool.guard(legal_moves == [], Error(Nil))
  iterative_deepening(game, 1, None, legal_moves, hash.new_table(), until)
}

fn iterative_deepening(
  game: Game,
  depth: Int,
  best_move: Option(Move),
  legal_moves: List(#(Move, Int)),
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
        reorder_moves(legal_moves, best_move),
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
  legal_moves: List(#(Move, Int)),
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
    [#(move, _), ..moves] -> {
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
        // If we had to stop this search before searching any positions, we return
        // an error, and instead we will use the best move from the previous
        // iteration.
        None -> Error(Nil)
        // Even if the search didn't complete fully, we can still use the best move.
        // Because we always search the best move first, we will be in one of two
        // situations:
        // - The new search still thinks the best move from the previous search is
        //   preferable, in which case it's the same result
        // - The new search has found a better move than the previous best move, in
        //   which case we've found a better move to play
        // Either way, it's advantageous to use the best move from incomplete searches.
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

  // Threefold repetition is also a draw
  use <- bool.guard(
    game.is_threefold_repetition(game),
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
      case order_moves(game) {
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
            // Once we reach the limit of our depth, we continue searching until
            // we reach only quiet positions.
            0 -> {
              let eval =
                quiescent_search(game, moves, best_eval, best_opponent_move)
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
  moves: List(#(Move, Int)),
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
    [#(move, _), ..moves] -> {
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
          eval:,
          cached_positions:,
          eval_kind: hash.Floor,
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

/// Sort moves by their guessed evaluation. We return the guesses with the moves
/// in order to save iterating the list a second time. The guesses are discarded
/// after this point.
fn order_moves(game: Game) -> List(#(Move, Int)) {
  game
  |> move.legal
  |> collect_guessed_eval(game, [])
  |> list.sort(fn(a, b) { int.compare(a.1, b.1) })
}

/// Reorder already ordered moves to move the best move to the front of the list,
/// so that it will be searched first on the next iteration.
/// It has been tested, and only pushing the first move to the front of the list
/// results in better performance than sorting all moves by their calculated
/// evaluation.
fn reorder_moves(
  moves: List(#(Move, Int)),
  best_move: Move,
) -> List(#(Move, Int)) {
  let moves_without_best = list.filter(moves, fn(pair) { pair.0 != best_move })
  [#(best_move, 0), ..moves_without_best]
}

fn collect_guessed_eval(
  moves: List(Move),
  game: Game,
  acc: List(#(Move, Int)),
) -> List(#(Move, Int)) {
  case moves {
    [] -> acc
    [move, ..moves] ->
      collect_guessed_eval(moves, game, [#(move, guess_eval(game, move)), ..acc])
  }
}

/// Rate captures and promotions higher than quiet moves
const capture_promotion_bonus = 10_000

/// Guess the evaluation of a move so we can hopefully search moves in a better
/// order than random. Searching better moves first improves alpha-beta pruning,
/// allowing us to search more positions.
fn guess_eval(game: Game, move: Move) -> Int {
  let assert board.Occupied(piece:, colour:) = board.get(game.board, move.from)
    as "Invalid move trying to move empty piece"

  let moving_piece = case move {
    move.Promotion(piece:, ..) -> piece
    move.Capture(..) | move.Castle(..) | move.EnPassant(..) | move.Move(..) ->
      piece
  }

  let from_score = piece_table.piece_score(moving_piece, colour, move.from)
  let to_score = piece_table.piece_score(moving_piece, colour, move.to)

  let position_improvement = to_score - from_score
  let move_specific_score = case move {
    // TODO store information in moves so we don't have to retrieve it from the
    // board every time.
    move.Capture(..) -> {
      let assert board.Occupied(piece: captured_piece, colour: _) =
        board.get(game.board, move.to)
        as "Invalid capture moving to empty square"

      capture_promotion_bonus
      // Capturing a more valuable piece is better, and using a less valuable
      // piece to capture is usually better. However, we prioritise the value of
      // the captured piece.
      + board.piece_value(captured_piece)
      * 2
      * -board.piece_value(moving_piece)
    }
    move.EnPassant(..) -> capture_promotion_bonus
    move.Promotion(..) -> {
      // Promotions can also be captures
      let capture_value = case board.get(game.board, move.to) {
        board.Empty | board.OffBoard -> 0
        board.Occupied(piece: captured_piece, colour: _) ->
          board.piece_value(captured_piece)
          * 2
          - board.piece_value(moving_piece)
      }

      // Promoting to a more valuable piece is usually better
      capture_promotion_bonus + capture_value + board.piece_value(move.piece)
    }
    // For castling and quite moves, we can't easily predict the score
    move.Castle(..) | move.Move(..) -> 0
  }

  position_improvement + move_specific_score
}

/// Search until we find a "quiet" position, to avoid thinking a position is good
/// while really on the next move a valuable piece could be captured.
/// https://www.chessprogramming.org/Quiescence_Search
fn quiescent_search(
  game: Game,
  moves: List(#(Move, Int)),
  best_eval: Int,
  best_opponent_move: Int,
) -> Int {
  let evaluation = evaluate.evaluate(game, moves)

  use <- bool.guard(evaluation >= best_opponent_move, evaluation)

  let best_eval = case evaluation > best_eval {
    True -> evaluation
    False -> best_eval
  }

  quiescent_search_loop(game, moves, best_eval, best_opponent_move)
}

fn quiescent_search_loop(
  game: Game,
  moves: List(#(Move, Int)),
  best_eval: Int,
  best_opponent_move: Int,
) -> Int {
  case moves {
    [] -> best_eval
    // We don't need to search quiet moves
    [#(move.Move(..), _), ..moves] | [#(move.Castle(..), _), ..moves] ->
      quiescent_search_loop(game, moves, best_eval, best_opponent_move)
    [#(move, _), ..moves] -> {
      let new_game = move.apply(game, move)
      let new_moves = order_moves(new_game)
      let evaluation =
        -quiescent_search(new_game, new_moves, -best_opponent_move, -best_eval)

      use <- bool.guard(evaluation >= best_opponent_move, evaluation)
      let best_eval = case evaluation > best_eval {
        True -> evaluation
        False -> best_eval
      }
      quiescent_search_loop(game, moves, best_eval, best_opponent_move)
    }
  }
}
