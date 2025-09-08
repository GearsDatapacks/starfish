# Starfish

A chess library for Gleam!

### TODO list
- [x] Implement full legal move generation
- [x] Implement basic minimax search
- [x] Use zobrist hashing to cache positions when searching
- [x] Use piece tables to give more weight to certain squares in evaluation
- [x] Implement a system to test the performance of the generated moves
- [x] Maybe have some way to perform a search using iterative deepening?
- [x] Order moves before searching via heuristics to improve alpha-beta pruning
- [x] Continue searching past regular depth when captures are available (https://www.chessprogramming.org/Quiescence_Search)
- [x] Incrementally update game information such as zobrist hash, material count, and other evaluation information
- [ ] Improve static evaluation using pawn structure
- [ ] Improve endgame play by encouraging the king to the centre of the board and encouraging pawns to promote
- [ ] Use an opening database to improve opening play
- [ ] Cache transposition table across searches
- [ ] Search deeper for certain "more interesting" moves: https://www.chessprogramming.org/Extensions
- [ ] Precalculate various pieces of data to avoid calculations when searching or evaluating (e.g. distance to edge)
- [ ] Use bitboards for efficient calculations
