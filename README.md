# Negamax and variations

### What iz?
Negamax is a variation on minimax which is an algorithm for determining the best move for a given player and game state in 2-player zero-sum game.

### How üz?
`MiniMax.hs` provides a game class `MiniMaxable`,
```
class MiniMaxable game where
  score :: (Real value) => Bool -> game -> value
  terminal :: game -> Bool
  allMoves ::  Bool -> game -> [game]
```

and 3 function, `negamax`, `negamax'`, and `negamaxalpha`.
`negamax` and `negamax'` are equivalent implementations of [negamax](https://en.wikipedia.org/wiki/Negamax), and `negamaxalpha` is my take on **alpha/beta** pruning with negamax.

If you can represent a 2-player zero-sum game in terms of the 3 functions required by `MiniMaxable`, the 3 version of negamax included in `MiniMax.hs` will tell you which moves are optimal.

### Caution
The algorithms included here do not parameterize depth information and therefore will explore the full depth of the game tree. This means that if you have a particularly deep game tree (cough chess), the included algorithms will run for longer than you are willing to wait.
