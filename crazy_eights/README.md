# Crazy Eights

**Marcelle de Matos Ribeiro**, Multiplayer Games in OCaml (Columbia, Fall 2026). My individual game, built next to the course's TicTacToe example.

## Rules (Hoyle Crazy Eights, one hand)

- One 52-card deck, 2 to 8 players. With 2 players each gets 7 cards; with 3 or more, 5 each.
- The next card from the stock starts the discard pile. If it's an 8, it goes to the bottom of the stock and the next card is flipped instead.
- On your turn, do one of these:
  - play a card that matches the top card's **rank** or the **suit to follow**
  - play any **8** and **declare** the suit the next player must follow
  - **draw** one card from the stock (your turn continues, so you can keep drawing)
  - **pass**, only when the stock is empty and you have nothing you can play
- The first player to empty their hand wins. If every player passes in a row, the game is blocked: the fewest penalty points left in hand wins (8 = 50, K/Q/J = 10, A = 1, number cards = face value). A shared lowest score is a tie.

## Files

| File | What it is |
|---|---|
| `logic/hw1.ml(i)` | **HW1**: types for the state and moves, plus 5 state → move → state examples (first move, draw, crazy eight, blocked game, win) |
| `playground/hw1_playground.ml` | The same HW1 code without `open! Core`, for https://ocaml.org/play |
| `logic/hw2_crazy_eights_logic.ml(i)` | **HW2**: `Game_state.create`, `get_all_moves`, `make_move` |
| `test/hw3_crazy_eights_logic_test.ml` | **HW3** (grew out of the HW2 tests): the 5 HW1 triplets; every HW1 state with its legal moves; legal transitions (same suit, same rank, crazy eights, drawing, passing, blocked games, ties, winning, turn order); every illegal-move error; `create`; and random exploration of the state space (see below) |

## How the tests explore the state space (HW3)

- **1000 random games** with 2 to 8 players: every game ends with a winner or a tie, and no card is ever lost or duplicated.
- **300 more random games where every single step is checked:**
  - every move from `get_all_moves` is accepted, and none is listed twice
  - 25 random moves out of all 262 possible ones (any card, with or without a declared suit, draw, pass) are tried at each step: `make_move` must accept exactly the ones `get_all_moves` lists
  - the chosen move changes the state the way the rules say (card on top, right suit to follow, right player next, stock and pass counts)
  - there are no legal moves exactly when the game is over

  This checks about 20,000 moves and 500,000 illegal attempts, and the games end in all three ways (won by playing the last card, blocked with a lowest hand, blocked with a tie).

## Test

From the repository root:

```shell
dune build @crazy_eights/runtest   # no output = all tests pass
```
