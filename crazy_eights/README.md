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
| `test/hw2_crazy_eights_logic_test.ml` | Checks each HW1 example against `make_move`, every illegal-move error, `create`, and 1000 random games |

## Test

From the repository root:

```shell
dune build @crazy_eights/runtest   # no output = all tests pass
```
