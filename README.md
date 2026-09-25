# Crazy Eights in OCaml

Marcelle de Matos Ribeiro, Multiplayer Games in OCaml (Columbia, Fall 2026).
Built on the course template: https://github.com/yoav-zibin/oxcaml

## Rules (Hoyle Crazy Eights, one hand)

- One 52-card deck, 2 to 8 players. With 2 players each gets 7 cards; with 3 or more, 5 each.
- The next card from the stock starts the discard pile. If it's an 8, it goes to the bottom of the stock and the next card is flipped instead.
- On your turn you do one of these:
  - play a card that matches the top card's **rank** or the **suit to follow**
  - play any **8** and **declare** the suit the next player must follow
  - **draw** one card from the stock. Your turn continues, so you can keep drawing until you can play.
  - **pass**, but only if the stock is empty and you have nothing you can play
- The first player to empty their hand wins. If every player passes in a row, the game is blocked. The player with the fewest penalty points left in hand wins (8 = 50, K/Q/J = 10, A = 1, number cards = face value). If the lowest score is shared, the game is a tie.

## Layout

| File | What it is |
|---|---|
| `logic/hw1.ml(i)` | **HW1**: plain types for the state and moves, plus 5 state → move → state triplets |
| `playground/hw1_playground.ml` | The same HW1 code without `open! Core`, so it runs in https://ocaml.org/play |
| `logic/hw2_crazy_eights_logic.ml(i)` | **HW2**: `Game_state.create`, `get_all_moves`, `make_move` (with Core modules) |
| `test/hw2_crazy_eights_logic_test.ml` | Checks each HW1 triplet against `make_move`, tests illegal moves and `create`, and plays 1000 random games checking that no card is ever lost or duplicated |

## Dev environment (GitHub Codespaces)

1. Create a GitHub repo named `oxcaml` and push this folder to it.
2. On GitHub, go to **Code → Codespaces → +**. The first build takes 20 to 40 minutes.
3. In the Codespace terminal, run:

```shell
opam init -a --disable-sandboxing --yes --bare && \
  opam update -a && \
  opam switch create 4.14.0 --yes && \
  eval $(opam env --switch 4.14.0) && \
  opam install --yes ocamlformat merlin ocaml-lsp-server bonsai
```

## Build and test

```shell
eval $(opam env --switch 4.14.0)
dune build @runtest        # builds and runs every test (no output = all pass)
dune build @runtest --watch
dune promote               # accept new expect-test output
dune fmt                   # format the code
```
