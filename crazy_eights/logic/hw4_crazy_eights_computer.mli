open! Core
open! Hw2_crazy_eights_logic

(** HW4: computer opponents for Crazy Eights.

    Both players only use what the player to move is allowed to know: their own hand, the
    discard pile, the suit to follow, and how many cards everyone else holds. *)

(** The trivial opponent: a uniformly random legal move. [None] when the game is over. *)
val random_move : Game_state.t -> random_state:Random.State.t -> Move.t option

(** A quick rule of thumb, used inside the simulations of [smart_move]: play a matching
    card that isn't an 8 (the one worth the most penalty points), otherwise an 8
    declaring the suit you hold most of, otherwise draw, otherwise pass. *)
val greedy_move : Game_state.t -> random_state:Random.State.t -> Move.t option

(** The better opponent: Monte Carlo search with "determinization".

    Until the time runs out, it repeatedly
    1. guesses the hidden cards: shuffles every card it can't see and deals them to the
       other players and the stock, keeping everyone's real number of cards,
    2. tries one of its legal moves in that guessed game,
    3. plays the rest of the game quickly with [greedy_move] for everyone.

    After trying every move once, it spends more simulations on the moves that are
    winning so far (the UCB1 rule), and finally picks the move with the best win rate.

    [time_limit] defaults to 2 seconds. [max_simulations] (default: no limit) stops it
    earlier, which makes tests fast and repeatable. *)
val smart_move
  :  ?time_limit:Time_ns.Span.t
  -> ?max_simulations:int
  -> Game_state.t
  -> random_state:Random.State.t
  -> Move.t option
