open! Core

type suit =
  | Clubs
  | Diamonds
  | Hearts
  | Spades

type rank =
  | Ace
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King

type card =
  { rank : rank
  ; suit : suit
  }

type decision =
  | In_progress of { whose_turn : int }
  | Winner of int
  | Tie of int list

type game_state =
  { hands : card list list
  ; stock : card list
  ; discard_pile : card list
  ; current_suit : suit
  ; consecutive_passes : int
  ; decision : decision
  }

type move =
  | Play of
      { card : card
      ; declared_suit : suit option
      }
  | Draw
  | Pass

val full_deck : card list

(* Triplet 1: first move of the game. *)
val initial_state : game_state
val first_move : move
val state_after_first_move : game_state

(* Triplet 2: drawing a card. *)
val state_before_draw : game_state
val draw_move : move
val state_after_draw : game_state

(* Triplet 3: playing a crazy eight and declaring a suit. *)
val state_before_eight : game_state
val eight_move : move
val state_after_eight : game_state

(* Triplet 4: blocked game (everyone passes), lowest hand wins. *)
val state_before_blocked : game_state
val pass_move : move
val state_after_blocked : game_state

(* Triplet 5: playing your last card wins. *)
val state_before_win : game_state
val winning_move : move
val terminal_state : game_state
