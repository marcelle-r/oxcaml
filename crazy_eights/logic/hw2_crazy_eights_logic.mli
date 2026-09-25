open! Core

(** Crazy Eights game logic. See hw1.ml for the rules. *)

module Suit : sig
  type t =
    | Clubs
    | Diamonds
    | Hearts
    | Spades
  [@@deriving sexp, compare, equal, enumerate]
end

module Rank : sig
  type t =
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
  [@@deriving sexp, compare, equal, enumerate]

  (** Penalty points of a card left in hand when the game is blocked:
      8 = 50, K/Q/J = 10, A = 1, number cards = face value. *)
  val points : t -> int
end

module Card : sig
  type t =
    { rank : Rank.t
    ; suit : Suit.t
    }
  [@@deriving sexp, compare, equal]

  include Comparable.S with type t := t

  (** All 52 cards, clubs first, Ace to King within each suit. *)
  val full_deck : t list

  val shuffled_deck : Random.State.t -> t list
end

module Move : sig
  type t =
    | Play of
        { card : Card.t
        ; declared_suit : Suit.t option (** [Some _] exactly when [card] is an 8. *)
        }
    | Draw
    | Pass
  [@@deriving sexp, compare, equal]
end

module Decision : sig
  type t =
    | In_progress of { whose_turn : int }
    | Winner of int
    | Tie of int list
  [@@deriving sexp, compare, equal]

  val is_game_over : t -> bool
end

module Game_state : sig
  type t =
    { hands : Card.t list list (** Index i is the hand of player i. *)
    ; stock : Card.t list (** Head is the top card. *)
    ; discard_pile : Card.t list (** Head is the top card; never empty. *)
    ; current_suit : Suit.t
    ; consecutive_passes : int
    ; decision : Decision.t
    ; last_move : (int * Move.t) option (** Who made the last move and what it was. *)
    }
  [@@deriving sexp, compare, equal]

  val min_players : int
  val max_players : int

  module Create_error : sig
    type t =
      | Invalid_number_of_players
      | Deck_is_not_a_full_deck
    [@@deriving sexp, compare, equal]
  end

  (** Deals a new game from [deck] (already shuffled; head is the top card). 2 players
      get 7 cards each, 3 or more get 5. Cards are dealt one at a time starting with
      player 0, who also plays first. The next card starts the discard pile; an 8 is
      moved to the bottom of the stock and the next card is flipped instead. *)
  val create : num_players:int -> deck:Card.t list -> (t, Create_error.t list) Result.t

  val num_players : t -> int
  val top_card : t -> Card.t
  val hand_points : Card.t list -> int

  (** Whether [card] may be put on the discard pile (ignoring whose hand it's in). *)
  val can_play_card : t -> Card.t -> bool

  module Move_error : sig
    type t =
      | Game_is_over
      | Card_not_in_hand
      | Card_does_not_match
      | Eight_needs_declared_suit
      | Only_eights_declare_suit
      | Stock_is_empty
      | Cannot_pass
    [@@deriving sexp, compare, equal]
  end

  (** All legal moves of the player whose turn it is ([[]] if the game is over).
      An 8 appears once per suit that can be declared. *)
  val get_all_moves : t -> Move.t list

  (** Applies a move by the player whose turn it is. *)
  val make_move : t -> Move.t -> (t, Move_error.t) Result.t
end
