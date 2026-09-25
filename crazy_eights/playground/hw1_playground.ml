(* Crazy Eights HW1 — paste into https://ocaml.org/play *)


(* Crazy Eights — HW1: types and values for the game state and moves.

   Rules used (standard "Hoyle" Crazy Eights, one hand):
   - One 52-card deck, 2-8 players. 2 players get 7 cards each, 3+ players get 5.
   - The top card of the stock is flipped to start the discard pile (an 8 is buried
     back into the stock and a new card flipped).
   - On your turn you either
     * play a card that matches the top card's rank, or the current suit, or
     * play any 8 ("crazy eight") and declare the suit the next player must follow, or
     * draw one card from the stock (you keep your turn, so you can draw until you can
       play), or
     * pass, but only when the stock is empty and you have nothing playable.
   - First player to empty their hand wins. If every player passes in a row, the game is
     blocked and the player with the fewest penalty points in hand wins
     (8 = 50, K/Q/J = 10, A = 1, number cards = face value); equal lowest scores tie.

   Players are identified by their index: 0, 1, ... *)

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
  | Tie of int list (* Blocked game where several players share the lowest score. *)

type game_state =
  { hands : card list list (* [hands] at index i is the hand of player i. *)
  ; stock : card list (* Face down. The head of the list is the top card. *)
  ; discard_pile : card list (* Face up. The head of the list is the top card. *)
  ; current_suit : suit
    (* Suit to follow. Same as the top card's suit, unless an 8 declared another. *)
  ; consecutive_passes : int (* When it reaches the number of players: game blocked. *)
  ; decision : decision
  }

type move =
  | Play of
      { card : card
      ; declared_suit : suit option (* Must be [Some _] exactly when [card] is an 8. *)
      }
  | Draw
  | Pass

(* ---------- Helpers, so the example states stay short. ---------- *)

let all_suits = [ Clubs; Diamonds; Hearts; Spades ]

let all_ranks =
  [ Ace; Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King ]
;;

let full_deck =
  Stdlib.List.concat_map
    (fun suit -> Stdlib.List.map (fun rank -> { rank; suit }) all_ranks)
    all_suits
;;

(* The cards of [full_deck] (in deck order) that are not in [excluding]. *)
let rest_of_deck ~excluding =
  Stdlib.List.filter (fun card -> not (Stdlib.List.mem card excluding)) full_deck
;;

(* ======================================================================
   Triplet 1: first move of the game (match by rank).
   ====================================================================== *)

(*=
  Discard: [5♠]   current suit: ♠   stock: 37 cards
  Player 0 (to play): 7♥ K♠ 3♦ 8♣ 5♥ Q♥ 2♣
  Player 1:           9♠ 4♦ J♣ 7♠ A♥ 10♦ 6♣
*)
let initial_hands =
  [ [ { rank = Seven; suit = Hearts }
    ; { rank = King; suit = Spades }
    ; { rank = Three; suit = Diamonds }
    ; { rank = Eight; suit = Clubs }
    ; { rank = Five; suit = Hearts }
    ; { rank = Queen; suit = Hearts }
    ; { rank = Two; suit = Clubs }
    ]
  ; [ { rank = Nine; suit = Spades }
    ; { rank = Four; suit = Diamonds }
    ; { rank = Jack; suit = Clubs }
    ; { rank = Seven; suit = Spades }
    ; { rank = Ace; suit = Hearts }
    ; { rank = Ten; suit = Diamonds }
    ; { rank = Six; suit = Clubs }
    ]
  ]
;;

let starter_card = { rank = Five; suit = Spades }

let initial_stock =
  rest_of_deck ~excluding:(starter_card :: Stdlib.List.concat initial_hands)
;;

let initial_state : game_state =
  { hands = initial_hands
  ; stock = initial_stock
  ; discard_pile = [ starter_card ]
  ; current_suit = Spades
  ; consecutive_passes = 0
  ; decision = In_progress { whose_turn = 0 }
  }
;;

(* Player 0 matches the 5♠ by rank with the 5♥; the suit to follow becomes ♥. *)
let first_move : move = Play { card = { rank = Five; suit = Hearts }; declared_suit = None }

(*=
  Discard: [5♥] 5♠   current suit: ♥   stock: 37 cards
  Player 0:           7♥ K♠ 3♦ 8♣ Q♥ 2♣
  Player 1 (to play): 9♠ 4♦ J♣ 7♠ A♥ 10♦ 6♣
*)
let state_after_first_move : game_state =
  { hands =
      [ [ { rank = Seven; suit = Hearts }
        ; { rank = King; suit = Spades }
        ; { rank = Three; suit = Diamonds }
        ; { rank = Eight; suit = Clubs }
        ; { rank = Queen; suit = Hearts }
        ; { rank = Two; suit = Clubs }
        ]
      ; Stdlib.List.nth initial_hands 1
      ]
  ; stock = initial_stock
  ; discard_pile = [ { rank = Five; suit = Hearts }; starter_card ]
  ; current_suit = Hearts
  ; consecutive_passes = 0
  ; decision = In_progress { whose_turn = 1 }
  }
;;

(* ======================================================================
   Triplet 2 (mid-game): a player with nothing playable draws a card.
   Their turn continues, and the card they drew (2♥) is playable.
   ====================================================================== *)

(* Cards below the top of the discard pile: everything not visible elsewhere. *)
let discard_pile ~top ~hands ~stock =
  top @ rest_of_deck ~excluding:(top @ Stdlib.List.concat hands @ stock)
;;

let hands_before_draw =
  [ [ { rank = King; suit = Spades }
    ; { rank = Eight; suit = Clubs }
    ; { rank = Three; suit = Clubs }
    ]
  ; [ { rank = Nine; suit = Spades }
    ; { rank = Four; suit = Diamonds }
    ; { rank = Jack; suit = Clubs }
    ]
  ]
;;

let stock_before_draw =
  [ { rank = Two; suit = Hearts }
  ; { rank = King; suit = Diamonds }
  ; { rank = Ten; suit = Clubs }
  ]
;;

let discard_before_draw =
  discard_pile
    ~top:[ { rank = Five; suit = Hearts } ]
    ~hands:hands_before_draw
    ~stock:stock_before_draw
;;

(*=
  Discard: [5♥] ...   current suit: ♥   stock: 2♥ K♦ 10♣ (top first)
  Player 0:           K♠ 8♣ 3♣
  Player 1 (to play): 9♠ 4♦ J♣      <- no heart, no 5, no 8
*)
let state_before_draw : game_state =
  { hands = hands_before_draw
  ; stock = stock_before_draw
  ; discard_pile = discard_before_draw
  ; current_suit = Hearts
  ; consecutive_passes = 0
  ; decision = In_progress { whose_turn = 1 }
  }
;;

let draw_move : move = Draw

(*=
  Discard: [5♥] ...   current suit: ♥   stock: K♦ 10♣
  Player 0:           K♠ 8♣ 3♣
  Player 1 (to play): 2♥ 9♠ 4♦ J♣   <- drew 2♥, still their turn
*)
let state_after_draw : game_state =
  { hands =
      [ Stdlib.List.nth hands_before_draw 0
      ; { rank = Two; suit = Hearts } :: Stdlib.List.nth hands_before_draw 1
      ]
  ; stock = [ { rank = King; suit = Diamonds }; { rank = Ten; suit = Clubs } ]
  ; discard_pile = discard_before_draw
  ; current_suit = Hearts
  ; consecutive_passes = 0
  ; decision = In_progress { whose_turn = 1 }
  }
;;

(* ======================================================================
   Triplet 3 (mid-game): a crazy eight. Player 0 has no heart and no 2, so they
   play the 8♣ and declare spades (they are holding the K♠).
   Continues from triplet 2, after player 1 played the 2♥.
   ====================================================================== *)

let hands_before_eight =
  [ Stdlib.List.nth hands_before_draw 0; Stdlib.List.nth hands_before_draw 1 ]
;;

let stock_before_eight = [ { rank = King; suit = Diamonds }; { rank = Ten; suit = Clubs } ]

let discard_before_eight =
  discard_pile
    ~top:[ { rank = Two; suit = Hearts }; { rank = Five; suit = Hearts } ]
    ~hands:hands_before_eight
    ~stock:stock_before_eight
;;

(*=
  Discard: [2♥] 5♥ ...   current suit: ♥   stock: K♦ 10♣
  Player 0 (to play): K♠ 8♣ 3♣
  Player 1:           9♠ 4♦ J♣
*)
let state_before_eight : game_state =
  { hands = hands_before_eight
  ; stock = stock_before_eight
  ; discard_pile = discard_before_eight
  ; current_suit = Hearts
  ; consecutive_passes = 0
  ; decision = In_progress { whose_turn = 0 }
  }
;;

let eight_move : move =
  Play { card = { rank = Eight; suit = Clubs }; declared_suit = Some Spades }
;;

(*=
  Discard: [8♣] 2♥ 5♥ ...   current suit: ♠ (declared)   stock: K♦ 10♣
  Player 0:           K♠ 3♣
  Player 1 (to play): 9♠ 4♦ J♣
*)
let state_after_eight : game_state =
  { hands =
      [ [ { rank = King; suit = Spades }; { rank = Three; suit = Clubs } ]
      ; Stdlib.List.nth hands_before_eight 1
      ]
  ; stock = stock_before_eight
  ; discard_pile = { rank = Eight; suit = Clubs } :: discard_before_eight
  ; current_suit = Spades
  ; consecutive_passes = 0
  ; decision = In_progress { whose_turn = 1 }
  }
;;

(* ======================================================================
   Triplet 4 (end of game): a blocked game. The stock is empty, player 1
   already passed, and player 0 cannot play either, so they pass too.
   Everybody passed in a row, so the lowest hand wins:
   player 0 has K♥ + 2♦ = 10 + 2 = 12 points, player 1 has Q♦ + 9♥ = 19.
   ====================================================================== *)

let hands_blocked =
  [ [ { rank = King; suit = Hearts }; { rank = Two; suit = Diamonds } ]
  ; [ { rank = Queen; suit = Diamonds }; { rank = Nine; suit = Hearts } ]
  ]
;;

let discard_blocked =
  discard_pile ~top:[ { rank = Four; suit = Spades } ] ~hands:hands_blocked ~stock:[]
;;

(*=
  Discard: [4♠] ...   current suit: ♠   stock: (empty)   consecutive passes: 1
  Player 0 (to play): K♥ 2♦
  Player 1:           Q♦ 9♥
*)
let state_before_blocked : game_state =
  { hands = hands_blocked
  ; stock = []
  ; discard_pile = discard_blocked
  ; current_suit = Spades
  ; consecutive_passes = 1
  ; decision = In_progress { whose_turn = 0 }
  }
;;

let pass_move : move = Pass

let state_after_blocked : game_state =
  { state_before_blocked with consecutive_passes = 2; decision = Winner 0 }
;;

(* ======================================================================
   Triplet 5 (end of game): player 1 plays their last card and wins.
   Player 0 just played the 8♣ and declared spades, hoping player 1 had
   none. But player 1 is holding exactly one card: the J♠.
   ====================================================================== *)

let hands_before_win =
  [ [ { rank = King; suit = Spades }; { rank = Three; suit = Clubs } ]
  ; [ { rank = Jack; suit = Spades } ]
  ]
;;

let stock_before_win = [ { rank = Ten; suit = Clubs } ]

let discard_before_win =
  discard_pile
    ~top:[ { rank = Eight; suit = Clubs }; { rank = Two; suit = Hearts } ]
    ~hands:hands_before_win
    ~stock:stock_before_win
;;

(*=
  Discard: [8♣] 2♥ ...   current suit: ♠ (declared)   stock: 10♣
  Player 0:           K♠ 3♣
  Player 1 (to play): J♠
*)
let state_before_win : game_state =
  { hands = hands_before_win
  ; stock = stock_before_win
  ; discard_pile = discard_before_win
  ; current_suit = Spades
  ; consecutive_passes = 0
  ; decision = In_progress { whose_turn = 1 }
  }
;;

let winning_move : move =
  Play { card = { rank = Jack; suit = Spades }; declared_suit = None }
;;

(*=
  Discard: [J♠] 8♣ 2♥ ...   Player 1 wins!
  Player 0: K♠ 3♣
  Player 1: (empty)
*)
let terminal_state : game_state =
  { hands = [ Stdlib.List.nth hands_before_win 0; [] ]
  ; stock = stock_before_win
  ; discard_pile = { rank = Jack; suit = Spades } :: discard_before_win
  ; current_suit = Spades
  ; consecutive_passes = 0
  ; decision = Winner 1
  }
;;
