open! Core
open Crazy_eights_logic_library
open Hw2_crazy_eights_logic

(* HW3: tests for the Crazy Eights logic.

   1. The five HW1 triplets: each [make_move from move] must give exactly [to_].
   2. Every HW1 state: what the player to move is allowed to do.
   3. Interesting legal transitions (matching suit or rank, crazy eights, drawing,
      passing, blocked games, ties, winning, turn order).
   4. Every illegal move error.
   5. Random exploration of the state space: thousands of random games where every
      step is checked against the rules. *)

(* ---------- Converting the HW1 values into HW2 values. ---------- *)

let suit_of_hw1 (suit : Hw1.suit) : Suit.t =
  match suit with
  | Clubs -> Clubs
  | Diamonds -> Diamonds
  | Hearts -> Hearts
  | Spades -> Spades
;;

let rank_of_hw1 (rank : Hw1.rank) : Rank.t =
  match rank with
  | Ace -> Ace
  | Two -> Two
  | Three -> Three
  | Four -> Four
  | Five -> Five
  | Six -> Six
  | Seven -> Seven
  | Eight -> Eight
  | Nine -> Nine
  | Ten -> Ten
  | Jack -> Jack
  | Queen -> Queen
  | King -> King
;;

let card_of_hw1 ({ rank; suit } : Hw1.card) : Card.t =
  { rank = rank_of_hw1 rank; suit = suit_of_hw1 suit }
;;

let cards_of_hw1 = List.map ~f:card_of_hw1

let move_of_hw1 (move : Hw1.move) : Move.t =
  match move with
  | Play { card; declared_suit } ->
    Play
      { card = card_of_hw1 card; declared_suit = Option.map declared_suit ~f:suit_of_hw1 }
  | Draw -> Draw
  | Pass -> Pass
;;

let decision_of_hw1 (decision : Hw1.decision) : Decision.t =
  match decision with
  | In_progress { whose_turn } -> In_progress { whose_turn }
  | Winner player -> Winner player
  | Tie players -> Tie players
;;

(* Turns a whole HW1 game state into the HW2 version, so the HW2 rules can use it. *)
let state_of_hw1 (state : Hw1.game_state) : Game_state.t =
  { hands = List.map state.hands ~f:cards_of_hw1
  ; stock = cards_of_hw1 state.stock
  ; discard_pile = cards_of_hw1 state.discard_pile
  ; current_suit = suit_of_hw1 state.current_suit
  ; consecutive_passes = state.consecutive_passes
  ; decision = decision_of_hw1 state.decision
  ; last_move = None
  }
;;

(* Every card in the game: all the hands, the stock and the discard pile. *)
let all_cards (t : Game_state.t) = List.concat t.hands @ t.stock @ t.discard_pile

(* True when the game still has exactly the 52 cards: none lost, none copied. *)
let is_one_full_deck t =
  List.equal
    Card.equal
    (List.sort (all_cards t) ~compare:Card.compare)
    (List.sort Card.full_deck ~compare:Card.compare)
;;

(* ---------- Short printing, so the expected outputs stay readable. ---------- *)

let rank_to_string (rank : Rank.t) =
  match rank with
  | Ace -> "A"
  | Two -> "2"
  | Three -> "3"
  | Four -> "4"
  | Five -> "5"
  | Six -> "6"
  | Seven -> "7"
  | Eight -> "8"
  | Nine -> "9"
  | Ten -> "10"
  | Jack -> "J"
  | Queen -> "Q"
  | King -> "K"
;;

let suit_to_string (suit : Suit.t) =
  match suit with
  | Clubs -> "C"
  | Diamonds -> "D"
  | Hearts -> "H"
  | Spades -> "S"
;;

let card_to_string (card : Card.t) = rank_to_string card.rank ^ suit_to_string card.suit
let cards_to_string cards = List.map cards ~f:card_to_string |> String.concat ~sep:" "

let move_to_string (move : Move.t) =
  match move with
  | Play { card; declared_suit = None } -> "play " ^ card_to_string card
  | Play { card; declared_suit = Some suit } ->
    "play " ^ card_to_string card ^ " and declare " ^ suit_to_string suit
  | Draw -> "draw"
  | Pass -> "pass"
;;

(* Prints the game in a short way: each hand, the top card, the suit to follow, the
   stock size, and whose turn it is (or who won). *)
let print_state (t : Game_state.t) =
  List.iteri t.hands ~f:(fun player hand ->
    printf "player %d: %s\n" player (cards_to_string hand));
  printf
    "top: %s | suit: %s | stock: %d cards | passes in a row: %d\n"
    (card_to_string (Game_state.top_card t))
    (suit_to_string t.current_suit)
    (List.length t.stock)
    t.consecutive_passes;
  print_s [%sexp (t.decision : Decision.t)]
;;

(* Prints every move the current player is allowed to make, one per line. *)
let print_moves t =
  match Game_state.get_all_moves t with
  | [] -> print_endline "(no moves: the game is over)"
  | moves -> List.iter moves ~f:(fun move -> print_endline (move_to_string move))
;;

(* Shortcuts to write cards and moves in the tests, e.g. [play Two Hearts]. *)
let card rank suit : Card.t = { rank; suit }
let play rank suit : Move.t = Play { card = card rank suit; declared_suit = None }

let play_eight suit ~declare : Move.t =
  Play { card = card Eight suit; declared_suit = Some declare }
;;

(* Builds a state for a scenario. Whatever cards are not in the hands, the discard pile
   or [stock] (when given) become the stock, so the state is always one full deck. *)
let make_state ?stock ?current_suit ?(consecutive_passes = 0) ?(whose_turn = 0) ~hands
    ~discard_pile ()
  : Game_state.t
  =
  let used = List.concat hands @ discard_pile in
  let stock =
    match stock with
    | Some stock -> stock
    | None ->
      List.filter Card.full_deck ~f:(fun c -> not (List.mem used c ~equal:Card.equal))
  in
  let top : Card.t = List.hd_exn discard_pile in
  { hands
  ; stock
  ; discard_pile
  ; current_suit = Option.value current_suit ~default:top.suit
  ; consecutive_passes
  ; decision = In_progress { whose_turn }
  ; last_move = None
  }
;;

(* The cards that are nowhere yet, used to build states with a nearly empty stock. *)
let rest_of_deck (t : Game_state.t) =
  List.filter Card.full_deck ~f:(fun c ->
    not (List.mem (all_cards t) c ~equal:Card.equal))
;;

let ok_exn result =
  match result with
  | Ok t -> t
  | Error error -> raise_s [%message "unexpected error" (error : Game_state.Move_error.t)]
;;

let move_and_print t move =
  match Game_state.make_move t move with
  | Error error -> print_s [%message "Error" (error : Game_state.Move_error.t)]
  | Ok next -> print_state next
;;

(* Tries a move and only prints "Ok" or the reason it was refused. *)
let try_move t move =
  match Game_state.make_move t move with
  | Ok _ -> print_endline "Ok"
  | Error error -> print_s [%sexp (error : Game_state.Move_error.t)]
;;

(* ======================================================================
   1. The HW1 triplets
   ====================================================================== *)

(* The HW1 check: start from my HW1 "before" state, make my HW1 move, and the result
   must be exactly my HW1 "after" state ([last_move] is ignored). *)
let check_triplet ~from ~move ~to_ =
  let from = state_of_hw1 from in
  let expected = state_of_hw1 to_ in
  let move = move_of_hw1 move in
  if not (is_one_full_deck from) then print_endline "from-state is not exactly one deck";
  if not (is_one_full_deck expected)
  then print_endline "to-state is not exactly one deck";
  if not (List.mem (Game_state.get_all_moves from) move ~equal:Move.equal)
  then print_endline "move is missing from get_all_moves";
  match Game_state.make_move from move with
  | Error error -> print_s [%message "make_move failed" (error : Game_state.Move_error.t)]
  | Ok actual ->
    let actual = { actual with last_move = None } in
    if Game_state.equal actual expected
    then print_endline "OK"
    else print_s [%message "Mismatch" (actual : Game_state.t) (expected : Game_state.t)]
;;

let%expect_test "HW1 triplet 1: first move" =
  check_triplet
    ~from:Hw1.initial_state
    ~move:Hw1.first_move
    ~to_:Hw1.state_after_first_move;
  [%expect {| OK |}]
;;

let%expect_test "HW1 triplet 2: draw" =
  check_triplet ~from:Hw1.state_before_draw ~move:Hw1.draw_move ~to_:Hw1.state_after_draw;
  [%expect {| OK |}]
;;

let%expect_test "HW1 triplet 3: crazy eight" =
  check_triplet
    ~from:Hw1.state_before_eight
    ~move:Hw1.eight_move
    ~to_:Hw1.state_after_eight;
  [%expect {| OK |}]
;;

let%expect_test "HW1 triplet 4: blocked game" =
  check_triplet
    ~from:Hw1.state_before_blocked
    ~move:Hw1.pass_move
    ~to_:Hw1.state_after_blocked;
  [%expect {| OK |}]
;;

let%expect_test "HW1 triplet 5: playing the last card wins" =
  check_triplet
    ~from:Hw1.state_before_win
    ~move:Hw1.winning_move
    ~to_:Hw1.terminal_state;
  [%expect {| OK |}]
;;

(* ======================================================================
   2. Every HW1 state, and what the player to move can do there
   ====================================================================== *)

(* Shows a HW1 state, checks it has all 52 cards, and lists the moves allowed there. *)
let print_hw1_state state =
  let t = state_of_hw1 state in
  print_state t;
  printf "one full deck: %b\n" (is_one_full_deck t);
  print_endline "legal moves:";
  print_moves t
;;

let%expect_test "HW1 initial_state" =
  print_hw1_state Hw1.initial_state;
  [%expect {|
    player 0: 7H KS 3D 8C 5H QH 2C
    player 1: 9S 4D JC 7S AH 10D 6C
    top: 5S | suit: S | stock: 37 cards | passes in a row: 0
    (In_progress (whose_turn 0))
    one full deck: true
    legal moves:
    play KS
    play 8C and declare C
    play 8C and declare D
    play 8C and declare H
    play 8C and declare S
    play 5H
    draw |}]
;;

let%expect_test "HW1 state_after_first_move" =
  print_hw1_state Hw1.state_after_first_move;
  [%expect {|
    player 0: 7H KS 3D 8C QH 2C
    player 1: 9S 4D JC 7S AH 10D 6C
    top: 5H | suit: H | stock: 37 cards | passes in a row: 0
    (In_progress (whose_turn 1))
    one full deck: true
    legal moves:
    play AH
    draw |}]
;;

let%expect_test "HW1 state_before_draw" =
  print_hw1_state Hw1.state_before_draw;
  [%expect {|
    player 0: KS 8C 3C
    player 1: 9S 4D JC
    top: 5H | suit: H | stock: 3 cards | passes in a row: 0
    (In_progress (whose_turn 1))
    one full deck: true
    legal moves:
    draw |}]
;;

let%expect_test "HW1 state_after_draw" =
  print_hw1_state Hw1.state_after_draw;
  [%expect {|
    player 0: KS 8C 3C
    player 1: 2H 9S 4D JC
    top: 5H | suit: H | stock: 2 cards | passes in a row: 0
    (In_progress (whose_turn 1))
    one full deck: true
    legal moves:
    play 2H
    draw |}]
;;

let%expect_test "HW1 state_before_eight" =
  print_hw1_state Hw1.state_before_eight;
  [%expect {|
    player 0: KS 8C 3C
    player 1: 9S 4D JC
    top: 2H | suit: H | stock: 2 cards | passes in a row: 0
    (In_progress (whose_turn 0))
    one full deck: true
    legal moves:
    play 8C and declare C
    play 8C and declare D
    play 8C and declare H
    play 8C and declare S
    draw |}]
;;

let%expect_test "HW1 state_after_eight" =
  print_hw1_state Hw1.state_after_eight;
  [%expect {|
    player 0: KS 3C
    player 1: 9S 4D JC
    top: 8C | suit: S | stock: 2 cards | passes in a row: 0
    (In_progress (whose_turn 1))
    one full deck: true
    legal moves:
    play 9S
    draw |}]
;;

let%expect_test "HW1 state_before_blocked" =
  print_hw1_state Hw1.state_before_blocked;
  [%expect {|
    player 0: KH 2D
    player 1: QD 9H
    top: 4S | suit: S | stock: 0 cards | passes in a row: 1
    (In_progress (whose_turn 0))
    one full deck: true
    legal moves:
    pass |}]
;;

let%expect_test "HW1 state_after_blocked" =
  print_hw1_state Hw1.state_after_blocked;
  [%expect {|
    player 0: KH 2D
    player 1: QD 9H
    top: 4S | suit: S | stock: 0 cards | passes in a row: 2
    (Winner 0)
    one full deck: true
    legal moves:
    (no moves: the game is over) |}]
;;

let%expect_test "HW1 state_before_win" =
  print_hw1_state Hw1.state_before_win;
  [%expect {|
    player 0: KS 3C
    player 1: JS
    top: 8C | suit: S | stock: 1 cards | passes in a row: 0
    (In_progress (whose_turn 1))
    one full deck: true
    legal moves:
    play JS
    draw |}]
;;

let%expect_test "HW1 terminal_state" =
  print_hw1_state Hw1.terminal_state;
  [%expect {|
    player 0: KS 3C
    player 1:
    top: JS | suit: S | stock: 1 cards | passes in a row: 0
    (Winner 1)
    one full deck: true
    legal moves:
    (no moves: the game is over) |}]
;;

(* ======================================================================
   3. Interesting legal transitions
   ====================================================================== *)

(* Top card 9♥. Player 0 has a heart, another 9, an 8 and a card that doesn't match. *)
let nine_of_hearts_state =
  make_state
    ~hands:
      [ [ card Two Hearts; card Nine Clubs; card Eight Spades; card King Clubs ]
      ; [ card Three Diamonds; card Four Diamonds; card Five Diamonds ]
      ; [ card Six Clubs; card Seven Clubs; card Jack Spades ]
      ]
    ~discard_pile:[ card Nine Hearts ]
    ()
;;

let%expect_test "legal moves: same suit, same rank, an 8 (once per suit), and draw" =
  print_moves nine_of_hearts_state;
  [%expect {|
    play 2H
    play 9C
    play 8S and declare C
    play 8S and declare D
    play 8S and declare H
    play 8S and declare S
    draw |}]
;;

let%expect_test "playing a card of the same suit" =
  move_and_print nine_of_hearts_state (play Two Hearts);
  [%expect {|
    player 0: 9C 8S KC
    player 1: 3D 4D 5D
    player 2: 6C 7C JS
    top: 2H | suit: H | stock: 41 cards | passes in a row: 0
    (In_progress (whose_turn 1)) |}]
;;

let%expect_test "playing a card of the same rank changes the suit to follow" =
  move_and_print nine_of_hearts_state (play Nine Clubs);
  [%expect {|
    player 0: 2H 8S KC
    player 1: 3D 4D 5D
    player 2: 6C 7C JS
    top: 9C | suit: C | stock: 41 cards | passes in a row: 0
    (In_progress (whose_turn 1)) |}]
;;

let%expect_test "a crazy eight sets the declared suit, even a different one" =
  move_and_print nine_of_hearts_state (play_eight Spades ~declare:Diamonds);
  [%expect {|
    player 0: 2H 9C KC
    player 1: 3D 4D 5D
    player 2: 6C 7C JS
    top: 8S | suit: D | stock: 41 cards | passes in a row: 0
    (In_progress (whose_turn 1)) |}]
;;

let%expect_test "after an 8, the next player must follow the declared suit" =
  let t =
    ok_exn
      (Game_state.make_move nine_of_hearts_state (play_eight Spades ~declare:Diamonds))
  in
  (* Player 1 has only diamonds, so all of them are playable now. *)
  print_moves t;
  [%expect {|
    play 3D
    play 4D
    play 5D
    draw |}]
;;

let%expect_test "drawing keeps your turn, and you may draw even if you could play" =
  move_and_print nine_of_hearts_state Draw;
  [%expect {|
    player 0: AC 2H 9C 8S KC
    player 1: 3D 4D 5D
    player 2: 6C 7C JS
    top: 9H | suit: H | stock: 40 cards | passes in a row: 0
    (In_progress (whose_turn 0)) |}]
;;

let%expect_test "drawing several times in a row" =
  let t = ok_exn (Game_state.make_move nine_of_hearts_state Draw) in
  let t = ok_exn (Game_state.make_move t Draw) in
  let t = ok_exn (Game_state.make_move t Draw) in
  printf
    "player 0 has %d cards, stock has %d, still player 0's turn: %b\n"
    (List.length (List.hd_exn t.hands))
    (List.length t.stock)
    (Decision.equal t.decision (In_progress { whose_turn = 0 }));
  [%expect {| player 0 has 7 cards, stock has 38, still player 0's turn: true |}]
;;

let%expect_test "the turn goes back to player 0 after the last player" =
  (* Player 2's cards (6♣ 7♣ J♠) don't match the 9♥, so drawing is the only move. *)
  let t = { nine_of_hearts_state with decision = In_progress { whose_turn = 2 } } in
  print_moves t;
  [%expect {| draw |}];
  let t =
    make_state
      ~hands:
        [ [ card Two Hearts ]
        ; [ card Three Diamonds ]
        ; [ card Six Hearts; card Ace Clubs ]
        ]
      ~discard_pile:[ card Nine Hearts ]
      ~whose_turn:2
      ()
  in
  move_and_print t (play Six Hearts);
  [%expect {|
    player 0: 2H
    player 1: 3D
    player 2: AC
    top: 6H | suit: H | stock: 47 cards | passes in a row: 0
    (In_progress (whose_turn 0)) |}]
;;

(* The stock is empty and nobody can play: everyone has to pass. *)
let blocked_state ~hands =
  let t = make_state ~hands ~discard_pile:[ card Nine Hearts ] ~stock:[] () in
  (* Put every other card under the top card, so the state is still one full deck. *)
  { t with discard_pile = t.discard_pile @ rest_of_deck t }
;;

let%expect_test "passing when stuck moves the turn on and counts passes" =
  let t =
    blocked_state
      ~hands:
        [ [ card King Clubs; card Two Spades ]; [ card Queen Clubs ]; [ card Jack Clubs ] ]
  in
  print_moves t;
  [%expect {| pass |}];
  move_and_print t Pass;
  [%expect {|
    player 0: KC 2S
    player 1: QC
    player 2: JC
    top: 9H | suit: H | stock: 0 cards | passes in a row: 1
    (In_progress (whose_turn 1)) |}]
;;

let%expect_test "blocked game: everyone passes, the lowest hand wins" =
  let t =
    blocked_state
      ~hands:
        [ [ card King Clubs; card Two Spades ]
        ; [ card Queen Clubs ]
        ; [ card Jack Clubs; card Ace Spades ]
        ]
  in
  (* Points: player 0 = 10 + 2 = 12, player 1 = 10, player 2 = 10 + 1 = 11. *)
  let t = ok_exn (Game_state.make_move t Pass) in
  let t = ok_exn (Game_state.make_move t Pass) in
  move_and_print t Pass;
  [%expect {|
    player 0: KC 2S
    player 1: QC
    player 2: JC AS
    top: 9H | suit: H | stock: 0 cards | passes in a row: 3
    (Winner 1) |}]
;;

let%expect_test "blocked game: equal lowest scores is a tie" =
  let t =
    blocked_state
      ~hands:
        [ [ card King Clubs ]; [ card Queen Clubs; card Ace Spades ]; [ card Jack Clubs ] ]
  in
  (* Points: 10, 11, 10. Players 0 and 2 tie. *)
  let t = ok_exn (Game_state.make_move t Pass) in
  let t = ok_exn (Game_state.make_move t Pass) in
  move_and_print t Pass;
  [%expect {|
    player 0: KC
    player 1: QC AS
    player 2: JC
    top: 9H | suit: H | stock: 0 cards | passes in a row: 3
    (Tie (0 2)) |}]
;;

let%expect_test "blocked 2-player game, and the penalty points of each rank" =
  let t =
    blocked_state
      ~hands:
        [ [ card Two Clubs ]; [ card King Clubs; card Queen Clubs; card Jack Clubs ] ]
  in
  (* Player 0 has 2 points, player 1 has 30. *)
  let t = ok_exn (Game_state.make_move t Pass) in
  move_and_print t Pass;
  [%expect {|
    player 0: 2C
    player 1: KC QC JC
    top: 9H | suit: H | stock: 0 cards | passes in a row: 2
    (Winner 0) |}];
  (* An 8 is worth 50, but it can never be stuck in a blocked game: an 8 can always be
     played, so a player holding one is never allowed to pass. *)
  List.iter Rank.all ~f:(fun rank ->
    printf "%s=%d " (rank_to_string rank) (Rank.points rank));
  print_endline "";
  [%expect {| A=1 2=2 3=3 4=4 5=5 6=6 7=7 8=50 9=9 10=10 J=10 Q=10 K=10 |}]
;;

let%expect_test "playing (or drawing) resets the count of passes" =
  let t =
    blocked_state
      ~hands:
        [ [ card King Clubs ]; [ card Queen Clubs ]; [ card Two Hearts; card Jack Clubs ] ]
  in
  let t = ok_exn (Game_state.make_move t Pass) in
  let t = ok_exn (Game_state.make_move t Pass) in
  printf "passes before player 2 plays: %d\n" t.consecutive_passes;
  let t = ok_exn (Game_state.make_move t (play Two Hearts)) in
  printf "passes after player 2 plays: %d\n" t.consecutive_passes;
  [%expect {|
    passes before player 2 plays: 2
    passes after player 2 plays: 0 |}]
;;

let%expect_test "playing your last card wins, even with an 8" =
  let t =
    make_state
      ~hands:[ [ card Eight Clubs ]; [ card Four Hearts; card Five Hearts ] ]
      ~discard_pile:[ card Nine Hearts ]
      ()
  in
  move_and_print t (play_eight Clubs ~declare:Spades);
  [%expect {|
    player 0:
    player 1: 4H 5H
    top: 8C | suit: S | stock: 48 cards | passes in a row: 0
    (Winner 0) |}]
;;

let%expect_test "drawing the last card of the stock" =
  let t =
    make_state
      ~hands:[ [ card King Clubs ]; [ card Queen Clubs ] ]
      ~discard_pile:[ card Nine Hearts ]
      ~stock:[ card Two Hearts ]
      ()
  in
  let t = { t with discard_pile = t.discard_pile @ rest_of_deck t } in
  let t = ok_exn (Game_state.make_move t Draw) in
  print_state t;
  print_endline "legal moves now:";
  print_moves t;
  [%expect {|
    player 0: 2H KC
    player 1: QC
    top: 9H | suit: H | stock: 0 cards | passes in a row: 0
    (In_progress (whose_turn 0))
    legal moves now:
    play 2H |}]
;;

(* ======================================================================
   4. Illegal moves (every Move_error)
   ====================================================================== *)

let%expect_test "illegal moves from the HW1 states" =
  let initial = state_of_hw1 Hw1.initial_state in
  (* 7♥ doesn't match the 5♠. *)
  try_move initial (play Seven Hearts);
  [%expect {| Card_does_not_match |}];
  (* Player 0 doesn't hold the 9♠ (player 1 does). *)
  try_move initial (play Nine Spades);
  [%expect {| Card_not_in_hand |}];
  try_move initial (Play { card = card Eight Clubs; declared_suit = None });
  [%expect {| Eight_needs_declared_suit |}];
  try_move initial (Play { card = card King Spades; declared_suit = Some Hearts });
  [%expect {| Only_eights_declare_suit |}];
  try_move initial Pass;
  [%expect {| Cannot_pass |}];
  try_move (state_of_hw1 Hw1.state_before_blocked) Draw;
  [%expect {| Stock_is_empty |}];
  try_move (state_of_hw1 Hw1.terminal_state) Draw;
  [%expect {| Game_is_over |}]
;;

let%expect_test "more illegal moves" =
  (* A card from another player's hand, even though it matches. *)
  try_move nine_of_hearts_state (play Three Diamonds);
  [%expect {| Card_not_in_hand |}];
  (* A card that is on the discard pile, not in anyone's hand. *)
  try_move nine_of_hearts_state (play Nine Hearts);
  [%expect {| Card_not_in_hand |}];
  (* The K♣ matches neither the suit (hearts) nor the rank (9). *)
  try_move nine_of_hearts_state (play King Clubs);
  [%expect {| Card_does_not_match |}];
  (* After an 8 declared diamonds, a heart no longer matches, even on a heart. *)
  let t = { nine_of_hearts_state with current_suit = Diamonds } in
  try_move t (play Two Hearts);
  [%expect {| Card_does_not_match |}];
  (* But the same rank still matches the top card. *)
  try_move t (play Nine Clubs);
  [%expect {| Ok |}];
  (* Passing with a playable card is not allowed, even with an empty stock. *)
  let stuck = blocked_state ~hands:[ [ card Two Hearts ]; [ card Queen Clubs ] ] in
  try_move stuck Pass;
  [%expect {| Cannot_pass |}];
  (* Passing while the stock still has cards is not allowed, even with nothing playable. *)
  let t =
    make_state
      ~hands:[ [ card King Clubs ]; [ card Queen Clubs ] ]
      ~discard_pile:[ card Nine Hearts ]
      ()
  in
  try_move t Pass;
  [%expect {| Cannot_pass |}];
  (* Every kind of move is refused once the game is over, including passing. *)
  let over = state_of_hw1 Hw1.state_after_blocked in
  List.iter [ play Two Hearts; Draw; Pass ] ~f:(try_move over);
  [%expect {|
    Game_is_over
    Game_is_over
    Game_is_over |}]
;;

let%expect_test "an illegal move doesn't change anything" =
  (* make_move returns a new state, so the old one must stay exactly the same. *)
  let before = nine_of_hearts_state in
  let copy = { before with hands = before.hands } in
  ignore (Game_state.make_move before (play King Clubs) : _ Result.t);
  ignore (Game_state.make_move before Pass : _ Result.t);
  printf "unchanged: %b\n" (Game_state.equal before copy);
  [%expect {| unchanged: true |}]
;;

(* ======================================================================
   5. Creating a game
   ====================================================================== *)

(* Shows how a new game was dealt: hand sizes, stock size, the first card on the pile. *)
let print_create_result result =
  match result with
  | Error errors -> print_s [%sexp (errors : Game_state.Create_error.t list)]
  | Ok (t : Game_state.t) ->
    print_s
      [%message
        ""
          ~hand_sizes:(List.map t.hands ~f:List.length : int list)
          ~stock_size:(List.length t.stock : int)
          ~top_card:(Game_state.top_card t : Card.t)
          ~one_full_deck:(is_one_full_deck t : bool)]
;;

let%expect_test "create" =
  print_create_result (Game_state.create ~num_players:2 ~deck:Card.full_deck);
  [%expect
    {|
    ((hand_sizes (7 7)) (stock_size 37) (top_card ((rank Two) (suit Diamonds)))
     (one_full_deck true))
    |}];
  print_create_result (Game_state.create ~num_players:4 ~deck:Card.full_deck);
  [%expect
    {|
    ((hand_sizes (5 5 5 5)) (stock_size 31)
     (top_card ((rank Nine) (suit Diamonds))) (one_full_deck true))
    |}];
  print_create_result
    (Game_state.create ~num_players:1 ~deck:(List.tl_exn Card.full_deck));
  [%expect {| (Invalid_number_of_players Deck_is_not_a_full_deck) |}]
;;

let%expect_test "create: every allowed number of players, and the limits" =
  List.iter [ 0; 1; 2; 3; 8; 9 ] ~f:(fun num_players ->
    printf "%d players: " num_players;
    print_create_result (Game_state.create ~num_players ~deck:Card.full_deck));
  [%expect {|
    0 players: (Invalid_number_of_players)
    1 players: (Invalid_number_of_players)
    2 players: ((hand_sizes (7 7)) (stock_size 37) (top_card ((rank Two) (suit Diamonds)))
     (one_full_deck true))
    3 players: ((hand_sizes (5 5 5)) (stock_size 36)
     (top_card ((rank Three) (suit Diamonds))) (one_full_deck true))
    8 players: ((hand_sizes (5 5 5 5 5 5 5 5)) (stock_size 11)
     (top_card ((rank Two) (suit Spades))) (one_full_deck true))
    9 players: (Invalid_number_of_players) |}]
;;

let%expect_test "create: a deck with a duplicate card is refused" =
  let deck = card Ace Spades :: List.tl_exn Card.full_deck in
  print_create_result (Game_state.create ~num_players:2 ~deck);
  [%expect {| (Deck_is_not_a_full_deck) |}]
;;

let%expect_test "create: an 8 can't start the discard pile" =
  (* 2 players get 14 cards; put two 8s right after them. *)
  let eights = [ card Eight Hearts; card Eight Spades ] in
  let others =
    List.filter Card.full_deck ~f:(fun c -> not (List.mem eights c ~equal:Card.equal))
  in
  let dealt, rest = List.split_n others 14 in
  let t =
    Game_state.create ~num_players:2 ~deck:(dealt @ eights @ rest)
    |> Result.ok
    |> Option.value_exn
  in
  printf
    "top card: %s, last two cards of the stock: %s\n"
    (card_to_string (Game_state.top_card t))
    (cards_to_string (List.drop t.stock (List.length t.stock - 2)));
  [%expect {| top card: 2D, last two cards of the stock: 8H 8S |}]
;;

(* ======================================================================
   6. Exploring the state space with random games
   ====================================================================== *)

(* Every move that can be written down: 52 cards x (no suit or one of 4 suits), draw,
   pass. Most of them are illegal in any given state. *)
let every_possible_move : Move.t list =
  List.concat_map Card.full_deck ~f:(fun card ->
    Move.Play { card; declared_suit = None }
    :: List.map Suit.all ~f:(fun suit -> Move.Play { card; declared_suit = Some suit }))
  @ [ Draw; Pass ]
;;

(* Plays 1000 games choosing random allowed moves. Every game must end (a winner or a
   tie) and no card may ever go missing. *)
let%expect_test "1000 random games" =
  let random_state = Random.State.make [| 42 |] in
  let games_over = ref 0 in
  for game = 1 to 1000 do
    let num_players = 2 + (game % 7) in
    let deck = Card.shuffled_deck random_state in
    let t = Game_state.create ~num_players ~deck |> Result.ok |> Option.value_exn in
    let rec play (t : Game_state.t) moves_made =
      if not (is_one_full_deck t) then raise_s [%message "Cards were lost or duplicated"];
      if moves_made > 10_000 then raise_s [%message "Game did not end"];
      match Game_state.get_all_moves t with
      | [] -> t.decision
      | moves ->
        let move = List.random_element_exn moves ~random_state in
        play (ok_exn (Game_state.make_move t move)) (moves_made + 1)
    in
    (* No legal moves must mean the game is over (someone won, or a tie). *)
    if Decision.is_game_over (play t 0) then incr games_over
  done;
  printf "%d of 1000 games ended with a winner or a tie\n" !games_over;
  [%expect {| 1000 of 1000 games ended with a winner or a tie |}]
;;

(* Checks one step of a game against the rules: did the move change the game the way it
   should? Stops the test with a message on any problem. *)
let check_step (before : Game_state.t) (move : Move.t) (after : Game_state.t) =
  let fail msg = raise_s [%message msg (before : Game_state.t) (move : Move.t)] in
  let player =
    match before.decision with
    | In_progress { whose_turn } -> whose_turn
    | _ -> fail "a move was accepted after the game was over"
  in
  let n = Game_state.num_players before in
  let hand_before = List.nth_exn before.hands player in
  let hand_after = List.nth_exn after.hands player in
  if not (is_one_full_deck after) then fail "cards were lost or duplicated";
  (* Nobody else's hand changes. *)
  List.iteri before.hands ~f:(fun i hand ->
    if i <> player && not (List.equal Card.equal hand (List.nth_exn after.hands i))
    then fail "another player's hand changed");
  match move with
  | Play { card; declared_suit } ->
    (* A played card goes on top, leaves the hand, and sets the suit to follow. *)
    if not (Card.equal (Game_state.top_card after) card)
    then fail "played card not on top";
    if List.mem hand_after card ~equal:Card.equal then fail "played card still in hand";
    if List.length hand_after <> List.length hand_before - 1 then fail "hand size wrong";
    let expected_suit = Option.value declared_suit ~default:card.suit in
    if not (Suit.equal after.current_suit expected_suit) then fail "wrong suit to follow";
    if after.consecutive_passes <> 0 then fail "passes not reset";
    (match after.decision with
     | Winner w when w = player && List.is_empty hand_after -> ()
     | In_progress { whose_turn } when whose_turn = (player + 1) % n -> ()
     | _ -> fail "wrong decision after a play")
  | Draw ->
    (* Drawing takes the top card of the stock, and it's still the same player's turn. *)
    if List.length after.stock <> List.length before.stock - 1 then fail "stock size";
    if not (List.mem hand_after (List.hd_exn before.stock) ~equal:Card.equal)
    then fail "drew a card that wasn't the top of the stock";
    if not (Decision.equal after.decision before.decision) then fail "turn changed"
  | Pass ->
    (* Passing adds one to the count; when everyone has passed, the game is decided. *)
    if after.consecutive_passes <> before.consecutive_passes + 1 then fail "passes count";
    (match after.decision with
     | In_progress { whose_turn } when whose_turn = (player + 1) % n -> ()
     | (Winner _ | Tie _) when after.consecutive_passes = n -> ()
     | _ -> fail "wrong decision after a pass")
;;

let%expect_test "random exploration: every step follows the rules" =
  (* 300 random games, 2 to 8 players. At every step:
     - every move from get_all_moves is accepted, and there are no duplicates,
     - 25 random moves out of all 262 possible ones are accepted exactly when
       get_all_moves lists them,
     - the chosen move changes the state the way the rules say (check_step),
     - get_all_moves is empty exactly when the game is over. *)
  let random_state = Random.State.make [| 2026 |] in
  let steps = ref 0 in
  let illegal_tried = ref 0 in
  let outcomes = String.Table.create () in
  let count key = Hashtbl.incr outcomes key in
  for game = 1 to 300 do
    let num_players = 2 + (game % 7) in
    let deck = Card.shuffled_deck random_state in
    let t = Game_state.create ~num_players ~deck |> Result.ok |> Option.value_exn in
    let rec loop (t : Game_state.t) =
      let moves = Game_state.get_all_moves t in
      if List.contains_dup moves ~compare:Move.compare
      then raise_s [%message "duplicate moves" (moves : Move.t list)];
      if Bool.equal (List.is_empty moves) (not (Decision.is_game_over t.decision))
      then raise_s [%message "no moves but game not over (or the reverse)"];
      List.iter moves ~f:(fun move ->
        match Game_state.make_move t move with
        | Ok _ -> ()
        | Error error ->
          raise_s
            [%message
              "listed move refused" (move : Move.t) (error : Game_state.Move_error.t)]);
      (* Try 25 random moves, most of them illegal: the game must accept exactly the
         ones in the list of allowed moves. *)
      for _ = 1 to 25 do
        let move = List.random_element_exn every_possible_move ~random_state in
        let listed = List.mem moves move ~equal:Move.equal in
        let accepted = Result.is_ok (Game_state.make_move t move) in
        if not listed then incr illegal_tried;
        if not (Bool.equal listed accepted)
        then raise_s [%message "make_move and get_all_moves disagree" (move : Move.t)]
      done;
      match t.decision with
      | Winner _ when t.consecutive_passes = 0 -> count "won by playing the last card"
      | Winner _ -> count "blocked, lowest hand won"
      | Tie _ -> count "blocked, tie"
      | In_progress _ ->
        (* Pick a random allowed move, make it, and check it followed the rules. *)
        let move = List.random_element_exn moves ~random_state in
        let next = ok_exn (Game_state.make_move t move) in
        check_step t move next;
        incr steps;
        loop next
    in
    loop t
  done;
  printf "checked %d moves and %d illegal attempts\n" !steps !illegal_tried;
  Hashtbl.to_alist outcomes
  |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
  |> List.iter ~f:(fun (outcome, n) -> printf "%s: %d games\n" outcome n);
  [%expect {|
    checked 20668 moves and 519328 illegal attempts
    blocked, lowest hand won: 44 games
    blocked, tie: 1 games
    won by playing the last card: 255 games |}]
;;
