open! Core
open Crazy_eights_logic_library
open Hw2_crazy_eights_logic

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

let all_cards (t : Game_state.t) = List.concat t.hands @ t.stock @ t.discard_pile

let is_one_full_deck t =
  List.equal
    Card.equal
    (List.sort (all_cards t) ~compare:Card.compare)
    (List.sort Card.full_deck ~compare:Card.compare)
;;

(* Checks that [make_move from move = to_], ignoring [last_move]. *)
let check_triplet ~from ~move ~to_ =
  let from = state_of_hw1 from in
  let expected = state_of_hw1 to_ in
  let move = move_of_hw1 move in
  if not (is_one_full_deck from) then print_endline "from-state is not exactly one deck";
  if not (is_one_full_deck expected) then print_endline "to-state is not exactly one deck";
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

(* ---------- A few illegal moves. ---------- *)

let try_move state move =
  match Game_state.make_move (state_of_hw1 state) move with
  | Ok _ -> print_endline "Ok"
  | Error error -> print_s [%sexp (error : Game_state.Move_error.t)]
;;

let card rank suit : Card.t = { rank; suit }

let%expect_test "illegal moves" =
  (* 7♥ doesn't match the 5♠. *)
  try_move Hw1.initial_state (Play { card = card Seven Hearts; declared_suit = None });
  [%expect {| Card_does_not_match |}];
  (* Player 0 doesn't hold the 9♠ (player 1 does). *)
  try_move Hw1.initial_state (Play { card = card Nine Spades; declared_suit = None });
  [%expect {| Card_not_in_hand |}];
  try_move Hw1.initial_state (Play { card = card Eight Clubs; declared_suit = None });
  [%expect {| Eight_needs_declared_suit |}];
  try_move
    Hw1.initial_state
    (Play { card = card King Spades; declared_suit = Some Hearts });
  [%expect {| Only_eights_declare_suit |}];
  try_move Hw1.initial_state Pass;
  [%expect {| Cannot_pass |}];
  try_move Hw1.state_before_blocked Draw;
  [%expect {| Stock_is_empty |}];
  try_move Hw1.terminal_state Draw;
  [%expect {| Game_is_over |}]
;;

(* ---------- Creating a game. ---------- *)

let%expect_test "create" =
  let print_result result =
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
  in
  print_result (Game_state.create ~num_players:2 ~deck:Card.full_deck);
  [%expect
    {|
    ((hand_sizes (7 7)) (stock_size 37) (top_card ((rank Two) (suit Diamonds)))
     (one_full_deck true))
    |}];
  print_result (Game_state.create ~num_players:4 ~deck:Card.full_deck);
  [%expect
    {|
    ((hand_sizes (5 5 5 5)) (stock_size 31)
     (top_card ((rank Nine) (suit Diamonds))) (one_full_deck true))
    |}];
  print_result (Game_state.create ~num_players:1 ~deck:(List.tl_exn Card.full_deck));
  [%expect {| (Invalid_number_of_players Deck_is_not_a_full_deck) |}]
;;

(* ---------- Random games: every game ends, and no card is lost or duplicated. ------- *)

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
        play (Game_state.make_move t move |> Result.ok |> Option.value_exn) (moves_made + 1)
    in
    (* No legal moves must mean the game is over (someone won, or a tie). *)
    if Decision.is_game_over (play t 0) then incr games_over
  done;
  printf "%d of 1000 games ended with a winner or a tie\n" !games_over;
  [%expect {| 1000 of 1000 games ended with a winner or a tie |}]
;;
