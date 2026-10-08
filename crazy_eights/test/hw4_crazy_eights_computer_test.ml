open! Core
open Crazy_eights_logic_library
open Hw2_crazy_eights_logic
open Hw4_crazy_eights_computer

(* HW4: tests for the computer opponents. *)

let card rank suit : Card.t = { rank; suit }

let make_state ?stock ?(whose_turn = 0) ~hands ~discard_pile () : Game_state.t =
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
  ; current_suit = top.suit
  ; consecutive_passes = 0
  ; decision = In_progress { whose_turn }
  ; last_move = None
  }
;;

let print_move move = print_s [%sexp (move : Move.t option)]
let is_legal t move = List.mem (Game_state.get_all_moves t) move ~equal:Move.equal

let new_game ~num_players ~random_state =
  Game_state.create ~num_players ~deck:(Card.shuffled_deck random_state)
  |> Result.ok
  |> Option.value_exn
;;

(* Top 9♥. Player 0 holds 2♥ (2 points), K♥ (10 points), 9♣, an 8 and a club. *)
let mid_game =
  make_state
    ~hands:
      [ [ card Two Hearts
        ; card King Hearts
        ; card Nine Clubs
        ; card Eight Spades
        ; card Four Clubs
        ]
      ; [ card Three Diamonds; card Four Diamonds; card Five Diamonds ]
      ]
    ~discard_pile:[ card Nine Hearts ]
    ()
;;

(* ---------- The random player ---------- *)

let%expect_test "random_move: always one of the legal moves" =
  let random_state = Random.State.make [| 1 |] in
  let all_legal = ref true in
  for _ = 1 to 200 do
    let t = new_game ~num_players:3 ~random_state in
    match random_move t ~random_state with
    | Some move -> if not (is_legal t move) then all_legal := false
    | None -> all_legal := false
  done;
  printf "all legal: %b\n" !all_legal;
  [%expect {| all legal: true |}]
;;

let%expect_test "random_move: picks different moves, including draw" =
  let random_state = Random.State.make [| 1 |] in
  let seen =
    List.init 200 ~f:(fun _ -> random_move mid_game ~random_state |> Option.value_exn)
    |> List.dedup_and_sort ~compare:Move.compare
  in
  printf
    "%d different moves out of %d legal ones\n"
    (List.length seen)
    (List.length (Game_state.get_all_moves mid_game));
  [%expect {| 8 different moves out of 8 legal ones |}]
;;

let%expect_test "no move once the game is over" =
  let random_state = Random.State.make [| 1 |] in
  let over = { mid_game with decision = Winner 1 } in
  print_move (random_move over ~random_state);
  print_move (greedy_move over ~random_state);
  print_move (smart_move over ~random_state);
  [%expect {|
    ()
    ()
    () |}]
;;

(* ---------- The rule of thumb used inside the simulations ---------- *)

let%expect_test "greedy_move: plays the matching card worth the most points, keeps the 8" =
  let random_state = Random.State.make [| 1 |] in
  print_move (greedy_move mid_game ~random_state);
  [%expect {| ((Play (card ((rank King) (suit Hearts))) (declared_suit ()))) |}]
;;

let%expect_test "greedy_move: an 8 when nothing else matches, declaring its best suit" =
  let random_state = Random.State.make [| 1 |] in
  let t =
    make_state
      ~hands:
        [ [ card Eight Spades; card Four Clubs; card Jack Clubs; card Two Diamonds ]
        ; [ card Three Diamonds ]
        ]
      ~discard_pile:[ card Nine Hearts ]
      ()
  in
  print_move (greedy_move t ~random_state);
  [%expect {| ((Play (card ((rank Eight) (suit Spades))) (declared_suit (Clubs)))) |}]
;;

let%expect_test "greedy_move: draws when stuck, passes when the stock is empty" =
  let random_state = Random.State.make [| 1 |] in
  let t =
    make_state
      ~hands:[ [ card Four Clubs ]; [ card Three Diamonds ] ]
      ~discard_pile:[ card Nine Hearts ]
      ()
  in
  print_move (greedy_move t ~random_state);
  [%expect {| (Draw) |}];
  let no_stock = { t with stock = []; discard_pile = t.discard_pile @ t.stock } in
  print_move (greedy_move no_stock ~random_state);
  [%expect {| (Pass) |}]
;;

(* ---------- The smart (Monte Carlo) player ---------- *)

let quick_smart_move t ~random_state =
  (* A fixed number of simulations instead of 2 seconds: fast, and the same every run. *)
  smart_move ~max_simulations:300 ~time_limit:(Time_ns.Span.of_int_sec 60) t ~random_state
;;

let%expect_test "smart_move: wins right away when it can play its last card" =
  let random_state = Random.State.make [| 1 |] in
  let t =
    make_state
      ~hands:[ [ card Four Hearts ]; [ card Three Diamonds; card Five Spades ] ]
      ~discard_pile:[ card Nine Hearts ]
      ()
  in
  print_move (quick_smart_move t ~random_state);
  [%expect {| ((Play (card ((rank Four) (suit Hearts))) (declared_suit ()))) |}]
;;

let%expect_test "smart_move: with only one legal move, it just plays it" =
  let random_state = Random.State.make [| 1 |] in
  let t =
    make_state
      ~hands:[ [ card Four Clubs ]; [ card Three Diamonds ] ]
      ~discard_pile:[ card Nine Hearts ]
      ()
  in
  print_move (quick_smart_move t ~random_state);
  [%expect {| (Draw) |}]
;;

let%expect_test "smart_move: a legal move in a real game position" =
  let random_state = Random.State.make [| 3 |] in
  print_move (quick_smart_move mid_game ~random_state);
  [%expect {| ((Play (card ((rank King) (suit Hearts))) (declared_suit ()))) |}]
;;

let%expect_test "smart_move doesn't cheat: it can't see the other hands or the stock" =
  (* Two positions that look the same to player 0 (same hand, same discard pile, same
     number of cards everywhere) but where player 1 and the stock hold different cards.
     With the same random seed, the smart player must choose the same move. *)
  let other_cards =
    List.filter Card.full_deck ~f:(fun c ->
      not
        (List.mem
           (List.hd_exn mid_game.hands @ mid_game.discard_pile)
           c
           ~equal:Card.equal))
  in
  let with_hidden_cards cards : Game_state.t =
    let opponent, stock = List.split_n cards 3 in
    { mid_game with hands = [ List.hd_exn mid_game.hands; opponent ]; stock }
  in
  let a = with_hidden_cards other_cards in
  let b = with_hidden_cards (List.rev other_cards) in
  let move_a = quick_smart_move a ~random_state:(Random.State.make [| 5 |]) in
  let move_b = quick_smart_move b ~random_state:(Random.State.make [| 5 |]) in
  printf
    "different hidden cards: %b, same move: %b\n"
    (not (Game_state.equal a b))
    (Option.equal Move.equal move_a move_b);
  [%expect {| different hidden cards: true, same move: true |}]
;;

let%expect_test "smart_move: stops after about 2 seconds" =
  let random_state = Random.State.make [| 4 |] in
  let t = new_game ~num_players:4 ~random_state in
  let start = Time_ns.now () in
  let move = smart_move t ~random_state in
  let seconds = Time_ns.Span.to_sec (Time_ns.diff (Time_ns.now ()) start) in
  printf
    "legal move: %b, finished in under 2.5 seconds: %b\n"
    (is_legal t (Option.value_exn move))
    Float.(seconds < 2.5);
  [%expect {| legal move: true, finished in under 2.5 seconds: true |}]
;;

(* ---------- Matches between the players ---------- *)

let play_game ~players ~random_state =
  let rec loop (t : Game_state.t) moves_made =
    if moves_made > 5_000 then raise_s [%message "game did not end"];
    match t.decision with
    | Winner _ | Tie _ -> t.decision
    | In_progress { whose_turn } ->
      let choose = List.nth_exn players whose_turn in
      let move = choose t ~random_state |> Option.value_exn in
      (match Game_state.make_move t move with
       | Ok t -> loop t (moves_made + 1)
       | Error error ->
         raise_s
           [%message
             "a computer player chose an illegal move"
               (move : Move.t)
               (error : Game_state.Move_error.t)])
  in
  loop (new_game ~num_players:(List.length players) ~random_state) 0
;;

(* Plays 2-player games between [a] and [b], switching who goes first every game. *)
let play_match ~games ~a ~b ~a_name ~b_name ~seed =
  let random_state = Random.State.make [| seed |] in
  let a_wins = ref 0 in
  let b_wins = ref 0 in
  let ties = ref 0 in
  for game = 1 to games do
    let a_goes_first = game % 2 = 1 in
    let a_index = if a_goes_first then 0 else 1 in
    let players = if a_goes_first then [ a; b ] else [ b; a ] in
    match play_game ~players ~random_state with
    | Winner winner -> if winner = a_index then incr a_wins else incr b_wins
    | Tie _ -> incr ties
    | In_progress _ -> assert false
  done;
  printf
    "%s won %d, %s won %d, ties %d (out of %d games)\n"
    a_name
    !a_wins
    b_name
    !b_wins
    !ties
    games
;;

let%expect_test "1000 games: smart player vs random player" =
  (* In a real game the smart player has 2 seconds per move. Here it gets 30 simulations
     per move, so 1000 games take seconds instead of hours, and the result is the same
     on every computer. *)
  let smart t ~random_state =
    smart_move
      ~max_simulations:30
      ~time_limit:(Time_ns.Span.of_int_sec 60)
      t
      ~random_state
  in
  play_match ~games:1000 ~a:smart ~b:random_move ~a_name:"smart" ~b_name:"random" ~seed:1;
  [%expect {| smart won 827, random won 162, ties 11 (out of 1000 games) |}]
;;

let%expect_test "400 games: smart player vs the greedy rule of thumb" =
  (* A harder opponent than random. With 300 simulations per move the smart player wins
     more often; with the real 2 seconds it runs many more simulations than that. *)
  let smart t ~random_state =
    smart_move
      ~max_simulations:300
      ~time_limit:(Time_ns.Span.of_int_sec 60)
      t
      ~random_state
  in
  play_match ~games:400 ~a:smart ~b:greedy_move ~a_name:"smart" ~b_name:"greedy" ~seed:2;
  [%expect {| smart won 221, greedy won 178, ties 1 (out of 400 games) |}]
;;
