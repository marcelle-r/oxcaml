open! Core
open Hw2_crazy_eights_logic

(* HW4: computer opponents for Crazy Eights.

   Why not alpha-beta (like the course's Tic-Tac-Toe example)? Alpha-beta needs a game
   where everyone sees everything and nothing is random. In Crazy Eights you can't see
   the other hands or the stock, and drawing gives you a random card. So the better
   opponent uses Monte Carlo simulations instead: it guesses the hidden cards many times,
   plays each of its options to the end of the game, and keeps the option that wins
   most often. *)

(* Whose turn is it? [None] when the game is over. *)
let whose_turn (t : Game_state.t) =
  match t.decision with
  | In_progress { whose_turn } -> Some whose_turn
  | Winner _ | Tie _ -> None
;;

(* ---------- The trivial opponent ---------- *)

(* Easy player: get every allowed move and pick one at random. *)
let random_move t ~random_state =
  List.random_element (Game_state.get_all_moves t) ~random_state
;;

(* ---------- A quick rule of thumb ---------- *)

(* The suit we hold most of (not counting 8s), so that after an 8 we can keep playing. *)
let best_suit_to_declare hand =
  let count suit =
    List.count hand ~f:(fun (c : Card.t) ->
      Suit.equal c.suit suit && not (Rank.equal c.rank Eight))
  in
  List.max_elt Suit.all ~compare:(fun a b -> Int.compare (count a) (count b))
  |> Option.value_exn
;;

(* Simple rules: play the matching card worth the most points; if none, play an 8 and
   pick my best suit; if no 8, draw; if the stock is empty, pass. *)
let greedy_move (t : Game_state.t) ~random_state : Move.t option =
  match whose_turn t with
  | None -> None
  | Some player ->
    let hand = List.nth_exn t.hands player in
    let playable = List.filter hand ~f:(Game_state.can_play_card t) in
    let eights, others =
      List.partition_tf playable ~f:(fun c -> Rank.equal c.rank Eight)
    in
    (match others, eights with
     | _ :: _, _ ->
       (* Get rid of the card that would cost the most points if the game got blocked;
          among equal ones, pick at random. *)
       let points (c : Card.t) = Rank.points c.rank in
       let most = List.map others ~f:points |> List.fold ~init:0 ~f:Int.max in
       let best = List.filter others ~f:(fun c -> points c = most) in
       let card = List.random_element_exn best ~random_state in
       Some (Play { card; declared_suit = None })
     | [], eight :: _ ->
       Some (Play { card = eight; declared_suit = Some (best_suit_to_declare hand) })
     | [], [] ->
       if List.is_empty t.stock then Some Pass else Some Draw)
;;

(* ---------- The better opponent: Monte Carlo search ---------- *)

(* Guesses the hidden cards. [player] knows their own hand and the discard pile; every
   other card is shuffled and dealt so that each hand and the stock keep their size. *)
let guess_hidden_cards (t : Game_state.t) ~player ~random_state : Game_state.t =
  let my_hand = List.nth_exn t.hands player in
  let known = my_hand @ t.discard_pile in
  let unknown =
    List.filter Card.full_deck ~f:(fun c -> not (List.mem known c ~equal:Card.equal))
    |> List.permute ~random_state
  in
  let hands, rest =
    List.fold_mapi t.hands ~init:unknown ~f:(fun i remaining hand ->
      if i = player
      then remaining, my_hand
      else (
        let dealt, remaining = List.split_n remaining (List.length hand) in
        remaining, dealt))
    |> fun (rest, hands) -> hands, rest
  in
  { t with hands; stock = rest }
;;

(* Plays the game to the end with [greedy_move] for everyone. *)
let play_out (t : Game_state.t) ~random_state =
  let rec loop (t : Game_state.t) moves_left =
    if moves_left = 0
    then t.decision
    else (
      match greedy_move t ~random_state with
      | None -> t.decision
      | Some move ->
        (match Game_state.make_move t move with
         | Ok t -> loop t (moves_left - 1)
         | Error _ -> t.decision))
  in
  loop t 1_000
;;

(* 1 for a win, a share of 1 for a tie, 0 otherwise. *)
let score (decision : Decision.t) ~player =
  match decision with
  | Winner winner -> if winner = player then 1. else 0.
  | Tie players ->
    if List.mem players player ~equal:Int.equal
    then 1. /. Float.of_int (List.length players)
    else 0.
  | In_progress _ -> 0.
;;

(* Smart player: for up to 2 seconds, imagine many games for each allowed move and
   pick the move that wins most often. *)
let smart_move
  ?(time_limit = Time_ns.Span.of_int_sec 2)
  ?(max_simulations = Int.max_value)
  (t : Game_state.t)
  ~random_state
  =
  match whose_turn t, Game_state.get_all_moves t with
  | None, _ | _, [] -> None
  (* Only one allowed move: no need to think. *)
  | Some _, [ only_move ] -> Some only_move
  | Some player, moves ->
    let moves = Array.of_list moves in
    let num_moves = Array.length moves in
    (* For each move: how many imagined games it won, and how many times we tried it. *)
    let wins = Array.create ~len:num_moves 0. in
    let tries = Array.create ~len:num_moves 0 in
    (* When to stop thinking: now + 2 seconds. *)
    let deadline = Time_ns.add (Time_ns.now ()) time_limit in
    let simulations = ref 0 in
    (* Try every move once, then pick which move to simulate next with UCB1: mostly the
       moves that are winning so far, but sometimes the ones we have tried less, in case
       we were unlucky with them. Keep going until the time (or the number of
       simulations) runs out. *)
    let pick_move () =
      if !simulations < num_moves
      then !simulations
      else (
        let total = Float.log (Float.of_int !simulations) in
        let ucb i =
          let n = Float.of_int tries.(i) in
          (wins.(i) /. n) +. Float.sqrt (2. *. total /. n)
        in
        let best = ref 0 in
        for i = 1 to num_moves - 1 do
          if Float.( > ) (ucb i) (ucb !best) then best := i
        done;
        !best)
    in
    while
      !simulations < max_simulations
      && (!simulations < num_moves || Time_ns.( < ) (Time_ns.now ()) deadline)
    do
      (* One imagined game: pick a move to test, guess the hidden cards, make the move,
         play the game to the end, and add 1 if we won. *)
      let i = pick_move () in
      let guess = guess_hidden_cards t ~player ~random_state in
      (match Game_state.make_move guess moves.(i) with
       | Ok after -> wins.(i) <- wins.(i) +. score (play_out after ~random_state) ~player
       | Error _ -> ());
      tries.(i) <- tries.(i) + 1;
      incr simulations
    done;
    (* Time's up: choose the move with the best wins / tries. *)
    let win_rate i = if tries.(i) = 0 then -1. else wins.(i) /. Float.of_int tries.(i) in
    let best = ref 0 in
    for i = 1 to num_moves - 1 do
      if Float.( > ) (win_rate i) (win_rate !best) then best := i
    done;
    Some moves.(!best)
;;
