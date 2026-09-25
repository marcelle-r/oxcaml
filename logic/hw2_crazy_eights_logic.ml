open! Core

module Suit = struct
  type t =
    | Clubs
    | Diamonds
    | Hearts
    | Spades
  [@@deriving sexp, compare, equal, enumerate]
end

module Rank = struct
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

  let points t =
    match t with
    | Eight -> 50
    | Jack | Queen | King -> 10
    | Ace -> 1
    | Two -> 2
    | Three -> 3
    | Four -> 4
    | Five -> 5
    | Six -> 6
    | Seven -> 7
    | Nine -> 9
    | Ten -> 10
  ;;
end

module Card = struct
  module T = struct
    type t =
      { rank : Rank.t
      ; suit : Suit.t
      }
    [@@deriving sexp, compare, equal]
  end

  include T
  include Comparable.Make (T)

  let full_deck =
    List.concat_map Suit.all ~f:(fun suit -> List.map Rank.all ~f:(fun rank -> { rank; suit }))
  ;;

  let shuffled_deck random_state = List.permute full_deck ~random_state
end

module Move = struct
  type t =
    | Play of
        { card : Card.t
        ; declared_suit : Suit.t option
        }
    | Draw
    | Pass
  [@@deriving sexp, compare, equal]
end

module Decision = struct
  type t =
    | In_progress of { whose_turn : int }
    | Winner of int
    | Tie of int list
  [@@deriving sexp, compare, equal]

  let is_game_over t =
    match t with
    | Winner _ | Tie _ -> true
    | In_progress _ -> false
  ;;
end

module Game_state = struct
  type t =
    { hands : Card.t list list
    ; stock : Card.t list
    ; discard_pile : Card.t list
    ; current_suit : Suit.t
    ; consecutive_passes : int
    ; decision : Decision.t
    ; last_move : (int * Move.t) option
    }
  [@@deriving sexp, compare, equal]

  let min_players = 2
  let max_players = 8

  module Create_error = struct
    type t =
      | Invalid_number_of_players
      | Deck_is_not_a_full_deck
    [@@deriving sexp, compare, equal]
  end

  let is_full_deck deck =
    List.equal
      Card.equal
      (List.sort deck ~compare:Card.compare)
      (List.sort Card.full_deck ~compare:Card.compare)
  ;;

  (* Flips the top card of [stock] to start the discard pile. Eights are not allowed as
     the starter, so they are moved to the bottom of the stock. A full deck always has a
     non-8 left: at most 8 * 5 = 40 cards are dealt, leaving 12, and there are 4 eights. *)
  let flip_starter stock =
    let rec loop buried_eights stock =
      match (stock : Card.t list) with
      | [] -> raise_s [%message "No starter card left in the stock"]
      | ({ rank = Eight; _ } as eight) :: rest -> loop (eight :: buried_eights) rest
      | starter :: rest -> starter, rest @ List.rev buried_eights
    in
    loop [] stock
  ;;

  let create ~num_players ~deck : (t, Create_error.t list) Result.t =
    let players_ok = num_players >= min_players && num_players <= max_players in
    let deck_ok = is_full_deck deck in
    match players_ok, deck_ok with
    | true, true ->
      let hand_size = if num_players = 2 then 7 else 5 in
      let dealt, rest = List.split_n deck (num_players * hand_size) in
      (* Deal one card at a time, round-robin: card i goes to player (i mod n). *)
      let hands =
        List.init num_players ~f:(fun player ->
          List.filteri dealt ~f:(fun i _ -> i % num_players = player))
      in
      let starter, stock = flip_starter rest in
      Ok
        { hands
        ; stock
        ; discard_pile = [ starter ]
        ; current_suit = starter.suit
        ; consecutive_passes = 0
        ; decision = In_progress { whose_turn = 0 }
        ; last_move = None
        }
    | _ ->
      Error
        ((if players_ok then [] else [ Create_error.Invalid_number_of_players ])
         @ if deck_ok then [] else [ Create_error.Deck_is_not_a_full_deck ])
  ;;

  let num_players t = List.length t.hands
  let top_card t = List.hd_exn t.discard_pile

  let hand_points hand =
    List.sum (module Int) hand ~f:(fun (card : Card.t) -> Rank.points card.rank)
  ;;

  let can_play_card t (card : Card.t) =
    Rank.equal card.rank Eight
    || Suit.equal card.suit t.current_suit
    || Rank.equal card.rank (top_card t).rank
  ;;

  let next_player t player = (player + 1) % num_players t

  let set_hand t player hand =
    List.mapi t.hands ~f:(fun i old_hand -> if i = player then hand else old_hand)
  ;;

  (* Everyone passed in a row: fewest penalty points in hand wins. *)
  let decide_blocked_game t : Decision.t =
    let points = List.map t.hands ~f:hand_points in
    let lowest = List.min_elt points ~compare:Int.compare |> Option.value_exn in
    let winners =
      List.filter_mapi points ~f:(fun player p -> Option.some_if (p = lowest) player)
    in
    match winners with
    | [ winner ] -> Winner winner
    | winners -> Tie winners
  ;;

  module Move_error = struct
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

  let playable_cards t hand = List.filter hand ~f:(can_play_card t)

  let get_all_moves t : Move.t list =
    match t.decision with
    | Winner _ | Tie _ -> []
    | In_progress { whose_turn } ->
      let hand = List.nth_exn t.hands whose_turn in
      let plays =
        List.concat_map (playable_cards t hand) ~f:(fun (card : Card.t) : Move.t list ->
          match card.rank with
          | Eight ->
            List.map Suit.all ~f:(fun suit : Move.t ->
              Play { card; declared_suit = Some suit })
          | _ -> [ Play { card; declared_suit = None } ])
      in
      let draw = if List.is_empty t.stock then [] else [ Move.Draw ] in
      let pass = if List.is_empty plays && List.is_empty draw then [ Move.Pass ] else [] in
      plays @ draw @ pass
  ;;

  let play_card t ~player ~hand ~(card : Card.t) ~suit =
    let hand = List.filter hand ~f:(fun c -> not (Card.equal c card)) in
    let decision : Decision.t =
      if List.is_empty hand
      then Winner player
      else In_progress { whose_turn = next_player t player }
    in
    { t with
      hands = set_hand t player hand
    ; discard_pile = card :: t.discard_pile
    ; current_suit = suit
    ; consecutive_passes = 0
    ; decision
    }
  ;;

  let make_move t (move : Move.t) : (t, Move_error.t) Result.t =
    match t.decision with
    | Winner _ | Tie _ -> Error Game_is_over
    | In_progress { whose_turn = player } ->
      let hand = List.nth_exn t.hands player in
      let result : (t, Move_error.t) Result.t =
        match move with
        | Play { card; declared_suit } ->
          if not (List.mem hand card ~equal:Card.equal)
          then Error Card_not_in_hand
          else if not (can_play_card t card)
          then Error Card_does_not_match
          else (
            match card.rank, declared_suit with
            | Eight, None -> Error Eight_needs_declared_suit
            | Eight, Some suit -> Ok (play_card t ~player ~hand ~card ~suit)
            | _, Some _ -> Error Only_eights_declare_suit
            | _, None -> Ok (play_card t ~player ~hand ~card ~suit:card.suit))
        | Draw ->
          (match t.stock with
           | [] -> Error Stock_is_empty
           | card :: stock ->
             (* Your turn continues after drawing. *)
             Ok
               { t with
                 hands = set_hand t player (card :: hand)
               ; stock
               ; consecutive_passes = 0
               })
        | Pass ->
          if (not (List.is_empty t.stock)) || not (List.is_empty (playable_cards t hand))
          then Error Cannot_pass
          else (
            let consecutive_passes = t.consecutive_passes + 1 in
            let t = { t with consecutive_passes } in
            let decision : Decision.t =
              if consecutive_passes >= num_players t
              then decide_blocked_game t
              else In_progress { whose_turn = next_player t player }
            in
            Ok { t with decision })
      in
      Result.map result ~f:(fun t -> { t with last_move = Some (player, move) })
  ;;
end
