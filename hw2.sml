(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(s: string, xs: string list) = 
   case xs of 
      [] => NONE
      | x::xs' => if same_string(s, x) then SOME xs'
      else case all_except_option(s, xs') of
            NONE => NONE
            | SOME tl => SOME (x::tl)


fun get_substitutions1(xs: string list list, s: string) =
   case xs of 
      [] => []
      | x::xs' => case all_except_option(s, x) of 
         NONE => get_substitutions1(xs', s)
         | SOME v => v @ get_substitutions1(xs', s)


fun get_substitutions2(xs: string list list, s: string) = 
   let fun helper (x: string list list, ans: string list) =
      case x of 
         [] => ans
         | hd::tl => case all_except_option(s, hd) of 
            NONE => helper(tl, ans)
            | SOME v => helper(tl, ans @ v)
   in 
      helper(xs, [])
   end     


fun similar_names (subs: string list list, full_name: {first:string, middle:string, last:string}) =
   let
      val {first=x, middle=y, last=z} = full_name
      val fname_subs = get_substitutions2(subs, x)
      fun helper (names: string list, ans: {first:string,middle:string,last:string} list) =
         case names of 
            [] => ans 
            | hd::tl => helper(tl, ans @ [{first=hd, last=z, middle=y}])
   in 
      helper(fname_subs, [{first=x, last=z, middle=y}])
   end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color (xs: card) =
   case xs of 
      (Clubs, _) => Black
      | (Spades, _) => Black 
      | (Diamonds, _) => Red
      | (Hearts, _) => Red


fun card_value (xs: card) =
   case xs of
      (_, Ace) => 11
      | (_, Num(i)) => i 
      | _ => 10


fun remove_card (cs: card list, c: card, e: exn) = 
   case cs of
      [] => raise e
      | hd::tl => if hd = c then tl else hd::remove_card(tl, c, e)


fun all_same_color (cs: card list) = 
   case cs of 
      [] => true
      | hd::[] => true
      | hd::(neck::rest) => if card_color(hd) = card_color(neck) andalso all_same_color(neck::rest)
      then true else false
   

fun sum_cards (cs: card list) =
   let fun helper (cs: card list, curr_score: int) = 
      case cs of 
         [] => curr_score
         | hd::tl => helper(tl, curr_score + card_value(hd))
   in 
      helper(cs, 0)
   end


fun score (cs: card list, goal: int) =
   let
      val sum = sum_cards(cs)
      val isSameColor = all_same_color(cs)
      val pre_score = if sum > goal then 3 * (sum - goal) else (goal - sum)
   in 
      if isSameColor then pre_score div 2 else pre_score
   end


fun officiate (cs: card list, moves: move list, goal: int) =
   let 
      fun state_helper (hand: card list, cs: card list, moves: move list) =
         if sum_cards(hand) > goal then score(hand, goal)
         else case moves of 
            [] => score(hand, goal)
            | mv::rest => case mv of 
                           Draw => (case cs of 
                              [] => score(hand, goal)
                              | c::deck => state_helper(c::hand, deck, rest)
                              )
                           | Discard(c) => state_helper(remove_card(hand, c, IllegalMove), cs, rest)
   in 
      state_helper([], cs, moves)
   end


fun score_challenge (cs: card list, goal: int) =
   (* Note: Enumerate each Ace of low (1) or high (11) 
   to get lowest possible score *)
   let 
      val highest = score(cs, goal) (* all Aces have value 11 *)

      fun enum_helper (cl: card list, hl: card list, lowest: int) =
         case cl of 
            [] => lowest 
            | c::cards => case c of 
               (s, Ace) => (let
                  val h = (s, Num 1)::remove_card(hl, c, IllegalMove)
                  val sc = score(h, goal)
               in
                  if sc < lowest then enum_helper(cards, h, sc) else enum_helper(cards, h, lowest)
               end)
               | _ => enum_helper(cards, hl, lowest)
   in
      enum_helper(cs, cs, highest)
   end 


fun officiate_challenge (cs: card list, moves: move list, goal: int) =
   let
      fun has_sum_lt_goal (cl: card list, hl: card list, goal: int) =
         case cl of 
            [] => NONE 
            | c::cards => case c of 
               (s, Ace) => (let
                  val h = (s, Num 1)::remove_card(hl, c, IllegalMove)
               in
                  if sum_cards(h) <= goal then SOME h else has_sum_lt_goal(cards, h, goal)
               end)
               | _ => has_sum_lt_goal(cards, hl, goal)

      fun state_helper (hand: card list, cs: card list, moves: move list) =
         if sum_cards(hand) > goal then 
            case has_sum_lt_goal(hand, hand, goal) of 
               NONE => score_challenge(hand, goal)
               | SOME h => state_helper(h, cs, moves)
         else case moves of 
            [] => score_challenge(hand, goal)
            | mv::rest => case mv of 
                           Draw => (case cs of 
                              [] => score_challenge(hand, goal)
                              | c::deck => state_helper(c::hand, deck, rest)
                              )
                           | Discard(c) => state_helper(remove_card(hand, c, IllegalMove), cs, rest)
   in 
      state_helper([], cs, moves)
   end


fun careful_player (cs: card list, goal: int) =
   let
      fun is_zero_possible (hand: card list, deck: card list) =
         let 
            fun helper (hand: card list, to_remove: card list, topdeck: card) =
               case to_remove of 
                  [] => NONE
                  | c::rest => if score(topdeck::remove_card(hand, c, IllegalMove), goal) = 0
                  then SOME [Discard (c), Draw] else 
                  helper(hand, rest, topdeck)
         in 
            case deck of
               [] => NONE
               | topdeck::rest => helper(hand, hand, topdeck)
         end
         

      fun generate_move (deck: card list, hand: card list, moveset: move list, goal: int) =
         if goal - sum_cards(hand) > 10 then 
            case deck of 
               [] => moveset @ [Draw]
               | c::cards => generate_move(cards, c::hand, moveset @ [Draw], goal)
         else if score(hand, goal) = 0 then moveset
         else case is_zero_possible(hand, deck) of
         (* No game-ending move, draw if its safe otherwise stop *) 
            NONE => (case deck of 
               [] => moveset @ [Draw]
               | c::cards => if sum_cards(c::hand) <= goal 
               then generate_move(cards, c::hand, moveset @ [Draw], goal)
               else moveset)
            | SOME moves => moveset @ moves
   in 
      generate_move(cs, [], [], goal)
   end