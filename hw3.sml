(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

(**** you can put all your code here ****)

(* Q1 *)
fun only_capitals xs = List.filter (fn s => Char.isUpper(String.sub (s, 0))) xs


(* Q2 *)
fun longest_string1 xs = 
	case xs of
		[] => ""
		| s::tl => List.foldl (fn (s', longest) => if String.size(s') > String.size(longest) then s' else longest) s tl


(* Q3 *)
fun longest_string2 xs =
		case xs of
		[] => ""
		| s::tl => List.foldl (fn (s', longest) => if String.size(s') >= String.size(longest) then s' else longest) s tl


(* Q4 *)
fun longest_string_helper f = fn xs =>
	case xs of
		[] => ""
		| s::tl => List.foldl (fn (s', longest) => if f(String.size(s'), String.size(longest)) then s' else longest) s tl
val longest_string3 = longest_string_helper (fn (x,y) => x > y)
val longest_string4 = longest_string_helper (fn (x,y) => x >= y)


(* Q5 *)
val longest_capitalized = longest_string1 o only_capitals 


(* Q6 *)
val rev_string = String.implode o List.rev o String.explode


(* Q7 *)
fun first_answer f = fn xs => case xs of 
	[] => raise NoAnswer 
	| x::tl => case f x of 
		SOME v => v 
		| NONE => first_answer f tl


(* Q8 *)
fun all_answers f = fn xs => 
	let fun helper f = fn acc => fn xs' =>
		case xs' of 
		[] => SOME acc 
		| x::tl => case f x of 
			NONE => NONE
			| SOME v => helper f (acc @ v) tl
	in
		helper f [] xs
	end


(** Provided code for Q9 - Q12 **)
datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
		val r = g f1 f2 
    in
		case p of
		    Wildcard          => f1 ()
		  | Variable x        => f2 x
		  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
		  | ConstructorP(_,p) => r p
		  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string


(* Q9a *)
fun count_wildcards p = 
	g (fn _ => 1) (fn _ => 0) p

(* Q9b *)
fun count_wild_and_variable_lengths p =
	g (fn _ => 1) String.size p 

(* Q9c *)
fun count_some_var (s: string, p: pattern) =
	g (fn _ => 0) (fn x => if x = s then 1 else 0) p


(* Q10 *)
fun check_pat p = 
	let
		fun get_variable_names p acc =
			let 
				val r = get_variable_names
			in
				case p of 
					Variable x => x::acc
					| TupleP pl => List.foldl (fn (p, i) => i @ (r p [])) acc pl
					| ConstructorP(_,p) => r p acc
					| _ => []
			end

		fun all_distinct xs = 
			case xs of 
				[] => true 
				| x::xs' => if List.exists (fn v => v = x) xs' then false else all_distinct xs'

		val variable_names = get_variable_names p []
	in
		all_distinct(variable_names)
	end


(* Q11 *)
fun match pair = 
	let 
		val r = match
	in 
		case pair of 
			(_, Wildcard) => SOME []
			| (v, Variable s) => SOME [(s,v)]
			| (Unit, UnitP) => SOME []
			| (Const x, ConstP y) => if x = y then SOME [] else NONE 
			| (Tuple vl, TupleP pl) => if List.length vl = List.length pl then 
				let
					val pairs = ListPair.zip(vl, pl)
				in
					all_answers r pairs (* all_answers function argument expects tuple *)
				end
			else NONE
			| (Constructor(s2, v), ConstructorP(s1, p)) => if s1 = s2 then r (v,p) else NONE 
			| _ => NONE
	end


(* Q12 *)
fun first_match v xs = 
	let
		(* curry match function from pair to partial application 
		of valu and pattern arguments *) 
		fun curry f x = fn y => f (x, y)
	in 
		SOME (first_answer ((curry match v)) xs)
		handle NoAnswer => NONE
	end


