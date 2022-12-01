(* Community practice problems - Week 2 *)

(* Q1 *)
fun alternate (xs: int list) =
	let
		fun sum_helper (flag: int, x: int list) = 
			if null x then 0
			else if flag < 0 then ~(hd(x)) + sum_helper(flag*(~1), tl(x))
			else hd(x) + sum_helper(flag*(~1), tl(x))
	in
		sum_helper(1, xs)
	end


(* Q2 *)
fun min_max (xs: int list) =
	let
		fun min_max_helper (x: int list, curr_min: int, curr_max: int) =
		 	if null x then (curr_min, curr_max)
		 	else if hd(x) < curr_min then min_max_helper(tl(x), hd(x), curr_max)
		 	else if hd(x) > curr_max then min_max_helper(tl(x), curr_min, hd(x))
		 	else min_max_helper(tl(x), curr_min, curr_max)
	in
		min_max_helper(xs, hd(xs), hd(xs))
	end


(* Q3 *)
fun cumsum (xs: int list) =
	let
		fun sum_helper (x: int list, psum: int) = 
			if null x then []
			else hd(x) + psum :: sum_helper(tl(x), hd(x) + psum)
	in
		sum_helper(xs, 0)
	end


(* Q4 *)
fun greeting (name: string option) =
	if isSome name then "Hello there, " ^ valOf (name) ^ "!"
	else "Hello there, you!"


(* Q5 *)
fun repeat (xs: int list, times: int list) =
	let
		fun mult (x: int, multiplier: int ) =
		 	if multiplier = 0 then []
		 	else x :: mult(x, multiplier-1)
	in
		if null xs then []
		else mult(hd(xs), hd(times)) @ repeat(tl(xs), tl(times))
	end


(* Q6 *)
fun addOpt (tup: int option * int option) =
	if isSome (#1 tup) andalso isSome (#2 tup) then SOME (valOf (#1 tup) + valOf (#2 tup))
	else NONE


(* Q7 *)
fun addAllOpt (xs: int option list) = 
	if null xs then NONE
	else let
		fun helper (x: int option list, sum: int option) =
			if null x then sum 
			else let
				val newsum = addOpt((hd(x), sum))
			in
				if not (isSome (sum)) andalso isSome (hd(x)) then helper(tl(x), hd(x))
				else if isSome (newsum) then helper(tl(x), newsum)
				else helper(tl(x), sum)
			end
	in
		helper(tl(xs), hd(xs))
	end


(* Q8 *)
fun any (xs: bool list) = 
	if null xs then false
	else let
		fun helper (x: bool list, last: bool) =
		 	if null x andalso not (last) then false
		 	else if last then true
		 	else helper(tl(x), hd(x))
	in
		helper(xs, hd(xs))
	end


(* Q9 *)
fun all (xs: bool list) =
	if null xs then true 
	else let
		fun helper (x: bool list, last: bool) =
		 	if null x andalso last then true 
		 	else if not (last) then false
		 	else helper(tl(x), hd(x))
	in
		helper(xs, hd(xs))
	end


(* Q10 *)
fun zip (l1: int list, l2: int list) =
	if null l1 orelse null l2 then []
	else (hd(l1), hd(l2)) :: zip(tl(l1), tl(l2))


(* Q11 *)
fun zipRecycle (l1: int list, l2: int list) =
	if null l1 orelse null l2 then []
	else let
		val length1 = List.length(l1)
		val length2 = List.length(l2)

		fun zip_helper (a: int list, b: int list, ans: (int*int) list) =
			if length2 > length1 andalso null a then zip_helper(l1, b, ans) (* 1st list is exhausted first *)
			else if length2 > length1 andalso null b then ans 
			else if length1 > length2 andalso null b then zip_helper(a, l2, ans) (* 2nd list is exhausted first *)
			else if length1 > length2 andalso null a then ans
			else zip_helper(tl(a), tl(b), ans @ [(hd(a), hd(b))]) (* continue if none of the lists are exhausted *)	
	in
		if length1 = length2 then zip (l1, l2)
		else zip_helper(tl(l1), tl(l2), [(hd(l1), hd(l2))])
	end


(* Q12 *)
fun zipOpt (l1: int list, l2: int list) =
	let
		val length1 = List.length(l1)
		val length2 = List.length(l2)
	in
		if length1 = length2 then SOME (zip(l1, l2))
		else NONE
	end


(* Q13 *)
fun lookup (xs: (string * int) list, s: string) =
	if null xs then NONE
	else if #1 (hd(xs)) = s then SOME (#2 (hd(xs)))
	else lookup (tl(xs), s)


(* Q14 *)
fun splitup (xs: int list) =
	let
		fun helper (x: int list, pos: int list, neg: int list) =
			if null x then (pos, neg)
			else if hd(x) >= 0 then helper(tl(x), pos @ [hd(x)], neg)
			else helper(tl(x), pos, neg @ [hd(x)])
	in
		helper(xs, [], [])
	end


(* Q15 *)
fun splitAt (xs: int list, thres: int) = 
	let
		fun helper (x: int list, gte: int list, lt: int list) =
			if null x then (gte, lt)
			else if hd(x) >= thres then helper(tl(x), gte @ [hd(x)], lt)
			else helper(tl(x), gte, lt @ [hd(x)])
	in
		helper(xs, [], [])
	end


(* Q16 *)
fun isSorted (xs: int list) = 
	let
		fun helper (x: int list, prev: int) =
			if null x then true 
			else if hd(x) >= prev then helper(tl(x), hd(x))
			else false
	in
		helper(tl(xs), hd(xs))
	end


(* Q17 *)
fun isAnySorted (xs: int list) = 
	let
		fun isIncreasing (x: int list, prev: int) =
			if null x then true 
			else if hd(x) >= prev then isIncreasing(tl(x), hd(x))
			else false

		fun isDecreasing (x: int list, prev: int) = 
			if null x then true 
			else if hd(x) <= prev then isDecreasing(tl(x), hd(x))
			else false
	in
		if isIncreasing(tl(xs), hd(xs)) orelse isDecreasing(tl(xs), hd(xs))
		then true 
		else false
	end


(* Q18 *)
fun sortedMerge (l1: int list, l2: int list) = 
	let
		fun helper (l1: int list, l2: int list, sortedLst: int list) =
			if null l1 andalso not (null l2) then sortedLst @ l2
			else if null l2 andalso not (null l1) then sortedLst @l1 
			else if hd(l1) <= hd(l2) then helper(tl(l1), l2, sortedLst @ [hd(l1)])
			else helper(l1, tl(l2), sortedLst @ [hd(l2)])
	in
		helper(l1, l2, [])
	end


(* Q19 *)
fun qsort (xs: int list) =
	let
		val pivot = hd(xs)
		val splits = splitAt(tl(xs), pivot)
		
		fun partition (x: int list) =
			if null x then []
			else if null (tl(x)) then x (* 1 element list is sorted *)
			else let
				val pivot = hd(x)
				val splits = splitAt(tl(x), pivot)
			in
				(* recursively split and combine list in order of elements < pivot, pivot, elements > pivot) *)
				partition(#2 splits) @ [pivot] @ partition(#1 splits) 
			end
	in
		partition(#2 splits) @ [pivot] @ partition(#1 splits)
	end


(* Q20 *)
fun divide (xs: int list) =
	let
		fun helper (x: int list, flag: int, l1: int list, l2: int list) = 
			if null x then (l1, l2)
			else if flag > 0 then helper(tl(x), flag*(~1), l1 @ [hd(x)], l2)
			else helper(tl(x), flag*(~1), l1, l2 @ [hd(x)])
	in
		helper(xs, 1, [], [])
	end


(* Q21 *)
fun not_so_quick_sort (xs: int list) =
	let
		fun partition (x: int list) =
			if null x then []
			else if null (tl(x)) then x 
			else let
				val splits = divide(x)
			in
				sortedMerge(partition(#1 splits), partition(#2 splits))
			end
	in
		partition(xs)
	end


(* Q22 *)
fun fullDivide (k: int, n: int) =
	let
		fun helper (d: int, res: int) =
			if res mod k = 0 then helper(d+1, res div k)
			else (d, res)
	in
		helper(0, n)
	end


(* Q23 *)
fun factorize (n: int) =
	let
		fun sqrt (a: int, s: int) = 
			if s*s <= a then s 
			else sqrt(a, s-1)

		val thres = sqrt(n, n-1)

		fun helper (x: int, k: int, res: (int * int) list) = 
			if x = 1 orelse k > thres + 1 then res 
			else let
				val result = fullDivide(k, x)
			in
				if #1 result = 0 then helper(x, k+1, res)
				else helper(#2 result, k+1, res @ [(k, #1 result)])
			end
	in
		helper(n, 2, [])
	end


(* Q24 *)
fun multiply (xs: (int * int) list) =
	let
		fun pow (x: int, y: int) = 
			if y=0 then 1
			else x * pow(x,y-1)
	in
		if null xs then 1
		else (pow(#1 (hd(xs)), #2 (hd(xs)))) * multiply(tl(xs))
	end



