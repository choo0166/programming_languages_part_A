(* A date has type int*int*int where #1 is the year, #2 is the month, #3 is the day *)

(* Q1 *)
fun is_older (d1: int*int*int, d2: int*int*int) =
	if #1 d1 < #1 d2 then true
	else if #1 d1 = #1 d2 andalso #2 d1 < #2 d2 then true
	else if #1 d1 = #1 d2 andalso #2 d1 = #2 d2 andalso #3 d1 < #3 d2 then true
	else false


(* Q2 *)
fun number_in_month (dates: (int*int*int) list, month: int) =
	if null dates
	then 0
	else if #2 (hd(dates)) = month then 1 + number_in_month(tl(dates), month)
	else number_in_month(tl(dates), month)


(* Q3 *)
fun number_in_months (dates: (int*int*int) list, months: int list) =
	if null months then 0
	else number_in_month(dates, hd(months)) + number_in_months(dates, tl(months))


(* Q4 *)
fun dates_in_month (dates: (int*int*int) list, month: int) =
	if null dates then []
	else if #2 (hd(dates)) = month then hd(dates)::dates_in_month(tl(dates), month)
	else dates_in_month(tl(dates), month)


(* Q5 *)
fun dates_in_months (dates: (int*int*int) list, months: int list) =
	if null months then []
	else dates_in_month(dates, hd(months)) @ dates_in_months(dates, tl(months))


(* Q6 *)
fun get_nth (xs: string list, n: int) =
	if n = 1 then hd(xs)
	else get_nth(tl(xs), n-1)


(* Q7 *)
fun date_to_string (date: int*int*int) =
	let
		val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
		val month = get_nth(months, #2 date)
	in
		month ^ " " ^ (Int.toString(#3 date)) ^ "," ^ " " ^ (Int.toString(#1 date))
	end


(* Q8 *)
fun number_before_reaching_sum (sum: int, xs: int list) =
	let
		val length = List.length(xs)
		fun sum_helper (start: int, x: int list, cum_sum: int) = 
			let
				val num = if null x then 0 else hd(x)
				exception NoValidIntegerFound
			in
				if start > length then raise NoValidIntegerFound
				else if cum_sum + num < sum then sum_helper(start + 1, tl(x), cum_sum + num)
				else start - 1
			end		
	in
		sum_helper(1, xs, 0)
	end


(* Q9 *)
fun what_month (day: int) =
	let 
		val num_days_by_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	in
		number_before_reaching_sum(day, num_days_by_month) + 1
	end


(* Q10 *)
fun month_range (day1: int, day2: int) =
	if day1 > day2 then []
	else what_month(day1) :: month_range(day1 + 1, day2)
	

(* Q11 *)
fun oldest (dates: (int*int*int) list) =
	if null dates then NONE
	else let
		fun oldest_helper (xs: (int*int*int) list, curr_oldest: int*int*int) =
			if null xs then curr_oldest
			else if is_older(hd(xs), curr_oldest) then oldest_helper(tl(xs), hd(xs))
			else oldest_helper(tl(xs), curr_oldest)
	in
		SOME (oldest_helper(tl(dates), hd(dates)))
	end


(* Q12 *)
fun number_in_months_challenge (dates: (int*int*int) list, months: int list) =
	let
		fun isNewElement (e: int, x: int list) =
			if null x then true
			else if hd(x) = e then false
			else isNewElement(e, tl(x))

		fun remove_dups (xs: int list, new: int list) =
			if null xs then new
			else if isNewElement(hd(xs), new) then remove_dups(tl(xs), hd(xs)::new)
			else remove_dups(tl(xs), new)
	in
		let
			val unique_months = remove_dups(months, [hd(months)])
		in
			number_in_months(dates, unique_months) 
		end
	end


(* Q13 *)
fun reasonable_date (date: int*int*int) =
	if #1 date <= 0 then false
	else if #2 date < 1 orelse #2 date > 12 then false
	else
	let
		fun isLeapYear (year: int) =
			if (year mod 400 = 0 orelse year mod 4 = 0) andalso (year mod 100 > 0) then true 
			else false

		fun get_nth_ele (xs: int list, n: int) =
			if n = 1 then hd(xs)
			else get_nth_ele(tl(xs), n-1)

		fun isValidDayinMonth (year: int, day: int, month: int) =
			let 
				val num_days_by_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
				val num_days_by_month_leap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
			in 
				if isLeapYear(year) andalso (day >= 1 andalso day <= get_nth_ele(num_days_by_month_leap, month)) then true
				else if not (isLeapYear(year)) andalso (day >= 1 andalso day <= get_nth_ele(num_days_by_month, month)) then true
				else false
			end	
	in
		isValidDayinMonth(#1 date, #3 date, #2 date)
	end


