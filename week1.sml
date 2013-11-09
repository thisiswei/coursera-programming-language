fun is_older(y: int*int*int, x: int*int*int) =
    let val m1 = #2y
        val m2 = #2x
    in
        (#1 x) >= (#1 y) andalso
        (m2 > m1 orelse (m2 = m1 andalso (#3 x) > (#3 y)))
    end

fun number_in_month(date: (int*int*int) list, m:int) =
    if null date
    then 0
    else let val rest = number_in_month(tl date, m)
         in if (#2 (hd date)) = m then 1 + rest  else rest
         end

fun number_in_months(date: (int*int*int) list, months: int list) =
    if null months orelse null date
    then 0
    else number_in_month(date, hd months) + number_in_months(date, tl months)

fun dates_in_month(date: (int*int*int) list, month: int) =
    if null date
    then []
    else let val head = hd date
             val rest = dates_in_month(tl date, month)
         in
             if #2 head = month
             then head :: rest
             else rest
         end

fun dates_in_months(date: (int*int*int) list, months: int list) =
    if null date orelse null months
    then []
    else dates_in_month(date, hd months) @ dates_in_months(date, tl months)

fun get_nth(lis: string list, ith: int) =
    if ith = 1
    then hd lis
    else get_nth(tl lis, ith-1)

val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]

fun date_to_string(date: int*int*int) =
    let val year = Int.toString (#1 date)
        val month = #2 date
        val day = Int.toString (#3 date)
    in
        get_nth(months, month) ^ " " ^ day ^ " " ^ year
    end

fun number_before_reaching_sum(sum: int, lis: int list) =
    let val head = hd lis
    in
        if head >= sum
        then 0
        else 1 + number_before_reaching_sum(sum - head, tl lis)
    end

val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
fun what_month(day_number: int) =
    1 + number_before_reaching_sum(day_number, days)

fun month_range(day1: int, day2: int) =
    if day1 > day2
    then []
    else what_month day1 :: month_range(day1 + 1, day2)

(* didnt get the correct answer

fun oldest(dates: (int*int*int) list) =
    if null dates
    then NONE
    else let val rest = oldest(tl dates)
             val head = hd dates
         in
             if is_older(valOf rest, head) andalso isSome rest then SOME rest else SOME head
         end
*)
