fun is_older (date1: int*int*int, date2: int*int*int) = 
    (#1 date1 < #1 date2) orelse (#2 date1 < #2 date2) orelse (#3 date1 < #3 date2)

fun number_in_month (date_list: (int*int*int) list, month: int) = 
    if null date_list
    then 0
    else
        let val count_next = (number_in_month ((tl date_list),month))
        in
            if (#2 (hd date_list) = month)
            then (count_next + 1)
            else count_next
        end
        
fun number_in_months (date_list: (int*int*int) list, month_list: int list) = 
    if null month_list
    then []
    else
        let val curr_month_count = (number_in_month (date_list,(hd month_list)))
        in
            curr_month_count :: (number_in_months (date_list,(tl month_list)))
        end

fun dates_in_month (date_list: (int*int*int) list, month: int) = 
    if null date_list
    then []
    else
        let 
            val curr_date = (hd date_list)
            val list_next = (dates_in_month ((tl date_list),month))
        in
            if (#2 curr_date = month)
            then  curr_date :: list_next
            else list_next
        end 

fun dates_in_months (date_list: (int*int*int) list, month_list: int list) = 
    if null month_list
    then []
    else
        let val curr_dateList = (dates_in_month (date_list,(hd month_list)))
        in
            curr_dateList :: (dates_in_months (date_list,(tl month_list)))
        end

fun get_nth (str_list: string list, nth: int) =
    if (nth <= 1)
    then (hd str_list)
    (* else if (nth = 0)
    then (last str_list) 
    else if (nth < 0)
    then (get_nth (last (take (str_list,(~ nth))))) *)
    else (get_nth ((tl str_list),(nth - 1)))

fun date_to_string (date: int*int*int) =
    let
        val month_str_list = [ "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
        (get_nth (month_str_list,(#2 date))) ^ " " ^ (Int.toString (#3 date)) ^ ", " ^ (Int.toString (#1 date)) 
    end

fun number_before_reaching_sum (target:int, n_list: int list) =
    let  
        fun get_num (target:int, n_list: int list, prev_sum: int, index: int) =
            if (null n_list)
            then index
            else 
                let 
                    val curr_sum = prev_sum + (hd n_list)
                in
                    if (curr_sum < target)
                    then get_num (target,(tl n_list),curr_sum,(index+1))
                    else index
                end
    in
        get_num (target,n_list,0,1)
    end

fun what_month(n: int) =
    let
        val month_date_nums_list = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
        number_before_reaching_sum (n,month_date_nums_list)
    end

fun month_range (day1: int, day2: int) =
    if (day1 > day2)
    then []
    else
        let
            fun intRangeList (n1: int, n2: int) =
                if (n1 > n2)
                then []
                else
                    n1 :: (intRangeList((n1 + 1), n2))
        in
            intRangeList ((what_month day1),(what_month day2))
        end

fun oldest (date_list: (int*int*int) list) =
    if (null date_list)
    then NONE
    else
        let 
            fun find_oldest (date_list: (int*int*int) list, last_oldest: (int*int*int)) = 
                if (null date_list)
                then last_oldest
                else 
                    let 
                        val curr_date = hd date_list
                    in
                        if (is_older (curr_date, last_oldest))
                        then find_oldest((tl date_list), curr_date)
                        else find_oldest((tl date_list), last_oldest)
                    end
        in
            SOME (find_oldest((tl date_list), (hd date_list)))
        end

fun rm_duplicate_month (month_list: int list) =
    let 
        fun doRm (month_list: int list, prev_month_set : int list) =
            if (null month_list)
            then prev_month_set
            else
                let
                    fun contains (month_list: int list, month: int) =
                        if (null month_list)
                        then false
                        else if ((hd month_list) = month)
                        then true
                        else contains((tl month_list),month)
                    val curr_month = hd month_list
                in 
                    if (contains(prev_month_set,curr_month))
                    then doRm ((tl month_list), prev_month_set)
                    else doRm ((tl month_list), (curr_month :: prev_month_set))
                end
    in
        doRm (month_list,[])
    end

fun number_in_months_challenge (date_list: (int*int*int) list, month_list: int list) = 
    number_in_months (date_list, (rm_duplicate_month month_list))
    
fun dates_in_months_challenge (date_list: (int*int*int) list, month_list: int list) = 
    dates_in_months (date_list, (rm_duplicate_month month_list))

fun reasonable_date (date: (int*int*int)) =
    if ((#1 date) <= 0)
    then false
    else if (((#2 date) < 1) orelse ((#2 date) > 12))
    then false
    else if  ((#3 date) < 1)
    then false
    else if ((#2 date) = 2)
    then
        if ((((#1 date) mod 4) = 0) andalso (((#1 date) mod 400) = 0) andalso (not (((#1 date) mod 100) = 0)))
        then 
            ((#3 date) <= 29)
        else
            ((#3 date) <= 28)
    else
        let
            val month_date_nums_list = [31,28,31,30,31,30,31,31,30,31,30,31]
            fun get_nth_int (int_list: int list, nth: int) =
                if (nth <= 1)
                then (hd int_list)
                else (get_nth_int ((tl int_list),(nth - 1)))
        in
            ((#3 date) <= (get_nth_int (month_date_nums_list,(#2 date))))
        end
