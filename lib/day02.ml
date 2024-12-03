let is_diff_valid diff = abs(diff) >= 1 && abs(diff) <= 3

let same_sign_not_zero a b = (a > 0 && b > 0) || (a < 0 && b < 0)

let rec is_report_valid retry report =
    match report with
    | [] -> true
    | cur :: next :: [] -> retry || is_diff_valid (cur - next)
    | prev :: cur :: next :: rest ->
        let prevDiff = prev - cur in
        let nextDiff = cur - next in
        let areDiffsValid = (is_diff_valid prevDiff) && (is_diff_valid nextDiff) in
        let areDiffsSameSign = same_sign_not_zero prevDiff nextDiff in

        if areDiffsValid && areDiffsSameSign then
            is_report_valid retry (cur :: next :: rest)
        else if retry then
            is_report_valid false (prev :: next :: rest)
            || is_report_valid false (cur :: next :: rest)
            || is_report_valid false (prev :: cur :: rest)
        else
            false
    | _ -> true

let do_stuff reports retry =
    reports
    |> List.filter (is_report_valid retry)
    |> List.length
    |> (fun n -> Printf.printf "%d\n" n)

let run () =
    let reports =
        Utils.read_lines "assets/inputs/day02/input.txt" 
        |> List.map (String.split_on_char ' ') 
        |> List.map (fun line -> List.map int_of_string line) 
    in
    do_stuff reports false; (* Part 1 *)
    do_stuff reports true   (* Part 2 *)
