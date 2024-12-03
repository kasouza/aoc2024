let is_valid_diff diff = abs(diff) >= 1 && abs(diff) <= 3

let same_sign_not_zero a b = (a > 0 && b > 0) || (a < 0 && b < 0)

let rec calculate_differences line =
    match line with
    | [] -> []
    | _ :: [] -> []
    | cur :: next :: rest -> cur - next :: calculate_differences (next :: rest)

let rec valid_diffs diffs =
    match diffs with
    | [] -> true
    | a :: [] -> is_valid_diff a
    | a :: b :: rest -> if  same_sign_not_zero a b then
        (is_valid_diff a) && valid_diffs (b :: rest) 
        else 
            false

let part1 file =
    Utils.read_lines file
    |> List.map (String.split_on_char ' ')
    |> List.map (fun line -> List.map int_of_string line)
    |> List.map calculate_differences
    |> List.filter valid_diffs
    |> List.length
    |> print_int;
    print_endline ""


let rec whatever retry report =
    match report with
    | [] -> true
    | cur :: next :: [] -> retry || is_valid_diff (cur - next)
    | prev :: cur :: next :: rest ->
        let prevDiff = prev - cur in
        let nextDiff = cur - next in
        let areDiffsValid = (is_valid_diff prevDiff) && (is_valid_diff nextDiff) in
        let areDiffsSameSign = same_sign_not_zero prevDiff nextDiff in

        if areDiffsValid && areDiffsSameSign then
            whatever retry (cur :: next :: rest)
        else if retry then
            whatever false (prev :: next :: rest)
            || whatever false (cur :: next :: rest)
            || whatever false (prev :: cur :: rest)
        else
            false
    | _ -> true

let part2 file =
    Utils.read_lines file
    |> List.map (String.split_on_char ' ')
    |> List.map (fun line -> List.map int_of_string line)
    |> List.filter (whatever true)
    |> List.length
    |> print_int;
    print_endline ""

let run () =
    part1 "assets/inputs/day02/input_short.txt";
    part2 "assets/inputs/day02/input.txt";
    print_endline ""
