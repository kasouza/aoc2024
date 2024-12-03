let rec calculate_differences line =
    match line with
    | [] -> []
    | _ :: [] -> []
    | cur :: next :: rest -> cur - next :: calculate_differences (next :: rest)

let is_valid_diff diff = abs(diff) >= 1 && abs(diff) <= 3

let same_sign_not_zero a b = (a > 0 && b > 0) || (a < 0 && b < 0)

let rec valid_diffs diffs =
    match diffs with
    | [] -> true
    | a :: [] -> is_valid_diff a
    | a :: b :: rest -> if  same_sign_not_zero a b then
        (is_valid_diff a) && valid_diffs (b :: rest) 
        else 
            false

let run () =
    Utils.read_lines "assets/inputs/day02/input.txt"
    |> List.map (String.split_on_char ' ')
    |> List.map (fun line -> List.map int_of_string line)
    |> List.map calculate_differences
    |> List.filter valid_diffs
    |> List.length
    |> print_int

    
