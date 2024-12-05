(*
TODO: The `check_direction` should also check the first character 
instead of we checking it manually beforing calling the function
*)
(* Directions and sequences setup *)
let directions = [
    (-1, -1); (0, -1); (1, -1);
    (-1,  0);           (1, 0);
    (-1,  1); (0,  1); (1,  1);
]

let sequence = [
    'M'; 'A'; 'S'
]

let mas_sequence = [ 'A'; 'S' ]
let sam_sequence = [ 'A'; 'M' ]

(* Common functions *)
let read_matrix filename =
    Utils.read_lines filename
    |> List.map Utils.array_of_string
    |> Array.of_list

let is_within_bounds mat (row, col) =
    let rows = Array.length mat in
    let cols = if rows > 0 then Array.length mat.(0) else 0 in
    not (row < 0 || row >= rows || col < 0 || col >= cols)

let rec check_direction mat (row, col) sequence (dir_row, dir_col) = 
    let (new_row, new_col) = (row + dir_row, col + dir_col) in
    if is_within_bounds mat (new_row, new_col) then
        match sequence with
        | cur :: [] when mat.(new_row).(new_col) = cur -> 1
        | cur :: rest when mat.(new_row).(new_col) = cur -> check_direction mat (new_row, new_col) rest (dir_row, dir_col)
        | _ ->0
    else
        0

let rec check_directions mat pos sequence directions =
    match directions with
    | [] -> 0
    | dir :: rest -> (check_direction mat pos sequence dir) + (check_directions mat pos sequence rest)

(* Part 1 *)
let part1 mat =
    let xs_len = Array.length mat in
    let ys_len = Array.length mat.(0) in
    let sum = ref 0 in
    for x = 0 to xs_len - 1 do
        for y = 0 to ys_len - 1 do
            if mat.(x).(y) = 'X' then
                sum := !sum + check_directions mat (x, y) sequence directions
        done;
    done;
    !sum

(* Part 2 *)
let check_mas mat (row, col) dir_to_check =
    if is_within_bounds mat (row, col) then
        match mat.(row).(col) with
        | 'M' -> (check_direction mat (row, col) mas_sequence dir_to_check)
        | 'S' -> (check_direction mat (row, col) sam_sequence dir_to_check)
        | _ -> 0
    else
        0

let part2 mat = 
    let rows = Array.length mat in
    let cols = Array.length mat.(0) in
    let sum = ref 0 in
    for row = 0 to rows - 1 do
        for col = 0 to cols - 1 do
            if mat.(row).(col) = 'A' then
                let left_to_right = check_mas mat (row - 1, col - 1) (1, 1) in
                let rigth_to_left = (check_mas mat (row - 1, col + 1) (1, -1)) in
                if left_to_right + rigth_to_left == 2 then
                     sum := !sum + 1
        done;
    done;
    !sum

let run () = 
    read_matrix "assets/inputs/day04/input_short.txt"
    |> part1
    |> print_int;

    print_endline "";

    read_matrix "assets/inputs/day04/input.txt"
    |> part2
    |> print_int;

    print_endline "";

