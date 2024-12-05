let read_lines file =
    let lines = ref [] in
    let ic = open_in file in

    try
        while true do
            let line = input_line ic in
            lines := line :: !lines
        done; 

        !lines

    with 
    | End_of_file ->
        close_in_noerr ic;
        List.rev !lines
    | e ->
        close_in_noerr ic;
        raise e

let rec join separator l =
    match l with
    | [] -> ""
    | cur :: [] -> cur
    | cur :: rest -> (cur ^ separator) ^ (join separator rest)
    

let read_to_str file =
    read_lines file
    |> join "\n"

let rec print_lines lines =
    match lines with
    | [] -> ()
    | line :: rest -> print_endline line; print_lines rest

let is_digit = function '0' .. '9' -> true | _ -> false

let substr_after str n = String.sub str n ((String.length str) - n)

let array_of_string s = Array.init (String.length s) (String.get s)

let print_mat mat =
    let rows_len = Array.length mat in
    let cols_len = Array.length mat.(0) in
    for row = 0 to rows_len - 1 do
        for col = 0 to cols_len - 1 do
            Printf.printf "%c " mat.(row).(col)
        done;
        print_endline ""
    done

