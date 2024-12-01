let parse_line line =
    line
    |> String.split_on_char ' '
    |> List.filter (fun s -> s <> "")
    |> List.map (int_of_string)
    |> fun list -> (List.nth list 0), (List.nth list 1)

let calculate_distance location_pair =
    match location_pair with
    (left, right) -> abs (left - right)

let sort_lists lists =
    match lists with
    (left_list, right_list) -> ((List.sort compare left_list), (List.sort compare right_list)) 

let combine_lists lists =
    match lists with
    (left_list, right_list) -> List.combine left_list right_list

let read_and_parse_lists file =
    file
    |> Utils.read_lines
    |> List.map parse_line
    |> List.split
    |> sort_lists

let calculate_total_distance lists =
    lists
    |> combine_lists
    |> List.map calculate_distance
    |> List.fold_left (fun acc distance -> acc + distance) 0

let count_occurences n items =
    List.fold_left (fun acc m -> acc + if n = m then  1 else 0) 0 items

let calculate_total_similarity_score (left, right) =
    left
    |> List.map (fun n -> n * (count_occurences n right))
    |> List.fold_left (fun acc n -> acc + n) 0

let run () =
    let lists = read_and_parse_lists "assets/inputs/day01/input.txt" in
    let (total_distance, total_similarity_score) = (calculate_total_distance lists, calculate_total_similarity_score lists) in
    Printf.printf "Total distance: %d | Total similarity score %d\n" total_distance total_similarity_score
