module Matrix = struct
    let is_within_bounds (row, col) mat =
        let rows = Array.length mat in
        let cols = if rows > 0 then Array.length mat.(0) else 0 in
        not (row < 0 || row >= rows || col < 0 || col >= cols)

    let get_opt (row, col) mat =
        if is_within_bounds (row, col) mat  then
            Some mat.(row).(col)
        else
            None

    let find_with_position func mat =
        let rows = Array.length mat in
        let cols = if rows > 0 then (Array.length mat.(0)) else 0 in
        if rows = 0 || cols = 0 then
            None
        else
            let rec aux row col =
                let ch = mat.(row).(col) in
                if func ch then
                    Some (ch, (row, col))
                else if (col + 1) < cols then
                    aux row (col + 1)
                else if (row + 1) < rows then
                    aux (row + 1) 0
                else
                    None
            in
                    
            aux 0 0

end

module Position = struct
    type t = int * int

    let compare (row, col) (row2, col2) = 
        if row < row2 then
            -1
        else if row > row2 then
            1
        else
            col - col2

    let add (row, col) (row2, col2) = (row + row2, col + col2)
end

module Position_set = Set.Make(Position)
module Position_map = Map.Make(Position)

let has_obstacle_at world pos =
    match Matrix.get_opt pos world  with
    | Some (ch) -> ch = '#'
    | None -> false

let rotate_right guard_dir =
    match guard_dir with
    | ( 0,  1)    -> ( 1,  0)
    | ( 1,  0)    -> ( 0, -1)
    | ( 0, -1)    -> (-1,  0)
    | (-1,  0)    -> ( 0,  1)
    | _           -> raise (Invalid_argument "Invalid dir to rotate")

let rec step world guard_pos guard_dir =
    let next_pos = Position.add guard_pos guard_dir in
    if not (Matrix.is_within_bounds  next_pos world) then
        Position_set.empty
    else if has_obstacle_at world next_pos then
        step world guard_pos (rotate_right guard_dir)
    else
        Position_set.add next_pos (step world next_pos guard_dir)

let adding_obstacle_creates_loop world initial_guard_pos initial_guard_dir =
    let obstacle_pos = Position.add initial_guard_pos initial_guard_dir in

    let rec aux guard_pos guard_dir visited =
        let next_pos = Position.add guard_pos guard_dir in
        let visited_pos = match Position_map.find_opt guard_pos visited with
            | Some (pos) -> pos
            | _          -> Position_set.empty
        in

        if not (Matrix.is_within_bounds  next_pos world) then
            false
        else if Position_set.exists (fun dir -> dir = guard_dir) visited_pos then
            true
        else if next_pos = obstacle_pos || has_obstacle_at world next_pos then
            aux guard_pos (rotate_right guard_dir) (Position_map.add guard_pos (Position_set.add guard_dir visited_pos) visited)
        else
            aux next_pos guard_dir (Position_map.add guard_pos (Position_set.add guard_dir visited_pos) visited)
    in

    aux initial_guard_pos (rotate_right initial_guard_dir) Position_map.empty

let step2 world initial_guard_pos guard_dir =
    let rec aux guard_pos guard_dir placed_obstacles visited =
        let next_pos = Position.add guard_pos guard_dir in

        let new_visited = if guard_pos != initial_guard_pos then Position_set.add guard_pos visited else visited in
        let new_placed_obstacles = Position_set.add next_pos placed_obstacles in
        let has_been_visited = Position_set.exists (fun other -> other = next_pos) visited in

        if not (Matrix.is_within_bounds  next_pos world) then
            placed_obstacles

        else if has_obstacle_at world next_pos then
            aux guard_pos (rotate_right guard_dir) placed_obstacles visited

        else if (not has_been_visited) && (adding_obstacle_creates_loop world guard_pos guard_dir) then
            aux next_pos guard_dir new_placed_obstacles new_visited

        else
            aux next_pos guard_dir placed_obstacles new_visited
    in

    aux initial_guard_pos guard_dir (Position_set.empty) (Position_set.empty)


let is_guard ch =
    match ch with
    | '>' -> true
    | 'v' -> true
    | '<' -> true
    | '^' -> true
    | _   -> false

let char_to_dir ch =
    match ch with
    | '>' -> ( 0,  1)
    | 'v' -> ( 1,  0)
    | '<' -> ( 0, -1)
    | '^' -> (-1,  0)
    | _   -> ( 0,  0)

let find_guard_pos_and_dir world =
    match Matrix.find_with_position is_guard world with
    | Some (ch, pos) -> Some (pos, char_to_dir ch)
    | None -> None

let part1 filename =
    let world = Utils.read_matrix filename in
    match find_guard_pos_and_dir world with
    | Some (pos, dir) -> 
            let unique_spots = step world pos dir in
            Printf.printf "%d\n" (Position_set.cardinal unique_spots)
    | None -> print_endline "Could not find guard"
 

let part2 filename =
    let world = Utils.read_matrix filename in
    match find_guard_pos_and_dir world with
    | Some (pos, dir) -> 
            let unique_spots = step2 world pos dir in
            Printf.printf "%d\n" (Position_set.cardinal unique_spots)

    | None -> print_endline "Could not find guard"
    
let run () = 
    part1 "assets/inputs/day06/input.txt";
    part2 "assets/inputs/day06/input.txt"
