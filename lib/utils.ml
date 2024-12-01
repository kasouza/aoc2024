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
