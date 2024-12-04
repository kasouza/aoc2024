type token = 
    | Open_parem
    | Close_parem
    | Comma
    | Mul
    | Num of int
    | Other of char
    | Do
    | Dont

let rec tok_num str num =
    match str.[0] with
    | ch when Utils.is_digit ch -> tok_num (Utils.substr_after str 1) (num ^ (String.make 1 ch))
    | _ -> Num (int_of_string num) :: tokenize str

and tokenize str =
    let str_len = String.length str in
    if str_len = 0 then
        []
    else match str.[0] with
        | 'm' when String.starts_with str ~prefix:"mul"     -> Mul  :: tokenize (Utils.substr_after str 3)
        | 'd' when String.starts_with str ~prefix:"don't"   -> Dont :: tokenize (Utils.substr_after str 5)
        | 'd' when String.starts_with str ~prefix:"do"      -> Do   :: tokenize (Utils.substr_after str 2)
        | '('                                               -> Open_parem :: tokenize (Utils.substr_after str 1)
        | ')'                                               -> Close_parem :: tokenize (Utils.substr_after str 1)
        | ','                                               -> Comma :: tokenize (Utils.substr_after str 1)
        | ch when Utils.is_digit ch                         -> tok_num str ""
        | ch                                                -> Other ch :: tokenize (Utils.substr_after str 1)

let rec parse doing tokens =
    match tokens with
    | Do :: Open_parem :: Close_parem :: rest -> parse true rest
    | Dont :: Open_parem :: Close_parem :: rest -> parse false rest
    | Mul :: Open_parem :: Num (a) :: Comma :: Num(b) :: Close_parem :: rest ->
            if (a < 1000) && (b < 1000) && doing then
                (a * b) + parse doing rest
            else
                parse doing rest
    | _ :: rest -> parse doing rest
    | [] -> 0

let run () = 
    Utils.read_to_str "assets/inputs/day03/input.txt"
    |> tokenize
    |> parse true
    |> print_int;
     print_endline ""
