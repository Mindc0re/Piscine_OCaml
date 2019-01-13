type t = None | Some of (int * int)

let intsOfString str =
    let lst = String.split_on_char ' ' str in
    if List.length lst <> 2 then None
    else match lst with
    | [] -> None
    | x::y::tl -> Some ( begin if int_of_string_opt x = None then ~-50 else (int_of_string x) - 1 end,
                         begin if int_of_string_opt y = None then ~-50 else (int_of_string y) - 1 end )
    | _::tl -> None

let getLine player board =
    let rec repeatGetLine str board =
        match intsOfString str with
        | None -> print_endline "Incorrect format." ; repeatGetLine (read_line ()) board
        | Some (x,y) when x = ~-50 || y = ~-50 -> print_endline "Incorrect format." ; repeatGetLine (read_line ()) board
        | Some (x,y) when x > 8 || y > 8 || x < 0 || y < 0 -> print_endline "Illegal move." ; repeatGetLine (read_line ()) board
        | Some (x,y) when Board.isMoveValid x y board = false -> print_endline "Illegal move." ; repeatGetLine (read_line ()) board
        | Some (x,y) -> (x,y)
    in
    print_newline ();print_string player;print_endline "'s turn to play." ; repeatGetLine (read_line ()) board


let rec game player1 player2 board = play player1 player2 board (getLine player1 board)

and play player1 player2 board (x,y) = 
    let updatedBoard = Board.updateBoard x y (Grid.Cell.toCell player1) board in
    match Board.isWon updatedBoard with
    | false -> print_newline () ; Board.printBoard @@ updatedBoard ; game player2 player1 updatedBoard
    | true  -> print_endline @@ (player1 ^ " wins the game!");print_newline ();Board.printBoard updatedBoard


let main () =
    let board = Board.createBoard () in
    Board.printBoard board ;
    game "O" "X" board





let () = main ()
