module Cell =
struct

    type c = O | X | None

    let toString = function
        | None -> "-"
        | O -> "O"
        | X -> "X"

    let toCell = function
        | "O" -> O
        | "X" -> X
        | _   -> None

end

type g = (Cell.c * Cell.c list)

let createGrid () = Cell.None , [Cell.None;Cell.None;Cell.None;Cell.None;Cell.None;Cell.None;Cell.None;Cell.None;Cell.None]

(*n should be > 0 and < 4*)
let toStringLine n (state, lst) =
    let rec toStringLineAux n lst =
        match lst with
        | [] ->  ""
        | c1::c2::c3::tl when n = 0 -> Cell.toString c1 ^ " " ^ Cell.toString c2 ^ " " ^ Cell.toString c3
        | c1::c2::c3::tl when n > 0 -> toStringLineAux (n - 1) tl
        | _::tl -> ""
    in match state with
        | Cell.None              -> toStringLineAux n lst
        | Cell.O when n = 0      -> "/ - \\"
        | Cell.O when n = 1      -> "|   |"
        | Cell.O when n = 2      -> "\\ - /"
        | Cell.O                 -> ""
        | Cell.X when n = 0      -> "\\   /"
        | Cell.X when n = 1      -> "  x  "
        | Cell.X when n = 2      -> "/   \\"
        | Cell.X                 -> ""

(*private lst -> bool true if grid is full else false *)
let rec isFull = function
    | []                         -> true
    | c::tl when c = Cell.None   -> false
    | c::tl                      -> isFull tl

(*private lst -> bool true if grid is won over one line else false*)
let rec isWonLine = function
    | []             -> false
    | c1::c2::c3::tl when c1 <> Cell.None && c1 = c2 && c2 = c3 -> true
    | c1::c2::c3::tl -> isWonLine tl
    | _::tl          -> false

(*private lst -> bool true if grid is won over one column else false*)
let isWonColumn lst =
    let isWonColumnAux x y z lst =
        if List.nth lst x <> Cell.None && List.nth_opt lst x = List.nth_opt lst y && List.nth_opt lst x = List.nth_opt lst z then true else false
    in
    if isWonColumnAux 0 3 6 lst then true
    else if isWonColumnAux 1 4 7 lst then true
    else if isWonColumnAux 2 5 8 lst then true
    else false

(*private lst -> bool true if grid is won over one crossed line else false*)
let isWonCross lst =
 let isWonCrossAux x y z lst =
        if List.nth lst x <> Cell.None && List.nth_opt lst x = List.nth_opt lst y && List.nth_opt lst x = List.nth_opt lst z then true else false
    in
    if isWonCrossAux 0 4 8 lst then true
    else if isWonCrossAux 2 4 6 lst then true
    else false
    
(*change the state of a pair (state x grid) if a grid is won by currentPlayer*)
let isWon gridN currentPlayer (state, lst) =
    let isWonAux lst =
        if isWonLine lst || isWonColumn lst || isWonCross lst then true else false
    in
    match state with
    | Cell.O | Cell.X -> (state, lst)
    | Cell.None       -> (fun p s l -> if isFull l || isWonAux l then
                                          begin print_endline (Cell.toString p ^ " wins grid " ^ (string_of_int (gridN + 1)) ^ "!") ; (p, l) end
                                       else (s, l)) currentPlayer state lst 

(*replace in list of cell index by currentPlayer and update state of grid*)
let updateCell gridN index currentPlayer grid =
    let updateCellAux (state, lst)  = state, List.mapi (fun i x -> if i = index then currentPlayer else x) lst in
    isWon gridN currentPlayer (updateCellAux grid)

let isCellEmpty index (state, lst) =
    if (List.nth lst index) <> Cell.None then false else true
