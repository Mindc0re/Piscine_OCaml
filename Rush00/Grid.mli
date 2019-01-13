module Cell :
sig

    type c = O | X | None

    val toString : c -> string
    val toCell   : string -> c
end

type g = (Cell.c * Cell.c list)

val createGrid : unit -> g
val toStringLine : int -> g -> string

(*current player -> grid -> grid with changed state*)
val isWon : int -> Cell.c -> g -> g

val updateCell : int -> int -> Cell.c -> g -> g

val isCellEmpty : int -> g -> bool
