type board = Grid.g list

val printBoard : board -> unit

val isWon : board -> bool

val updateBoard : int -> int -> Grid.Cell.c -> board -> board

val isMoveValid : int -> int -> board -> bool

val createBoard : unit -> board
