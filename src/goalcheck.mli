(** Ending the game. *)

type t = string array array
(** [t] is the type of the board. *)

val size : t -> int
(** [size b] of an n x n board b is n. *)

val rows : t -> string list list
(** [rows b] is a list containing the n rows of board b *)

val columns : t -> string list list
(** [columns b] is a list containing the n columns of board b *)

val diagonals : t -> string list list
(** [diagonals b] is a list containing the 2 diagonals of board b *)

val set_terminal : t -> string -> bool
(** [set_terminal b i] returns true if there are n in a row i's on the n x n
    board b *)

val full : string array array -> bool
(** [full b] returns true if there are no empty spots on the board *)

val tie : string array array -> bool
(** [tie b] returns true if there is a tie on the board *)
