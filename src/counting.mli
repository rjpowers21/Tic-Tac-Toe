(** Counting 'X's and 'O's on the board. *)

(**********************************************************************)

val count : string list -> string -> int
(** [count lst elm] counts the number of elements elm in list lst. *)

val count_x : string array array -> int -> int
(** [count_x arr r] counts the number of 'X's in row r of 2D array arr. Requires
    that arr is not empty. *)

val count_o : string array array -> int -> int
(** [count_o arr r] counts the number of 'O's in row r of 2D array arr. Requires
    that arr is not empty. *)

val count_total_x : string array array -> int
(** [count_total_x arr] counts the number of 'X's in 2D array arr. Requires that
    arr is not empty. *)

val count_total_o : string array array -> int
(** [count_total_o arr] counts the number of 'O's in 2D array arr. Requires that
    arr is not empty.*)
