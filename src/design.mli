(** Printing the game board. *)

val get_value : int * int -> string array array -> string
(** [get_value (r,c) arr] extracts a value of arr at position [(r,c)]*)

val print_board1 :
  string array array -> string -> string ref -> string ref -> unit
(** [print_board1 arr] prints board of 3 x 3 dimension *)

val print_board2 :
  string array array -> string -> string ref -> string ref -> unit
(** [print_board2 arr] prints board of 6 x 6 dimension*)

val print_board3 :
  string array array -> string -> string ref -> string ref -> unit
(** [print_board3 arr] prints board of 9 x 9 dimension*)
