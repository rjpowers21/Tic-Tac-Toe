(** Menu of the game. *)

val play : string array array -> string ref -> string ref -> unit
(** [play] updates array if [get_inputi] returns valid position else it raises
    Invalid_input. A position is valid if it's in bounds of current board and no
    user has claimed it yet *)

val make_array : int -> int -> string array array
(** [make_array n n] creates 2D array of n x n dimensions *)

val action : unit -> unit
(** [action] displays appropriate menu based on user's previous choices;
    displays board and utilizes commands that correspond to users' choice *)

val fill_array : string array array -> string array array
(** [fill_array arr] initializes array to values that should be displayed on the
    board to identify all position on the board .*)

val update_array : string -> string array array -> unit
(** [update_array input a] updatess value of 2D array based on user's input. For
    example: if user inputs 2 which is a position on the board then value at
    index 2 of a would change to that users symbol that would either be 'X' or
    'O'*)

val print_board :
  string array array -> string -> string ref -> string ref -> unit
(** [print_board arr] prints board of 3 x 3 dimension*)

val user_1 : string ref
val user_2 : string ref

val commands : string array array -> unit
(** [commands curr_arr] is set of functions call for default setting of the game*)

val opt : unit -> string
(* [opt] is listing options that user has at start of game, 1. play original
   default configuration of game 2. change the configuration and play custom
   game*)

val is_custom : string -> bool
(** [is_custom opt] returns true if user's input indicates they want to
    customize game else returns false*)

val update_print : string array array -> string ref -> string ref -> unit
(** [update_print] displays updated board and updates user*)

val select : string -> string list -> string
(** [select prompt lst] returns the choice in list corresponding to index that
    user inputs. It gives user a menu consisting of choices in the lst and their
    indices and displays [prompt]. It ensures user is inputting a valid index.*)

val user : int ref
(** [user] is a counter variable that keeps track of user's turn *)

val print_array : string array array -> unit
(** [print_array curr_array] prints index followed by curr_array's value, used
    for debugging *)
