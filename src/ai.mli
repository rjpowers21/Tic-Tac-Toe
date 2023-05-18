(** Single-player AI functionality. *)

(**********************************************************************)

val available : string array array -> int -> bool
(** [available arr inp] takes in a 2D array arr and input inp from the user and
    returns true if the inp is an available/untaken spot on the baord, and false
    if it has already been taken. *)

val rand_index : string array array -> int
(** [rand_index arr] generates two random numbers of indices of a
    multidimensional array Precondition: curr_array is a square grid with
    lengths greater than 0*)

val total_elements : string array array -> int
(**[total_elements arr] returns total number of elements in 2D array*)

val array_to_list_1D : string array -> string list
(** [array_to_list1D] takes in a 1D array arr and returns a list with all of the
    elements in arr. *)

val array_to_list_2D : string array array -> string list
(** [array_to_list_2D arr] takes in a 2D array arr and returns a list with all
    of the elements in arr. *)

val list_to_dict : string list -> int -> (int * string) list
(** [list_to_dict] takes in a list and index (starting at 0) and returns a new
    list with (k, v) pairings, where k is the index and v is the value at that
    index in the original list lst. Example: list_to_dict [""; "X"; "O"] returns
    [(0, "");
(1, "X"); (2, "O")] *)

val rem_spots_helper : (int * string) list -> int list
(** [rem_spots_helper lst] takes in a list of (k, v) pairings, where k is an
    index and v is the string, and returns a list of indexes corresponding to
    the remaining available spots on the tic tac toe board. *)

val rem_spots : string array array -> int list
(** [rem_spots arr] takes in a 2D array arr, and returns a list of indexes
    corresponding to the remaining available spots on the tic tac toe board.*)

val best_move : string array array -> int
(** [best_move b] returns the position on board b that will lead to O's win and
    X's loss*)
