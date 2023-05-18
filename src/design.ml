open Goalcheck

let get_value (r, c) arr = arr.(r).(c)

let get_val (i, j) arr =
  let ival = get_value (i, j) arr in
  if String.length ival < 2 then ival ^ " " else ival

let counter = ref 2
let username = ref ""
let count = ref 0

(* [print_board1 arr] prints board of 3 x 3 dimension*)
let print_board1 arr s user1 user2 =
  incr counter;
  let _ =
    if !counter mod 2 = 0 then (
      username := !user2;
      count := Counting.count_total_o arr + 1)
    else (
      username := !user1;
      count := Counting.count_total_x arr + 1)
  in
  Format.(
    set_margin 10;
    set_max_indent 5;

    (* update it become a loop or something; maybe run a counter and run a
       separate print func let i = 0 in while i < 9 do printf "@[ | | @[ %d | %d
       | %d @]@. " (get arr i) (get arr i+1) (get arr i+2); i = i+3 done; *)
    if s = "Initial" then printf "@[\n<<<<<<<<<<<<<<< >>>>>>>>>>>>>>>> @]@.";
    printf "@[||| <<< Displaying %s Board:  @]@." s;

    printf "@[      |     |   @[  %s   |  %s  | %s@]@[ _____|_____|_____@] @."
      (get_value (0, 0) arr)
      (get_value (0, 1) arr)
      (get_value (0, 2) arr);

    printf "@[      |     |   @[  %s   |  %s  | %s@]@[ _____|_____|_____@] @."
      (get_value (1, 0) arr)
      (get_value (1, 1) arr)
      (get_value (1, 2) arr);
    printf "@[      |     |   @[  %s   |  %s  | %s@]@[      |     |     @] @."
      (get_value (2, 0) arr)
      (get_value (2, 1) arr)
      (get_value (2, 2) arr);
    if set_terminal arr "X" || set_terminal arr "O" || tie arr then ()
    else printf "@[||| <<< Turn %i for %s  @]@." !count !username)

(* [print_board2 arr] prints board of 6 x 6 dimension*)
let print_board2 arr s user1 user2 =
  incr counter;
  let _ =
    if !counter mod 2 = 0 then (
      username := !user2;
      count := Counting.count_total_o arr + 1)
    else (
      username := !user1;
      count := Counting.count_total_x arr + 1)
  in
  Format.(
    set_margin 10;
    set_max_indent 5;
    (* update it become a loop or something; maybe run a counter and run a
       separate print func let i = 0 in while i < 9 do printf "@[ | | @[ %d | %d
       | %d @]@. " (get arr i) (get arr i+1) (get arr i+2); i = i+3 done; *)
    if s = "Initial" then printf "@[\n<<<<<<<<<<<<<<< >>>>>>>>>>>>>>>> @]@.";
    printf "@[||| <<< Displaying %s Board:  @]@." s;

    printf
      "@[       |       |       |       |       |      @[   %s   |   %s   |   \
       %s   |   %s   |   %s   |   %s   \
       @]@[_______|_______|_______|_______|_______|_______@] @."
      (get_value (0, 0) arr)
      (get_value (0, 1) arr)
      (get_value (0, 2) arr)
      (get_value (0, 3) arr)
      (get_value (0, 4) arr)
      (get_value (0, 5) arr);

    printf
      "@[       |       |       |       |       |      @[   %s   |   %s   |   \
       %s   |   %s   |   %s  |   %s   \
       @]@[_______|_______|_______|_______|_______|_______@] @."
      (get_value (1, 0) arr)
      (get_value (1, 1) arr)
      (get_value (1, 2) arr)
      (get_value (1, 3) arr)
      (get_val (1, 4) arr)
      (get_val (1, 5) arr);

    printf
      "@[       |       |       |       |       |      @[   %s  |   %s  |   \
       %s  |   %s  |   %s  |   %s  \
       @]@[_______|_______|_______|_______|_______|_______@] @."
      (get_val (2, 0) arr)
      (get_val (2, 1) arr)
      (get_val (2, 2) arr)
      (get_val (2, 3) arr)
      (get_val (2, 4) arr)
      (get_val (2, 5) arr);

    printf
      "@[       |       |       |       |       |      @[   %s  |   %s  |   \
       %s  |   %s  |   %s  |   %s  \
       @]@[_______|_______|_______|_______|_______|_______@] @."
      (get_val (3, 0) arr)
      (get_val (3, 1) arr)
      (get_val (3, 2) arr)
      (get_val (3, 3) arr)
      (get_val (3, 4) arr)
      (get_val (3, 5) arr);
    printf
      "@[       |       |       |       |       |      @[   %s  |   %s  |   \
       %s  |   %s  |   %s  |   %s  \
       @]@[_______|_______|_______|_______|_______|_______@] @."
      (get_val (4, 0) arr)
      (get_val (4, 1) arr)
      (get_val (4, 2) arr)
      (get_val (4, 3) arr)
      (get_val (4, 4) arr)
      (get_val (4, 5) arr);

    printf
      "@[       |       |       |       |       |      @[   %s  |   %s  |   \
       %s  |   %s  |   %s  |   %s  @]@[       |       |       |       |       \
       |       @] @."
      (get_val (5, 0) arr)
      (get_val (5, 1) arr)
      (get_val (5, 2) arr)
      (get_val (5, 3) arr)
      (get_val (5, 4) arr)
      (get_val (5, 5) arr);
    if set_terminal arr "X" || set_terminal arr "O" || tie arr then ()
    else printf "@[||| <<< Turn %i for %s  @]@." !count !username)

(* [print_board3 arr] prints board of 9 x 9 dimension*)
let print_board3 arr s user1 user2 =
  incr counter;
  let _ =
    if !counter mod 2 = 0 then (
      username := !user2;
      count := Counting.count_total_o arr + 1)
    else (
      username := !user1;
      count := Counting.count_total_x arr + 1)
  in
  Format.(
    set_margin 10;
    set_max_indent 5;
    (* update it become a loop or something; maybe run a counter and run a
       separate print func let i = 0 in while i < 9 do printf "@[ | | @[ %d | %d
       | %d @]@. " (get arr i) (get arr i+1) (get arr i+2); i = i+3 done; *)
    if s = "Initial" then printf "@[\n<<<<<<<<<<<<<<< >>>>>>>>>>>>>>>> @]@.";
    printf "@[||| <<< Displaying %s Board:  @]@." s;
    printf
      "@[       |       |       |       |       |       |       |       \
       |       @[   %s   |   %s   |   %s   |   %s   |   %s   |   %s   |   %s   \
       |   %s   |   %s   \
       @]@[_______|_______|_______|_______|_______|_______|_______|_______|_______@] \
       @."
      (get_value (0, 0) arr)
      (get_value (0, 1) arr)
      (get_value (0, 2) arr)
      (get_value (0, 3) arr)
      (get_value (0, 4) arr)
      (get_value (0, 5) arr)
      (get_value (0, 6) arr)
      (get_value (0, 7) arr)
      (get_value (0, 8) arr);

    printf
      "@[       |       |       |       |       |       |       |       \
       |       @[   %s   |   %s  |   %s  |   %s  |   %s  |   %s  |   %s  |   \
       %s  |   %s  \
       @]@[_______|_______|_______|_______|_______|_______|_______|_______|_______@] \
       @."
      (get_value (1, 0) arr)
      (get_val (1, 1) arr)
      (get_val (1, 2) arr)
      (get_val (1, 3) arr)
      (get_val (1, 4) arr)
      (get_val (1, 5) arr)
      (get_val (1, 6) arr)
      (get_val (1, 7) arr)
      (get_val (1, 8) arr);

    printf
      "@[       |       |       |       |       |       |       |       \
       |       @[   %s  |   %s  |   %s  |   %s  |   %s  |   %s  |   %s  |   \
       %s  |   %s  \
       @]@[_______|_______|_______|_______|_______|_______|_______|_______|_______@] \
       @."
      (get_val (2, 0) arr)
      (get_val (2, 1) arr)
      (get_val (2, 2) arr)
      (get_val (2, 3) arr)
      (get_val (2, 4) arr)
      (get_val (2, 5) arr)
      (get_val (2, 6) arr)
      (get_val (2, 7) arr)
      (get_val (2, 8) arr);
    printf
      "@[       |       |       |       |       |       |       |       \
       |       @[   %s  |   %s  |   %s  |   %s  |   %s  |   %s  |   %s  |   \
       %s  |   %s  \
       @]@[_______|_______|_______|_______|_______|_______|_______|_______|_______@] \
       @."
      (get_val (3, 0) arr)
      (get_val (3, 1) arr)
      (get_val (3, 2) arr)
      (get_val (3, 3) arr)
      (get_val (3, 4) arr)
      (get_val (3, 5) arr)
      (get_val (3, 6) arr)
      (get_val (3, 7) arr)
      (get_val (3, 8) arr);
    printf
      "@[       |       |       |       |       |       |       |       \
       |       @[   %s  |   %s  |   %s  |   %s  |   %s  |   %s  |   %s  |   \
       %s  |   %s  \
       @]@[_______|_______|_______|_______|_______|_______|_______|_______|_______@] \
       @."
      (get_val (4, 0) arr)
      (get_val (4, 1) arr)
      (get_val (4, 2) arr)
      (get_val (4, 3) arr)
      (get_val (4, 4) arr)
      (get_val (4, 5) arr)
      (get_val (4, 6) arr)
      (get_val (4, 7) arr)
      (get_val (4, 8) arr);

    printf
      "@[       |       |       |       |       |       |       |       \
       |       @[   %s  |   %s  |   %s  |   %s  |   %s  |   %s  |   %s  |   \
       %s  |   %s  \
       @]@[_______|_______|_______|_______|_______|_______|_______|_______|_______@] \
       @."
      (get_val (5, 0) arr)
      (get_val (5, 1) arr)
      (get_val (5, 2) arr)
      (get_val (5, 3) arr)
      (get_val (5, 4) arr)
      (get_val (5, 5) arr)
      (get_val (5, 6) arr)
      (get_val (5, 7) arr)
      (get_val (5, 8) arr);

    printf
      "@[       |       |       |       |       |       |       |       \
       |       @[   %s  |   %s  |   %s  |   %s  |   %s  |   %s  |   %s  |   \
       %s  |   %s  \
       @]@[_______|_______|_______|_______|_______|_______|_______|_______|_______@] \
       @."
      (get_val (6, 0) arr)
      (get_val (6, 1) arr)
      (get_val (6, 2) arr)
      (get_val (6, 3) arr)
      (get_val (6, 4) arr)
      (get_val (6, 5) arr)
      (get_val (6, 6) arr)
      (get_val (6, 7) arr)
      (get_val (6, 8) arr);

    printf
      "@[       |       |       |       |       |       |       |       \
       |       @[   %s  |   %s  |   %s  |   %s  |   %s  |   %s  |   %s  |   \
       %s  |   %s  \
       @]@[_______|_______|_______|_______|_______|_______|_______|_______|_______@] \
       @."
      (get_val (7, 0) arr)
      (get_val (7, 1) arr)
      (get_val (7, 2) arr)
      (get_val (7, 3) arr)
      (get_val (7, 4) arr)
      (get_val (7, 5) arr)
      (get_val (7, 6) arr)
      (get_val (7, 7) arr)
      (get_val (7, 8) arr);

    printf
      "@[       |       |       |       |       |       |       |       \
       |       @[   %s  |   %s  |   %s  |   %s  |   %s  |   %s  |   %s  |   \
       %s  |   %s  @]@[       |       |       |       |       |       |       \
       |       |       @] @."
      (get_val (8, 0) arr)
      (get_val (8, 1) arr)
      (get_val (8, 2) arr)
      (get_val (8, 3) arr)
      (get_val (8, 4) arr)
      (get_val (8, 5) arr)
      (get_val (8, 6) arr)
      (get_val (8, 7) arr)
      (get_val (8, 8) arr);
    if set_terminal arr "X" || set_terminal arr "O" || tie arr then ()
    else printf "@[||| <<< Turn %i for %s  @]@." !count !username)
