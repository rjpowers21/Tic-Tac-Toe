(*Choose Multiplayer, Single Player, Ask name and randomized name, Choose the
  board, Choose difficulty level ---- AI good, AI is Dumb; *)

open Ai
open Design
open Goalcheck
open Counting

exception Invalid_input

(* [user] is a counter variable that keeps track of user's turn *)
let user = ref 2
let prompt4 = "|||> Enter name of Player 1: "
let prompt5 = "|||> Enter name of Player 2: "
let prompt1 = "||| <<< Input a number to continue : "

(** [make_array n n] creates 2D array of n x n dimensions *)
let make_array n n = Array.make_matrix n n ""

(* [print_array curr_array] prints index followed by curr_array's value, used
   for debugging *)
let print_array curr_arr =
  for i = 0 to Array.length curr_arr - 1 do
    for j = 0 to Array.length curr_arr - 1 do
      print_endline
        ("(" ^ string_of_int i ^ "," ^ string_of_int j ^ "): "
        ^ curr_arr.(i).(j))
    done
  done

(*[fill_array arr] initializes array to values that should be displayed on the
  board to identify all position on the board .*)
let fill_array arr =
  let count = ref 0 in
  for i = 0 to Array.length arr - 1 do
    for j = 0 to Array.length arr - 1 do
      arr.(i).(j) <- string_of_int !count;
      incr count
    done
  done;
  arr

(*[curr_arr] keeps track of current array corresponding to current board*)
(* let curr_arr = make_array 3 3 *)

(*[get_inputi prompt] reads any integer user inputs*)
let get_inputi prompt =
  let () = print_string prompt in
  read_int ()

(* [get_input prompt] returns a name inputed by user when prompted*)
let get_input prompt =
  let () = print_string prompt in
  read_line ()

(*[update_array input a] updatess value of 2D array based on user's input. For
  example: if user inputs 2 which is a position on the board then value at index
  2 of a would change to that users symbol that would either be 'X' or 'O'*)
let update_array input a : unit =
  for i = 0 to Array.length a - 1 do
    for j = 0 to Array.length a - 1 do
      match a.(i).(j) = input with
      | true -> if !user mod 2 = 0 then a.(i).(j) <- "X" else a.(i).(j) <- "O"
      | false -> ()
    done
  done

(* [print_board arr] prints board of 3 x 3 dimension*)
let print_board arr s user_1 user_2 =
  if size arr = 3 then print_board1 arr s user_1 user_2
  else if size arr = 6 then print_board2 arr s user_1 user_2
  else if size arr = 9 then print_board3 arr s user_1 user_2
  else failwith "Impossible"

(* [make_array_helper board_type] makes array based on user's choice*)
let make_array_helper board_type =
  if board_type = "3x3 " then make_array 3 3
  else if board_type = "6x6 " then make_array 6 6
  else if board_type = "9x9 " then make_array 9 9
  else failwith "Impossible"

(*[update_print ()] displays updated board and updates user*)
let update_print curr_arr user_1 user_2 =
  print_board curr_arr "New" user_1 user_2;
  user := !user + 1

(*[play ()] updates array if [get_inputi] returns valid position else it raises
  Invalid_input. A position is valid if it's in bounds of current board and no
  user has claimed it yet *)
let play curr_arr user_1 user_2 =
  let input = get_inputi prompt1 in
  if input < total_elements curr_arr && available curr_arr input then (
    let inp = string_of_int input in
    let user = ref (if !user mod 2 = 0 then !user_1 else !user_2) in
    let len = String.length !user in
    if len < 6 then
      for i = len to 6 do
        user := !user ^ " "
      done
    else if len > 6 then
      for i = len downto 6 do
        user := !user ^ " "
      done
    else ();
    let _ =
      Format.printf
        "@[---------------------------------------- \n\
         |     %s makes the move           |\n\
         ----------------------------------------@]@."
        !user
    in
    let () = update_array inp curr_arr in
    update_print curr_arr user_1 user_2)
  else failwith "Inavalid input"

(* let pos_2D p arr= let s = size arr in (2->) *)

let welcome () =
  Format.printf "@[  \n \n@[|||>WELCOME TO TIC TAC TOE! <||| @]@[@] @. "

let user_1 = ref ""
let user_2 = ref ""

(*[current_user ()] returns current users name, helpful in debugging, should
  return opposite*)
let current_user () = if !user mod 2 = 0 then !user_2 else !user_1

(*[commands curr_arr] is set of functions call for default setting of the game*)
let commands curr_arr =
  print_board (curr_arr |> fill_array) "Initial" user_1 user_2;
  while
    not (set_terminal curr_arr "X" || set_terminal curr_arr "O" || tie curr_arr)
  do
    (* print_array curr_arr; print_endline (string_of_bool (set_terminal
       curr_arr "X" && set_terminal curr_arr "O")); *)
    play curr_arr user_1 user_2
  done

(*[win_msg ()] is called once the terminal state has been reached to print the
  win message*)
let win_msg curr_arr =
  if tie curr_arr then Format.printf "It IS A TIE !!"
  else Format.printf "Congratulations, %s Won!!" (current_user ())
(* let () = Unix.sleepf 0.6 *)

(*[select prompt lst] returns the choice in list corresponding to index that
  user inputs. It gives user a menu consisting of choices in the lst and their
  indices and displays [prompt]. It ensures user is inputting a valid index.*)
let select prompt lst =
  match lst with
  | [] -> ""
  | choices ->
      let rec menu () =
        Format.printf "@[\n@[|||> %s <||| @]@[@] \n @. " prompt;
        List.iteri (Printf.printf "    %d: %s") choices;

        try List.nth choices (read_int ()) with _ -> menu ()
      in
      menu ()

(* [boptE] are possible board options if user choses Easy mode to play*)
let boptE = [ "3x3 " ]

(* [boptH] are possible board options if user choses Hard mode to play*)
let boptH = [ "3x3 "; "6x6 "; "9x9 " ]

(*[default] defines the default configuration of the game, its here for
  documentation purposes mostly*)
let default = ("Play with a friend", "Go easy", "3x3 ")

(* [opt] is listing options that user has at start of game, 1. play original
   default configuration of game 2. change the configuration and play custom
   game*)
let opt () = select "Select Game Type" [ "Default Game"; "Custom Game " ]

let opt1 () =
  select "Select Game Type" [ "Play with a friend"; "Play with AI " ]

(* [is_custom opt] returns true if user's input indicates they want to customize
   game else returns false*)
let is_custom opt =
  match opt with
  | "Default Game" -> false
  | "Custom Game " -> true
  | _ -> failwith "Error"

(* [board bopt] asks user their choice of board from the options listed in
   bopt*)
let board bopt = select "Select a board" bopt

(* [mode ())] asks user the mode of game they'd like to play*)
let mode () = select "Choose game mode" [ "Go easy"; "Go hard " ]

(*[ai_play ()] updates array if [get_inputi] returns valid position else it
  raises Invalid_input. A position is valid if it's in bounds of current board
  and no user has claimed it yet *)
let ai_play curr_arr =
  let input = string_of_int (rand_index curr_arr) in
  let () = update_array input curr_arr in
  update_print curr_arr

(*[ai_play ()] updates array if [get_inputi] returns valid position else it
  raises Invalid_input. A position is valid if it's in bounds of current board
  and no user has claimed it yet *)
let ai_play_smart curr_arr =
  (* print_endline "Curr_array being sent into the function"; *)
  (* print_array curr_arr; *)
  let input = string_of_int (best_move curr_arr) in
  update_array input curr_arr;
  (* print_string "Array after updating based on value generated by Smart AI";
     print_array curr_arr; *)
  update_print curr_arr

(*[ai_commands curr_arr] is set of functions call for default setting of the
  game*)
let ai_commands curr_arr param user_1 user_2 =
  print_board (curr_arr |> fill_array) "Initial" user_1 user_2;
  while
    not (set_terminal curr_arr "X" || set_terminal curr_arr "O" || tie curr_arr)
  do
    (* print_array curr_arr; print_endline (string_of_bool (set_terminal
       curr_arr "X" && set_terminal curr_arr "O")); *)
    let a = if !user mod 2 = 0 then "" else "AI" in
    match a with
    | "AI" ->
        let () = print_endline "----------------------------" in
        let () = print_endline "|     AI makes the move    |" in
        let () = print_endline "----------------------------" in
        if param = "Smart" then ai_play_smart curr_arr user_1 user_2
        else if param = "Dumb" then ai_play curr_arr user_1 user_2
        else failwith "Impossible"
    | "" -> play curr_arr user_1 user_2
    | _ -> failwith "Impossible"
  done

let action () =
  welcome ();
  user_1 := get_input prompt4;
  if is_custom (opt ()) then
    let opt1 = opt1 () in
    match opt1 with
    | "Play with a friend" -> (
        let m = mode () in
        match m with
        | "Go easy" ->
            let _ = board boptE in
            let curr_arr = make_array 3 3 in
            let () = user_2 := get_input prompt5 in
            let () = commands curr_arr in
            win_msg curr_arr
        | "Go hard " ->
            let b = board boptH in
            let curr_arr =
              if String.get b 0 = '3' then make_array 3 3
              else if String.get b 0 = '6' then make_array 6 6
              else if String.get b 0 = '9' then make_array 9 9
              else failwith "Invalid"
            in
            let () = user_2 := get_input prompt5 in
            let () = commands curr_arr in
            win_msg curr_arr
        | _ -> failwith "Impossible")
    | "Play with AI " -> (
        let m = mode () in
        match m with
        | "Go easy" ->
            let _ = board boptE in
            let curr_arr = make_array 3 3 in
            let () = user_2 := "AI" in
            let () = ai_commands curr_arr "Dumb" user_1 user_2 in
            win_msg curr_arr
        | "Go hard " ->
            let board_type = board boptE in
            let curr_array = make_array_helper board_type in
            let () = user_2 := "AI" in
            let () = ai_commands curr_array "Smart" user_1 user_2 in
            win_msg curr_array
        | _ -> failwith "Impossible")
    | _ -> failwith "Impossible"
  else
    let curr_arr = make_array 3 3 in
    let () = print_endline "" in
    let () = user_2 := get_input prompt5 in
    let () = commands curr_arr in
    win_msg curr_arr

(*All work is being done by following call*)
(* let () = action () *)
