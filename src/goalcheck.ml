(* [size b] of an n x n board is n *)
let size b = Array.length b.(0)

type t = string array array

(* [rows b] is a list containing the n rows of board b *)
let rows b =
  let acc = ref [] in
  for i = size b - 1 downto 0 do
    acc := Array.to_list b.(i) :: !acc
  done;
  !acc

let column b n =
  let acc = ref [] in
  for i = 0 to size b - 1 do
    acc := !acc @ [ b.(i).(n - 1) ]
  done;
  !acc

(* [columns b] is a list containing the n columns of board b *)
let columns b =
  let acc = ref [] in
  for i = size b downto 1 do
    acc := column b i :: !acc
  done;
  !acc

let l_diagonal b =
  let acc = ref [] in
  for i = 0 to size b - 1 do
    acc := !acc @ [ b.(i).(i) ]
  done;
  !acc

let r_diagonal b =
  let counter = ref 0 in
  let acc = ref [] in
  for i = size b - 1 downto 0 do
    acc := !acc @ [ b.(!counter).(i) ];
    counter := !counter + 1
  done;
  !acc

(* [diagonals b] is a list containing the 2 diagonals of board b *)
let diagonals b = [ l_diagonal b; r_diagonal b ]

let rec nlist x = function
  | 0 -> []
  | n -> x :: nlist x (n - 1)

(* [set_terminal b x] returns true if there are n in a row i's on the board *)
let set_terminal b x =
  let win = nlist x (size b) in
  let lines = rows b @ columns b @ diagonals b in
  List.exists (fun x -> List.equal ( = ) win x) lines

(* [full b] returns true if there are no empty spots on the board *)
let full b =
  let result = ref true in
  for i = 0 to Array.length b - 1 do
    for j = 0 to Array.length b - 1 do
      match b.(i).(j) with
      | "X" | "O" -> ()
      | _ -> result := false
    done
  done;
  !result

(* [tie b] returns true if there is a tie on the board *)
let tie b =
  let no_win = set_terminal b "X" = false && set_terminal b "O" = false in
  no_win && full b
