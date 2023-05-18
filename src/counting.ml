open Ai
open Goalcheck

let rec count lst elm =
  match lst with
  | [] -> 0
  | h :: t -> if h = elm then 1 + count t elm else count t elm

let count_x arr r : int =
  let subarray = arr.(r) in
  let cnt = count (array_to_list_1D subarray) "X" in
  cnt

let count_o arr r : int =
  let subarray = arr.(r) in
  let cnt = count (array_to_list_1D subarray) "O" in
  cnt

let count_total_x arr =
  let cur = ref 0 in
  for i = 0 to size arr - 1 do
    cur := count_x arr i + !cur
  done;
  !cur

let count_total_o arr =
  let cur = ref 0 in
  for i = 0 to size arr - 1 do
    cur := count_o arr i + !cur
  done;
  !cur
