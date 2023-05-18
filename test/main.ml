(** Test Plan: Our testing approach consists of both automated and manual
    testing methods to ensure the correctness and reliability of our program.

    Automated testing utilizing the OUnit test suite focuses on functions
    involving background calculations such as the Minimax algorithm, random AI,
    array updates, and terminal game statistics. All the modules are fully
    testing using automated tests except parts of Menu. These tests cover a wide
    range of scenarios, including edge cases, to achieve thorough coverage.

    Manual testing is conducted for parts of the program in Menu that require
    specific function calls based on user interaction/input. Additionally,
    functions responsible for displaying string representations of arrays on the
    board are tested manually.

    The OUnit tests employ a combination of black box and glass box testing
    techniques. Black box testing validates the overall functionality of the
    program without considering internal implementation details, while glass box
    testing examines the internal logic to ensure correct state changes and
    accurate detection.

    By passing these comprehensive test cases, our testing plan confirms the
    correctness of the system, including the proper handling of state changes
    and the effectiveness of our AI logic.

    Overall, our testing plan thoroughly evaluates every aspect of our system,
    providing confidence in its intended behavior and ensuring its reliability. *)

open Menulib.Menu
open Menulib.Ai
open Menulib.Design
open Menulib.Goalcheck
open Menulib.Counting
open OUnit2

(** [size_test name input expected_output] constructs an OUnit test named [name]
    that asserts the equality of [expected_output] with [size input]. *)
let size_test (name : string) (input : string array array)
    (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (size input)

(** [rows_test name input expected_output] constructs an OUnit test named [name]
    that asserts the equality of [expected_output] with [rows input]. *)
let rows_test (name : string) (input : string array array)
    (expected_output : string list list) : test =
  name >:: fun _ -> assert_equal expected_output (rows input)

(** [columns_test name input expected_output] constructs an OUnit test named
    [name] that asserts the equality of [expected_output] with [columns input]. *)
let columns_test (name : string) (input : string array array)
    (expected_output : string list list) : test =
  name >:: fun _ -> assert_equal expected_output (columns input)

(** [diagonals_test name input expected_output] constructs an OUnit test named
    [name] that asserts the equality of [expected_output] with
    [diagonals input]. *)
let diagonals_test (name : string) (input : string array array)
    (expected_output : string list list) : test =
  name >:: fun _ -> assert_equal expected_output (diagonals input)

(** [check_win_test name input expected_output] constructs an OUnit test named
    [name] that asserts the equality of [expected_output] with [check_win b x]. *)
let check_win_test (name : string) (b : string array array) (x : string)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (set_terminal b x)

(** [full_test name input expected_output] constructs an OUnit test named [name]
    that asserts the equality of [expected_output] with [full b]. *)
let full_test (name : string) (b : string array array) (expected_output : bool)
    : test =
  name >:: fun _ -> assert_equal expected_output (full b)

(** [tie_test name input expected_output] constructs an OUnit test named [name]
    that asserts the equality of [expected_output] with [tie b]. *)
let tie_test (name : string) (b : string array array) (expected_output : bool) :
    test =
  name >:: fun _ -> assert_equal expected_output (tie b)

(** [best_move_test name input expected_output] constructs an OUnit test named
    [name] that asserts the equality of [expected_output] with [best_move b]. *)
let best_move_test (name : string) (b : string array array)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (best_move b) ~printer:string_of_int

let array_to_list_1D_test (test_description : string) arr expected_output : test
    =
  test_description >:: fun _ ->
  assert_equal expected_output (array_to_list_1D arr)

let array_to_list_2D_test (test_description : string) arr expected_output : test
    =
  test_description >:: fun _ ->
  assert_equal expected_output (array_to_list_2D arr)

let list_to_dict_test (test_description : string) lst index expected_output :
    test =
  test_description >:: fun _ ->
  assert_equal expected_output (list_to_dict lst index)

let rem_spots_helper_test (test_description : string) d expected_output : test =
  test_description >:: fun _ ->
  assert_equal expected_output (rem_spots_helper d)

let rem_spots_test (test_description : string) arr expected_output : test =
  test_description >:: fun _ -> assert_equal expected_output (rem_spots arr)

let available_test (test_description : string) arr inp expected_output : test =
  test_description >:: fun _ -> assert_equal expected_output (available arr inp)

let count_test (test_description : string) lst elm expected_output : test =
  test_description >:: fun _ -> assert_equal expected_output (count lst elm)

let countx_test (test_description : string) arr r expected_output : test =
  test_description >:: fun _ -> assert_equal expected_output (count_x arr r)

let counto_test (test_description : string) arr r expected_output : test =
  test_description >:: fun _ -> assert_equal expected_output (count_o arr r)

let count_totalx_test (test_description : string) arr expected_output : test =
  test_description >:: fun _ -> assert_equal expected_output (count_total_x arr)

let count_totalo_test (test_description : string) arr expected_output : test =
  test_description >:: fun _ -> assert_equal expected_output (count_total_o arr)

let board1 = [| [| ""; ""; "" |]; [| ""; ""; "" |]; [| ""; ""; "" |] |]
let board2 = [| [| "X"; "O"; "O" |]; [| ""; "X"; "" |]; [| ""; ""; "X" |] |]

let board3 =
  [|
    [| "O"; ""; ""; ""; "" |];
    [| "O"; ""; "X"; ""; "" |];
    [| "O"; "X"; ""; ""; "" |];
    [| "O"; ""; ""; "X"; "" |];
    [| "O"; ""; ""; "X"; "" |];
  |]

let board4 =
  [|
    [| "O"; ""; ""; ""; ""; ""; ""; ""; "" |];
    [| ""; "O"; ""; ""; ""; ""; ""; ""; "" |];
    [| ""; ""; "O"; ""; ""; ""; ""; ""; "" |];
    [| ""; ""; ""; "O"; ""; ""; ""; ""; "" |];
    [| "X"; "X"; "X"; "X"; "X"; "X"; "X"; "X"; "X" |];
    [| ""; ""; ""; ""; ""; "O"; ""; ""; "" |];
    [| ""; ""; ""; ""; ""; ""; "O"; ""; "" |];
    [| ""; ""; ""; ""; ""; ""; ""; "O"; "" |];
    [| ""; ""; ""; ""; ""; ""; ""; ""; "O" |];
  |]

let board5 =
  [|
    [| "O"; "O"; "O"; "O"; "X" |];
    [| "X"; "O"; "X"; "O"; "O" |];
    [| "X"; "X"; "X"; "X"; "O" |];
    [| "O"; "X"; "O"; "X"; "O" |];
    [| "X"; "X"; "O"; "X"; "X" |];
  |]

let board6 = [| [| "O"; "X"; "O" |]; [| "O"; "X"; "X" |]; [| "X"; "O"; "X" |] |]

let size_tests =
  [
    size_test "size of Board 1 is 3" board1 3;
    size_test "size of Board 3 is 5" board3 5;
  ]

let rows_tests =
  [
    rows_test "Rows of empty 3x3 board" board1
      [ [ ""; ""; "" ]; [ ""; ""; "" ]; [ ""; ""; "" ] ];
    rows_test "Rows of board 2" board2
      [ [ "X"; "O"; "O" ]; [ ""; "X"; "" ]; [ ""; ""; "X" ] ];
    rows_test "Rows of board 3" board3
      [
        [ "O"; ""; ""; ""; "" ];
        [ "O"; ""; "X"; ""; "" ];
        [ "O"; "X"; ""; ""; "" ];
        [ "O"; ""; ""; "X"; "" ];
        [ "O"; ""; ""; "X"; "" ];
      ];
    rows_test "Rows of board 4" board4
      [
        [ "O"; ""; ""; ""; ""; ""; ""; ""; "" ];
        [ ""; "O"; ""; ""; ""; ""; ""; ""; "" ];
        [ ""; ""; "O"; ""; ""; ""; ""; ""; "" ];
        [ ""; ""; ""; "O"; ""; ""; ""; ""; "" ];
        [ "X"; "X"; "X"; "X"; "X"; "X"; "X"; "X"; "X" ];
        [ ""; ""; ""; ""; ""; "O"; ""; ""; "" ];
        [ ""; ""; ""; ""; ""; ""; "O"; ""; "" ];
        [ ""; ""; ""; ""; ""; ""; ""; "O"; "" ];
        [ ""; ""; ""; ""; ""; ""; ""; ""; "O" ];
      ];
  ]

let columns_tests =
  [
    columns_test "Columns of empty 3x3 board" board1
      [ [ ""; ""; "" ]; [ ""; ""; "" ]; [ ""; ""; "" ] ];
    columns_test "Columns of board 2" board2
      [ [ "X"; ""; "" ]; [ "O"; "X"; "" ]; [ "O"; ""; "X" ] ];
    columns_test "Columns of board 3" board3
      [
        [ "O"; "O"; "O"; "O"; "O" ];
        [ ""; ""; "X"; ""; "" ];
        [ ""; "X"; ""; ""; "" ];
        [ ""; ""; ""; "X"; "X" ];
        [ ""; ""; ""; ""; "" ];
      ];
    columns_test "Columns of board 4" board4
      [
        [ "O"; ""; ""; ""; "X"; ""; ""; ""; "" ];
        [ ""; "O"; ""; ""; "X"; ""; ""; ""; "" ];
        [ ""; ""; "O"; ""; "X"; ""; ""; ""; "" ];
        [ ""; ""; ""; "O"; "X"; ""; ""; ""; "" ];
        [ ""; ""; ""; ""; "X"; ""; ""; ""; "" ];
        [ ""; ""; ""; ""; "X"; "O"; ""; ""; "" ];
        [ ""; ""; ""; ""; "X"; ""; "O"; ""; "" ];
        [ ""; ""; ""; ""; "X"; ""; ""; "O"; "" ];
        [ ""; ""; ""; ""; "X"; ""; ""; ""; "O" ];
      ];
  ]

let diagonals_tests =
  [
    diagonals_test "Diagonals of empty 3x3 board" board1
      [ [ ""; ""; "" ]; [ ""; ""; "" ] ];
    diagonals_test "Columns of board 2" board2
      [ [ "X"; "X"; "X" ]; [ "O"; "X"; "" ] ];
    diagonals_test "Diagonals of board 3" board3
      [ [ "O"; ""; ""; "X"; "" ]; [ ""; ""; ""; ""; "O" ] ];
    diagonals_test "Diagonals of board 4" board4
      [
        [ "O"; "O"; "O"; "O"; "X"; "O"; "O"; "O"; "O" ];
        [ ""; ""; ""; ""; "X"; ""; ""; ""; "" ];
      ];
  ]

let check_win_tests =
  [
    check_win_test "X doesn't win on empty board" board1 "X" false;
    check_win_test "O doesn't win on empty board" board1 "O" false;
    check_win_test "X wins on Board 2" board2 "X" true;
    check_win_test "O doesn't win on Board 2" board2 "O" false;
    check_win_test "O wins on Board 3" board3 "O" true;
    check_win_test "X doesn't win on Board 3" board3 "X" false;
    check_win_test "X wins on Board 4" board4 "X" true;
    check_win_test "O doesn't win on Board 4" board4 "O" false;
  ]

let board7 = [| [| "X"; "1"; "2" |]; [| "X"; "O"; "5" |]; [| "6"; "7"; "8" |] |]
let board8 = [| [| "X"; "X"; "2" |]; [| "3"; "O"; "5" |]; [| "6"; "7"; "8" |] |]
let board9 = [| [| "X"; "1"; "2" |]; [| "O"; "O"; "5" |]; [| "X"; "7"; "X" |] |]

let full_tests =
  [
    full_test "board 5 is full" board5 true;
    full_test "board 6 is full" board6 true;
    full_test "board 7 is not full" board7 false;
    full_test "board 8 is not full" board8 false;
    full_test "board 7 is not full" board9 false;
  ]

let tie_tests =
  [
    tie_test "board 1 is not a tie" board1 false;
    tie_test "board 2 is not a tie" board2 false;
    tie_test "board 3 is not a tie" board3 false;
    tie_test "board 4 is not a tie" board4 false;
    tie_test "board 5 is a tie" board5 true;
    tie_test "board 6 is a tie" board6 true;
  ]

let best_move_tests =
  [
    best_move_test "AI should choose 6 on board 7" board7 6;
    best_move_test "AI should choose 2 on board 8" board8 2;
    best_move_test "AI should choose 5 on board 9" board9 5;
  ]

let ai_tests =
  [
    (******************** TESTING FOR ARRAY FUNCTIONS ********************)
    (* testing for array_to_list *)
    array_to_list_1D_test
      "empty 1D array with all empty strings, results in a \n\
      \  list of empty strings" [| ""; ""; "" |] [ ""; ""; "" ];
    array_to_list_1D_test
      "non-empty 1D array case 1, results in the following \n  list strings"
      [| "O"; ""; "" |] [ "O"; ""; "" ];
    array_to_list_1D_test
      "non-empty 1D array case 2, results in the following \n  list strings"
      [| ""; "O"; "" |] [ ""; "O"; "" ];
    array_to_list_1D_test
      "non-empty 1D array case 3, results in the following \n  list strings"
      [| ""; ""; "O" |] [ ""; ""; "O" ];
    array_to_list_1D_test
      "non-empty 1D array case 4, results in the following \n  list strings"
      [| ""; "O"; "O" |] [ ""; "O"; "O" ];
    array_to_list_1D_test
      "non-empty 1D array case 5, results in the following \n  list strings"
      [| "O"; "O"; "" |] [ "O"; "O"; "" ];
    array_to_list_1D_test
      "non-empty 1D array case 6, results in the following \n  list strings"
      [| "O"; ""; "O" |] [ "O"; ""; "O" ];
    array_to_list_1D_test
      "non-empty 1D array case 7, results in the following \n  list strings"
      [| "O"; "O"; "O" |] [ "O"; "O"; "O" ];
    array_to_list_2D_test
      "Given 3x3 array with each inner array having 3 empty \n\
      \  string, results in a list of 9 empty strings"
      [| [| ""; ""; "" |]; [| ""; ""; "" |]; [| ""; ""; "" |] |]
      [ ""; ""; ""; ""; ""; ""; ""; ""; "" ];
    array_to_list_2D_test
      "Given a 3x3 array with first inner array having at \n\
      \  least 1 non-empty string, results in at least 6 non-empty strings"
      [| [| ""; ""; "X" |]; [| ""; ""; "" |]; [| ""; ""; "" |] |]
      [ ""; ""; "X"; ""; ""; ""; ""; ""; "" ];
    array_to_list_2D_test
      "Given a 3x3 array with second inner array having at \n\
      \  least 1 non-empty string, results in at least 6 non-empty strings"
      [| [| ""; ""; "" |]; [| ""; ""; "X" |]; [| ""; ""; "" |] |]
      [ ""; ""; ""; ""; ""; "X"; ""; ""; "" ];
    array_to_list_2D_test
      "Given a 3x3 array with third inner array having at \n\
      \  least 1 non-empty string, results in at least 6 non-empty strings"
      [| [| ""; ""; "" |]; [| ""; ""; "" |]; [| ""; ""; "X" |] |]
      [ ""; ""; ""; ""; ""; ""; ""; ""; "X" ];
    array_to_list_2D_test
      "Given a 3x3 array with first and second inner array \n\
      \  having at least 1 non-empty string, results in at least 3 non-empty \
       strings"
      [| [| ""; ""; "X" |]; [| "X"; ""; "" |]; [| ""; ""; "" |] |]
      [ ""; ""; "X"; "X"; ""; ""; ""; ""; "" ];
    array_to_list_2D_test
      "Given a 3x3 array with second and third inner array \n\
      \  having at least 1 non-empty string, results in at least 3 non-empty \
       strings"
      [| [| ""; ""; "" |]; [| ""; ""; "X" |]; [| "X"; ""; "" |] |]
      [ ""; ""; ""; ""; ""; "X"; "X"; ""; "" ];
    array_to_list_2D_test
      "Given a 3x3 array with first and third inner array \n\
      \  having at least 1 non-empty string, results in at least 3 non-empty \
       strings"
      [| [| ""; ""; "X" |]; [| ""; ""; "" |]; [| "X"; ""; "" |] |]
      [ ""; ""; "X"; ""; ""; ""; "X"; ""; "" ];
    (* testing for list_to_dict *)
    list_to_dict_test
      "An input of an empty list results in an empty list with no \n\
      \  pairing, thus an empty dictionary" [] 0 [];
    list_to_dict_test
      "An input of list [''; 'O'; 'X'] results in [(0, ''); (1, \n\
      \  'O'); (2, 'X')]" [ ""; "O"; "X" ] 0
      [ (0, ""); (1, "O"); (2, "X") ];
    list_to_dict_test
      "An input of list [''; ''; 'X'] results in [(0, ''); (1, \n\
      \  ''); (2, 'X')]" [ ""; ""; "X" ] 0
      [ (0, ""); (1, ""); (2, "X") ];
    list_to_dict_test
      "An input of list ['X'; ''; ''] results in [(0, 'X'); (1, \n\
      \  ''); (2, '')]" [ "X"; ""; "" ] 0
      [ (0, "X"); (1, ""); (2, "") ];
    list_to_dict_test
      "An input of list [''; 'O'; ''] results in [(0, ''); (1, \n\
      \  'O'); (2, '')]" [ ""; "O"; "" ] 0
      [ (0, ""); (1, "O"); (2, "") ];
    list_to_dict_test
      "An input of list ['X'; 'O'; ''] results in [(0, 'X'); (1, \n\
      \  'O'); (2, '')]" [ "X"; "O"; "" ] 0
      [ (0, "X"); (1, "O"); (2, "") ];
    list_to_dict_test
      "An input of list ['X'; 'O'; ''] results in [(0, 'X'); (1, \n\
      \  'O'); (2, '')]" [ "X"; "O"; "" ] 0
      [ (0, "X"); (1, "O"); (2, "") ];
    (* testing for rem_spots_helper *)
    rem_spots_helper_test
      "Given an empty dictionary of pairings, this returns an\n  empty list" []
      [];
    rem_spots_helper_test
      "Given dictionary [(0, ''); (1, ''); (2, ''); (3, 'X')]\n\
      \  , this returns list [0, 1, 2]"
      [ (0, ""); (1, ""); (2, ""); (3, "X") ]
      [ 0; 1; 2 ];
    rem_spots_helper_test
      "Given dictionary [(0, ''); (1, ''); (2, 'X'); (3, '')]\n\
      \  , this returns list [0, 1, 3]"
      [ (0, ""); (1, ""); (2, "X"); (3, "") ]
      [ 0; 1; 3 ];
    rem_spots_helper_test
      "Given dictionary [(0, ''); (1, 'X'); (2, ''); (3, '')]\n\
      \  , this returns list [0, 2, 3]"
      [ (0, ""); (1, "X"); (2, ""); (3, "") ]
      [ 0; 2; 3 ];
    rem_spots_helper_test
      "Given dictionary [(0, 'X'); (1, ''); (2, ''); (3, '')]\n\
      \  , this returns list [1, 2, 3]"
      [ (0, "X"); (1, ""); (2, ""); (3, "") ]
      [ 1; 2; 3 ];
    rem_spots_helper_test
      "Given dictionary [(0, ''); (1, 'X'); (2, 'O'); (3, '')]\n\
      \  , this returns list [0, 3]"
      [ (0, ""); (1, "X"); (2, "O"); (3, "") ]
      [ 0; 3 ];
    (* testing for rem spots *)
    rem_spots_test
      "The remaining spots of an empty 2D array, results in the empty\n  list"
      [| [||] |] [];
    rem_spots_test "remaining spots of a non-empty 2D array case 1 results in: "
      [| [| ""; ""; "O" |]; [| "X"; ""; "X" |]; [| ""; ""; "O" |] |]
      [ 0; 1; 4; 6; 7 ];
    rem_spots_test "non-empty 2D array, second and third row empty "
      [| [| ""; ""; "O" |]; [| ""; ""; "" |]; [| ""; ""; "" |] |]
      [ 0; 1; 3; 4; 5; 6; 7; 8 ];
    rem_spots_test "non-empty 2D array, first and third row empty"
      [| [| ""; ""; "" |]; [| "X"; "O"; "" |]; [| ""; ""; "" |] |]
      [ 0; 1; 2; 5; 6; 7; 8 ];
    rem_spots_test "non-empty 2D array, first and second row empty"
      [| [| ""; ""; "" |]; [| ""; ""; "" |]; [| "O"; "O"; "X" |] |]
      [ 0; 1; 2; 3; 4; 5 ];
    rem_spots_test "remaining spots of a non-empty 2D array case 3 results in: "
      [| [| "X"; "X"; "O" |]; [| "O"; "X"; "" |]; [| "O"; "O"; "X" |] |]
      [ 5 ];
    available_test
      "user input corresponding to taken spot on the board, case 1:"
      [| [| ""; ""; "O" |]; [| "X"; ""; "X" |]; [| ""; ""; "O" |] |]
      2 false;
    available_test
      "user input corresponding to taken spot on the board, case 2:"
      [| [| ""; ""; "O" |]; [| "X"; ""; "X" |]; [| ""; ""; "O" |] |]
      5 false;
    available_test "user input corresponding to an empty spot results in true"
      [| [| ""; ""; "O" |]; [| "X"; ""; "X" |]; [| ""; ""; "O" |] |]
      6 true;
    (******************** TESTING FOR COUNTING FUNCTIONS ********************)
    (* testing for count *)
    count_test "the count of element 'x' in the empty list is 0" [] "x" 0;
    count_test "the count of element 'a' in list ['b'; 'a'] is 1" [ "b"; "a" ]
      "a" 1;
    count_test "the count of element 'd' in list ['a'; 'b'; 'c'] is 0"
      [ "a"; "b"; "c" ] "d" 0;
    (* testing for count_x *)
    countx_test "the count of 'X' in row 0 of given non-empty array is one"
      [| [| "X"; "O"; "" |]; [| ""; "X"; "X" |]; [| "O"; "O"; "" |] |]
      0 1;
    countx_test "the count of 'X' in row 1 of given non-empty array is two"
      [| [| "X"; "O"; "" |]; [| ""; "X"; "X" |]; [| "O"; "O"; "" |] |]
      0 1;
    countx_test "the count of 'X' in row 2 of given non-empty array is zero"
      [| [| "X"; "O"; "" |]; [| ""; "X"; "X" |]; [| "O"; "O"; "" |] |]
      2 0;
    counto_test "the count of 'O' in row 0 of given non-empty array is one"
      [| [| "X"; "O"; "" |]; [| ""; "X"; "" |]; [| "O"; "O"; "" |] |]
      0 1;
    counto_test "the count of 'O' in row 1 of given non-empty array is zero"
      [| [| "X"; "O"; "" |]; [| ""; "X"; "" |]; [| "O"; "O"; "" |] |]
      1 0;
    counto_test "the count of 'O' in row 2 of given non-empty array is two"
      [| [| "X"; "O"; "" |]; [| ""; "X"; "" |]; [| "O"; "O"; "" |] |]
      2 2;
    count_totalx_test "the total x count of the given non-empty array is zero"
      [| [| "O"; "O"; "" |]; [| ""; "O"; "" |]; [| ""; ""; "" |] |]
      0;
    count_totalx_test "the total x count of the given non-empty array is three"
      [| [| "X"; "O"; "" |]; [| ""; "X"; "X" |]; [| "O"; "O"; "" |] |]
      3;
    count_totalo_test "the total o count of the given non-empty array is zero"
      [| [| "X"; "X"; "" |]; [| ""; "X"; "X" |]; [| "X"; "X"; "" |] |]
      0;
    count_totalo_test "the total o count of the given non-empty array is five"
      [| [| "X"; "O"; "O" |]; [| ""; "O"; "X" |]; [| "O"; "O"; "" |] |]
      5;
  ]

let fill_array1 =
  [| [| "0"; "1"; "2" |]; [| "3"; "4"; "5" |]; [| "6"; "7"; "8" |] |]

let fill_array2 =
  [|
    [| "0"; "1"; "2"; "3"; "4"; "5" |];
    [| "6"; "7"; "8"; "9"; "10"; "11" |];
    [| "12"; "13"; "14"; "15"; "16"; "17" |];
    [| "18"; "19"; "20"; "21"; "22"; "23" |];
    [| "24"; "25"; "26"; "27"; "28"; "29" |];
    [| "30"; "31"; "32"; "33"; "34"; "35" |];
  |]

let fill_array3 =
  [|
    [| "0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8" |];
    [| "9"; "10"; "11"; "12"; "13"; "14"; "15"; "16"; "17" |];
    [| "18"; "19"; "20"; "21"; "22"; "23"; "24"; "25"; "26" |];
    [| "27"; "28"; "29"; "30"; "31"; "32"; "33"; "34"; "35" |];
    [| "36"; "37"; "38"; "39"; "40"; "41"; "42"; "43"; "44" |];
    [| "45"; "46"; "47"; "48"; "49"; "50"; "51"; "52"; "53" |];
    [| "54"; "55"; "56"; "57"; "58"; "59"; "60"; "61"; "62" |];
    [| "63"; "64"; "65"; "66"; "67"; "68"; "69"; "70"; "71" |];
    [| "72"; "73"; "74"; "75"; "76"; "77"; "78"; "79"; "80" |];
  |]

let update_array1 =
  [| [| "0"; "1"; "2" |]; [| "3"; "X"; "5" |]; [| "6"; "7"; "8" |] |]

let update_array2 =
  [|
    [| "0"; "1"; "2"; "3"; "4"; "5" |];
    [| "6"; "7"; "8"; "9"; "10"; "11" |];
    [| "12"; "13"; "14"; "X"; "16"; "17" |];
    [| "18"; "19"; "20"; "21"; "22"; "23" |];
    [| "24"; "25"; "26"; "27"; "28"; "29" |];
    [| "30"; "31"; "32"; "33"; "34"; "35" |];
  |]

let update_array3 =
  [|
    [| "0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8" |];
    [| "9"; "10"; "11"; "12"; "13"; "14"; "15"; "16"; "17" |];
    [| "18"; "19"; "20"; "21"; "22"; "23"; "24"; "25"; "26" |];
    [| "27"; "28"; "29"; "30"; "31"; "32"; "33"; "34"; "35" |];
    [| "36"; "37"; "38"; "39"; "40"; "41"; "42"; "43"; "44" |];
    [| "45"; "46"; "47"; "48"; "49"; "50"; "51"; "52"; "53" |];
    [| "54"; "55"; "56"; "57"; "58"; "59"; "60"; "61"; "62" |];
    [| "63"; "64"; "65"; "66"; "67"; "68"; "69"; "O"; "71" |];
    [| "72"; "73"; "74"; "75"; "76"; "77"; "78"; "79"; "80" |];
  |]

let updatetests =
  let arr1 = make_array 3 3 |> fill_array in
  let arr2 = make_array 6 6 |> fill_array in
  let arr3 = make_array 9 9 |> fill_array in
  [
    ( "Testing update_array2 1" >:: fun _ ->
      assert_equal update_array2
        (let _ = arr2 |> update_array "15" in
         arr2) );
    ( "Testing update_array1 2" >:: fun _ ->
      assert_equal update_array1
        (let _ = arr1 |> update_array "4" in
         arr1) );
    ( "Testing update_array using get_val 3" >:: fun _ ->
      assert_equal "X"
        (let _ = arr1 |> update_array "1" in
         get_value (0, 1) arr1) );
    ( "Testing update_array using get_val 4" >:: fun _ ->
      assert_equal "X"
        (let _ = arr1 |> update_array "1" in
         get_value (0, 1) arr1) );
    ( "Testing update_array 5" >:: fun _ ->
      assert_equal "O"
        (user := !user + 1;
         let () = arr1 |> update_array "0" in
         get_value (0, 0) arr1) );
    ( "Testing update_array1 2" >:: fun _ ->
      assert_equal update_array3
        (user := !user + 1;
         let _ = arr3 |> update_array "70" in
         arr3) );
  ]

let menu_tests =
  [
    ( "Testing total element 1" >:: fun _ ->
      assert_equal 81 (fill_array3 |> total_elements) ~printer:string_of_int );
    ( "Testing total element 2" >:: fun _ ->
      assert_equal 9 (fill_array1 |> total_elements) );
    ( "Testing total element 3" >:: fun _ ->
      assert_equal 36 (fill_array2 |> total_elements) );
    ( "Testing make_array using\n\n total_elements 4" >:: fun _ ->
      assert_equal 36 (make_array 6 6 |> total_elements) );
    ( "Testing make_array using total_elements 5" >:: fun _ ->
      assert_equal 9 (make_array 3 3 |> total_elements) );
    ( "Testing make_array using\n       total_elements 6" >:: fun _ ->
      assert_equal 81 (make_array 9 9 |> total_elements) );
    ( "Testing fill_array 6 x 6 7" >:: fun _ ->
      assert_equal fill_array2 (make_array 6 6 |> fill_array) );
    ( "Testing      fill_array 9 x 9 8" >:: fun _ ->
      assert_equal fill_array3 (make_array 9 9 |> fill_array) );
    ( "Testing get_value 3x3 " >:: fun _ ->
      assert_equal 0
        (int_of_string (get_value (0, 0) fill_array3))
        ~printer:string_of_int );
    ( "Testing get_value 1" >:: fun _ ->
      assert_equal "4" (get_value (1, 1) fill_array1) );
    ( "Testing get_value 2" >:: fun _ ->
      assert_equal "3" (get_value (1, 0) fill_array1) );
    ( "Testing get_value 3" >:: fun _ ->
      assert_equal "8" (get_value (2, 2) fill_array1) );
    ( "Testing get_value 3x3 4" >:: fun _ ->
      assert_equal "2" (get_value (0, 2) fill_array1) );
    ( "Testing get_value 6x6" >:: fun _ ->
      assert_equal "0" (get_value (0, 0) fill_array2) );
    ( "Testing get_value 6x6" >:: fun _ ->
      assert_equal "35" (get_value (5, 5) fill_array2) );
    ( "Testing get_value 6x6" >:: fun _ ->
      assert_equal "17" (get_value (2, 5) fill_array2) );
    ( "Testing get_value 6x6" >:: fun _ ->
      assert_equal "18" (get_value (3, 0) fill_array2) );
    ( "Testing get_value 9x9" >:: fun _ ->
      assert_equal "0" (get_value (0, 0) fill_array3) );
    ( "Testing get_value 9x9" >:: fun _ ->
      assert_equal "80" (get_value (8, 8) fill_array3) );
    ( "Testing    get_value 9x9" >:: fun _ ->
      assert_equal "50" (get_value (5, 5) fill_array3) );
    ( "Testing is_customn " >:: fun _ ->
      assert_equal false (is_custom "Default Game") );
    ( "Testing is_customn " >:: fun _ ->
      assert_equal true (is_custom "Custom Game ") );
  ]

let tests =
  "test suite"
  >::: List.flatten
         [
           size_tests;
           rows_tests;
           columns_tests;
           diagonals_tests;
           check_win_tests;
           full_tests;
           tie_tests;
           ai_tests;
           best_move_tests;
           menu_tests;
           updatetests;
         ]

let _ = run_test_tt_main tests
