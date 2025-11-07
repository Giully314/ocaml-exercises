open OUnit2
open Exercises.Beginner

let tests = "test suite for last" >::: [
  "Empty lists" >:: (fun _ -> assert_equal None (last []));
]


let make_test_sum name expected_output input = 
  name >:: (fun _ -> assert_equal expected_output (sum input) ~printer:string_of_int)

let test_sum = "test suite for sum" >::: [
  make_test_sum "Empty list" 0 [];
]


let exc_tests = "suite" >::: [
  "empty" >:: (fun _ -> assert_raises (Failure "hd") (fun () -> List.hd []))
]

let make_product_test name expected_output input = 
    name >:: (fun _ -> assert_equal expected_output (product input) ~printer:string_of_int)
let make_product_tr_test name expected_output input = 
    name >:: (fun _ -> assert_equal expected_output (product_tr input) ~printer:string_of_int)

let test_product = "test product" >::: [
  make_product_test "Empty" 1 [];
  make_product_test "3 * 2" 6 [3;2];
  make_product_tr_test "Empty" 1 [];
  make_product_tr_test "3 * 2" 6 [3;2];
]

let make_concatenate_test name expected_output input = 
  name >:: (fun _ -> assert_equal expected_output (concatenate input))
let make_concatenate_tr_test name expected_output input = 
  name >:: (fun _ -> assert_equal expected_output (concatenate_tr input))
let test_concatenate = "test concatenate" >::: [
  make_concatenate_test "Empty" "" [];
  make_concatenate_test "Hello world" "Hello world" ["Hello"; " "; "world"];
  make_concatenate_tr_test "Empty" "" [];
  make_concatenate_tr_test "Hello world" "Hello world" ["Hello"; " "; "world"];
]


let make_descending_order_test name expected_output input =
    name >:: (fun _ -> assert_equal expected_output (descending_order input))

let test_descending_order = "test descending order" >::: [
  make_descending_order_test "Simple" [10; 9; 4; 1] [4; 1; 9; 10]
]

let _ = run_test_tt_main tests
let _ = run_test_tt_main test_sum
let _ = run_test_tt_main exc_tests
let _ = run_test_tt_main test_product
let _ = run_test_tt_main test_concatenate
let _ = run_test_tt_main test_descending_order