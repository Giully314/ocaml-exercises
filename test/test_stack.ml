open OUnit2
open Exercises.Stack

let rec build_stack_for_test elems stack =   
  match elems with 
    | [] -> stack
    | h :: t -> build_stack_for_test t (ListStackCached.push h stack)

let make_test_stack_push name expected_output stack input =
  name >:: (fun _ -> assert_equal 
    (build_stack_for_test expected_output ListStackCached.empty) 
    (ListStackCached.push input stack))



let test_stack_push = "test stack push" >::: [
  make_test_stack_push "one element" [2] ListStackCached.empty 2
]

let _ = run_test_tt_main test_stack_push