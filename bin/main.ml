let l = List.init 10 Fun.id
(* let l = [] *)
let q = Exercises.Beginner.last_two l
let () = print_endline "hello "

let () =
  match q with
  | None -> print_endline "list is empty"
  | Some (u, v) -> Printf.printf "last element is %d %d\n" u v
