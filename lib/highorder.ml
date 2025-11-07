(* To force evaluation from left to right, we use the let ... in ... in the recursive call.
Remember ocaml doesn't guarantee the order of sub expressions (but usually is right to left)
This usually matters when there are side effects (such as printing) *)
let rec map f = function
  | [] -> []
  | h :: t -> let h' = f h in h' :: map f t

(* Because we are using single linked list (which appends to front), this is tail recursive but it reverse the original list. 
We could use the operator @ but this would mean to go to O(nˆ2) *)
let map_tr f = 
  let rec aux f acc = function
    | [] -> acc
    | h :: t -> aux f (f h :: acc) t 
in aux f []


let rec filter p = function 
  | [] -> []
  | h :: t -> if p h then h :: filter p t else filter p t


let filter_tr p = 
  let rec aux p' acc = function
    | [] -> acc (* Here we could call the List.rev to reverse directly here, as done in the standard library. *)
    | h :: t -> if p' h then aux p' (h :: acc) t else aux p' acc t
in aux p


let rec fold_right f lst (acc : 'acc) = match lst with 
  | [] -> acc  
  | h :: t -> f h (fold_right f t acc)

  (* Same as fold right but using the let to assign acc. Notice the difference between this and left_folo_tr*)
let rec combine f acc = function
  | [] -> acc
  | h :: t -> let acc' = combine f acc t in f h acc'

let rec left_fold_tr f acc = function
  | [] -> acc
  | h :: t -> let acc' = f acc h in left_fold_tr f acc' t


let rec repeat f n x = 
  if n = 1 then f x
  else repeat f (n-1) (f x) 