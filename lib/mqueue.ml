type 'a t = {o : 'a list; i : 'a list}
(** Implementation of a batched queue formed by two lists for ammortized constant complexity. 
  [o;i] represents the queue [o @ List.rev i].
  A condition always respected is that if the o is empty then the queue is empty. 
  This means that if 'o' is empty 'i' is also empty. 
    *)

exception Empty
let empty = {o = []; i = []}

let is_empty = function 
  | {o = []; _} -> true
  | _ -> false

let enqueue x = function
  | {o=[]; _} -> {o = [x]; i = []}
  | {o; i} -> {o; i = x :: i}

let front = function 
  | {o = []; _} -> raise Empty
  | {o= h :: _; _} -> h

let dequeue = function 
  | {o = []; _} -> raise Empty
  | {o = [_]; i} -> {o = List.rev i; i = []}
  | {o = _ :: t; i} -> {o=t; i}

let size {o; i} = List.(length o + length i)

let to_list {o; i} = o @ List.rev i