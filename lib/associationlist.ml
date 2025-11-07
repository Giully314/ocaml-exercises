(* Association list *)
type ('a, 'b) listmap = ('a * 'b) list

let insert (k: 'a) (v: 'b) (lst: ('a, 'b) listmap ) = (k, v) :: lst

(** [lookup k lst] Return an optional with the value associated to the key if present, None otherwise. *)
let rec lookup k = function
  | [] -> None
  | (k', v) :: t -> if k = k' then Some v else lookup k t


(** [keys lst] get all the unique keys in the association list. *)
let keys (lst: ('a, 'b) listmap) = 
  lst 
  |> List.rev_map fst (* Because we don't care about the order, using the reverse option is better for tail recursion. *)
  |> List.sort_uniq Stdlib.compare