module type Tree = sig
  type 'a tree

  val size : 'a tree -> int

end


module BinaryTree  = struct
  type 'a tree = 
    | Nil 
    | Node of 'a node
  and 
  'a node = {value: 'a; left: 'a tree; right: 'a tree; }
  
  (* let rec size = function 
    | Nil -> 0 
    | Node {left; right; _} -> 1 + size left + size right *)
  
  let size_tr t = 
    let rec aux acc = function
      | [] -> acc
      | Nil :: t -> aux acc t
      | Node {left; right; _} :: t -> aux (acc+1) (left :: right :: t)
  in aux 0 [t]
  
  
  let rec map f = function
    | Nil -> Nil
    | Node {value; left; right} -> Node {value = f value; left = map f left; right = map f right}
  
  let rec fold f acc = function
    | Nil -> acc
    | Node {value; left; right} -> f value (fold f acc left) (fold f acc right)
  
  
  let size t = fold (fun _ l r -> 1 + l + r) 0 t
  let depth t = fold (fun _ l r -> 1 + max l r) 0 t
  let preorder t = fold (fun v l r -> [v] @ l @ r) [] t
  
  
  let rec filter f = function
    | Nil -> Nil
    | Node {value; left; right} -> if f value then Node {value; left=filter f left; right=filter f right} else Nil
end

(* To avoid to mask all the values, we do the check of interface signature on the tree here. *)
module BinaryTreeCheck : Tree = BinaryTree 