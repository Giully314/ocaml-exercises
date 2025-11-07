type 'a binary_tree = 
  | Nil 
  | Node of 'a * 'a binary_tree * 'a binary_tree


let tree_size_tr tree = 
  let rec aux acc work_list = 
    match work_list with
      | [] -> acc
      | Nil :: rest -> aux acc rest 
      | Node (_, l, r) :: rest -> aux (acc + 1) (l :: r :: rest)
  in aux 0 [tree]

let rec binary_tree_size = function
  | Nil -> 0
  | Node (_, l, r) -> 1 + binary_tree_size l + binary_tree_size r


type 'a tree = 
  | Nil
  | Node of 'a node 
and 'a node = {
  value : 'a;
  left: 'a tree;
  right: 'a tree;
}
