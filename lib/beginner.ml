
(* Tail of a list *)
let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: x -> last x


let rec last_two (l: 'a list) = 
  match l with
    | [] -> None
    | [x; y] -> Some (x, y)
    | _ :: l -> last_two l


(* let rec nth l n = 
  if n > 0 then 
    match l with 
      | [] -> None
      | _ :: t -> nth t (n-1)
  else if n == 0 then 
    match l with 
      | [] -> None 
      | h :: _ -> Some h
  else None *)

let nth l n =
  if n < 0 then None
  else let rec aux l n =
    match l with 
      | [] -> None
      | h :: t -> if n == 0 then Some h else aux t (n-1)
  in aux l n


(* let length l = 
  let rec aux l = 
    match l with 
      | [] -> 0
      | _ :: t -> 1 + aux t 
  in aux l  *)

let length l = 
  let rec aux l acc = 
    match l with 
      | [] -> acc
      | _ :: t -> aux t (acc+1)
  in aux l 0
  
let rev l =  
  let rec aux l acc = 
    match l with
    | [] -> acc
    | h :: t -> aux t (h :: acc)
  in aux l []


let is_palindrome l = 
  l = rev l


let rle_encode l = 
  let rec aux count acc = function 
    | [] -> acc
    | [x] -> (count + 1, x) :: acc
    | h1 :: (h2 :: _ as t) -> if h1 = h2 then aux (count + 1) acc t else aux 0 ((count + 1, h1) :: acc) t
in rev (aux 0 [] l)


type 'a rle = 
  | One of 'a
  | Many of int * 'a

let rle_encode2 l = 
  let rec aux count acc = function 
    | [] -> acc
    | [x] -> if count = 0 then One x :: acc else Many (count+1, x) :: acc
    | h1 :: (h2 :: _ as t) -> 
      if h1 = h2 then aux (count + 1) acc t 
      else 
        if count <> 0 then aux 0 (Many (count+1, h1) :: acc) t else aux 0 (One h1 :: acc) t
in rev (aux 0 [] l)


let duplicate l = 
  let rec aux acc = function 
    | [] -> acc
    | h :: t -> aux (h :: h :: acc) t
in rev (aux [] l)


let split list n = 
  let rec aux n l r = 
    if n = 0 then rev l, r
    else match r with 
      | [] -> rev l, r
      | h :: t -> aux (n-1) (h :: l) t
  in aux n [] list


(* let rec remove_at n = function 
  | [] -> []
  | h :: t -> if n = 0 then t else h :: remove_at (n-1) t *)

let remove_at n list = 
  let rec aux n acc l = 
    if n = 0 then match l with
      | [] -> rev acc
      | _ :: t -> List.rev_append acc t
    else match l with
      | [] -> rev acc
      | h :: t -> aux (n-1) (h :: acc) t
  in aux n [] list


(* let rec insert_at e n = function 
  | [] -> [e]
  | h :: t -> if n = 0 then e :: h :: t else h :: insert_at e (n-1) t *)

let insert_at e n list = 
  let rec aux e n acc = function 
    | [] -> List.rev_append acc [e]
    | h :: t -> if n = 0 then List.rev_append acc (e :: h :: t) else aux e (n-1) (h :: acc) t
in aux e n [] list


(* let range start end_ = 
  if start < end_ then
    let rec aux s e acc = 
      if s = e then rev (s :: acc)
      else aux (s+1) e (s :: acc)
    in aux start end_ []
  else 
    let rec aux s e acc = 
      if s = e then rev (s :: acc)
      else aux (s-1) e (s :: acc)
    in aux start end_ [] *)

(* Implementation from the solution, much cleaner than mine. *)
let range a b = 
  let rec aux high low acc = 
    if high >= low then aux (high - 1) low (high :: acc)
    else acc
  in if a < b then aux b a [] else rev (aux a b [])



type 'a tree = Node of 'a * 'a tree * 'a tree | Empty

let count_leaves t = 
  let rec aux  = function 
    | Empty -> 0
    | Node(_, Empty, Empty) -> 1
    | Node(_, l, r) -> (aux l) + (aux r)
in aux t