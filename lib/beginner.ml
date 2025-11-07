
let rec sum = function
  | [] -> 0
  | x :: v -> x + sum v

let rec last l = 
  match l with
    | [] -> None
    | [ v ] -> Some v
    | _ :: v -> last v

let rec last_two l = 
  match l with
    | [] | [_] -> None
    | [ u; v ] -> Some (u, v)
    | _ :: v -> last_two v

(*Get a list and an index and return an option of the element if present*)
(* let rec at l i = 
  match l with 
    | [] -> None
    | h :: t -> if i *)

let rec at k = function  
  | [] -> None
  | h :: t -> if k = 0 then Some h else at (k - 1) t


let rec fib_h n f1 f2 = 
  if n = 1 then f1 
  else fib_h (n-1) f2 (f1 + f2)

let fib_fast n = fib_h n 1 1


let rec list_max = function
  | [] -> None
  | h :: t -> begin 
    match list_max t with
      | None -> Some h
      | Some m -> Some (max h m)
  end


let rec product = function 
  | [] -> 1
  | h :: t -> h * product(t)

let product_tr l = 
  let rec aux l' acc =
    match l' with 
      | [] -> acc
      | h :: t -> aux t (acc * h)
  in aux l 1


let rec concatenate = function 
  | [] -> ""
  | h :: t -> h ^ concatenate t
  
let concatenate_tr l = 
  let rec aux l' acc = 
    match l' with
      | [] -> acc
      | h :: t -> aux t (acc ^ h)
  in aux l ""


(* Exercise on patterns *)

let first_is_bigred = function 
  | "bigred" :: _ -> true 
  | _ -> false

let has_2_or_4 = function
  | _ :: _ :: [] -> true
  | _ :: _ :: _ :: _ :: [] -> true
  | _ -> false


let has_first_2_equals = function
  | x :: y :: _ -> x = y 
  | _ -> false


let fifth_or_zero (l : int list) = 
  if List.length l >= 5 then List.nth l 5
  else 0

(* A better alternative is to pass directly a compare fuction which inverts the standard behavior. *)
let descending_order (l : int list) = 
  List.sort Stdlib.compare l |> List.rev


let last_element l = 
  List.nth l ((List.length l) - 1)

let any_zeros l =
  List.exists (fun x -> x == 0) l


(* let take n lst = 
  let take_size_less_than_n n lst = lst in
  let take_aux n lst = 
    match n with
      0 -> []
      _ ->  *)

let take n lst =
  if n >= List.length lst then lst
  else  
    let rec aux n lst =   
      match lst with
        | [] -> []
        | h :: t -> if n = 0 then [] else h :: aux (n-1) t 
    in aux n lst

(* This approach is a lot easier to read.
My approach (above) optimizes the cases where n > size list by returning it directly. (useless without profiling and checking code usage).
*)
let rec take_from_solution n lst = 
  if n = 0 then [] else match lst with
    | [] -> []
    | h :: t -> h :: take_from_solution (n-1) t

let take_tr n lst =
  if n >= List.length lst then lst
  else 
    let rec aux n lst acc =
      match lst with
        | [] -> acc
        | h :: t -> if n = 0 then acc else aux (n-1) t (h :: acc)
    in aux n lst [] |> List.rev

let drop n lst = 
  if n >= List.length lst then []
  else
    let rec aux n = function 
      | [] -> []
      | _ :: t -> if n = 1 then t else aux (n-1) t
  in aux n lst