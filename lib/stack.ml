module type Stack = sig
  type 'a t
  (** Abstact type representing a stack.*)

  exception Empty
  (**Exception for empty stack.*)

  val empty : 'a t
  (**Get an empty stack.*)

  val is_empty : 'a t -> bool
  (**Check if the stack is empty.*)

  val push : 'a -> 'a t -> 'a t
  val peek : 'a t -> 'a
  val pop : 'a t -> 'a t
  val size : 'a t -> int

  val equal : 'a t -> 'a t -> bool
end

module ListStackCached : Stack = struct
  type 'a t = 'a list * int

  exception Empty

  let empty = ([], 0)
  let is_empty = function [], 0 -> true | _ -> false
  let push x (stack, size) = (x :: stack, size + 1)
  let peek = function [], _ -> raise Empty | h :: _, _ -> h
  let pop = function [], _ -> raise Empty | _ :: t, size -> (t, size - 1)
  let size = snd

  let equal s1 s2 = 
    List.equal (=) (fst s1) (fst s2)
end

module CustomStack : Stack = struct
  type 'a entry = { top : 'a; rest : 'a t; size : int }
  and 'a t = S of 'a entry option

  exception Empty 

  let empty = S None

  let is_empty = function
    | S None -> true
    | _ -> false

    let size = function 
      | S None -> 0
      | S (Some {size; _}) -> size

  let push x s = S (Some {top=x; rest=s; size=size s + 1})


  let peek = function 
    | S None -> raise Empty
    | S (Some {top; _}) -> top

  let pop = function
    | S None -> raise Empty
    | S (Some {rest; _}) -> rest

  let equal _ _ = true
end
