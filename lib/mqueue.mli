type 'a t
(** An ['a t] is a queue whose elements have type ['a]. *)

exception Empty
(** Raised if [front] or [dequeue] is applied to an empty queue. *)

val empty : 'a t
(** Construct an empty queue. *)

val is_empty : 'a t -> bool
(** [is_empty q] is wheter [q] is empty. *)

val enqueue : 'a -> 'a t -> 'a t
(** [enqueue x q] insert the element [x] into the queue [q] at the end. *)

val front : 'a t -> 'a 
(** [front q] returns the element at the front of the queue (the first). *)

val dequeue : 'a t -> 'a t
(** [dequeue q] is the queue containing all elements of [q] except the first. 
    Raises [Empty] if the queue is empty. *)

val size : 'a t -> int
(** [size q] returns the number of elements in the queue. *)

val to_list : 'a t -> 'a list
(** [to_list q] returns a list representation of the queue. *)