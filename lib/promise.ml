module type PROMISE = sig
  type 'a state = Pending | Fulfilled of 'a | Rejected of exn

  type 'a promise

  type 'a resolver

  val make : unit -> 'a promise * 'a resolver
  (** [make] create a new promise with a resolver. *)

  val return : 'a -> 'a promise
  (** [return x] is a new promise which is already fullfilled with value. *)

  val state : 'a promise -> 'a state
  (** [state] is the state of the promise. *)

  val fulfill  : 'a resolver -> 'a -> unit
  (** [fulfill r x] fullfills the promise [p] associated with [r] with value [x] mean *)

  val reject : 'a resolver -> exn -> unit
  (** [reject r x] rejects the promise [p] associated with [r] with exception [x], meaning that [state p] will become [Rejecte x].
    Requires [p] is pending. *)
end 
