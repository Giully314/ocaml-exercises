(* module type Map = sig
  type ('k, 'v) t
  (** Type representation of the map. *)

  val empty : ('k, 'v) t
  (** Empty instance of the map. *)

  (* val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t *)
  (** Insert a pair of key, value in the map. *)

  (* val lookup : 'k -> ('k, 'v) t -> 'v *)
  (** Retrieve the element associated to the key. Raise if the element is not
      present. *)
end

module BstMap : Map = struct
  (* type ('k, 'v) t = Nil | Node of 'k * 'v * ('k, 'v) t * ('k, 'v) t *)
  type ('k, 'v) t = Nil | Node of ('k, 'v) node

  and ('k, 'v) node = {
    key : 'k;
    value : 'v;
    left : ('k, 'v) t;
    right : ('k, 'v) t;
  }

  let empty = Nil

  
end *)
