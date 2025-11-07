


(** A ring is a set with 2 operations ([+] and [*]) which is the union of  
an abelian group under [+] and a monoid under [*] with the additional
link axiom of distribution of multiplication with respect to addition. *)
module type Ring = sig
  type t
  (** Type of the ring. Can be integer or float. *)
  val zero : t
  (** [zero] is the neutral element under addition. *)
  val one : t
  (** [one] is the neutral element under multiplication. *)
  
  val ( + ) : t -> t -> t
  (** [+] in a ring is associative and commutative. *)

  val ( * ) : t -> t -> t
  (** [*] in a ring is associative *)
  
  val ( ~- ) : t -> t 
  (** [-] represent the inverse operation of [+]. *)

  val to_string : t -> string
end

(** A field is ring with the additional axioms that the multiplication is 
also commutative and has an inverse operation. This means 
that both [+] and [*] are abelian groups. Alternatively we can say
that a field is a commutative ring where each non-zero element has an inverse. *)
module type Field = sig
  include Ring 

  val ( / ) : t -> t -> t
  (** [/] represent the inverse operation of [*]. *)

end

module MakeRing (M : Ring) = (struct
include M
end : Ring with type t = M.t)


module IntWrapper = struct 
  type t = int
  let zero = 0
  let one = 1
  let (+) = ( + )
  let (~-) = (~-)
  let ( * ) = ( * )
  let to_string = string_of_int
end

module IntRing = MakeRing(IntWrapper)