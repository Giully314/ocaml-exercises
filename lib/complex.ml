module type ComplexSig = sig
  type t
  (** Define the type of the representation of a complex number. *)

  val zero : t
  (** Get the zero value. *)

  val add : t -> t -> t
  (** Add two complex numbers. *)

end


module ComplexFloat : ComplexSig = struct
  type t = float * float

  let zero = (0.0, 0.0)

  let add (xr, xi) (yr, yi) = (xr +. yr, xi +. yi)

end