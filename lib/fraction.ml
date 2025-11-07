module type Fraction = sig
  type t
  (** A Fraction is a rational number p/q where q != 0. *)

  val make : int -> int -> t
  (** [make n d] represents n/d, a fraction with numerator [n] and denominator [d].
      Requires d <> 0. *)
  
      val numerator : t -> int
      val denominator : t -> int
      val to_string : t -> string
      val to_float : t -> float

      val add : t -> t -> t
      val mul : t -> t -> t

end


module TupleFraction : Fraction = struct 
  type t = int * int

  let make (x : int) (y : int) : t = (x, y)

  let numerator (x, _) = x

  let denominator (_, y) = y
  
  let to_string (x, y) = Printf.sprintf "%d/%d" x y

  let to_float (x, y) = float_of_int x /. float_of_int y

  let add (x1, y1) (x2, y2) = 
      let d = y1 * y2 in
        (x1 * y2 + x2 * y1, d)

  let mul (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)

end


module ReducedFraction : Fraction = struct
  type t = int * int

  let rec gcd x y = 
      if x = 0 then y
      else if x < y then gcd (y - x) x
      else gcd y (x - y)
  
  let rec make x y = 
    let x' = abs x in 
    let y' = abs y in
    let g = gcd x' y' in
    if g = 1 then (x', y')
    else make (x' / g) (y' / g)
      
    
  let numerator (x, _) = x

  let denominator (_, y) = y
  
  let to_string (x, y) = Printf.sprintf "%d/%d" x y

  let to_float (x, y) = float_of_int x /. float_of_int y

  let add (x1, y1) (x2, y2) = 
    let d = y1 * y2 in 
    make (x1 * y2 + x2 * y1) d

  let mul (x1, y1) (x2, y2) = 
    make (x1 * x2) (y1 * y2)

end