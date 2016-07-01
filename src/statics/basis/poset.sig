signature POSET =
sig
  type t
  val eq : t * t -> bool
  val R : t * t -> bool
  val toString : t -> string
end
