signature POSET =
sig
  type t
  val R : t * t -> bool
end

signature EQ =
sig
  type t
  val eq : t * t -> bool
end

signature SHOW =
sig
  type t
  val toString : t -> string
end

signature READ =
sig
  type t
  val read : string -> t
  exception ReadError
end

signature REFINEMENT_SET =
sig
  type t
  include POSET
  include EQ
  include SHOW
  include READ
end
