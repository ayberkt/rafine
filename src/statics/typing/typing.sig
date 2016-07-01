signature TYPING =
sig
  type exp
  type typ

  type ctxref
  type ectx

  exception TypeError of string

  val check : ectx * ctxref -> exp -> typ -> bool
end
