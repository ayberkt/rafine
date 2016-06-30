signature TYPING =
sig
  type exp
  type typ

  type tctx
  type ctxref
  type ectx

  val checkTyp : tctx -> typ -> bool
  val check : tctx * ectx * ctxref -> exp -> typ -> bool
end
