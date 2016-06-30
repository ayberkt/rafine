functor Typing (Syn : SYNTAX) : TYPING =
struct
  open Syn

  type tctx   = unit Ctx.dict
  type ctxref = unit Ctx.dict
  type ectx   = typ Ctx.dict

  exception Todo

  fun checkTyp delta t = raise Todo

  fun inferTyp delta t = raise Todo

  fun check (gamma, pi) e t = raise Todo

end
