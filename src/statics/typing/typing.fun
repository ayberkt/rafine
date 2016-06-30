functor Typing (Syn : SYNTAX) : TYPING =
struct
  open Syn

  type tctx = unit Ctx.dict
  type ectx = typ Ctx.dict

  exception Todo

  fun checkTyp delta t = raise Todo

  fun check (delta, gamma) e t = raise Todo

end
