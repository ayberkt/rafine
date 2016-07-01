functor Typing (Syn : SYNTAX) : TYPING =
struct
  open Syn

  type ctxref = typ Ctx.dict
  type ectx   = typ Ctx.dict

  exception TypeError of string
  exception Todo

  fun $ (f, x) = f x
  infixr 0 $

  fun check (gamma, pi) (e : exp) (t : typ) : bool =
    case (outExp e, outTyp t) of
      (LAM (x, m), ARR (r, s)) => check (Ctx.insert gamma x r, pi) m s
    | (_, _) => typEq (t, infer (gamma, pi) e)
  and infer (gamma, pi) (e : exp) : typ =
    case outExp e of
      VAR x => valOf $ Ctx.find gamma x
    | AP (e, m) =>
        let
          val ARR (r, s) = outTyp $ infer (gamma, pi) e
        in
          if check (gamma, pi) e r
          then s
          else raise TypeError "operator type does not match function domain."
        end
    | _ => raise TypeError "problem!"

end
