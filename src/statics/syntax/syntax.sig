signature PRE_SYNTAX =
sig
  type var

  type exp
  type typ

  datatype ('t, 'e) exp_view =
    VAR of var
  | LAM of var * 'e
  | AP of  'e * 'e
  | ANN of 'e * 't

  datatype 't typ_view =
    BASE
  | ARR of 't * 't
  | REF of 't * 't
  | SUB of 't * 't

  val intoExp : (typ, exp) exp_view -> exp
  val intoTyp : typ typ_view -> typ
end

signature SYNTAX =
sig
  include PRE_SYNTAX

  val typEq : typ * typ -> bool
  val expEq : exp * exp -> bool

  val substTyp : var * typ -> typ -> typ

  structure Ctx : DICT where type key = var

  val outExp : exp -> (typ, exp) exp_view
  val outTyp : typ -> typ typ_view
end
