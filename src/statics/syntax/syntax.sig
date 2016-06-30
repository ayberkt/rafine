signature PRE_SYNTAX =
sig
  type var

  type exp
  type typ

  datatype ('t, 'e) val_view =
    LAM of var * 'e

  datatype ('t, 'e) neu_view =
      VAR of var
    | AP of 'e * 'e

  datatype ('t, 'e) exp_view =
      ANN of 'e * 't
    | NEU of ('t, 'e) neu_view
    | VAL of ('t, 'e) val_view

  datatype 't typ_view =
      ARR of 't * 't
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
