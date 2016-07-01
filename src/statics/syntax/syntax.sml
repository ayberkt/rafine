structure Abt = SimpleAbt (Operator)
structure ShowAbt = DebugShowAbt (Abt)

functor PreSyntax (View : ABT_SYNTAX_VIEW_INTO
                     where type 'a spine = 'a list
                      and type 'a operator = OperatorData.t
                      and type sort = SortData.t) : PRE_SYNTAX =
struct
  type var = View.variable
  type exp = View.term
  type typ = View.term

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

  exception Todo

  open View
  infix $ $$ \

  structure O = OperatorData

  val intoExp =
    fn (VAR x) => check (`x, SortData.EXP)
     | (AP (e1, e2)) => O.AP $$ [([],[]) \ e1, ([],[]) \ e2]
     | (LAM (x, e)) => O.LAM $$ [([],[x]) \ e]
     | (ANN (e, t)) => O.ANN $$ [([],[]) \ e, ([],[]) \ t]

  val intoTyp =
    let
      fun mkType tau (t1, t2) = tau $$ [([],[]) \ t1, ([],[]) \ t2]
    in
      fn  ARR  (t1, t2) => mkType O.ARR (t1, t2)
        | REF  (t1, t2) => mkType O.REF (t1, t2)
        | SUB  (t1, t2) => mkType O.SUB (t1, t2)
        | BASE          => O.BASE $$ []
    end

end

structure Syntax : SYNTAX =
struct
  local
    structure View = AbtSyntaxView (Abt)
    structure S = PreSyntax (View)
    structure Abt = Abt
    open Abt
    structure O = OperatorData
    infix $ \
  in
    open S

    structure Ctx = Abt.Var.Ctx
    val typEq = Abt.eq
    val expEq = Abt.eq

    fun substTyp (x, t) t' =
      Abt.subst (t, x) t'

    fun outExp e =
      case View.infer e of
         (`x, SortData.EXP) => VAR x
      | (O.LAM $ [(_,[x]) \ e], _) => LAM (x, e)
      | (O.AP $ [_ \ e1, _ \ e2], _) => AP (e1, e2)
      | (O.ANN $ [_ \ e, _ \ t], _) => ANN (e, t)
      | _ => raise Fail "Invalid expression"

    fun   outTyp t =
      case View.infer t of
         (O.ARR $ [_ \ t1, _ \ t2], _) => ARR (t1, t2)
      |  (O.BASE $ [], _) => BASE
      | _ => raise Fail "Invalid type"
  end
end

structure Ast = Ast (structure Operator = Operator and Metavar = Abt.Metavar)
structure AstAbt =
struct
  structure Abt = Abt
  structure Ast = Ast
end
structure AstSyntax : PRE_SYNTAX = PreSyntax (AstSyntaxView (structure Ast = Ast type sort = SortData.t))
structure AstToAbt = AstToAbt (AstAbt)
