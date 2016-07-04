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
  type rfn = View.term

  datatype ('t, 'r, 'e) exp_view =
    VAR of var
  | LAM of var * 'e
  | AP of  'e * 'e
  | ANN of 'e * 't
  | RFANN of 'e * 'r

  datatype 't typ_view =
    BASE
  | ARR of 't * 't

  datatype 'r rfn_view =
    ATOM of OperatorData.RefSet.t
  | RFARR of 'r * 'r

  exception Todo

  open View
  infix $ $$ \

  structure O = OperatorData

  val intoExp =
    fn (VAR x) => check (`x, SortData.EXP)
     | (AP (e1, e2)) => O.AP $$ [([],[]) \ e1, ([],[]) \ e2]
     | (LAM (x, e)) => O.LAM $$ [([],[x]) \ e]
     | (ANN (e, t)) => O.ANN $$ [([],[]) \ e, ([],[]) \ t]
     | (RFANN (e, r)) => O.RFANN $$ [([],[]) \ e, ([],[]) \ r]

  val intoTyp =
    let
      fun mkType tau (t1, t2) = tau $$ [([],[]) \ t1, ([],[]) \ t2]
    in
      fn  ARR  (t1, t2) => mkType O.ARR (t1, t2)
        | BASE          => O.BASE $$ []
    end

  val intoRfn =
    fn RFARR (t1, t2) => O.RFARR $$ [([], []) \ t1, ([], []) \ t2]

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

    fun outTyp t =
      case View.infer t of
         (O.ARR $ [_ \ t1, _ \ t2], _) => ARR (t1, t2)
      |  (O.BASE $ [], _) => BASE
      | _ => raise Fail "Invalid type"

    fun outRfn t =
      case View.infer t of
        (O.RFARR $ [_ \ r1, _ \ r2], _) => RFARR (r1, r2)
      | ((O.ATOM i) $ [], _) => ATOM i
      | _ => raise Fail "Invalid refinement"

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
