structure OperatorData =
struct
    datatype t = LAM | AP | ARR | BASE | REF | SUB | ANN

    val eq : t * t -> bool = op=

    val toString =
      fn LAM  => "lam"
       | AP   => "ap"
       | BASE => "base"
       | ARR  => "arr"
       | REF  => "ref"
       | SUB  => "sub"
       | ANN  => "ann"
end

structure SimpleOperator : ABT_SIMPLE_OPERATOR =
struct
  open OperatorData
  structure Ar = ListAbtArity (Sort)

  (* to make a valence *)
  fun mkVal q s = (([], q), s)

  val arity =
    fn LAM  => ([mkVal [SortData.EXP] SortData.EXP], SortData.EXP)
     | AP   => ([mkVal [] SortData.EXP, mkVal [] SortData.EXP], SortData.EXP)
     | BASE => ([], SortData.TYP)
     | ARR  => ([mkVal [] SortData.TYP, mkVal [] SortData.TYP], SortData.TYP)
     | REF  => ([mkVal [] SortData.TYP, mkVal [] SortData.TYP], SortData.TYP)
     | SUB  => ([mkVal [] SortData.TYP, mkVal [] SortData.TYP], SortData.TYP)
     | ANN  => ([mkVal [] SortData.EXP, mkVal [] SortData.TYP], SortData.EXP)
end

structure Operator = AbtSimpleOperator (SimpleOperator)
