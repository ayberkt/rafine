functor OperatorData (P : REFINEMENT_SET) =
struct

    structure RefSet = P
    datatype t = LAM | AP | ARR | BASE
               | ATOM of RefSet.t | RFARR | RFANN | ANN

    structure RefSet : REFINEMENT_SET = P

    fun eq (LAM, LAM)        = true
      | eq (AP, AP)          = true
      | eq (ARR, ARR)        = true
      | eq (RFARR, RFARR)    = true
      | eq (RFANN, RFANN)    = true
      | eq (BASE, BASE)      = true
      | eq (ATOM i, ATOM i') = P.eq (i, i')
      | eq _                 = false

    val toString =
      fn LAM    => "lam"
       | AP     => "ap"
       | BASE   => "base"
       | ATOM i => "atom[" ^ P.toString i ^ "]"
       | ARR    => "arr"
       | RFARR  => "rfarr"
       | RFANN  => "rfann"
       | ANN    => "ann"
end

structure OperatorData = OperatorData (Organisms)

structure SimpleOperator : ABT_SIMPLE_OPERATOR =
struct
  open OperatorData
  structure Ar = ListAbtArity (Sort)

  (* to make a valence *)
  fun mkVal q s = (([], q), s)

  structure SD = SortData

  val arity =
    fn LAM    => ([mkVal [SD.EXP] SD.EXP], SD.EXP)
     | AP     => ([mkVal [] SD.EXP, mkVal [] SD.EXP], SD.EXP)
     | BASE   => ([], SD.TYP)
     | ATOM i => ([], SD.RFN)
     | ARR    => ([mkVal [] SD.TYP, mkVal [] SD.TYP], SD.TYP)
     | RFARR  => ([mkVal [] SD.RFN, mkVal [] SD.RFN], SD.RFN)
     | RFANN  => ([mkVal [] SD.EXP, mkVal [] SD.RFN], SD.EXP)
     | ANN    => ([mkVal [] SD.EXP, mkVal [] SD.TYP], SD.EXP)
end

structure Operator = AbtSimpleOperator (SimpleOperator)
