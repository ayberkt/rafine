structure Repl =
struct

  local
    structure Typing = Typing (Syntax)
    fun printLn s = print (s ^ "\n")
    fun printTypeError msg = printLn ("Type error: " ^ msg)
    fun @@ (f, x) = f x
    infixr 0 @@
  in
    fun loop f =
      let val dummyEOF = ExprLrVals.Tokens.EOF(0, 0)
          val input = valOf ( TextIO.output(TextIO.stdOut, "> ")
                            ; TextIO.flushOut(TextIO.stdOut)
                            ; TextIO.inputLine TextIO.stdIn)
          val result = (f input, SortData.EXP)
          val resultAbt = AstToAbt.convert Abt.Metavar.Ctx.empty result
          val wellTyped =
            case Syntax.outExp resultAbt of
              Syntax.ANN (e, t) =>
                let
                  val gamma = Abt.Var.Ctx.empty
                  val pi = Abt.Var.Ctx.empty
                in
                  Typing.check (gamma, pi) e t
                  handle
                    Typing.TypeError msg => (printTypeError msg; false)
                  | _ => (printTypeError "Something went wrong!"; false)
                end
            | _ => (raise Fail "no annotation!"; false)
      in
        printLn (ShowAbt.toString resultAbt);
        printLn (if wellTyped then "Good!" else "Bad!");
        loop f
      end
    end


  fun parseLoop () = loop Parser.parse

  fun main _ = (parseLoop (); 1)

end

val _ = Repl.main ()
