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
          val result = SOME (f input, SortData.EXP)
                        handle
                          Parser.ParseError (s, pos, pos') =>
                            (printLn @@ "Cannot parse input: " ^ s; NONE)
                        | Fail s =>
                            (printLn @@ "Error: " ^ s; NONE)
                        | _ =>
                            (printLn "Something went seriously wrong!"; NONE)

          val resultAbt =
            case result of
              SOME result' =>
                SOME (AstToAbt.convert Abt.Metavar.Ctx.empty result')
            | NONE => NONE
          (*val wellTyped =*)
            (*case Syntax.outExp resultAbt of*)
              (*Syntax.ANN (e, t) =>*)
                (*let*)
                  (*val gamma = Abt.Var.Ctx.empty*)
                  (*val pi = Abt.Var.Ctx.empty*)
                (*in*)
                  (*Typing.check (gamma, pi) e t*)
                  (*handle*)
                    (*Typing.TypeError msg => (printTypeError msg; false)*)
                  (*| Typing.Unbound msg => (printLn @@ "Error: " ^ msg; false)*)
                  (*| _ => (printTypeError "Something went wrong!"; false)*)
                (*end*)
            (*| _ => (raise Fail "no annotation!"; false)*)
      in (
        case resultAbt of
          SOME abt =>  printLn (ShowAbt.toString abt)
        | NONE => ();
        (*printLn (if wellTyped then "Good!" else "Bad!");*)
        loop f
      )
      end
    end


  fun parseLoop () = loop Parser.parse

  fun main _ = (parseLoop (); 1)

end

val _ = Repl.main ()
