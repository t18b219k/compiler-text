structure Top = struct
  fun readAndPrintLoop declarations gamma stream =
    let
      val (dec, stream) = Parser.doParse stream
      val (newGamma, typed_expr) = Typeinf.typeinf_with_typed_expr gamma dec
      val Syntax.VAL (name, _) = dec
      val new_declarations = TypedSyntax.VAL (name, typed_expr) :: declarations
      val wasmCode = WasmComp.compile true new_declarations
      val compile_and_execute = _import "build_and_execute":string->()
      val exec = compile_and_execute (WasmModule.moduleToString wasmCode)
    in
      readAndPrintLoop new_declarations newGamma stream
    end

  fun top file =
    let
      val inStream =
        case file of
          "" => TextIO.stdIn
        | _ => TextIO.openIn file
      val stream = Parser.makeStream inStream
      val gamma = TypeUtils.emptyTyEnv
      val declarations = []
      val start_logger = _import "start_logger": ()->()
      val _ = start_logger ()
    in
      readAndPrintLoop declarations gamma stream handle
        Parser.EOF => ()
      | Parser.ParseError => print "Syntax error\n"
      | Typeinf.TypeError => print "Type error\n"
      | Typeinf.UndefinedVariable id =>print ("E001 Error identifier:"^id^" is not defined\n")
      | UnifyTy.CyclicType =>print "E002 given declaration contains infinite size type\n"
      | Typeinf.NotFunction{expected,actual,expression}=>print ("E003 expected "^ Syntax.expToString expression ^" to be "^ Type.tyToString expected ^" but "^Type.tyToString actual ^" inferred.\n")
      | Typeinf.ExpectedToBePair{expected,actual,expression}=>print ("E004 expected "^ Syntax.expToString expression ^" to be "^ Type.tyToString expected ^" but "^Type.tyToString actual ^" inferred.\n")
      | Typeinf.ExpectedToBeBool{actual,expression}=>print ("E005 expected "^ Syntax.expToString expression ^" to be bool but "^Type.tyToString actual ^" inferred.\n")
      | Typeinf.IfArmsNotMatch{then_ty,then_exp,else_ty,else_exp}=>print ("E006 expression :"^ Syntax.expToString then_exp ^" and "^Syntax.expToString else_exp  ^" must have same type. but, \n"^
      Syntax.expToString then_exp ^":"^Type.tyToString then_ty^"\n"^
      Syntax.expToString else_exp ^":"^Type.tyToString else_ty^"\n"
      )
      | Typeinf.ExpectedToBeInt{actual,expression}=>print("E007 expression :"^Syntax.expToString expression^" must be int but, "^Type.tyToString actual ^" inffered")
      | Exec.RuntimeError => print "Runtime error\n"
      | WasmComp.CantMapToWasmValType ty => WasmComp.print_CantMapToWasmValType ty;
      case file of
        "" => ()
      | _ => TextIO.closeIn inStream
    end
end