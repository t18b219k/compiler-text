    open Dynamic
    exception Unreachable
    val _ = let 
      val start_logger = _import "start_logger": ()->()
      val _ = start_logger ()
      val start_server = _import "start_server": (unit ptr->string)->()
    in 
        start_server(fn source => 
        
        let 
            val string = Pointer.importString(SMLSharp_Builtin.Pointer.fromUnitPtr source)
            val stream = Parser.makeStringStream string
            val declarations = ref []
            val gamma = TypeUtils.emptyTyEnv 
            fun readAndParseLoop gamma stream =(let
                    val (dec, stream) = Parser.doParse stream
                    val _ = Dynamic.pp stream 
                    val (newGamma, typed_expr) = Typeinf.typeinf_with_typed_expr gamma dec
                    val Syntax.VAL (name, _) = dec
                in 
                   declarations := TypedSyntax.VAL(name,typed_expr):: !declarations;
                   readAndParseLoop gamma stream
                end
                ) 
            val _ = readAndParseLoop gamma stream  handle 
        Parser.EOF =>( print "EOF\n")
      | Parser.ParseError =>( print "Syntax error\n")
      | Typeinf.TypeError => (print "Type error\n")
      | Typeinf.UndefinedVariable id =>(print ("E001 Error identifier:"^id^" is not defined\n"))
      | UnifyTy.CyclicType =>(print "E002 given declaration contains infinite size type\n")
      | Typeinf.NotFunction{expected,actual,expression}=>(print ("E003 expected "^ Syntax.expToString expression ^" to be "^ Type.tyToString expected ^" but "^Type.tyToString actual ^" inferred.\n"))
      | Typeinf.ExpectedToBePair{expected,actual,expression}=>(print ("E004 expected "^ Syntax.expToString expression ^" to be "^ Type.tyToString expected ^" but "^Type.tyToString actual ^" inferred.\n"))
      | Typeinf.ExpectedToBeBool{actual,expression}=>(print ("E005 expected "^ Syntax.expToString expression ^" to be bool but "^Type.tyToString actual ^" inferred.\n"))
      | Typeinf.IfArmsNotMatch{then_ty,then_exp,else_ty,else_exp}=>(print ("E006 expression :"^ Syntax.expToString then_exp ^" and "^Syntax.expToString else_exp  ^" must have same type. but, \n"^
      Syntax.expToString then_exp ^":"^Type.tyToString then_ty^"\n"^
      Syntax.expToString else_exp ^":"^Type.tyToString else_ty^"\n"
      ))
      | Typeinf.ExpectedToBeInt{actual,expression}=>(print("E007 expression :"^Syntax.expToString expression^" must be int but, "^Type.tyToString actual ^" inffered"))
      | WasmComp.CantMapToWasmValType ty => (WasmComp.print_CantMapToWasmValType ty)
      val _ = Dynamic.pp declarations
      val wasmCode = WasmComp.compile true (!declarations)
    in 
            WasmModule.moduleToString wasmCode
    end)
    end 
       