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
            val declarations = []
            val gamma = TypeUtils.emptyTyEnv 
            fun readAndParseLoop gamma stream  declarations = let
                    val (dec, stream) = Parser.doParse stream 
                    val _ = Dynamic.pp stream 
                    val (newGamma, typed_expr) = Typeinf.typeinf_with_typed_expr gamma dec
                    val Syntax.VAL (name, _) = dec
                    val new_declarations = TypedSyntax.VAL(name,typed_expr)::declarations
                in 
                   readAndParseLoop newGamma stream new_declarations handle 
        Parser.EOF =>( print "EOF\n";new_declarations)
      | Parser.ParseError =>( print "Syntax error\n";new_declarations)
      | Typeinf.TypeError => (print "Type error\n";new_declarations)
      | Typeinf.UndefinedVariable id =>(print ("E001 Error identifier:"^id^" is not defined\n");new_declarations)
      | UnifyTy.CyclicType =>(print "E002 given declaration contains infinite size type\n";new_declarations)
      | Typeinf.NotFunction{expected,actual,expression}=>(print ("E003 expected "^ Syntax.expToString expression ^" to be "^ Type.tyToString expected ^" but "^Type.tyToString actual ^" inferred.\n");new_declarations)
      | Typeinf.ExpectedToBePair{expected,actual,expression}=>(print ("E004 expected "^ Syntax.expToString expression ^" to be "^ Type.tyToString expected ^" but "^Type.tyToString actual ^" inferred.\n");new_declarations)
      | Typeinf.ExpectedToBeBool{actual,expression}=>(print ("E005 expected "^ Syntax.expToString expression ^" to be bool but "^Type.tyToString actual ^" inferred.\n");new_declarations)
      | Typeinf.IfArmsNotMatch{then_ty,then_exp,else_ty,else_exp}=>(print ("E006 expression :"^ Syntax.expToString then_exp ^" and "^Syntax.expToString else_exp  ^" must have same type. but, \n"^
      Syntax.expToString then_exp ^":"^Type.tyToString then_ty^"\n"^
      Syntax.expToString else_exp ^":"^Type.tyToString else_ty^"\n"
      );new_declarations)
      | Typeinf.ExpectedToBeInt{actual,expression}=>(print("E007 expression :"^Syntax.expToString expression^" must be int but, "^Type.tyToString actual ^" inffered");new_declarations)
      | WasmComp.CantMapToWasmValType ty => (WasmComp.print_CantMapToWasmValType ty;new_declarations) 
                end 
      val declarations = readAndParseLoop gamma stream  declarations
      val _ = Dynamic.pp declarations
      val wasmCode = WasmComp.compile true (declarations)
    in 
            WasmModule.moduleToString wasmCode
    end)
    end 
       