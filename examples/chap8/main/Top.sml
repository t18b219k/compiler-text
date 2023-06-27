structure Top =
struct
  fun readAndPrintLoop gamma stream =
    let
      val (dec, stream) = Parser.doParse stream
      val (newGamma,typed_expr)=Typeinf.typeinf_with_typed_expr gamma dec 
      val Syntax.VAL(name,_)=dec
      val wasmCode = WasmComp.compile [TypedSyntax.VAL(name,typed_expr)]
    in
      readAndPrintLoop  newGamma stream
    end
  fun top file =
    let
      val inStream = case file of 
                       "" => TextIO.stdIn
                     | _ => TextIO.openIn file
      val stream = Parser.makeStream inStream
      val gamma = TypeUtils.emptyTyEnv
      val module = WasmModule.emptyModule 
    in
      readAndPrintLoop  gamma stream 
      handle Parser.EOF => ()
           | Parser.ParseError => print "Syntax error\n"
           | Typeinf.TypeError => print "Type error\n"
           | Exec.RuntimeError => print "Runtime error\n";
      case file of "" => () 
                 | _ => TextIO.closeIn inStream
    end
end
