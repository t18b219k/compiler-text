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
      | Exec.RuntimeError => print "Runtime error\n"
      | WasmComp.CantMapToWasmValType ty => WasmComp.print_CantMapToWasmValType ty;
      case file of
        "" => ()
      | _ => TextIO.closeIn inStream
    end
end