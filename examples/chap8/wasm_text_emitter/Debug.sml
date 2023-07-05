
structure Debug =
struct 
    exception Unreachable
    (*
    sig: generate_text_print:WasmModule.module*string*word32 ->WasmModule.module * WasmModule.expr * word32
    *)
    fun generate_text_print(module:WasmModule.module,text:string,memoffset)=
    (
             { ty = #ty module, fn_ = #fn_ module, ta = #ta module, me = #me module, gl = #gl module, el = #el module, da = #da module , im = #im module, ex = #ex module, st = #st module },[(WasmModule.i32const memoffset),(WasmModule.call (IDX.funcidx( IDX.text_id "print_string")))],memoffset+(String.size text)) 
    (*
    sig:val generate_stack_debug: string * Type.ty*WasmModule.module*word32-> WasmModule.module*WasmModule.expr*word32
    スタックから値を取り,それを表示する命令列を生成する.
    *)
    fun generate_stack_debug(id,ty,module,memoffset)= case ty of 
    Type.INTty=>(module,[WasmModule.call (IDX.funcidx (IDX.text_id "print_int"))],memoffset)
    |Type.BOOLty => (module,[WasmModule.call (IDX.funcidx (IDX.text_id "print_bool"))],memoffset)
    |Type.STRINGty=>(module,[WasmModule.call (IDX.funcidx (IDX.text_id "print_string"))],memoffset)
    |Type.PAIRty(ty1,ty2)=>let

     val (module,print_v1,memoffset)=(generate_stack_debug (id,ty1,module,memoffset))

     val (module,print_v2,memoffset)= generate_stack_debug(id,ty2,module,memoffset)
    in 
    (module,
    [   WasmModule.call (IDX.funcidx (IDX.text_id "print_lparen")),
        WasmModule.i32load(WasmModule.memarg(0w0,0w4))]
    @print_v1
    @[  WasmModule.localget(IDX.localidx(IDX.text_id id)),
        WasmModule.i32load(WasmModule.memarg(0w4,0w4)),
        WasmModule.call(IDX.funcidx (IDX.text_id "print_comma"))]
    @print_v2
    @[WasmModule.call (IDX.funcidx (IDX.text_id "print_rparen"))],memoffset)
    end 
    |_=>raise Unreachable
    (*
    sig:val insert_debug_instructions: string * Type.ty*module*word32 -> WasmModule.module*WasmModule.expr*word32
    *)
    fun insert_debug_instructions (id,ty,module,memoffset) = case ty of 
    Type.POLYty(tids,ty)=>(module,[],memoffset)
    |Type.INTty=>let val (module,iseq,memoffset)= generate_stack_debug (id,ty,module,memoffset)
    in 
        (module,WasmModule.localget(IDX.localidx(IDX.text_id id))::iseq,memoffset)
    end 
    (*print_boolは実行環境から与えられ,0=false ,1=true とコンソールに表示する関数である.*)
    |Type.BOOLty=>let val (module,iseq,memoffset)= generate_stack_debug (id,ty,module,memoffset)
    in 
        (module,WasmModule.localget(IDX.localidx(IDX.text_id id))::iseq,memoffset)
    end 
    (*print_stringは実行環境から与えられ,linear memory のポインタが与えられた時ポインタがさす先のデータをUTF-8の文字列として表示する関数である.*)
    |Type.STRINGty=>let val (module,iseq,memoffset)= generate_stack_debug (id,ty,module,memoffset)
    in 
        (module,WasmModule.localget(IDX.localidx(IDX.text_id id))::iseq,memoffset)
    end 
    (*PAIR の要素はptr かi32しかないので*)
    |Type.PAIRty(_,_)=>let val (module,iseq,memoffset)= generate_stack_debug (id,ty,module,memoffset)
    in 
        (module,WasmModule.localget(IDX.localidx(IDX.text_id id))::iseq,memoffset)
    end 
    |Type.FUNty(arg_ty,ret_ry)=>(module,[],memoffset)
    |Type.TYVARty _ => (module,[],memoffset)




end