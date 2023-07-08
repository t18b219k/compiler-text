structure Debug = struct
    exception Unreachable

    (*
    sig: generate_text_print:WasmModule.module*string*word32 ->WasmModule.module * WasmModule.expr * word32
    *)
    fun generate_text_print (module : WasmModule.module, text : string, memoffset) =
        let
            val data = WasmModule.data (NONE, WasmModule.active_data_mode (IDX.memidx (IDX.text_id "linear_memory"), [WasmModule.i32const memoffset]), text)
        in
            ({ ty = #ty module, fn_ = #fn_ module, ta = #ta module, me = #me module, gl = #gl module, el = #el module, da = data::(#da module), im = #im module, ex = #ex module, st = #st module }, [(WasmModule.i32const memoffset), (WasmModule.i32const (size text)), (WasmModule.call (IDX.funcidx (IDX.text_id "print_string")))], memoffset + (String.size text))
        end

    (*
    sig:val generate_stack_debug: string * Type.ty*WasmModule.module*word32-> WasmModule.module*WasmModule.expr*word32
    スタックから値を取り,それを表示する命令列を生成する.
    *)
    val type_list = [WasmModule.type_definition (SOME "print_int", WasmModule.functype ([WasmModule.param ("value", WasmModule.numtype WasmModule.i32)], [])), WasmModule.type_definition (SOME "print_bool", WasmModule.functype ([WasmModule.param ("value", WasmModule.numtype WasmModule.i32)], [])), WasmModule.type_definition (SOME "print_string", WasmModule.functype ([WasmModule.param ("ptr", WasmModule.numtype WasmModule.i32),WasmModule.param ("size", WasmModule.numtype WasmModule.i32)], [])), WasmModule.type_definition (SOME "print_token", WasmModule.functype ([], []))]

    val import_list = [WasmModule.import ("env", "print_int", WasmModule.f (SOME (IDX.funcidx (IDX.text_id "print_int")), WasmModule.name_only (IDX.typeidx (IDX.text_id "print_int")))), WasmModule.import ("env", "print_bool", WasmModule.f (SOME (IDX.funcidx (IDX.text_id "print_bool")), WasmModule.name_only (IDX.typeidx (IDX.text_id "print_bool")))),
    WasmModule.import ("env", "print_string", WasmModule.f (SOME (IDX.funcidx (IDX.text_id "print_string")), WasmModule.name_only (IDX.typeidx (IDX.text_id "print_string")))),
    WasmModule.import ("env", "print_lparen", WasmModule.f (SOME (IDX.funcidx (IDX.text_id "print_lparen")), WasmModule.name_only (IDX.typeidx (IDX.text_id "print_token")))),
    WasmModule.import ("env", "print_rparen", WasmModule.f (SOME (IDX.funcidx (IDX.text_id "print_rparen")), WasmModule.name_only (IDX.typeidx (IDX.text_id "print_token")))), 
    WasmModule.import ("env", "print_val", WasmModule.f (SOME (IDX.funcidx (IDX.text_id "print_val")), WasmModule.name_only (IDX.typeidx (IDX.text_id "print_token")))), 
    WasmModule.import ("env", "print_comma", WasmModule.f (SOME (IDX.funcidx (IDX.text_id "print_comma")), WasmModule.name_only (IDX.typeidx (IDX.text_id "print_token")))), WasmModule.import ("env", "print_equal", WasmModule.f (SOME (IDX.funcidx (IDX.text_id "print_equal")), WasmModule.name_only (IDX.typeidx (IDX.text_id "print_token")))), WasmModule.import ("env", "print_colon", WasmModule.f (SOME (IDX.funcidx (IDX.text_id "print_colon")), WasmModule.name_only (IDX.typeidx (IDX.text_id "print_token")))),
    WasmModule.import ("env", "print_arrow", WasmModule.f (SOME (IDX.funcidx (IDX.text_id "print_arrow")), WasmModule.name_only (IDX.typeidx (IDX.text_id "print_token")))),
    WasmModule.import ("env", "print_space",WasmModule.f (SOME (IDX.funcidx (IDX.text_id "print_space")), WasmModule.name_only (IDX.typeidx (IDX.text_id "print_token")))),
    WasmModule.import ("env", "print_double_quote",WasmModule.f (SOME (IDX.funcidx (IDX.text_id "print_double_quote")), WasmModule.name_only (IDX.typeidx (IDX.text_id "print_token")))),
    WasmModule.import ("env", "print_new_line",WasmModule.f (SOME (IDX.funcidx (IDX.text_id "print_new_line")), WasmModule.name_only (IDX.typeidx (IDX.text_id "print_token"))))
    
    ]

    fun install_system_functions module : WasmModule.module = { ty = (#ty module) @ type_list, im = (#im module) @ import_list, fn_ = #fn_ module, ta = #ta module, me = #me module, gl = #gl module, ex = #ex module, st = #st module, el = #el module, da = #da module }

    (*
    We need to give this to interpreter.
    and declare memory before execute
    const memory = new WebAssembly.Memory({
        initial: 2,
        maximum: 100
      });

    {
    env: {
    linear_memory:memory
    print_int: function(i) {
        console.log(i);
    },
    print_bool: function(i) {
   		if (i == 0){
            console.log("false");
        }else{
            console.log("true");
        }
      },
    print_string: function(ptr) {
        console.log(ptr);
    },
    print_lparen: function() {
        console.log("(");
    },
    print_rparen: function() {
        console.log(")");
    },
    print_val: function() {
        console.log("val");
    },
    print_comma: function() {
        console.log(",");
    },
    print_equal: function() {
        console.log("=");
    },
    print_colon: function() {
        console.log(":");
    },
    print_arrow: function() {
        console.log("->");
    },
    print_space: function() {
        console.log(" ");
    },
    print_double_quote: function () {
        console.log("\"");
    },
    print_new_line: function () {
        console.log("\n");
    }
    }
    }
    *)
    val call_print_int = WasmModule.call (IDX.funcidx (IDX.text_id "print_int"))

    val call_print_bool = WasmModule.call (IDX.funcidx (IDX.text_id "print_bool"))

    val call_print_string = WasmModule.call (IDX.funcidx (IDX.text_id "print_string"))

    val call_print_lparen = WasmModule.call (IDX.funcidx (IDX.text_id "print_lparen"))

    val call_print_rparen = WasmModule.call (IDX.funcidx (IDX.text_id "print_rparen"))

    val call_print_val = WasmModule.call (IDX.funcidx (IDX.text_id "print_val"))

    val call_print_comma = WasmModule.call (IDX.funcidx (IDX.text_id "print_comma"))

    val call_print_equal = WasmModule.call (IDX.funcidx (IDX.text_id "print_equal"))

    val call_print_colon = WasmModule.call (IDX.funcidx (IDX.text_id "print_colon"))

    val call_print_arrow = WasmModule.call (IDX.funcidx (IDX.text_id "print_arrow"))

    val call_print_space = WasmModule.call (IDX.funcidx (IDX.text_id "print_space"))

    val call_print_double_quote = WasmModule.call (IDX.funcidx (IDX.text_id "print_double_quote"))

    val call_print_new_line = WasmModule.call (IDX.funcidx (IDX.text_id "print_new_line"))
    (*スタック上のデータをプリントする.*)
    fun generate_stack_debug (id, ty, module, memoffset) =
        case ty of
            Type.INTty => (module, [call_print_int], memoffset)
        | Type.BOOLty => (module, [call_print_bool], memoffset)
        | Type.STRINGty => (module, [
            WasmModule.localtee(IDX.localidx(IDX.text_id "str_ptr")),
            WasmModule.i32load (WasmModule.memarg(0w0,0w4)),(*fetch ptr (str)->ptr*)
            WasmModule.localget(IDX.localidx(IDX.text_id "str_ptr" )),(*fetch &str*)
            WasmModule.i32load(WasmModule.memarg(0w4,0w4)),(*fetch size (str)->size*)
            call_print_double_quote,
            call_print_string,
            call_print_double_quote], memoffset)
        | Type.PAIRty (ty1, ty2) =>
                let
                    val (module, print_v1, memoffset) = generate_stack_debug (id, ty1, module, memoffset)
                    val (module, print_v2, memoffset) = generate_stack_debug (id, ty2, module, memoffset)
                    (*ペアの表示関数名.これをキーとして使う.*)
                    val pair_print_sig = "pp_"^Type.tyToString ty1^"_"^Type.tyToString ty2

                in
                    (module, [
                        call_print_lparen,
                        WasmModule.i32load (WasmModule.memarg (0w0, 0w4))]
                        @
                        print_v1
                        @
                        [call_print_comma,
                        WasmModule.localget (IDX.localidx (IDX.text_id id)),(*これだとスタック上の値を引っ張れない. 何らかの関数を埋め込まないとネストしたペアで問題が起きる?*)
                        WasmModule.i32load (WasmModule.memarg (0w4, 0w4))]
                        @ 
                        print_v2 
                        @ 
                        [call_print_rparen], memoffset)
                end
        | _ => raise Unreachable

    fun generate_identifier_print (id, module, memoffset) = generate_text_print (module, id, memoffset)

    (*
    sig:val insert_debug_instructions: string * Type.ty*module*word32 -> WasmModule.module*WasmModule.expr*word32
    *)
    fun insert_debug_instructions (id, ty, module, memoffset) =
        case ty of
        Type.INTty =>
                let
                    val (module, call_print_id, memoffset) = generate_identifier_print (id, module, memoffset)
                    val (module, iseq, memoffset) = generate_stack_debug (id, ty, module, memoffset)
                    val (module, print_type, memoffset) = generate_text_print(module,Type.tyToString ty, memoffset)
                in
                    (module, [call_print_val, call_print_space] @ call_print_id @ [call_print_equal , WasmModule.localget (IDX.localidx (IDX.text_id id)) ]@ iseq@call_print_colon::print_type@[call_print_new_line], memoffset)
                end
        | Type.BOOLty =>
                let
                    (*print_boolは実行環境から与えられ,0=false ,1=true とコンソールに表示する関数である.*)
                    val (module, call_print_id, memoffset) = generate_identifier_print (id, module, memoffset)
                    val (module, iseq, memoffset) = generate_stack_debug (id, ty, module, memoffset)
                    val (module, print_type, memoffset) = generate_text_print(module,Type.tyToString ty, memoffset)
                in
                    (module, [call_print_val, call_print_space] @ call_print_id @ [call_print_equal , WasmModule.localget (IDX.localidx (IDX.text_id id))]@ iseq@call_print_colon::print_type@[call_print_new_line], memoffset)
                end
        | Type.STRINGty =>
                let
                    (*print_stringは実行環境から与えられ,linear memory のポインタが与えられた時ポインタがさす先のデータをUTF-8の文字列として表示する関数である.*)
                    val (module, call_print_id, memoffset) = generate_identifier_print (id, module, memoffset)
                    val (module, iseq, memoffset) = generate_stack_debug (id, ty, module, memoffset)                    
                    val (module, print_type, memoffset) = generate_text_print(module,Type.tyToString ty, memoffset)
                in
                    (module, [call_print_val, call_print_space] @ call_print_id @ [call_print_equal, WasmModule.localget (IDX.localidx (IDX.text_id id))] @ iseq@call_print_colon::print_type@[call_print_new_line], memoffset)
                end
        | Type.PAIRty (_, _) =>
                let
                    (*PAIR の要素はptr かi32しかないので*)
                    val (module, call_print_id, memoffset) = generate_identifier_print (id, module, memoffset)
                    val (module, iseq, memoffset) = generate_stack_debug (id, ty, module, memoffset)
                    val (module, print_type, memoffset) = generate_text_print(module,Type.tyToString ty, memoffset)
                in
                    (module, [call_print_val, call_print_space] @ call_print_id @ [call_print_equal , WasmModule.localget (IDX.localidx (IDX.text_id id)) ]@ iseq@call_print_colon::print_type@[call_print_new_line], memoffset)
                end
        | Type.FUNty (arg_ty, ret_ry) =>
                let
                    val (module, call_print_id, memoffset) = generate_identifier_print (id, module, memoffset)
                    val (module, print_type, memoffset) = generate_text_print(module,Type.tyToString ty, memoffset)
                in
                    (module, [call_print_val, call_print_space] @ call_print_id @ call_print_colon:: print_type@[call_print_new_line], memoffset)
                end
        |_ => raise Unreachable (*スタックにあるものはペア,関数,定数であるからここには到達しない*)
end