structure WasmComp =
struct

  (*
  型付き抽象構文木からWASMに落とす
  *)
  open Type
  structure TS = TypedSyntax
  structure S = Syntax
  structure I = WasmModule
  exception Unreachable
  (*
  多相関数は単相化してからwasmに落とすため、ここではPOLYty は考慮しない
  関数ポインタを置くテーブルの名前を生成
  *)
  exception CantMapToWasmValType of Type.ty
  (*CoreMLの型からWASMのvaltype に変換*)
  fun CMLTyTovaltype ty =
    I.numtype
      (case ty of
         INTty => I.i32
       | BOOLty => I.i32
       | STRINGty => I.i32
       | PAIRty (_, _) => I.i32
       | FUNty (_, _) => I.i32
       | TYVARty _ => I.i32
       | POLYty (_, _) =>
           I.i32 (*多相関数にしかならないのでポインタにしかならない*))
  fun size_of_numtype t =
    case t of
      I.i32 => 4
    | I.i64 => 8
    | I.f32 => 4
    | I.f64 => 8
  fun size_of_valtype t =
    case t of
      I.numtype nt => (size_of_numtype nt)
    | I.reftype _ => 4
    | I.vectype _ => 16

  (*CoreMLの型からWASMのblocktype に変換*)
  fun CMLTyToblocktype ty =
    I.blocktype (SOME (I.result (CMLTyTovaltype ty)))
  val internal_local_vars =
    [ (I.local_ (SOME (IDX.localidx (IDX.text_id "str_ptr")), I.numtype I.i32))
    , (I.local_
         (SOME (IDX.localidx (IDX.text_id "closure_ptr")), I.numtype I.i32))
    ]
  (*型付き抽象構文木の自由変数を求める*)

  fun fv e =
    case e of
      TS.EXPID (x, ty) => SEnv.singleton (x, ty)
    | TS.EXPFN (x, e, _) =>
        let
          val m = fv e
          val m = #1 (SEnv.remove (m, x)) handle _ => m
        in
          m
        end
    | TS.EXPFIX (f, x, e, _) =>
        let
          val m = fv e
          val m = #1 (SEnv.remove (m, x)) handle _ => m
          val m = #1 (SEnv.remove (m, f)) handle _ => m
        in
          m
        end
    | TS.EXPAPP (e1, e2, _) => SEnv.unionWith (fn (a, _) => a) (fv e1, fv e2)
    | TS.EXPIF (e1, e2, e3) =>
        SEnv.unionWith (fn (a, _) => a)
          (fv e1, SEnv.unionWith (fn (a, b) => a) (fv e2, fv e3))
    | TS.EXPPAIR (e1, e2) => SEnv.unionWith (fn (a, _) => a) (fv e1, fv e2)
    | TS.EXPPRIM (_, e1, e2) => SEnv.unionWith (fn (a, _) => a) (fv e1, fv e2)
    | TS.EXPPROJ1 e => fv e
    | TS.EXPPROJ2 e => fv e
    | _ => SEnv.empty
  (*クロージャの表現から関数に落とす*)
  (*type func =  funcidx option * typeuse * local_ list * instruction list *)
  fun generate_local_list e =
    let val fvs = fv e
    in SEnv.foldr (op::) [] (SEnv.map CMLTyToblocktype fvs)
    end
  exception CantCalicurateSizeOfCMLType of Type.ty
  fun size_of ty =
    case ty of
      INTty => 4
    | BOOLty => 4
    | STRINGty => 4
    | PAIRty (a, b) => size_of a + size_of b
    | FUNty (_, _) => 4
    | TYVARty _ => 4
    | POLYty (_, _) => 4

  val main_export = WasmModule.export ("__cml_main", WasmModule.func_e
    (IDX.funcidx (IDX.text_id "__cml_main")))

  (*
   pc_table ペア生成関数名から関数本体とシグネチャの集合.
   vt1: e1 の型のwasm での表現
   vt2: e2 の型のwasm での表現
   returns: pc_table,
     計算の順序, この計算が終わった後にはスタックにはペアのポインタのみが残る.
     ネストしたペアについて考えなければならない.
     ネストしたペアではpair_addr は適切に保存されなければならない.
     e1 ,e2 はスタックにただ一つ表すその計算結果を表す値を積む.
     pair_addr は計算の前後で一致させなければネストしたペアで問題が起こる.
     そこでペア生成は関数とした
     0 top ... bottom
     1 ヒープを割り当てる. top pair_addr,... bottom
     2 要素1を計算.        top v1 ,pair_addr,... bottom
     3 ヒープに要素1を保存 top　... bottom 
     4 ペアのアドレスをスタックに積む top pair_addr,... bottom
     5 要素2を計算         top v2,pair_addr,... bottom 
     6 ヒープに要素2を保存 top ,... bottom      
     7 ペアのアドレスをスタックに積む　top pair_addr,... bottom
  *)
  fun generate_pair_construction_call (pc_table, t1, t2) =
    let

      fun need_to_gc t =
        case t of
          INTty => false
        | BOOLty => false
        | STRINGty => false
        | FUNty (a, b) =>
            true (*キャプチャした環境をGC対象にする*)
        | POLYty (_, _) => true
        | TYVARty _ => false
        | PAIRty (_, _) => true
      val vt1 = CMLTyTovaltype t1
      val vt2 = CMLTyTovaltype t2
      val gc_e1 = need_to_gc t1
      val gc_e2 = need_to_gc t2
      (*関数名　これによって過剰に関数が生成されるのを抑制する.*)
      val pc_fn_name =
        "pair_con" ^ (if gc_e1 then "_gc_" else "_nogc_")
        ^ I.valtypeToString vt1 ^ (if gc_e2 then "_gc_" else "_nogc_")
        ^ I.valtypeToString vt2
    in
      case SEnv.find (pc_table, pc_fn_name) of
        SOME _ => (pc_table, I.call (IDX.funcidx (IDX.text_id pc_fn_name)))
      | NONE =>
          let

            val (sti1, offset) =
              case vt1 of
                I.numtype I.i32 => (I.i32store (I.memarg (0w0, 0w4)), 0w4)
              | I.numtype I.i64 => (I.i64store (I.memarg (0w0, 0w4)), 0w8)
              | I.numtype I.f32 => (I.f32store (I.memarg (0w0, 0w4)), 0w4)
              | I.numtype I.f64 => (I.f64store (I.memarg (0w0, 0w4)), 0w8)
              | _ => raise Unreachable
            val sti2 =
              case vt2 of
                I.numtype I.i32 => I.i32store (I.memarg (offset, 0w4))
              | I.numtype I.i64 => I.i64store (I.memarg (offset, 0w4))
              | I.numtype I.f32 => I.f32store (I.memarg (offset, 0w4))
              | I.numtype I.f64 => I.f64store (I.memarg (offset, 0w4))
              | _ => raise Unreachable
            val iseq =
              [ I.i32const (size_of_valtype vt1 + size_of_valtype vt2)
              , I.call (IDX.funcidx (IDX.text_id "alloc"))
              , I.localtee (IDX.localidx (IDX.text_id "pair_addr"))
              , I.localget (IDX.localidx (IDX.int_id 0w0))
              , sti1
              , I.localget (IDX.localidx (IDX.text_id "pair_addr"))
              , I.localget (IDX.localidx (IDX.int_id 0w1))
              , sti2
              , I.localget (IDX.localidx (IDX.text_id "pair_addr"))
              ]

            val pc_ty_def = I.type_definition (SOME pc_fn_name, I.functype
              ( [I.param ("e1", vt1), I.param ("e2", vt2)]
              , [I.result (I.numtype I.i32)]
              ))
            val pc_table = SEnv.insert
              ( pc_table
              , pc_fn_name
              , ( ( SOME (IDX.funcidx (IDX.text_id pc_fn_name))
                  , I.name_only (IDX.typeidx (IDX.text_id pc_fn_name))
                  , [(I.local_
                        ( SOME (IDX.localidx (IDX.text_id "pair_addr"))
                        , (I.numtype I.i32)
                        ))]
                  , iseq
                  )
                , pc_ty_def
                )
              )
          in
            (pc_table, I.call (IDX.funcidx (IDX.text_id pc_fn_name)))
          end
    end
  (*Kは継続　(後に続く計算.)*)
  fun compile_expr e K module memoffset (function_ids, pc_table) =
    case e of
      TS.INT i =>
        ((I.i32const i) :: K, module, memoffset, function_ids, pc_table)
    (*文字列はヒープのポインタとサイズのペア
      0 top ,... bottom
      1 top str_ptr,... bottom
      2 top ptr,str_ptr,... bottom
      3 top ,... bottom
      4 top str_ptr,... bottom 
      5 top size,str_ptr,... bottom
      6 top ,... bottom 
      7 top str_ptr,... bottom
      *)
    | TS.STRING s =>
        let
          val string_data = I.data
            ( NONE
            , I.active_data_mode
                ( IDX.memidx (IDX.text_id "linear_memory")
                , [I.i32const memoffset]
                )
            , s
            )
          val new_module =
            { ty = #ty module
            , im = #im module
            , fn_ = #fn_ module
            , ta = #ta module
            , me = #me module
            , gl = #gl module
            , ex = #ex module
            , st = #st module
            , el = #el module
            , da = (string_data :: #da module)
            }
        in

          ( [ (I.i32const 8)
            , (I.call (IDX.funcidx (IDX.text_id "alloc")))
            , (I.localtee (IDX.localidx (IDX.text_id "str_ptr")))
            , (I.i32const memoffset)
            , (I.i32store (I.memarg (0w0, 0w4)))
            , (I.localget (IDX.localidx (IDX.text_id "str_ptr")))
            , (I.i32const (size s))
            , (I.i32store (I.memarg (0w4, 0w4)))
            , (I.localget (IDX.localidx (IDX.text_id "str_ptr")))
            ] @ K
          , new_module
          , memoffset + (size s)
          , function_ids
          , pc_table
          )
        end

    | TS.TRUE =>
        ((I.i32const 1) :: K, module, memoffset, function_ids, pc_table)

    | TS.FALSE =>
        ((I.i32const 0) :: K, module, memoffset, function_ids, pc_table)

    | TS.EXPID (id, _) =>
        ( I.localget (IDX.localidx (IDX.text_id id)) :: K
        , module
        , memoffset
        , function_ids
        , pc_table
        )

    | TS.EXPPAIR (e1, e2) =>
        let
          val (pc_table, call) =
            generate_pair_construction_call (pc_table, TS.getTy e1, TS.getTy e2)
          val (K2, module, memoffset, function_ids, pc_table) =
            compile_expr e2 (call :: K) module memoffset
              (function_ids, pc_table)
        in
          compile_expr e1 K2 module memoffset (function_ids, pc_table)
        end

    (*pair #1は (pair_addr)->#1して読みだす.
       0 top pair_addr,... bottom
       1 top #2 ,... bottom 
    *)
    | TS.EXPPROJ1 e =>
        let
          val ty = TS.getTy e
          val i =
            case ty of
              PAIRty (INTty, _) => I.i32load (I.memarg (0w0, 0w4))
            | PAIRty (BOOLty, _) => I.i32load (I.memarg (0w0, 0w4))
            | PAIRty (STRINGty, _) => I.i32load (I.memarg (0w0, 0w4))
            | PAIRty (PAIRty (_, _), _) => I.i32load (I.memarg (0w0, 0w4))
            | PAIRty (FUNty (_, _), _) => I.i32load (I.memarg (0w0, 0w4))
            | PAIRty (POLYty (_, _), _) => I.i32load (I.memarg (0w0, 0w4))
            | PAIRty (TYVARty _, _) => I.i32load (I.memarg (0w0, 0w4))
            | POLYty (_, _) => I.i32load (I.memarg (0w0, 0w4))
            | x => raise CantMapToWasmValType x
        in
          compile_expr e (i :: K) module memoffset (function_ids, pc_table)
        end
    (*pair #2は#1のlinear memory のバイト数オフセットして読みだす.
       0 top pair_addr,... bottom
       1 top #2 ,... bottom 
    *)
    | TS.EXPPROJ2 e =>
        let
          val ty = TS.getTy e
        in
          let
            val offset =
              (case ty of
                 PAIRty (INTty, _) => 0w4
               | PAIRty (BOOLty, _) => 0w4
               | PAIRty (STRINGty, _) => 0w4
               | PAIRty (FUNty (_, _), _) => 0w4
               | PAIRty (PAIRty (_, _), _) => 0w4
               | PAIRty (TYVARty _, _) => 0w4
               | PAIRty (POLYty (_, _), _) => 0w4
               | POLYty (tyvars, ty) => 0w4
               | x => raise CantMapToWasmValType x)
          in
            let
              val i =
                (case ty of
                   PAIRty (_, INTty) => I.i32load (I.memarg (offset, 0w4))
                 | PAIRty (_, BOOLty) => I.i32load (I.memarg (offset, 0w4))
                 | PAIRty (_, STRINGty) => I.i32load (I.memarg (offset, 0w4))
                 | PAIRty (_, PAIRty (_, _)) =>
                     I.i32load (I.memarg (offset, 0w4))
                 | PAIRty (_, FUNty (_, _)) =>
                     I.i32load (I.memarg (offset, 0w4))
                 | PAIRty (_, TYVARty _) => I.i32load (I.memarg (offset, 0w4))
                 | PAIRty (_, POLYty (_, _)) =>
                     I.i32load (I.memarg (offset, 0w4))
                 | POLYty (tyvars, ty) => I.i32load (I.memarg (offset, 0w4))
                 | x => raise CantMapToWasmValType x)
            in
              compile_expr e (i :: K) module memoffset (function_ids, pc_table)
            end
          end
        end
    (*0 top ,... bottom 
      1 top x1,... bottom 
      2 top x2,x1 ,... bottom
      3 top x1 prim x2 ,... bottom
    *)
    | TS.EXPPRIM (prim, e1, e2) =>
        let
          val p =
            case prim of
              S.ADD => I.i32add
            | S.SUB => I.i32sub
            | S.MUL => I.i32mul
            | S.DIV => I.i32div_s
            | S.EQ => I.i32eq
          val (K, module, memoffset, function_ids, pc_table) =
            compile_expr e2 (p :: K) module memoffset (function_ids, pc_table)
        in
          compile_expr e1 K module memoffset (function_ids, pc_table)
        end
    (*
    0 top ,... bottom
    1 top 0 or 1 ,... bottom 
    2 top e2 or e3 value,... bottom 
    *)
    | TS.EXPIF (e1, e2, e3) =>
        let
          val ty = TS.getTy e2
          val bt = CMLTyToblocktype ty
          val (e2K, module, memoffset, function_ids, pc_table) =
            compile_expr e2 nil module memoffset (function_ids, pc_table)
          val (e3K, module, memoffset, function_ids, pc_table) =
            compile_expr e3 nil module memoffset (function_ids, pc_table)
        in
          compile_expr e1 (I.if_ (NONE, bt, e2K, e3K) :: K) module memoffset
            (function_ids, pc_table)
        end
    | TS.EXPFN (x, e_inner, ty) =>
        generate_closure
          ( "non_recursive_closure"
          , x
          , e
          , K
          , module
          , memoffset
          , function_ids
          , pc_table
          )
    | TS.EXPFIX (f, x, e_inner, ty) =>
        generate_closure (f, x, e, K, module, memoffset, function_ids, pc_table)
    | TS.EXPAPP (e_1, e_2, ty) =>
        (*
          クロージャの呼び出しには専用の関数 closure_call を用いる.
          closure_call はclosure_ptr とarg を引数にとりクロージャを呼び出す組み込み関数である.
        0 top closure_ptr,... bottom e1の計算
        1 top arg,closure_ptr,... bottom e2の計算
        3 top calicurated value ,... bottom  closure_call を呼ぶ
        *)
        let
          val ft = FUNty (TS.getTy e_2, ty)
          val (K, module, memoffset, function_ids, pc_table) =
            compile_expr e_2
              (I.call (IDX.funcidx (IDX.text_id "closure_call")) :: K) module
              memoffset (function_ids, pc_table)
        in
          compile_expr e_1 K module memoffset (function_ids, pc_table)
        end
  and
    (*クロージャの生成コードと,クロージャの実際の関数を生成してモジュールに入れる.
    f: クロージャ名　再帰があるなら　, そうでないならばsyntactically incorrect な 名前
    x: 引数名
    e: クロージャである式 EXPFIX or EXPFN 
    K: 継続
    module: 現在ビルド中の WasmModule.module
    memoffset: 現在ビルド中の静的領域で使ったメモリの量.  
    *) generate_closure (f, x, e, K, module, memoffset, function_ids, pc_table) =
    let
      val e_inner =
        (case e of
           TS.EXPFIX (f, x, e_inner, ty) => e_inner
         | TS.EXPFN (x, e_inner, ty) => e_inner
         | _ => raise Unreachable)
      val fvs = fv e
      (*クロージャ内で使う変数群の生成*)
      val prelude =
        SEnv.foldri (fn (v_name, v_ty, K) => (v_name, CMLTyTovaltype v_ty) :: K)
          [] fvs
      (*クロージャは関数ポインタ(関数テーブルのインデックス)を含むため 自由変数+ 関数ポインタ4バイトが必要*)
      val closure_size = foldr (op+) 4
        (map (fn (v_name, v_ty) => size_of_valtype v_ty) prelude)
      val (closure_name_offset_size, _) =
        foldr
          (fn ((name, size), (xs, last_offset)) =>
             (xs @ [(name, last_offset, size)], last_offset + size)) ([], 0w4)
          (map
             (fn (v_name, v_ty) => (v_name, Word.fromInt (size_of_valtype v_ty)))
             prelude)
      val (var_ty, return_ty) =
        case TS.getTy e of
          Type.FUNty (a, b) => (a, b)
        | x => raise CantMapToWasmValType x
      (*関数内部名生成*)
      val f_sig = "function"
      val f_sig_in_cml =
        ( [I.param (f, I.numtype I.i32), I.param (x, CMLTyTovaltype var_ty)]
        , [I.result (CMLTyTovaltype return_ty)]
        )

      val (internal_function_name, function_index, function_ids) =
        case SEnv.find (function_ids, f_sig) of
          NONE => (f_sig ^ "0", 0, SEnv.insert (function_ids, f_sig, 1))
        | SOME count =>
            ( f_sig ^ (Int.toString count)
            , count
            , SEnv.insert (function_ids, f_sig, count + 1)
            )
      (*ローカル変数からとってきてクロージャに詰め込む処理 問題なし*)
      val K =
        (foldr
           (fn ((v_name, offset, _), iseq) =>
              [ I.localget (IDX.localidx (IDX.text_id "closure_ptr"))
              , I.localget (IDX.localidx (IDX.text_id v_name))
              , I.i32store (I.memarg (offset, 0w4))
              ] @ iseq) [] closure_name_offset_size)
        @ I.localget (IDX.localidx (IDX.text_id "closure_ptr")) :: K
      (*クロージャをlinear メモリに保管するための領域を確保
        クロージャの大きさを積み, alloc を呼び出し,
        closure_ptr に保存,
        関数ポインタを積む,
        クロージャに関数ポインタを書き込む
        7/19 15:15 修正 
      *)
      val K =
        [ I.i32const closure_size
        , I.call (IDX.funcidx (IDX.text_id "alloc"))
        , I.localtee (IDX.localidx (IDX.text_id "closure_ptr"))
        , I.i32const function_index
        , I.i32store (I.memarg (0w0, 0w4))
        ] @ K
      (*渡されてきたクロージャをローカル変数に展開する
        これによりネストしたクロージャが実現できる.
        we need to respect execution of localset, i32load ,localget .
        0 top ,... bottom 初期状態
        1 top closure_ptr,... bottom　クロージャのアドレスをスタックに積む
        2 top memoffset,closure_ptr,... bottom　スタックにクロージャ先頭からのオフセットを積む
        3 top ,... bottom ローカル変数に値をセット.
      *)
      val f_closure_load =
        foldr
          (fn ((name, offset, size), iseq) =>
             iseq
             @
             [ I.localget (IDX.localidx (IDX.text_id f))
             , I.i32load (I.memarg (offset, 0w4))
             , I.localset (IDX.localidx (IDX.text_id name))
             ]) [] closure_name_offset_size

      (*関数の定義に必要なものがそろった*)
      val (f_K, module, memoffset, function_ids, pc_table) =
        compile_expr e_inner [] module memoffset (function_ids, pc_table)
      val f_body = f_closure_load @ f_K
      val locals =
        (map
           (fn (v_name, v_ty) =>
              I.local_ (SOME (IDX.localidx (IDX.text_id v_name)), v_ty)) prelude)
        @ internal_local_vars


      val function =
        ( SOME (IDX.funcidx (IDX.text_id internal_function_name))
        , I.with_functype
            ( IDX.typeidx (IDX.text_id "function")
            , #1 f_sig_in_cml
            , #2 f_sig_in_cml
            )
        , locals
        , f_body
        )

      fun filter_fn (SOME (IDX.tableidx (IDX.text_id a)), I.tabletype b) =
            a = f_sig
        | filter_fn (_, I.tabletype b) = false

      fun table_and_funcs {ta = tables, fn_ = funcs, ...} =
        ( (case List.filter filter_fn tables of
             nil =>
               ( SOME (IDX.tableidx (IDX.text_id f_sig))
               , I.tabletype (I.min 0w1, I.funcref)
               ) :: tables
           | (_, I.tabletype (I.min limit, reftype)) :: tables =>
               ( SOME (IDX.tableidx (IDX.text_id f_sig))
               , I.tabletype (I.min (limit + 0w1), reftype)
               ) :: tables
           | (_, I.tabletype (I.minmax (min, max), reftype)) :: tables =>
               ( SOME (IDX.tableidx (IDX.text_id f_sig))
               , I.tabletype (I.minmax (min + 0w1, max + 0w1), reftype)
               ) :: tables)
        , function :: funcs
        )
      val (new_ta, new_fn) = table_and_funcs module
    in
      ( K
      , { ty = #ty module
        , me = #me module
        , gl = #gl module
        , el = #el module
        , da = #da module
        , im = #im module
        , ex = #ex module
        , st = #st module
        , ta = new_ta
        , fn_ = new_fn
        }
      , memoffset
      , function_ids
      , pc_table
      )
    end


  (*__start 関数の中に宣言と対応する命令列を積む
  compile_declarations Dec list * function_ids -> (I.local list , expr list , module ,offset)      
  compile_expr:TypedSyntax.typed_exp-> WasmModule instruction list-> WasmModule.module -> int ->(WasmModule.instruction list * WasmModule.module * int ) *)
  fun compile_declarations ([], function_ids, pc_table) =
        (internal_local_vars, [], I.emptyModule, 0, function_ids, pc_table)
    | compile_declarations (x :: xs, function_ids, pc_table) =
        let
          val TS.VAL (name, e) = x
          val (locals, instructions, module, offset, function_ids, pc_table) =
            compile_declarations (xs, function_ids, pc_table)
          val (iseq, module, offset, function_ids, pc_table) =
            compile_expr e [] module offset (function_ids, pc_table)
        in
          ( (I.local_
               ( SOME (IDX.localidx (IDX.text_id name))
               , CMLTyTovaltype (TS.getTy e)
               )) :: locals
          , iseq :: instructions
          , module
          , offset
          , function_ids
          , pc_table
          )
        end

  val alloc =
    let

      val alloc_iseq =
        [ I.globalget (IDX.globalidx (IDX.text_id "alloc_ptr"))
        , I.localset (IDX.localidx (IDX.text_id "old"))
        , I.localget (IDX.localidx (IDX.text_id "old"))
        , I.localget (IDX.localidx (IDX.text_id "size"))
        , I.i32add
        , I.globalset (IDX.globalidx (IDX.text_id "alloc_ptr"))
        , I.localget (IDX.localidx (IDX.text_id "old"))
        ]
    in
      ( SOME (IDX.funcidx (IDX.text_id "alloc"))
      , I.with_functype
          ( IDX.typeidx (IDX.text_id "alloc_sig")
          , [(I.param ("size", I.numtype I.i32))]
          , [(I.result (I.numtype I.i32))]
          )
      , [(I.local_ (SOME (IDX.localidx (IDX.text_id "old")), (I.numtype I.i32)))]
      , alloc_iseq
      )
    end
  val closure_call =
    let
      val closure_call_iseq =
        [ I.localget (IDX.localidx (IDX.text_id "closure"))
        , I.localget (IDX.localidx (IDX.text_id "arg"))
        , I.localget (IDX.localidx (IDX.text_id "closure"))
        , I.i32load (I.memarg (0w0, 0w4))
        , I.call_indirect (IDX.tableidx (IDX.text_id "function"), I.name_only
            (IDX.typeidx (IDX.text_id "function")))
        ]
    in
      ( SOME (IDX.funcidx (IDX.text_id "closure_call"))
      , I.with_functype
          ( IDX.typeidx (IDX.text_id "closure_call_sig")
          , [ I.param ("closure", I.numtype I.i32)
            , I.param ("arg", I.numtype I.i32)
            ]
          , [I.result (I.numtype I.i32)]
          )
      , []
      , closure_call_iseq
      )
    end
  (*__cml_main 関数を作る. *)
  fun compile is_debug declarations =

    let
      val function_ids = SEnv.empty
      val pc_table = SEnv.empty
      val pp_table = SEnv.empty
      val ty_env =
        foldr
          (fn (decl, ty_env) =>
             let val TypedSyntax.VAL (id, expr) = decl
             in (id, TypedSyntax.getTy expr)
             end :: ty_env) [] declarations
      val (locals, wasm_expr_list, module, memoffset, function_ids, pc_table) =
        compile_declarations (declarations, function_ids, pc_table)
      val alloc_sig = I.type_definition (SOME "alloc_sig", I.functype
        ([I.param ("size", I.numtype I.i32)], [(I.result (I.numtype I.i32))]))
      val alloc_ptr =
        ( SOME (IDX.globalidx (IDX.text_id "alloc_ptr"))
        , I.mutable (I.numtype I.i32)
        , [I.i32const 0]
        )
      val closure_call_sig =
        I.type_definition (SOME "closure_call_sig", I.functype
          ( [ I.param ("closure", I.numtype I.i32)
            , I.param ("arg", I.numtype I.i32)
            ]
          , [I.result (I.numtype I.i32)]
          ))
      val mem = I.import ("env", "linear_memory", I.m
        (SOME (IDX.memidx (IDX.text_id "linear_memory")), I.memtype (I.min 0w2)))
      val pair_constructions = SEnv.listItemsi pc_table

      val pc_sigs =
        foldr (fn ((name, (_, s)), l) => s :: l) [] pair_constructions
      val pc_fns =
        foldr (fn ((name, (f, _)), l) => f :: l) [] pair_constructions
      val module =
        { ty = closure_call_sig :: alloc_sig :: pc_sigs @ (#ty module)
        , im = mem :: (#im module)
        , fn_ =
            if (SEnv.numItems function_ids = 0) then
              alloc :: pc_fns @ (#fn_ module)
            else
              closure_call :: alloc :: pc_fns @ (#fn_ module)
        , ta = #ta module
        , me = #me module
        , gl = alloc_ptr :: (#gl module)
        , ex = #ex module
        , st = #st module
        , el = #el module
        , da = #da module
        }
      val local_and_exprs = ListPair.zip (locals, wasm_expr_list)

      fun folder
        ( (I.local_ (local_id, val_ty), expr)
        , (module, iseq, memoffset, pp_table)
        ) =
        (case local_id of
           SOME idx =>
             if is_debug then
               let
                 val SOME (id, ty) =
                   List.find
                     (fn elem =>
                        case idx of
                          IDX.localidx (IDX.text_id id) => id = #1 elem
                        | _ => raise Unreachable) ty_env
                 val (module, print, memoffset, pp_table) =
                   Debug.insert_debug_instructions
                     (id, ty, module, memoffset, pp_table)
               in
                 ( module
                 , iseq @ expr @ (I.localset idx) :: print
                 , memoffset
                 , pp_table
                 )
               end
             else
               (module, iseq @ expr @ [(I.localset idx)], memoffset, pp_table)
         | NONE => raise Unreachable)

      val (module, iseq, memoffset, pp_table) =
        foldr folder (module, [], memoffset, pp_table) local_and_exprs
      (*初めにグローバル変数 alloc_ptr の初期化を行う.*)
      val aligned_initial_alloc_ptr = ((memoffset div 4) + 1) * 4
      val iseq =
        [ I.i32const aligned_initial_alloc_ptr
        , I.globalset (IDX.globalidx (IDX.text_id "alloc_ptr"))
        ] @ iseq
      val f =
        ( SOME (IDX.funcidx (IDX.text_id "__cml_main"))
        , I.with_functype (IDX.typeidx (IDX.text_id "entry_point"), [], [])
        , locals
        , iseq
        )
      val module =
        if is_debug then
          let
            val pair_debugs = SEnv.listItemsi pp_table
            val pp_sigs =
              foldr (fn ((name, (_, s)), l) => s :: l) [] pair_debugs
            val pp_fns = foldr (fn ((name, (f, _)), l) => f :: l) [] pair_debugs
          in
            { ty = pp_sigs @ (#ty module)
            , fn_ = pp_fns @ (f :: (#fn_ module))
            , ta = #ta module
            , me = #me module
            , gl = #gl module
            , el = #el module
            , da = #da module
            , im = #im module
            , ex = #ex module
            , st = #st module
            }
          end
        else
          { ty = #ty module
          , fn_ = f :: (#fn_ module)
          , ta = #ta module
          , me = #me module
          , gl = #gl module
          , el = #el module
          , da = #da module
          , im = #im module
          , ex = #ex module
          , st = #st module
          }
      val types =
        [I.type_definition (SOME "function", I.functype
           ( [ I.param ("closure", I.numtype I.i32)
             , I.param ("arg", I.numtype I.i32)
             ]
           , [I.result (I.numtype I.i32)]
           ))]
      fun f_names f_prefix 0 = []
        | f_names f_prefix i =
            (f_prefix ^ (Int.toString (i - 1))) :: (f_names f_prefix (i - 1))
      val elements =
        foldr
          (fn ((SOME (IDX.tableidx (IDX.text_id tid)), tt), xs) =>
             let
               val SOME count = SEnv.find (function_ids, tid)
               val f_names = f_names tid count
             in
               (I.element
                  ( NONE
                  , I.elemlist
                      ( I.funcref
                      , foldl
                          (fn (a, k) =>
                             (I.elemexpr
                                [I.reffunc (IDX.funcidx (IDX.text_id a))]) :: k)
                          [] f_names
                      )
                  , I.active_elem_mode
                      (IDX.tableidx (IDX.text_id tid), [I.i32const 0])
                  ) :: xs)
             end) [] (#ta module)
      val module =
        if is_debug then
          Debug.install_system_functions
            { ty = types @ (#ty module)
            , fn_ = #fn_ module
            , ta = #ta module
            , me = #me module
            , gl = #gl module
            , el = elements
            , da = #da module
            , im = #im module
            , ex = main_export :: (#ex module)
            , st = #st module
            }

        else
          { ty = types @ (#ty module)
          , fn_ = #fn_ module
          , ta = #ta module
          , me = #me module
          , gl = #gl module
          , el = elements
          , da = #da module
          , im = #im module
          , ex = main_export :: (#ex module)
          , st = #st module
          }

    in
      print ((I.moduleToString module) ^ "\n");
      module
    end
  fun print_CantMapToWasmValType ty =
    print ("Compile error:" ^ Type.tyToString ty ^ "\n")
end
