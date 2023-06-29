open Type

structure WasmComp =
struct
  (*
  型付き抽象構文木からWASMに落とす
  *)
  structure TS = TypedSyntax
  structure S = Syntax
  structure I = WasmModule

  (*
  多相関数は単相化してからwasmに落とすため、ここではPOLYty は考慮しない
  関数ポインタを置くテーブルの名前を生成
  *)
  exception CantMapToDispatchTableName of  Type.ty
  fun generate_dispatch_table_name ty = case ty of 
    INTty =>"i"
    |BOOLty=>"b"
    |STRINGty=>"s"
    |FUNty(t1,t2)=> generate_dispatch_table_name t1 ^"->"^ generate_dispatch_table_name t2 
    |PAIRty(t1,t2)=>"("^generate_dispatch_table_name t1^","^generate_dispatch_table_name t2^")"
    |_=>raise CantMapToDispatchTableName ty
  fun generate_type_id ty  ="fun"^generate_dispatch_table_name ty
  exception CantMapToWasmValType of Type.ty 
  (*CoreMLの型からWASMのvaltype に変換*)
  fun CMLTyTovaltype ty = I.numtype( case ty of 
  INTty=>I.i32
  |BOOLty=>I.i32
  |STRINGty=>I.i32
  |PAIRty(_,_)=>I.i32
  |FUNty(_,_)=>I.i32
  |_=>raise CantMapToWasmValType ty
  )
  fun size_of_numtype t =case t of 
  I.i32=>4
  |I.i64=>8
  |I.f32=>4
  |I.f64=>8 
  fun size_of_valtype t=case t of 
  I.numtype nt=>(size_of_numtype nt)
  |I.reftype _ =>4
  |I.vectype _=>16

  (*CoreMLの型からWASMのblocktype に変換*)
  fun CMLTyToblocktype ty =I.blocktype(SOME(I.result(CMLTyTovaltype ty)))
  val internal_local_vars = [
    (I.local_(SOME(IDX.localidx(IDX.text_id "pair_addr")),I.numtype I.i32)),
    (I.local_(SOME(IDX.localidx(IDX.text_id "closure_ptr")),I.numtype I.i32)),
    (I.local_(SOME(IDX.localidx(IDX.text_id "__fptr")),I.numtype I.i32))]
  (*型付き抽象構文木の自由変数を求める*)
  fun fv e=case e of 
    TS.INT _=>SEnv.empty
    |TS.TRUE=>SEnv.empty
    |TS.FALSE=>SEnv.empty
    |TS.STRING _ =>SEnv.empty
    |TS.EXPID(x,ty)=>SEnv.singleton (x,ty)
    |TS.EXPFN(x,e,_)=>
    let val (m,a)= SEnv.remove (fv e,x)in
     m 
    end 
    |TS.EXPFIX(f,x,e,_)=>
    let val (m,a)=SEnv.remove (fv e ,x)
        val (m,a)=SEnv.remove(m,f)in 
      m
    end 
    |TS.EXPAPP(e1,e2,_)=>SEnv.unionWith (fn (a,_)=>a)(fv e1,fv e2)
    |TS.EXPIF(e1,e2,e3)=>SEnv.unionWith (fn (a,_)=>a)(fv e1,SEnv.unionWith (fn (a,b)=>a) (fv e2,fv e3))
    |TS.EXPPAIR(e1,e2)=>SEnv.unionWith (fn (a,_)=>a)(fv e1,fv e2)
    |TS.EXPPRIM(_,e1,e2)=>SEnv.unionWith (fn (a,_)=>a)(fv e1,fv e2)
    |TS.EXPPROJ1(e)=>fv e 
    |TS.EXPPROJ2(e)=>fv e
   
   (*クロージャの表現から関数に落とす*)
   (*type func =  funcidx option * typeuse * local_ list * instruction list *)
   fun generate_local_list e = let val fvs =fv e in 
   SEnv.foldr (op ::) [] (SEnv.map CMLTyToblocktype  fvs)  
   end 
   exception CantCalicurateSizeOfCMLType of  Type.ty
   fun size_of ty=case ty of 
   INTty =>4
   |BOOLty=>4 
   |STRINGty=>4 
   |PAIRty(a,b)=>size_of a + size_of b
   (*4 byte of closure pointer.*)
   |FUNty(_,_)=>4
   |_=>raise CantCalicurateSizeOfCMLType ty
  fun getStringCount ctx=length (#strings ctx)

  (*compile_expr:TypedSyntax.typed_exp-> WasmModule instruction list-> WasmModule.module -> int ->(WasmModule.instruction list * WasmModule.module * int ) *)
  (*Kは継続　(後に続く計算.)*)
  fun compile_expr e K module memoffset= case e of 
  TS.INT i =>((I.i32const i) ::K,module,memoffset)
  |TS.STRING s =>
  let val data_count = length (#da module) 
     val string_data = I.data(SOME(IDX.dataidx(IDX.text_id("string_"^Int.toString data_count))),I.active_data_mode(IDX.memidx(IDX.text_id("linear_memory")),[I.i32const memoffset]),s ) 
     val new_module = {ty = #ty module,im = #im module,fn_ = #fn_ module ,ta = #ta module ,me = #me module ,gl = #gl module,ex = #ex module,st = #st module,el= #el module ,da = (string_data::  #da module) }in 
    ((I.i32const memoffset)::K,new_module,memoffset + (size s) + 1  )
  end  
  |TS.TRUE=>((I.i32const 1) ::K,module ,memoffset)
  |TS.FALSE=>((I.i32const 0) ::K,module ,memoffset)
  |TS.EXPID(id,_)=>(I.localget (IDX.localidx(IDX.text_id id))::K,module,memoffset)
  |TS.EXPPAIR (e1, e2) =>
    let val size_of_pair = size_of (PAIRty(TS.getTy e1 ,TS.getTy e2))
        val alloc_call = I.call (IDX.funcidx(IDX.text_id("alloc"))) 
        val (e1_store,offset) = (case TS.getTy e1 of
          INTty=>(I.i32store(I.memarg(0w0,0w4)),0w4)
          |BOOLty=>(I.i32store(I.memarg(0w0,0w4)),0w4)
          |STRINGty=>(I.i32store(I.memarg(0w0,0w4)),0w4)
          |PAIRty(_,_)=>(I.i32store(I.memarg(0w0,0w4)),0w4)
          |FUNty(_,_)=>(I.i32store(I.memarg(0w0,0w4)),0w4)
          |t=>raise CantCalicurateSizeOfCMLType t)  
        val e2_store = (case TS.getTy e2 of 
            INTty=>I.i32store(I.memarg(offset,0w4))
            |BOOLty=>I.i32store(I.memarg(offset,0w4))
            |STRINGty=>I.i32store(I.memarg(offset,0w4))
            |PAIRty(a,b)=>I.i32store(I.memarg(offset,0w4))
            |FUNty(a,b)=>I.i32store(I.memarg(offset,0w4))
            |t=>raise CantCalicurateSizeOfCMLType t )
        val pair_addr = IDX.localidx(IDX.text_id "pair_addr")
                (*after exec e1,e2   call alloc              e1_store         e2_store
              top|   e2    | =>        top|pair_addr|=> top|pair_addr|      =>|pair_addr|
                 |   e1    |              |    e2   |      |e1       |        |    ...  |
                 |   ...   |              |    e1   |      |   ...   |
                                          |    ...  |  *)
        val (K,module,memoffset) = compile_expr e2 (
                (I.i32const size_of_pair)::
                alloc_call::
                I.localtee(pair_addr)::
                e2_store::
                I.localget(pair_addr)::
                e1_store::
                I.localget(pair_addr)::K) module memoffset in 
        compile_expr e1 K module memoffset 

              end
  |TS.EXPPROJ1 e =>
  let val ty =TS.getTy e 
      val i =case ty of
      PAIRty(INTty,_)=>I.i32load(I.memarg(0w0,0w4)) 
      |PAIRty(BOOLty,_)=>I.i32load(I.memarg(0w0,0w4)) 
      |PAIRty(STRINGty,_)=>I.i32load(I.memarg(0w0,0w4)) 
      |PAIRty(PAIRty(_,_),_)=>I.i32load(I.memarg(0w0,0w4)) 
      |PAIRty(FUNty(_,_),_)=>I.i32load(I.memarg(0w0,0w4)) 
     in
     (*スタックのトップはペアのポインタになっている.
       したがって読みだせばよい.
      *)
      compile_expr e (i:: K) module memoffset 
    end
    (*pair #2は#1のlinear memory のバイト数オフセットして
      読みだす.
    *)
    | TS.EXPPROJ2 e =>
    let val ty = TS.getTy e in
      let val offset = (case ty of 
        PAIRty(INTty,_)=>0w4
        |PAIRty(BOOLty,_)=>0w4
        |PAIRty(STRINGty,_)=>0w4
        |PAIRty(FUNty(_,_),_)=>0w4
        |PAIRty(PAIRty(_,_),_)=>0w4
        |_=>0w0) in 
        let val i = (case ty of 
          PAIRty(_,INTty)=>I.i32load(I.memarg(offset,0w4))
          |PAIRty(_,BOOLty)=>I.i32load(I.memarg(offset,0w4))
          |PAIRty(_,STRINGty)=>I.i32load(I.memarg(offset,0w4))
          |PAIRty(_,PAIRty(_,_))=>I.i32load(I.memarg(offset,0w4))
          |PAIRty(_,FUNty(_,_))=>I.i32load(I.memarg(offset,0w4))
          |_=>I.nop)
        in 
          compile_expr e (i:: K) module memoffset
        end   
      end 
    end 
    | TS.EXPPRIM (prim, e1, e2) => 
      let
        val p = 
          case prim of S.ADD => I.i32add| S.SUB => I.i32sub | S.MUL => I.i32mul
                       | S.DIV => I.i32div_s | S.EQ => I.i32eq  
        val (K,module,memoffset)=compile_expr e2 (p::K) module memoffset in
        compile_expr e1 K module memoffset 
        end 
      (*
      クロージャを生成するとはメモリ上に関数ポインタと環境へのポインタを置くこと
      型ごとにテーブルを変える.
      テーブル名は型から生成. generate_dispatch_table_name
      *)
    |TS.EXPIF (e1, e2, e3) => 
    let val ty = TS.getTy e2 
        val bt = CMLTyToblocktype ty 
        val (e2K,module,memoffset)= compile_expr e2 nil module memoffset  
        val (e3K,module,memoffset)= compile_expr e3 nil module memoffset in  
          compile_expr e1 (I.if_(NONE,bt,e2K, e3K) :: K) module memoffset
        end   

    (*e=typed_expr K= instructions module =WasmModule  memoffset = int*)
    (*prelude をfvsから作る *)
    |TS.EXPFN(x,e_inner,ty)=>
    let val fvs  =fv e  
        val prelude =  SEnv.foldr (op ::) [] (SEnv.mapi (fn (v_name,v_ty)=> (v_name,CMLTyTovaltype v_ty)) fvs)  
        (*クロージャは関数ポインタを含むため 自由変数+ 関数ポインタ4バイトが必要*)
        val closure_size =foldr (op + )  4 (map (fn (v_name,v_ty)=> size_of_valtype v_ty)  prelude) 
        (*クロージャをlinear メモリに保管するための領域を確保*)
        val alloc_call = I.localtee(IDX.localidx(IDX.text_id("closure_ptr")))::I.call(IDX.funcidx(IDX.text_id("alloc"))) ::I.i32const closure_size::[]  
        
        (*クロージャのデータを保存する.*)
        (*ftbl 関数テーブルのインデックスの保存.*)
        (*あとはprelude の順に詰めていく.*)
        (*変数は val ,fun で宣言され,local にある.*)
        val saves = [] in
            (saves@alloc_call@K, module ,memoffset)
        end 
    (*closure conversion
      lambda lifting
    *)
    |TS.EXPFIX(f,x,e_inner,ty)=>
    let val fvs  =fv e  
        (*クロージャ内で使う変数群の生成*)
        val prelude =  SEnv.foldr (op ::) [] (SEnv.mapi (fn (v_name,v_ty)=> (v_name,CMLTyTovaltype v_ty)) fvs)  
        (*クロージャは関数ポインタを含むため 自由変数+ 関数ポインタ4バイトが必要*)
        val closure_size =foldr (op + )  4 (map (fn (v_name,v_ty)=> size_of_valtype v_ty)  prelude) 
        (*クロージャをlinear メモリに保管するための領域を確保*)
        val K =I.i32const closure_size::I.call(IDX.funcidx(IDX.text_id("alloc"))):: I.localtee(IDX.localidx(IDX.text_id("closure_ptr")))::K  
        (*クロージャのデータを保存する.*)
        (*ftbl 関数テーブルのインデックスの保存.*)
        (*あとはprelude の順に詰めていく.*)
        (*変数は val ,fun で宣言され,local にある.*)
        
        (*ここで関数の生成を行う.*)
        (*クロージャのコード生成*)
        val function_body = compile_expr e [] module memoffset
        val locals =(map (fn (v_name,v_ty)=>I.local_(SOME(IDX.localidx(IDX.text_id v_name)),v_ty)) prelude) @ internal_local_vars
        (*クロージャからローカル変数に展開*)
        (*メモリからオフセット付きでアクセスしてローカル変数に保存*)
        val saves = [] 
        in
            (saves@K, module ,memoffset)
        end 

    |TS.EXPAPP(e_1,e_2,ty)=>
    (*スタックにはクロージャptr が積まれている.*)
    (*クロージャptr からftbl ptr を参照.*)
    (*クロージャポインタから関数ポインタを取り出すためにローカル変数 __fptr を使う *)
    (*スタックは次のようになっている
            |クロージャポインタ |       =>top |関数ポインタ      |
            |引数              |             |クロージャポインタ|
            |      ...         |             |引数              |
                                              |...               |
          *)

      let val ft = FUNty(TS.getTy e_2,ty)  
          val (K,module,memoffset) =compile_expr e_2 
          (I.localtee(IDX.localidx(IDX.text_id "__fptr"))::
            I.localget(IDX.localidx(IDX.text_id "__fptr"))::
            I.i32load(I.memarg(0w0,0w4))::
            I.call_indirect(IDX.tableidx(IDX.text_id(generate_dispatch_table_name ft)),I.name_only(IDX.typeidx(IDX.text_id(generate_type_id ft))))::K) module memoffset  
          in
           compile_expr e_1 K module memoffset 
          end 

    (*__start 関数の中に宣言と対応する命令列を積む*)
    (*compile_declarations Dec list -> (I.local list , expr list , module ,offset)*)      
    (*compile_expr:TypedSyntax.typed_exp-> WasmModule instruction list-> WasmModule.module -> int ->(WasmModule.instruction list * WasmModule.module * int ) *)
    fun compile_declarations [] = (internal_local_vars,[],I.emptyModule,0)
    | compile_declarations (x::xs) = let
     val  TS.VAL(name,e) =x
     val (locals,instructions,module,offset)=compile_declarations xs  
     in
      let val (iseq,module,offset)= compile_expr e [] module offset in 
        ((I.local_(SOME(IDX.localidx(IDX.text_id(name))),CMLTyTovaltype( TS.getTy e)))::locals,iseq::instructions,module,offset)
      end 
    end  
    (*__cml_main 関数を作る. *)     
    fun compile declarations =
     let
      val (locals,wasm_expr_list,module,_)=compile_declarations declarations
      val local_and_exprs=ListPair.zip (locals,wasm_expr_list) 
      val iseq= foldr (fn ((I.local_(SOME(idx),val_ty),expr),iseq)=>iseq@expr@[(I.localset(idx)
      )] ) [] local_and_exprs
      val f =(SOME(IDX.funcidx(IDX.text_id("__cml_main"))),I.with_functype(IDX.typeidx(IDX.text_id("entry_point")),[],[]),locals,iseq) 
      val new_module={ty = #ty module,fn_ = (f::(#fn_ module)),ta = #ta module,me = #me module,gl = #gl module,el = #el module ,da = #da module,im = #im module,ex= #ex module,st = #st module}
     in 
     (*let's build function.!!!! *)
      print ( "Compiled to:\n" ^ I.moduleToString new_module ^ "\n");
     
     new_module
     end 
end