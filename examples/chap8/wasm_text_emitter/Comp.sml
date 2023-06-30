

structure WasmComp =
struct
  local 
    val function_ids = ref SEnv.empty 
  in 
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
    let val (m,_)= SEnv.remove (fv e,x)in
     m 
    end 
    |TS.EXPFIX(f,x,e,_)=>
    let val (m,_)=SEnv.remove (fv e ,x)
        val (m,_)=SEnv.remove(m,f)in 
      m
    end 
    |TS.EXPAPP(e1,e2,_)=>SEnv.unionWith (fn (a,_)=>a)(fv e1,fv e2)
    |TS.EXPIF(e1,e2,e3)=>SEnv.unionWith (fn (a,_)=>a)(fv e1,SEnv.unionWith (fn (a,b)=>a) (fv e2,fv e3))
    |TS.EXPPAIR(e1,e2)=>SEnv.unionWith (fn (a,_)=>a)(fv e1,fv e2)
    |TS.EXPPRIM(_,e1,e2)=>SEnv.unionWith (fn (a,_)=>a)(fv e1,fv e2)
    |TS.EXPPROJ1 e =>fv e 
    |TS.EXPPROJ2 e =>fv e
   
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
        val alloc_call = I.call (IDX.funcidx(IDX.text_id "alloc")) 
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
            |PAIRty(_,_)=>I.i32store(I.memarg(offset,0w4))
            |FUNty(_,_)=>I.i32store(I.memarg(offset,0w4))
            |t=>raise CantCalicurateSizeOfCMLType t )
        val pair_addr = IDX.localidx(IDX.text_id "pair_addr")

        (*
        計算の順序, この計算が終わった後にはスタックにはペアのポインタのみが残る.
        0 top ... bottom
        1 ヒープを割り当てる. top pair_addr,... bottom
        2 要素1を計算.        top v1 ,pair_addr,... bottom
        3 ヒープに要素1を保存 top　... bottom 
        4 ペアのアドレスをスタックに積む top pair_addr,... bottom
        5 要素2を計算         top v2,pair_addr,... bottom 
        6 ヒープに要素2を保存 top ... bottom      
        7 ペアのアドレスをスタックに積む top pair_addr ,... bottom        
        *)
        val (K2,module,memoffset) = compile_expr e2 ([e2_store,I.localget pair_addr]@K) module memoffset
        val (K1,module,memoffset) 
        = compile_expr e1 ([I.localget pair_addr,e1_store,I.localget pair_addr]@K2)  module memoffset
        in 
            ((I.i32const size_of_pair)::alloc_call::K1,module,memoffset)
        end 
  |TS.EXPPROJ1 e =>
  let val ty =TS.getTy e 
      val i =case ty of
      PAIRty(INTty,_)=>I.i32load(I.memarg(0w0,0w4)) 
      |PAIRty(BOOLty,_)=>I.i32load(I.memarg(0w0,0w4)) 
      |PAIRty(STRINGty,_)=>I.i32load(I.memarg(0w0,0w4)) 
      |PAIRty(PAIRty(_,_),_)=>I.i32load(I.memarg(0w0,0w4)) 
      |PAIRty(FUNty(_,_),_)=>I.i32load(I.memarg(0w0,0w4))
      |x=>raise  CantMapToWasmValType x
     in
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
        |x=>raise CantMapToWasmValType x) in 
        let val i = (case ty of 
          PAIRty(_,INTty)=>I.i32load(I.memarg(offset,0w4))
          |PAIRty(_,BOOLty)=>I.i32load(I.memarg(offset,0w4))
          |PAIRty(_,STRINGty)=>I.i32load(I.memarg(offset,0w4))
          |PAIRty(_,PAIRty(_,_))=>I.i32load(I.memarg(offset,0w4))
          |PAIRty(_,FUNty(_,_))=>I.i32load(I.memarg(offset,0w4))
          |x=>raise CantMapToWasmValType x)
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

    (*prelude をfvsから作る *)
    |TS.EXPFN(x,e_inner,ty)=>
      let val (K,module,memoffset) =generate_closure("non_recursive_closure",x,e,K,module,memoffset)
      in 
        (K,module,memoffset)
      end 
    (*closure conversion
      lambda lifting
    *)
    |TS.EXPFIX(f,x,e_inner,ty)=>
      let val (K,module,memoffset) =generate_closure(f,x,e,K,module,memoffset)
      in 
        (K,module,memoffset)
      end 
    |TS.EXPAPP(e_1,e_2,ty)=>
    (*スタックにはクロージャptr が積まれている.
    クロージャptr からftbl ptr を参照.
    クロージャポインタから関数ポインタを取り出すためにローカル変数 __fptr を使う 
    スタックは次のようになっている
            |引数　　　　　　　|       =>top |関数ポインタ      |
            |クロージャポインタ|             |引数|
            |      ...         |             |クロージャポインタ|
                                              |...              |
          *)

      let val ft = FUNty(TS.getTy e_2,ty)  
          val (K,module,memoffset) =compile_expr e_2 
          (I.localtee(IDX.localidx(IDX.text_id "__fptr"))::
            I.localget(IDX.localidx(IDX.text_id "__fptr"))::
            I.i32load(I.memarg(0w0,0w4))::
            I.call_indirect(IDX.tableidx(IDX.text_id(generate_type_id ft)),I.name_only(IDX.typeidx(IDX.text_id(generate_type_id ft))))::K) module memoffset  
          in
           compile_expr e_1 K module memoffset 
          end 
    and 
    generate_closure(f,x,e,K,module,memoffset)= 
    let val e_inner = (case e of 
    TS.EXPFIX(f,x,e_inner,ty)=>e_inner
    |TS.EXPFN(x,e_inner,ty)=>e_inner
    |_ =>raise Unreachable)
    val fvs  =fv e  
    (*クロージャ内で使う変数群の生成*)
    val prelude =  SEnv.foldr (op ::) [] (SEnv.mapi (fn (v_name,v_ty)=> (v_name,CMLTyTovaltype v_ty)) fvs)  
    (*クロージャは関数ポインタを含むため 自由変数+ 関数ポインタ4バイトが必要*)
    val closure_size =foldr (op + )  4 (map (fn (v_name,v_ty)=> size_of_valtype v_ty)  prelude) 
    val closure_name_offset_size= foldr (fn ((name,size),xs)=> 
          let val (_,last_offset,last_size) = List.last xs in 
            xs@[(name,last_offset+size,size)]
          end 
        ) [("__fptr",0w0,0w4)] (map (fn (v_name,v_ty)=>(v_name,Word.fromInt (size_of_valtype v_ty)) ) prelude)
    val (var_ty,return_ty) = case TS.getTy e of 
        Type.FUNty(a,b)=>(a,b)
        |x =>raise CantMapToWasmValType x
        (*関数内部名生成*)
    val f_sig = generate_type_id (TS.getTy e) 

    val (internal_function_name,function_index) =
        case SEnv.find (!function_ids,f_sig)  of 
        NONE=> (function_ids := SEnv.insert (!function_ids,f_sig,0);
        (f_sig ^ "0",0)) 
        |SOME count =>(
        function_ids :=SEnv.insert(!function_ids,f_sig,count+1) ;
        (f_sig ^(Int.toString count),0)) 
    val K = [
          I.i32const function_index,
          I.localset(IDX.localidx(IDX.text_id "__fptr"))
          ] @ (foldr (fn ((v_name,offset,_),iseq)=>[
          I.localget(IDX.localidx (IDX.text_id v_name)),
          I.i32store(I.memarg(offset,0w4)),
          I.localget(IDX.localidx ( IDX.text_id "closure_ptr"))]@iseq
         ) [] closure_name_offset_size)  @K 
        (*クロージャをlinear メモリに保管するための領域を確保*)
    val K =
        I.i32const closure_size::
        I.call(IDX.funcidx(IDX.text_id "alloc" ))::
        I.localtee(IDX.localidx(IDX.text_id "closure_ptr" ))::K
        (*渡されてきたクロージャをローカル変数に展開する*)
    val f_closure_load =foldr (fn ((name,offset,size),iseq)=>iseq@[
          I.localget (IDX.localidx (IDX.text_id f)),
          I.i32load (I.memarg(offset,0w4)),
          I.localset(IDX.localidx(IDX.text_id name))
        ] ) [] closure_name_offset_size (*ローカル変数の展開*)
        (*関数の定義に必要なものがそろった*)
    val (f_K,module,memoffset) = compile_expr e_inner [] module memoffset
    val f_body=f_closure_load@f_K
    val locals =(map (fn (v_name,v_ty)=>I.local_(SOME(IDX.localidx(IDX.text_id v_name)),v_ty)) prelude) @ internal_local_vars


    val function =(    SOME(IDX.funcidx(IDX.text_id internal_function_name)),I.with_functype(IDX.typeidx(IDX.text_id (generate_type_id (TS.getTy e))) ,[
          I.param(f,I.numtype I.i32)(*クロージャのポインタ*)
          ,I.param(x,CMLTyTovaltype var_ty)],[I.result(CMLTyTovaltype return_ty)]),locals,f_body)
    val filter_fn =fn (SOME (IDX.tableidx (IDX.text_id a )),I.tabletype (b)) => a =f_sig
    fun table_and_funcs {ta=tables,fn_=funcs,...}=
          (
           (case List.filter filter_fn tables of 
            nil=> (SOME(IDX.tableidx (IDX.text_id f_sig )),I.tabletype (I.min 0w1 , I.funcref ))::tables
            |[(table_id,I.tabletype(I.min limit,reftype))] =>(table_id,I.tabletype (I.min (limit+0w1) ,reftype ))::tables )
          ,function::funcs
        ) 
    val (new_ta,new_fn) = table_and_funcs module           
      in
      (K,{ty= # ty module , me = #me module , gl = #gl module , el = #el module, da = #da module, im = #im module, ex = #ex module,st = #st module,ta=new_ta,fn_=new_fn},memoffset)
  end


    (*__start 関数の中に宣言と対応する命令列を積む
    compile_declarations Dec list -> (I.local list , expr list , module ,offset)      
    compile_expr:TypedSyntax.typed_exp-> WasmModule instruction list-> WasmModule.module -> int ->(WasmModule.instruction list * WasmModule.module * int ) *)
    fun compile_declarations [] = (internal_local_vars,[],I.emptyModule,0)
    | compile_declarations (x::xs) = let
     val  TS.VAL(name,e) =x
     val (locals,instructions,module,offset)=compile_declarations xs  
     in
      let val (iseq,module,offset)= compile_expr e [] module offset in 
        ((I.local_(SOME(IDX.localidx(IDX.text_id name)),CMLTyTovaltype( TS.getTy e)))::locals,iseq::instructions,module,offset)
      end 
    end  

    (*__cml_main 関数を作る. *)     
    fun compile declarations =
     let
      val (locals,wasm_expr_list,module,_)=compile_declarations declarations
      val local_and_exprs=ListPair.zip (locals,wasm_expr_list) 
      val iseq= foldr (fn ((I.local_(SOME idx,val_ty),expr),iseq)=>iseq@expr@[(I.localset(idx)
      )] ) [] local_and_exprs
      val f =(SOME(IDX.funcidx(IDX.text_id("__cml_main"))),I.with_functype(IDX.typeidx(IDX.text_id("entry_point")),[],[]),locals,iseq) 
      val new_module={ty = #ty module,fn_ = (f::(#fn_ module)),ta = #ta module,me = #me module,gl = #gl module,el = #el module ,da = #da module,im = #im module,ex= #ex module,st = #st module}
     in 
     (*let's build function.!!!! *)
      print ( "Compiled to:\n" ^ I.moduleToString new_module ^ "\n");
     
     new_module
    end 
  end
end