

structure WasmComp =
struct
  local 
    val function_ids = ref SEnv.empty 
    val function_sigs=ref SEnv.empty
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
    (I.local_(SOME(IDX.localidx(IDX.text_id "old_pair_addr")),I.numtype I.i32)),
    (I.local_(SOME(IDX.localidx(IDX.text_id "str_ptr")),I.numtype I.i32)),
    (I.local_(SOME(IDX.localidx(IDX.text_id "closure_ptr")),I.numtype I.i32)),
    (I.local_(SOME(IDX.localidx(IDX.text_id "__fptr")),I.numtype I.i32))]
  (*型付き抽象構文木の自由変数を求める*)
  fun fv e=case e of 
    TS.EXPID(x,ty)=>SEnv.singleton (x,ty)
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
    |_=>SEnv.empty    
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
  (*Kは継続　(後に続く計算.)*)
  fun compile_expr e K module memoffset= case e of 
  TS.INT i =>((I.i32const i) ::K,module,memoffset)
  (*文字列はヒープのポインタとサイズのペア*)
  |TS.STRING s =>
  let val data_count = length (#da module) 
     val string_data = I.data(SOME(IDX.dataidx(IDX.text_id("string_"^Int.toString data_count))),I.active_data_mode(IDX.memidx(IDX.text_id "linear_memory"),[I.i32const memoffset]),s ) 
     val new_module = {ty = #ty module,im = #im module,fn_ = #fn_ module ,ta = #ta module ,me = #me module ,gl = #gl module,ex = #ex module,st = #st module,el= #el module ,da = (string_data::  #da module) }in 
    (*
    0 top ,... bottom
    1 top str_ptr,... bottom
    2 top ptr,str_ptr,... bottom
    3 top ,... bottom
    4 top str_ptr,... bottom 
    5 top size,str_ptr,... bottom
    6 top ,... bottom 
    7 top str_ptr,... bottom
    *)
    ([
      (I.i32const 8),
      (I.call (IDX.funcidx(IDX.text_id "alloc"))),
      (I.localtee(IDX.localidx(IDX.text_id "str_ptr"))),
      (I.i32const memoffset),
      (I.i32store(I.memarg(0w0,0w4))),
      (I.localget(IDX.localidx(IDX.text_id "str_ptr"))),
      (I.i32const (size s)),
      (I.i32store(I.memarg(0w4,0w4))),
      (I.localget(IDX.localidx(IDX.text_id "str_ptr")))
      ]@K,new_module,memoffset + (size s)   )
  end  
  
  |TS.TRUE=>((I.i32const 1) ::K,module ,memoffset)
  
  |TS.FALSE=>((I.i32const 0) ::K,module ,memoffset)

  |TS.EXPID(id,_)=>(I.localget (IDX.localidx(IDX.text_id id))::K,module,memoffset)
  (*
        計算の順序, この計算が終わった後にはスタックにはペアのポインタのみが残る.
        ネストしたペアについて考えなければならない.
        ネストしたペアではpair_addr は適切に保存されなければならない.
        e1 ,e2 はスタックにただ一つ表すその計算結果を表す値を積む.
        0 top ... bottom
        1 pair_addr ローカル変数を退避 top ,... bottom 
        1 ヒープを割り当てる. top pair_addr,... bottom
        2 要素1を計算.        top v1 ,pair_addr,... bottom
        3 ヒープに要素1を保存 top　old_pair_addr,... bottom 
        4 ペアのアドレスをスタックに積む top pair_addr,... bottom
        5 要素2を計算         top v2,pair_addr,... bottom 
        6 ヒープに要素2を保存 top ,... bottom      
        7 ペアのアドレスをスタックに積む top pair_addr ,... bottom       
        8 pair_addr ローカル変数を復元 top pair_addr,...bottom 
  *)
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
        val old_pair_addr = IDX.localidx(IDX.text_id "old_pair_addr")

        val (K2,module,memoffset)
         = compile_expr e2 ([e2_store,I.localget pair_addr,I.localget old_pair_addr,I.localset pair_addr]@K) module memoffset
        val (K1,module,memoffset) 
        = compile_expr e1 ([e1_store,I.localget pair_addr]@K2)  module memoffset
        in 
            ([I.localget pair_addr,I.localset old_pair_addr,I.i32const size_of_pair]@alloc_call::K1,module,memoffset)
        end 
    (*pair #1は (pair_addr)->#1して読みだす.
       0 top pair_addr,... bottom
       1 top #2 ,... bottom 
    *)
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
    (*pair #2は#1のlinear memory のバイト数オフセットして読みだす.
       0 top pair_addr,... bottom
       1 top #2 ,... bottom 
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
    (*0 top ,... bottom 
      1 top x1,... bottom 
      2 top x2,x1 ,... bottom
      3 top x1 prim x2 ,... bottom
    *)
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
      テーブル名は型から生成. generate_type_id 
      0 top ,... bottom
      1 top 0 or 1 ,... bottom 
      2 top e2 or e3 value,... bottom 
      *)
    |TS.EXPIF (e1, e2, e3) => 
    let val ty = TS.getTy e2 
        val bt = CMLTyToblocktype ty 
        val (e2K,module,memoffset)= compile_expr e2 nil module memoffset  
        val (e3K,module,memoffset)= compile_expr e3 nil module memoffset in  
          compile_expr e1 (I.if_(NONE,bt,e2K, e3K) :: K) module memoffset
        end   

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
    0 top ,... bottom
    1 top closure_ptr,... bottom
    2 top arg,closure_ptr,... bottom
    3 top ftbl,arg,closure_ptr,... bottom 
    4 top calicurated value ,... bottom  
            |引数　　　　　　　|       =>top |関数ポインタ      |
            |クロージャポインタ|             |引数|
            |      ...         |             |クロージャポインタ|
                                              |...              |
          *)

      let val ft = FUNty(TS.getTy e_2,ty)  
          val call =I.call_indirect(IDX.tableidx(IDX.text_id(generate_type_id ft)),I.name_only(IDX.typeidx(IDX.text_id(generate_type_id ft))))
          val (K,module,memoffset) =compile_expr e_2 
          ([I.localget(IDX.localidx(IDX.text_id "__fptr")),I.i32load(I.memarg(0w0,0w4)),call]@K) module memoffset  
          in
           compile_expr e_1 (I.localtee(IDX.localidx (IDX.text_id "__fptr"))::K) module memoffset 
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
    val f_sig_in_cml = ([I.param(f,I.numtype I.i32),I.param(x,CMLTyTovaltype var_ty)],[I.result(CMLTyTovaltype return_ty)])

    val (internal_function_name,function_index) =
        case SEnv.find (!function_ids,f_sig)  of 
        NONE=> (function_ids := SEnv.insert (!function_ids,f_sig,0);
        (f_sig ^ "0",0)) 
        |SOME count =>(
        function_ids :=SEnv.insert(!function_ids,f_sig,count+1) ;
        (f_sig ^(Int.toString count),count)) 
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


    val function =(    SOME(IDX.funcidx(IDX.text_id internal_function_name)),I.with_functype(IDX.typeidx(IDX.text_id (generate_type_id (TS.getTy e))),#1 f_sig_in_cml ,#2 f_sig_in_cml),locals,f_body)
    val filter_fn =fn (SOME (IDX.tableidx (IDX.text_id a )),I.tabletype (b)) => a =f_sig
    fun table_and_funcs {ta=tables,fn_=funcs,...}=
          (
           (case List.filter filter_fn tables of 
            nil=> (SOME(IDX.tableidx (IDX.text_id f_sig )),I.tabletype (I.min 0w1 , I.funcref ))::tables
            |(table_id,I.tabletype(I.min limit,reftype))::tables =>(table_id,I.tabletype (I.min (limit+0w1) ,reftype ))::tables
            |(table_id,I.tabletype(I.minmax (min,max),reftype))::tables =>(table_id,I.tabletype (I.minmax (min+0w1,max+0w1) ,reftype ))::tables  )
          ,function::funcs
        ) 
    val (new_ta,new_fn) = table_and_funcs module           
      in
      function_sigs := SEnv.insert(!function_sigs,f_sig,f_sig_in_cml);
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
    (*alloc 
      fn alloc(size:i32)->i32{
        let old=alloc_ptr;
        alloc_ptr+= size;
        old
      }
      (local $old i32 )
      global.get alloc_ptr
      local.set $old 
      local.get $old 
      local.get $size 
      i32.add 
      global.set $alloc_ptr
      local.get $old
     *)
    val alloc =let 

      val alloc_iseq = [
        I.globalget(IDX.globalidx(IDX.text_id "alloc_ptr")),
        I.localset(IDX.localidx(IDX.text_id "old")),
        I.localget(IDX.localidx(IDX.text_id "old")),
        I.localget(IDX.localidx(IDX.text_id "size")),
        I.i32add,
        I.globalset(IDX.globalidx(IDX.text_id "alloc_ptr")),
        I.localget(IDX.localidx(IDX.text_id "old"))
      ]
      in
        (SOME(IDX.funcidx(IDX.text_id "alloc")),I.with_functype(IDX.typeidx(IDX.text_id "alloc_sig"),[(I.param ("size",I.numtype I.i32))],[(I.result (I.numtype I.i32))]),[(I.local_(SOME (IDX.localidx(IDX.text_id "old")),(I.numtype I.i32)))],alloc_iseq) 
      end 
    (*__cml_main 関数を作る. *)     
    fun compile declarations =
     let
      val ty_env = foldr (fn (decl,ty_env)=> 
        let val TypedSyntax.VAL(id,expr)=decl in
          (id,TypedSyntax.getTy expr)
        end 
      ::ty_env) [] declarations 
      val (locals,wasm_expr_list,module,memoffset)=compile_declarations declarations
      val alloc_sig = I.type_definition (SOME "alloc_sig", I.functype ([I.param("size",I.numtype I.i32)], [(I.result(I.numtype I.i32))]))
      val alloc_ptr = (SOME (IDX.globalidx(IDX.text_id "alloc_ptr")),I.mutable(I.numtype I.i32),[I.i32const 0])
      val mem =I.import("env","linear_memory",I.m(SOME (IDX.memidx(IDX.text_id "linear_memory")),I.memtype(I.min 0w2)))

      val module = { ty = alloc_sig::(#ty module), im = mem::(#im module), fn_ = alloc::(#fn_ module) , ta = #ta module , me = #me module, gl = alloc_ptr::(#gl module) , ex = #ex module, st = #st module, el = #el module, da = #da module  }
      val local_and_exprs=ListPair.zip (locals,wasm_expr_list) 

      val (module,iseq,memoffset)= foldr (fn ((I.local_(SOME idx,val_ty),expr),(module,iseq,memoffset))=>
        let 
          val SOME (id,ty)=List.find (fn elem=> case idx of IDX.localidx(IDX.text_id id)=> id = #1 elem|_=>raise Unreachable ) ty_env
          val (module,print,memoffset)=Debug.insert_debug_instructions(id,ty,module,memoffset) 
        in 
          (module,iseq@expr@(I.localset(idx))::print,memoffset)
        end
      ) (module,[],memoffset) local_and_exprs
      (*WASM32 では4バイトアラインであるからこのようにする.*)
      (*初めにグローバル変数 alloc_ptr の初期化を行う.*)
      val aligned_initial_alloc_ptr = ((memoffset div 4)+1)*4
      val iseq  = [I.i32const aligned_initial_alloc_ptr ,I.globalset(IDX.globalidx(IDX.text_id "alloc_ptr"))]@iseq
      val f =(SOME(IDX.funcidx(IDX.text_id "__cml_main" )),I.with_functype(IDX.typeidx(IDX.text_id "entry_point" ),[],[]),locals,iseq) 
      val module={ty = #ty module,fn_ = (f::(#fn_ module)),ta = #ta module,me = #me module,gl = #gl module,el = #el module ,da = #da module,im = #im module,ex= #ex module,st = #st module}
      val types = foldr (fn ((SOME (IDX.tableidx (IDX.text_id tid)),t_type),other)=>
      let val SOME f_sig_in_cml =SEnv.find(!function_sigs,tid)in 
       I.type_definition (SOME tid,I.functype(#1 f_sig_in_cml,#2 f_sig_in_cml))::other
      end ) [] (#ta module)
      fun f_names f_prefix 0 = [f_prefix^ "0"]
      | f_names f_prefix i =if i <0 then raise Unreachable else  (f_prefix ^ (Int.toString i))::(f_names f_prefix (i-1))
      val elements =
       foldr (fn ((SOME(IDX.tableidx (IDX.text_id tid)),tt),xs)=>
       let 
       val SOME count = SEnv.find(!function_ids,tid) 
       val f_names =(f_names tid count)  
       val _ = print "successfully generate f_names"
        in (
        I.element(
        NONE,I.elemlist(I.funcref,
        foldl 
        (
          fn (a,k)=>  k@[(I.elemexpr[I.reffunc (IDX.funcidx (IDX.text_id a))])])
        []
        f_names
        ),
        I.active_elem_mode(IDX.tableidx (IDX.text_id tid),[I.i32const 0])
      )::xs) end ) [] (#ta module)
      val module
      =Debug.install_system_functions {ty = types@(#ty module),fn_ = #fn_ module,ta = #ta module,me = #me module,gl = #gl module,el = elements ,da = #da module,im = #im module,ex= #ex module,st = #st module}
     in 
      print ( "Compiled to:\n" ^ I.moduleToString module ^ "\n");
     module
    end 
  end
end