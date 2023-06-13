structure ID =
struct 
    datatype idx =int_id of word32 | text_id of string 
end 
structure WasmModule=
struct
    datatype idx =int_id of word32|text_id of string 
    fun idxToString (int_id w)= Word32.toString w
    | idxToString (text_id s)= "$"^s
    datatype typeidx  = typeidx of idx  
    fun typeidxToString (typeidx idx)  =idxToString idx
    datatype funcidx =funcidx of idx 
    fun funcidxToString (funcidx idx)  =idxToString idx
    datatype tableidx = tableidx of idx
    fun tableidxToString (tableidx idx)  =idxToString idx
    datatype memidx  =memidx of idx
    fun memidxToString (memidx idx)  =idxToString idx
    datatype globalidx = globalidx of idx
    fun globalidxToString (globalidx idx)  =idxToString idx
    datatype elemidx = elemidx of idx
    fun elemidxToString (elemidx idx)  =idxToString idx
    datatype dataidx = dataidx of idx
    fun dataidxToString (dataidx idx)  =idxToString idx
    datatype localidx = localidx of idx
    fun localidxToString (localidx idx)  =idxToString idx
    datatype labelidx = labelidx of idx
    fun labelidxToString (labelidx idx)  =idxToString idx

    datatype numtype =i32|i64|f32|f64
    fun numtypeToString x = case x of
    i32=>"i32"
    |i64=>"i64"
    |f32=>"f32"
    |f64=>"f64"
    datatype vectype = v128
    fun vectypeToString x = case x of
    v128=>"v128"
    datatype reftype = funcref | externref    
    datatype heaptype =func|extern
    fun reftypeToString x = case x of
    funcref=>"funcref"
    |externref=>"externref"
    fun heaptypeToString x = case x of 
    func=>"func"
    |extern=>"extern"
    datatype valtype = numtype of numtype | vectype of vectype | reftype of reftype
    fun valtypeToString x= case x of 
    numtype(x)=>numtypeToString x
    |vectype(x)=>vectypeToString x
    |reftype(x)=>reftypeToString x 
    datatype param = param of string * valtype 
    fun paramToString x  = case x of
    param(id,ty)=> "( param "^id^" "^valtypeToString ty^ ")"
    datatype result = result of valtype 
    fun resultToString x  = case x of
    result(ty)=> "( result "^valtypeToString ty^ ")"
    datatype functype = functype of param list *  result list
    fun functypeToString x  = case x of
    functype(params,results)=> "( func "^(foldr (op ^) "" (map paramToString params) )^" "^(foldr (op ^) "" (map resultToString results) )^  ")"
    datatype limits = min of word32 | minmax of word32*word32
    fun limitsToString l=case l of 
    min(n)=>Word32.toString n
    |minmax(n,m)=> Word32.toString n ^" "^Word32.toString m
    datatype memtype = memtype of limits
    fun memtypeToString mt = case mt of 
    memtype(l)=>limitsToString l
    datatype tabletype = tabletype of limits*reftype
    fun tabletypeToString tt = case tt of 
    tabletype(l,rt)=> limitsToString l ^ reftypeToString rt
    datatype globaltype = const of valtype | mutable of valtype
    fun globaltypeToString gt = case gt of 
    const(vt)=>valtypeToString vt 
    |mutable(vt)=>"(mut "^ valtypeToString vt^" )"
    datatype comment = line of string | block of string
    datatype label = label of string

    datatype blocktype = blocktype of result option 
    fun blocktypeToString bt = case bt of 
    blocktype(NONE)=>""
    |blocktype(SOME(r))=>resultToString r 

    datatype memarg =memarg of word32 * word32

    fun memargToString ma =case ma of 
    memarg(offset,align)=>"offset="^Word32.toString offset ^" "^"align="^Word32.toString align

    datatype typeuse = name_only of typeidx | with_functype of typeidx * param list * result list
    
    datatype instruction =
     (*制御構造*)
     block_i of label option * blocktype * instruction list
    |loop  of label option * blocktype * instruction list
    |if_    of label option * blocktype * instruction list* instruction list
    |nop
    |unreachable
    |br of label
    |br_if of label
    |br_table of label list* label
    |return
    |call of funcidx
    |call_indirect of tableidx * typeuse
    |drop
    |select
    (*local variable instruction*)
    |localget of localidx
    |localset of localidx 
    |localtee of localidx
    (*global variable instruction*) 
    |globalget of globalidx
    |globalset of globalidx
    (*table isntructions*)
    |tableget of tableidx
    |tableset of tableidx 
    |tablesize of tableidx
    |tablegrow of tableidx 
    |tablefill of tableidx
    |tablecopy of tableidx*tableidx
    |tableinit of tableidx*elemidx
    |elemdrop of elemidx
    (*memory instruction*)
    |i32load of memarg
    |i64load of memarg
    |f32load of memarg
    |f64load of memarg
    |i32load8_s of memarg
    |i32load8_u of memarg
    |i32load16_s of memarg
    |i32load16_u of memarg 
    |i64load8_s of memarg
    |i64load8_u of memarg
    |i64load16_s of memarg
    |i64load16_u of memarg 
    |i64load32_s of memarg
    |i64load32_u of memarg
    |i32store of memarg
    |i64store of memarg
    |f32store of memarg
    |f64store of memarg
    |i32store8 of memarg
    |i32store16 of memarg
    |i64store8 of memarg
    |i64store16 of memarg
    |i64store32 of memarg
    |memorysize
    |memorygrow
    |memoryfill
    |memorycopy 
    |memoryinit of dataidx
    |datadrop of dataidx
    (*数値演算命令*)
    |i32const of int32 | i64const of int64|f32const of real32|f64const of real
    (*count leading zeros*)
    |i32clz
    |i32ctz
    |i32popcnt
    |i32add
    |i32sub
    |i32mul
    |i32div_s
    |i32div_u
    |i32rem_s
    |i32rem_u
    |i32and
    |i32or
    |i32xor
    |i32shl
    |i32shr_s
    |i32shr_u
    |i32rotl
    |i32rotr
    |i64clz
    |i64ctz
    |i64popcnt
    |i64add
    |i64sub
    |i64mul
    |i64div_s
    |i64div_u
    |i64rem_s
    |i64rem_u
    |i64and
    |i64or
    |i64xor
    |i64shl
    |i64shr_s
    |i64shr_u
    |i64rotl
    |i64rotr
    |f32abs
    |f32neg
    |f32ceil
    |f32floor
    |f32trunc
    |f32nearest
    |f32sqrt
    |f32add
    |f32sub
    |f32mul
    |f32div
    |f32min
    |f32max
    |f32copysign
    |f64abs
    |f64neg
    |f64ceil
    |f64floor
    |f64trunc
    |f64nearest
    |f64sqrt
    |f64add
    |f64sub
    |f64mul
    |f64div
    |f64min
    |f64max
    |f64copysign    
    |i32eqz
    |i32eq
    |i32ne
    |i32lt_s
    |i32lt_u
    |i32gt_s
    |i32gt_u
    |i32le_s
    |i32le_u
    |i32ge_s
    |i32ge_u
    |i64eqz
    |i64eq
    |i64ne
    |i64lt_s
    |i64lt_u
    |i64gt_s
    |i64gt_u
    |i64le_s
    |i64le_u
    |i64ge_s
    |i64ge_u
    |f32eq
    |f32ne
    |f32lt
    |f32gt
    |f32le
    |f32ge
    |f64eq
    |f64ne
    |f64lt
    |f64gt
    |f64le
    |f64ge
    fun instructionToString inst = case inst of
    block_i( l , bt , iseq)=>"block " ^ (case l of 
    SOME(label(l))=>"$"^l
    |NONE=>"")^foldr (op ^) "" (map instructionToString iseq)  ^"end"
    |loop ( l , bt , iseq)=>"loop " ^ (case l of 
    SOME(label(l))=>"$"^l
    |NONE=>"")^foldr (op ^) "" (map instructionToString iseq)  ^"end"
    |if_ ( l , bt , iseq_true,iseq_false)=>"if " ^ (case l of 
    SOME(label(l))=>"$"^l
    |NONE=>"")^foldr (op ^) "" (map instructionToString iseq_true)^"else" ^foldr (op ^) "" (map instructionToString iseq_false)^ "end"
    |i32const(x)=>"i32.const "^ Int.toString x
    |i64const(x)=>"i64.const" ^ Int64.toString x
    |f32const(x)=>"f32.const"^ Real32.toString x
    |f64const(x)=>"f64.const"^ Real.toString x
    (*count leading zeros*)
    |i32clz=>"i32.clz"
    |i32ctz=>"i32.ctz"
    |i32popcnt=>"i32.popcnt"
    |i32add=>"i32.add"
    |i32sub=>"i32.sub"
    |i32mul=>"i32.mul"
    |i32div_s=>"i32.div_s"
    |i32div_u=>"i32.div_u"
    |i32rem_s=>"i32.rem_s"
    |i32rem_u=>"i32.rem_u"
    |i32and=>"i32.and"
    |i32or=>"i32.or"
    |i32xor=>"i32.xor"
    |i32shl=>"i32.shl"
    |i32shr_s=>"i32.shr_s"
    |i32shr_u=>"i32.shr_u"
    |i32rotl=>"i32.rotl"
    |i32rotr=>"i32.rotr"
    |i64clz=>"i64.clz"
    |i64ctz=>"i64.ctz"
    |i64popcnt=>"i64.popcnt"
    |i64add=>"i64.add"
    |i64sub=>"i64.sub"
    |i64mul=>"i64.mul"
    |i64div_s=>"i64.div_s"
    |i64div_u=>"i64.div_u"
    |i64rem_s=>"i64.rem_s"
    |i64rem_u=>"i64.rem_u"
    |i64and=>"i64.and"
    |i64or=>"i64.or"
    |i64xor=>"i64.xor"
    |i64shl=>"i64.shl"
    |i64shr_s=>"i64.shr_s"
    |i64shr_u=>"i64.shr_u"
    |i64rotl=>"i64.rotl"
    |i64rotr=>"i64.rotr"
    |f32abs=>"f32.abs"
    |f32neg=>"f32.neg"
    |f32ceil=>"f32.ceil"
    |f32floor=>"f32.floor"
    |f32trunc=>"f32.trunc"
    |f32nearest=>"f32.nearest"
    |f32sqrt=>"f32.sqrt"
    |f32add=>"f32.add"
    |f32sub=>"f32.sub"
    |f32mul=>"f32.mul"
    |f32div=>"f32.div"
    |f32min=>"f32.min"
    |f32max=>"f32.max"
    |f32copysign=>"f32.copysign"
    |f64abs=>"f64.abs"
    |f64neg=>"f64.neg"
    |f64ceil=>"f64.ceil"
    |f64floor=>"f64.floor"
    |f64trunc=>"f64.trunc"
    |f64nearest=>"f64.nearest"
    |f64sqrt=>"f64.sqrt"
    |f64add=>"f64.add"
    |f64sub=>"f64.sub"
    |f64mul=>"f64.mul"
    |f64div=>"f64.div"
    |f64min=>"f64.min"
    |f64max=>"f64.max"
    |f64copysign=>"f64.copysign"

    type expr =  instruction list
    fun exprToString_bracketted il =  foldr (op ^)""(map (fn i => instructionToString i ^"\n") il)
    fun exprToString il =(foldr (op ^)""(map (fn i => instructionToString i ^"\n") il))^"end"
    (*module field elements*)
    datatype type_definition = type_definition of string option * functype 
    fun type_definitionToString definition = case definition of 
    type_definition(id,ft)=> "(type"^
    (case id of 
    SOME(id)=>id
    |NONE=>"" ^
    functypeToString ft)
    ^")" 

    fun typeuseToString ty_use=case ty_use of 
    name_only(id) =>"(type "^typeidxToString id^")"
    |with_functype(id,params,results)=>"(type "
    ^typeidxToString id 
    ^")"
    ^(foldr (op ^) "" (map paramToString params) )
    ^" "
    ^(foldr (op ^) "" (map resultToString results) )
    datatype import_desc = f of funcidx option  *typeuse | t of tableidx  option* tabletype  |m of memidx option * memtype  |g of globalidx option* globaltype
    fun import_descToString desc = (case desc of 
    f(id,x)=>"(func" ^(case id of 
    NONE=>""
    |SOME(id)=> funcidxToString id)^
    typeuseToString x
    ^ ")"
    |t(id,tt)=>"(table"^(case id of 
    NONE=>" "
    |SOME(id)=> tableidxToString id)^
    tabletypeToString tt
    |m(id,mt)=>"(memory"^(case id of 
    NONE=>" "
    |SOME(id)=> memidxToString id)^
    memtypeToString mt^")"
    |g(id,gt)=>"(global"^(case id of 
    NONE=>" "
    |SOME(id) =>globalidxToString id)^
    globaltypeToString gt
    ^
    ")"
    )
    datatype import = import of string*string*import_desc
    
    fun importToString im = case im of 
    import(module,nm,d)=>"(import"^
    module ^" "^nm ^" " ^import_descToString d
    ^")"
    datatype local_ = local_ of localidx option * valtype
    fun local_ToString l =case l of
    local_(id,vt)=>"(local "^ (case id of 
    NONE=>""
    |SOME(id)=>localidxToString id 
    )
    ^" "^valtypeToString vt ^")"

    type func =  funcidx option * typeuse * local_ list * instruction list 
    fun funcToString (id,type_use,ll,il)= 
     "(func " ^ (case id of
     NONE=>""
     |SOME(idx)=>funcidxToString idx) 
     ^" "
     ^typeuseToString type_use 
     ^"\n"
     ^(foldr (op ^) ""  (map local_ToString ll) )
     ^"\n"
     ^(foldr (op ^) "" (map instructionToString il))
     ^")"
    type table =  tableidx option * tabletype 
    fun tableToString (ti,tt) = "(table "^
    (case ti of 
    NONE=>" "
    |SOME(idx)=>tableidxToString idx
    )^
    tabletypeToString tt
    ^
    ")"
    type mem =  memidx option *memtype
    fun memToString (id,mt)="(memory "^(case id of 
    SOME(id)=> memidxToString id 
    |NONE=>"" )^
    " "
    ^ memtypeToString mt
    ^")"
    type global = globalidx option * globaltype *expr 
    fun globalToString (id,gt,e)= "(global "^(case id of 
    SOME(id)=>globalidxToString id
    |NONE=>"")
    ^globaltypeToString gt 
    ^" "
    ^ exprToString_bracketted e
    ^")"
    datatype export_desc =func_e of funcidx | table_e of tableidx |memory_e of memidx | global_e of globalidx 
    fun export_descToString desc = case desc of 
    func_e(fid)=>"(func "^funcidxToString fid ^")" 
    |table_e(tid)=>"(table "^tableidxToString tid^")"
    |memory_e(mid)=>"(memory "^memidxToString mid^")"
    |global_e(gid)=>"(global "^globalidxToString gid^")"
    datatype export = export of string * export_desc
    fun exportToString ex  =case ex of 
    export(name,desc)=>"(export "^name^export_descToString desc^")"
    datatype start = start of funcidx
    fun startToString s = case s of 
    start(fid)=>"(start "^funcidxToString fid^")"
    datatype elemexpr =elemexpr of expr 
    fun elemexprToString ee = case ee of 
    elemexpr(e)=> "(item "^exprToString_bracketted e ^")" 
    datatype elemlist =elemlist of reftype * elemexpr list
    fun elemlistToString el = case el of 
    elemlist(rt,eel)=> reftypeToString rt ^" "^ foldr (op ^) "" (map (fn ee =>elemexprToString ee ^"\n")  eel)
    datatype elemmode = passive_elem_mode |active_elem_mode of tableidx * expr | declarative
    fun elemmodeToString em =case em of 
    passive_elem_mode=>" "
    |active_elem_mode(tid,e)=>"(table "^tableidxToString tid^")"^"(offset "^exprToString_bracketted e^")"   
    |declarative=>"declare"
    datatype elem = element of elemidx option * elemlist * elemmode
    fun elementToString e =case e of 
    element(id,el,em)=>"(elem "^ (case id of 
    NONE=>" "
    |SOME(id)=>elemidxToString id 
    )
    ^elemmodeToString em 
    ^elemlistToString el
    ^
    ")"
    datatype datamode = passive_data_mode | active_data_mode of memidx * expr 
    fun datamodeToString dm = case dm of 
    passive_data_mode=>" "
    |active_data_mode(id,e)=>memidxToString id ^ "(offset "^exprToString_bracketted e^")" 
    (*とりあえずは文字列が扱えればよし*)
    datatype data = data of dataidx option * datamode *string 
    fun dataToString d=case d of 
    data(did,dm,s)=>"(data "^(case did of 
    NONE=>" "
    |SOME(id)=>dataidxToString id)^datamodeToString dm ^" "^s^")" 
    (*module fields *)
    type types = type_definition list
    fun typesToString tl = foldr (op ^) "" (map type_definitionToString tl)
    type funcs = func list
        fun funcsToString tl = foldr (op ^) "" (map funcToString tl)
    type tables = table list
        fun tablesToString tl = foldr (op ^) "" (map tableToString tl)
    type mems =mem list 
        fun memsToString tl = foldr (op ^) "" (map memToString tl)
    type globals = global list
        fun globalsToString tl = foldr (op ^) "" (map globalToString tl)
    type elems =elem list
        fun elemsToString tl = foldr (op ^) "" (map elementToString tl)
    type datas = data list 
        fun datasToString tl = foldr (op ^) "" (map dataToString tl)

    type imports = import list 
        fun importsToString tl = foldr (op ^) "" (map importToString tl)
    type exports =export list
        fun exportsToString tl = foldr (op ^) "" (map exportToString tl)

    (*Abstract WASM Module *)
    type module = {ty:types,im:imports,fn_:funcs,ta:tables,me:mems,gl:globals,ex:exports,st:start,el:elems,da:datas}
    fun moduleToString {ty:types,im:imports,fn_:funcs,ta:tables,me:mems,gl:globals,ex:exports,st:start,el:elems,da:datas}=
    "( module "^
    typesToString (ty )^
    importsToString (im )^
    funcsToString (fn_ )^
    tablesToString (ta )^ 
    memsToString (me )^
    globalsToString (gl )^
    exportsToString (ex )^
    startToString (st  )^
    elemsToString (el  )^
    datasToString (da )  ^")"
    val emptyModule = {ty=[],fn_=[],ta=[],me=[],gl=[],el=[],da=[],im=[],ex=[],st=start (funcidx(text_id "__start"))}
end