structure WasmModule=
struct
    datatype typeidx  =int_id of word32|text_id of string
    fun typeidxToString x= case x of 
        int_id(x) => Word32.toString x
        |text_id(x)=>"$"^x 
    datatype funcidx =int_id of word32|text_id of string
        fun funcidxToString x= case x of 
        int_id(x) => Word32.toString x
        |text_id(x)=>"$"^x 
    datatype tableidx = int_id of word32|text_id of string
        fun tableidxToString x= case x of 
        int_id(x) => Word32.toString x
        |text_id(x)=>"$"^x 
    datatype memidx = int_id of word32|text_id of string
        fun memidxToString x= case x of 
        int_id(x) => Word32.toString x
        |text_id(x)=>"$"^x 
    datatype globalidx = int_id of word32|text_id of string
        fun globalidxToString x= case x of 
        int_id(x) => Word32.toString x
        |text_id(x)=>"$"^x 
    datatype elemidx = int_id of word32|text_id of string
        fun elemidxToString x= case x of 
        int_id(x) => Word32.toString x
        |text_id(x)=>"$"^x 
    datatype dataidx = int_id of word32|text_id of string
        fun dataidxToString x= case x of 
        int_id(x) => Word32.toString x
        |text_id(x)=>"$"^x 
    datatype localidx = int_id of word32|text_id of string
        fun localidxToString x= case x of 
        int_id(x) => Word32.toString x
        |text_id(x)=>"$"^x 
    datatype labelidx = int_id of word32|text_id of string
    fun labelidxToString x= case x of 
        int_id(x) => Word32.toString x
        |text_id(x)=>"$"^x 
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
    datatype memtype = memtype of limits
    datatype tabletype = tabletype of limits*reftype
    datatype globaltype = const of valtype | mutable of valtype

    datatype comment = line of string | block of string
    datatype label = label of string

    datatype blocktype = blocktype of result option 
    fun blocktypeToString bt = case bt of 
    blocktype(NONE)=>""
    |blocktype(SOME(r))=>resultToString r 
    datatype instruction =
     (*制御構造*)
     block of label option * blocktype * instruction list
    |loop  of label option * blocktype * instruction list
    |if_    of label option * blocktype * instruction list* instruction list
    |nop
    |unreachable
    |br of label
    |br_if of label
    |br_table of label list* label
    |return
    |call of label
    |call_indirect of label * label
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
    fun instructionToString inst = case inst of
    block( l , bt , iseq)=>"block " ^ (case l of 
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
    end