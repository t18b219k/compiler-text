structure IDX = struct
    datatype idx =
        int_id of word32
    | text_id of string

    fun idxToString (int_id w) = Word32.toString w
        | idxToString (text_id s) = "$" ^ s

    datatype typeidx = typeidx of idx

    fun typeidxToString (typeidx idx) = idxToString idx

    datatype funcidx = funcidx of idx

    fun funcidxToString (funcidx idx) = idxToString idx

    datatype tableidx = tableidx of idx

    fun tableidxToString (tableidx idx) = idxToString idx

    datatype memidx = memidx of idx

    fun memidxToString (memidx idx) = idxToString idx

    datatype globalidx = globalidx of idx

    fun globalidxToString (globalidx idx) = idxToString idx

    datatype elemidx = elemidx of idx

    fun elemidxToString (elemidx idx) = idxToString idx

    datatype dataidx = dataidx of idx

    fun dataidxToString (dataidx idx) = idxToString idx

    datatype localidx = localidx of idx

    fun localidxToString (localidx idx) = idxToString idx

    datatype labelidx = labelidx of idx

    fun labelidxToString (labelidx idx) = idxToString idx
end

structure WasmModule = struct
    datatype numtype =
        i32
    | i64
    | f32
    | f64

    fun numtypeToString x =
        case x of
            i32 => "i32"
        | i64 => "i64"
        | f32 => "f32"
        | f64 => "f64"

    datatype vectype = v128

    fun vectypeToString v128 = "v128"

    datatype reftype =
        funcref
    | externref

    datatype heaptype =
        func
    | extern

    fun reftypeToString x =
        case x of
            funcref => "funcref"
        | externref => "externref"

    fun heaptypeToString x =
        case x of
            func => "func"
        | extern => "extern"

    datatype valtype =
        numtype of numtype
    | vectype of vectype
    | reftype of reftype

    fun valtypeToString x =
        case x of
            numtype x => numtypeToString x
        | vectype x => vectypeToString x
        | reftype x => reftypeToString x

    datatype param = param of string * valtype

    fun paramToString (param (id, ty)) = "(param $" ^ id ^ " " ^ valtypeToString ty ^ ")"

    datatype result = result of valtype

    fun resultToString (result ty) = "(result " ^ valtypeToString ty ^ ")"

    datatype functype = functype of param list * result list

    fun functypeToString (functype (params, results)) = "(func " ^ (foldr (op ^) "" (map paramToString params)) ^ " " ^ (foldr (op ^) "" (map resultToString results)) ^ ")"

    datatype limits =
        min of word32
    | minmax of word32 * word32

    fun limitsToString l =
        case l of
            min n => Word32.toString n
        | minmax (n, m) => Word32.toString n ^ " " ^ Word32.toString m

    datatype memtype = memtype of limits

    fun memtypeToString (memtype l) = limitsToString l

    datatype tabletype = tabletype of limits * reftype

    fun tabletypeToString (tabletype (l, rt)) = limitsToString l ^ " " ^ reftypeToString rt

    datatype globaltype =
        const of valtype
    | mutable of valtype

    fun globaltypeToString gt =
        case gt of
            const vt => valtypeToString vt
        | mutable vt => "(mut " ^ valtypeToString vt ^ " )"

    datatype comment =
        line of string
    | block of string

    datatype label = label of string

    datatype blocktype = blocktype of result option

    fun blocktypeToString bt =
        case bt of
            blocktype NONE => ""
        | blocktype (SOME r) => resultToString r

    datatype memarg = memarg of word32 * word32

    fun memargToString (memarg (offset, align)) = "offset=" ^ Word32.toString offset ^ " " ^ "align=" ^ Word32.toString align

    datatype typeuse =
        name_only of IDX.typeidx
    | with_functype of IDX.typeidx * param list * result list

    datatype type_definition = type_definition of string option * functype

    fun type_definitionToString (type_definition (id, ft)) = "(type " ^ (case id of
        SOME id => "$" ^ id
    | NONE => "") ^ functypeToString ft ^ ")"

    fun typeuseToString ty_use =
        case ty_use of
            name_only id => "(type " ^ IDX.typeidxToString id ^ ")"
        | with_functype (id, params, results) => "(type " ^ IDX.typeidxToString id ^ ")" ^ (foldr (op ^) "" (map paramToString params)) ^ " " ^ (foldr (op ^) "" (map resultToString results))

    datatype instruction =
        block_i of label option * blocktype * instruction list
    | loop of label option * blocktype * instruction list
    | if_ of label option * blocktype * instruction list * instruction list
    | nop
    | unreachable
    | br of IDX.labelidx
    | br_if of IDX.labelidx
    | br_table of IDX.labelidx list * IDX.labelidx
    | return
    | call of IDX.funcidx
    | call_indirect of IDX.tableidx * typeuse
    | drop
    | select
    | localget of IDX.localidx
    | localset of IDX.localidx
    | localtee of IDX.localidx
    | globalget of IDX.globalidx
    | globalset of IDX.globalidx
    | tableget of IDX.tableidx
    | tableset of IDX.tableidx
    | tablesize of IDX.tableidx
    | tablegrow of IDX.tableidx
    | tablefill of IDX.tableidx
    | tablecopy of IDX.tableidx * IDX.tableidx
    | tableinit of IDX.tableidx * IDX.elemidx
    | elemdrop of IDX.elemidx
    | i32load of memarg
    | i64load of memarg
    | f32load of memarg
    | f64load of memarg
    | i32load8_s of memarg
    | i32load8_u of memarg
    | i32load16_s of memarg
    | i32load16_u of memarg
    | i64load8_s of memarg
    | i64load8_u of memarg
    | i64load16_s of memarg
    | i64load16_u of memarg
    | i64load32_s of memarg
    | i64load32_u of memarg
    | i32store of memarg
    | i64store of memarg
    | f32store of memarg
    | f64store of memarg
    | i32store8 of memarg
    | i32store16 of memarg
    | i64store8 of memarg
    | i64store16 of memarg
    | i64store32 of memarg
    | memorysize
    | memorygrow
    | memoryfill
    | memorycopy
    | memoryinit of IDX.dataidx
    | datadrop of IDX.dataidx
    | i32const of int32
    | i64const of int64
    | f32const of real32
    | f64const of real
    | i32clz
    | i32ctz
    | i32popcnt
    | i32add
    | i32sub
    | i32mul
    | i32div_s
    | i32div_u
    | i32rem_s
    | i32rem_u
    | i32and
    | i32or
    | i32xor
    | i32shl
    | i32shr_s
    | i32shr_u
    | i32rotl
    | i32rotr
    | i64clz
    | i64ctz
    | i64popcnt
    | i64add
    | i64sub
    | i64mul
    | i64div_s
    | i64div_u
    | i64rem_s
    | i64rem_u
    | i64and
    | i64or
    | i64xor
    | i64shl
    | i64shr_s
    | i64shr_u
    | i64rotl
    | i64rotr
    | f32abs
    | f32neg
    | f32ceil
    | f32floor
    | f32trunc
    | f32nearest
    | f32sqrt
    | f32add
    | f32sub
    | f32mul
    | f32div
    | f32min
    | f32max
    | f32copysign
    | f64abs
    | f64neg
    | f64ceil
    | f64floor
    | f64trunc
    | f64nearest
    | f64sqrt
    | f64add
    | f64sub
    | f64mul
    | f64div
    | f64min
    | f64max
    | f64copysign
    | i32eqz
    | i32eq
    | i32ne
    | i32lt_s
    | i32lt_u
    | i32gt_s
    | i32gt_u
    | i32le_s
    | i32le_u
    | i32ge_s
    | i32ge_u
    | i64eqz
    | i64eq
    | i64ne
    | i64lt_s
    | i64lt_u
    | i64gt_s
    | i64gt_u
    | i64le_s
    | i64le_u
    | i64ge_s
    | i64ge_u
    | f32eq
    | f32ne
    | f32lt
    | f32gt
    | f32le
    | f32ge
    | f64eq
    | f64ne
    | f64lt
    | f64gt
    | f64le
    | f64ge
    | refis_null
    | refnull
    | reffunc of IDX.funcidx

    (*制御構造*)
    fun instructionToString inst =
        case inst of
            block_i (l, bt, iseq) => "block " ^ (case l of
            SOME (label l) => "$" ^ l ^ " "
        | NONE => "") ^ blocktypeToString bt ^ foldr (op ^) "" (map instructionToString iseq) ^ "end"
        | loop (l, bt, iseq) => "loop " ^ (case l of
            SOME (label l) => "$" ^ l
        | NONE => "") ^ blocktypeToString bt ^ foldr (op ^) "" (map instructionToString iseq) ^ "end"
        | if_ (l, bt, iseq_true, iseq_false) => "if " ^ (case l of
            SOME (label l) => "$" ^ l ^ " "
        | NONE => "") ^ blocktypeToString bt ^ "\n" ^ foldr (fn (inst, buf) => inst ^ "\n" ^ buf) "" (map instructionToString iseq_true) ^ "else\n" ^ foldr (fn (inst, buf) => inst ^ "\n" ^ buf) "" (map instructionToString iseq_false) ^ "end"
        | nop => "nop"
        | unreachable => "unreachable"
        | br l => "br " ^ IDX.labelidxToString l
        | br_if l => "br_if " ^ IDX.labelidxToString l
        | br_table (label_list, l) => "br_table " ^ foldr (fn (l, buf) => (IDX.labelidxToString l) ^ "\n" ^ buf) "" label_list ^ " " ^ IDX.labelidxToString l
        | return => "return"
        | call funcidx => "call " ^ IDX.funcidxToString funcidx
        | call_indirect (tableidx, typeuse) => "call_indirect " ^ IDX.tableidxToString tableidx ^ " " ^ typeuseToString typeuse
        | drop => "drop"
        | select => "select"
        | i32const x => "i32.const " ^ Int.toString x
        | i64const x => "i64.const" ^ Int64.toString x
        | f32const x => "f32.const" ^ Real32.toString x
        | f64const x => "f64.const" ^ Real.toString x
        | i32clz => "i32.clz"
        | i32ctz => "i32.ctz"
        | i32popcnt => "i32.popcnt"
        | i32add => "i32.add"
        | i32sub => "i32.sub"
        | i32mul => "i32.mul"
        | i32div_s => "i32.div_s"
        | i32div_u => "i32.div_u"
        | i32rem_s => "i32.rem_s"
        | i32rem_u => "i32.rem_u"
        | i32and => "i32.and"
        | i32or => "i32.or"
        | i32xor => "i32.xor"
        | i32shl => "i32.shl"
        | i32shr_s => "i32.shr_s"
        | i32shr_u => "i32.shr_u"
        | i32rotl => "i32.rotl"
        | i32rotr => "i32.rotr"
        | i64clz => "i64.clz"
        | i64ctz => "i64.ctz"
        | i64popcnt => "i64.popcnt"
        | i64add => "i64.add"
        | i64sub => "i64.sub"
        | i64mul => "i64.mul"
        | i64div_s => "i64.div_s"
        | i64div_u => "i64.div_u"
        | i64rem_s => "i64.rem_s"
        | i64rem_u => "i64.rem_u"
        | i64and => "i64.and"
        | i64or => "i64.or"
        | i64xor => "i64.xor"
        | i64shl => "i64.shl"
        | i64shr_s => "i64.shr_s"
        | i64shr_u => "i64.shr_u"
        | i64rotl => "i64.rotl"
        | i64rotr => "i64.rotr"
        | f32abs => "f32.abs"
        | f32neg => "f32.neg"
        | f32ceil => "f32.ceil"
        | f32floor => "f32.floor"
        | f32trunc => "f32.trunc"
        | f32nearest => "f32.nearest"
        | f32sqrt => "f32.sqrt"
        | f32add => "f32.add"
        | f32sub => "f32.sub"
        | f32mul => "f32.mul"
        | f32div => "f32.div"
        | f32min => "f32.min"
        | f32max => "f32.max"
        | f32copysign => "f32.copysign"
        | f64abs => "f64.abs"
        | f64neg => "f64.neg"
        | f64ceil => "f64.ceil"
        | f64floor => "f64.floor"
        | f64trunc => "f64.trunc"
        | f64nearest => "f64.nearest"
        | f64sqrt => "f64.sqrt"
        | f64add => "f64.add"
        | f64sub => "f64.sub"
        | f64mul => "f64.mul"
        | f64div => "f64.div"
        | f64min => "f64.min"
        | f64max => "f64.max"
        | f64copysign => "f64.copysign"
        | localget idx => "local.get " ^ IDX.localidxToString idx
        | localset idx => "local.set " ^ IDX.localidxToString idx
        | localtee idx => "local.tee " ^ IDX.localidxToString idx
        | i32eq => "i32.eq"
        | i32load ma => "i32.load " ^ memargToString ma
        | i64load ma => "i64.load " ^ memargToString ma
        | f32load ma => "f32.load " ^ memargToString ma
        | f64load ma => "f64.load " ^ memargToString ma
        | i32load8_s ma => "i32.load8_s " ^ memargToString ma
        | i32load8_u ma => "i32.load8_u " ^ memargToString ma
        | i32load16_s ma => "i32.load16_s " ^ memargToString ma
        | i32load16_u ma => "i32.load16_u " ^ memargToString ma
        | i64load8_s ma => "i64.load8_s " ^ memargToString ma
        | i64load8_u ma => "i64.load8_u " ^ memargToString ma
        | i64load16_s ma => "i64.load16_s " ^ memargToString ma
        | i64load16_u ma => "i64.load16_u " ^ memargToString ma
        | i64load32_s ma => "i64.load32_s " ^ memargToString ma
        | i64load32_u ma => "i64.load32_u " ^ memargToString ma
        | i32store ma => "i32.store " ^ memargToString ma
        | i64store ma => "i64.store " ^ memargToString ma
        | f32store ma => "f32.store " ^ memargToString ma
        | f64store ma => "f64.store " ^ memargToString ma
        | i32store8 ma => "i32.store8 " ^ memargToString ma
        | i32store16 ma => "i32.store16 " ^ memargToString ma
        | i64store8 ma => "i64.store8 " ^ memargToString ma
        | i64store16 ma => "i64.store16 " ^ memargToString ma
        | i64store32 ma => "i64.store32 " ^ memargToString ma
        | memorysize => "memory.size"
        | memorygrow => "memory.grow"
        | memoryfill => "memory.fill"
        | memorycopy => "memory.copy"
        | memoryinit dataidx => "memory.init " ^ IDX.dataidxToString dataidx
        | datadrop dataidx => "datadrop " ^ IDX.dataidxToString dataidx
        | tableget tableidx => "table.get " ^ IDX.tableidxToString tableidx
        | tableset tableidx => "table.set " ^ IDX.tableidxToString tableidx
        | tablesize tableidx => "table.size " ^ IDX.tableidxToString tableidx
        | tablegrow tableidx => "table.grow " ^ IDX.tableidxToString tableidx
        | tablefill tableidx => "table.fill " ^ IDX.tableidxToString tableidx
        | tablecopy (tableidx1, tableidx2) => "table.copy " ^ IDX.tableidxToString tableidx1 ^ " " ^ IDX.tableidxToString tableidx2
        | tableinit (tableidx, elemidx) => "table.init " ^ IDX.tableidxToString tableidx ^ " " ^ IDX.elemidxToString elemidx
        | elemdrop elemidx => "elem.drop " ^ IDX.elemidxToString elemidx
        | globalget gid => "global.get " ^ IDX.globalidxToString gid
        | globalset gid => "global.set " ^ IDX.globalidxToString gid
        | i32eqz => "i32.eqz"
        | i32ne => "i32.ne"
        | i32lt_s => "i32.lt_s"
        | i32lt_u => "i32.lt_u"
        | i32gt_s => "i32.gt_s"
        | i32gt_u => "i32.gt_u"
        | i32le_s => "i32.le_s"
        | i32le_u => "i32.le_u"
        | i32ge_s => "i32.ge_s"
        | i32ge_u => "i32.ge_u"
        | i64eqz => "i64.eqz"
        | i64eq => "i64.eq"
        | i64ne => "i64.ne"
        | i64lt_s => "i64.lt_s"
        | i64lt_u => "i64.lt_u"
        | i64gt_s => "i64.gt_s"
        | i64gt_u => "i64.gt_u"
        | i64le_s => "i64.le_s"
        | i64le_u => "i64.le_u"
        | i64ge_s => "i64.ge_s"
        | i64ge_u => "i64.ge_u"
        | f32eq => "f32.eq"
        | f32ne => "f32.ne"
        | f32lt => "f32.lt"
        | f32gt => "f32.gt"
        | f32le => "f32.le"
        | f32ge => "f32.ge"
        | f64eq => "f64.eq"
        | f64ne => "f64.ne"
        | f64lt => "f64.lt"
        | f64gt => "f64.gt"
        | f64le => "f64.le"
        | f64ge => "f64.ge"
        | refis_null => "ref.is_null"
        | refnull => "ref.null"
        | reffunc idx => "ref.func " ^ IDX.funcidxToString idx

    (*| x => raise DumpInstruction x*)
    type expr = instruction list

    fun exprToString_bracketted il = foldl (fn (i, buf) => buf ^ (instructionToString i) ^ "\n") "" il

    fun exprToString il = (foldr (op ^) "" (map (fn i => instructionToString i ^ "\n") il)) ^ "end"

    (*module field elements*)
    datatype import_desc =
        f of IDX.funcidx option * typeuse
    | t of IDX.tableidx option * tabletype
    | m of IDX.memidx option * memtype
    | g of IDX.globalidx option * globaltype

    fun import_descToString desc =
        (case desc of
            f (id, x) => "(func " ^ (case id of
            NONE => ""
        | SOME id => IDX.funcidxToString id) ^ " " ^ typeuseToString x ^ ")"
        | t (id, tt) => "(table " ^ (case id of
            NONE => " "
        | SOME id => IDX.tableidxToString id) ^ " " ^ tabletypeToString tt
        | m (id, mt) => "(memory " ^ (case id of
            NONE => " "
        | SOME id => IDX.memidxToString id) ^ " " ^ memtypeToString mt ^ ")"
        | g (id, gt) => "(global " ^ (case id of
            NONE => " "
        | SOME id => IDX.globalidxToString id) ^ " " ^ globaltypeToString gt ^ ")")

    datatype import = import of string * string * import_desc

    fun importToString (import (module, nm, d)) = "(import \"" ^ module ^ "\" \"" ^ nm ^ "\" " ^ import_descToString d ^ ")"

    datatype local_ = local_ of IDX.localidx option * valtype

    fun local_ToString (local_ (id, vt)) = "(local " ^ (case id of
        NONE => ""
    | SOME id => IDX.localidxToString id) ^ " " ^ valtypeToString vt ^ ")"

    type func = IDX.funcidx option * typeuse * (local_ list) * (instruction list)

    fun funcToString (id, type_use, ll, il) = "(func " ^ (case id of
        NONE => ""
    | SOME idx => IDX.funcidxToString idx) ^ " " ^ typeuseToString type_use ^ "\n" ^ (foldr (op ^) "" (map local_ToString ll)) ^ "\n" ^ (exprToString_bracketted il) ^ ")"

    type table = IDX.tableidx option * tabletype

    fun tableToString (ti, tt) = "(table " ^ (case ti of
        NONE => ""
    | SOME idx => IDX.tableidxToString idx) ^ " " ^ tabletypeToString tt ^ ")"

    type mem = IDX.memidx option * memtype

    fun memToString (id, mt) = "(memory " ^ (case id of
        SOME id => IDX.memidxToString id
    | NONE => "") ^ " " ^ memtypeToString mt ^ ")"

    type global = IDX.globalidx option * globaltype * expr

    fun globalToString (id, gt, e) = "(global " ^ (case id of
        SOME id => IDX.globalidxToString id
    | NONE => "") ^ globaltypeToString gt ^ " " ^ exprToString_bracketted e ^ ")"

    datatype export_desc =
        func_e of IDX.funcidx
    | table_e of IDX.tableidx
    | memory_e of IDX.memidx
    | global_e of IDX.globalidx

    fun export_descToString desc =
        case desc of
            func_e fid => "(func " ^ IDX.funcidxToString fid ^ ")"
        | table_e tid => "(table " ^ IDX.tableidxToString tid ^ ")"
        | memory_e mid => "(memory " ^ IDX.memidxToString mid ^ ")"
        | global_e gid => "(global " ^ IDX.globalidxToString gid ^ ")"

    datatype export = export of string * export_desc

    fun exportToString (export (name, desc)) = "(export \"" ^ name ^ "\" " ^ export_descToString desc ^ ")"

    datatype start = start of IDX.funcidx

    fun startToString (start fid) = "(start " ^ IDX.funcidxToString fid ^ ")"

    datatype elemexpr = elemexpr of expr

    fun elemexprToString (elemexpr e) = "(item " ^ exprToString_bracketted e ^ ")"

    datatype elemlist = elemlist of reftype * elemexpr list

    fun elemlistToString (elemlist (rt, eel)) = reftypeToString rt ^ " " ^ foldr (op ^) "" (map (fn ee => elemexprToString ee ^ "\n") eel)

    datatype elemmode =
        passive_elem_mode
    | active_elem_mode of IDX.tableidx * expr
    | declarative

    fun elemmodeToString em =
        case em of
            passive_elem_mode => " "
        | active_elem_mode (tid, e) => "(table " ^ IDX.tableidxToString tid ^ ")" ^ "(offset " ^ exprToString_bracketted e ^ ")"
        | declarative => "declare"

    datatype elem = element of IDX.elemidx option * elemlist * elemmode

    fun elementToString (element (id, el, em)) = "(elem " ^ (case id of
        NONE => " "
    | SOME id => IDX.elemidxToString id) ^ elemmodeToString em ^ elemlistToString el ^ ")"

    datatype datamode =
        passive_data_mode
    | active_data_mode of IDX.memidx * expr

    fun datamodeToString dm =
        case dm of
            passive_data_mode => " "
        | active_data_mode (id, e) => IDX.memidxToString id ^ "(offset " ^ exprToString_bracketted e ^ ")"

    (*
    とりあえずは文字列が扱えればよし
    どうせ文字列しか埋め込まないから.
    *)
    datatype data = data of IDX.dataidx option * datamode * string

    fun dataToString (data (did, dm, s)) = "(data " ^ (case did of
        NONE => " "
    | SOME id => IDX.dataidxToString id) ^ datamodeToString dm ^ " \"" ^ s ^ "\")"

    (*module fields *)
    type types = type_definition list

    fun typesToString tl = foldr (op ^) "" (map type_definitionToString tl)

    type funcs = func list

    fun funcsToString tl = foldr (op ^) "" (map funcToString tl)

    type tables = table list

    fun tablesToString tl = foldr (op ^) "" (map tableToString tl)

    type mems = mem list

    fun memsToString tl = foldr (op ^) "" (map memToString tl)

    type globals = global list

    fun globalsToString tl = foldr (op ^) "" (map globalToString tl)

    type elems = elem list

    fun elemsToString tl = foldr (op ^) "" (map elementToString tl)

    type datas = data list

    fun datasToString tl = foldr (op ^) "" (map dataToString tl)

    type imports = import list

    fun importsToString tl = foldr (op ^) "" (map importToString tl)

    type exports = export list

    fun exportsToString tl = foldr (op ^) "" (map exportToString tl)

    (*Abstract WASM Module *)
    type module = { ty : types, im : imports, fn_ : funcs, ta : tables, me : mems, gl : globals, ex : exports, st : start, el : elems, da : datas }

    fun moduleToString { ty : types, im : imports, fn_ : funcs, ta : tables, me : mems, gl : globals, ex : exports, st : start, el : elems, da : datas } = "(module " ^ typesToString ty ^ importsToString im ^ funcsToString fn_ ^ tablesToString ta ^ memsToString me ^ globalsToString gl ^ exportsToString ex ^ startToString st ^ elemsToString el ^ datasToString da ^ ")"

    val emptyModule = { ty = [type_definition (SOME "entry_point", functype ([], []))], fn_ = [], ta = [], me = [], gl = [], el = [], da = [], im = [], ex = [], st = start (IDX.funcidx (IDX.text_id "__cml_main")) }
end