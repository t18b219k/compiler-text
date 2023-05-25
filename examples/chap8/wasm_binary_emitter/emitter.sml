(*
WASM バイナリの出力
*)
structure Emitter = 
struct
    datatype numtype = i32|i64|f32|f64
    datatype typeidx = typeidx of word32
    datatype funcidx = funcidx of word32 
    datatype tableidx = tabledidx of word32 
    datatype memidx = memidx of word32 
    datatype globalidx = globalidx of word32 
    datatype elemidx = elemidx of word32 
    datatype dataidx = dataidx of word32 
    datatype localidx = localidx of word32
    datatype labelidx = labelidx of word32
    datatype section_kind = custom|ty|import|function|table|memory|global|export|start|element|code|data|data_count
    datatype custom_section =custom_section of {bytes:word8 array,name:string}
    datatype type_section = type_section of {functypes:functype list}
    datatype import_desc = func of typeidx | table of tabletype| mem of memtype | global of globaltype

    type import_section = {
        import:{
            module:string,
            name:string
        },
        desc:import_desc
    }
    datatype function_section = function_section of  typeidx list
    
    type table = tabletype
    datatype table_section = table_section of table list 
    
    type mem = memtype
    datatype memory_section = memory_section of mem list 
    
    datatype instruction = unreachable|nop
    |block of blocktype * instruction list
    |loop of blocktype * instruction list
     
    datatype blocktype = none | valtype of valtype | s33 of int64
    
    fun dump_blocktype bt = (case bt of
        none => [0w0:word8]
        |valtype(vt) =>case vt of 
            numtype(numtype) => dump_numtype numtype
        |s33(x)=>[0w0] 
    )
    fun dump_instruction i = case i of 
        unreachable=>[0wx0:word8]
        |nop => [0wx01]
        |block(bt , il)=>0wx02 ::  foldr (map dump_instruction il)  ::[0wx0b]
        |loop ( bt , il)=>0wx03 :: foldr (map dump_instruction il) ::[0wx0b]
        

    type 'a section ={kind:section_kind,size:word32,content:'a}    
    fun section_id kind =case kind of 
            custom =>0w0:word8
            |ty=>0w1
            |import=>0w2
            |function=>0w3
            |table=>0w4
            |memory=>0w5
            |global=>0w6
            |export=>0w7
            |start=>0w8 
            |element=>0w9
            |code=>0w10
            |data=>0w11
            |data_count=>0w12
end