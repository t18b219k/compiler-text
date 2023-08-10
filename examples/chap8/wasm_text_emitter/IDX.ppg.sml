structure IDX = struct 
    (*% *)
    datatype idx =(*% 
    * @format(id) id 
    *) int_id of word |
    (*%
    * @format(id) "$" id 
    *)
    text_id of string
fun format_idx  y0 = case y0 of 
int_id x0  => (case x0 of id => 
SMLFormat.BasicFormatters.format_word(id))
 | text_id x0  => (case x0 of id => 
let
val z0 = SMLFormat.BasicFormatters.format_string(id)
in SMLFormat.FormatExpression.Term(1,"$")::z0 end)
 
end 
structure TypeIDX = struct 
    (*% @formatter(IDX.idx) IDX.format_idx*)
        datatype TypeIDX =(*% @format(id) id *)new of  IDX.idx
fun format_TypeIDX  y0 = case y0 of 
new x0  => (case x0 of id => 
IDX.format_idx(id))

end 
structure FuncIDX = struct 
    (*% @formatter(IDX.idx) IDX.format_idx*)
    datatype FuncIDX =(*% @format(id) id *)new of  IDX.idx
fun format_FuncIDX  y0 = case y0 of 
new x0  => (case x0 of id => 
IDX.format_idx(id))

end 
structure TableIDX = struct 
    (*% @formatter(IDX.idx) IDX.format_idx*)
    datatype TableIDX =(*% @format(id) id *)new of  IDX.idx
fun format_TableIDX  y0 = case y0 of 
new x0  => (case x0 of id => 
IDX.format_idx(id))

end 
structure MemIDX = struct 
    (*% @formatter(IDX.idx) IDX.format_idx*)
    datatype MemIDX=(*% @format(id) id *)new of  IDX.idx
fun format_MemIDX  y0 = case y0 of 
new x0  => (case x0 of id => 
IDX.format_idx(id))

end 
structure GlobalIDX = struct 
    (*% @formatter(IDX.idx) IDX.format_idx*)
    datatype GlobalIDX =(*% @format(id) id *)new of  IDX.idx
fun format_GlobalIDX  y0 = case y0 of 
new x0  => (case x0 of id => 
IDX.format_idx(id))

end
structure ElemIDX = struct 
    (*% @formatter(IDX.idx) IDX.format_idx*)
    datatype ElemIDX =(*% @format(id) id *)new of  IDX.idx
fun format_ElemIDX  y0 = case y0 of 
new x0  => (case x0 of id => 
IDX.format_idx(id))
 
end 
structure DataIDX =struct 
    (*% @formatter(IDX.idx) IDX.format_idx*)
    datatype DataIDX = (*% @format(id) id *)new of  IDX.idx
fun format_DataIDX  y0 = case y0 of 
new x0  => (case x0 of id => 
IDX.format_idx(id))

end
structure LocalIDX = struct 
    (*% @formatter(IDX.idx) IDX.format_idx*)
    datatype LocalIDX =(*% @format(id) id *)new of  IDX.idx
fun format_LocalIDX  y0 = case y0 of 
new x0  => (case x0 of id => 
IDX.format_idx(id))

end 
structure LabelIDX = struct 
    (*% @formatter(IDX.idx) IDX.format_idx*)
    datatype LabelIDX =(*% @format(id) id *)new of  IDX.idx
fun format_LabelIDX  y0 = case y0 of 
new x0  => (case x0 of id => 
IDX.format_idx(id))

end 
