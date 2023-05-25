open Type
open WasmType
structure Comp =
struct
    (*
  val compile : string*TypedInstruction.C -> string * 
  val lower_ty : Type.ty ->*)
  (*CoreML の型からWasmの型に落とす*)
  (*0終端にすると文字列もptr 一個で表せる*)
  fun lower_ty_to_num_ty ty = case ty of 
  INTty =>I64
  |BOOLty => I32
  |STRINGty =>I32
  |FUNty(_,_) => I32
  |PAIRty(_,_)=>I32 
  fun lower_ty_to_value_ty ty =number( case ty of 
    INTty =>I64
  |BOOLty => I32
  |STRINGty =>I32
  |FUNty(_,_) => I32
  |PAIRty(_,_)=>I32 )
  (* 関数は関数ポインタとキャプチャする環境へのポインタ*)
  fun lower_ty_to_result_ty ty=case ty of 
  INTty =>types(([number I64]))
  |BOOLty => types( ([number I32]))
  |STRINGty =>types(([number I32]))
  |FUNty(_,_) => types( ([number I32,number I32]))
  |PAIRty(_,_)=>types( ([number I32,number I32]))
end