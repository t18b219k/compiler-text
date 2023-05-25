structure WasmType =
struct
  datatype num_ty = I32|I64|F32|F64
  datatype vec_ty = v128
  datatype ref_ty = funcref|externref
  datatype value_ty = number of num_ty | vector of vec_ty | reference of ref_ty
  datatype result_ty =types of  value_ty list
  datatype limit = min of word32 | minmax of word32*word32
  type fun_ty = result_ty * result_ty

  fun dump_word32 x =
  let val c_helper=_import "dump_word32":(word32,word8 vector)->() in 
    (*最大で5バイトになり得るので要素数が可変でないため5バイトSML#で確保してC関数に渡す *)
    let val buffer =Vector.fromList[0w0,0w0,0w0,0w0,0w0]in
      c_helper (x ,buffer);
      Vector.foldr (fn (element,l) =>element::l ) [] buffer
    end
  end  
  
  fun dump_limit limit= case limit of 
    min(n)=>[0wx00:word8]@(dump_word32 n)
    |minmax(n,m)=>[0wx01]@(dump_word32 n)@(dump_word32 m) 
  fun dump_num_ty t = case t of 
  I32 =>[0wx7f:word8]
  |I64 =>[0wx7e]
  |F32 =>[0wx7d]
  |F64 =>[0wx7c]
  fun dump_vec_ty t =[0wx7b:word8]
  fun dump_ref_ty t= case t of
    funcref=>[0wx70:word8] 
    |externref=>[0wx6f]
  fun dump_value_ty t= case t of 
    number(t) =>dump_num_ty t 
    |vector(t)=>dump_vec_ty t
    |reference(t)=>dump_ref_ty t 
  fun dump_result_ty ty_list =
  case ty_list of 
  types(ty_list)=>foldr (op @) [] (map dump_value_ty ty_list)
  fun dump_fun_ty (rt1,rt2)=[0wx60:word8]@dump_result_ty rt1 @ dump_result_ty rt2
end
