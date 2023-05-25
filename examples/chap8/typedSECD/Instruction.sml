open Type;
structure TypedInstruction = 
struct
  type ty =Type.ty
  datatype prim = EQ | ADD | SUB | MUL | DIV
  datatype inst 
    = PushI of int | PushS of string | PushB of bool
    | Acc of string * ty| App of ty| Pair of ty| Proj1 of ty| Proj2 of ty
    | Prim of prim | MkCLS of string * inst list *ty
    | MkREC of string * string * inst list *ty
    | If of inst list * inst list *ty| Ret of ty
  type C = inst list

  fun instToString inst = Dynamic.format inst
  fun codeToString C = Dynamic.format C
  fun instructionToTy inst = case inst of 
   PushI(_) => INTty
  |PushS(_) =>STRINGty
  |PushB(_) =>BOOLty
  |Acc (_,ty) => ty
  |App (ty) => ty
  |Pair (ty) => ty
  |Proj1 (ty) => ty
  |Proj2 (ty) => ty
  |Prim(p) =>(case p of 
    EQ => BOOLty
    |_=>INTty)
  |MkCLS (_,_,ty)=>ty
  |MkREC (_,_,_,ty)=>ty
  |If (_,_,ty)=>ty
  |Ret (ty) => ty
end
