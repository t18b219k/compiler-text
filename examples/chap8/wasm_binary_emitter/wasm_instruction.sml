(*
CoreMLの実装に必要な最小限のWASM 命令
 真理値　ポインタの表現用
i32.const
i32.eq
整数値用
i64.const
i64.add
i64.sub
i64.mul 
i64.div 
i64.eq
call_indirect


*)
structure WASMInstruction =
struct
  datatype Instruction = I32Const of int32|I32Eq| I64Const of int64|I64Add|I64Sub|I64Mul|I64Div|I64Eq |CallIndirect
end
