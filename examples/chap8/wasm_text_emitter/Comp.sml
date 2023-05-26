open Type
open WasmType

structure Comp =
struct
  (*
  型付き抽象構文木からWASMに落とす
  *)
  fun compile TypedSyntax.typed_exp = WASMModule
end