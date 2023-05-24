open Syntax;
open Type;
structure TypedComp = struct
  structure S = TypedSyntax
  structure I = TypedInstruction
  val getTy = S.getTy
  fun comp e K = 
    case e of
      S.INT (int) => I.PushI int :: K
    | S.STRING (string) => I.PushS string :: K
    | S.TRUE => I.PushB true :: K
    | S.FALSE => I.PushB false :: K
    | S.EXPID (string,ty) => I.Acc(string ,ty):: K
    | S.EXPFN (x, e,ty) =>(case ty of 
      FUNty(_,o_ty)=> I.MkCLS(x, comp e [I.Ret(o_ty)],ty) :: K
      )
    | S.EXPAPP (e1, e2,ty) => comp e1 (comp e2 (I.App ty :: K))
    | S.EXPPAIR (e1, e2) => comp e1 (comp e2 (I.Pair(PAIRty(S.getTy e1 ,S.getTy e2))  :: K))
    | S.EXPPROJ1 (e) => comp e (I.Proj1(S.getTy e) :: K)
    | S.EXPPROJ2 (e) => comp e (I.Proj2(S.getTy e) :: K)
    | S.EXPPRIM (prim, e1, e2) => 
      let
        val p = 
          case prim of ADD => I.ADD | SUB => I.SUB |MUL => I.MUL
                       |DIV => I.DIV |EQ => I.EQ
      in
        comp e1 (comp e2 (I.Prim p::K))
      end
    | S.EXPFIX (f, x, e,ty) => (case ty of 
    FUNty(_,o_ty) => I.MkREC(f, x, comp e [I.Ret(o_ty)],ty) :: K)
    | S.EXPIF (e1, e2, e3) => 
      comp e1 (I.If(comp e2 nil, comp e3 nil,getTy e2) :: K)
  fun compile (S.VAL (id, e)) =
    let val C = comp e nil in
      print ( "Compiled to:\n" ^ I.codeToString C ^ "\n");
      (id, C)
    end
end
