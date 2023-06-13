(**
 * Type inference module
 * @author tach-nology
 *)
structure Typeinf = struct
  open Syntax Type TypeUtils UnifyTy
  structure TS = TypedSyntax
  exception TypeError
  infixr ++
  fun s1 ++ s2 = composeSubst s1 s2
fun W gamma exp = 
  case exp of
    INT (int) => (emptySubst, INTty)
  | EXPID (string) => 
    (case SEnv.find(gamma, string) of
       SOME ty => (emptySubst, freshInst ty)
     | NONE => raise TypeError)
  | EXPFN (string, exp) =>  
    let val ty1 = newTy()
        val newGamma = SEnv.insert(gamma, string, ty1)
        val (S, ty2) = W newGamma exp
    in 
      (S, FUNty(substTy S ty1, ty2))
    end
  | EXPAPP (exp1, exp2) =>
    let
      val (S1, ty1) = W gamma exp1
      val (S2, ty2) = W (substTyEnv S1 gamma) exp2
      val ty3 = newTy()
      val S3 = unify [(FUNty(ty2, ty3), substTy S2 ty1)]
      val S4 = composeSubst S3 (composeSubst S2 S1)
    in
      (S4, substTy S4 ty3)
    end
  | STRING (string) => (emptySubst, STRINGty)
  | TRUE => (emptySubst, BOOLty)
  | FALSE => (emptySubst, BOOLty)
  | EXPPAIR (exp1, exp2) =>
    let
      val (S1, ty1) = W gamma exp1
      val (S2, ty2) = W (substTyEnv S1 gamma) exp2
    in
      (
       S2 ++ S1,
       PAIRty(substTy S2 ty1,ty2)
      )
    end
  | EXPPROJ1 exp =>
    let
      val (S1, ty) = W gamma exp
      val ty1 = newTy()
      val ty2 = newTy()
      val S2 = unify [(ty, PAIRty (ty1, ty2))]
    in
      (
       S2 ++ S1,
       substTy S2 ty1
      )
    end
  | EXPPROJ2 exp =>
    let
      val (S1, ty) = W gamma exp
      val ty1 = newTy()
      val ty2 = newTy()
      val S2 = unify [(ty, PAIRty (ty1, ty2))]
    in
      (
       S2 ++ S1,
       substTy S2 ty2
      )
    end
    | EXPIF (exp1, exp2, exp3) => 
        let
          val (S1, ty1) = W gamma exp1
          val S2 = unify [(ty1, BOOLty)]
          val (S3, ty2) = W (substTyEnv (S2 ++ S1) gamma) exp2
          val (S4, ty3) = W (substTyEnv (S3 ++ S2 ++ S1) gamma) exp3
          val S5 = unify [(ty2, ty3)]
          val S = S5 ++ S4 ++ S3 ++ S2 ++ S1
          val newGamma = substTyEnv S gamma
        in
          (S, substTy S5 ty2)
        end
    | EXPFIX (fid, xid, exp) =>  
        let
          val argTy = newTy()
          val bodyTy = newTy()
          val funTy = FUNty(argTy, bodyTy)
          val newGamma = 
              SEnv.insert(SEnv.insert(gamma, fid, funTy),
                          xid, argTy)
          val (S1, ty) = W newGamma exp
          val S2 = unify [(ty, bodyTy)]
          val S = S2 ++ S1
        in
          (S, substTy S funTy)
        end
    | EXPPRIM (p, exp1, exp2) =>
      let
        val (S1, ty1) = W gamma exp1
        val (S2, ty2) = W (substTyEnv S1 gamma) exp2
        val S3 = unify [(substTy S2 ty1, INTty), (ty2, INTty)]
        val ty3 = 
            case p of EQ => BOOLty | _ => INTty
      in
        (S3 ++ S2 ++ S1, ty3)
      end
fun typed_W gamma exp = 
(*
    = EXPID of  string*ty 
    | INT of int 
    | STRING of string 
    | TRUE | FALSE 
    | EXPFN of string * typed_exp *ty
    | EXPAPP of typed_exp * typed_exp *ty 
    | EXPPAIR of typed_exp * typed_exp
    | EXPPROJ1 of typed_exp 
    | EXPPROJ2 of typed_exp 
    | EXPPRIM of prim *  typed_exp * typed_exp 
    | EXPIF of typed_exp * typed_exp * typed_exp 
    | EXPFIX of string * string * typed_exp * ty
*)
  case exp of
    INT (int) => (emptySubst, INTty,TS.INT(int))
  | EXPID (string) => 
    (case SEnv.find(gamma, string) of
       SOME ty => (emptySubst, freshInst ty,TS.EXPID(string,ty))
     | NONE => raise TypeError)
  | EXPFN (string, exp) =>  
    let val ty1 = newTy()
        val newGamma = SEnv.insert(gamma, string, ty1)
        val (S, ty2,exp) = typed_W newGamma exp
        val ty = FUNty(substTy S ty1, ty2)
    in 
      (S,ty,TS.EXPFN(string,exp,ty))
    end
  | EXPAPP (exp1, exp2) =>
    let
      val (S1, ty1,exp1) = typed_W gamma exp1
      val (S2, ty2,exp2) = typed_W (substTyEnv S1 gamma) exp2
      val ty3 = newTy()
      val S3 = unify [(FUNty(ty2, ty3), substTy S2 ty1)]
      val S4 = composeSubst S3 (composeSubst S2 S1)
      val ty = substTy S4 ty3
    in
      (S4,ty,TS.EXPAPP(exp1,exp2,ty))
    end
  | STRING (string) => (emptySubst, STRINGty,TS.STRING(string))
  | TRUE => (emptySubst, BOOLty,TS.TRUE)
  | FALSE => (emptySubst, BOOLty,TS.FALSE)
  | EXPPAIR (exp1, exp2) =>
    let
      val (S1, ty1,exp1) = typed_W gamma exp1
      val (S2, ty2,exp2) = typed_W (substTyEnv S1 gamma) exp2
      val ty=PAIRty(substTy S2 ty1,ty2)
    in
      (
       S2 ++ S1,
       ty,
        TS.EXPPAIR(exp1,exp2)
      )
    end
  | EXPPROJ1 exp =>
    let
      val (S1, ty,exp) = typed_W gamma exp
      val ty1 = newTy()
      val ty2 = newTy()
      val S2 = unify [(ty, PAIRty (ty1, ty2))]
    in
      (
       S2 ++ S1,
       substTy S2 ty1,
       TS.EXPPROJ1(exp)
      )
    end
  | EXPPROJ2 exp =>
    let
      val (S1, ty,exp) = typed_W gamma exp
      val ty1 = newTy()
      val ty2 = newTy()
      val S2 = unify [(ty, PAIRty (ty1, ty2))]
    in
      (
       S2 ++ S1,
       substTy S2 ty2,
       TS.EXPPROJ2(exp)
      )
    end
    | EXPIF (exp1, exp2, exp3) => 
        let
          val (S1, ty1,exp1) = typed_W gamma exp1
          val S2 = unify [(ty1, BOOLty)]
          val (S3, ty2,exp2) = typed_W (substTyEnv (S2 ++ S1) gamma) exp2
          val (S4, ty3,exp3) = typed_W (substTyEnv (S3 ++ S2 ++ S1) gamma) exp3
          val S5 = unify [(ty2, ty3)]
          val S = S5 ++ S4 ++ S3 ++ S2 ++ S1
          val newGamma = substTyEnv S gamma
        in
          (S, substTy S5 ty2,TS.EXPIF(exp1,exp2,exp3))
        end
    | EXPFIX (fid, xid, exp) =>  
        let
          val argTy = newTy()
          val bodyTy = newTy()
          val funTy = FUNty(argTy, bodyTy)
          val newGamma = 
              SEnv.insert(SEnv.insert(gamma, fid, funTy),
                          xid, argTy)
          val (S1, ty,exp) = typed_W newGamma exp
          val S2 = unify [(ty, bodyTy)]
          val S = S2 ++ S1
          val ty = substTy S funTy
        in
          (S,ty,TS.EXPFIX(fid,xid,exp,ty))
        end
    | EXPPRIM (p, exp1, exp2) =>
      let
        val (S1, ty1,exp1) = typed_W gamma exp1
        val (S2, ty2,exp2) = typed_W (substTyEnv S1 gamma) exp2
        val S3 = unify [(substTy S2 ty1, INTty), (ty2, INTty)]
        val ty3 = 
            case p of EQ => BOOLty | _ => INTty
      in
        (S3 ++ S2 ++ S1, ty3,TS.EXPPRIM(p,exp1,exp2))
      end
  fun typeinf gamma (VAL (id, exp)) =
    let
      val (subst, ty) = W gamma exp
      val tids = SSet.listItems (FTV ty)
      val newTy = if null tids then ty else POLYty (tids, ty)
      val _ = print ("Inferred typing:\n"
                     ^ "val " ^ id ^ " : "
                     ^ Type.tyToString newTy ^ "\n")
    in
      SEnv.insert(gamma, id,  newTy)
    end
    handle Unify => raise TypeError
    (*  (*型に残っている型変数を型で置き換える*)
  fun apply_subst(t:Type.ty,subst)=case t of 
    (*基底ケース　型変数*)
    TYVARty(string)=>case SEnv.find(subst,string) of
      NONE=>t
      |SOME(codom)=>codom 
    (*合成型　関数型*)
    | FUNty(t1,t2)=>FUNty(apply_subst(t1,subst),apply_subst(t2,subst))
    (*合成型　ペア型*)
    | PAIRty(t1,t2)=>PAIRty(apply_subst(t1,subst),apply_subst(t2,subst))
    (*型変数のリストから*)
    | POLYty(tyvars,t)=>
    let val remain_tyvars =List.filter (fn tyvar => not(SEnv.inDomain(subst,tyvar))) tyvars in 
      if null remain_tyvars then 
      apply_subst(t,subst)
      else   
      POLYty(remain_tyvars, apply_subst (t,subst))
    end 
  (*基底ケース \iota 基底型*)  
  |_=>t*)
  fun typeinf_with_typed_expr gamma (VAL (id, exp)) =
    let
      val (subst, ty,exp) = typed_W gamma exp
      val tids = SSet.listItems (FTV ty)
      val newTy = if null tids then ty else POLYty (tids, ty)
      val _ = print ("Inferred typing:\n"
                     ^ "val " ^ id ^ " : "
                     ^ Type.tyToString newTy ^ "\n")
    in
      (SEnv.insert(gamma, id,  newTy),exp)
    end
    handle Unify => raise TypeError
end
