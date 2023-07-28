(**
 * Type inference module
 * @author Kenta Kasahara
 *)
structure Typeinf =
struct
  open Syntax Type TypeUtils UnifyTy

  structure TS = TypedSyntax
  exception NotFunction of {expected: ty, actual: ty, expression: exp}
  exception ExpectedToBeBool of {actual: ty, expression: exp}
  exception ExpectedToBeInt of {actual: ty, expression: exp}
  exception ArgNotMatch of {expected: ty, actual: ty, expression: exp}
  exception ExpectedToBePair of {expected: ty, actual: ty, expression: exp}
  exception ResultNotMatch of {expected: ty, actual: ty, expression: exp}
  exception IfArmsNotMatch of
    {then_ty: ty, else_ty: ty, then_exp: exp, else_exp: exp}
  exception TypeError
  exception UndefinedVariable of string
  exception Unreachable
  infixr ++

  fun s1 ++ s2 = composeSubst s1 s2

  fun typed_W gamma exp =
    case exp of
      INT int => (emptySubst, INTty, TS.INT int)
    | EXPID string =>
        (case SEnv.find (gamma, string) of
           SOME ty => (emptySubst, freshInst ty, TS.EXPID (string, ty))
         | NONE => raise UndefinedVariable string)
    | EXPFN (string, exp) =>
        let
          val ty1 = newTy ()
          val newGamma = SEnv.insert (gamma, string, ty1)
          val (S, ty2, exp) = typed_W newGamma exp
          val ty = FUNty (substTy S ty1, ty2)
        in
          (S, ty, TS.EXPFN (string, exp, ty))
        end
    | EXPAPP (exp1, exp2) =>
        let
          val (S1, ty1, typed_exp1) = typed_W gamma exp1
          val (S2, ty2, typed_exp2) = typed_W (substTyEnv S1 gamma) exp2
          val ty3 = newTy ()
          val S3 =
            (unify [(FUNty (ty2, ty3), substTy S2 ty1)])
            handle UnifyTy (a, b, _) =>
              case b of
                FUNty (actual, result) =>
                  if actual = ty2 then
                    raise ResultNotMatch
                      {expected = ty3, actual = result, expression = exp1}
                  else
                    raise ArgNotMatch
                      {expected = ty2, actual = actual, expression = exp2}
              | _ =>
                  raise NotFunction
                    {expected = a, actual = b, expression = exp1}
          val S4 = composeSubst S3 (composeSubst S2 S1)
          val ty = substTy S4 ty3
        in
          (S4, ty, TS.EXPAPP (typed_exp1, typed_exp2, ty))
        end
    | STRING string => (emptySubst, STRINGty, TS.STRING string)
    | TRUE => (emptySubst, BOOLty, TS.TRUE)
    | FALSE => (emptySubst, BOOLty, TS.FALSE)
    | EXPPAIR (exp1, exp2) =>
        let
          val (S1, ty1, exp1) = typed_W gamma exp1
          val (S2, ty2, exp2) = typed_W (substTyEnv S1 gamma) exp2
          val ty = PAIRty (substTy S2 ty1, ty2)
        in
          (S2 ++ S1, ty, TS.EXPPAIR (exp1, exp2))
        end
    | EXPPROJ1 exp =>
        let
          val (S1, ty, typed_exp) = typed_W gamma exp
          val ty1 = newTy ()
          val ty2 = newTy ()
          val S2 =
            unify [(ty, PAIRty (ty1, ty2))]
            handle UnifyTy (actual, expected, _) =>
              raise ExpectedToBePair
                {expected = expected, actual = actual, expression = exp}
        in
          (S2 ++ S1, substTy S2 ty1, TS.EXPPROJ1 (typed_exp))
        end
    | EXPPROJ2 exp =>
        let
          val (S1, ty, typed_exp) = typed_W gamma exp
          val ty1 = newTy ()
          val ty2 = newTy ()
          val S2 =
            unify [(ty, PAIRty (ty1, ty2))]
            handle UnifyTy (actual, expected, _) =>
              raise ExpectedToBePair
                {expected = expected, actual = actual, expression = exp}
        in
          (S2 ++ S1, substTy S2 ty2, TS.EXPPROJ2 (typed_exp))
        end
    | EXPIF (exp1, exp2, exp3) =>
        let
          val (S1, ty1, typed_exp1) = typed_W gamma exp1
          val S2 =
            unify [(ty1, BOOLty)]
            handle UnifyTy (a, b, _) =>
              raise ExpectedToBeBool {actual = a, expression = exp1}
          val (S3, ty2, typed_exp2) = typed_W (substTyEnv (S2 ++ S1) gamma) exp2
          val (S4, ty3, typed_exp3) =
            typed_W (substTyEnv (S3 ++ S2 ++ S1) gamma) exp3
          val S5 =
            unify [(ty2, ty3)]
            handle UnifyTy (a, b, _) =>
              raise IfArmsNotMatch
                {then_ty = a, then_exp = exp2, else_ty = b, else_exp = exp3}
          val S = S5 ++ S4 ++ S3 ++ S2 ++ S1
          val newGamma = substTyEnv S gamma
        in
          (S, substTy S5 ty2, TS.EXPIF (typed_exp1, typed_exp2, typed_exp3))
        end
    | EXPFIX (fid, xid, exp) =>
        let
          val argTy = newTy ()
          val bodyTy = newTy ()
          val funTy = FUNty (argTy, bodyTy)
          val newGamma = SEnv.insert
            (SEnv.insert (gamma, fid, funTy), xid, argTy)
          val (S1, ty, typed_exp) = typed_W newGamma exp
          val S2 = unify [(ty, bodyTy)]
          val S = S2 ++ S1
          val ty = substTy S funTy
        in
          (S, ty, TS.EXPFIX (fid, xid, typed_exp, ty))
        end
    | EXPPRIM (p, exp1, exp2) =>
        let
          val (S1, ty1, typed_exp1) = typed_W gamma exp1
          val (S2, ty2, typed_exp2) = typed_W (substTyEnv S1 gamma) exp2
          val S3 =
            unify [(substTy S2 ty1, INTty), (ty2, INTty)]
            handle UnifyTy (a, b, id) =>
              case id of
                2 => raise ExpectedToBeInt {actual = a, expression = exp1}
              | 1 => raise ExpectedToBeInt {actual = a, expression = exp2}
              | _ => raise Unreachable
          val ty3 =
            case p of
              EQ => BOOLty
            | _ => INTty
        in
          (S3 ++ S2 ++ S1, ty3, TS.EXPPRIM (p, typed_exp1, typed_exp2))
        end

  (*型付き式に残っている型変数を型で置き換える*)
  fun apply_subst_for_exp (ts, subst) =
    case ts of
      TS.EXPID (string, ty) => TS.EXPID (string, TypeUtils.substTy subst ty)
    | TS.INT int => TS.INT int
    | TS.STRING string => TS.STRING string
    | TS.TRUE => TS.TRUE
    | TS.FALSE => TS.FALSE
    | TS.EXPFN (string, typed_exp, ty) =>
        TS.EXPFN
          ( string
          , apply_subst_for_exp (typed_exp, subst)
          , TypeUtils.substTy subst ty
          )
    | TS.EXPAPP (typed_exp1, typed_exp2, ty) =>
        TS.EXPAPP
          ( apply_subst_for_exp (typed_exp1, subst)
          , apply_subst_for_exp (typed_exp2, subst)
          , TypeUtils.substTy subst ty
          )
    | TS.EXPPAIR (typed_exp1, typed_exp2) =>
        TS.EXPPAIR
          ( apply_subst_for_exp (typed_exp1, subst)
          , apply_subst_for_exp (typed_exp2, subst)
          )
    | TS.EXPPROJ1 typed_exp =>
        TS.EXPPROJ1 (apply_subst_for_exp (typed_exp, subst))
    | TS.EXPPROJ2 typed_exp =>
        TS.EXPPROJ2 (apply_subst_for_exp (typed_exp, subst))
    | TS.EXPPRIM (prim, typed_exp1, typed_exp2) =>
        TS.EXPPRIM
          ( prim
          , apply_subst_for_exp (typed_exp1, subst)
          , apply_subst_for_exp (typed_exp2, subst)
          )
    | TS.EXPIF (typed_exp1, typed_exp2, typed_exp3) =>
        TS.EXPIF
          ( apply_subst_for_exp (typed_exp1, subst)
          , apply_subst_for_exp (typed_exp2, subst)
          , apply_subst_for_exp (typed_exp3, subst)
          )
    | TS.EXPFIX (f, x, typed_exp, ty) =>
        TS.EXPFIX
          ( f
          , x
          , apply_subst_for_exp (typed_exp, subst)
          , TypeUtils.substTy subst ty
          )

  fun typeinf_with_typed_expr gamma (VAL (id, exp)) =
    let
      val (subst, ty, exp) = typed_W gamma exp
      val exp = apply_subst_for_exp (exp, subst)
      val tids = SSet.listItems (FTV ty)
      val newTy = if null tids then ty else POLYty (tids, ty)
      val _ = print
        ("Inferred typing:\n" ^ "val " ^ id ^ " : " ^ tyToString ty ^ "\n")
    in
      (SEnv.insert (gamma, id, newTy), exp)
    end
end
