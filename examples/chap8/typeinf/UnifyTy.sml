structure UnifyTy = struct
  open Type TypeUtils
  exception UnifyTy of ty*ty*int
  exception CyclicType
  fun FTV ty =
    let fun scan ty set =
          case ty of
            TYVARty string => SSet.add (set,string)
          | FUNty (domTy, ranTy) => scan ranTy (scan domTy set)
          | PAIRty (fstTy, sndTy) => scan sndTy (scan fstTy set)
          | _ => set
    in scan ty SSet.empty end
  (*型変数がtyの中に自由出現しないかチェック*)
  fun occurs (TYVARty string, ty) = SSet.member(FTV ty, string)
    | occurs _ = false
  fun rewrite (nil, S,_) = S
    | rewrite((ty1,ty2)::E, S,exp_id) =
      if ty1 = ty2 then rewrite(E, S,exp_id+1) else 
      case (ty1,ty2) of
        (TYVARty tv, _) => 
        if occurs (ty1, ty2) then raise CyclicType else
        let val S1 = SEnv.singleton(tv, ty2)
        in rewrite (map (fn (ty1,ty2) => 
                            (substTy S1 ty1, substTy S1 ty2))
                        E,
                    composeSubst S1 S,exp_id+1)
        end
      | (_, TYVARty tv) => rewrite ((ty2, ty1)::E, S,exp_id+1)
      | (FUNty(ty11, ty12), FUNty(ty21, ty22)) => 
        rewrite ((ty11,ty21)::(ty12,ty22)::E, S,exp_id+1)
      | (PAIRty(ty11, ty12), PAIRty(ty21, ty22)) => 
        rewrite ((ty11, ty21)::(ty12, ty22)::E,S,exp_id+1)
      | (a,b) => raise UnifyTy (a,b,exp_id)
  fun unify E = rewrite (E, SEnv.empty,0)
end

