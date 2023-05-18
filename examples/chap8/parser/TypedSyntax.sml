(**
 * expression syntax
 * @copyright (c) 2006, Tohoku University.
 * @author Atsushi Ohori
 *)
structure TypedSyntax = 
struct
  type ty = Type.ty
  type prim = Syntax.prim
  datatype exp
    = EXPID of  string*ty | INT of int | STRING of string 
    | TRUE | FALSE | EXPFN of string * exp *ty
    | EXPAPP of exp * exp *ty | EXPPAIR of exp * exp
    | EXPPROJ1 of exp | EXPPROJ2 of exp 
    | EXPPRIM of prim *  exp * exp 
    | EXPIF of exp * exp * exp 
    | EXPFIX of string * string * exp * ty
  and dec 
    = VAL of string * exp
  fun getTy e =  case e of
      INT int => INTty
      | STRING string => STRINGty
      | TRUE => BOOLty
      | FALSE => BOOLty
      | EXPID (string,ty)=> ty
      | EXPPAIR (exp1, exp2) => PAIRty(getTy exp1,getTy exp2)
      | EXPAPP (exp1, exp2) =>case getTy exp1 of
        FUNty(_,result)=>result
        |_=>INTty 
      | EXPIF (exp1, exp2, exp3) =>getTy exp2        
      | EXPFN (string, exp,ty) =>ty
      | EXPPROJ1 exp => case getTy exp of
        PAIRty(a,_)=>a
        |_=>INTty  
      | EXPPROJ2 exp =>  case getTy exp of
        PAIRty(_,b)=>b
        |_=>INTty  
      | EXPFIX (f, x, exp,ty) =>ty
      | EXPPRIM (p, exp1, exp2) =>case p of 
        Eq=>BOOLty
        |_=>INTty

  fun expToString exp =
      case exp of
        INT int => Int.toString int
      | STRING string => "\"" ^ string ^ "\""
      | TRUE => "true"
      | FALSE => "false"
      | EXPID(string,ty) => string ^ Type.tyToString ty
      | EXPPAIR (exp1, exp2) => 
        "(" ^ expToString exp1 ^ "," ^ expToString exp2 ^ ")"
      | EXPAPP (exp1, exp2,ty) =>
        "(" ^ expToString exp1 ^ " " ^ expToString exp2 ^ ")" ^ Type.tyToString ty
      | EXPIF (exp1, exp2, exp3) =>
        "if " 
         ^ expToString exp1
         ^ " then "
         ^ expToString exp2
         ^ " else "
         ^ expToString exp3
         ^ Type.tyToString (getTy exp2)
      | EXPFN (string, exp,ty) =>
        "(fn " ^ string ^ " => " ^ expToString exp ^ ") : "^ Type.tyToString ty
      | EXPPROJ1 exp => "#1 " ^ expToString exp " : " ^ Type.tyToString getTy (case exp of
        PAIRty(a,b)=>a)
      | EXPPROJ2 exp => "#2 " ^ expToString exp " : " ^ Type.tyToString getTy (case exp of
        PAIRty(a,b)=>b)
      | EXPFIX (f, x, exp,ty) =>
        "(fix " 
        ^ f 
        ^ "("
        ^ x 
        ^ ") => " ^ expToString exp ^ ") : " ^ Type.tyToString ty
      | EXPPRIM (p, exp1, exp2) =>
        let
          val prim = case p of ADD => "add" | SUB => "sub"
                             | MUL => "mul" | DIV => "div"
                             | EQ => "eq"
        in
          "prim(" ^ prim ^ "," ^ expToString exp1 ^ "," ^ expToString exp2 ^ ")" ^ Type.tyToString (case p of Eq => BOOLty
          |_=>INTty)
        end
  and decToString dec =
      case dec of
        VAL (x, exp) =>
        "val " ^ x ^ " = " ^ expToString exp
  

    
(*
  fun printExp exp = print (expToString exp)
  fun printDec dec = print (decToString dec)
  fun expToString exp = Dynamic.format exp
  fun decToString dec = Dynamic.format dec
  fun getTy  exp = Type.ty
*)
end
