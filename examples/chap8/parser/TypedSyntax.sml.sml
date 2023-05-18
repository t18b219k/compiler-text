(**
 * expression syntax
 * @copyright (c) 2006, Tohoku University.
 * @author Atsushi Ohori
 *)
open Type
open Syntax
structure TypedSyntax = 
struct
  type ty = Type.ty
  type prim = Syntax.prim
  datatype typed_exp
    = EXPID of  string*ty | INT of int | STRING of string 
    | TRUE | FALSE | EXPFN of string * typed_exp *ty
    | EXPAPP of typed_exp * typed_exp *ty | EXPPAIR of typed_exp * typed_exp
    | EXPPROJ1 of typed_exp | EXPPROJ2 of typed_exp 
    | EXPPRIM of prim *  typed_exp * typed_exp 
    | EXPIF of typed_exp * typed_exp * typed_exp 
    | EXPFIX of string * string * typed_exp * ty
  and dec 
    = VAL of string * typed_exp
  fun getTy e =  case e of
      INT int => INTty
      | STRING string => STRINGty
      | TRUE => BOOLty
      | FALSE => BOOLty
      | EXPID (string,ty)=> ty
      | EXPPAIR (exp1, exp2) => PAIRty(getTy exp1,getTy exp2)
      | EXPAPP (exp1, exp2,ty) =>ty
      | EXPIF (exp1, exp2, exp3) =>getTy exp2        
      | EXPFN (string, typed_exp,ty) =>ty
      | EXPPROJ1 typed_exp =>( case (getTy typed_exp) of
        PAIRty(a,_)=>a
    )
      | EXPPROJ2 typed_exp =>  (case (getTy typed_exp) of
        PAIRty(_,b)=>b
    )
      | EXPFIX (f, x, typed_exp,ty) =>ty
      | EXPPRIM (p, exp1, exp2) =>(case p of 
        EQ=>BOOLty
        |_=>INTty)

  fun expToString typed_exp =
      case typed_exp of
        INT int => Int.toString int
      | STRING string => "\"" ^ string ^ "\""
      | TRUE => "true"
      | FALSE => "false"
      | EXPID(string,ty) => string ^ tyToString ty
      | EXPPAIR (exp1, exp2) => 
        "(" ^ expToString exp1 ^ "," ^ expToString exp2 ^ ")"
      | EXPAPP (exp1, exp2,ty) =>
        "(" ^ expToString exp1 ^ " " ^ expToString exp2 ^ ")" ^ tyToString ty
      | EXPIF (exp1, exp2, exp3) =>
        "if " 
         ^ expToString exp1
         ^ " then "
         ^ expToString exp2
         ^ " else "
         ^ expToString exp3
         ^ tyToString (getTy exp2)
      | EXPFN (var_id, typed_exp,ty) =>
        "(fn " ^ var_id ^ " => " ^ expToString typed_exp ^ ") : "^ tyToString ty
      | EXPPROJ1 typed_exp => "#1 " ^ expToString typed_exp ^ " : " ^ tyToString  (case (getTy typed_exp) of
        PAIRty(a,b)=>a)
      | EXPPROJ2 typed_exp => "#2 " ^ expToString typed_exp ^ " : " ^ tyToString  (case (getTy typed_exp) of
        PAIRty(a,b)=>b)
      | EXPFIX (f, x, typed_exp,ty) =>
        "(fix " 
        ^ f 
        ^ "("
        ^ x 
        ^ ") => " ^ expToString typed_exp ^ ") : " ^ (tyToString ty)
      | EXPPRIM (p, exp1, exp2) =>
        let
          val prim = case p of ADD => "add"
           | SUB => "sub"
           | MUL => "mul" 
           | DIV => "div"
            | EQ => "eq"
        in
          "prim(" ^ prim ^ "," ^ expToString exp1 ^ "," ^ expToString exp2 ^ ") : " ^ (tyToString (case p of EQ => BOOLty
          |_ => INTty))
        end
  and decToString dec =
      case dec of
        VAL (x, typed_exp) =>
        "val " ^ x ^ " = " ^ expToString typed_exp
  

    
(*
  fun printExp typed_exp = print (expToString typed_exp)
  fun printDec dec = print (decToString dec)
  fun expToString typed_exp = Dynamic.format typed_exp
  fun decToString dec = Dynamic.format dec
  fun getTy  typed_exp = Type.ty
*)
end