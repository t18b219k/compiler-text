(**
 * expression syntax
 * @copyright (c) 2006, Tohoku University.
 * @author Atsushi Ohori
 *)
structure TypedSyntax =
struct
  open Syntax

  open Type

  type ty = Type.ty

  type prim = Syntax.prim

  datatype typed_exp =
    EXPID of string * ty
  | INT of int
  | STRING of string
  | TRUE
  | FALSE
  | EXPFN of string * typed_exp * ty
  | EXPAPP of typed_exp * typed_exp * ty
  | EXPPAIR of typed_exp * typed_exp
  | EXPPROJ1 of typed_exp
  | EXPPROJ2 of typed_exp
  | EXPPRIM of prim * typed_exp * typed_exp
  | EXPIF of typed_exp * typed_exp * typed_exp
  | EXPFIX of string * string * typed_exp * ty
  and typed_dec =
    VAL of string * typed_exp

  exception UnexpectedType of ty

  fun getTy e =
    case e of
      INT _ => Type.INTty
    | STRING _ => Type.STRINGty
    | TRUE => Type.BOOLty
    | FALSE => Type.BOOLty
    | EXPID (_, ty) => ty
    | EXPPAIR (exp1, exp2) => Type.PAIRty (getTy exp1, getTy exp2)
    | EXPAPP (_, _, ty) => ty
    | EXPIF (_, exp2, _) => getTy exp2
    | EXPFN (_, _, ty) => ty
    | EXPPROJ1 typed_exp =>
        (case (getTy typed_exp) of
           Type.PAIRty (a, _) => a
         | Type.POLYty (tyvars, Type.PAIRty (a, _)) => Type.POLYty (tyvars, a)
         | x => raise UnexpectedType x)
    | EXPPROJ2 typed_exp =>
        (case (getTy typed_exp) of
           Type.PAIRty (_, b) => b
         | Type.POLYty (tyvars, Type.PAIRty (_, b)) => Type.POLYty (tyvars, b)
         | x => raise UnexpectedType x)
    | EXPFIX (_, _, _, ty) => ty
    | EXPPRIM (p, _, _) =>
        (case p of
           EQ => Type.BOOLty
         | _ => Type.INTty)

  fun expToString typed_exp =
    case typed_exp of
      INT int => Int.toString int
    | STRING string => "\"" ^ string ^ "\""
    | TRUE => "true"
    | FALSE => "false"
    | EXPID (string, ty) => string ^ " : " ^ tyToString ty
    | EXPPAIR (exp1, exp2) =>
        "(" ^ expToString exp1 ^ "," ^ expToString exp2 ^ ")"
    | EXPAPP (exp1, exp2, ty) =>
        "(" ^ expToString exp1 ^ " " ^ expToString exp2 ^ ") : " ^ tyToString ty
    | EXPIF (exp1, exp2, exp3) =>
        "if " ^ expToString exp1 ^ " then " ^ expToString exp2 ^ " else "
        ^ expToString exp3
    | EXPFN (var_id, exp, ty) =>
        "(fn " ^ var_id ^ " => " ^ expToString exp ^ ") : "
        ^ tyToString ty
    | EXPPROJ1 exp =>
        "#1 " ^ expToString exp ^ " : "
        ^
        tyToString
          (case (getTy exp) of
             PAIRty (a, _) => a
           | x => raise UnexpectedType x)
    | EXPPROJ2 exp =>
        "#2 " ^ expToString exp ^ " : "
        ^
        tyToString
          (case (getTy typed_exp) of
             PAIRty (_, b) => b
           | x => raise UnexpectedType x)
    | EXPFIX (f, x, exp, ty) =>
        "(fix " ^ f ^ "(" ^ x ^ ") => " ^ expToString exp ^ ") : "
        ^ (tyToString ty)
    | EXPPRIM (p, exp1, exp2) =>
        let
          val prim =
            case p of
              ADD => "add"
            | SUB => "sub"
            | MUL => "mul"
            | DIV => "div"
            | EQ => "eq"
        in
          "prim(" ^ prim ^ "," ^ expToString exp1 ^ "," ^ expToString exp2
          ^ ") : "
          ^
          (tyToString
             (case p of
                EQ => BOOLty
              | _ => INTty))
        end
  and typed_decToString (VAL (x, typed_exp)) =
    "val " ^ x ^ " = " ^ expToString typed_exp
end
