structure CoreMLLrVals
 : sig structure ParserData : PARSER_DATA
       structure Tokens : CoreML_TOKENS
       structure Parser : PARSER
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct

end
local open Header in
type pos = int
type arg = unit
type lexarg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | STRING of unit ->  (string) | INT of unit ->  (int)
 | ID of unit ->  (string) | prim of unit ->  (Syntax.prim)
 | top of unit ->  (Syntax.dec) | dec of unit ->  (Syntax.dec)
 | exp of unit ->  (Syntax.exp) | const of unit ->  (Syntax.exp)
 | atexp of unit ->  (Syntax.exp) | appexp of unit ->  (Syntax.exp)
end
type svalue = MlyValue.svalue
type result = Syntax.dec
end
structure ParserArg = struct type pos = pos type svalue = svalue type arg = arg end
structure LrParser = LrParserFun(ParserArg)
structure Token = LrParser.Token
structure LrTable = LrParser.LrTable
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\029\000\004\000\028\000\007\000\027\000\009\000\026\000\
\\010\000\025\000\012\000\024\000\013\000\023\000\014\000\022\000\
\\015\000\021\000\016\000\020\000\017\000\019\000\018\000\018\000\
\\021\000\017\000\022\000\016\000\024\000\015\000\000\000\
\\001\000\001\000\029\000\004\000\028\000\007\000\027\000\009\000\026\000\
\\012\000\024\000\013\000\023\000\014\000\022\000\016\000\020\000\
\\017\000\019\000\018\000\018\000\021\000\017\000\022\000\016\000\
\\024\000\015\000\000\000\
\\001\000\002\000\041\000\019\000\040\000\000\000\
\\001\000\002\000\044\000\000\000\
\\001\000\003\000\043\000\000\000\
\\001\000\005\000\050\000\000\000\
\\001\000\006\000\000\000\020\000\000\000\000\000\
\\001\000\008\000\008\000\000\000\
\\001\000\008\000\030\000\000\000\
\\001\000\011\000\005\000\025\000\004\000\000\000\
\\001\000\014\000\006\000\000\000\
\\001\000\014\000\007\000\000\000\
\\001\000\014\000\009\000\000\000\
\\001\000\014\000\037\000\000\000\
\\001\000\017\000\031\000\000\000\
\\001\000\019\000\049\000\000\000\
\\001\000\019\000\051\000\000\000\
\\001\000\023\000\042\000\000\000\
\\054\000\000\000\
\\055\000\000\000\
\\056\000\000\000\
\\057\000\001\000\029\000\004\000\028\000\007\000\027\000\009\000\026\000\
\\012\000\024\000\013\000\023\000\014\000\022\000\016\000\020\000\
\\017\000\019\000\018\000\018\000\021\000\017\000\022\000\016\000\
\\024\000\015\000\000\000\
\\058\000\000\000\
\\059\000\000\000\
\\060\000\000\000\
\\061\000\000\000\
\\062\000\000\000\
\\063\000\000\000\
\\064\000\000\000\
\\065\000\000\000\
\\066\000\000\000\
\\067\000\000\000\
\\068\000\000\000\
\\069\000\000\000\
\\070\000\000\000\
\\071\000\000\000\
\\072\000\000\000\
\\073\000\000\000\
\\074\000\000\000\
\\075\000\000\000\
\\076\000\000\000\
\\077\000\000\000\
\"
val actionRowNumbers =
"\009\000\018\000\010\000\011\000\
\\007\000\012\000\000\000\008\000\
\\014\000\019\000\026\000\024\000\
\\021\000\035\000\039\000\034\000\
\\040\000\000\000\033\000\000\000\
\\027\000\001\000\001\000\013\000\
\\036\000\037\000\041\000\038\000\
\\000\000\000\000\025\000\002\000\
\\017\000\031\000\030\000\004\000\
\\020\000\003\000\029\000\000\000\
\\000\000\000\000\000\000\015\000\
\\005\000\023\000\016\000\028\000\
\\000\000\032\000\022\000\006\000"
val gotoT =
"\
\\005\000\001\000\006\000\051\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\012\000\002\000\011\000\003\000\010\000\004\000\009\000\
\\007\000\008\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\030\000\003\000\010\000\007\000\008\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\012\000\002\000\011\000\003\000\010\000\004\000\031\000\
\\007\000\008\000\000\000\
\\000\000\
\\001\000\012\000\002\000\011\000\003\000\010\000\004\000\032\000\
\\007\000\008\000\000\000\
\\000\000\
\\002\000\033\000\003\000\010\000\007\000\008\000\000\000\
\\002\000\034\000\003\000\010\000\007\000\008\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\012\000\002\000\011\000\003\000\010\000\004\000\036\000\
\\007\000\008\000\000\000\
\\001\000\012\000\002\000\011\000\003\000\010\000\004\000\037\000\
\\007\000\008\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\012\000\002\000\011\000\003\000\010\000\004\000\043\000\
\\007\000\008\000\000\000\
\\001\000\012\000\002\000\011\000\003\000\010\000\004\000\044\000\
\\007\000\008\000\000\000\
\\001\000\012\000\002\000\011\000\003\000\010\000\004\000\045\000\
\\007\000\008\000\000\000\
\\001\000\012\000\002\000\011\000\003\000\010\000\004\000\046\000\
\\007\000\008\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\012\000\002\000\011\000\003\000\010\000\004\000\050\000\
\\007\000\008\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 52
val numrules = 24
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 5) => true | _ => false
val showTerminal =
fn (T 0) => "ADD"
  | (T 1) => "COMMA"
  | (T 2) => "DARROW"
  | (T 3) => "DIV"
  | (T 4) => "ELSE"
  | (T 5) => "EOF"
  | (T 6) => "EQ"
  | (T 7) => "EQUAL"
  | (T 8) => "FALSE"
  | (T 9) => "FN"
  | (T 10) => "FUN"
  | (T 11) => "HASH1"
  | (T 12) => "HASH2"
  | (T 13) => "ID"
  | (T 14) => "IF"
  | (T 15) => "INT"
  | (T 16) => "LPAREN"
  | (T 17) => "MUL"
  | (T 18) => "RPAREN"
  | (T 19) => "SEMICOLON"
  | (T 20) => "STRING"
  | (T 21) => "SUB"
  | (T 22) => "THEN"
  | (T 23) => "TRUE"
  | (T 24) => "VAL"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 19) $$ (T 18) $$ (T 17)
 $$ (T 16) $$ (T 14) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8)
 $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 
0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
fun actionFun1
     (i392:int,defaultPos:pos,stack:(LrTable.state * (svalue * pos * pos)) list,
     (()):arg) =
  case (i392, stack) of 
 ( 0, ( ( _, ( MlyValue.dec dec1, dec1left, dec1right)) :: rest671))
 => let val  result = MlyValue.top (fn _ => let val  (dec as dec1) = 
dec1 ()
 in (dec)
end)
 in ( LrTable.NT 5, ( result, dec1left, dec1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, VAL1left, _)) :: rest671)) => let
 val  result = MlyValue.dec (fn _ => let val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in (Syntax.VAL(ID,exp))
end)
 in ( LrTable.NT 4, ( result, VAL1left, exp1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, _, _)) :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _,
 FUN1left, _)) :: rest671)) => let val  result = MlyValue.dec (fn _ =>
 let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  (exp as exp1) = exp1 ()
 in (Syntax.VAL(ID1, Syntax.EXPFIX(ID1, ID2, exp)))
end)
 in ( LrTable.NT 4, ( result, FUN1left, exp1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.appexp appexp1, appexp1left, appexp1right))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (
appexp as appexp1) = appexp1 ()
 in (appexp)
end)
 in ( LrTable.NT 3, ( result, appexp1left, appexp1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.exp exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: 
( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.exp
 (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in (Syntax.EXPIF(exp1, exp2, exp3))
end)
 in ( LrTable.NT 3, ( result, IF1left, exp3right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, FN1left, _)) :: rest671)) => let
 val  result = MlyValue.exp (fn _ => let val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in (Syntax.EXPFN(ID, exp))
end)
 in ( LrTable.NT 3, ( result, FN1left, exp1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.atexp atexp1, atexp1left, atexp1right)) :: 
rest671)) => let val  result = MlyValue.appexp (fn _ => let val  (
atexp as atexp1) = atexp1 ()
 in (atexp)
end)
 in ( LrTable.NT 0, ( result, atexp1left, atexp1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.atexp atexp1, _, atexp1right)) :: ( _, ( 
MlyValue.appexp appexp1, appexp1left, _)) :: rest671)) => let val  
result = MlyValue.appexp (fn _ => let val  (appexp as appexp1) = 
appexp1 ()
 val  (atexp as atexp1) = atexp1 ()
 in (Syntax.EXPAPP(appexp, atexp))
end)
 in ( LrTable.NT 0, ( result, appexp1left, atexp1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.const const1, const1left, const1right)) :: 
rest671)) => let val  result = MlyValue.atexp (fn _ => let val  (const
 as const1) = const1 ()
 in (const)
end)
 in ( LrTable.NT 1, ( result, const1left, const1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.atexp (fn _ => let val  (ID as ID1) = ID1
 ()
 in (Syntax.EXPID(ID))
end)
 in ( LrTable.NT 1, ( result, ID1left, ID1right), rest671)
end
|  ( 10, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp2, _,
 _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( _, LPAREN1left
, _)) :: rest671)) => let val  result = MlyValue.atexp (fn _ => let
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Syntax.EXPPAIR(exp1, exp2))
end)
 in ( LrTable.NT 1, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 11, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.atexp (fn _ => let val  (exp as exp1) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 1, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.atexp atexp1, _, atexp1right)) :: ( _, ( _,
 HASH11left, _)) :: rest671)) => let val  result = MlyValue.atexp (fn
 _ => let val  (atexp as atexp1) = atexp1 ()
 in (Syntax.EXPPROJ1 atexp)
end)
 in ( LrTable.NT 1, ( result, HASH11left, atexp1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.atexp atexp1, _, atexp1right)) :: ( _, ( _,
 HASH21left, _)) :: rest671)) => let val  result = MlyValue.atexp (fn
 _ => let val  (atexp as atexp1) = atexp1 ()
 in (Syntax.EXPPROJ2 atexp)
end)
 in ( LrTable.NT 1, ( result, HASH21left, atexp1right), rest671)
end
|  ( 14, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp2, _,
 _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: _ :: ( _, ( 
MlyValue.prim prim1, prim1left, _)) :: rest671)) => let val  result = 
MlyValue.atexp (fn _ => let val  (prim as prim1) = prim1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Syntax.EXPPRIM(prim, exp1, exp2))
end)
 in ( LrTable.NT 1, ( result, prim1left, RPAREN1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671)
) => let val  result = MlyValue.const (fn _ => let val  (INT as INT1)
 = INT1 ()
 in (Syntax.INT(INT))
end)
 in ( LrTable.NT 2, ( result, INT1left, INT1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.STRING STRING1, STRING1left, STRING1right))
 :: rest671)) => let val  result = MlyValue.const (fn _ => let val  (
STRING as STRING1) = STRING1 ()
 in (Syntax.STRING(STRING))
end)
 in ( LrTable.NT 2, ( result, STRING1left, STRING1right), rest671)
end
|  ( 17, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.const (fn _ => (Syntax.TRUE))
 in ( LrTable.NT 2, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 18, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.const (fn _ => (Syntax.FALSE))
 in ( LrTable.NT 2, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 19, ( ( _, ( _, EQ1left, EQ1right)) :: rest671)) => let val  
result = MlyValue.prim (fn _ => (Syntax.EQ))
 in ( LrTable.NT 6, ( result, EQ1left, EQ1right), rest671)
end
|  ( 20, ( ( _, ( _, ADD1left, ADD1right)) :: rest671)) => let val  
result = MlyValue.prim (fn _ => (Syntax.ADD))
 in ( LrTable.NT 6, ( result, ADD1left, ADD1right), rest671)
end
|  ( 21, ( ( _, ( _, SUB1left, SUB1right)) :: rest671)) => let val  
result = MlyValue.prim (fn _ => (Syntax.SUB))
 in ( LrTable.NT 6, ( result, SUB1left, SUB1right), rest671)
end
|  ( 22, ( ( _, ( _, MUL1left, MUL1right)) :: rest671)) => let val  
result = MlyValue.prim (fn _ => (Syntax.MUL))
 in ( LrTable.NT 6, ( result, MUL1left, MUL1right), rest671)
end
|  ( 23, ( ( _, ( _, DIV1left, DIV1right)) :: rest671)) => let val  
result = MlyValue.prim (fn _ => (Syntax.DIV))
 in ( LrTable.NT 6, ( result, DIV1left, DIV1right), rest671)
end
| _ => raise (mlyAction i392)
val actions = actionFun1
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.top x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Parser =
 struct
   type token = ParserData.LrParser.Token.token
   type stream = ParserData.LrParser.Stream.stream
   type result = ParserData.result
   type pos = ParserData.pos
   type arg = ParserData.arg
   exception ParseError= ParserData.LrParser.ParseError
   fun makeStream {lexer: unit -> token} : stream
     = ParserData.LrParser.Stream.streamify lexer
   val consStream = ParserData.LrParser.Stream.cons
   val getStream = ParserData.LrParser.Stream.get
   val sameToken = ParserData.Token.sameToken
   fun parse {lookahead:int, stream:stream, error: (string * pos * pos -> unit),arg:arg} =
      (fn (a,b) => (ParserData.Actions.extract a,b))
      (ParserData.LrParser.parse
         {table = ParserData.table,
          lexer=stream,
          lookahead=lookahead,
          saction = ParserData.Actions.actions,
          arg=arg,
          void= ParserData.Actions.void,
          ec = {is_keyword = ParserData.EC.is_keyword,
                noShift = ParserData.EC.noShift,
                preferred_change = ParserData.EC.preferred_change,
                errtermvalue = ParserData.EC.errtermvalue,
                error=error,
                showTerminal = ParserData.EC.showTerminal,
                terms = ParserData.EC.terms}}
      )
 end
structure Token = ParserData.LrParser.Token
structure Tokens : CoreML_TOKENS =
struct
type pos = ParserData.pos
type token = ParserData.Token.token
fun ADD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun DARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun FN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun HASH1 (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun HASH2 (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun SUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun VAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
end
end
