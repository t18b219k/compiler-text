signature CoreML_TOKENS =
sig
type pos
type token
val VAL:  pos * pos -> token
val TRUE:  pos * pos -> token
val THEN:  pos * pos -> token
val SUB:  pos * pos -> token
val STRING: (string) *  pos * pos -> token
val SEMICOLON:  pos * pos -> token
val RPAREN:  pos * pos -> token
val MUL:  pos * pos -> token
val LPAREN:  pos * pos -> token
val INT: (int) *  pos * pos -> token
val IF:  pos * pos -> token
val ID: (string) *  pos * pos -> token
val HASH2:  pos * pos -> token
val HASH1:  pos * pos -> token
val FUN:  pos * pos -> token
val FN:  pos * pos -> token
val FALSE:  pos * pos -> token
val EQUAL:  pos * pos -> token
val EQ:  pos * pos -> token
val EOF:  pos * pos -> token
val ELSE:  pos * pos -> token
val DIV:  pos * pos -> token
val DARROW:  pos * pos -> token
val COMMA:  pos * pos -> token
val ADD:  pos * pos -> token
end
signature CoreML_LRVALS=
sig
structure Tokens : CoreML_TOKENS
structure Parser : PARSER
sharing type Parser.token = Tokens.token
end
