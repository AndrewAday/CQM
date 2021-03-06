(* Ocamllex scanner for MicroC *)

{ open Parser }

let esc    = '\\' ['\\' ''' '"' 'n' 'r' 't']
let ascii  = ([' '-'!' '#'-'[' ']'-'~'])
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| "//"     { inline_comment lexbuf }
(*-----------------------------------SYNTAX-----------------------------------*)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACK }
| ']'      { RBRACK }
| ';'      { SEMI }
| ','      { COMMA }
| '.'      { PERIOD }
(*---------------------------------OPERATORS----------------------------------*)
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| "=>"     { PIPE }
| "**"     { POW }
| '%'      { MOD }
| '^'      { MATTRANS }
| ".."     { DOT }
| ':'      { SLICE }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
(*----------------------------------CONTROL-----------------------------------*)
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "extern" { EXTERN }
| "make"   { MAKE }
(*-----------------------------------TYPES------------------------------------*)
| "int"    		{ INT }
| "bool"   		{ BOOL }
| "void"   		{ VOID }
| "float"   	{ FLOAT }
| "string"   	{ STRING }
| "imatrix"   	{ IMATRIX }
| "fmatrix"   	{ FMATRIX }
| "struct" 		{ STRUCT }
| "fp"                { FPTR }
(*---------------------------------LITERALS-----------------------------------*)
| "true"   { TRUE }
| "false"  { FALSE }
| "NULL"   { NULL }
| ['0'-'9']+ as lxm                                           { INTLIT(int_of_string lxm) }
| (['0'-'9']+'.'['0'-'9']* | ['0'-'9']*'.'['0'-'9']+) as lxm  { FLOATLIT(float_of_string lxm) }
| '"' ((ascii | esc)* as s)'"'                                { STRINGLIT(s) }
| id as lxm                                                   { ID(lxm) }
| eof                                                         { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and inline_comment = parse
  ['\n'] { token lexbuf }
| _    { inline_comment lexbuf }
