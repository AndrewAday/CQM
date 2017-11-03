(* Ocamllex scanner for MicroC *)

{ open Parser }

let esc    = '\\' ['\\' ''' '"' 'n' 'r' 't']
let ascii  = ([' '-'!' '#'-'[' ']'-'~'])

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
(*-----------------------------------SYNTAX-----------------------------------*)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACK }
| ']'      { RBRACK }
| ';'      { SEMI }
| ','      { COMMA }
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
| ".."     { MATMUL }
| ".*"     { MATDOTMUL }
| ':'      { SLICE }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"    { AND }
| "||"     { OR }
| "!"      { NOT }
(*----------------------------------CONTROL-----------------------------------*)
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "extern" { EXTERN }
| "new"    { NEW }
(*-----------------------------------TYPES------------------------------------*)
| "int"    { INT }
| "bool"   { BOOL }
| "void"   { VOID }
| "float"   { FLOAT }
| "string"   { STRING }
| "imatrix"   { IMATRIX }
| "fmatrix"   { FMATRIX }
| "smatrix"   { SMATRIX }
| "tup"    { TUPLE }
| "struct" { STRUCT }
(*---------------------------------LITERALS-----------------------------------*)
| "true"   { TRUE }
| "false"  { FALSE }
| ['0'-'9']+ as lxm                                       { INTLIT(int_of_string lxm) }
| (['0'-'9']+'.'['0'-'9']* | ['0'-'9']*'.'['0'-'9']+) as lxm { FLOATLIT(float_of_string lxm) }
| '"' ((ascii | esc)* as s)'"'               { STRINGLIT(s) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm  { ID(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*"*" as lxm  { PNTR(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
