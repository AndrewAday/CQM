(* Ocamllex scanner for MicroC *)

{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
(*-------------Sections and Keywords------------*)
| "layer"  { LAYER }
| "model"  { MODEL }
| "run"    { RUN }
| "trainable" { TRAINABLE }
| "train"  { TRAIN }
| "batch"  { BATCH }
| "external" { EXTERNAL }
| "new"     { NEW }
(*------------------Syntax---------------------*)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACK }
| ']'      { RBRACK }
| ';'      { SEMI }
| ','      { COMMA }
(* ------------Arithmetic Operators------------ *)
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '%'      { MOD }
| "**"     { POWER }
| '='      { ASSIGN }
| "=>"     { PIPE }
| '^'      { MATTRANPOSE }
| ".."     { MATMUL }
| ".*"     { MATDOTMUL }
(* ------------Boolean/Other Operators------------ *)
| ':'      { COLON }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
(* ------------Control Flow------------ *)
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "return" { RETURN }
(* ------------Types------------ *)
| "int"    { INT }
| "bool"   { BOOL }
| "void"   { VOID }
| "float"   { FLOAT }
| "fmatrix" { FMATRIX }
| "smatrix" { SMATRIX }
| "imatrix" { IMATRIX }
| "tup"     { TUP }
(* ------------Boolean------------ *)
| "true"   { TRUE }
| "false"  { FALSE }
(* ------------Literals and Identifiers------------ *)
| ['0'-'9']+ as lxm { INTLIT(int_of_string lxm) }
| (['0'-'9']+'.'['0'-'9']* | ['0'-'9']*'.'['0'-'9']+) as lxm { FLOATLIT(float_of_string lxm) }
| '"' + ['a'-'z' 'A' - 'Z' '0' - '9'] + '"' as lxm { STRLIT(lxm) }
| ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { VARID(lxm) }
| ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { COMPID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
