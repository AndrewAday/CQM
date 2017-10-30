%{
open Ast
%}

%token EXTERNAL NEW
%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN PIPE NOT POWER
%token MATMUL MATTRANPOSE MATDOTMUL
%token EQ NEQ LT LEQ GT GEQ AND OR COLON
%token TRUE FALSE
%token RETURN IF ELSE FOR
%token INT BOOL VOID FLOAT STRING SMATRIX IMATRIX FMATRIX TUP
%token LAYER MODEL RUN
%token TRAINABLE TRAIN BATCH
%token <int> INTLIT 
%token <float> FLOATLIT
%token <string> STRLIT
%token <string> VARID
%token <string> COMPID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN PIPE
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left COLON
%left PLUS MINUS
%left TIMES DIVIDE MATMUL MATDOTMUL
%left POWER
%right NOT NEG
%left MATTRANPOSE

%start program
%type <Ast.program> program

%%

(*----------------------------------OVERALL-----------------------------------*)
program:
  decls EOF { $1 }

decls:
  /* nothing */ { [], [] }
| decls section { $2 :: $1 }

(*---------------------------------SECTIONS-----------------------------------*)
section:
  LAYER COMPID LPAREN formals_opt RPAREN LBRACE layer_stmt_list RBRACE 
    { { name = $2;
        params = $4;
        stmts = List.rev $7;
        location = Local; } }
| MODEL COMPID LPAREN model_formals_opt RPAREN LBRACE model_stmt_list RBRACE
    { { name = $2;
        params = $4;
        stmts = List.rev $7;
        location = Local; } }
(*
| EXTERNAL COMPID LPAREN model_formals_opt RPAREN LBRACE model_stmt_list RBRACE
    { { name = $2;
        params = $4;
        stmts = [];
        location = External; } }

| EXTERNAL LAYER COMPID LPAREN formals_opt RPAREN LBRACE layer_stmt_list RBRACE 
    { { name = $2;
        params = $4;
        stmts = [];
        location = External; } } *)
| RUN LPAREN RPAREN LBRACE run_stmt_list RBRACE
    { { name = "run";
        params = $3; 
        stmts = List.rev $6; } }

layer_stmt_list:
  /* nothing */               { [] }
| layer_stmt_list layer_stmt SEMI { $2 :: $1 }

model_stmt_list:
  /* nothing */               { [] }
| model_stmt_list model_stmt SEMI { $2 :: $1 }
| model_stmt_list model_stmt_pipe SEMI { $2 :: $1 }

run_stmt_list:
  /* nothing */               { [] }
| run_stmt_list run_stmt SEMI { $2 :: $1 }

gen_stmt_list:
  /* nothing */               { [] }
| gen_stmt_list gen_stmt SEMI { $2 :: $1 }

(*-------------------------------STATEMENTS-----------------------------------*)
layer_stmt:
  gen_stmt      { $1 }
(*  TRAINABLE var { Trainable($2) } *)

model_stmt_pipe:
  model_stmt PIPE model_stmt { Binop($1, $2, $3) }

model_stmt:
  model_expr { Expr $1 }
| FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN model_stmt_list
                                                    { For($3, $5, $7, $9) }

run_stmt:
  gen_stmt                          { $1 }
(* TRAIN LBRACE gen_stmt_list RBRACE { Train(List.rev $3) } *)

gen_stmt:
  expr                                              { Expr $1 }
| RETURN                                            { Return Noexpr }
| RETURN expr                                       { Return $2 }
| LBRACE layer_stmt_list RBRACE                     { Block(List.rev $2) }
| IF LPAREN expr RPAREN gen_stmt %prec NOELSE       { If($3, $5, Block([]))}
| IF LPAREN expr RPAREN gen_stmt ELSE gen_stmt      { If($3, $5, $7) }
| FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN gen_stmt
                                                    { For($3, $5, $7, $9) }

(*------------------------------EXPRESSIONS-----------------------------------*)

model_expr:
  VARID         { VarId($1) }
| COMPID LPAREN actuals_opt RPAREN { Call($1, $3) }

expr_opt:
  /* nothing */ { Noexpr }
| expr          { $1 }

expr:
  INTLIT                { Lit($1) }
| FLOATLIT              { FloatList($1) }
| STRLIT                { StringLit($1) }
| TRUE                  { BoolLit(true) }
| FALSE                 { BoolLit(false) }
| VARID                 { VarId($1) }
| expr PLUS   expr      { Binop($1, Add,   $3) }
| expr MINUS  expr      { Binop($1, Sub,   $3) }
| expr TIMES  expr      { Binop($1, Mult,  $3) }
| expr DIVIDE expr      { Binop($1, Div,   $3) }
(*| expr POWER  expr      { Binop($1, Power,   $3) }*)
| expr EQ     expr      { Binop($1, Equal, $3) }
| expr NEQ    expr      { Binop($1, Neq,   $3) }
| expr LT     expr      { Binop($1, Less,  $3) }
| expr LEQ    expr      { Binop($1, Leq,   $3) }
| expr GT     expr      { Binop($1, Greater, $3) }
| expr GEQ    expr      { Binop($1, Geq,   $3) }
| expr AND    expr      { Binop($1, And,   $3) }
| expr OR     expr      { Binop($1, Or,    $3) }
(*| expr MATMUL expr      { Binop($1, Matmul, $3) }
| expr MATDOTMUL expr   { Binop($1, Matdotmul, $3) }
| expr MATTRANPOSE      { Unop(Transpose, $1) } *)
| MINUS expr %prec NEG  { Unop(Neg, $2) }
| NOT expr              { Unop(Not, $2) }
| var ASSIGN expr       { Assign($1, $3) }
| VARID ASSIGN expr     { Assign($1, $3) }
| COMPID LPAREN actuals_opt RPAREN { Call($1, $3) }
| LPAREN expr RPAREN    { $2 }
| MINUS expr %prec NEG                  { Unop(Neg, $2) }
| NOT expr                              { Unop(Not, $2) }
(* CITE: MiniMat
| LBRACK rows SEMI RBRACK               { MatLit(List.rev $2) }
| LBRACK actuals_opt RBRACK             { TupLit($2) }
| VARID LBRACK expr COMMA expr RBRACK ASSIGN expr
                                        { Matassign(VarId($1),$3,$5,$8)}
| VARID LBRACK expr COMMA expr RBRACK   { Matselect(VarId($1),$3,$5) }
| VARID LBRACK expr RBRACK ASSIGN expr  { Tupassign(VarId($1),$3,$6) }
| VARID LBRACK expr RBRACK              { Tupselect(VarId($1),$3) }
| expr COLON expr COLON expr            { Stride($1,$3,$5) }
| NEW typ LPAREN actuals_opt RPAREN     { Call(string_of_typ $2, $4)} *)

(*------------------------------LISTS OF PARAMS-------------------------------*)
model_formals_opt:
  /* nothing */ { [] }
| model_formal_list   { List.rev $1 }

formals_opt:
  /* nothing */ { [] }
| formal_list   { List.rev $1 }

model_formal_list:
  var                         { [$1] }
| model_formal_list COMMA var { $3 :: $1 }
(*| BATCH var                 { [BatchVar($1)] }*)

formal_list:
  var                   { [$1] }
| formal_list COMMA var { $3 :: $1 }

(* CITE: MiniMat
rows:
  actuals_opt           { [$1] }
| rows SEMI actuals_opt { $3 :: $1 } *)

actuals_opt:
  /* nothing */ { [] }
| actuals_list  { List.rev $1 }

actuals_list:
  expr                    { [$1] }
| actuals_list COMMA expr { $3 :: $1 }


(*------------------------------BUILDING BLOCKS-------------------------------*)
var:
  typ VARID { ($1, $2) }
(* | VARID     { (Fmatrix, $1) } *)

typ:
    INT { Int }
  | BOOL { Bool }
  | VOID { Void }
(*| FLOAT { Float }
  | STRING { String }
  | FMATRIX { Fmatrix }
  | SMATRIX { Smatrx }
  | IMATRIX { Imatrix } *)
