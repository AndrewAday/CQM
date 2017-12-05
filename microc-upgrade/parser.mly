/* Ocamlyacc parser for Onion */

%{
open Ast

%}


%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA PERIOD LBRACK RBRACK
%token PLUS MINUS TIMES DIVIDE POW ASSIGN PIPE MOD MATTRANS DOT SLICE
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR NOT
%token RETURN IF ELSE FOR WHILE EXTERN MAKE
%token INT BOOL VOID FLOAT STRING IMATRIX FMATRIX TUPLE STRUCT
%token <int> INTLIT
%token <string> STRINGLIT
%token <float> FLOATLIT
%token <string> ID
%token <string> PNTR
%token EOF

%nonassoc NOELSE
%nonassoc ELSE

%right ASSIGN
%left PIPE
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left SLICE
%left PLUS MINUS
%left TIMES DIVIDE MOD DOT
%left POW
%right NOT NEG
%left MATTRANS

%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */  {{ global_vars = []; functions = []; structs = []; }}
 | decls vdecl    {{
                    global_vars = $2 :: $1.global_vars;
                    functions = $1.functions;
                    structs = $1.structs;
                  }}
 | decls fdecl    {{
                    global_vars = $1.global_vars;
                    functions = $2 :: $1.functions;
                    structs = $1.structs;
                  }}
 | decls str_decl {{
                    global_vars = $1.global_vars;
                    functions = $1.functions;
                    structs = $2 :: $1.structs;
                  }}
str_decl:
  STRUCT ID LBRACE vdecl_list RBRACE
  {{
    name = $2;
    members = List.rev $4;
  }}

fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
      { { typ         = $1;
      	  fname       = $2;
      	  formals     = $4;
      	  locals      = List.rev $7;
          body        = List.rev $8;
          location    = Local; } }
 | EXTERN typ ID LPAREN formals_opt RPAREN SEMI
      { { typ         = $2;
          fname       = $3;
          formals     = $5;
          locals      = [];
          body        = [];
          location    = External; } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [($1,$2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

/*==============================Type Parsing==================================*/
typ:
    primitive_type {PrimitiveType($1)}
  | struct_type    {$1}
  | array_type     {$1}

primitive_type:
    INT     { Int }
  | FLOAT   { Float }
  | STRING  { String }
  | BOOL    { Bool }
  | VOID    { Void }
  | IMATRIX { Imatrix }
  | FMATRIX { Fmatrix }
  /*| TUPLE   { Tuple }*/

struct_type:
    STRUCT ID { StructType($2) }

array_type:
    typ LBRACK RBRACK { ArrayType($1) }

/*============================================================================*/

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
    typ ID SEMI                      { ($1, $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI                                 { Expr $1 }
  | RETURN SEMI                               { Return Noexpr }
  | RETURN expr SEMI                          { Return $2 }
  | LBRACE stmt_list RBRACE                   { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE   { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt      { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
                                              { For($3, $5, $7, $9) }

  | WHILE LPAREN expr RPAREN stmt             { While($3, $5) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    INTLIT           { IntLit($1) }
  | FLOATLIT         { FloatLit($1) }
  | STRINGLIT        { StringLit($1) }
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | ID               { Id($1) }

  | expr PLUS   expr { Binop($1, Add,     $3) }
  | expr MINUS  expr { Binop($1, Sub,     $3) }
  | expr TIMES  expr { Binop($1, Mult,    $3) }
  | expr DIVIDE expr { Binop($1, Div,     $3) }
  | expr POW    expr { Binop($1, Pow,     $3) }
  | expr MOD    expr { Binop($1, Mod,     $3) }
  | expr EQ     expr { Binop($1, Equal,   $3) }
  | expr NEQ    expr { Binop($1, Neq,     $3) }
  | expr LT     expr { Binop($1, Less,    $3) }
  | expr LEQ    expr { Binop($1, Leq,     $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | expr DOT expr { Binop($1, Dot, $3)}
  | expr MATTRANS    { Unop(Transpose, $1) }
  /*| LBRACK rows RBRACK                      { MatLit(List.rev $2) }*/
/*  | LBRACK actuals_opt RBRACK                    { TupLit($2) } */
/*  | ID LBRACK expr COMMA expr RBRACK ASSIGN expr { Matassign(Id($1),$3,$5,$8)} */
/*  | ID LBRACK expr COMMA expr RBRACK             { Matselect(Id($1),$3,$5) } */
/*  | ID LBRACK expr RBRACK ASSIGN expr            { Tupassign(Id($1),$3,$6) } */
/*  | ID LBRACK expr RBRACK                        { Tupselect(Id($1),$3) } */
/*  | expr SLICE expr SLICE expr                   { Slice($1,$3,$5) } */
  | MINUS expr %prec NEG  { Unop(Neg, $2) }
  | NOT expr              { Unop(Not, $2) }
  | ID ASSIGN expr        { Assign($1, $3) }
  /*| expr PIPE expr   { Pipe($1, $3) }*/
  | MAKE LPAREN typ RPAREN    { MakeStruct($3) }
  | MAKE LPAREN typ COMMA expr RPAREN { MakeArray($3, $5) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }
  | ID PERIOD ID      { StructAccess($1, $3) }
  | ID PERIOD ID ASSIGN expr { StructAssign($1, $3, $5) }
  | ID LBRACK expr RBRACK    { ArrayAccess($1, $3) }
  | ID LBRACK expr RBRACK ASSIGN expr { ArrayAssign($1, $3, $6) }

  /*TODO: struct array assign/access */
actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }

 rows:
    actuals_opt             { [$1] }
  | rows COMMA actuals_opt   { $3 :: $1 }
