open Ast

type sexpr =
    SLiteral of int
  | SFloatLit of float
  | SBoolLit of bool
  | SStringLit of string
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SAssign of string * sexpr
  | SCall of string * sexpr list
  | SNoexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt

type sfunc_decl = {
    sftyp : typ;
    sfname : string;
    sformals : bind list;
    slocals: bind list;
    sbody : sstmt list;
  }

type sprogram =  bind list * sfunc_decl list
