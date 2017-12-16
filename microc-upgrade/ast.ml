(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | Pow | Mod | Dot

type uop = Neg | Not | Transpose

(* TODO: to support nested structs, will want to def StructAccess of (StructAccess * string) *)
(* type struct_access = string * string *)

(* Types *)
type primitive_type = Float | Int | Bool | Void | String | Imatrix | Fmatrix
                      (* Tuple *)
type typ =
    PrimitiveType of primitive_type
  | StructType of string
  | FptrType of typ list
  | ArrayType of typ

type location = Local | External

type bind = typ * string

type expr =
    IntLit of int
  | FloatLit of float
  | StringLit of string
  | BoolLit of bool
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | StructAccess of (string * string)
  | StructAssign of (string * string * expr)
  | ArrayAccess of (string * expr)  (* only allow 1-dim arrays *)
  | ArrayAssign of (string * expr * expr)
  | MakeStruct of typ
  | MakeArray of (typ * expr)
  | ArrayLit of (typ * expr list)
  | Pipe of (expr * expr)
  | Dispatch of (string * string * (expr list))
  | MatLit of (expr list list)
  | MatIndex of (string * expr * expr)
  | MatIndexAssign of (string * expr * expr * expr)
  (* | StructLit of (typ * (string * expr) list) *)
  | Null
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    body : stmt list;
    location : location;
}

type struct_decl = {
    name : string;
    members : bind list;
}

type fptr_type = {
  rt: typ;
  args: typ list;
}

type program = {
    global_vars: bind list;
    functions: func_decl list;
    structs: struct_decl list;
}
