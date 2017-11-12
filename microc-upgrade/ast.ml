(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | Pow | Mod | Dot

type uop = Neg | Not | Transpose

type typ = Float | Int | Bool | Void | String | Tuple | Imatrix | Fmatrix

type location = Local | External

type bind = typ * string

type expr =
    IntLit of int
  | FloatLit of float
  | StringLit of string
  | BoolLit of bool
  | TupLit of expr list
  | MatLit of expr list list
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Pipe of expr * expr
  | Slice of expr * expr *expr
  | Tupselect of expr * expr
  | Tupassign of expr * expr * expr
  | Matselect of expr * expr * expr
  | Matassign of expr * expr * expr * expr
  | Call of string * expr list
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

type program = bind list * func_decl list

(* Pretty-printing functions *)

let string_of_op = function
    Add       -> "+"
  | Sub       -> "-"
  | Mult      -> "*"
  | Div       -> "/"
  | Pow       -> "**"
  | Mod       -> "%"
  | Equal     -> "=="
  | Neq       -> "!="
  | Less      -> "<"
  | Leq       -> "<="
  | Greater   -> ">"
  | Geq       -> ">="
  | And       -> "&&"
  | Or        -> "||"
  | Dot       -> ".."
  | Matdotmul -> ".*"

let string_of_uop = function
    Neg       -> "-"
  | Not       -> "!"
  | Transpose -> "^"

let rec string_of_expr = function
    IntLit(l)                 -> string_of_int l
  | FloatLit(f)               -> string_of_float f
  | StringLit(s)              -> s
  | BoolLit(true)             -> "true"
  | BoolLit(false)            -> "false"
  | Id(s)                     -> s
  | Binop(e1, o, e2)          -> 
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2

  | Unop(o, e)                -> string_of_uop o ^ string_of_expr e
  | Assign(v, e)              -> v ^ " = " ^ string_of_expr e
  | Pipe(v, e)                -> string_of_expr v ^ " => " ^ string_of_expr e
  | Slice(b, s, e)            -> 
      string_of_expr b ^ ":" ^ string_of_expr s ^ ":" ^ string_of_expr e

  | Tupselect(v, e)           -> string_of_expr v ^ "[" ^ string_of_expr e ^ "]"
  | Tupassign(v, e, x)        ->
      string_of_expr v ^ "[" ^ string_of_expr e ^ "] = " ^ string_of_expr x

  | Matselect(v, e1, e2)      ->
      string_of_expr v ^ "[" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ "]"

  | Matassign(v, e1, e2, x)   -> string_of_expr v ^ "[" ^ string_of_expr e1 ^
      ", " ^ string_of_expr e2 ^ "] = " ^ string_of_expr x
 
  | TupLit(el)                -> 
      "[" ^ String.concat ", " (List.map string_of_expr el) ^ "]"
  
  | MatLit(el)                -> "[" ^ String.concat "; " (List.map (fun e ->
      String.concat ", " (List.map string_of_expr e)) el) ^ ";]"

  | Call(f, el)               ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"

  | Noexpr                    -> ""

let rec string_of_stmt = function
    Block(stmts)        ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"

  | Expr(expr)          -> string_of_expr expr ^ ";\n";
  | Return(expr)        -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2)       ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2

  | For(e1, e2, e3, s)  ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s

  | While(e, s)         -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_typ = function
    Int     -> "int"
  | Float   -> "float"
  | Bool    -> "bool"
  | Void    -> "void"
  | String  -> "string"
  | Tuple   -> "tuple"
  | Imatrix -> "imatrix"
  | Fmatrix -> "fmatrix"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl = match fdecl.location with
    Local -> string_of_typ fdecl.typ ^ " " ^
      fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^ 
      ")\n{\n" ^ String.concat "" (List.map string_of_vdecl fdecl.locals) ^ 
      String.concat "" (List.map string_of_stmt fdecl.body) ^ "}\n"
  | External -> "extern" ^ string_of_typ fdecl.typ ^ " " ^ fdecl.fname ^
      "(" ^ String.concat ", " (List.map snd fdecl.formals) ^ ");\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
