open Ast

(*=============================Semantic Type Checkers ======================= *)
let check_not_void exceptf = function
    (PrimitiveType(t), n) when t = Void -> raise (Failure (exceptf n))
  | _ -> ()

let check_asn_silent lvaluet rvaluet =
  match (lvaluet, rvaluet) with
      (PrimitiveType(p1), PrimitiveType(p2)) -> if p1 == p2 then true else false
    | (StructType(s1), StructType(s2)) -> if s1 == s2 then true else false
    | _ -> false

(* Raise an exception of the given rvalue type cannot be assigned to
   the given lvalue type *)
   (* TODO: pattern match on typ type *)
let check_assign lvaluet rvaluet err =
  if check_asn_silent lvaluet rvaluet then lvaluet else raise err

let match_bool = function
      PrimitiveType(p) -> if p = Bool then true else false
    | _ -> false

(*============================== List Checkers ============================== *)
let report_duplicate exceptf lst =
  let rec helper = function
      n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
    | _ :: t -> helper t
    | [] -> ()
  in helper (List.sort compare lst)

(*============================== Struct Checkers ============================ *)
let check_struct_not_empty exceptf = function
    { name = n; members = []; } -> raise (Failure (exceptf n))
  | _ -> ()

let check_no_opaque exceptf = function
    (_, n) -> raise (Failure (exceptf n))

let get_struct_member_type struct_decl member exceptf =
  try
    let member_bind = List.find (fun (_, n) -> n = member) struct_decl.members
    in fst member_bind  (* return the typ *)
  with Not_found -> raise (Failure exceptf)

let get_struct_member_idx struct_decl member =
  let rec find idx = function
      [] -> raise Not_found
    | (_,name) :: tl -> if name = member then idx else find (idx+1) tl
  in
  try find 0 struct_decl.members
  with Not_found -> raise (Failure (member ^ "not found in struct " ^ struct_decl.name))


(*============================= Function Checkers =========================== *)
let get_result_primitive_name f_name = function
    Void -> ""
  | _ -> f_name ^ "_result"
let get_result_name f_name = function
    PrimitiveType(t) -> get_result_primitive_name f_name t
  | _ -> f_name ^ "_result"


(*=============================Pretty-printing functions===================== *)
let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | FloatLit(f) -> string_of_float f
  | StringLit(s) -> s
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""
  | StructAccess(s_name, member) -> s_name ^ "." ^ member
  | StructAssign(s_name, member, e) -> s_name ^ "." ^ member ^ " = " ^ (string_of_expr e)

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_primitive_type = function
      Int -> "int"
    | Float -> "float"
    | Bool -> "bool"
    | Void -> "void"
    | String -> "string"

let string_of_typ = function
    PrimitiveType(t) -> string_of_primitive_type t
  | StructType(s)    -> "struct " ^ s

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl = match fdecl.location with
    Local -> string_of_typ fdecl.typ ^ " " ^
      fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
      ")\n{\n" ^ String.concat "" (List.map string_of_vdecl fdecl.locals) ^
      String.concat "" (List.map string_of_stmt fdecl.body) ^ "}\n"
  | External -> "extern" ^ string_of_typ fdecl.typ ^ " " ^ fdecl.fname ^
      "(" ^ String.concat ", " (List.map snd fdecl.formals) ^ ");\n"

let string_of_struct_decl s =
  let vdecls = String.concat "" (List.map string_of_vdecl s.members) in
  "struct " ^ s.name ^ "{\n" ^
    vdecls ^
  "}\n"

let string_of_program program =
  let vars = program.global_vars
  and funcs = program.functions
  and structs = program.structs in
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "" (List.map string_of_struct_decl structs) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
  (*========================================================================= *)