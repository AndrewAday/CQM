open Ast

(*=============================Pretty-printing functions===================== *)
let string_of_primitive_type = function
      Int -> "int"
    | Float -> "float"
    | Bool -> "bool"
    | Void -> "void"
    | String -> "string"
    | Imatrix -> "imatrix"
    | Fmatrix -> "fmatrix"

let rec string_of_typ = function
    PrimitiveType(t) -> string_of_primitive_type t
  | StructType(s)    -> "struct " ^ s
  | ArrayType(typ) -> (string_of_typ typ) ^ "[]"
  | FptrType(typs) -> String.concat ", " (List.map string_of_typ typs)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Pow -> "**"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | Dot -> ".."

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"
  | Transpose -> "^"

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
  (*
  | Slice(b, s, e) ->
      string_of_expr b ^ ":" ^ string_of_expr s ^ ":" ^ string_of_expr e
  | Tupselect(v, e) -> string_of_expr v ^ "[" ^ string_of_expr e ^ "]"
  | Tupassign(v, e, x) ->
      string_of_expr v ^ "[" ^ string_of_expr e ^ "] = " ^ string_of_expr x *)
  (* | Matselect(v, e1, e2) ->
      string_of_expr v ^ "[" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ "]"
  | Matassign(v, e1, e2, x) -> string_of_expr v ^ "[" ^ string_of_expr e1 ^
      ", " ^ string_of_expr e2 ^ "] = " ^ string_of_expr x
  | TupLit(el) -> "[" ^ String.concat ", " (List.map string_of_expr el) ^ "]"
  | MatLit(el) -> "[" ^ String.concat "; " (List.map (fun e ->
      String.concat ", " (List.map string_of_expr e)) el) ^ ";]" *)
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""
  | StructAccess(s_name, member) -> s_name ^ "." ^ member
  | StructAssign(s_name, member, e) -> s_name ^ "." ^ member ^ " = " ^ (string_of_expr e)
  | ArrayAccess(arr_name, e) -> arr_name ^ "[" ^ string_of_expr e ^ "]"
  | ArrayAssign(arr_name, e1, e2) -> arr_name ^ "[" ^ string_of_expr e1 ^ "]" ^ "=" ^ string_of_expr e2
  | MakeStruct(t) -> "make(" ^ string_of_typ t ^ ")"
  | MakeArray(t,e) -> "make(" ^ string_of_typ t ^ "," ^ string_of_expr e ^ ")"
  | ArrayLit(typ, el) -> "(" ^ string_of_typ typ ^ ") {" ^ String.concat ", " (List.map string_of_expr el) ^ "}"
  | Pipe(e1, e2) -> string_of_expr e1 ^ " => " ^ string_of_expr e2
  | Dispatch(strct, mthd_name, el) ->
    strct ^ "." ^ mthd_name ^ ".(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  (* | StructLit(typ, bind_list) -> ignore(bind_list); string_of_typ typ (* TODO: make this real lol *) *)
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
(*=========================================================================== *)


(*=============================Semantic Type Checkers ======================= *)
let check_not_void exceptf = function
    (PrimitiveType(t), n) when t = Void -> raise (Failure (exceptf n))
  | _ -> ()

let check_no_structs exceptf = function
    (StructType(n), _) -> raise (Failure (exceptf n))
  | _ -> ()

let rec check_asn_silent lvaluet rvaluet =
  match (lvaluet, rvaluet) with
      (PrimitiveType(p1), PrimitiveType(p2)) -> if p1 = p2 then true else false
    | (StructType(s1), StructType(s2)) -> if s1 = s2 then true else
        (print_endline (s1 ^ s2); false)
    | (ArrayType(typ1), ArrayType(typ2)) ->
        if check_asn_silent typ1 typ2 then true else false
    | (FptrType(fp1), FptrType(fp2)) ->
        if List.length fp1 != List.length fp2 then
          (print_endline (string_of_typ (FptrType(fp1)) ^ string_of_typ (FptrType(fp2))); false)
        else if fp1 = fp2 then true else
          (print_endline (string_of_typ (FptrType(fp1)) ^ string_of_typ (FptrType(fp2))); false)
    | _ -> false

(* Raise an exception of the given rvalue type cannot be assigned to
   the given lvalue type *)
   (* TODO: pattern match on typ type *)
let check_assign lvaluet rvaluet expr =
  if check_asn_silent lvaluet rvaluet then lvaluet else raise
  (Failure ("illegal assignment " ^ string_of_typ lvaluet ^
            " = " ^ string_of_typ rvaluet ^ " in " ^
            string_of_expr expr))

let check_func_param_assign lvaluet rvaluet err =
  if check_asn_silent lvaluet rvaluet then lvaluet else raise err

let rec contains x = function
    [] -> false
  | hd :: tl -> if x = hd then true else contains x tl

let rec try_get x = function
    [] -> None
  | hd :: tl -> if x = hd then Some x else try_get x tl

let match_primitive primitives = function
    PrimitiveType(p) -> contains p (Array.to_list primitives)
  | _ -> false

let match_struct = function
    StructType(_) -> true
  | _ -> false

let match_array = function
    ArrayType(_) -> true
  | _ -> false

(*============================== List Checkers ============================== *)
let report_duplicate exceptf lst =
  let rec helper = function
      n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
    | _ :: t -> helper t
    | [] -> ()
  in helper (List.sort compare lst)

let rec get_last = function
  [t] -> t
| _ :: tl -> get_last tl
| _ -> raise (Failure "must be nonempty list")

(*============================== Struct Checkers ============================ *)
let check_struct_not_empty exceptf = function
    { name = n; members = []; } -> raise (Failure (exceptf n))
  | _ -> ()

let check_struct_no_nested exceptf struct_decl =
  let n = struct_decl.name in
  if
    List.exists match_struct (List.map (fun member -> fst member) struct_decl.members)
  then raise (Failure (exceptf n)) else ()

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

let is_struct_access s =
  try
    ignore (Str.search_forward (Str.regexp "[.]") s 0); true
  with Not_found -> false

let parse_struct_access s =
  let l = Str.split (Str.regexp "[.]") s in
  let a = Array.of_list l in
  (a.(0), a.(1))

(* foo.bar(), converts bar to __foo_bar(foo) *)
let methodify mthd_name s_name = "__" ^ s_name ^ "_" ^ mthd_name

(*============================== Array Checkers ============================= *)
let check_array_or_throw typ a_name =
  if match_array typ then () else raise (Failure (a_name ^ " is not an array"))

let get_array_type = function
    ArrayType(typ) -> typ
  | _ -> raise (Failure "invalid array type")

(*============================= Function Checkers =========================== *)
let get_result_primitive_name f_name = function
    Void -> ""
  | _ -> f_name ^ "_result"
let get_result_name f_name = function
    PrimitiveType(t) -> get_result_primitive_name f_name t
  | _ -> f_name ^ "_result"

let parse_fptr_type typ_list =
  let arg_typs = if List.length typ_list = 1 then [] else
    (List.rev (List.tl (List.rev typ_list)))
  in
  let ret_typ = get_last typ_list in
  (arg_typs, ret_typ)

(*================================== Misc==================================== *)
let try_get_id_str = function
  Id(s) -> Some s
| _ -> None
