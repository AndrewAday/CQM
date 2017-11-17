(* Semantic checking for the MicroC compiler *)

open Ast
open Util

module StringMap = Map.Make(String)

(* Semantic checking of a program. Returns void if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check program =

  let globals = program.global_vars
  and functions = program.functions
  and structs = program.structs in

(*=========================== Checking Globals ===============================*)
  List.iter (check_not_void (fun n -> "illegal void global " ^ n)) globals;

  (* TODO: support global structs. To do this construct struct definitions first in codegen *)
  (* List.iter (check_no_opaque (fun n -> "opaque struct " ^ n)) globals; *)

  report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);

(*=========================== Checking Structs ===============================*)
(* TODO: struct empty fail test, struct duplicate fail test *)
(* TODO: passing struct info function test *)
  List.iter (check_struct_not_empty (fun n -> "empty struct " ^ n)) structs;

  report_duplicate (fun n -> "duplicate struct name: " ^ n)
    (List.map (fun s -> s.name) structs);

  let struct_decls = List.fold_left (fun m sd -> StringMap.add sd.name sd m)
                      StringMap.empty structs in

  let get_struct_decl s =
    try StringMap.find s struct_decls
    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in
(*=========================== Checking Functions =============================*)

  (* if List.mem "print" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function print may not be defined")) else (); *)

  report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.fname) functions);

  (* Function declaration for a named function *)
  (* let built_in_decls = StringMap.singleton "printbig"
     { typ = Void; fname = "printbig"; formals = [(Int, "x")]; locals = []; body = [] }
  in *)
  let built_in_decls = StringMap.empty in

  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
                         built_in_decls functions
  in

  let function_decl s = try StringMap.find s function_decls
       with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = function_decl "main" in (* Ensure "main" is defined *)

  let check_function func =

    List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^
      " in " ^ func.fname)) func.formals;

    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
      (List.map snd func.formals);

    List.iter (check_not_void (fun n -> "illegal void local " ^ n ^
      " in " ^ func.fname)) func.locals;

    report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname)
      (List.map snd func.locals);

    (* Type of each variable (global, formal, or local *)
    (* TODO: add support for global structs *)
    let symbols = List.fold_left (fun m (t, n) -> StringMap.add n t m)
	     StringMap.empty (globals @ func.formals @ func.locals )
    in

    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* ========================= Binary Operators ========================= *)

    let check_default_ops t1 t2 op =
      let helper p1 p2 = function
          Add | Sub | Mult | Div when p1 = Int && p2 = Int -> Int
        | Equal | Neq when p1 = p2 -> Bool
        | Less | Leq | Greater | Geq when p1 = Int && p2 = Int -> Bool
        | And | Or when p1 = Bool && p2 = Bool -> Bool
        | _ -> raise Not_found
      in
      let t =
        match (t1, t2) with
            (PrimitiveType(p1), PrimitiveType(p2)) -> helper p1 p2 op
          | _ -> raise Not_found
      in PrimitiveType(t)
    in

    let check_float_ops t1 t2 op =
      let helper p1 p2 = function
          Add | Sub | Mult | Div when p1 = Float && p2 = Float -> Float
        | Equal | Neq when p1 = p2 -> Bool
        | Less | Leq | Greater | Geq when p1 = Float && p2 = Float -> Bool
        | _ -> raise Not_found
      in
      let t =
      match (t1, t2) with
          (PrimitiveType(p1), PrimitiveType(p2)) -> helper p1 p2 op
        | _ -> raise Not_found
      in PrimitiveType(t)
    in

    let check_string_ops (t1: typ) (t2: typ) op =
      let helper (p1: primitive_type) (p2: primitive_type) = function
          Add when p1 = String && p2 = String -> String
        | Equal | Neq when p1 = p2 -> Bool
        | _ -> raise Not_found
      in
      let t =
        match (t1, t2) with
            (PrimitiveType(p1), PrimitiveType(p2)) -> helper p1 p2 op
          | _ -> raise Not_found
      in PrimitiveType(t)
    in

    let check_unary_ops t op =
      let helper p = function
          Neg when p = Int -> Int
        | Neg when p = Float -> Float
        | Not when p = Bool -> Bool
        | _ -> raise Not_found
      in
      match t with
          PrimitiveType(t) -> PrimitiveType(helper t op)
        | _ -> raise Not_found
    in


    (* ==================================================================== *)

    (* Return the type of an expression or throw an exception *)
    let rec expr : expr -> typ = function
	      IntLit _ -> PrimitiveType(Int)
      | FloatLit _ -> PrimitiveType(Float)
      | BoolLit _ -> PrimitiveType(Bool)
      | StringLit _ -> PrimitiveType(String)
      | Noexpr -> PrimitiveType(Void)
      | Id s -> type_of_identifier s
      (* TODO: illegal access test *)
      | StructAccess (s_name, member) -> ignore(type_of_identifier s_name); (*check it's declared *)
          let s_decl = get_struct_decl s_name in (* get the ast struct_decl type *)
          get_struct_member_type s_decl member
          ("Illegal struct member access: " ^ s_name  ^ "." ^ member)
      | StructAssign (s_name, member, e) as ex ->  (* TODO: add illegal assign test *)
          let t = expr e and struct_decl = get_struct_decl s_name in
          let member_t = get_struct_member_type struct_decl member
              ("Illegal struct member access: " ^ s_name  ^ "." ^ member) in
          check_assign member_t t (Failure ("illegal assignment " ^ string_of_typ member_t ^
  				     " = " ^ string_of_typ t ^ " in " ^
  				     string_of_expr ex))
      | Binop(e1, op, e2) as e -> let t1: typ = expr e1 and t2: typ = expr e2 in
         let expr_type =
           try
             (match t1 with
               PrimitiveType(t) when t = Float -> check_float_ops t1 t2 op
             | PrimitiveType(t) when t = String -> check_string_ops t1 t2 op
             | _ -> check_default_ops t1 t2 op
             )
           with Not_found -> raise (Failure ("illegal binary operator " ^
                                  string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                                  string_of_typ t2 ^ " in " ^ string_of_expr e))
         in expr_type
      | Unop(op, e) as ex -> let t = expr e in
          let expr_type =
            try check_unary_ops t op
        	  with Not_found -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
        	  		                 string_of_typ t ^ " in " ^ string_of_expr ex))
          in expr_type
      | Assign(var, e) as ex -> let lt = type_of_identifier var
                                and rt = expr e in
        check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
				     " = " ^ string_of_typ rt ^ " in " ^
				     string_of_expr ex))
      | Call("printf", _) -> PrimitiveType(Int)
      | Call(fname, actuals) as call -> let fd = function_decl fname in
         if List.length actuals != List.length fd.formals then
           raise (Failure ("expecting " ^ string_of_int
             (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
         else
           List.iter2 (fun (ft, _) e -> let et = expr e in
              ignore (check_assign ft et
                (Failure ("illegal actual argument found " ^ string_of_typ et ^
                " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e))))
             fd.formals actuals;
           fd.typ
    in

    let check_bool_expr e = if not (match_bool (expr e))
     then raise (Failure (
       "expected Boolean expression in " ^ string_of_expr e
       ))
     else () in

    (* Verify a statement or throw an exception *)
    let rec stmt = function
        Block sl -> let rec check_block = function
           [Return _ as s] -> stmt s
         | Return _ :: _ -> raise (Failure "nothing may follow a return")
         | Block sl :: ss -> check_block (sl @ ss)
         | s :: ss -> stmt s ; check_block ss
         | [] -> ()
        in check_block sl
      | Expr e -> ignore (expr e)
      | Return e -> let t = expr e in
          if (check_asn_silent t func.typ) then () else
          raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                         string_of_typ func.typ ^ " in " ^ string_of_expr e))
      | If(p, b1, b2) -> check_bool_expr p; stmt b1; stmt b2
      | For(e1, e2, e3, st) -> ignore (expr e1); check_bool_expr e2;
                               ignore (expr e3); stmt st
      | While(p, s) -> check_bool_expr p; stmt s
    in

    stmt (Block func.body)

  in
  List.iter check_function functions
