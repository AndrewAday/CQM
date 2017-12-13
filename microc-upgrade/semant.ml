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
  (* List.iter (check_no_structs (fun n -> "illegal struct global " ^ n)) globals; *)

  (* TODO: support global structs. To do this construct struct definitions first in codegen *)
  (* List.iter (check_no_opaque (fun n -> "opaque struct " ^ n)) globals; *)

  report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);

(*=========================== Checking Structs ===============================*)
(* TODO: struct empty fail test, struct duplicate fail test *)
(* TODO: passing struct info function test *)
  List.iter (check_struct_not_empty (fun n -> "empty struct " ^ n)) structs;
  List.iter (check_struct_no_nested (fun n -> "nested struct " ^ n)) structs;
  report_duplicate (fun n -> "duplicate struct name: " ^ n)
    (List.map (fun s -> s.name) structs);

  let struct_decls = List.fold_left (fun m sd -> StringMap.add sd.name sd m)
                      StringMap.empty structs in

(*=========================== Checking Functions =============================*)
  let built_in_keywords = Array.to_list
    [|
      "make"; "len"; "free"; "free_arr"; "size"; "memset"; "memcpy";
      "concat"; "append";
    |]
  in

  List.iter (fun fname ->
    if List.mem fname (List.map (fun fd -> fd.fname) functions)
    then raise (Failure ("function " ^ fname ^ " may not be defined"))
  ) built_in_keywords;

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

    let get_struct_decl s =
      match type_of_identifier s with
        StructType(s_name) -> (
          try StringMap.find s_name struct_decls
          with Not_found -> raise (Failure ("undeclared identifier " ^ s))
        )
      | _ -> raise (Failure ("Not a struct " ^ s))
    in

    (* ==================================================================== *)

    (* Return the type of an expression or throw an exception *)
    let rec expr : expr -> typ = function
	      IntLit _ -> PrimitiveType(Int)
      | FloatLit _ -> PrimitiveType(Float)
      | BoolLit _ -> PrimitiveType(Bool)
      | StringLit _ -> PrimitiveType(String)
      | Noexpr -> PrimitiveType(Void)
      | Id s ->
        let ret_typ =
        try
          type_of_identifier s
        with _ ->  (*try searching for function ptr *)
          try
            let fdecl = function_decl s in
              let rt_typ = fdecl.typ
              and form_typs = List.map (fun (typ, _) -> typ) fdecl.formals in
                FptrType (List.append form_typs [rt_typ])
          with _ -> raise (Failure ("undeclared identifier " ^ s))
        in ret_typ
      | MakeStruct (typ) as ex ->
          if match_struct typ then typ else
          raise (Failure  ("illegal make, must be type struct, in " ^ string_of_expr ex))
      | MakeArray (typ, e) as ex ->
          if match_primitive [|Int|] (expr e) then ArrayType(typ) else
          raise (Failure  ("illegal make, must provide integer size, in " ^ string_of_expr ex))
      | StructAccess (s_name, member) -> ignore(type_of_identifier s_name); (*check it's declared *)
          let s_decl = get_struct_decl s_name in (* get the ast struct_decl type *)
          get_struct_member_type s_decl member
          ("Illegal struct member access: " ^ s_name  ^ "." ^ member)
      | StructAssign (s_name, member, e) as ex ->  (* TODO: add illegal assign test *)
          let t = expr e and struct_decl = get_struct_decl s_name in
          let member_t = get_struct_member_type struct_decl member
              ("Illegal struct member access: " ^ s_name  ^ "." ^ member) in
          check_assign member_t t ex
      | ArrayAccess (a_name, _) ->
          let t = type_of_identifier a_name in
          check_array_or_throw t a_name;
          get_array_type t
      | ArrayAssign (a_name, _, e) as ex ->
          let t = (type_of_identifier a_name)
          and expr_t = (expr e) in
          check_array_or_throw t a_name;
          let arr_t = get_array_type t in
          check_assign arr_t expr_t ex
      | ArrayLit(arr_type, expr_list) as ex ->
        if match_array arr_type then
          let inner_type = get_array_type arr_type in
          List.iter (fun e -> ignore(check_assign inner_type (expr e) ex)) expr_list;
          arr_type
        else raise (Failure ("expected array type in expr " ^ string_of_expr ex))
      | Binop(e1, op, e2) as e -> let typ1 = expr e1 and typ2 = expr e2 in
        let ret =
        try
           match (typ1, typ2) with
              (PrimitiveType(t1), PrimitiveType(t2)) -> (
                let inner_type =
                     match op with
                        Add | Sub | Mult | Div when (t1 = t2) &&
                          (t1 = Int || t1 = Float || t1 = String || t1 = Imatrix || t1 = Fmatrix) -> t1
                      | Dot when (t1 = t2) && (t1 = Imatrix || t2 = Fmatrix) -> t1
                      (* Is it possible here to cast int scalar types into double to prevent
                      a compiler error later on? *)
                      | Add | Sub | Mult | Div when (t1 = Fmatrix || t1 = Imatrix) && (t2 = Float) -> t1
                      | Add | Sub | Mult | Div when (t1 = Float) && (t2 = Fmatrix || t2 = Imatrix) -> t2
                      | Equal | Neq when (t1 = t2) ->
                        let check_eq_typ = function
                          | (Imatrix | Fmatrix)  -> Imatrix
                          | (Float | Int | Bool) -> Bool
                          | _                    -> raise Not_found
                        in check_eq_typ t1

                      | Less | Leq | Greater | Geq when (t1 = t2) && (t1 = Float || t1 = Int) -> Bool
                      | And | Or when (t1 = t2) && (t1 = Bool) -> Bool

                      | _ -> raise Not_found
                        (* TODO: Need to figure out return type of a boolean matrix... is that just an Imatrix?? *)
                in PrimitiveType(inner_type)
              )
            | _ -> raise (Failure "not implemented")
        with Not_found -> raise (Failure ("Illegal binary operator " ^
                            string_of_typ typ1 ^ " " ^ string_of_op op ^ " " ^
                            string_of_typ typ2 ^ " in " ^ string_of_expr e))
        in ret
      | Unop(op, e) as ex ->
         let typ1 = expr e in (
           match typ1 with
              PrimitiveType(t) -> (
                 let inner_type =
                	 match op with
                	    Neg when (t != String && t != Bool) -> t
                    | Not when t = Bool -> t
                    | Transpose when (t = Fmatrix || t = Imatrix) -> t
                    | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
                	  		                 string_of_typ typ1 ^ " in " ^ string_of_expr ex))
                 in PrimitiveType(inner_type)
              )
            | _ -> raise (Failure "not implemented")
         )
      | Assign(var, e) as ex ->
        let lt = type_of_identifier var
        and rt = expr e in
        check_assign lt rt ex
  (*============================= built in fns ===============================*)
      | Call("printf", _) -> PrimitiveType(Int)
      | Call("len", [e]) ->
        let t = expr e in
        if match_array t then PrimitiveType(Int)
        else raise (Failure ("Illegal argument of type " ^ string_of_typ t ^ " to len, must be array"))
      | Call("free", [e]) ->
        let t = expr e in
        if (match_struct t || match_primitive [|Imatrix; Fmatrix|] t)
        then PrimitiveType(Void)
        else raise (Failure ("Illegal argument of type " ^ string_of_typ t ^ " to free, must be struct or matrix"))
      | Call("free_arr", [e]) ->
        let t = expr e in
        if (match_array t)
        then PrimitiveType(Void)
        else raise (Failure ("Illegal argument of type " ^ string_of_typ t ^ " to free_arr, must be array"))
      | Call("concat", [e1; e2]) as ex ->
        let arr1 = expr e1
        and arr2 = expr e2 in
        if match_array arr1 && match_array arr2
        then check_assign arr1 arr2 ex
        else raise (Failure ("Illegal arguments of types " ^
          string_of_typ arr1 ^ "and" ^ string_of_typ arr2 ^
          " to concat, must be arrays"))
      | Call("append", [e1; e2]) as ex ->
        let arr = expr e1
        and elem = expr e2 in
        ignore (check_assign (get_array_type arr) elem ex); arr
  (*==========================================================================*)
      | Call(fname, actuals) as call ->
        try
          let var = type_of_identifier fname in
            match var with
              FptrType(fp) ->
                let (args, rt) = parse_fptr_type fp in
                  if List.length actuals != List.length args then
                    raise (Failure ("Fail"))
                  else
                    List.iter2 (fun ft e -> let et = expr e in
                      ignore (check_func_param_assign ft et (Failure ("Fail"))))
                    args actuals;
                  rt
            | _ -> raise (Failure ("Fail"))
        with _ ->
          let fd = function_decl fname in
           if List.length actuals != List.length fd.formals then
             raise (Failure ("expecting " ^ string_of_int
               (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
           else
             List.iter2 (fun (ft, _) e -> let et = expr e in
                ignore (check_func_param_assign ft et
                  (Failure ("illegal actual argument found " ^ string_of_typ et ^
                  " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e))))
               fd.formals actuals;
             fd.typ
    in

    let check_bool_expr e = if not (match_primitive [|Bool|] (expr e))
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
