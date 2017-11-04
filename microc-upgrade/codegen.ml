(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

exception Bug of string;;

let translate (globals, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "MicroC"

  (* ========================= Types ========================= *)
  and float_t = L.double_type context
  and i32_t  = L.i32_type  context
  and i8_t   = L.i8_type   context
  and i1_t   = L.i1_type   context
  and void_t = L.void_type context in

  let string_t = L.pointer_type i8_t in

  let ltype_of_typ = function
      A.Int -> i32_t
    | A.Float -> float_t
    | A.String -> string_t
    | A.Bool -> i1_t
    | A.Void -> void_t in
  (* ========================================================= *)

  (* Declare each global variable; remember its value in a map *)
  let global_vars =
    let global_var m (t, name) =
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add name (L.define_global name init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (*
  Define each function (arguments and return type) so we can call it
  Builds a map fname [string] -> ([llvalue], [Ast.func_decl])
  *)
  let local_functions =
    List.filter (fun fdecl -> fdecl.A.location = A.Local) functions
  and extern_functions =
    List.filter (fun fdecl -> fdecl.A.location = A.External) functions in

  let extern_decls =
    let extern_decl m fdecl =
      let name = fdecl.A.fname and
        formal_types = Array.of_list
          (List.map(fun (t, _) -> ltype_of_typ t) fdecl.A.formals) in
      let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
      StringMap.add name (L.declare_function name ftype the_module, fdecl) m in
    List.fold_left extern_decl StringMap.empty extern_functions in

  let function_decls =  (* TODO: make a local_fn_decls and extern_fn_decls *)
    let function_decl m fdecl =
      let name = fdecl.A.fname
      and formal_types = Array.of_list
        (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals) in
      let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty local_functions in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = try StringMap.find fdecl.A.fname function_decls with Not_found -> raise (Bug "2") in
    (* return an instruction builder positioned at end of formal store/loads *)
    let builder = L.builder_at_end context (L.entry_block the_function) in

    (* let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in *)

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      (* need to alloc and store params *)
      let add_formal m (t, n) p =
        L.set_value_name n p;
  	    let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);  (* local is stack pointer *)
        StringMap.add n local m in
      (* only need to alloc local vars *)
      let add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder
        in StringMap.add n local_var m in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
          (Array.to_list (L.params the_function)) in
    (* produces a map name[string] --> stack pointer [llvalue] *)
    List.fold_left add_local formals fdecl.A.locals in

    (* Return the llvalue for a variable or formal argument *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    (* ========================= Binary Operators ========================= *)

    let default_ops = function
      A.Add     -> L.build_add
    | A.Sub     -> L.build_sub
    | A.Mult    -> L.build_mul
    | A.Div     -> L.build_sdiv
    | A.And     -> L.build_and
    | A.Or      -> L.build_or
    | A.Equal   -> L.build_icmp L.Icmp.Eq
    | A.Neq     -> L.build_icmp L.Icmp.Ne
    | A.Less    -> L.build_icmp L.Icmp.Slt
    | A.Leq     -> L.build_icmp L.Icmp.Sle
    | A.Greater -> L.build_icmp L.Icmp.Sgt
    | A.Geq     -> L.build_icmp L.Icmp.Sge
    | _         -> raise Not_found
    in

    let float_ops = function
      A.Add     -> L.build_fadd
    | A.Sub     -> L.build_fsub
    | A.Mult    -> L.build_fmul
    | A.Div     -> L.build_fdiv
    | A.Equal   -> L.build_fcmp L.Fcmp.Ueq
    | A.Neq     -> L.build_fcmp L.Fcmp.Une
    | A.Less    -> L.build_fcmp L.Fcmp.Ult
    | A.Leq     -> L.build_fcmp L.Fcmp.Ule
    | A.Greater -> L.build_fcmp L.Fcmp.Ugt
    | A.Geq     -> L.build_fcmp L.Fcmp.Uge
    | _ -> raise Not_found
    in

    (* ==================================================================== *)

    (* Construct code for an expression; return its value *)
    let rec expr builder = function
        A.IntLit i -> L.const_int i32_t i
      | A.FloatLit f -> L.const_float float_t f
      | A.StringLit s -> L.build_global_stringptr (Scanf.unescaped s) "str" builder
      | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | A.Noexpr -> L.const_int i32_t 0
      | A.Id s -> L.build_load (lookup s) s builder
      | A.Binop (e1, op, e2) ->
	       let e1' = expr builder e1
         and e2' = expr builder e2 in
         let l_typ = L.type_of e1' in
         let build_op =
         try
           (match l_typ with
             _ when l_typ = float_t -> float_ops op
             (* TODO: Add matrix operations here *)
           | _ -> default_ops op
           )
         with Not_found -> raise (Failure ((A.string_of_op op) ^ " not defined for "
                                 ^ (L.string_of_lltype l_typ) ^ " in "
                                 ^ (A.string_of_expr e2)))
         in
         build_op e1' e2' "tmp" builder
      | A.Unop(op, e) ->
	       let e' = expr builder e in
         let l_typ = L.type_of e' in
         (match op with
	          A.Neg     -> if l_typ = float_t then L.build_fneg else L.build_neg
          | A.Not     -> L.build_not
         ) e' "tmp" builder
      | A.Assign (s, e) ->
            let e' = expr builder e in
            ignore (L.build_store e' (lookup s) builder); e'
      (* | A.Call ("print", [e]) | A.Call ("printb", [e]) -> *)
      | A.Call ("printf", act) ->
         (* TODO: make generic *)
         (* TODO: add printb and print library fns *)
         let actuals = List.map (expr builder) act in
  	     L.build_call
            printf_func
            (Array.of_list actuals)
            "printf"
            builder
      | A.Call (f, act) ->
         (* we double reverse here for historic reasons. should we undo?
            Need to specify the order we eval fn arguments in LRM
         *)
         let actuals = List.rev (List.map (expr builder) (List.rev act)) in
         if (StringMap.mem f function_decls) then
           let (fdef, fdecl) = StringMap.find f function_decls in
           let result_name = (match fdecl.A.typ with A.Void -> "" | _ -> f ^ "_result") in
           L.build_call fdef (Array.of_list actuals) result_name builder
         else 
           let (fdef, fdecl) = StringMap.find f extern_decls in
           let result_name = (match fdecl.A.typ with A.Void -> "" | _ -> f ^ "_result") in
           L.build_call fdef (Array.of_list actuals) result_name builder
    in

    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (f builder)

      in

    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
        A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); builder
      | A.Return e -> ignore
          (match fdecl.A.typ with
	          A.Void -> L.build_ret_void builder
            | _ -> L.build_ret (expr builder e) builder
          ); builder
      | A.If (predicate, then_stmt, else_stmt) ->
          let bool_val = expr builder predicate in
          let merge_bb = L.append_block context "merge" the_function in
          let then_bb = L.append_block context "then" the_function in
          add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
                       (L.build_br merge_bb);
          let else_bb = L.append_block context "else" the_function in
          add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
                       (L.build_br merge_bb);
          ignore (L.build_cond_br bool_val then_bb else_bb builder);
          L.builder_at_end context merge_bb
      | A.While (predicate, body) ->
	       let pred_bb = L.append_block context "while" the_function in
                       ignore (L.build_br pred_bb builder);
         let body_bb = L.append_block context "while_body" the_function in
         add_terminal (stmt (L.builder_at_end context body_bb) body)
	                    (L.build_br pred_bb);
         let pred_builder = L.builder_at_end context pred_bb in
         let bool_val = expr pred_builder predicate in
	       let merge_bb = L.append_block context "merge" the_function in
	       ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
      	 L.builder_at_end context merge_bb
      | A.For (e1, e2, e3, body) -> stmt builder
          ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block fdecl.A.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.typ with
      (* TODO: catch void *)
        A.Void -> L.build_ret_void
      | A.Float -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0)
    )
  in

  List.iter build_function_body local_functions;
  the_module
