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
module U = Util

module P = Printf

module StringMap = Map.Make(String)

exception Bug of string;;

let translate program =
  let globals = program.A.global_vars
  and functions = program.A.functions
  and structs = program.A.structs in

  let context = L.global_context () in
  let the_module = L.create_module context "MicroC"

(* =============================== Types ==================================== *)
  and float_t = L.double_type context
  and i32_t  = L.i32_type  context
  and i8_t   = L.i8_type   context
  and i1_t   = L.i1_type   context
  and void_t = L.void_type context in

  let string_t = L.pointer_type i8_t
  and i32_ptr_t = L.pointer_type i32_t
  and i8_ptr_t = L.pointer_type i8_t
  and fmatrix_t = L.pointer_type i32_t in

  let ltype_of_primitive_type = function
      A.Int -> i32_t
    | A.Float -> float_t
    | A.String -> string_t
    | A.Bool -> i1_t
    | A.Void -> void_t
    | A.Fmatrix -> fmatrix_t
    | A.Imatrix -> fmatrix_t
  in

  let rec ltype_of_typ struct_decl_map = function
      A.PrimitiveType(primitive) -> ltype_of_primitive_type(primitive)
    | A.StructType(s) -> L.pointer_type (fst (StringMap.find s struct_decl_map))
    | A.ArrayType(typ) ->
        if (U.match_struct typ || U.match_array typ)
        then ltype_of_typ struct_decl_map typ  (* already a pointer, don't cast *)
        else L.pointer_type (ltype_of_typ struct_decl_map typ)
    | A.FptrType(fp) -> 
        let rt = ltype_of_typ struct_decl_map (List.hd fp)
        and args = Array.of_list
          (List.map (fun t -> ltype_of_typ struct_decl_map t) (List.tl fp)) in
          L.pointer_type (L.function_type rt args)
  in
(* ========================================================================== *)
  (* Collect struct declarations. Builds a map struct_name[string] -> (lltype, A.struct_decl)  *)
  let struct_decl_map =
    let add_struct m struct_decl =
      let name = struct_decl.A.name
      and members = Array.of_list
        (List.map (fun (t, _) -> ltype_of_typ m t) struct_decl.A.members) in
      let struct_type = L.named_struct_type context ("struct."^name) in
        L.struct_set_body struct_type members false;
      (* let struct_type = L.struct_type context members in (* TODO: use named or unnamed structs? *) *)
      StringMap.add name (struct_type, struct_decl) m in
    List.fold_left add_struct StringMap.empty structs in

  (* function used to initialize global and local variables *)
  let empty_string = L.define_global "__empty_string" (L.const_stringz context "") the_module in
  let init_var = function
      A.PrimitiveType(typ) -> (
        match typ with
          A.Float -> L.const_float float_t 0.0
        | A.Bool -> L.const_int i1_t 0
        | A.String -> L.const_bitcast empty_string string_t
        | A.Imatrix -> L.const_null fmatrix_t
        | A.Fmatrix -> L.const_null fmatrix_t
        (* TODO: jayz, what are the default types here? *)
        | _ -> L.const_int i32_t 0
      )
    | A.StructType(_) as typ -> L.const_null (ltype_of_typ struct_decl_map typ)
    | A.ArrayType(_) as typ -> L.const_null (ltype_of_typ struct_decl_map typ)
    | A.FptrType(_) as typ -> L.const_null (ltype_of_typ struct_decl_map typ)
  in

  (* Declare each global variable; remember its value in a map *)
  (* Map variable_name[string] --> (llvalue, A.typ) *)
  let global_vars =
    let global_var m (t, name) =
      let init = init_var t in
      let global_llvalue = L.define_global name init the_module in
      StringMap.add name (global_llvalue, t) m in
    List.fold_left global_var StringMap.empty globals in

(*======================= EXTERNAL FUNCTION DECLARATIONS =====================*)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in
(*============================================================================*)
  (* TODO: jz matliteral *)
  (* let init_fmat_literal_t = L.var_arg_function_type fmatrix_t [| L.pointer_type float_t; i32_t; i32_t; |] in
  let init_fmat_literal_func = L.declare_function "init_fmat_literal" init_fmat_literal_t the_module in *)

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
          (List.map(fun (t, _) -> ltype_of_typ struct_decl_map t) fdecl.A.formals) in
      let ftype = L.function_type (ltype_of_typ struct_decl_map fdecl.A.typ) formal_types in
      StringMap.add name (L.declare_function name ftype the_module, fdecl) m in
    List.fold_left extern_decl StringMap.empty extern_functions in

  let local_decls =
    let function_decl m fdecl =
      let name = fdecl.A.fname
      and formal_types = Array.of_list
        (List.map (fun (t,_) -> ltype_of_typ struct_decl_map t) fdecl.A.formals) in
      let ftype = L.function_type (ltype_of_typ struct_decl_map fdecl.A.typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty local_functions in

  let find_func fname = 
    if (StringMap.mem fname local_decls) then StringMap.find fname local_decls
    else StringMap.find fname extern_decls
  in

  (* call an externally defined function by name and arguments *)
  let build_external fname actuals the_builder =
    let (fdef, fdecl) = (try StringMap.find fname extern_decls with
      Not_found -> raise(Failure("Not defined: " ^ fname ))) in
    let result = (match fdecl.A.typ with
        A.PrimitiveType(t) when t = A.Void -> ""
      | _ -> fname ^ "_res")
    in
    L.build_call fdef actuals result the_builder in

  (* Fill in the body of the given function *)
  (* TODO: need to make all structs default on heap. If initialized locally, put on heap.
     If seen in function signature, treat as struct pointer
   *)
  let build_function_body fdecl =
    let (the_function, _) = try StringMap.find fdecl.A.fname local_decls with Not_found -> raise (Bug "2") in
    (* return an instruction builder positioned at end of formal store/loads *)
    let builder = L.builder_at_end context (L.entry_block the_function) in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      (* need to alloc and store params *)
      let add_formal m (t, n) p =
        L.set_value_name n p;
  	    let local = L.build_alloca (ltype_of_typ struct_decl_map t) n builder in
        ignore (L.build_store p local builder);  (* local is stack pointer *)
        StringMap.add n (local, t) m in
      (* only need to alloc local vars *)
      let add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ struct_decl_map t) n builder in
        ignore (L.build_store (init_var t) local_var builder);
        StringMap.add n (local_var, t) m
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
          (Array.to_list (L.params the_function)) in
    (* produces a map name[string] --> llvalue *)
    List.fold_left add_local formals fdecl.A.locals in

    (* Return the llvalue for a variable or formal argument *)
    let lookup_llval n = try fst (StringMap.find n local_vars)
                   with Not_found -> fst (StringMap.find n global_vars)
    in

    (* returns the A.typ for a var *)
    let lookup_typ n = try snd (StringMap.find n local_vars)
                   with Not_found -> snd (StringMap.find n global_vars)
    in

    (* TODO: fail test trying to access a member of an undeclared struct *)
    let get_struct_decl s_name =
      try
        let typ = lookup_typ s_name in
        match typ with
          A.StructType(s) -> snd (StringMap.find s struct_decl_map)
        | _ -> raise Not_found
      with Not_found -> raise (Failure (s_name ^ " not declared"))
    in

    (* ========================= Binary Operators ========================= *)

    let int_ops = function
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
    | _         -> raise Not_found
    in

    let bool_ops = function
      A.And     -> L.build_and
    | A.Or      -> L.build_or
    | _         -> raise Not_found
    in

    let matrix_matrix_ops = function
      A.Add     ->  "mm_add"
    | A.Sub     ->  "mm_sub"
    | A.Mult    ->  "mm_mult"
    | A.Div     ->  "mm_div"
    | A.Dot     ->  "dot"
    | _         -> raise Not_found
    in

    let scalar_matrix_ops = function
      A.Add     ->  "sm_add"
    | A.Sub     ->  "sm_sub"
    | A.Mult    ->  "sm_mult"
    | A.Div     ->  "sm_div"
    | A.Equal   ->  "sm_eq"
    | A.Neq     ->  "sm_neq"
    | A.Less    ->  "sm_lt"
    | A.Leq     ->  "sm_leq"
    | A.Greater ->  "sm_gt"
    | A.Geq     ->  "sm_geq"
    | _         -> raise Not_found
    in



    (* ==================================================================== *)

    (* ========================= Array Constructors ========================= *)
    (*
      Whenever an array is made, we malloc and additional 16 bytes of metadata,
      which contains size and length information. This allows us to implement
      len() in a static context, and opens several possibilities including
      array concatenation, dynamic array resizing, etc.
      The layout will be:
      +--------------+----------+--------+
      | size (bytes) | len[int] | elem1  | ...
      +--------------+----------+--------+
    *)

    let size_offset = L.const_int i32_t (-2)
    and len_offset = L.const_int i32_t (-1)
    and metadata_sz = L.const_int i32_t 8 in  (* 8 bytes overhead *)

    let put_meta body_ptr offset llval builder =
      let ptr = L.build_bitcast body_ptr i32_ptr_t "i32_ptr_t" builder in
      let meta_ptr = L.build_gep ptr [| offset |] "meta_ptr" builder in
      L.build_store llval meta_ptr builder
    in

    let get_meta body_ptr offset builder =
      let ptr = L.build_bitcast body_ptr i32_ptr_t "i32_ptr_t" builder in
      let meta_ptr = L.build_gep ptr [| offset |] "meta_ptr" builder in
      L.build_load meta_ptr "meta_data" builder
    in

    let meta_to_body meta_ptr builder =
      let ptr = L.build_bitcast meta_ptr i8_ptr_t "meta_ptr" builder in
      L.build_gep ptr [| (L.const_int i8_t (8)) |] "body_ptr" builder
    in

    (* let body_to_meta body_ptr builder =
      let ptr = L.build_bitcast body_ptr i8_ptr_t "body_ptr" builder in
      L.build_gep ptr [| (L.const_int i8_t (-8)) |] "meta_ptr" builder
    in *)

    let make_array element_t len builder =
      let element_sz = L.build_bitcast (L.size_of element_t) i32_t "b" builder in
      let body_sz = L.build_mul element_sz len "body_sz" builder in
      let malloc_sz = L.build_add body_sz metadata_sz "make_array_sz" builder in
      let meta_ptr = L.build_array_malloc i8_t malloc_sz "make_array" builder in
      let body_ptr = meta_to_body meta_ptr builder in
      ignore (put_meta body_ptr size_offset malloc_sz builder);
      ignore (put_meta body_ptr len_offset len builder);
      L.build_bitcast body_ptr (L.pointer_type element_t) "make_array_ptr" builder
    in

    (* ==================================================================== *)

    (* Construct code for an expression; return its value *)
    let rec expr builder = function
        A.IntLit i            -> L.const_int i32_t i
      | A.FloatLit f          -> L.const_float float_t f
      | A.StringLit s         -> L.build_global_stringptr (Scanf.unescaped s) "str" builder
      | A.BoolLit b           -> L.const_int i1_t (if b then 1 else 0)
      (* | A.MatLit a            -> let ravel a = Array.of_list (List.map (expr builder) a) in
                               let m = Array.concat (List.map ravel a)
                                 and r = List.length a and c = List.length (List.hd a) in
                               (L.build_call init_fmat_literal_func [|m; r; c;|] "m_lit" builder) *)
      | A.Noexpr              -> L.const_int i32_t 0
      | A.Id s                -> L.build_load (lookup_llval s) s builder
      | A.MakeArray(typ, e) ->
        let len = expr builder e
        and element_t =
          if U.match_struct typ || U.match_array typ
          then L.element_type (ltype_of_typ struct_decl_map typ)
          else ltype_of_typ struct_decl_map typ
        in
        make_array element_t len builder
      | A.MakeStruct(typ) ->
        let llname = "make_struct"
        and struct_t = L.element_type (ltype_of_typ struct_decl_map typ) in
        L.build_malloc struct_t llname builder
      | A.MakeFptr(fname) -> 
          let (fdef, _) = find_func fname in
            fdef
          (*raise (Failure (L.string_of_llvalue fdef))*)
          (*L.build_gep fdef [|(L.const_int i32_t 0)|] "build_fptr" builder*)
      | A.ArrayAccess (arr_name, idx_expr) ->
        let idx = expr builder idx_expr in
        let llname = arr_name ^ "[" ^ L.string_of_llvalue idx ^ "]" in
        let arr_ptr_load =
          if U.is_struct_access arr_name
            then  (* this is to handle foo.arr[1]  TODO: currently nonfunctional. parse ambiguity *)
              let (s_name, member) = U.parse_struct_access arr_name in
              expr builder (A.StructAccess(s_name, member))
            else (* this is to handle normal arr[1] *)
              let arr_ptr = lookup_llval arr_name in
              L.build_load arr_ptr arr_name builder in
        let arr_gep = L.build_in_bounds_gep arr_ptr_load [|idx|] llname builder in
        let arr_typ = U.get_array_type (lookup_typ arr_name) in
        (* If it's a pointer type, i.e. struct/array don't load *)
        if U.match_struct arr_typ || U.match_array arr_typ then arr_gep
        else L.build_load arr_gep (llname ^ "_load") builder
      | A.ArrayAssign (arr_name, idx_expr, val_expr) ->
        let idx = (expr builder idx_expr)
        and assign_val = (expr builder val_expr) in
        let llname = arr_name ^ "[" ^ L.string_of_llvalue idx ^ "]" in
        let arr_ptr = lookup_llval arr_name in
        let arr_ptr_load = L.build_load arr_ptr arr_name builder in
        let arr_gep = L.build_in_bounds_gep arr_ptr_load [|idx|] llname builder in
        ignore (L.build_store assign_val arr_gep builder); assign_val
      | A.StructAccess (s_name, member) ->
        let struct_ptr = lookup_llval s_name
        and llname = (s_name ^ "." ^ member)
        and struct_decl = get_struct_decl s_name in
        let struct_ptr_load = L.build_load struct_ptr ("struct."^s_name) builder in
        let struct_gep =
          L.build_struct_gep struct_ptr_load (U.get_struct_member_idx struct_decl member)
          llname builder in
        L.build_load struct_gep (llname ^ "_load") builder
      | A.StructAssign (s_name, member, e) ->
          let e' = expr builder e
          and struct_ptr = lookup_llval s_name
          and llname = (s_name ^ "." ^ member)
          and struct_decl = get_struct_decl s_name in
          let struct_ptr_load = L.build_load struct_ptr ("struct."^s_name) builder in
          let struct_gep =
            L.build_struct_gep struct_ptr_load (U.get_struct_member_idx struct_decl member)
            llname builder in
          ignore (L.build_store e' struct_gep builder); e'
     | A.Binop (e1, op, e2)  ->
         let e1' = expr builder e1 and e2' = expr builder e2 in
         let l_typ1 = L.type_of e1' and l_typ2 = L.type_of e2' in
         let l_typs = (l_typ1, l_typ2) in
         (
           if      l_typs = (fmatrix_t, fmatrix_t) then (build_external (matrix_matrix_ops op) [| e1'; e2'|] builder)
           else if l_typs = (float_t, float_t) then (float_ops op e1' e2' "tmp" builder)
           else if l_typs = (i32_t, i32_t) then (int_ops op e1' e2' "tmp" builder)
           else if l_typs = (i1_t, i1_t) then (bool_ops op e1' e2' "tmp" builder)
           else if l_typ1 = fmatrix_t && (l_typ2 = i32_t || l_typ2 = float_t) then
                            (build_external (scalar_matrix_ops op) [|e1'; e2'|] builder)
           else if l_typ2 = fmatrix_t && (l_typ1 = i32_t || l_typ1 = float_t) then
                            (build_external (scalar_matrix_ops op) [|e1'; e2'|] builder)
           else raise (Failure ((U.string_of_op op) ^ " not defined for " ^
                                  (L.string_of_lltype l_typ1) ^ " and " ^
                                  (L.string_of_lltype l_typ2) ^ " in " ^
                                  (U.string_of_expr e2)
                                )
                         )
         )
      | A.Unop(op, e) ->
         let e' = expr builder e in
         let l_typ = L.type_of e' in
         (match op with
            A.Neg          ->
             if      l_typ = float_t then L.build_fneg e' "tmp" builder
             else if l_typ = fmatrix_t then build_external "negate" [| e' |] builder
             else    L.build_neg e' "tmp" builder
         | A.Not          -> L.build_not e' "tmp" builder
         | A.Transpose    -> build_external "transpose" [| e' |] builder
         )
      | A.Assign (s, e) -> (* TODO: matrix reassign *)
            let e' = expr builder e in
            ignore (L.build_store e' (lookup_llval s) builder); e'
      | A.Call ("printf", act) ->
         let actuals = List.map (expr builder) act in
  	     L.build_call
            printf_func
            (Array.of_list actuals)
            "printf"
            builder
  (*============================= built in fns ===============================*)
      | A.Call("len", [e]) ->
        let arr_ptr = expr builder e in
        let is_null = L.build_is_null arr_ptr "null" builder in
        L.build_select
          is_null
          (L.const_int i32_t 0)
          (get_meta arr_ptr len_offset builder)
          "len"
          builder
  (*==========================================================================*)
      | A.Call (f_name, act) ->
        let actuals = Array.of_list (List.rev (List.map (expr builder) (List.rev act))) in
          try
            let fdef = lookup_llval f_name in
              L.build_call fdef actuals (f_name ^ "_result") builder
          with Not_found ->
            (* we double reverse here for historic reasons. should we undo?
              Need to specify the order we eval fn arguments in LRM
            *)
            let (fdef, fdecl) = find_func f_name in
              L.build_call fdef actuals (U.get_result_name f_name fdecl.A.typ) builder
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
	          A.PrimitiveType(t) when t = A.Void -> L.build_ret_void builder
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
        A.PrimitiveType(t) when t = A.Void -> L.build_ret_void
      | A.PrimitiveType(t) when t = A.Float -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ struct_decl_map t) 0)
    )
  in

  List.iter build_function_body local_functions;
  the_module
