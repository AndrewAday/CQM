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
        let (arg_typs, ret_typ) = U.parse_fptr_type fp in
        let rt = ltype_of_typ struct_decl_map ret_typ
        and args = Array.of_list
          (List.map (fun t -> ltype_of_typ struct_decl_map t) arg_typs) in
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

  let struct_lltype_list =
    let bindings = StringMap.bindings struct_decl_map in
    List.map (fun (_, (lltype, _)) -> L.pointer_type lltype) bindings
  in

  (* determines if the lltype is a ptr to struct type *)
  let is_ptr_to_struct llval =
    let lltype = L.type_of llval in
    U.contains lltype struct_lltype_list
  in

  let get_struct_pointer_lltype llval =
    let lltype = L.type_of llval in
    U.try_get lltype struct_lltype_list
  in

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

  let time_t = L.var_arg_function_type i32_t [| L.pointer_type i32_t |] in
  let time_func = L.declare_function "time" time_t the_module in

  (* let memset_t = L.function_type void_t [| i8_ptr_t; i32_t; i32_t|] in
  let memset = L.declare_function "memset" memset_t the_module in *)

  let memcpy_t = L.function_type i32_t [| i8_ptr_t; i8_ptr_t; i32_t|] in
  let memcpy = L.declare_function "memcpy" memcpy_t the_module in
(*============================================================================*)
  (* TODO: jz matliteral *)
  let init_fmat_literal_t = L.var_arg_function_type fmatrix_t [| L.pointer_type float_t; i32_t; i32_t; |] in
  let init_fmat_literal_func = L.declare_function "init_fmat_literal" init_fmat_literal_t the_module in

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
      +--------------+--------------+----------+--------+
      | element size | size (bytes) | len[int] | elem1  | ...
      +--------------+--------------+----------+--------+
    *)

    let elem_size_offset = L.const_int i32_t (-3)
    and size_offset = L.const_int i32_t (-2)
    and len_offset = L.const_int i32_t (-1)
    and metadata_sz = L.const_int i32_t 12 in  (* 12 bytes overhead *)

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
      L.build_gep ptr [| (L.const_int i8_t (12)) |] "body_ptr" builder
    in

    let body_to_meta body_ptr builder =
      let ptr = L.build_bitcast body_ptr i8_ptr_t "body_ptr" builder in
      L.build_gep ptr [| (L.const_int i8_t (-12)) |] "meta_ptr" builder
    in

    let make_array element_t len builder =
      let element_sz = L.build_bitcast (L.size_of element_t) i32_t "b" builder in
      let body_sz = L.build_mul element_sz len "body_sz" builder in
      let malloc_sz = L.build_add body_sz metadata_sz "make_array_sz" builder in
      let meta_ptr = L.build_array_malloc i8_t malloc_sz "make_array" builder in
      let body_ptr = meta_to_body meta_ptr builder in
      ignore (put_meta body_ptr elem_size_offset element_sz builder);
      ignore (put_meta body_ptr size_offset malloc_sz builder);
      ignore (put_meta body_ptr len_offset len builder);
      L.build_bitcast body_ptr (L.pointer_type element_t) "make_array_ptr" builder
    in

    (* TODO: free old array before append, and handle edge case where we are
    appending a pointer to struct within the existing array.
    *)
    let arr_copy_and_free arr_gep elem_ptr elem_sz restore_ptr builder =
      let casted_elem_ptr = L.build_bitcast elem_ptr i8_ptr_t "struct_to_char_ptr" builder
      and casted_arr_ptr = L.build_bitcast arr_gep i8_ptr_t "arr_to_char_ptr" builder
      and casted_elem_sz = L.build_bitcast elem_sz i32_t "i64_to_i32" builder in
      ignore(L.build_call memcpy [|casted_arr_ptr; casted_elem_ptr; casted_elem_sz|] "" builder);
      ignore(L.build_free casted_elem_ptr builder); (* free original object *)
      match restore_ptr with
        Some ptr -> L.build_store arr_gep ptr builder
      | None -> elem_ptr
    in

    (* allocates an additional elem_sz bytes to array and memcpys
      Note: appending a struct will
      1. memcpy original struct to array
      2. free original struct
      3. have struct ptr now point to the element in the array.

      Note: it is illegal to have a pointer to a struct within an arr, and then
      append that internal struct to the end of the array. This breaks because
      it causes a double free: once to free the original array, and another
      to free the strict. It is VERY difficult to track statically whether or
      not a pointer is pointing within array bounds, so we will just not support
      this.

      @param restore_ptr: the stack address of the pointer to an element we are
      appending. Optional. Only populated if we are appending a struct. This is
      so we may reset the struct ptr to the newly malloced array element.
    *)
    let array_append arr_ptr elem_val restore_ptr builder =
      let orig_sz = get_meta arr_ptr size_offset builder
      and elem_sz = get_meta arr_ptr elem_size_offset builder
      and orig_len = get_meta arr_ptr len_offset builder
      and src_meta_ptr = body_to_meta arr_ptr builder in
      let new_len = L.build_add orig_len (L.const_int i32_t 1) "post_append_len" builder in
      let new_sz = L.build_add orig_sz elem_sz "post_append_sz" builder in
      let dst_meta_ptr = L.build_array_malloc i8_t new_sz "append_array" builder in
      let dst_body_ptr = meta_to_body dst_meta_ptr builder in
      (* memcpy and update metadata, then free old array *)
      ignore(L.build_call memcpy [|dst_meta_ptr; src_meta_ptr; orig_sz; |] "" builder);
      ignore(put_meta dst_body_ptr size_offset new_sz builder);
      ignore(put_meta dst_body_ptr len_offset new_len builder);
      ignore(L.build_free src_meta_ptr builder);
      (* now we need to copy the elem_val into the buffer *)
      let ret_ptr = L.build_bitcast dst_body_ptr (L.type_of arr_ptr) "append_array_ptr" builder in
      let arr_gep = L.build_in_bounds_gep ret_ptr [|orig_len|] "append_idx" builder in
      ignore (
      if is_ptr_to_struct elem_val
      then (* cannot just store; we need to do a memcpy *)
        arr_copy_and_free arr_gep elem_val elem_sz restore_ptr builder
      else
        L.build_store elem_val arr_gep builder
      );
      ret_ptr
    in

    (*
    During concat(a, b) we will malloc a new array of len(a) + len(b) and
    memcpy the contents of a and b.
    We then free the original array a, and leave b untouched.
    usage: a = concat(a, b)
    *)
    let array_concat left_arr_ptr right_arr_ptr builder =
      let left_meta_ptr = body_to_meta left_arr_ptr builder
      and right_casted_ptr = L.build_bitcast right_arr_ptr i8_ptr_t "" builder
      and left_arr_sz = get_meta left_arr_ptr size_offset builder
      and right_arr_sz =
        L.build_sub
          (get_meta right_arr_ptr size_offset builder)
          metadata_sz "minus_meta_sz" builder
      and left_arr_len = get_meta left_arr_ptr len_offset builder
      and right_arr_len = get_meta right_arr_ptr len_offset builder in
      let new_sz = L.build_add left_arr_sz right_arr_sz "concat_sz" builder in
      let new_len = L.build_add left_arr_len right_arr_len "concat_len" builder in
      let dst_meta_ptr = L.build_array_malloc i8_t new_sz "concat_array" builder in
      let dst_body_ptr = meta_to_body dst_meta_ptr builder in
      let ret_ptr = L.build_bitcast dst_body_ptr (L.type_of left_arr_ptr) "concat_ret_ptr" builder in
      let dst_concat_ptr =
        L.build_bitcast
          ( L.build_in_bounds_gep ret_ptr [|left_arr_len|] "" builder )
          i8_ptr_t
          "concat_pos_ptr"
          builder
      in
      ignore(L.build_call memcpy [|dst_meta_ptr; left_meta_ptr; left_arr_sz|] "" builder);
      ignore(L.build_call memcpy [|dst_concat_ptr; right_casted_ptr; right_arr_sz|] "" builder);
      ignore(put_meta dst_body_ptr size_offset new_sz builder);
      ignore(put_meta dst_body_ptr len_offset new_len builder);
      ignore(L.build_free left_meta_ptr builder);
      ret_ptr
    in


    (* ==================================================================== *)

    (* Construct code for an expression; return its value *)
    let rec expr builder = function
        A.IntLit i            -> L.const_int i32_t i
      | A.FloatLit f          -> L.const_float float_t f
      | A.StringLit s         -> L.build_global_stringptr (Scanf.unescaped s) "str" builder
      | A.BoolLit b           -> L.const_int i1_t (if b then 1 else 0)
      | A.Noexpr              -> L.const_int i32_t 0
      | A.Null                -> L.const_pointer_null void_t
      | A.Id s                ->
        let ret =
        try
          L.build_load (lookup_llval s) s builder
        with Not_found -> (* then it's probably a function pointer *)
          fst (find_func s)
        in ret
      | A.Pipe(e1, e2) ->
        begin
          match e2 with
            A.Call(fname, actuals) -> expr builder (A.Call(fname, e1 :: actuals))
          | _ -> raise (Failure "illegal pipe")  (* this should never execute *)
        end
      | A.Dispatch(s_name, mthd_name, el) ->
        let struct_decl = get_struct_decl s_name in
        let real_method = U.methodify mthd_name struct_decl.A.name in
        expr builder (A.Call(real_method, (A.Id(s_name)) :: el))
      | A.MatIndex(mat, e2, e3) ->
        let fm = expr builder (A.Id(mat))
        and i = expr builder e2
        and j = expr builder e3 in
        build_external "mat_index" [|fm; i; j|] builder
      | A.MatIndexAssign(mat, e2, e3, e4) ->
        let fm = expr builder (A.Id(mat))
        and i = expr builder e2
        and j = expr builder e3
        and f = expr builder e4 in
        build_external "mat_index_assign" [|fm; i; j; f|] builder
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
        (
        match get_struct_pointer_lltype assign_val with
          (* Note:
            assigning a struct will memcpy the contents of the struct over,
            free the former struct, and have the struct ptr now point to the
            array element.
            We do this to prevent assignment "implicitly" duplicating memory
          *)
          Some struct_ptr ->
            let elem_type = L.element_type struct_ptr in
            let elem_sz = L.size_of elem_type in
            let restore_ptr =
            (
              match (U.try_get_id_str val_expr) with
                Some s -> Some (lookup_llval s)
                (*TODO: match case for struct literals *)
              | None -> None
            ) in
            arr_copy_and_free arr_gep assign_val elem_sz restore_ptr builder
        | None -> L.build_store assign_val arr_gep builder
        )
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
     | A.ArrayLit (arr_type, expr_list) ->
        let arr_ptr = expr builder (A.MakeArray(U.get_array_type arr_type,
                                  A.IntLit(List.length expr_list))) in
        List.iteri (fun idx e ->  (* TODO: need to make this work with struct literals *)
          let arr_gep = L.build_in_bounds_gep arr_ptr [| L.const_int i32_t (idx) |] "array_lit" builder in
          let assign_val = expr builder e in
          ignore (L.build_store assign_val arr_gep builder)
        ) expr_list;
        arr_ptr
    | A.MatLit m ->
        let r = expr builder (A.IntLit(List.length m))
        and c = expr builder (A.IntLit(List.length (List.hd m)))
        and a = expr builder (A.ArrayLit(A.ArrayType(A.PrimitiveType(A.Float)), List.concat m)) in
        (L.build_call init_fmat_literal_func [|a; r; c;|] "m_lit" builder)
    | A.StructArrayAccess(s_name, member, e) ->
           let struct_member = expr builder (A.StructAccess(s_name, member))
           and idx = expr builder e
           and llname = s_name ^ "." ^ member ^ "[]"
           and struct_decl = get_struct_decl s_name in
           let arr_gep = L.build_in_bounds_gep struct_member [|idx|] llname builder in
           let arr_typ = U.get_struct_member_type struct_decl member "not found" in
           let arr_inner_typ = U.get_array_type arr_typ in
           (* If it's a pointer type, i.e. struct/array don't load *)
           if U.match_struct arr_inner_typ || U.match_array arr_inner_typ then arr_gep
           else L.build_load arr_gep (llname ^ "_load") builder
         | A.StructArrayAssign(s_name, member, e1, e2) ->
           let struct_member = expr builder (A.StructAccess(s_name, member))
           and idx = expr builder e1
           and assign_val = expr builder e2
           and llname = s_name ^ "." ^ member ^ "[]"
           (* and struct_decl = get_struct_decl s_name  *)
           in
           let arr_gep = L.build_in_bounds_gep struct_member [|idx|] llname builder in
           (
             match get_struct_pointer_lltype assign_val with
               Some struct_ptr ->
                let elem_type = L.element_type struct_ptr in
                let elem_sz = L.size_of elem_type in
                let restore_ptr =
                (
                  match (U.try_get_id_str e2) with
                    Some s -> Some (lookup_llval s)
                    (*TODO: match case for struct literals *)
                  | None -> None
                ) in
                arr_copy_and_free arr_gep assign_val elem_sz restore_ptr builder
             | None -> L.build_store assign_val arr_gep builder
          )

     | A.Binop (e1, op, e2)  ->
         let e1' = expr builder e1 and e2' = expr builder e2 in
         let l_typ1 = L.type_of e1' and l_typ2 = L.type_of e2' in
         let l_typs = (l_typ1, l_typ2) in
         (
           if      l_typs = (fmatrix_t, fmatrix_t) then (build_external (matrix_matrix_ops op) [| e1'; e2'|] builder)
           else if l_typs = (float_t, float_t) then (float_ops op e1' e2' "tmp" builder)
           else if l_typs = (i32_t, i32_t) && op = A.Mod then (build_external "modulo" [|e1'; e2'|] builder)
           else if l_typs = (i32_t, i32_t) then (int_ops op e1' e2' "tmp" builder)
           else if l_typs = (i1_t, i1_t) then (bool_ops op e1' e2' "tmp" builder)
           else if l_typ1 = fmatrix_t && (l_typ2 = i32_t || l_typ2 = float_t) then
                            let rev = expr builder (A.IntLit(1)) in
                            (build_external (scalar_matrix_ops op) [|e1'; e2'; rev|] builder)
           else if l_typ2 = fmatrix_t && (l_typ1 = i32_t || l_typ1 = float_t) then
                            let rev = expr builder (A.IntLit(0)) in
                            (build_external (scalar_matrix_ops op) [|e2'; e1'; rev|] builder)
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
  (*============================= built in fns ===============================*)
      | A.Call ("printf", act) ->
         let actuals = List.map (expr builder) act in
  	     L.build_call
            printf_func
            (Array.of_list actuals)
            "printf"
            builder
      | A.Call("time", []) ->
          L.build_call
             time_func
             [| L.const_pointer_null (L.pointer_type i32_t) |]
             "time"
             builder
      | A.Call("float_of_int", [e]) ->
          L.build_sitofp (expr builder e) float_t "float_of_int" builder
      | A.Call("int_of_float", [e]) ->
          L.build_fptosi (expr builder e) i32_t "int_of_float" builder
      | A.Call("len", [e]) ->
        let arr_ptr = expr builder e in
        let is_null = L.build_is_null arr_ptr "null" builder in
        L.build_select
          is_null
          (L.const_int i32_t 0)
          (get_meta arr_ptr len_offset builder)
          "len"
          builder
      | A.Call("free", [e]) ->
        let ptr = expr builder e in
        let lltype = L.type_of ptr in
        if lltype = fmatrix_t
        then build_external  "del_mat" [|ptr|] builder
        else L.build_free ptr builder
      | A.Call("free_arr", [e]) ->
      (* we have to make a separate free for arrays to know to move ptr back
      8 bytes so we can free metadata *)
        let body_ptr = expr builder e in
        let meta_ptr = body_to_meta body_ptr builder in
        L.build_free meta_ptr builder
      | A.Call("append", [e1; e2]) ->
        let arr_ptr = expr builder e1
        and elem = expr builder e2 in
        let restore_ptr = (
          match U.try_get_id_str e2 with
            Some s -> Some (lookup_llval s)
          | None -> None
        ) in
        array_append arr_ptr elem restore_ptr builder
      | A.Call("concat", [e1; e2]) ->
        let left_arr_ptr = expr builder e1
        and right_arr_ptr = expr builder e2 in
        array_concat left_arr_ptr right_arr_ptr builder
  (*==========================================================================*)
      | A.Call (f_name, act) ->
        let actuals = Array.of_list (List.rev (List.map (expr builder) (List.rev act))) in
          try
            let fdef = lookup_llval f_name in  (* first search for function pointer *)
              let fptr_load = L.build_load fdef "load_fptr" builder in
                let result_name =
                  if (L.return_type(L.element_type (L.element_type(L.type_of fdef)))) = void_t
                  then ""
                  else f_name ^ "_result"
                in
                L.build_call fptr_load actuals result_name builder
          with Not_found ->
            (* we double reverse here for historic reasons. should we undo?
              Need to specify the order we eval fn arguments in LRM
            *)
            let (fdef, fdecl) = find_func f_name in (* searching for normal function call *)
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
