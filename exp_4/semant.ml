open Common
open Common.AggResult
open Common.AggResult.Syntax
open Sem_ast

(* type semant_error = string *)

let tac_elem_type_of = function
  | IntType -> Tac.Int
  | FloatType -> Tac.Float
  | VoidType -> Tac.Void

(* Name table entry. *)
type name_entry =
  | VarEntry of {
      id : int;
      ty : sem_type;
      const_val : int option;
    }
  | FuncEntry of {
      id : int;
      args : sem_type list;
      ret : b_type;
    }

let scalar_type_of t = { elem_ty = t; dims = [] }
let int_type = scalar_type_of IntType
let float_type = scalar_type_of FloatType
let fallback_exp_attr = { ty = int_type; const_val = None }
let fallback_texp = TIntLit 0

type obj_kind =
  | Param
  | Local
  | Global
  | Temp

(*
  The translation context.
*)
type translation_context = {
  names : name_entry StringMap.t;
  obj_kinds : obj_kind IntMap.t;
  obj_tys : sem_type IntMap.t;
  global_inits : Tac.tac_init IntMap.t;
  next_obj_id : int;
  next_func_id : int;
  next_label_id : int;
  current_ir : Tac.tac_instr list;
  functions : Tac.tac_function list;
  loop_stack : (int * int) list; (* A list of (l_start, l_end). *)
  current_ret_ty : b_type;
  is_global_scope : bool;
}

let exit_global (ctx : translation_context) : translation_context =
  { ctx with is_global_scope = false }

let enter_global (ctx : translation_context) : translation_context =
  { ctx with is_global_scope = true }

let emit (instr : Tac.tac_instr) (ctx : translation_context) : translation_context =
  { ctx with current_ir = instr :: ctx.current_ir }

let enter_loop (range : int * int) (ctx : translation_context) : translation_context =
  { ctx with loop_stack = range :: ctx.loop_stack }

let exit_loop (ctx : translation_context) : translation_context =
  { ctx with loop_stack = List.tl ctx.loop_stack }

(*
  Helper function for merging a sub-block's translation context into its parent context.

  Preserved sub->parent:
    * obj_tys -- for keep uniquely-identified temporaries
    * next_*_id -- for allocation
    * current_ir -- for code generation
*)
let merge_sub_block_context (inner : translation_context) (outer : translation_context) :
    translation_context =
  if inner.current_ret_ty <> outer.current_ret_ty then
    internal_error "Sub-blocks should never modify return type in context"
  else if inner.functions <> outer.functions then
    internal_error "Sub-blocks should never add new functions to context"
  else
    {
      outer with
      obj_tys = IntMap.union (fun _ v_outer _ -> Some v_outer) outer.obj_tys inner.obj_tys;
      obj_kinds = IntMap.union (fun _ v_outer _ -> Some v_outer) outer.obj_kinds inner.obj_kinds;
      next_obj_id = inner.next_obj_id;
      next_func_id = inner.next_func_id;
      next_label_id = inner.next_label_id;
      current_ir = inner.current_ir;
    }

(*
  Helper function for merging a function body's translation context into its parent context.

  Allocate a new name_entry in the outer context, then add the assembled IR (from the body context)
  to the outer functions.
*)
let merge_func_context (id : int) (ast_node : Ast.func_def) (t_params : t_func_param list)
    (arg_tys : sem_type list) (ret_ty : b_type) (body : translation_context)
    (outer : translation_context) : translation_context =
  let tac_func =
    {
      Tac.func_id = id;
      Tac.func_name = ast_node.Ast.func_name;
      func_params = List.map (fun p -> p.t_param_id) t_params;
      func_body = List.rev body.current_ir;
      func_ret_type = tac_elem_type_of ret_ty;
      func_obj_types =
        IntMap.to_seq body.obj_tys
        |> Seq.filter_map (fun (id, v) ->
            match IntMap.find_opt id body.obj_kinds with
            | Some Global -> None
            | Some _ ->
                Some (id, { Tac.elem_ty = tac_elem_type_of v.elem_ty; is_array = v.dims <> [] })
            | None -> None)
        |> IntMap.of_seq;
    }
  in
  {
    outer with
    names =
      StringMap.add ast_node.Ast.func_name
        (FuncEntry { id; args = arg_tys; ret = ret_ty })
        outer.names;
    next_obj_id = body.next_obj_id;
    next_func_id = body.next_func_id;
    next_label_id = body.next_label_id;
    functions = tac_func :: outer.functions;
  }

let alloc_obj_id (ctx : translation_context) : int * translation_context =
  (ctx.next_obj_id, { ctx with next_obj_id = ctx.next_obj_id + 1 })

let alloc_func_id (ctx : translation_context) : int * translation_context =
  (ctx.next_func_id, { ctx with next_func_id = ctx.next_func_id + 1 })

let alloc_label_id (ctx : translation_context) : int * translation_context =
  (ctx.next_label_id, { ctx with next_label_id = ctx.next_label_id + 1 })

let alloc_named_obj (name : string) (ty : sem_type) (kind : obj_kind) (const_val : int option)
    (ctx : translation_context) : int * translation_context =
  let _ =
    match (ctx.is_global_scope, kind) with
    | _, Temp -> internal_error "Use alloc_temp_obj to allocate temps"
    | true, Global -> ()
    | true, _ -> internal_error "Cannot allocate non-globals in global context"
    | false, Global -> internal_error "Cannot allocate globals in non-global context"
    | false, _ -> ()
  in
  let id, ctx = alloc_obj_id ctx in
  ( id,
    {
      ctx with
      names = StringMap.add name (VarEntry { id; ty; const_val }) ctx.names;
      obj_kinds = IntMap.add id kind ctx.obj_kinds;
      obj_tys = IntMap.add id ty ctx.obj_tys;
    } )

let alloc_func (name : string) (args : sem_type list) (ret : b_type) (ctx : translation_context) :
    int * translation_context =
  let id, ctx = alloc_func_id ctx in
  ( id,
    {
      ctx with
      names = StringMap.add name (FuncEntry { id; args; ret }) ctx.names;
      current_ret_ty = ret;
    } )

let alloc_temp_obj (ty : sem_type) (ctx : translation_context) : int * translation_context =
  let _ =
    if ctx.is_global_scope then internal_error "Cannot allocate temps in global context" else ()
  in
  let id, ctx = alloc_obj_id ctx in
  ( id,
    {
      ctx with
      obj_kinds = IntMap.add id Temp ctx.obj_kinds;
      obj_tys = IntMap.add id ty ctx.obj_tys;
    } )

let insert_global_init (id : int) (init_val : Tac.tac_init) (ctx : translation_context) :
    translation_context =
  let _ =
    if not ctx.is_global_scope then internal_error "Cannot assign global init in non-global context"
    else
      match IntMap.find_opt id ctx.obj_kinds with
      | Some Global -> ()
      | Some _ -> internal_error "Cannot assign global init to non-global object"
      | None -> internal_error "Object id not found for global init assignment"
  in
  { ctx with global_inits = IntMap.add id init_val ctx.global_inits }

let empty_translation_context =
  {
    names = StringMap.empty;
    obj_kinds = IntMap.empty;
    obj_tys = IntMap.empty;
    global_inits = IntMap.empty;
    next_obj_id = 0;
    next_func_id = 0;
    next_label_id = 0;
    current_ir = [];
    functions = [];
    loop_stack = [];
    current_ret_ty = VoidType;
    is_global_scope = true;
  }

let can_coerce_type (dst_ty : b_type) (src_ty : b_type) : bool =
  match (dst_ty, src_ty) with
  | t1, t2 when t1 = t2 -> true
  | IntType, FloatType -> true
  | FloatType, IntType -> true
  | _ -> false

let coerce_type (opnd : Tac.operand) (dst_ty : b_type) (src_ty : b_type) (ctx : translation_context)
    : Tac.operand * translation_context =
  match (dst_ty, src_ty) with
  | t1, t2 when t1 = t2 -> (opnd, ctx)
  | IntType, FloatType ->
      let temp, ctx = alloc_temp_obj int_type ctx in
      let instr = Tac.Fti (temp, opnd) in
      (Tac.Object temp, emit instr ctx)
  | FloatType, IntType ->
      let temp, ctx = alloc_temp_obj float_type ctx in
      let instr = Tac.Itf (temp, opnd) in
      (Tac.Object temp, emit instr ctx)
  | _ -> internal_error "Cannot coerce between these types"

let common_type_of (ty1 : b_type) (ty2 : b_type) : b_type =
  match (ty1, ty2) with
  | ty1, ty2 when ty1 = ty2 -> ty1
  | _, VoidType | VoidType, _ -> VoidType
  | FloatType, IntType | IntType, FloatType -> FloatType
  | _ -> internal_error "Common type is not defined for these types"

let find_obj_type (id : int) (ctx : translation_context) : sem_type =
  match IntMap.find_opt id ctx.obj_tys with
  | Some ty -> ty
  | None -> internal_error (Printf.sprintf "Object %d not found in type table" id)

let eval_unary_op (v : int) = function
  | Ast.Pos -> v
  | Ast.Neg -> -v
  | Ast.Not -> Bool.to_int (v = 0)

let eval_binary_op (lv : int) (rv : int) = function
  | Ast.Add -> Some (lv + rv)
  | Ast.Sub -> Some (lv - rv)
  | Ast.Mul -> Some (lv * rv)
  | Ast.Div when rv <> 0 -> Some (lv / rv)
  | Ast.Mod when rv <> 0 -> Some (lv mod rv)
  | Ast.Lt -> Some (Bool.to_int (lv < rv))
  | Ast.Gt -> Some (Bool.to_int (lv > rv))
  | Ast.Leq -> Some (Bool.to_int (lv <= rv))
  | Ast.Geq -> Some (Bool.to_int (lv >= rv))
  | Ast.Eq -> Some (Bool.to_int (lv = rv))
  | Ast.Neq -> Some (Bool.to_int (lv <> rv))
  | Ast.And -> Some (Bool.to_int (lv <> 0 && rv <> 0))
  | Ast.Or -> Some (Bool.to_int (lv <> 0 || rv <> 0))
  | _ -> None

(*
  Calculates index for multi-dim array indexing.

  Returns:
    * Linear index operand
    * Components operand
    * New context
*)
let rec gen_opnd_index (indices : t_exp list) (dims : int list) (ctx : translation_context) :
    Tac.operand * Tac.operand list * translation_context =
  let idx_exp =
    Seq.zip (List.to_seq indices) (List.to_seq dims)
    |> Seq.fold_left
         (fun acc (i, n) ->
           match (acc, i) with
           | TIntLit c_acc, TIntLit c_i -> TIntLit ((c_acc * n) + c_i)
           | TIntLit 0, _ -> i
           | _ ->
               let mul = TBinary (Ast.Mul, acc, TIntLit n, int_type) in
               TBinary (Ast.Add, mul, i, int_type))
         (TIntLit 0)
  in
  let comps_rev, ctx =
    List.to_seq indices
    |> Seq.fold_left
         (fun (comps, ctx) i ->
           let opnd, _, ctx = gen_exp i ctx in
           (opnd :: comps, ctx))
         ([], ctx)
  in
  let opnd, _, ctx = gen_exp idx_exp ctx in
  (opnd, List.rev comps_rev, ctx)

and gen_exp (exp : t_exp) (ctx : translation_context) : Tac.operand * sem_type * translation_context
    =
  match exp with
  | TIntLit n -> (Tac.Const n, int_type, ctx)
  | TVar (id, _, [], _) ->
      let ty = find_obj_type id ctx in
      (Tac.Object id, ty, ctx)
  | TVar (id, _, indices, ty) ->
      let dims = (find_obj_type id ctx).dims in
      let opnd_index, components, ctx = gen_opnd_index indices dims ctx in
      let temp, ctx = alloc_temp_obj ty ctx in
      let rd = Tac.ArrRd (temp, id, opnd_index, components) in
      (Tac.Object temp, ty, ctx |> emit rd)
  | TUnary (op, e, res_ty) ->
      let opnd, sub_ty, ctx = gen_exp e ctx in
      let temp, ctx = alloc_temp_obj res_ty ctx in
      let instr =
        match op with
        | Ast.Not -> Tac.UnaryOp (temp, op, opnd)
        | _ ->
            if sub_ty.elem_ty = FloatType then Tac.FUnaryOp (temp, op, opnd)
            else Tac.UnaryOp (temp, op, opnd)
      in
      (Tac.Object temp, res_ty, ctx |> emit instr)
  | TBinary (op, e1, e2, res_ty) ->
      let opnd1, ty1, ctx = gen_exp e1 ctx in
      let opnd2, ty2, ctx = gen_exp e2 ctx in
      let target_ty = common_type_of ty1.elem_ty ty2.elem_ty in
      let opnd1, ctx = coerce_type opnd1 target_ty ty1.elem_ty ctx in
      let opnd2, ctx = coerce_type opnd2 target_ty ty2.elem_ty ctx in
      let temp, ctx = alloc_temp_obj res_ty ctx in
      let instr =
        if target_ty = FloatType then Tac.FBinOp (temp, op, opnd1, opnd2)
        else Tac.BinOp (temp, op, opnd1, opnd2)
      in
      (Tac.Object temp, res_ty, ctx |> emit instr)
  | TCall (func_id, name, params, ret_ty) ->
      let args =
        match StringMap.find_opt name ctx.names with
        | Some (FuncEntry f) -> f.args
        | _ -> internal_error "Function not found"
      in
      let param_opnds, ctx =
        List.fold_left2
          (fun (opnds, ctx) p arg_ty ->
            let opnd, p_ty, ctx = gen_exp p ctx in
            let opnd, ctx = coerce_type opnd arg_ty.elem_ty p_ty.elem_ty ctx in
            (opnd :: opnds, ctx))
          ([], ctx) params args
      in
      let ret_temp, ctx = alloc_temp_obj ret_ty ctx in
      let call = Tac.Call (ret_temp, func_id, List.rev param_opnds) in
      (Tac.Object ret_temp, ret_ty, ctx |> emit call)

let rec gen_stmt (s : t_stmt) (ctx : translation_context) : translation_context =
  match s with
  | TAssign (id, _, [], rhs) ->
      let rhs_opnd, rhs_ty, ctx = gen_exp rhs ctx in
      let lhs_ty = find_obj_type id ctx in
      let rhs_opnd, ctx = coerce_type rhs_opnd lhs_ty.elem_ty rhs_ty.elem_ty ctx in
      let instr = Tac.Move (id, rhs_opnd) in
      { ctx with current_ir = instr :: ctx.current_ir }
  | TAssign (id, _, indices, rhs) ->
      let lhs_ty = find_obj_type id ctx in
      let rhs_opnd, rhs_ty, ctx = gen_exp rhs ctx in
      let rhs_opnd, ctx = coerce_type rhs_opnd lhs_ty.elem_ty rhs_ty.elem_ty ctx in
      let opnd_index, components, ctx = gen_opnd_index indices lhs_ty.dims ctx in
      let wr = Tac.ArrWr (id, opnd_index, rhs_opnd, components) in
      ctx |> emit wr
  | TExprStmt (Some e) ->
      let _, _, ctx = gen_exp e ctx in
      ctx
  | TExprStmt None -> ctx
  | TBlock items ->
      let ctx_body =
        List.fold_left
          (fun ctx item ->
            match item with
            | TStmt s -> gen_stmt s ctx
            | _ -> ctx)
          ctx items
      in
      merge_sub_block_context ctx_body ctx
  | TIf (cond, then_s, else_s_opt) ->
      let cond_opnd, _, ctx = gen_exp cond ctx in
      let l_then, ctx = alloc_label_id ctx in
      let l_end, ctx = alloc_label_id ctx in
      let ctx = ctx |> emit (Tac.Jc (cond_opnd, l_then)) in
      map_or (fun else_s -> gen_stmt else_s ctx) ctx else_s_opt
      |> emit (Tac.Jump l_end) |> emit (Tac.Label l_then) |> gen_stmt then_s
      |> emit (Tac.Label l_end)
  | TWhile (cond, body) ->
      let l_start, ctx = alloc_label_id ctx in
      let l_body, ctx = alloc_label_id ctx in
      let l_end, ctx = alloc_label_id ctx in
      let ctx = ctx |> emit (Tac.Label l_start) |> enter_loop (l_start, l_end) in
      let cond_opnd, _, ctx = gen_exp cond ctx in
      ctx
      |> emit (Tac.Jc (cond_opnd, l_body))
      |> emit (Tac.Jump l_end) |> emit (Tac.Label l_body) |> gen_stmt body
      |> emit (Tac.Jump l_start) |> emit (Tac.Label l_end) |> exit_loop
  | TBreak -> (
      match ctx.loop_stack with
      | (_, l_end) :: _ -> ctx |> emit (Tac.Jump l_end)
      | [] -> internal_error "Break outside of loop")
  | TContinue -> (
      match ctx.loop_stack with
      | (l_start, _) :: _ -> ctx |> emit (Tac.Jump l_start)
      | [] -> internal_error "Continue outside of loop")
  | TReturn (Some e) ->
      let opnd, ty, ctx = gen_exp e ctx in
      let opnd, ctx = coerce_type opnd ctx.current_ret_ty ty.elem_ty ctx in
      ctx |> emit (Tac.Return (Some opnd))
  | TReturn None -> ctx |> emit (Tac.Return None)

let rec translate_exp (exp : Ast.exp) (ctx : translation_context) :
    (t_exp * exp_attr * translation_context, string) agg_result =
  let rec translate_indices idxs ctx =
    match idxs with
    | [] -> agg_ok ([], ctx)
    | i :: rest ->
        let* i_expr, i_attr, ctx = translate_exp i ctx in
        let* rest_results, ctx = translate_indices rest ctx in
        let result = (i_expr, i_attr) :: rest_results in

        if i_attr.ty.elem_ty <> IntType || i_attr.ty.dims <> [] then
          agg_error "Array index must be an integer scalar" (result, ctx)
        else agg_ok (result, ctx)
  and translate_args args pms ctx =
    match (args, pms) with
    | [], [] -> agg_ok ([], ctx)
    | a :: a_rest, p_ty :: p_rest ->
        let* a_expr, a_attr, ctx = translate_exp a ctx in
        let* rest_exprs, ctx = translate_args a_rest p_rest ctx in
        let current_exprs = a_expr :: rest_exprs in

        if a_attr.ty.dims <> p_ty.dims then
          agg_error "Argument dimension mismatch" (current_exprs, ctx)
        else if
          a_attr.ty.elem_ty <> p_ty.elem_ty
          && not (a_attr.ty.elem_ty = IntType && p_ty.elem_ty = FloatType)
        then agg_error "Argument type mismatch" (current_exprs, ctx)
        else agg_ok (current_exprs, ctx)
    | _ -> agg_error "Argument count mismatch" ([], ctx)
  in
  let translate_var_scalar name id ty const_val ctx =
    let t_node = TVar (id, name, [], ty) in
    agg_ok (t_node, { ty; const_val }, ctx)
  and translate_var_array name id ty indices ctx =
    let* indices_results, ctx = translate_indices indices ctx in
    let t_indices = List.map fst indices_results in
    let idx_count = List.length indices and dim_count = List.length ty.dims in

    if idx_count > dim_count then
      agg_error "Too many subscripts for array"
        (TVar (id, name, t_indices, ty), { ty; const_val = None }, ctx)
    else
      let dims' = List.drop idx_count ty.dims in
      let ty = { elem_ty = ty.elem_ty; dims = dims' } in
      let t_node = TVar (id, name, t_indices, ty) in
      agg_ok (t_node, { ty; const_val = None }, ctx)
  in
  let translate_var name indices ctx =
    match (StringMap.find_opt name ctx.names, indices) with
    | None, _ ->
        (* Add a fallback entry for undefined variable. *)
        let id, ctx = alloc_named_obj name int_type Local None ctx in
        agg_error
          (Printf.sprintf "Undefined variable `%s`" name)
          (TVar (id, name, [], int_type), { ty = int_type; const_val = None }, ctx)
    | Some (FuncEntry _), _ ->
        agg_error
          (Printf.sprintf "`%s` is a function, not a variable" name)
          (fallback_texp, fallback_exp_attr, ctx)
    | Some (VarEntry v), [] -> translate_var_scalar name v.id v.ty v.const_val ctx
    | Some (VarEntry v), indices -> translate_var_array name v.id v.ty indices ctx
  and translate_unary op sub ctx =
    let* sub_expr, sub_attr, ctx = translate_exp sub ctx in
    match op with
    | Ast.Pos | Ast.Neg ->
        let exp_val =
          match (op, sub_attr.const_val) with
          | op, Some v -> Some (eval_unary_op v op)
          | _ -> None
        in
        let result_attr = { ty = sub_attr.ty; const_val = exp_val } in
        let result_expr = TUnary (op, sub_expr, sub_attr.ty) in

        if sub_attr.ty.dims <> [] then
          agg_error "Unary operator applied to array" (result_expr, result_attr, ctx)
        else if sub_attr.ty.elem_ty = VoidType then
          agg_error "Unary operator applied to `void`" (result_expr, result_attr, ctx)
        else agg_ok (result_expr, result_attr, ctx)
    | Ast.Not ->
        let exp_val = Option.map (fun v -> if v = 0 then 1 else 0) sub_attr.const_val in
        let result_expr = TUnary (op, sub_expr, int_type)
        and result_attr = { ty = int_type; const_val = exp_val } in

        if sub_attr.ty.dims <> [] then
          agg_error "`!` applied to array" (result_expr, result_attr, ctx)
        else if sub_attr.ty.elem_ty = VoidType then
          agg_error "`!` applied to `void`" (result_expr, result_attr, ctx)
        else agg_ok (result_expr, result_attr, ctx)
  and translate_binary op lhs rhs ctx =
    let* l_expr, l_attr, ctx = translate_exp lhs ctx in
    let* r_expr, r_attr, ctx = translate_exp rhs ctx in
    let res_elem_ty =
      match op with
      | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Mod ->
          common_type_of l_attr.ty.elem_ty r_attr.ty.elem_ty
      | _ -> IntType
    and exp_val =
      match (op, l_attr.const_val, r_attr.const_val) with
      | op, Some lv, Some rv -> eval_binary_op lv rv op
      | _ -> None
    in
    let res_ty = scalar_type_of res_elem_ty in
    let result_expr = TBinary (op, l_expr, r_expr, res_ty)
    and result_attr = { ty = res_ty; const_val = exp_val } in

    if l_attr.ty.dims <> [] || r_attr.ty.dims <> [] then
      agg_error "Binary operator applied to array" (result_expr, result_attr, ctx)
    else if l_attr.ty.elem_ty = VoidType || r_attr.ty.elem_ty = VoidType then
      agg_error "Binary operator applied to void" (result_expr, result_attr, ctx)
    else agg_ok (result_expr, result_attr, ctx)
  and translate_call name params ctx =
    match StringMap.find_opt name ctx.names with
    | None ->
        agg_error
          (Printf.sprintf "Undefined function `%s`" name)
          (fallback_texp, fallback_exp_attr, ctx)
    | Some (VarEntry _) ->
        agg_error
          (Printf.sprintf "`%s` is a variable, not a function" name)
          (fallback_texp, fallback_exp_attr, ctx)
    | Some (FuncEntry f) ->
        let* param_exprs, ctx = translate_args params f.args ctx in
        let ret_ty = scalar_type_of f.ret in
        let result_expr = TCall (f.id, name, param_exprs, ret_ty)
        and result_attr = { ty = ret_ty; const_val = None } in
        agg_ok (result_expr, result_attr, ctx)
  in
  match exp with
  | Ast.IntLit v ->
      let t_node = TIntLit v in
      agg_ok (t_node, { ty = int_type; const_val = Some v }, ctx)
  | Ast.Var (name, indices) -> translate_var name indices ctx
  | Ast.Unary (op, sub) -> translate_unary op sub ctx
  | Ast.Binary (op, lhs, rhs) -> translate_binary op lhs rhs ctx
  | Ast.Call (name, params) -> translate_call name params ctx

let rec expand_array_init_zeros (dims : int list) : (int list * t_exp) list =
  match dims with
  | [] -> [ ([], TIntLit 0) ]
  | d :: rest ->
      let sub_zeros = expand_array_init_zeros rest in
      let rec aux i acc =
        if i < 0 then acc
        else
          let shifted = List.map (fun (idxs, e) -> (i :: idxs, e)) sub_zeros in
          aux (i - 1) (shifted @ acc)
      in
      aux (d - 1) []

let translate (comp_unit : Ast.comp_unit) (ctx : translation_context) :
    (t_comp_unit * Tac.tac_program, string list) result =
  let eval_const_dim e ctx =
    let* t_e, attr, ctx = translate_exp e ctx in

    match attr.const_val with
    | Some v -> agg_ok (v, t_e, ctx)
    | None -> agg_error "Array dimension must be a constant integer" (1, t_e, ctx)
  in
  let rec eval_dims dims acc_vals acc_exprs ctx =
    match dims with
    | [] -> agg_ok (List.rev acc_vals, List.rev acc_exprs, ctx)
    | h :: t ->
        let* v, t_e, ctx = eval_const_dim h ctx in
        eval_dims t (v :: acc_vals) (t_e :: acc_exprs) ctx
  in
  let rec translate_const_init init ctx =
    match init with
    | Ast.ConstExp e ->
        let* t_e, attr, ctx = translate_exp e ctx in

        if attr.const_val = None then
          agg_error "Constant initializer must be constant" (TConstExp t_e, Tac.InitInt 0, ctx)
        else agg_ok (TConstExp t_e, Tac.InitInt (Option.get attr.const_val), ctx)
    | Ast.ConstArray vals ->
        let rec visit_vals vs ctx =
          match vs with
          | [] -> agg_ok ([], [], ctx)
          | v :: rest ->
              let* t_v, init_v, ctx = translate_const_init v ctx in
              let* t_rest, init_rest, ctx = visit_vals rest ctx in
              agg_ok (t_v :: t_rest, init_v :: init_rest, ctx)
        in
        let* t_vals, init_vals, ctx = visit_vals vals ctx in
        agg_ok (TConstArray t_vals, Tac.InitList init_vals, ctx)
  in
  let rec translate_init init ctx =
    match init with
    | Ast.InitExp e ->
        let* t_e, attr, ctx = translate_exp e ctx in
        let init_val = Option.map (fun v -> Tac.InitInt v) attr.const_val in
        agg_ok (TInitExp t_e, init_val, ctx)
    | Ast.InitArray vals ->
        let rec visit_vals vs ctx =
          match vs with
          | [] -> agg_ok ([], Some [], ctx)
          | v :: rest ->
              let* t_v, init_v, ctx = translate_init v ctx in
              let* t_rest, init_rest, ctx = visit_vals rest ctx in
              let combined_init =
                match (init_v, init_rest) with
                | Some v, Some rest -> Some (v :: rest)
                | _ -> None
              in
              agg_ok (t_v :: t_rest, combined_init, ctx)
        in
        let* t_vals, init_vals, ctx = visit_vals vals ctx in
        let init_val = Option.map (fun v -> Tac.InitList v) init_vals in
        agg_ok (TInitArray t_vals, init_val, ctx)
  in
  let rec expand_array_init dims init ctx =
    match (dims, init) with
    | [], TInitExp e -> agg_ok ([ ([], e) ], ctx)
    | d :: rest_dims, TInitArray vals ->
        let rec aux i vals_ptr acc ctx =
          if i >= d then agg_ok (List.rev acc, ctx)
          else
            let* sub_inits, ctx =
              match vals_ptr with
              | [] -> agg_ok (expand_array_init_zeros rest_dims, ctx)
              | v :: _ -> expand_array_init rest_dims v ctx
            in
            let shifted = List.map (fun (idxs, e) -> (i :: idxs, e)) sub_inits in
            let next_vals = tl_or [] vals_ptr in
            aux (i + 1) next_vals (List.rev_append shifted acc) ctx
        in
        if List.length vals > d then agg_error "Too many initializers" ([], ctx)
        else aux 0 vals [] ctx
    | [], TInitArray _ -> agg_error "Initializer is an array but scalar expected" ([], ctx)
    | _ :: _, TInitExp _ -> agg_error "Initializer is a scalar but array expected" ([], ctx)
  and expand_const_array_init dims init ctx =
    match (dims, init) with
    | [], TConstExp e -> agg_ok ([ ([], e) ], ctx)
    | d :: rest_dims, TConstArray vals ->
        let rec aux i vals_ptr acc ctx =
          if i >= d then agg_ok (List.rev acc, ctx)
          else
            let* sub_inits, ctx =
              match vals_ptr with
              | [] -> agg_ok (expand_array_init_zeros rest_dims, ctx)
              | v :: _ -> expand_const_array_init rest_dims v ctx
            in
            let shifted = List.map (fun (idxs, e) -> (i :: idxs, e)) sub_inits in
            let next_vals = tl_or [] vals_ptr in
            aux (i + 1) next_vals (List.rev_append shifted acc) ctx
        in
        if List.length vals > d then agg_error "Too many initializers" ([], ctx)
        else aux 0 vals [] ctx
    | [], TConstArray _ -> agg_error "Initializer is an array but scalar expected" ([], ctx)
    | _ :: _, TConstExp _ -> agg_error "Initializer is a scalar but array expected" ([], ctx)
  in
  let rec add_const_defs defs ty acc_defs acc_stmts ctx =
    match defs with
    | [] -> agg_ok (List.rev acc_defs, List.rev acc_stmts, ctx)
    | d :: ds ->
        let* dims_vals, dims_exprs, ctx = eval_dims d.Ast.const_dims [] [] ctx in
        let* t_init, init_val, ctx = translate_const_init d.Ast.const_init ctx in
        let kind = if ctx.is_global_scope then Global else Local in
        let const_val =
          match (ty, dims_vals, init_val) with
          | IntType, [], Tac.InitInt v -> Some v
          | _ -> None
        in
        let id, ctx =
          alloc_named_obj d.Ast.const_name { elem_ty = ty; dims = dims_vals } kind const_val ctx
        in
        let ctx = if kind = Global then insert_global_init id init_val ctx else ctx in
        let* init_stmt, ctx =
          match (kind, t_init) with
          | Global, _ -> agg_ok (None, ctx)
          | _, TConstExp init_exp ->
              agg_ok (Some (TAssign (id, d.Ast.const_name, dims_exprs, init_exp)), ctx)
          | _, (TConstArray _ as t_init_arr) ->
              let* inits, ctx = expand_const_array_init dims_vals t_init_arr ctx in
              let stmts =
                List.map
                  (fun (indices, expr) ->
                    let t_indices = List.map (fun i -> TIntLit i) indices in
                    TStmt (TAssign (id, d.Ast.const_name, t_indices, expr)))
                  inits
              in
              agg_ok (Some (TBlock stmts), ctx)
        in
        let t_def =
          {
            t_const_id = id;
            t_const_name = d.Ast.const_name;
            t_const_dims = dims_exprs;
            t_const_init = t_init;
          }
        in
        let acc_stmts = map_or (fun s -> s :: acc_stmts) acc_stmts init_stmt in
        add_const_defs ds ty (t_def :: acc_defs) acc_stmts ctx
  and add_var_defs defs ty acc_defs acc_stmts ctx =
    match defs with
    | [] -> agg_ok (List.rev acc_defs, List.rev acc_stmts, ctx)
    | d :: ds ->
        let* dims_vals, dims_exprs, ctx = eval_dims d.Ast.var_dims [] [] ctx in
        let* t_init, init_val, ctx =
          match d.Ast.var_init with
          | Some init ->
              let* t_i, init_v, ctx = translate_init init ctx in
              agg_ok (Some t_i, init_v, ctx)
          | None -> agg_ok (None, None, ctx)
        in
        let kind = if ctx.is_global_scope then Global else Local in
        let id, ctx =
          alloc_named_obj d.Ast.var_name { elem_ty = ty; dims = dims_vals } kind None ctx
        in
        let* _ =
          if kind = Global && d.Ast.var_init <> None && init_val = None then
            agg_error "Global initializer must be constant" ()
          else agg_ok ()
        in
        let ctx =
          match (kind, init_val) with
          | Global, Some v -> { ctx with global_inits = IntMap.add id v ctx.global_inits }
          | _ -> ctx
        in
        let* init_stmt, ctx =
          match (kind, t_init) with
          | Global, _ -> agg_ok (None, ctx)
          | _, Some (TInitExp init_exp) ->
              agg_ok (Some (TAssign (id, d.Ast.var_name, dims_exprs, init_exp)), ctx)
          | _, Some (TInitArray _ as t_init_arr) ->
              let* inits, ctx = expand_array_init dims_vals t_init_arr ctx in
              let stmts =
                List.map
                  (fun (indices, expr) ->
                    let t_indices = List.map (fun i -> TIntLit i) indices in
                    TStmt (TAssign (id, d.Ast.var_name, t_indices, expr)))
                  inits
              in
              agg_ok (Some (TBlock stmts), ctx)
          | _, None -> agg_ok (None, ctx)
        in
        let t_def =
          {
            t_var_id = id;
            t_var_name = d.Ast.var_name;
            t_var_dims = dims_exprs;
            t_var_init = t_init;
          }
        in
        let acc_stmts = map_or (fun s -> s :: acc_stmts) acc_stmts init_stmt in
        add_var_defs ds ty (t_def :: acc_defs) acc_stmts ctx
  in

  let rec translate_stmt stmt ret_ty in_loop ctx =
    match stmt with
    | Ast.Assign (name, indices, rhs) ->
        let* lhs_expr, lhs_attr, ctx = translate_exp (Ast.Var (name, indices)) ctx in
        let* rhs_expr, rhs_attr, ctx = translate_exp rhs ctx in
        let id, t_indices =
          match lhs_expr with
          | TVar (id, _, idxs, _) -> (id, idxs)
          | TIntLit 0 -> (0, []) (* Failsafe types are TIntLit 0 *)
          | _ -> internal_error "Ast.Var translated to non-TVar"
        in
        let res_stmt = TAssign (id, name, t_indices, rhs_expr) in

        if Option.is_some lhs_attr.const_val then
          agg_error "Cannot assign to constant variable" (res_stmt, ctx)
        else if lhs_attr.ty.dims <> [] || rhs_attr.ty.dims <> [] then
          agg_error "Assignment operands must be scalars" (res_stmt, ctx)
        else if lhs_attr.ty.elem_ty = VoidType then
          agg_error "Cannot assign to `void`" (res_stmt, ctx)
        else if
          lhs_attr.ty.elem_ty <> rhs_attr.ty.elem_ty
          && not (lhs_attr.ty.elem_ty = FloatType && rhs_attr.ty.elem_ty = IntType)
        then agg_error "Type mismatch in assignment" (res_stmt, ctx)
        else agg_ok (res_stmt, ctx)
    | Ast.ExprStmt (Some e) ->
        let* t_e, _, ctx = translate_exp e ctx in
        let res_stmt = TExprStmt (Some t_e) in
        agg_ok (res_stmt, ctx)
    | Ast.ExprStmt None ->
        let res_stmt = TExprStmt None in
        agg_ok (res_stmt, ctx)
    | Ast.Block items ->
        let* t_items, sub_ctx = translate_block_items items ret_ty in_loop ctx in
        let res_stmt = TBlock t_items in
        let ctx = merge_sub_block_context sub_ctx ctx in
        agg_ok (res_stmt, ctx)
    | Ast.If (cond, then_s, else_s) ->
        let* cond_expr, cond_attr, ctx = translate_exp cond ctx in
        let* then_stmt, ctx = translate_stmt then_s ret_ty in_loop ctx in
        let* else_stmt, ctx =
          match else_s with
          | Some s ->
              let* s_stmt, ctx = translate_stmt s ret_ty in_loop ctx in
              agg_ok (Some s_stmt, ctx)
          | None -> agg_ok (None, ctx)
        in

        let res_stmt = TIf (cond_expr, then_stmt, else_stmt) in

        if cond_attr.ty.elem_ty = VoidType || cond_attr.ty.dims <> [] then
          agg_error "Condition must be non-`void` scalar" (res_stmt, ctx)
        else agg_ok (res_stmt, ctx)
    | Ast.While (cond, body) ->
        let* cond_expr, cond_attr, ctx = translate_exp cond ctx in
        let* body_stmt, ctx = translate_stmt body ret_ty true ctx in

        let res_stmt = TWhile (cond_expr, body_stmt) in

        if cond_attr.ty.elem_ty = VoidType || cond_attr.ty.dims <> [] then
          agg_error "Condition must be non-`void` scalar" (res_stmt, ctx)
        else agg_ok (res_stmt, ctx)
    | Ast.Break ->
        let res_stmt = TBreak in
        if in_loop then agg_ok (res_stmt, ctx)
        else agg_error "`break` statement not within a loop" (res_stmt, ctx)
    | Ast.Continue ->
        let res_stmt = TContinue in
        if in_loop then agg_ok (res_stmt, ctx)
        else agg_error "`continue` statement not within a loop" (res_stmt, ctx)
    | Ast.Return e_opt -> (
        match (e_opt, ret_ty) with
        | None, VoidType ->
            let res_stmt = TReturn None in
            agg_ok (res_stmt, ctx)
        | Some _, VoidType ->
            let res_stmt = TReturn None in
            agg_error "`void` function cannot return values" (res_stmt, ctx)
        | None, _ ->
            let res_stmt = TReturn None in
            agg_error "Non-`void` function must return a value" (res_stmt, ctx)
        | Some e, t ->
            let* t_e, attr, ctx = translate_exp e ctx in
            let res_stmt = TReturn (Some t_e) in

            if attr.ty.dims <> [] then agg_error "Cannot return an array" (res_stmt, ctx)
            else if can_coerce_type t attr.ty.elem_ty then agg_ok (res_stmt, ctx)
            else agg_error "Return type mismatch" (res_stmt, ctx))
  and translate_block_items items ret_ty in_loop ctx =
    match items with
    | [] -> agg_ok ([], ctx)
    | item :: rest ->
        let* t_items, ctx =
          match item with
          | Ast.Decl (Ast.VarDecl (t, defs)) ->
              let* t_defs, init_stmts, ctx = add_var_defs defs (b_type_from_ast t) [] [] ctx in
              let decl_item = TDecl (TVarDecl (b_type_from_ast t, t_defs)) in
              let stmt_items = List.map (fun s -> TStmt s) init_stmts in
              agg_ok (decl_item :: stmt_items, ctx)
          | Ast.Decl (Ast.ConstDecl (t, defs)) ->
              let* t_defs, init_stmts, ctx = add_const_defs defs (b_type_from_ast t) [] [] ctx in
              let decl_item = TDecl (TConstDecl (b_type_from_ast t, t_defs)) in
              let stmt_items = List.map (fun s -> TStmt s) init_stmts in
              agg_ok (decl_item :: stmt_items, ctx)
          | Ast.Stmt s ->
              let* t_s, ctx = translate_stmt s ret_ty in_loop ctx in
              agg_ok ([ TStmt t_s ], ctx)
        in
        let* rest_items, final_ctx = translate_block_items rest ret_ty in_loop ctx in
        agg_ok (t_items @ rest_items, final_ctx)
  in
  let rec translate_comp_unit_item_list items ctx =
    match items with
    | [] -> agg_ok ([], ctx)
    | item :: rest -> (
        match item with
        | Ast.DeclItem (Ast.VarDecl (t, defs)) ->
            let* t_defs, _, ctx = add_var_defs defs (b_type_from_ast t) [] [] ctx in
            let t_item = TDeclItem (TVarDecl (b_type_from_ast t, t_defs)) in
            let* rest_items, ctx = translate_comp_unit_item_list rest ctx in
            agg_ok (t_item :: rest_items, ctx)
        | Ast.DeclItem (Ast.ConstDecl (t, defs)) ->
            let* t_defs, _, ctx = add_const_defs defs (b_type_from_ast t) [] [] ctx in
            let t_item = TDeclItem (TConstDecl (b_type_from_ast t, t_defs)) in
            let* rest_items, ctx = translate_comp_unit_item_list rest ctx in
            agg_ok (t_item :: rest_items, ctx)
        | Ast.FuncDefItem f ->
            let rec process_params params acc_arg_tys acc_t_params ctx =
              match params with
              | [] -> agg_ok (List.rev acc_arg_tys, List.rev acc_t_params, ctx)
              | p :: p_rest ->
                  let ty = b_type_from_ast p.Ast.param_type in
                  let* dims_vals, dims_exprs, ctx =
                    match p.Ast.param_dims with
                    | None -> agg_ok ([], None, ctx)
                    | Some ds ->
                        let* d_vals, d_exprs, ctx = eval_dims ds [] [] ctx in
                        agg_ok (0 :: d_vals, Some d_exprs, ctx)
                  in
                  let arg_ty = { elem_ty = ty; dims = dims_vals } in
                  let id, ctx = alloc_named_obj p.param_name arg_ty Param None ctx in
                  let t_param =
                    {
                      t_param_id = id;
                      t_param_type = ty;
                      t_param_name = p.param_name;
                      t_param_dims = dims_exprs;
                    }
                  in
                  process_params p_rest (arg_ty :: acc_arg_tys) (t_param :: acc_t_params) ctx
            in
            let ret_ty = b_type_from_ast_func f.Ast.func_ret_type in
            let ctx_body = exit_global ctx in
            let* arg_tys, t_params, ctx_body = process_params f.Ast.func_params [] [] ctx_body in
            let func_id, ctx_body = alloc_func f.Ast.func_name arg_tys ret_ty ctx_body in
            let* t_body, ctx_body = translate_block_items f.Ast.func_body ret_ty false ctx_body in
            let ctx_body = gen_stmt (TBlock t_body) ctx_body in
            let ctx = merge_func_context func_id f t_params arg_tys ret_ty ctx_body ctx in
            let* rest_items, ctx = translate_comp_unit_item_list rest ctx in
            let t_func =
              {
                t_func_id = func_id;
                t_func_ret_type = f.Ast.func_ret_type |> Option.map b_type_from_ast;
                t_func_name = f.Ast.func_name;
                t_func_params = t_params;
                t_func_body = t_body;
              }
            in
            agg_ok (TFuncDefItem t_func :: rest_items, ctx))
  in
  let result =
    let ctx = enter_global ctx in
    let* comp_unit, ctx = translate_comp_unit_item_list comp_unit ctx in
    let globals =
      IntMap.bindings ctx.obj_kinds
      |> List.filter_map (fun (id, kind) -> if kind = Global then Some id else None)
    in
    let objects =
      IntMap.bindings ctx.obj_tys
      |> List.map (fun (id, ty) ->
          let elem_ty = tac_elem_type_of ty.elem_ty in
          (id, { Tac.elem_ty; is_array = ty.dims <> [] }))
      |> List.to_seq |> IntMap.of_seq
    in
    let program =
      { Tac.globals; global_init = ctx.global_inits; functions = List.rev ctx.functions; objects }
    in
    agg_ok (comp_unit, program)
  in
  agg_to_result result
