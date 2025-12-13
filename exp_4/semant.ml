open Common
open Common.AggResult
open Common.AggResult.Syntax
open Sem_ast

(* type semant_error = string *)

let tac_elem_type_of = function
  | IntType -> Tac.Int
  | FloatType -> Tac.Float
  | VoidType -> Tac.Void

(* Symbol table entry. *)
type name_entry =
  | VarEntry of {
      id : int;
      ty : sem_type;
    }
  | FunEntry of {
      id : int;
      args : sem_type list;
      ret : b_type;
    }

let scalar_type_of t = { elem_ty = t; dims = [] }
let int_type = scalar_type_of IntType
let float_type = scalar_type_of FloatType
let fallback_exp_attr = { ty = int_type; const_val = None }
let fallback_texp = TIntLit 0

type symbol_kind =
  | Param
  | Named
  | Temp

(*
  The translation context.

  Some quirks:
    * Functions and objects are both "sym", so `next_name_id` track them all.
    * `sym_kinds` contains functions, but `sym_tys` don't.
    * `names` contains named variables and functions.
  TODO:
    * Separate functions from objects.
*)
type translation_context = {
  names : name_entry StringMap.t;
  sym_kinds : symbol_kind IntMap.t;
  sym_tys : sem_type IntMap.t;
  next_name_id : int;
  next_label_id : int;
  current_ir : Tac.tac_instr list;
  functions : Tac.tac_function list;
  loop_stack : (int * int) list; (* A list of (l_start, l_end). *)
  current_ret_ty : b_type;
}

let emit (instr : Tac.tac_instr) (ctx : translation_context) : translation_context =
  { ctx with current_ir = instr :: ctx.current_ir }

let enter_loop (range : int * int) (ctx : translation_context) : translation_context =
  { ctx with loop_stack = range :: ctx.loop_stack }

let exit_loop (ctx : translation_context) : translation_context =
  { ctx with loop_stack = List.tl ctx.loop_stack }

(*
  Helper function for merging a sub-block's translation context into its parent context.

  Preserved sub->parent:
    * sym_tys -- for keep uniquely-identified temporaries
    * next_name_id, next_label_id -- for allocation
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
      sym_tys = IntMap.union (fun _ v_outer _ -> Some v_outer) outer.sym_tys inner.sym_tys;
      next_name_id = inner.next_name_id;
      next_label_id = inner.next_label_id;
      current_ir = inner.current_ir @ outer.current_ir;
    }

(*
  Helper function for merging a function body's translation context into its parent context.
  
  Allocate a new name_entry in the outer context, then add the assembled IR (from the body context)
  to the outer functions.
*)
let merge_fun_context (id : int) (ast_node : Ast.func_def) (t_params : t_func_param list)
    (arg_tys : sem_type list) (ret_ty : b_type) (body : translation_context)
    (outer : translation_context) : translation_context =
  let tac_func =
    {
      Tac.func_id = id;
      Tac.func_name = ast_node.Ast.func_name;
      func_params = List.map (fun p -> p.t_param_id) t_params;
      func_body = List.rev body.current_ir;
      func_ret_type = tac_elem_type_of ret_ty;
      func_symbol_types =
        IntMap.map
          (fun v -> { Tac.elem_ty = tac_elem_type_of v.elem_ty; is_array = v.dims <> [] })
          body.sym_tys;
    }
  in
  {
    outer with
    names =
      StringMap.add ast_node.Ast.func_name
        (FunEntry { id; args = arg_tys; ret = ret_ty })
        outer.names;
    next_name_id = body.next_name_id;
    next_label_id = body.next_label_id;
    functions = tac_func :: outer.functions;
  }

let alloc_name_id (ctx : translation_context) : int * translation_context =
  (ctx.next_name_id, { ctx with next_name_id = ctx.next_name_id + 1 })

let alloc_label_id (ctx : translation_context) : int * translation_context =
  (ctx.next_label_id, { ctx with next_label_id = ctx.next_label_id + 1 })

let alloc_named (name : string) (ty : sem_type) (kind : symbol_kind) (ctx : translation_context) :
    int * translation_context =
  let id, ctx = alloc_name_id ctx in
  ( id,
    {
      ctx with
      names = StringMap.add name (VarEntry { id; ty }) ctx.names;
      sym_kinds = IntMap.add id kind ctx.sym_kinds;
      sym_tys = IntMap.add id ty ctx.sym_tys;
    } )

let alloc_func (name : string) (args : sem_type list) (ret : b_type) (ctx : translation_context) :
    int * translation_context =
  let id, ctx = alloc_name_id ctx in
  ( id,
    {
      ctx with
      names = StringMap.add name (FunEntry { id; args; ret }) ctx.names;
      current_ret_ty = ret;
    } )

let alloc_temp (ty : sem_type) (ctx : translation_context) : int * translation_context =
  let id, ctx = alloc_name_id ctx in
  ( id,
    {
      ctx with
      sym_kinds = IntMap.add id Temp ctx.sym_kinds;
      sym_tys = IntMap.add id ty ctx.sym_tys;
    } )

let empty_translation_context =
  {
    names = StringMap.empty;
    sym_kinds = IntMap.empty;
    sym_tys = IntMap.empty;
    next_name_id = 0;
    next_label_id = 0;
    current_ir = [];
    functions = [];
    loop_stack = [];
    current_ret_ty = VoidType;
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
      let temp, ctx = alloc_temp int_type ctx in
      let instr = Tac.FloatToInt (temp, opnd) in
      (Tac.Symbol temp, emit instr ctx)
  | FloatType, IntType ->
      let temp, ctx = alloc_temp float_type ctx in
      let instr = Tac.IntToFloat (temp, opnd) in
      (Tac.Symbol temp, emit instr ctx)
  | _ -> internal_error "Cannot coerce between these types"

let find_sym_type (id : int) (ctx : translation_context) : sem_type =
  match IntMap.find_opt id ctx.sym_tys with
  | Some ty -> ty
  | None -> internal_error (Printf.sprintf "Symbol %d not found in type table" id)

let rec gen_opnd_index (indices : t_exp list) (dims : int list) (ctx : translation_context) :
    Tac.operand * translation_context =
  Seq.zip (List.to_seq indices) (List.to_seq dims)
  |> Seq.fold_left
       (fun (acc, ctx) (i, n) ->
         let opnd, opnd_ty, ctx = gen_exp i ctx in
         let opnd, ctx = coerce_type opnd IntType opnd_ty.elem_ty ctx in
         let temp_mul, ctx = alloc_temp int_type ctx in
         let mul = Tac.BinOp (temp_mul, Ast.Mul, acc, Tac.Const n) in
         let temp_add, ctx = alloc_temp int_type ctx in
         let add = Tac.BinOp (temp_add, Ast.Add, Tac.Symbol temp_mul, opnd) in
         (Tac.Symbol temp_add, ctx |> emit mul |> emit add))
       (Tac.Const 0, ctx)

and gen_exp (exp : t_exp) (ctx : translation_context) : Tac.operand * sem_type * translation_context
    =
  match exp with
  | TIntLit n -> (Tac.Const n, int_type, ctx)
  | TVar (id, _, [], _) ->
      let ty = find_sym_type id ctx in
      (Tac.Symbol id, ty, ctx)
  | TVar (id, _, indices, ty) ->
      let dims = (find_sym_type id ctx).dims in
      let opnd_index, ctx = gen_opnd_index indices dims ctx in
      let temp, ctx = alloc_temp ty ctx in
      let rd = Tac.MemRead (temp, id, opnd_index) in
      (Tac.Symbol temp, ty, ctx |> emit rd)
  | TUnary (op, e, res_ty) ->
      let opnd, sub_ty, ctx = gen_exp e ctx in
      let temp, ctx = alloc_temp res_ty ctx in
      let instr =
        match op with
        | Ast.Not -> Tac.UnaryOp (temp, op, opnd)
        | _ ->
            if sub_ty.elem_ty = FloatType then Tac.FUnaryOp (temp, op, opnd)
            else Tac.UnaryOp (temp, op, opnd)
      in
      (Tac.Symbol temp, res_ty, ctx |> emit instr)
  | TBinary (op, e1, e2, res_ty) ->
      let opnd1, ty1, ctx = gen_exp e1 ctx in
      let opnd2, ty2, ctx = gen_exp e2 ctx in
      let target_ty =
        if ty1.elem_ty = FloatType || ty2.elem_ty = FloatType then FloatType else IntType
      in
      let opnd1, ctx = coerce_type opnd1 target_ty ty1.elem_ty ctx in
      let opnd2, ctx = coerce_type opnd2 target_ty ty2.elem_ty ctx in
      let temp, ctx = alloc_temp res_ty ctx in
      let instr =
        if target_ty = FloatType then Tac.FBinOp (temp, op, opnd1, opnd2)
        else Tac.BinOp (temp, op, opnd1, opnd2)
      in
      (Tac.Symbol temp, res_ty, ctx |> emit instr)
  | TCall (func_id, name, params, ret_ty) ->
      let args =
        match StringMap.find_opt name ctx.names with
        | Some (FunEntry f) -> f.args
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
      let ret_temp, ctx = alloc_temp ret_ty ctx in
      let call = Tac.Call (ret_temp, func_id, List.rev param_opnds) in
      (Tac.Symbol ret_temp, ret_ty, ctx |> emit call)

let rec gen_stmt (s : t_stmt) (ctx : translation_context) : translation_context =
  match s with
  | TAssign (id, _, [], rhs) ->
      let rhs_opnd, rhs_ty, ctx = gen_exp rhs ctx in
      let lhs_ty = find_sym_type id ctx in
      let rhs_opnd, ctx = coerce_type rhs_opnd lhs_ty.elem_ty rhs_ty.elem_ty ctx in
      let instr = Tac.Move (id, rhs_opnd) in
      { ctx with current_ir = instr :: ctx.current_ir }
  | TAssign (id, _, indices, rhs) ->
      let lhs_ty = find_sym_type id ctx in
      let rhs_opnd, rhs_ty, ctx = gen_exp rhs ctx in
      let rhs_opnd, ctx = coerce_type rhs_opnd lhs_ty.elem_ty rhs_ty.elem_ty ctx in
      let opnd_index, ctx = gen_opnd_index indices lhs_ty.dims ctx in
      let wr = Tac.MemWrite (id, opnd_index, rhs_opnd) in
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
      let ctx = ctx |> emit (Tac.CondJump (cond_opnd, l_then)) in
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
      |> emit (Tac.CondJump (cond_opnd, l_body))
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
        let* a_expr, a_attr, cyx = translate_exp a ctx in
        let* rest_exprs, ctx = translate_args a_rest p_rest cyx in
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
  let translate_var name indices ctx =
    match StringMap.find_opt name ctx.names with
    | None ->
        (* Add a fallback entry for undefined variable. *)
        let id, ctx = alloc_named name int_type Named ctx in
        agg_error
          (Printf.sprintf "Undefined variable `%s`" name)
          (TVar (id, name, [], int_type), { ty = int_type; const_val = None }, ctx)
    | Some (FunEntry _) ->
        agg_error
          (Printf.sprintf "`%s` is a function, not a variable" name)
          (fallback_texp, fallback_exp_attr, ctx)
    | Some (VarEntry v) ->
        let* indices_results, ctx = translate_indices indices ctx in
        let t_indices = List.map fst indices_results in
        let idx_count = List.length indices and dim_count = List.length v.ty.dims in

        if idx_count > dim_count then
          agg_error "Too many subscripts for array"
            (TVar (v.id, name, t_indices, v.ty), { ty = v.ty; const_val = None }, ctx)
        else
          let dims' = List.drop idx_count v.ty.dims in
          let ty = { elem_ty = v.ty.elem_ty; dims = dims' } in
          let t_node = TVar (v.id, name, t_indices, ty) in
          agg_ok (t_node, { ty; const_val = None }, ctx)
  and translate_unary op sub ctx =
    let* sub_expr, sub_attr, ctx = translate_exp sub ctx in
    match op with
    | Ast.Pos | Ast.Neg ->
        let exp_val =
          match (op, sub_attr.const_val) with
          | Ast.Neg, Some v -> Some (-v)
          | Ast.Pos, Some v -> Some v
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
          if l_attr.ty.elem_ty = FloatType || r_attr.ty.elem_ty = FloatType then FloatType
          else IntType
      | _ -> IntType
    in
    let res_ty = scalar_type_of res_elem_ty in
    let result_expr = TBinary (op, l_expr, r_expr, res_ty)
    and result_attr = { ty = res_ty; const_val = None } in

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
    | Some (FunEntry f) ->
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
          agg_error "Constant initializer must be constant" (TConstExp t_e, ctx)
        else agg_ok (TConstExp t_e, ctx)
    | Ast.ConstArray vals ->
        let rec visit_vals vs ctx =
          match vs with
          | [] -> agg_ok ([], ctx)
          | v :: rest ->
              let* t_v, ctx = translate_const_init v ctx in
              let* t_rest, ctx = visit_vals rest ctx in
              agg_ok (t_v :: t_rest, ctx)
        in
        let* t_vals, ctx = visit_vals vals ctx in
        agg_ok (TConstArray t_vals, ctx)
  in
  let rec translate_init init ctx =
    match init with
    | Ast.InitExp e ->
        let* t_e, _, ctx = translate_exp e ctx in
        agg_ok (TInitExp t_e, ctx)
    | Ast.InitArray vals ->
        let rec visit_vals vs ctx =
          match vs with
          | [] -> agg_ok ([], ctx)
          | v :: rest ->
              let* t_v, ctx = translate_init v ctx in
              let* t_rest, ctx = visit_vals rest ctx in
              agg_ok (t_v :: t_rest, ctx)
        in
        let* t_vals, ctx = visit_vals vals ctx in
        agg_ok (TInitArray t_vals, ctx)
  in
  let rec add_const_defs defs ty acc_defs ctx =
    match defs with
    | [] -> agg_ok (List.rev acc_defs, ctx)
    | d :: ds ->
        let* dims_vals, dims_exprs, ctx = eval_dims d.Ast.const_dims [] [] ctx in
        let* t_init, ctx = translate_const_init d.Ast.const_init ctx in
        let id, ctx = alloc_named d.Ast.const_name { elem_ty = ty; dims = dims_vals } Named ctx in
        let t_def =
          {
            t_const_id = id;
            t_const_name = d.Ast.const_name;
            t_const_dims = dims_exprs;
            t_const_init = t_init;
          }
        in
        add_const_defs ds ty (t_def :: acc_defs) ctx
  and add_var_defs defs ty acc_defs acc_stmts ctx =
    match defs with
    | [] -> agg_ok (List.rev acc_defs, List.rev acc_stmts, ctx)
    | d :: ds ->
        let* dims_vals, dims_exprs, ctx = eval_dims d.Ast.var_dims [] [] ctx in
        let* t_init, ctx =
          match d.Ast.var_init with
          | Some init ->
              let* t_i, ctx = translate_init init ctx in
              agg_ok (Some t_i, ctx)
          | None -> agg_ok (None, ctx)
        in
        let id, ctx = alloc_named d.Ast.var_name { elem_ty = ty; dims = dims_vals } Named ctx in
        let init_stmt =
          match t_init with
          | Some (TInitExp init_exp) -> Some (TAssign (id, d.Ast.var_name, dims_exprs, init_exp))
          | Some (TInitArray _) -> internal_error "todo"
          | None -> None
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

        if lhs_attr.ty.dims <> [] || rhs_attr.ty.dims <> [] then
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
              let* t_defs, ctx = add_const_defs defs (b_type_from_ast t) [] ctx in
              agg_ok ([ TDecl (TConstDecl (b_type_from_ast t, t_defs)) ], ctx)
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
            let* t_defs, ctx = add_const_defs defs (b_type_from_ast t) [] ctx in
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
                  let id, ctx = alloc_named p.param_name arg_ty Param ctx in
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
            let* arg_tys, t_params, ctx_params = process_params f.Ast.func_params [] [] ctx in
            let func_id, ctx_body = alloc_func f.Ast.func_name arg_tys ret_ty ctx_params in
            let* t_body, ctx_body = translate_block_items f.Ast.func_body ret_ty false ctx_body in
            let ctx_body = gen_stmt (TBlock t_body) ctx_body in
            let ctx = merge_fun_context func_id f t_params arg_tys ret_ty ctx_body ctx in
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
    let* comp_unit, ctx = translate_comp_unit_item_list comp_unit ctx in
    let globals =
      IntMap.bindings ctx.sym_kinds
      |> List.filter_map (fun (id, kind) -> if kind = Named then Some id else None)
    in
    let symbols =
      IntMap.bindings ctx.sym_tys
      |> List.map (fun (id, ty) ->
          let elem_ty = tac_elem_type_of ty.elem_ty in
          (id, { Tac.elem_ty; is_array = ty.dims <> [] }))
      |> List.to_seq |> IntMap.of_seq
    in
    let program = { Tac.globals; functions = List.rev ctx.functions; symbols } in
    agg_ok (comp_unit, program)
  in
  agg_to_result result
