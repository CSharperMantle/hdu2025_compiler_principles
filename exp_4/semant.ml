open Common
open Common.AggResult
open Common.AggResult.Syntax
open Sem_ast

(* type semant_error = string *)

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

let scalar_type t = { elem_ty = t; dims = [] }
let int_type = scalar_type IntType
let fallback_exp_attr = { ty = int_type; const_val = None }
let fallback_texp = TIntLit 0

type symbol_kind =
  | Param
  | Named
  | Temp

type translation_context = {
  names : name_entry StringMap.t;
  var_kinds : symbol_kind IntMap.t;
  next_name_id : int;
  next_label_id : int;
  (* TODO: Hole for codegen *)
  ir : unit list;
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
      var_kinds = IntMap.add id kind ctx.var_kinds;
    } )

let alloc_temp (ctx : translation_context) : int * translation_context =
  let id, ctx = alloc_name_id ctx in
  (id, { ctx with var_kinds = IntMap.add id Temp ctx.var_kinds })

let empty_translation_context =
  {
    names = StringMap.empty;
    var_kinds = IntMap.empty;
    next_name_id = 0;
    next_label_id = 0;
    ir = [];
  }

(* Placeholder for codegen functions *)
let gen_exp_hole (_e : t_exp) (ctx : translation_context) : translation_context = ctx
let gen_stmt_hole (_s : t_stmt) (ctx : translation_context) : translation_context = ctx

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
        agg_error
          (Printf.sprintf "Undefined variable `%s`" name)
          (fallback_texp, fallback_exp_attr, ctx)
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
          let ctx = gen_exp_hole t_node ctx in
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
        let ctx = gen_exp_hole result_expr ctx in

        if sub_attr.ty.dims <> [] then
          agg_error "Unary operator applied to array" (result_expr, result_attr, ctx)
        else if sub_attr.ty.elem_ty = VoidType then
          agg_error "Unary operator applied to `void`" (result_expr, result_attr, ctx)
        else agg_ok (result_expr, result_attr, ctx)
    | Ast.Not ->
        let exp_val = Option.map (fun v -> if v = 0 then 1 else 0) sub_attr.const_val in
        let result_expr = TUnary (op, sub_expr, int_type)
        and result_attr = { ty = int_type; const_val = exp_val } in
        let ctx = gen_exp_hole result_expr ctx in

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
    let res_ty = scalar_type res_elem_ty in
    let result_expr = TBinary (op, l_expr, r_expr, res_ty)
    and result_attr = { ty = res_ty; const_val = None } in
    let ctx = gen_exp_hole result_expr ctx in

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
        let ret_ty = scalar_type f.ret in
        let result_expr = TCall (f.id, name, param_exprs, ret_ty)
        and result_attr = { ty = ret_ty; const_val = None } in
        let ctx = gen_exp_hole result_expr ctx in
        agg_ok (result_expr, result_attr, ctx)
  in
  match exp with
  | Ast.IntLit v ->
      let t_node = TIntLit v in
      let ctx = gen_exp_hole t_node ctx in
      agg_ok (t_node, { ty = int_type; const_val = Some v }, ctx)
  | Ast.Var (name, indices) -> translate_var name indices ctx
  | Ast.Unary (op, sub) -> translate_unary op sub ctx
  | Ast.Binary (op, lhs, rhs) -> translate_binary op lhs rhs ctx
  | Ast.Call (name, params) -> translate_call name params ctx

let translate (comp_unit : Ast.comp_unit) (ctx : translation_context) :
    (t_comp_unit * translation_context, string list) result =
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
        let _, ctx = alloc_named d.Ast.const_name { elem_ty = ty; dims = dims_vals } Named ctx in
        let t_def =
          { t_const_name = d.Ast.const_name; t_const_dims = dims_exprs; t_const_init = t_init }
        in
        add_const_defs ds ty (t_def :: acc_defs) ctx
  and add_var_defs defs ty acc_defs ctx =
    match defs with
    | [] -> agg_ok (List.rev acc_defs, ctx)
    | d :: ds ->
        let* dims_vals, dims_exprs, ctx = eval_dims d.Ast.var_dims [] [] ctx in
        let* t_init, ctx =
          match d.Ast.var_init with
          | Some init ->
              let* t_i, ctx = translate_init init ctx in
              agg_ok (Some t_i, ctx)
          | None -> agg_ok (None, ctx)
        in
        let _, ctx = alloc_named d.Ast.var_name { elem_ty = ty; dims = dims_vals } Named ctx in
        let t_def = { t_var_name = d.Ast.var_name; t_var_dims = dims_exprs; t_var_init = t_init } in
        add_var_defs ds ty (t_def :: acc_defs) ctx
  in

  let rec translate_stmt stmt ret_ty in_loop ctx =
    match stmt with
    | Ast.Assign (name, indices, rhs) ->
        let* lhs_expr, lhs_attr, ctx = translate_exp (Ast.Var (name, indices)) ctx in
        let* rhs_expr, rhs_attr, ctx = translate_exp rhs ctx in
        let t_indices =
          match lhs_expr with
          | TVar (_, _, idxs, _) -> idxs
          | _ -> []
        in
        let res_stmt = TAssign (name, t_indices, rhs_expr) in
        let ctx = gen_stmt_hole res_stmt ctx in

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
        let ctx = gen_stmt_hole res_stmt ctx in
        agg_ok (res_stmt, ctx)
    | Ast.ExprStmt None ->
        let res_stmt = TExprStmt None in
        let ctx = gen_stmt_hole res_stmt ctx in
        agg_ok (res_stmt, ctx)
    | Ast.Block items ->
        let* t_items, ctx = translate_block items ret_ty in_loop ctx in
        let res_stmt = TBlock t_items in
        let ctx = gen_stmt_hole res_stmt ctx in
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
        let ctx = gen_stmt_hole res_stmt ctx in

        if cond_attr.ty.elem_ty = VoidType || cond_attr.ty.dims <> [] then
          agg_error "Condition must be non-`void` scalar" (res_stmt, ctx)
        else agg_ok (res_stmt, ctx)
    | Ast.While (cond, body) ->
        let* cond_expr, cond_attr, ctx = translate_exp cond ctx in
        let* body_stmt, ctx = translate_stmt body ret_ty true ctx in

        let res_stmt = TWhile (cond_expr, body_stmt) in
        let ctx = gen_stmt_hole res_stmt ctx in

        if cond_attr.ty.elem_ty = VoidType || cond_attr.ty.dims <> [] then
          agg_error "Condition must be non-`void` scalar" (res_stmt, ctx)
        else agg_ok (res_stmt, ctx)
    | Ast.Break ->
        let res_stmt = TBreak in
        let ctx = gen_stmt_hole res_stmt ctx in
        if in_loop then agg_ok (res_stmt, ctx)
        else agg_error "`break` statement not within a loop" (res_stmt, ctx)
    | Ast.Continue ->
        let res_stmt = TContinue in
        let ctx = gen_stmt_hole res_stmt ctx in
        if in_loop then agg_ok (res_stmt, ctx)
        else agg_error "`continue` statement not within a loop" (res_stmt, ctx)
    | Ast.Return e_opt -> (
        match (e_opt, ret_ty) with
        | None, VoidType ->
            let res_stmt = TReturn None in
            let ctx = gen_stmt_hole res_stmt ctx in
            agg_ok (res_stmt, ctx)
        | Some _, VoidType ->
            let res_stmt = TReturn None in
            let ctx = gen_stmt_hole res_stmt ctx in
            agg_error "`void` function cannot return values" (res_stmt, ctx)
        | None, _ ->
            let res_stmt = TReturn None in
            let ctx = gen_stmt_hole res_stmt ctx in
            agg_error "Non-`void` function must return a value" (res_stmt, ctx)
        | Some e, t ->
            let* t_e, attr, ctx = translate_exp e ctx in
            let res_stmt = TReturn (Some t_e) in
            let ctx = gen_stmt_hole res_stmt ctx in

            if attr.ty.dims <> [] then agg_error "Cannot return an array" (res_stmt, ctx)
            else if attr.ty.elem_ty = t then agg_ok (res_stmt, ctx)
            else if t = FloatType && attr.ty.elem_ty = IntType then agg_ok (res_stmt, ctx)
            else agg_error "Return type mismatch" (res_stmt, ctx))
  and translate_block items ret_ty in_loop ctx =
    match items with
    | [] -> agg_ok ([], ctx)
    | item :: rest ->
        let* t_item, ctx =
          match item with
          | Ast.Decl (Ast.VarDecl (t, defs)) ->
              let* t_defs, ctx = add_var_defs defs (b_type_from_ast t) [] ctx in
              agg_ok (TDecl (TVarDecl (b_type_from_ast t, t_defs)), ctx)
          | Ast.Decl (Ast.ConstDecl (t, defs)) ->
              let* t_defs, ctx = add_const_defs defs (b_type_from_ast t) [] ctx in
              agg_ok (TDecl (TConstDecl (b_type_from_ast t, t_defs)), ctx)
          | Ast.Stmt s ->
              let* t_s, ctx = translate_stmt s ret_ty in_loop ctx in
              agg_ok (TStmt t_s, ctx)
        in
        let* rest_items, final_ctx = translate_block rest ret_ty in_loop ctx in
        agg_ok (t_item :: rest_items, final_ctx)
  in
  let rec translate_comp_unit_item_list items ctx =
    match items with
    | [] -> agg_ok ([], ctx)
    | item :: rest -> (
        match item with
        | Ast.DeclItem (Ast.VarDecl (t, defs)) ->
            let* t_defs, ctx = add_var_defs defs (b_type_from_ast t) [] ctx in
            let t_item = TDeclItem (TVarDecl (b_type_from_ast t, t_defs)) in
            let* rest_items, ctx = translate_comp_unit_item_list rest ctx in
            agg_ok (t_item :: rest_items, ctx)
        | Ast.DeclItem (Ast.ConstDecl (t, defs)) ->
            let* t_defs, ctx = add_const_defs defs (b_type_from_ast t) [] ctx in
            let t_item = TDeclItem (TConstDecl (b_type_from_ast t, t_defs)) in
            let* rest_items, ctx = translate_comp_unit_item_list rest ctx in
            agg_ok (t_item :: rest_items, ctx)
        | Ast.FuncDefItem f ->
            let ret_ty = b_type_from_ast_func f.Ast.func_ret_type in
            let rec process_params params acc_args acc_t_params ctx =
              match params with
              | [] -> agg_ok (List.rev acc_args, List.rev acc_t_params, ctx)
              | p :: p_rest ->
                  let ty = b_type_from_ast p.Ast.param_type in
                  let* dims_vals, dims_exprs, ctx =
                    match p.Ast.param_dims with
                    | None -> agg_ok ([], None, ctx)
                    | Some ds ->
                        let* d_vals, d_exprs, ctx = eval_dims ds [] [] ctx in
                        agg_ok (0 :: d_vals, Some d_exprs, ctx)
                  in
                  let t_param =
                    {
                      t_param_type = ty;
                      t_param_name = p.Ast.param_name;
                      t_param_dims = dims_exprs;
                    }
                  in
                  process_params p_rest
                    ((p.Ast.param_name, { elem_ty = ty; dims = dims_vals }) :: acc_args)
                    (t_param :: acc_t_params) ctx
            in
            let* named_arg_types, t_params, ctx = process_params f.Ast.func_params [] [] ctx in
            let arg_types = List.map snd named_arg_types in
            let new_id, ctx = alloc_name_id ctx in
            let entry = FunEntry { args = arg_types; ret = ret_ty; id = new_id } in
            let ctx = { ctx with names = StringMap.add f.Ast.func_name entry ctx.names } in
            let ctx_body =
              List.fold_left (fun ctx (n, t) -> snd (alloc_named n t Param ctx)) ctx named_arg_types
            in
            let* t_body, _ = translate_block f.Ast.func_body ret_ty false ctx_body in
            let t_func =
              {
                t_func_ret_type = f.Ast.func_ret_type |> Option.map b_type_from_ast;
                t_func_name = f.Ast.func_name;
                t_func_params = t_params;
                t_func_body = t_body;
              }
            in
            let* rest_items, ctx = translate_comp_unit_item_list rest ctx in
            agg_ok (TFuncDefItem t_func :: rest_items, ctx))
  in
  let result = translate_comp_unit_item_list comp_unit ctx in
  agg_to_result result
