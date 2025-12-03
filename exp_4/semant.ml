open Common
open Common.AggResult
open Common.AggResult.Syntax

(* type semant_error = string *)

type b_type =
  | IntType
  | FloatType
  | VoidType

let b_type_from_ast = function
  | Ast.Int -> IntType
  | Ast.Float -> FloatType

let b_type_from_ast_func = function
  | Some t -> b_type_from_ast t
  | None -> VoidType

type sem_type = {
  elem_ty : b_type;
  dims : int list;
}

type name_entry =
  | VarEntry of sem_type
  | FunEntry of {
      args : sem_type list;
      ok : b_type;
    }

type name_env = name_entry StringMap.t

type exp_attr = {
  ty : sem_type;
  const_val : int option;
}

let scalar_type t = { elem_ty = t; dims = [] }
let int_type = scalar_type IntType
let float_type = scalar_type FloatType
let void_type = scalar_type VoidType
let fallback_exp_attr = { ty = int_type; const_val = None }

let rec visit_exp (exp : Ast.exp) (env : name_env) : (exp_attr, string) agg_result =
  let visit_var name indices =
    match StringMap.find_opt name env with
    | None -> agg_error (Printf.sprintf "Undefined variable `%s`" name) fallback_exp_attr
    | Some (FunEntry _) ->
        agg_error (Printf.sprintf "`%s` is a function, not a variable" name) fallback_exp_attr
    | Some (VarEntry v_ty) ->
        let rec check_indices idxs =
          match idxs with
          | [] -> agg_ok []
          | i :: rest ->
              let* i_attr = visit_exp i env in
              let* rest_attrs = check_indices rest in
              let result = i_attr :: rest_attrs in
              if i_attr.ty.elem_ty <> IntType || i_attr.ty.dims <> [] then
                agg_error "Array index must be an integer scalar" result
              else agg_ok result
        in
        let* _ = check_indices indices in
        let idx_count = List.length indices and dim_count = List.length v_ty.dims in
        if idx_count > dim_count then
          agg_error "Too many subscripts for array"
            { ty = { elem_ty = v_ty.elem_ty; dims = [] }; const_val = None }
        else
          let dims' = List.drop idx_count v_ty.dims in
          agg_ok { ty = { elem_ty = v_ty.elem_ty; dims = dims' }; const_val = None }
  and visit_unary op sub =
    let* sub_attr = visit_exp sub env in
    match op with
    | Ast.Pos | Ast.Neg ->
        let exp_val =
          match (op, sub_attr.const_val) with
          | Ast.Neg, Some v -> Some (-v)
          | Ast.Pos, Some v -> Some v
          | _ -> None
        in
        let result = { ty = sub_attr.ty; const_val = exp_val } in

        if sub_attr.ty.dims <> [] then agg_error "Unary operator applied to array" result
        else if sub_attr.ty.elem_ty = VoidType then
          agg_error "Unary operator applied to `void`" result
        else agg_ok result
    | Ast.Not ->
        let exp_val = Option.map (fun v -> if v = 0 then 1 else 0) sub_attr.const_val in
        let result = { ty = int_type; const_val = exp_val } in

        if sub_attr.ty.dims <> [] then agg_error "`!` applied to array" result
        else if sub_attr.ty.elem_ty = VoidType then agg_error "`!` applied to `void`" result
        else agg_ok result
  and visit_binary op lhs rhs =
    let* l_attr = visit_exp lhs env in
    let* r_attr = visit_exp rhs env in
    let res_elem_ty =
      match op with
      | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Mod ->
          if l_attr.ty.elem_ty = FloatType || r_attr.ty.elem_ty = FloatType then FloatType
          else IntType
      | _ -> IntType
    in
    let result = { ty = scalar_type res_elem_ty; const_val = None } in
    if l_attr.ty.dims <> [] || r_attr.ty.dims <> [] then
      agg_error "Binary operator applied to array" result
    else if l_attr.ty.elem_ty = VoidType || r_attr.ty.elem_ty = VoidType then
      agg_error "Binary operator applied to void" result
    else agg_ok result
  and visit_call name params =
    match StringMap.find_opt name env with
    | None -> agg_error (Printf.sprintf "Undefined function `%s`" name) fallback_exp_attr
    | Some (VarEntry _) ->
        agg_error (Printf.sprintf "`%s` is a variable, not a function" name) fallback_exp_attr
    | Some (FunEntry f) ->
        let rec check_args args pms =
          match (args, pms) with
          | [], [] -> agg_ok ()
          | a :: a_rest, p_ty :: p_rest ->
              let* a_attr = visit_exp a env in
              let* _ = check_args a_rest p_rest in

              if a_attr.ty.dims <> p_ty.dims then agg_error "Argument dimension mismatch" ()
              else if
                a_attr.ty.elem_ty <> p_ty.elem_ty
                && not (a_attr.ty.elem_ty = IntType && p_ty.elem_ty = FloatType)
              then agg_error "Argument type mismatch" ()
              else agg_ok ()
          | _ -> agg_error "Argument count mismatch" ()
        in
        let* _ = check_args params f.args in
        agg_ok { ty = scalar_type f.ok; const_val = None }
  in
  match exp with
  | Ast.IntLit v -> agg_ok { ty = int_type; const_val = Some v }
  | Ast.Var (name, indices) -> visit_var name indices
  | Ast.Unary (op, sub) -> visit_unary op sub
  | Ast.Binary (op, lhs, rhs) -> visit_binary op lhs rhs
  | Ast.Call (name, params) -> visit_call name params

let typecheck (comp_unit : Ast.comp_unit) : (unit, string list) result =
  let eval_const_dim e env =
    let* attr = visit_exp e env in
    match attr.const_val with
    | Some v -> agg_ok v
    | None -> agg_error "Array dimension must be a constant integer" 1
  in
  let rec eval_dims dims acc env =
    match dims with
    | [] -> agg_ok (List.rev acc)
    | h :: t ->
        let* v = eval_const_dim h env in
        eval_dims t (v :: acc) env
  in
  let rec add_const_defs defs ty env =
    match defs with
    | [] -> agg_ok env
    | d :: ds ->
        let* dims = eval_dims d.Ast.const_dims [] env in
        let entry = VarEntry { elem_ty = ty; dims } in
        add_const_defs ds ty (StringMap.add d.Ast.const_name entry env)
  and add_var_defs defs ty env =
    match defs with
    | [] -> agg_ok env
    | d :: ds ->
        let* dims = eval_dims d.Ast.var_dims [] env in
        let entry = VarEntry { elem_ty = ty; dims } in
        add_var_defs ds ty (StringMap.add d.Ast.var_name entry env)
  in
  let rec visit_stmt stmt ret_ty in_loop env =
    match stmt with
    | Ast.Assign (name, indices, rhs) ->
        let* lhs_attr = visit_exp (Ast.Var (name, indices)) env in
        let* rhs_attr = visit_exp rhs env in

        if lhs_attr.ty.dims <> [] || rhs_attr.ty.dims <> [] then
          agg_error "Assignment operands must be scalars" env
        else if lhs_attr.ty.elem_ty = VoidType then agg_error "Cannot assign to `void`" env
        else if
          lhs_attr.ty.elem_ty <> rhs_attr.ty.elem_ty
          && not (lhs_attr.ty.elem_ty = FloatType && rhs_attr.ty.elem_ty = IntType)
        then agg_error "Type mismatch in assignment" env
        else agg_ok env
    | Ast.ExprStmt (Some e) ->
        let* _ = visit_exp e env in
        agg_ok env
    | Ast.ExprStmt None -> agg_ok env
    | Ast.Block items ->
        let* _ = visit_block items ret_ty in_loop env in
        agg_ok env
    | Ast.If (cond, then_s, else_s) ->
        let* cond_attr = visit_exp cond env in
        let* _ = visit_stmt then_s ret_ty in_loop env in
        let* _ =
          match else_s with
          | Some s -> visit_stmt s ret_ty in_loop env
          | None -> agg_ok env
        in

        if cond_attr.ty.elem_ty = VoidType || cond_attr.ty.dims <> [] then
          agg_error "Condition must be non-`void` scalar" env
        else agg_ok env
    | Ast.While (cond, body) ->
        let* cond_attr = visit_exp cond env in
        let* _ = visit_stmt body ret_ty true env in
        if cond_attr.ty.elem_ty = VoidType || cond_attr.ty.dims <> [] then
          agg_error "Condition must be non-`void` scalar" env
        else agg_ok env
    | Ast.Break ->
        if in_loop then agg_ok env else agg_error "`break` statement not within a loop" env
    | Ast.Continue ->
        if in_loop then agg_ok env else agg_error "`continue` statement not within a loop" env
    | Ast.Return e_opt -> (
        match (e_opt, ret_ty) with
        | None, VoidType -> agg_ok env
        | Some _, VoidType -> agg_error "`void` function cannot return values" env
        | None, _ -> agg_error "Non-`void` function must return a value" env
        | Some e, t ->
            let* attr = visit_exp e env in

            if attr.ty.dims <> [] then agg_error "Cannot return an array" env
            else if attr.ty.elem_ty = t then agg_ok env
            else if t = FloatType && attr.ty.elem_ty = IntType then agg_ok env
            else agg_error "Return type mismatch" env)
  and visit_block items ret_ty in_loop env =
    match items with
    | [] -> agg_ok env
    | item :: rest ->
        let* env' =
          match item with
          | Ast.Decl (Ast.VarDecl (t, defs)) -> add_var_defs defs (b_type_from_ast t) env
          | Ast.Decl (Ast.ConstDecl (t, defs)) -> add_const_defs defs (b_type_from_ast t) env
          | Ast.Stmt s ->
              let* _ = visit_stmt s ret_ty in_loop env in
              agg_ok env
        in
        visit_block rest ret_ty in_loop env'
  in
  let rec visit_comp_unit_item_list items env =
    match items with
    | [] -> agg_ok env
    | item :: rest -> (
        match item with
        | Ast.DeclItem (Ast.VarDecl (t, defs)) ->
            let* env' = add_var_defs defs (b_type_from_ast t) env in
            visit_comp_unit_item_list rest env'
        | Ast.DeclItem (Ast.ConstDecl (t, defs)) ->
            let* env' = add_const_defs defs (b_type_from_ast t) env in
            visit_comp_unit_item_list rest env'
        | Ast.FuncDefItem f ->
            let ret_ty =
              match f.Ast.func_ret_type with
              | Some t -> b_type_from_ast t
              | None -> VoidType
            in
            let rec process_params params acc_args =
              match params with
              | [] -> agg_ok (List.rev acc_args)
              | p :: p_rest ->
                  let ty = b_type_from_ast p.Ast.param_type in
                  let* dims =
                    match p.Ast.param_dims with
                    | None -> agg_ok []
                    | Some ds ->
                        let* d_vals = eval_dims ds [] env in
                        agg_ok (0 :: d_vals)
                  in
                  process_params p_rest ((p.Ast.param_name, { elem_ty = ty; dims }) :: acc_args)
            in
            let* named_arg_types = process_params f.Ast.func_params [] in
            let arg_types = List.map snd named_arg_types in
            let entry = FunEntry { args = arg_types; ok = ret_ty } in
            let env_with_func = StringMap.add f.Ast.func_name entry env in
            let env_body =
              List.fold_left
                (fun e (n, t) -> StringMap.add n (VarEntry t) e)
                env_with_func named_arg_types
            in
            let* _ = visit_block f.Ast.func_body ret_ty false env_body in
            visit_comp_unit_item_list rest env_with_func)
  in
  let _, errs = visit_comp_unit_item_list comp_unit StringMap.empty in
  if errs = [] then Ok () else Error errs
