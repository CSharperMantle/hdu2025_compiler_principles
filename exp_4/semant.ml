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

type t_exp =
  | TIntLit of int
  | TVar of string * t_exp list * sem_type
  | TUnary of Ast.unary_op * t_exp * sem_type
  | TBinary of Ast.bin_op * t_exp * t_exp * sem_type
  | TCall of string * t_exp list * sem_type

type t_const_init_val =
  | TConstExp of t_exp
  | TConstArray of t_const_init_val list

type t_init_val =
  | TInitExp of t_exp
  | TInitArray of t_init_val list

type t_const_def = {
  t_const_name : string;
  t_const_dims : t_exp list;
  t_const_init : t_const_init_val;
}

type t_var_def = {
  t_var_name : string;
  t_var_dims : t_exp list;
  t_var_init : t_init_val option;
}

type t_decl =
  | TConstDecl of b_type * t_const_def list
  | TVarDecl of b_type * t_var_def list

and t_stmt =
  | TAssign of string * t_exp list * t_exp
  | TExprStmt of t_exp option
  | TBlock of t_block_item list
  | TIf of t_exp * t_stmt * t_stmt option
  | TWhile of t_exp * t_stmt
  | TBreak
  | TContinue
  | TReturn of t_exp option

and t_block_item =
  | TDecl of t_decl
  | TStmt of t_stmt

type t_func_param = {
  t_param_type : b_type;
  t_param_name : string;
  t_param_dims : t_exp list option;
}

type t_func_def = {
  t_func_ret_type : b_type option;
  t_func_name : string;
  t_func_params : t_func_param list;
  t_func_body : t_block_item list;
}

type t_comp_unit_item =
  | TDeclItem of t_decl
  | TFuncDefItem of t_func_def

type t_comp_unit = t_comp_unit_item list

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
let fallback_exp_attr = { ty = int_type; const_val = None }
let fallback_texp = TIntLit 0

let rec type_exp (exp : Ast.exp) (env : name_env) : (t_exp * exp_attr, string) agg_result =
  let rec type_indices idxs =
    match idxs with
    | [] -> agg_ok []
    | i :: rest ->
        let* i_expr, i_attr = type_exp i env in
        let* rest_results = type_indices rest in
        let result = (i_expr, i_attr) :: rest_results in
        if i_attr.ty.elem_ty <> IntType || i_attr.ty.dims <> [] then
          agg_error "Array index must be an integer scalar" result
        else agg_ok result
  and type_args args pms =
    match (args, pms) with
    | [], [] -> agg_ok []
    | a :: a_rest, p_ty :: p_rest ->
        let* a_expr, a_attr = type_exp a env in
        let* rest_exprs = type_args a_rest p_rest in
        let current_exprs = a_expr :: rest_exprs in

        if a_attr.ty.dims <> p_ty.dims then agg_error "Argument dimension mismatch" current_exprs
        else if
          a_attr.ty.elem_ty <> p_ty.elem_ty
          && not (a_attr.ty.elem_ty = IntType && p_ty.elem_ty = FloatType)
        then agg_error "Argument type mismatch" current_exprs
        else agg_ok current_exprs
    | _ -> agg_error "Argument count mismatch" []
  in
  let type_var name indices =
    match StringMap.find_opt name env with
    | None ->
        agg_error (Printf.sprintf "Undefined variable `%s`" name) (fallback_texp, fallback_exp_attr)
    | Some (FunEntry _) ->
        agg_error
          (Printf.sprintf "`%s` is a function, not a variable" name)
          (fallback_texp, fallback_exp_attr)
    | Some (VarEntry v_ty) ->
        let* indices_results = type_indices indices in
        let t_indices = List.map fst indices_results in
        let idx_count = List.length indices and dim_count = List.length v_ty.dims in
        if idx_count > dim_count then
          agg_error "Too many subscripts for array"
            ( TVar (name, t_indices, { elem_ty = v_ty.elem_ty; dims = [] }),
              { ty = { elem_ty = v_ty.elem_ty; dims = [] }; const_val = None } )
        else
          let dims' = List.drop idx_count v_ty.dims in
          let ty = { elem_ty = v_ty.elem_ty; dims = dims' } in
          agg_ok (TVar (name, t_indices, ty), { ty; const_val = None })
  and type_unary op sub =
    let* sub_expr, sub_attr = type_exp sub env in
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
          agg_error "Unary operator applied to array" (result_expr, result_attr)
        else if sub_attr.ty.elem_ty = VoidType then
          agg_error "Unary operator applied to `void`" (result_expr, result_attr)
        else agg_ok (result_expr, result_attr)
    | Ast.Not ->
        let exp_val = Option.map (fun v -> if v = 0 then 1 else 0) sub_attr.const_val in
        let result_expr = TUnary (op, sub_expr, int_type)
        and result_attr = { ty = int_type; const_val = exp_val } in

        if sub_attr.ty.dims <> [] then agg_error "`!` applied to array" (result_expr, result_attr)
        else if sub_attr.ty.elem_ty = VoidType then
          agg_error "`!` applied to `void`" (result_expr, result_attr)
        else agg_ok (result_expr, result_attr)
  and type_binary op lhs rhs =
    let* l_expr, l_attr = type_exp lhs env in
    let* r_expr, r_attr = type_exp rhs env in
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

    if l_attr.ty.dims <> [] || r_attr.ty.dims <> [] then
      agg_error "Binary operator applied to array" (result_expr, result_attr)
    else if l_attr.ty.elem_ty = VoidType || r_attr.ty.elem_ty = VoidType then
      agg_error "Binary operator applied to void" (result_expr, result_attr)
    else agg_ok (result_expr, result_attr)
  and type_call name params =
    match StringMap.find_opt name env with
    | None ->
        agg_error (Printf.sprintf "Undefined function `%s`" name) (fallback_texp, fallback_exp_attr)
    | Some (VarEntry _) ->
        agg_error
          (Printf.sprintf "`%s` is a variable, not a function" name)
          (fallback_texp, fallback_exp_attr)
    | Some (FunEntry f) ->
        let* param_exprs = type_args params f.args in
        let res_ty = scalar_type f.ok in
        let result_expr = TCall (name, param_exprs, res_ty)
        and result_attr = { ty = res_ty; const_val = None } in

        agg_ok (result_expr, result_attr)
  in
  match exp with
  | Ast.IntLit v -> agg_ok (TIntLit v, { ty = int_type; const_val = Some v })
  | Ast.Var (name, indices) -> type_var name indices
  | Ast.Unary (op, sub) -> type_unary op sub
  | Ast.Binary (op, lhs, rhs) -> type_binary op lhs rhs
  | Ast.Call (name, params) -> type_call name params

let type_comp_unit (comp_unit : Ast.comp_unit) : (t_comp_unit, string list) result =
  let eval_const_dim e env =
    let* t_e, attr = type_exp e env in
    match attr.const_val with
    | Some v -> agg_ok (v, t_e)
    | None -> agg_error "Array dimension must be a constant integer" (1, t_e)
  in
  let rec eval_dims dims acc_vals acc_exprs env =
    match dims with
    | [] -> agg_ok (List.rev acc_vals, List.rev acc_exprs)
    | h :: t ->
        let* v, t_e = eval_const_dim h env in
        eval_dims t (v :: acc_vals) (t_e :: acc_exprs) env
  in
  let rec type_const_init init env =
    match init with
    | Ast.ConstExp e ->
        let* t_e, attr = type_exp e env in
        if attr.const_val = None then
          agg_error "Constant initializer must be constant" (TConstExp t_e)
        else agg_ok (TConstExp t_e)
    | Ast.ConstArray vals ->
        let rec visit_vals vs =
          match vs with
          | [] -> agg_ok []
          | v :: rest ->
              let* t_v = type_const_init v env in
              let* t_rest = visit_vals rest in
              agg_ok (t_v :: t_rest)
        in
        let* t_vals = visit_vals vals in
        agg_ok (TConstArray t_vals)
  in
  let rec type_init init env =
    match init with
    | Ast.InitExp e ->
        let* t_e, _ = type_exp e env in
        agg_ok (TInitExp t_e)
    | Ast.InitArray vals ->
        let rec visit_vals vs =
          match vs with
          | [] -> agg_ok []
          | v :: rest ->
              let* t_v = type_init v env in
              let* t_rest = visit_vals rest in
              agg_ok (t_v :: t_rest)
        in
        let* t_vals = visit_vals vals in
        agg_ok (TInitArray t_vals)
  in
  let rec add_const_defs defs ty env acc_defs =
    match defs with
    | [] -> agg_ok (List.rev acc_defs, env)
    | d :: ds ->
        let* dims_vals, dims_exprs = eval_dims d.Ast.const_dims [] [] env in
        let* t_init = type_const_init d.Ast.const_init env in
        let entry = VarEntry { elem_ty = ty; dims = dims_vals } in
        let t_def =
          { t_const_name = d.Ast.const_name; t_const_dims = dims_exprs; t_const_init = t_init }
        in
        add_const_defs ds ty (StringMap.add d.Ast.const_name entry env) (t_def :: acc_defs)
  and add_var_defs defs ty env acc_defs =
    match defs with
    | [] -> agg_ok (List.rev acc_defs, env)
    | d :: ds ->
        let* dims_vals, dims_exprs = eval_dims d.Ast.var_dims [] [] env in
        let* t_init =
          match d.Ast.var_init with
          | Some init ->
              let* t_i = type_init init env in
              agg_ok (Some t_i)
          | None -> agg_ok None
        in
        let entry = VarEntry { elem_ty = ty; dims = dims_vals } in
        let t_def = { t_var_name = d.Ast.var_name; t_var_dims = dims_exprs; t_var_init = t_init } in
        add_var_defs ds ty (StringMap.add d.Ast.var_name entry env) (t_def :: acc_defs)
  in

  let rec type_stmt stmt ret_ty in_loop env =
    match stmt with
    | Ast.Assign (name, indices, rhs) ->
        let* lhs_expr, lhs_attr = type_exp (Ast.Var (name, indices)) env in
        let* rhs_expr, rhs_attr = type_exp rhs env in

        let t_indices =
          match lhs_expr with
          | TVar (_, idxs, _) -> idxs
          | _ -> []
        in

        let res_stmt = TAssign (name, t_indices, rhs_expr) in

        if lhs_attr.ty.dims <> [] || rhs_attr.ty.dims <> [] then
          agg_error "Assignment operands must be scalars" (res_stmt, env)
        else if lhs_attr.ty.elem_ty = VoidType then
          agg_error "Cannot assign to `void`" (res_stmt, env)
        else if
          lhs_attr.ty.elem_ty <> rhs_attr.ty.elem_ty
          && not (lhs_attr.ty.elem_ty = FloatType && rhs_attr.ty.elem_ty = IntType)
        then agg_error "Type mismatch in assignment" (res_stmt, env)
        else agg_ok (res_stmt, env)
    | Ast.ExprStmt (Some e) ->
        let* t_e, _ = type_exp e env in
        agg_ok (TExprStmt (Some t_e), env)
    | Ast.ExprStmt None -> agg_ok (TExprStmt None, env)
    | Ast.Block items ->
        let* t_items, _ = type_block items ret_ty in_loop env in
        agg_ok (TBlock t_items, env)
    | Ast.If (cond, then_s, else_s) ->
        let* cond_expr, cond_attr = type_exp cond env in
        let* then_stmt, _ = type_stmt then_s ret_ty in_loop env in
        let* else_stmt, _ =
          match else_s with
          | Some s ->
              let* s_stmt, _ = type_stmt s ret_ty in_loop env in
              agg_ok (Some s_stmt, env)
          | None -> agg_ok (None, env)
        in

        let res_stmt = TIf (cond_expr, then_stmt, else_stmt) in

        if cond_attr.ty.elem_ty = VoidType || cond_attr.ty.dims <> [] then
          agg_error "Condition must be non-`void` scalar" (res_stmt, env)
        else agg_ok (res_stmt, env)
    | Ast.While (cond, body) ->
        let* cond_expr, cond_attr = type_exp cond env in
        let* body_stmt, _ = type_stmt body ret_ty true env in

        let res_stmt = TWhile (cond_expr, body_stmt) in

        if cond_attr.ty.elem_ty = VoidType || cond_attr.ty.dims <> [] then
          agg_error "Condition must be non-`void` scalar" (res_stmt, env)
        else agg_ok (res_stmt, env)
    | Ast.Break ->
        if in_loop then agg_ok (TBreak, env)
        else agg_error "`break` statement not within a loop" (TBreak, env)
    | Ast.Continue ->
        if in_loop then agg_ok (TContinue, env)
        else agg_error "`continue` statement not within a loop" (TContinue, env)
    | Ast.Return e_opt -> (
        match (e_opt, ret_ty) with
        | None, VoidType -> agg_ok (TReturn None, env)
        | Some _, VoidType -> agg_error "`void` function cannot return values" (TReturn None, env)
        | None, _ -> agg_error "Non-`void` function must return a value" (TReturn None, env)
        | Some e, t ->
            let* t_e, attr = type_exp e env in
            let res_stmt = TReturn (Some t_e) in

            if attr.ty.dims <> [] then agg_error "Cannot return an array" (res_stmt, env)
            else if attr.ty.elem_ty = t then agg_ok (res_stmt, env)
            else if t = FloatType && attr.ty.elem_ty = IntType then agg_ok (res_stmt, env)
            else agg_error "Return type mismatch" (res_stmt, env))
  and type_block items ret_ty in_loop env =
    match items with
    | [] -> agg_ok ([], env)
    | item :: rest ->
        let* t_item, env' =
          match item with
          | Ast.Decl (Ast.VarDecl (t, defs)) ->
              let* t_defs, env' = add_var_defs defs (b_type_from_ast t) env [] in
              agg_ok (TDecl (TVarDecl (b_type_from_ast t, t_defs)), env')
          | Ast.Decl (Ast.ConstDecl (t, defs)) ->
              let* t_defs, env' = add_const_defs defs (b_type_from_ast t) env [] in
              agg_ok (TDecl (TConstDecl (b_type_from_ast t, t_defs)), env')
          | Ast.Stmt s ->
              let* t_s, _ = type_stmt s ret_ty in_loop env in
              agg_ok (TStmt t_s, env)
        in
        let* rest_items, final_env = type_block rest ret_ty in_loop env' in
        agg_ok (t_item :: rest_items, final_env)
  in
  let rec type_comp_unit_item_list items env =
    match items with
    | [] -> agg_ok []
    | item :: rest -> (
        match item with
        | Ast.DeclItem (Ast.VarDecl (t, defs)) ->
            let* t_defs, env' = add_var_defs defs (b_type_from_ast t) env [] in
            let t_item = TDeclItem (TVarDecl (b_type_from_ast t, t_defs)) in
            let* rest_items = type_comp_unit_item_list rest env' in
            agg_ok (t_item :: rest_items)
        | Ast.DeclItem (Ast.ConstDecl (t, defs)) ->
            let* t_defs, env' = add_const_defs defs (b_type_from_ast t) env [] in
            let t_item = TDeclItem (TConstDecl (b_type_from_ast t, t_defs)) in
            let* rest_items = type_comp_unit_item_list rest env' in
            agg_ok (t_item :: rest_items)
        | Ast.FuncDefItem f ->
            let ret_ty = b_type_from_ast_func f.Ast.func_ret_type in
            let rec process_params params acc_args acc_t_params =
              match params with
              | [] -> agg_ok (List.rev acc_args, List.rev acc_t_params)
              | p :: p_rest ->
                  let ty = b_type_from_ast p.Ast.param_type in
                  let* dims_vals, dims_exprs =
                    match p.Ast.param_dims with
                    | None -> agg_ok ([], None)
                    | Some ds ->
                        let* d_vals, d_exprs = eval_dims ds [] [] env in
                        agg_ok (0 :: d_vals, Some d_exprs)
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
                    (t_param :: acc_t_params)
            in
            let* named_arg_types, t_params = process_params f.Ast.func_params [] [] in
            let arg_types = List.map snd named_arg_types in
            let entry = FunEntry { args = arg_types; ok = ret_ty } in
            let env_with_func = StringMap.add f.Ast.func_name entry env in
            let env_body =
              List.fold_left
                (fun e (n, t) -> StringMap.add n (VarEntry t) e)
                env_with_func named_arg_types
            in
            let* t_body, _ = type_block f.Ast.func_body ret_ty false env_body in
            let t_func =
              {
                t_func_ret_type = f.Ast.func_ret_type |> Option.map b_type_from_ast;
                t_func_name = f.Ast.func_name;
                t_func_params = t_params;
                t_func_body = t_body;
              }
            in
            let* rest_items = type_comp_unit_item_list rest env_with_func in
            agg_ok (TFuncDefItem t_func :: rest_items))
  in
  let result = type_comp_unit_item_list comp_unit StringMap.empty in
  agg_to_result result
