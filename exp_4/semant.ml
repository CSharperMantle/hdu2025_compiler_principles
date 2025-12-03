open Common
open Result.Syntax

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
      return : b_type;
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

let rec visit_exp (exp : Ast.exp) (env : name_env) : (exp_attr, string list) result =
  let visit_var name indices =
    match StringMap.find_opt name env with
    | None -> Error [ Printf.sprintf "Undefined variable `%s`" name ]
    | Some (FunEntry _) -> Error [ Printf.sprintf "`%s` is a function, not a variable" name ]
    | Some (VarEntry v_ty) ->
        let rec check_indices idxs =
          match idxs with
          | [] -> Ok []
          | i :: rest ->
              let* i_attr = visit_exp i env in
              if i_attr.ty.elem_ty <> IntType || i_attr.ty.dims <> [] then
                Error [ "Array index must be an integer scalar" ]
              else Result.map (fun r -> i_attr :: r) (check_indices rest)
        in
        let* _ = check_indices indices in
        let idx_count = List.length indices and dim_count = List.length v_ty.dims in
        if idx_count > dim_count then Error [ "Too many subscripts for array" ]
        else
          let dims' = List.drop idx_count v_ty.dims in
          Ok { ty = { elem_ty = v_ty.elem_ty; dims = dims' }; const_val = None }
  and visit_unary op sub =
    let* sub_attr = visit_exp sub env in
    match op with
    | Ast.Pos | Ast.Neg ->
        if sub_attr.ty.dims <> [] then Error [ "Unary operator applied to array" ]
        else if sub_attr.ty.elem_ty = VoidType then Error [ "Unary operator applied to void" ]
        else
          let exp_val =
            match (op, sub_attr.const_val) with
            | Ast.Neg, Some v -> Some (-v)
            | Ast.Pos, Some v -> Some v
            | _ -> None
          in
          Ok { ty = sub_attr.ty; const_val = exp_val }
    | Ast.Not ->
        if sub_attr.ty.dims <> [] then Error [ "`!` applied to array" ]
        else if sub_attr.ty.elem_ty = VoidType then Error [ "`!` applied to void" ]
        else
          let exp_val = Option.map (fun v -> if v = 0 then 1 else 0) sub_attr.const_val in
          Ok { ty = int_type; const_val = exp_val }
  and visit_binary op lhs rhs =
    let* l_attr = visit_exp lhs env in
    let* r_attr = visit_exp rhs env in
    if l_attr.ty.dims <> [] || r_attr.ty.dims <> [] then
      Error [ "Binary operator applied to array" ]
    else if l_attr.ty.elem_ty = VoidType || r_attr.ty.elem_ty = VoidType then
      Error [ "Binary operator applied to void" ]
    else
      let res_elem_ty =
        match op with
        | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Mod ->
            if l_attr.ty.elem_ty = FloatType || r_attr.ty.elem_ty = FloatType then FloatType
            else IntType
        | _ -> IntType
      in
      Ok { ty = scalar_type res_elem_ty; const_val = None }
  and visit_call name params =
    match StringMap.find_opt name env with
    | None -> Error [ Printf.sprintf "Undefined function `%s`" name ]
    | Some (VarEntry _) -> Error [ Printf.sprintf "`%s` is a variable, not a function" name ]
    | Some (FunEntry f) ->
        let rec check_args args pms =
          match (args, pms) with
          | [], [] -> Ok ()
          | a :: a_rest, p_ty :: p_rest ->
              let* a_attr = visit_exp a env in
              if a_attr.ty.dims <> p_ty.dims then Error [ "Argument dimension mismatch" ]
              else if
                a_attr.ty.elem_ty <> p_ty.elem_ty
                && not (a_attr.ty.elem_ty = IntType && p_ty.elem_ty = FloatType)
              then Error [ "Argument type mismatch" ]
              else check_args a_rest p_rest
          | _ -> Error [ "Argument count mismatch" ]
        in
        let* _ = check_args params f.args in
        Ok { ty = scalar_type f.return; const_val = None }
  in
  match exp with
  | Ast.IntLit v -> Ok { ty = int_type; const_val = Some v }
  | Ast.Var (name, indices) -> visit_var name indices
  | Ast.Unary (op, sub) -> visit_unary op sub
  | Ast.Binary (op, lhs, rhs) -> visit_binary op lhs rhs
  | Ast.Call (name, params) -> visit_call name params

let typecheck (comp_unit : Ast.comp_unit) : (unit, string list) result =
  let eval_const_dim e env =
    match visit_exp e env with
    | Ok { const_val = Some v; _ } -> Ok v
    | Ok _ -> Error [ "Array dimension must be a constant integer" ]
    | Error e -> Error e
  in
  let rec eval_dims dims acc env =
    match dims with
    | [] -> Ok (List.rev acc)
    | h :: t ->
        let* v = eval_const_dim h env in
        eval_dims t (v :: acc) env
  in
  let rec add_const_defs defs ty env =
    match defs with
    | [] -> Ok env
    | d :: ds ->
        let* dims = eval_dims d.Ast.const_dims [] env in
        let entry = VarEntry { elem_ty = ty; dims } in
        add_const_defs ds ty (StringMap.add d.Ast.const_name entry env)
  and add_var_defs defs ty env =
    match defs with
    | [] -> Ok env
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
          Error [ "Assignment operands must be scalars" ]
        else if lhs_attr.ty.elem_ty = VoidType then Error [ "Cannot assign to void" ]
        else if
          lhs_attr.ty.elem_ty <> rhs_attr.ty.elem_ty
          && not (lhs_attr.ty.elem_ty = FloatType && rhs_attr.ty.elem_ty = IntType)
        then Error [ "Type mismatch in assignment" ]
        else Ok env
    | Ast.ExprStmt (Some e) ->
        let* _ = visit_exp e env in
        Ok env
    | Ast.ExprStmt None -> Ok env
    | Ast.Block items ->
        let* _ = visit_block items ret_ty in_loop env in
        Ok env
    | Ast.If (cond, then_s, else_s) -> (
        let* cond_attr = visit_exp cond env in
        if cond_attr.ty.elem_ty = VoidType || cond_attr.ty.dims <> [] then
          Error [ "Condition must be non-void scalar" ]
        else
          let* _ = visit_stmt then_s ret_ty in_loop env in
          match else_s with
          | Some s -> visit_stmt s ret_ty in_loop env
          | None -> Ok env)
    | Ast.While (cond, body) ->
        let* cond_attr = visit_exp cond env in
        if cond_attr.ty.elem_ty = VoidType || cond_attr.ty.dims <> [] then
          Error [ "Condition must be non-void scalar" ]
        else visit_stmt body ret_ty true env
    | Ast.Break -> if in_loop then Ok env else Error [ "break statement not within loop" ]
    | Ast.Continue -> if in_loop then Ok env else Error [ "continue statement not within loop" ]
    | Ast.Return e_opt -> (
        match (e_opt, ret_ty) with
        | None, VoidType -> Ok env
        | Some _, VoidType -> Error [ "Void function cannot return a value" ]
        | None, _ -> Error [ "Non-void function must return a value" ]
        | Some e, t ->
            let* attr = visit_exp e env in
            if attr.ty.dims <> [] then Error [ "Cannot return array" ]
            else if attr.ty.elem_ty = t then Ok env
            else if t = FloatType && attr.ty.elem_ty = IntType then Ok env
            else Error [ "Return type mismatch" ])
  and visit_block items ret_ty in_loop env =
    match items with
    | [] -> Ok env
    | item :: rest ->
        let* env' =
          match item with
          | Ast.Decl (Ast.VarDecl (t, defs)) -> add_var_defs defs (b_type_from_ast t) env
          | Ast.Decl (Ast.ConstDecl (t, defs)) -> add_const_defs defs (b_type_from_ast t) env
          | Ast.Stmt s ->
              let* _ = visit_stmt s ret_ty in_loop env in
              Ok env
        in
        visit_block rest ret_ty in_loop env'
  in
  let rec visit_comp_unit_item_list items env =
    match items with
    | [] -> Ok env
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
              | [] -> Ok (List.rev acc_args)
              | p :: p_rest ->
                  let ty = b_type_from_ast p.Ast.param_type in
                  let* dims =
                    match p.Ast.param_dims with
                    | None -> Ok []
                    | Some ds ->
                        let* d_vals = eval_dims ds [] env in
                        Ok (0 :: d_vals)
                  in
                  process_params p_rest ((p.Ast.param_name, { elem_ty = ty; dims }) :: acc_args)
            in
            let* named_arg_types = process_params f.Ast.func_params [] in
            let arg_types = List.map snd named_arg_types in
            let entry = FunEntry { args = arg_types; return = ret_ty } in
            let env_with_func = StringMap.add f.Ast.func_name entry env in
            let env_body =
              List.fold_left
                (fun e (n, t) -> StringMap.add n (VarEntry t) e)
                env_with_func named_arg_types
            in
            let* _ = visit_block f.Ast.func_body ret_ty false env_body in
            visit_comp_unit_item_list rest env_with_func)
  in
  let* _ = visit_comp_unit_item_list comp_unit StringMap.empty in
  Ok ()
