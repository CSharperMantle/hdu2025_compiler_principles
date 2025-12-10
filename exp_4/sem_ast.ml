open Common

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
  | TVar of int * string * t_exp list * sem_type
  | TUnary of Ast.unary_op * t_exp * sem_type
  | TBinary of Ast.bin_op * t_exp * t_exp * sem_type
  | TCall of int * string * t_exp list * sem_type

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

type exp_attr = {
  ty : sem_type;
  const_val : int option;
}

let prettify_id_name ((id, name) : int option * string) : string =
  match id with
  | Some id -> Printf.sprintf "Id %d (%s)" id name
  | None -> Printf.sprintf "Id ? (%s)" name

let b_type_to_string = function
  | IntType -> "int"
  | FloatType -> "float"
  | VoidType -> "void"

let prettify_b_type (node : b_type) : string =
  Printf.sprintf "BType type=%s" (b_type_to_string node)

let prettify_sem_type (ty : sem_type) : string =
  let dims_str =
    if ty.dims = [] then ""
    else
      let dims = List.map string_of_int ty.dims |> String.concat "," in
      Printf.sprintf "[%s]" dims
  in
  Printf.sprintf "sem_type=%s%s" (b_type_to_string ty.elem_ty) dims_str

let rec prettify_t_exp (node : t_exp) : string list =
  match node with
  | TIntLit n -> [ Printf.sprintf "TIntLit val=%d" n ]
  | TVar (id, name, indices, ty) ->
      let indices_lines = List.map prettify_t_exp indices |> List.flatten in
      Printf.sprintf "TLVal %s" (prettify_sem_type ty)
      :: (prettify_id_name (Some id, name) :: indices_lines |> indent)
  | TUnary (op, e, ty) ->
      Printf.sprintf "TUnaryExp op='%s' %s" (Ast.unary_op_to_string op) (prettify_sem_type ty)
      :: (prettify_t_exp e |> indent)
  | TBinary (op, e1, e2, ty) ->
      Printf.sprintf "TBinaryExp op='%s' %s" (Ast.bin_op_to_string op) (prettify_sem_type ty)
      :: (prettify_t_exp e1 @ prettify_t_exp e2 |> indent)
  | TCall (id, name, args, ty) ->
      let args_lines = List.map prettify_t_exp args |> List.flatten in
      Printf.sprintf "TCall %s" (prettify_sem_type ty)
      :: (prettify_id_name (Some id, name) :: args_lines |> indent)

let rec prettify_t_const_init_val (node : t_const_init_val) : string list =
  match node with
  | TConstExp e -> "TConstInitVal" :: indent (prettify_t_exp e)
  | TConstArray vals ->
      let vals_lines = List.map prettify_t_const_init_val vals |> List.flatten in
      "TConstInitVal" :: indent vals_lines

let prettify_t_const_def (node : t_const_def) : string list =
  let dims_lines = List.map prettify_t_exp node.t_const_dims |> List.flatten in
  let init_lines = prettify_t_const_init_val node.t_const_init in
  "TConstDef" :: ((prettify_id_name (None, node.t_const_name) :: dims_lines) @ init_lines |> indent)

let prettify_t_const_decl (t : b_type) (defs : t_const_def list) : string list =
  let child_lines =
    prettify_b_type t :: (List.map (fun def -> prettify_t_const_def def) defs |> List.flatten)
  in
  "TConstDecl" :: indent child_lines

let rec prettify_t_init_val (node : t_init_val) : string list =
  match node with
  | TInitExp e -> "TInitVal" :: (prettify_t_exp e |> indent)
  | TInitArray vals ->
      let vals_lines = List.map prettify_t_init_val vals |> List.flatten in
      "TInitVal" :: indent vals_lines

let prettify_t_var_def (node : t_var_def) : string list =
  let dims_lines = List.map prettify_t_exp node.t_var_dims |> List.flatten in
  let init_lines =
    match node.t_var_init with
    | Some init -> prettify_t_init_val init
    | None -> []
  in
  "TVarDef" :: ((prettify_id_name (None, node.t_var_name) :: dims_lines) @ init_lines |> indent)

let prettify_t_var_decl (t : b_type) (defs : t_var_def list) : string list =
  let child_lines =
    prettify_b_type t :: (List.map (fun def -> prettify_t_var_def def) defs |> List.flatten)
  in
  "TVarDecl" :: indent child_lines

let prettify_t_decl (node : t_decl) : string list =
  let child_lines =
    match node with
    | TConstDecl (t, defs) -> prettify_t_const_decl t defs
    | TVarDecl (t, defs) -> prettify_t_var_decl t defs
  in
  "TDecl" :: indent child_lines

let prettify_t_func_param (node : t_func_param) : string list =
  let dims_lines =
    match node.t_param_dims with
    | Some dims -> List.map prettify_t_exp dims |> List.flatten
    | None -> []
  in
  "TFuncFParam"
  :: (prettify_b_type node.t_param_type :: prettify_id_name (None, node.t_param_name) :: dims_lines
     |> indent)

let rec prettify_t_stmt (node : t_stmt) : string list =
  match node with
  | TAssign (name, indices, rhs) ->
      let indices_lines = List.map prettify_t_exp indices |> List.flatten in
      "TAssign"
      :: (("TLVal" :: indent (prettify_id_name (None, name) :: indices_lines)) @ prettify_t_exp rhs
         |> indent)
  | TExprStmt (Some e) -> "TExprStmt" :: (prettify_t_exp e |> indent)
  | TExprStmt None -> [ "TExprStmt" ]
  | TBlock items -> "TBlock" :: (List.map prettify_t_block_item items |> List.flatten |> indent)
  | TIf (cond, then_s, else_s) ->
      let else_lines =
        match else_s with
        | Some s -> prettify_t_stmt s
        | None -> []
      in
      "TIf" :: (prettify_t_exp cond @ prettify_t_stmt then_s @ else_lines |> indent)
  | TWhile (cond, body) -> "TWhile" :: (prettify_t_exp cond @ prettify_t_stmt body |> indent)
  | TBreak -> [ "TBreak" ]
  | TContinue -> [ "TContinue" ]
  | TReturn (Some e) -> "TReturn" :: (prettify_t_exp e |> indent)
  | TReturn None -> [ "TReturn" ]

and prettify_t_block_item (node : t_block_item) : string list =
  match node with
  | TDecl d -> prettify_t_decl d
  | TStmt s -> prettify_t_stmt s

let prettify_t_func_def (node : t_func_def) : string list =
  let ret_type_line =
    match node.t_func_ret_type with
    | Some t -> prettify_b_type t
    | None -> prettify_b_type VoidType
  in
  let params_lines = List.map prettify_t_func_param node.t_func_params |> List.flatten in
  let body_lines =
    "TBlock" :: (List.map prettify_t_block_item node.t_func_body |> List.flatten |> indent)
  in
  "TFuncDef"
  :: ((ret_type_line :: prettify_id_name (None, node.t_func_name) :: params_lines) @ body_lines
     |> indent)

let prettify_t_comp_unit_item (node : t_comp_unit_item) : string list =
  match node with
  | TDeclItem child -> prettify_t_decl child
  | TFuncDefItem child -> prettify_t_func_def child

let prettify_t_comp_unit (node : t_comp_unit) : string list =
  let child_lines = List.map (fun child -> prettify_t_comp_unit_item child) node |> List.flatten in
  "TCompUnit" :: indent child_lines
