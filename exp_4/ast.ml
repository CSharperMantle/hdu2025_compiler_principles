(* Basic types (BType). *)
type b_type =
  | Int
  | Float

let b_type_to_string = function
  | Int -> "int"
  | Float -> "float"

type unary_op =
  | Pos
  | Neg
  | Not

let unary_op_to_string = function
  | Pos -> "+"
  | Neg -> "-"
  | Not -> "!"

type bin_op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Lt
  | Gt
  | Leq
  | Geq
  | Eq
  | Neq
  | And
  | Or

let bin_op_to_string = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Lt -> "<"
  | Gt -> ">"
  | Leq -> "<="
  | Geq -> ">="
  | Eq -> "=="
  | Neq -> "!="
  | And -> "&&"
  | Or -> "||"

type exp =
  | IntLit of int
  | Var of string * exp list
  | Unary of unary_op * exp
  | Binary of bin_op * exp * exp
  | Call of string * exp list

type const_init_val =
  | ConstExp of exp
  | ConstArray of const_init_val list

type init_val =
  | InitExp of exp
  | InitArray of init_val list

type const_def = {
  const_name : string;
  const_dims : exp list;
  const_init : const_init_val;
}

type var_def = {
  var_name : string;
  var_dims : exp list;
  var_init : init_val option;
}

type func_param = {
  param_type : b_type;
  param_name : string;
  param_dims : exp list option;
}

type stmt =
  | Assign of string * exp list * exp
  | ExprStmt of exp option
  | Block of block_item list
  | If of exp * stmt * stmt option
  | While of exp * stmt
  | Break
  | Continue
  | Return of exp option

and block_item =
  | Decl of decl
  | Stmt of stmt

and decl =
  | ConstDecl of b_type * const_def list
  | VarDecl of b_type * var_def list

type func_def = {
  func_ret_type : b_type option;
  func_name : string;
  func_params : func_param list;
  func_body : block_item list;
}

type comp_unit_item =
  | DeclItem of decl
  | FuncDefItem of func_def

type comp_unit = comp_unit_item list

let indent (lines : string list) : string list = List.map (fun l -> "  " ^ l) lines
let prettify_id_name (name : string) : string = Printf.sprintf "Id name=%s" name

let prettify_b_type (node : b_type) : string =
  Printf.sprintf "BType type=%s" (b_type_to_string node)

let rec prettify_exp (node : exp) : string list =
  match node with
  | IntLit n -> [ Printf.sprintf "IntLit val=%d" n ]
  | Var (name, indices) ->
      let indices_lines = List.map prettify_exp indices |> List.flatten in
      "LVal" :: indent (prettify_id_name name :: indices_lines)
  | Unary (op, e) ->
      Printf.sprintf "UnaryExp op='%s'" (unary_op_to_string op) :: indent (prettify_exp e)
  | Binary (op, e1, e2) ->
      Printf.sprintf "BinaryExp op='%s'" (bin_op_to_string op)
      :: indent (prettify_exp e1 @ prettify_exp e2)
  | Call (name, args) ->
      let args_lines = List.map prettify_exp args |> List.flatten in
      "Call" :: indent (prettify_id_name name :: args_lines)

let rec prettify_const_init_val (node : const_init_val) : string list =
  match node with
  | ConstExp e -> "ConstInitVal" :: indent (prettify_exp e)
  | ConstArray vals ->
      let vals_lines = List.map prettify_const_init_val vals |> List.flatten in
      "ConstInitVal" :: indent vals_lines

let prettify_const_def (node : const_def) : string list =
  let dims_lines = List.map prettify_exp node.const_dims |> List.flatten in
  let init_lines = prettify_const_init_val node.const_init in
  "ConstDef" :: indent ((prettify_id_name node.const_name :: dims_lines) @ init_lines)

let prettify_const_decl (t : b_type) (defs : const_def list) : string list =
  let child_lines =
    prettify_b_type t :: (List.map (fun def -> prettify_const_def def) defs |> List.flatten)
  in
  "ConstDecl" :: indent child_lines

let rec prettify_init_val (node : init_val) : string list =
  match node with
  | InitExp e -> "InitVal" :: indent (prettify_exp e)
  | InitArray vals ->
      let vals_lines = List.map prettify_init_val vals |> List.flatten in
      "InitVal" :: indent vals_lines

let prettify_var_def (node : var_def) : string list =
  let dims_lines = List.map prettify_exp node.var_dims |> List.flatten in
  let init_lines =
    match node.var_init with
    | Some init -> prettify_init_val init
    | None -> []
  in
  "VarDef" :: indent ((prettify_id_name node.var_name :: dims_lines) @ init_lines)

let prettify_var_decl (t : b_type) (defs : var_def list) : string list =
  let child_lines =
    prettify_b_type t :: (List.map (fun def -> prettify_var_def def) defs |> List.flatten)
  in
  "VarDecl" :: indent child_lines

let prettify_decl (node : decl) : string list =
  let child_lines =
    match node with
    | ConstDecl (t, defs) -> prettify_const_decl t defs
    | VarDecl (t, defs) -> prettify_var_decl t defs
  in
  "Decl" :: indent child_lines

let prettify_func_param (node : func_param) : string list =
  let dims_lines =
    match node.param_dims with
    | Some dims -> List.map prettify_exp dims |> List.flatten
    | None -> []
  in
  "FuncFParam"
  :: indent (prettify_b_type node.param_type :: prettify_id_name node.param_name :: dims_lines)

let rec prettify_stmt (node : stmt) : string list =
  match node with
  | Assign (name, indices, rhs) ->
      let indices_lines = List.map prettify_exp indices |> List.flatten in
      "Assign"
      :: indent (("LVal" :: indent (prettify_id_name name :: indices_lines)) @ prettify_exp rhs)
  | ExprStmt (Some e) -> "ExprStmt" :: indent (prettify_exp e)
  | ExprStmt None -> [ "ExprStmt" ]
  | Block items -> "Block" :: indent (List.map prettify_block_item items |> List.flatten)
  | If (cond, then_s, else_s) ->
      let else_lines =
        match else_s with
        | Some s -> prettify_stmt s
        | None -> []
      in
      "If" :: indent (prettify_exp cond @ prettify_stmt then_s @ else_lines)
  | While (cond, body) -> "While" :: indent (prettify_exp cond @ prettify_stmt body)
  | Break -> [ "Break" ]
  | Continue -> [ "Continue" ]
  | Return (Some e) -> "Return" :: indent (prettify_exp e)
  | Return None -> [ "Return" ]

and prettify_block_item (node : block_item) : string list =
  match node with
  | Decl d -> prettify_decl d
  | Stmt s -> prettify_stmt s

let prettify_func_def (node : func_def) : string list =
  let ret_type_line =
    match node.func_ret_type with
    | Some t -> prettify_b_type t
    | None -> "Void"
  in
  let params_lines = List.map prettify_func_param node.func_params |> List.flatten in
  let body_lines =
    "Block" :: indent (List.map prettify_block_item node.func_body |> List.flatten)
  in
  "FuncDef"
  :: indent ((ret_type_line :: prettify_id_name node.func_name :: params_lines) @ body_lines)

let prettify_comp_unit_item (node : comp_unit_item) : string list =
  match node with
  | DeclItem child -> prettify_decl child
  | FuncDefItem child -> prettify_func_def child

let prettify_comp_unit (node : comp_unit) : string list =
  let child_lines = List.map (fun child -> prettify_comp_unit_item child) node |> List.flatten in
  "CompUnit" :: indent child_lines
