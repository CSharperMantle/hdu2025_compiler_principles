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

type exp_attr = {
  ty : sem_type;
  const_val : int option;
}
