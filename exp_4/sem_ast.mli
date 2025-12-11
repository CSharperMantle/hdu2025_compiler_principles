type b_type =
  | IntType
  | FloatType
  | VoidType

val b_type_from_ast : Ast.b_type -> b_type
val b_type_from_ast_func : Ast.b_type option -> b_type

(* A value type produced by semantics analysis. *)
type sem_type = {
  elem_ty : b_type;
  dims : int list; (* When this is [], then it's a scalar. Otherwise it's an array. *)
}

(* Typed counterpart of Ast.exp. *)
type t_exp =
  | TIntLit of int
  | TVar of int * string * t_exp list * sem_type
  | TUnary of Ast.unary_op * t_exp * sem_type
  | TBinary of Ast.bin_op * t_exp * t_exp * sem_type
  | TCall of int * string * t_exp list * sem_type

(* Typed counterpart of Ast.const_init_val. *)
type t_const_init_val =
  | TConstExp of t_exp
  | TConstArray of t_const_init_val list

(* Typed counterpart of Ast.init_val. *)
type t_init_val =
  | TInitExp of t_exp
  | TInitArray of t_init_val list

(* Typed counterpart of Ast.const_def. *)
type t_const_def = {
  t_const_id : int;
  t_const_name : string;
  t_const_dims : t_exp list;
  t_const_init : t_const_init_val;
}

(* Typed counterpart of Ast.var_def. *)
type t_var_def = {
  t_var_id : int;
  t_var_name : string;
  t_var_dims : t_exp list;
  t_var_init : t_init_val option;
}

(* Typed counterpart of Ast.decl. *)
type t_decl =
  | TConstDecl of b_type * t_const_def list
  | TVarDecl of b_type * t_var_def list

(* Typed counterpart of Ast.stmt. *)
and t_stmt =
  | TAssign of int * string * t_exp list * t_exp
  | TExprStmt of t_exp option
  | TBlock of t_block_item list
  | TIf of t_exp * t_stmt * t_stmt option
  | TWhile of t_exp * t_stmt
  | TBreak
  | TContinue
  | TReturn of t_exp option

(* Typed counterpart of Ast.block_item. *)
and t_block_item =
  | TDecl of t_decl
  | TStmt of t_stmt

(* Typed counterpart of Ast.func_param. *)
type t_func_param = {
  t_param_id : int;
  t_param_type : b_type;
  t_param_name : string;
  t_param_dims : t_exp list option;
}

(* Typed counterpart of Ast.func_def. *)
type t_func_def = {
  t_func_id : int;
  t_func_ret_type : b_type option;
  t_func_name : string;
  t_func_params : t_func_param list;
  t_func_body : t_block_item list;
}

(* Typed counterpart of Ast.comp_unit_item. *)
type t_comp_unit_item =
  | TDeclItem of t_decl
  | TFuncDefItem of t_func_def

(* Typed counterpart of Ast.comp_unit. *)
type t_comp_unit = t_comp_unit_item list

type exp_attr = {
  ty : sem_type;
  const_val : int option;
}

val prettify_t_comp_unit : t_comp_unit -> string list
