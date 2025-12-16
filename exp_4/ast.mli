type b_type =
  | Int
  | Float

val b_type_to_string : b_type -> string

type unary_op =
  | Pos
  | Neg
  | Not

val prettify_unary_op : unary_op -> string
val string_of_unary_op : unary_op -> string

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

val prettify_bin_op : bin_op -> string
val string_of_bin_op : bin_op -> string

type exp =
  | IntLit of int
  | Var of string * exp list (* (id, array indices) *)
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
  const_dims : exp list; (* When this is [], then it's a scalar. Otherwise it's an array. *)
  const_init : const_init_val;
}

type var_def = {
  var_name : string;
  var_dims : exp list; (* When this is [], then it's a scalar. Otherwise it's an array. *)
  var_init : init_val option;
}

type decl =
  | ConstDecl of b_type * const_def list
  | VarDecl of b_type * var_def list

and stmt =
  | Assign of string * exp list * exp (* (lval, indices, rhs) *)
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

type func_param = {
  param_type : b_type;
  param_name : string;
  param_dims : exp list option; (* When this is None, then it's a scalar. Otherwise it's an array. *)
}

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

val prettify_comp_unit : comp_unit -> string list
