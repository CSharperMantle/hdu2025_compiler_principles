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
  | Assign of string * exp list * exp (* lval, indices, rhs *)
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

let comp_unit_to_string (indent : int) (comp_unit : comp_unit) : string =
  let indented (ind : int) (msg : string) = String.make ind ' ' ^ msg in
  failwith "todo"
