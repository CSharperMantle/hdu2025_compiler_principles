(* Basic types (BType). *)
type b_type =
  | Int
  | Float

type unary_op =
  | Pos
  | Neg
  | Not

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

type func_type =
  | Void
  | IntFunc
  | FloatFunc

type func_def = {
  func_ret_type : func_type;
  func_name : string;
  func_params : func_param list;
  func_body : block_item list;
}

type comp_unit_item =
  | DeclItem of decl
  | FuncDefItem of func_def

type comp_unit = comp_unit_item list

let rec print_ast_indent indent = function
  | [] -> ()
  | item :: rest ->
      print_comp_unit_item indent item;
      print_ast_indent indent rest

and print_comp_unit_item indent = function
  | DeclItem d -> print_decl indent d
  | FuncDefItem f -> print_func_def indent f

and print_decl indent = function
  | ConstDecl (typ, defs) ->
      Printf.printf "%sConstDecl\n" (String.make indent ' ');
      print_b_type (indent + 2) typ;
      List.iter (print_const_def (indent + 2)) defs
  | VarDecl (typ, defs) ->
      Printf.printf "%sVarDecl\n" (String.make indent ' ');
      print_b_type (indent + 2) typ;
      List.iter (print_var_def (indent + 2)) defs

and print_b_type indent = function
  | Int -> Printf.printf "%sInt\n" (String.make indent ' ')
  | Float -> Printf.printf "%sFloat\n" (String.make indent ' ')

and print_const_def indent def =
  Printf.printf "%sConstDef\n" (String.make indent ' ');
  Printf.printf "%s%s\n" (String.make (indent + 2) ' ') def.const_name;
  List.iter (print_exp (indent + 2)) def.const_dims;
  print_const_init_val (indent + 2) def.const_init

and print_const_init_val indent = function
  | ConstExp e -> print_exp indent e
  | ConstArray vals ->
      Printf.printf "%sConstArray\n" (String.make indent ' ');
      List.iter (print_const_init_val (indent + 2)) vals

and print_var_def indent def =
  Printf.printf "%sVarDef\n" (String.make indent ' ');
  Printf.printf "%s%s\n" (String.make (indent + 2) ' ') def.var_name;
  List.iter (print_exp (indent + 2)) def.var_dims;
  match def.var_init with
  | Some init -> print_init_val (indent + 2) init
  | None -> ()

and print_init_val indent = function
  | InitExp e -> print_exp indent e
  | InitArray vals ->
      Printf.printf "%sInitArray\n" (String.make indent ' ');
      List.iter (print_init_val (indent + 2)) vals

and print_func_def indent func =
  Printf.printf "%sFuncDef\n" (String.make indent ' ');
  print_func_type (indent + 2) func.func_ret_type;
  Printf.printf "%s%s\n" (String.make (indent + 2) ' ') func.func_name;
  List.iter (print_func_param (indent + 2)) func.func_params;
  List.iter (print_block_item (indent + 2)) func.func_body

and print_func_type indent = function
  | Void -> Printf.printf "%sVoid\n" (String.make indent ' ')
  | IntFunc -> Printf.printf "%sInt\n" (String.make indent ' ')
  | FloatFunc -> Printf.printf "%sFloat\n" (String.make indent ' ')

and print_func_param indent param =
  Printf.printf "%sFuncParam\n" (String.make indent ' ');
  print_b_type (indent + 2) param.param_type;
  Printf.printf "%s%s\n" (String.make (indent + 2) ' ') param.param_name;
  match param.param_dims with
  | Some dims -> (
    Printf.printf "%sArray\n" (String.make (indent + 2) ' ');
    List.iter (print_exp (indent + 2)) dims;
  )
  | None -> ()

and print_block_item indent = function
  | Decl d -> print_decl indent d
  | Stmt s -> print_stmt indent s

and print_stmt indent = function
  | Assign (name, indices, rhs) ->
      Printf.printf "%sAssign\n" (String.make indent ' ');
      Printf.printf "%s%s\n" (String.make (indent + 2) ' ') name;
      List.iter (print_exp (indent + 2)) indices;
      print_exp (indent + 2) rhs
  | ExprStmt (Some e) -> print_exp indent e
  | ExprStmt None -> Printf.printf "%sEmptyStmt\n" (String.make indent ' ')
  | Block items ->
      Printf.printf "%sBlock\n" (String.make indent ' ');
      List.iter (print_block_item (indent + 2)) items
  | If (cond, then_stmt, else_opt) -> (
      Printf.printf "%sIf\n" (String.make indent ' ');
      print_exp (indent + 2) cond;
      print_stmt (indent + 2) then_stmt;
      match else_opt with
      | Some s -> print_stmt (indent + 2) s
      | None -> ())
  | While (cond, body) ->
      Printf.printf "%sWhile\n" (String.make indent ' ');
      print_exp (indent + 2) cond;
      print_stmt (indent + 2) body
  | Break -> Printf.printf "%sBreak\n" (String.make indent ' ')
  | Continue -> Printf.printf "%sContinue\n" (String.make indent ' ')
  | Return (Some e) ->
      Printf.printf "%sReturn\n" (String.make indent ' ');
      print_exp (indent + 2) e
  | Return None -> Printf.printf "%sReturn\n" (String.make indent ' ')

and print_exp indent = function
  | IntLit n -> Printf.printf "%sIntLit %d\n" (String.make indent ' ') n
  | Var (name, indices) ->
      Printf.printf "%sVar %s\n" (String.make indent ' ') name;
      List.iter (print_exp (indent + 2)) indices
  | Unary (op, e) ->
      Printf.printf "%sUnary %s\n" (String.make indent ' ') (string_of_unary_op op);
      print_exp (indent + 2) e
  | Binary (op, e1, e2) ->
      Printf.printf "%sBinary %s\n" (String.make indent ' ') (string_of_bin_op op);
      print_exp (indent + 2) e1;
      print_exp (indent + 2) e2
  | Call (name, args) ->
      Printf.printf "%sCall %s\n" (String.make indent ' ') name;
      List.iter (print_exp (indent + 2)) args

and string_of_unary_op = function
  | Pos -> "+"
  | Neg -> "-"
  | Not -> "!"

and string_of_bin_op = function
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
