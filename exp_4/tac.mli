open Common

type operand =
  | Object of int
  | Const of int
  | ConstFloat of float

type tac_elem_type =
  | Int
  | Float
  | Void

type tac_obj_type = {
  elem_ty : tac_elem_type;
  is_array : bool;
}

type tac_instr =
  | BinOp of int * Ast.bin_op * operand * operand (* %0 <- %1 %op.d %2 *)
  | FBinOp of int * Ast.bin_op * operand * operand (* %0 <- %1 %op.f %2 *)
  | UnaryOp of int * Ast.unary_op * operand (* %0 <- %op.d %1 *)
  | FUnaryOp of int * Ast.unary_op * operand (* %0 <- %op.f %1 *)
  | Mv of int * operand (* %0 <- %1 *)
  | Dtf of int * operand (* %0 <- %1.f *)
  | Ftd of int * operand (* %0 <- %1.d *)
  | Label of int (* .L%0: *)
  | Jump of int (* jmp .L%0 *)
  | Jc of operand * int (* jc %0, .L%1 *)
  | Call of int * int * operand list (* %0 <- call %1, ...%2 *)
  | Return of operand option (* ret %0 *)
  | Rd of int * int * operand (* %0 <- %1[%2] *)
  | Wr of int * operand * operand (* %0[%1] <- %2 *)

type tac_function = {
  func_id : int;
  func_name : string;
  func_params : int list;
  func_body : tac_instr list;
  func_ret_type : tac_elem_type;
  func_obj_types : tac_obj_type IntMap.t;
}

type tac_program = {
  globals : int list;
  functions : tac_function list;
  objects : tac_obj_type IntMap.t;
}

val prettify_tac_function : tac_function -> string
val prettify_tac_program : tac_program -> string
