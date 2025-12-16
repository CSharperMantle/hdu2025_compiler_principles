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

type tac_init =
  | InitInt of int
  | InitList of tac_init list

type tac_instr =
  | BinOp of int * Ast.bin_op * operand * operand (* %0 <- %1 %op.i %2 *)
  | FBinOp of int * Ast.bin_op * operand * operand (* %0 <- %1 %op.f %2 *)
  | UnaryOp of int * Ast.unary_op * operand (* %0 <- %op.i %1 *)
  | FUnaryOp of int * Ast.unary_op * operand (* %0 <- %op.f %1 *)
  | Move of int * operand (* %0 <- %1 *)
  | Itf of int * operand (* %0 <- %1.f *)
  | Fti of int * operand (* %0 <- %1.i *)
  | Label of int (* .L%0: *)
  | Jump of int (* jmp .L%0 *)
  | Br of operand * int (* br %0, .L%1 *)
  | Call of int * int * operand list (* %0 <- call %1, ...%2 *)
  | Return of operand option (* ret %0 *)
  | ArrRd of int * int * operand * operand list (* %0 <- %1[%2] aka %1[...%3] *)
  | ArrWr of int * operand * operand * operand list (* %0[%1] aka %1[...%3] <- %2 *)

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
  global_init : tac_init IntMap.t;
  functions : tac_function list;
  objects : tac_obj_type IntMap.t;
}

val prettify_tac_function : tac_function -> string
val prettify_tac_program : tac_program -> string
