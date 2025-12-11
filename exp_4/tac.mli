type operand =
  | Symbol of int
  | Const of int

type tac_instr =
  | BinOp of int * Ast.bin_op * operand * operand (* %0 <- %1 %op %2 *)
  | UnaryOp of int * Ast.unary_op * operand (* %0 <- %op %1 *)
  | Move of int * operand (* %0 <- %1 *)
  | Label of int (* %L0: *)
  | Jump of int (* goto %L0 *)
  | CondJump of operand * int (* if %0 goto %L1 *)
  | Call of int * int * operand list (* %0 <- call %1, ...%2 *)
  | Return of operand option (* ret %0 *)
  | MemRead of int * int * operand (* %0 <- %1[%2] *)
  | MemWrite of int * operand * operand (* %0[%1] <- %2 *)

type tac_function = {
  func_id : int;
  func_name : string;
  func_params : int list;
  func_body : tac_instr list;
}

type tac_program = {
  globals : int list;
  functions : tac_function list;
}

val prettify_tac_function : tac_function -> string
val prettify_tac_program : tac_program -> string
