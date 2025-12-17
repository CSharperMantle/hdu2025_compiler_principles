open Common

type value = int * int

type operand =
  | Value of value
  | Const of int
  | ConstFloat of float

type phi = {
  phi_dest : value;
  phi_incoming : value IntMap.t;
}

type mem_loc =
  | LocalArray of value
  | GlobalScalar of int
  | GlobalArray of int

type instr =
  | BinOp of value * Ast.bin_op * operand * operand
  | FBinOp of value * Ast.bin_op * operand * operand
  | UnaryOp of value * Ast.unary_op * operand
  | FUnaryOp of value * Ast.unary_op * operand
  | Move of value * operand
  | Itf of value * operand
  | Fti of value * operand
  | Call of value * int * operand list
  | Alloca of value * int
  | Load of value * mem_loc * operand list
  | Store of mem_loc * operand list * operand

type terminator =
  | Jump of int
  | Br of operand * int * int
  | Return of operand option

type basic_block = {
  bb_id : int;
  bb_phis : phi list;
  bb_code : instr list;
  bb_term : terminator;
}

type func = {
  func_id : int;
  func_name : string;
  func_params : value list;
  func_entry_block : int;
  func_blocks : basic_block Common.IntMap.t;
  func_ret_type : Tac.tac_elem_type;
  func_obj_types : Tac.tac_obj_type Common.IntMap.t;
}

val prettify_func : func -> string

type proto_block = {
  id : int;
  labels : int list;
  code : Tac.tac_instr list;
  term : Tac.tac_instr option;
}

type program = {
  globals : int list;
  global_init : Tac.tac_init Common.IntMap.t;
  functions : func list;
  objects : Tac.tac_obj_type Common.IntMap.t;
}

val prettify_program : program -> string

type build_ssa_context

val empty_build_ssa_context : build_ssa_context
val build_cfg : Tac.tac_program -> build_ssa_context -> program * build_ssa_context
val build_ssa : Tac.tac_program -> build_ssa_context -> program
