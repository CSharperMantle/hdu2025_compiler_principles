let current_version = 1

type mir_instruction = {
  ptr: int;
  id: int;
  opcode: string;
  attributes: string list;
  inputs: int list;
  uses: int list;
  memInputs: Yojson.Safe.t list; (* TODO: refine type *)
  type_: string; [@key "type"]
} [@@deriving yojson]

type mir_block = {
  ptr: int;
  id: int;
  loopDepth: int;
  attributes: string list;
  predecessors: int list;
  successors: int list;
  instructions: mir_instruction list;
} [@@deriving yojson]

type lir_instruction = {
  ptr: int;
  id: int;
  mirPtr: int option; [@key "mirPtr"]
  opcode: string;
  defs: int list;
} [@@deriving yojson]

type lir_block = {
  ptr: int;
  id: int;
  instructions: lir_instruction list;
} [@@deriving yojson]

type mir_data = {
  blocks: mir_block list;
} [@@deriving yojson]

type lir_data = {
  blocks: lir_block list;
} [@@deriving yojson]

type pass = {
  name: string;
  mir: mir_data;
  lir: lir_data;
} [@@deriving yojson]

type func = {
  name: string;
  passes: pass list;
} [@@deriving yojson]

type ion_json = {
  version: int;
  functions: func list;
} [@@deriving yojson]