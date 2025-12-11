open Common
open Sem_ast

(* Symbol table entry. *)
type name_entry =
  | VarEntry of {
      id : int;
      ty : sem_type;
    }
  | FunEntry of {
      id : int;
      args : sem_type list;
      ret : b_type;
    }

type symbol_kind =
  | Param
  | Named
  | Temp

type translation_context = {
  names : name_entry StringMap.t;
  var_kinds : symbol_kind IntMap.t;
  next_name_id : int;
  next_label_id : int;
  ir : Tac.tac_instr list;
  functions : Tac.tac_function list;
}

val empty_translation_context : translation_context

val translate :
  Ast.comp_unit ->
  translation_context ->
  (t_comp_unit * translation_context * Tac.tac_program, string list) result
