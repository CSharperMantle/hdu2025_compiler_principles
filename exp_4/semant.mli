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

type translation_context = {
  names : name_entry Common.StringMap.t;
  next_name_id : int;
  next_label_id : int;
  (* TODO: Hole for codegen *)
  ir : unit list;
}

val empty_translation_context : translation_context

val translate :
  Ast.comp_unit -> translation_context -> (t_comp_unit * translation_context, string list) result
