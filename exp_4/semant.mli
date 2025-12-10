open Sem_ast

(* Symbol table entry. *)
type name_entry =
  | VarEntry of sem_type
  | FunEntry of {
      args : sem_type list;
      ok : b_type;
    }

type name_env = name_entry Common.StringMap.t
type translation_context = { ir : unit list }

val empty_translation_context : translation_context

val translate :
  Ast.comp_unit -> translation_context -> (t_comp_unit * translation_context, string list) result
