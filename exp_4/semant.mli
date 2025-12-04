open Typed_ast

(* Symbol table entry. *)
type name_entry =
  | VarEntry of sem_type
  | FunEntry of {
      args : sem_type list;
      ok : b_type;
    }

type name_env = name_entry Common.StringMap.t

val type_comp_unit : Ast.comp_unit -> (t_comp_unit, string list) result
