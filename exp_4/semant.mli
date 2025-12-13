open Sem_ast

type translation_context

val empty_translation_context : translation_context

val translate :
  Ast.comp_unit -> translation_context -> (t_comp_unit * Tac.tac_program, string list) result
