open Sem_ast

val eval_unary_op : const -> Ast.unary_op -> const
val eval_binary_op : const -> const -> Ast.bin_op -> const option

type translation_context

val empty_translation_context : translation_context

val translate :
  Ast.comp_unit -> translation_context -> (t_comp_unit * Tac.tac_program, string list) result
