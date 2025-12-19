module Const_prop : sig
  val simple_const_prop : Ssa.program -> Ssa.program
end

module Copy_prop : sig
  val copy_prop : Ssa.program -> Ssa.program
end

module Dead_code_elim : sig
  val dead_code_elim : Ssa.program -> Ssa.program
end

val opt_pipe :
  string * Ssa.program ->
  (string * (Ssa.program -> Ssa.program)) list ->
  (string * Ssa.program) list

val default_opt_passes : (string * (Ssa.program -> Ssa.program)) list
