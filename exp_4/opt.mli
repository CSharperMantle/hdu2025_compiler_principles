type opt_pass = Ssa.program -> Ssa.program * bool

module Const_prop : sig
  val simple_const_prop : opt_pass
end

module Copy_prop : sig
  val copy_prop : opt_pass
end

module Dead_code_elim : sig
  val dead_code_elim : opt_pass
end

type opt_pipe_stage = string * opt_pass

type opt_pipe_component =
  | Once of opt_pipe_stage
  | Fixedpoint of opt_pipe_stage list

val opt_pipe : string * Ssa.program -> opt_pipe_component list -> (string * Ssa.program) list
val default_opt_passes : opt_pipe_component list
