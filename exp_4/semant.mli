type b_type =
  | IntType
  | FloatType
  | VoidType

type sem_type = {
  elem_ty : b_type;
  dims : int list; (* When this is [], then it's a scalar. Otherwise it's an array. *)
}

type name_entry =
  | VarEntry of sem_type
  | FunEntry of {
      args : sem_type list;
      return : b_type;
    }

type name_env = name_entry Common.StringMap.t

type exp_attr = {
  ty : sem_type;
  const_val : int option;
}

val typecheck : Ast.comp_unit -> (unit, string list) result
