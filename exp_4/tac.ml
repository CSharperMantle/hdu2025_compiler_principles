open Common

type operand =
  | Object of int
  | Const of int
  | ConstFloat of float

type tac_elem_type =
  | Int
  | Float
  | Void

let string_of_tac_elem_type = function
  | Int -> "int"
  | Float -> "float"
  | Void -> "void"

type tac_obj_type = {
  elem_ty : tac_elem_type;
  is_array : bool;
}

let prettify_tac_obj_type (ty : tac_obj_type) : string =
  Printf.sprintf "%s%s" (string_of_tac_elem_type ty.elem_ty) (if ty.is_array then "[]" else "")

type tac_instr =
  | BinOp of int * Ast.bin_op * operand * operand
  | FBinOp of int * Ast.bin_op * operand * operand
  | UnaryOp of int * Ast.unary_op * operand
  | FUnaryOp of int * Ast.unary_op * operand
  | Move of int * operand
  | IntToFloat of int * operand
  | FloatToInt of int * operand
  | Label of int
  | Jump of int
  | CondJump of operand * int
  | Call of int * int * operand list
  | Return of operand option
  | MemRead of int * int * operand
  | MemWrite of int * operand * operand

type tac_function = {
  func_id : int;
  func_name : string;
  func_params : int list;
  func_body : tac_instr list;
  func_ret_type : tac_elem_type;
  func_obj_types : tac_obj_type IntMap.t;
}

type tac_program = {
  globals : int list;
  functions : tac_function list;
  objects : tac_obj_type IntMap.t;
}

let prettify_operand = function
  | Object id -> Printf.sprintf "%%%d" id
  | Const c -> string_of_int c
  | ConstFloat f -> string_of_float f

let prettify_tac_instr = function
  | BinOp (dest, op, src1, src2) ->
      Printf.sprintf "%s <- %s %s.d %s" (prettify_operand (Object dest)) (prettify_operand src1)
        (Ast.prettify_bin_op op) (prettify_operand src2)
  | FBinOp (dest, op, src1, src2) ->
      Printf.sprintf "%s <- %s %s.f %s" (prettify_operand (Object dest)) (prettify_operand src1)
        (Ast.prettify_bin_op op) (prettify_operand src2)
  | UnaryOp (dest, op, src) ->
      Printf.sprintf "%s <- %s.d %s" (prettify_operand (Object dest)) (Ast.prettify_unary_op op)
        (prettify_operand src)
  | FUnaryOp (dest, op, src) ->
      Printf.sprintf "%s <- %s.f %s" (prettify_operand (Object dest)) (Ast.prettify_unary_op op)
        (prettify_operand src)
  | Move (dest, src) ->
      Printf.sprintf "%s <- %s" (prettify_operand (Object dest)) (prettify_operand src)
  | IntToFloat (dest, src) ->
      Printf.sprintf "%s <- %s.f" (prettify_operand (Object dest)) (prettify_operand src)
  | FloatToInt (dest, src) ->
      Printf.sprintf "%s <- %s.d" (prettify_operand (Object dest)) (prettify_operand src)
  | Label l -> Printf.sprintf ".L%d:" l
  | Jump l -> Printf.sprintf "jmp .L%d" l
  | CondJump (cond, l) -> Printf.sprintf "jc %s, .L%d" (prettify_operand cond) l
  | Call (dest, func_id, args) -> (
      match args with
      | [] -> Printf.sprintf "%s <- call $%d" (prettify_operand (Object dest)) func_id
      | _ ->
          let args_str = List.map prettify_operand args |> String.concat ", " in
          Printf.sprintf "%s <- call $%d, %s" (prettify_operand (Object dest)) func_id args_str)
  | Return (Some op) -> Printf.sprintf "ret %s" (prettify_operand op)
  | Return None -> "ret"
  | MemRead (dest, base, offset) ->
      Printf.sprintf "%s <- %s[%s]" (prettify_operand (Object dest))
        (prettify_operand (Object base)) (prettify_operand offset)
  | MemWrite (base, offset, src) ->
      Printf.sprintf "%s[%s] <- %s" (prettify_operand (Object base)) (prettify_operand offset)
        (prettify_operand src)

let prettify_obj_type (id : int) (ty : tac_obj_type) =
  Printf.sprintf "%s: %s" (prettify_operand (Object id)) (prettify_tac_obj_type ty)

let prettify_tac_function (f : tac_function) : string =
  let params_str =
    List.map (fun id -> prettify_operand (Object id)) f.func_params |> String.concat ", "
  and obj_types_str =
    IntMap.mapi (fun id ty -> prettify_obj_type id ty) f.func_obj_types
    |> IntMap.to_list |> List.map snd |> indent |> String.concat "\n"
  and body_str =
    List.map
      (fun instr ->
        let s = prettify_tac_instr instr in
        match instr with
        | Label _ -> s
        | _ -> indent_single s)
      f.func_body
    |> String.concat "\n"
  and ret_type_str = string_of_tac_elem_type f.func_ret_type in
  Printf.sprintf "%s$%d(%s) -> %s:\n%s\n\n%s" f.func_name f.func_id params_str ret_type_str
    obj_types_str body_str

let prettify_tac_program (p : tac_program) : string =
  let globals_str =
    List.map (fun id -> Printf.sprintf ".global %s" (prettify_operand (Object id))) p.globals
    |> String.concat "\n"
  in
  let funcs_str = List.map prettify_tac_function p.functions |> String.concat "\n\n" in
  globals_str ^ "\n\n" ^ funcs_str
