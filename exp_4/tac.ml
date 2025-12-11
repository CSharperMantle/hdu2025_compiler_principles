open Common

type operand =
  | Symbol of int
  | Const of int

type tac_instr =
  | BinOp of int * Ast.bin_op * operand * operand
  | UnaryOp of int * Ast.unary_op * operand
  | Move of int * operand
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
}

type tac_program = {
  globals : int list;
  functions : tac_function list;
}

let prettify_operand = function
  | Symbol id -> Printf.sprintf "%%%d" id
  | Const c -> string_of_int c

let prettify_tac_instr = function
  | BinOp (dest, op, src1, src2) ->
      Printf.sprintf "%s <- %s %s %s" (prettify_operand (Symbol dest)) (prettify_operand src1)
        (Ast.prettify_bin_op op) (prettify_operand src2)
      |> indent_single
  | UnaryOp (dest, op, src) ->
      Printf.sprintf "%s <- %s %s" (prettify_operand (Symbol dest)) (Ast.prettify_unary_op op)
        (prettify_operand src)
      |> indent_single
  | Move (dest, src) ->
      Printf.sprintf "%s <- %s" (prettify_operand (Symbol dest)) (prettify_operand src)
      |> indent_single
  | Label l -> Printf.sprintf ".L%d:" l
  | Jump l -> Printf.sprintf "goto .L%d" l |> indent_single
  | CondJump (cond, l) ->
      Printf.sprintf "if %s goto .L%d" (prettify_operand cond) l |> indent_single
  | Call (dest, func_id, args) ->
      let args_str = List.map prettify_operand args |> String.concat ", " in
      Printf.sprintf "%s <- call %%F%d, %s" (prettify_operand (Symbol dest)) func_id args_str
      |> indent_single
  | Return (Some op) -> Printf.sprintf "ret %s" (prettify_operand op) |> indent_single
  | Return None -> "ret" |> indent_single
  | MemRead (dest, base, offset) ->
      Printf.sprintf "%s <- %s[%s]" (prettify_operand (Symbol dest))
        (prettify_operand (Symbol base)) (prettify_operand offset)
      |> indent_single
  | MemWrite (base, offset, src) ->
      Printf.sprintf "%s[%s] <- %s" (prettify_operand (Symbol base)) (prettify_operand offset)
        (prettify_operand src)
      |> indent_single

let prettify_tac_function (f : tac_function) =
  let params_str =
    List.map (fun id -> prettify_operand (Symbol id)) f.func_params |> String.concat ", "
  in
  let body_str = List.map prettify_tac_instr f.func_body |> String.concat "\n" in
  Printf.sprintf "%s%%%d (%s):\n%s" f.func_name f.func_id params_str body_str

let prettify_tac_program (p : tac_program) =
  let globals_str =
    "globals: " ^ (List.map (fun id -> prettify_operand (Symbol id)) p.globals |> String.concat ", ")
  in
  let funcs_str = List.map prettify_tac_function p.functions |> String.concat "\n\n" in
  globals_str ^ "\n\n" ^ funcs_str
