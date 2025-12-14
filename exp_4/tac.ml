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

type tac_init =
  | InitInt of int
  | InitList of tac_init list

let rec prettify_tac_init = function
  | InitInt v -> string_of_int v
  | InitList vs ->
      let content = List.map prettify_tac_init vs |> String.concat ", " in
      Printf.sprintf "{%s}" content

type tac_instr =
  | BinOp of int * Ast.bin_op * operand * operand
  | FBinOp of int * Ast.bin_op * operand * operand
  | UnaryOp of int * Ast.unary_op * operand
  | FUnaryOp of int * Ast.unary_op * operand
  | Mv of int * operand
  | Dtf of int * operand
  | Ftd of int * operand
  | Label of int
  | Jump of int
  | Jc of operand * int
  | Call of int * int * operand list
  | Return of operand option
  | Rd of int * int * operand
  | Wr of int * operand * operand

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
  global_init : tac_init IntMap.t;
  functions : tac_function list;
  objects : tac_obj_type IntMap.t;
}

let prettify_operand = function
  | Object id -> Printf.sprintf "%%%d" id
  | Const c -> Printf.sprintf "%d" c
  | ConstFloat f -> Printf.sprintf "%f" f

let prettify_bin_op = function
  | Ast.Add -> "add"
  | Ast.Sub -> "sub"
  | Ast.Mul -> "mul"
  | Ast.Div -> "div"
  | Ast.Mod -> "mod"
  | Ast.Eq -> "ceq"
  | Ast.Neq -> "cne"
  | Ast.Lt -> "clt"
  | Ast.Leq -> "cle"
  | Ast.Gt -> "cgt"
  | Ast.Geq -> "cge"
  | Ast.And -> "and"
  | Ast.Or -> "or"

let prettify_unary_op = function
  | Ast.Pos -> "pos"
  | Ast.Neg -> "neg"
  | Ast.Not -> "not"

let prettify_tac_instr = function
  | BinOp (dest, op, src1, src2) ->
      Printf.sprintf "%s.d\t%s, %s, %s" (prettify_bin_op op) (prettify_operand (Object dest))
        (prettify_operand src1) (prettify_operand src2)
  | FBinOp (dest, op, src1, src2) ->
      Printf.sprintf "%s.f\t%s, %s, %s" (prettify_bin_op op) (prettify_operand (Object dest))
        (prettify_operand src1) (prettify_operand src2)
  | UnaryOp (dest, op, src) ->
      Printf.sprintf "%s.d\t%s, %s" (prettify_unary_op op) (prettify_operand (Object dest))
        (prettify_operand src)
  | FUnaryOp (dest, op, src) ->
      Printf.sprintf "%s.f\t%s, %s" (prettify_unary_op op) (prettify_operand (Object dest))
        (prettify_operand src)
  | Mv (dest, src) ->
      Printf.sprintf "mv\t%s, %s" (prettify_operand (Object dest)) (prettify_operand src)
  | Dtf (dest, src) ->
      Printf.sprintf "dtf\t%s, %s" (prettify_operand (Object dest)) (prettify_operand src)
  | Ftd (dest, src) ->
      Printf.sprintf "ftd\t%s, %s" (prettify_operand (Object dest)) (prettify_operand src)
  | Label l -> Printf.sprintf ".L%d:" l
  | Jump l -> Printf.sprintf "jmp\t.L%d" l
  | Jc (cond, l) -> Printf.sprintf "jc\t%s, .L%d" (prettify_operand cond) l
  | Call (dest, func_id, args) -> (
      match args with
      | [] -> Printf.sprintf "call\t%s, $%d" (prettify_operand (Object dest)) func_id
      | _ ->
          let args_str = List.map prettify_operand args |> String.concat ", " in
          Printf.sprintf "call\t%s, $%d, %s" (prettify_operand (Object dest)) func_id args_str)
  | Return (Some op) -> Printf.sprintf "ret\t%s" (prettify_operand op)
  | Return None -> "ret"
  | Rd (dest, base, offset) ->
      Printf.sprintf "rd\t%s, %s[%s]" (prettify_operand (Object dest))
        (prettify_operand (Object base)) (prettify_operand offset)
  | Wr (base, offset, src) ->
      Printf.sprintf "wr\t%s, %s[%s]" (prettify_operand src) (prettify_operand (Object base))
        (prettify_operand offset)

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
    List.map
      (fun id ->
        let decl = Printf.sprintf ".global\t%s" (prettify_operand (Object id)) in
        match IntMap.find_opt id p.global_init with
        | Some init -> Printf.sprintf "%s, %s" decl (prettify_tac_init init)
        | None -> decl)
      p.globals
    |> String.concat "\n"
  in
  let funcs_str = List.map prettify_tac_function p.functions |> String.concat "\n\n" in
  globals_str ^ "\n\n" ^ funcs_str
