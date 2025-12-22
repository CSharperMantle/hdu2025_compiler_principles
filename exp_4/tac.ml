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

type mem_loc =
  | LocalScalar of int
  | LocalArray of int
  | GlobalScalar of int
  | GlobalArray of int

let prettify_mem_loc = function
  | LocalScalar id | LocalArray id -> Printf.sprintf "%%%d" id
  | GlobalScalar id | GlobalArray id -> Printf.sprintf "@%d" id

type tac_obj_type = {
  elem_ty : tac_elem_type;
  is_array : bool;
}

let prettify_tac_obj_type (ty : tac_obj_type) : string =
  Printf.sprintf "%s%s" (string_of_tac_elem_type ty.elem_ty) (if ty.is_array then "[]" else "")

type tac_init =
  | InitInt of int
  | InitFloat of float
  | InitList of tac_init list

let rec prettify_tac_init = function
  | InitInt v -> string_of_int v
  | InitFloat v -> string_of_float v
  | InitList vs ->
      let content = List.map prettify_tac_init vs |> String.concat ", " in
      Printf.sprintf "{%s}" content

type tac_instr =
  | BinOp of int * Ast.bin_op * operand * operand
  | FBinOp of int * Ast.bin_op * operand * operand
  | UnaryOp of int * Ast.unary_op * operand
  | FUnaryOp of int * Ast.unary_op * operand
  | Move of int * operand
  | Itf of int * operand
  | Fti of int * operand
  | Label of int
  | Jump of int
  | Br of operand * int
  | Call of int * int * operand list
  | Return of operand option
  | Alloca of int * int
  | Load of int * mem_loc * operand * operand list
  | Store of mem_loc * operand * operand * operand list

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

let prettify_tac_instr = function
  | BinOp (dest, op, src1, src2) ->
      Printf.sprintf "%s.i\t(%s, %s, %s)" (Ast.string_of_bin_op op) (prettify_operand (Object dest))
        (prettify_operand src1) (prettify_operand src2)
  | FBinOp (dest, op, src1, src2) ->
      Printf.sprintf "%s.f\t(%s, %s, %s)" (Ast.string_of_bin_op op) (prettify_operand (Object dest))
        (prettify_operand src1) (prettify_operand src2)
  | UnaryOp (dest, op, src) ->
      Printf.sprintf "%s.i\t(%s, %s)" (Ast.string_of_unary_op op) (prettify_operand (Object dest))
        (prettify_operand src)
  | FUnaryOp (dest, op, src) ->
      Printf.sprintf "%s.f\t(%s, %s)" (Ast.string_of_unary_op op) (prettify_operand (Object dest))
        (prettify_operand src)
  | Move (dest, src) ->
      Printf.sprintf "Move\t(%s, %s)" (prettify_operand (Object dest)) (prettify_operand src)
  | Itf (dest, src) ->
      Printf.sprintf "Itf\t(%s, %s)" (prettify_operand (Object dest)) (prettify_operand src)
  | Fti (dest, src) ->
      Printf.sprintf "Fti\t(%s, %s)" (prettify_operand (Object dest)) (prettify_operand src)
  | Label l -> Printf.sprintf ".L%d:" l
  | Jump l -> Printf.sprintf "Jump\t(.L%d)" l
  | Br (cond, l) -> Printf.sprintf "Br\t(%s, .L%d)" (prettify_operand cond) l
  | Call (dest, func_id, args) -> (
      match args with
      | [] -> Printf.sprintf "Call\t(%s, $%d)" (prettify_operand (Object dest)) func_id
      | _ ->
          let args_str = List.map prettify_operand args |> String.concat ", " in
          Printf.sprintf "Call\t(%s, $%d, [%s])" (prettify_operand (Object dest)) func_id args_str)
  | Return (Some op) -> Printf.sprintf "Ret\t(%s)" (prettify_operand op)
  | Return None -> "Ret\t()"
  | Alloca (dest, count) -> Printf.sprintf "Alloca\t(%s, %d)" (prettify_operand (Object dest)) count
  | Load (dest, mem, offset, indices) ->
      let mem_str = prettify_mem_loc mem in
      let idx_str = List.map prettify_operand indices |> String.concat "][" in
      if indices = [] then
        Printf.sprintf "Load\t(%s, %s, %s)" (prettify_operand (Object dest)) mem_str
          (prettify_operand offset)
      else
        Printf.sprintf "Load\t(%s, %s, %s)\t[%s]" (prettify_operand (Object dest)) mem_str
          (prettify_operand offset) idx_str
  | Store (mem, offset, src, indices) ->
      let mem_str = prettify_mem_loc mem in
      let idx_str = List.map prettify_operand indices |> String.concat "][" in
      if indices = [] then Printf.sprintf "Store\t(%s, %s)" mem_str (prettify_operand src)
      else
        Printf.sprintf "Store\t(%s, %s, %s)\t[%s]" mem_str (prettify_operand offset)
          (prettify_operand src) idx_str

let prettify_obj_type (id : int) (ty : tac_obj_type) =
  Printf.sprintf "%s: %s" (prettify_operand (Object id)) (prettify_tac_obj_type ty)

let prettify_tac_function (f : tac_function) : string =
  let params_str =
    List.map (fun id -> prettify_operand (Object id)) f.func_params |> String.concat ", "
  and obj_types_str =
    IntMap.mapi (fun id ty -> prettify_obj_type id ty) f.func_obj_types
    |> IntMap.to_seq |> Seq.map snd |> indent_seq |> List.of_seq |> String.concat "\n"
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
        let decl = Printf.sprintf ".global\t@%d" id in
        match IntMap.find_opt id p.global_init with
        | Some init -> Printf.sprintf "%s, %s" decl (prettify_tac_init init)
        | None -> decl)
      p.globals
    |> String.concat "\n"
  in
  let funcs_str = List.map prettify_tac_function p.functions |> String.concat "\n\n" in
  globals_str ^ "\n\n" ^ funcs_str
