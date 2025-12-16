open Common

let prettify_basic_block_id (v : int) : string = Printf.sprintf ".BB%%%d" v

type value = int * int (* (base id, version) *)

let new_value (id : int) : value = (id, 0)
let prettify_value (v : value) : string = Printf.sprintf "%%%d/%d" (fst v) (snd v)

type operand =
  | Value of value
  | Const of int
  | ConstFloat of float

let operand_of_tac_operand = function
  | Tac.Object id -> Value (new_value id)
  | Tac.Const c -> Const c
  | Tac.ConstFloat f -> ConstFloat f

let prettify_operand = function
  | Value v -> prettify_value v
  | Const v -> string_of_int v
  | ConstFloat v -> string_of_float v

type phi = {
  phi_dest : value;
  phi_incoming : value IntMap.t; (* Mapping from BB id to incoming value *)
}

let prettify_phi (v : phi) : string =
  let incoming_str =
    IntMap.bindings v.phi_incoming
    |> string_of_list (fun (bb, v) ->
        Printf.sprintf "%s (%s)" (prettify_value v) (prettify_basic_block_id bb))
  in
  Printf.sprintf "%s <- Phi %s" (prettify_value v.phi_dest) incoming_str

type instr =
  | BinOp of value * Ast.bin_op * operand * operand
  | FBinOp of value * Ast.bin_op * operand * operand
  | UnaryOp of value * Ast.unary_op * operand
  | FUnaryOp of value * Ast.unary_op * operand
  | Move of value * operand
  | Itf of value * operand
  | Fti of value * operand
  | Call of value * int * operand list
  | ArrRd of value * value * operand list
  | ArrWr of value * operand list * operand

let instr_of_tac_instr_opt = function
  | Tac.BinOp (d, op, s1, s2) ->
      Some (BinOp (new_value d, op, operand_of_tac_operand s1, operand_of_tac_operand s2))
  | Tac.FBinOp (d, op, s1, s2) ->
      Some (FBinOp (new_value d, op, operand_of_tac_operand s1, operand_of_tac_operand s2))
  | Tac.UnaryOp (d, op, s) -> Some (UnaryOp (new_value d, op, operand_of_tac_operand s))
  | Tac.FUnaryOp (d, op, s) -> Some (FUnaryOp (new_value d, op, operand_of_tac_operand s))
  | Tac.Move (d, s) -> Some (Move (new_value d, operand_of_tac_operand s))
  | Tac.Itf (d, s) -> Some (Itf (new_value d, operand_of_tac_operand s))
  | Tac.Fti (d, s) -> Some (Fti (new_value d, operand_of_tac_operand s))
  | Tac.Call (d, f, args) -> Some (Call (new_value d, f, List.map operand_of_tac_operand args))
  | Tac.ArrRd (d, b, _, indices) ->
      Some (ArrRd (new_value d, new_value b, List.map operand_of_tac_operand indices))
  | Tac.ArrWr (b, _, s, indices) ->
      Some (ArrWr (new_value b, List.map operand_of_tac_operand indices, operand_of_tac_operand s))
  | _ -> None

let prettify_instr = function
  | BinOp (dest, op, src1, src2) ->
      Printf.sprintf "%s.i\t(%s, %s, %s)" (Ast.string_of_bin_op op) (prettify_value dest)
        (prettify_operand src1) (prettify_operand src2)
  | FBinOp (dest, op, src1, src2) ->
      Printf.sprintf "%s.f\t(%s, %s, %s)" (Ast.string_of_bin_op op) (prettify_value dest)
        (prettify_operand src1) (prettify_operand src2)
  | UnaryOp (dest, op, src) ->
      Printf.sprintf "%s.i\t(%s, %s)" (Ast.string_of_unary_op op) (prettify_value dest)
        (prettify_operand src)
  | FUnaryOp (dest, op, src) ->
      Printf.sprintf "%s.f\t(%s, %s)" (Ast.string_of_unary_op op) (prettify_value dest)
        (prettify_operand src)
  | Move (dest, src) -> Printf.sprintf "Move\t(%s, %s)" (prettify_value dest) (prettify_operand src)
  | Itf (dest, src) -> Printf.sprintf "Itf\t(%s, %s)" (prettify_value dest) (prettify_operand src)
  | Fti (dest, src) -> Printf.sprintf "Fti\t(%s, %s)" (prettify_value dest) (prettify_operand src)
  | Call (dest, func_id, args) ->
      let args_str = List.map prettify_operand args |> String.concat ", " in
      if args = [] then Printf.sprintf "Call\t(%s, $%d)" (prettify_value dest) func_id
      else Printf.sprintf "Call\t(%s, $%d, [%s])" (prettify_value dest) func_id args_str
  | ArrRd (dest, base, indices) ->
      let indices_str = List.map prettify_operand indices |> String.concat "][" in
      Printf.sprintf "ArrRd\t(%s, %s, [%s])" (prettify_value dest) (prettify_value base) indices_str
  | ArrWr (base, indices, src) ->
      let indices_str = List.map prettify_operand indices |> String.concat "][" in
      Printf.sprintf "ArrWr\t(%s, [%s], %s)" (prettify_value base) indices_str
        (prettify_operand src)

type terminator =
  | Jump of int (* Jump to BB %0 *)
  | Br of operand * int * int (* If %0 is truthy, then go to BB %1, otherwise BB %2 *)
  | Return of operand option

let prettify_terminator = function
  | Jump bb -> Printf.sprintf "Jump\t(%s)" (prettify_basic_block_id bb)
  | Br (cond, bb_truthy, bb_falsy) ->
      Printf.sprintf "Br\t(%s, %s, %s)" (prettify_operand cond)
        (prettify_basic_block_id bb_truthy)
        (prettify_basic_block_id bb_falsy)
  | Return retval -> Printf.sprintf "Return\t(%s)" (map_or prettify_operand "" retval)

type basic_block = {
  bb_id : int;
  bb_phis : phi list;
  bb_code : instr list;
  bb_term : terminator;
}

let prettify_basic_block (v : basic_block) : string =
  let phis_str =
    List.map prettify_phi v.bb_phis |> indent |> String.concat "\n" |> maybe_add_newline
  in
  let code_str =
    List.map prettify_instr v.bb_code |> indent |> String.concat "\n" |> maybe_add_newline
  in
  Printf.sprintf "%s:\n%s>%s<%s" (prettify_basic_block_id v.bb_id) phis_str code_str
    (prettify_terminator v.bb_term |> indent_single)

type func = {
  func_id : int;
  func_name : string;
  func_params : value list;
  func_entry_block : int;
  func_blocks : basic_block IntMap.t;
  func_ret_type : Tac.tac_elem_type;
  func_obj_types : Tac.tac_obj_type IntMap.t;
}

let prettify_func (f : func) : string =
  let params_str = List.map prettify_value f.func_params |> String.concat ", " in
  let prettify_obj_type id (ty : Tac.tac_obj_type) =
    Printf.sprintf "%%%d: %s%s" id
      (Tac.string_of_tac_elem_type ty.elem_ty)
      (if ty.is_array then "[]" else "")
  in
  let obj_types_str =
    IntMap.mapi prettify_obj_type f.func_obj_types
    |> IntMap.bindings |> List.map snd |> indent |> String.concat "\n" |> maybe_add_newline
  in
  let blocks_str =
    IntMap.bindings f.func_blocks
    |> List.map (fun (_, bb) -> prettify_basic_block bb)
    |> String.concat "\n\n"
  in
  Printf.sprintf "%s$%d(%s) -> %s @ %s:\n%s\n%s" f.func_name f.func_id params_str
    (Tac.string_of_tac_elem_type f.func_ret_type)
    (prettify_basic_block_id f.func_entry_block)
    obj_types_str blocks_str

(* Proto blocks are precursor of regular basic blocks. *)
type proto_block = {
  id : int;
  labels : int list;
  code : Tac.tac_instr list;
  term : Tac.tac_instr option;
}

type program = {
  globals : int list;
  global_init : Tac.tac_init IntMap.t;
  functions : func list;
  objects : Tac.tac_obj_type IntMap.t;
}

let prettify_program (p : program) : string =
  let globals_str =
    List.map
      (fun id ->
        let decl = Printf.sprintf ".global\t%%%d" id in
        match IntMap.find_opt id p.global_init with
        | Some init -> Printf.sprintf "%s, %s" decl (Tac.prettify_tac_init init)
        | None -> decl)
      p.globals
    |> String.concat "\n"
  in
  let funcs_str = List.map prettify_func p.functions |> String.concat "\n\n" in
  globals_str ^ "\n\n" ^ funcs_str

type build_ssa_context = { next_bb_id : int }

let alloc_bb_id (ctx : build_ssa_context) : int * build_ssa_context =
  (ctx.next_bb_id, { next_bb_id = succ ctx.next_bb_id })

let empty_build_ssa_context : build_ssa_context = { next_bb_id = 0 }

let rec partition_cfg_blocks (instrs : Tac.tac_instr list) (acc_labels : int list)
    (curr_code : Tac.tac_instr list) (blocks : proto_block list) (ctx : build_ssa_context) :
    proto_block list * build_ssa_context =
  match instrs with
  | [] ->
      if acc_labels = [] && curr_code = [] then (List.rev blocks, ctx)
      else
        let id, ctx = alloc_bb_id ctx in
        let blk = { id; labels = acc_labels; code = List.rev curr_code; term = None } in
        (List.rev (blk :: blocks), ctx)
  | instr :: rest -> (
      match instr with
      | Tac.Label label ->
          if curr_code = [] then partition_cfg_blocks rest (label :: acc_labels) [] blocks ctx
          else
            let id, ctx = alloc_bb_id ctx in
            let blk = { id; labels = acc_labels; code = List.rev curr_code; term = None } in
            partition_cfg_blocks rest [ label ] [] (blk :: blocks) ctx
      | Tac.Jump _ | Tac.Br _ | Tac.Return _ ->
          let id, ctx = alloc_bb_id ctx in
          let blk = { id; labels = acc_labels; code = List.rev curr_code; term = Some instr } in
          partition_cfg_blocks rest [] [] (blk :: blocks) ctx
      | _ -> partition_cfg_blocks rest acc_labels (instr :: curr_code) blocks ctx)

let build_func_cfg (f : Tac.tac_function) (ctx : build_ssa_context) : func * build_ssa_context =
  let pbs, ctx = partition_cfg_blocks f.func_body [] [] [] ctx in
  let labels =
    List.to_seq pbs
    |> Seq.flat_map (fun pb -> List.to_seq pb.labels |> Seq.map (fun l -> (l, pb.id)))
    |> IntMap.of_seq
  in
  let find_bb_id label map =
    match IntMap.find_opt label map with
    | Some v -> v
    | None -> internal_error "Label not found"
  in
  let convert_block pb next_pb_opt =
    let bb_code = List.filter_map instr_of_tac_instr_opt pb.code in
    let bb_term =
      match pb.term with
      | Some (Tac.Jump l) -> Jump (find_bb_id l labels)
      | Some (Tac.Br (cond, l)) ->
          let true_target = find_bb_id l labels in
          let false_target =
            match next_pb_opt with
            | Some next -> next.id
            | None -> -1
          in
          Br (operand_of_tac_operand cond, true_target, false_target)
      | Some (Tac.Return op) -> Return (Option.map operand_of_tac_operand op)
      | None -> (
          match next_pb_opt with
          | Some next -> Jump next.id
          | None -> Return None)
      | _ -> internal_error "Invalid terminator"
    in
    { bb_id = pb.id; bb_phis = []; bb_code; bb_term }
  in
  let rec process_blocks pbs =
    match pbs with
    | [] -> []
    | [ pb ] -> [ convert_block pb None ]
    | pb :: next :: rest -> convert_block pb (Some next) :: process_blocks (next :: rest)
  in
  let bbs = process_blocks pbs in
  let func_blocks = List.to_seq bbs |> Seq.map (fun bb -> (bb.bb_id, bb)) |> IntMap.of_seq in
  let func =
    {
      func_id = f.func_id;
      func_name = f.func_name;
      func_params = List.map new_value f.func_params;
      func_entry_block = (List.hd bbs).bb_id;
      func_blocks;
      func_ret_type = f.func_ret_type;
      func_obj_types = f.func_obj_types;
    }
  in
  (func, ctx)

let build_cfg (p : Tac.tac_program) (ctx : build_ssa_context) : program * build_ssa_context =
  let functions, ctx =
    List.fold_left
      (fun (funcs, ctx) tac_f ->
        let f, ctx = build_func_cfg tac_f ctx in
        (f :: funcs, ctx))
      ([], ctx) p.functions
  in
  let program =
    {
      globals = p.globals;
      global_init = p.global_init;
      functions = List.rev functions;
      objects = p.objects;
    }
  in
  (program, ctx)
