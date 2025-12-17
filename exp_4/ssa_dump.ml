open Common
open Ssa
open Iongraph.Types

let make_fake_bb_ptr (id : int) : int = id + 0xbeef0000
let make_fake_instr_ptr (id : int) : int = id + 0xcafe0000

module ValueMap = Map.Make (struct
  type t = Ssa.value

  let compare = compare
end)

let prettify_type_of (v : value) (func : Ssa.func) : string =
  match IntMap.find_opt (fst v) func.func_obj_types with
  | Some ty -> Tac.prettify_tac_obj_type ty
  | None -> "???"

type dump_context = {
  next_id : int;
  val_map : int ValueMap.t;
  uses_map : int list IntMap.t;
  preds_map : int list IntMap.t;
  succs_map : int list IntMap.t;
  opcodes_map : string IntMap.t;
  current_block_instrs : mir_instruction list;
}

let empty_dump_context =
  {
    next_id = 0;
    val_map = ValueMap.empty;
    uses_map = IntMap.empty;
    preds_map = IntMap.empty;
    succs_map = IntMap.empty;
    opcodes_map = IntMap.empty;
    current_block_instrs = [];
  }

type graph_operand =
  | Operand of int
  | Const of int
  | ConstFloat of float
  | Undefined

let alloc_id (ctx : dump_context) : int * dump_context =
  (ctx.next_id, { ctx with next_id = ctx.next_id + 1 })

let assign_val (v : value) (id : int) (ctx : dump_context) : dump_context =
  { ctx with val_map = ValueMap.add v id ctx.val_map }

let add_use (def_id : int) (use_id : int) (ctx : dump_context) : dump_context =
  { ctx with uses_map = prepend_int_or_singleton_list use_id def_id ctx.uses_map }

let assign_succ (bb_id : int) (succs : int list) (ctx : dump_context) : dump_context =
  { ctx with succs_map = IntMap.add bb_id succs ctx.succs_map }

let assign_opcode (id : int) (op : string) (ctx : dump_context) : dump_context =
  { ctx with opcodes_map = IntMap.add id op ctx.opcodes_map }

let add_pred succ_id pred_id ctx =
  { ctx with preds_map = prepend_int_or_singleton_list succ_id pred_id ctx.preds_map }

let succs_of_terminator (term : Ssa.terminator) : int list =
  match term with
  | Jump t -> [ t ]
  | Br (_, t, f) -> [ t; f ]
  | Return _ -> []

let def_id_of_instr = function
  | BinOp (d, _, _, _)
  | FBinOp (d, _, _, _)
  | UnaryOp (d, _, _)
  | FUnaryOp (d, _, _)
  | Move (d, _)
  | Itf (d, _)
  | Fti (d, _)
  | Call (d, _, _)
  | Alloca (d, _)
  | Load (d, _, _) -> Some d
  | Store _ -> None

let opcode_name_of_instr = function
  | BinOp (_, op, _, _) -> Ast.string_of_bin_op op
  | FBinOp (_, op, _, _) -> Ast.string_of_bin_op op
  | UnaryOp (_, op, _) -> Ast.string_of_unary_op op
  | FUnaryOp (_, op, _) -> Ast.string_of_unary_op op
  | Move _ -> "Move"
  | Itf _ -> "Itf"
  | Fti _ -> "Fti"
  | Call _ -> "Call"
  | Alloca _ -> "Alloca"
  | Load _ -> "Load"
  | Store _ -> "Store"

(* Make an instruction: id <- opcode ...inputs [type_] *)
let make_instr (id : int) (opcode : string) (inputs : graph_operand list) (type_ : string)
    (ctx : dump_context) : mir_instruction * dump_context =
  let inputs =
    List.filter_map
      (fun input ->
        match input with
        | Operand op -> Some op
        | Const _ | ConstFloat _ | Undefined -> None)
      inputs
  in
  let ctx = List.fold_left (fun ctx inp -> add_use id inp ctx) ctx inputs in
  ( {
      ptr = make_fake_instr_ptr id;
      id;
      opcode;
      attributes = [];
      inputs;
      uses = [];
      memInputs = [];
      type_;
    },
    { ctx with opcodes_map = IntMap.add id opcode ctx.opcodes_map } )

let get_operand_str (operand : graph_operand) (ctx : dump_context) : string =
  match operand with
  | Operand operand ->
      let op_name =
        IntMap.find_opt operand ctx.opcodes_map
        |> Option.map (fun op ->
            match String.split_on_char ' ' op with
            | part_1 :: _ -> Some part_1
            | [] -> None)
        |> Option.join |> or_default "???"
      in
      Printf.sprintf "%s#%d" op_name operand
  | Const c -> Printf.sprintf "%d" c
  | ConstFloat c -> Printf.sprintf "%f" c
  | Undefined -> "Undefined"

let emit_mir (instr : mir_instruction) (ctx : dump_context) : dump_context =
  { ctx with current_block_instrs = instr :: ctx.current_block_instrs }

let resolve_operand (operand : operand) (ctx : dump_context) : graph_operand * dump_context =
  match operand with
  | Value v -> (
      match ValueMap.find_opt v ctx.val_map with
      | Some id -> (Operand id, ctx)
      | None -> (Undefined, ctx))
  | Const c -> (Const c, ctx)
  | ConstFloat f -> (ConstFloat f, ctx)

let transcribe_params (func : Ssa.func) (ctx : dump_context) : dump_context =
  List.fold_left
    (fun ctx (i, p) ->
      let id = ValueMap.find p ctx.val_map in
      let instr, ctx =
        make_instr id (Printf.sprintf "Parameter %d" i) [] (prettify_type_of p func) ctx
      in
      emit_mir instr ctx)
    ctx
    (List.mapi (fun i p -> (i, p)) func.func_params)

let transcribe_phi (phi : phi) (func : Ssa.func) (preds : int list) (ctx : dump_context) :
    dump_context =
  let id = ValueMap.find phi.phi_dest ctx.val_map in
  let inputs, ctx =
    List.fold_left
      (fun (acc_inputs, ctx) pred_bb ->
        match IntMap.find_opt pred_bb phi.phi_incoming with
        | Some v -> (
            match ValueMap.find_opt v ctx.val_map with
            | Some vid -> (Operand vid :: acc_inputs, ctx)
            | None -> (Undefined :: acc_inputs, ctx))
        | None -> (Undefined :: acc_inputs, ctx))
      ([], ctx) preds
  in
  let inputs = List.rev inputs in
  let op_str =
    let inputs_str = List.map (fun id -> get_operand_str id ctx) inputs |> String.concat ", " in
    Printf.sprintf "Phi <- %s" inputs_str
  in
  let instr, ctx = make_instr id op_str inputs (prettify_type_of phi.phi_dest func) ctx in
  emit_mir instr ctx

let transcribe_instr (instr : instr) (func : Ssa.func) (ctx : dump_context) : dump_context =
  let id, ctx =
    match def_id_of_instr instr with
    | Some d -> (ValueMap.find d ctx.val_map, ctx)
    | None -> alloc_id ctx
  in
  let (opcode, inputs, type_), ctx =
    match instr with
    | BinOp (d, op, s1, s2) ->
        let i1, ctx = resolve_operand s1 ctx in
        let i2, ctx = resolve_operand s2 ctx in
        let op_str =
          Printf.sprintf "%s %s, %s" (Ast.string_of_bin_op op) (get_operand_str i1 ctx)
            (get_operand_str i2 ctx)
        in
        ((op_str, [ i1; i2 ], prettify_type_of d func), ctx)
    | FBinOp (d, op, s1, s2) ->
        let i1, ctx = resolve_operand s1 ctx in
        let i2, ctx = resolve_operand s2 ctx in
        let op_str =
          Printf.sprintf "%s %s, %s" (Ast.string_of_bin_op op) (get_operand_str i1 ctx)
            (get_operand_str i2 ctx)
        in
        ((op_str, [ i1; i2 ], prettify_type_of d func), ctx)
    | UnaryOp (d, op, s) ->
        let i, ctx = resolve_operand s ctx in
        let op_str = Printf.sprintf "%s %s" (Ast.string_of_unary_op op) (get_operand_str i ctx) in
        ((op_str, [ i ], prettify_type_of d func), ctx)
    | FUnaryOp (d, op, s) ->
        let i, ctx = resolve_operand s ctx in
        let op_str = Printf.sprintf "%s %s" (Ast.string_of_unary_op op) (get_operand_str i ctx) in
        ((op_str, [ i ], prettify_type_of d func), ctx)
    | Move (d, s) ->
        let i, ctx = resolve_operand s ctx in
        let op_str = Printf.sprintf "Move %s" (get_operand_str i ctx) in
        ((op_str, [ i ], prettify_type_of d func), ctx)
    | Itf (d, s) ->
        let i, ctx = resolve_operand s ctx in
        let op_str = Printf.sprintf "Itf %s" (get_operand_str i ctx) in
        ((op_str, [ i ], prettify_type_of d func), ctx)
    | Fti (d, s) ->
        let i, ctx = resolve_operand s ctx in
        let op_str = Printf.sprintf "Fti %s" (get_operand_str i ctx) in
        ((op_str, [ i ], prettify_type_of d func), ctx)
    | Call (d, f_idx, args) ->
        let args_ids, ctx =
          List.fold_left
            (fun (acc, ctx) arg ->
              let i, ctx = resolve_operand arg ctx in
              (i :: acc, ctx))
            ([], ctx) args
        in
        let args_ids = List.rev args_ids in
        let args_str = List.map (fun id -> get_operand_str id ctx) args_ids |> String.concat ", " in
        ((Printf.sprintf "Call $%d (%s)" f_idx args_str, args_ids, prettify_type_of d func), ctx)
    | Alloca (d, size) -> ((Printf.sprintf "Alloca %d" size, [], prettify_type_of d func), ctx)
    | Load (d, mem, indices) ->
        let mem_inp =
          match mem with
          | LocalArray v -> Some (Operand (ValueMap.find v ctx.val_map))
          | _ -> None
        in
        let idx_inps, ctx =
          List.fold_left
            (fun (acc, ctx) idx ->
              let i, ctx = resolve_operand idx ctx in
              (i :: acc, ctx))
            ([], ctx) indices
        in
        let idx_inps = List.rev idx_inps in
        let op_str =
          match mem with
          | LocalArray _ ->
              let base = get_operand_str (Option.get mem_inp) ctx in
              let indices =
                List.map (fun id -> get_operand_str id ctx) idx_inps |> String.concat ", "
              in
              Printf.sprintf "Load %s[%s]" base indices
          | GlobalScalar i -> Printf.sprintf "Load @%d" i
          | GlobalArray i ->
              let indices =
                List.map (fun id -> get_operand_str id ctx) idx_inps |> String.concat ", "
              in
              Printf.sprintf "Load @%d[%s]" i indices
        in
        ((op_str, map_or_default List.singleton [] mem_inp @ idx_inps, prettify_type_of d func), ctx)
    | Store (mem, indices, src) ->
        let mem_inp =
          match mem with
          | LocalArray v -> Some (Operand (ValueMap.find v ctx.val_map))
          | _ -> None
        in
        let idx_inps, ctx =
          List.fold_left
            (fun (acc, ctx) idx ->
              let i, ctx = resolve_operand idx ctx in
              (i :: acc, ctx))
            ([], ctx) indices
        in
        let idx_inps = List.rev idx_inps in
        let src_inp, ctx = resolve_operand src ctx in
        let src_str = get_operand_str src_inp ctx in
        let op_str =
          match mem with
          | LocalArray _ ->
              let base = get_operand_str (Option.get mem_inp) ctx in
              let indices =
                List.map (fun id -> get_operand_str id ctx) idx_inps |> String.concat ", "
              in
              Printf.sprintf "Store %s[%s] = %s" base indices src_str
          | GlobalScalar i -> Printf.sprintf "Store @%d = %s" i src_str
          | GlobalArray i ->
              let indices =
                List.map (fun id -> get_operand_str id ctx) idx_inps |> String.concat ", "
              in
              Printf.sprintf "Store @%d[%s] = %s" i indices src_str
        in
        ((op_str, map_or_default List.singleton [] mem_inp @ idx_inps @ [ src_inp ], "None"), ctx)
  in
  let instr, ctx = make_instr id opcode inputs type_ ctx in
  emit_mir instr ctx

let transcribe_terminator (term : terminator) (ctx : dump_context) : dump_context =
  let instr, ctx =
    match term with
    | Jump t ->
        let id, ctx = alloc_id ctx in
        make_instr id (Printf.sprintf "Jump -> .BB%%%d" t) [] "None" ctx
    | Br (cond, t, f) ->
        let id, ctx = alloc_id ctx in
        let cond_id, ctx = resolve_operand cond ctx in
        let op_str =
          Printf.sprintf "Br %s -> T: .BB%%%d, F: .BB%%%d" (get_operand_str cond_id ctx) t f
        in
        make_instr id op_str [ cond_id ] "None" ctx
    | Return (Some op) ->
        let id, ctx = alloc_id ctx in
        let op_id, ctx = resolve_operand op ctx in
        let op_str = Printf.sprintf "Return %s" (get_operand_str op_id ctx) in
        make_instr id op_str [ op_id ] "None" ctx
    | Return None ->
        let id, ctx = alloc_id ctx in
        make_instr id "Return" [] "None" ctx
  in
  emit_mir instr ctx

let finalize_block (bb_id : int) (prog : Ssa.program) (ctx : dump_context) :
    mir_block * dump_context =
  let preds = IntMap.find_opt bb_id ctx.preds_map |> or_default [] |> List.sort compare
  and succs = IntMap.find_opt bb_id ctx.succs_map |> or_default [] |> List.sort compare in

  let loop_depth = IntMap.find_opt bb_id prog.loop_depths |> Option.value ~default:0 in
  let attributes =
    (if IntSet.mem bb_id prog.loop_headers then [ "loopheader" ] else [])
    @ if IntSet.mem bb_id prog.back_edges then [ "backedge" ] else []
  in

  let mir_bb : mir_block =
    {
      ptr = make_fake_bb_ptr bb_id;
      id = bb_id;
      loopDepth = loop_depth;
      attributes;
      predecessors = preds;
      successors = succs;
      instructions = List.rev ctx.current_block_instrs;
    }
  in
  (mir_bb, { ctx with current_block_instrs = [] })

let compute_initial_cfg (func : Ssa.func) : dump_context =
  IntMap.fold
    (fun id bb ctx ->
      let succs = succs_of_terminator bb.bb_term in
      let ctx = assign_succ id succs ctx in
      List.fold_left (fun ctx s -> add_pred s id ctx) ctx succs)
    func.func_blocks empty_dump_context

let dump_func (pass_name : string) (prog : Ssa.program) (func : Ssa.func) : Iongraph.Types.func =
  (* 1. Compute CFG *)
  let ctx = compute_initial_cfg func in
  (* 2. Assign IDs to definitions *)
  let ctx =
    List.fold_left
      (fun ctx (i, p) ->
        let id, ctx = alloc_id ctx in
        let ctx = assign_val p id ctx in
        assign_opcode id (Printf.sprintf "Parameter %d" i) ctx)
      ctx
      (List.mapi (fun i p -> (i, p)) func.func_params)
  in
  let ctx =
    IntMap.fold
      (fun _ bb ctx ->
        let ctx =
          List.fold_left
            (fun ctx phi ->
              let id, ctx = alloc_id ctx in
              let ctx = assign_val phi.phi_dest id ctx in
              assign_opcode id "Phi" ctx)
            ctx bb.bb_phis
        in
        List.fold_left
          (fun ctx instr ->
            match def_id_of_instr instr with
            | Some d ->
                let id, ctx = alloc_id ctx in
                let ctx = assign_val d id ctx in
                assign_opcode id (opcode_name_of_instr instr) ctx
            | None -> ctx)
          ctx bb.bb_code)
      func.func_blocks ctx
  in
  (* 3. Generate Blocks *)
  let blocks, ctx =
    IntMap.fold
      (fun bb_id bb (acc_blocks, ctx) ->
        let blocks, ctx =
          (* Entry block parameters *)
          let ctx = if bb_id = func.func_entry_block then transcribe_params func ctx else ctx in
          (* Phis *)
          let preds = IntMap.find_opt bb_id ctx.preds_map |> or_default [] |> List.sort compare in
          let ctx =
            List.fold_left (fun ctx phi -> transcribe_phi phi func preds ctx) ctx bb.bb_phis
          in
          (* Instructions *)
          let ctx =
            List.fold_left (fun ctx instr -> transcribe_instr instr func ctx) ctx bb.bb_code
          in
          (* Terminator *)
          let ctx = transcribe_terminator bb.bb_term ctx in
          let mir_bb, ctx = finalize_block bb_id prog ctx in
          (mir_bb :: acc_blocks, ctx)
        in
        (blocks, ctx))
      func.func_blocks ([], ctx)
  in
  let blocks = List.rev blocks in

  (* 4. Fix uses *)
  let blocks =
    List.map
      (fun (bb : mir_block) ->
        let instrs =
          List.map
            (fun (instr : mir_instruction) ->
              let uses = IntMap.find_opt instr.id ctx.uses_map |> Option.value ~default:[] in
              { instr with uses })
            bb.instructions
        in
        { bb with instructions = instrs })
      blocks
  in
  {
    name = func.func_name;
    passes = [ { name = pass_name; mir = { blocks }; lir = { blocks = [] } } ];
  }

let collect_passes (passes : Iongraph.Types.func list) : Iongraph.Types.func option =
  match passes with
  | [] -> None
  | h :: rest ->
      let func =
        List.fold_left
          (fun func pass ->
            let _ =
              if pass.name <> func.name then
                internal_error "Cannot collect across different functions"
              else if List.length pass.passes <> 1 then
                internal_error "Must be a complete left fold"
              else ()
            in
            { func with passes = List.hd pass.passes :: func.passes })
          h rest
      in
      Some { func with passes = List.rev func.passes }

let dump_ssa (passes : (string * Ssa.program) list) : Yojson.Safe.t =
  let all_funcs =
    List.fold_left
      (fun acc (pass_name, prog) ->
        let funcs_in_pass = List.map (dump_func pass_name prog) prog.functions in
        List.fold_left
          (fun acc func ->
            match List.find_opt (fun f -> f.name = func.name) acc with
            | Some existing -> (
                let others = List.filter (fun f -> f.name <> func.name) acc in
                match collect_passes [ existing; func ] with
                | Some merged -> merged :: others
                | None -> func :: others)
            | None -> func :: acc)
          acc funcs_in_pass)
      [] passes
  in

  let obj = { version = current_version; functions = all_funcs } in
  obj |> ion_json_to_yojson
