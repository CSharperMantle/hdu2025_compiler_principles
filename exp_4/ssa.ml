open Common

let prettify_basic_block_id (v : int) : string = Printf.sprintf ".BB%%%d" v

type value = int * int (* (base id, version) *)

let new_value (id : int) : value = (id, 0)
let prettify_value (v : value) : string = Printf.sprintf "%%%d/%d" (fst v) (snd v)
let id_of_value (v : value) : int = fst v

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
  phi_id : int;
  phi_dest : value;
  phi_incoming : value IntMap.t;
}

let prettify_phi (v : phi) : string =
  let incoming_str =
    IntMap.bindings v.phi_incoming
    |> string_of_list (fun (bb, v) ->
        Printf.sprintf "%s (%s)" (prettify_value v) (prettify_basic_block_id bb))
  in
  Printf.sprintf "%s <- Phi %s" (prettify_value v.phi_dest) incoming_str

type mem_loc =
  | LocalArray of value
  | GlobalScalar of int
  | GlobalArray of int

let prettify_mem_loc = function
  | LocalArray v -> prettify_value v
  | GlobalScalar id -> Printf.sprintf "@%d" id
  | GlobalArray id -> Printf.sprintf "@%d" id

type instr =
  | BinOp of int * value * Ast.bin_op * operand * operand
  | FBinOp of int * value * Ast.bin_op * operand * operand
  | UnaryOp of int * value * Ast.unary_op * operand
  | FUnaryOp of int * value * Ast.unary_op * operand
  | Move of int * value * operand
  | Itf of int * value * operand
  | Fti of int * value * operand
  | Call of int * value * int * operand list
  | Alloca of int * value * int
  | Load of int * value * mem_loc * operand list
  | Store of int * mem_loc * operand list * operand

let instr_of_tac_instr_opt (id : int) (instr : Tac.tac_instr) : instr option =
  match instr with
  | Tac.BinOp (d, op, s1, s2) ->
      Some (BinOp (id, new_value d, op, operand_of_tac_operand s1, operand_of_tac_operand s2))
  | Tac.FBinOp (d, op, s1, s2) ->
      Some (FBinOp (id, new_value d, op, operand_of_tac_operand s1, operand_of_tac_operand s2))
  | Tac.UnaryOp (d, op, s) -> Some (UnaryOp (id, new_value d, op, operand_of_tac_operand s))
  | Tac.FUnaryOp (d, op, s) -> Some (FUnaryOp (id, new_value d, op, operand_of_tac_operand s))
  | Tac.Move (d, s) -> Some (Move (id, new_value d, operand_of_tac_operand s))
  | Tac.Itf (d, s) -> Some (Itf (id, new_value d, operand_of_tac_operand s))
  | Tac.Fti (d, s) -> Some (Fti (id, new_value d, operand_of_tac_operand s))
  | Tac.Call (d, f, args) -> Some (Call (id, new_value d, f, List.map operand_of_tac_operand args))
  | Tac.Alloca (d, size) -> Some (Alloca (id, new_value d, size))
  | Tac.Load (d, b, _, indices) ->
      let mem =
        match b with
        | Tac.LocalScalar _ -> internal_error "Local scalar should not require Load"
        | Tac.LocalArray vid -> LocalArray (new_value vid)
        | Tac.GlobalScalar vid -> GlobalScalar vid
        | Tac.GlobalArray vid -> GlobalArray vid
      in
      Some (Load (id, new_value d, mem, List.map operand_of_tac_operand indices))
  | Tac.Store (d, _, s, indices) ->
      let mem =
        match d with
        | Tac.LocalArray vid -> LocalArray (new_value vid)
        | Tac.GlobalScalar vid -> GlobalScalar vid
        | Tac.GlobalArray vid -> GlobalArray vid
        | Tac.LocalScalar _ -> internal_error "Local scalar in memory"
      in
      Some (Store (id, mem, List.map operand_of_tac_operand indices, operand_of_tac_operand s))
  | Tac.Label _ | Tac.Jump _ | Tac.Br _ | Tac.Return _ -> None

let prettify_instr = function
  | BinOp (_, dest, op, src1, src2) ->
      Printf.sprintf "%s.i\t(%s, %s, %s)" (Ast.string_of_bin_op op) (prettify_value dest)
        (prettify_operand src1) (prettify_operand src2)
  | FBinOp (_, dest, op, src1, src2) ->
      Printf.sprintf "%s.f\t(%s, %s, %s)" (Ast.string_of_bin_op op) (prettify_value dest)
        (prettify_operand src1) (prettify_operand src2)
  | UnaryOp (_, dest, op, src) ->
      Printf.sprintf "%s.i\t(%s, %s)" (Ast.string_of_unary_op op) (prettify_value dest)
        (prettify_operand src)
  | FUnaryOp (_, dest, op, src) ->
      Printf.sprintf "%s.f\t(%s, %s)" (Ast.string_of_unary_op op) (prettify_value dest)
        (prettify_operand src)
  | Move (_, dest, src) ->
      Printf.sprintf "Move\t(%s, %s)" (prettify_value dest) (prettify_operand src)
  | Itf (_, dest, src) ->
      Printf.sprintf "Itf\t(%s, %s)" (prettify_value dest) (prettify_operand src)
  | Fti (_, dest, src) ->
      Printf.sprintf "Fti\t(%s, %s)" (prettify_value dest) (prettify_operand src)
  | Call (_, dest, func_id, args) ->
      let args_str = List.map prettify_operand args |> String.concat ", " in
      if args = [] then Printf.sprintf "Call\t(%s, $%d)" (prettify_value dest) func_id
      else Printf.sprintf "Call\t(%s, $%d, [%s])" (prettify_value dest) func_id args_str
  | Alloca (_, dest, size) -> Printf.sprintf "Alloca\t(%s, %d)" (prettify_value dest) size
  | Load (_, dest, base, indices) ->
      let indices_str = List.map prettify_operand indices |> String.concat "][" in
      Printf.sprintf "Load\t(%s, %s, [%s])" (prettify_value dest) (prettify_mem_loc base)
        indices_str
  | Store (_, base, indices, src) ->
      let indices_str = List.map prettify_operand indices |> String.concat "][" in
      Printf.sprintf "Store\t(%s, [%s], %s)" (prettify_mem_loc base) indices_str
        (prettify_operand src)

type terminator =
  | Jump of int * int
  | Br of int * operand * int * int
  | Return of int * operand option

let prettify_terminator = function
  | Jump (_, bb) -> Printf.sprintf "Jump\t(%s)" (prettify_basic_block_id bb)
  | Br (_, cond, bb_truthy, bb_falsy) ->
      Printf.sprintf "Br\t(%s, %s, %s)" (prettify_operand cond)
        (prettify_basic_block_id bb_truthy)
        (prettify_basic_block_id bb_falsy)
  | Return (_, retval) -> Printf.sprintf "Return\t(%s)" (map_or_default prettify_operand "" retval)

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
  Printf.sprintf "%s$%d(%s) -> %s [entry: %s]:\n%s\n%s" f.func_name f.func_id params_str
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

type program_loop_props = {
  loop_headers : IntSet.t;
  back_edges : IntSet.t;
  back_edge_list : (int * int) list;
  loop_depths : int IntMap.t;
}

type program = {
  globals : int list;
  global_init : Tac.tac_init IntMap.t;
  functions : func list;
  objects : Tac.tac_obj_type IntMap.t;
  next_instr_id : int;
  next_bb_id : int;
  loop_props : program_loop_props;
}

let prettify_program (p : program) : string =
  let globals_str =
    List.map
      (fun id ->
        let decl = Printf.sprintf ".global\t@%d" id in
        match IntMap.find_opt id p.global_init with
        | Some init -> Printf.sprintf "%s, %s" decl (Tac.prettify_tac_init init)
        | None -> decl)
      p.globals
    |> String.concat "\n"
  in
  let funcs_str = List.map prettify_func p.functions |> String.concat "\n\n" in
  globals_str ^ "\n\n" ^ funcs_str

(*
  Context for building SSA.

  Should be okay to share between functions since BB id are unique.
*)
type build_ssa_context = {
  next_bb_id : int;
  next_instr_id : int;
  successors : IntSet.t IntMap.t;
  predecessors : IntSet.t IntMap.t;
  dom_frontier : IntSet.t IntMap.t;
  idom : int IntMap.t;
  dom_tree : IntSet.t IntMap.t;
  loop_headers : IntSet.t;
  back_edges : IntSet.t;
  back_edge_list : (int * int) list;
  loop_depths : int IntMap.t;
}

let alloc_bb_id (ctx : build_ssa_context) : int * build_ssa_context =
  (ctx.next_bb_id, { ctx with next_bb_id = ctx.next_bb_id + 1 })

let alloc_instr_id (ctx : build_ssa_context) : int * build_ssa_context =
  (ctx.next_instr_id, { ctx with next_instr_id = ctx.next_instr_id + 1 })

let empty_build_ssa_context : build_ssa_context =
  {
    next_bb_id = 0;
    next_instr_id = 0;
    successors = IntMap.empty;
    predecessors = IntMap.empty;
    dom_frontier = IntMap.empty;
    idom = IntMap.empty;
    dom_tree = IntMap.empty;
    loop_headers = IntSet.empty;
    back_edges = IntSet.empty;
    back_edge_list = [];
    loop_depths = IntMap.empty;
  }

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

let build_func_cfg (tac_func : Tac.tac_function) (ctx : build_ssa_context) :
    func * build_ssa_context =
  let pbs, ctx = partition_cfg_blocks tac_func.func_body [] [] [] ctx in
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
  let convert_block pb next_pb_opt ctx =
    let bb_code', ctx =
      List.fold_left
        (fun (code, ctx) instr ->
          let id, ctx' = alloc_instr_id ctx in
          match instr_of_tac_instr_opt id instr with
          | Some instr -> (instr :: code, ctx')
          | None -> (code, ctx))
        ([], ctx) pb.code
    in
    let bb_code = List.rev bb_code' in
    let bb_term, ctx =
      match pb.term with
      | Some (Tac.Jump l) ->
          let id, ctx = alloc_instr_id ctx in
          (Jump (id, find_bb_id l labels), ctx)
      | Some (Tac.Br (cond, l)) ->
          let id, ctx = alloc_instr_id ctx in
          let true_target = find_bb_id l labels in
          let false_target =
            match next_pb_opt with
            | Some next -> next.id
            | None -> -1
          in
          (Br (id, operand_of_tac_operand cond, true_target, false_target), ctx)
      | Some (Tac.Return op) ->
          let id, ctx = alloc_instr_id ctx in
          (Return (id, Option.map operand_of_tac_operand op), ctx)
      | None -> (
          let id, ctx = alloc_instr_id ctx in
          match next_pb_opt with
          | Some next -> (Jump (id, next.id), ctx)
          | None -> (Return (id, None), ctx))
      | _ -> internal_error "Invalid terminator"
    in
    ({ bb_id = pb.id; bb_phis = []; bb_code; bb_term }, ctx)
  in
  let rec process_blocks pbs ctx =
    match pbs with
    | [] -> ([], ctx)
    | [ pb ] ->
        let bb, ctx = convert_block pb None ctx in
        ([ bb ], ctx)
    | pb :: next :: rest ->
        let bb, ctx = convert_block pb (Some next) ctx in
        let rest_bbs, ctx = process_blocks (next :: rest) ctx in
        (bb :: rest_bbs, ctx)
  in
  let bbs, ctx = process_blocks pbs ctx in
  let func_blocks = List.to_seq bbs |> Seq.map (fun bb -> (bb.bb_id, bb)) |> IntMap.of_seq in

  (* Calculate successors from terminators *)
  let successors =
    List.fold_left
      (fun acc bb ->
        let succs =
          match bb.bb_term with
          | Jump (_, target) -> IntSet.singleton target
          | Br (_, _, true_target, false_target) -> IntSet.of_list [ true_target; false_target ]
          | Return _ -> IntSet.empty
        in
        IntMap.add bb.bb_id succs acc)
      IntMap.empty bbs
  in

  (* Calculate predecessors by inverting successors *)
  let predecessors =
    IntMap.fold
      (fun bb_id succs acc ->
        IntSet.fold
          (fun succ_id acc ->
            let existing = IntMap.find_opt succ_id acc |> or_default IntSet.empty in
            IntMap.add succ_id (IntSet.add bb_id existing) acc)
          succs acc)
      successors IntMap.empty
  in
  let successors = IntMap.fold IntMap.add successors ctx.successors
  and predecessors = IntMap.fold IntMap.add predecessors ctx.predecessors in
  let ctx = { ctx with successors; predecessors }
  and func =
    {
      func_id = tac_func.func_id;
      func_name = tac_func.func_name;
      func_params = List.map new_value tac_func.func_params;
      func_entry_block = (List.hd bbs).bb_id;
      func_blocks;
      func_ret_type = tac_func.func_ret_type;
      func_obj_types = tac_func.func_obj_types;
    }
  in
  (func, ctx)

let build_cfg (program : Tac.tac_program) (ctx : build_ssa_context) : program * build_ssa_context =
  let functions, ctx =
    List.fold_left
      (fun (funcs, ctx) tac_f ->
        let f, ctx = build_func_cfg tac_f ctx in
        (f :: funcs, ctx))
      ([], ctx) program.functions
  in
  let program =
    {
      globals = program.globals;
      global_init = program.global_init;
      functions = List.rev functions;
      objects = program.objects;
      next_instr_id = ctx.next_instr_id;
      next_bb_id = ctx.next_bb_id;
      loop_props =
        {
          loop_headers = IntSet.empty;
          back_edges = IntSet.empty;
          back_edge_list = [];
          loop_depths = IntMap.empty;
        };
    }
  in
  (program, ctx)

let def_id_of = function
  | BinOp (_, (id, _), _, _, _)
  | FBinOp (_, (id, _), _, _, _)
  | UnaryOp (_, (id, _), _, _)
  | FUnaryOp (_, (id, _), _, _)
  | Move (_, (id, _), _)
  | Itf (_, (id, _), _)
  | Fti (_, (id, _), _)
  | Call (_, (id, _), _, _)
  | Alloca (_, (id, _), _)
  | Load (_, (id, _), _, _) -> Some id
  | Store _ -> None

type lt_state = {
  dfnum : int IntMap.t;
  vertex : int IntMap.t;
  parent : int IntMap.t;
  semi : int IntMap.t;
  ancestor : int IntMap.t;
  label : int IntMap.t;
  bucket : int list IntMap.t;
  dom : int IntMap.t;
  n : int;
}

(* Algorithm 19.9, "Modern Compiler Implementation in C", Appel. *)
let lengauer_tarjan (func : func) (ctx : build_ssa_context) : int IntMap.t =
  let link v w state = { state with ancestor = IntMap.add w v state.ancestor } in
  let rec compress v state =
    match IntMap.find_opt v state.ancestor with
    | None -> state
    | Some a -> (
        match IntMap.find_opt a state.ancestor with
        | None -> state
        | Some _ ->
            let state = compress a state in
            let a_label = IntMap.find a state.label and v_label = IntMap.find v state.label in
            let semi_a = IntMap.find a_label state.semi
            and semi_v = IntMap.find v_label state.semi in
            let new_label = if semi_a < semi_v then a_label else v_label
            and new_ancestor = IntMap.find a state.ancestor in
            {
              state with
              ancestor = IntMap.add v new_ancestor state.ancestor;
              label = IntMap.add v new_label state.label;
            })
  in
  let eval v state =
    match IntMap.find_opt v state.ancestor with
    | None -> (v, state)
    | Some _ ->
        let state = compress v state in
        let label = IntMap.find v state.label in
        (label, state)
  in
  (* DFS part of the algorithm. *)
  let rec dfs u succs state =
    let n' = state.n + 1 in
    let state =
      {
        state with
        dfnum = IntMap.add u n' state.dfnum;
        vertex = IntMap.add n' u state.vertex;
        semi = IntMap.add u n' state.semi;
        label = IntMap.add u u state.label;
        n = n';
      }
    in
    let u_succs = IntMap.find_opt u succs |> or_default IntSet.empty in
    IntSet.fold
      (fun v state ->
        if IntMap.mem v state.dfnum then state
        else
          let state' = { state with parent = IntMap.add v u state.parent } in
          dfs v succs state')
      u_succs state
  in
  let rec loop_1 i preds state =
    if i < 2 then state
    else
      let n = IntMap.find i state.vertex in
      let p = IntMap.find n state.parent in
      let n_preds = IntMap.find_opt n preds |> or_default IntSet.empty in
      (* for each predecessor v of n *)
      let state =
        IntSet.fold
          (fun v state ->
            let u, acc_state = eval v state in
            let semi_u = IntMap.find u acc_state.semi and semi_n = IntMap.find n acc_state.semi in
            if semi_u < semi_n then { acc_state with semi = IntMap.add n semi_u acc_state.semi }
            else acc_state)
          n_preds state
      in
      let semi_n = IntMap.find (IntMap.find n state.semi) state.vertex in
      let state = { state with bucket = prepend_int_or_singleton_list semi_n n state.bucket } in
      let state = link p n state in
      let bucket_p = IntMap.find_opt p state.bucket |> or_default [] in
      (* for each v in bucket[p] *)
      let state =
        List.fold_left
          (fun state v ->
            let y, state = eval v state in
            let semi_y = IntMap.find y state.semi and semi_v = IntMap.find v state.semi in
            let dom_v = if semi_y < semi_v then y else p in
            { state with dom = IntMap.add v dom_v state.dom })
          state bucket_p
      in
      let state = { state with bucket = IntMap.remove p state.bucket } in
      loop_1 (i - 1) preds state
  in
  let rec loop_2 i state =
    if i > state.n then state
    else
      let w = IntMap.find i state.vertex in
      let state =
        if i = 1 then state
        else
          let d = IntMap.find w state.dom in
          let w_semi_node = IntMap.find (IntMap.find w state.semi) state.vertex in
          if d <> w_semi_node then
            { state with dom = IntMap.add w (IntMap.find d state.dom) state.dom }
          else state
      in
      loop_2 (i + 1) state
  in

  let preds = ctx.predecessors
  and succs = ctx.successors
  and state =
    {
      dfnum = IntMap.empty;
      vertex = IntMap.empty;
      parent = IntMap.empty;
      semi = IntMap.empty;
      ancestor = IntMap.empty;
      label = IntMap.empty;
      bucket = IntMap.empty;
      dom = IntMap.empty;
      n = 0;
    }
  in
  let state = dfs func.func_entry_block succs state in
  (* for i <- N − 1 downto 1 *)
  let state = loop_1 state.n preds state in
  (* for i <- 1 to N − 1 *)
  let state = loop_2 2 state in
  state.dom

let compute_dom_frontier (func : func) (ctx : build_ssa_context) : build_ssa_context =
  (*
    computeDF[n]
    Chapter 19.1.2 "The Dominance Frontier", "Modern Compiler Implementation in C", Appel.
  *)
  let rec compute_df (n : int) (ctx : build_ssa_context) : build_ssa_context =
    let succ = IntMap.find_opt n ctx.successors |> or_default IntSet.empty in
    let df_local =
      IntSet.filter_map
        (fun y ->
          IntMap.find_opt y ctx.idom
          |> Option.map (fun idom_y -> if idom_y <> n then Some y else None)
          |> Option.join)
        succ
    in
    let df_up, ctx =
      IntSet.fold
        (fun c (s, ctx) ->
          let ctx = compute_df c ctx in
          let df_c =
            IntMap.find_opt c ctx.dom_frontier
            |> or_else (fun () -> internal_error "compute_df c called but c not found in result")
          in
          let s' =
            IntSet.filter_map
              (fun w ->
                if
                  (* FIXME: In textbook this is: if n is not a dominator of w, or ...*)
                  IntMap.find_opt w ctx.idom |> map_or_default (fun idom_w -> idom_w <> n) true
                  || n = w
                then Some w
                else None)
              df_c
          in
          (IntSet.union s s', ctx))
        (IntMap.find_opt n ctx.dom_tree |> or_default IntSet.empty)
        (IntSet.empty, ctx)
    in
    { ctx with dom_frontier = IntMap.add n (IntSet.union df_local df_up) ctx.dom_frontier }
  in
  let idom = lengauer_tarjan func ctx in
  let dom_tree =
    IntMap.fold
      (fun node parent acc -> union_int_or_singleton_int_set parent node acc)
      idom IntMap.empty
  in
  let ctx = { ctx with idom; dom_tree } in
  compute_df func.func_entry_block ctx

let compute_loop_props (func : func) (ctx : build_ssa_context) : build_ssa_context =
  let rec dominates target source =
    if target = source then true
    else
      match IntMap.find_opt source ctx.idom with
      | Some idom -> if idom = source then false else dominates target idom
      | None -> false
  in
  let back_edges =
    IntMap.fold
      (fun u _ acc ->
        let succs = IntMap.find_opt u ctx.successors |> or_default IntSet.empty in
        IntSet.fold (fun v acc -> if dominates v u then (u, v) :: acc else acc) succs acc)
      func.func_blocks []
  in
  let headers = List.fold_left (fun acc (_, h) -> IntSet.add h acc) IntSet.empty back_edges in
  let back_edge_sources =
    List.fold_left (fun acc (u, _) -> IntSet.add u acc) IntSet.empty back_edges
  in
  let loops =
    List.fold_left
      (fun acc (u, h) ->
        let cur = IntMap.find_opt h acc |> or_default [] in
        IntMap.add h (u :: cur) acc)
      IntMap.empty back_edges
  in
  let depths =
    IntMap.fold
      (fun h sources acc ->
        let rec loop_body worklist visited =
          match worklist with
          | [] -> visited
          | n :: rest ->
              if IntSet.mem n visited then loop_body rest visited
              else
                let visited = IntSet.add n visited in
                let preds = IntMap.find_opt n ctx.predecessors |> or_default IntSet.empty in
                let new_work =
                  IntSet.to_seq preds
                  |> Seq.filter_map (fun p -> if p <> h then Some p else None)
                  |> List.of_seq
                in
                loop_body (new_work @ rest) visited
        in
        let body = loop_body sources (IntSet.singleton h) in
        IntSet.fold
          (fun b acc ->
            let d = IntMap.find_opt b acc |> or_default 0 in
            IntMap.add b (d + 1) acc)
          body acc)
      loops IntMap.empty
  in
  {
    ctx with
    loop_headers = IntSet.union ctx.loop_headers headers;
    back_edges = IntSet.union ctx.back_edges back_edge_sources;
    back_edge_list = ctx.back_edge_list @ back_edges;
    loop_depths = IntMap.union (fun _ v_orig _ -> Some v_orig) ctx.loop_depths depths;
  }

let reset_loop_props (program : program) : program =
  {
    program with
    loop_props =
      {
        loop_headers = IntSet.empty;
        back_edges = IntSet.empty;
        back_edge_list = [];
        loop_depths = IntMap.empty;
      };
  }

let recompute_loop_props (program : program) : program =
  (* Rebuild context from program's current state *)
  let ctx =
    {
      empty_build_ssa_context with
      next_bb_id = program.next_bb_id;
      next_instr_id = program.next_instr_id;
    }
  in

  (* Rebuild successors and predecessors from all functions *)
  let ctx =
    List.fold_left
      (fun ctx func ->
        let succs, preds =
          IntMap.fold
            (fun bb_id bb (succs, preds) ->
              let bb_succs =
                match bb.bb_term with
                | Jump (_, target) -> IntSet.singleton target
                | Br (_, _, true_target, false_target) ->
                    IntSet.of_list [ true_target; false_target ]
                | Return _ -> IntSet.empty
              in
              let preds =
                IntSet.fold
                  (fun succ_id acc ->
                    let existing = IntMap.find_opt succ_id acc |> or_default IntSet.empty in
                    IntMap.add succ_id (IntSet.add bb_id existing) acc)
                  bb_succs preds
              in
              (IntMap.add bb_id bb_succs succs, preds))
            func.func_blocks (IntMap.empty, IntMap.empty)
        in
        {
          ctx with
          successors = IntMap.fold IntMap.add succs ctx.successors;
          predecessors = IntMap.fold IntMap.add preds ctx.predecessors;
        })
      ctx program.functions
  in

  (* Recompute dominators and loop properties for each function *)
  let ctx =
    List.fold_left
      (fun ctx func ->
        let ctx = compute_dom_frontier func ctx in
        compute_loop_props func ctx)
      ctx program.functions
  in

  {
    program with
    loop_props =
      {
        loop_headers = ctx.loop_headers;
        back_edges = ctx.back_edges;
        back_edge_list = ctx.back_edge_list;
        loop_depths = ctx.loop_depths;
      };
  }

(* Algorithm 19.6, "Modern Compiler Implementation in C", Appel. *)
let insert_phi (func : func) (ctx : build_ssa_context) : func * build_ssa_context =
  let def_sites =
    (* Arguments *)
    List.fold_left
      (fun acc (a, _) -> union_int_or_singleton_int_set a func.func_entry_block acc)
      IntMap.empty func.func_params
    (* for each node n *)
    |> IntMap.fold
         (fun n bb acc ->
           (* for each variable a in A_orig[n] *)
           List.fold_left
             (fun acc instr ->
               (* def_sites[a] <- def_sites[a] ∪ {n} *)
               match def_id_of instr with
               | Some a -> union_int_or_singleton_int_set a n acc
               | None -> acc)
             acc bb.bb_code)
         func.func_blocks
  in
  let phis_to_add =
    IntMap.fold
      (fun a sites acc ->
        let rec aux w added =
          match w with
          | [] -> added
          | x :: rest ->
              let df_x = IntMap.find_opt x ctx.dom_frontier |> or_default IntSet.empty in
              let w', added' =
                IntSet.fold
                  (fun y (w_acc, added_acc) ->
                    if not (IntSet.mem y added_acc) then
                      let added_acc = IntSet.add y added_acc
                      and w_acc = if not (IntSet.mem y sites) then y :: w_acc else w_acc in
                      (w_acc, added_acc)
                    else (w_acc, added_acc))
                  df_x (rest, added)
              in
              aux w' added'
        in
        let phi_blocks = aux (IntSet.elements sites) IntSet.empty in
        IntSet.fold (fun bb_id acc -> prepend_int_or_singleton_list bb_id a acc) phi_blocks acc)
      def_sites IntMap.empty
  in
  let func_blocks, ctx =
    IntMap.fold
      (fun bb_id bb (blocks, ctx) ->
        let vars = IntMap.find_opt bb_id phis_to_add |> or_default []
        and preds = IntMap.find_opt bb_id ctx.predecessors |> or_default IntSet.empty in
        let new_phis, ctx =
          List.fold_left
            (fun (phis, ctx) var_id ->
              let phi_id, ctx = alloc_instr_id ctx in
              let phi =
                {
                  phi_id;
                  phi_dest = new_value var_id;
                  phi_incoming =
                    IntSet.to_seq preds |> Seq.map (fun p -> (p, new_value var_id)) |> IntMap.of_seq;
                }
              in
              (phi :: phis, ctx))
            ([], ctx) vars
        in
        (IntMap.add bb_id { bb with bb_phis = bb.bb_phis @ List.rev new_phis } blocks, ctx))
      func.func_blocks (IntMap.empty, ctx)
  in
  ({ func with func_blocks }, ctx)

type rename_value_context = {
  counts : int IntMap.t;
  stacks : int list IntMap.t;
}

let new_rename_value_context_with (init_vals : value list) : rename_value_context =
  let counts = List.to_seq init_vals |> Seq.map (fun (id, _) -> (id, 0)) |> IntMap.of_seq in
  let stacks = List.to_seq init_vals |> Seq.map (fun (id, _) -> (id, [ 0 ])) |> IntMap.of_seq in
  { counts; stacks }

let versioned_value_of_id (id : int) (rctx : rename_value_context) : value =
  match IntMap.find_opt id rctx.stacks with
  | Some (v :: _) -> (id, v)
  | _ -> (id, 0)

let versioned_value (v : value) (rctx : rename_value_context) : value =
  versioned_value_of_id (fst v) rctx

let versioned_operand (op : operand) (rctx : rename_value_context) : operand =
  match op with
  | Value (id, _) -> Value (versioned_value_of_id id rctx)
  | _ -> op

let look_up_value_id (id : int) (rctx : rename_value_context) : int * int list =
  let max_v = IntMap.find_opt id rctx.counts |> or_default 0 in
  let stack = IntMap.find_opt id rctx.stacks |> or_default [ 0 ] in
  (max_v, stack)

let bump_version_for (id : int) (rctx : rename_value_context) : int * rename_value_context =
  let v, stack = look_up_value_id id rctx in
  let v' = v + 1 in
  let counts = IntMap.add id v' rctx.counts and stacks = IntMap.add id (v' :: stack) rctx.stacks in
  (v', { counts; stacks })

let rewrite_instr_def_into (new_def : value) = function
  | BinOp (id, _, op, s1, s2) -> BinOp (id, new_def, op, s1, s2)
  | FBinOp (id, _, op, s1, s2) -> FBinOp (id, new_def, op, s1, s2)
  | UnaryOp (id, _, op, s) -> UnaryOp (id, new_def, op, s)
  | FUnaryOp (id, _, op, s) -> FUnaryOp (id, new_def, op, s)
  | Move (id, _, s) -> Move (id, new_def, s)
  | Itf (id, _, s) -> Itf (id, new_def, s)
  | Fti (id, _, s) -> Fti (id, new_def, s)
  | Call (id, _, f, args) -> Call (id, new_def, f, args)
  | Alloca (id, _, count) -> Alloca (id, new_def, count)
  | Load (id, _, b, indices) -> Load (id, new_def, b, indices)
  | Store _ as instr -> instr

let rewrite_instr_uses (rctx : rename_value_context) = function
  | BinOp (id, d, op, s1, s2) ->
      BinOp (id, d, op, versioned_operand s1 rctx, versioned_operand s2 rctx)
  | FBinOp (id, d, op, s1, s2) ->
      FBinOp (id, d, op, versioned_operand s1 rctx, versioned_operand s2 rctx)
  | UnaryOp (id, d, op, s) -> UnaryOp (id, d, op, versioned_operand s rctx)
  | FUnaryOp (id, d, op, s) -> FUnaryOp (id, d, op, versioned_operand s rctx)
  | Move (id, d, s) -> Move (id, d, versioned_operand s rctx)
  | Itf (id, d, s) -> Itf (id, d, versioned_operand s rctx)
  | Fti (id, d, s) -> Fti (id, d, versioned_operand s rctx)
  | Call (id, d, f, args) -> Call (id, d, f, List.map (fun a -> versioned_operand a rctx) args)
  | Alloca _ as instr -> instr
  | Load (id, d, mem, indices) ->
      let mem =
        match mem with
        | LocalArray v -> LocalArray (versioned_value v rctx)
        | _ -> mem
      in
      Load (id, d, mem, List.map (fun idx -> versioned_operand idx rctx) indices)
  | Store (id, mem, indices, src) ->
      let mem =
        match mem with
        | LocalArray v -> LocalArray (versioned_value v rctx)
        | _ -> mem
      in
      Store
        ( id,
          mem,
          List.map (fun idx -> versioned_operand idx rctx) indices,
          versioned_operand src rctx )

let rewrite_terminator_uses (rctx : rename_value_context) = function
  | Br (id, cond, t, f) -> Br (id, versioned_operand cond rctx, t, f)
  | Return (id, Some op) -> Return (id, Some (versioned_operand op rctx))
  | t -> t

(* Algorithm 19.7, "Modern Compiler Implementation in C", Appel. *)
let rename_value (func : func) (ctx : build_ssa_context) : func * build_ssa_context =
  let rec rename_block n blocks rctx =
    (* for each statement S in block n *)
    let bb = IntMap.find n blocks in
    (*
      1. Phis (only care about defs)
        for each definition of some variable a in S
    *)
    let bb_phis, rctx =
      List.fold_left
        (fun (phis, rctx) phi ->
          let a, _ = phi.phi_dest in
          let v', rctx = bump_version_for a rctx in
          ({ phi with phi_dest = (a, v') } :: phis, rctx))
        ([], rctx) bb.bb_phis
    in
    let bb_phis = List.rev bb_phis in
    (*
      2. Other instructions
        for each use of some variable x in S
        for each definition of some variable a in S
    *)
    let bb_code, rctx =
      List.fold_left
        (fun (code, rctx) s ->
          let s = rewrite_instr_uses rctx s in
          match def_id_of s with
          | Some a ->
              let v', rctx = bump_version_for a rctx in
              let s = rewrite_instr_def_into (a, v') s in
              (s :: code, rctx)
          | None -> (s :: code, rctx))
        ([], rctx) bb.bb_code
    in
    let bb_code = List.rev bb_code in
    (*
      3. Terminators
        for each use of some variable x in S
    *)
    let bb_term = rewrite_terminator_uses rctx bb.bb_term in
    let blocks = IntMap.add n { bb with bb_phis; bb_code; bb_term } blocks in
    (* for each successor Y of block n *)
    let succs = IntMap.find_opt n ctx.successors |> or_default IntSet.empty in
    let blocks =
      IntSet.fold
        (fun y blks ->
          let y_bb =
            IntMap.find_opt y blks |> or_else (fun () -> internal_error "Successor block not found")
          in
          (* for each φ-function in Y *)
          let phis' =
            List.map
              (fun phi ->
                let phi_id = id_of_value phi.phi_dest in
                let incoming = versioned_value_of_id phi_id rctx in
                { phi with phi_incoming = IntMap.add n incoming phi.phi_incoming })
              y_bb.bb_phis
          in
          IntMap.add y { y_bb with bb_phis = phis' } blks)
        succs blocks
    in
    (* for each child X of n *)
    let children = IntMap.find_opt n ctx.dom_tree |> or_default IntSet.empty in
    (*
      This is for fast restoration of stack changes, so this is no longer necessary:
      > for each definition of some variable a in the original S
      >   pop Stack[a]
    *)
    let stacks = rctx.stacks in
    IntSet.fold
      (fun x (blks, rctx) ->
        let rctx = { rctx with stacks } in
        rename_block x blks rctx)
      children (blocks, rctx)
  in
  let rctx = new_rename_value_context_with func.func_params in
  let blocks, _ = rename_block func.func_entry_block func.func_blocks rctx in
  ({ func with func_blocks = blocks }, ctx)

let build_ssa (program : Tac.tac_program) (ctx : build_ssa_context) : program =
  let program, ctx = build_cfg program ctx in
  let funcs, _ =
    List.fold_left
      (fun (funcs, ctx) func ->
        let ctx = compute_dom_frontier func ctx in
        let ctx = compute_loop_props func ctx in
        let func, ctx = insert_phi func ctx in
        let func, ctx = rename_value func ctx in
        (func :: funcs, ctx))
      ([], ctx) program.functions
  in
  (* Use recompute_loop_props instead of manually setting fields *)
  let program = { program with functions = List.rev funcs } |> reset_loop_props in
  recompute_loop_props program
