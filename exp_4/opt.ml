open Common

module ValueSet = Set.Make (struct
  type t = Ssa.value

  let compare = compare
end)

module ValueMap = Map.Make (struct
  type t = Ssa.value

  let compare = compare
end)

let def_value_of_opt (instr : Ssa.instr) : Ssa.value option =
  match instr with
  | BinOp (_, d, _, _, _)
  | FBinOp (_, d, _, _, _)
  | UnaryOp (_, d, _, _)
  | FUnaryOp (_, d, _, _)
  | Move (_, d, _)
  | Itf (_, d, _)
  | Fti (_, d, _)
  | Call (_, d, _, _)
  | Alloca (_, d, _)
  | Load (_, d, _, _) -> Some d
  | Store _ -> None

let uses_of_operand (op : Ssa.operand) : Ssa.value list =
  match op with
  | Value v -> [ v ]
  | Const _ | ConstFloat _ -> []

let uses_of_mem (m : Ssa.mem_loc) : Ssa.value list =
  match m with
  | LocalArray v -> [ v ]
  | GlobalArray _ | GlobalScalar _ -> []

let uses_of_instr (instr : Ssa.instr) : Ssa.value list =
  match instr with
  | BinOp (_, _, _, s1, s2) | FBinOp (_, _, _, s1, s2) -> uses_of_operand s1 @ uses_of_operand s2
  | UnaryOp (_, _, _, s) | FUnaryOp (_, _, _, s) | Move (_, _, s) | Itf (_, _, s) | Fti (_, _, s) ->
      uses_of_operand s
  | Call (_, _, _, args) -> List.map uses_of_operand args |> List.concat
  | Alloca _ -> []
  | Load (_, _, m, indices) -> uses_of_mem m @ (List.map uses_of_operand indices |> List.concat)
  | Store (_, m, indices, s) ->
      uses_of_mem m @ (List.map uses_of_operand indices |> List.concat) @ uses_of_operand s

let uses_of_phi (phi : Ssa.phi) : Ssa.value list = IntMap.bindings phi.phi_incoming |> List.map snd

let uses_of_terminator (term : Ssa.terminator) : Ssa.value list =
  match term with
  | Br (_, cond, _, _) -> uses_of_operand cond
  | Return (_, Some op) -> uses_of_operand op
  | Return (_, None) | Jump _ -> []

(*
  Generic function for rewriting all uses in a instruction.
  Returns:
    * Rewritten instruction
    * If rewriting has happened
*)
let generic_rewrite_uses_of_instr (rewrite : Ssa.operand -> Ssa.operand * bool) (instr : Ssa.instr)
    : Ssa.instr * bool =
  match instr with
  | Ssa.BinOp (id, d, op, s1, s2) ->
      let s1', c1 = rewrite s1 and s2', c2 = rewrite s2 in
      (Ssa.BinOp (id, d, op, s1', s2'), c1 || c2)
  | Ssa.FBinOp (id, d, op, s1, s2) ->
      let s1', c1 = rewrite s1 and s2', c2 = rewrite s2 in
      (Ssa.FBinOp (id, d, op, s1', s2'), c1 || c2)
  | Ssa.UnaryOp (id, d, op, s) ->
      let s', c = rewrite s in
      (Ssa.UnaryOp (id, d, op, s'), c)
  | Ssa.FUnaryOp (id, d, op, s) ->
      let s', c = rewrite s in
      (Ssa.FUnaryOp (id, d, op, s'), c)
  | Ssa.Move (id, d, s) ->
      let s', c = rewrite s in
      (Ssa.Move (id, d, s'), c)
  | Ssa.Itf (id, d, s) ->
      let s', c = rewrite s in
      (Ssa.Itf (id, d, s'), c)
  | Ssa.Fti (id, d, s) ->
      let s', c = rewrite s in
      (Ssa.Fti (id, d, s'), c)
  | Ssa.Call (id, d, f, args) ->
      let args', changes = List.split (List.map rewrite args) in
      (Ssa.Call (id, d, f, args'), List.exists Fun.id changes)
  | Ssa.Load (id, d, mem, indices) ->
      let indices', changes = List.split (List.map rewrite indices) in
      (Ssa.Load (id, d, mem, indices'), List.exists Fun.id changes)
  | Ssa.Store (id, mem, indices, src) ->
      let indices', changes1 = List.split (List.map rewrite indices) in
      let src', c2 = rewrite src in
      (Ssa.Store (id, mem, indices', src'), List.exists Fun.id changes1 || c2)
  | Ssa.Alloca _ -> (instr, false)

let generic_rewrite_uses_of_terminator (rewrite : Ssa.operand -> Ssa.operand * bool)
    (term : Ssa.terminator) : Ssa.terminator * bool =
  match term with
  | Ssa.Br (id, cond, t, f) ->
      let cond', changed = rewrite cond in
      (Ssa.Br (id, cond', t, f), changed)
  | Ssa.Return (_, None) -> (term, false)
  | Ssa.Return (id, Some op) ->
      let op', changed = rewrite op in
      (Ssa.Return (id, Some op'), changed)
  | Ssa.Jump _ -> (term, false)

let generic_rewrite_uses_of_phi (rewrite : Ssa.value -> Ssa.value * bool) (phi : Ssa.phi) :
    Ssa.phi * bool =
  let incoming', changed =
    IntMap.fold
      (fun bb_id v (acc, changed) ->
        let v', v_changed = rewrite v in
        (IntMap.add bb_id v' acc, changed || v_changed))
      phi.Ssa.phi_incoming (IntMap.empty, false)
  in
  ({ phi with phi_incoming = incoming' }, changed)

type opt_pass = Ssa.program -> Ssa.program * bool

module Const_prop = struct
  type const_val =
    | CInt of int
    | CFloat of float

  let operand_of = function
    | CInt i -> Ssa.Const i
    | CFloat f -> Ssa.ConstFloat f

  let get_const (op : Ssa.operand) (map : const_val ValueMap.t) : const_val option =
    match op with
    | Const i -> Some (CInt i)
    | ConstFloat f -> Some (CFloat f)
    | Value v -> ValueMap.find_opt v map

  let eval_bin_op (op : Ast.bin_op) (v1 : const_val) (v2 : const_val) : const_val option =
    match (v1, v2, op) with
    | CInt i1, CInt i2, Ast.Add -> Some (CInt (i1 + i2))
    | CInt i1, CInt i2, Ast.Sub -> Some (CInt (i1 - i2))
    | CInt i1, CInt i2, Ast.Mul -> Some (CInt (i1 * i2))
    | CInt i1, CInt i2, Ast.Div -> if i2 <> 0 then Some (CInt (i1 / i2)) else None
    | CInt i1, CInt i2, Ast.Mod -> if i2 <> 0 then Some (CInt (i1 mod i2)) else None
    | CInt i1, CInt i2, Ast.Lt -> Some (CInt (Bool.to_int (i1 < i2)))
    | CInt i1, CInt i2, Ast.Leq -> Some (CInt (Bool.to_int (i1 <= i2)))
    | CInt i1, CInt i2, Ast.Gt -> Some (CInt (Bool.to_int (i1 > i2)))
    | CInt i1, CInt i2, Ast.Geq -> Some (CInt (Bool.to_int (i1 >= i2)))
    | CInt i1, CInt i2, Ast.Eq -> Some (CInt (Bool.to_int (i1 = i2)))
    | CInt i1, CInt i2, Ast.Neq -> Some (CInt (Bool.to_int (i1 <> i2)))
    | CInt i1, CInt i2, Ast.And -> Some (CInt (Bool.to_int (i1 <> 0 && i2 <> 0)))
    | CInt i1, CInt i2, Ast.Or -> Some (CInt (Bool.to_int (i1 <> 0 || i2 <> 0)))
    | CFloat f1, CFloat f2, Ast.Add -> Some (CFloat (f1 +. f2))
    | CFloat f1, CFloat f2, Ast.Sub -> Some (CFloat (f1 -. f2))
    | CFloat f1, CFloat f2, Ast.Mul -> Some (CFloat (f1 *. f2))
    | CFloat f1, CFloat f2, Ast.Div -> Some (CFloat (f1 /. f2))
    | CFloat f1, CFloat f2, Ast.Lt -> Some (CInt (Bool.to_int (f1 < f2)))
    | CFloat f1, CFloat f2, Ast.Leq -> Some (CInt (Bool.to_int (f1 <= f2)))
    | CFloat f1, CFloat f2, Ast.Gt -> Some (CInt (Bool.to_int (f1 > f2)))
    | CFloat f1, CFloat f2, Ast.Geq -> Some (CInt (Bool.to_int (f1 >= f2)))
    | CFloat f1, CFloat f2, Ast.Eq -> Some (CInt (Bool.to_int (f1 = f2)))
    | CFloat f1, CFloat f2, Ast.Neq -> Some (CInt (Bool.to_int (f1 <> f2)))
    | _ -> None

  let eval_unary_op (op : Ast.unary_op) (v : const_val) : const_val option =
    match (v, op) with
    | v, Ast.Pos -> Some v
    | CInt i, Ast.Neg -> Some (CInt (-i))
    | CInt i, Ast.Not -> Some (CInt (Bool.to_int (i = 0)))
    | CFloat f, Ast.Neg -> Some (CFloat (-.f))
    | _ -> None

  let collect_const_from_instr (map : const_val ValueMap.t) (instr : Ssa.instr) :
      const_val ValueMap.t =
    match instr with
    | Ssa.Move (_, d, s) -> get_const s map |> map_or_default (fun c -> ValueMap.add d c map) map
    | Ssa.BinOp (_, d, op, s1, s2) -> (
        match (get_const s1 map, get_const s2 map) with
        | Some c1, Some c2 ->
            eval_bin_op op c1 c2 |> map_or_default (fun res -> ValueMap.add d res map) map
        | _ -> map)
    | Ssa.FBinOp (_, d, op, s1, s2) -> (
        match (get_const s1 map, get_const s2 map) with
        | Some c1, Some c2 ->
            eval_bin_op op c1 c2 |> map_or_default (fun res -> ValueMap.add d res map) map
        | _ -> map)
    | Ssa.UnaryOp (_, d, op, s) ->
        get_const s map
        |> map_or_default
             (fun c -> eval_unary_op op c |> map_or_default (fun res -> ValueMap.add d res map) map)
             map
    | Ssa.FUnaryOp (_, d, op, s) ->
        get_const s map
        |> map_or_default
             (fun c -> eval_unary_op op c |> map_or_default (fun res -> ValueMap.add d res map) map)
             map
    | Ssa.Itf (_, d, s) ->
        get_const s map
        |> map_or_default
             (fun c ->
               match c with
               | CInt i -> ValueMap.add d (CFloat (float_of_int i)) map
               | _ -> map)
             map
    | Ssa.Fti (_, d, s) ->
        get_const s map
        |> map_or_default
             (fun c ->
               match c with
               | CFloat f -> ValueMap.add d (CInt (int_of_float f)) map
               | _ -> map)
             map
    | _ -> map

  let collect_const_from_phi (map : const_val ValueMap.t) (phi : Ssa.phi) : const_val ValueMap.t =
    let vals =
      IntMap.bindings phi.Ssa.phi_incoming |> List.map (fun (_, v) -> get_const (Value v) map)
    in
    match vals with
    | [] -> map
    | [ single_val ] ->
        (* %dest <- Phi [%src] can always be elided *)
        single_val |> map_or_default (fun c -> ValueMap.add phi.phi_dest c map) map
    | hd :: tl ->
        if List.for_all (fun v -> v = hd) tl then
          hd |> map_or_default (fun c -> ValueMap.add phi.phi_dest c map) map
        else map

  let collect_constants (f : Ssa.func) : const_val ValueMap.t =
    IntMap.fold
      (fun _ bb map ->
        let map = List.fold_left collect_const_from_phi map bb.Ssa.bb_phis in
        List.fold_left collect_const_from_instr map bb.Ssa.bb_code)
      f.Ssa.func_blocks ValueMap.empty

  let rewrite_operand (map : const_val ValueMap.t) (op : Ssa.operand) : Ssa.operand * bool =
    get_const op map
    |> map_or_default
         (fun c ->
           match c with
           | CInt i -> (Ssa.Const i, op <> Const i)
           | CFloat f -> (Ssa.ConstFloat f, op <> ConstFloat f))
         (op, false)

  let rewrite_instr (map : const_val ValueMap.t) (instr : Ssa.instr) : Ssa.instr * bool =
    let rewrite_operand = rewrite_operand map in
    let instr, changed1 = generic_rewrite_uses_of_instr rewrite_operand instr in
    def_value_of_opt instr
    |> map_or_default
         (fun v ->
           ValueMap.find_opt v map
           |> map_or_default
                (fun c ->
                  let const_op = operand_of c in
                  match instr with
                  | Move (_, _, src) when src = const_op -> (instr, changed1)
                  | BinOp (id, d, _, _, _)
                  | FBinOp (id, d, _, _, _)
                  | UnaryOp (id, d, _, _)
                  | FUnaryOp (id, d, _, _)
                  | Itf (id, d, _)
                  | Fti (id, d, _) -> (Move (id, d, const_op), true)
                  | _ -> (instr, changed1))
                (instr, changed1))
         (instr, changed1)

  let rewrite_terminator (map : const_val ValueMap.t) (term : Ssa.terminator) :
      Ssa.terminator * bool =
    let rewrite_operand = rewrite_operand map in
    generic_rewrite_uses_of_terminator rewrite_operand term

  let rewrite_phi (map : const_val ValueMap.t) (phi : Ssa.phi) : Ssa.phi option * bool =
    if IntMap.cardinal phi.Ssa.phi_incoming = 1 then (None, true)
    else
      let incomings =
        IntMap.bindings phi.Ssa.phi_incoming |> List.map (fun (_, v) -> get_const (Value v) map)
      in
      match incomings with
      | [] -> (Some phi, false)
      | hd :: tl ->
          if List.for_all (fun c -> c = hd) tl && Option.is_some hd then
            (* All incomings are actually the same constant! *)
            (None, true)
          else (Some phi, false)

  let simple_const_prop_func (f : Ssa.func) : Ssa.func * bool =
    let rec aux f_curr =
      let map = collect_constants f_curr in
      let blocks', changed =
        IntMap.fold
          (fun bb_id bb (blocks, changed_acc) ->
            let phis, phi_changed =
              List.fold_left
                (fun (phis, any_changed) phi ->
                  let phi_opt, changed = rewrite_phi map phi in
                  match phi_opt with
                  | Some p -> (p :: phis, any_changed || changed)
                  | None -> (phis, true))
                ([], false) bb.Ssa.bb_phis
            in
            let phis = List.rev phis in
            let code, code_changes = List.split (List.map (rewrite_instr map) bb.Ssa.bb_code) in
            let term, term_changed = rewrite_terminator map bb.Ssa.bb_term in
            let bb_changed = phi_changed || List.exists Fun.id code_changes || term_changed in
            let new_bb = { bb with Ssa.bb_phis = phis; Ssa.bb_code = code; Ssa.bb_term = term } in
            (IntMap.add bb_id new_bb blocks, changed_acc || bb_changed))
          f_curr.Ssa.func_blocks (IntMap.empty, false)
      in
      if changed then aux { f_curr with func_blocks = blocks' } else (f_curr, false)
    in
    aux f

  let simple_const_prop (p : Ssa.program) : Ssa.program * bool =
    let funcs_and_changes = List.map simple_const_prop_func p.functions in
    let funcs = List.map fst funcs_and_changes in
    let changed = List.exists snd funcs_and_changes in
    ({ p with functions = funcs }, changed)
end

module Copy_prop = struct
  let collect_copies (f : Ssa.func) : Ssa.operand ValueMap.t =
    IntMap.fold
      (fun _ bb map ->
        (* Phis *)
        let map =
          List.fold_left
            (fun acc phi ->
              if IntMap.cardinal phi.Ssa.phi_incoming = 1 then
                (* %dest <- Phi [%src] is effectively a Move *)
                let incoming_val = snd (List.hd (IntMap.bindings phi.Ssa.phi_incoming)) in
                ValueMap.add phi.Ssa.phi_dest (Ssa.Value incoming_val) acc
              else acc)
            map bb.Ssa.bb_phis
        in
        (* Moves *)
        List.fold_left
          (fun acc instr ->
            match instr with
            | Ssa.Move (_, d, s) -> ValueMap.add d s acc
            | _ -> acc)
          map bb.Ssa.bb_code)
      f.Ssa.func_blocks ValueMap.empty

  let rec resolve_operand (map : Ssa.operand ValueMap.t) (op : Ssa.operand) : Ssa.operand =
    match op with
    | Ssa.Value v -> (
        match ValueMap.find_opt v map with
        | Some resolved -> resolve_operand map resolved
        | None -> op)
    | Ssa.Const _ | Ssa.ConstFloat _ -> op

  let propagate_in_operand (map : Ssa.operand ValueMap.t) (op : Ssa.operand) : Ssa.operand * bool =
    let resolved = resolve_operand map op in
    (resolved, resolved <> op)

  let propagate_in_instr (map : Ssa.operand ValueMap.t) (instr : Ssa.instr) : Ssa.instr * bool =
    let prop = propagate_in_operand map in
    generic_rewrite_uses_of_instr prop instr

  let propagate_in_phi (map : Ssa.operand ValueMap.t) (phi : Ssa.phi) : Ssa.phi * bool =
    let rec find_root_value (op : Ssa.operand) : Ssa.operand option =
      match op with
      | Ssa.Value v -> (
          match ValueMap.find_opt v map with
          | Some op' -> (
              match find_root_value op' with
              | Some _ as result -> result
              | None -> Some op)
          | None -> Some op)
      | Ssa.Const _ | Ssa.ConstFloat _ -> None
    in
    let rewrite (v : Ssa.value) : Ssa.value * bool =
      match find_root_value (Ssa.Value v) with
      | Some (Ssa.Value v') -> (v', v' <> v)
      | Some _ -> internal_error "Non-value returned when rewriting Phi RHS"
      | None -> (v, false)
    in
    generic_rewrite_uses_of_phi rewrite phi

  let propagate_in_terminator (map : Ssa.operand ValueMap.t) (term : Ssa.terminator) :
      Ssa.terminator * bool =
    let prop = propagate_in_operand map in
    generic_rewrite_uses_of_terminator prop term

  let copy_prop_func (f : Ssa.func) : Ssa.func * bool =
    let rec aux f_curr =
      let map = collect_copies f_curr in
      if ValueMap.is_empty map then (f_curr, false)
      else
        let blocks', changed =
          IntMap.fold
            (fun bb_id bb (blocks, changed_acc) ->
              let phis, phi_changes = List.split (List.map (propagate_in_phi map) bb.Ssa.bb_phis) in
              let code, code_changes =
                List.split (List.map (propagate_in_instr map) bb.Ssa.bb_code)
              in
              let term, term_changed = propagate_in_terminator map bb.Ssa.bb_term in
              let bb_changed =
                List.exists Fun.id phi_changes || List.exists Fun.id code_changes || term_changed
              in
              let new_bb = { bb with Ssa.bb_phis = phis; Ssa.bb_code = code; Ssa.bb_term = term } in
              (IntMap.add bb_id new_bb blocks, changed_acc || bb_changed))
            f_curr.Ssa.func_blocks (IntMap.empty, false)
        in
        if changed then aux { f_curr with func_blocks = blocks' } else (f_curr, false)
    in
    aux f

  let copy_prop (p : Ssa.program) : Ssa.program * bool =
    let funcs_and_changes = List.map copy_prop_func p.functions in
    let funcs = List.map fst funcs_and_changes in
    let changed = List.exists snd funcs_and_changes in
    ({ p with functions = funcs }, changed)
end

module Dead_code_elim = struct
  type definition =
    | DefInstr of Ssa.instr
    | DefPhi of Ssa.phi
    | DefParam

  let is_const_truthy (op : Ssa.operand) : bool =
    match op with
    | Ssa.Const v -> v <> 0
    | Ssa.ConstFloat v -> v <> 0.0
    | Ssa.Value _ -> false

  let is_const_falsy (op : Ssa.operand) : bool =
    match op with
    | Ssa.Const v -> v = 0
    | Ssa.ConstFloat v -> v = 0.0
    | Ssa.Value _ -> false

  let elide_branches (f : Ssa.func) : Ssa.func =
    let new_blocks =
      IntMap.map
        (fun bb ->
          match bb.Ssa.bb_term with
          | Ssa.Br (id, cond, tgt_truthy, tgt_falsy) ->
              if is_const_truthy cond then { bb with bb_term = Ssa.Jump (id, tgt_truthy) }
              else if is_const_falsy cond then { bb with bb_term = Ssa.Jump (id, tgt_falsy) }
              else bb
          | _ -> bb)
        f.Ssa.func_blocks
    in
    { f with func_blocks = new_blocks }

  let find_reachables (entry : int) (blocks : Ssa.basic_block IntMap.t) : IntSet.t =
    let rec dfs visited worklist =
      match worklist with
      | [] -> visited
      | bb_id :: rest -> (
          if IntSet.mem bb_id visited then dfs visited rest
          else
            let visited = IntSet.add bb_id visited in
            match IntMap.find_opt bb_id blocks with
            | None -> dfs visited rest
            | Some bb ->
                let succs =
                  match bb.Ssa.bb_term with
                  | Ssa.Jump (_, target) -> [ target ]
                  | Ssa.Br (_, _, true_target, false_target) -> [ true_target; false_target ]
                  | Ssa.Return _ -> []
                in
                dfs visited (succs @ rest))
    in
    dfs IntSet.empty [ entry ]

  let remove_unreachable_blocks (f : Ssa.func) : Ssa.func =
    let reachable = find_reachables f.Ssa.func_entry_block f.Ssa.func_blocks in
    let new_blocks = IntMap.filter (fun bb_id _ -> IntSet.mem bb_id reachable) f.func_blocks in
    (* Clean up Phis by removing incomings from unreachable blocks *)
    let new_blocks =
      IntMap.map
        (fun bb ->
          let bb_phis =
            List.map
              (fun phi ->
                let phi_incoming =
                  IntMap.filter (fun pred_id _ -> IntSet.mem pred_id reachable) phi.Ssa.phi_incoming
                in
                { phi with Ssa.phi_incoming })
              bb.Ssa.bb_phis
          in
          { bb with Ssa.bb_phis })
        new_blocks
    in
    { f with func_blocks = new_blocks }

  let dce_func (f : Ssa.func) : Ssa.func * bool =
    (* 1. Branch elision *)
    let f_after_branches = elide_branches f in
    let f_after_unreachable = remove_unreachable_blocks f_after_branches in
    let changed_1 = f_after_unreachable <> f in
    (* 2. Collect defs *)
    let defs =
      let map =
        List.to_seq f_after_unreachable.func_params
        |> Seq.map (fun p -> (p, DefParam))
        |> ValueMap.of_seq
      in
      IntMap.fold
        (fun _ bb map ->
          let map =
            List.fold_left
              (fun acc phi -> ValueMap.add phi.Ssa.phi_dest (DefPhi phi) acc)
              map bb.Ssa.bb_phis
          in
          List.fold_left
            (fun acc instr ->
              match def_value_of_opt instr with
              | Some d -> ValueMap.add d (DefInstr instr) acc
              | None -> acc)
            map bb.bb_code)
        f_after_unreachable.func_blocks map
    in
    (* 3. Find critical instrs (Store, Call, terminators) and initial live values *)
    let worklist, live =
      IntMap.fold
        (fun _ bb (wl, live) ->
          let wl, live =
            List.fold_left
              (fun (wl, live) instr ->
                match instr with
                | Ssa.Store _ | Ssa.Call _ ->
                    let uses = uses_of_instr instr in
                    let uses = List.filter (fun u -> not (ValueSet.mem u live)) uses in
                    (uses @ wl, List.fold_right ValueSet.add uses live)
                | _ -> (wl, live))
              (wl, live) bb.Ssa.bb_code
          in
          let uses = uses_of_terminator bb.Ssa.bb_term in
          let uses = List.filter (fun u -> not (ValueSet.mem u live)) uses in
          (uses @ wl, List.fold_right ValueSet.add uses live))
        f_after_unreachable.func_blocks ([], ValueSet.empty)
    in
    (* 4. Collect liveness *)
    let rec propagate wl live =
      match wl with
      | [] -> live
      | v :: rest -> (
          match ValueMap.find_opt v defs with
          | None -> propagate rest live
          | Some def ->
              let uses =
                match def with
                | DefInstr i -> uses_of_instr i
                | DefPhi p -> uses_of_phi p
                | DefParam -> []
              in
              let uses = List.filter (fun u -> not (ValueSet.mem u live)) uses in
              let live = List.fold_right ValueSet.add uses live in
              propagate (uses @ rest) live)
    in
    let liveness = propagate worklist live in
    (* 5. Sweep *)
    let new_blocks, changed_2 =
      IntMap.fold
        (fun bb_id bb (blocks, changed_acc) ->
          let bb_phis =
            List.filter (fun phi -> ValueSet.mem phi.Ssa.phi_dest liveness) bb.Ssa.bb_phis
          in
          let bb_code =
            List.filter
              (fun instr ->
                match (instr, def_value_of_opt instr) with
                | Ssa.Store _, _ | Ssa.Call _, _ -> true
                | _, Some d -> ValueSet.mem d liveness
                | _, None -> false)
              bb.bb_code
          in
          let bb_changed =
            List.length bb.Ssa.bb_phis <> List.length bb_phis
            || List.length bb.bb_code <> List.length bb_code
          in
          let new_bb = { bb with bb_phis; bb_code } in
          (IntMap.add bb_id new_bb blocks, changed_acc || bb_changed))
        f_after_unreachable.func_blocks (IntMap.empty, false)
    in
    ({ f_after_unreachable with func_blocks = new_blocks }, changed_1 || changed_2)

  let dead_code_elim (p : Ssa.program) : Ssa.program * bool =
    let funcs_and_changes = List.map dce_func p.functions in
    let funcs = List.map fst funcs_and_changes in
    let changed = List.exists snd funcs_and_changes in
    let p' = { p with functions = funcs } in
    let p'' = if changed then Ssa.reset_loop_props p' |> Ssa.recompute_loop_props else p' in
    (p'', changed)
end

type opt_pipe_stage = string * opt_pass

type opt_pipe_component =
  | Once of opt_pipe_stage
  | Fixedpoint of opt_pipe_stage list

let opt_pipe (initial : string * Ssa.program) (components : opt_pipe_component list) :
    (string * Ssa.program) list =
  let run_component prog component =
    match component with
    | Once (name, pass) ->
        let prog, _ = pass prog in
        [ (name, prog) ]
    | Fixedpoint stages ->
        let rec iterate prog iteration =
          let results', any_changed =
            List.fold_left
              (fun (results, changed_acc) (name, pass) ->
                let prog = if results = [] then prog else snd (List.hd results) in
                let prog, changed = pass prog
                and full_name = Printf.sprintf "%s %d" name (iteration + 1) in
                ((full_name, prog) :: results, changed_acc || changed))
              ([], false) stages
          in
          let results = List.rev results' in
          if any_changed then results @ iterate (snd (List.hd results')) (iteration + 1)
          else results
        in
        iterate prog 0
  in
  let _, results =
    List.fold_left
      (fun (prog, acc) component ->
        let new_results = run_component prog component in
        let last_prog = snd (List.hd (List.rev new_results)) in
        (last_prog, acc @ new_results))
      (snd initial, [])
      components
  in
  initial :: results

let default_opt_passes : opt_pipe_component list =
  [
    Fixedpoint
      [
        ("Simple Const Prop", Const_prop.simple_const_prop);
        ("Copy Prop", Copy_prop.copy_prop);
        ("Dead Code Elim", Dead_code_elim.dead_code_elim);
      ];
  ]
