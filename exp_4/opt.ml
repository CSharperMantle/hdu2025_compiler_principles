module Const_prop = struct
  open Common

  module ValueMap = Map.Make (struct
    type t = Ssa.value

    let compare = compare
  end)

  type const_val =
    | CInt of int
    | CFloat of float

  let ssa_operand_of = function
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

  let def_value_of = function
    | Ssa.BinOp (d, _, _, _)
    | Ssa.FBinOp (d, _, _, _)
    | Ssa.UnaryOp (d, _, _)
    | Ssa.FUnaryOp (d, _, _)
    | Ssa.Move (d, _)
    | Ssa.Itf (d, _)
    | Ssa.Fti (d, _)
    | Ssa.Call (d, _, _)
    | Ssa.Alloca (d, _)
    | Ssa.Load (d, _, _) -> Some d
    | Ssa.Store _ -> None

  let collect_const_from_instr (map : const_val ValueMap.t) (instr : Ssa.instr) :
      const_val ValueMap.t =
    match instr with
    | Ssa.Move (d, s) -> get_const s map |> map_or_default (fun c -> ValueMap.add d c map) map
    | Ssa.BinOp (d, op, s1, s2) -> (
        match (get_const s1 map, get_const s2 map) with
        | Some c1, Some c2 ->
            eval_bin_op op c1 c2 |> map_or_default (fun res -> ValueMap.add d res map) map
        | _ -> map)
    | Ssa.FBinOp (d, op, s1, s2) -> (
        match (get_const s1 map, get_const s2 map) with
        | Some c1, Some c2 ->
            eval_bin_op op c1 c2 |> map_or_default (fun res -> ValueMap.add d res map) map
        | _ -> map)
    | Ssa.UnaryOp (d, op, s) ->
        get_const s map
        |> map_or_default
             (fun c -> eval_unary_op op c |> map_or_default (fun res -> ValueMap.add d res map) map)
             map
    | Ssa.FUnaryOp (d, op, s) ->
        get_const s map
        |> map_or_default
             (fun c -> eval_unary_op op c |> map_or_default (fun res -> ValueMap.add d res map) map)
             map
    | Ssa.Itf (d, s) ->
        get_const s map
        |> map_or_default
             (fun c ->
               match c with
               | CInt i -> ValueMap.add d (CFloat (float_of_int i)) map
               | _ -> map)
             map
    | Ssa.Fti (d, s) ->
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
           | CInt i ->
               let changed = op <> Const i in
               (Ssa.Const i, changed)
           | CFloat f ->
               let changed = op <> ConstFloat f in
               (Ssa.ConstFloat f, changed))
         (op, false)

  let rewrite_instr (map : const_val ValueMap.t) (instr : Ssa.instr) : Ssa.instr * bool =
    let rewrite_operand = rewrite_operand map in
    let instr, changed1 =
      match instr with
      | Ssa.BinOp (d, op, s1, s2) ->
          let s1', c1 = rewrite_operand s1 and s2', c2 = rewrite_operand s2 in
          (Ssa.BinOp (d, op, s1', s2'), c1 || c2)
      | Ssa.FBinOp (d, op, s1, s2) ->
          let s1', c1 = rewrite_operand s1 and s2', c2 = rewrite_operand s2 in
          (Ssa.FBinOp (d, op, s1', s2'), c1 || c2)
      | Ssa.UnaryOp (d, op, s) ->
          let s', c = rewrite_operand s in
          (Ssa.UnaryOp (d, op, s'), c)
      | Ssa.FUnaryOp (d, op, s) ->
          let s', c = rewrite_operand s in
          (Ssa.FUnaryOp (d, op, s'), c)
      | Ssa.Move (d, s) ->
          let s', c = rewrite_operand s in
          (Ssa.Move (d, s'), c)
      | Ssa.Itf (d, s) ->
          let s', c = rewrite_operand s in
          (Ssa.Itf (d, s'), c)
      | Ssa.Fti (d, s) ->
          let s', c = rewrite_operand s in
          (Ssa.Fti (d, s'), c)
      | Ssa.Call (d, f, args) ->
          let args', changes = List.split (List.map rewrite_operand args) in
          (Ssa.Call (d, f, args'), List.exists Fun.id changes)
      | Ssa.Load (d, mem, indices) ->
          let indices', changes = List.split (List.map rewrite_operand indices) in
          (Ssa.Load (d, mem, indices'), List.exists Fun.id changes)
      | Ssa.Store (mem, indices, src) ->
          let indices', changes1 = List.split (List.map rewrite_operand indices) in
          let src', c2 = rewrite_operand src in
          (Ssa.Store (mem, indices', src'), List.exists Fun.id changes1 || c2)
      | _ -> (instr, false)
    in
    def_value_of instr
    |> map_or_default
         (fun v ->
           ValueMap.find_opt v map
           |> map_or_default
                (fun c ->
                  let const_op = ssa_operand_of c in
                  match instr with
                  | Move (_, src) when src = const_op -> (instr, changed1)
                  | BinOp _ | FBinOp _ | UnaryOp _ | FUnaryOp _ | Itf _ | Fti _ ->
                      (Move (v, const_op), true)
                  | _ -> (instr, changed1))
                (instr, changed1))
         (instr, changed1)

  let rewrite_terminator (map : const_val ValueMap.t) (term : Ssa.terminator) :
      Ssa.terminator * bool =
    match term with
    | Ssa.Br (cond, t, f) ->
        let cond', changed = rewrite_operand map cond in
        (Ssa.Br (cond', t, f), changed)
    | Ssa.Return op ->
        let op', changed =
          op
          |> map_or_default
               (fun o ->
                 let o', c = rewrite_operand map o in
                 (Some o', c))
               (None, false)
        in
        (Ssa.Return op', changed)
    | _ -> (term, false)

  let simple_const_prop_func (f : Ssa.func) : Ssa.func =
    let rec aux f_curr =
      let map = collect_constants f_curr in
      let blocks', changed =
        IntMap.fold
          (fun bb_id bb (blocks, changed_acc) ->
            let code, code_changes = List.split (List.map (rewrite_instr map) bb.Ssa.bb_code) in
            let term, term_changed = rewrite_terminator map bb.Ssa.bb_term in
            let bb_changed = List.exists Fun.id code_changes || term_changed in
            let new_bb = { bb with Ssa.bb_code = code; Ssa.bb_term = term } in
            (IntMap.add bb_id new_bb blocks, changed_acc || bb_changed))
          f_curr.Ssa.func_blocks (IntMap.empty, false)
      in
      if changed then aux { f_curr with func_blocks = blocks' } else f_curr
    in
    aux f

  let simple_const_prop (p : Ssa.program) : Ssa.program =
    { p with functions = List.map simple_const_prop_func p.functions }
end
