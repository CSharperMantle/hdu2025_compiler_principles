module StringMap = Map.Make (String)
module IntMap = Map.Make (Int)

module AggResult = struct
  type ('a, 'b) agg_result = 'a * 'b list

  let agg_ok (x : 'a) : ('a, 'b) agg_result = (x, [])
  let agg_error (msg : string) (x : 'a) : ('a, 'b) agg_result = (x, [ msg ])

  let bind ((x, errs1) : ('a, 'b) agg_result) (f : 'a -> ('c, 'b) agg_result) : ('c, 'b) agg_result
      =
    let y, errs2 = f x in
    (y, errs1 @ errs2)

  let agg_is_ok ((_, errs) : ('a, 'b) agg_result) : bool = List.is_empty errs

  let agg_to_result ((x, errs) : ('a, 'b) agg_result) : ('a, 'b list) result =
    if agg_is_ok (x, errs) then Ok x else Error errs

  module Syntax = struct
    let ( let* ) = bind
  end
end

let indent_single (line : string) : string = "  " ^ line
let indent (lines : string list) : string list = List.map (fun l -> "  " ^ l) lines
let indent_seq (lines : string Seq.t) : string Seq.t = Seq.map (fun l -> "  " ^ l) lines
let internal_error (msg : string) : 'a = failwith (Printf.sprintf "Internal error: %s" msg)

let map_or (f : 'a -> 'b) (default : 'b) (opt : 'a option) : 'b =
  match opt with
  | Some v -> f v
  | None -> default
