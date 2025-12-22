module StringMap = Map.Make (String)
module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

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

type source_location = {
  lineno : int;
  colno : int;
}

let split_position (pos : Lexing.position) : source_location =
  { lineno = pos.pos_lnum; colno = pos.pos_cnum - pos.pos_bol + 1 }

let indent_single (line : string) : string = "\t" ^ line
let indent (lines : string list) : string list = List.map (fun l -> "\t" ^ l) lines
let indent_seq (lines : string Seq.t) : string Seq.t = Seq.map (fun l -> "\t" ^ l) lines
let internal_error (msg : string) : 'a = failwith (Printf.sprintf "Internal error: %s" msg)
let todo () : 'a = failwith "TODO: todo()"

let map_or_default (f : 'a -> 'b) (default : 'b) (opt : 'a option) : 'b =
  match opt with
  | Some v -> f v
  | None -> default

let or_default (default : 'a) (opt : 'a option) : 'a = Option.value ~default opt

let or_else (f : unit -> 'a) (opt : 'a option) : 'a =
  match opt with
  | Some v -> v
  | None -> f ()

let tl_or (default : 'a list) = function
  | [] -> default
  | _ :: l -> l

let prepend_int_or_singleton_list (k : int) (v : 'a) (map : 'a list IntMap.t) : 'a list IntMap.t =
  let existing = IntMap.find_opt k map |> or_default [] in
  IntMap.add k (v :: existing) map

let union_int_or_singleton_int_set (k : int) (v : int) (map : IntSet.t IntMap.t) : IntSet.t IntMap.t
    =
  let existing = IntMap.find_opt k map |> or_default IntSet.empty in
  IntMap.add k (IntSet.add v existing) map

let string_of_list (f : 'a -> string) (l : 'a list) : string =
  Printf.sprintf "[%s]" (List.map f l |> String.concat "; ")

let string_of_int_list (l : int list) : string = string_of_list string_of_int l
let maybe_add_newline (s : string) : string = if s <> "" then s ^ "\n" else ""

let rec list3_snd = function
  | [] -> []
  | (_, y, _) :: l -> y :: list3_snd l
