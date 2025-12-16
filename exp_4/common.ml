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

let indent_single (line : string) : string = "\t" ^ line
let indent (lines : string list) : string list = List.map (fun l -> "\t" ^ l) lines
let indent_seq (lines : string Seq.t) : string Seq.t = Seq.map (fun l -> "\t" ^ l) lines
let internal_error (msg : string) : 'a = failwith (Printf.sprintf "Internal error: %s" msg)
let todo () : 'a = failwith "TODO: todo()"

let map_or (f : 'a -> 'b) (default : 'b) (opt : 'a option) : 'b =
  match opt with
  | Some v -> f v
  | None -> default

let tl_or (default : 'a list) = function
  | [] -> default
  | _ :: l -> l

let string_of_list (f : 'a -> string) (l : 'a list) : string =
  Printf.sprintf "[%s]" (List.map f l |> String.concat "; ")

let string_of_int_list (l : int list) : string = string_of_list string_of_int l
let maybe_add_newline (s : string) : string = if s <> "" then s ^ "\n" else ""
