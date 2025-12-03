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

  module Syntax = struct
    let ( let* ) = bind
  end
end
