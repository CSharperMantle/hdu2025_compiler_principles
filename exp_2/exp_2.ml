type token =
  | Char of char
  | Star
  | Plus
  | Pipe
  | LParen
  | RParen
  | Concat (* implicit *)

let tokenize (s : string) : token list =
  let len = String.length s in
  let rec aux i acc =
    if i >= String.length s then List.rev acc
    else
      match s.[i] with
      | '*' -> aux (i + 1) (Star :: acc)
      | '+' -> aux (i + 1) (Plus :: acc)
      | '|' -> aux (i + 1) (Pipe :: acc)
      | '(' -> aux (i + 1) (LParen :: acc)
      | ')' -> aux (i + 1) (RParen :: acc)
      | '\\' ->
          if i + 1 < len then aux (i + 2) (Char s.[i + 1] :: acc)
          else failwith "unterminated escape sequence"
      | c -> aux (i + 1) (Char c :: acc)
  in
  aux 0 []

(*
  Inserts concat token.
  Examples:
  * ab -> a ++ b
  * a(b) -> a ++ (b)
  * (a)(b) -> (a) ++ (b)
  * a*b -> a* ++ b
  * a+b -> a+ ++ b
*)
let explicitize_concat (tokens : token list) : token list =
  let needs_concat_after = function
    | Char _ | Star | Plus | RParen -> true
    | _ -> false
  and needs_concat_before = function
    | Char _ | LParen -> true
    | _ -> false
  in
  let rec aux = function
    | [] -> []
    | [ x ] -> [ x ]
    | x :: y :: rest ->
        if needs_concat_after x && needs_concat_before y then x :: Concat :: aux (y :: rest)
        else x :: aux (y :: rest)
  in
  aux tokens

(*
  <https://mathcenter.oxford.emory.edu/site/cs171/shuntingYardAlgorithm/>
*)
let shunting_yard (tokens : token list) : token list =
  let precedence = function
    | Star -> 4
    | Plus -> 3
    | Concat -> 2
    | Pipe -> 1
    | _ -> 0
  in
  let rec aux tokens output stack =
    match tokens with
    | [] -> List.rev_append output stack
    | (Char _ as ch) :: rest -> aux rest (ch :: output) stack
    | LParen :: rest -> aux rest output (LParen :: stack)
    | RParen :: rest ->
        let rec pop_until_lparen out ops =
          match ops with
          | [] -> (out, [])
          | LParen :: ops_rest -> (out, ops_rest)
          | ((Star | Plus | Pipe | Concat) as top) :: ops_rest ->
              pop_until_lparen (top :: out) ops_rest
          | _ -> failwith "Char in stack"
        in
        let output', stack' = pop_until_lparen output stack in
        aux rest output' stack'
    | ((Star | Plus | Pipe | Concat) as op) :: rest ->
        let rec pop_ops out ops =
          match ops with
          | ((Star | Plus | Pipe | Concat) as top) :: ops_rest when precedence op <= precedence top
            -> pop_ops (top :: out) ops_rest
          | _ -> (out, ops)
        in
        let output', stack' = pop_ops output stack in
        aux rest output' (op :: stack')
  in
  aux tokens [] []


