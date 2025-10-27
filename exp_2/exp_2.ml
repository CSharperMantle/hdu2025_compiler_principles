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
