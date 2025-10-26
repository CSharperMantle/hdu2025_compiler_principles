type token =
  | Char of char
  | Closure
  | Alt
  | LParen
  | RParen

let tokenize (s: string) : token list =
  let rec aux i acc =
    if i >= String.length s then List.rev acc
    else
      match s.[i] with
      | '*' -> aux (i + 1) (Closure :: acc)
      | '|' -> aux (i + 1) (Alt :: acc)
      | '(' -> aux (i + 1) (LParen :: acc)
      | ')' -> aux (i + 1) (RParen :: acc)
      | c   -> aux (i + 1) (Char c :: acc)
  in
  aux 0 []

