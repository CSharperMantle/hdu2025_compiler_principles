type token =
  | Char of char
  | Star
  | Plus
  | Pipe
  | LParen
  | RParen

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
          else raise (Invalid_argument "unterminated escape")
      | c -> aux (i + 1) (Char c :: acc)
  in
  aux 0 []
