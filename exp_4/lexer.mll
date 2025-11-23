{
open Tokens
exception Lexing_error of string

let get_pos lexbuf =
  let pos = Lexing.lexeme_start_p lexbuf in
  (pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1)

let raise_error (message : string) lexbuf =
  let (lineno, colno) = get_pos lexbuf in
  raise (Lexing_error (Format.sprintf "%d:%d: %s" lineno colno message))
}

let whitespace = [' ' '\t' '\r' '\n']
let digit = ['0'-'9']
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let letter = ['a'-'z' 'A'-'Z']
let identifier = ('_' | letter) ('_' | letter | digit)*
let dec_or_oct = digit+
let hex = "0x" hex_digit*

rule token = parse
  | whitespace+      { token lexbuf }
  | "//" [^ '\n']*   { token lexbuf }
  | "/*"             { block_comment lexbuf }
  | identifier as id {
    match StringMap.find_opt id keywords with
      | Some tok -> tok
      | None -> TokenId id
  }
  | dec_or_oct as n {
    if String.starts_with ~prefix:"0" n && String.length n > 1 then
      match int_of_string_opt ("0o" ^ n) with
      | Some v -> TokenIntLit v
      | None -> raise_error (Format.sprintf "Bad octal literal: %s" n) lexbuf
    else
      match int_of_string_opt ("0u" ^ n) with
      | Some v -> TokenIntLit v
      | None -> raise_error (Format.sprintf "Bad decimal literal: %s" n) lexbuf
  }
  | hex as n {
    match int_of_string_opt n with
    | Some v -> TokenIntLit v
    | None -> raise_error (Format.sprintf "Bad hexadecimal literal: %s" n) lexbuf
  }
  | "+"              { TokenPlus }
  | "-"              { TokenMinus }
  | "*"              { TokenMult }
  | "/"              { TokenDiv }
  | "%"              { TokenMod }
  | "=="             { TokenEq }
  | "!="             { TokenNeq }
  | "<="             { TokenLeq }
  | ">="             { TokenGeq }
  | "<"              { TokenLess }
  | ">"              { TokenGreater }
  | "&&"             { TokenAnd }
  | "||"             { TokenOr }
  | "!"              { TokenNot }
  | "="              { TokenAssign }
  | ";"              { TokenSemicolon }
  | ","              { TokenComma }
  | "("              { TokenLParen }
  | ")"              { TokenRParen }
  | "["              { TokenLBracket }
  | "]"              { TokenRBracket }
  | "{"              { TokenLBrace }
  | "}"              { TokenRBrace }
  | eof              { TokenEof }
  | _ as c           { raise_error (Format.sprintf "Unexpected char: %c" c) lexbuf }

and block_comment = parse
  | "*/"             { token lexbuf }
  | eof              { raise_error "Unterminated block comment" lexbuf }
  | _                { block_comment lexbuf }

{
let get_all_tokens lexbuf =
  let rec aux acc =
    let tok = token lexbuf in
    match tok with
    | TokenEof -> List.rev (TokenEof :: acc)
    | _ -> aux (tok :: acc)
  in
  aux []
}
