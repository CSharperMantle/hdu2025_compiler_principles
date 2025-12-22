{
open Common
open Parser
open Tokens

exception Lexing_error of source_location * string

let raise_error (message : string) lexbuf =
  raise (Lexing_error (split_position (Lexing.lexeme_start_p lexbuf), message))
}

let whitespace = [' ' '\t' '\r']
let digit = ['0'-'9']
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let letter = ['a'-'z' 'A'-'Z']
let identifier = ('_' | letter) ('_' | letter | digit)*
let dec_or_oct = digit+
let hex = "0x" hex_digit*
let float_lit = digit* '.' digit*

rule token = parse
  | whitespace+      { token lexbuf }
  | '\n'             { Lexing.new_line lexbuf; token lexbuf }
  | "//" [^ '\n']*   { token lexbuf }
  | "/*"             { block_comment lexbuf }
  | identifier as id {
    match StringMap.find_opt id keywords with
      | Some tok -> tok
      | None -> ID id
  }
  | float_lit as f {
    match float_of_string_opt f with
    | Some v -> FLOAT_LIT v
    | None -> raise_error (Format.sprintf "Bad float literal: %s" f) lexbuf
  }
  | dec_or_oct as n {
    if String.starts_with ~prefix:"0" n && String.length n > 1 then
      match int_of_string_opt ("0o" ^ n) with
      | Some v -> INT_LIT v
      | None -> raise_error (Format.sprintf "Bad octal literal: %s" n) lexbuf
    else
      match int_of_string_opt ("0u" ^ n) with
      | Some v -> INT_LIT v
      | None -> raise_error (Format.sprintf "Bad decimal literal: %s" n) lexbuf
  }
  | hex as n {
    match int_of_string_opt n with
    | Some v -> INT_LIT v
    | None -> raise_error (Format.sprintf "Bad hexadecimal literal: %s" n) lexbuf
  }
  | "+"              { PLUS }
  | "-"              { MINUS }
  | "*"              { MULT }
  | "/"              { DIV }
  | "%"              { MOD }
  | "=="             { EQ }
  | "!="             { NEQ }
  | "<="             { LEQ }
  | ">="             { GEQ }
  | "<"              { LESS }
  | ">"              { GREATER }
  | "&&"             { AND }
  | "||"             { OR }
  | "!"              { NOT }
  | "="              { ASSIGN }
  | ";"              { SEMICOLON }
  | ","              { COMMA }
  | "("              { LPAREN }
  | ")"              { RPAREN }
  | "["              { LBRACKET }
  | "]"              { RBRACKET }
  | "{"              { LBRACE }
  | "}"              { RBRACE }
  | eof              { EOF }
  | _ as c           { raise_error (Format.sprintf "Unexpected char: %c" c) lexbuf }

and block_comment = parse
  | "*/"             { token lexbuf }
  | '\n'             { Lexing.new_line lexbuf; block_comment lexbuf }
  | eof              { raise_error "Unterminated block comment" lexbuf }
  | _                { block_comment lexbuf }

{
let get_all_tokens lexbuf =
  let rec aux acc =
    let tok = token lexbuf in
    match tok with
    | EOF -> List.rev (EOF :: acc)
    | _ -> aux (tok :: acc)
  in
  aux []
}
