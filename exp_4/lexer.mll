{
open Tokens
exception Lexing_error of string
}

let whitespace = [' ' '\t' '\r' '\n']
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let identifier = ('_' | letter) ('_' | letter | digit)*
let decimal = ['1'-'9'] digit+ (* Rule out octal literals. *)
let octal = '0' ['0'-'7']+
let hexadecimal = "0x" ['0'-'9' 'a'-'f' 'A'-'F']+

rule token = parse
  | whitespace+      { token lexbuf }
  | "//" [^ '\n']*   { token lexbuf }
  | "/*"             { block_comment lexbuf }
  | identifier as id {
    match StringMap.find_opt id keywords with
      | Some tok -> tok
      | None -> TokenId id
  }
  | decimal as n     { TokenIntConst (int_of_string ("0u" ^ n)) }
  | octal as n       { TokenIntConst (int_of_string ("0o" ^ n)) }
  | hexadecimal as n { TokenIntConst (int_of_string n) }
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
  | _ as c           { raise (Lexing_error (Format.sprintf "Unexpected char: %c" c)) }

and block_comment = parse
  | "*/"             { token lexbuf }
  | eof              { raise (Lexing_error "Unterminated block comment") }
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
