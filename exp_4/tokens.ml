open Common
open Parser

let string_of_token = function
  | INT -> "INT"
  | FLOAT -> "FLOAT"
  | VOID -> "VOID"
  | CONST -> "CONST"
  | IF -> "IF"
  | ELSE -> "ELSE"
  | WHILE -> "WHILE"
  | BREAK -> "BREAK"
  | CONTINUE -> "CONTINUE"
  | RETURN -> "RETURN"
  | ID _ -> "ID"
  | INT_LIT _ -> "INT_LIT"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | MULT -> "MULT"
  | DIV -> "DIV"
  | MOD -> "MOD"
  | ASSIGN -> "ASSIGN"
  | EQ -> "EQ"
  | NEQ -> "NEQ"
  | LESS -> "LESS"
  | LEQ -> "LEQ"
  | GREATER -> "GREATER"
  | GEQ -> "GEQ"
  | AND -> "AND"
  | OR -> "OR"
  | NOT -> "NOT"
  | SEMICOLON -> "SEMICOLON"
  | COMMA -> "COMMA"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | EOF -> "EOF"

let prettify_token (token : token) =
  let token_name = string_of_token token in
  match token with
  | ID id -> Format.sprintf "%s %s" token_name id
  | INT_LIT n -> Format.sprintf "%s %d" token_name n
  | _ -> token_name

let keywords =
  StringMap.of_list
    [
      ("const", CONST);
      ("int", INT);
      ("float", FLOAT);
      ("void", VOID);
      ("if", IF);
      ("else", ELSE);
      ("while", WHILE);
      ("break", BREAK);
      ("continue", CONTINUE);
      ("return", RETURN);
    ]
