type token =
  | TokenInt
  | TokenVoid
  | TokenConst
  | TokenIf
  | TokenElse
  | TokenWhile
  | TokenBreak
  | TokenContinue
  | TokenReturn
  | TokenId of string
  | TokenIntLit of int
  | TokenPlus
  | TokenMinus
  | TokenMult
  | TokenDiv
  | TokenMod
  | TokenAssign
  | TokenEq
  | TokenNeq
  | TokenLess
  | TokenLeq
  | TokenGreater
  | TokenGeq
  | TokenAnd
  | TokenOr
  | TokenNot
  | TokenSemicolon
  | TokenComma
  | TokenLParen
  | TokenRParen
  | TokenLBracket
  | TokenRBracket
  | TokenLBrace
  | TokenRBrace
  | TokenEof

let name_of = function
  | TokenInt -> "TokenInt"
  | TokenVoid -> "TokenVoid"
  | TokenConst -> "TokenConst"
  | TokenIf -> "TokenIf"
  | TokenElse -> "TokenElse"
  | TokenWhile -> "TokenWhile"
  | TokenBreak -> "TokenBreak"
  | TokenContinue -> "TokenContinue"
  | TokenReturn -> "TokenReturn"
  | TokenId _ -> "TokenId"
  | TokenIntLit _ -> "TokenIntLit"
  | TokenPlus -> "TokenPlus"
  | TokenMinus -> "TokenMinus"
  | TokenMult -> "TokenMult"
  | TokenDiv -> "TokenDiv"
  | TokenMod -> "TokenMod"
  | TokenAssign -> "TokenAssign"
  | TokenEq -> "TokenEq"
  | TokenNeq -> "TokenNeq"
  | TokenLess -> "TokenLess"
  | TokenLeq -> "TokenLeq"
  | TokenGreater -> "TokenGreater"
  | TokenGeq -> "TokenGeq"
  | TokenAnd -> "TokenAnd"
  | TokenOr -> "TokenOr"
  | TokenNot -> "TokenNot"
  | TokenSemicolon -> "TokenSemicolon"
  | TokenComma -> "TokenComma"
  | TokenLParen -> "TokenLParen"
  | TokenRParen -> "TokenRParen"
  | TokenLBracket -> "TokenLBracket"
  | TokenRBracket -> "TokenRBracket"
  | TokenLBrace -> "TokenLBrace"
  | TokenRBrace -> "TokenRBrace"
  | TokenEof -> "TokenEof"

let token_to_string (token : token) =
  let token_name = name_of token in
  match token with
  | TokenId id -> Format.sprintf "%s %s" token_name id
  | TokenIntLit n -> Format.sprintf "%s %d" token_name n
  | _ -> token_name

module StringMap = Map.Make (String)

let keywords =
  StringMap.of_list
    [
      ("const", TokenConst);
      ("int", TokenInt);
      ("void", TokenVoid);
      ("const", TokenConst);
      ("if", TokenIf);
      ("else", TokenElse);
      ("while", TokenWhile);
      ("break", TokenBreak);
      ("continue", TokenContinue);
      ("return", TokenReturn);
    ]
