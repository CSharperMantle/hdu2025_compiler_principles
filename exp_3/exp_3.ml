open Lib

(*
  Demo expression grammar for SysV.
    E -> TE'
    E' -> ATE' | ε
    T -> FT'
    T' -> MFT' | ε
    F -> (E) | i
    A -> + | -
    M -> * | /
*)
let grammar_sysv_expr : grammar =
  [
    { lhs = 1; rhs = [ NonTerminal 3; NonTerminal 2 ] };
    { lhs = 2; rhs = [ NonTerminal 6; NonTerminal 3; NonTerminal 2 ] };
    { lhs = 2; rhs = [] };
    { lhs = 3; rhs = [ NonTerminal 5; NonTerminal 4 ] };
    { lhs = 4; rhs = [ NonTerminal 7; NonTerminal 5; NonTerminal 4 ] };
    { lhs = 4; rhs = [] };
    { lhs = 5; rhs = [ Terminal "("; NonTerminal 1; Terminal ")" ] };
    { lhs = 5; rhs = [ Terminal "i" ] };
    { lhs = 6; rhs = [ Terminal "+" ] };
    { lhs = 6; rhs = [ Terminal "-" ] };
    { lhs = 7; rhs = [ Terminal "*" ] };
    { lhs = 7; rhs = [ Terminal "/" ] };
  ]

let parser_sysv_expr =
  grammar_sysv_expr |> eliminate_common_prefix |> eliminate_left_recursion |> make_parser

let lex_sysv_expr (input : string) : string list =
  let rec aux pos acc =
    if pos >= String.length input then List.rev acc
    else
      let c = input.[pos] in
      match c with
      | ' ' | '\t' | '\n' | '\r' -> aux (pos + 1) acc (* Skip whitespace *)
      | '(' -> aux (pos + 1) ("(" :: acc)
      | ')' -> aux (pos + 1) (")" :: acc)
      | '+' -> aux (pos + 1) ("+" :: acc)
      | '-' -> aux (pos + 1) ("-" :: acc)
      | '*' -> aux (pos + 1) ("*" :: acc)
      | '/' -> aux (pos + 1) ("/" :: acc)
      | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' ->
          (* All identifiers/numbers as 'i' *)
          let rec consume_identifier p =
            if p >= String.length input then p
            else
              match input.[p] with
              | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '_' -> consume_identifier (p + 1)
              | _ -> p
          in
          let end_pos = consume_identifier pos in
          aux end_pos ("i" :: acc)
      | _ -> failwith ("Unexpected character: " ^ String.make 1 c)
  in
  aux 0 []

let () =
  try
    while true do
      print_string "Expr?> ";
      flush stdout;
      let input = read_line () in
      if String.trim input = "" then exit 0;
      let tokens = lex_sysv_expr input in
      Printf.printf "Tokens: %s\n"
        (tokens |> List.map (fun s -> "\"" ^ s ^ "\"") |> String.concat " ");
      let result = parse tokens parser_sysv_expr in
      if result then print_endline "Good.\n" else print_endline "Bad.\n"
    done
  with End_of_file -> ()
