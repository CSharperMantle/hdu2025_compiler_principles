let lex_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try
    let tokens = Lexer.get_all_tokens lexbuf in
    close_in ic;
    List.iter
      (fun tok ->
        match tok with
        | Parser.EOF -> ()
        | _ -> print_endline (Tokens.token_to_string tok))
      tokens
  with Lexer.Lexing_error msg ->
    close_in ic;
    Printf.eprintf "Error type A at %s:%s\n" filename msg

let parse_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let _ = Parsing.set_trace true in
  try
    let ast = Parser.comp_unit Lexer.token lexbuf in
    close_in ic;
    Ast.print_ast_indent 0 ast
  with
  | Lexer.Lexing_error msg ->
      close_in ic;
      Printf.eprintf "Error type A at %s:%s\n" filename msg
  | Parser.Error ->
      close_in ic;
      let pos = Lexing.lexeme_start_p lexbuf in
      Printf.eprintf "Error type B at %s:%d:%d\n" filename pos.pos_lnum
        (pos.pos_cnum - pos.pos_bol + 1)

let usage () =
  Printf.eprintf "usage: %s --lex <source-file>\n" Sys.argv.(0);
  Printf.eprintf "       %s --parse <source-file>\n" Sys.argv.(0);
  exit 1

let () =
  if Array.length Sys.argv < 3 then usage ();
  let cmd = String.trim Sys.argv.(1) in
  if cmd = "--lex" then lex_file Sys.argv.(2)
  else if cmd = "--parse" then parse_file Sys.argv.(2)
  else usage ()
