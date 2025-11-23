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
        | Tokens.TokenEof -> ()
        | _ -> print_endline (Tokens.token_to_string tok))
      tokens
  with Lexer.Lexing_error msg ->
    close_in ic;
    Printf.eprintf "Error type A at %s:%s\n" filename msg

let () =
  if Array.length Sys.argv < 2 then (
    Printf.eprintf "usage: %s <source-file>\n" Sys.argv.(0);
    exit 1);
  lex_file Sys.argv.(1)
