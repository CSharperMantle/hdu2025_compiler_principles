exception Parsing_error of int * int * string

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
        | _ -> Tokens.token_to_string tok |> print_endline)
      tokens
  with Lexer.Lexing_error (lineno, colno, msg) ->
    close_in ic;
    Printf.eprintf "Error type Lexer.Lexing_error at %s:%d:%d: %s\n" filename lineno colno msg;
    exit 1

let parse_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };

  let rec iterate checkpoint : Ast.comp_unit =
    match checkpoint with
    | Parser.MenhirInterpreter.InputNeeded _env ->
        let token = Lexer.token lexbuf
        and startp = lexbuf.lex_start_p
        and endp = lexbuf.lex_curr_p in
        Parser.MenhirInterpreter.offer checkpoint (token, startp, endp) |> iterate
    | Parser.MenhirInterpreter.Shifting _ | Parser.MenhirInterpreter.AboutToReduce _ ->
        Parser.MenhirInterpreter.resume checkpoint |> iterate
    | Parser.MenhirInterpreter.HandlingError _env ->
        let pos = Lexing.lexeme_start_p lexbuf in
        raise (Parsing_error (pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1, "Generic error"))
    | Parser.MenhirInterpreter.Accepted v -> v
    | Parser.MenhirInterpreter.Rejected ->
        let pos = Lexing.lexeme_start_p lexbuf in
        raise (Parsing_error (pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1, "Rejected"))
  in
  try
    let checkpoint = Parser.Incremental.comp_unit lexbuf.lex_curr_p in
    let comp_unit = iterate checkpoint in
    close_in ic;
    Ast.prettify_comp_unit comp_unit
    |> List.fold_left (fun acc l -> acc ^ l ^ "\n") ""
    |> print_endline
  with
  | Lexer.Lexing_error (lineno, colno, msg) ->
      close_in ic;
      Printf.eprintf "Error type Lexer.Lexing_error at %s:%d:%d: %s\n" filename lineno colno msg;
      exit 1
  | Parsing_error (lineno, colno, msg) ->
      close_in ic;
      Printf.eprintf "Error type Parsing_error at %s:%d:%d: %s\n" filename lineno colno msg;
      exit 1

let usage () =
  Printf.eprintf "usage: %s <lex|parse> <source-file>\n" Sys.argv.(0);
  exit 1

let () =
  if Array.length Sys.argv < 3 then usage ();
  let cmd = String.trim Sys.argv.(1) in
  if cmd = "lex" then lex_file Sys.argv.(2)
  else if cmd = "parse" then parse_file Sys.argv.(2)
  else usage ()
