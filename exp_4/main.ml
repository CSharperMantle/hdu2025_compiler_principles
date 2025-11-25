open Parser.MenhirInterpreter

type parsing_error = int * int * string

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

let parse (lexbuf : Lexing.lexbuf) : (Ast.comp_unit, parsing_error list) result =
  let rec iterate errors checkpoint =
    match checkpoint with
    | InputNeeded _env ->
        let token = Lexer.token lexbuf
        and startp = lexbuf.lex_start_p
        and endp = lexbuf.lex_curr_p in
        iterate errors (offer checkpoint (token, startp, endp))
    | Shifting _ | AboutToReduce _ -> iterate errors (resume checkpoint)
    | HandlingError _env ->
        let pos = Lexing.lexeme_start_p lexbuf in
        (None, (pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1, "Generic error") :: errors)
    | Accepted v -> (Some v, errors)
    | Rejected ->
        let pos = Lexing.lexeme_start_p lexbuf in
        (None, (pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1, "Rejected") :: errors)
  in
  let checkpoint = Parser.Incremental.comp_unit lexbuf.lex_curr_p in
  let comp_unit, errors = iterate [] checkpoint in
  if errors <> [] then Error errors else Option.to_result ~none:errors comp_unit

let parse_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try
    let comp_unit = parse lexbuf in
    close_in ic;
    match comp_unit with
    | Error errors ->
        List.iter
          (fun (lineno, colno, msg) ->
            Printf.eprintf "Error type parsing_error at %s:%d:%d: %s\n" filename lineno colno msg)
          errors
    | Ok comp_unit ->
        Ast.prettify_comp_unit comp_unit
        |> List.fold_left (fun acc l -> acc ^ l ^ "\n") ""
        |> print_endline
  with Lexer.Lexing_error (lineno, colno, msg) ->
    close_in ic;
    Printf.eprintf "Error type Lexer.Lexing_error at %s:%d:%d: %s\n" filename lineno colno msg;
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
