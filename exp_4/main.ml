open Parser.MenhirInterpreter
open Util

type parsing_error = source_location * string

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
  with Lexer.Lexing_error (loc, msg) ->
    close_in ic;
    Printf.eprintf "Error type Lexer.Lexing_error at %s:%d:%d: %s\n" filename loc.lineno loc.colno
      msg;
    exit 1

let parse (lexbuf : Lexing.lexbuf) : (Ast.comp_unit, parsing_error list) result =
  let recover env =
    let rec resync lexbuf =
      match Lexer.token lexbuf with
      | (Parser.EOF | Parser.SEMICOLON | Parser.RPAREN | Parser.RBRACKET | Parser.RBRACE) as tok ->
          (tok, lexbuf.lex_start_p, lexbuf.lex_curr_p)
      | _ -> resync lexbuf
    in
    let base_error_loc = split_position (Lexing.lexeme_start_p lexbuf) in
    let base_error = (base_error_loc, "Syntax error") in
    let sync_tok, startp, endp = resync lexbuf in
    let rec find_accepting_state env =
      let checkpoint = input_needed env in
      match offer checkpoint (sync_tok, startp, endp) with
      | HandlingError _ -> (
          match pop env with
          | None -> Error (base_error_loc, "Can't recover")
          | Some env' -> find_accepting_state env')
      | ckpt' -> Ok ckpt'
    in
    match find_accepting_state env with
    | Ok ckpt -> Ok (ckpt, base_error)
    | Error sub_err -> Error [ base_error; sub_err ]
  in
  let rec iterate errors checkpoint =
    match checkpoint with
    | InputNeeded _env ->
        let token = Lexer.token lexbuf
        and startp = lexbuf.lex_start_p
        and endp = lexbuf.lex_curr_p in
        iterate errors (offer checkpoint (token, startp, endp))
    | Shifting _ | AboutToReduce _ -> iterate errors (resume checkpoint)
    | HandlingError env -> (
        match recover env with
        | Ok (ckpt, err) -> iterate (err :: errors) ckpt
        | Error errs -> (None, errs @ errors))
    | Accepted v -> (Some v, errors)
    | Rejected ->
        let loc = split_position (Lexing.lexeme_start_p lexbuf) in
        (None, (loc, "Rejected") :: errors)
  in
  let checkpoint = Parser.Incremental.comp_unit lexbuf.lex_curr_p in
  let comp_unit, errors = iterate [] checkpoint in
  let errors = List.rev errors in
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
          (fun (loc, msg) ->
            Printf.eprintf "Error type parsing_error at %s:%d:%d: %s\n" filename loc.lineno
              loc.colno msg)
          errors;
        exit 1
    | Ok comp_unit ->
        Ast.prettify_comp_unit comp_unit
        |> List.fold_left (fun acc l -> acc ^ l ^ "\n") ""
        |> print_endline
  with Lexer.Lexing_error (loc, msg) ->
    close_in ic;
    Printf.eprintf "Error type Lexer.Lexing_error at %s:%d:%d: %s\n" filename loc.lineno loc.colno
      msg;
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
