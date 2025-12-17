open Exp_4
open Parser.MenhirInterpreter
open Util

type parsing_error = source_location * string

let with_lexed (filename : string) (f : Lexing.lexbuf -> 'a) : 'a =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try
    let res = f lexbuf in
    close_in ic;
    res
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
    find_accepting_state env
    |> Result.map (fun ckpt -> (ckpt, base_error))
    |> Result.map_error (fun sub_err -> [ base_error; sub_err ])
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

let with_parsed (filename : string) (lexbuf : Lexing.lexbuf) (f : Ast.comp_unit -> 'a) : 'a =
  match parse lexbuf with
  | Error errors ->
      List.iter
        (fun (loc, msg) ->
          Printf.eprintf "Error type parsing_error at %s:%d:%d: %s\n" filename loc.lineno loc.colno
            msg)
        errors;
      exit 1
  | Ok comp_unit -> f comp_unit

let with_translated (filename : string) (comp_unit : Ast.comp_unit)
    (f : Sem_ast.t_comp_unit * Tac.tac_program -> 'a) =
  match Semant.translate comp_unit Semant.empty_translation_context with
  | Ok res -> f res
  | Error errs ->
      List.iter (fun msg -> Printf.eprintf "Error type semant_error at %s: %s\n" filename msg) errs;
      exit 1

let lex_file (filename : string) : unit =
  with_lexed filename @@ fun lexbuf ->
  let tokens = Lexer.get_all_tokens lexbuf in
  List.iter
    (fun tok ->
      match tok with
      | Parser.EOF -> ()
      | _ -> Tokens.token_to_string tok |> print_endline)
    tokens

let parse_file (filename : string) : unit =
  with_lexed filename @@ fun lexbuf ->
  with_parsed filename lexbuf @@ fun comp_unit ->
  Ast.prettify_comp_unit comp_unit
  |> List.fold_left (fun acc l -> acc ^ l ^ "\n") ""
  |> print_endline

let type_file (filename : string) : unit =
  with_lexed filename @@ fun lexbuf ->
  with_parsed filename lexbuf @@ fun comp_unit ->
  with_translated filename comp_unit @@ fun (tree, _) ->
  Sem_ast.prettify_t_comp_unit tree
  |> List.fold_left (fun acc l -> acc ^ l ^ "\n") ""
  |> print_endline

let tac_file (filename : string) : unit =
  with_lexed filename @@ fun lexbuf ->
  with_parsed filename lexbuf @@ fun comp_unit ->
  with_translated filename comp_unit @@ fun (_, program) ->
  Tac.prettify_tac_program program |> print_endline

let cfg_file (filename : string) : unit =
  with_lexed filename @@ fun lexbuf ->
  with_parsed filename lexbuf @@ fun comp_unit ->
  with_translated filename comp_unit @@ fun (_, program) ->
  let program, _ = Ssa.build_cfg program Ssa.empty_build_ssa_context in
  Ssa.prettify_program program |> print_endline

let ssa_file (filename : string) : unit =
  with_lexed filename @@ fun lexbuf ->
  with_parsed filename lexbuf @@ fun comp_unit ->
  with_translated filename comp_unit @@ fun (_, program) ->
  let program = Ssa.build_ssa program Ssa.empty_build_ssa_context in
  Ssa.prettify_program program |> print_endline

let opt_file (filename : string) : unit =
  with_lexed filename @@ fun lexbuf ->
  with_parsed filename lexbuf @@ fun comp_unit ->
  with_translated filename comp_unit @@ fun (_, program) ->
  let program = Ssa.build_ssa program Ssa.empty_build_ssa_context in
  let program = Opt.Const_prop.simple_const_prop program in
  Ssa.prettify_program program |> print_endline

let usage () : 'a =
  Printf.eprintf "usage: %s <lex|parse|type|tac|cfg|ssa|opt> <source-file>\n" Sys.argv.(0);
  exit 1

let main () : unit =
  if Array.length Sys.argv < 3 then usage ();
  let cmd = String.trim Sys.argv.(1) in
  if cmd = "lex" then lex_file Sys.argv.(2)
  else if cmd = "parse" then parse_file Sys.argv.(2)
  else if cmd = "type" then type_file Sys.argv.(2)
  else if cmd = "tac" then tac_file Sys.argv.(2)
  else if cmd = "cfg" then cfg_file Sys.argv.(2)
  else if cmd = "ssa" then ssa_file Sys.argv.(2)
  else if cmd = "opt" then opt_file Sys.argv.(2)
  else usage ()

let () : unit =
  if not !Sys.interactive then (
    Printexc.record_backtrace true;
    main ())
