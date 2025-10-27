type token =
  | Char of char
  | Star
  | Plus
  | Pipe
  | LParen
  | RParen
  | Concat (* implicit *)

let tokenize (s : string) : token list =
  let len = String.length s in
  let rec aux i acc =
    if i >= String.length s then List.rev acc
    else
      match s.[i] with
      | '*' -> aux (i + 1) (Star :: acc)
      | '+' -> aux (i + 1) (Plus :: acc)
      | '|' -> aux (i + 1) (Pipe :: acc)
      | '(' -> aux (i + 1) (LParen :: acc)
      | ')' -> aux (i + 1) (RParen :: acc)
      | '\\' ->
          if i + 1 < len then aux (i + 2) (Char s.[i + 1] :: acc)
          else failwith "unterminated escape sequence"
      | c -> aux (i + 1) (Char c :: acc)
  in
  aux 0 []

(*
  Inserts concat token.
  Examples:
  * ab -> a ++ b
  * a(b) -> a ++ (b)
  * (a)(b) -> (a) ++ (b)
  * a*b -> a* ++ b
  * a+b -> a+ ++ b
*)
let explicitize_concat (tokens : token list) : token list =
  let needs_concat_after = function
    | Char _ | Star | Plus | RParen -> true
    | _ -> false
  and needs_concat_before = function
    | Char _ | LParen -> true
    | _ -> false
  in
  let rec aux = function
    | [] -> []
    | [ x ] -> [ x ]
    | x :: y :: rest ->
        if needs_concat_after x && needs_concat_before y then x :: Concat :: aux (y :: rest)
        else x :: aux (y :: rest)
  in
  aux tokens

(*
  <https://mathcenter.oxford.emory.edu/site/cs171/shuntingYardAlgorithm/>
*)
let shunting_yard (tokens : token list) : token list =
  let precedence = function
    | Star -> 4
    | Plus -> 3
    | Concat -> 2
    | Pipe -> 1
    | _ -> 0
  in
  let rec aux tokens output stack =
    match tokens with
    | [] -> List.rev_append output stack
    | (Char _ as ch) :: rest -> aux rest (ch :: output) stack
    | LParen :: rest -> aux rest output (LParen :: stack)
    | RParen :: rest ->
        let rec pop_until_lparen out ops =
          match ops with
          | [] -> (out, [])
          | LParen :: ops_rest -> (out, ops_rest)
          | ((Star | Plus | Pipe | Concat) as top) :: ops_rest ->
              pop_until_lparen (top :: out) ops_rest
          | _ -> failwith "Char in stack"
        in
        let output', stack' = pop_until_lparen output stack in
        aux rest output' stack'
    | ((Star | Plus | Pipe | Concat) as op) :: rest ->
        let rec pop_ops out ops =
          match ops with
          | ((Star | Plus | Pipe | Concat) as top) :: ops_rest when precedence op <= precedence top
            -> pop_ops (top :: out) ops_rest
          | _ -> (out, ops)
        in
        let output', stack' = pop_ops output stack in
        aux rest output' (op :: stack')
  in
  aux tokens [] []

type nfa_symbol =
  | Epsilon
  | Symbol of char

type nfa = {
  q0 : int;
  qf : int;
  transitions : (int * nfa_symbol * int) list;
}

(*
  <http://www.csci.viu.ca/~wesselsd/courses/csci439/slides/thompsons.pdf>
  <https://en.wikipedia.org/wiki/Thompson%27s_construction>
*)
let thompson (tokens : token list) : nfa =
  let alloc_node id = (id, id + 1) in
  let create_char_fragment c id =
    let s1, id' = alloc_node id in
    let s2, id'' = alloc_node id' in
    ({ q0 = s1; qf = s2; transitions = [ (s1, Symbol c, s2) ] }, id'')
  and create_concat_fragment frag1 frag2 =
    {
      q0 = frag1.q0;
      qf = frag2.qf;
      transitions = [ (frag1.qf, Epsilon, frag2.q0) ] @ frag1.transitions @ frag2.transitions;
    }
  and create_alt_fragment frag1 frag2 id =
    let s1, id' = alloc_node id in
    let s2, id'' = alloc_node id' in
    ( {
        q0 = s1;
        qf = s2;
        transitions =
          [
            (s1, Epsilon, frag1.q0);
            (s1, Epsilon, frag2.q0);
            (frag1.qf, Epsilon, s2);
            (frag2.qf, Epsilon, s2);
          ]
          @ frag1.transitions @ frag2.transitions;
      },
      id'' )
  and create_star_fragment frag id =
    let s1, id' = alloc_node id in
    let s2, id'' = alloc_node id' in
    ( {
        q0 = s1;
        qf = s2;
        transitions =
          [
            (s1, Epsilon, frag.q0);
            (s1, Epsilon, s2);
            (frag.qf, Epsilon, frag.q0);
            (frag.qf, Epsilon, s2);
          ]
          @ frag.transitions;
      },
      id'' )
  in
  let expand_plus_fragment frag id =
    let star, id' = create_star_fragment frag id in
    (create_concat_fragment frag star, id')
  in
  let rec aux tokens stack id =
    match tokens with
    | [] -> (
        match stack with
        | [ frag ] -> (frag, id)
        | _ -> failwith "invalid postfix expr")
    | Char c :: rest ->
        let frag, id' = create_char_fragment c id in
        aux rest (frag :: stack) id'
    | Concat :: rest -> (
        match stack with
        | frag2 :: frag1 :: stack' ->
            let frag = create_concat_fragment frag1 frag2 in
            aux rest (frag :: stack') id
        | _ -> failwith "invalid postfix expr")
    | Pipe :: rest -> (
        match stack with
        | frag2 :: frag1 :: stack' ->
            let frag, id' = create_alt_fragment frag1 frag2 id in
            aux rest (frag :: stack') id'
        | _ -> failwith "invalid postfix expr")
    | Star :: rest -> (
        match stack with
        | frag :: stack' ->
            let frag, id' = create_star_fragment frag id in
            aux rest (frag :: stack') id'
        | _ -> failwith "invalid postfix expr")
    | Plus :: rest -> (
        match stack with
        | frag :: stack' ->
            let frag, id' = expand_plus_fragment frag id in
            aux rest (frag :: stack') id'
        | _ -> failwith "invalid postfix expr")
    | _ -> failwith "bad tokens in expr"
  in
  let final_frag, _ = aux tokens [] 0 in
  { q0 = final_frag.q0; qf = final_frag.qf; transitions = final_frag.transitions }

let eval_nfa (nfa : nfa) (str : string) : bool =
  let epsilon_closure states =
    let rec aux visited = function
      | [] -> visited
      | q :: rest ->
          if List.mem q visited then aux visited rest
          else
            let states' =
              List.filter_map
                (fun (from, symbol, to_) -> if from = q && symbol = Epsilon then Some to_ else None)
                nfa.transitions
            in
            aux (q :: visited) (states' @ rest)
    in
    aux [] states
  and move states c =
    List.filter_map
      (fun (from, symbol, to_) ->
        if List.mem from states && symbol = Symbol c then Some to_ else None)
      nfa.transitions
  in
  let rec aux current i =
    if i >= String.length str then List.mem nfa.qf (epsilon_closure current)
    else
      let c = str.[i] in
      let next_states = move (epsilon_closure current) c in
      aux next_states (i + 1)
  in
  aux [ nfa.q0 ] 0

type dfa = {
  q0 : int;
  qf : int list;
  transitions : (int * char * int) list;
}

let nfa_to_dfa (nfa : nfa) : dfa =
  let alphabet =
    List.fold_left
      (fun acc (_, symbol, _) ->
        match symbol with
        | Symbol c -> if List.mem c acc then acc else c :: acc
        | Epsilon -> acc)
      [] nfa.transitions
  and epsilon_closure states =
    let rec aux visited = function
      | [] -> visited
      | q :: rest ->
          if List.mem q visited then aux visited rest
          else
            let states' =
              List.filter_map
                (fun (from, symbol, to_) -> if from = q && symbol = Epsilon then Some to_ else None)
                nfa.transitions
            in
            aux (q :: visited) (states' @ rest)
    in
    aux [] states
  and move states c =
    List.filter_map
      (fun (from, symbol, to_) ->
        if List.mem from states && symbol = Symbol c then Some to_ else None)
      nfa.transitions
  and sort_states states = List.sort_uniq compare states in
  let p0 = sort_states (epsilon_closure [ nfa.q0 ]) in
  let rec aux pending visited transitions state_map id =
    match pending with
    | [] -> (visited, transitions, state_map)
    | first :: rest ->
        let first_id = List.assoc first state_map in
        let pending', visited', transitions', state_map', id' =
          (* For each character c in alphabet ... *)
          List.fold_left
            (fun (f_pend, f_vis, f_trans, f_sm, f_id) c ->
              (* Have we seen its eps-closure(move(T, c)) already? *)
              match sort_states (epsilon_closure (move first c)) with
              | [] -> (f_pend, f_vis, f_trans, f_sm, f_id)
              | s when List.mem_assoc s f_sm ->
                  (* Yes! *)
                  (f_pend, f_vis, (first_id, c, List.assoc s f_sm) :: f_trans, f_sm, f_id)
              | s ->
                  (* No. *)
                  ( s :: f_pend,
                    s :: f_vis,
                    (first_id, c, f_id) :: f_trans,
                    (s, f_id) :: f_sm,
                    f_id + 1 ))
            (rest, visited, transitions, state_map, id)
            alphabet
        in
        aux pending' visited' transitions' state_map' id'
  in
  let _, transitions, state_map = aux [ p0 ] [] [] [ (p0, 0) ] 1 in
  let qf =
    List.filter_map (fun (states, id) -> if List.mem nfa.qf states then Some id else None) state_map
  in
  { q0 = 0; qf; transitions }
