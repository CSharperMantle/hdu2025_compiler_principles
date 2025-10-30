module IntSet = Set.Make (Int)
module CharSet = Set.Make (Char)

type raw_token =
  | RawChar of char
  | RawStar
  | RawPlus
  | RawPipe
  | RawLParen
  | RawRParen

let tokenize (s : string) : raw_token list =
  let len = String.length s in
  let rec aux i acc =
    if i >= String.length s then List.rev acc
    else
      match s.[i] with
      | '*' -> aux (i + 1) (RawStar :: acc)
      | '+' -> aux (i + 1) (RawPlus :: acc)
      | '|' -> aux (i + 1) (RawPipe :: acc)
      | '(' -> aux (i + 1) (RawLParen :: acc)
      | ')' -> aux (i + 1) (RawRParen :: acc)
      | '\\' ->
          if i + 1 < len then aux (i + 2) (RawChar s.[i + 1] :: acc)
          else failwith "unterminated escape sequence"
      | c -> aux (i + 1) (RawChar c :: acc)
  in
  aux 0 []

type token =
  | Char of char
  | Star
  | Plus
  | Pipe
  | LParen
  | RParen
  | Concat

(*
  Inserts concat token.
  Examples:
  * ab -> a ++ b
  * a(b) -> a ++ (b)
  * (a)(b) -> (a) ++ (b)
  * a*b -> a* ++ b
  * a+b -> a+ ++ b
*)
let explicitize_concat (tokens : raw_token list) : token list =
  let needs_concat_after = function
    | RawChar _ | RawStar | RawPlus | RawRParen -> true
    | _ -> false
  and needs_concat_before = function
    | RawChar _ | RawLParen -> true
    | _ -> false
  and raw_to_token = function
    | RawChar c -> Char c
    | RawStar -> Star
    | RawPlus -> Plus
    | RawPipe -> Pipe
    | RawLParen -> LParen
    | RawRParen -> RParen
  in
  let rec aux = function
    | [] -> []
    | [ x ] -> [ raw_to_token x ]
    | x :: y :: rest ->
        if needs_concat_after x && needs_concat_before y then
          raw_to_token x :: Concat :: aux (y :: rest)
        else raw_to_token x :: aux (y :: rest)
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
  let rec aux output stack = function
    | [] -> List.rev_append output stack
    | (Char _ as ch) :: rest -> aux (ch :: output) stack rest
    | LParen :: rest -> aux output (LParen :: stack) rest
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
        aux output' stack' rest
    | ((Star | Plus | Pipe | Concat) as op) :: rest ->
        let rec pop_ops out ops =
          match ops with
          | ((Star | Plus | Pipe | Concat) as top) :: ops_rest when precedence op <= precedence top
            -> pop_ops (top :: out) ops_rest
          | _ -> (out, ops)
        in
        let output', stack' = pop_ops output stack in
        aux output' (op :: stack') rest
  in
  aux [] [] tokens

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
  let rec aux stack id = function
    | [] -> (
        match stack with
        | [ frag ] -> (frag, id)
        | _ -> failwith "invalid postfix expr")
    | Char c :: rest ->
        let frag, id' = create_char_fragment c id in
        aux (frag :: stack) id' rest
    | Concat :: rest -> (
        match stack with
        | frag2 :: frag1 :: stack' ->
            let frag = create_concat_fragment frag1 frag2 in
            aux (frag :: stack') id rest
        | _ -> failwith "invalid postfix expr")
    | Pipe :: rest -> (
        match stack with
        | frag2 :: frag1 :: stack' ->
            let frag, id' = create_alt_fragment frag1 frag2 id in
            aux (frag :: stack') id' rest
        | _ -> failwith "invalid postfix expr")
    | Star :: rest -> (
        match stack with
        | frag :: stack' ->
            let frag, id' = create_star_fragment frag id in
            aux (frag :: stack') id' rest
        | _ -> failwith "invalid postfix expr")
    | Plus :: rest -> (
        match stack with
        | frag :: stack' ->
            let frag, id' = expand_plus_fragment frag id in
            aux (frag :: stack') id' rest
        | _ -> failwith "invalid postfix expr")
    | _ -> failwith "bad tokens in expr"
  in
  let final_frag, _ = aux [] 0 tokens in
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
      (fun (q1, sym, q2) -> if List.mem q1 states && sym = Symbol c then Some q2 else None)
      nfa.transitions
  in
  let rec aux current i =
    if i >= String.length str then List.mem nfa.qf (epsilon_closure current)
    else aux (move (epsilon_closure current) str.[i]) (i + 1)
  in
  aux [ nfa.q0 ] 0

type dfa = {
  q0 : int;
  qf : int list;
  transitions : (int * char * int) list;
}

(*
  雨课堂PPT《9.30 词法分析2-自动机》
  <https://en.wikipedia.org/wiki/Powerset_construction>
*)
let nfa_to_dfa (nfa : nfa) : dfa =
  let alphabet =
    CharSet.of_list
      (List.filter_map
         (fun (_, sym, _) ->
           match sym with
           | Symbol c -> Some c
           | Epsilon -> None)
         nfa.transitions)
  and epsilon_closure states =
    let rec aux visited = function
      | [] -> IntSet.of_list visited
      | q :: rest ->
          if List.mem q visited then aux visited rest
          else
            let states' =
              List.filter_map
                (fun (q1, sym, q2) -> if q1 = q && sym = Epsilon then Some q2 else None)
                nfa.transitions
            in
            aux (q :: visited) (states' @ rest)
    in
    aux [] states
  and move states c =
    List.filter_map
      (fun (q1, sym, q2) -> if IntSet.mem q1 states && sym = Symbol c then Some q2 else None)
      nfa.transitions
  in
  let p0 = epsilon_closure [ nfa.q0 ] in
  let rec aux visited transitions state_map id = function
    | [] -> (visited, transitions, state_map)
    | first :: rest ->
        let first_id = List.assoc first state_map in
        let pending', visited', transitions', state_map', id' =
          (* For each character c in alphabet ... *)
          CharSet.fold
            (fun c (f_pend, f_vis, f_trans, f_sm, f_id) ->
              let s = epsilon_closure (move first c) in
              (* Have we seen its eps-closure(move(T, c)) already? *)
              match IntSet.cardinal s with
              | 0 -> (f_pend, f_vis, f_trans, f_sm, f_id)
              | _ ->
                  if List.mem_assoc s f_sm then
                    (* Yes! *)
                    (f_pend, f_vis, (first_id, c, List.assoc s f_sm) :: f_trans, f_sm, f_id)
                  else
                    (* No... *)
                    ( s :: f_pend,
                      s :: f_vis,
                      (first_id, c, f_id) :: f_trans,
                      (s, f_id) :: f_sm,
                      f_id + 1 ))
            alphabet
            (rest, visited, transitions, state_map, id)
        in
        aux visited' transitions' state_map' id' pending'
  in
  let _, transitions, state_map = aux [] [] [ (p0, 0) ] 1 [ p0 ] in
  let qf =
    List.filter_map
      (fun (states, id) -> if IntSet.mem nfa.qf states then Some id else None)
      state_map
  in
  { q0 = 0; qf; transitions }

(*
  <https://swaminathanj.github.io/fsm/dfaminimization.html>
  <https://en.wikipedia.org/wiki/DFA_minimization#Hopcroft%27s_algorithm>
*)
let hopcroft (dfa : dfa) : dfa =
  let all_states =
    IntSet.of_list (List.flatten (List.map (fun (s1, _, s2) -> [ s1; s2 ]) dfa.transitions))
  and alphabet = CharSet.of_list (List.map (fun (_, c, _) -> c) dfa.transitions)
  and delta start c =
    List.find_map
      (fun (q1, ch, q2) -> if q1 = start && ch = c then Some q2 else None)
      dfa.transitions
  in
  let split partition distinguisher c =
    let in_set, out_set =
      IntSet.partition
        (fun s ->
          match delta s c with
          | Some t -> IntSet.mem t distinguisher
          | None -> false)
        partition
    in
    if IntSet.is_empty in_set || IntSet.is_empty out_set then [ partition ] else [ in_set; out_set ]
  in
  let rec aux partitions =
    let refine parts =
      let rec aux result changed = function
        | [] -> (List.rev result, changed)
        | part :: rest ->
            let rec split_by_char alphas subparts changed =
              match CharSet.cardinal alphas with
              | 0 -> (subparts, changed)
              | _ ->
                  let c = CharSet.choose alphas in
                  let subparts' =
                    List.fold_left (fun acc p -> List.rev_append (split p part c) acc) [] subparts
                  in
                  split_by_char (CharSet.remove c alphas) subparts'
                    (changed || List.length subparts' <> List.length subparts)
            in
            let subparts', changed' = split_by_char alphabet [ part ] changed in
            aux (List.rev_append subparts' result) changed' rest
      in
      aux [] false parts
    in
    let new_parts, changed = refine partitions in
    if changed then aux new_parts else new_parts
  in
  let qfs = IntSet.of_list dfa.qf in
  let partitions = aux [ qfs; IntSet.diff all_states qfs ] in
  let find_partition_id q =
    Option.get (List.find_index (fun part -> IntSet.mem q part) partitions)
  in
  {
    q0 = find_partition_id dfa.q0;
    qf = List.sort_uniq compare (List.map find_partition_id dfa.qf);
    transitions =
      List.sort_uniq compare
        (List.map
           (fun (q1, c, q2) -> (find_partition_id q1, c, find_partition_id q2))
           dfa.transitions);
  }

let eval_dfa (dfa : dfa) (str : string) : bool =
  let delta q c =
    List.find_map (fun (q1, ch, q2) -> if q1 = q && ch = c then Some q2 else None) dfa.transitions
  in
  let rec aux state i =
    if i >= String.length str then List.mem state dfa.qf
    else
      match delta state str.[i] with
      | Some state' -> aux state' (i + 1)
      | None -> false
  in
  aux dfa.q0 0

let print_dfa (dfa : dfa) : unit =
  Printf.printf "{ q0 = %d; qf = [" dfa.q0;
  List.iteri
    (fun i qf ->
      if i > 0 then Printf.printf "; ";
      Printf.printf "%d" qf)
    dfa.qf;
  Printf.printf "];\n  transitions = [\n";
  List.iteri
    (fun i (q1, c, q2) ->
      if i > 0 then Printf.printf ";\n";
      Printf.printf "    (%d, '%c', %d)" q1 c q2)
    dfa.transitions;
  Printf.printf "\n  ]\n}\n"

let () =
  Printf.printf "Regex?> ";
  flush stdout;
  let regex = read_line () in
  try
    let min_dfa =
      regex |> tokenize |> explicitize_concat |> shunting_yard |> thompson |> nfa_to_dfa |> hopcroft
    in
    Printf.printf "min-DFA:\n";
    print_dfa min_dfa;
    while true do
      Printf.printf "Test string?> ";
      flush stdout;
      let test_str = read_line () in
      if String.trim test_str = "" then exit 0;
      let result = eval_dfa min_dfa test_str in
      Printf.printf "Result: %b\n" result
    done
  with
  | Failure msg -> Printf.eprintf "Error: %s\n" msg
  | e -> Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e)
