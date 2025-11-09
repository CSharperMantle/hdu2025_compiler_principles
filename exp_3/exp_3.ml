type symbol =
  | Terminal of string
  | NonTerminal of int
  | Epsilon

(* A context-free production rule. *)
type production = {
  lhs : int;
  rhs : symbol list;
}

type grammar = production list

(* Example grammar from Example 4.14. *)
let grammar_example_4_14 =
  [
    { lhs = 1; rhs = [ NonTerminal 2; Terminal "c" ] };
    { lhs = 1; rhs = [ Terminal "c" ] };
    { lhs = 2; rhs = [ NonTerminal 3; Terminal "b" ] };
    { lhs = 2; rhs = [ Terminal "b" ] };
    { lhs = 3; rhs = [ NonTerminal 1; Terminal "a" ] };
    { lhs = 3; rhs = [ Terminal "a" ] };
  ]

(*
  SAFETY:
    The set of all LHS of a sound grammar is the same as its non-terminal set.
*)
let nonterminals (g : grammar) : int list =
  let rec aux result = function
    | [] -> List.sort_uniq compare result
    | first :: rest -> aux (first.lhs :: result) rest
  in
  aux [] g

(*
  SAFETY:
    We don't care about holes in numbering.
*)
let alloc_nonterminal_id (g : grammar) : int =
  let nt = nonterminals g in
  List.fold_left max 0 nt + 1

let eliminate_direct_left_recursion (grammar : grammar) : grammar =
  let nt = nonterminals grammar in
  let rec aux g = function
    | [] -> g
    | a :: rest ->
        let rules = List.filter (fun r -> r.lhs = a) g in
        let recs, non_recs =
          List.partition
            (fun r ->
              match r.rhs with
              | NonTerminal sym :: _ when sym = a -> true
              | _ -> false)
            rules
        in
        if List.is_empty recs then aux g rest
        else
          let a' = alloc_nonterminal_id g in
          let g' =
            (* others *)
            List.filter (fun r -> r.lhs <> a) g
            (* A -> Aa => A' -> aA' *)
            @ List.map (fun r -> { lhs = a; rhs = r.rhs @ [ NonTerminal a' ] }) non_recs
            (* A -> b => A -> bA' *)
            @ List.map
                (fun r ->
                  match r.rhs with
                  | NonTerminal _ :: alpha -> { lhs = a'; rhs = alpha @ [ NonTerminal a' ] }
                  | _ -> failwith "unreachable")
                recs
            (* => A' -> ε *)
            @ [ { lhs = a'; rhs = [ Epsilon ] } ]
          in
          aux g' rest
  in
  aux grammar nt

let eliminate_left_recursion (grammar : grammar) : grammar =
  let nt = nonterminals grammar in
  let rec aux scanned g =
    (*
      Substitute away non-terminal B from A -> Bb.
      Since B may have multiple productions, we need to return a list and then flatten it.
    *)
    let substitute rule =
      match rule.rhs with
      | NonTerminal b :: beta ->
          List.filter_map
            (fun r -> if r.lhs = b then Some { lhs = rule.lhs; rhs = r.rhs @ beta } else None)
            g
      | _ -> [ rule ]
    in
    function
    | [] -> g
    | a :: rest ->
        let rules = List.filter (fun r -> r.lhs = a) g in
        let candidates =
          List.filter
            (fun r ->
              List.exists
                (fun s ->
                  match r.rhs with
                  | NonTerminal sym :: _ when sym = s -> true
                  | _ -> false)
                scanned)
            rules
        in
        let g' =
          List.flatten (List.map (fun r -> if List.mem r candidates then substitute r else [ r ]) g)
        in
        (* If the grammar changed, reiterate to find new substitution opportunities. *)
        if g' <> g then aux [] g' nt
        else aux (a :: scanned) (eliminate_direct_left_recursion g') rest
  in
  aux [] grammar nt

type trie =
  | TrieNode of symbol * trie
  | TrieBranch of trie list
  | TrieTerminal

let join_trie (trie : trie option) (str : symbol list) : trie =
  let rec make_trie = function
    | [] -> TrieTerminal
    | sym :: rest -> TrieNode (sym, make_trie rest)
  in
  let rec graft_trie = function
    | TrieTerminal -> TrieTerminal
    | TrieBranch _ -> TrieTerminal
    | TrieNode (s, t) -> TrieNode (s, graft_trie t)
  in
  let rec aux tr str =
    match (tr, str) with
    | TrieTerminal, [] -> TrieTerminal
    | TrieTerminal, _ -> TrieBranch [ TrieTerminal; make_trie str ]
    | TrieNode (s1, child), s2 :: rest when s1 = s2 -> TrieNode (s1, aux child rest)
    | TrieNode (s, child), _ -> TrieBranch [ TrieNode (s, child); make_trie str ]
    | TrieBranch children, _ ->
        let rec merge = function
          | [] -> [ make_trie str ]
          | first :: rest -> (
              match (first, str) with
              | TrieNode (s, child), s' :: rest' when s = s' ->
                  TrieNode (s, aux child rest') :: rest
              | _ -> first :: merge rest)
        in
        TrieBranch (merge children)
  in
  match trie with
  | None -> make_trie str
  | Some trie -> graft_trie (aux trie str)

type trie_forest = trie list

let join_forest (forest : trie_forest) (str : symbol list) : trie_forest =
  let rec aux str = function
    | [] -> [ join_trie None str ]
    | this :: rest -> (
        match (this, str) with
        | TrieNode (s1, _), s2 :: _ when s1 = s2 -> join_trie (Some this) str :: rest
        | _, _ -> this :: aux str rest)
  in
  aux str forest
