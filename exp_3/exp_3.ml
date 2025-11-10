type symbol =
  | Terminal of string
  | NonTerminal of int

(* A context-free production rule. rhs = [] means ε. *)
type production = {
  lhs : int;
  rhs : symbol list;
}

(*
  ASSUMPTION:
    The first rule's LHS is the starting symbol.
*)
type grammar = production list

(* Example grammar from Example 4-6 for FIRST() and SELECT(). *)
let grammar_example_4_6 =
  [
    { lhs = 1; rhs = [ NonTerminal 3; NonTerminal 2 ] };
    { lhs = 2; rhs = [ Terminal "+"; NonTerminal 3; NonTerminal 2 ] };
    { lhs = 2; rhs = [] };
    { lhs = 3; rhs = [ NonTerminal 5; NonTerminal 4 ] };
    { lhs = 4; rhs = [ Terminal "*"; NonTerminal 5; NonTerminal 4 ] };
    { lhs = 4; rhs = [] };
    { lhs = 5; rhs = [ Terminal "("; NonTerminal 1; Terminal ")" ] };
    { lhs = 5; rhs = [ Terminal "id" ] };
  ]

(* Example grammar from Example 4-10 for common prefix extraction. *)
let grammar_example_4_10 =
  [
    { lhs = 1; rhs = [ Terminal "a"; NonTerminal 1; Terminal "b" ] };
    { lhs = 1; rhs = [ Terminal "a"; NonTerminal 1; Terminal "a" ] };
    { lhs = 1; rhs = [ Terminal "b" ] };
  ]

(* Example grammar from Example 4-14 for left recursion elimination. *)
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
let alloc_nonterminal_id (g : grammar) : int = List.fold_left max 0 (nonterminals g) + 1

let eliminate_direct_left_recursion (grammar : grammar) : grammar =
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
            @ [ { lhs = a'; rhs = [] } ]
          in
          aux g' rest
  in
  aux grammar (nonterminals grammar)

let eliminate_left_recursion (grammar : grammar) : grammar =
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
        if g' <> g then aux [] g' (a :: rest)
        else aux (a :: scanned) (eliminate_direct_left_recursion g') rest
  in
  aux [] grammar (nonterminals grammar)

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
    | TrieTerminal | TrieBranch _ -> TrieTerminal
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

let rec collect_trie_prefix (trie : trie) : symbol list =
  match trie with
  | TrieTerminal | TrieBranch _ -> []
  | TrieNode (s, child) -> s :: collect_trie_prefix child

type trie_forest = trie list

let trie_forest_empty : trie list = []

let rec join_forest (forest : trie_forest) (str : symbol list) : trie_forest =
  match forest with
  | [] -> [ join_trie None str ]
  | this :: rest -> (
      match (this, str) with
      | TrieNode (s1, _), s2 :: _ when s1 = s2 -> join_trie (Some this) str :: rest
      | _, _ -> this :: join_forest rest str)

let rec has_prefix (lhs : symbol list) (rhs : symbol list) : bool =
  match (lhs, rhs) with
  | _, [] -> true (* ε is a prefix of any string. *)
  | [], _ :: _ -> false (* non-ε isn't a prefix of ε. *)
  | l0 :: _, r0 :: _ when l0 <> r0 -> false
  | _ :: lr, _ :: rr -> has_prefix lr rr

let rec subtract_prefix (lhs : symbol list) (rhs : symbol list) : symbol list =
  match (lhs, rhs) with
  | [], _ -> [] (* ε \ anything -> ε *)
  | _ :: _, [] -> lhs (* s \ ε -> s *)
  | l0 :: lr, r0 :: rr when l0 = r0 -> subtract_prefix lr rr
  | _, _ -> lhs (* The common prefix has been consumed. *)

let eliminate_common_prefix (grammar : grammar) : grammar =
  let rec aux g = function
    | [] -> g
    | a :: rest ->
        let rec build_forest = function
          | [] -> trie_forest_empty
          | rule :: remaining -> join_forest (build_forest remaining) rule.rhs
        in
        let rec replace a a' forest rules =
          match forest with
          | [] -> rules
          | tree :: forest' ->
              let prefix = collect_trie_prefix tree in
              let prefixed, other_rules =
                List.partition
                  (* s is a prefix of s, so filter that out. *)
                  (fun r -> has_prefix r.rhs prefix && List.length r.rhs <> List.length prefix)
                  rules
              in
              let prefix_empty = List.is_empty prefixed in
              (* => A' -> b1 | b2 | ... | bn *)
              let a'_rules =
                List.map (fun r -> { lhs = a'; rhs = subtract_prefix r.rhs prefix }) prefixed
              in
              (if prefix_empty then []
               else
                 (* A -> a(b1 | b2 | ... | bn) => A -> aA' *)
                 [ { lhs = a; rhs = prefix @ [ NonTerminal a' ] } ] @ a'_rules)
              @ replace a (if prefix_empty then a' else a' + 1) forest' other_rules
        in
        let rules, others = List.partition (fun r -> r.lhs = a) g in
        let forest = build_forest rules in
        replace a (alloc_nonterminal_id g) forest rules @ aux others rest
  in
  aux grammar (nonterminals grammar)

module DeducedHeadSet = Set.Make (struct
  type t = string option

  let compare = compare
end)

(* Get all initial terminals that can be deduced from a symbol. *)
let deduced_head (grammar : grammar) (symbol : symbol) : DeducedHeadSet.t =
  let rec rhs_heads table rhs =
    match rhs with
    | [] -> DeducedHeadSet.singleton None (* ε *)
    | Terminal s :: _ -> DeducedHeadSet.singleton (Some s)
    | NonTerminal b :: rest ->
        let sb = List.assoc b table in
        let sans_eps = DeducedHeadSet.filter (fun x -> x <> None) sb in
        if DeducedHeadSet.mem None sb then
          (* b -> ε, so continue with rest *)
          DeducedHeadSet.union sans_eps (rhs_heads table rest)
        else sans_eps
  in
  let rec iterate table =
    let table' =
      List.map
        (fun (nt, set) ->
          let set' =
            List.fold_left
              (fun acc prod -> DeducedHeadSet.union acc (rhs_heads table prod.rhs))
              set
              (List.filter (fun p -> p.lhs = nt) grammar)
          in
          (nt, set'))
        table
    in
    let changed =
      List.exists2
        (fun (_, old_set) (_, new_set) -> not (DeducedHeadSet.equal old_set new_set))
        table table'
    in
    if changed then iterate table' else table'
  in
  match symbol with
  | Terminal s -> DeducedHeadSet.singleton (Some s)
  | NonTerminal a ->
      let nts = nonterminals grammar in
      let init_table = List.map (fun nt -> (nt, DeducedHeadSet.empty)) nts in
      let final_table = iterate init_table in
      List.assoc a final_table

type first_symbol =
  | FirstSymNormal of symbol
  | FirstSymEpsilon

module FirstSet = Set.Make (struct
  type t = first_symbol

  let compare = compare
end)

let deduced_head_to_first (s : DeducedHeadSet.t) : FirstSet.t =
  DeducedHeadSet.to_list s
  |> List.map (fun e ->
      match e with
      | Some s -> FirstSymNormal (Terminal s)
      | None -> FirstSymEpsilon)
  |> FirstSet.of_list

let first (grammar : grammar) (str : symbol list) : FirstSet.t =
  let rec aux g some = function
    | [] -> if some then FirstSet.empty else FirstSet.singleton FirstSymEpsilon
    | x :: rest ->
        let heads = deduced_head g x |> deduced_head_to_first in
        let sans_eps = FirstSet.filter (fun e -> e <> FirstSymEpsilon) heads in
        let this_some = not (FirstSet.is_empty sans_eps) in
        if FirstSet.mem FirstSymEpsilon heads then
          FirstSet.union sans_eps (aux g (some || this_some) rest)
        else sans_eps
  in
  if List.is_empty str then FirstSet.singleton FirstSymEpsilon else aux grammar false str

type follow_symbol =
  | FollowSymNormal of symbol
  | FollowSymEof

module FollowSet = Set.Make (struct
  type t = follow_symbol

  let compare = compare
end)

let first_to_follow (first : FirstSet.t) : FollowSet.t =
  FirstSet.elements first
  |> List.filter_map (fun e ->
      match e with
      | FirstSymNormal sym -> Some (FollowSymNormal sym)
      | FirstSymEpsilon -> None)
  |> FollowSet.of_list

let starting_nonterminal_of (grammar : grammar) : int =
  match grammar with
  | [] -> failwith "empty grammar"
  | first :: _ -> first.lhs

let follow (grammar : grammar) (a : symbol) : follow_symbol list = failwith "todo"
