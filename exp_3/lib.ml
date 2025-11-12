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
          List.map (fun r -> if List.mem r candidates then substitute r else [ r ]) g
          |> List.flatten
        in
        (* If the grammar changed, reiterate to find new substitution opportunities. *)
        if g' <> g then aux scanned g' (a :: rest)
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

module DeducedInitialSet = Set.Make (struct
  type t = string option

  let compare = compare
end)

(* Get all initial terminals that can be deduced from a symbol. *)
let deduced_initials (grammar : grammar) (symbol : symbol) : DeducedInitialSet.t =
  let rec rhs_initials table rhs =
    match rhs with
    | [] -> DeducedInitialSet.singleton None (* ε *)
    | Terminal s :: _ -> DeducedInitialSet.singleton (Some s)
    | NonTerminal b :: rest ->
        let sb = List.assoc b table in
        let sans_eps = DeducedInitialSet.filter (fun x -> x <> None) sb in
        if DeducedInitialSet.mem None sb then
          (* b -> ε, so continue with rest *)
          DeducedInitialSet.union sans_eps (rhs_initials table rest)
        else sans_eps
  in
  let rec iterate table =
    let table' =
      List.map
        (fun (nt, set) ->
          let set' =
            List.fold_left
              (fun acc prod -> DeducedInitialSet.union acc (rhs_initials table prod.rhs))
              set
              (List.filter (fun p -> p.lhs = nt) grammar)
          in
          (nt, set'))
        table
    in
    let changed =
      List.exists2
        (fun (_, old_set) (_, new_set) -> not (DeducedInitialSet.equal old_set new_set))
        table table'
    in
    if changed then iterate table' else table'
  in
  match symbol with
  | Terminal s -> DeducedInitialSet.singleton (Some s)
  | NonTerminal a ->
      let nts = nonterminals grammar in
      let init_table = List.map (fun nt -> (nt, DeducedInitialSet.empty)) nts in
      let final_table = iterate init_table in
      List.assoc a final_table

type first_symbol =
  | FirstSymNormal of string
  | FirstSymEpsilon

module FirstSet = Set.Make (struct
  type t = first_symbol

  let compare = compare
end)

let deduced_initials_to_first (s : DeducedInitialSet.t) : FirstSet.t =
  DeducedInitialSet.to_list s
  |> List.map (fun e ->
      match e with
      | Some s -> FirstSymNormal s
      | None -> FirstSymEpsilon)
  |> FirstSet.of_list

let first (str : symbol list) (grammar : grammar) : FirstSet.t =
  let rec aux = function
    | [] -> FirstSet.singleton FirstSymEpsilon
    | x :: rest ->
        let initials = deduced_initials grammar x |> deduced_initials_to_first in
        let sans_eps = FirstSet.filter (fun e -> e <> FirstSymEpsilon) initials in
        if FirstSet.mem FirstSymEpsilon initials then FirstSet.union sans_eps (aux rest)
        else sans_eps
  in
  if List.is_empty str then FirstSet.singleton FirstSymEpsilon else aux str

type follow_symbol =
  | FollowSymNormal of string
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

type follow_sets = (int * FollowSet.t) list

let follow (grammar : grammar) : follow_sets =
  let rec iterate g table =
    let table' =
      (* For each nonterminal ... *)
      List.map
        (fun (nt, set) ->
          let set' =
            (* For each rule ... *)
            List.fold_left
              (fun acc prod ->
                let rec scan_rhs rule =
                  match rule with
                  | [] -> FollowSet.empty
                  | NonTerminal b :: beta when b = nt ->
                      (* ...Bβ *)
                      let first_beta = first beta g in
                      let follow_from_lhs =
                        if FirstSet.mem FirstSymEpsilon first_beta then
                          (* A -> αB, A -> αBβ and β =>* ε *) List.assoc prod.lhs table
                        else FollowSet.empty
                      in
                      FollowSet.union
                        (FollowSet.union (first_to_follow first_beta) follow_from_lhs)
                        (scan_rhs beta)
                  | _ :: rest -> scan_rhs rest
                in
                FollowSet.union acc (scan_rhs prod.rhs))
              set g
          in
          (nt, set'))
        table
    in
    if List.exists2 (fun (_, set) (_, set') -> not (FollowSet.equal set set')) table table' then
      iterate g table'
    else table'
  in
  let start = starting_nonterminal_of grammar and nt = nonterminals grammar in
  let initial =
    List.map
      (fun id ->
        if id = start then (id, FollowSet.singleton FollowSymEof) else (id, FollowSet.empty))
      nt
  in
  iterate grammar initial

let follow_of (a : int) (follow : follow_sets) = List.assoc a follow

let flatten_follow (follow : follow_sets) =
  List.map (fun (a, set) -> (a, FollowSet.to_list set)) follow

let combinations (k : int) (l : 'a list) : 'a list list =
  let rec aux k l acc =
    match (k, l) with
    | 0, _ -> [ List.rev acc ]
    | _, [] -> []
    | k, x :: rest -> aux (k - 1) rest (x :: acc) @ aux k rest acc
  in
  aux k l []

let reduce_left_unwrap (f : 'a -> 'a -> 'a) (l : 'a list) : 'a =
  match l with
  | [] -> failwith "empty list"
  | x :: rest -> List.fold_left f x rest

let decide_ll_1 (grammar : grammar) : bool =
  let decide_one_nonterm g follows a =
    let first_pairs =
      List.filter_map (fun r -> if r.lhs = a then Some (first r.rhs g) else None) g
      |> combinations 2
    and follow_a = follow_of a follows in
    (* ∀α,β in A -> α|β. FIRST(α) ∩ FIRST(β) = ∅ *)
    let cond_1 =
      List.for_all
        (fun tuple ->
          FirstSet.is_empty (reduce_left_unwrap (fun s si -> FirstSet.inter s si) tuple))
        first_pairs
    (* ∀α,β | (ε ∈ FIRST(α) ∨ ε ∈ FIRST(β)). (FOLLOW(A) ∩ FIRST(α) = ∅ ∨ FOLLOW(A) ∩ FIRST(β) = ∅) *)
    and cond_2 =
      List.filter
        (fun pair -> List.exists (fun s -> FirstSet.mem FirstSymEpsilon s) pair)
        first_pairs
      |> List.for_all (fun pair ->
          List.exists
            (fun s -> FollowSet.is_empty (FollowSet.inter follow_a (first_to_follow s)))
            pair)
    in
    cond_1 && cond_2
  and follows = follow grammar in
  List.for_all (decide_one_nonterm grammar follows) (nonterminals grammar)

(* A row in the LL(1) parser. *)
type parser_row = {
  (* Nonterminal on the LHS. *)
  nonterm : int;
  (*
    Shape: (Terminal or $, production.rhs)[].
  *)
  inputs : (string option * production) list;
}

type parser = {
  start : int;
  rows : parser_row list;
}

let make_parser (grammar : grammar) : parser =
  if not (decide_ll_1 grammar) then failwith "not an LL(1) grammar"
  else
    let follows = follow grammar in
    let rows =
      List.map
        (fun a ->
          let inputs =
            (* ∀A -> α ... *)
            List.filter (fun r -> r.lhs = a) grammar
            |> List.map (fun r ->
                first grammar r.rhs |> FirstSet.to_list
                (* ... Va ∈ FIRST(α) ...*)
                |> List.map (fun sym ->
                    (* ... M[..., a] = A -> α *)
                    match sym with
                    | FirstSymNormal s -> [ (Some s, r) ]
                    | FirstSymEpsilon ->
                        (* ∀A -> α | ε ∈ FIRST(α). Vb ∈ FOLLOW(A). M[A, b] = A -> α *)
                        follow_of a follows |> FollowSet.to_list
                        |> List.map (fun b ->
                            match b with
                            | FollowSymNormal s -> (Some s, r)
                            | FollowSymEof -> (None, r)))
                |> List.flatten)
            |> List.flatten
          in
          { nonterm = a; inputs })
        (nonterminals grammar)
    in
    { start = starting_nonterminal_of grammar; rows }

let parse (tokens : string list) (parser : parser) : bool =
  let rec aux stack input =
    match (stack, input) with
    | [], [] -> true
    | [], _ :: _ -> false (* Input remains but stack is empty *)
    | Terminal s0 :: stack', t0 :: input' ->
        if s0 = t0 then aux stack' input' else false (* Terminal mismatch *)
    | Terminal _ :: _, [] -> false (* Expecting terminal but input exhausted *)
    | NonTerminal a :: stack', _ -> (
        let lookahead =
          match input with
          | [] -> None
          | tok :: _ -> Some tok
        in
        let row = List.find_opt (fun r -> r.nonterm = a) parser.rows in
        match row with
        | None -> false (* No rule for this nonterminal *)
        | Some r -> (
            let prod = List.find_opt (fun (tok, _) -> tok = lookahead) r.inputs in
            match prod with
            | None -> false (* No production for this input *)
            | Some (_, p) -> aux (p.rhs @ stack') input))
  in
  aux [ NonTerminal parser.start ] tokens
