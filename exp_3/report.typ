#import "../assets/hdu-report-typst/template/template.typ": *

#show: project.with(
  title: [
    杭州电子科技大学\
    《编译原理》课程实践\
    实验报告\
  ],
  subtitle: [],
  class: "计算机科学英才班",
  department: "卓越学院",
  authors: "鲍溶",
  author_id: "23060827",
  date: datetime.today(),
  cover_style: "hdu_report",
)

#toc()
#pagebreak()

= 实验目的

== 上下文无关文法左递归消去算法实现

- 理解上下文无关文法中左递归的概念及其对语法分析的影响。
- 掌握消去上下文无关文法中直接和间接左递归的算法。
- 培养运用编程语言实现文法变换的能力。

== 文法左公共因子提取方法及实现

- 理解上下文无关文法中的左公共因子的概念及其对语法分析的影响。
- 掌握从上下文无关文法中提取左公共因子的算法，形成无二义性的语法结构。
- 熟练运用数据结构（如 Trie）处理和优化文法。

== 上下文无关文法 $"FIRST"$ 集和 $"FOLLOW"$ 集求解及实现

- 理解上下文无关文法中 $"FIRST"$ 集和 $"FOLLOW"$ 集的概念及其在语法分析中的重要性。
- 掌握计算文法中 $"FIRST"$ 集和 $"FOLLOW"$ 集的算法及其实现。
- 培养分析和解决文法问题的能力。

== $"LL"(1)$ 文法判定与预测分析器设计及实现

- 理解 $"LL"(1)$ 文法的概念及其在语法分析中的应用。
- 掌握判定文法是否为 $"LL"(1)$ 的方法。
- 学习设计和实现 $"LL"(1)$ 预测分析器的过程。
- 培养运用编程语言实现自顶向下语法分析的能力。

= 实验内容与实验要求

== 上下文无关文法左递归消去算法实现

=== 理论背景

左递归是指文法中存在非终结符 $A$ 能够推导出以自身开头的句型，即 $A arrow.r.double A alpha$。左递归分为直接左递归和间接左递归两种形式：

- *直接左递归*：形如 $A -> A alpha | beta$ 的产生式，其中 $alpha$ 和 $beta$ 是任意文法符号序列，且至少有一个 $beta$ 不以 $A$ 开头。
- *间接左递归*：通过一系列推导，非终结符 $A$ 可以推导出以 $A$ 开头的句型，如 $A arrow.r.double B alpha arrow.r.double A beta alpha$。

左递归会导致自顶向下语法分析器陷入无限递归，因此需要改写。消除直接左递归的方法是引入新的非终结符 $A'$，将产生式改写为：

#equation(
  $
     A & -> beta A' \
    A' & -> alpha A' | epsilon
  $,
) <eq:left-rec-rewrite>

消除间接左递归则需要先通过代换将间接左递归转化为直接左递归，再使用上述方法消除。

=== 任务描述

实现消去上下文无关文法中所有左递归的算法。首先对非终结符集合进行排序，然后按顺序遍历每个非终结符，检查其候选式是否以排在其前面的非终结符开头，若是则进行代换，将间接左递归转化为直接左递归。最后消去所有直接左递归，确保输出文法与输入文法等价且无左递归。

=== 实验步骤

+ 对非终结符集合进行排序。
+ 按顺序遍历每个非终结符，检查其候选式是否以排在其前面的非终结符开头，并进行代换。
+ 消去直接左递归。

=== 实验要求

+ 输入：一个上下文无关文法，包括非终结符、终结符和产生式。输出: 消去左递归后的文法。
+ 应处理直接和间接左递归，确保输出文法与输入文法等价。

== 文法左公共因子提取方法及实现

=== 理论背景

左公共因子是指同一非终结符的多个候选式具有相同的前缀。形如 $A -> alpha beta_1 | alpha beta_2$ 的产生式存在左公共因子 $alpha$。左公共因子会导致自顶向下语法分析器在选择产生式时产生回溯，降低分析效率，甚至可能引入二义性。

提取左公共因子的方法是将产生式改写为：

#equation(
  $
     A & -> alpha A' \
    A' & -> beta_1 | beta_2
  $,
) <eq:common-prefix-rewrite>

字典树（Trie）是一种高效的数据结构，可用于识别和提取最长公共前缀。通过构建 Trie，可以系统地分析所有候选式的公共前缀结构。

=== 任务描述

实现从上下文无关文法中提取左公共因子的算法。对每个非终结符的候选式，识别最长的公共前缀。构建 Trie 辅助提取最长公共前缀，将公共前缀提取为新非终结符的候选式。输出去除左公共因子的等价文法，确保输出文法无二义性且易于自顶向下分析。

=== 实验步骤

+ 对每个非终结符的候选式，识别最长的公共前缀。
+ 构建字典树（Trie），辅助提取最长公共前缀，将公共前缀提取为新非终结符的候选式。
+ 输出去除左公共因子的等价文法。

=== 实验要求

+ 输入：一个上下文无关文法，包括非终结符、终结符和产生式。输出：提取左公共因子后的文法。
+ 使用适当的数据结构（如 Trie）提高提取效率。
+ 确保输出文法无二义性，且与输入文法等价。

== 上下文无关文法 $"FIRST"$ 集和 $"FOLLOW"$ 集求解及实现

=== 理论背景

*$bold("FIRST")$ 集*：对于文法符号串 $alpha$，$"FIRST"(alpha)$ 是可以从 $alpha$ 推导出的所有串的首个终结符的集合，即 $"FIRST"(alpha) = { a | alpha arrow.r.double^* a B }$。计算规则如下：

- 若 $X$ 是终结符，则 $"FIRST"(X) = {X}$；
- 若 $X -> epsilon$，则将 $epsilon$ 加入 $"FIRST"(X)$；
- 若 $X -> Y_1 Y_2 dots.h Y_k$，则：
  - 将 $"FIRST"(Y_1) - {epsilon}$ 加入 $"FIRST"(X)$；
  - 若 $epsilon in "FIRST"(Y_i)$（$1 <= i <= k-1$），则将 $"FIRST"(Y_(i+1)) - {epsilon}$ 加入 $"FIRST"(X)$；
  - 若 $epsilon in "FIRST"(Y_i)$（$1 <= i <= k$），则将 $epsilon$ 加入 $"FIRST"(X)$。

*$bold("FOLLOW")$ 集*：对于非终结符 $A$，$"FOLLOW"(A)$ 是可能在某个句型中紧跟在 $A$ 右边的终结符集合。计算规则如下：

- 将 $\$$ 加入 $"FOLLOW"(S)$，其中 $S$ 是开始符号；
- 若存在产生式 $A -> alpha B beta$，则将 $"FIRST"(beta) - {epsilon}$ 加入 $"FOLLOW"(B)$；
- 若存在产生式 $A -> alpha B$ 或 $A -> alpha B beta$（且 $epsilon in "FIRST"(beta)$），则将 $"FOLLOW"(A)$ 加入 $"FOLLOW"(B)$。

=== 任务描述

实现求解上下文无关文法的 $"FIRST"$ 集和 $"FOLLOW"$ 集的算法。输入上下文无关文法后，按照算法规则迭代计算每个非终结符的 $"FIRST"$ 集和 $"FOLLOW"$ 集，直至收敛。输出结果应准确反映文法的各种情况，为后续 $"LL"(1)$ 分析器的构造提供基础。

=== 实验步骤

+ 输入上下文无关文法。
+ 计算每个非终结符的 $"FIRST"$ 集。
+ 计算每个非终结符的 $"FOLLOW"$ 集。

=== 实验要求

+ 输入：一个上下文无关文法，包括非终结符、终结符和产生式。输出：每个非终结符的 $"FIRST"$ 集和 $"FOLLOW"$ 集。
+ 算法应考虑文法的各种情况，确保输出结果准确。

== $"LL"(1)$ 文法判定与预测分析器设计及实现

=== 理论背景

$"LL"(1)$ 文法（从左向右扫描、最左推导、向前看 1 个符号文法）是一类可以用自顶向下方法进行语法分析的上下文无关文法。文法 $G$ 是 $"LL"(1)$ 的充要条件是：对于文法中的每个非终结符 $A$ 的任意两个不同产生式 $A -> alpha | beta$，满足：

- $"FIRST"(alpha) sect "FIRST"(beta) = emptyset$
- 若 $epsilon in "FIRST"(alpha)$，则 $"FIRST"(beta) sect "FOLLOW"(A) = emptyset$；
- 若 $epsilon in "FIRST"(beta)$，则 $"FIRST"(alpha) sect "FOLLOW"(A) = emptyset$

预测分析表 $M[A, a]$ 记录了当栈顶非终结符为 $A$、向前看符号为 $a$ 时应该使用的产生式。构造规则如下：

+ 对于产生式 $A -> alpha$，若 $a in "FIRST"(alpha)$，则将 $A -> alpha$ 加入 $M[A, a]$
+ 若 $epsilon in "FIRST"(alpha)$，则对所有 $b in "FOLLOW"(A)$，将 $A -> alpha$ 加入 $M[A, b]$

=== 任务描述

实现 $"LL"(1)$ 文法的判定算法和预测分析器。首先判断输入文法是否满足 $"LL"(1)$ 条件，若是，则构造预测分析表。实现预测分析器，根据预测分析表对输入字符串进行语法分析，输出分析过程及结果（接受或拒绝）。通过实际例子（如表达式文法）验证实现的正确性。

=== 实验步骤

+ 输入上下文无关文法。
+ 判断文法是否为LL(1)。
+ 构造预测分析表。
+ 实现预测分析器，能够根据输入串进行语法分析。

=== 实验要求

+ 输入：一个上下文无关文法，包括非终结符、终结符和产生式。输出：文法是否为 $"LL"(1)$ 的判断结果；若是，给出预测分析表。
+ 输入一个字符串，输出语法分析结果（是否成功以及分析过程）。

= 设计方案与算法描述

== 上下文无关文法左递归消去算法实现

== 文法左公共因子提取方法及实现

== 上下文无关文法 $"FIRST"$ 集和 $"FOLLOW"$ 集求解及实现

== $"LL"(1)$ 文法判定与预测分析器设计及实现

= 测试结果

== 上下文无关文法左递归消去算法实现

#img(
  image("assets/task-1-demo.png"),
  caption: [左递归消去算法演示]
) <figure:task-1-demo>

== 文法左公共因子提取方法及实现

#img(
  image("assets/task-2-demo.png"),
  caption: [左公共因子提取算法演示]
) <figure:task-2-demo>

== 上下文无关文法 $"FIRST"$ 集和 $"FOLLOW"$ 集求解及实现

#img(
  image("assets/task-3-grammar.png"),
  caption: [例 4-6 的文法结构]
) <figure:task-3-grammar>

#img(
  image("assets/task-3-demo-first.png"),
  caption: [部分文法符号的 $"FIRST"$ 集计算演示]
) <figure:task-3-demo-first>

#img(
  image("assets/task-3-demo-follow.png"),
  caption: [所有非终结符的 $"FOLLOW"$ 集计算演示]
) <figure:task-3-demo-follow>

== $"LL"(1)$ 文法判定与预测分析器设计及实现

#img(
  image("assets/task-4-demo.png"),
  caption: [$"LL"(1)$ 分析表与分析器运行演示]
) <figure:task-4-demo>

= 源代码

完整的实验程序与报告源代码可以从 #link("https://github.com/CSharperMantle/hdu2025_compiler_principles/tree/main/exp_3") 处获取。

#code(
  ```ml
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
  ```,
  caption: [直接左递归消除算法的 OCaml 实现]
) <code:task-1-dir-left-rec-elim>

#code(
  ```ml
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
  ```,
  caption: [间接左递归消除算法的 OCaml 实现]
) <code:task-1-indir-left-rec-elim>

#code(
  ```ml
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

  type trie_forest = trie list

  let trie_forest_empty : trie list = []

  let rec join_forest (forest : trie_forest) (str : symbol list) : trie_forest =
    match forest with
    | [] -> [ join_trie None str ]
    | this :: rest -> (
        match (this, str) with
        | TrieNode (s1, _), s2 :: _ when s1 = s2 -> join_trie (Some this) str :: rest
        | _, _ -> this :: join_forest rest str)
  ```,
  caption: [字典树与字典森林的 OCaml 实现]
) <code:task-2-trie>

#code(
  ```ml
  ```,
  caption: [提取左公因子算法的 OCaml 实现]
) <code:task-2-common-prefix-elim>

#code(
  ```ml
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
  ```,
  caption: [$"INITIAL"(A)$ 求解的 OCaml 实现]
) <code:task-3-deduce-initials>

#code(
  ```ml
  let first (grammar : grammar) (str : symbol list) : FirstSet.t =
    let rec aux = function
      | [] -> FirstSet.singleton FirstSymEpsilon
      | x :: rest ->
          let initials = deduced_initials grammar x |> deduced_initials_to_first in
          let sans_eps = FirstSet.filter (fun e -> e <> FirstSymEpsilon) initials in
          if FirstSet.mem FirstSymEpsilon initials then FirstSet.union sans_eps (aux rest)
          else sans_eps
    in
    if List.is_empty str then FirstSet.singleton FirstSymEpsilon else aux str
  ```,
  caption: [$"FIRST"(alpha)$ 求解的 OCaml 实现]
) <code:task-3-first>

#code(
  ```ml
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
                        let first_beta = first g beta in
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
  ```,
  caption: [${ "FOLLOW"(A) | A in Sigma_"NT" }$ 求解的 OCaml 实现]
) <code:task-3-follow>

#code(
  ```ml
  let decide_ll_1 (grammar : grammar) : bool =
    let decide_one_nonterm g follows a =
      let first_pairs =
        List.filter_map (fun r -> if r.lhs = a then Some (first g r.rhs) else None) g
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
  ```,
  caption: [$"LL"(1)$ 文法判别的 OCaml 实现]
) <code:task-4-ll-1-decide>

#code(
  ```ml
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
  ```,
  caption: [构造 $"LL"(1)$ 分析表的 OCaml 实现]
) <code:task-4-parser-constr>

#code(
  ```ml
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
  ```,
  caption: [语法分析器的 OCaml 实现]
) <code:task-4-parse>
