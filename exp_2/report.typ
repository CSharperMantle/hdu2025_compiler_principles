#import "@preview/algorithmic:1.0.5"
#import algorithmic: algorithm-figure, style-algorithm
#import "@preview/finite:0.5.0"

#import "../assets/hdu-report-typst/template/template.typ": *

#show: style-algorithm
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
  date: datetime(year: 2025, month: 11, day: 1),
  cover_style: "hdu_report",
)

#show smallcaps: set text(font: font_style.heiti)

#toc()
#pagebreak()

= 实验目的

== 正规表达式转NFA算法及实现

+ 掌握正规表达式与有限自动机的基本概念和转换方法。
+ 了解非确定有限自动机（NFA）的构建过程。
+ 熟悉编程实现正规表达式到 NFA 转换的算法。
+ 提高编程能力和算法设计的技能。

== NFA转DFA算法及实现

+ 掌握非确定有限自动机（NFA）与确定有限自动机（DFA）的基本概念及其转换方法。
+ 了解 NFA 到 DFA 转换过程中的子集构造算法。
+ 实现 NFA 到 DFA 的转换算法，并验证 DFA 的正确性。
+ 设计合理的数据结构，延续上一次实验的结构，以便为后续 DFA 最小化实验任务做好准备。
+ 提高编程能力及算法设计和优化的技能。

== DFA最小化算法及实现

+ 掌握确定有限自动机（DFA）的最小化原理和算法，尤其是 Hopcroft 算法（即课上所讲的“求异法”）。
+ 学习 DFA 状态等价性的判定方法，理解最小化过程中的分割和合并策略。
+ 实现 DFA 最小化算法，并验证最小化 DFA 的正确性。
+ 延续前両次实验的设计，确保数据结构能贯通整箇自动机系列实验。
+ 提高算法优化和编程实现能力，增彊对编译原理的理解。

= 实验内容与实验要求

== 正规表达式转NFA算法及实现

=== 理论背景

正规表达式是一种用于描述词法单元的形式化表示法，而 NFA 是一种用于词法分析的状态机。正规表达式可以通过算法转化为 NFA，从而实现对字符串的模式匹配。

=== 任务描述

实现正规表达式到 NFA 的转换算法，并验证生成的 NFA 对给定输入字符串的接受性。同时，设计适合 NFA 的数据结构，为后续 NFA 转 DFA、DFA 最小化等实验任务提供基础支持。

=== 实验步骤

+ 解析输入的正规表达式。
+ 构建对应的 NFA，包括处理基本符号、连接、并联（或操作）、闭包（星号操作）等运算。
+ 设计并实现合理的数据结构表示NFA，如状态集合、转移关系、初始状态和接受状态。
+ 对 NFA 进行模拟，验证其是否接受给定的输入字符串。

=== 案例分析

给定一个简单的正规表达式（如 `a(b|c)*`），手动推导其 NFA，并用程序实现自动生成 NFA 的过程。

=== 实验要求

- 输入：正规表达式和多个测试字符串。输出：生成的 NFA 状态集合及其转换关系，指明每个测试字符串是否被 NFA 接受。
- 支持基本的正规表达式运算符，如连接（`ab`）、或（`a|b`）、闭包（ `a*`）。实现 Thompson 构造法，将正规表达式分解为基本操作，然后逐步合成NFA。
- 设计合理的数据结构来表示 NFA（如图的表示方式），应包括状态集、状态转移表、初始状态和接受状态的表示。数据结构需具备扩展性，以便在后续实验中使用，如 NFA 到 DFA 的转换、 DFA 的最小化。

== NFA 转 DFA 算法及实现

=== 理论背景

NFA 是一种可以处理多条路径的状态机，而 DFA 是其确定版本，不存在多条路径。通过子集构造算法（Subset Construction），可以将 NFA 转换为等价的 DFA，从而实现字符串匹配的确定性处理。

=== 任务描述

实现将 NFA 转换为 DFA 的算法，并对转换后的 DFA 进行验证。同时，设计适合 DFA 的数据结构，使其兼容前一次实验的 NFA 数据结构。

=== 实验步骤

+ 理解子集构造算法的原理，包括 $epsilon$-闭包的计算和状态集合的映射。
+ 利用子集构造算法，将 NFA 转换为 DFA。
+ 设计并实现 DFA 的数据结构，确保其能够表示状态集合、状态转换、初始状态和接受状态。
+ 验证 DFA 的正确性，对比 DFA 与 NFA 在同一组测试输入上的匹配结果。

=== 实验要求

- 输入：一个 NFA（包括状态集、转换表、初始状态和接受状态集合）和多个测试字符串。输出：生成的 DFA 状态集合及其转换关系，指明每个测试字符串是否被 DFA 接受。
- 实现子集构造算法，将 NFA 状态集合的子集映射为 DFA 的单个状态。处理 $epsilon$-闭包及其状态转换，生成对应的 DFA。
- 在上一实验的基础上，设计 DFA 的数据结构，包含状态集合、转换关系、初始状态和接受状态集合的表示。确保数据结构可以支持后续的 DFA 最小化任务，便于后续实验任务的延续。

== DFA最小化算法及实现

=== 理论背景

DFA 最小化是将 DFA 状态数减少到最小的过程，通过合并等价状态，实现最优的状态机表示。Hopcroft 算法是求异法的一种高效实现，它通过维护状态的分割并使用快速查找机制来优化最小化过程。

=== 任务描述

实现 DFA 最小化算法，将给定的 DFA 简化为状态数最少的等价 DFA。验证最小化  

=== 实验步骤

+ 理解 Hopcroft 算法的基本原理，包括状态等价的判定标准和状态合并的方法。
+ 实现 Hopcroft 算法，将原 DFA 简化为等价的最小化 DFA。
+ 设计合理的数据结构表示最小化后的 DFA，确保其与前两次实验的 NFA 和 DFA 数据结构保持一致。
+ 验证最小化 DFA 的正确性，确保其接受的语言与原 DFA 相同。

=== 实验要求

- 输入：一个 DFA（包括状态集合、状态转换表、初始状态和接受状态集合）。输出：最小化后的 DFA 状态集合及其转换关系，指明最小化前后的状态数和状态转换关系。
- 实现 Hopcroft 算法，通过分割状态集合和快速查找机制来最小化 DFA。支持状态等价性判定及状态的合并操作。
- 设计适合 Hopcroft 算法的高效数据结构，如用于记录状态分割的集合、合并后的状态转换表等。

= 设计方案与算法描述

#equation(
  $
    E &-> (E)   && #h(2em) "(grouping)"         \
      &-> E*    && #h(2em) "(closure)"          \
      &-> E+    && #h(2em) "(positive closure)" \
      &-> E E   && #h(2em) "(concatenation)"    \
      &-> E | E && #h(2em) "(alternation)"      \
      &-> alpha in Sigma                        \
  $,
  caption: [本实验中正则表达式的语法]
) <eq:regex-grammar>

本实验中所实现的正则表达式语法如@eq:regex-grammar 所示。为了让字母表 $Sigma$ 中包含操作符字符 `<(>`、`<)>`、`<*>`、`<+>` 和 `<|>`，我们将这些字符以反斜杠转义后形式加入。

== 正规表达式转NFA算法及实现

=== 带转义的分词

将正则表达式字符串转换为词素序列相对简单，仅需在每轮循环中判断字符串开头的字符类型并输出相应的词素即可。由于实验中使用 OCaml 语言，需要将该过程实现为@algo:lexing 所示的递归形式。其实现见@code:task-1-tokenize。

#algorithm-figure(
  [正则表达式分词],
  supplement: [算法],
  vstroke: .5pt + luma(200),{
    import algorithmic: *
    Procedure(
      "Tokenize",
      $S$,
      {
        Comment[$S$：待转换的字符流]
        LineBreak
        IfElseChain(
          [$S$已耗尽],
          {
            Return[]
          },
          {
            Assign[$c, S'$][#CallInline([Consume], [S])]
            IfElseChain(
              [$c in {#raw("<(>"), #raw("<)>"), #raw("<+>"), #raw("<*>"), #raw("<|>")}$],
              {
                ([输出相应的关键词词素 $c$],)
                Call([Tokenize], [$S'$])
              },
              [$c = #raw("<\>")$],
              {
                Assign[$c', S''$][#CallInline([Consume], [S'])]
                ([输出相应的字符词素 $c'$],)
                Call([Tokenize], [$S''$])
              },
              {
                ([输出相应的字符词素 $c$],)
                Call([Tokenize], [$S'$])
              }
            )
          }
        )
      }
    )
  }
) <algo:lexing>

=== 连接运算与后缀表达式

正则表达式中的连接运算是一个二元操作，但是其在表达式字符串中并不会显式写出。如果首先在需要的位置插入一个表示连接操作的词素，在进一步处理时就会减少很多特殊处理。由于我们并不在此阶段构造完整的语法树，因此需要在词素层面确定插入连接运算的位置。观察@eq:regex-grammar 可知，当满足以下所有条件时，需要在两个词素之间插入一个连接运算词素：

+ 当前位置词素为 $alpha$、`<*>`、`<+>` 或 `<)>`；
+ 下一个词素为 $alpha$ 或 `<(>`。

插入连接运算词素后，便可以使用调度场算法（Knuth's Shunting Yard）将中缀表达式转换为后缀表达式，其描述如@algo:shunting-yard 所示。

#algorithm-figure(
  [调度场算法],
  supplement: [算法],
  vstroke: .5pt + luma(200),
  {
    import algorithmic: *
    Procedure(
      "Shunting-Yard",
      $s$,
      {
        Comment[$s$：待转换的中缀表达式词素列表]
        LineBreak
        Assign[$o$][空串]
        Assign[$T$][空栈]
        LineBreak
        For(
          [$t$ #sym.in $s$],
          {
            IfElseChain(
              [$s$ 是操作数],
              {
                Call([Append], [$o$, $t$])
              },
              [$t =$ `<(>`],
              {
                Call([Push], [$T$, $t$])
              },
              [$t =$ `<)>`],
              {
                While(
                  [#sym.not #CallInline([Empty], [$T$]) #sym.and #CallInline([Top], [$T$]) $!=$ `<(>`],
                  {
                    Call([Append], [$o$, #CallInline([Pop], [$T$])])
                  }
                )
                If(
                  [#CallInline([Empty], [$T$])],
                  {
                    [错误：括号不匹配]
                  }
                )
                Call([Pop], [$T$])
                Comment[弹出并丢弃左括号]
              },
              [$t$ 是操作符],
              {
                While(
                  [#sym.not #CallInline([Empty], [$T$]) #sym.and #CallInline([Top], [$T$]) 是操作符],
                  {
                    Assign[$"top"$][#CallInline([Top], [$T$])]
                    If(
                      [#CallInline([Precedence], [$"top"$]) $>$ #CallInline([Precedence], [$t$]) #sym.or\ (#CallInline([Precedence], [$"top"$]) $=$ #CallInline([Precedence], [$t$]) #sym.and #sym.not #CallInline([IsRightAssociative], [$t$]))],
                      {
                        Call([Append], [$o$, #CallInline([Pop], [$T$])])
                      },
                      Break
                    )
                  }
                )
                Call([Push], [$T$, $t$])
              },
              {
                [错误：未知词素]
              }
            )
          }
        )
        LineBreak
        While(
          [#sym.not #CallInline([Empty], [$T$])],
          {
            If(
              [#CallInline([Top], [$T$]) $=$ `<(>` #sym.or #CallInline([Top], [$T$]) $=$ `<)>`],
              {
                [错误：括号不匹配]
              }
            )
            Call([Append], [$o$, #CallInline([Pop], [$T$])])
          }
        )
        LineBreak
        Return[$o$]
      },
    )
  }
) <algo:shunting-yard>

上述算法的 OCaml 实现见@code:task-1-tokenize 和@code:task-1-shunting-yard。

=== Thompson 算法

Thompson 算法是从正则表达式树递归构造 NFA 的方法。

对于连接操作，首先递归构造左右子树的 NFA，之后添加一条左子树的接受状态至右子树初始状态的 $epsilon$ 边，如@figure:thompson-concat 所示；

#img(
  finite.automaton(
    (
      e1s: (e1a: ()),
      e1a: (e2s: ()),
      e2s: (e2a: ()),
      e2a: ()
    ),
    labels: (
      e1s: $E_1^S$,
      e1a: $E_1^A$,
      e2s: $E_2^S$,
      e2a: $E_2^A$
    ),
    style: (
      e1s: (initial: (label: (text: ""))),
      e1s-e1a: (stroke: (dash: "dashed")),
      e2s-e2a: (stroke: (dash: "dashed"))
    ),
    initial: "e1s",
    final: ("e2a",)
  ),
  caption: [连接操作 $E_1 E_2$ 的自动机结构]
) <figure:thompson-concat>

对于闭包操作，首先递归构造子树的 NFA，之后加入三条 $epsilon$ 边表示 0 或多次进入子树，如@figure:thompson-closure 所示；

#img(
  finite.automaton(
    (
      s: (es: (), a: ()),
      es: (ea: ()),
      ea: (es: (), a: ()),
      a: (),
    ),
    labels: (
      s: $S$,
      es: $E^S$,
      ea: $E^A$,
      a: $A$
    ),
    style: (
      s: (initial: (label: (text: ""))),
      s-es: (label: (text: $epsilon$)),
      es-ea: (stroke: (dash: "dashed")),
      ea-es: (label: (text: $epsilon$)),
      ea-a: (label: (text: $epsilon$)),
      s-a: (label: (text: $epsilon$)),
    ),
    initial: "s",
    final: ("a",)
  ),
  caption: [闭包操作 $E^*$ 的自动机结构]
) <figure:thompson-closure>

对于选择操作，首先递归构造子树的 NFA，并将两棵子树并列插入，如@figure:thompson-alternation 所示；

#img(
  finite.automaton(
    (
      s: (e1s: (), e2s: ()),
      e1s: (e1a: ()),
      e1a: (a: ()),
      e2s: (e2a: ()),
      e2a: (a: ()),
      a: (),
    ),
    labels: (
      s: $S$,
      e1s: $E_1^S$,
      e1a: $E_1^A$,
      e2s: $E_2^S$,
      e2a: $E_2^A$,
      a: $A$
    ),
    style: (
      s: (initial: (label: (text: ""))),
      s-e1s: (label: (text: $epsilon$)),
      e1s-e1a: (stroke: (dash: "dashed")),
      e1a-a: (label: (text: $epsilon$)),
      s-e2s: (label: (text: $epsilon$)),
      e2s-e2a: (stroke: (dash: "dashed")),
      e2a-a: (label: (text: $epsilon$)),
    ),
    initial: "s",
    final: ("a",)
  ),
  caption: [选择操作 $E_1 | E_2$ 的自动机结构]
) <figure:thompson-alternation>

对于正闭包操作，仅需将其分解为 $E E^*$ 即可，如@figure:thompson-pos-closure-expansion 所示。

#img(
  finite.automaton(
    (
      es: (ea: ()),
      ea: (eps: ()),
      eps: (epa: ()),
      epa: ()
    ),
    labels: (
      es: $E^S$,
      ea: $E^A$,
      eps: $E_*^S$,
      epa: $E_*^A$
    ),
    style: (
      es: (initial: (label: (text: ""))),
      es-ea: (stroke: (dash: "dashed")),
      eps-epa: (stroke: (dash: "dashed"))
    ),
    initial: "es",
    final: ("epa",)
  ),
  caption: [正闭包操作$E^+$展开示意图]
) <figure:thompson-pos-closure-expansion>

将流程综合整理后可编写@code:task-1-thompson 所示的 OCaml 实现。在实现中，NFA 状态图使用邻接表存储，方便递归函数遍历。

== NFA 转 DFA 算法及实现

本实验使用子集构造法实现 NFA 到 DFA 的转换。其原理为将 NFA 中可能的现态集合与每个输入的次态集合分别视作 DFA 中的单独状态，即维护一个 $cal(P)(S_"NFA") -> S_"DFA"$ 的映射关系。由于 $epsilon$ 边的存在，次态集合需使用 $S' = epsilon"-closure"("move"(S, alpha))$ 求得。

#algorithm-figure(
  [子集构造法],
  supplement: [算法],
  vstroke: .5pt + luma(200),
  {
    import algorithmic: *
    Procedure(
      "Subset-Construction",
      $cal(N)$,
      {
        Comment[$cal(N)$：待转换的 NFA]
        LineBreak
        Assign[$S$][${ epsilon"-closure"({ cal(N).q_0 }) }$]
        Assign[$V$][${ S }$]
        Assign[$U$][${ S }$]
        Assign[$Delta$][$emptyset$]
        While(
          [$U != emptyset$],
          {
            Assign[$T$][#CallInline([Pop], [$U$])]
            For(
              [$alpha in Sigma$],
              {
                Assign[$W$][$epsilon"-closure"("move"(T, alpha))$]
                If(
                  [$W != emptyset$],
                  {
                    If(
                      [$W in.not V$],
                      {
                        Call([Add], [$V, W$])
                        Call([Add], [$U, W$])
                      }
                    )
                    Call([Add], [$Delta, T stretch(->)^alpha W$])
                  }
                )
              }
            )
          }
        )
        Assign[$A$][${ T in V | T inter cal(N).q_f != emptyset }$]
        Return[$(Sigma, Delta, S, A)$]
      },
    )
  }
) <algo:subset-construction>

该算法的递归 OCaml 实现见@code:task-2-subset-construction。

== DFA 最小化算法及实现

实验中使用 Hopcroft 法（求异法）最小化 DFA。其基本思想为，首先将所有原 DFA 状态划分为简单的两类：接受状态与非接受状态，之后的每一轮中，依次观察每个分类里的状态是否都具有相同的行为（即在某个输入符号下，一个分类中的所有状态均转移到同一个分类中的次态）。若是，则继续观察下一个分类；若否，则根据行为的不同进行拆分。当在一轮中无法进行任何拆分时，即完成对状态的等价划分。该算法的递归 OCaml 实现见@code:task-3-hopcroft。

实际上，有多种不同的最小化算法实现，如 Brzozowski 法：反转 DFA $cal(D)$ 的边以及其初始、接受状态，得到 NFA $cal(N) = overline(cal(D))$，再使用子集构造法转换该 NFA 得到 $cal(D)' = text(#smallcaps[Subset-Construction])(cal(N))$。重复上述过程一次，即能得到：

#equation(
  $
    "min"(cal(D)) = text(#smallcaps[Subset-Construction])(overline(text(#smallcaps[Subset-Construction])(overline(cal(D)))))
  $
) <eq:brzozowski-minimization>

= 测试结果

== 正规表达式转 NFA 算法及实现

如@figure:task-1-demo 与 @figure:task-1-demo-eval 所示，实验程序能够从正则表达式字符串生成 NFA 并模拟。生成的 NFA 如@figure:task-1-nfa 所示。

#img(
  image("assets/task-1-demo.png"),
  caption: [NFA 生成功能演示]
) <figure:task-1-demo>

#img(
  image("assets/task-1-demo-eval.png"),
  caption: [NFA 模拟功能演示]
) <figure:task-1-demo-eval>

#img(
  finite.automaton(
    (
      q6: (q4: (), q7: ()),
      q4: (q0: (), q2: ()),
      q0: (q1: ()),
      q1: (q5: ()),
      q2: (q3: ()),
      q3: (q5: ()),
      q5: (q4: (), q7: ()),
      q7: (q8: ()),
      q8: (q9: ()),
      q9: (q10: ()),
      q10: (q11: ()),
      q11: (q12: ()),
      q12: (q13: ()),
      q13: ()
    ),
    labels: (
      q6: $q_6$,
      q4: $q_4$,
      q0: $q_0$,
      q1: $q_1$,
      q2: $q_2$,
      q3: $q_3$,
      q5: $q_5$,
      q7: $q_7$,
      q8: $q_8$,
      q9: $q_9$,
      q10: $q_(10)$,
      q11: $q_(11)$,
      q12: $q_(12)$,
      q13: $q_(13)$
    ),
    style: (
      q6: (initial: (label: (text: ""))),
      q6-q4: (label: (text: $epsilon$)),
      q6-q7: (label: (text: $epsilon$)),
      q4-q0: (label: (text: $epsilon$)),
      q4-q2: (label: (text: $epsilon$)),
      q0-q1: (label: (text: $a$)),
      q1-q5: (label: (text: $epsilon$)),
      q2-q3: (label: (text: $b$)),
      q3-q5: (label: (text: $epsilon$)),
      q5-q4: (label: (text: $epsilon$)),
      q5-q7: (label: (text: $epsilon$)),
      q7-q8: (label: (text: $epsilon$)),
      q8-q9: (label: (text: $a$)),
      q9-q10: (label: (text: $epsilon$)),
      q10-q11: (label: (text: $b$)),
      q11-q12: (label: (text: $epsilon$)),
      q12-q13: (label: (text: $b$)),
    ),
    layout: finite.layout.circular,
    initial: "q6",
    final: ("q13",)
  ),
  caption: [算法为 `(a|b)*abb` 生成的 NFA 结构图]
) <figure:task-1-nfa>

== NFA 转 DFA 算法及实现

如@figure:task-2-demo 与 @figure:task-2-demo-eval 所示，实验程序能够将 NFA 正确转换为 DFA 并模拟。生成的 DFA 如@figure:task-2-dfa 所示。

#img(
  image("assets/task-2-demo.png"),
  caption: [DFA 生成功能演示]
) <figure:task-2-demo>

#img(
  image("assets/task-2-demo-eval.png"),
  caption: [DFA 模拟功能演示]
) <figure:task-2-demo-eval>

#img(
  finite.automaton(
    (
      q0: (q1: (), q2: ()),
      q1: (q1: (), q3: ()),
      q2: (q1: (), q2: ()),
      q3: (q1: (), q4: ()),
      q4: (q1: (), q2: ()),
    ),
    labels: (
      q0: $q_0$,
      q1: $q_1$,
      q2: $q_2$,
      q3: $q_3$,
      q4: $q_4$,
    ),
    style: (
      q0: (initial: (label: (text: ""))),
      q0-q1: (label: (text: $a$)),
      q0-q2: (label: (text: $b$)),
      q1-q1: (label: (text: $a$)),
      q1-q3: (label: (text: $b$)),
      q2-q1: (label: (text: $a$)),
      q2-q2: (label: (text: $b$)),
      q3-q1: (label: (text: $a$)),
      q3-q4: (label: (text: $b$)),
      q4-q1: (label: (text: $a$)),
      q4-q2: (label: (text: $b$)),
    ),
    initial: "q0",
    final: ("q4",)
  ),
  caption: [转换得到的 DFA 结构图]
) <figure:task-2-dfa>

== DFA 最小化算法及实现

如@figure:task-3-demo 与 @figure:task-3-demo-eval 所示，实验程序能够正确最小化 DFA 并模拟。生成的 DFA 如@figure:task-3-min-dfa 所示。

#img(
  image("assets/task-3-demo.png"),
  caption: [DFA 最小化功能演示]
) <figure:task-3-demo>

#img(
  image("assets/task-3-demo-eval.png"),
  caption: [最小 DFA 模拟功能演示]
) <figure:task-3-demo-eval>

#img(
  finite.automaton(
    (
      q3: (q2: (), q3: ()),
      q0: (q2: (), q3: ()),
      q1: (q2: (), q0: ()),
      q2: (q2: (), q1: ()),
    ),
    labels: (
      q0: $q_0$,
      q1: $q_1$,
      q2: $q_2$,
      q3: $q_3$,
    ),
    style: (
      q3: (initial: (label: (text: ""))),
      q3-q2: (label: (text: $a$)),
      q3-q3: (label: (text: $b$)),
      q0-q2: (label: (text: $a$)),
      q0-q3: (label: (text: $b$)),
      q1-q2: (label: (text: $a$)),
      q1-q0: (label: (text: $b$)),
      q2-q2: (label: (text: $a$)),
      q2-q1: (label: (text: $b$)),
    ),
    initial: "q3",
    final: ("q0",)
  ),
  caption: [最小 DFA 结构图]
) <figure:task-3-min-dfa>

== 完整程序展示

实验中还编写了一个能够统一演示三个任务内功能的驱动程序，如@figure:integrated-demo。

#img(
  image("assets/integrated-demo.png", height: 40%),
  caption: [完整演示程序输出]
) <figure:integrated-demo>

= 源代码

完整的实验程序与报告源代码可以从 #link("https://github.com/CSharperMantle/hdu2025_compiler_principles/tree/main/exp_2") 处获取。

== 正规表达式转 NFA 算法及实现

#code(
  ```ml
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
  let cook_tokens (tokens : raw_token list) : token list =
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
  ```,
  caption: [字符串分词并插入隐含连接操作的 OCaml 代码]
) <code:task-1-tokenize>

#code(
  ```ml
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
  ```,
  caption: [将中缀表达式转换为后缀表达式的 OCaml 代码]
) <code:task-1-shunting-yard>

#code(
  ```ml
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
  ```,
  caption: [NFA 数据结构定义与 Thompson 算法的 OCaml 实现]
) <code:task-1-thompson>

== NFA 转 DFA 算法及实现

#code(
  ```ml
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
  ```,
  caption: [子集构造法实现 NFA 转 DFA 的 OCaml 代码]
) <code:task-2-subset-construction>

== DFA 最小化算法及实现

#code(
  ```ml
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
  ```,
  caption: [Hopcroft 算法最小化 DFA 的 OCaml 实现]
) <code:task-3-hopcroft>
