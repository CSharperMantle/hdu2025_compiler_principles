#import "@preview/algorithmic:1.0.5"
#import algorithmic: algorithm-figure, style-algorithm

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
  date: datetime(year: 2025, month: 12, day: 22),
  cover_style: "hdu_report",
)

#show link: underline
#show smallcaps: set text(font: font_style.heiti)

#toc()
#pagebreak()

= 实验目的

任务 4.1 “SysY 语法和语义规范理解”旨在通过分析 SysY 语⾔的语法和语义规范，加深对编译原理中语法分析和语义分析部分的理解，通过编写测试程序和分析其语法成分，更好地理解语⾔规范的实现和应⽤。

任务 4.2 “词法分析器设计与实现”通过编写⼀个词法分析器、对使⽤SysY语⾔书写的源代码进⾏词法分析并打印分析结果，加深对词法分析器实现方法的掌握程度。实验的实现⽅式可以⼿⼯编写程序，也可以采⽤词法分析生成⼯具 GNU Flex 等。

任务 4.3 “语法分析器设计与实现”通过在任务 4.2 的词法分析程序基础上编写⼀个语法分析器、对使⽤SysY语⾔书写的源代码进⾏语法分析并打印分析结果，加深对语法分析器结构、语义操作、错误处理等的理解。可以⼿⼯编写程序，也可以采⽤语法分析生成⼯具 Bison 等。

任务 4.4 “语义分析” 的任务是在词法和语法分析的基础上对使⽤ SysY 语⾔书写的源代码进⾏静态语义检查并打印分析结果，加深对词法作用域、类型检查、语义检查实现方法与原理的掌握。实验的实现⽅式可以⼿⼯编写程序，也可以探索在 Bison 中增加语义检查规则来实现。

在上述规定的实验任务之外，本实验还探索了简单三地址中间代码生成、控制流图构造、静态单赋值形式构造与转换、简单优化变换、中间表示可视化等额外主题。

= 实验内容与实验要求

在任务 4.2 中，程序要能够查出 SysY 源代码中可能包含的词法错误。词法错误（错误类型代码为 A）指的是出现 SysY 词法中未定义的字符以及任何不符合 SysY 词法单元定义的字符。程序在输出错误提示信息时，需要输出具体的错误类型、出错的位置（源程序的⾏号）以及相关的说明⽂字。

在任务 4.3 中，除了任务 4.2 的功能之外，程序还要能够查出 SysY 源代码中可能包含的语法错误。语法错误，类型代码为 B。程序在输出错误提示信息时，需要输出具体的错误类型、出错的位置（源程序的⾏号）以及相关的说明⽂字。

在任务 4.4 中，程序要能够查出 SysY 源代码中可能包含的语义错误。本次实验中，SysY语⾔的语义特性符合《SysY 语⾔定义（2022 版）》中的语义约束描述，简单归纳如下：

+ 特性 1：⼀个 SysY 程序由单个⽂件构成，⽂件内容对应语法单元为 `CompUnit`，其中必须存在⼀个标识符为“main”、⽆参数、返回类型为 `int` 的 `FuncDef`（函数定义）；
+ 特性 2：每个 `CompUnit` 对应的顶层变量或常量声明语句、函数定义，都不能重复定义同名标识符，即使类型不同也不⾏；
+ 特性 3：任何函数只进⾏⼀次定义，⽆法进⾏函数声明；
+ 特性 4：与 C 语⾔标准⼀致，函数⽆法进⾏嵌套定义，即函数内部不能再定义函数；
+ 特性 5：作为 `if` 和 `while` 条件部分的语法单位 `Cond` 是⼀个 `LOrExp`，需要特别注意逻辑运算表达式，单⽬运算符 `!` 只能出现在 `LOrExp`，⽽不能在 `Exp` 中出现；
+ 特性 6：函数调⽤时实际参数和函数定义中的参数个数和类型必须保持完全匹配；
+ 特性 7：SysY 允许类型隐式转换。

各⼩组的程序需要对输⼊⽂件进⾏语义分析（输⼊⽂件中可能包含函数、⼀维或多维数组），并检查如下类型的错误：

+ 错误类型 1：变量未声明；
+ 错误类型 2：变量重复声明；
+ 错误类型 3：函数在调⽤时未定义；
+ 错误类型 4：函数重复定义（同样的函数名出现了不⽌⼀次定义）；
+ 错误类型 5：把变量当做函数调⽤，如对普通变量使⽤括号 (...) 或 () 运算符（当函数调⽤）；
+ 错误类型 6：对函数名的不当引⽤（如把函数名当做普通变量来引⽤）；
+ 错误类型 7：对数组的不当引⽤，如数组访问运算符“[...]”中出现⾮整数表达式，即数组变量的下标不是整型；
+ 错误类型 8：对⾮数组变量使⽤数组访问“[...]”运算符；
+ 错误类型 9：函数调⽤时参数个数或类型不匹配；
+ 错误类型 10：`return` 语句返回的类型与函数定义的返回类型不匹配；
+ 错误类型 11：操作数类型不匹配，或操作数类型与操作符不匹配，如：整型变量与数组变量相加减，或数组变量与数组变量相加减；
+ 错误类型 12：`break` 语句不在循环体内；
+ 错误类型 13：`continue` 语句不在循环体内；

要求⾄少能识别上述 3 个以上的错误类型，程序在输出错误提示信息时，需要输出具体的错误类型、出错的位置（源程序的⾏号）以及相关的说明⽂字。

= 设计方案与算法描述

== 任务 4.1、4.2、4.3 与 4.4

本实验的词法分析器使用 ocamllex #footnote[#link("https://ocaml.org/manual/5.4/lexyacc.html")。 它是 OCaml 语言中 Unix Lex 和 GNU Flex 的等价物，能将 ocamllex 说明文件转换为 OCaml 实现的词法分析器代码。] 实现。大部分词法单元的解析是直接的，仅需使用基础的正则表达式即可匹配。对于数值字面量，本实验考虑了以下类型：

+ 十进制整数；
+ 以 `0` 开头的八进制整数；
+ 以 `0x` 开头的十六进制整数；
+ 十进制自然计数法小数。

在模式层面，词法分析器将所有数值字面量分为三大类：上述 1、2 项称为 `dec_or_oct`，第 3 项为 `hex`，第 4 项为 `float`。第一大类再根据前缀 `0` 长度分别解析为十进制或八进制。

词法分析器没有实现错误恢复，在遇到第一个无法识别的词法单元时立即报告错误，不再处理之后的输入。

本实验的语法分析器使用 Menhir #footnote[#link("https://gallium.inria.fr/~fpottier/menhir/manual.html")。OCaml 语言使用 ocamlyacc 实现 Unix YACC 和 GNU Bison 的功能，而 Menhir 是 ocamlyacc 的现代版。] 实现。

大部分《SysY 2022 语言定义》中的语法规则都可以简便地转换为 Menhir 规则。语言定义中使用表达式结构定义了各运算符的优先级，在实现时可以直接使用 Menhir 说明中的 `%left`、`%right` 和 `%nonassoc` 指令定义运算符优先级，无需显式写出语法结构。

Menhir 支持“增量解析（incremental parsing）模式”，使其从推迭代器变为拉迭代器，即在需要移进（`InputNeeded`、`Shifting`）、即将归约（`AboutToReduce`）、处于接受态（`Accepted`）、出现错误（`HandlingError`）等状态时，由调用者确定下一步行为，如提供下一个词素、记录检查点、恢复检查点等行为，与完全由 Menhir 完成状态转换的推迭代器模式相比，能实现更自由的错误处理。本实验中，使用该功能实现了“恐慌模式（panic mode）”错误恢复，即删除输入的后续词素，直到遇到下一个诸如分号、后括号、EOF 等的分隔符时再次开始语法检查。虽然最终依然需要报告错误，但是与遇到第一个语法错误即退出的默认策略相比，能够发现更多潜在的语法错误，帮助调试。

类型检查与确定在语义分析阶段完成。作为局部变量数组的所有维数均需要在该阶段计算得到，作为函数形参的数组引用的尾部维数也需要在本阶段进行验证。在此阶段，由于一级

SysY 语言中由 `const` 修饰的名字要求必须具有初始化表达式，并且其中仅能包含其他被声明为 `const` 的名字。`const` 修饰的变量还可以用于需要常量表达式的位置。因此，该关键字的语义并非 C 语言中的“一次复制”，而是编译期常量，类似于 C++ 中对于变量的 `constexpr` 修饰。本实验的语法分析阶段会对所有局部作用域的 `const` 变量进行常量传播，提前折叠潜在的常量表达式，减少之后阶段的中间代码量。

语义分析阶段也需要实现相应的错误恢复，如遇到未定义标识符时可以插入一个类型为 `int` 的哑对象使语法分析继续进行，在完成后统一报告错误。与生成的语法分析器不同，编写语义分析阶段时，需完全手动编写一套错误积累与报告机制。本实验中使用的是 Monad 模式，如@code:agg-result-monad。该 Monad 的 `return` 操作包含两种，创建成功结果的 `agg_ok` 和积累错误结果的 `agg_error`。其 `bind` 操作将子闭包中产生的错误添加到当前错误列表之中，并返回相应结果。这种模式在语义分析中大量使用，配合 OCaml 的绑定语法 `let*`，很大减少了负责错误处理的代码冗余。

#code(
  ```ml
  module AggResult = struct
    type ('a, 'b) agg_result = 'a * 'b list

    let agg_ok (x : 'a) : ('a, 'b) agg_result = (x, [])
    let agg_error (msg : string) (x : 'a) : ('a, 'b) agg_result = (x, [ msg ])

    let bind ((x, errs1) : ('a, 'b) agg_result) (f : 'a -> ('c, 'b) agg_result) : ('c, 'b) agg_result
        =
      let y, errs2 = f x in
      (y, errs1 @ errs2)

    let agg_is_ok ((_, errs) : ('a, 'b) agg_result) : bool = List.is_empty errs

    let agg_to_result ((x, errs) : ('a, 'b) agg_result) : ('a, 'b list) result =
      if agg_is_ok (x, errs) then Ok x else Error errs

    module Syntax = struct
      let ( let* ) = bind
    end
  end
  ```,
  caption: [聚合错误结果 Monad],
) <code:agg-result-monad>

== 三地址代码生成

=== 对象、名字与初始化

在本实验中，任何存在内存或寄存器中的物体均为“对象”。根据对象的行为，可以分为以下几类：

- *全局标量*：分配在静态存储区，对象读写为内存读写；
- *全局数组*：分配在静态存储区，对象读写为内存读写；
- *局部标量*：分配在寄存器中，对象读写为寄存器复制，若出现分配溢出则需保存至活动记录中；
- *局部数组*：分配在活动记录中，对象读写为内存读写。

根据对象的分配来源与生命周期，又可以将其分为以下几类：

- *全局对象*；
- *函数形参*；
- *局部对象*：源码中显式创建的非全局非形参对象；
- *临时对象*：一般为右值创建。

上述前三类对象在源代码中具有用户定义的名字（name）；其他非对象也拥有名字，如函数。在同一个作用域内，唯一定义规则要求不能包含名字相同的同类对象，如不可存在两个同名的函数，但是作用域之间可以嵌套，外层作用域的名字可能被内层作用域的名字所隐藏（shadow）。

在三地址代码生成阶段，对于全局标量与全局数组的名字，在中间代码中具有单独的命名空间（如 `@1`），引用时直接生成内存读写指令即可，在生成时将标量视为单元素数组处理。对于局部数组，我们认为其实际上是一个包含首地址、数组形状与元素类型的局部标量，同样使用内存读写指令访问即可。局部数组和局部标量都需要在三地址代码中包含初始化代码，局部数组还需要包含内存分配指令。

=== 短路求值

SysY 表达式要求实现布尔表达式的短路求值。对于形如 `d = e1 || e2` 的表达式而言，生成代码的模板如@code:template-sc-or 所示：

#code(
  ```txt
      e1 <- ...;
      d  <- 1;
      if (e1) goto .Lend;
  .Le2:
      e2 <- ...;
      d  <- e2 != 0;
  .Lend:
  ```,
  caption: [短路布尔操作符 `||` 的三地址代码模板],
) <code:template-sc-or>

`d = e1 && e2` 的模板也类似。

=== 隐式类型转换

为实现简单起见，本实验中的隐式转换规则十分宽松，即 `float` 可以通过指令 `Fti` 转换为 `int`，相反 `int` 也可以通过指令 `Itf` 转换为 `float`。在翻译赋值、函数调用等语句时，需要先求得右手侧结果与左手侧结果的公共类型，然后生成相应的转换指令。二元表达式翻译时需要求两个操作数的公共类型。

== SSA 形式中间表示的构造

=== SSA 形式变体的选择

静态单赋值（SSA）形式的基本原则为每一个值类型变量只能赋值一次。实践上有多种相似的结构可以实现这个要求 #footnote[#link("https://bernsteinbear.com/blog/ssa/")]。

“教科书式”的 SSA 形式要求每个基本块在开头存在 0 或多个 #sym.phi.alt 函数以聚合前驱块的赋值，在末尾有唯一一条控制流语句离开该基本块。Pizlo #footnote[#text("https://gist.github.com/pizlonator/cf1e72b8600b1437dda8153ea3fdb963")] 认为这种形式需要将 #sym.phi.alt 与普通运算指令分开处理，违反了指令的均相原则，并提出了 Pizlo SSA 形式。这种形式中存在两种指令：

- *#sym.upsilon 函数*：副作用函数，$upsilon ( #text[`%src`], #text[`%dest`]^* )$ 将 `%src` 复制入 `%dest` 的隐藏变量 $#text[`%dest`]^*$ 中；
- *#sym.phi.alt 函数*：副作用函数，$#text[`%dest`] <- phi.alt ()$ 将隐藏变量 $#text[`%dest`]^*$ 的值复制入 `%dest` 中。

这样可以将这对函数像普通指令一样进行移动操作，不需要特殊处理。Pizlo SSA 形式已在 JavaScriptCore 的 JIT 引擎中使用，但是由于该形式缺少详尽的文献说明，实验不选择该 SSA 形式。

=== 控制流图构造

控制流图是程序分析的基础数据结构，它表示了程序执行过程中可能的所有控制流路径。在本实验中，控制流图的构造过程如下：

+ 顺序扫描三地址代码序列，每当遇到标签（Label）或控制流改变指令（Jump、Br、Return）时，结束当前基本块，开始新块；
+ 为每个基本块分配唯一标识符，并建立标签到基本块ID的映射关系。对于没有显式跳转指令的基本块，会自动添加一条跳转到下一个基本块的指令，确保控制流的连续性；
+ 构建控制流图的边关系，通过分析每个基本块的终止指令确定其后继基本块。
+ 通过反向遍历这些边关系，构建前驱集合映射，为后续的支配关系分析和循环识别提供数据。

在测试中发现，程序中可能存在入口基本块作为循环头、需要插入 #sym.phi.alt 的情况，如测试用例 tests/snippet_gcd.sy。因此，需要创建一个空块，加入一个无条件跳转，避免这种特殊情况。最后，通过可达性分析移除不可达的基本块，确保控制流图只包含实际可达的代码路径，提高后续分析的效率。

=== #sym.phi.alt 节点插入

#sym.phi.alt 函数用于在控制流汇合点合并不同路径上定义的变量值。本实验中Phi函数的插入遵循《Modern Compiler Implementation in C》一书中描述的经典 SSA 构造算法。

+ 收集每个变量的定义点。这包括函数参数和所有产生新值的指令（如二元运算、一元运算、赋值、函数调用等）;
+ 使用 Lengauer-Tarjan 计算支配树，以此计算支配边界信息，确定需要插入 #sym.phi.alt 函数的位置。支配边界是支配关系分析的重要结果，表示一个节点开始支配其某些后继节点但不再支配所有后继节点的边界；
+ 遍历每个变量的定义点集合，对于每个定义点，检查其支配边界中的基本块，并在这些基本块中插入该变量的 #sym.phi.alt 函数。

在初始阶段，所有变量的版本值均为 0，将在后续的变量重命名阶段被正确更新。

=== 重编号变量

变量重编号是 SSA 构造的最后一步，它确保每个变量在程序中只被赋值一次，并且每次使用都能明确指向唯一的定义。本实验中的变量重编号实现基于经典的 SSA 重命名算法，具体过程如下。

我们为每个变量维护一个版本号栈，栈顶表示该变量的当前活跃版本。对于函数参数，在入口块开始时将其版本号初始化为1，并推入对应的栈中。然后，以深度优先遍历的方式遍历控制流图，从入口块开始。对于每个基本块，执行以下操作：

+ 为每个Phi函数的目标变量分配新的版本号，更新栈，并将 #sym.phi.alt 函数的目标值更新为新版本；
+ 首先使用当前栈顶的版本号重写指令中的所有使用点，然后为指令的定义点分配新版本号；
+ 使用当前栈顶的版本号重写块末跳转指令中的操作数；
+ 对于当前块的每个后继块，找到其中的 #sym.phi.alt 函数，并将当前块中相应变量的当前版本添加到 #sym.phi.alt 函数的输入映射中；
+ 按照支配树的顺序，递归处理当前块的所有直接支配的节点；
+ 在处理完所有子节点后，将当前块中定义的变量的版本号从栈中弹出，恢复到进入该块之前的状态，确保不影响其他路径上的变量版本。

这种基于栈的重命名机制确保了在遍历控制流图时，每个变量的使用总是指向最近定义的版本，符合 SSA 形式的单赋值原则。同时，通过递归处理支配树，保证了变量重命名的正确性和完整性。重命名完成后，每个变量在程序中的每次使用都明确指向唯一的定义点，形成了完整的 SSA 形式，为后续的优化和分析提供了便利。

== SSA 可视化

本实验使用 Mozilla SpiderMonkey 团队的 Iongraph 工具 #footnote[#link("https://spidermonkey.dev/blog/2025/10/28/iongraph-web.html")] 实现 SSA 形式的可视化。

实现可视化需要将 SSA 数据结构转换为 Iongraph 能够读取的 JSON 格式输出。Iongraph 需要对每一条指令进行编号，以该编号作为该条 SSA 指令的定义点的值，并以此计算交叉引用信息，因此需要将 SSA 图中的值映射到指令编号。

Iongraph 使用循环信息进行图布局，所以需要标记自然循环的循环头基本块与回边基本块。由于原始 SSA 结构中并没有存储这些信息，所以需要使用如@code:loop-props-calc 所示的方法，在导出前即时计算。

Iongraph 支持可视化每个优化变换的结果，而已有的优化管线是按照程序进行的，需要将其以类似“转置”的操作，对每个函数的各 pass 输出进行合并。

最后，使用 ppx_deriving_yojson #footnote[#link("https://github.com/ocaml-ppx/ppx_deriving_yojson")] 实现从 OCaml 类型定义生成 JSON 序列化器的操作，调用序列化器实现结果输出。

#code(
  ```ml
  let recompute_loop_props (program : program) : program =
    let ctx =
      {
        empty_build_ssa_context with
        next_bb_id = program.next_bb_id;
        next_instr_id = program.next_instr_id;
      }
    in
    (* Rebuild successors and predecessors *)
    let ctx =
      List.fold_left
        (fun ctx func ->
          let succs, preds =
            IntMap.fold
              (fun bb_id bb (succs, preds) ->
                let bb_succs =
                  match bb.bb_term with
                  | Jump (_, target) -> IntSet.singleton target
                  | Br (_, _, true_target, false_target) ->
                      IntSet.of_list [ true_target; false_target ]
                  | Return _ -> IntSet.empty
                in
                let preds =
                  IntSet.fold
                    (fun succ_id acc ->
                      let existing = IntMap.find_opt succ_id acc |> or_default IntSet.empty in
                      IntMap.add succ_id (IntSet.add bb_id existing) acc)
                    bb_succs preds
                in
                (IntMap.add bb_id bb_succs succs, preds))
              func.func_blocks (IntMap.empty, IntMap.empty)
          in
          {
            ctx with
            successors = IntMap.fold IntMap.add succs ctx.successors;
            predecessors = IntMap.fold IntMap.add preds ctx.predecessors;
          })
        ctx program.functions
    in
    let ctx =
      List.fold_left
        (fun ctx func ->
          let ctx = compute_dom_frontier func ctx in
          compute_loop_props func ctx)
        ctx program.functions
    in
    {
      program with
      loop_props =
        {
          loop_headers = ctx.loop_headers;
          back_edges = ctx.back_edges;
          back_edge_list = ctx.back_edge_list;
          loop_depths = ctx.loop_depths;
        };
    }
  ```
) <code:loop-props-calc>

== 简单优化变换

=== 简单常量传播

在 SSA 形式上进行简单常量传播确实非常简单。以深度优先顺序遍历 SSA 图，每遇到一个常量定义点即将其传播至所有使用点处；对于算术运算 `BinaryOp`、`FBinaryOp`、`UnaryOp`、`FUnaryOp`、`Itf`、`Fti`，若所有操作数均为常量，则结果也是常量。实验中对除法与取模操作进行了特别判断，对于被 0 除的情况阻止常量传播，保持运行时行为。

=== 复制传播

由于 SSA 的唯一定义特性，我们可以对所有 $#text[`%dest`] <- #text[`Move`] (#text[`%src`])$ 与 $#text[`%dest`] <- phi.alt (#text[`%src`])$ 两种形式的指令构建等价集 ${ #text[`%dest`], #text[`%src`] }$。又由于复制操作具有传递性，复制链上的所有元素都可以随后加入相应的等价集中。在遍历完所有的使用点以后，将所有使用点重写为等价集中的代表元素，即可完成复制传播。在效果上，所有使用点均被重写为最早定义的值。

变换完成后会多出很多无用的定义点，这些指令可以在死代码消除阶段中被清除。

=== 死代码消除

对于所有没有使用点的定义点，若该条指令不存在副作用，则可以将该指令删除。在本实验中，为了简化实现，我们假定所有 `Ld`、`St`、`Call` 指令均存在副作用，在未来的改进中可以通过计算函数是否为纯函数而删去更多代码。

对于条件为常量的分支指令，可将其化简为一条无条件跳转指令。在修改后可能导致一个分支上的基本块不可达。在该阶段结束后，需要删除不可达的块并重新计算自然循环信息。

= 测试结果

实验中除了手动编写的针对性正向负向测试样例、由大语言模型生成的机器综合程序片段之外，还包含了北京大学编译原理课程 2024 年春季学期提供的 467 个测试用例 #footnote[#link("https://github.com/jokerwyt/sysy-testsuit-collection")]。目前的编译管线能够拒绝所有的负向样例并给出期望数量的错误信息，能够接受所有的正向样例并给出可视化的 SSA 图，如@figure:digui3 所示。

#img(
  image("assets/demo-iongraph.png"),
  caption: [北大测试集 digui3.c 中 `solve3` 函数经过优化后的部分 SSA 图],
) <figure:digui3>

= 源代码

完整的实验程序与报告源代码可以从 #link("https://github.com/CSharperMantle/hdu2025_compiler_principles/tree/main/exp_4") 处获取。
