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
  date: datetime(year: 2025, month: 10, day: 30),
  cover_style: "hdu_report",
)

#toc()
#pagebreak()

= 实验目的

练习使用GCC/Clang编译C程序，理解并运用各种编译选项。

= 实验内容与实验要求

通过对一个简单的C程序示例`sample.c`,使用不同编译选项进行编译，得到程序的不同表示形
式，尝试理解这些形式之间的对应关系，进而理解编译的主要阶段：预处理、编译、汇编、链接。该程序涉及的主要语言特征有：

- 条件编译（1-5行）：根据是否定义宏`NEG`，定义不同的`M`
- 宏定义（第2、4行）以及宏引用（第8行）

#code(
  ```c
  #ifdef NEG
  #define M -4
  #else
  #define M 4
  #endif
  int main()
  {
      int a = M;
      if (a)
          a = a + 4;
      else
          a = a * 4;
      return 0;
  }
  ```,
  caption: "实验中使用的样例C语言程序",
) <code:sample>

+ 如果在命令行下执行`gcc -DNEG -E sample.c -o sample.i`，生成的`sample.i`与之前的有何区别？
+ 请对比`sample-32.s`和`sample.s`，找出它们的区别，并上网检索给出产生这些区别的原因。
+ 使⽤`clang`替换`gcc`，重复上⾯的各步，重复上面的各步，比较使用`clang`和`gcc`分别输出的结果有何异同。

= 设计方案与算法描述

#code(
  ```makefile
  .PHONY: all
  all: sample

  sample: sample.o
  	$(CC) -o $@ $^

  sample.o: sample.s
  	$(AS) $(ASFLAGS) -o $@ $^

  sample.s: sample.i
  	$(CC) -S $(CCFLAGS) -o $@ $^

  sample.i: sample.c
  	$(CPP) $(CPPFLAGS) -o $@ $^

  .PHONY: preprocess
  preprocess: sample.i

  .PHONY: compile
  compile: sample.s

  .PHONY: assemble
  assemble: sample.o

  .PHONY: link
  link: sample

  .PHONY: run
  run: link
  	./sample

  .PHONY: clean
  clean:
  	rm -f sample sample.o sample.s sample.i
  ```,
  caption: "用于自动化进行实验操作的Makefile",
) <code:makefile>

编写如@code:makefile 所示的Makefile后，可以使用以下命令完成实验：

+ `make preprocess`，`make preprocess CPPFLAGS=-DNEG`
+ `make compile`，`make compile CCFLAGS=-m32`
+ `make assemble`
+ `make link`
+ `make all CC=clang CCFLAGS=-no-integrated-as`#footnote[#link("https://stackoverflow.com/questions/62436694/why-cant-gcc-compile-assembly-produced-by-clang")]

= 测试结果

== 问题1

如@code:diff-preprocess 所示。

#code(
  ```diff
  diff --unified a/sample.i b/sample.i
  --- a/sample.i  2025-10-30 11:33:28.276337796 +0800
  +++ b/sample.i  2025-10-30 11:33:54.336461856 +0800
  @@ -11,7 +11,7 @@
  
   int main()
   {
  -    int a = 4;
  +    int a = -4;
       if (a)
          a = a + 4;
       else
  ```,
  caption: [`-DNEG`对预处理产物的影响],
) <code:diff-preprocess>

== 问题2

#code(
  ```diff
  diff --unified a/sample.s b/sample.s
  --- a/sample.s  2025-10-30 11:38:40.024482344 +0800
  +++ b/sample.s  2025-10-30 11:38:26.991301919 +0800
  @@ -5,42 +5,40 @@
   main:
   .LFB0:
          .cfi_startproc
  -       endbr64
  -       pushq   %rbp
  -       .cfi_def_cfa_offset 16
  -       .cfi_offset 6, -16
  -       movq    %rsp, %rbp
  -       .cfi_def_cfa_register 6
  -       movl    $4, -4(%rbp)
  -       cmpl    $0, -4(%rbp)
  +       pushl   %ebp
  +       .cfi_def_cfa_offset 8
  +       .cfi_offset 5, -8
  +       movl    %esp, %ebp
  +       .cfi_def_cfa_register 5
  +       subl    $16, %esp
  +       call    __x86.get_pc_thunk.ax
  +       addl    $_GLOBAL_OFFSET_TABLE_, %eax
  +       movl    $4, -4(%ebp)
  +       cmpl    $0, -4(%ebp)
          je      .L2
  -       addl    $4, -4(%rbp)
  +       addl    $4, -4(%ebp)
          jmp     .L3
   .L2:
  -       sall    $2, -4(%rbp)
  +       sall    $2, -4(%ebp)
   .L3:
          movl    $0, %eax
  -       popq    %rbp
  -       .cfi_def_cfa 7, 8
  +       leave
  +       .cfi_restore 5
  +       .cfi_def_cfa 4, 4
          ret
          .cfi_endproc
   .LFE0:
          .size   main, .-main
  +       .section          .text.__x86.get_pc_thunk.ax,"axG",@progbits,__x86.get_pc_thun  k.ax,comdat
  +       .globl  __x86.get_pc_thunk.ax
  +       .hidden __x86.get_pc_thunk.ax
  +       .type   __x86.get_pc_thunk.ax, @function
  +__x86.get_pc_thunk.ax:
  +.LFB1:
  +       .cfi_startproc
  +       movl    (%esp), %eax
  +       ret
  +       .cfi_endproc
  +.LFE1:
          .ident  "GCC: (Ubuntu 15.2.0-4ubuntu4) 15.2.0"
          .section        .note.GNU-stack,"",@progbits
  -       .section        .note.gnu.property,"a"
  -       .align 8
  -       .long   1f - 0f
  -       .long   4f - 1f
  -       .long   5
  -0:
  -       .string "GNU"
  -1:
  -       .align 8
  -       .long   0xc0000002
  -       .long   3f - 2f
  -2:
  -       .long   0x3
  -3:
  -       .align 8
  -4:
  ```,
  caption: [`-m32`（x86）模式与默认（x86-64）模式的编译产物差异],
) <code:diff-m32>

如@code:diff-m32，x86模式下的寄存器最大为32位宽的`%eXX`，而x86-64具有64位宽的`%rXX`寄存器，操作相应寄存器的指令也分别为`XXXl`与`XXXq`。

== 问题3

#code(
  ```diff
  diff --unified a/sample.i b/sample.i
  --- a/sample.i  2025-10-30 11:44:45.831831475 +0800
  +++ b/sample.i  2025-10-30 11:45:14.300835212 +0800
  @@ -1,9 +1,10 @@
  -# 0 "sample.c"
  -# 0 "<built-in>"
  -# 0 "<command-line>"
  -# 1 "/usr/include/stdc-predef.h" 1 3 4
  -# 0 "<command-line>" 2
   # 1 "sample.c"
  +# 1 "<built-in>" 1
  +# 1 "<built-in>" 3
  +# 410 "<built-in>" 3
  +# 1 "<command line>" 1
  +# 1 "<built-in>" 2
  +# 1 "sample.c" 2
  ```,
  caption: [GCC与Clang预处理产物的差异],
) <code:diff-clang-preprocess>

#code(
  ```diff
  diff --unified a/sample.s b/sample.s
  --- a/sample.s  2025-10-30 11:44:45.836164808 +0800
  +++ b/sample.s  2025-10-30 11:45:14.324668545 +0800
  @@ -1,46 +1,37 @@
          .file   "sample.c"
          .text
  -       .globl  main
  -       .type   main, @function
  -main:
  -.LFB0:
  +       .globl  main                            # -- Begin   function main
  +       .p2align        4
  +       .type   main,@function
  +main:                                   # @main
          .cfi_startproc
  -       endbr64
  +# %bb.0:
          pushq   %rbp
          .cfi_def_cfa_offset 16
  -       .cfi_offset 6, -16
  +       .cfi_offset %rbp, -16
          movq    %rsp, %rbp
  -       .cfi_def_cfa_register 6
  -       movl    $4, -4(%rbp)
  -       cmpl    $0, -4(%rbp)
  -       je      .L2
  -       addl    $4, -4(%rbp)
  -       jmp     .L3
  -.L2:
  -       sall    $2, -4(%rbp)
  -.L3:
  -       movl    $0, %eax
  +       .cfi_def_cfa_register %rbp
  +       movl    $0, -4(%rbp)
  +       movl    $4, -8(%rbp)
  +       cmpl    $0, -8(%rbp)
  +       je      .LBB0_2
  +# %bb.1:
  +       movl    -8(%rbp), %eax
  +       addl    $4, %eax
  +       movl    %eax, -8(%rbp)
  +       jmp     .LBB0_3
  +.LBB0_2:
  +       movl    -8(%rbp), %eax
  +       shll    $2, %eax
  +       movl    %eax, -8(%rbp)
  +.LBB0_3:
  +       xorl    %eax, %eax
          popq    %rbp
  -       .cfi_def_cfa 7, 8
  -       ret
  +       .cfi_def_cfa %rsp, 8
  +       retq
  +.Lfunc_end0:
  +       .size   main, .Lfunc_end0-main
          .cfi_endproc
  -.LFE0:
  -       .size   main, .-main
  -       .ident  "GCC: (Ubuntu 15.2.0-4ubuntu4) 15.2.0"
  -       .section        .note.GNU-stack,"",@progbits
  -       .section        .note.gnu.property,"a"
  -       .align 8
  -       .long   1f - 0f
  -       .long   4f - 1f
  -       .long   5
  -0:
  -       .string "GNU"
  -1:
  -       .align 8
  -       .long   0xc0000002
  -       .long   3f - 2f
  -2:
  -       .long   0x3
  -3:
  -       .align 8
  -4:
  +                                        # -- End function
  +       .ident  "Ubuntu clang version 21.1.2 (2ubuntu6)"
  +       .section        ".note.GNU-stack","",@progbits
  ```,
  caption: [GCC与Clang编译产物的差异],
) <code:diff-clang-compile>

= 源代码

本实验程序与报告源代码可以从#link("https://github.com/CSharperMantle/hdu2025_compiler_principles/tree/main/exp_1")处获取。
