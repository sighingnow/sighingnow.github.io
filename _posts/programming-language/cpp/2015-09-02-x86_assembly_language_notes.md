---
title: X86汇编学习笔记
author: Tao He
date: 2015-09-02
tags: C/C++
categories: 编程语言
layout: post
---

X86汇编学习笔记。

<!--more-->

寄存器
-----

寄存器命令示意图：

![寄存器命名]({{site.url}}/resource/x86_assembly_language_notes/register_naming.png)

8个通用寄存器：

+ %eax
+ %ecx
+ %edx
+ %ebx
+ %esi
+ %edi
+ %esp: 栈指针
+ %ebp: 帧指针

内存寻址模式
----------

指令的操作数可以使立即数，寄存器和存储器引用。x86支持多种内存寻址模式，允许不同形式的存储器引用。如下表所示：

| 类型    | 格式                    | 操作数的值                  | 名称                  |
|:-------:|-------------------------|-----------------------------|-----------------------|
| 立即数  | `$Imm`                  | Imm                         | 立即数寻址            |
| 寄存器  | `Ea`                    | R[Ea]                       | 寄存器寻址            |
| 存储器  | `Imm`                   | M[Imm]                      | 绝对寻址              |
| 存储器  | `(Ea)`                  | M[R[Ea]]                    | 间接寻址              |
| 存储器  | `Imm(Eb)`               | M[R[Eb] + Imm]              | （基址 + 偏移）寻址   |
| 存储器  | `(Eb, Ei)`              | M[R[Eb] + R[Ei]]            | 变址寻址              |
| 存储器  | `Imm(Eb, Ei)`           | M[R[Eb] + R[Ei] + Imm]      | 变址寻址              |
| 存储器  | `(,Ei,s)`               | M[R[Ei] * s]                | 比例变址寻址          |
| 存储器  | `Imm(,Ei,s)`            | M[R[Ei] * s + Imm]          | 比例变址寻址          |
| 存储器  | `(Eb, Ei, s)`           | M[R[Eb] + R[Ei] * s]        | 比例变址寻址          |
| 寄存器  | `Imm(Eb, Ei, s)`        | M[R[Eb] + R[Ei] * s + Imm]  | 比例变址寻址          |

注：比例因子 s 必须是 1, 2, 4 或 8。

数据传送指令
----------

+ MOV S, D： `D <- S`
+ MOVS S, D:  `D <- (signed extend) S`
+ MovZ S, D:  `D <- (zero extend) S`
+ PUSHL S: `R[%esp] <- R[%esp] - 4; M[R[%esp]] <- S`
+ POPL D: `D <- M[R[%esp]]; R[%esp] <- R[%esp] + 4`

寄存器 %esp 保存着栈顶元素的地址。

加载有效地址
-----------

加载有效地址(load effective address)指令：leal

指令格式：leal S, D

指令执行效果：D <- &S

leal指令可以用来描述普通的算术操作，例如：

    leal 7(%edx, %edx, 4), %eax           %eax = 5 * %edx + 7

算术指令
-------

常用的算术指令有：

+ inc, dec, neg, not
+ add, sub, imul, xor, or, and
+ sal, shl, sar, shr

乘除法指令
---------

+ imull S
+ mull S

有符号/无符号乘法：`R[%edx]:R[%eax] <- S * R[%eax]`

+ idivl S
+ divl S

有符号/无符号除法：`R[%edx] <- R[edx]:R[%eax] % S; R[%eax] <- R[edx]:R[%eax] / S;`

+ cltd

转换为4字：`R[%edx]:R[%eax] <- SignExtend(R[%eax])`

控制结构
-------

x86及其代码提供的低级机制来实现有条件的行为：测试数据值，然后根据测试的结果来改变控制流或者数据流。

条件码：x86体系结构的CPU维护者一组单个位的条件吗(condition code)寄存器，它们用来描述最近的算术或者逻辑操作的属性。可以检测这些寄存器的值来执行条件分支指令。

+ CF: 进位标志。最近的操作使最高位产生了进位。可以用来检查无符号操作数的溢出。
+ ZF: 零标志。最近的操作得出的结果为0。
+ SF: 符号标志。最近的操作得到的结果为负数。
+ OF: 溢出标志。最近的操作导致了一个补码溢出（正溢出或者负溢出）。

值得注意的是：leal指令本质上仅仅是在进行地址计算，用leal指令来表示某些算术过程使不会设置条件码。但其他的算术指令在出现上述结果时会设置相应的状态码。

比较指令(cmp)和测试指令(test)仅仅设置状态码而不改变其他的寄存器。

+ cmp S2, S1

如果S1-S2是负数，设置SF寄存器。

+ test S2, S1

如果 S1 & S2 为零，设置 ZF 寄存器。

x86汇编中，通过`set`系列的指令来访问条件码，通常有以下三种方法：

+ 根据条件码的某个组合，将一个字节设置为0或者1。
+ 可以跳转到程序的某个其他的部分。
+ 可以有条件地传送数据。

跳转指令会导致执行切换到程序的一个全新的位置。在汇编代码中，这些跳转的目的地通常用一个标号(label)来指明。

高级语言控制结构的翻译
--------------------

通过CPU所提供的底层条件控制机制，可以将C语言等高级语言中的条件控制结构与汇编代码相对应起来。

### 翻译条件分支

将条件表达式和语句从C语言翻译到机器语言代码，最常用的方式是结合有条件和无条件跳转。另一种方式是使用数据的条件转移指令，而不是控制的条件转移。

C语言的if-else通用模板：

    if (test-expr)
        then-statement
    else
        else-statement

对于这种通用形式，汇编代码产生的控制流：

        t = test-expr
        if(!t)
            goto false
        then-statement
        goto done
    false:
        else-statement
    done:

例如：

~~~c
int absdiff(int x, int y) {
    if(x < y) {
        return y-x;
    }
    else {
        return x-y;
    }
}
~~~

生成的汇编代码为：

~~~asm
_absdiff:
    pushl   %ebp
    movl    %esp, %ebp
    movl    8(%ebp), %eax
    cmpl    12(%ebp), %eax
    jge L2
    movl    12(%ebp), %eax
    subl    8(%ebp), %eax
    jmp L3
L2:
    movl    8(%ebp), %eax
    subl    12(%ebp), %eax
L3:
    popl    %ebp
    ret
~~~

### 翻译循环结构

C 语言提供了多种循环结构，for, while, 以及 do-while。汇编中没有相应的指令的存在，可以用条件测试和跳转组合结合起来的办法实现循环控制结构。

do-while 循环的通用模板：

    do
        body-statement
    while(test-expr)

将其翻译成汇编代码的通用结构为：

    loop:
        body-statement
        t = test-expr
        if(t)
            goto loop

while 循环的通用形式：

    while(test-expr)
        body-statement

翻译成汇编指令的结果：

        t = test-expr
        if(!t)
            goto done
    loop:
        body-statement
        t = test-expr
        if(t)
            goto loop
    done:

for 循环的通用形式：

    for(init-expr; test-expr; update-expr)
        body-statement

对应的汇编代码框架：

        init-expr;
        t = test-expr
        if(!t)
            goto done
    loop:
        body-statement
        update-expr
        t = test-expr
        if(t)
            goot loop
    done:

### 条件传送指令

实现条件操作的传统方法是利用控制的条件转移，当条件满足时，程序沿着一条路径执行，而当条件不能满足时，程序沿着另外一条路径执行。这种机制简单而通用，但在现代处理器上，效率可能会很低。数据的条件转移时一种替代的策略。这种方法先计算一个条件操作的两种结果，然后再根据条件是否满足而选取一个。**只有在一些限制的情况下，这种策略才可行。但是，如果可行，就可以利用一条非常简单的条件传送指令来实现它。**条件传送指令更好地匹配了现代处理器的性能特性。

cmov 指令用于实现数据的条件传送，例如，gcc生成的使用条件传送的absdiff函数的汇编代码：

~~~asm
_absdiff:
    pushl   %ebx
    movl    8(%esp), %ecx
    movl    12(%esp), %edx
    movl    %ecx, %eax
    movl    %edx, %ebx
    subl    %edx, %eax
    subl    %ecx, %ebx
    cmpl    %edx, %ecx
    cmovl   %ebx, %eax
    popl    %ebx
    ret
~~~

关于常规的分支结构性能损失的解释：现代微处理器的流水线结构采取分支预测器来提高CPU的利用效率，但分支预测一旦失败，流水线需要丢弃已经做好的工作，重新回到正确的开始处执行。这样会有很严重的时间惩罚，导致性能受损。在absdiff函数的示例中，两个分支的执行序列都只有一两条指令，因此，分支预测的错误惩罚主导者这个函数的性能。

并不是所有的条件表达式都可以用条件转移指令来求值。如果if分支和else分支中任意一个可能产生错误条件或者副作用，就会导致出现非法的行为。

例如：

~~~c
int cread(int *xp) {
    return xp ? *xp : 0;
}
~~~

这段代码就不适合采取数据条件传送指令，因为如果 xp 为空指针，执行第一个分支是就会导致一个空引用。

当某个分支的执行有副作用，例如对全局变量的修改时，也只能采取传统的分支策略，例如：

~~~c
int count = 0;
int absdiff_se(int x, int y) {
    return x < y ? (count++, y-x) : x - y;
}
~~~

使用条件传送指令也不是总会提升程序的性能，当if分支和else分支需要大量的计算时，就不宜采取这种策略。

函数代码的机器级表示
-----------------

IA32程序用程序栈来支持过程调用。程序用栈来传递参数、存储返回值信息，保存寄存器用于以后恢复，以及本地存储。为单个过程分配的那部分栈称为栈帧(stack frame)栈帧的最顶端以两个指针界定。寄存器%esp为帧指针，%ebp为栈指针。

### 栈帧结构

IA32 栈帧结构示意图：

~~~
        -------------------------------   高地址
                 .                |
                 .             较早的帧
                 .                |
        -------------------------------
                 .                |
                 .                |
                 .                |
            --------------        |
               参数 n             |
            --------------        |
                 .             调用者的帧
                 .                |
               参数 1             |
            --------------        |
               返回地址           |
        -------------------------------
              被保存的%ebp        |
            --------------        |
             被保存的：           |
               寄存器           当前帧
               本地变量           |
               临时变量           |
            --------------        |
              参数构造区域        |
        -------------------------------  低地址
~~~

假设过程 P 调用过程 Q ，则Q的参数放在 P 的栈帧中，另外，当 P 调用 Q 时，返回地址（就是当程序从 Q 中返回时继续执行的位置）被压入栈中，形成 P 的栈帧的末尾。Q 的栈帧从保存的帧指针开始。

### 转移控制指令

+ call: 过程调用
+ enter: 为新的过程准备栈帧。
+ leave: 为返回准备栈
+ ret: 从过程调用中返回

* call 指令的效果是将返回地址入栈，并跳转到被调用过程的起始处。ret 指令从栈中弹出地址，并跳转到这个位置。

* enter的作用相当`push ebp`和`mov ebp, esp`。

* leave的作用相当`mov esp, ebp`和`pop ebp`。

Win32汇编中局部变量的使用方法可以解释一个很有趣的现象：在DOS汇编的时候，如果在子程序中的push指令和pop指令不配对，那么返回的时候ret指令从堆栈里得到的肯定是错误的返回地址，
程序也就死掉了。但在Win32汇编中，push指令和pop指令不配对可能在逻辑上产生错误，却不会影响子程序正常返回，原因就是在返回的时候esp不是靠相同数量的push和pop指令来保持一致的，
而是靠leave指令从保存在ebp中的原始值中取回来的，也就是说，即使把esp改得一塌糊涂也不会影响到子程序的返回，当然，“窍门”就在ebp，把ebp改掉，程序就玩完了

### AT&T汇编的enter指令和leave指令

+ enter指令

在AT&T汇编中，enter等效于以下汇编指令：

    pushl %ebp        # 将%ebp压栈
    movl %esp %ebp    # 将%esp保存到%ebp， 这两步是函数的标准开头

+ leave指令

在AT&T汇编中，leave等效于以下汇编指令：

    movl %ebp, %esp
    popl %ebp

+ call指令

在AT&T汇编中，call foo（foo是一个标号）等效于以下汇编指令：

    pushl %eip
    movl f, %eip

+ ret指令

在AT&T汇编中，ret等效于以下汇编指令：

    popl %eip

### 寄存器使用惯例

根据惯例，

+ 寄存器 `%eax`, `%ecx`, `%edx` 被划分为**调用者保存**寄存器。
+ 寄存器 `%ebx`, `%esi`, `%edi` 被划分为**被调用者保存**寄存器。


