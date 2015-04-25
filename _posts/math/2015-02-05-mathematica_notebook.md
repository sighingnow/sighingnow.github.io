---
title: Mathematica 笔记本(notebook)
author: He Tao
date: 2015-02-05
tag: Mathematica
category: Math
layout: post
---

本文主要记录Mathematica笔记本(notebook)使用过程中的一些小细节(Tips)。

### 注释

所有包含在`(*`和`*)`之间的内容为注释内容，都会被Mathematica内核忽略。

### Mathematica区分字母大小写

所有的Mathematica命令都是以大写字母开头的，Mathematica区分字母大小写。因此，为了避免冲突，所有的用户定义符号都应当用小写字母开头。

### 函数参数

Mathematica中用**方括号**`[]`表示函数参数。例如：

<!--more-->

    Sqrt(10);

### N[expr] 和 //

Mathematica中默认并不给出数值值，例如，`Sqrt[10]`将得到根号表示的数，而不会得到小数数值。`N[expr]`函数可以得到表达式的数值值。

    N[expr] 给出 expr 的数值值.
    N[expr, n] 尝试给出具有 n 位精度的数值值.

除非`expr`是精确的，或者具有足够高的精度，否则`N[expr, n]`可能无法给出具有`n`位精度的结果。

进行近似计算的另外一种方法是在需要求值的表达式右边用`//N`,因此，`N[expr]`和`expr // N` 功能上等价。但`//N`的方法作用于变量时，会直接把数值值赋给变量，而 `N[expr]`作用于变量时则不会。

### ? var

可以通过如下语句查看符号所代表的意义：

    ? var

### ? func 和 ?? func

+ `? func`表示查看函数的定义及基本用法。
+ `?? func`表示查看关于函数用法的详细信息。

### Clear 与 Remove

+ Clear[符号名]：清除指定符号的定义和取值，但并没有清除它的属性、信息或默认值。因此，指定的符号仍在Mathematica的符号清单中。
+ Remove[符号名]：完全删除指定的符号，因此除非重新进行了定义，否则不再识别这个符号(Information::notfound)。

### Out(%)

+ % n 或 Out[n]
    是一个对象，被赋予在第 n 个[Null]输出行上产生的值.
+ %
    给出产生的最后一个结果.
+ %%
    给出最后一个结果之前的结果. %% [Ellipsis]% (k 次)给出倒数第 k 个结果.

在命令提示符环境下，可以用`%`来表示上一条语句。其内容可以通过输入

    %

然后按下`[Shift][Enter]`键查看。

还有一下两个技巧：

+ Out[] 等于 %.
+ Out[-k] 等于 %%...% (k次).

### 通配符"\*"与命令查找

+ 找出所有以"A"开头的命令

        ? A*

+ 找出所有以"A"结尾的命令

        ? *A

+ 找出所有中间包含"A"的命令

        ? *A*

+ 找出到现在为止内核中已经用到了哪些符号

        ? `*
    
    解释：反引号"\`"表示全局性。

    `Clear["\`*"]`会清楚所有全局符号。`Remove["\`*"]`会删除所有全局符号。

### 常用常数

Pi: 圆周率（圆的周长和直径的比率）。

E：自然对数的底。

Degree：给出1度的弧度，即 [Pi]/180。

GoldenRatio：给出黄金比率 [Phi]=1/2 (Sqrt[5]+1)，其数值 [TildeEqual]1.61803.

Infinity：Infinity 或 [Infinity] 表示正无穷大的符号.

EulerGamma：表示欧拉常数 [Gamma]，其数值 [TildeEqual]0.577216.

Catalan：Catalan 常数，数值 [TildeEqual]0.915966.

### 字符串拼接

    strA <> strB

或者：

    StringJoin[strA, strB, ...]




