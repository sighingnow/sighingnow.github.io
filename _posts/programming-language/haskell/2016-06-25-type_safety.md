---
title: 类型安全
author: Tao He
date: 2016-06-25
tag: [Haskell]
category: 编程语言
layout: post
---

编程语言的类型系统可以带来诸如减少错误、文档化、利于编译器优化等好处。读MirageOS的论文，作者提到了
使用OCaml的类型安全的好处，本文大致整理了一些类型安全相关的概念。

<!--more-->

类型分类
--------

参考@rainoftime的一个帖子[^1]，首先定义Well behaved与Ill behaved，并在此基础上定义强类型与弱类型、
静态类型与动态类型。

1. Program Errors

    + trapped errors: 导致程序立即终止执行。
    + untrapped errors: 出错后程序继续运行（缓冲区溢出）。

2. Forbidden behaviours：包括所有的untrapped errors，并且可能包含trapped errors。
3. 如果程序执行过程中不会出现Forbidden behaviours，则成为Well behaved。
4. 如果一种语言的所有程序都是Well behaved，则成为Strongly typed（例如Java/C#），否则为Weakly typed（例如C/C++）。
5. 如果在编译时拒绝Ill behaved程序，则称为静态（Statistically）类型（C/C++/Java/C#），如果在运行时拒绝Ill behaviours，则称为动态（Dynamiclly）类型（Python）。

关于类型系统分类的例子：

|:---------------------|:--------------:|
| 无类型               | 汇编           |
| 弱类型、静态类型     | C/C++          |
| 弱类型、动态类型检查 | Perl/PHP       |
| 强类型、静态类型检查 | Java/C#        |
| 强类型、动态类型检查 | Python, Scheme |
| 静态显式类型         | Java/C         |
| 静态隐式类型         | Ocaml, Haskell |

类型安全
-------

类型安全的定义[^2]：

> Safety: Any attempt to misinterpret data is caught at compile time or generates a well-specified error at runtime.

可以理解为类型系统给内存中的plain data赋予了类型信息，如果不按照类型信息来解释内存中的数据，应该产生编译错误或者
产生well-specified的运行时错误。_What is type safety_一文[^3]也提到Type Safety的一个Intuitive的解释“Well typed programs
cannot go wrong.”。

每种编程语言都有自己的语法和语义约束，语法定义决定了程序应该写成什么样，语义定义决定了程序的含义和行为。编程语言中普遍存在
者语法合法但语义不合法的例子。例如C语言程序`{ char buf[4]; buf[4] = 'x'; }`就属于这类错误。在C语言中，数组越界访问属于
未定义行为，运行这种含有未定义行为的程序，就会出现ill-specified的运行时错误。一个具有Type safety的编程语言，可以保证所有能够
通过便宜检查的程序，都不会出现上输错误（go wrong）。C/C++不是type safe的，Java/C#是type safe的，对于数组越界访问，C/C++
语言中是未定义行为，Java/C#中会抛出一个特定类型的异常，同样，Python/Ruby也具有Type safety。

References
----------

[^1]: https://www.zhihu.com/question/19918532/answer/21647195
[^2]: http://www.cs.cornell.edu/courses/cs1130/2012sp/1130selfpaced/module1/module1part4/strongtyping.html
[^3]: http://www.pl-enthusiast.net/2014/08/05/type-safety/
