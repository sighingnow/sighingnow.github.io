---
title: 《Purely Functional Data Structures》笔记
author: Tao He
date: 2015-10-04
tag: [Haskell]
category: 编程语言
layout: post
---

**Purely Functional Data Structures**. Chris Okasaki. September 1996. CMU-CS-96-177. School of Computer Science. Carnegie Mellon University. [Download pdf version](https://www.cs.cmu.edu/~rwh/theses/okasaki.pdf) from CMU. 与此相关的 StackExchange 上的另一个问题：[What's new in purely functional structures since okasaki ?](http://cstheory.stackexchange.com/questions/1539/whats-new-in-purely-functional-data-structures-since-okasaki)。

C语言程序员学习数据结构往往很容易找到很多优秀的教科书，但是，诸如使用 Standard ML 和 Haskell 的函数式编程语言程序员去很难拥有这种奢侈品。尽管有一些为命令式语言设计的
数据结构很容易用函数式编程语言实现，但是，**most cannot**，原因在于函数式编程语言并不赞成使用这些数据结构所依赖的那种赋值方式。这本书将展示延迟求值(lazy evaluation)
在 amortized functional data structure 中的基础性的作用，以及与可持久化(presistance)数据结构的联系，还有将 strict 和 lazy 两种求值策略组合在一起，以及
polymorphic recursion, higher-order 和 recursive modules.

<!--more-->

Chapter 1: Introduction
-----------------------

> Programmers can use any language thry want, as long as it's imperative.   -- by Henry Ford.

尽管函数式编程方法论层面的好处(methodological benefits)已经众所周知，但这些益处并没有在使用函数式语言来描述数据结构这一点上体现出来。有两个基本的原因：

1. 函数式语言反对 destructive updates (assignments)

> destructive updates can be dangerous when misused, but tremendously effective when used properly.

2. 与命令式语言相比，人们对函数式编程语言描述的数据结构的灵活性的期待更高

使用命令式语言时，当我们更新一个数据结构时，原来的值已经不复存在，但当我们更新函数式语言的数据结构时，我们希望能够同时使用原来的值和新的值，用来完成后续的计算。函数式编程中，数据结构自动地就是
可持久化的(autommatically presistent)。

> Furthermore, theoreicians have established lower bounds suggesting that functional programming languages may be fundamentally less efficient than imperative languages
> in some situation.

严格(strict)求值与惰性(lazy)求值主要的表现实在对函数的参数的处理上。

Chapter 2: Lazy Evaluation and $-Notation
-----------------------------------------

Two essential properties of lazy evaluation:

+ the evaluation of a given expression id delayed(suspended), until its result is neeeded.
+ the first time a suspended expression is evalated, the result is mmeoized, so that the next time it is needed, it can be looked up rather than recomputed.

Primitives of lazy evaluation(delay and force):

    type a susp
    val delay: (unit -> a) -> a susp
    val force: a susp -> a

$-notation: import $-notation to make suspending an expression as syntactically lightweight as possible.

    delay(e): `$_e`: to suspend the evaluation of some expression `e`.
    force(e): evaluate and memoize the contents of `$_e` and return the result.

function `take(ns, s)`: extract the first n element of a stream.

    fun take(ns, s) = delay(fn() => case n of
                                         0 => Nil
                                         _ => case force s of
                                                   Nil => Nil
                                                   Cons (x, s') => Cons (x, take (n-1, s')))


