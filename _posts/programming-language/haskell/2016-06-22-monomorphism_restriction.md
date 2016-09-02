---
title: Monomorphism Restriction
author: sighingnow
date: 2016-06-22
tag: [Haskell]
category: 编程语言
layout: post
---

Haskell Wiki上Monomorphism Restriction的定义为

> a counter-intuitive rule in Haskell type inference. If you **forget to provide a type
> signature**, sometimes this rule will fill the free type variables with specific types using
> "type defaulting" rules.

对于一个函数，当没有给出类型签名是，Haskell编译器会自动推导其类型，自由类型变量会被赋为一个默认
的类型，通常默认类型都没有我们所期待的那么“多态”。

定义
----

Haskell2010 Report里在[4.5.5 The Monomorphism Restriction](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-930004.5.5)章节中最Monomorphism Restriction做了详细的规定。

Ad-hoc polymorphism
-------------------

Haskell的Type classes属于Ad-hoc polymorphism的一种。Type classes在编译时会被翻译成类似于字典
的结构[^1]，

规则与必要性
------------



you often cannot overload a function unless you provide an explicit type signature.

Defaulting Rules
----------------

Haskell2010 Report中规定了如何处理与`Num`类型类相关的不明确类型的Defaulting Rules。在任何一个
module中，都可以用`default (t1, t2, ...)`的语法来说声明默认类型，`default ()`表示在该module中
禁用任何Defaulting Rules，如果一个module中没有任何`default`声明，假定声明
了`default (Integter, Double)`。这也是当启用Monomorphic Restriction时`Num`类型类约束的自由类型变量
都被特化成`Integter`类型或者`Double`类型。



[^1]: [http://stackoverflow.com/a/7662890/5080177](http://stackoverflow.com/a/7662890/5080177)

