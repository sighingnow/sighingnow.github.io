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

<!--more-->

对于一个函数，当没有给出类型签名是，Haskell编译器会自动推导其类型，自由类型变量会被赋为一个默认
的类型，通常默认类型都没有我们所期待的那么“多态”。

定义
----

Haskell2010 Report里在[4.5.5 The Monomorphism Restriction](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-930004.5.5)章节中最Monomorphism Restriction做了详细的规定。

Ad-hoc polymorphism
-------------------

Haskell的Type classes属于Ad-hoc polymorphism的一种。Type classes在编译时会被翻译成类似于字典
的结构[^1]，但这种翻译方式会导致一个严重的问题：每一个值都会被提升为一个函数。在StackOverflow
上给出的这个例子中，正是因为这个原因，值的记忆化被破坏。如果编译器不能做到足够的特化，程序的
性能会受到影响。

Defaulting Rules
----------------

Haskell2010 Report中规定了如何处理与`Num`类型类相关的不明确类型的Defaulting Rules。在任何一个
module中，都可以用`default (t1, t2, ...)`的语法来说声明默认类型，`default ()`表示在该module中
禁用任何Defaulting Rules，如果一个module中没有任何`default`声明，假定声明
了`default (Integter, Double)`。这也是当启用Monomorphic Restriction时`Num`类型类约束的自由类型变量
都被特化成`Integter`类型或者`Double`类型。

Monomorphism Restriction
------------------------

回顾Haskell Wiki中对 Monomorphism Restriction 的定义，对于一个函数，如果没有类型签名，编译器会
自动按照 defaulting rules 计算满足其所在的上下文约束的类型签名。

Haskell2010标准中定义了不受限的函数需要满足两个条件[^2]：

+ every variable in the group is bound by a function binding, `f x = x`.
+ a simple pattern binding and an explicit type signature is given for every variable in the group that is bound by
  simple pattern binding, `plus :: Num a => a -> a -> a; plus = (+)`.

不满足这两个条件就会受到Monomorphism Restriction的影响。更直观的解释，在没有手动给出类型签名的前提下，如果一个定义
看上去不像是一个函数（在等号的左侧没有参数），则这个定义必须满足单一同态（在不同的地方有相同的类型）。

如果没有Monomorphism Restriction，某些情况下会导致重复计算的问题。例如`genericLength`函数的类型签名为
`genericLength :: Num a => [b] -> a`，在

~~~haskell
f xs = let len = genericLength xs
       in (len, len)
~~~

中，`len`的类型为`Num a => a`，如果没有Monomorphism Restriction，`len`的值就有可能会被计算两次，`f`的类型会被推导为
`(Num a, Num b) => [x] -> (a, b)`，这就意味这`len`的值不能被记忆化。



此外，Monomorphsim Restriction可以帮助发现某些潜在的类型错误，比如[^3]:

~~~haskell
(===) :: Eq a => Maybe a -> a -> Maybe a
Just l === r | l == r = Just l
_ === _ = Nothing
infixl 4 ===

result = Just 1 === Nothing
~~~

如果有Monomorphism Restriction，`1`会按照defaulting rules被翻译为`Integer`类型，显然不会和`Nothing`属于同一个类型，
如果禁用了Monomorphism Restriction，GHC推导出的`result`的类型为`forall {a}. (Num (Maybe a), Eq a) => Maybe (Maybe a)`，
显然，`1`被赋予了类型`Maybe a`，而`Maybe a`需要是一个`Num`类型类的实例类型。

GHC Extension
-------------

在GHC 8.0.1 中，ghci 中默认关掉了Monomorphism Restriction，使用 ghc 编译代码时，默认启用
Monomorphism Restriction。可以使用 GHC 的扩展 `MonomorphismRestriction` 和 `NoMonomorphismRestriction`
来手动控制是否启用 Monomorphism Restriction。


[^1]: [http://stackoverflow.com/a/7662890/5080177](http://stackoverflow.com/a/7662890/5080177)
[^2]: [The monomorphism restriction](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-930004.5.5)
[^3]: [Haskell: Demonstrating Monomorphism Restriction](https://gist.github.com/CMCDragonkai/5cce00f732fcac0ec026)


https://kseo.github.io/posts/2017-01-04-type-defaulting-in-haskell.html
