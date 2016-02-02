---
title: 自然数的除法与取模
author: He Tao
date: 2015-11-06
tags: [Coq, 自然数, Logic]
category: Coq
layout: post
---

Coq 对自然数的除法与取模的实现很有讲究！

皮亚诺公理
---------

自然数，natural number, 严格定义有皮亚诺公理给出。平亚诺序数理论提出自然数的五条公理，这五条公理的非形式化描述如下：

1. 0 是自然数；
2. 每一个确定的自然数 n 都有一个确定的后继，记作 n+1, n+1 也是自然数。
3. 如果 m，n 都是自然数，并且 m+1=n+1, 那么，m = n。
4. 0 不是任何自然数的后继。
5. 如果一些自然数的集合 S 具有性质：

    + 1 在 S 中；
    + 若 n 在 S 中，那么 n+1 也在 S 中。

    那么 **S = N**。(公理 5 保证了**数学归纳法的正确性**，因此也被称作归纳法原理。)

皮亚诺公理的形式化描述：

+ (e in S)
+ (forall a in S)(f(a) in S)
+ (forall b in S)(forall c in S)(f(b) = f(c) -> b = c)
+ (forall a in S)( f(a) /= e)
+ (forall A in S)(((e in A) and (forall a in A)(f(a) in A)) -> (A = S) )

自然数的运算
----------




