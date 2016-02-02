---
title: Y combinator
author: He Tao
date: 2016-01-12
tags: [Scheme]
categories: 编程语言
layout: post
---

不动点(Fix-point)指的是 $f(x)$ 的定义域内的一个点 $c$ 是函数 $f(x)$ 的不动点，当且仅当 $c$ 满足：

$$f(c) = c$$

Fix-point Combinator 指的是一个高阶函数 $y$ 满足

$$\forall f: y \ f = f \ (y \ f)$$

也就是说，如果令 $x = y \ f$ 那么

$$x = f \ x$$

Fix-point Combinator 是一个用于求函数的不动点的高阶函数，$y \ f$ 表示函数 $f$ 的一个不动点。通过不动点组合子，可以实现通过非递归的 Lambda 表达式来定义匿名的递归函数。

Y Combinator 是 Haskell B. Curry 发现的一种不动点组合子，定义为

$$Y := \lambda f. (\lambda x. (f (x \ x)) \lambda x. (f (x \ x)))$$

<!--more-->

证明过程
----------

通过将 Y Combinator 作用与一个函数 $g$ 上，证明 $Y \ g$ 是函数 $g$ 的一个不动点。

$$\begin{aligned}
& (Y \ g) \\
=& (\lambda f. (\lambda x. (f (x \ x)) \lambda x. (f (x \ x))) g) && \text{defination of } Y \\
=& (\lambda x. (g (x \ x)) \lambda x. (g (x \ x))) && \beta-\text{reduction}: \text{applied } Y \text{ to } g \\
=& (\lambda y. (g (y \ y)) \lambda x. (g (x \ x))) && \alpha-\text{conversion} \\
=& (g (\lambda x. (g (x \ x)) \lambda x. (g (x \ x)))) && \beta-\text{reduction}: \text{applied left function to right function} \\
=& (g (Y \ g))
\end{aligned}$$


