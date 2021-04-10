---
title: Non-constructive proof of existance
author: Tao He
date: 2016-01-31
tags: [Coq, Proof, Logic]
category: 编程语言
layout: post
---

在 _A Course in Discrete Structures_ by Rafael Pass and Wei-Lung Dustin Tseng 上看到一个很有意思的证明方法：Non-constructive proof of existance。

<!--more-->

Proof by Cases
---------------

Proof by Cases 指的是这样一种证明技巧：将定理的定义域拆分成多个小区间，证明每个区间内，定理成立。
例如用这种方法证明：

$$(n+1)^2 \ge 2^n \textit{ for all integers n satisfying } 0 \le n \le 5.$$

只需要枚举$0$到$5$的所有$n$，证明其都满足$(n+1)^2 \ge 2^n$。

| $n$ | $(n+1)^2$ |       | $2^n$ |
|:---:|:---------:|:-----:|:-----:|
| 0   |  0        | $\le$ |  1    |
| 1   |  1        | $\le$ |  2    |
| 2   |  4        | $\le$ |  4    |
| 3   |  9        | $\le$ |  8    |
| 4   | 16        | $\le$ | 16    |
| 5   | 25        | $\le$ | 32    |

由此，可证得结论。

另一个例子，证明：

$$\textit{For all real x, } |x^2| = |x|^2.$$

将其定义域分为两部分，$x \ge 0$ 和 $x < 0$.

$${\begin{cases}
\text{If } x \ge 0, & \text{ then } |x^2| = x^2 = |x|^2. \\
\text{If } x < 0,   & \text{ then } |x^2| = (-x)^2 = |x|^2.
\end{cases}}$$

Proof by Example
----------------

Proof by Example 只是证明一个谓词逻辑的命题，只需要找出一个例子证明原命题或者原命题的否命题即可。例如，证明
$$\textit{There exists some n such that } (n+1)^2 < 2^n.$$
为了证明这一命题，找出一个样例，例如$n=6$就可以满足条件，命题得证。

Non-constructive proof of existance
-----------------------------------

对于existance类的证明，找出一个case或者一个example即可。而对于有些命题，并不需要显式地将这个样例构造出来，就可以证明其
存在性。这一证明技巧叫做Non-constructive proof of existance.

例如，证明：

$$\textit{There exists irrational numbers x and y such that } x^y \textit{ is rational.}$$

+ Proof: we know that $\sqrt{2}$ is irrational, then let $z = \sqrt{2}^{\sqrt{2}}$.
    + If $z$ is rational, then we conclude this theorem.
    + If $z$ is irrational, then take $x = z = {\sqrt{2}}^{\sqrt{2}}$, and $y = \sqrt{2}$. Then

      $$x^y = ({\sqrt{2}}^{\sqrt{2}})^{\sqrt{2}} = {\sqrt{2}}^{\sqrt{2}\sqrt{2}} = {\sqrt{2}}^2 = 2$$

      is indeed a rational number.

+ $z$ is rational and $z$ is irrational cover the domain of this problem, we proof the existance of
  the pair of $x$ and $y$ without construct the example asked by the theorem explicitly.

