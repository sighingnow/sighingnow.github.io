---
title: 斐波那契数列
author: He Tao
date: 2015-05-22
tag: Algorithm
category: Algorithm
layout: post
---

2014年蓝桥杯本科C/C++组预赛第9题是很好的一道关于斐波那契(Fibonacci)数列的题目。本文将从这一题母出发，探讨一些与斐波那契数列相关的性质。

题目
----

题目链接：[斐波那契 http://lx.lanqiao.org/problem.page?gpid=T121][1]

题目内容：

> 问题描述

斐波那契数列大家都非常熟悉。它的定义是：

　　$$ f(x) = 1 .... (x=1,2) $$
　　$$ f(x) = f(x-1) + f(x-2) .... (x>2) $$

对于给定的整数 n 和 m，我们希望求出：$ f(1) + f(2) + \dots + f(n) $ 的值。但这个值可能非常大，所以我们把它对 $ f(m) $取模。

公式如下: $$ (\sum_{i=n}^n{f(i)}) \ mod \ f(m) $$

但这个数字依然很大，所以需要再对$ p $求模。

> 输入格式

输入为一行用空格分开的整数 n m p (0 < n, m, p < 10^18)

> 输出格式

输出为1个整数，表示答案

> 样例输入

    2 3 5

> 样例输出

    0

> 样例输入

    15 11 29

> 样例输出

    25

Fibonacci数列
-------------

通过递推式和特征方程，不难得到Fibonacci数列的通项公式为：

$$f(n)={\frac{1}{\sqrt{5}}}*((\frac{1+\sqrt(5)}{2})^n-(\frac{1+\sqrt(5)}{2})^n)$$

Fibonacci数列有很多很有用的性质，首先根据Fibonacci数列的递推关系，有：
$$ f(n+1) = f(n) + f(n-1) $$
那么，由此得到：
$$ f(n) = f(n+1)-f(n-1) $$
从这个等式可以推出：
$$ \begin{align*} 
\sum_{i=1}^n {f(i)} &= f(1)+f(2)+f(3)+\dots+f(n) \\
&= f(1)+f(3)-f(1)+f(4)-f(2)+\dots+f(n+1)-f(n-1) \\
&= f(n)+f(n+1)-f(2) \\
&= f(n+2)-1
\end{align*} $$
这个性质非常重要，通过这个性质，可以将Fibonacci数列前$N$项的求和转化为求解某一项的值。

与上式同理，不难得到：

$$\sum_{i=1}^n {f(2*i-1)} = f(1)+f(3)+f(5)+\dots+f(2*n-1) = f(2*n)-1 $$
$$\sum_{i=1}^n {f(2*i)} = f(2)+f(4)+f(5)+\dots+f(2*n) = f(2*n+1) - 1 $$

此外，Fibonacci数列还有以下这些有用的性质：

$$ \sum_{i=1}^n {f(i)^2} = f(n)*f(n+1) $$
$$ \sum_{i=1}^n {f(i)*(-1)^i} = (-1)^n * (f(n+1)-f(n))+1 $$

常用的还有下列结论：

$$ f(n+m) = f(n+1)*f(m) + f(n)*f(m-1) $$
$$ f(n)^2 = (-1)^{n+1} + f(n-1)*f(n+1) $$

这两个公式都可以通过数学归纳法证明。

题目分析
---------

通过上面的Fibonacci数列前N项求和公式，可以将原来的问题简化成$f(n)%f(m)%p$的情形。

参考
----

1. [Fibonacci数列的幂和][3]
2. [从蓝桥杯来谈Fibonacci数列][2]


<!---------------------------links------------------------------->

[1]: http://lx.lanqiao.org/problem.page?gpid=T121
[2]: http://blog.csdn.net/acdreamers/article/details/21822165
[3]: http://blog.csdn.net/acdreamers/article/details/23039571


