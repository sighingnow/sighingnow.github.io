---
title: Coupon Collector's Problem
author: Tao He
date: 2018-08-23
tag: [Probability Theory]
category: Math
layout: post
---

The coupon collector's problem describes the following question: there are $n$ different types of
coupons and the collector will receive a random coupon every time, what is the expected trials
that the collector can collect all $n$ types of coupons? We can conclude that the expected trails
grows as $\mathcal{O}(n\log{n})$.


<!--more-->

The Distribution of Trails for a new Coupon
-------------------------------------------

Let $T$ be the time to collect all $n$ types of coupons, and $t_i$ be the time to collect a new
coupon after $i-1$ coupons have been collected. We can see that the probability of $t_i$ satisfies
_geometric distribution_ with $p_i = \frac{n-(i-1)}{n}$, then we conclude

$$E(t_i) = \frac{1}{p_i} = \frac{n}{n-(i-1)}$$

We also obviously know that $t_i$ and $t_j$ are independent. Thus

$$\begin{aligned}E(T) &= \sum{E(t_i)} \\
                      &= \frac{1}{p_1} + \frac{1}{p_2} + \cdots + \frac{1}{p_n} \\
                      &= \frac{n}{n} + \frac{n}{n-1} + \cdots + \frac{n}{1} \\
                      &= n \cdot (\frac{1}{n} + \frac{n}{n-1} + \cdots + \frac{1}{1}) \\
                      &\simeq n\log{n} + \gamma n + \frac{1}{2} + O(\frac{1}{n})
\end{aligned}$$

The variance of the random variable $T$ can be calculated as (since $t_i$ are independent)

$$\begin{aligned}Var(T) &= \sum{Var(t_i)} \\
                        &= \frac{1-p_1}{p_1^2} + \frac{1-p_2}{p_2^2} + \cdots + \frac{1-p_n}{p_n^2} \\
                        &<= \frac{n^2}{n^2} + \frac{n^2}{(n-1)^2} + \cdots + \frac{n^2}{1^2} \\
                        &<= n^2 \cdot (\frac{1}{1^2} + \frac{1}{2^2} + \cdots + \frac{1}{n^2}) \\
                        &= n^2 \cdot \frac{\pi^2}{6}
\end{aligned}$$

A classical puzzle that can be modeled as Coupon Collector's problem is that, for a cubic dice, we
are expected to get all six points after how many trails? The answer for the question should be

$$\sum_1^6{\frac{6}{i}} \simeq 14.7 \simeq 15$$

Generalization
--------------

The Coupon Collector's Problem has been generalized as the expected trails of collect $m$ copy of
each $n$ coupons. When $m=2$, the problem is also called _The Double Dixie Cup Problem_[^9].
The expectation of $T_m$ satisfies

$$E(T_m) = n\log{n} + (m-1) n \log{\log{n}} + O(n), \texttt{as}\, n \to \infty$$

In the more general case, when $p_i$ is nonuniform, Philippe Flajolet gives[^1]

$$E(T) = \int_0^\infty{(1-\prod_{i=1}^n{1-e^{-p_i \cdot t}})}\,dt$$

If the collector receives $d$ coupons each run[^6], in the asymptotic case the $m$ copy of of each $n$
coupons will be collected expected within time

$$E(T_m) = n\log{n}/d + (m-1) (n/d) \log{\log{n}} + O(n \cdot m)$$

The Coupon Collector's Problem can also be generalized as stochastic process[^2][^3][^4]. The paper from
_MAT_[^7] also gives exhaustive analysis of this problem. The expectation can also be expressed using
the Stirling numbers[^5][^8].

References
----------

[^1]: [Birthday paradox, coupon collectors, caching algorithms and self-organizing search](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.217.5965&rep=rep1&type=pdf)
[^2]: [https://www.zhihu.com/question/48132206/answer/109307268](https://www.zhihu.com/question/48132206/answer/109307268)
[^3]: [https://www.zhihu.com/question/59058341/answer/162035021](https://www.zhihu.com/question/59058341/answer/162035021)
[^4]: [https://www.zhihu.com/question/38331955/answer/125686675](https://www.zhihu.com/question/38331955/answer/125686675)
[^5]: [https://www.zhihu.com/question/20426032/answer/86877804](https://www.zhihu.com/question/20426032/answer/86877804)
[^6]: [A Generalized Coupon Collector Problem, _Weiyu Xu, etc._](https://people.ece.cornell.edu/atang/pub/11/Coupon_collector.pdf)
[^7]: [The Coupon Collector’s Problem, _Marco Ferrante, Monica Saltalamacchia_](http://mat.uab.cat/matmat/PDFv2014/v2014n02.pdf)
[^8]: [Using Stirling numbers to solve coupon collector problems, _Marko R. Riedel_](http://pnp.mathematik.uni-stuttgart.de/iadm/Riedel/papers/coupon-stirling.pdf)
[^9]: [The Double Dixie Cup Problem][https://faculty.wharton.upenn.edu/wp-content/uploads/2012/04/Double-dixie-cup-problem.pdf]
