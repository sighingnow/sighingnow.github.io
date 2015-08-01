---
title: 子模性(Submodular)
author: He Tao
date: 2015-05-23
tag: 机器学习
category: 机器学习
layout: post
---



$A$是$B$的子集，则对于函数$ f() $，如果：
$$f(A+e)-f(A)>=f(B+e)-f(B)$$
成立，则说$f()$函数是子模的。

子模性描述的是一种增益递减的现象。例如：

$$u={1,2,3,4,5,6,7,8}$$
$$A={1,2,3}$$
$$B={1,2,3,5,6}$$
$f(A)=|A|$ 表示集合A的个数，那么：
$$f(A+e)-f(A)>=f(B+e)-f(B)$$
例如$e={3,4,5}$。


离散优化问题(Discrete Optimization Problems)
------------------

submodularity is ubiquitous(到处存在的) for discrete(离散) problems as is convexity(凸) for continuous(连续) problems.

例子(Motivation & Application)
-------------------------------

1. 集合覆盖与最大覆盖(set cover and maximum coverage)

NP-hard -> fast greedy approximation algorithm.

2. 边覆盖(edge cover)，顶点覆盖(vertex cover)

最少的点覆盖所有边，最少的边覆盖所有点。

3. 传感器放置(sensor placement)

最少传感器覆盖全部区域

k个传感器(k-子集)能够覆盖的最大区域

有约束条件下的传感器放置问题(例如墙壁的限制), sensor placement in buildings. 三种选择(规划): 使用较便宜但范围较小的额传感器，使用较贵但范围较大的传感器，将这两种传感器混合使用。

4. Graph Cut Problems

Minimum CUT: 找出一个点的子集S，使得 S 和 V-S 之间的边最少。
Maximum CUT: 找出一个点的子集S，使得 S 和 V-S 之间的边最多。

如果边/点有权重，如何考虑？

5. Facility/Plant Location (uncapacitated, 不限量的)

联想：线性规划、网络流问题，满足不同的要求。

6. Information Gain and Feature Seletion，信息获取和特征提取

选取尽量少的特征使得对未知量的预测(prediction)依然准确。

f(A) = I(Y;XA) = H(Y) − H(Y|XA) = H(XA) − H(XA|Y)

选取特征向量的一个k-子集使得对未知量的预测尽可能准确。

7. Monge Matrices

$ m \times n $ matrices $ C = [cij]_{ij} $ are called Monge matrices if they satisfy
the Monge property, namely(也就是说):
$$ c_{ij}+ c_{rs} \lt c_{is} + c_{rj} $$ 

for all $ 1 \le i \lt r \le m $ and $ 1 \le j \lt s \le n $.

四边形不等式(quadrangle inqeuality)，加速DP(dynamic programming problems)。

从一个凸包(凸多边形)中删边来构造 Monge Matrix。

8. A model of Influence in Social Networks

Given a graph G = (V,E), each v ∈ V corresponds to a person, to
each v we have an activation function fv: 2V→ [0,1] dependent
only on its neighbors.

找到社交网络中一个最小的子集，使得其能影响尽量多的人。

9. The value of a friend

基于一个子模函数来计算朋友在一个社交网络中的价值

10. Information Cascades 信息瀑布

找到最具影响力的信息源。

11. Diversity(多样性) Functions

Given a set V of of items, how do we choose a subset S ⊆ V that is as diverse as possible, with perhaps constraints on S such as its size.

How do we choose the smallest set S that maintains a given quality of diversity?

Goal of diversity: ensure proper representation in chosen set that, say otherwise in a random sample, would lead to poor representation of normally underrepresented groups.

差异意味着不同的观点(异常子网)。

Submodular Motivation Recap
----------------------------

Given a set of objects $ V = \\{ v1,\dots,vn \\} $ and a function $ f : 2V \to R $ that returns a real value for any subset $ S \subseteq V $.

Suppose we are interested in finding the subset that either maximizes or minimizes the function, e.g., $ argmax_{S \subseteq V}f(S) $, possibly subject to some constraints.

In general, this problem has exponential time complexity.

Example: f might correspond to the value (e.g., information gain) of a set of sensor locations in an environment, and we wish to find the best set S ⊆ V of sensors locations given a fixed upper limit on the number of sensors $|S|$.

In many cases (such as above) f has properties that make its optimization tractable to either exactly or approximately compute.

One such property is submodularity.

基础定义
--------

1. $ f(A) + f(B) \ge f(A \bigcup B) + f(A \bigcap B) $
2. $ f(A \bigcup {v}) − f(A) \ge f(B \bigcup {v}) − f(B) $
This means that the incremental “value”, “gain”, or “cost” of v
decreases (diminishes) as the context in which v is considered grows from
A to B.
3. $ f(A) + f(B)\ge f(A\bigcup B)$
This means that the “whole” is less than the sum of the parts.




