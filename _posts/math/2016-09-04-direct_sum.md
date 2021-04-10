---
title: 直和
author: Tao He
date: 2016-09-04
tag: [Algebra]
category: Math
layout: post
---

<!--more-->

$W_1$和$W_2$的和定义为

$$W_1 + W_2 = \{\alpha_1 + \alpha_2 | \alpha_1 \in W_1, \alpha_2 \in W_2\}$$

定义
----

内直和（或直和，internal direct sum）：设$W = W_1 + \dots + W_s$，$W_i$ 是线性空间V的子空间（$i = 1, \dots, s$），
$W$称为是$W_1, \dots, W_s$的内直和（或直和），记为$W=W_1 \oplus \dots \oplus W_s = \oplus_{i=1}^s W_i$，
如果每个$\alpha \in W$表为$W_1,\dots，W_s$中元素和的方法使唯一的，即由

$$\alpha = \alpha_1 + \dots + \alpha_s = \beta_1 + \dots + \beta_s (\alpha_i, \beta_i \in W_i, i = 1, \dots, s)$$

必有$\alpha_i = \beta_i (i = 1,\dots,s)$。

考虑无限个空间的内直和，若$V = \sum_{i=1}^{\infty}V_i = \sum_{i=1}^{\infty}\alpha_i, \alpha_i \in V_i)$，
如果$V$中的每个元素表为$\sum \alpha_i$的方法是唯一的，则$V$为$V_i$的直和，记为$\oplus_{i=1}^{\infty} V_i$。
$\sum V_i$为之和的充分必要条件是：

$$V_i \bigcap (\sum_{j \neq i}^{} V_j) = 0$$

并且

$$dim(V) = \sum_{i=1}^{\infty} dim(V_i)$$

外直和：设$V_1$和$V_2$是域$F$上的两个线性空间，令

$$V = \{(\alpha, \beta)|\alpha \in V_1, \beta \in V_2\}$$

且定义$\forall \alpha_i \in V_1, \beta_i \in V_2, \lambda \in F$

$$(\alpha_1, \beta_1) + (\alpha_2, \beta_2) = (\alpha_1 + \alpha_2, \beta_1 + \beta_2)$$

$$\lambda(\alpha, \beta) = (\lambda \alpha, \lambda \beta)$$

则$V$是$F$上线性空间，称为$V_1$和$V_2$的外直和（external direct sum）。并且有

$$dim(V) = dim(V_1) + dim(V_2)$$

区别与联系
----------

1. 对于外直和与内直和，$\oplus$运算都是可结合的。
2. 对于任何两个空间（维数可以不同）都可以求外直和，只有一个线性空间的两个子空间才能求内直和（直和）。
3. 任何两个子空间（维数相同）都可以求外直和与和，如果和恰好是直和，那么就称之为内直和。外直和与内
直和并不相等，但二者使同构的（isomorphic）。

