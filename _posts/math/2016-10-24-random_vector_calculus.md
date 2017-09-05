---
title: 随机向量和概率统计
author: sighingnow
date: 2016-10-24
tag: [Mathematical Analysis]
category: Math
layout: post
---

<!--more-->

随机向量的数值特征
------------------

设$x = (x_1, x_2, \dots, x_n)^T$，$y = (y_1, y_2, \dots, y_n)^T$，

+ $x$的期望$\mathbb{E}$定义为
  $$\mathbb{E}(x) = (\mathbb{E}(x_1), \mathbb{E}(x_2), \dots, \mathbb{E}(x_n))$$
+ $x$的方差$\mathbb{D}$定义为
  $$\mathbb{D}(x) = \mathbb{E}(xx^T)-\mathbb{E}(x)\mathbb{E}(x^T)$$
+ $x$和$y$协方差矩阵定义为
  $$cov(x, y) = \mathbb{E}(x-\mathbb{E}(x))\mathbb{E}(y-\mathbb{E}(y))^T$$
  协方差矩阵的第$i$行第$j$列的元素等于$cov(x_i, y_j)$。

设$x$为$n$维实值随机列向量，$A$为$n \times n$常数矩阵，记$\mu=\mathbb{E}(x)$，
$\Sigma=\mathbb{D}(x)$，那么
$$\mathbb{E}(x^TAx)=tr(A\Sigma)+\mu^TA\mu$$

多元正态分布
------------

设$\Sigma$为$p$阶正定实对称矩阵，记$X = (X_1, X_2, \dots, X_p)^T$为$p$维实值随机
列向量，$\mu$为$p$维实值列向量，表示$X$的均值向量，则$X$的联合概率密度函数
$$f(x) = f(x_1,x_2,\dots,x_p)
       = \frac{1}{\sqrt{2\pi}^p|\Sigma|^{1/2}}exp\{-\frac{1}{2}(x-\mu)^T\Sigma^{-1}(x-\mu)\}$$
记为$X \sim N_p(\mu, \Sigma)$。式中，$\Sigma$为协方差矩阵，若$\Sigma$为对角阵，则$X=(X_1, X_2,
\dots, X_n)^T$的各分量是相互独立的随机变量。

若$X \sim N_p(\mu, \Sigma)$，$X$的特征函数为
$$\varphi(t) = exp\{i\mu^Tt - \frac{1}{2}t^T\Sigma t\}$$
