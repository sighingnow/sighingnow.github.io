---
title: 矩母函数和特征函数
author: sighingnow
date: 2016-10-17
tag: [Mathematical Analysis]
category: Math
layout: post
---

概率论中，矩母函数（Moment-generating Function）和特征函数（Characteristic Function）是定义
概率分布函数的另一种形式。

<!--more-->

矩母函数
--------

随机变量$X$的矩母函数定义为
$$M_x(t) := \mathbb{E}(e^{tX})$$

矩母函数并不总是存在（随机变量的中心矩也不一定存在）。如果矩母函数在$t=0$的邻域存在，$X$的各
阶原点矩矩的值等于矩母函数在 $t=0$ 处的各阶导数值，
$$m_n = \mathbb{E}(X^n) = M_X^{(n)}(0) = \frac{d^n{M_X}}{d{t^n}}|_{t=0}$$

矩母函数具有与母函数类似的生成函数的性质，考虑$e^{tX}$的级数，
$$e^{tX} = 1 + tX + \frac{t^2X^2}{2!} + \dots + \frac{t^nX^n}{n!} + \dots$$

因此，
$$\begin{aligned}
M_X(t) = \mathbb{E}(e^{tX}) &= 1 + t\mathbb{E}(X) + \frac{t^2\mathbb{E}(X^2)}{2!} + \dots + \frac{t^n\mathbb{E}(X^n)}{n!} + \dots \\
                            &= 1 + tm_1+\frac{t^2m_2}{2!}+\dots+\frac{t^nm_n}{n!} + \dots
\end{aligned}$$

对于离散随机变量分布，矩母函数定义为
$$M_X(t) = \sum_{i=1}^{\infty} e^{tx_i}p_i$$
对于连续随机变量分布，矩母函数定义为
$$M_X(t) = \int_{\mathbb{R}} e^{tX}\,d{F(x)} = \int_{\mathbb{R}} e^{tX}f(x)\,dx$$

对于独立随机变量分布的线性组合，记$S_n = \sum a_i X_i$，则矩母函数表达为
$$M_{S_n}(t) = \prod M_{x_i}(a_i t)$$

两个随便变量分布具有相同的矩母函数，当且仅当两个随机变量分布的分布函数几乎处处相同。

特征函数
--------

随机变量$X$的特征函数定义为
$$ \varphi(t) := \mathbb{E}(e^{itX}) = \int_{D} e^{itX}f(x)\,dx $$

特征函数能够唯一确定随机变量的概率分布，如果随机变量的概率密度函数$f(x)$存在，特征函数相当于
$f(x)$的傅里叶变换。

如果随机变量分布的矩母函数存在，那么矩母函数和特征函数之间存在关系
$$\varphi_X(-it) = M_X(t)$$

随机变量分布的矩与特征函数的关系：
$$\mathbb{E}(X^n) = i^{-n}\varphi_X^{(n)}(0) = i^{-n}\frac{d^n\varphi_X(t)}{d{t^n}}|_{t=0}$$

对于相互独立的随机变量分布的线性组合，特征函数具有如下性质：

1. 记$Y = aX + b$
   $$\varphi_Y(t) = e^{ibt}\varphi_X(at)$$
2. 记$S_n = \sum X_i$，其中$X_i$相互独立的一个充要条件是
   $$\varphi_{S_n}(t) = \prod \varphi_{X_i}(t)$$

特征函数可以通过对概率密度函数做傅里叶变换得到，特征函数和概率密度函数之间存在关系：
$$f_X(x) = F_X' = \frac{1}{2\pi}\int_{\mathbb{R}}e^{-itx}\varphi_X(t)\,dt$$