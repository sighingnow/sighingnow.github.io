---
title: 数学分析题目集锦
author: Tao He
date: 2016-09-10
tag: [Mathematical Analysis]
category: Mathematics
layout: post
---

<!--more-->

Stolz定理的应用
---------------

1. 设$a_1 \in (0, \pi)$，$a_{n+1} = sin a_n (n \ge 1)$，证明$\lim_{n\to\infty} \sqrt{n}a_n = \sqrt{3}$。（题目来源：数学分析讲义，梅加强，习题5.8(10)，p192）

证明：根据题意不难得出$\lim_{n\to\infty} = 0$，考虑将$\sqrt{n}$从表达式中去掉，不妨先求解$\lim_{n\to\infty} n a_n^2$，使用Stolz定理，有

$$\begin{aligned} \lim_{n\to\infty} n a_n^2
    &= \lim_{n\to\infty} \frac{n}{1/a_n^2} \\
    &= \lim_{n\to\infty}\frac{1}{1/a_{n+1}^2 - 1/a_n^2} \\
    &= \lim_{n\to\infty}\frac{a_n^2 \cdot sin^2{a_n}}{a_n^2-sin^2{a_n}} \\
    &= \lim_{n\to\infty}\frac{a_n^4}{(a_n + a_n - \frac{1}{3!}a_n^3)(a_n - a_n + \frac{1}{3!}a_n^3)} \\
    &= 3
\end{aligned}$$

因此，$\lim_{n\to\infty} \sqrt{n}a_n = \sqrt{3}$。

凸函数的应用
------------

1. Hadamard不等式：设$f$为$[a,b]$上的连续凸函数，则

$$f(\frac{a+b}{2} \le \frac{1}{b-a}\int_{a}^{b}f(x)\,dx \le \frac{f(a)+f(b)}{2})$$

直观解释：将三个式子同时乘以$(b-a)$，考虑到$f$为凸函数，则左边为下梯形的面积，中间为函数的曲边梯形的面积，后边为
上梯形的面积，结论显然成立。

证明：考虑积分

$$\int_a^b f(x)\,dx = \int_a^{\frac{a+b}{2}} f(x)\,dx + \int_{\frac{a+b}{2}}^b f(x)\,dx$$

因为$\int_{\frac{a+b}{2}}^{b} f(x)\,dx = \int_{a}^{\frac{a+b}{2}} f(a+b-x)\,dx$，因此

$$\begin{aligned} \int_a^b f(x)\,dx
    &= \int_a^{\frac{a+b}{2}}[f(x)+f(a+b-x)]\,dx \\
    &\ge 2\int_{a}^{\frac{a+b}{2}} f(\frac{a+b}{2})\,dx \\
    &= (b-a)f(\frac{a+b}{2})
\end{aligned}$$

即有$f(\frac{a+b}{2}) \le \frac{1}{b-a}\int_a^b f(x)\,dx$。

令$x = b-(b-a)t, (0 \le t \le 1)$，有

$$\begin{aligned} \int_a^b f(x)\,dx
    &= (b-a)\int_0^1 f[ta + (1-t)b]\,dt \\
    &\le (b-a)\int_0^1 [tf(a) + (1-t)f(b)]\,dt \\
    &= (b-a)\frac{f(a)+f(b)}{2}
\end{aligned}$$

即有$\frac{1}{b-a}\int_a^b f(x)\,dx \le \frac{f(a)+f(b)}{2}$。

综上，HaHadamard不等式成立。

2. 设$f$为$(-\infty, +\infty)$中的连续函数，如何对任意$x \in R$，均有

$$\lim_{h \to 0}\frac{f(x+h)+f(x-h)-2f(x)}{h^2} = 0$$

证明$f(x)$是线性函数。

证明：对于$\forall \in R$，定义辅助函数

$$F(x) = f(x) - [f(a)+\frac{f(b)-f(a)}{b-a}(x-a)]+\epsilon(x-a)(x-b)$$

对于$F(x)$，有$F(a) = F(b) = 0$。定义

$$\begin{aligned} D^2 F(x)
    &= \lim_{h \to 0}\frac{F(x+h)+F(x-h)-2F(x)}{h^2} = 0 \\
    &= \lim_{h \to 0}\frac{f(x+h)+f(x-h)-2f(x)}{h^2} = 0 + 2\epsilon \\
    &= 2\epsilon
\end{aligned}$$

若$\epsilon > 0$，$F(x)$在区间$[a,b]$上的最大值必在端点处取得，否则如果$\exists x_0 \in (a,b)$使得
$F(x_0) = max\{F(x)\}$ 那么$D^2 F(x) < 0$，矛盾。因此，$\forall x \in [a,b]$，有$F(x) \le F(a) = 0$，
使$\epsilon \to 0$，取极限，得到

$$f(x) \le f(a) + \frac{f(b)-f(a)}{b-a}(x-a)$$

若$\epsilon < 0$，同理可得

$$f(x) \ge f(a) + \frac{f(b)-f(a)}{b-a}(x-a)$$

因此

$$f(x) = f(a) + \frac{f(b)-f(a)}{b-a}(x-a)$$

表明$f(x)$是线性函数。

扩展：考虑将题目的条件放宽为$f(x)$可导且可二阶导，那么，原式等价于

$$\begin{aligned} \lim_{h \to 0}\frac{f(x+h)+f(x-h)-2f(x)}{h^2} = 0
    &\implies \lim_{h \to 0}\frac{\frac{f(x+h)-f(x)}{h}-\frac{f(x)-f(x-h)}{h}}{h} = 0 \\
    &\implies \lim_{h \to 0}\frac{f'(x+h)-f'(x)}{h} = 0 \\
    &\implies \lim_{h \to 0}f''(x) = 0
\end{aligned}$$

由二阶导数为$0$可知一阶导数为定值，因此$f(x)$是线性函数。



