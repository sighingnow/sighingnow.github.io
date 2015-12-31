---
title: Tak函数和Tarai函数的性能
author: He Tao
date: 2015-12-31
tag: [Haskell]
category: 编程语言
layout: post
---

Tak函数和Tarai函数是两个非常类似，但本质上差异显著的函数，具体的定义参考[Wikipedia
的页面][1]。

+ Tak 函数

It is defined as follows:

$$
\tau (x,y,z) = {
    \begin{cases}
        \tau (\tau (x-1,y,z), \tau (y-1,z,x), \tau (z-1,x,y))
            & {\text{if }}y < x \\
        z
            & {\text{otherwise}}
    \end{cases}}
$$

<!--more-->



<!--links-->

[1]. https://en.wikipedia.org/wiki/Tak_(function)

