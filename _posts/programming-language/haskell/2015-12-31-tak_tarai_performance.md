---
title: Tak函数和Tarai函数的性能
author: Tao He
date: 2015-12-31
tag: [Haskell]
category: 编程语言
layout: post
---

Tak函数和Tarai函数是两个非常类似，但本质上差异显著的函数，具体的定义参考[Wikipedia
的页面](https://en.wikipedia.org/wiki/Tak_(function))。

+ Tak 函数

It is defined as follows:

$$\tau (x,y,z) = {\begin{cases}
\tau (\tau (x-1,y,z), \tau (y-1,z,x), \tau (z-1,x,y)) & {\text{if }}y < x \\
z                                                     & {\text{otherwise}}
\end{cases}}$$

John McMarchy 的论文 [_An Interesting Lisp Function_](http://dl.acm.org/citation.cfm?id=1411833) 中论述了以下两个 tak 函数的性质。

$$tak(x+a, y+a, z+a) = tak(x, y, z) + a$$

以及

$$tak(x, y, z) = \text{ if } x \le y \text{ then } y \text{ else if } y \le z \text{ then } z \text{ else } x$$

+ Tarai 函数

$$\tau (x,y,z) = {\begin{cases}
\tau (\tau (x-1,y,z), \tau (y-1,z,x), \tau (z-1,x,y)) & {\text{if }}y < x \\
y                                                     & {\text{otherwise}}
\end{cases}}$$

<!--more-->

性能
----

这两个函数非常相似，表达式中唯一的区别在于 $\text{otherwise}$ 条件下函数的值，这一区别
导致了这两个函数在执行性能上的巨大差异。tak 函数经常被作为测试对递归的优化的基准(It can be made to run a
long time without generating large number or using much stack)，而
tarai 函数很容易通过记忆化或者 Lazy Evaluation 的手段进行优化。

分别使用 C 语言和 Haskell 实现这两个函数：

~~~cpp
int tak(int x, int y, int z) {
    if (x <= y) {
        return z;
    }
    else {
        return tak(tak(x-1, y, z), tak(y-1, z, x), tak(z-1, x, y));
    }
}
~~~

~~~cpp
int tarai(int x, int y, int z) {
    if (x <= y) {
        return y;
    }
    else {
        return tak(tak(x-1, y, z), tak(y-1, z, x), tak(z-1, x, y));
    }
}
~~~

对应的 Haskell 版本:

~~~haskell
tak :: Int -> Int -> Int -> Int
tak x y z
    | y < x     = tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)
    | otherwise = z
~~~

~~~haskell
tarai :: Int -> Int -> Int -> Int
tarai x y z
    | y < x     = tarai (tarai (x-1) y z) (tarai (y-1) z x) (tarai (z-1) x y)
    | otherwise = y
~~~

这几个函数都没有任何实现上的优化，但由于函数本身的特性以及 C 语言和 Haskell 的
求值策略的不同，其执行性能存在显著差异：

+ 初始参数：

~~~
    x = 22, y = 12, z = 1
~~~

+ 编译选项

~~~
    ghc xxx.hs       (GHC 7.10.2)
    gcc xxx.cpp      (MinGW-w64 gcc 5.2.0)
~~~

+ 执行时间

|              | C 语言     |  Haskell  |
|:------------:|:----------:|:---------:|
| tak          | 1.083s     | 9.886s    |
| tarai        | 1.083s     | 0.049s    |

从表中可以看出，Haskell 编写的 tarai 函数在执行时间上具有很大的优势，但 tak 函
数却比 C 语言版本慢了很多。

接下来，我们通过模拟延迟求值的方式来优化 C 语言的实现：

~~~cpp
int tarai(int x, int y, int z)
{
    while (x > y) {
        int oldx = x, oldy = y;
        x = tarai(x - 1, y, z);
        y = tarai(y - 1, z, oldx);
        if (x <= y) break;
        z = tarai(z - 1, oldx, oldy);
    }
    return y;
}
~~~

同样的初始参数，函数的执行时间为：

    0.027s

优化的核心在于那行

    if (x <= y) break;

正是因为有这个判断，所以可以避免执行最后一个无用的递归调用。在这个判断之前，`y`
的值已经算出来了，而`z`的确没有。`tak`函数以`z`作为返回值，而`tarai`以`y`作为
函数的返回值。因此，`tarai`函数可以采用这样的优化，而`tak`不行。Haskell 默认
惰性求值，很自然而然地就舍去了`tarai`函数中多余的递归运算。

思考
----

Haskell 作为一门抽象程序极高，表达能力极强的编程语言，在带来很多不少便利的同时也必然会引起性能上的损失。在 `tak` 函数的表现上，C 语言的速度
是 Haskell 的十倍，但这并不意味着 Haskell 不适用于生产环境，不意味着 Haskell 不适合用于真实的软件开发。GHC 的惰性求值是通过在堆上构造 thunk 来
实现的，堆空间的操作效率肯定比不上栈空间，在运行过程中，程序还可能会一次又一次地检查一个 thunk 是否已经求值，与 C 语言程序相比，Haskell 的程序中有太
多的跳转，这非常不利于 CPU 的分支预测，还有导致更多的 Cache Miss。

而对于上文中`tak`函数 Haskell 的实现同样可以做很多优化。通过使用`ST Monad`等手段可以显著提升这段程序的运行效率，但代价是失去了 Haskell 本身的美感。
对于一个默认严格求值的语言如C++或者Ocaml，很容易通过库来实现 Lazy Evaluation，Lazy 的核心无非是 STG-Machine, Graph reduce 以及 chunk。但是，
在 Haskell 这种默认惰性求值的语言中实现 Strict 确实非常难的事情，`BangPattern`以及Strict标记(`!`)的使用对程序的可读性和可维护行没有什么好处，但
这是在Haskell里使用Strict Evaluation的唯一途径。不过好在 GHC 8.0 将会加入`Strict`和`StrictData`扩展，可以将整个模块直接设为 Strict Evaluation,
能够将程序从散落各处的`!`标记中解救出来。
