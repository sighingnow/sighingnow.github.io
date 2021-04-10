---
title: static_assert
author: Tao He
date: 2015-10-02
tags: C/C++
categories: 编程语言
layout: post
---

`assert` 是 C/C++ 中的运行时断言机制，但是，如果包含断言的函数不被调用，就无法触发该断言，
实际上，编译期的断言也非常重要，例如在模板实例化时的编译期断言很有利于提高程序的质量。Boost
库中内置了 `BOOST_STATIC_ASSERT` 断言机制，C++0x/C++11标准中也加入了 `static_assert`
断言。不借助 Boost 库和 C++0x/C++11，如何实现一个编译期断言？

+ 利用 `switch...case` 语句中两个 `case expression` 不能相等的机制

~~~cpp
#define assert_static(e) \
    do { \
        switch (e) case 0: case(e): ; \
    } while (0)
~~~

使用 `do {} while (0)` 可以使得 `assert_static` 宏在使用的时候更像是函数调用。这一技巧在
Linux 内核中的队列实现部分也有很多非常精彩的运用。

+ 利用数组长度不能为负数的机制

~~~cpp
#define assert_static(e) \
    do { \
        int __assert_static[(e) ? 1 : (-1)]; \
    } while (0)
~~~

这么使用 `do {} while (0)` 可以将数组定义包裹在一个新的作用域中，不至于引起和程序中已经定义
的变量的冲突。

值得一提的是，_深入理解 C++11 新特性解析与应用_ 一书中第30页使用了 “‘除0’会导致编译器报错”
来实现 `assert_static` 的方法是不正确的。在 C++ 中，“除0” 是未定义行为，在 C++11 标准
(ISO/IEC14882) Section 5.6 中，明确指出

> If the second operand of / or % is zero the behavior is undefined.

至于为什么要这么做，C++ 之父 **Bjarne Stroustrup** 的 _The Design and Evolution of C++_ (Addison Wesley, 1994) 一书中提到

> low-level events, such as arithmetic overflows and divide by zero, are
> assumed to be handled by a dedicated lower-level mechanism rather than by
> exceptions. This enables C++ to match the behaviour of other languages when
> it comes to arithmetic. It also avoids the problems that occur on heavily
> pipelined architectures where events such as divide by zero are asynchronous.



