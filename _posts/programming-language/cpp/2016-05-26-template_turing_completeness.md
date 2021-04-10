---
title: C++模板的图灵完备
author: Tao He
date: 2016-05-26
tags: C/C++
categories: 编程语言
layout: post
---

一门编程语言图灵完备，指的是能够计算任何一个图灵可计算的函数，或者说，可以用来模拟通用图灵机。

<!--more-->

C++模板的图灵完备
----------------

一个统领及可以由一个四元组表示：$(K, \Sigma, \delta, s)$，其中：

+ $K$ 表示有限的状态集合。
+ $\Sigma$ 表示字母表。
+ $\delta$ 表示状态转移函数，$K \times \Sigma \to (K \cup \{h\}) \times (\Sigma \cup \{\Rightarrow, \Leftarrow\})$，其中，$h$表示停机状态。
+ $s$ 表示起始状态，$s \in K$。

### 字典和动作

首先考虑字典（动作）和状态集合。状态包括`Q0`、$Q1$和停机状态，字典包括`Left`、`Right`，表示向左或者向右移动一个单元，`A`表示将
当前格的内容置为`A`，`Blank`表示将当前格的内容置为`Blank`。

~~~cpp
// States
struct Halt {};
struct Q0 {};
struct Q1 {};

// Alphabet
struct Left {};
struct Right {};
struct A {};
struct Blank {};
~~~

### 编码纸带

首先编码纸带，使用函数式列表，纸带由三部分组成(Zipper)：当前位置、左边部分、右边部分。

~~~cpp
// Functional list.
struct Nil {};
template <typename Hd, typename Tl>
struct Pair {
    using head = Hd;
    using tail = Tl;
};
~~~

### 编码图灵机实例

一个图灵机实例包括当前的状态、指针在纸带上的位置，以及纸带的内容。可以表达为：

~~~cpp
// Execute action at current position.
template<typename S,                                       // state.
         typename L, typename V, typename R,               // tape.
         template<typename Q, typename Sigma> class Delta> // transition function.
struct Instance {
    using next = typename Delta<S, V>::next;
    using act = typename Delta<S, V>::act;
    using next_instance = typename Move<next, act, L, V, R, Delta>::next_instance;
};
~~~

### 执行动作

图灵机每次在当前执行完一个动作之后，都按照状态转移函数转移到下一个状态，得到下一个状态机实例，可能执行的动作包括：

+ 在当前位置执行给定的Action。

~~~cpp
// Default: Write V sell with give value.
template <typename Next, typename Act, typename L, typename V, typename R,
          template <typename Q, typename Sigma> class Delta>
struct Move {
    using next_instance = typename Instance<Next, L, Act, R, Delta>::next_instance;
};
~~~

+ 向左移动一个单元。

~~~cpp
// Move left.
template <typename Next, typename L, typename V, typename R,
          template <typename Q, typename Sigma> class Delta>
struct Move<Next, Left, L, V, R, Delta> {
    using next_instance = typename Instance<Next, typename L::tail, typename L::head, Pair<V, R>, Delta>::next_instance;
};
~~~

+ 向右移动一个单元。

~~~cpp
// Move right.
template <typename Next, typename L, typename V, typename R,
         template <typename Q, typename Sigma> class Delta>
struct Move<Next, Right, L, V, R, Delta> {
    using next_instance = typename Instance<Next, Pair<V, L>, typename R::head, typename R::tail, Delta>::next_instance;
};
~~~

+ 已经到达最左边的单元且仍然向左移动，向左边扩展一个单元，并进入停机状态。

~~~cpp
// Generate a new blank cell at left and halt.
template <typename Next, typename V, typename R, template <typename Q, typename Sigma> class Delta>
struct Move<Next, Left, Nil, V, R, Delta> {
    using next_instance = typename Instance<Next, Nil, Blank, Pair<V, R>, Delta>::next_instance;
};
~~~

+ 已经到达最右边的单元且仍然向右移动，向右边扩展一个单元，并进入停机状态。

~~~cpp
// Generate a new blank cell at right and halt.
template <typename Next, typename L, typename V, template <typename Q, typename Sigma> class Delta>
struct Move<Next, Right, L, V, Nil, Delta> {
    using next_instance = typename Instance<Next, Pair<V, L>, Blank, Nil, Delta>::next_instance;
};
~~~

### 停机状态

当图灵机到达停机状态时，不再有下一步动作。

~~~cpp
// Halt S.
template <typename Act, typename L, typename V, typename R, template<typename Q, typename Sigma> class Delta>
struct Move<Halt, Act, L, V, R, Delta> {};
~~~

### 状态转移函数

这里图灵机的功能是从起始状态出发，把右侧所有的`A`都置为`Blank`，具体的状态转移函数编码如下：

~~~cpp
// Transition function.
template <typename S, typename Sigma>
struct Transition {};

template <>
struct Transition<Q0, A> {
    using next = Q1;
    using act = Blank;
};

template <>
struct Transition<Q0, Blank> {
    using next = Halt;
    using act = Blank;
};

template <>
struct Transition<Q1, A> {
    using next = Q0;
    using act = A;
};

template <>
struct Transition<Q1, Blank> {
    using next = Q0;
    using act = Right;
};
~~~

### 运行

设置好初始状态，通过模板实例化的过程来执行状态机：

~~~cpp
using Initial = typename Instance<Q0, Nil, A, Pair<A, Pair<A, Nil>>, Transition>::next_instance;
~~~

错误信息显示出图灵机按照规则运行的轨迹：

~~~cpp
turing-complete.cxx:94:36:  'Instance<Q0, Pair<Blank, Pair<Blank, Pair<Blank, Nil> > >, Blank, Nil, Transition>' requested here
turing-complete.cxx:68:35:  'Instance<Q1, Pair<Blank, Pair<Blank, Nil> >, Blank, Nil, Transition>' requested here
turing-complete.cxx:82:35:  'Instance<Q0, Pair<Blank, Pair<Blank, Nil> >, A, Nil, Transition>' requested here
turing-complete.cxx:82:35:  'Instance<Q0, Pair<Blank, Nil>, A, Pair<A, Nil>, Transition>' requested here
turing-complete.cxx:68:35:  'Instance<Q1, Nil, Blank, Pair<A, Pair<A, Nil> >, Transition>' requested here
turing-complete.cxx:103:26: 'Instance<Q0, Nil, A, Pair<A, Pair<A, Nil> >, Transition>' requested here

    using Initial = typename Instance<Q0, Nil, A, Pair<A, Pair<A, Nil> >, Transition>::next_instance;
                         ^
1 error generated.
~~~

C++模板的图灵完备与编译器
-----------------------

图灵停机问题是不可判定问题，这也就意味着可以构造出时编译器不停机的C++程序，为此，C++标准中队模板深度做了限定，以保证对模板的实
例化和编译过程能够终止。ISO/IEC 14882的14.7.1节给出了一个可以导致模板实例化过程中出现无穷递归的例子：

~~~cpp
template <typename T> class X {
    X<T *> p;
};
~~~

g++ 5.4.0对模板递归深度的默认限制是900，clang++ 3.8.0的默认限制是256。可以使用参数`-ftemplate-depth=N`来自定义模板嵌套深度。
Todd L. Veldhuizen的论文[^1]中给出了一个例子，这个例子在编译过程中不会突破编译器的递归深度的限制，但是却会生成大量($k^{17}$)的模板实例。

~~~cpp
// Example for k = 5.
template <int Depth, int A, typename B>
struct K17 {
    static const int x = K17<Depth+1, 0, K17<Depth, A, B>>::x
                       + K17<Depth+1, 1, K17<Depth, A, B>>::x
                       + K17<Depth+1, 2, K17<Depth, A, B>>::x
                       + K17<Depth+1, 3, K17<Depth, A, B>>::x
                       + K17<Depth+1, 4, K17<Depth, A, B>>::x;
};

template <int A, typename B>
struct K17<16, A, B> {
    static const int x = 1;
};

static const int z = K17<0, 0, int>::x;
~~~

constexpr与图灵完备
------------------

C++ 11引入的constexpr极大地减小了元编程的难度，使得编译器计算的程序的编写变得更加容易，但constexpr同样图灵完备。clang在其单元测试中给
出了一个例子[constexpr-turing.cpp](https://github.com/llvm-mirror/clang/blob/master/test/SemaCXX/constexpr-turing.cpp)。

C/C++宏与图灵完备
---------------

在Hacker News看到过一篇帖子[^2]，讲的是拿C/C++的宏实现一个一个BrainFuck的解释器，但这并不能表明C Preprocessor Language的图灵完备，
因为无法用宏去编码无穷递归，图灵机可以用来表达一个不可停机的问题，意味着计算本身需要无限长的纸带或者无限的计算步骤。C99和C11都不允许
宏的预处理出现递归，显然，使用宏实现的BrainFuck的解释器所能够运行的程序一定具有有限上届，这与经典的BlooP and Floop的定义并不等价。
对于一个图灵完备的编程语言，其能够表达的程序一定是不可判定是否停机的。也正是同样的原因，一些Total的Functional Programming
Languages，比如Coq，同样不能满足图灵完备。

References
----------

[^1]: [C++ Templates are Turing Complete](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.14.3670), Todd L. Veldhuizen.
[^2]: [Brainfuck interpreter written in the C preprocessor](https://news.ycombinator.com/item?id=4795542).

