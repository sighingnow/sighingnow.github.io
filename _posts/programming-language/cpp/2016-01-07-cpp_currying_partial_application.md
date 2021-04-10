---
title: C++ 实现 Currying 和 Partial application
author: Tao He
date: 2016-01-07
tags: C/C++
categories: 编程语言
layout: post
---

Curry 和 Partical application 是高阶函数的一个特点，在函数式编程中有着重要的应用。
借助 C++11 的变长参数模板(Variadic template)，可以在 C++ 中实现 Currying 和
Partial application。

<!--more-->

Currying and partial application
--------------------------------

Currying 和 Partial application 都是高阶函数的体现，**但这二者是不同的概念**！
**Currying 是将一个有 _n_ 个参数的函数转换成 _n_ 个只有 _1_ 个参数的函数。** 在
Lambda calculus 中，函数只能有一个参数。而 Parial application 则是接收函数的部分
参数并返回一个新的函数。

Currying 和 Parial application 的区别就在于 Currying 每次只接受一个参数，而 Parial
application 可以接受多个参数。

对于

    f :: x -> y -> z -> res

`f x y` 值 Parial application 了一次，而从 Currying 的角度理解，则是 Curry 了两次。

根据Wikipedia的定义：

+ Currying

> Currying is the technique of translating the evaluation of a function that
> takes multiple arguments (or a tuple of arguments) into evaluating a sequence
> of functions, each with a single argument.

+ Parial application

> Partial application (or partial function application) refers to the process
> of fixing a number of arguments to a function, producing another function of
> smaller arity.

C++ 实现
--------

+ Parial application 的实现

借助 C++ 的 Lambda expression, 非常容易实现 Partial application。

~~~cpp
template<typename F, typename... Args>
auto partial(F f, Args... args) {
    return [=](auto... rest) -> auto {
        return f(args..., rest...);
    };
}
~~~

使用 `partial` 对函数做 Parial application:

~~~cpp
void demo(int a, int & b, int c, int & d) {
    b = b + 1;
    d = d + 1;
}

int main() {
    int a = 11, b = 111, c = 1111, d = 11111;
    std::cout << "a: " << a << " b: " << b << " c: " << c << " d: " << d << std::endl;

    partial(demo, a, std::ref(b))(c, std::ref(d));

    std::cout << "a: " << a << " b: " << b << " c: " << c << " d: " << d << std::endl;

    return 0;
}
~~~

关于例子中的 `std::ref` 的使用，是因为**引用传参**的需要。注意到`partial`函数是一个
模板函数，如果不用`std::ref`将参数转化为引用类型`std::reference_wrapper`，那么模板
做类型推导时会认为参数是值类型，尽管`demo`函数的参数`b`的类型是`int &`，但是，这个引用
是对参数拷贝之后的值(`args`中的值)的引用，而不是对`main`函数中的`b`的引用。这个地方，
如果不使用`std::ref`，错误信息会很诡异：

    error: binding value of type 'const int' to reference to type 'int' drops 'const' qualifier.

同理，我们也无法在`partial`返回的 Lambda expression 中将参数声明为引用类型，
因此，参数`d`也会被认为是值类型，尽管`demo`函数的参数`d`的类型是`int &`，
但是这个引用是对参数拷贝之后的值(`rest`中的值)的引用，而不是对`main`函数中
的变量`d`的引用。

使用`std::forward`进一步优化`partial`函数，以减少运行过程中对参数的复制行为：

~~~cpp
template<typename F, typename Arg>
auto partial(F && f, Arg && arg) {
    return [f=std::forward<F>(f), arg=std::forward<Arg>(arg)]
            (auto &&... rest) {
        return f(arg, decltype(rest)(rest)...);
    };
}

template<typename F, typename Arg, typename... Args>
auto partial(F && f, Arg && arg, Args &&... args) {
    return partial(partial(std::forward<F>(f),
            std::forward<Arg>(arg)), std::forward<Args>(args)...);
}
~~~

+ Currying 的实现

与 Parial application 相比，Currying 的实现要更加复杂一些。首先考虑简单的情形：将
`std::function`类型的函数科里化：

~~~cpp
template<typename Return>
auto curry_impl(std::function<Return()> & f) {
    return std::forward<decltype(f)>(f);
}

template<typename Return, typename Arg>
auto curry_impl(std::function<Return(Arg)> & f) {
    return std::forward<decltype(f)>(f);
}

template<typename Return, typename Arg, typename... Args>
auto curry_impl(std::function<Return(Arg, Args...)> & f) {
    return [f=std::forward<decltype(f)>(f)](Arg arg) {
        std::function<Return(Args...)> rest = [&f, &arg](Args... args) -> Return {
            return f(arg, args...);
        };
        return curry_impl(rest);
    };
}
~~~

使用示例：

~~~cpp
int main() {

    std::function<int(int &, int, int &, int)> demo = [](int & a, int b, int & c, int d) {
        a += 1;
        c += 1;
        return (a+b+c+d);
    };

    int a = 11, b = 111, c = 1111, d = 11111;

    std::cout << "a: " << a << " b: " << b << " c: " << c << " d: " << d << std::endl;

    auto curried = curry(demo);
    auto partial_demo = curried(std::ref(a))(b);
    auto res = partial_demo(std::ref(c))(d);

    std::cout << "a: " << a << " b: " << b << " c: " << c << " d: " << d << std::endl;

    return 0;
}
~~~

接下来，需要实现一个模板，将函数指针、成员函数、Lambda expression、仿函数(functor) 等
都转换成一个`std::function`类型的函数，然后就可以方便地进行科里化了。

~~~cpp
// for lambda expression, and functor object, after capturing,
// forward lambda expression to
//      `function_traits<Return (Class::*)(Args...) const>`,
// forward functor object to
//      `struct function_traits<Return (Class::*)(Args...)>`
template <typename Functor>
struct function_traits
        : public function_traits<decltype(&Functor::operator())> {};

// for std::function, ordinary functions and static member functions.
template <typename Return, typename... Args>
struct function_traits<Return(Args...)> {
    using func_type = std::function<Return(Args...)>;
};

// for function pointer. e.g, &f.
template <typename Return, typename... Args>
struct function_traits<Return (*)(Args...)> {
    using func_type = std::function<Return(Args...)>;
};

// for capturing functor object (callable struct, struct with overloaded `()`
// operator).
template <typename Class, typename Return, typename... Args>
struct function_traits<Return (Class::*)(Args...)> {
    using func_type = std::function<Return(Args...)>;
};

// for capturing lambda expression.
template <typename Class, typename Return, typename... Args>
struct function_traits<Return (Class::*)(Args...) const> {
    using func_type = std::function<Return(Args...)>;
};
~~~

有了`function_traits`，我们在进行科里化之前，只需要进行一次类型转换即可(转换成
`function_traits<T>::func_type`类型。使用示例：

~~~cpp
template<typename F>
auto curry(F const & f) {
    typename function_traits<F>::func_type _f = f;
    return curry_impl(_f);
}

int f1(int a, int & b, int c, int & d) {
    b = b + 1;
    d = d + 1;
    return (a+b+c+d);
}

int main() {

    std::function<int(int &, int &)> f2 = [](int & m, int & k) {
        k = k + 1;
        m = m + 1;
        return k + 100;
    };
    auto f3 = [](int a, int & b, int c, int & d) -> auto {
        b = b + 1;
        d = d + 1;
    };

    int a = 11, b = 111, c = 1111, d = 11111;

    std::cout << "a: " << a << " b: " << b << " c: " << c << " d: " << d << std::endl;

    curry(f1)(a)(std::ref(b))(c)(std::ref(d));
    curry(f2)(std::ref(a))(std::ref(c));
    curry(f3)(a)(std::ref(b))(c)(std::ref(d));

    std::cout << "a: " << a << " b: " << b << " c: " << c << " d: " << d << std::endl;

    return 0;
}
~~~
