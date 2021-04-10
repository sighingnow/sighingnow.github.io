---
title: Concept in C++
author: Tao He
date: 2016-04-09
tags: C/C++
categories: 编程语言
layout: post
---

> Concept is a term that describes a named set of requirements for a type.

<!--more-->

Motivation of Concept
-----------------------

C++的Concept主要是为了对模板参数进行约束，来解决模板参数出错是出错信息太长的问题。引入Concept的约束可以很好地
改善这一状况。另一方面，一个C++模板类中，没有用到的函数是不予编译的，也就无法检测出可能存在的bug，使用Concept
可以对模板的类型参数设置一些必要的约束。

然而Concept最终还是不能进入C++17。

requires
--------

首先实现`requires`，是的当类型参数不满足要求时能够阻止模板继续编译。一个可行的思路是使用`std::enable_if`：

~~~cpp
template <bool Pred>
using Requires = typename std::enable_if<Pred>::type;
~~~

用法：

~~~cpp
template <typename A, typename B, typename = Requires<std::is_convertible<A, B>::value>>
bool is_equal(A const & a, B const & b) {
    return a == b;
}

bool fn() {
    std::string s;
    return is_equal(s, 1);
}
~~~

使用`Requires`，GCC 的报错信息为(gcc 6.0 expreimental, `g++ -c -std=c++11`)：

~~~cpp
a.cxx: In function 'bool fn()':
a.cxx:33:25: error: no matching function for call to 'is_equal(std::__cxx11::string&, int)'
     return is_equal(s, 1);
                         ^
a.cxx:27:6: note: candidate: template<class A, class B, class> bool is_equal(const A&, const B&)
     bool is_equal(A const & a, B const & b) {
          ^~~~~~~~
a.cxx:27:6: note:   template argument deduction/substitution failed:
~~~

而如果不使用`Requires`，GCC 产生了超过300行的错误信息，非常不利于定位和修复错误。

Concept
-------

而Concept的实现可以大致分为这样几个类型。

### std::is_xx

`<type_traits>`头文件中提供了一系列类似于`std::is_xxx`的函数，可以使用这些函数来判断一个类型是否满足某些性质，
使之成为一个Concept。举例：

~~~cpp
/**
 * Specifies that an object of the type can be constructed from rvalue.
 */
template <typename T>
struct MoveConstructible {
    static constexpr bool value = std::is_move_constructible<T>::value;
    constexpr operator bool() const noexcept { return value; }
};
~~~

### SFINAE

SFINAE 即 "Substitution Failure Is Not An Error"，指的是当模板类型参数替换发生错误时，该模板特化定义会被忽略
而不是被认为错误。这一特性可以用来判断某个类型是否具有制定的函数。例如`EuqalityComparable`这一Concept的实现：

~~~cpp
/**
 * Operator== is an equivalence relation.
 */
namespace _inner_impl {
// Check `==` using SFINAE.
template <typename A, typename B>
auto test_eq(decltype(std::declval<A>() == std::declval<B>()) *)
        -> decltype(std::declval<A>() == std::declval<B>());
template <typename A, typename B>
auto test_eq(...) -> void;
};

template <typename A, typename B>
struct EuqalityComparable {
    static constexpr bool value =
            std::is_convertible<decltype(_inner_impl::test_eq<A, B>(nullptr)),
                                bool>::value;
    constexpr operator bool() const noexcept { return value; }
};
~~~

首先使用SFINAE，根据模板替换的优先级，第一个`test_eq`的参数更加特化，会被优先考虑。如果`A`类型的值和`B`类型的
值之间不能使用`==`来作比较，模板参数替换就会失败，而第二个`test_eq`的参数类型为`(...)`，会无条件匹配成功。如果
`A`、`B`两个类型的值之间能够进行`==`比较，`test_eq`的返回值类型将会是`bool`，如果不能，返回值类型将会是`void`。
根据这一点区别，就可以得到`EqualityComparable`这个Concept。以此类推，要想判断一个类是否具有某个指定的成员函数、
成员变量，以及某个函数是否可以作用在指定类型的值上，都可以通过SFINAE来实现。

### Iterator

对于Iterator的约束，可以使用`iteator_tag`来实现。例如`RandomAccessIterator`：

~~~cpp
template <typename Iterator>
struct RandomAccessIterator {
    static constexpr bool value = std::is_same<
            std::random_access_iterator_tag,
            typename std::iterator_traits<Iterator>::iterator_category>::value;
    constexpr operator bool() const noexcept { return value; }
};
~~~

### Template Instance

对于模板类型和模板实例类型，可以判断二者之间是否存在关系，例如`std::vector`是`std::vector<int>`的模板。

~~~cpp
/**
 * Check if a type is the base template of another parametrised type.
 *
 * e.g.:
 *      BaseTemplate<std::vector<int>, std::vector> = true
 *      BaseTemplate<std::vector<int>, std::map> = false
 */
template <typename T, template <typename...> class U>
struct BaseTemplate {
    static constexpr bool value = false;
};

template <template <typename...> class U, typename... Ts>
struct BaseTemplate<U<Ts...>, U> {
    static constexpr bool value = true;
};
~~~

此外还可以判断两个类型实例类型对应的模板是否相同，例如`std::vector<int>`和`std::vector<string>`对应着相同的模板
`std::vector`，而它们与`std::list<int>`的模板不同。

~~~cpp
template <typename T, typename U>
struct SameTemplate;

template <template <typename...> class T, template <typename...> class U,
          typename... Ts,
          typename... Us  // Here, `Us` is just a placeholder, indicates that
                          // `U` is a parameterised
                          // tempalte.
          >
struct SameTemplate<T<Ts...>, U<Us...>> {
    static constexpr bool value = std::is_same<T<Ts...>, U<Ts...>>::value;
};
~~~

完整实现
--------

[type_concepts.hpp](https://github.com/sighingnow/algebra.h/blob/master/include/algebra/basic/type_concepts.hpp)
中包含一个对[Library Concepts](http://en.cppreference.com/w/cpp/concept)中定义的Concepts的一个完整实现。


参考
----

1. C++ 中的Concepts列表：[Library Concepts](http://en.cppreference.com/w/cpp/concept)

