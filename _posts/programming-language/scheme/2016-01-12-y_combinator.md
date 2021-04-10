---
title: Y combinator
author: Tao He
date: 2016-01-12
tags: [Scheme]
categories: 编程语言
layout: post
---

不动点(Fixed-point)指的是 $f(x)$ 的定义域内的一个点 $c$ 是函数 $f(x)$ 的不动点，当且仅当 $c$ 满足：

$$f(c) = c$$

Fixed-point Combinator 指的是一个高阶函数 $y$ 满足

$$\forall f: y\ f = f\ (y\ f)$$

也就是说，如果令 $x = y\ f$ 那么

$$x = f\ x$$

Fixed-point Combinator 是一个用于求函数的不动点的高阶函数，接受一个函数作为参数，并且返回一个函数。
$y\ f$ 表示函数 $f$ 的一个不动点。通过不动点组合子，可以实现通过非递归的 Lambda 表达式来定义匿名的递归函数。

Y Combinator 是 Haskell B. Curry 发现的一种不动点组合子，定义为

$$Y := \lambda f.\ (\lambda x.\ f\ (x\ x))\ (\lambda x.\ f\ (x\ x))$$

<!--more-->

证明过程
-------

通过将 Y Combinator 作用与一个函数 $g$ 上，证明 $Y \ g$ 是函数 $g$ 的一个不动点。

$$\begin{aligned}
& (Y\ g) \\
=& ((\lambda f.\ (\lambda x.\ f\ (x\ x))\ (\lambda x.\ f\ (x\ x)))\ g) && \text{defination of } Y \\
=& ((\lambda x.\ g\ (x\ x))\ (\lambda x.\ g\ (x\ x))) && \beta-\text{reduction}: \text{applied } Y \text{ to } g \\
=& ((\lambda t.\ g\ (t\ t))\ (\lambda x.\ g\ (x\ x))) && \alpha-\text{conversion} \\
=& (g\ ((\lambda x.\ g\ (x \ x))\ (\lambda x.\ g\ (x\ x))))) && \beta-\text{reduction}: \text{applied left function to right function} \\
=& (g\ (Y\ g))
\end{aligned}$$

Y Combinator 的作用
------------------

例如定义一个递归函数用于求自然数阶乘：

~~~scheme
(define fact
  (lambda (n)
    (if (zero? n) 1 (* n (fact (- n 1))))))
~~~

这个函数使用了命名递归，即在函数体中使用了函数自身的函数名来调用自身。对于支持高阶函数的编程语言，为了不使用命名递归，可以将需要递归调用的函数作为参数，并且返回一个函数作为结果。可以通过将函数自身使用参数传递的方法来避免命名递归，上文中的`fact`函数实现为：

~~~scheme
(define (fact-maker self x)
  (if (= x 0) 1 (* x (self self (- x 1)))))
;; usage: (display (fact-maker fact-maker 5))
~~~

通过Currying转换为一次只能接受一个参数的函数：

~~~scheme
(define fact-maker
  (lambda (self)
    (lambda (x)
      (if (= x 0) 1 (* x ((self self) (- x 1)))))))
;; usage: (display ((fact-maker fact-maker) 5))
~~~

在上述变换过程中，只是将递归函数自身函数名当成第一个参数传递，然后科里化得到`fact`函数。相较于最初的简单的`fact`函数 只需要将所有对`fact`自身的调用转换成对辅助函数的自身(self)的复合即`(self self)`即可。`fact = fact-maker fact-maker`，而`fact-maker`本身是一个lambda表达式，因此，可以直接按照如下方式定义`fact`函数：

~~~scheme
(define fact
  ((lambda (self)
     (lambda (x)
       (if (= x 0) 1 (* x ((self self) (- x 1))))))
   (lambda (self)
     (lambda (x)
       (if (= x 0) 1 (* x ((self self) (- x 1))))))))
;; usage: (display (fact 5))
~~~

按照这种方式定义的`fact`函数完全没有使用命名递归而实现了递归的功能。对于这一技巧，更加一般化的情形即为 Y Combinator。

将`fact`函数中的执行计算功能的部分与其他操作函数的部分分离开，有：

~~~scheme
(define F
  (lambda (x)
    (if (= x 0) 1 (* x ((self self) (- x 1))))))
~~~

将`(self self)`分离出去，得到：

~~~scheme
(define F*
  (lambda (self)
    (lambda (n)
      (if (zero? n) 1 (* n (self (- n 1)))))))
~~~

`F*` 具有与上文中简单递归`fact`函数相同的形式。将对`fact`函数中对`F*`操作的部分抽象出来，就得到了 Y Combinator：

~~~scheme
(define Y
  (lambda (recurse)
    ((lambda (self) (recurse (self self))))
     (lambda (self) (recurse (self self)))))
~~~

这与 Y Combinator 的形式化 Lambda 表达式 $Y := \lambda f.\ (\lambda x.\ f\ (x\ x))\ (\lambda x.\ f\ (x\ x))$ 具有相同的形式。在Scheme中，`(recurse (self self))`里对参数`(self self)`的求值不会终止，又因为`f arg = (lambda arg: f arg) arg`，转换后，得到**一个**可以直接当成参数传递的函数，而不是一个需要求值规约的表达式。因此，Scheme中的Y Combinator为：

~~~scheme
(define Y
  (lambda (recurse)
    ((lambda (self) (recurse (lambda (arg) ((self self) arg))))
     (lambda (self) (recurse (lambda (arg) ((self self) arg)))))))
~~~

使用 Y Combinator，阶乘函数 `fact` 可以表达为：

~~~scheme
(define fact (Y F*))
~~~

另一个例子，使用 Y combinator 来使用匿名递归定义 Fibonacci 函数：

~~~scheme
(define fibonacci
  (Y (lambda (recurse)
       (lambda (n)
         (if (< n 2) n (+ (recurse (- n 1)) (recurse (- n 2))))))))
~~~

在这两个使用 Y Combinator 的例子中，递归函数不再通过调用自身函数名进行，而通过一个参数进行。
Y Combinator 实现了通过非递归的 Lambda 抽象来定义递归函数。
通过 Fixed-point Combinator，可以将函数的名字与外部环境隔离，使得函数被重命名不会影响函数
内部递归逻辑的正确性。
在函数是编程语言中，Y Combinator 可以用于使用 `let` 来实现 `letrec`：

    letrec v = B in E => let v = Y (\v. B) in E

Memoizing
----------

Memoizing 指的是能够自动完成缓存函数调用的中间运算结果，以此提升函数的性能。可以通过将 Fix-point Combinator 与 memorize 结合使用，来以较高的抽象程度完成特定的功能。

首先定义 Y Combinator：

~~~cpp
template<typename F>
struct Y_struct {
    F f;
    template<typename T>
    auto operator () (const T & t) const {
        return f(*this, t);
    }
};
template<typename F>
auto Y(const F & f) {
    return Y_struct<F>{ f };
}
~~~

接下来，定义 memorize，使用`std::map`来缓存结果：

~~~cpp
template<typename F, typename K, typename V>
struct memorize_struct {
    F f;
    mutable std::map<K, V> cache;
    template<typename SELF>
    auto operator () (const SELF & self, const K & k) const -> V {
        auto iter = cache.find(k);
        if (iter == cache.end()) {
            iter = cache.emplace(k, f(self, k)).first;
        }
        return std::move(iter->second);
    }
};
template<typename K, typename V, typename F>
auto memorize(const F & f) {
    return memorize_struct<F, K, V>{ f, { } };
}
~~~

示例：

~~~cpp
auto fact = Y([](const auto & self, size_t n) -> size_t {
    return n == 0 ? 1 : n * self(n-1);
})
auto fibonacci = Y(memorize<size_t, size_t>([](const auto & self, size_t n) -> size_t {
    return n == 0 ? 0 : n == 1 ? 1 : self(n-1) + self(n-2);
}));
~~~

Haskell中使用这一技巧的例子：

~~~haskell
import qualified Data.Map as M

type Gen a = (a -> a)

fix :: Gen a -> a
fix f = f (fix f)

type Memo a b = State (M.Map a b)

memoize :: Ord a => Gen (a -> Memo a b b)
memoize self x = do
    cached <- query x
    case cached of
      Just v  -> return v
      Nothing -> self x >>= \v -> store x v >> return v
    where
        query k = fmap (M.lookup k) get
        store k v = fmap (M.insert k v) get >>= put

fibHelper :: Monad m => Gen (Integer -> m Integer)
fibHelper _ 0    = return 0
fibHelper _ 1    = return 1
fibHelper self n = do
    a <- self (n-1)
    b <- self (n-2)
    return (a + b)

fib n = evalState (fix (fibHelper . memoize) n) M.empty
~~~


尾递归优化
----------

一些解释型语言不支持尾递归优化，并且严格限制了递归层数，例如Python。Y Combinator能够将递归实现为匿名递归，
对Y Combinator做一处修改，推迟递归调用求值，就可以实现突破尾递归函数的递归层数的限制。原始的Y Combinator如下：

~~~python
def Y(recurse):
    def helper(self):
        return recurse(lambda *x: self(self)(*x))
    return helper(helper)
~~~

使用一个无参数的Lambda表达式，将值转换为函数，将`self(self)(*x)`求值延迟：

~~~python
def Ylazy(recurse):
    def helper(self):
        return recurse(lambda *x: lambda: self(self)(*x))
    return helper(helper)
~~~

借助这个函数，就可以实现突破递归层数限制：

~~~python
def rec_no_limit(func):
    def wrapper(*args):
        out = Ylazy(func)(*args)
        while callable(out):
            out = out()
        return out
    return wrapper

## example. TODO the question is that when use pypy, the 'sum_n'
## is faster, but under Python 3, 'sum_n_normal' is better, why ?
## A related discussion:
##  http://stackoverflow.com/questions/13591970/does-python-optimize-tail-recursion
def sum_n(n):
    def tail_rec_helper(recurse):
        def calc_core(n, acc):
            return acc if n == 0 else recurse(n-1, n+acc)
        return calc_core
    return rec_no_limit(tail_rec_helper)(n, 0)
def sum_n_normal(n):
    def calc_core(n, acc):
        return acc if n == 0 else calc_core(n-1, n+acc)
    return calc_core(n)
~~~

一个很有意思的事实是在普通的CPython下，上面的`sum_n`的性能比`sum_n_normal`差大概十倍，但是，在Pypy下，`sum_n`的性能
比`sum_n_normal`要快一百多倍，猜测原因是Pypy的尾递归优化带来了显著的性能提升。

Y Combinator in Haskell
-----------------

对于Haskell这一类支持惰性求值的编程语言，可以直接从数学定义来实现 Y Combinator（如果没有Lazy Evaluation，这种实现会产生 infinite application stream）：

    fix :: (a -> a) -> a
    fix f = let x = f x in x

`fix` 函数返回的是函数`f`在 domain ordering 上的最后一个不动点(least defined fixed point of a function)，涉及到denotational semantics，Haskell中每一个类型都包含了一个特殊的值：$\bot$，并且，$\bot$是任何类型的 least-defined value。因此，如果 $$f\ \bot = \bot$$ 那么，

$$fix\ f = \bot$$

这可以解释为什么 $(3\ \ast)\ 0\ =\ 0$ 但是 $fix\ (3\ \ast)\ =\ \bot$ ($0$ 和 $\bot$ 都是函数 $(3\ \ast)$
的 Fixed-point，但是按照 Partial order，$\bot < 3$)。

在GHCi中运行

    fix id

这行代码就会一直运行下去。GHCi 中运行`fix id`相当于`show (fix id)`，`fix id`会被严格求值，这会导致 infinite function application stream。
从另一个角度讲，`id`的类型是`a -> a`，那么，`fix id`的类型是一个多态类型：`a`，只有一个值的类型可以是任意的：$\bot$，而$\bot$与 non-terminating computation不可区分。为了使得计算能够终止，应用函数`f`应该能包含某种结构，使得下一次应用`f`十能够通过pattern matching来终止递归调用，而不需要计算全部参数的值。

例如：

~~~haskell
fix (const "hello")
=> let x = (const "hello") x in x
=> let x = (const "hello") ((const "hello") x) in x
=> let x = "hello" in x
=> "hello"

fix (1:)
=> let x = (1:) x in x
=> let x = (1:) ((1:) x) in x
=> let x = (1:) ((1:) ((1:) x)) in x
=> ...
~~~

在 System F (Polymorphic lambda calculus)中，polymorphic fixed-point combinator 的类型声明为：
$$\forall a.\ (a \to a) \to a$$
在 Simply typed lambda calculus 中，Y combinator 无法获得正确的类型，$Y := \lambda f.\ (\lambda x.\ f\ (x\ x))\ (\lambda x.\ f\ (x\ x))$ 中的子项 $(x\ x)$ 的类型推导规则为：

$$\frac{
    \Gamma \vdash x: t_1 \to t_2
    \quad
    \Gamma \vdash x: t_1
    }{
        \Gamma \vdash x\ x: t_2
    }$$

这就意味着 `x` 的类型是一个infinite type: $t_1 = t_1 \to t_2$，没有任何高阶函数能够具有这样的类型，因此，在这类编程语言中，必须提供语言级别的对递归的支持。

Haskell 中另外两种 Y Combinator 的实现方法：

~~~haskell
fix1 f = x where x = f x
fix2 f = f (fix2 f)
~~~

其他语言实现
-----------

### Python

~~~python
def Y(recurse):
    def helper(self):
        return recurse(lambda *x: self(self)(*x))
    return helper(helper)

fact = Y(lambda recurse: lambda n: n == 0 and 1 or n * recurse(n-1))
fibonacci = Y(lambda recurse: lambda n: n if n <= 1 else (recurse(n-1)+recurse(n-2)))
# TODO why the following implementation fails ?
# fibonacci = Y(lambda recurse: lambda n: n <= 1 and n or (recurse(n-1)+recurse(n-2)))
~~~

### C++

C++ 中另外一种 Y Combinator 的实现，支持 full-currying 的匿名函数。

~~~cpp
#include <iostream>
#include <functional>

/**
 * typename F: the type of origin recursive function.
 * typename T: the type of principle argument in the recursive function.
 * typename R: the type of the final result.
 */
template<typename F, typename T, typename R>
auto Y(auto recurse) -> F {
    auto helper = [=](auto self) -> F {
        return recurse([=](T x) -> R {
            return self(self)(x);
        });
    };
    return helper(helper);
};

int main() {
    auto fact = Y<std::function<int(int)>, int, int>([](auto recurse) {
        return [=](auto n) {
            return n == 0 ? 1 : n * recurse(n-1);
        };
    });
    auto fibonacci = Y<std::function<int(int)>, int, int>([](auto recurse) {
        return [=](auto n) {
            return n == 0 ? 0 : n == 1 ? 1 : recurse(n-1) + recurse(n-2);
        };
    });
    std:: cout << fact(10) << ' ' << fibonacci(10) << std::endl;

    return 0;
}
~~~

参考
----

1. 本文关于`memorize_struct`的部分参考了[专栏文章](http://zhuanlan.zhihu.com/marisa/20421894)。

