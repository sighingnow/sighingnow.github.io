---
title: Combinator "seq"
author: Tao He
date: 2015-12-29
tag: [Haskell]
category: 编程语言
layout: post
---

Haskell 是一门拥有惰性赋值特性的函数式编程语言，Normal Form是表达惰性求值的重要概念，
`seq` 和 `deepseq` 用于将表达式求值到不同等级的 Normal Form。

<!--more-->

Normal Form
-----------

+ Normal Form

NF是指已经被完全求值，不能再规约的表达式。对于一个求值能结束的表达式，non-strict语义可以
保证一个表达式中存在多个可规约项时，不同求值顺序最终将其规约到同样的Normal Form。

+ Weak Head Normal Form

WHNF指将表达式求值到最外层构造器或者函数抽象或者一个部分调用的内建函数。惰性求值(Lazy
Evaluation)就是将一个表达式求值到WHNF。

+ Head Normal Form

对于 Head Normal Form的解释：

> A term describing a lambda expression whose top level is either a variable,
> a data value, a built-in function applied to too few arguments, or a lambda
> abstraction whose body is not reducible. I.e. the top level is neither a
> redex nor a lambda abstraction with a reducible body.
>
> An expression in HNF may contain redexes in argument postions whereas a
> normal form may not.

具体来说，相当于HNF的函数体不可规约，但参数中可以可规约项。

这三者之间的关系，可以表述为HNF是WNHF的真子集，NF是HFN的真子集。

seq
---

`seq` 函数用于 strict evaluation：

> The seq function is the most basic method of introducing strictness to a
> Haskell program. `seq :: a -> b -> b` takes two arguments of any type,
> and returns the second. However, it also has the important property that
> it is magically strict in its first argument.

`seq` 的原理并不是直接对第一个参数求值，而是**引入数据依赖**，当 `seq` 函数的结果，也
就是第二个参数被严格求值时，保证第一个参数一定会被求值。因此也就不难理解，`seq x x`
和 `x` 是等价的。`seq` 与 `strictness annotations` 和 `BangPatterns` 一样，都
用在程序的语义解释阶段，用于表达严格求值。

例如：

~~~haskell
f !x !y = z
~~~

的语义等价于

~~~haskell
f x y
    | x `seq` y `seq` False = undefined
    | otherwise             = z
~~~

GHC 的惰性求值通过 `thunks` 来实现，`seq` 可以被用于累积参数以确保不会形成过大的
`thunks` 导致程序运行时内存压力过大。例如，`fold`函数就可以通过严格求值来提高效率。

~~~haskell
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ z [] = z
foldl' f z (x:xs) = let z' = f z x in z' `seq` foldl' f z' xs
~~~

这个例子中，`seq` 函数保证了每次 `z'` 都会被严格求值。

`$` 运算符表示将函数应用到参数上，求值。`$!` 是 Call-by-value 的函数应用运算符，使用
`$!` 时，参数会先被求值到 WHNF(Weak head normal form，弱首范式)，然后再调用函数。
可以使用 `seq` 函数来定义 `$!` 运算符（实际实现是使用 strictness annotations，与
`seq`在语义上等价）：

~~~haskell
($!) :: (a -> b) -> a -> b
f $! x = x `seq` f x

-- f $! x = let !vx = x in f vx
~~~

> `seq` is the only way to force evaluation of a value with a function type
> (except by applying it, which is liable to cause other problems).

对于函数来说，`seq`会将函数求值到lambda表达式的形式，也就是`ready for application`的
形式。例子：

~~~haskell
-- ok, lambda is outermost
Prelude> seq (\x -> undefined) 'a'
'a'

-- not ok.  Because of the inner seq, `undefined` must be evaluated before
-- the lambda is showing
Prelude> seq (seq undefined (\x -> x)) 'b'
*** Exception: Prelude.undefined
~~~

GHC中，`case...of`对于参数也是严格求值，因此可以认为：

~~~haskell
seq a b = case a of
                _ -> b
~~~

deepseq 和 NFData
-----------------

deepseq 将表达式求值到NF。函数类型声明：

    deepseq :: NFData a => a -> b -> b

`deepseq` 和 `seq` 的区别在于`deepseq`会将参数彻底严格求值，而不仅仅是`seq`那样只
在顶层求值。`deepseq`可以被用于强制抛出异常、减少内存消耗和严格求值IO操作等，以及用于
连接并行计算策略。

`deepseq`对参数的求值是并行求值，**不保证参数各部分的求值顺序**。

> There is no guarantee about the ordering of evaluation. The implementation
> may evaluate the components of the structure in any order or in parallel.
> To impose an actual order on evaluation, use `pseq` from `Control.Parallel`
> in the parallel package.

同时，`deepseq`包中还包含了两个常用的函数：`$!!`和`force`。

+ `$!!`

`$!!`跟`$`和`$!`类似，区别在于`$!!`首先将参数求值到NF而不是WHNF。

~~~haskell
($!!) :: (NFData a) => (a -> b) -> a -> b
f $!! x = x `deepseq` f x
~~~

+ `force`

`force`函数对参数进行严格求值，并返回结果：

~~~haskell
force :: (NFData a) => a -> a
force x = x `deepseq` x
~~~

`deepseq`包中定义了数据类型`NFData`用于表达可以被彻底地严格求值的数据类型。数据类型
Int, Float, Bool 以及 List 等都是`NFData`类型类的实例类型。`deepseq`包中的函数`rnf`
可以对一个`NFData`的实例类型的表达式进行严格求值，将其规约到NF(rnf: reduce to normal
form)。

    rnf :: (NFData a) -> a -> ()

> `rnf` should reduce its argument to normal form (that is, fully evaluate all
> sub-components), and then return `()`.

`seq` is bad, and why?
-------------------

Polymorphic `seq` 可能会破坏一些变换的等价性。例如在Haskell中，以下等式成立：

    map g (f xs) = f (map g xs)

其中，`f`、`g` 和 `xs` 的类型声明分别为：

    f :: [a] -> [b]
    g :: a -> b
    xs :: [a]

为`f`、`g`和分别取值：

    f (x:y:_) = [seq x y]
    g True = True

如果`xs`的值为`xs = [False, True]`，那么：

    map g (f xs) = map g (f [False, True]) = map g [True] = True
    f (map g [False, True]) = f [undefined, True] = [undefined]

等式两边的值不一样，破坏了等式的性质。之所以会出现这个问题，愿意就在于`seq`会将List `xs`
中不同位置的两个值建立起依赖。更深层次的探讨，这一问题与Free Theorem有关， _[Free
Theorems in the Presence of seq](http://www.janis-voigtlaender.eu/papers/FreeTheoremsInThePresenceOfSeq.pdf)_ 以及 Philip Wadler 的论文 _[Theorems for
free!](http://dl.acm.org/citation.cfm?id=99404)_ 就在讨论这一问题，此外，_[Improvements for Free](http://arxiv.org/pdf/1107.1203.pdf)_ 一文看上去也很有
意思。

Polymorphic `seq` 在特定情况下会产生一些问题，但是其他的可选方案同样不是非常理想。如果
为`seq`添加一个类型类作为依赖，在使用`seq`时可能就需要很多的类型约束。同时，已经明确很多
内存占用过大的问题可以通过`seq`来解决，因此，`seq`也不可能被去除。

StackOverflow上另一个问题 [A simple example showing that IO doesn't satisfy the
monad laws?](http://stackoverflow.com/questions/12617664/a-simple-example-showing-that-io-doesnt-satisfy-the-monad-laws/12620418#12620418) 也很有意思，答案是使用 `seq` !!! 事实上，将`seq`和`undefined`组合
会破坏Monad Laws，因此，所有的 Monad 都会失效。

对于一个Monad，有：

    (Monad m) => m >>= return  = m

然而：

~~~haskell
Prelude> seq ( undefined >>= return :: IO () ) "hello, world"
"hello, world"

Prelude> seq ( undefined :: IO () ) "hello, world"
*** Exception: Prelude.undefined
~~~

在这个例子中：`undefined >>= return` 与 `undefined` 的值不相等。使用
`unsafePerformIO`也会产生类似的效果。

另一个使得 `Maybe` 不满足 Monad Laws 的例子：

~~~haskell
Prelude> seq ( undefined >>= return :: Maybe () ) "hello, world"
*** Exception: Prelude.undefined

Prelude> seq ( undefined :: Maybe () ) "hello, world"
*** Exception: Prelude.undefined
~~~

引用一段证明如下：

> In the Kleisli category the monad gives rise to, return is the identity
> morphism and `(<=<)` is composition. So return must be an identity for
> `(<=<)`:

    return <=< x = x

> Using `seq` even `Identity` and `Maybe` fail to be monads:

~~~haskell
seq (return <=< undefined :: a -> Identity b) () = ()
seq (undefined            :: a -> Identity b) () = undefined

seq (return <=< undefined :: a -> Maybe b) () = ()
seq (undefined            :: a -> Maybe b) () = undefined
~~~

参考
----

最后一部分内容关于 `seq` is bad 的内容参考了 Jan Christiansen 在 StackOverflow
上的问题 [Why is seq bad?](http://stackoverflow.com/questions/12687392/why-is-seq-bad) 下的 [回答](http://stackoverflow.com/a/12688908)。
