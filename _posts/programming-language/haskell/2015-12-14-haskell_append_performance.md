---
title: Haskell List Append Performance
author: Tao He
date: 2015-12-14
tag: [Haskell]
category: 编程语言
layout: post
---

阅读 Graham Hutton 的书 _Programming in Haskell_ 的 Chapter 13 Reasoning about programs 时，书中分析 `reverse` 函数的时间复杂度时提到Haskell中列表拼接运算符`(++)`的时间复杂度是$O(n)$的！

<!--more-->

Implement `reverse`
--------------------

`reverse`函数用于将一个List倒转方向，不难写出一个简单的实现：

~~~haskell
-- | reverse a list.
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]
~~~

分析这一实现的时间复杂度：

首先，`(++)`的时间复杂度与第一个参数List的长度正相关，一共需要 n 次拼接操作，以此，最终的结果为：

> `reverse` takes quadratic time in the length of its argument.

我们不难看出，`(++)`函数的复杂度是瓶颈所在，Hutton 的书中给出了另一个实现：

~~~haskell
-- | reverse a list in linear time.
reverse :: [a] -> [a]
reverse xs = auxiliary xs []
    where
        auxiliary [] ys = ys
        auxiliary (x:xs) ys = auxiliary xs (x:ys)
~~~

在这个实现中，辅助函数`auxiliary`的作用是将已经reverse的结果保存到一个List中，每次采用`(:)`操作来更新结果，这样，每一步`reverse`的时间复杂度都是$O(1)$的，因此，整个列表的reverse可以在$O(n)$的时间内完成，也即`reverse`函数具有线性时间复杂度。

Haskell的`reverse`函数定义在base库中，具体实现：

~~~haskell
-- | 'reverse' @xs@ returns the elements of @xs@ in reverse order.
-- @xs@ must be finite.
reverse                 :: [a] -> [a]
#ifdef USE_REPORT_PRELUDE
reverse                 =  foldl (flip (:)) []
#else
reverse l =  rev l []
  where
    rev []     a = a
    rev (x:xs) a = rev xs (x:a)
#endif
~~~

这个实现也采取了使用`(:)`而不是`(++)`的思路，以保证函数的效率。

关于几个`reverse`的实现的简单的benchmark：

+ 代码

~~~haskell
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -O2 #-}
import Criterion.Main

reverse1 = reverse

reverse2 [] = []
reverse2 (x:xs) = reverse2 xs ++ [x]

reverse3 xs = auxiliary xs []
    where
        auxiliary [] ys = ys
        auxiliary (x:xs) ys = auxiliary xs (x:ys)

main :: IO ()
main = defaultMain $ let !xs = ([1..1000] :: [Int]) in [
        bgroup "reverse" [
            bench "library reverse" (nf reverse1 xs)
            , bench "simple reverse" (nf reverse2 xs)
            , bench "fast reverse" (nf reverse3 xs)
        ]
    ]
~~~

[执行结果]({{site.url}}/resource/haskell_append_performance/bench_report_reverse.html)（编译优化选项：`-O2`）：

| 函数        | 具体实现                        |  运行时间    |
|:-----------:|---------------------------------|:------------:|
| reverse1    | 库函数实现(`Data.List.reverse`) | 9.92 us      |
| reverse2    | 使用`++`                        | 6.05 ms      |
| reverse3    | 使用`:`和辅助函数               | 9.49 us      |

`reverse`的高效实现方法中所采取的缓存结果的思路对于算法设计和分析有着重要的启发意义。

Efficient concatenation
-----------------------

基于命令式语言的列表拼接操作极其简单，并且具有常数级别的时间复杂度。但函数是编程语言中，数据是不可变的(pure data is immutable)，因此，也无法做到将后一个列表的head pointer直接指向上一个列表的tail这样的操作。Haskell中，每一次做拼接操作都需要创建一个新的List。看标准库中`(++)`的实现：

~~~haskell
(++) :: [a] -> [a] -> [a]
{-# NOINLINE [1] (++) #-}    -- We want the RULE to fire first.
                             -- It's recursive, so won't inline anyway,
                             -- but saying so is more explicit
(++) []     ys = ys
(++) (x:xs) ys = x : xs ++ ys

{-# RULES
"++"    [~1] forall xs ys. xs ++ ys = augment (\c n -> foldr c n xs) ys
  #-}
~~~

相当于将第一个参数列表展开成单个元素，再一个一个地加到第二个List的开头。因此，`(++)`函数的时间复杂度与第一个参数列表的长度成线性关系。

但是，有没有更高效的实现？

> Difference lists are implemented as single-argument functions, which take a list as argument and prepend to that list. As a consequence, concatenation of difference lists of the second type is implemented essentially as **function composition**, which is $O(1)$. However, of course the list still has to be constructed eventually (assuming all of its elements are needed), which is plainly at least $O(n)$.

Difference List是一个很有意思的东西，他用函数来表达List，将List上的操作表达为对 function application，能够实现constant time的append/prepend操作。仅仅在最后队列表求值时的时间复杂度为$O(n)$。对于需要重复多次的列表拼接操作来说，这一数据结构在效率方面具有明显的优势。

DList的核心在于将数据保存在函数中，用函数的组合来表示数据的组合，这是一个很自然而然地想法。DList的实现：

~~~
-- Lists as functions
newtype DList a = DL { runDL :: [a] -> [a] }

-- The empty list
empty       = DL id

-- The append case: composition, a la Hughes
append xs ys = DL (runDL xs . runDL ys)

-- Converting to a regular list, linear time.
toList      = ($[]) . runDL
~~~

另一个高效地实现List的append/prepend的数据结构是 Figure Tree。Haskell的库containers中导出了Figure Tree的一个实现 `Data.Sequence`，具有很好的执行效率。

> Standard `Sequence` has $O(1)$ for addition from 'both ends' and $O(log(min(n1,n2)))$ for general concatenation (The difference from lists though is that `Sequence` is strict).


