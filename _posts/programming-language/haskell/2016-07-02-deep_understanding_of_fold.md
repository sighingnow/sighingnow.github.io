---
title: Deep Understanding of Fold in Haskell
author: sighingnow
date: 2016-07-02
tag: [Haskell]
category: 编程语言
layout: post
---

`fold` is a very important combinator in functional programming. In Haskell four versions of `fold` operation
are provided: `foldl`, `foldr`, `foldl'` and `foldr'`. Now lets take a close look at them. We'll gain a better
understanding of the underlying principles behind `fold` operation and learn how to use them correctly and
efficiently in our real world applications.

<!--more-->

Implementation
--------------

`fold` is a higher order function that takes a function and a list as arguments, look at the
detailed implementations of these four `fold` functions in the
[base](http://hackage.haskell.org/package/base) library.

~~~haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z0 xs0 = lgo z0 xs0
             where
                lgo z []     =  z
                lgo z (x:xs) = lgo (f z x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr k z = go
          where
            go []     = z
            go (y:ys) = y `k` go ys

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z0 xs0 = lgo z0 xs0
    where lgo z []     = z
          lgo z (x:xs) = let z' = f z x in z' `seq` lgo z' xs

foldr' :: (a -> b -> b) -> b -> t a -> b
    foldr' f z0 xs = foldl f' id xs z0
      where f' k x z = k $! f x z
~~~

`foldl` means _left-associative_ fold operation, folding up a list form the left to right and `foldr` are
_right-associative_ fold operation. `foldl'` and `foldr'` are strict version of `foldl` and `foldr`.
We can see that `foldl` and `foldl'` are both tail recursion and `foldr` and `foldr'` are not.
Because Haskell is a programming language with non-strict evalution strategy, the lazy version of `foldl`
and `foldr` will accumulate too many unevaluted **chunks** on heap when the list to fold over is too long.

There's also a picture from Wikipedia that can illustrate the core feature of `foldl` and `foldr` very well:

![Fold transformation]({{site.url}}/resource/deep_understanding_of_fold/fold_transformation.png)

Benchmark
---------

I make a basic benchmark to measure the performance of these four version of fold operation, and the
result of benchmark is corresponded with our knowledge about above detailed implementations of fold
operation. The task is computation summary of an integer sequence `[1..1000000]`.

~~~haslell
r = @fold (+) 0 ([1..1000000] :: [Int])
~~~

All experiments are done using GHC 8.0.1 with `-O2` optimization on 64bit Windows 10 (CPU: Intel 5600U).

### CPU Usage

I use the [criterion](http://hackage.haskell.org/package/criterion) which is a well-known framework for
executing and analysing benchmark in Haskell to perform CPU time usage benchmark.

~~~
Case       CPU time
foldl      174.8 ms
foldr      29.23 ms
foldl'     9.806 ms
foldr'     87.38 ms
~~~

`foldl'` is the fastest version of these four fold operations. I have also noticed an interesting phenomenon that
`foldr` with `-O` optimization is four times slower than using `-O2` optimization option.

### Memory Allocation

Fpcomplete has developed a very convenient tool called [weigh](https://www.fpcomplete.com/blog/2016/05/weigh-package)
which can measure allocations in Haskell program. The result is as follows:

~~~
Case          Bytes  GCs  Check
foldl   169,259,440  322  OK
foldr    96,646,080  153  OK
foldl'   95,999,920  186  OK
foldr'  127,999,920  247  OK
~~~

`foldl'` use the least memory.

Best Practice
-------------

After analysis the principles of fold operation, we can conclude some best practical strategies to improve
performance when use `fold` operator in Haskell:

+ When we are sure that the list to fold is finite, the fold computation will terminate, then `foldl'` would
be the best choice.
+ `foldr` is the only fold function that can process infinite lists, when the binary function has the property
of short-circuit evaluation (like logical _and_ `&&` and logical _or_ `||`), the computation will terminate.
Be careful that `foldr'` can't be used to process infinite list.
+ Under almost all conditions the `foldl` combinator has the worse performance both in time usage and mmeory
allocations and shouldn't be used in production-level code.

