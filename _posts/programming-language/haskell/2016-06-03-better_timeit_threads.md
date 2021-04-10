---
title: A Better Approach for Timing Multi-thread Computations
author: Tao He
date: 2016-06-03
tag: [Haskell]
category: 编程语言
layout: post
---

There's already a package named [timeit](https://hackage.haskell.org/package/timeit) on hackage,
it provides utilitities under the `System.TimeIt` module which can be used to time an IO computation.
It depends on the [`getCPUTime`](https://hackage.haskell.org/package/base-4.9.0.0/docs/System-CPUTime.html#v:getCPUTime)
to get timestamp with picoseseconds precision and it works quite well for single thread applications,
however when I use the `timeIt` function to time a multi-thread computation, it report the wrong time
usage. After some experiments, I proposed a proper approach for time computations in `IO` environment,
both for single-thread applications and multi-thread applications.

<!--more-->

Wrong timing result
-------------------

First, look at the following example:

~~~haskell
import System.TimeIt

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

parFib :: Integer -> Integer
parFib 0 = 0
parFib 1 = 1
parFib n | n < 12 = fib n
         | otherwise = runEval $ do
             x <- rpar (fib (n-1))
             y <- rpar (parFib (n-2))
             return (x + y)

main :: IO ()
main = timeIt $ print . parFib $ 38
~~~

It's quite a simple parallel program, and the result of `timeIt` is so confusing:

~~~shell
$ ghc --make -O2 -threaded -rtsopts timing.hs && time ./timing.exe +RTS -N4
63245986
CPU time:   4.06s

real    0m2.242s
user    0m0.000s
sys     0m0.000s
~~~

You see that the result of `timeIt` is almost as twice time as the program really comsumes.
The underlying reason of this strange phenomenon is still unknown. I have looked
at the implementation of `timeIt` function. I modify it's original code to emit
a user events when the two `getCPUTime` actions are performed. But I can't find
any useful information about the right timeing result.I have also tried the timer
functions provided by high-resolution clock library [clock](https://hackage.haskell.org/package/clock),
the same error still exists when timing a multi-thread computation. I use `-eventlog` options
to get runtime events related to multi-threads and more detailed information of time usage.
I think the `getCPUTime` function and utilitities provided by high-resolution clock library
may time the sumary usage of all worked threads when given computations are performed, but
i'm not sure.

A butter approach
-----------------

I noticed that when we calculate the time usage of a function, we usually focus
on how many seconds it used, so ordinary UTC time is enough for us to get the time
usage information to assess the performance of our program. The `Data.Time.Clock` module
in [time](https://hackage.haskell.org/package/time) package provide a such function,
I propose a `timeIt` implementation with `getCurrentTime`:

~~~haskell
import Data.Time.Clock

timeItT :: IO a -> IO (Double, a)
timeItT ioa = do
    t1 <- getCurrentTime
    a <- ioa
    t2 <- getCurrentTime
    let t :: Double
        t = realToFrac $ diffUTCTime t2 t1
    return (t, a)
~~~

Now, I use this version of `timeIt` to time the previous `parFib` computation and I
get the right result:

~~~shell
$ ghc --make -O2 -threaded -rtsopts timing.hs && time ./timing.exe +RTS -N4
63245986
CPU time:   2.20s

real    0m2.277s
user    0m0.000s
sys     0m0.000s
~~~


