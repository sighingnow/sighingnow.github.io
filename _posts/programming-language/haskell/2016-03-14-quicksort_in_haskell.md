---
title: Quicksort in Haskell
author: He Tao
date: 2016-03-14
tag: [Haskell]
category: 编程语言
layout: post
---

The quicksort algorithm is a beautiful algorithm that has been used in introductions to the elegance and simplicity of
functional programming. Compared with the implementation in C/C++, the Haskell code below is short and looks elegant:

<!--more-->

~~~haskell
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = qsort (x:xs) = qsort (filter (< x) xs) ++ [x] ++ qsort (filter (>= x) xs)
~~~

However, this much beloved and fairly typical example is not the genuine quicksort algorithm!

Where it sucks ?
-----------------

We can gain the idea that there may be a critical loss of perfermance behind code seems straightforward from the paper
_The Genuine Sieve of Eratosthenes_ by Melissa E. O'Neill. Let's do an analysis of complexity of the code above, both
in time usage and space usage.

Assume we need sort a list whose length is $n$ using $T(n)$ time, at first we need $O(n)$ time to do the paritioning,
then we'll comsume $T(\frac{n}{2})$ to sort elements less than the pivot and $T(\frac{n}{2})$ for elements
greater than the pivot, as well as $O{\frac{n}{2}}$ time will be used to concatenate sorted sub-lists, under the
average condition. Now we can draw the conclusion: $$T(n) = O(n) + 2T(\frac{n}{2}) + O(\frac{n}{2})$$
According to the master theorem, $$T(n)=\Theta(n\log{n})$$

The time complexity seems not so bad, which is different with the case described in _The Genuine Sieve of Eratosthenes_.
So, where is the real problem ?

Let's look at the original Quicksort algorithm. The real Quicksort algorithm has to be imperative because it relies on
destructive update. The real Quicksort uses a so amazing partitioning strategy. The partitioning works in this way: scan
from the left for an element bigger than the pivot, then scan from the right for an element smaller than the pivot, and
then swap them. Repeat this util no such pair can be found, indicates all elements has been partitioned. Describe this
process in imperative language:

~~~c
algorithm partition(A, lo, hi) is
    pivot := A[hi]
    i := lo        // place for swapping
    for j := lo to hi – 1 do
        if A[j] <= pivot then
            swap A[i] with A[j]
            i := i + 1
    swap A[i] with A[hi]
    return i
~~~

But in Haskell, as well as many other functional programming language, the data is immutable! When we partition the given list
and merge sorted sub-lists, many new list will be created. Obviously it's no an actual in-place algorithm! Although that Haskell
can express the algorithm easily, the academics have addressed Haskell's failure by bastardizing the algorithm. It completely
fails to capture the essence, specifically the in-place partitioning using swaps, of the real quicksort algorithm represented
in Tony Hoare's original paper that makes it so efficient.

In-place quicksort in Haskell
-----------------------------

There's just one array constructor type namely `Array` in Haskell'98 standard, which provides immutable boxed arrays. The mian
Haskell compiler, GHC, support these libraries contains a new implementation of arrays with far more features which is backward
compatible with the Haskell'98 one. `STArray` is just one of mutable array based on `MutableArray#` defined in `GHC.Prim` and
powered by `ST Monad`.

~~~haskell
data STArray s i e
         = STArray !i                  -- the lower bound, l
                   !i                  -- the upper bound, u
                   !Int                -- a cache of (rangeSize (l,u))
                                       -- used to make sure an index is
                                       -- really in range
                   (MutableArray# s e) -- The actual elements
        -- No Ix context for STArray.  They are stupid,
        -- and force an Ix context on the equality instance.
~~~

Now we can translate the imperative pseudo code of quicksort algorithm from Wikipedia directly into Haskell code. First, we need
to create a function to represent the `for` control-flow in impreative language:

~~~haskell
foreach :: (Monad a, Foldable t) => t a -> b -> (b -> a -> m b) -> m b
foreach xs v f = foldM f v xs
~~~

And an auxiliary function, to swap elements at two positions in an array:

~~~haskell
swap :: (Ix i, MArray arr e m) => arr i e -> i -> i -> m ()
swap arr ia ib = do
    a <- readArray arr ia
    b <- readArray arr ib
    writeArray arr ia b
    writeArray arr ib a
~~~

Then translate the functions `partition` and `quicksort` into Haskell (the impreative pseudo code can be found at
[Quicksort - Wikipedia](https://en.wikipedia.org/wiki/Quicksort)):

~~~haskell
qsortImpl arr l r = when (r > l) $ do
    let mid = l + (r - l) `div` 2
    nmid <- partition arr l r mid
    qsortImpl arr l (nmid - 1)
    qsortImpl arr (nmid + 1) r

partition arr l r mid = do
    pivot <- readArray arr mid
    swap arr mid r
    slot <- foreach [l..r-1] l (\slot i -> do
        val <- readArray arr i
        if val < pivot
           then swap arr i slot >> return (slot+1)
           else return slot)
    swap arr slot r >> return slot
~~~

The most commonly used and most convenient data structure for sequence is `Data.List`, not mutable array. We also need to create
a abstract layer so that we can used the efficient genuine quicksort implementation on `List`.

~~~haskell
qsort :: [Int] -> [Int]
qsort xs = runST $ do
    let len = length xs - 1
    arr <- newListArray (0, len) xs :: ST s (STUArray s Int Int) -- or `ST s (STArray s Int Int)
    qsortImpl arr 0 len >> getElems arr
~~~

You can obtain the complete source code [here]({{site.url}}/resource/quicksort_in_haskell/st-sort.hs).

Parallel and concurrency
------------------------

Quicksort is a divide and conquer algorithm, divides a large array into two sub-arrays, one contains elements less than pivot,
one contains elements greater than pivot and sort these two sub-arrays using quicksort, then merge sorted sub-arrays. Finially,
all elements in this whole array are in order. Soring the sub-arrays are independent task so it's easy to do them parallelly.

Thanks to `rpar` strategy provided in [parallel](https://hackage.haskell.org/package/parallel) package, it's so convenient to
refactor the code above into parallelism.

~~~haskell
    withStrategy rpar $ qsortImpl arr l (nmid - 1)
    qsortImpl arr (nmid + 1) r
~~~

This solution uses Haskell's parallel "strategies". This concepts was introduced to give haskell programmer more control over
parallelization and help translate original sequential code to parallel version at the lowest price. `rpar` will sparks its
argument for evaluation in parallel.

Another similar idea can be used to improve perfermance is concurrency. It's also not hard to use `forkIO` to run tasks in
multiple threads. Haskell's runtime does support multi-core well.

~~~haskell
    (qsortImpl arr l (nmid - 1)) `dualThread` (qsortImpl arr (nmid + 1) r)
    where
        dualThread fg bg = do
            wait <- backgroud bg
            fg >> wait
        backgroud task = do
            m <- newEmptyMVar
            forkIO (task >>= putMVar m)
            return $ takeMVar m
~~~

Here are the complete Haskell sources code, including efficient
implementation of [parallel strategy]({{site.url}}/resource/quicksort_in_haskell/par-sort.hs)
and [concurrency implementation]({{site.url}}/resource/quicksort_in_haskell/concurrent-sort.hs)
for the famous quicksort algorithm.

Further work
------------

A thorough benchmark is need to analysis the perfermance of our ST-powered quick sort and parallel quick sort, and compare
to the sort algorithm in Haskell's library `Data.List.sort`. The function `qsort` should be made polymorphic for reusable
reason and this function can be used more wilely. More optimization should be added for better perfermance, such as when
the size of sub-arrays is small enough, sort them with bubble sort algorithm or swap sort algorithm, and a more optimal
strategy to select the pivot can also do good to avoiding encountering worse time complexity ($O(n^2)$) of the quicksort algorithm.

