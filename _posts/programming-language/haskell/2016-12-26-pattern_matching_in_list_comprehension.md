---
title: Pattern Matching in List Comprehension
author: Tao He
date: 2016-12-26
tag: [Haskell]
category: 编程语言
layout: post
---

After the `|` in list comprehension, pattern matching can be used to filter expected values.

<!--more-->

Useful syntax
-------------

It's actually extremely useful syntax and adds a lot of expression power to list comprehension.
The well-known `catMaybes` can be implemented in this way as:

~~~haskell
catMaybes :: [Maybe a] -> [a]
catMaybes ms = [x | Just x <- ms]
~~~

If the pattern matching failed, it just move to next element in the list, avoiding explicit
handing constructors we are not interested in.

`fail` in Monad
-------------

Why failure of pattern matching doesn't cause an error? What happens when pattern matching fails
in list comprehension?

In [session 3.11][1] of Haskell 2010 Language Report, in list comprehension if a match fails
then that element of the list is
simply skipped over. There's a desugar rule:

~~~haskell
[ e | p <- l, Q ] = let ok p = [ e | Q ]
                        ok _ = []
                     in concatMap ok  l
~~~

But, we must wonder why define such a rule? The underlying fact is that list is a monad in
Haskell. Function `catMaybes` can be implemented with `do` notation:

~~~haskell
catMaybes :: [Maybe a] -> [a]
catMaybes ms = do
    Just x <- ms
    return x
~~~

If pattern matching fails in a binding within `do` notation, the `fail` method of a monad
instance will be called. In Haskell 2010 Language Report [session 3.14][2], the desugar rule
is:

~~~haskell
do {p <- e; stmts} = let ok p = do {stmts}
                         ok _ = fail "..."
                      in e >>= ok
~~~

And, when instantiate `[]` as a Monad, the `fail` is:

~~~haskell
fail _ = []
~~~

Now, it's trivial to understand why mismatching isn't an error. For other monad when pattern
matching fails the `fail` method will be called. `IO`, for example, if pattern mismatch,
`failIO` will be called and an exception will be throw out.


[1]: https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-420003.11
[2]: https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-470003.14

