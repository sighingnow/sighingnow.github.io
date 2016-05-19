---
title: Notes on Combinatory Logic
author: sighingnow
date: 2016-04-14
tag: [Haskell]
category: 编程语言
layout: post
---

Combinatory logic is a way which eliminates the need for quantified variables in lambda calculi. It was introduced by
Moses Schonfinkel and haskell B. Curry, and has been used to as the basis for the design of functional programming languages,
such as Haskell.

<!--more-->

Combinatory calculi
-------------------

Compared with lambda calculus, combinatory calculus provides a limited set of primitive functions, instead of abstraction, out of
which other functions can be built. A combinatory term has one of the following forms:

+ $x$: a variable.
+ $P$: one of the primitive functions.
+ $(E_1 \ E_2)$: the application of combinatory terms $E_1$ and $E_2$.

A combinator is a higher-order function that uses only function application and eariler defined combinators to define a result from
its arguments. The primitive functions themselves are combinators contain no free variables. The convention of multiple application
is left-associativity. In combinatory logic, the reduction rule has the form $(P \ x_1 \dots x_n) = E$ where $E$ is a term mentioning
only variables from the set $\{x_1 \dots x_n \}$. There are three commonly used combinators in combinatory logic:

+ The identity combinator: $\forall x. (I \ x) = x$.
+ The constant combinator: $\forall x, y. ((K \ x) \ y) = x$.
+ The application combinator: $(S \ x \ y \ z) = (x \ z \ (y \ z))$. It means $x$ is applied to $y$ inside the environment $z$.

There's [a simple SKI interpreter]({{site.url}}/resource/notes_on_combinatory_logic/ski.hs) in Haskell, which can show you the final
form of a combinatory logic term after reduction.

Extensionally equal to lambda calculi
-------------------------------------

By Church's thesis, lambda calculus is extensionally to any computable function. The following proof will present that $S$ and $K
can be composed to produce combinators that are extensionally equal to any lambda term.

Let $T$ as the the transformation from lambda terms to combinatory logic terms, $T[]$ can be defined as follows:

1. $T[x] \implies x$.
2. $T[(E_1 \ E_2)] \implies (T[E_1] \ T[E_2])$.
3. $T[\lambda x. E] \implies (K \ T[E])$, if $x$ doesn't occur free in $E$.
4. $T[\lambda x. x] \implies I$.
5. $T[\lambda x. \lambda y. E] \implies T[\lambda x. T[\lambda y. E]]$, if $x$ occurs free in $E$.
6. $T[\lambda x. (E_1 \ E_2)] \implies (S \ T[\lambda x. E_1] \ T[\lambda x. E_2])$, if $x$ occurs free in $E_1$ or $E_2$.

This construction is also called abstraction elimination, indicates completeness of the S-K basis.

In the reverse direction, the conversion $L$ from combinatory logic terms to lambda terms is trivial:

1. $L[x] \implies x$.
2. $L[I] \implies \lambda x. x$.
3. $L[K] \implies \lambda x. \lambda y. x$.
4. $L[S] \implies \lambda x. \lambda y. \lambda z. ((x \ z) \ (y \ z))$.

Logic and proof
---------------

A typed combinatory logic corresponds to a Hilbert system (the third Lukasiewicz's axiom system excepts the exclude-middle law) in
proof theory. The $K$ and $S$ combinators correspond to the axioms

+ $AK: A \to (B \to A)$
+ $AS: (A \to (B \to C)) -> ((A \to B) \to (A \to C))$

and function application correspond to the modus ponens rule

+ $MP: A, A \to B \vdash B$

B, C, K, W system
-----------------

The B, C, K, W system is a variant of combinatory logic takes the combinators $B$, $C$, $K$, $W$ as primitives. The B, C, K, W
systems corresponds to the Hilbert's axiom system. The four combinators are:

+ $B \ f \ g \ x = f \ (g \ x)$
+ $C \ f \ x \ y = f \ y \ x$
+ $K \ x \ y = x$
+ $W \ f \ x = f \ x \ x$

SKI can be defined by these four combinators:

+ $I = W \ K$
+ $K = K$
+ $S = B \ (B \ (B \ W) \ C) \ (B \ B) = B \ (B \ W) \ (B \ B \ C)$

Combinatory logic and Haskell
-----------------------------

S, K, I are one basic set of combinators in combinatory logic, B, C, K, W system is another.

~~~haskell
_S = ap = <*>   -- m (a -> b) -> m a -> m b where m = ((->) r)
_K = const      -- a -> b -> a
_I = id         -- a -> a
_B = (.)        -- (b -> c) -> (a -> b) -> (b -> c)
_C = flip       -- (a -> b -> c) -> (b -> a -> c)
_W = join       -- m (m a) -> m a where m = ((->) r)
~~~

With these primitives we can make Haskell program point-free (elimination of named parameters, in other words, omitting the
introduction of explicit variables).

Tools and applications
----------------------

An interesting application of $S$ combinator is quine. In computer science, a quine is a non-empty program takes no input and produces
a copy of its own source code as its only output, also named self-replicating programs or self-reproducing programs. Look at the $S$
combinator:

$$S \ f \ g \ x \implies ((f \ x) \ (g \ x))$$

We can explain $S$ combinator as _apply $f$ on $g$ in the environment of $x$_. A classic quine contains some code and a string, the
string is the code text and the code can treat string as its parameter then print it. The $S$ combinator can transit $f$ and
$g$ to $f \ x$ and $g \ x$. It's not hard to construct one with this trick:

~~~haskell
((++)<*>show)"((++)<*>show)"
~~~

More generalized, a quine is a CL-term which is euqivalent to it's own representation.

Reference
---------

This page is my notes during the process of studying combinatory logic, some materials are quoted from
[Wikipedia - Combinatory Logic](https://en.wikipedia.org/wiki/Combinatory_logic). Thanks for people's contribution and their spirit
to share knowledge and opinions.

