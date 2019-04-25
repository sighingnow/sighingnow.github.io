---
title: Arithmetic on Types
author: sighingnow
date: 2016-04-16
tag: [Haskell]
category: 编程语言
layout: post
---


> Haskell's algebraic data types are named such since they correspond to an initial algebra in category theory, giving us some **laws**, some **operations**
> and some **symbols** to manipulate.   -- by _Don Stewart_, [answer for the question _Haskell's algebraic data types_](http://stackoverflow.com/a/5917133).

<!--more-->

Algebraic Data Type
-------------------

Don Stewart's answer is some kink of abstract and confused me. What are the laws, operations, and symbols in Haskell? I had struggled here for a long time
before I really understood algebraic data type.

+ symbols: symbols are polymorphic types, link $a$, $b$, and etc.
+ operations: necessary operations of types are:

    - $+$: represents sum types, corresponds to `Either` in Haskell, as well as disjoint union or tagged union in other programming languages link C++.
    - $ast$: represents product types, corresponds to tuples and structs.
    - $X$: represents singleton type, such as `data X a = X a`.
    - $1$: represents the unit type `()`, as well as `void` in some other programming languages.
    - $\mu$: represents the least fixed point, used in recursive types.

Treat type constructors in Haskell as functions, all regular types can be expressed in these five terms.

+ `data () = ()`: $Unit = 1$
+ `data Either a b = Left a | Right b`: $E = a + b$
+ `data Maybe a = Nothing | Just a`: $S = 1 + X$
+ `data List a = Nil | Cons a (List a)`: $L = 1 + X * L$
+ `data BinTree a = Leaf | Branch a (BinTree a) (BinTree a)`: $B = 1 + X*B*B$

Function types are abstracted as exponential types: `a -> b` corresponds to $a^b$. And, what about the `Bool` type ? Look at the definition
`data Bool = True | False`, then it's not hard to conclude that $Bool = 1 + 1 = 2$.

Arithmetic on Types
--------------------

What is really interesting is the representation of `List`. It's well known to us that the type signature of `List` is `data [] a = [] | a : [a]` (defined in
`GHC.Types`), express this type with these operations, we will get $L(x) = 1 + x * L(x)$, which is equivalent to $L(x) = \frac{1}{1-x}$. Then do _Taylor
expansion_, the result is $$L(x) = 1+x+x^2+x^3+\dots$$
What we can know from this math formula is that a list $L(x)$ is either empty, or it have one element, or two elements, or three, or ... There must be
some kind of relation between algebra arithmetic and the type system behind this amazing fact. We can regard $x$ as just a simple variable, which is
used to represents primitive type, and $1$, $+$ and $\ast$ as the common notations in mathematics. The algebra laws of $1$, $+$ and $ast$ operations, such as
reflexixity, symmetry and distrubution laws, also hold when do math on types.

We can understand the formula above from another perspective:

$$\begin{aligned}
L(x) =& 1 + x * L(x) \\
     =& 1 + x * (1 + x * L(x)) \\
     =& 1 + x * (1 + x * (1 + x * L(x))) \\
     =& \dots \\
     =& 1 + x + x^2 + x^3 + \dots
\end{aligned}$$

Let's look back at the binary tree type,  `data BinTree a = Leaf | Branch a (BinTree a) (BinTree a)`, the corresponding formula to this algebraic data type
is $B(x) = 1 + x \ast B(x) \ast B(x)$. Solve the equation and we will get $B(x) = \frac{1 - \sqrt{1-4 \ast x}}{2 \ast x}$ <!-- TODO what does the other solution mean ? -->
Do _Taylor expansion_, the result is:

$$B = 1 + x + 2x^2 + 5x^3 + 14x^4 + \dots$$

It's not easy to guess what it means at first glance. The result tell us that a binary tree can be empty, in only one different pattern, or contain
one value, also in only one pattern, or contain two elements in two different ways, or contains three elements in five different ways, and etc. These
coefficients are the _Catalan Numbers_.

How do we make sense of arithmetic on types? A so important application of this theory is proof of isomorphism between two data types. A famous conclusion
is _Seven Trees in One_, first observated by Bill Lawvere. The idea is that there's a natural bijection between the set of finite binary trees and the set
of seven-tuple of trees. Some detailed description can be found in Andreas Blass's paper [Seven Tree in One](http://arxiv.org/abs/math/9405205), published in
1995. We can proof this isomorphism via type arithmetic:

$$\begin{aligned}
B^7 =& BB^6 \\
   =& B(B-1)^3 \\
   =& B(B^3-3B^2+3B-1) \\
   =& B(B^2(B-3)+3B-1) \\
   =& B((B-1)(B-3)+3B-1) \\
   =& B(B^2-B+2) \\
   =& B(B-1-B+2) \\
   =& B
\end{aligned}$$

Differentiation of Fixed Point and Zipper
-----------------------------------------

In the paper _The Derivative of a Regular Type is its Type of One-Hole Contexts_ Conor McBride introduced an interesting relation between type and mathematics.
The Zipper is a way to focus on one element of a data structure, in order to update it efficiently. Zipper transformation is related to the **derivative** of
the original type. In general, a zipper for a datatype T parameterized by some other type A will be in the following form:

$$Zipper_{T(A)} = \frac{\partial{T(A)}}{\partial{A}} * A$$

Take list zippper as the example,

~~~haskell
data ListZipper a = ListZipper a (Context a)
data Context a = Context ([a], [a])
~~~

The type of `Context a` is

$$T(x) = L(x) * L(x) = \frac{1}{(1-x)^2} = 1+2x+3x^2+\dots$$

The context just tell us that we poke a hole on a list with length $0$, we will get nothing, or we poke a hole on two sublists with length $1$, we will get two
sublists contain $0$ elements in total, or ..., or we poke a hold on list with length $n$, we will get two sublists which contain $n-1$ elements totally in
$n$ different patterns. If we have observated the derivative of $L(x)$,

$$\frac{\partial L(x)}{\partial x} = \frac{1}{(1-x)^2} = 1+2x+3x^2 + \dots$$

It's easy to find that $T(x) = \frac{\partial{L(x)}}{\partial{x}}$. It's not just a coincidence, however, the secret behind this equivalence is that
the derivative is exactly the type of one-hole context of a list.

As the tree zippper:

~~~haskell
data TreeZipper a = TreeZipper a (Context a)
data Conetxt a = Hole (BinTree a) (BinTree a)
               | NodeL (BinTree a) a (TreeZipper a)
               | NodeR (TreeZipper a) a (BinTree a)
~~~

Describe the type of `Context a` in algebra,

$$T(x) = B(x) * B(x) + B(x) * x * T(x) + T(x) * B(x)$$

Solve this equation and do expanding on it then we will get

$$T(x) = 1 + 4x + 15x^2 + 56x^3 + \dots = \frac{\partial{B(x)}}{\partial{x}}$$

What does this result really mean ? A zipper of a binary tree is a part of the original tree, which remains after poke a hole on the original tree. It's really
naturally to think that the exponents is how many elements remains and coefficients means how man patterns the remains part can be in. As same as the a list,
the one-hole context of a binary tree has the same type with the derivative of binary tree's type.

We also can do differentiating in another way:

$$T(x) = \frac{\partial{B(x)}}{\partial{x}} = B(x)^2 + 2*x*B(x)*\frac{\partial{B(x)}}{\partial{x}}$$

Simplify it,

$$T(x) = \frac{\partial{B(x)}}{\partial{x}} = \frac{B(x)^2}{1-2*x*B(x)} = B(x)^2*L(2*x*B(x))$$

It looks so differently with $T(x)$ above and we can build a link between the previous definition of `TreeZipper a` and this formula. It's exactly correct and
corresponds to another form of `TreeZipper`. We have learned how to write the type expressions from definitions of corresponding algebraic data types, here
what we need to do is write down the definition from type expression:

~~~haskell
data Context a = Context { lsub :: BinTree a
                         , rsub :: BinTree a
                         , path :: [(Bool, a, BinTree a)]
                         }
~~~

Which says that one-hole context of a binary tree is a product of two subtrees and a list holding elements of type `(Bool, a, BinTree a)`, where the number
$2$ represents `Bool` type, which decides where to go next step, left or right, `a` means the value of current node, and `BinTree a` contains all the elements
we will miss if we go the path which the direction value (of `Bool` type) tells us. There're two kind of description of zipper of a binary tree, **but they
are equivalent**, their type is equivalent in math, there must be at least an isomorphism between them.

We can make sense if the hole is just an element of a list or a binary tree, but what if the hole is a subtree ? We can do differentiating on $B(x)$, we will get

$$T(x) = \frac{\partial{B(x)}}{\partial{B(x)}} = x * 2 * B(x)$$

It means that a zipper for a binary tree can also be considered as a path, which is a triples consisting of

+ a value for the current node.
+ a choice of left or right subtree in which to find the hole (notice that a hole here is a subtree, either left or right).
+ the other subtree we will miss.

Notice that $T(x)$ is just the type of one-hole context, zipper's type is

$$ B(x) * x * 2 * B(x) $$

Reference
---------

There is a wonderful series of articles published by Chris Taylor at his [blog](http://chris-taylor.github.io/blog/2013/02/10/the-algebra-of-algebraic-data-types/),
as well as his talk _[The Algebra of Algebraic Data Types](https://www.youtube.com/watch?v=YScIPA8RbVE)_. Conor McBride's famous paper _[The Derivative of a Regular
Type is its Type of One-Hole Contexts](http://strictlypositive.org/diff.pdf)_ and Michael Abbott's work about differentiating on data structures
_[∂ for Data: Differentiating Data Structures](http://www.cs.nott.ac.uk/~psztxa/publ/jpartial.pdf)_ do help me understand the concepts and applications of derivative on types.

