---
title: 使用generalize的技巧
author: Tao He
date: 2016-03-16
tags: [Coq, Proof, Logic]
category: 编程语言
layout: post
---

Coq中`generalize`和`generalize dependent`的用法说明如下：

> `generalize term`:
> This tactic applies to any goal. It generalizes the conclusion with respect to some term.
>
> `generalize dependent term`:
> This generalizes term but also all hypotheses that depend on term. It clears the generalized
> hypotheses.

<!--more-->

使用`generalize`策略可以将上下文的假设引入到目标中，如果目标中不存在这个假设，则构造一个箭头类型，表示蕴含关系。
当目标中包含了这个假设元素，通过全称量词`forall`构造一个依赖积。而`generalize dependent`会将上下文中所有的
依赖这个假设元素的项都引入到目标中。

一个证明
-------

首先通过一个简单的证明来说明`generalize`的作用。

定义函数`last_elem`里获取一个泛型列表中最后一个元素的值，如果列表为空，返回`None`，否则返回`Some A`：

~~~coq
Fixpoint last_elem {A : Type} (l : list A) : option A :=
  match l with
    | nil      => None
    | (a::nil) => Some a
    | (_::xs)  => last_elem xs
  end.
~~~

证明定理：对于任意的非空列表`l`，`last_elem l`的值不等于`None`：

~~~coq
Theorem last_elem_cons_not_none :
  forall (A : Type) (a : A) (l : list A), last_elem (a :: l) <> None.
~~~

对于`list`这类归纳数据结构上的函数的性质的证明，很自然而然的想法是使用归纳法：

    induction l.

得到这样两个子目标：

    2 subgoals
    A : Type
    a : A
    ______________________________________(1/2)
    last_elem (a :: nil) <> None
    ______________________________________(2/2)
    last_elem (a :: a0 :: l) <> None


对于第一个子目标，化简，`last_elem (a::nil) = Some a`，因此目标变为`Some a <> None`，不等号左右两边的式子是同一个
归纳类型的不同构造函数，不等号显然成立。具体来说，使用如下的tactics：

    simpl; discriminate.

证明基本情形之后，可以得到一个归纳假设，同时还剩下第二个归纳结论需要证明：

    1 subgoal
    A : Type
    a, a0 : A
    l : list A
    IHl : last_elem (a :: l) <> None
    ______________________________________(1/1)
    last_elem (a :: a0 :: l) <> None

此时我们无法再继续进行下去了，问题就处在归纳假设`IHl`上。化简`last_elem (a::a0::l)`，结果是`last_elem (a0 l)`，
与`IHl`对比，只是`a`和`a0`的不同。也正是由于这个差别，导致无法使用`IHl`来证明当前的目标。显然，对于任意的`a`，命题
`last_elem (a::l) <> None`都应该成立，而不仅仅是对当前上下文中的`a`成立。`IHl`的形态应该是：

    forall a, last_elem (a :: l) <> None

而在`induction l`时，参数中排在`l`前面的`forall n`就已经引入到了上下文中，而没有保留在目标命题里，归纳假设`IHl`中
也就自然不会出现对`a`的`forall`限定。为了解决这个问题，就需要用到`generalize`。一个很直接的思路是在考虑对base case
的证明时使用`generalize`将`a`从上下文中引入到命题里，对于基础命题，使用

    generalize a

之后目标变成：

    1 subgoal
    A : Type
    a : A
    ______________________________________(1/1)
    forall a0 : A, last_elem (a0 :: nil) <> None

这已经与我们想到的归纳假设接近，但是，`simpl; discriminate.`之后，上下文仍然是：

    1 subgoal
    A : Type
    a, a0 : A
    l : list A
    IHl : last_elem (a :: l) <> None

与之前相比，结果并没有改善。这个问题的原因在在于`generalize`操作是在`induction`之后的，并没有真正改变归纳假设。
正确的做法，应该在`induction`之前使用`generalize`将`a`重新引入到目标命题中：

    intros A a l.
    generalize a.
    induction l.

证明基础条件下的子目标成立，得到的上下文：

    1 subgoal
    A : Type
    a, a0 : A
    l : list A
    IHl : forall a : A,
          last_elem (a :: l) <> None
    ______________________________________(1/1)
    forall a1 : A,
    last_elem (a1 :: a0 :: l) <> None

对于当前的目标，只需要

    apply (IHl a0).

就行。


