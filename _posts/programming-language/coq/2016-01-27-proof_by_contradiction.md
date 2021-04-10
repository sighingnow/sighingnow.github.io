---
title: Proof by Contradiction
author: Tao He
date: 2016-01-27
tags: [Coq, Proof, Logic]
category: 编程语言
layout: post
---

**Proof by Contradiction** is an important proof skill that: in order to proof $\Phi$, and $\neg \Phi$ as a new given, and attempt
to deduce an evidently false statement($\bot$).

In a schema:

+ Given: $\dots$
+ To be proved: $\Phi$
+ Proof:
    + Suppose $\neg \Phi$
    + To be proved: $\bot$
    + Proof: $\dots$
+ Thus $\Phi$.

<!--more-->

Example
-------

A simple case:

+ Theorem:

From $\neg Q \implies \neg P$ it follows that $P \implies Q$.

+ The proof:

    + Given: $\neg Q \implies \neg P$.
    + To be proved: $P \implies Q$.
    + Concise proof:
        + Assume that $P$.
        + If $\neg Q$, then it follows that $\neg P$ (by $\neg Q \implies \neg P$).
        + Contradiction.
        + Thus $Q$.
    + Thus $P \implies Q$.

Another more complex one:

+ Theorem:

From $(p \implies Q) \implies P$ it follows that $P$. (From the book _The Haskell Road to Logic, Math and Programming_,
exercise 3.9)

+ The proof is as follows:

    + Given: $(p \implies Q) \implies P$.
    + To be proved: $P$.
    + Proof:
        + Assume $\neg P$.
        + If $P \implies Q$, then from the given, we get $P$, contradict with our assumption.
        + So $\neg (P \implies Q)$.
    + Thus $P$.

$\neg$ introduction rule
------------------------

$\neg$ introduction rule: if $\neg \Phi$ is to be proved, assume $\Phi$ as a new given, and attempt to
prove something that is evidently false.

Proof by Contradiction looks very similar to the $\neg$ introduction rule. Indeed, in ordinary mathematical contexts,
it's usually better to move negation inside instead of applying $\neg$ introduction.

Proof by Contradiction sometimes looks so inviting. However, many times it must be considered poisoned. **Proof by Contradiction
should be considered as the last way out.**


