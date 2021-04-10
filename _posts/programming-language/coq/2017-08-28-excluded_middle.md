---
title: Excluded Middle in Coq 
author: Tao He
date: 2017-08-28
tags: [Coq, Proof, Logic]
category: 编程语言
layout: post
---

Excluded middle is an important theorem in classical logic. However in intuitionistic logic, the excluded
middle theory doesn't satisfy the constructive principle. The excluded middle law is provable in Coq and
often confused with some other theorems in Coq.

<!--more-->


Proof by Contradiction
----------------------

Proof by contradiction is an important technique in practical proof works, it means: in order to prove
$\Phi$, use $\neg \Phi$ as a new given and attempt to deduce a false statement($\bot$).



Excluded Middle in Classical Logic
----------------------------------

These five laws in classical logic could be added as axioms safely into constructive logic, without
causing any inconsistency. We cannot prove the negation of such an axiom.

