---
title: A Formal Proof for Square Root of 2 is Irrational
author: sighingnow
date: 2016-03-18
tags: [Proof, Logic]
category: Coq
layout: post
---

It was one of the most surprising discovers of the Pythagorean, a famous Greek mathematicians, that there are irrational
numbers. The square root of $2$ sometimes has the name _Pythagora's Constant_:

$$\sqrt{2} = 1.4142135...$$

<!--more-->

As we all know that $\sqrt{2}$ is irrational and there's a classic example of _proof by contradiction_ for this proposition.
Here I will present how to prove it and how to formalize the proof in Coq.

Proof by Contradiction
----------------------

In logic, proof by contradiction is a form of indirect proof, that a proposition is proved by assuming that the oppsite of
the proposition is true and showing the assuming is false, thereby implying the proposition must be true.

Suppose $\sqrt{2}$ is rational, according to the definition of irrational there exists two coprime integers $p$, $q (q > 0)$ and
$\sqrt{2} = \frac{p^2}{q^2}$ holds. It follows that $$\frac{p^2}{q^2} = 2 \text{  and  } p^2 = 2 q^2.$$
**Therefore $p$ must be even as squares of odd integers can't be even.** So there exists an integer $k$ fulfills $p = 2 k$.
Do substitution and simplication then we get $q^2 = 2 k^2$. We know that $q^2$ is even so $q$ is also even.
Because $p$ and $q$ are both even, they actually have a common factor $2$, which is a contradiction to our assumption that
$p$ and $q$ are coprime integers.

Now we get the conclusion that our assumption of $\sqrt{2}$ is rational is wrong and $\sqrt{2}$ muse be irrational. Q.E.D.

Formal Proof in Coq
-------------------

Coq is so powerful that we can represent the proposition $\sqrt{2}$ is irrational as the following:

~~~coq
Theorem sqrt_2_irrational :
  ~ exists p q : Z, (q > 0)%Z /\ (IZR p / IZR q)%R = x.
~~~

We can also defintion a function takes a real number $x$ and return a proposition that $x$ is irrational, then get the
goal we want to prove using this function.

~~~coq
Definition irrational (x : R) : Prop :=
  ~ exists p q : Z, (q > 0)%Z /\ (IZR p / IZR q)%R = x.

Theorem sqrt_2_irrational :
  irrational (sqrt 2%R)%R.
~~~

According to the introduction rule for $\exists$, we can set up assumptions and transform our goal the `False`. After run
the tactic `intros [p [q [q_gt_0_Z p'q_eq_sqrt_2_Z]]].`, we get the following context:

~~~
1 subgoal
p, q : Z
q_gt_0_Z : (q > 0)%Z
p'q_eq_sqrt_2_R : (IZR p / IZR q)%R = sqrt 2
______________________________________(1/1)
False
~~~

Coq is an amazing tools and we can construct a formal proof by represent the above proof directly in Coq. The core of the
mathematical proof above is that we draw the conclusion that $p$ must be even then $q$ must be even, then we can get the
contradiction. Why ? What's the underlying facts involved here ?

The real reason is that if $p$ is even, in other words $p$ is divisible by $2$, then $p^2$ must be divisible by $4$!
Now we can conclude that for every integers in binary radix the number of tailing zeros of $p^2$ must even. Next, we
formalize this principle both for $p$ and $q$:

~~~coq
  assert (ctz_p : Nat.even (p*p) = true).
  assert (ctz_q : Nat.even (q*q) = true).
~~~

It's not hard to obtain `p*p = 2*q*q` from the premise `p'q_eq_sqrt_2_R`, but now I want to concentrate on how to prove the
`False` and I'll mention this again later. A trival fact we can deduce from $p*p=2*q*q$ is the count of tailing zeros
of $p*p$ is one more than $2*q*q$. In Coq, this fact can be formalized and proved as
the follows (we also need another lemma to finish this proof):

~~~coq
  assert (double_inc_ctz : forall x, (x <> 0%Z) -> ctz_Z (2 * x) = S (ctz_Z x)).
    destruct x; intuition || intro; reflexivity.

  assert (ctz_)

  assert (ctz_p_eq_S_ctz_q : ctz_Z (p*p) = S (ctz_Z (q * q))).
    rewrite <- double_inc_ctz.
    rewrite pp_eq_2qq_Z; rewrite Z.mul_assoc; reflexivity.
    assert (q*q > 0)%Z.
      auto with zarith.
    auto with zarith.
~~~

Another obvious fact is that for every integer $x$, $x$ and $2*x$ can't be both even or both odd, as well as $p*p$.

~~~coq
Lemma cannot_both_even :
  forall x, Nat.even x = negb (Nat.even (S x)).
Proof.
  induction x.
  + reflexivity.
  + simpl (Nat.even (S (S x))).
    rewrite IHx; rewrite negb_involutive; reflexivity.
Qed.
~~~

Go over current context again we will find we are in the following condition:

~~~
1 subgoal
p, q : Z
ctz_p : Nat.even (ctz_Z (p * p)) = true
ctz_q : Nat.even (ctz_Z (q * q)) = true
ctz_p_eq_S_ctz_q : ctz_Z (p * p) = S (ctz_Z (q * q))
______________________________________(1/1)
False
~~~

It's easy to deduce these three premises by some steps `rewrite`:

~~~coq
  rewrite ctz_p_eq_S_ctz_q in ctz_p.
  rewrite cannot_both_even in ctz_q.
  rewrite ctz_p in ctz_q.
~~~

Then we get `ctz_q : negb true = true`. It's just the contradiction we want and the theorem of $\sqrt{2}$ is rational is
proved completely.

It's time to look back at the immediate conclusion `pp_eq_2qq_Z`. Because $\sqrt{2}$ is real number but $p$ and $q$ are
integers some conversions is need to illustrate the conclusion holds. It's somewhat triky and the whole proof can't
be found at [sqrt_2_irrational.v]({{site.url}}/resource/a_formal_proof_for_sqrt_2_is_irrational/sqrt_2_irrational.v).

Summary
-------

It seems make no sense of formalizing such a trival theorem using Coq with our efforts, but it's so inspried to witness
the expressive power of programming language and to forsee the feature of automated theorem proving!

