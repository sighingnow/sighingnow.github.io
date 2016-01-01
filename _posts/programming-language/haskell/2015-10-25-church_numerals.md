---
title: Church Numerals
author: He Tao
date: 2015-10-25
tag: [Haskell]
category: 编程语言
layout: post
---

在一个可以时过程做各种操作的语言里，我们完全可以没有输(至少在只考虑非负整数的情况下) ，可以将 “0” 和 “加一” 操作实现为：

~~~scheme
;; Church numeral: 0
(define zero
  (lambda (f)
    (lambda (x) x)))

;; Chruch numeral: +1
(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))
~~~

这一表示形式称为Church计数，各宇来醺于其发明人数理逻辑学家Alonzo Church (丘奇) ，lambda 演算也是他发明的。
**丘奇数(Church numeral)，并不是一个数字，而是指应用某个函数的次数。** (这两个参数可以组合起来，但是，formal lambda
calculus只允许使用一个参数。)

<!--more-->

SICP Exercise 2.6
-----------------

SICP Exercise 2.6 的题目要求：

> Define one and two directly (not in terms of zero and add-1). (Hint: Use substitution to evaluate (add-1 zero)). Give a direct definition of the addition procedure + (not in terms of repeated application of add-1).

一个丘奇数种包括两部分：`f`和`x`。`f`指的是将会被应用(apply) n 次的函数，x 指的是函数`f`的操作数。例如：Church numeral 的 4 的表示：

~~~scheme
;; 4
(lambda (f)
  (lambda (x)
    (f (f (f (f x))))))
~~~

回到 SICP Exercise 2.6, 用这种形式来表达 1，2：

~~~scheme
;; 1
(define one
  (lambda (f)
    (lambda (x)
      (f x))))

;; 2
(define two
  (lambda (f)
    (lambda (x)
      (f (f x)))))
~~~

那么就如何实现 Church Numerals 的 `add` 运算呢？参考 `(+1)`的实现，不难得到：

~~~scheme
;; `add`
;; Add a and b by applying a to the result of applying b.
(define (add a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))
~~~







