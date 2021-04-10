---
title: Church Encoding
author: Tao He
date: 2015-10-25
tag: [Scheme]
category: 编程语言
layout: post
---

丘奇编码是把数据和运算符都嵌入到 lambda 演算中的一种方式，整数、布尔值、序对、列表等都可以映射到使用丘奇编码的高阶函数中。

<!--more-->

丘奇数
-----

在一个可以时过程做各种操作的语言里，我们完全可以没有数(至少在只考虑非负整数的情况下) ，可以将 “0” 和 “加一” 操作实现为：

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

这一表示形式称为Church计数，命名来源于其发明人数理逻辑学家 Alonzo Church (丘奇，同时也是 lambda 演算的作者)。
**丘奇数(Church numeral)，并不是一个数字，而是指应用某个函数的次数**。丘奇数是 Church 编码下自然数的表示法，
表示自然数 $n$ 的**高阶函数**是把函数 $f$ 映射到其 $n$ 重函数复合

$$f^n = f \circ f \circ f \dots \circ f$$

的函数。

SICP Exercise 2.6
-----------------

SICP Exercise 2.6 的题目要求：

> Define one and two directly (not in terms of zero and add-1). (Hint: Use substitution to evaluate (add-1 zero)).
> Give a direct definition of the addition procedure + (not in terms of repeated application of add-1).

一个丘奇数种包括两部分：`f`和`x`。`f`指的是将会被应用(apply) n 次的函数，x 指的是函数`f`的参数。例如：Church numeral 的 4 的表示：

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

通过这一原理，直接实现 Church Numerals 的 `add` 运算。不难得到：

~~~scheme
;; `add`
;; Add a and b by applying a to the result of applying b.
(define (add a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))
~~~

自然数运算
---------

其他的自然数的运算的 Church 计数表示：

+ 加法函数(利用恒等式 $f^(m+n)(x) = f^m(f^n(x))$)

~~~scheme
(define plus
  (lambda (m)
    (lambda (n)
      (lambda (f)
        (lambda (x)
          ((m f) ((n f) x)))))))
~~~

+ 后继函数

~~~scheme
(define succ
  (lambda (n)
    (lambda (f)
      (lambda (x)
        (f ((n f) x))))))

(define plus-with-succ
  (lambda (m)
    (lambda (n)
      ((m succ) n))))
~~~

+ 乘法函数(利用恒等式 $f^(m \times n) = (f^m)^n$)

~~~scheme
(define mult
  (lambda (m)
    (lambda (n)
      (lambda (f)
        (m (n f))))))
~~~

+ 指数函数

~~~scheme
(define exp
  (lambda (m)
    (lambda (n)
      (n m))))
~~~

+ 前驱函数

$$pred(n) = {\begin{cases}
    0   & {\text{if } n = 0} \\
    n-1 & {\text{otherwise}}
\end{cases}}$$

~~~scheme
(define pred
  (lambda (n)
    (lambda (f)
      (lambda (x)
        (n (lambda (g)
             (lambda (h)
               (h (g f))))
           (lambda (u) x)
           (lambda (u) u))))))
~~~

+ 减法函数($m - n = pred^n (m)$ )

~~~scheme
(define minus
  (lambda (m)
    (lambda (n)
      ((n pred) m))))
~~~

+ 丘奇数和自然数之间的转换

~~~scheme
(define integer->church
  (lambda (n)
    (if (= n 0) zero (succ (integer->church (- n 1))))))

(define church->integer
  (lambda (hf)
    ((hf (lambda (x)
          (+ x 1))) 0)))
~~~

undefined
---------

因为$\bot$与non-termination computation不可区分，因此，使用后者来编码$\bot$，用以表示错误(error)。

~~~scheme
(define error
  (lambda () ((lambda (f) (f f)) (lambda (f) (f f)))))
~~~

布尔值和布尔运算
--------------

+ 布尔值的丘奇编码

~~~scheme
(define true  (lambda (on-true on-false) on-true))
(define false (lambda (on-true on-false) on-false))
~~~

+ 布尔运算

~~~scheme
(define and (lambda (m n) ((m n) false)))
(define or  (lambda (m n) ((m true) n)))
(define not (lambda (m)   ((m false) true)))
(define xor (lambda (m n) ((m (not n)) n)))
~~~

元祖(pair)
---------

~~~scheme
(define pair
  (lambda (x)
    (lambda (y)
      (lambda (z)
        ((z x) y)))))

(define first
  (lambda (p)
    (P true)))

(define second
  (lambda (p)
    (p false)))
~~~

列表(list)
----------

~~~scheme
(define nil (lambda (on-null on-pair) on-null))
(define cons (lambda (x xs) (lambda (on-null on-pair) (on-pair x xs))))

(define head (lambda (list)
               (list (lambda () (error))
                     (lambda (x xs) x))))
(define tail (lambda (list)
               (list (lambda () (error))
                     (lambda (x xs) xs))))
~~~

参考
---

1. [Chruch Encoding on Wikipedia](https://en.wikipedia.org/wiki/Church_encoding)

