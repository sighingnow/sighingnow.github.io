---
title: Scheme 中的 Pairs 和 Lists
author: Tao He
date: 2015-07-03
tag: [Scheme]
category: 编程语言
layout: post
---

Pairs 和 Lists 是 Scheme中的重要的数据类型。

<!--more-->

Pairs and lists
----------------

> A pair (sometimes called a dotted pair) is a record structure with two fields called the `car` and `cdr` fields (for historical reasons).
Pairs are created by the procedure `cons`. The `car` and `cdr` fields are accessed by the procedures `car` and `cdr`.
The car and cdr fields are assigned by the procedures `set-car!` and `set-cdr!`.

点值对是将两个任意数值组合成有序数偶的复合类型。点值对的第一个数值被称作`car`，第二值被称作`cdr`，而将两个值组合成点值对的过程是`cons`。点值对的元素可以通过修改器过程`set-car!`和`set-cdr!`来进行修改：

    > (define x (cons 1 #t))
    > x
    (1 . #t)
    > (car x)
    1
    > (cdr x)
    #t
    > (set-car! x 2)
    > (set-cdr! x "abcde")
    > x
    (2 . "abcde")

点值对也可以包含其它的点值对。(Dotted pairs can contain other dotted pairs.)

    > (define y (cons (cons 1 2) 3))
    > y
    ((1 . 2) . 3)

当第二个元素是一个嵌套的点值对时，Scheme使用一种特殊的标记来表示表达式的结果：

    > (cons 1 (cons 2 (cons 3 (cons 4 5))))
    (1 2 3 4 . 5)

即，`(1 2 3 4 . 5)`是对`(1 . (2 . (3 . (4 . 5))))`的一种简化。

> Pairs are used primarily to represent lists. A list can be defined recursively as either the empty list or a pair whose cdr is a list.

表中连续各点对的car 域内的对象是表的元素。例如，一个拥有两个元素的表是一个点对，该点对的car 域包含表的第一个元素，其cdr 域又是一个点对，这个点对的car域包含表的第二个元素， cdr 域是空表。表的长度是元素的数量，也等于其点对的数量。

**空表是一个隶属于其自身类型的特殊对象（它不是点对），它不包含任何元素，长度为零。**

关于List和Pair的更精确的形式化定义：

The set of lists is defined as the smallest set X such that

    + The empty list is in X.
    + If list is in X, then any pair whose cdr field contains list is also in X.

参考
---

1. [R5RS, Chapter 6](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.2)

