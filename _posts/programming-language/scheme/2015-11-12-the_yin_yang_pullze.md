---
title: The Yin-Yang puzzle
author: Tao He
date: 2015-11-12
tag: [Scheme]
category: 编程语言
layout: post
---

所谓的 Yin-Yang puzzle，指的是这样一段Scheme代码：

~~~scheme
(let* ((Yin
         ((lambda (cc) (display #\@) cc) (call/cc (lambda (c) c))))
       (Yang
         ((lambda (cc) (display #\*) cc) (call/cc (lambda (c) c)))) )
    (Yin Yang))
~~~

执行这段代码的输出：

    @*@**@***@****@*****@******@*******@********@********* ...

<!--more-->

执行过程分析
-----------

假设`Yin`的`call/cc`捕获的Continuation分别为$N_1, N_2, \dots$，$N_i$的执行相当于：

1. 通过`let*`进行`Yang`绑定；
2. 执行`(Yin Yang)`调用。

`Yang`的`call/cc`捕获的Continuation分别为$G_1, G_2, \dots$，$G_i$的执行相当于：

1. 执行`(Yin Yang)`调用。

执行完`let*`绑定之后，`Yin`、`Yang`两个变量实际上是两个Continuation(可以在`(Yin Yang)`之前加入`(display Yin/Yang)`语句来观察)，用元组$(C_a, C_b)$来
表示`Yin`和`Yang`在某一时刻的状态。执行`Yin`和`Yang`所代表的Continuation所产生的副作用输出分别为：

+ `Yin`：输出`@`
+ `Yang`：输出`*`

对于语句`(Yin Yang)`，函数调用产生的效果相当于先跳转到绑定到`Yin`的Continuation产生的位置，恢复到那个时刻的状态，接下来的执行`let*`绑定，如果跳转到到`Yin`中，则把参数`Yang`当前绑定的Continuation值绑定到`Yin`上，如果跳转到`Yang`则将其绑定到`Yang`。
也就是说，`(Yin Yang)`包含了两步对状态$(C_a, C_b)$的改变。同时注意到这段代码中，只有运行`let*`绑定才会生成新的Continuation。

对于一个Continuation，执行Continuation就意味着跳转到程序中产生Continuation的地方执行，程序中变量的值、状态也变成产生这个Continuation的状态。根据这一原理，不难分析出这段程序的执行过程如下表所示：

| No. | 执行操作     | 执行  | 副作用(累积)       | 产生  | 旧状态$(Yin, Yang)$ | 新状态$(Yin, Yang)$ |
|:---:|--------------|:-----:|--------------------|:-----:|:-------------------:|:-------------------:|
| 1   | `let* Yin`   |       | `@`                | $N_1$ | $(null, null)$      | $(N_1, null)$       |
| 2   | `let* Yang`  |       | `@*`               | $G_1$ | $(N_1, null)$       | $(N_1, G_1)$        |
| 3   | `(Yin Yang)` | $N_1$ | `@*@`              |       | $(N_1, G_1)$        | $(G_1, null)$       |
| 4   | `let* Yang`  |       | `@*@*`             | $G_2$ | $(G_1, null)$       | $(G_1, G_2)$        |
| 5   | `(Yin Yang)` | $G_1$ | `@*@**`            |       | $(G_1, G_2)$        | $(N_1, G_2)$        |
| 6   | `(Yin Yang)` | $N_1$ | `@*@**@`           |       | $(N_1, G_2)$        | $(G_2, null)$       |
| 7   | `let* Yang`  |       | `@*@**@*`          | $G_3$ | $(G_2, null)$       | $(G_2, G_3)$        |
| 8   | `(Yin Yang)` | $G_2$ | `@*@**@**`         |       | $(G_2, G_3)$        | $(G_1, G_3)$        |
| 9   | `(Yin Yang)` | $G_1$ | `@*@**@***`        |       | $(G_1, G_3)$        | $(N_1, G_3)$        |
| 10  | `(Yin Yang)` | $N_1$ | `@*@**@***@`       |       | $(N_1, G_3)$        | $(G_3, null)$       |
| 11  | `let* Yang`  |       | `@*@**@***@*`      | $G_4$ | $(G_3, null)$       | $(G_3, G_4)$        |
| 12  | `(Yin Yang)` | $G_3$ | `@*@**@***@**`     |       | $(G_3, G_4)$        | $(G_2, G_4)$        |
| 13  | `(Yin Yang)` | $G_2$ | `@*@**@***@***`    |       | $(G_2, G_4)$        | $(G_1, G_4)$        |
| 14  | `(Yin Yang)` | $G_1$ | `@*@**@***@****`   |       | $(G_1, G_4)$        | $(N_1, G_4)$        |
| 15  | `(Yin Yang)` | $N_1$ | `@*@**@***@****@`  |       | $(N_1, G_4)$        | $(G_4, null)$       |
| 16  | `let* Yang`  |       | `@*@**@***@****@*` | $G_5$ | $(G_4, null)$       | $(G_4, G_5)$        |

通过分析程序的运行过程，可见，Continuation $N_i$值产生了一次，在最初的`let* Yin`和`let* Yang`绑定完成之后，只有执行Continuation $N_1$，才会导致运行`let* Yang`，生成一个新的Continuation $G_i$。
新生成的$G_i$绑定到`Yang`，按照 Contination 的执行规则，当`Yin`上绑定的Continuation重新迭代到$N_1$时，再次生成新的$G_i$。

如果要实现相同的效果，只需要通过一个二重循环来表示$(Yin, Yang)$状态跳转即可：

~~~python
def YinYang(limit):
    for i in range(1, limit):
        print('@', end='')
        for j in range(i):
            print('*', end='')
YinYang(10)
~~~

在Haskell这样的能够表示无限数据结构的编程语言中，可以更好地表示这一程序逻辑：

~~~haskell
YinYang = concat [['@'] ++ ['*' | j <- [0..i]] | i <- [0..]]
~~~

一个能够达到同样的效果的Scheme实现：

~~~scheme
(define (A a)
  (display "@")
  (display "*")
  (a (lambda (aa) (display "*") (a aa))))
(A A)
~~~

