---
title: Continuation 和 call/cc
author: Tao He
date: 2015-09-11
tag: [Scheme]
category: 编程语言
layout: post
---

维基百科关于 Continuation 的定义：

> The continuation is a data structure that represents the computational process at a given point
> in the process's execution; the created data structure can be accessed by the programming
> language, instead of being hidden in the runtime environment.

Continuation，延续性，是对程序的控制状态/流程的抽象表达，Continuation 使得程序的控制流程具体化。

Current continuation，也就是 continuation of the computation step，指的是源自于程序当前运行点的一种
continuation。Scheme中，在代码某处调用call/cc后，产生了一个等待着参数的过程，这个参数是程序在该处的上下文，
在参数之后的程序就叫做 current continuation。Continuation 一词也指 first-class continuations,
指的是编程语言中使得编程语言具有保存任意一点的运行状态，并从之后的程序中回到这一状态(可能多次返回)的可能
的结构和概念。

<!--more-->

First-class continuations
--------------------------

First-class continuations 指的是一门编程语言能够完全控制程序的执行顺序的能力。程序既可以跳转到一个将要调用
当前函数的函数，也可以跳转到之前已经退出的函数。first-class continuation可以被看作是保存程序的执行状态(仅仅
是上下文，而不是程序数据，区别于 processing image)。First-class, 是指 Continuation 可以被当作参数传递和
作为返回值。

一个解释 continuation 与普通的程序调用和返回的区别的例子：

> Say you're in the kitchen in front of the refrigerator, thinking about a sandwich. You take a
> continuation right there and stick it in your pocket. Then you get some turkey and bread out of
> the refrigerator and make yourself a sandwich, which is now sitting on the counter. You invoke
> the continuation in your pocket, and you find yourself standing in front of the refrigerator
> again, thinking about a sandwich. But fortunately, there's a sandwich on the counter, and all the
> materials used to make it are gone. So you eat it.

**此处，我们并没有调用一个用来 make sandwich 的函数并返回，而是调用了一个 make sandwich with current
continuation 的函数，然后 create the sandwish，最后返回到之前离开时的continuation(fron of the refrigerator)。**

Scheme的call/cc
---------------

Scheme是第一个提供Continuation支持的产品级编程语言，Scheme提供了使用call/cc(call-with-current-continuation)
的控制流运算符,call/cc的参数是只能接受一个参数的函数。在Scheme中，Continuation被表达为一个函数，
假设 call/cc 捕捉了当前的 continuation，并绑定到 lambda 的参数 cc，那么**在 lambda 函数体内**，一旦 cc
被**直接或间接**的作为过程调用，那么 call/cc 会立即返回，并且提供给 cc 的参数即为 call/cc 的返回值。

> In the name "call‐with‐current‐continuation", "call" refers to the way a function is called
> to hand over the continuation. Don't be confused by the fact the continuation object is
> later invoked by calling it, that's entirely separate.

call/cc 本质上其实是非本地返回(non-local return)，其他的例如 setjump/longjump, exception 等机制也属于 non-local
return 的范畴。call/cc 机制主要用来实现一些复杂的流程控制结构。Scheme并没有提供像C语言那样的break语句，可以用
call/cc来实现退出函数的功能。在过程的入口调用call/cc，将这个函数体都放在call/cc的参数里。在需要中途退出的地方
参数调用continuation，就可以直接退出函数。**本质上是借助调用continuation将会把上下文设置为执行call/cc的位置，即
调用call/cc之后的下一条语句。**

> Note that Scheme does not syntactically distinguish continuation application from function application.

例如，

~~~scheme
(define (test e cc) (if (zero? e) (cc "find zero")))

(define (search-zero test lst)
  (call-with-current-continuation
    (lambda (ret) (for-each (lambda (e) (test e ret) (display e)) lst))))

(display (search-zero test '(-3 -2 -1 0 1 2 3)))
~~~

执行的输出结果(R5RS)：

    -3-2-1find zero

我们使用 call/cc 来实现一个功能类似于 Haskell 的 product 的函数：

~~~scheme
(define product
  (lambda (ns)
    (call-with-current-continuation
      (lambda (exit)
        (let f ((ns ns))
          (cond ((null? ns) 1)
                ((= (car ns) 0) (exit "zero"))
                (else (* (car ns) (f (cdr ns))))))))))

(product '(1 2 3 4 5 6))
(product '(1 2 3 0 4 5 6))
~~~

为了方便叙述，我们称包含call/cc的函数为callee，在该函数外部，无参数调用continuation的函数为caller。
关于call/cc的代码执行流程，在**首次运行callee函数时，cc会被赋值，用于保存一个上下文环境**。当在caller中无
参数调用continuation时，拿之前保存的上下文环境来运行call/cc的参数函数，并一直执行到函数的callee函数的末尾，
以callee函数的值作为返回值，返回给caller函数，call函数继续执行。下一次在caller中无参数调用continuation，
接着用之前保存的上下文环境来运行call/cc的参数函数，知道callee末尾，返回值给caller。下面的例子可以很好地
说明这个过程：

~~~scheme
(define the-continuation #f) ; dummy value - will be used to store continuation later

(define (func)
  (let ((x 0))
    (call-with-current-continuation
      (lambda (cc) (set! the-continuation cc)))  ; set cc to the continuation.
    (set! x (+ x 1)) x))

(func)
(the-continuation)
(the-continuation)
(the-continuation)
~~~

运行结果：

    1   2   3   4

如果调用(the-continuation)之前不先调用(func)，the-continuation就无法被初始化，后面调用(the-continuation)就会出错。

call/cc 的引用透明性
------------------

引用透明性(referential transparent)是纯函数式编程语言的重要特性，而是用call/cc，可以写出非引用透明性的函数：

    (define (get-cc) (call/cc (lambda (c) c)))

get-cc 函数捕捉到当前的 continuation，然后返回，显然，这个函数的返回值会受到上下文的影响。

    Welcome to DrRacket, version 6.2 [3m].
    Language: R5RS; memory limit: 128 MB.
    > ((get-cc) 10)
    . . application: not a procedure;
     expected a procedure that can be applied to arguments
      given: 10
      arguments.:
    > (define x (get-cc))
    > x
    #<continuation>
    > (x 10)
    > x
    10

**(get-cc) 获取它这个位置上的 continuation， (get-cc) 自己被用来做了什么事，它返回的 continuation 就
对别人做同样的事。** 经过define之后，x 获得一个 continuation，这个continuation 的作用就是获取一个值，
然后返回这个值，当以参数 10 来调用 x 时，continuation 返回一个值：10，并把这个值绑定到 x，x 被重新绑定，
变成了数字10。但是直接以((get-cc) 10) 来调用的时候，(get-cc) 被当成了函数调用，显然就会出现错误了。

(get-cc) 的非引用透明性来源于它的语义，它总是捕捉当前的 continuation 并返回之。可以这么理解 call/cc，它
可以出现在任何一个本应是表达式的地方（它占了表达式的位置）。凡是表达式都要求值，并且还要求它的后续表达式的
值，我们通过call/cc，可以在该表达式出现的地方捕捉（catch）到该表达式的后续操作。被捕捉到的后续操作即为
continuation，调用捕捉到的 continuation 可以回到过去。但是注意调用 continuation 和非本地退出的区别，后者
是在 call/cc 的函数体内（直接或间接）调用捕捉到的 cc，这是 continuation 的特殊用法，它能立即退出，而且可以
在非本地退出；而前者是在相应 continuation 的 call/cc 之外调用，它的作用就是重复后续操作。

+ (let ((x (get-cc))) (x (lambda (unused) "result")))
+ (((get-cc) (lambda (x) x)) "result")

根据上面的原理不难理解，这两个表达式的值都是 `"result"`。

call/cc 模拟多任务
-----------------

多任务控制流的一个关键就是，保存每个任务的上下文，让它切出去再返回的时候能接着执行，就像没有发生过切换一样。
这个任务，continuation 完全胜任。生产者-消费者问题是检验多任务机制的经典问题，我们可以用 continuation
模拟这个过程。利用call/cc来捕获上下文，利用调用continuation来在不同的上下文之间切换。

~~~scheme
#lang racket

(define dish #f)

(define (put! fruit) (set! dish fruit))
(define (get!) (let ((fruit dish)) (set! dish #f) fruit))

(define (consumer do-other-stuff)
  (let loop ()
    (when dish
      (printf "C: get ~a~%" (get!))
      (set! do-other-stuff (call/cc do-other-stuff))
      (loop))))

(define (producer do-other-stuff)
  (for-each (lambda (fruit)
              (put! fruit)
              (printf "P: put ~a~%" fruit)
              (set! do-other-stuff (call/cc do-other-stuff)))
            '("a" "b" "c" "d" "e")))

(producer consumer)
~~~

call/cc的实现
-------------

> In any case, continuations can be used in any language with closures by manually writing in continuation passing style.

在Scheme中，运行效率更高的continuation可以通过更低级的直接操作栈空间的方法来实现，但这仅仅是一种优化。在Scheme
中，当我们使用了 CPS 后，call/cc可以被如下的等价表达式替代：

    (lambda (f k) (f (lambda (v k0) (k v)) k))

在这个式子中，`k` 是需要保存的continuation，`(lambda (v k0) (k v))` 用来重新保存(restore) continuation。

[Call-with-current-continuation for C programmers](http://community.schemewiki.org/?call-with-current-continuation-for-C-programmers)
一文介绍了C语言中的 setjump/longjump 机制与 continuation 的异同，并从更加 low-level 的方式阐述了大多数主流
Scheme 解释器的 call/cc 的实现细节。Continuation 操作程序控制流的原理与命令式语言中的`goto`有着本质的不同。
[Parent pointer tree](https://en.wikipedia.org/wiki/Parent_pointer_tree) (也作Spaghetti stack) 就是编译器
中实现call/cc，进行垃圾回收的一种方法。在并发领域，Coroutine就是基于Continuation实现的。Continuation可认为
是对PCB的抽象，其实它就是函数当前的执行栈，并且是实实在在可以被保存的东西，因此，很容易通过CPS来实现协程、
non-local-return 等。

参考
----

1. [Wikipedia Continuation](https://en.wikipedia.org/wiki/Continuation)
2. [Wikipedia Call with Current Continuation](https://en.wikipedia.org/wiki/Call-with-current-continuation)
3. [Call-with-current-continuation for C programmers](http://community.schemewiki.org/?call-with-current-continuation-for-C-programmers)

