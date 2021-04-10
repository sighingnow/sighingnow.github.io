---
title: Continuation Passing Style
author: Tao He
date: 2015-09-12
tag: [Scheme]
category: 编程语言
layout: post
---

CPS(Continuation Passing Style, 后续传递风格), 是指将程序的控制流显示地用 Continuation 来传递，而不是像
direct style 那样明确指出下一条语句的编码风格。符合 CPS 的函数需要一个额外的参数：一个显式的Continuation
(通常为一个只有一个参数的函数)。一个CPS函数通过以返回值为参数，调用Continuation的方式来返回函数值。当我们调
用一个CPS函数式，被调用的函数需要一个额外的`return procedure`来供被调用的函数使用。用CPS的方式来表达代码可
以使普通程序中很多不明确的内容变得明确。例如，函数的返回值看起来就像是对Continuation的调用，参数的求值顺序也
变得明确，同时，尾调用(tail call)也被直接传递给调用者(caller)。

一个函数可以被自动地从 direct style 转换成 continuation passing style。当在命令式或者过程式语言的编译器中
需要表达SSA(事实上，SSA可以被视作CPS的一个子集)时，编译器可以使用CPS来表达中间代码。

<!--more-->

正常函数的返回都隐含一个continuation，就是利用这个函数的返回值来 做的后续事情，而cps的本质就是将这个隐式的
continuation显式的当做参数传递进去，并在函数中完成应有的continuation并将最终结果返回。这跟尾递归似乎很像，
在改造递归为尾递归的时候，就将当前状态通过accumulator汇集到函数内部的操作，当达到结束条件时返回汇集结果，而
不必再返回来收集递归过程中的返回值。cps似乎就是同样的道理，每次将continuation传递到内部进行操作的组合，当达
到底部的时候直接将汇集的continuation的计算结果返回，而不必返回来再去计算每一步的continuation。

The key to CPS:

+ every function takes an extra argument, its continuation
+ every argument in a function call must be either a variable or a lambda expression

> This means that each function consumes a function that represents the rest of the computation relative to
> this function call. To return a value, the function calls this "continuation function" with a return value;
> to abort the computation it returns a value.

以阶乘函数为例：

~~~scheme
(define (factorial n)
  (if (= n 0)
     1     ; NOT tail-recursive
     (* n (factorial (- n 1)))))
~~~

改成CPS：

~~~scheme
(define (factorial& n k)
  (=& n 0 (lambda (b)
          (if b                    ;; growing continuation
              (k 1)                ;; in the recursive call
              (-& n 1 (lambda (num)
                       (factorial& num (lambda (f)
                                        (*& n f k))))))))) ;; apply continuation `k` to the result.
~~~

参数 k 表示对于 `factorial&` 函数的结果的行为，即Continuation。从这个例子中，我们不难看到，所谓CPS，其实传入
一个函数，返回这个函数对被调用者的结果(可以是多个)处理后的值。我们可以以如下形式来调用 `factorial&`函数：

    (factorial& 10 (lambda (x) x))

在尾递归的程序中，我们使用了一个变量(acc)来保存乘法计算结果的累积，而在CPS版本的factorial函数中，
我们使用一个contination(k)来保存需要对结果进行的处理的累积。More precisely, k is a function that takes
the list that factorial& would otherwise return, and does something to it to get the real final result.

需要注意的是，显式地调用了contination的语句之后的代码都不会被执行到。

CPS形式的map
-----------

用Scheme实现map：

~~~scheme
(define map1
  (lambda (f ls)
    (if (null? ls)
        '()
        (cons (f (car ls)) (map1 f (cdr ls))))))
~~~

改写成CPS形式：

~~~scheme
(define map2
  (lambda (f ls k)
    (if (null? ls)
        (k '())
        (map2 f (cdr ls) (lambda (v)
                           (f (car ls) (lambda (n)
                                         (k (cons n v)))))))))
~~~

其中，f 是需要map的函数，ls是列表，k是Continuation，需要注意的是，f本身也是一个CPS风格的函数，可以这样调用这个函数：

    (map2 (lambda (x k) (k (* x 2))) '(1 2 3) (lambda (x) (display x) x))

call/cc 和 CPS
-------------

理论上，所有使用了call/cc的函数，都可以使用CPS来重写，但是有些时候难度很大，而且有时候要修改Scheme所提供的基础函
数（primitives）。

例如，Scheme实现的阶乘函数：

~~~scheme
(define product
  (lambda (ns)
    (call-with-current-continuation
      (lambda (exit)
        (let f ((ns ns))
          (cond ((null? ns) 1)
                ((= (car ns) 0) (exit 0))
                (else (* (car ns) (f (cdr ns))))))))))
~~~

接下来，将这个函数改成CPS的形式。首先，将call/cc的调用从函数体中去掉，然后为product函数加上一个参数k，
k为接受一个参数的函数。另外，因为product增加了一个参数，因此对f这个命名let也需要增加一个参数。最后，
在f的body里面调用f，也需要改写成CPS形式。因为对f的调用不是尾部调用，因此在f返回之前，需要进行计算，然后才是
对该结果进行下一步的计算。此时需要的后续计算为：

    (lambda (v) (k (* (car ls) v)))

对于cond的每个分支，都需要对其结果进行后续的k计算。至此，我们得到了product函数的CPS版本：

~~~scheme
(define product-cps
  (lambda (ns k)
    (let f ((ns ns) (k k))
      (cond ((null? ns) (k 1))
            ((= (car ns) 0) (k 0))
            (else (f (cdr ns)
                     (lambda (x) (k (* (car ns) x)))))))))
~~~

调用这个函数：

    (product-cps '(1 2 3 4 5 6) (lambda (x) x))

CPS变换
-------

CPS程序与普通程序相比有如下明显的不同：

1. CPS函数都有一个额外的参数 k ，表示控制流。函数需要返回，必须显式的调用 k .
2. 在函数的末尾调用了另外一个函数，这种调用称为尾调用，tail call。相应的在尾部递归调用，称之为尾递归，tail
recursion。CPS所有函数都是尾调用(在不支持尾递归的语言中，可能出现栈溢出)。

将普通函数变换为CPS程序，主要有以下几种类型：

+ 普通函数

~~~javascript
function foo(x) {
    return x + 1;
}
function cps_foo(x k) {
    (function (k1) {
        k1(x+1)
    })(function (x1) {
        k(x1);
    });
}
~~~

+ 有赋值语句的：

~~~javascript
function foo(x) {
    var a = 1;
    return x+a;
}
function cps_foo(x, k) {
    var a;
    (function (k1) {
        a = 200;
        k1();
    })(function () {
        (function (k2) {
            k2(a+x);
        })(function (x2) {
            k(x2);
        });
    });
}
~~~

+ 有分支结构的：

~~~javascript
function foo(x) {
    if(x > 100) {
        console.log("x > 100");
    }
    else {
        console.log("x <= 100");
    }
}
function cps_foo(x k) {
    (function (k1) {
        k1(x > 100);
    })(function (b) {
        function k2() {
            k(x);
        }
        if(b) {
            (function (k3) {
                console.log("x > 100");
                k3();
            })(k2);
        }
        else {
            (function (k4) {
                console.log("x <= 100");
                k4();
            })(k2);
        }
    });
}
~~~

+ try/catch 结构

~~~javascript
function goo(x) {
    function foo(a, b) {
        if(a < b) {
            return a - b;
        }
        else {
            throw "a must gt b";
        }
    }

    try {
        x = foo(x, 100);
    } catch (ex) {
        console.log("invalid parameter.");
        return;
    }
    return x;
}
function cps_goo(x, k) {
    (function (a, b, k, thro) {
        if(a > b) {
            k(a-b);
        }
        else {
            thro("a must be gt b");
        }
    })(function (result) {
        x = result;
        k(x);
    }, function(ex) {
        console.log("invalid parameter");
        k();
    });
}
~~~

翻译成CPS的表达式，有一种inside-out的效果，也就是说，表达式最内部的部分需要最先被计算出来。区别于
普通的最先被计算在最外层。CPS变换的一些策略：

+ 每一条语句都被包装在一个函数内。原函数内剩下的语句被包装在 continuation中
+ 最终每个函数内只做一件不能在被分割的事情（譬如+，-，*，/ 或者调用系统API等）
+ 每个函数实际上只关心传入自身的continuation参数

CPS中，我们没有在调用return了，控制流必须显式通过continuation传递。`return` 语句只是一个语法糖而已。
Exception仅仅是一个特殊的continuation而已。`try/catch` 也可以被视作是语法糖。犹他大学的课件
[Continuation-Passing Style (CS 6520, Spring 2002, The University of Utah)](https://www.cs.utah.edu/~mflatt/past-courses/cs6520/public_html/s02/cps.pdf)
详细讲述了如何做CPS变换的一般原理，并将这一过程形式化描述。

CPS and tail calls
------------------

**Note that in CPS, there is no implicit continuation—every call is a tail call.**

 > There is no "magic" here, as the continuation is simply explicitly passed. Using CPS without
 > tail call optimization (TCO) will cause not only the constructed continuation to potentially
 > grow during recursion, but also the call stack. This is usually undesirable, but has been used
 > in interesting ways - see the Chicken Scheme compiler.

 > As CPS and TCO eliminate the concept of an implicit function return, their combined use can
 > eliminate the need for a run-time stack. Several compilers and interpreters for functional
 > programming languages use this ability in novel ways.

如果算法本身就是尾递归(尾递归是尾调用的特殊情形)的，那么，可以直接改写成迭代，这是尾调用优化的一种特例。如果
算法本身是非**尾递归**的，那么，CPS变换可以将算法改写成**尾调用**形式，从而可以进行尾调用优化。改写过后的
空间复杂度仍然是O(n)，只不过是从O(n)的栈变成了O(n)的continuation chain。
如果语言支持尾递归优化，完全是CPS风格的代码的堆栈是永远随着函数调用的深入而消耗。 如果语言本身不支持尾递归
优化，那么翻译后的CPS代码及有可能很快的消耗完栈空间。当然在不支持尾递归优化的语言中，我们可以使用Trampoline
技术进行优化，实现尾调用优化。
CPS变换并没有使得栈消失，栈以Continuation的形式存在，CPS变换对于解决栈溢出体现在变换前需要O(n)的栈空间，
而栈可能会有深度和大小限制，而变换后栈空间(control stack)的消耗减少(更少的control context存储)，主要消耗
堆空间(continuation 越来越大)，受限制更小，但也更慢。非尾递归算法，需要O(n)的调用栈空间，使用CPS变换，
也只是得到尾调用形式，空间仍然是O(n)。可以说，CPS变换只是把栈换了一个地方存放(continuation)。

> CPS stands for Continuation is Poor man's Stack.

CPS 与异步编程
------------

异步编程一般是指调用者(caller)不去等待一个耗时操作(callee)执行结束，一般通过ballback来获悉异步操作
执行情况。所有原本可能阻塞的操作全部都接受一个callback，当请求完成的时候调用callback。这种通过callback进行
异步编程的风格是不完全的CPS(Partially CPS converting)，callback 可以看成 continuation。异步编程相较于
多线程编程有很多优势，但是，通过callback进行异步编程是很困难的，程序的局部性被打碎了(没有if/else、do/while
等方式来实现控制流)，逻辑分割在各个callbcak里，编程复杂度提高。同时，CPS难以阅读和编写，并且，通过外部状态来
达到影响程序的控制流是很困难的。已经有arrowlets等库能够将控制流视为一等公民，提供更抽象和申明的方式对控制流
进行描述和组合。

CPS 与编译技术
------------

CPS已经成为了功能性编程语言的编译器的一种强大的中间表达形式，在编译技术方面也有很多应用。

> CPS desugars function return, exceptions and first-class continuations; function call turns into
> a single jump instruction.

我们可以将Lambda表达式转换成 CPS 代码。编程语言中“表达式(expression)”的语法形式如下所示：

    exp ::= (exp exp)           ; function application
          |  (lambda (var) exp)  ; anonymous function
          |  var                 ; variable reference

通过下面的程序，可以对符合这一语法的代码做CPS转换：

~~~scheme
#lang racket

;; translating the lambda calculus to CPS.

;; expression's grammar:
;; exp ::= (exp exp)           ; function application
;;      |  (lambda (var) exp)  ; anonymous function
;;      |  var                 ; variable reference

(define (cps-convert term cont)
  (match term
    [`(,f ,e)
     ; =>
     (let (($f (gensym 'f))
           ($e (gensym 'e)))
       (cps-convert f `(lambda (,$f)
         ,(cps-convert e `(lambda (,$e)
             (,$f ,$e ,cont))))))]

    [`(lambda (,v) ,e)
     ; =>
     (let (($k (gensym 'k)))
       `(,cont (lambda (,v ,$k)
                 ,(cps-convert e $k))))]

    [(? symbol?)
     ; =>
     `(,cont ,term)]))

(define (cps-convert-program term)
  (cps-convert term '(lambda (ans) ans)))
~~~

用CPS变换来实现call/cc:

    call/cc => (lambda (f cc) (f (lambda (x k) (cc x)) cc))

> It calls the procedure given as an argument with a procedure that has captured the current continuation.

用Javascirpt来表达这一程序：

    function callcc (f,cc) {
        f(function(x,k) { cc(x) },cc)
    }

CPS 的另一个重要的用途是用来实现 interprocedure analysing，例如C#的async await之类的。

参考
----

1. [By example: Continuation-passing style in JavaScript](http://matt.might.net/articles/by-example-continuation-passing-style/)
2. [CPS Lecture](https://cgi.soic.indiana.edu/~c311/lib/exe/fetch.php?media=cps-notes.scm)
3. [Continuation-Passing Style (CS 6520, Spring 2002, The University of Utah)](https://www.cs.utah.edu/~mflatt/past-courses/cs6520/public_html/s02/cps.pdf)

