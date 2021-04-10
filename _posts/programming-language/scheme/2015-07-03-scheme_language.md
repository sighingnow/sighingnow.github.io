---
title: Scheme 编程语言
author: Tao He
date: 2015-07-03
tag: [Scheme]
category: 编程语言
layout: post
---

Scheme 语言是 函数式编程语言，是现代两大Lisp方言之一，诞生于1975年，由 MIT 的 Gerald J. Sussman and Guy L. Steele Jr. 完成。

本文是根据[Teach Yourself Scheme in Fixnum Days](http://ds26gte.github.io/tyscheme/index.html)学习Scheme的笔记。

第一个Scheme程序:

~~~scheme
; the first scheme program.
(begin
  (display "hello, first scheme program\n"))
~~~

<!--more-->

交互式命令行
----------

在交互式命令行环境中(mit-scheme或Racket)可以输入Scheme指令。

可以使用`load`方法来载入并执行一个Scheme文件：

    (load "first.scm")

退出命令行环境：

    (exit)

数据类型(Data types)
-------------------

Scheme的数据类型主要有：

> Booleans, Numbers, Characters, Symbols, Strings, Vectors, Dotted Pairs, Lists

1. Booleans

Scheme中的booleans类型用`#t`、`#f`来分别表示true和false。Scheme拥有一个叫`boolean?`的过程，可以用来检测它的参数是否为boolean类型。

    > (boolean? #t)
    #t
    > (boolean? #f)
    #t
    > (boolean? "abcd")
    #f

not过程直接取其参数的相反值做为boolean类型结果。在一个需要boolean类型的上下文中，Scheme会将任何非`#f`的值看成true。

    > (not "abcde")
    #f

2. Numbers

Scheme的numbers类型可以是integers(整型，例如42)，rationals(有理数，例如22/7)，reals(实数，例如3.14159)，或complex(复数，2+3i)。一个整数是一个有理数，一个有理数是一个实数，一个实数是一个复数，一个复数是一个数字。Scheme中有可供各种数字进行类型判断的过程：

    > (number? 10)
    #t
    > (real? 2+3i)
    #f
    > (real? 22/7)
    #t
    > (number? 2+3i)
    #t

Scheme的integers(整型)不需要一定是10进制格式。可以通过在数字前加前缀 #b 来规定实现2进制。这样 #b1100就是10进制数字12了。实现8进制和16进制格式的前缀分别是 #o 和 #x。(decimal前缀 #d是可选项)。

我们可以使用通用相等判断过程 `eqv?` 来检测数字的相等性。`eqv?`的参数不一定需要时数字类型：

    > (eqv? 42 42)
    #t
    > (eqv? 42 42.0)
    #f
    > (eqv? 42 "42")
    #f
    > (eqv? "abcde" "abcde")
    #t

其它的数字比较还包括 <, <=, >, >=。Scheme也拥有 +, -, *, /, expt等数学运算过程。其中`expt`表示求幂：

    > (expt 2 10)
    1024

3. Characters

Scheme中字符型数据通过在字符前加`#\`前缀来表示。像 `#\c`就表示字符 `c`。那些非可视字符会有更多的描述名称，例如，`#\newline`, `#\tab`。空格字符可以写成 `#\` ，或者可读性更好一些的`#\space`。

字符类型判断过程是`char?`, 数据的分号字符不会引发注释。字符类型数据有自己的比较判断过程：`char=?`, `char<?`, `char<=?`, `char>?`, `char>=?`。

要实现忽略大小写的比较，得使用 `char-ci` 过程代替 `char`过程：类型转换过程分别是 `char-downcase` 和`char-upcase`。

    > (char=? #\w #\W)
    #f
    > (char-ci=? #\w #\W)
    #t
    > (char-downcase #\W)
    #\w

4. Symbols

Symbols通常在Scheme程序中被用来当做变量的标识，这样可以运算出变量所承载的值。然而symbols是一种简单数据类型，而且就像characers、numbers以及其它类型数据一样，是Scheme中可以传递的有效值类型。创建一个单纯的symbol而非变量时，你需要使用quote过程：

    (quote xyz)

因为在Scheme中经常要引用这种类型，我们有一种更简便的方式。表达式 `'E`和 `(quote E)` 在Scheme中是等价的。

    'xyz

Scheme的Symbols类型通常都是不区分大小写的。因此`XYZ`和`xyz`是等价的

    > (eqv? 'XYZ 'xyz)
    #t

Symbols在命名时不要和其它类型数据发生冲突，比如characters 、booleans、numbers 或复合类型。用来检查Symbols类型数据的过程(predicate)是`symbol?`:

    > (symbol? 'number)
    #t
    > (symbol? 'xyz)
    #t
    > (symbol? 42)
    #f

We can use the symbol `xyz` as a global variable by using the form define:

    (define xyz 9)

This says the variable `xyz` holds the value 9. If we feed `xyz` to the listener, the result will be the value held by `xyz`:

    > xyz
    9

We can use the form `set!` to change the value held by a variable:

    > (set! xyz #\c)
    > xyz
    #\c

5. Strings

字符串类型是由字符组成的序列（不能和Symbols混淆，Symbols仅是由一组字符来命名的简单类型(not to be confused with symbols, which are simple data that have a sequence of characters as their name)）。可以通过将一些字符包上闭合的双引号来得到字符串。Strings是自运算类型。

    > "abcde"
    "abcde"

可以通过向string 过程传递一组字符并返回由它们合并成的字符串：

    > (string #\h #\e #\l #\l #\o)
    "hello"

可以通过`define`定义一个全局字符串变量：

    > (define greeting "Hello!")
    > greeting
    "Hello!"

一个给定字符串数据中的字符可以分别被访问和更改。通过向`string-ref`过程传递一个字符串和一个从0开始的索引号，可以返回该字符串指定索引号位置的字符。

    > (string-ref greeting 2)
    #\l

可以使用`string-append`过程通过在一个现有的字符串上追加其它字符串的方式来获得新字符串：

    > (string-append "Hello " "World" "!")
    "Hello World!"

可以使用`make-string`过程来定义一个指定长度的空字符串：

    > (define a (make-string 3))
    > a
    "\u0000\u0000\u0000"

直接定义的形如`(define a "abcd")`的字符串是不可变的，而通过调用`string`，`make-string`和`string-append`获得的字符串结果都是可修改的。而过程`string-set!`就可以替换字符串指定索引处的字符。

    > (define a (string #\H #\e #\l #\l #\o #\!))
    > a
    "Hello!"
    > (string-set! a 0 #\h)
    > a
    "hello!"
    > (define a (make-string 3))
    > a
    "\u0000\u0000\u0000"
    > (string-set! a 1 #\A)
    > a
    "\u0000A\u0000"

6. Vectors

Vectors are sequences like strings, but their elements can be anything, not just characters. Indeed, the elements can be vectors themselves(multidimensional vectors).

    > (vector 0 1 2 3 4)
    #(0 1 2 3 4)
    > (vector 0 1 2 3 (vector 0 1 2 3))
    #(0 1 2 3 #(0 1 2 3))

过程`make-vectors`可以构建一个指定长度的向量：

    > (define v (make-vector 5))
    > v
    #(0 0 0 0 0)

过程`vector-ref`和`vector-set!`分别可以访问和修改向量元素。检测值是否是一个向量的过程是`vector?`。

7. Dotted pairs(点值对)

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

Scheme提供了可以简化car 和 cdr组合起来连续访问操作的简化过程。像`caar`表示“`car`运算结果的`car`运算结果”， `cdar`表示“`car`运算结果的`cdr`运算结果”，等等。像`c...r`这样风格的简写最多只支持四级连续操作。像`cadr`，`cdadr`，和 `cdaddr`都是存在的。而`cdadadr`这样的就不对了。

当第二个元素是一个嵌套的点值对时，Scheme使用一种特殊的标记来表示表达式的结果：

    > (cons 1 (cons 2 (cons 3 (cons 4 5))))
    (1 2 3 4 . 5)

即，`(1 2 3 4 . 5)`是对`(1 . (2 . (3 . (4 . 5))))`的一种简化。

8. Lists

诸如像`(1 . (2 . (3 . (4 . ()))))`这样形式的点值对被简化成`(1 2 3 4)`。像这样第二元素都是一个点值对特殊形式的嵌套点值对就称作列表list。

Scheme提供了一个list过程可以更方便的创建列表：

    > (list 1 2 3 4)
    (1 2 3 4)

如果我们知道列表所包含的所有元素，我们还可以用quote 来定义一个列表：

    > '(1 2 3 4)
    (1 2 3 4)

列表的元素可以使用`list-ref`通过指定索引号来访问。`list-tail`返回了给定索引号后的所有元素。`pair?`， `list?`和`null?`判断过程可以分别用来检查它们的参数是不是一个点值对，列表或空列表。

**在scheme中没有list-set!过程**, 可以实现如下：

~~~scheme
(define (list-set! l k v)
  (cond ((or (< k 0) (null? 1)) #f)
        ((= k 0) (set-car! l v))
        (else (list-set! (cdr l) (- k 1) v))))
~~~

测试这个过程的实现的正确性：

~~~scheme
(begin
  (define l (list 1 2 3 4))
  (display l) (newline)
  (list-set! l 2 100)
  (display l))
~~~

数据类型转换
----------

Scheme提供了许多可以进行数据类型转换的过程。

1. 可以通过`char-downcase` 和 `char-upcase`过程来进字符大小写的转换。
2. 字符可以通过使用`char->integer`来转换成整型，同样的整型也可以通过`integer->char`被转换成字符(字符转换成整型得到的结果通常是这个字符的ascii码值)。
3. 字符串可以通过使用`string->list`被转换成等价的字符列表。
4. 字符串也可以通过使用`string->number`转换成数字。如果字符串不能转换成数字，则会返回`#f`。同样，数字可以通过`number->string`转换成字符串。
5. `list->string`， `vector->list` 和 `list->vector`等可以实现list, vector, string数据类型间的转换。
6. Symbols也可以转换为字符串，反之亦然。(`symbol->string`和`string->symbol`)。

Scheme还包含了一些其它数据类型。一个是procedure(过程)。我们已经见过了许多过程了，例如，display， +， cons等。实际上，它们是一些承载了过程值的变量，过程本身内部的数值和字符并不可见。

    > cons
    #<procedure:mcons>

还有另外种数据类型是port端口。一个端口是为输入输出提供执行的通道。端口通常会和文件和控制台操作相关联。

display可以接受两个参数，第一个参数值是将输出的值，另一个值则表示了即将承载显示结果的输出port(端口)。在我们的程序中，display的第二参数是隐式参数。这时候display会采用标准输出端口作为它的默认输出端口。我们可以通过调用`current-output-port`过程来取得当前的标准输出端口。

    > (display "Hello, World!" (current-output-port))
    Hello, World!

**S-expressions(S-表达式)：所有这些已经被讨论过的数据类型可以被统一成一种通用的叫作s-expression(符号表达式或s-表达式)的数据类型(s代表符号)。**

Forms代码结构(Forms)
--------------------

**对所有的Scheme程序来说都适用：程序是数据。** 不是所有的s-表达式都可以自运算。比如symbol 表达式 xyz运算得到的结果是xyz这个变量所承载的值；list 表达式 (string->number "16")运算的结果是数字16。也不是所有的s-表达式都是有效的程序。如果直接输入点值对(1 . 2)，将会得到一个错误。

Scheme运行一个列表形式的代码结构时，首先要检测列表第一个元素，或列表头。如果这个列表头是一个过程，则代码结构的其余部分则被当成将传递给这个过程的参数集，而这个过程将接收这些参数并运算。如果这个代码结构的列表头是一个特殊的代码结构，则将会采用一种特殊的方式来运行。我们已经碰到过的特殊的代码结构有`begin`， `define`和 `set!`。`begin`可以让它的子结构可以有序的运算，而最后一个子结构的结果将成为整个代码结构的运行结果。`define`会声明并会初始化一个变量。`set!`可以给已经存在的变量重新赋值。

1. 过程(procedures)

我们已经见过了许多系统过程，比如，cons， string->list等。用户可以使用代码结构lambda来创建自定义的过程。

    (lambda (x) (+ x 2))
    (define add2 (lambda (x) (+ x 2)))

这个过程可以像系统过程一样，通过传递一个参数完成调用：

    ((lambda (x) (+ x 2)) 5)

定义过程还可以有另一种简单的方式，直接用define而不使用lambda来创建：

    (define (add2 x) (+ x 2)))

2. 不定长参数

有一些过程可以在不同的时候传给它不同个数的参数来完成调用。为了实现这样的过程，lambda表达式列表形式的参数要被替换成单个的符号。这个符号会像一个变量一样来承载过程调用时接收到的参数列表。

通常，lambda的参数列表可以是一个列表构结`(x ...)`，一个符号，或者`(x ... . z)`这样的一个点值对结构。**当参数是一个点值对结构时，在点之前的所有变量将一一对应过程调用时的前几个参数，点之后的那个变量会将剩余的参数值作为一个列表来承载。**

~~~scheme
(define (func x . t)
  (display x)
  (newline)
  (display t))

(begin
  (func 1 2 3 4))
~~~

可以使用`for-each`过程来遍历参数列表，如下：

~~~scheme
(define (sum t . x)
  (define s 0)
  (for-each (lambda (k) (set! s (+ s k))) x)
  (+ t s))

(begin
  (display (sum 1 2 3 4)))
~~~

或者也可以写成：

~~~scheme
(define (sum . x)
  (define s 0)
  (for-each (lambda (k) (set! s (+ s k))) x)
  s)
~~~

3. Apply过程

`apply`过程允许我们直接传递一个装有参数的list 给一个过程来完成对这个过程的批量操作。例如：

    > (apply + '(1 2 3 4))
    10

通常，apply需要传递一个过程给它，后面紧接着是不定长参数，**但最后一个参数值一定要是list**。它会根据最后一个参数和中间其它的参数来构建参数列表。然后返回根据这个参数列表来调用过程得到的结果。

    > (apply + 1 2 3 4 '(1 2 3 4))
    20
    > (apply + 1 2 3 4 '())
    10

条件语句(Conditionals)
---------------------

1. if结构

if结构是Scheme中的最基本的条件分支控制结构，if过程的框架如下：

    (if 测试条件
        then-分支
        else-分支)

if语句的例子：

    > (define p 100)
    > (if (> p 80) "big" "small")
    "big"

2. when 和 unless

当我们只需要一个基本条件语句分支时（”then”分支或”else”分支），使用when 和 unless会更方便。并不是所有的Scheme环境都提供when和unless。MIT-Scheme就没有提供when和else。如果你的Scheme中没有，你可以用宏来自定义出when和unless。

3. cond 过程

cond结构在表示多重if表达式时很方便，多重if结构除了最后一个else分支以外的其余分支都会包含一个新的if条件。

一个使用cond结构的例子：

~~~scheme
(define cmp
  (lambda (a b)
    (cond ((char<? a b) -1)
          ((char>? a b) 1)
          (else 0))))

(begin
  (display (cmp #\a #\b)))
~~~

4. case 结构

当cond结构的每个测试条件是一个测试条件的分支条件时，可以缩减为一个case表达式。

~~~
(define (func c)
  (case c
    (#\a 0)
    (#\b 1)
    (#\c 2)
    (else -1)))

(begin
  (display (func #\b)))
~~~

5. and 和 or

Scheme也提供了一系列的特殊子form来进行一些逻辑运算，这里面包括and和or。 **但是不包括not，not是一个过程名。**

Scheme中，and和or也是短路求值(short-circuit evaluation)：

    > (and 1 #f (/ 1 0))
    #f
    > (and 1 (/ 1 0) #f )
    . . /: division by zero

词法变量(Lexical variables)
--------------------------

Scheme的变量有一定的词法作用域，即它们在程序代码中只对特定范围的代码结构可见。lambda过程的参数，当过程被调用时这些变量会被赋值，而它们的作用域仅限于在过程的内部。当全局量和局部量同名时，局部定义会覆盖全局定义。

1. `let` 和 `let*`

并不是一定要显式的创建过程才可以创建局部变量。有个特殊的代码结构`let`可以创建一列局部变量以便在其结构体中使用:

    (let ((x 1)
          (y 2)
          (z 3))
      (list x y z))

有时候，用`let`依次的创建局变量非常的方便，如果在初始化区域中可以用先创建的变量来为后创建的变量赋值也会非常方便。`let*`结构就可以这样做：

    (let* ((x 1)
           (y x))
      (+ x y))

`let*`完全等价于下面这个`let`嵌套的程序，更深了说，实际上就是let嵌套的缩写。

    (let ((x 1))
      (let ((y x))
        (+ x y)))

2. fluid-let

fluid-let是一个非标准的特殊结构,但并不是暂时的隐藏了全局变量的值，而是在fluid-let执行体中临时的将全局变量counter的值保持为某一值直到执行体结束。当fluid-let表达式计算结束后，全局变量counter会恢复成之前的的值。

~~~scheme
(define x 100)

(define (func v)
  (set! x (+ x v))
  (display x))

(display x)
(fluid-let ((x 10))
  (func 1)
  (func 2))
(display x)
~~~

递归(Recursion)
--------------

一个过程体中可以包含对其它过程的调用，特别的是也可以调用自己。

例子：

~~~scheme
(define fact
  (lambda (n)
    (if (= n 0) 1 (* n (fact (- n 1))))))
~~~

互递归过程也是可以的。下面判断奇偶数的过程相互进行了调用。

~~~scheme
(define is-even?
 (lambda (n)
    (if (= n 0) #t
        (is-odd? (- n 1)))))

(define is-odd?
 (lambda (n)
    (if (= n 0) #f (is-even? (- n 1)))))
~~~

Scheme已经提供了简单的判断过程`even?`和`odd?`。

1. letrec

用letrec创建的词法变量不仅可以在letrec执行体中可见而且在初始化中也可见。letrec是专门为局部的递归和互递归过程而设置的。(这里也可以使用define来创建两个子结构的方式来实现局部递归)

将上面的判断奇偶的例子可以改写成如下形式：

~~~scheme
(letrec ((local-even? (lambda (n)
                      (if (= n 0) #t (local-odd? (- n 1)))))
        (local-odd? (lambda (n)
                    (if (= n 0) #f (local-even? (- n 1))))))
(list (local-even? 23) (local-odd? 23)))
~~~

2. 命名let

使用letrec定义递归过程可以实现循环。Scheme允许使用一种叫“命名let”的let变体来更简洁的写出这样的循环:

~~~scheme
(let countdown ((i 10))
  (if (= i 0) 'liftoff
      (begin
        (display i)
        (countdown (- i 1)))))
~~~

在let的后面立即声明了一个变量用来表示这个循环。这个程序和先前用letrec写的程序是等价的。你可以将“命名let”看成一个对letrec结构进行扩展的宏。

Scheme十分注意确保上面使用过的递归类型不会产生过程调用/返回开销。Scheme通过一种消除尾部调用（tail-call elimination）的过程完成这个功能。

3. reverse!

Scheme利用递归实现的反转列表：

~~~scheme
(define reverse!
  (lambda (s)
    (let loop ((s s) (r '()))
      (if (null? s) r
      (let ((d (cdr s)))
            (set-cdr! s r)
        (loop d s))))))
~~~

它将自身的参数列表就地反转，也就是使现有的列表内容产生变异，而没有分配一个新的列表。

4. map和for-each

+ map程序为给定列表中的每个元素提供了一种既定程序，并返回一个结果的列表。
+ for-each程序也为列表中的每个元素提供了一个程序，但返回值为空。这个程序纯粹是产生的副作用。

这个由map和for-each用在列表上的程序并不一定是单参数程序。举例来说，假设一个n参数的程序，map会接受n个列表，每个列表都是由一个参数所组成的集合，而map会从每个列表中取相应元素提供给程序。

    map cons '(1 2 3) '(10 20 30))
    =>  ((1 . 10) (2 . 20) (3 . 30))
    (map + '(1 2 3) '(10 20 30))
    =>  (11 22 33)

输入输出(I/O)
------------

Scheme的输入/输出程序可以使你从输入端口读取或者将写入到输出端口。端口可以关联到控制台，文件和字符串。

Scheme的读取程序带有一个可选的输入端口参数。如果端口没有特别指定，则假设为当前端口（一般是控制台）。读取的内容可以是一个字符，一行数据或是S表达式。当每次执行读取时，端口的状态就会改变，因此下一次就会读取当前已读取内容后面的内容。如果没有更多的内容可读，读取程序将返回一个特殊的数据——文件结束符或EOF对象。这个对象只能用`eof-object?`函数来判断。`read-char`程序会从端口读取下一个字符。`read-line`程序会读取下一行数据，并返回一个字符串（**不包括最后的换行符**），`read`程序则会读取下一个S表达式。

Scheme的写入程序接受一个要被写入的对象和一个可选的输出端口参数。如果未指定端口，则假设为当前端口（一般为控制台）。写入的对象可以是字符或是S表达式。`write-char`程序可以向输出端口写入一个给定的字符（不包括`#\`）。 `write`和`display`程序都可以向端口写入一个给定的S表达式，唯一的区别是：`write`程序会使用机器可读型的格式而`display`程序却不用。例如，`write`用双引号表示字符串，用`#\`句法表示字符，但`display`却不这么做。`newline`程序会在输出端口输出一个换行符。

如果端口是标准的输入和输出端口，Scheme的I/O程序就不需要端口参数。但是，如果你明确需要这些端口，则`current-input-port`和`current-output-port`这些零参数程序会提供这个功能。

一个端口通过打开文件和这个文件关联在一起。`open-input-file`程序会接受一个文件名作为参数并返回一个和这个文件关联的新的输入端口。`open-output-file`程序会接受一个文件名作为参数并返回一个和这个文件关联的新的输出端口。**如果打开一个不存在的输入文件，或者打开一个已经存在的输出文件，程序都会出错。** 当你已经在一个端口执行完输入或输出后，你需要使用`close-input-port`或`close-output-port`程序将它关闭。

Scheme提供了`call-with-input-file`和`call-with-output-file`过程，这些过程会照顾好打开的端口并在你使用完后将端口关闭。

~~~scheme
(call-with-input-file "hello.txt"
  (lambda (i)
    (let* ((a (read-char i))
           (b (read-char i))
           (c (read-char i)))
      (list a b c))))
~~~

一般来说将字符串与端口相关联是很方便的。因此，open-input-string程序将一个给定的字符串和一个端口关联起来。读取这个端口的程序将读出下述字符串：

~~~scheme
(define i (open-input-string "hello world"))

(read-char i)
=>  #\h
(read i)
=>  ello
(read i)
=>  world
~~~

`open-output-string`创建了一个输出端口，最终可以用于创建一个字符串。然后可以使用`get-output-string`程序得到保留在字符串端口中的字符串。

可以使用`load`加载并执行一个外部的Scheme源文件。

跳转(Jumps)
-----------

Scheme的一个显著标志是它支持跳转或者**nonlocal control**。特别是Scheme允许程序控制跳转到程序的任意位置，相比之下条件语句和函数调用的限制要更多一些。Scheme的nonlocal control操作符是一个名为`call-with-current-continuation`的过程。

参考
----

1. [http://songjinghe.github.io/TYS-zh-translation/](http://songjinghe.github.io/TYS-zh-translation/)
