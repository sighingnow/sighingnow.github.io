---
title: Javascript 闭包
author: Tao He
date: 2015-07-04
tag: Javascript
category: 编程语言
layout: post
---

闭包这个概念第一次出现在1964年的《The Computer Journal》上，由P. J. Landin在《The mechanical evaluation of expressions》一文中提出了applicative expression和closure的概念。

在StackOverflow上的关于闭包的问题描述[How do JavaScript closures work?](http://stackoverflow.com/questions/111102/how-do-javascript-closures-work) 中有这样一段话,很有意思：

> If you can't explain it to a six-year-old, you really don't understand it yourself.

闭包的定义
---------

引用维基百科：

> 闭包（Closure）是词法闭包（Lexical Closure）的简称，是引用了自由变量的函数。这个被引用的自由变量将和这个函数一同存在，即使已经离开了创造它的环境也不例外。

另一段关于闭包的叙述：

> Also we represent the value of a λ-expression by a bundle of information called a "closure", comprising the λ-expression and the environment relative to which it was evaluated. We must therefore arrange that such a bundle is correctly interpreted whenever it has to be applied to some argument.

A closure has an environment part which is a list whose two items are:

+ (1) an environment
+ (2) an identifier or list of identifiers

<!--more-->

JavaScript中的闭包
-----------------

> 在JavaScript中，我们称函数对象为闭包。根据ECMA-262规范，JavaScript的函数包含一个[[scope]]属性。[[scope]]指向scope chain（ECMA-262v3）或者Lexical Environment（ECMA-262v5）。这对应于闭包的环境部分，[[scope]]中可访问的属性列表即是标识符列表，对象本身的引用则对应于环境。控制部分即是函数对象本身了。

在ECMAscript的脚本的函数运行时，每个函数关联都有一个执行上下文场景(Execution Context) ，这个执行上下文场景中包含三个部分

1. 文法环境（The LexicalEnvironment）
2. 变量环境（The VariableEnvironment）
3. this绑定

一个JS闭包的例子：

~~~js
function f(){
    var n=999;
    function g(){
        return (n++);
    }
    return g;
}
~~~

此处，`g`函数和变量`n`就是一个闭包，它包含一个执行过程`return (n++);`和其执行的上下文`n`(一个函数加上这个函数用到的非局部变量。)。`n`不会因为其创造者`f`执行完被销毁而随之销毁。**对于闭包来说，函数和环境，缺一不可。**

闭包的用途
---------

1. 因为闭包只有在被调用时才执行操作，即“惰性求值”，所以它可以被用来定义控制结构。
2. 多个函数可以使用一个相同的环境，这使得它们可以通过改变那个环境相互交流。
3. 闭包可以用来实现封装和对象系统，可以加强模块化。某种意义上讲，jQuery就是一种闭包。
4. 实现缓存。通过闭包缓存某些局部变量。

闭包的实现原理
------------

> 典型实现方式是定义一个特殊的数据结构，保存了函数地址指针与闭包创建时的函数的词法环境表示（那些nonlocal变量的绑定）。使用函数调用栈的语言实现闭包比较困难，因而这也说明了为什么大多数实现闭包的语言是基于垃圾收集机制。闭包的实现与函数对象很相似。这种技术也叫做**lambda lifting**。

JavaScript 中的闭包与其 Scope Chain 特性真是密不可分的。首先在 JavaScript 的执行中会一直存在一个 Execute Context Stack (想想 JavaScript 解释器在看到一个 alert(x) 的时候, 如果没有上下文他怎么知道这个 x 是什么?), Execute Context Stack 中最下面一个一定是 GlobalContext, 而在每一个函数的执行开始就会向这个 stack 中压入一个此 Function 的 Execution Context; 而一个 Execution Context 的组成分为三部分:

1. Variable Object: 存储方法内的变量 vars, 方法传入的参数, 函数内定义的函数等等(函数表达式不保存), Variable Object 在任何时候是不可以被直接访问到的, 当然不同的 JS 引擎提供了访问接口就说不定了;
2. Scope Chain: 这个函数执行的时候用以寻找值的 Scope Chain, 这个 Scope Chain 由 Variable Object + All Parent Scopes 组成, Variable Object 会放在这个 Scope Chain 的最前面, 这也是为什么函数内的变量会被最先找到;
3. thisValue, 函数被调用的时候的 this 对象, 存储的就是函数的调用者(caller)的引用。

对于 Variable Object 在不同的情况下会有不同的定义, 例如在全局的时候被称为 Global Object, 而在函数中则被称为 Activation Object 激活对象; 正是由于有了 Execution Context 中的 Scope Chain, JavaScript 才能够使得在子方法的内部访问到外部方法中的变量, 才能够使子方法将变量关闭在自己的作用范围内不让它随外部方法的执行完毕而销毁。

使用闭包
-------

使用闭包应当注意这几个问题：

1. 闭包中局部变量是引用而非拷贝。

~~~js
function f() {
    var n = 999;
    function g() {
        return n;
    }
    n += 1;
    return g;
}

console.log(f()());
~~~

得到的结果是1000而不是99。

2. 由于闭包会使得函数中的变量都被保存在内存中，内存消耗很大，所以不能滥用闭包，否则会造成网页的性能问题(在多个版本的IE上都存在内存泄漏(Memory Leak)的问题，主要是由于引用计数(ref-count)和 lexical scope context的暴力实现)。解决方法是，在退出函数之前，将不使用的局部变量全部删除。
3. 闭包会在父函数外部，改变父函数内部变量的值。所以，如果你把父函数当作对象（object）使用，把闭包当作它的公用方法（Public Method），把内部变量当作它的私有属性（private value），这时一定要小心，不要随便改变父函数内部变量的值。

Explain to six-year-old
-----------------------

(By _Jacob Swartwood_ from Stackoverflow)

There was a princess...

    function princess() {

She lived in a wonderful world full of adventures. She met her Prince Charming, rode around her world on a unicorn, battled dragons, encountered talking animals, and many other fantastical things.

        var adventures = [];
        function princeCharming() { /* ... */ }
        var unicorn = { /* ... */ },
            dragons = [ /* ... */ ],
            squirrel = "Hello!";

But she would always have to return back to her dull world of chores and grown-ups.

        return {

And she would often tell them of her latest amazing adventure as a princess.

            story: function() {
                return adventures[adventures.length - 1];
            }
        };
    }

But all they would see is a little girl...

    var littleGirl = princess();
    ...telling stories about magic and fantasy.
    littleGirl.story();

And even though the grown-ups knew of real princesses, they would never believe in the unicorns or dragons because they could never see them. The grown-ups said that they only existed inside the little girl's imagination.

**But we know the real truth; that the little girl with the princess inside......is really a princess with a little girl inside.**

我对这个故事的理解：故事表达的是大臣的作用域看不到公主的作用域里的东西呀。但公主作用域里面的东西真是存在，并且可以告诉大臣。

Final points
------------

最后，引用StackOverflow上的一个回答作为对闭包的总结：

(From _JavaScript Closures for Dummies_ By _Morris_ and put under the Creative Commons Attribution / Share alike license.)

1. Whenever you use function inside another function, a closure is used.
2. Whenever you use eval() inside a function, a closure is used. The text you eval can reference local variables of the function, and within eval you can even create new local variables by using eval('var foo = …')
3. When you use new Function(…) (the Function constructor) inside a function, it does not create a closure. (The new function cannot reference the local variables of the outer function.)
4. A closure in JavaScript is like keeping a copy of all the local variables, just as they were when a function exited.
5. It is probably best to think that a closure is always created just on entry to a function, and the local variables are added to that closure.
6. A new set of local variables is kept every time a function with a closure is called (given that the function contains a function declaration inside it, and a reference to that inside function is either returned or an external reference is kept for it in some way).
7. Two functions might look like they have the same source text, but have completely different behaviour because of their 'hidden' closure. I don't think JavaScript code can actually find out if a function reference has a closure or not.
8. If you are trying to do any dynamic source code modifications (for example: myFunction = Function(myFunction.toString().replace(/Hello/,'Hola'));), it won't work if myFunction is a closure (of course, you would never even think of doing source code string substitution at runtime, but...).
9. It is possible to get function declarations within function declarations within functions — and you can get closures at more than one level.
10. I think normally a closure is the term for both the function along with the variables that are captured. Note that I do not use that definition in this article!
11. I suspect that closures in JavaScript differ from those normally found in functional languages.

参考
----

1. [http://ejohn.org/apps/learn/](http://ejohn.org/apps/learn/)
2. [how-do-javascript-closures-work](http://stackoverflow.com/questions/111102/how-do-javascript-closures-work)
3. [ECMA-262-3 in detail. Chapter 1. Execution Contexts](http://dmitrysoshnikov.com/ecmascript/chapter-1-execution-contexts/).
4. [MDN文档. 闭包(Closures)](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Closures).

