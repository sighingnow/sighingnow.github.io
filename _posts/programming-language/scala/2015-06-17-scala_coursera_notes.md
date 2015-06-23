---
title: Scala 公开课笔记
author: He Tao
date: 2015-06-17
tag: [Scala]
category: 编程语言
layout: post
---

Coursera上的瑞士洛桑理工大学Scala函数式编程原理(Functional Programming Principles in Scala)课程笔记。

课程链接：[https://class.coursera.org/progfun-005](https://class.coursera.org/progfun-005)

Getting Started
---------------

Scala开发环境配置相关的几点经验：

1. 解决Scala编译太慢的问题：使用sbt构建，保持一个运行sbt的console。也就是说，让一个JVM常驻内存，可以显著提升编译效率。
2. sbt解决依赖的过程中出现sha1 error: 在build.sbt中增加如下内容：

        checksums in update := Nil

    或者在sbtconfig.txt中增加如下内容，禁用全局sha1 check:

        -Dsbt.ivy.checksums="" # 默认值为"md5, sha1"

3. 自定义sbt的缓存位置: 在sbtconfig.txt中增加如下内容：

        -Dsbt.ivy.home=d:/Java/sbt/
        -Dsbt.boot.directory=d:/Java/sbt/boot/

<!--more-->

Week 1: Functions & Evaluations
-------------------------------

1. 三种编程范式(Programming Paradigms)

    + impreative programming
    + functional programming
    + logic programming

2. 命令式编程受限于冯·诺依曼(Von Neumann)体系结构。

3. 不可变类型理论(Theories without Mutation)

4. 三个要点：将精力集中于定义运算符，最小化状态改变，将运算符视为函数。

5. REPL：Read-Eval-Print Loop

求值过程：从左至右求值，如果有函数，先从左至右对参数求值，再将参数的值带入。

带入模型(The substitution model)是在lambda演算中形式化，是Functional Programming的基础(foundation)。

终止问题：

    def loop: Int = loop

这个求值过程永远不会终止。

> The interpreter reduces function arguments to values before rewriting the function application.

6. 两种求值策略(evaluation strategy)：Call-by-Name, Call-by-Value

求值策略与Subsitution Model.

Call-by-value has the advantage that it evaluates every function argument only once.

Call-by-name has the advantage that a function argument is not evaluated if the corresponding parameter is unused in the evaluation of the function body.

由于两种求值策略的区别，某些求值序列可能在一种策略下会终止，在另外一种求值策略下并不会终止。

例如：

    def loop(): Int = loop
    def func(a:Int, b:Int) = 1

求值:

    func(1, loop)

如果是CBV，参数的求值过程不会终止，而如果是CBN，只有在用到参数时才会对参数求值，因此这个求值过程会直接返回1，而不会对参数loop求值。

只有在以下情况下两种策略会返回相同的值：

> the reduced expression consists of pure functions, and both evaluations terminate.

在Scala中，一般都是Call-by-Value, 如果函数的参数用`=>`来表示类型，那么该参数会是Call-by-Name。

例如：

    def func(x: Int, y: => Int) = 1

对于这个函数定义，x是CBV，而y是CBN。`func(1, loop)`能够终止，因为并不需要对loop进行求值。

7. 语句块和词法作用域(Blocks and Lexical Scope)

Scala允许函数嵌套。例如：

```scala
// Square roots with Newton’s method
def sqrt(x: Double) = {
  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)
  def improve(guess: Double, x: Double) =
    (guess + x / guess) / 2
  def isGoodEnough(guess: Double, x: Double) =
    abs(square(guess) - x) < 0.001
  sqrtIter(1.0, x)
}
```

Scala中的Block:

```scala
{
  var x = f(3)
  x*x
}
```

对于Block，Scala中变量和函数的可见性如下：

> The definitions inside a block are only visible from within the block.

> The definitions inside a block shadow definitions of the same names outside the block.

> Deﬁnitions of outer blocks are visible inside a block unless they are shadowed.

8. 尾递归(Tail Recursion)

Implementation Consideration: If a function calls itself as its last action, the function’s stack frame can be reused. This is called tail recursion.

在Scala中，只有对当前函数的直接递归调用能够被尾递归优化。使用`@tailrec`注解表明当前函数存在尾递归。如果对一个不是尾递归实现的函数使用`@tailrec`注解，将会产生错误。

```scala
@tailrec
def gcd(a:Int, b:Int): Int = {
  if (b == 0) a else gcd(b, a % b)
}

// tail recursion version of factorial function.
def factorial(n: Int): Int = {
  @tailrec
  def factorial_i(acc: Int, n: Int): Int = {
    if(n == 0) acc
    else factorial_i(acc*n, n-1)
  }
  factorial_i(1, n)
}
```

Week 2: Higher Order Functions
------------------------------

1. Functional languages treat functions as *ﬁrst-class* values.

在Functional Programming中，函数可以被当成一个参数，也可以作为返回值。

高阶函数的定义：

> Functions that take other functions as parameters or that return functions as results are called higher order functions.

使用高阶函数的例子：

```scala
def sum(f: Int=>Int, a: Int, b: Int) {
  if(a > 0) 0
  else f(a) + sum(f, a+1, b)
}

def id(x:Int): Int   = x
def cube(x:Int): Int = x*x*x
def fact(x:Int): Int = if(x==1) 1 else x*fact(x-1)

def sumId(a, b)   = sum(id, a, b)
def sumCube(a, b) = sum(cube, a, b)
def sumFact(a, b) = sum(fact, a, b)
```

2. 在Scala中，函数也是一种类型。例如`Int=>Int`表示的就是参数类型为一个`Int`, 返回值也是`Int`的函数的类型。

3. 匿名函数(Anonymous Functions)

One can therefore say that anonymous functions are *syntactic suger*

另一种构造匿名函数方法：

    { def f(???) = ???; f }

例如：

    List.range(0, 10).map({def f(a) = println(a+1); f})

等价于：

    List.range(0, 10).map(a:Int => println(a+1))

4. Functions Returning Functions

Scala中，函数可以将一个函数作为参数，也可以将一个函数作为返回值。如下例：

```scala
object progfun {
  def sum(f: Int=>Int): (Int, Int) => Int = {
    def sum_i(a:Int, b:Int): Int = {
      if(a > b) 0
      else f(a) + sum_i(a+1, b)
    }
    sum_i
  }                              //> sum: (f: Int => Int)(Int, Int) => Int
  sum(x => x*x*x)(1,3)           //> res0: Int = 36
}
```

5. Multiple Parameter Lists

Scala在定义函数时，可以有一个参数列表，也可以有多个参数列表。如下例：

```scala
def sum(f: Int => Int)(a: Int, b: Int): Int = {
  if(a > b) 0 else f(a) + sum(f)(a+1, b)
}                                //> sum: (f: Int => Int)(a: Int, b: Int)Int


// application
sum(a => a*a*a)(2,4)             //> res0: Int = 99
```

这也是Scala的一个语法糖(syntactic suger)的例子。

6. Carrying

By repeating the process n times

    def f(args1):::(argsn 1)(argsn) = E

is shown to be equivalent to

    def f = (args1 ) (args2 ) :::(argsn ) E):::))

This style of definition and function application is called currying, named for its instigator, Haskell Brooks Curry (1900-1982), a twentieth century logician.

Conceptually, currying is a fairly simple idea. Wikipedia defines it as follows:

In computer science, currying, invented by Moses Schönfinkel and Gottlob Frege, is the technique of transforming a function that takes multiple arguments into a function that takes a single argument (the other arguments having been specified by the curry).

For example, in Scala, we can write:

    def add(x: Int, y: Int) = x + y
    add(1, 2)   // 3
    add(7, 3)   // 10

And after currying:

    def add(x: Int) = (y: Int) => x + y
    add(1)(2)   // 3
    add(7)(3)   // 10

You can also use the following syntax to define a curried function:

    def add(x: Int)(y: Int) = x+y
    add(1)(2)
    add(7)(3)

使用Scala Function包中的uncurried函数可以得到一个函数的普通版本。例如：

    def sum(a: Int)(b: Int) = a+b
    def normalSum = Function.uncurried(sum)
    println(sum(1)(2))
    println(normalSum(1, 2))

7. Scala中的`=>`是右结合。例如：

    Int => Int => Int

等价于：

    Int => (Int => Int)

8. 使用Currying来实现一个简单的mapReduce：

```scala
def mapReduce(f:Int=>Int,combine:(Int,Int)=>Int, zero:Int)(a:Int,b:Int):Int = 
  if(a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a+1, b))
```

上面的

```scala
def sum(f: Int => Int)(a: Int, b: Int): Int = {
  if(a > b) 0 else f(a) + sum(f)(a+1, b)
}
```

可以改成mapReduce的写法：

```scala
def sum_mr(f: Int=>Int)(a: Int, b: Int): Int = mapReduce(f, (x, y)=>x+y, 0)(a, b)
```

9. 在Scala中应该注意：一个函数能够找到（调用）定义在他下边的函数当且仅当这两个函数之间没有求值表达式。

10. Types in Scala

Scala中的几种数据类型：

+ A numeric type: Int, Double, Float, Char, Byte, Long
+ The Boolean type with the values true and false.
+ The String type.
+ A function type, like Int => Int, (Int, Int) => Int

11. Class

Class可以用来定义新的数据类型。例如，使用Scala按照如下方式来定义分数：

```scala
class Rational(x: Int, y: Int) {
  def numer = x
  def denom = y
}
```

This definition introduces two entities:

+ A new type, named Rational.
+ A constructor Rational to create elements of this type.

12. Object

> We call the elements of a class type objects.

如何创建对象：

    new Rational(1, 2)

13. Methods

类的方法相关的示例：

```scala
class Rational(x: Int, y: Int) {
  def numer = x
  def denom = y
  def add(that: Rational) =
    new Rational(numer*that.denom + that.numer*denom,
        denom*that.denom)
  def neg: Rational = new Rational(-numer, -denom)
  override def toString(): String = numer + "/" + denom
}
```

此处的`toStirng`方法是覆盖Object类的方法，因此，需要显式地加上一个`override`，否则将会出现编译错误。这个`toString`函数也可以简单地写成：

    override def toString = numer + "/" + denom

这也体现出了Scala语言本身语法的灵活性。

14. 值得注意的是，上述的Scala代码编译后得到的class文件反编译为java文件是这样的：

```java
public class Rational
{
  private final int x;
  private final int y;

  public int numer()
  {
    return this.x;
  }

  public int denom()
  {
    return this.y;
  }

  public Rational add(Rational that)
  {
    return new Rational(numer() * that.denom() + that.numer() * denom(), 
      denom() * that.denom());
  }

  public String toString()
  {
    return new StringBuilder().append(numer())
                              .append("/")
                              .append(BoxesRunTime.boxToInteger(denom()))
                              .toString();
  }
}
```

另外，我发现

    def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b, a%b)

对应的java代码是这样的：

    private int gcd(int a, int b)
    {
        for (;;)
        {
            if (b == 0) {
                return a;
            }
            b = a % b;a = b;
        }
    }

可见，Scala编译器对于递归的优化做得相当不错。

15. Data Abstraction: This ability to choose diﬀerent implementations of the data without
affecting clients is called data abstraction.

16. Self Reference：Scala中也有与Java对应的this的概念和语法。

17. Preconditions: 定义类的构造方法的前置条件require

在上面的分数的例子中，分母不能为0，可以使用require来做一下限制：

```scala
class Rational(x: Int, y: Int) {
  require(x != 0, "the denominator can't be zero.")
  def numer = x
  def denom = y
}
```

如果传入的参数y为0，将会产生一个IllegalArgumentException:

    var t = new Rational(10, 0)

抛出异常：

    java.lang.IllegalArgumentException: requirement failed: the denominator can't be zero.

关于require的详细解释如下：

> require is a predeﬁned function.

> It takes a condition and an optional message string.

> If the condition passed to require is false, an IllegalArgumentException is thrown with the given message string.

18. Assertions

失败时同样是抛出一个java.lang.IllegalArgumentException。

二者的区别：

+ require is used to enforce a precondition on the caller of a function.
+ assert is used as to check the code of the function itself.

19. 构造函数示例：

Scala中，直接使用this来定义构造函数。如下例：

    def this(x: Int) = this(x, 1)

20. Scala的求值模型为实参替换形参的求值模型(based on substitution)。

21. 操作符重载：

重载+, -, *, /, <, >

    def + (r: Rational) = new Rational(numer*r.denom + r.numer*denom,
                                       denom*r.denom)

重载负号(-)

符号是一个一元运算符(Unary operator)：

    def unary_- : Rational = new Rational(-numer, denom)

注意，`-`和`:`之间的空格不能省略，否则会导致编译器认为`-:`是一个运算符。 

Week 3: Data and Abstraction
----------------------------

1. Abstraction Classes, 抽象类。

Abstraction classes can contain members which are missing an implementations. No instance of an abstract class can be created with the operator new.

```scala
abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
}
```

2. Class Extensions

Scala中类支持继承，示例：

```scala
class Empty extends IntSet {
  def incl(x: Int): IntSet = ???
  def contains(x: Int): Boolean = ???
}
```

但，如果子类也是abstract修饰的，那么就可以不同实现父类的所有抽象方法。




Week 4: Types and Pattern Matching
----------------------------------

Week 5: Lists
--------------

Week 6: Collections
-------------------

Week 7: Lazy Evaluation
-----------------------







