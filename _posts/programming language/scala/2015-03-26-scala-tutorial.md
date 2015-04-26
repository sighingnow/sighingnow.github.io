---
title: Scala 入门笔记
author: He Tao
date: 2015-03-28
tag: [Scala]
category: 编程语言
layout: post
---

Scala初学笔记。

Hello World in Scala
---------------------

学习Scala的语法，还是从Hello World开始吧：

```scala
object HelloWorld {
  def main(args: Array[String]) {
    print("Hello World, Scala!")
  }
}
```

<!--more-->

编译,

    scalac HelloWorld.scala

<!--more-->

运行,

    scala -classpath . HelloWorld

在控制台输出：

    Hello World, Scala!

跟Java挺像！需要**注意**的是，`main`函数没有返回值(procedure method)。

在Scala中，可以每行写一条语句，行末不用使用`;`分隔，如果在同一行书写多条语句，语句间需要用`;`隔开。

Interaction with Java
---------------------

Scala运行于JVM之上，Scala代码也很容易与Java代码进行交互。Scala中，可以使用`import`来导入Java的包，`java.lang`包会默认导入，其他的包需要显式导入。Scala中的`import`与Java相比有一些语法上的扩展，使得更灵活易用。例如：

    import java.lang.{Math, Boolean} // import Math 和 Boolean
    import java.lang.Math._ // import java.lang.Math包中的所有内容

Scala与Java进行代码级的交互的例子：

```scala
import java.util.{Data, Locale}

object Main {
  def main(args: Array[String]) {
    val now = new Date
    print(now)
  }
}
```

编译，运行，得到输出：

    Thu Mar 26 23:31:14 CST 2015

面向对象特性
------------

Scala是一门纯面向对象的语言(a pure object-oritented language)，一切皆对象，(everything is an object)，包括数字、函数等。在这一点上，Scala与Java之间存在差异，Java中区分基本类型与引用类型，例如boolean与Boolean、int与Integer，并且，在Java中，函数不能被当成值来操作。

纯面向对象的一个体现：

    1+2*3

等价于：

    1.+(2.*(3))

运算符`+`、`-`、`*`、`/`等都是number对象的方法。

Scala中，函数也是对象，可以把函数当成值来传参和作为函数返回值，这也是Scala函数式编程特性的体现。将函数作为参数传递时类似C/C++中的函数指针。如下例：

```scala
object Main {  

  def timer(callback: () => Unit) : Unit {
    var cnt = 0       // var表示定义变量
    while(cnt < 10) {
      Thread sleep 2000
      cnt += 1
      callback()
    }
  }

  def task() : Unit {
    println("working...")
  }

  def main(args: Array[String]) : Unit {
    timer(task)
  }
```

此处，`timer`函数进行传递回调函数是，还可以使用匿名函数，写成这样：

```scala
    timer(() => Unit {
      println("working...")
    })
```

面向对象自然少不了类的概念，在Scala中，也是用`class`关键字来定义类。例如，用Scala定义一个Person类：

```scala
class Student {
  private var id = Int.MaxValue
  def setId(id: Int) {
    
  }
}
class Person(id: Integer, name: String) {
}
```

可以用

    var p = new Person(10, "abcd")

来实例化得到一个Person类的对象p。

同样，在类中也可以定义类的方法和属性，只是在这一点上Scala更多地具有函数式编程的特点。在这一点上，Scala的语法与Haskell的**“绑定”**类似。举例：



```scala
class Person(id: Integer, name: String) {
  def aid = id
  def aname = name
  def getId(pid: Integer) = id
  def getName(pname: String) = name
}
```

实例化类得到对象并调用类的方法，操作(读/写)类的属性：



