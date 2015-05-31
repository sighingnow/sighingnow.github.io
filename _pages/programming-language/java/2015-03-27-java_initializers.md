---
title: Java对象初始化机制
author: He Tao
date: 2015-03-23
tag: [Java]
category: 编程语言
layout: post
---

在Java里，对象的初始化工作主要由构造函数完成，除此之外，静态初始化和实例初始化会完成另外一部分初始化工作。

构造函数初始化
---------------

类的构造函数承担类的实例化工作，Java中，每次使用`new`来创建一个新的对象时，都会调用对象的对应参数类型的构造函数。

Java中，构造函数的定义如下：

> A constructor is used in the creation of an object that is an instance of a class.

[文档](http://docs.oracle.com/javase/specs/jls/se8/html/jls-8.html#jls-8.8)

<!--more-->

静态初始化
----------

静态初始化仅仅在类初始化(类加载)的时候进行，大多数情形下，也即第一次实例化的时候进行，此后不再运行静态初始化相关的程序内容。

静态初始化由两部分内容组成，一部分为**静态**变量定义时直接初始化，例如：

    private static String name = "class name";

除了定义时直接初始化，还有静态初始化代码块(Static Initializers):

    StaticInitializer:
        static Block

> A static initializer declared in a class is executed when the class is initialized.

关于Static Initializers的文档，[祥见](http://docs.oracle.com/javase/specs/jls/se8/html/jls-8.html#jls-8.6)。如果一个类中有多个静态`static`代码块，将按照这些代码块在程序中出现的先后顺序依次执行。

如果一个类有`main(String [] args)`作为程序入口，这个类中的`static`语句块也会比main函数中的语句先执行。比如：

```java
public class Main {
    static {
        System.out.println("static block");
    }
    public static void main(String [] args) {
        System.out.println("main entry");
    }
}
```

编译，运行，在控制台得到如下输出：

    static block
    main entry

在直接通过类名来调用类的静态方法时，会首先调用类的静态代码块，或许，Java编译器在编译阶段便把所有static块的代码拷贝到了所有的static函数中（与下文中构造代码块被拷贝到构造函数中类似）。

由于static代码块具有这些性质，因此，很多初始化工作便可以通过static代码块的方式来放在类载入时执行，并且只执行一次。例如，在Java中载入C/C++编写的动态链接库时，便可以放在static代码块中进行。如下例：

```java
static {
    System.loadLibrary("Library");
}
```

实例初始化
----------

实例初始化表示构造函数之外的，每次类实例化生成新的对象的时候都会执行的一段代码。一部分实例初始化工作在定义类的**非静态**变量时直接初始化完成，例如:

    private int id = 12345;

此外，Java中还有一个构造代码块(Instance Initializers)的概念：

    InstanceInitializer:
        Block

> An instance initializer declared in a class is executed when an instance of the class is created.

关于构造代码块的更具体的定义，[详见](http://docs.oracle.com/javase/specs/jls/se8/html/jls-8.html#jls-8.6)

Java编译器会将这段初始化代码块拷贝到构造函数中去。因此，这一语法可以用于多个构造函数（重载构造函数）共享同一段代码并减少代码冗余。

另一个实例初始化的方式是采用一个`final`函数来初始化变量，如下例：

```java
class Whatever {
    private varType myVar = initializeInstanceVariable();       
    protected final varType initializeInstanceVariable() {
        // initialization code goes here
    }
}
```

单例模式
---------

在一些应用场景下，某些类只允许有一个实例存在，这种设计模式称为单例模式，通过限制类的构造函数的访问权限，可以对类的实例化采取有效的控制。那么，对于构造函数为`private`的类，又如何得到类的实例呢？这时便需要用到`static`方法来获取类的实例。

```java
class Demo {
    private Demo() {
        // ....
    }
    public static Demo getInstance() {
        return (new Demo());
    }
}
```

参考(Reference)：

1. [The Java® Language Specification](http://docs.oracle.com/javase/specs/jls/se8/html/index.html)
2. [The Java™ Tutorials](http://docs.oracle.com/javase/tutorial/java/javaOO/initial.html)


*注:*测试环境：java version "1.8.0_40"

