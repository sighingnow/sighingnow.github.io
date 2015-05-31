---
title: Java继承中的小细节
author: He Tao
date: 2015-03-23
tag: [Java]
category: 编程语言
layout: post
---

Java继承机制是其Object-Oriented特性之一，使用继承`extends`时有很多需要注意的容易出错的小细节。例如，构造函数的调用顺序和规则、

构造函数
--------

首先从构造函数说起。

对于一个类，如果没有显式定义构造函数，那么便有一个无参的默认构造函数。但是，一定自己定义了构造函数（无论有没有参数），就都只能使用自己定义的构造函数。

继承时，如果子类没有显式定义构造函数，那么便会去寻找与调用的参数匹配的父类的构造函数。一旦子类定义了一个构造函数，无论参数类型，`new`构造子类对象时都不会直接匹配父类的同类型参数构造函数。如果不存在调用参数类型的构造函数，编译时会出现错误。需要**注意**的是，就算子类中没有定义构造函数，子类也没有默认的无参构造函数。

<!--more-->

继承时，`new`生成子类对象时，如果调用的子类构造函数中没有使用`super`语句，那么将会首先调用父类的无参构造函数，如果父类没有无参构造函数，编译时会出现错误。如：

```java
class A {
    public A(String name) {
        System.out.println("A1 is called");
    }
}

class B extends A {
    public B() {
        System.out.println("B0 is called");
    }
    public B(String name) {
        System.out.println("B1 is called");
    }
}
```

无论是
    
    B b = new B()

还是

    B b = new B("b")

编译时都会出错。错误信息为：

    对于A(), 找不到合适的构造器。

如果子函数中使用了`super`语句(**注意：**`super`语句只能写在子类构造函数的*第一行*)，此时，便会根据`super`的参数类型去调用父类中相同参数类型的构造函数。如果父类中不存在以`super`中的参数为参数的构造函数，编译时会报错。

当存在多级继承时，会按照上面的原则逐级向上调用构造函数，知道顶层类。如下面的一段代码：

```java
public class Main {
    public static void main(String [] args) {
        C c = new C(10);
    }
}

class A {
    public A() {
        System.out.println("A0 is called");
    }
}

class B extends A {
    public B(String name) {
        System.out.println("B1 is called");
    }
}

class C extends B {
    public C(int id) {
        super(String.valueOf(id));
        System.out.println("C2 is called");
    }
}
```

编译，运行，会得到如下的输出：

    A0 is called
    B1 is called
    C2 is called

如果类中有`static`语句块，那么实例化子类时会从祖类到子类依次执行`static`代码块的内容，然后按照上文的顺序依次调用构造函数。这实际上也是由`static`语句块的性质所决定的。`static`在类初始化（类加载）时执行，并且仅仅在此时执行。

```java
public class Main {
    static {
        System.out.println("main static block");
    }
    public static void main(String [] args) {
        B.staticfunc();
    }
}

class A {
    static {
        System.out.println("A static block");
    }
}

class B extends A {
    static {
        System.out.println("B static block");
    }
    public static void staticfunc() {
        System.out.println("B static func is called");
    }
}
```

编译、运行，输出内容为：

    main static block
    A static block
    B static block
    B static func is called

如果类中有构造代码块，那么构造代码块执行顺序也是从祖类到子类依次执行。这是由于在编译阶段Java编译器会将构造代码块拷贝到类的构造函数中去。因此，使用继承机制时，构造代码块的执行顺序与构造函数的执行顺序相同。

关于构造函数、静态初始化和示例初始化的相关内容，详见[Java对象初始化机制](./java_initializers)。

以上，是Java中构造函数在继承机制中的大致行为。

覆盖
----

覆盖对于继承机制至关重要，覆盖使得子类能够在父类的基础上扩展其行为，并与其父类和兄弟类具有一致的对外的接口。

访问一般的函数，总是访问继承关系最近的函数。也就是说，访问子类的函数时，会根据继承关系链向上回溯，知道找到被访问的函数的实现为止。Java的单一继承的机制保证了不会在寻找父类的方法时出现多个父类具有同名同参数所带来的矛盾。C++支持多继承，但是在C++中，可以使用`父类名::子类名`的语法来直接指定调用哪一个父类的方法。

类型转换
---------

Java中涉及对象的类型转换时，只允许向上转换。即由子类向父类转换，否则会出现运行时错误：

    java.lang.ClassCaseException

`instanceof`、`getClass`与`getSuperclass`
-----------------------------------------

上文提到了错误的类型转换会产生异常`Exception`，那么，又如何知道能够进行类型转换呢？

这是便需要用到`instanceof`运算符。它可以判断一个对象是否为某个类的实例，如果是，那么便可以进行类型转换：

```java
Cls c = null;
if(obj instanceof Cls) {
    c = (Cls)obj;
}
```

在涉及到继承时，使用`instanceof`，对于这个对象本身的类极其所有的子类的结果都为`true`。也就是说，子类的实例也是其父类的实例。

除了`instanceof`以外，还可以用`getClass`方法来判断一个对象的类型。`getClass`是`Object`类的一个方法，因此，在Java中，所有类都有一个继承来的`getClass`方法。`getClass`方法返回的实例化这个对象时所使用的类。也就是说，将一个对象向上做类型转换后，这个对象的`getClass`方法返回的仍然是那个实例化它的子类。

```java
public class Main {
    public static void main(String [] args) {
        B b = new B();
        A a = (A) b;

        System.out.println(b.getClass());
        System.out.println(a.getClass());
        
        System.out.println(a instanceof A);
        System.out.println(b instanceof A);
        System.out.println(a instanceof B);
        System.out.println(b instanceof B);
    }
}

class A {
}
class B extends A {
}
```

编译，运行，会得到如下的输出：

    Class B
    Class B
    true
    true
    true
    true

那么，又如何判断某个类是否继承自另外一个类呢？Java中还提供了一个`getSuperclass`方法。这个方法是`java.lang.Class`类的一个方法，会返回一个类的直接父类，具体用法如下：

    对象名.getClass().getSuperclass()

或：

    类名.getSuperclass()

`getSuperclass`仅仅能得到一个类的直接父类，那么，又如何判断两个类之间有无间接继承关系呢？这是可以使用`java.lang.Class`类的`isAssignableFrom`方法：

> Determines if the class or interface represented by this Class object is either the same as, or is a superclass or superinterface of, the class or interface represented by the specified Class parameter.

对于

    A.class.isAssignableFrom(B.class)

如果`B`直接或间接继承于`A`，那么将返回`true`，否则返回`false`。

继承与接口
----------

继承关注的是数据规格以及对数据的相关操作，而接口更关心的行为。接口的设计很大程度上避免了多重继承的滥用。

参考：

1. [15.20.2. Type Comparison Operator `instanceof`](http://docs.oracle.com/javase/specs/jls/se8/html/jls-15.html#jls-15.20.2)
2. [How do I determine if a class extends another class in Java?](http://stackoverflow.com/questions/4100281/how-do-i-determine-if-a-class-extends-another-class-in-java)


*注:*代码测试环境：java version "1.8.0_40"



