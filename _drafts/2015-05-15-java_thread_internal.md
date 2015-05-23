---
title: Java 多线程--线程入门
author: He Tao
date: 2015-03-23
tag: [Java]
category: 编程语言
layout: post
---

Java多线程

1. 父线程结束，如果子线程正在运行，子线程不会跟着结束。
2. 每个线程拥有一个线程栈，会维护引用，用于GC。

Java中使用线程
--------------

Java提供了以下两种常用的方式来使用多线程：

1. 继承`Thread`类
2. 实现`Runnable`接口

### 继承`Thread`类

```
class MyThread extends Thread {
    @Override
    public void run() {
    
    } 
}

Thread t = new MyThread();
t.start();
```

### 实现`Runnable`接口

```
class MyThread implements Runnable {
    @Override
    public void run() {
    
    }
}

MyThread mt = new MyThread();
Thread t = new Thread(mt);
t.start();
```

需要注意的是：**一个线程只能start()一次**，如果多次调用`start()`方法，会出现`Java.lang.IllegalStateException`。

两种方式的区别在于继承`Thread`方式是每次都会创建一个`MyThread`对象，而如果采用实现`Runnable`接口的方式，可以使用同一个`MyThread`对象来创建多个线程，便于线程间共享内存。因此，设计线程间的数据共享时，一般应当采用实现`Runnable`接口的方式。

### Callable与Future

`java.util.concurrent`包中还提供了一个`Callable`接口来实现线程，使用`Callable`需要与`Future`类配合使用。


简单的线程同步
---------------

通常使用`synchronized`关键字来进行线程间的同步。`synchronized`有两种用法：修饰方法和修饰代码块。

### 修饰方法

可以使用`synchronized`关键字来修饰类中的方法，从而达到线程间同步的目的。示例：

```java
class A {
    public synchronized void Func() {
        // ...
    }
}
```

`synchronized`关键字修饰方法表明该方法在运行期间将会对`this`加锁，也就是说，当方法`Func`运行时其他方法无法操作`this`对象。需要注意的是，`synchronized`与`static`关键字是可以共存的。

### 修饰代码块

`synchronized`关键字除了用来修饰方法，还可以用来修饰代码块：

```java
class A {
    Integer i;
    // ...
    public void func() {
        // ...
        synchronized(i) {
            // ...
        }
    }
}
```

这表明在`synchronized`中的代码块运行期间将会占有锁`this.i`。`synchronized`修饰方法等价于下列写法：

```java
class A {
    public void Func() {
        synchronized(this) {
            // ...
        }
    }
}
```

可以看到，`synchronized`修饰代码块

可重入锁，与C++11的thread相比。

wait, notify, notifyAll

关于只读对象的安全和`volatile`
------------------------------

volatile

回调
-----

callback，使用Interface定义同一的接口

Lock/Condition同步机制
-----------------------

BlockingQueue的使用
-------------------

BlockingQueue,阻塞队列。
