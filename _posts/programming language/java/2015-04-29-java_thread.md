---
title: Java 多线程--线程入门
author: He Tao
date: 2015-04-29
tag: [Java]
category: 编程语言
layout: post
---

随着多核芯片逐渐成为主流，大多数软件开发人员不可避免地需要了解并行编程的知识。而同时，主流程序语言正在将越来越多的并行特性合并到标准库或者语言本身之中。我们可以看到，JDK 在这方面同样走在潮流的前方。从JDK 5到JDK 7，越来越多的线程相关的新API加入到了标准库中，为不同场景下对线程实现与调度提供了完善的支持。

Java中使用线程
--------------

Java的多线程基于以下两个重要的机制：

1. 父线程结束，如果子线程正在运行，子线程不会跟着结束。
2. 每个线程拥有一个线程栈，会维护引用，用于GC。

<!--more-->

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

```java
public class TEMP {
    public static void main(String[] args) throws Exception {
         FutureTask<Integer> task = new FutureTask<>(new FF());
         new Thread(task).start();
         System.out.println("main thread");
         System.out.println(task.get());
    }
}

class FF implements Callable<Integer> {
    @Override
    public Integer call() throws Exception {
        Thread.sleep(2000);
        System.out.println("ff thread");
        return new Random().nextInt(100);
    }
}
```

程序运行的输出为：

    main thread
    ff thread
    34

`Callable`接口与`Runnable`接口类似，不同的是，`call`方法是可以有返回值的，并且可以抛出异常。如果没有`System.out.println(task.get())`，那么主线程在运行完`System.out.println("main thread")`就会结束，但是因为有`task.get()`，因此，主线程会等待`Callable`线程执行完毕。

与`Callable-Future`类似的一个方法是回调(callback)。在子线程结束时调用父线程的方法来通知父线程。

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

可以看到，`synchronized`修饰代码块与修饰方法类似，但控制粒度更细，同步开销也较小。但是，在尽量减小同步代码块规模时应该注意原子操作中的语句必须在同一个同步代码块中。

### 静态方法同步

要同步静态方法，需要一个作用于整个类对象的锁。这个锁就是类自身。举例：

```java
class A {
    public static void func() {
        synchronized(B.class) {
            // ...
        }
    }
}
```

### 可重入锁

另一个需要注意的地方是Java的同步锁匙可重入锁，例如，递归调用`run`方法或者调用的函数有同样的`synchronized`锁并不会造成死锁。示例如下：

```java
class KK extends Thread {
    static Integer x = 10;
    @Override
    public void run() {
        synchronized (x) {
            if(x > 5) {
                x -= 1;
                System.out.println(x);
                run();
            }
        }
    }
}
```

`wait`, `notify`, `notifyAll`，`join`是常用的线程控制调度方法。

对象安全和`volatile`
------------------------------

`volatile`是一个声明，`volatile`关键字表示内存可见，`volatile`声明的变量不会被copy到线程的本地内存(**缓存、缓冲区、寄存器，Cache等**)中，而是直接在主存上进行操作。每次使用前从主存中读取新值，修改后立即写入主存，这用于防止线程间因寄存器和缓存引起的肮脏数据的读取和使用。

进一步的，JVM的规范里并没有要求`volatile`关键字修饰的变量一定不能被copy到线程的本地内存中，而是要求对其读写需要遵循`happens-before`语义，类似于互斥锁。同时，`volatile`还涉及到编译器的指令重排和CPU的重排序等等（影响变量之间的访问顺序）。

有些时候，可能一个线程只需要**读**一个变量的值，这时也有可能需要使用`volatile`的方式来防止出现同步错误。但需要注意的是，`volatile`不能和`final`一起共同修饰一个变量，因为`final`变量是不可改的。

Lock/Condition机制
------------------

Lock是控制多个线程对共享资源进行访问的工具。通常，锁提供了对共享资源的独占访问，每次只能有一个线程对Lock对象加锁，线程开始访问共享资源之前应先获得Lock对象。不过某些锁支持共享资源的并发访问，如：ReadWriteLock（读写锁），在线程安全控制中，通常使用ReentrantLock（可重入锁）。使用该Lock对象可以显示加锁、释放锁。

使用Lock的一般框架：

```java
Lock lock = new ReetrantLock();
lock.lock();
try {
    // ...
}
finally {
    lock.unlock();
}
```

注意，`try`中的语句块是可能抛出异常的，因此，如果可能有异常抛出，那么`lock.unlock()`必须在`finally`语句块中，以确保JVM会释放锁。通常认为：Lock提供了比synchronized方法和synchronized代码块更广泛的锁定操作，Lock更灵活的结构，有很大的差别，并且可以支持多个Condition对象。

Condition的功能更类似于传统多线程技术中的`Object.wait()`(`Condition.await()`)和`Object.notifyAll()`(`Condition.signal()`)，用于实现线程间通信，一个Lock锁可以支持多个Condition。

一个使用Lock/Condition机制的例子：

```java
class AA implements Runnable {
    Lock lock = new ReentrantLock();
    @Override
    public void run() {
        lock.lock();
        System.out.println("get lock");
        // ...
        System.out.println("release lock");
        lock.unlock();
    }
}
```

在这里面，可以配合使用Condition完成较复杂的线程管理和调度。在[Java多线程之Condition接口的实现](http://blog.csdn.net/huang_xw/article/details/7090122)一文中，作者使用`Condition/Lock`机制来实现了经典的“生产者-消费者”调度模型。在很多情景下，Condition提供了一种更加高效的、更有针对性的线程调度和同步方式。

ReentrantLock深究
-----------------

BlockingQueue的使用
-------------------

BlockingQueue,阻塞队列。BlockingQueue是一种特殊的Queue，若BlockingQueue是空的，从BlockingQueue取东西的操作将会被阻断进入等待状态直到BlocingkQueue进了新货才会被唤醒。同样，如果BlockingQueue是满的任何试图往里存东西的操作也会被阻断进入等待状态，直到BlockingQueue里有新的空间才会被唤醒继续操作。

BlockingQueue提供的方法主要有：

1. `add(anObject)`: 把anObject加到BlockingQueue里，如果BlockingQueue可以容纳返回true，否则抛出IllegalStateException异常。

2. `offer(anObject)`: 把anObject加到BlockingQueue里，如果BlockingQueue可以容纳返回true，否则返回false。

3. `put(anObject)`: 把anObject加到BlockingQueue里，如果BlockingQueue没有空间，调用此方法的线程被阻断直到BlockingQueue里有新的空间再继续。

4. `poll(time)`: 取出BlockingQueue里排在首位的对象，若不能立即取出可等time参数规定的时间。取不到时返回null。

5. `take()`: 取出BlockingQueue里排在首位的对象，若BlockingQueue为空，阻断进入等待状态直到BlockingQueue有新的对象被加入为止。

根据不同的需要BlockingQueue有4种具体实现：

1. `ArrayBlockingQueue`：规定大小的BlockingQueue，其构造函数必须带一个int参数来指明其大小。其所含的对象是以FIFO（先入先出）顺序排序的。

2. `LinkedBlockingQueue`：大小不定的BlockingQueue，若其构造函数带一个规定大小的参数，生成的BlockingQueue有大小限制。若不带大小参数，所生成的BlockingQueue的大小由Integer.MAX_VALUE来决定。其所含的对象是以FIFO（先入先出）顺序排序的。LinkedBlockingQueue和ArrayBlockingQueue比较起来，它们背后所用的数据结构不一样，导致LinkedBlockingQueue的数据吞吐量要大于ArrayBlockingQueue，但在线程数量很大时其性能的可预见性低于ArrayBlockingQueue。

3. `PriorityBlockingQueue`：类似于LinkedBlockingQueue，但其所含对象的排序不是FIFO，而是依据对象的自然排序顺序或者是构造函数所带的Comparator决定的顺序。

4. `SynchronousQueue`：特殊的BlockingQueue，对其的操作必须是放和取交替完成的。

BlockingQueue特别适用于线程间共享缓冲区的场景。BlockingQueue的四种实现也能够满足大多数的缓冲区调度需求。

Fork/Join
----------

线程池的实现
------------

参考
----

1. [Java 理论与实践: 正确使用 Volatile 变量](http://www.ibm.com/developerworks/cn/java/j-jtp06197.html)
2. [用happen-before规则重新审视DCL](http://www.iteye.com/topic/260515)
3. [Java多线程之Condition接口的实现](http://blog.csdn.net/huang_xw/article/details/7090122)
4. [ReentrantLock(重入锁)以及公平性](http://ifeve.com/reentrantlock-and-fairness/)


