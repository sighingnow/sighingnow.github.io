---
title: 线程和锁
author: Tao He
date: 2015-08-23
tag: [并发模型]
category: 并发模型
layout: post
---

线程与锁模型是并发程序的经典架构，本质上是对底层硬件的运行过程的形式化。这种形式化是其最大的优点，也是其最大的缺点。该模型中，并发的基本单位是线程，并通过锁提供同步机制，保证线程的安全。

<!--more-->

线程和内置锁
----------

Java通过`synchronized`块提供了强制原子性的内置锁机制，synchronized块包含所对象的引用以及这个锁保护的代码块，扮演了互斥锁(mutex)的角色。一个简单的例子：

~~~java
private int count = 0;

public synchronized void increase() {
    count++;
}
~~~

这段代码的字节码：

~~~
getfield    #15
iconst_1
iadd
putfiled    #15
~~~

如果不同步，任意两条语句之间具有可能被打断，在多线程环境下这段代码将会出现 race condition 。

当一个线程想要使用多把锁是，就需要考虑死锁的问题。有一个简单的规则来避免死锁问题：**总是按照一个全局的固定的顺序获取锁**。以经典的哲学家进餐问题为例：

~~~java
import java.util.Random;

class Philosopher extends Thread {
    private Chopstick left, right;
    private Random random;

    public Philosopher(Chopstick left, Chopstick right) {
        this.left = left;
        this.right = right;
        random = new Random();
    }

    public void run() {
        try {
            while (true) {
                Thread.sleep(random.nextInt(1000)); // Think for a while
                synchronized (left) { // Grab left chopstick //
                    synchronized (right) { // Grab right chopstick //
                        Thread.sleep(random.nextInt(1000)); // Eat for a while
                    }
                }
            }
        } catch (InterruptedException e) {
        }
    }
}
~~~

很显然，这段代码存在死锁问题，但是，如果在获取左、右筷子是按照一定的顺序加锁(按照筷子的编号来拿起筷子)，就可以避免这个问题：

~~~java
public Philosopher(Chopstick left, Chopstick right) {
    if(left.getId() < right.getId()) {
        first = left; second = right;
    } else {
        first = right; second = left;
    }
    random = new Random();
}
~~~

一个常用的技巧是适用对象的HashID作为锁的全局顺序，好处在于每个对象都有一个id值，不用为锁专门定义并维护一个顺序表。但是，对象的散列值并不能保证唯一(可能重复，尽管几率很小)。

乱序执行和内存可见性
-----------------

~~~java
public class Visibility {
    private static boolean ready;
    private static int number;

    private static class ReaderThread extends Thread {
        public void run() {
            while (!ready)
                Thread.yield();
            System.out.println(number);
        }
    }

    public static void main(String[] args) {
        new ReaderThread().start();
        number = 42;
        ready = true;
    }
}
~~~

在某些情形下，这段代码会出现输出0的运行结果，原因便是因为乱序执行。

+ 编译器的静态优化可以打乱代码的执行顺序
+ JVM的动态优化可以打乱代码的执行顺序
+ 硬件可以打乱代码的执行顺序

在Java内存模型中，如果读线程和写线程不进行同步，iu不能保证内存可见性。Java通过`volatile`关键字提供了与此相关的低级别的同步机制。将变量标记为`volatile`，可以保证对变量的读和写不被乱序执行。进一步的，JVM的规范里并没有要求`volatile`关键字修饰的变量一定不能被copy到线程的本地内存中，而是要求对其读写需要遵循`happens-before`语义，类似于互斥锁。

volatile机制并没能解决上文提到的`count++`那一类的同步问题。随着JVM被不断优化，其提供了一些低开销的锁机制。volatile变量的适用场景也越来越少，考虑考虑使用volatile变量，也许应当在`java.util.concurrent.atomic`包中找到更合适的选择。

ReentrantLock与条件变量
----------------------

synchronized块的限制在于获取锁和释放锁的代码都必须严格嵌在同一个方法中。并且，**一个线程因为等待内置锁而进入阻塞状态之后，就无法中断这个线程了**。在尝试获取内置锁时，也无法设置超时。与synchronized不同，`ReentrantLock`提供了显式的`lock`和`unlock`方法，很好地克服了synchronized机制的几个问题。程序的大体框架如下：

~~~java
Lock lock = new ReentrantLock();
lock.lock();
try {
    // ...
} finally {
    lock.unlock();
}
~~~

`ReentrantLock`提供了`lockInterruptibly`方法，可以使用`Thread.interrupt()`方法来中断一个正在等待锁的线程。此外，`tryLock()`方法可以设置超时，提供了从死锁中恢复的手段。通过采用为每个线程设置不同的超时时间等策略，可以有效减少所有线程超时后又重新陷入死锁的几率。

并发编程中，经常需要等待某个事件的发生。条件变量就是为了这种情况而设计的。一般框架如下：

~~~java
ReentrantLock lock = new ReentrantLock();
Condition condition = lock.newCondition();
lock.lock();
try {
    while (!<<condition is true>>)
        condition.await();
    <<use shared resources>>
} finally { lock.unlock(); }
~~~

通过条件变量来解决哲学家进餐问题的一个例子：

~~~java
import java.util.Random;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

class Philosopher extends Thread {
    private boolean eating;
    private Philosopher left;
    private Philosopher right;
    private ReentrantLock table;
    private Condition condition;
    private Random random;

    public Philosopher(ReentrantLock table) {
        eating = false;
        this.table = table;
        condition = table.newCondition();
        random = new Random();
    }

    public void setLeft(Philosopher left) {
        this.left = left;
    }

    public void setRight(Philosopher right) {
        this.right = right;
    }

    public void run() {
        try {
            while (true) {
                think();
                eat();
            }
        } catch (InterruptedException e) {
        }
    }

    private void think() throws InterruptedException {
        table.lock();
        try {
            eating = false;
            left.condition.signal();
            right.condition.signal();
        } finally {
            table.unlock();
        }
        Thread.sleep(1000);
    }

    private void eat() throws InterruptedException {
        table.lock();
        try {
            while (left.eating || right.eating)
                condition.await();
            eating = true;
        } finally {
            table.unlock();
        }
        Thread.sleep(1000);
    }
}
~~~

现在只有一把锁(table)，我们将竞争条件从对筷子的争夺转换为对状态的判断，通过判断左右两人的状态来决定自己是否可以同时拿到两双筷子，进餐。这一改进也使得程序的并发性能显著提高。

原子变量(atomic)
---------------

原子变量(Atomic Variables)是对于内置锁的另一替代方案，以`count++`为例，我们可以使用原子变量，从而并没在程序中手动、显式地使用锁进行同步。

~~~java
import java.util.concurrent.atomic.AtomicInteger;

final AtomicInteger counter = new AtomicInteger();
class CountingThread extends Thread {
    public void run() {
        for (int x = 0; x < 10000; ++x)
            counter.incrementAndGet();
    }
}
~~~

原子变量的好处在于有效解决了由于没有在正确的时候获取锁而导致的不一致性问题，同时，没有锁的参与，对原子变量的操作不会引发死锁。原子变量是无锁非阻塞算法(non-blocking, lock-free algorithms)的基础，这种算法可以不用锁和阻塞来达到同步的目的。`java.util.concurrent`包中的类都尽量使用了无锁的代码。

写时复制(Copy on Write)
----------------------

设想有这样一个类：

~~~java
class Manager extends Thread {
    public synchronized void addListener(ProgressListener listener) {
        listeners.add(listener);
    }
    public synchronized void removeListener(ProgressListener listener) {
        listeners.remove(listener);
    }
    public synchronized void updateProgress(int n) {
        for (ProgressListener listener : listeners)
            listener.onProgress(n);
    }
}
~~~

这三个方法都已经使用了`synchronized`做了同步，但是，仍然有可能发生死锁。在`updateProgress`方法中，调用了`listener`的`onProgress`方法，如果这个方法中将会持有另一把锁，因为对加锁顺序一无所知，因此，这就有可能发生死锁。一个解决思路是避免拥有锁时调用外部方法：

~~~java
public void updateProgress(int n) {
    ArrayList<ProgressListener> listenersCopy;
    synchronized(this) {
        listenersCopy = (ArrayList<ProgressListener>)listeners.clone();
    }
    for (ProgressListener listener : listenersCopy)
        listener.onProgress(n);
}
~~~

但是，这样的做法会产生一定的性能缺陷。因为更新`listeners`的几率比较小，因此，不必再每次调用时`updateProgress`时都创建一个副本。此时便能够体现出Copy-on-Write策略的好处(Java中，使用java.util.concurrent.CopyOnWriteArrayList)。

~~~java
listeners = new CopyOnWriteArrayList<ProgressListener>();
public void updateProgress(int n) {
    for (ProgressListener listener : listenersCopy)
        listener.onProgress(n);
}
~~~

Fork/Join框架与并行流
-------------------

Fork/Join机制是JDK 7新增加的多线程框架，如果一个应用能被分解成多个子任务，并且组合多个子任务的结果就能够获得最终的结果。这一机制使用和线程池和工作窃取(work-stealing)算法来实现task的调度和对线程池的高效利用。

工作窃取（work-stealing）算法是指某个线程从其他队列里窃取任务来执行。核心思想在于以下两点：

1. 将一个任务分解为多个互不依赖的子任务。
2. 当一个线程完成自己队列中的任务后，将其他线程的任务队列里的任务取出来执行。
