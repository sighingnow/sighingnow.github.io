---
title: Rust-Book Effective Rust
author: He Tao
date: 2015-06-05
tags: Rust
category: 编程语言
layout: post
---

The Stack and the Heap
----------------------

栈内存访问比堆内存访问快，局部变量存放在栈上。

Box: A pointer type for heap allocation.可以使用`Box<T> type`来申请对内存空间。例如：

```rust
fn main() {
    let x = Box::new(5);
    ley y = 42;
}
```

`Box`分配的堆内存资源可以使用`Drop`来释放。Rust没有垃圾回收(GC)机制。

Rust使用[jemalloc][1]来进行内存分配，

Testing
--------

使用`test`属性来表明一个函数是测试函数。运行`cargo test`将会运行项目中所有的测试用例(具有`test`属性的)。例子：

```rust
#[test]
fn it_works() {
    // ...
}
```

在测试函数中，可以使用`assert`来判断变量的值是否符合预期。

```rust
#[test]
fn it_works() {
    // ...
    assert!(false);
}
```

注意，此处的`assert!`也是一个宏(Macro)，跟`println!`一样。`assert`用于检查一个逻辑值是否为真，此外，还有`assert_eq!`来用来检查两个变量是否相等，等等。

Conditional Compilation
-----------------------

条件编译，Rust使用`#[cfg]`属性来控制条件编译选项。例如：

    #[cfg(all(unix, target_pointer_width = "32"))]

也可以直接在`Cargo.toml`中通过参数的方式来控制这些条件编译选项的值。

    [features]
    # no features by default
    default = []
    # The “secure-password” feature depends on the bcrypt package.
    secure-password = ["bcrypt"]

还可以直接通过给`rustc`命令传递参数的方式来进行控制。类似于gcc通过`-D`选项来控制宏定义及其值。

    --cfg feature="${feature_name}"

Rust Documentation
-------------------

Rust允许如下的注释方式：

1. `/*....*/`
2. `//`
3. `///`

其中，第三种方式称为`Rust Doc`，可以使用Markdown的语法。可以使用`rustdoc`命令来生成文档。

文档格式遵循[RFC 505](https://github.com/rust-lang/rfcs/blob/master/text/0505-api-comment-conventions.md)标准。

此外，还可以使用`doc`属性(`doc` attributes)。例如：

```rust
/// this

#[doc="this"]
```

等价于下面的写法：

```rust
//! this

#![doc="/// this"]
```

Iterators
---------

Iterators, 迭代器，几乎所有的现代编程语言里都直接提供了原生的对迭代器和迭代器模式的支持。

```rust
for i in 1..10 {
    println!("{}", i);
}
```

其中，1..10将会生成一个`1,2,3,4,5,6,7,8,9`的迭代器。如果给出的上界小于下界，将会返回一个空迭代器。

```rust
let mut range = 1..10;
loop {
    match range.next() {
        Some(x) => { println!("{}", x); },
        None => { break; }
    }
}
```

`next` returns an `Option<i32>`, in this case, which will be `Some(i32)` when we have a value and `None` once we **run out**. If we get `Some(i32)`, we print it out, and if we get `None`, we break out of the loop.

Consumers
---------

### collect()

    let one_to_one_hundred : Vec<i32> = (1..101).collect();
    let one_to_one_hundred = (1..101).collect::<Vec<i32>>();
    let one_to_one_hundred = (1..101).collect::<Vec<_>>();

`collect()`可以自动类型推断。

### find()

    let a = (0..100).find(|x| *x > 50);

### fold()

    let sum = (1..4).fold(0, |sum, x| sum + x);

fold() is a consumer that looks like this: fold(base, |accumulator, element| ...)

### Iterators

```rust
let nums = vec![1,2,3];
for n in nums.iter() {
    println!("{}", n);
}
```

### Iterator adapters

    (1..100).map(|x| println!("{}", x));

跟Python 3中的`map`类似，此处并不会直接执行。

> iterator adaptors are lazy and do nothing unless consumed.

    (1..100).map(|x| println!("{}", x)).collect::<Vect<_>>();

只有使用其结果时，才会执行。

```rust
for i in (1..10).take(5) { // (1..).take(5)
    println!("{}", i);
}
```

将会只输出迭代器的前5项的值。

`filter()` is an adapter that takes a closure as an argument. This closure returns true or false. The new iterator `filter()` produces only the elements that that closure returns true for:

```rust
for i in (1..100).filter(|&x| x % 2 == 0) {
    println!("{}", i);
}
```

一个更复杂的filter的例子：

```rust
(1..1000).filter(|&x| x%2 == 0)
         .filter(|&x| x%3 == 0)
         .take(5)
         .collect::<Vec<i32>>();
```

Concurrency
------------

多线程安全：

1. 内存安全，没有数据竞争
2. Rust的类型系统

两个traits: `Send`和`Sync`

#### `Send`

The first trait we're going to talk about is `Send`. When a type `T` implements `Send`, **it indicates to the compiler that something of this type is able to have ownership transferred safely between threads**.

This is important to enforce certain restrictions. For example, if we have a channel connecting two threads, we would want to be able to send some data down the channel and to the other thread. Therefore, we'd ensure that `Send` was implemented for that type.

In the opposite way, if we were wrapping a library with FFI that isn't threadsafe, we wouldn't want to implement `Send`, and so the compiler will help us enforce that it can't leave the current thread.

通过类型来控制线程间的数据共享（线程间的所有权ownership切换）。

#### `Sync`

The second of these traits is called `Sync`. When a type `T` implements `Sync`, **it indicates to the compiler that something of this type has no possibility of introducing memory unsafety when used from multiple threads concurrently.**

For example, sharing immutable data with an atomic reference count is threadsafe. Rust provides a type like this, `Arc<T>`, and it implements `Sync`, so it is safe to share between threads.

正是这两种模型为线程间的数据安全提供了保障。

### Threads

使用线程的例子：

```rust
fn main() {
    thread::spawn(|| {
        println!("abcde");
    });
    // thread::sleep_ms(100);
}
```

> The `thread::spawn()` method accepts a closure, which is executed in a new thread. It returns a handle to the thread, that can be used to wait for the child thread to finish and extract its result.

```rust
fn main() {
    /**
    thread::spawn(|| {
        println!("abcde");
    }).join();
    **/
    let handle = thread::spawn(|| {
        "abcde"
    });
    println!("{}", handle.join().unwrap());
}
```

Rust在多线程方面的优势：

> Many languages have the ability to execute threads, but it's wildly unsafe. There are entire books about how to prevent errors that occur from shared mutable state. Rust helps out with its type system here as well, by preventing data races at compile time. Let's talk about how you actually share things between threads.

### Safe Shared Mutable State

> Shared mutable state is the root of all evil. Most languages attempt to deal with this problem through the 'mutable' part, but Rust deals with it by solving the 'shared' part.

Rust的所有权机制`ownership system`可以有效地防止指针错用、消除数据竞争。

在Rust中，如下代码将会直接编译失败：

```rust
use std::thread;
fn main() {
    let mut data = vec![1u32, 2, 3];
    for i in 0..3 {
        thread::spawn(move || {
            data[i] += 1;
        });
    }
    thread::sleep_ms(50);
}
```

因为此处有三个线程会共享`data`,也就是说，`data`将会有三个`owner`。

<!---------------------------------------------
Rust书中代码和叙述与实际编译、运行情况不符。
---------------------------------------------->

### Channels

### Panics

A `panic!` will crash the currently executing thread. You can use Rust's threads as a simple isolation mechanism:

```rust
use std::thread;
let result = thread::spawn(move || {
    panic!("oops!");
}).join();
assert!(result.is_err());
```

Our `Thread` gives us a `Result` back, which allows us to check if the thread has panicked or not.

_注_: 上述代码摘自Rust Book.



<!---------------------------links------------------------------>

[1]: http://www.canonware.com/jemalloc/
[2]: http://www.cs.northwestern.edu/~pdinda/icsclass/doc/dsa.pdf


