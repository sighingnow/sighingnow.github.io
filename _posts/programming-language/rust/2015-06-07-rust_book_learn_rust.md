---
title: Rust-Book Learn Rust
author: He Tao
date: 2015-06-05
tags: Rust
category: 编程语言
layout: post
---

Guessing Game
--------------

Our program will generate a random integer between one and a hundred. It will then prompt us to enter a guess. Upon entering our guess, it will tell us if we’re too low or too high. Once we guess correctly, it will congratulate us.

### Set Up

创建项目：

    cargo new guessing_game --bin

`--bin`表示创建二进制可执行程序(binary)而不是程序库(library)。

项目目录结构(`cargo build`之后)：

<!--more-->

```
│  .gitignore
│  a.txt
│  Cargo.lock
│  Cargo.toml
│  
├─src
│      main.rs
│      
└─target
    └─debug
        │  guessing_game.exe
        │  
        ├─.fingerprint
        │  └─guessing_game-375af41f7208f0f5
        │          bin-guessing_game
        │          dep-bin-guessing_game
        │          
        ├─build
        ├─deps
        ├─examples
        └─native
```

### Processing a Guess

代码(src/main.rs)：

```
use std::io;
fn main() {
    println!("Guess the number!");
    println!("Please input your guess.");
    let mut guess = String::new();
    io::stdin().read_line(&mut guess)
        .ok()
        .expect("Failed to read line");
    println!("You guessed: {}", guess);
}
```

代码解释：

`use std::io`: 使用标准输入输出库(IO library)。

`let`：let语句用户变量绑定(variable bindings)。默认是`immutable`，如果想要`mutable`类型需要手动指明。

`String`: A String is a growable, UTF-8 encoded bit of text.

`::new`: static method. create a new, empty String.

`std::io::stdin()`: returns a handle to the standard input for your terminal. 返回一个指向标准输入的句柄。

`.read_line(&mut line)`: `read_line`是`stdin()`获得的IO句柄对象的一个方法，可以接受一个可变的String的**引用(Reference)**作为参数。

`.ok().expect()`: `read_line()`返回一个`io::Result`, `Result`的目的在于编码错误信息(encode error handing information)。`io::Result` has an `ok()` method, which says ‘we want to assume this value is a successful one. If not, just throw away the error information’. The `ok()` method returns a value which has another method defined on it: `expect()`. The `expect()` method takes a value it’s called on, and if it isn’t a successful one, `panic!`s with a message you passed it. A `panic!` like this will cause our program to crash, displaying the message. 这应该是Rust的异常机制的体现，不应为局部的一个错误而直接导致整个程序的Crash。如果去掉`.ok().expect()`，编译时会产生一个Waring: unused value must be used.这是因为：Rust is trying to tell you that you haven’t handled a possible error.

`println!("{}", line);`: `{}`是一个占位符，类似于格式化输出`printf`中的格式化参数。如果需要输出多个参数，写成`println!("{}, {}", arg1, arg2)`即可。

### Generating a secret number

Rust标准库中并没有生成随机数的函数，因此，需要依赖第三方库来实现这一功能。为了解决依赖问题，需要修改`Cargo.toml`文件。

    [dependencies]
    rand="0.3.0"

然后，`Cargo build`, Cargo会自动去下载依赖。如果这个库依赖其他第三方库，Cargo也会在下载或者构建这个库的时候解决这些依赖。

`Crates.io`: Crates.io is where people in the Rust ecosystem(生态环境) post their open source Rust projects for others to use.

`Cargo.lock`: 这个文件用于锁定当前的可用的外部依赖的版本，以防止随着外部依赖的自动更新而引入错误(we’ll stay at x.x.x until we explicitly upgrade)。Cargo升级命令：`cargo update`。

生成随机数的代码：

```rust
extern crate rand;
use std::io;
use rand::Rng;

fn main() {
    let num = rand::thread_rng().gen_range(1, 101);
    println!("random number: {}", num);
}
```

代码解释：

`extern crate rand;`: we can use extern crate to let Rust know we’ll be making use of it. 

`rand::thread_rng()`: We use the rand::thread_rng() function to get a copy of the random number generator, which is local to the particular thread of execution we’re in.

### Comparing guesses

```rust
match guess.cmp(&secret_number) {
    Ordering::Less    => println!("Too small!"),
    Ordering::Greater => println!("Too big!"),
    Ordering::Equal   => println!("You win!"),
}
```

`match`: 模式匹配，与Haskell很像。

`Ordering`：枚举(`enum`)类型。

### Looping

示例语法：

```rust
loop {
    // ...
    break; // jump out of loop.
}
```

```rust
let guess: u32 = match guess.trim().parse() {
    Ok(num) => num,
    Err(_) => continue,
};
```

解释：

`Ok` is a success, and `Err` is a failure.

We don’t care what kind of error it is, so we just use `_` instead of a name. 很像是Haskell中的匿名哨兵。

Dining Philosophers
--------------------

著名的哲学家进餐问题的模拟。

使用线程、泛型Vector、map, collect。有很类似于Java 8的并行流(Parallel Stream)里的概念。

Rust Inside Other Languages
---------------------------

FFI编程接口，与Ruby, Python, NodeJS等语言通过动态链接库进行交互。

例如，在多线程方面可以克服这些语言的全局解释器锁(Global Interpreter Lock)对并发的限制。

<!---------------------------------links------------------------------>



