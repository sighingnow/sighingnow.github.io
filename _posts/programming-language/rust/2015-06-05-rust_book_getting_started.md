---
title: Rust-Book Getting Started
author: He Tao
date: 2015-06-05
tags: Rust
category: 编程语言
layout: post
---

从零开始学习Rust。

1. Rust的安装
--------------

略。

2. Rust版Hello World
--------------------

```rust
fn main() {
    println!("hello world");
}
```

`main`函数是程序入口。`println!`中的`!`表示这是一个宏。

> This is calling a Rust macro, which is how metaprogramming is done in Rust.

<!--more-->

3. Cargo
--------

> Cargo is a tool that Rustaceans use to help manage their Rust projects.

Cargo manages three things: 
1. building your code, 
2. downloading the dependencies your code needs, 
3. building those dependencies.

类似于Maven? 

构建脚本文件名：`Cargo.toml`。语法参考：[toml-lang/toml][1]。

命令：`cargo build`, `cargo run`, `cargo new xxx`(创建新项目, 会自动创建git仓库(.git和.gitignore))。


<!---------------------------------links------------------------------>

[1]: https://github.com/toml-lang/toml



