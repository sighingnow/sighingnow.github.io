---
title: Dive Into Haskell(1) GHCi
author: He Tao
date: 2015-02-02
tags: [Haskell]
category: 编程语言
layout: post
---

What's GHC ?
-----------

GHC(Glasgow Haskell Compiler):  an interactive and batch compilation system for the [Haskell 98](http://www.haskell.org/ "Haskell") language.

GHC has two main components: an interactive Haskell interpreter (also known as GHCi), and a batch compiler, described throughout. In fact, GHC consists of a single program which is just run with different options to provide either the interactive or the batch system.

What's GHCi ?
-------------

GHCi是一个GHC的交互式环境，在GHCi中可以交互式地求得对Haskell表达式的值，Haskell程序也可以在GHCi中被解释。GCHi还包含了一个交互式的Haskell调试器。

GHCi的常用命令：
---------------

<!--more-->

+ :? 或 :help 获取关于GHCi的帮助。
+ :q 或 :quit 退出GHCi。
+ :!<cmd> 执行外部命令<cmd>。
+ :l 和 :r 或 :load 和 :reload 加载外部的hs文件。
+ :t 或 :type 显示出表达式的类型。
+ :set + t 设置所有表达式默认显示出其类型。
+ :i<name> 显示<name>相关的信息(info)。可以通过这种方式获取帮助。例如：

```haskell
Prelude> :i print
print :: Show a => a -> IO() -- Defined in 'System.IO'
Prelude> :i +
class Num a where
  (+) :: a -> a -> a
  ...
        -- Defined in 'CHC.Num'
```

GHCi 使用
---------

在GHCi的使用中，应该注意以下几点：

+ GHCi 中可以直接进行数学表示式的运算，在这一点上和Python的交互式环境(Python Shell) 很像。
+ GHCi 中支持的数学运算符号：+, -, \*, /, \*\*(乘方)。**注意**：\*\*计算得到的结果是浮点数：
        
        Prelude> :t 2 ** 2
        2 ** 100 :: Floating a->a

+ Haskell中不同类型的数不能直接进行数学运算。需要做类型转换。
    - `fromIntegral`函数可以将整数类型转换为别的类型。
    - `toIntegral`函数可以将其他类型的数(如：浮点数)转换成整数。
    - 类似的函数还有：`fromInteger`,`toInteger`。

+ GHCi 中不能用 % 进行取模运算。
+ GHCi 中，用`/=`表达不等号。举例：

        Prelude> 10 /= 20
        True
        Prelude> 10 /= 10
        False

+ GHCi 有一个名为 `it` 的变量，自动保存上一次计算的结果。其类型也随上一条表达式的结果的变化而变化。

GHCi交互式环境与加载hs文件的区别
--------------------------------

如果加载一个hs文件，GHCi会将整个文件看作一个过程，其解释顺序是不可预测的。**但是**，在GHCi中直接输入表达式是，其解释顺序是由输入(IO)的顺序的决定的。例如：

    c = a + b
    a = 1
    b = 2

将这段代码写在hs文件中，在load进GHCi中后，执行`c`可以正常输出`3`，但在GHCi中，无法通过直接输入表达式来得到这样的结果。这也是Haskell的函数式的特性所决定的。

GHCi 与函数
-----------

在GHCi中输入加减乘除等表达式时都是在调用函数，`+, -, *, /` 等都是函数。因此，有如下的用法：

    Prelude> (+) 1 2
    3

除此以外，还有这些函数可以使用：

+ `succ` 函数

        Prelude> succ 1
        2
        Prelude> succ 'a'
        'b'

+ `max` 和 `min` 函数

前面提到的`/`表示的真除法(和Python类似)，如果只想保留结果的整数部分(和C++类似)，那么可以使用`div`函数:

    Prelude> div 10 3
    3

显然，这种形式并不便于理解和区分除数和被除数，因此，可以写成中缀函数的形式(**注意** `div` 左右各有一个**"\`"**符号)：

    Prelude> 10 `div` 3
    3

前面提到，在GHCi中，不能用 `%` 来表示取模运算，可以这样实现取模:

    Prelude> mod 10 3
    1
    Prelude> 10 `mod` 3
    1

GHCi 中定义变量
----------------

在GHCi中使用`let`来定义变量：

    Prelude> let a = 1
    Prelude> a
    1

在GHCi中用`let`定义变量与在hs文件中通过用`=`赋值来定义变量是等价的。例如：

    a = 1

在GHCi中，如果直接使用`=`对变量赋值会出错。

GHCi 自动补全
-------------

在GHCi中输入命令时，输入命令的前几个字符后，按`Tab`键便可以补全命令。如果有多个相匹配的命令，GHCi会在下一行列出这些命令。如果只有一个，会直接补全。


