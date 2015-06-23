---
title: Dive Into Haskell(2) 数据类型和类型类(Typeclasses)
author: He Tao
date: 2015-02-11
tags: [Haskell]
category: 编程语言
layout: post
---

静态类型
---------------------

Haskell是静态类型(Static Type)语言，在编译时期每个表达式的类型都已经确定下来。如果在代码中有类型错误，就不可能通过编译。这极大地提高了代码的安全性。在Haskell中，所有东西都有类型。

GHCi中查看数据类型
------------------

在GHCi中，通过 `:t <name> ` 或者 `:type <name>`来查看变量、常量或表达式的数据类型。例如：

    Prelude> let a = 1
    Prelude> :t a
    a :: Num a => a
    Prelude> :type True
    True :: Bool
    Prelude> :t 4 == 5
    4 == 5 :: Bool

基本数据类型
-------------

Haskell有以下几种基本数据类型：

<!--more-->

+ Char
    单个Unicode字符。
+ Bool
    表示一个Bool逻辑值。这个类型只有两个值：`True`或`False`。
+ Int
    表示一个整数。范围为`-2147483648 ~ 2147483647`。
+ Integer
    可以认为是无限范围的整数。
+ Floating
    表示浮点数。注意`**`运算得到的结果的数据类型是`Floating`。

List
-----

在Haskell中，一个List由一对方括号括起来。其中的元素的数据类型必须相同，相邻元素之间用逗号`,`隔开。

### Haskell的字符串

在Haskell中，字符串实际上是一组字符的List，例如，`"Hello"`只是`['H', 'e', 'l', 'l', 'o']`的语法糖而已。

    Prelude> :t "Hello"
    "Hello" :: [Char]

### List的拼接

两个List之间的拼接用`++`运算符：

    Prelude> [1, 2] + [3, 4]
    [1, 2, 3, 4]

**注意**：`++`符号会遍历整个`++`符号**左边**的List，因此，当给较长的List追加元素时，会有严重的性能问题。

### 向List中追加元素

`:`运算符的作用是在一个List的前面追加一个元素：

    Prelude> 1: [2, 3, 4]
    [1, 2, 3, 4]

`:`运算符无法在List的后面追加元素，例如，以下用法会出现错误：

    Prelude> [2, 3, 4] : 1

    <interactive>:77:1
        No instance for (Num [[t0]]) arising from a use of 'it'
        In a stmt of an interactive GHCi command: print it

如果需要在List的后面追加元素，可以使用如下的语法：

    Prelude> [2, 3, 4] ++ [1]
    [2, 3, 4, 1]

即把要追加的元素写成一个单独的List，再用 `++`运算符将两个List连接。

### 多级List

List中的元素可以是List，其长度可以不同，但其中的元素的数据类型必须是相同的。

### List索引元素

使用 `!!`按照索引取得List中的元素，例如：

    Prelude> [1, 2, 3] !! 1
    2

List的索引值是从 `0` 开始的。如果索引值超过了List的长度，便会出错。

对于多级列表的索引，只需要逐级索引即可，例如：

    Prelude> let a = [[1, 2, 3], [4, 5], [6]]
    Prelude> a !! 1 !! 1
    5

### List大小比较

当List中装有课比较的元素时，可以用`>`运算符和`>=`运算符以及`<`运算符和`<=`运算符比较List的大小。用来比较大小的两个List中的元素的数据类型必须是相同的。它会先比较第一个元素，如果相等，在比较下一个元素。如果直到其中一个List已经到达末尾其对应位置的元素仍相等，而另一个没有到达末尾，则另一个List较大。举例：

    Prelude> [2, 2] > [2]
    True
    Prelude> [1, 2] < [2]
    True

### List相关的常用函数

+ head
    返回一个List的头部（首个元素）。如果当前List为空，则会产生异常。

        Prelude> head [1, 2, 3]
        1
        Prelude> head []
        *** Exception: Prelude.head: empty list

+ tail
    返回一个List除去头部元素之后的元素的List。如果当前List为空，则会产生异常。

        Prelude> tail [1, 2, 3]
        [2, 3]
        Prelude> tail [1]
        []
        Prelude> tail []
        *** Exception: Prelude.tail: empty list

+ last
    返回一个List的最后一个元素。
+ init
    返回一个List的除去最后一个元素的部分(List)。
+ length
    返回一个List的长度（包含的元素个数）。
+ null
    检查一个List是否为空，若为空，返回`True`，否则返回`False`。
+ reverse
    将一个List反转。
+ take
    取得一个List的前几个元素。

        Prelude> take 2 [1, 2, 3]
        [1, 2]
        Prelude> take 1 [1, 2, 3]
        [1]

    如果要求的元素个数大于List的长度，则返回整个List，不会产生异常。

        Prelude> take 4 [1, 2]
        [1, 2]

+ maximum
    取得一个List中的元素的最大值。
+ minimum
    取得一个List中的元素的最小值。
+ sum
    求得一个List中所有元素的和。**注意**用于求和的List中的元素必须是可相加的类型。例如，其元素类型不能是`Char`。对空List的求和结果为`0`。
+ elem
    判断一个元素是否在List中。如果在，返回`True`，否则返回`False`。也可以用中缀函数的方法调用`elem`函数。
    
        Prelude> elem 1 [1, 2, 3]
        True
        Prelude> 4 `elem` [1, 2, 3]
        False

### List与Range

对于可枚举的值，通过区间(Range)的方式可以很方便地生成列表。比如，要生成一个包含`1-20`的自然数的列表，只需要运行如下命令：

    Prelude> [1..20]
    [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]

对于字符型列表的生成，Haskell会按照字符的ASCII码的顺序来生成。

    Prelude> ['a'..'d']
    "abcd"
    Prelude> ['a'..'a']
    "a"

如果给出的上界大于下界，则返回一个空List。

    Prelude> ['b'..'a']
    ""
    Prelude> [1..0]
    []

通过Range的生成方式可以指定其规律，例如：

    Prelude> [2, 4..10]
    [2,4,6,8,10]

Haskell正是根据前两项推导出生成规则的。

Range也可以生成无限长的列表，不指定上界即可：

    Prelude> [2, 4..]

在GHCi中执行这条命令，便会不停输出一个无限长的列表`[2,4,6,8,10 ...]`

由于Haskell是**惰性**的，因此，也可以通过这这种方式来得到List的前几项：

    Prelude> take 3 [2, 4..]
    [2, 4, 6]

### List与`cycle`函数

`cycle`函数的作用是无限重复某一列表的内容：

    Prelude> take 5 (cycle [1, 2, 3])
    [1, 2, 3, 1, 1]

### List与repeat函数

`repeat`函数的作用是接受一个值作为参数，并返回一个仅包含该值的无限List。

    Prelude> take 5 (repeat 2)
    [2,2,2,2,2]

`replicate`函数也具有同样的功能：

    Prelude> replicate 3 10
    [10, 10, 10]

### List推导式(List Comprehension)

在Haskell中，List Comprehension的核心思想是通过不断添加谓词(predicate)(限制条件)来从一个集合中不断筛选出符合条件的元素，最终得到想要的集合。

    Prelude> [x | x <- [10 .. 20], odd x]
    [11, 13, 15, 17, 19]
    Prelude> [x*2 | x <- [10 .. 20], even x, x /= 16]
    [10, 12, 14, 18, 20]

**注意**：多个谓词(predicate)之间用","连接。

Tuple
------

### Tuple的特点

在Haskell中，元组用"()"来表示，元组中的不同元素之间用","分隔开。通常用元组来表示向量等数据结构。使用元组(Tuple)时应该注意以下几点问题：

+ 每一个元组中的元素可以是不同类型的元素，例如：

        Prelude> ('a', 1, "abcd")
        ('a',1,"abcd")

+ 不同长度的元组是不同的**类型**。
+ 同样长度，对应位置元素类型相同，且元素可比较的Tuple是可以比较大小的。
+ 元组中元素的最小长度为2，因此不存在只有一个元素的元组，但可以存在只有一个元素的List。

#### 序对(Pair)

Haskell中，序对(Pair)是只有两个元素的元组。

+ `fst`函数返回一个序对的首项。
+ `snd`函数返回一个序对的尾项。

    Prelude> fst (8, 11)
    8
    Prelude> snd (8, 11)
    11

**注意**：`fst`函数和`snd`函数仅仅对序对(Pair,只有两个元素的元组)有效。

### zip 函数

`zip`函数把两个List交叉配对，生成一个Pair的List。如果两个List长度不同，以较短的List的长度为基准，较长List中多余的项舍去。

    Prelude> zip [1, 2, 3] "abcde"
    [(1,'a'),(2,'b'),(3,'c')]

类型变量与多态
--------------

在GHCi中运行如下命令，会看到：

    Prelude> :t head
    head :: [a] -> a

此处，`a`便是一个类型变量(Type variables)，它可以是任何类型。使用了类型变量的函数是多态函数。Haskell中，类似的函数还有：

    head, tail, sum, fst, snd ... 等等。

类型类(Typeclasses)
--------------------

Haskell中，类型类相当于提供了一系列的接口，属于某一类型类的数据类型具有对应的一类性质。

主要有以下几种类型类：

+ Eq类型类
    
    Eq类型类用于可以判断相等性的类型。其实例必须实现`==`和`/=`这两个函数。

+ Ord类型类

    Ord类型类用于可以比较大小的类型。Ord类型中包含了所有的比较函数。
        
    - `>`、`<`、`>=`、`<=`。
    - `compare`函数。`compare` 函数读取连个Ord中的相同类型的值作为参数，返回一个Ordering类型的值。Ordering类型有 `GT`、`LT`、`EQ`三种值，分别表示大于、小于和等于。

    举例：

        Prelude> compare 1 2
        LT
        Prelude> 1 `compare` 2
        LT

+ Show 类型类

    Show类型类的实例为可以表示为字符串的类型。

        Prelude> :t show
        show :: Show a => a -> String

+ Read类型类

    Read类型类与Show类型类相反。`read`函数可以取一个字符串作为参数并转为Read的某个实例的类型，其具体类型可以在表达式中自动推断。

        Prelude> read "True" || True
        True
        Prelude> read "1" + 1
        2
        Prelude> read "[1, 2, 3]" ++ [4, 5, 6]
        [1, 2, 3, 4, 5, 6]

    **注意**:`read`函数不能将字符串解析成字符串。

        Prelude> read "123" ++ "456"
        *** Exception: Prelude.read: no parse

    除了自动推断类型以外，还可以通过类型注解(type annotation)的方式显示指出应该将字符串解析成何种类型。类型注解跟在表达式后面，通过`::`连接。

        Prelude> read "123" :: Int
        123
        Prelude> read "123" :: Float
        123.0

    如果将`read`函数放在列表中，便可以根据这个列表中其他元素的类型来解析得到对应的类型。

        Prelude> [read "123", 4]
        [123, 4]
        Prelude> [read "123", 4.0]
        [123.0, 4.0]

+ Enum 类型类
    
    Enum类型类的实例类型都是有连续顺序的，都是可枚举的。对于Enum类型的实例类型，每个值都有相应的后继(successer)和前驱(predecesor)。可以用`succ`函数和`pred`函数得到。该类型类包含的主要类型有：

        (), Bool, Char, Ordering, Int, Integer, Float 和 Double。

+ Bounded类型类
    
    Bounded类型类的实例类型都有一个上限和下限，分别可以通过`maxBound`和`minBound`函数得到。

        Prelude> minBound :: Int
        -2147483648
        Prelude> maxBound :: Bool
        True
        Prelude> maxBound :: Char
        '\1114111'
        Prelude> maxBound :: (Bool, Int, Char)
        (True, 2147483647, '\1114111')

+ Floating类型类

    Floating类型类用于存储浮点数，仅包含Float和Double两种浮点类型。

+ Integral类型类

    Integral类型类仅包含整数，其实力类型有Int和Integer。

haskell中的类型转换
--------------------

前面提到的`Read`类可以通过自动推断或类型注解的方式将字符串转换成其他类型。Haskell同时还提供了在不同类型之间进行转换(type case)的方法。

例如：`fromInteger`函数可以将`Integer`类型的数据转换成其他类型。

```haskell
func :: (Floating a) => a -> a
func a = a * 2.0

main :: IO()
main = do
    let a = read "20" :: Integer
    print $ func $ fromInteger a
```

还可以通过与`read`函数类似的加注解的方式来指定`fromInteger`函数将`Integer`类型的变量转换为哪一种类型而非自动类型推断。

```haskell
main :: IO()
main = do
    let a = read "1234" :: Integer
    print $ (fromInteger a :: Double)
```

代码运行后，将输出：

    1234.0

