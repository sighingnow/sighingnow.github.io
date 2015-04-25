---
title: Dive Into Haskell(3) 函数
author: He Tao
date: 2015-02-20
tags: [Haskell]
category: 编程语言
layout: post
---

在Haskell中，一切皆函数。

定义一个函数
--------------

haskell中，可以通过如下方式定义一个简单的函数：

```haskell
doubleMe x = x + x
```

这样的做法其实是实现了一个绑定(binding)。更广泛的函数定义的方法：

```haskell
func1 :: Int -> String
{- function body. -}

func2 :: Int->Int -> String
{- function body. -}

func3 :: (Integral a) => a -> String

{- function body. -}

func4 :: (Integral a) => a->a -> String
{- function body. -}
```

<!--more-->

从上面四个例子中可以看出haskell定义函数的几种语法。前两种语法类似，函数名后面前几个类型都为参数类型，最后一个类型为返回值类型。后两种语法类似，使用了类型类来标识参数的类型，同样，最后一个类型为返回值类型，前几个为参数类型。

函数的调用
-----------

上面定义的几个函数，可以通过如下语法调用：

```haskell
doubleMe 1
func1 1
func2 1 2
func3 1
func4 1 2
```

如果函数有两个参数，那么可以通过中缀函数的方式调用，注意使用中缀函数语法是要在函数名左右两侧各加一个`\``符号。如下例：

```haskell
1 `func2` 2
1 `func4` 2
```

Haskell的库函数中，可以用中缀函数语法调用的函数还有`div`, `mod`等。使用中缀函数的好处在于可以让参数之间的运算关系更加清楚。

函数中的模式匹配
-----------------

定义函数时，可以为不同的模式分别定义函数体本身。函数匹配可以匹配一切数据类型。如下例：

```haskell
func :: Int -> String
func 1 = "number 1"
func 2 = "number 2"
```

这样，当函数参数为`1`时，函数返回`"number 1"`，当函数参数为`2`时，函数返回`"number 2"`。如果参数与任何一个模式都不能匹配，那么程序将会出现运行时错误。错误内容为：

    Non-exhaustive patterns in function func

因此，一种解决方法是在函数定义的最后加上一个默认的模式。例如，在上例函数的最后加入：

    func x: "number is not in all patterns."

这样，对于任何不再模式中的参数(既不等于1又不等于2的数)，参数值都将会被`x`捕获，执行模式`x`所对应的函数体。

注意：如果将模式`x`（默认模式）放在最前面，编译时将产生如下Warning:

    Pattern match(es) are overlapped

函数运行时，无论参数是什么，都将运行对应于默认模式(`x`)对应的函数体。

又如下例定义阶乘函数的方法：

```haskell
fact :: (Integral a) => a -> a
fact 0 = 1
fact n = n * fact (n-1)
```

与其他编程语法相比，代码简洁明了。

List与模式匹配
---------------

对于List也可以使用`[]`或`:`进行模式匹配。例如`[1, 2, 3]`或`1:2:3:[]`将匹配元素为1、2、3的列表。

形如`x:xs`的模式将匹配长度大于等于`1`的List。并把List的头部绑定到`x`，其他部分绑定到`xs`。由此类推，`x:y:z:xs`将匹配元素个数大于等于`3`的List。

此外，`as`模式也常用语List的模式匹配中。例如：

    func all@(x, xs)

语句中，`x`将匹配List的第一项，`xs`将匹配List的其他项，**`all`的函数是整个List**。可以方便地通过`all`来操作参数的整个List。

哨兵(Guards)
-------------

Haskell中与函数有关的另一个重要概念是哨兵(Guards)。模式匹配可以用来匹配参数的值或结构，而哨兵则用来匹配和检验参数的性质（如大小、正负等）。

Guards的应用举例：

```haskell
func1 :: Int -> String
func1 number
    | number == 0 = "number is equal to 0."
    | number > 0  = "number is a positive."
    | number < 0  = "number is a negative."

func2 :: Int->Int -> String
func2 a b
    | a > b  = "a ia greater than b."
    | a == b = "a is equal to b."
    | a < b  = "a is less than b."
```

一般而言，排在最后的哨兵是`otherwise`，它能捕获一切条件。如：

```haskell
func1 :: Int -> String
func1 number
    | number == 0 = "number is equal to 0."
    | otherwise   = "number is not equal to 0."
```

但需要注意，如果把`otherwise`放在第一个哨兵的位置，那么所有条件都会被`otherwise`捕获，只会执行`otherwise`对应的程序。

**注意**:`|`符号前面至少要有一个空格的缩进，否则会有编译错误。

所有的哨兵也可以写在一行，但代码可读性不好，这一用法仅仅用于展示较短的函数。例如：

```haskell
{- 返回两个可比较类型数据中的较大值。 -}
{- 注意，haskell中函数名中可以有"'"(单引号)。
 - 此处这样用是为了与标准库中的 max 函数分开。
 - -}
max' :: (Ord a) => a -> a -> a  
max' a b | a > b = a | otherwise = b
```

where 关键字
--------------

`where`关键字用于在一个作用域中实现绑定。`where`关键字既可以绑定名字，也可以定义函数。`where`关键字定义的名字只对本函数可见，不会污染其他函数的命名空间。

`where`关键字用在函数的底部，在函数的最后来定义绑定。

let 关键字
-----------

`let`绑定与`where`绑定类似，`let`绑定是一个表达式，允许在任何位置定义函数的局部变量。并且对不同的Guard可见性不同。

`let`定义局部绑定的用法为：

    let [bindings] in [expressions]

`let`定义的绑定仅仅对`in`部分可见。在函数中，可以直接用

    let ...

语句来定义局部变量和局部函数。

case 语句
----------

模式匹配也可以用`case`语句来实现，具体用法为：

```haskell
case expression of pattern -> result
                   pattern -> result
                   ...
```

函数参数的模式匹配只能在函数定义时使用，而`case`语句既可以用于函数定义时的参数匹配，也可以用在函数内部的表达式中。

Haskell 中的递归(Recursive)
----------------------------

使用递归函数求解的一个经典例子为求Fibonacci数列。Haskell中实现如下：

```haskell
fibonacci :: Int -> Int
fibonacci n
    | n==0      = 0
    | n==1      = 1
    | otherwise = (fibonacci (n-1))+(fibonacci (n-1))
```

函数式编程的一个特点是将对列表的遍历实现为对子列表的递归。例如使用递归的方法来求一个List的最大值：

```haskell
max' :: (Ord a) => [a] -> a
max' []            = (error "abcde")
max' [x]           = x
max' (x:xs)
    | x >= maxTail = x
    | otherwise    = maxTail
    where maxTail  = max' xs
```

用递归和函数式编程的思想实现快速排序算法：

```haskell
qsort :: (Ord a) => [a] -> [a]
qsort []  = []
qsort (x:xs) = qsort(filter (< x) xs) ++ [x] ++ qsort(filter (>=x) xs)
```

用其他一些具有函数式编程特性的语法来实现快速排序同样很方便，比如Python：

```python
def qsort(a):
if a == []:
    return []
return qsort(list(filter(lambda e:e<a[0], a[1:]))) \
        + [a[0]] \
        + qsort(list(filter(lambda e:e>=a[0], a[1:])))
```

由此不难看出列表推导(List Comprehension)和递归(Recursive)的威力。

在使用递归来求解问题的时候，一定要注意边界条件，例如上例中的List为空的情形。







