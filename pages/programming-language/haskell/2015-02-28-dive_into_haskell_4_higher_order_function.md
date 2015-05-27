---
title: Dive Into Haskell(4) 高阶函数
author: He Tao
date: 2015-02-28
tags: [Haskell]
category: 编程语言
layout: post
---

返回函数的函数
--------------

以求最大值的函数`max'`为例，

```haskell
max' :: (Ord a) => a->a -> a
max' x y
    | x >= y = x
    | x < y  = y
```

调用该函数时可以这样做：

    max' 1 2

也可以这样做：

    (max' 1) 2

原因是这个函数的定义也可以看成是

<!--more-->

    max' :: (Ord a) => a -> (a->a)

即一个参数为`(Ord a)`类型，返回函数`(Ord a) => (a->a)`的函数。这样的函数就是一个高阶函数。

高阶函数的部分应用
------------------

在Haskell中，所有的多个参数的函数都叫做科里函数(Curried functions)。高阶函数的一个好处是**部分应用**。即可以通过传递给函数一部分参数来构造一个新的函数。如：

    let max'1  = max' 1

便构造了一个一元函数，该一元函数的作用是返回一个数与`1`中的较大值。使用该新函数时，只需要这样调用就好：

    max'1 3

这样便求出了`1`和`3`两个数中的较大者。

对于中缀函数，还可以通过截断(section)的方法进行部分应用。例如，

    let max'2 = (`max'` 2)
    {- 注意，此处的括号用于指定优先级，不能省略。 -}

调用时：

    max'2 3

便求出了`2`和`3`中的较大者。

通过中缀函数进行应用的另一个例子：

    let sumWith2 = (+ 2)

或者：

    sumWith2 :: (Integral a) => a -> a
    sumWith2 = (+ 2)

这样便定义了一个求一个数与`2`的和的函数。

函数用作参数--map 与 filter
----------------------------

Haskell中，也可以将一个函数用作参数。例如标准库中的`map`和`filter`函数。

    map :: (a -> b) -> [a] -> [b]
    filter :: (a -> Bool) -> [a] -> [a]

通过上文提到的函数的部分应用，实现这两个函数：

```haskell
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = (f x) : (map f xs)

filter :: (a->Bool) -> [a] -> [a]
filter _ [] = []
filter f (x:xs)
    {- The list has only one item. -}
    | f x       = x : (filter p xs) {- x if (f x) == True else None -}
    | otherwise = (filter f xs)
```

再回顾之前提到的快速排序函数：

```haskell
qsort :: (Ord a) => [a] -> [a]
qsort []  = []
qsort (x:xs) = qsort(filter (< x) xs) ++ [x] ++ qsort(filter (>=x) xs)
```

此处用`filter`函数来从List选择元素，而`(< x)`是二元函数的中缀形式的局部应用。

Lambda表达式
--------------

Lambda表达式就是匿名函数。有时，我们需要传给高阶函数一个函数，而这函数我们只会用这一次，这就弄个特定功能的 Lambda。

编写Lambda，就写个`\\`，后面是空格隔开的参数。然后是`->`然后是函数体。一般来说，lambda都括在括号里。例如：

```haskell
{- 对List中的值逐项求平方。 -}
map (\a -> a*a) [1, 2, 3, 4, 5]

{- 多个参数的lambda
 - 注意多个参数不能用"()"括起来，否则会出错。
 - -}
(\a b -> a+b) 1 2

{- Lambda中使用模式匹配
 - 注意，lambda中不能为参数设置多个模式。
 - -}
map (\(a, b) -> a+b) [(1, 2), (3, 4), (5, 6)]
```

通常lambda 都是括在括号中，否则，后面的整个语句都将作为lambda的函数体。

由于有科里化，以下两种表达形式等价：

```haskell
sum' :: (Num a) => a -> a -> a -> a
sum' x y z = x+y+z
```

```haskell
sum' :: (Num a) => a -> a -> a -> a
sum' \x -> \y -> \z -> x+y+z
```

List折叠(Fold)
---------------

### fold

有关函数式编程的另一个应用是列表折叠。主要有以下两组函数：

+ foldl, foldr
+ foldl1, foldr1

foldl和foldr类似，函数的定义为：

    foldl :: (b->a->b) -> b -> [a] -> b
    foldr :: (b->a->b) -> b -> [a] -> b

以b为初值，对整个列表进行函数`(b->a->b)`的迭代，方向前者向左，后者向右。

foldl1和foldr1类似，函数定义为：

    foldl1 :: (a->a->a) -> [a] -> a
    foldr1 :: (a->a->a) -> [a] -> a

这两个函数以List的第一个元素为初值，对后面的所有元素进行迭代。方向前者向左，后者向右。

举例：

```haskell
let a = [1, 2, 3, 5, 6]
print $ foldl (\a b -> a*b) 1 a
print $ foldr1 (\a b -> a*b) a
```

由于Haskell的惰性求值特性，列表折叠会产生一个延迟计算栈，当列表过长时，对列表的折叠会造成栈空间的极大消耗，甚至可能造成栈溢出。Haskell还提供了一组严格折叠的函数来避免这一个问题。

这组函数位于`Data.List`中，函数定义如下：

    foldl' :: (b->a->b) -> b -> [a] -> b
    foldr' :: (b->a->b) -> b -> [a] -> b
    
    foldl1' :: (a->a->a) -> [a] -> a
    foldr1' :: (a->a->a) -> [a] -> a

其用法与惰性求值的列表折叠相同，只是底层的实现原理有差异。

### scan

scan积累了fold函数执行过程中产生的左右变量，并返回一个包含这些结果的List。

    scanl :: (b->a->b)->b->[a]->[b]
    scanr :: (b->a->b)->b->[a]->[b]

    scanl1 :: (a->a->a)->[a]->[a]
    scanlr :: (a->a->a)->[a]->[a]

`$` 的函数调用
------------

`$`函数又称为函数应用符(Function Application Operator)。作用是改变函数调用的优先级。通常，普通的函数调用有最高的优先级（左结合），而`$`函数调用的优先级最低（右结合）。例如：`sqrt 5+5`的值为`7.236`，等于`(sqrt 5)+5`，然而，`sqrt $ 5+5`的值为`3.162`，等于`sqrt (5+5)`。

`$`函数调用还可以把数据转换成函数。示例：

    map ($ 1) [(1+), (1*), (1^), (^1)]

函数组合
---------

函数组合(Function composition)也是Haskell中的一个重要特性。函数组合的运算符为`.`，定义如下：

    (.) :: (b->c)->(a->b)->a->c

也就是：

    (f .g) = \x -> f(g x)

point free style 更强调函数的组合方式而非数据的传递方式。这种程序逻辑更加简洁明了。可以通过这种方式将一系列函数组合在一起从而形成一个复杂的函数。





