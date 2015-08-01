---
title: Haskell 构造Types和Typeclasses
author: He Tao
date: 2015-08-01
tag: [Haskell]
category: 编程语言
layout: post
---

`data` 关键字
------------

首先我们来看看 Bool 在标准函式库中的定义：

    data Bool = False | True

data 表示我们要定义一个新的类型。`=` 的左端标明类型的名称即 Bool，`=` 的右端就是值构造子 (Value Constructor)，它们明确了该类型可能的值。**类型名和值构造子的首字母必大写**。

另一个例子：

    data Shape = Circle Float Float Float | Rectangle Float Float Float Float

我们在定义值构造子时，可以在后面跟几个类型表示它包含值的类型。Circle 的值构造子有三个项(field)，都是 Float，Rectangle 的值构造子取四个 Float 项(field)。**值构造子（可以有参数，也可以没有参数）的本质是个函数，可以返回一个类型的值。** “项”(field)，其实应为“参数”(parameters)。

```haskell
ghci> :t Circle
Circle :: Float -> Float -> Float -> Shape
ghci> :t Rectangle
Rectangle :: Float -> Float -> Float -> Float -> Shape
```

deriving（派生）
---------------

例如，若在 data 声明的后面加上 deriving (Show)，那 Haskell 就会自动将该类型至于 Show 类型类之中。

```haskell
ghci> data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
ghci> print $ Circle 10 20 5
```

我们若要取一组不同半径的同心圆，可以这样：

```haskell
ghci> map (Circle 10 20) [4,5,6,6]
[Circle 10.0 20.0 4.0,Circle 10.0 20.0 5.0,Circle 10.0 20.0 6.0,Circle 10.0 20.0 6.0]
```

Record Syntax
-------------

可以理解为给类型构造器的域(filed)建立别名。

```haskell
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Show)
```

通过 Record Syntax，Haskell 就自动生成了这些函数：firstName, lastName, age, 参数为`Person`, 返回值类型为这些field声明的类型。

```haskell
ghci> :t age
age :: Person -> String
```

通过 Record Syntax, 在构造该类型的值的时候就不必必须遵守构造类型时的参数的顺序：

    Person {firstName = "a", lastName = "b", age = 10}

Type parameters
---------------







