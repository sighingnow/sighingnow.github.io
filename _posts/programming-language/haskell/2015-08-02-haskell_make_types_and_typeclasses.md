---
title: Haskell 构造Types和Typeclasses
author: He Tao
date: 2015-08-02
tag: [Haskell]
category: 编程语言
layout: post
---

`data` 关键字和deriving（派生）
----------------------------

首先我们来看看 Bool 在标准函式库中的定义：

    data Bool = False | True

data 表示我们要定义一个新的类型。`=` 的左端标明类型的名称即 Bool，`=` 的右端就是值构造子 (Value Constructor)，它们明确了该类型可能的值。**类型名和值构造子的首字母必大写**。

<!--more-->

另一个例子：

    data Shape = Circle Float Float Float | Rectangle Float Float Float Float

我们在定义值构造子时，可以在后面跟几个类型表示它包含值的类型。Circle 的值构造子有三个项(field)，都是 Float，Rectangle 的值构造子取四个 Float 项(field)。**值构造子（可以有参数，也可以没有参数）的本质是个函数，可以返回一个类型的值。** “项”(field)，其实应为“参数”(parameters)。

```haskell
ghci> :t Circle
Circle :: Float -> Float -> Float -> Shape
ghci> :t Rectangle
Rectangle :: Float -> Float -> Float -> Float -> Shape
```

`data`可以用来递归定义数据类型：

```haskell
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

-- data List a = Empty | Cons { listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)
```

`Cons` 构造子: `Cons` 其实就是指 `:`。对 List 而言，`:` 其实是一个构造子，他接受一个值跟另一串 List 来构造一个 List。现在我们可以使用我们新定义的 List 型态。换句话说，他有两个 field，其中一个 field 具有型态 `a`，另一个有型态 `[a]`。

值构造子可以局部应用（科里化）我们若要取一组不同半径的同心圆，可以这样：

```haskell
ghci> map (Circle 10 20) [4,5,6,6]
[Circle 10.0 20.0 4.0,Circle 10.0 20.0 5.0,Circle 10.0 20.0 6.0,Circle 10.0 20.0 6.0]
```

在Haskell中定义类型时可以使用派生(deriving)。若在 data 声明的后面加上 deriving (Show)，那 Haskell 就会自动将该类型至于 Show 类型类之中。

```haskell
ghci> data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
ghci> print $ Circle 10 20 5
```

Record Syntax
-------------

Record Syntax可以理解为给类型构造器的域(filed)建立别名。

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

值构造子可以取几个参数产生一个新值，如 Car 的构造子是取三个参数返回一个 Car。与之相似，**类型构造子可以取类型作参数，产生新的类型**。

例如Maybe函子：

    data Maybe a = Nothing | Just a

这里的a就是个类型参数。也正因为有了它，Maybe 就成为了一个类型构造子。在它的值不是 Nothing 时，它的类型构造子可以搞出 Maybe Int，Maybe String 等等诸多类型。**但只一个 Maybe 是不行的，因为它不是类型，而是类型构造子。要成为真正的类型，必须得把它需要的类型参数全部填满。**

在Haskell中，`Nothing` 的类型为 `Maybe a`。它是**多态**的，若有函数取 `Maybe Int` 类型的参数，就一概可以传给它一个 `Nothing`，Nothing 中不包含任何值。

类型参数有很多好处，但前提是用对了地方才行。一般都是不关心类型里面的内容，如 `Maybe a`。一个类型的行为若有点像是容器，那么使用类型参数会是个不错的选择。

**Haskell 中有一个严格的约定，那就是永远不要在 data 声明中添加类型约束(尽管可以)。因为这样没好处，反而得写更多不必要的类型约束。** 例如，`Map k v` 要是有 `Ord k` 的约束，那就相当于假定每个 `Map` 的相关函数都认为 `k` 是可排序的。若不给数据类型加约束，我们就不必给那些不关心键是否可排序的函数另加约束了。这类函数，例如 `toList`，它只是把一个 `Map` 转换为关联 `List` 罢了，类型声明为 `toList :: Map k v -> [(k, v)]`。要是加上类型约束，就只能是 `toList :: (Ord k) => Map k a -> [(k,v)]`，明显没必要。

接下来，通过两个type parameters的例子来说明如何使用type parameters：

```haskell
Prelude> data Vector a = Vec a a a deriving (Show)
Prelude> let fV :: (Num a) => Vector a -> a; fV (Vec i j k) = i+j+k;
Prelude> :t fV
fV :: Num a => Vector a -> a
Prelude> fV $ Vec 10 101 10
121
Prelude> 
```

另一个例子：

```haskell
data T v = A v | B v v | C v v v deriving (Show)

fV :: T String -> String
fV (A x) = "param type: A, params: " ++ x
fV (B x y) = "param type: B, params: " ++ x ++ ", " ++ y
fV (C x y z) = "param type: C, params: " ++ x ++ ", " ++ y ++ ", " ++ z

main :: IO()
main = do
    print $ fV $ A "param-a"
    print $ fV $ B "param-a" "param-b"
    print $ fV $ C "param-a" "param-b" "param-c"
```

程序运行的结果：

    E:\> runhaskell type_param.hs
    "param type: A, params: param-a"
    "param type: B, params: param-a, param-b"
    "param type: C, params: param-a, param-b, param-c"

从这个例子中，可以看出一些与Scala中的`case class`的相似点。二者都在函数的模式匹配中发挥作用，而这恰恰是函数式编程的一个重要的概念。

Derived instances
-----------------

```haskell
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq)
```

在一个类型 derive 为 Eq 的 instance 后，就可以直接使用 `==` 或 `/=` 来判断它们的相等性了。Haskell 会先看下这两个值的值构造子是否一致(这里只是一个值构造子)，再用 `==` 来检查其中的所有数据(必须都是 Eq 的成员)是否一致。

Type synonyms
-------------

`type` 关键字: 给一个既有类型提供一个别名(并不是用来创造新类, 创造新类应使用`data`关键字)。

```haskell
type String = [Char]
```

类型别名也是可以有参数的，如果你想搞个类型来表示关联 List，但依然要它保持通用，好让它可以使用任意类型作 key 和 value，我们可以这样：

```haskell
type AssocList k v = [(k,v)]
```

自定义typeclass
--------------

typeclass 就像是 interface。一个 typeclass 定义了一些行为(像是比较相不相等，比较大小顺序，能否穷举)而我们会把希望满足这些性质的类型定义成这些 typeclass 的 instance。typeclass 的行为是由定义的函数来描述。并写出对应的实作。当我们把一个类型定义成某个 typeclass 的 instance，就表示我们可以对那个类型使用 typeclass 中定义的函数。

```haskell
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)
```

如何让一个类型成为 Eq 的 `instance`：

```haskell
data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

-- equivalent to:
-- data TrafficLight = Red | Yellow | Green deriving (Eq)
```

`Functor` typeclass
-------------------

`Functor` is a Prelude class for types which can be mapped over. It has a single method, called `fmap`. We can conceivably define a map-style function for any arbitrary data structure using `Functor`. The class is defined as follows:

`Data.Functor`的定义：

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

f 是一个类型构造子，它接受一个类型。

All instances of Functor should obey:

```haskell
fmap id      = id
fmap (p . q) = (fmap p) . (fmap q)
```

Haskell中的fmap的定义：

```haskell
fmap :: Functor f => (a -> b) -> f a -> f b
```

`map` 就是针对 List 的 `fmap`。 List 如何被定义成 Functor 的 instance 的:

```haskell
instance Functor [] where
    fmap = map
```

Maybe 作为一个 functor 的定义：

```haskell
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing
```

参考： [The functor design pattern][http://www.haskellforall.com/2012/09/the-functor-design-pattern.html]

Kind
-----

kind 是类型的类型。

    Prelude> :k Int
    Int :: *
    Prelude> :k Num
    Num :: * -> Constraint
    Prelude> :k Maybe
    Maybe :: * -> *

一个 `*` 代表这个类型是具体类型。一个具体类型是没有任何类型参数，而值只能属于具体类型。而 `*` 的读法叫做 star 或是 type。

当我们在写一般实用的 Haskell 程序时，你几乎不会碰到需要动到 kind 的东西，也不需要动脑去推敲 kind。通常只需要在定义 instance 时 partially apply 自己的 `* -> *` 或是 `*` 类型。


<!--------------------------------------links-------------------------------->

[1]: http://www.haskellforall.com/2012/09/the-functor-design-pattern.html

