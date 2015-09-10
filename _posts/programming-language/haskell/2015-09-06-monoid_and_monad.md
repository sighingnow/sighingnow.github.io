---
title: Monoid(幺半群)和Monad(单子)
author: He Tao
date: 2015-09-06
tag: [Haskell]
category: 编程语言
layout: post
---


Haskell 中 typeclass 是用来表示一个类型之间共有的行为，是一种 interface。当我们定义一个类型时，我们会想说他应该要支持的行为。也就是表现的行为是什么，并且要让他属于哪些 typeclass。

Monoid typeclass
----------------

> A class for monoids (types with an associative binary operation that has an identity) with various general-purpose instances.

`Monoid`的定义：

```
class Monoid m where  
    mempty :: m  
    mappend :: m -> m -> m  
```

Monoid的实例类型是不接受任何参数的类型构造子，Monoid类型类的实例类型的 `mempty`和`mappend`的实现必须遵守以下的 Monoid Laws:

+ mappend mempty x = x
+ mappend x mempty = x
+ mappend x (mappend y z) = mappend (mappend x y) z
+ mconcat = foldr mappend mempty

`mempty`是一个不接受任何参数的函数，可以看作是一个多态的(polymorphic)的常数，表示一个特定 monoid 的 Identity。

`mappend`是一个满足结合律的二元运算。

更进一步，monoid是种很广泛的代数结构, 一旦可以利用这代数结构的性质做些什么事情, 同样也可以应用到满足这些性质的数据类型(instance)上。
monoid的广泛是因为它的"简单": 一个集合, 一个满足结合律(**结果跟运算顺序无关**)的封闭二元运算(mappend), 一个单位元(mempty)。

再深入一些，monoid 所体现的是Haskell强大的抽象能力。 举个例子，你要做尾递归优化，譬如说下面这个函数

    sum [] = 0
    sum (x:xs) = x + sum xs

其实是可以写成循环的，但是你这么写，他就不是一个尾递归函数。那要怎么办呢？我们需要加一个acc参数把他写成为递归的，但是实际上这么做我们是把右结合的加法改成了左结合的加法。那Haskell怎么知道加法可以这么做呢？当然他可以hardcode。那我们自己的运算怎么办呢？
所以还会看到Haskell里面有一个type class叫做幺半群(monoid)（抽象代数: A monoid is an algebraic structure with a single, associative binary operation and an identity element. In other words, it is a unital semigroup.），**你声明了他，或许他就会帮你优化了**。
很多抽象就是这么一点一点搞出来的。它不像我们写C++和C#，每一个抽象背后的代码都十分底层无比复杂，Haskell喜欢在一个抽象里面，改一点点东西做成一个新的抽象，当你得到了大量的抽象之后，你突然发现它可以用来解决现实问题了。所以你才会看到 Haskell 中有大量的这些你平时估计直接用不到的东西。

semigroup(半群)
--------------

From [Wiki-Monoid](http://en.wikipedia.org/wiki/Monoid):

> Any semigroup S may be turned into a monoid simply by adjoining an element e not in S and defining e*e = e and e*s = s = s*e for all s \belongs S." Since there is no "Semigroup

Monad
------

Functor 是一个抽象概念，代表是一种可以被 map over 的值。Applicative Functor，代表一种带有 context 的型态，我们可以用函数操作他而且同时还保有他的 context。而Monoid呢 ？ 如果有一个具有 context 的值 m a，如何把他传给一个只接受普通值 a 为参数的函数，并回传一个具有 context 的值 ？我们需要一个这样的函数：

    (>>=) :: (Monad m) => m a -> (a -> m b) -> m b

Monad 可以看作是支持 `>>=` 操作的 applicative functors，`>>=` 称为 `bind`。

> The Monad class defines the basic operations over a monad, a concept from a branch of mathematics known as category theory. 

> From the perspective of a Haskell programmer, however, it is best to think of a monad as an abstract datatype of actions. Haskell's do expressions provide a convenient syntax for writing monadic expressions.

最小化的完整定义：

```haskell
class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b 
```

`return`和`(>>=)`的定义应该满足 Monad Laws:

+ Left identity

        return a >>= k  ==  k a

+ Right identity

        m >>= return  ==  m

+ Associativity

        m >>= (\x -> k x >>= h)  ==  (m >>= k) >>= h

此外，还要满足如下需求：

    fmap f xs  ==  xs >>= return . f

在 Monad typeclass 中定义的第一个函数是 return。它其实等价于 Applicative 的 pure 函数。他的型态是 (Monad m) => a -> m a。他接受一个普通值并把他放进一个最小的 context 中。也就是说，将一个值转换为一个 Action。而 `>>=`函数则是将一个 Monad 作为参数传给一个以值为参数，以 Monad 为返回值的函数，并最终得到一个 Monad。

`Maybe`类型是 Monad 的一个实例类型，定义为：

```haskell
instance  Monad Maybe  where
    (Just x) >>= k      = k x
    Nothing  >>= _      = Nothing

    return              = Just
    fail _              = Nothing
```

Haskell中的List作为Monad的定义如下：

```
instance Monad [] where  
    return x = [x]  
    xs >>= f = concat (map f xs)  
    fail _ = []
```

`do` notation 用来来串连所有的 monadic value。在 do expression 中，每一行都是一个 monadic value。要检查处理的结果的话，就要使用 <-。例如如果我们拿到一个 Maybe String，并用 <- 来绑定给一个变量，那个变量就会是一个 String。

由于 do 表示法是一行一行写，他们会看起来很像是命令式的写法。**但实际上他们只是代表串行而已，每一步的值都倚赖前一步的结果，并带着他们的 context 继续下去。**

可以说，Monad也可以被称为"computation builder"，monad 将很多条有关联的语句组合到一起，并对外体现出纯函数(pure functional)的风格和接口定义。

> Both the listcomprehension syntax and the do-notation are syntactic sugar for chaining operations using the `>>=` operator. A monad is basically just a type that supports the `>>=` operator.

例如，

    [x*2 | x<‐[1..10], odd x]

可以写成是：

    do
        x <‐ [1..10]
        if odd x
            then [x * 2]
            else []

甚至是：

    [1..10] >>= (\x ‐> if odd x then [x*2] else [])




一些实用的 Moandic functions
---------------------------

+ `liftM`

LiftM 函数接受一个函数跟一个 monadic value，然后把函数 map over 那些 monadic value，最终得到一个 monadic value 作为结果。某种程度上说， `LiftM` 函数与 `fmap` 函数具有相似之处。

```haskell
-- | Promote a function to a monad.
liftM   :: (Monad m) => (a -> b) -> m a -> m b
liftM f mval              = do { aval <- mval; return (f aval) }
```

此外，还有适用于不同参数个数的 LiftM2, LiftM3, LiftM4 和 LiftM5 的定义。

+ `join`

如果一个 monadic value 的结果是另一个 monadic value，也就是其中一个 monadic value 被包在另一个里面，即嵌套 Monad 。此时，就需要用到`join`函数来将其变为一个普通的 monadic value。函数的定义与实现：

```haskell
-- | The 'join' function is the conventional monad join operator. It is used to
-- remove one level of monadic structure, projecting its bound argument into the
-- outer level.
join              :: (Monad m) => m (m a) -> m a
join x            =  x >>= id
```

+ `filterM`

filter 函数是 Haskell 中不可或缺的要素。他接受一个断言(predicate)跟一个 list 来过滤掉断言为否的部份并回传一个新的 list。predicate 能接 list 中的一个元素并回传一个 Bool 型态的值。但如果 Bool 型态其实是一个 monadic value ，也就是有一个 context 呢？也就是说，最终的谓词的真或者假依赖于上下文。此时，我们就需要用到 `filterM`函数。

```haskell
-- | This generalizes the list-based 'filter' function.

filterM          :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM _ []     =  return []
filterM p (x:xs) =  do
   flg <- p x
   ys  <- filterM p xs
   return (if flg then x:ys else ys)
```

+ `(=<<)`

`(=<<)` 函数与`(>>=)` 函数类似，只不过是交换了两个参数的位置。

```haskell
-- | Same as '>>=', but with the arguments interchanged.
{-# SPECIALISE (=<<) :: (a -> [b]) -> [a] -> [b] #-}
(=<<)           :: Monad m => (a -> m b) -> m a -> m b
f =<< x         = x >>= f
```

+ `(>=>)` 和 `(<=<)`

Kleisli composition of monads, 用于组合两个返回值为`Monad`的函数：

```haskell
-- | Left-to-right Kleisli composition of monads.
(>=>)       :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g     = \x -> f x >>= g

-- | Right-to-left Kleisli composition of monads. @('>=>')@, with the arguments flipped
(<=<)       :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(<=<)       = flip (>=>)
```

其中，`(<=<)`函数与函数合成函数 `.`类似(`f . g x = f(g(x))`)。

几种 Monad
----------

+ Writer Monad
+ Reader Monad
+ Difference Lists
+ State Monad

Functor, Applicative 和 Monad
-----------------------------

Functor, Applicative 和 Monad 本质上都代表着 Haskell 对于 non-deterministic 的情形的处理，以及如何将这些内容与 deterministic 的代码，也就是Haskell中的纯函数，建立起联系。


与之对应，我们注意到 `fmap`函数的类型定义：

    fmap :: (Functor f) => (a -> b) -> f a -> f b