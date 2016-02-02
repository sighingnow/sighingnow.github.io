---
title: Haskell Functors(函子) 和 Applicative functors
author: He Tao
date: 2015-08-03
tag: [Haskell]
category: 编程语言
layout: post
---

Functors 是可以被 **map over** 的对象，像是 lists，Maybe，trees 等等。在 Haskell 中我们是用 Functor 这个 typeclass 来描述他。这个 typeclass 只有一个 method，叫做 fmap，他的型态是 `fmap :: (a -> b) -> f a -> f b`。

~~~haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
~~~

<!--more-->

如果一个 type constructor 要是 Functor 的 instance，那他的 kind 必须是 `* -> *`。这代表他必须刚好接受一个 type 当作 type parameter，例如`Maybe`。

IO Action 与 Functor
--------------------

在Haskell中，IO Action 也是一个 Functor的实例(instance)，当 `fmap` 用一个 function 来 map over `I/O action` 时，我们会想要拿回一个装着已经用 function 映射过值的 `I/O action`。对一个 I/O action 做 map over 动作的结果仍会是一个 I/O action。

~~~haskell
instance Functor IO where
    fmap f action = do
        value <- action
        return $ f value
~~~

对IO Action应用fmap的一个例子：

~~~haskell
main :: IO ()
main = do
    line <- fmap reverse getLine
    putStrLn line
~~~

如果我们限缩 fmap 到 IO 型态上，那 fmap 的型态是

    fmap :: (a -> b) -> IO a -> IO b

fmap 接受一个函数跟一个 I/O action，并回传一个 I/O action 包含了已经 apply 过 function 的结果。

Functions As Functors
----------------------

`((-> ) r)`也是Functor的一个实例，`(->)`表示函数，是一个有两个参数的类型构造器(type constructor)，`r (->) a`也可以写成是`(->) r a`，`((->) r)`作为Functor的实例的定义：

~~~haskell
instance Functor ((->) r) where
    fmap f g = (\x -> f (g x))
~~~

将`f = (->) r` 应用到`fmap`(`fmap :: (a -> b) -> f a -> f b`)中去，有：

    fmap :: (a -> b) -> ((->) r a) -> ((->) r b)

写成中缀表达式：

    fmap :: (a -> b) -> (r -> a) -> (r -> b)

将一个 function map over 一个 function 会得到另一个 function。这根本就是 **function composition**。把 r -> a 的输出接到 a -> b 的输入。同样的，我们可以不要把 fmap 想成是一个接受 function 跟 functor 并回传一个 functor 的 function。而是想成一个接受 function 并回传一个新的 function 的 function，回传的 function 接受一个 functor 并回传一个 functor。他接受 a -> b 并回传 f a -> f b。这动作叫做 **lifting**。

functor laws
------------

一个东西要成为 functor，必须要遵守某些定律。这些条件不会在 Haskell 中自动被检查，所以必须自己确认这些条件。Instances of Functor should satisfy the following laws:

    fmap id  ==  id
    fmap (f . g)  ==  fmap f . fmap g

其中，`id`(Identity function)的定义：

    id :: a -> a Source

fmap的定义：

    fmap :: (a -> b) -> f a -> f b

**在Haskell中，列表 `[]` 也是Functor 的一个实例(instance)。** Functor 实质上代表着恒值映射，有 `a > b` 函数，在列表上就有 `[a] > [b]` ，在树上就有 `Tree a > Tree a`。有 `[a] > [b]` ，在列表上就有 `[[a]] > [[b]]` ，依次类推。

Applicative functors
---------------------

Applicative Functor, 也就是加强的 Functor，定义在 `Control.Applicative` 中。A functor with application, providing operations to

+ embed pure expressions (`pure`).
+ sequence computations and combine their results (`<*>`).

~~~haskell
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
~~~

`pure` 和 `<*>` 函数并没有提供缺省的实作，如果我们想使一个类型为 Applicative Functor 的实例，必须要提供这两个函数的实现。

其中，`pure` 函数将一个值转换为一个包含该值的对应类型的 Applicative Functor。

> A better way of thinking about pure would be to say that it takes a value and puts it in some sort of default (or pure) context—a minimal context that still yields that value.

`(<*>)`函数将一个 Functor 中的函数 map over 到一个包含在 Functor 中的值(Value) 上，最终返回一个包含结果的值的 Functor。某种角度上讲，可以将 `(<*>)` 看成是一个增强版(beefed-up)的 `fmap` 函数。

> fmap takes a function and a functor value and applies the function inside the
functor value, <*> takes a functor value that has a function in it and another
functor, and extracts that function from the first functor and then maps it
over the second one.

我们是无法单纯用 fmap 来把包在一个 functor 的函数 map 另一个包在 functor 中的值。我们只能用模式匹配 Just 来把函数从里面抽出来，然后再将函数作用到值上面。而 Applicative Functor 很好地解决了这种问题。我们可以使用 `pure` 来从值获取一个 Functor ， 可以使用 `(<*>)` 函数来将一个Functor中的函数作用于另一个Functor，然后得到一个Functor作为结果。

`Maybe`类型也是一个 Applicative Functor：

~~~haskell
instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> something = fmap f something
~~~

例如，`fmap (+) (Just 3)` 得到一个包含函数的 Maybe 实例，类型为：`Maybe (Num -> Num)`，我们无法将这个通过 fmap 应用到一个值(value)或者(Maybe value)上，而 `<*>` 函数可以：

~~~haskell
>> fmap (+) (Just 3) <*> (pure 10)
Just 13
~~~

Functor，函子，本质上在表达计算过程中的某种不确定性，而与普通的Functor相比，Applicative Functor在这方面提供了更强大的能力。`fmap`函数完成了普通函数在函子上的映射，而`<*>`完成了不确定的函数在函子上的映射，而`pure`函数则在完成从值到函子的转换。因此，用"beefed-up"一词来形容Applicative Functor很恰当。从计算方式的角度出发，也就不难理解 `pure f <*> x` 等于 `fmap f x`了，这也是 applicative laws 中的一条(`pure id <*> v = v`)。

List 也是 applicative functor ，定义为：

~~~haskell
instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]
~~~

应用举例：

~~~haskell
ghci> [(*0),(+100),(^2)] <*> [1,2,3]
[0,0,0,101,102,103,1,4,9]
~~~

由于在Haskell中，函数缺省就是Curried的，我们可以使用 applicative style 的方式来使用 Applicative Functor，如下例：

~~~haskell
ghci> [(+),(*)] <*> [1,2] <*> [3,4]
[4,5,5,6,3,4,6,8]
~~~

`Control.Applicative` 导出了一个函数 `<$>`，可以认为是中缀版(infix synonym)的`fmap`，将一个函数map到一个函子上，并返回一个函子。定义如下：

~~~haskell
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x
~~~

使用举例：

~~~haskell
ghci> (++) <$> Just "johntra" <*> Just "volta"
Just "johntravolta"
ghci> pure (++) Just "johntra" <*> Just "volta"
Just "johntravolta"

ghci> (+) <$> (Just 1) <*> (Just 2)
Just 3
ghci> pure (+) <*> (Just 1) <*> (Just 2)
Just 3
~~~

可以将 list 看作是一个 non-deterministic 的计算，Applicative style 对于 list 而言是一个取代 list comprehension 的好方式。

~~~haskell
ghci> [ x*y | x <- [2,5,10], y <- [8,10,11]]
[16,20,22,40,50,55,80,100,110]

ghci> (*) <$> [2,5,10] <*> [8,10,11]
[16,20,22,40,50,55,80,100,110]
~~~

`IO` 作为 Applicative Functor 的定义：

~~~haskell
instance Applicative IO where
    pure = return
    a <*> b = do
        f <- a
        x <- b
        return (f x)
~~~

本质上是在从一个值产生出一个 IO Action。

函数也可以是Applicative Functor, 在GHC的源码中，有：

~~~haskell
instance Applicative ((->) a) where
    pure = const
    (<*>) f g x = f x (g x)

-- | Constant function.
const :: a -> b -> a
const x _ = x
~~~

也就是说，`((->) r)` 作为 Applicative Functor 的定义可以写成如下形式：

~~~haskell
instance Applicative ((->) r) where
    pure x = (\_ -> x)
    f <*> g = \x -> f x (g x)
~~~

**当我们用 pure 将一个值包成 applicative functor 的时候，他产生的结果永远都会是那个值。** 也就是最小的 context。那也是为什么对于 function 的 pure 实现来讲，他就是接受一个值，然后造一个函数永远回传那个值，不管传递了什么参数。如果你限定 pure 的类型至 `(->) r` 上，他就会是 `pure :: a -> (r -> a)`。将两个 applicative functor 传递给 <*> 可以产生一个新的 applicative functor，所以如果传递给他两个函数，我们能得到一个新的函数。值得注意的是这种语法的**求值顺序**：

~~~haskell
ghci> :t (+) <$> (+3) <*> (*100)
(+) <$> (+3) <*> (*100) :: (Num a) => a -> a
ghci> (+) <$> (+3) <*> (*100) $ 5
508
~~~

为什么会是这个结果呢？

首先 5 被丢给 (+3) 跟 (*100)，产生 8 跟 500。然后 + 被套用到 8 跟 500，得到 508。

再例如:

~~~haskell
ghci> (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5
[8.0,10.0,2.5]
~~~

也是同样的道理。这也跟`((->) r)` 作为Applicative Functor时的`<*>`函数的定义以及`Control.Applicative`模块中的**`<$>`函数的定义** 相一致。

在`Control.Applicative`中还有一组liftA函数，也采用了 applicative style 的代码，用来将一个函数转变成一个Action，

~~~haskell
-- | Lift a function to actions.
-- This function may be used as a value for `fmap` in a `Functor` instance.
liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f a = pure f <*> a

-- | Lift a binary function to actions.
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

-- | Lift a ternary function to actions.
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = f <$> a <*> b <*> c
~~~

liftA系列函数的一个例子：

> 读入两个整数，并返回这两个数组成的二元组：

~~~haskell
import Control.Applicative

main :: IO ()
main = do
    tup <- liftA2 (,) (fmap read getLine) (fmap read getLine) :: IO (Int, Int)
    print tup
~~~

Action 版本的 flip 的实现便是一个 litfA2 函数应用的例子：

~~~haskell
-- | @'flip' f@ takes its (first) two arguments in the reverse order of @f@.
flip                    :: (a -> b -> c) -> b -> a -> c
flip f x y              =  f y x

-- | Application operator.  This operator is redundant, since ordinary
-- application @(f x)@ means the same as @(f '$' x)@. However, '$' has
-- low, right-associative binding precedence, so it sometimes allows
-- parentheses to be omitted; for example:
--
-- >     f $ g $ h x  =  f (g (h x))
--
-- It is also useful in higher-order situations, such as @'map' ('$' 0) xs@,
-- or @'Data.List.zipWith' ('$') fs xs@.
{-# INLINE ($) #-}
($)                     :: (a -> b) -> a -> b
f $ x                   =  f x

-- | A variant of '<*>' with the arguments reversed.
(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = liftA2 (flip ($))
~~~

Sequence Actions
-----------------

Haskell的 Control.Applicative 中有两个函数提供了默认实现：

~~~haskell
u *> v = pure (const id) <*> u <*> v
u <* v = pure const <*> u <*> v
~~~

对应的，这个模块还导出了以下两个函数：

~~~haskell
-- Sequence actions, discarding the value of the first argument.
(*>) :: f a -> f b -> f b

-- Sequence actions, discarding the value of the second argument.
(<*) :: f a -> f b -> f a
~~~

还有一个函数值得关注：

~~~haskell
(<$) :: Functor f => a -> f b -> f a
-- Replace all locations in the input with the same value.
~~~

将 a 类型的函子转化为 b 类型的函子。 默认的实现是 `fmap . const`。`const` 函数的定义：

~~~haskell
-- | Constant function.
const                   :: a -> b -> a
const x _               =  x
~~~

Applicative Functor's Laws
--------------------------

A minimal complete definition must include implementations of these functions satisfying the following laws:

+ identity

        pure id <*> v = v

+ composition

        pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

+ homomorphism

        pure f <*> pure x = pure (f x)

+ interchange

        u <*> pure y = pure ($ y) <*> u

