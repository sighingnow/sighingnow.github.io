---
title: Functor, Applicative and Monad
author: Tao He
date: 2016-03-08
tag: [Haskell]
category: 编程语言
layout: post
---

Functor, Applicative 和 Monad 的数学定义：

+ A functor is a type of mapping between categories, a functor is a morphism from a source category to a target category.
+ An applicative functor is a strong lax monoidal functor.
+ A monad is an endofunctor (a functor mapping a category to itself), together with two natural transformations.

<!--more-->

定义
----

### Functor

函数 (Function) 指的是在特定的类型之间的映射，而函子 (Functor) 则体现的是范畴之间的映射。设 $C$, $D$ 为范畴，从 $C$ 到 D的一个函
子$F$指的是一个满足如下要求的映射：

+ 对于 $C$ 中的每一个对象(object) $X$，都有 $F(X) \in D$
+ 对于 C 中的每一个态射(morphism) $f: X \to Y$，都有 F(f): F(X) \to F(Y) \in D

并且满足条件：

+ Identity morphism:

    $$\forall X \in C, F(id_x) = id_{F(X)}$$

+ Composition of morphisms:

    $$\forall f: X \to Y, g : Y \to Z \in C, F(g \circ f) = F(g) \circ F (f) $$

对应到 Haskell，Functor 的定义如下：

~~~haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b

-- * Functor laws:
--
-- > Identity:    fmap id  ==  id
-- > Composition: fmap (f . g)  ==  fmap f . fmap g
~~~

Functor指的是范畴之间的映射，如果映射的定义域和值域是同一个范畴，称谓自函子。两个范畴之间有很多函子，构成函子范畴，函子范畴之间的
态射就是自然变换(Natural transformation)。某种意义上可以这样理解Functor和Natural transformation：Functor指的是操作某对
象的内容而不涉及对象的结构。而Natural transformation则与之相反，指的是操作对象的结构而不涉及对象的具体内容。

### Applicative functors

Applicative Functor, 是介于 Functor 和 Monad 之间的一种代数结构，是保留了 source/target categories 的幺半群结构(monoidal
structure)的 Functor。对于Haskell，所有的类型和函数都属于Hask范畴，因此，Applicative实际上是一个自函子(Endofunctor)，此处的
monoidal structure 指的是Cartesian product。对于Cartesian product 运算下的一个monoidal object $m$，相应的幺元和满足结合律的
变换分别为 $(m, m) \to m$ 和 $() \to m$，对应的两个自然变换`mempty`和`mappend`的类型为：

~~~haskell
mempty :: Applicative f => f ()
mempty = pure ()

mappend :: Applicative f => (f a, f b) -> f (a, b)
mappend = uncurry (liftA2 (,))  -- = \(x, y) -> fmap (,) (x <*> y)
~~~

`mempty`表达的是 effect-free computations，`mappend`表达的是如果有多个有effects的computations，在将他们的value组合起来
时，也将他们的effects组合在一起了。Haskell中Applicative的最小化定义为函数`pure`和函数`<*>`，类型签名如下：

~~~haskell
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

-- * Applicative laws:
--
-- > Identity:     pure id <*> v = v
-- > Composition:  pure (.) <*> u <*> v <*> w = u <*> (v <*>) w
-- > Homomorphism: pure f <*> pure x = pure (f x)
-- > Interchange:  u <*> pure y = pure ($ y) <*> u
~~~

`pure`和`<*>`可以通过`mempty`和`mappend`这两个自然变换得到：

~~~haskell
pure x = fmap (\() -> x) mempty

f <*> x = fmap (uncurry ($)) (mappend f x)
~~~

### Monad

> A monad is just a monoid in the category of endofunctors.

Monad是一个自函子范畴上的幺半群，具体来讲，一个Monad包括：

+ An endofunctor: $T : X \to X$，(Haskell中，是一个Kind为`* -> *`的 Type Constructor)。
+ A natural transformation (Identity morphism): $\eta: I \to T, \textrm{ where } I \textrm{ is the identity endofunctor on } X$，(Haskell中的`return`)。
+ A natural transformation (Composition morphism): $\mu: T \times T \to T, \textrm{ where } \times \textrm{ means functor composition }$，(Haskell中的`join`)。

满足：

+ Associativity:

    $$\mu (\mu (T \times T) \times T) = \mu (T \times \mu (T \times T))$$

+ Identity:

    $$\mu (\eta (T)) = \mu (T (\eta))$$

借助GHC的`RankNTypes`和`TypeOperators`扩展来进行类型操作，定义Hask范畴上的自函子范畴的Natural transformation 以及 Identity Functor 和
Compose Functor：

~~~haskell
{-# LANGUAGE RankNTypes, TypeOperators #-}

-- | Natural transformation. `f` and `g` are functors with kind `* -> *`.
type f ~> g = forall x. f x -> g x

-- | Identity functor.
newtype Id a = Id { runId :: a }

-- | Composition functor.
newtype Compose f g a = Compose { runCompose :: f (g a) }
~~~

根据Haskell Report中关于`newtype`的语义，`newtype`表示 the type being defined 与 the type of the arguments 之间的
同构。根据这一原理不难证明`Id`的左单位元、右单位元和`Compose`的结合律。然后再这两个Functor的基础上定义自然变换 $\eta$ 和 $\mu$，
得到Monad的范畴论定义：

~~~haskell
class Monad m where
    eta :: Id ~> m
    mu  :: Compose m m ~> m
~~~

Haskell中Monad的定义为

~~~haskell
class Applicative m => Monad m where
    return :: a -> m a
    (>>=)  :: forall a b. m a -> (a -> m b) -> m b
~~~

可以这两种定义是等价的。Identity morphism $\eta$ 与 `return` 对应，Compose morphism $\mu$ 与 `join` 相对应。`join`和`(>>=)`的相互定义如下：

~~~haskell
m >>= f = join (fmap f m)
join m = m >>= id
~~~

Monad 的 Kleisli 范畴定义(使用`return`, `(>=>)`)：

~~~haskell
class Monad m where
    (>=>)   :: forall a b c. (a -> m b) -> (b -> m c) -> (a -> m c)
    return  :: a -> m a
~~~

联系与区别
---------

Functor表达的是Haskell的范畴Hask中函数之间的映射关系。例如`a -> b` 与 `[a] -> [b]`。Applicative 和 Monad都是自函子的Monoidal范畴上的幺半群，其单位元都是Id Functor，区别在于
范畴的张量积(tensor product, $\otimes: M \times M \to M$)。Applicative对应的是自函子的Day Convolution，而Monad对应的自函子的Composition。

> A tensor is just a bifunctor over that category, which satisfies some conditions. It is associative, has a unit, and meets a couple of scarier higher order conditions.

Functor代表着恒值映射，有函数`a -> b`，在List上就有`[a] -> [b]`，在Tree上就有`Tree a -> Tree b`，等等。函数`fmap :: (a -> b) -> (f a -> f b)`的作用是Lifting，是的原本作用
于简单的值的函数能够作用于 Context中的值，本质上意味着计算过程中的某种不确定性（Context的不确定性）。而与普通的Functor相比，Applicative Functor在这方面提供了更强大的能力。
`fmap`函数完成了普通函数在函子上的映射，而`<*>`完成了不确定的函数在函子上的映射，而`pure`函数则在完成从值到函子的转换,
因此，用"beefed-up"一词来形容Applicative Functor很恰当。从计算方式的角度出发，也就不难理解 `pure f <*> x` 等于 `fmap f x`了，applicative laws 中也有`pure id <*> v = v`。`<*>`
与`>>=`的区别在于后者引入了值之间的依赖而前者参数可以并行求值。`<*>`的两个参数的类型分别是`f (a -> b)`、`f a`，可以并行地从Context `f`中计算`a -> b`和`a`，然后再将两个结
果reduce得到最终的`b`。而`>>=`的两个参数的类型分别是`m a`和`a -> m b`，只有先从Context `m`中计算出`a`，才能使用`a -> m b`来计算最终的结果。前者参数各个部分的计算时独立的，
而后者存在前后依赖关系。Applicative是自函子水平方向的组合，Monad是自函子垂直方向的组合。Applicative表达的是一个Batching and aggregation的过程，而Monad表达的是一个Sequence
的过程。Functor、Applicative和Monad的区别也直接决定了三者在表达能力上的强弱。以Parser Combinator为例，Monad既可以处理CFG，也可以处理CSG，而Applicative只能处理CFG。
而维护Context需要额外的开销，Applicative在CFG parser上比Monadic parser更有优势。

从这三者之间的区别，可以看出Haskell自底向上的设计哲学。Haskell正是通过不同程序的抽象：Functor -> Applicative -> Monad来满足不同的需求，并且通过这种细粒度的抽象来提高模块
化的程序，利于代码复用和应对需求调整的灵活性。John Hughes _Why Functional Programming Matters_ 一文中举的例子就能够非常好地体现这种抽象在实际开发中带来的好处。

do notation
-----------

do notation 是 `>>=`的语法糖，`a >>= fn`可以写成

~~~haskell
do
    v <- a
    fn a
~~~

GHC 8 引入了`ApplicativeDo`扩展，类型于`fn <$> a <*> b <*> c`的代码可以写成：

~~~haskell
do
    va <- a
    vb <- b
    vc <- c
    return (fn a b)
~~~

对于 ApplicativeDO，`va <- a`，`vb <- b`，`vc <- c`这三个步骤是可以并行求值的，而Monadic do需要串行执行。

具体实例类型
-----------

### List

~~~haskell
instance Functor [] where
    fmap = map
instance Applicative [] where
    pure x = [x]
    (<*>) fs xs = [f x | f <- fs, x <- xs]
instance Monad [] where
    return = [x]
    (>>=) xs f = [y | x <- [xs], y <- f x] -- (>>=) = concatMap
~~~

### Function

`((-> ) r)`是Functor、Applicative和Monad的一个实例，`(->)`表示函数，是一个有两个参数的类型构造器(type constructor)，`r (->) a`也可以写成是`(->) r a`，`((->) r)`作为Functor的实例的定义：

~~~haskell
instance Functor ((->) r) where
    fmap f g = . -- (\x -> f (g x))
instance Applicative ((->) r) where
    pure x = const -- \_ -> x
    (<*>) f g = \x -> f x (g x)
instance Monad ((->) r) where
    return = const -- \_ -> x
    (>>=) f k = \r -> k (f r) r
~~~

### IO Action

IO Action是一种典型的Context：

~~~haskell
newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))

instance  Functor IO where
    fmap f x = x >>= (return . f)

instance Applicative IO where
    pure  = return
    (<*>) = ap

instance  Monad IO  where
    return = IO $ \ s -> (# s, x #)
    (>>=)  = IO $ \ s -> case m s of (# new_s, a #) -> unIO (k a) new_s
~~~

### Pair

~~~haskell
instance Functor ((,) a) where
    fmap f (x,y) = (x, f y)
~~~

但是元组`((,) a)`不能作为 Applicative 和 Monad 的实例。

Not Functor/Applicative/Monad
-----------------------------

+ A type constructor which is not a Functor:

~~~haskell
newtype T a = T (a -> Int)
~~~

无法使用一个类型为`a -> b`的函数将一个类型为`a -> Int`的函数转换成一个`b -> Int`的函数。

+ A type constructor which is a functor, but not an Applicative:

~~~haskell
instance Functor ((,) a) where
    fmap f (x, y) = (x, f y)
~~~

但是无法对任意的类型`a`定义`pure :: a -> (r, a)`。

+ A type constructor which is an Applicative, but not a Monad:

~~~haskell
newtype ZipList a = ZipList { getZipList :: [a] }

instance Functor ZipList where
    fmap f (ZipList xs) = ZipList (map f xs)

instance Applicative ZipList where
    pure x = ZipList (repeat x)
    ZipList fs <*> ZipList xs = ZipList (zipWith id fs xs)
~~~

`pure`返回的是一个无限列表`repeat x`，因此，无法做出满足Moand Laws的`>>=`，`ZipList`不能作为Monad的实例。


参考
---

1. [All About Monad](https://wiki.haskell.org/All_About_Monads)
2. [What a Monad is not](https://wiki.haskell.org/What_a_Monad_is_not)
3. [Monads for functional programming](http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf), _Philip Wadler_
4. [Where do the applicative laws come from? ](https://www.reddit.com/r/haskell/comments/2lompe/where_do_the_applicative_laws_come_from/)
