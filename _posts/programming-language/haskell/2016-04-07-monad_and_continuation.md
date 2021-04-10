---
title: Monad 与 Continuation
author: Tao He
date: 2016-04-17
tag: [Haskell]
category: 编程语言
layout: post
---

Continuation 是一个非常重要的概念，用于表达程序执行过程中某一处的上下文。处于会破坏引用透明的原因，Haskell中并没有像Scheme那样在语言
级别提供对Continuation的支持，Continuation是一个Monad类型，而另一方面，Continuation可以被用来实现各种Monad。

<!--more-->

Monad (Cont r)
--------------

Haskell中可以使用一个以函数作为构造参数的类型来表达Continuation，即"the rest of computation"：

~~~haskell
newtype Cont r a = Cont { runCont :: (a -> r) -> r }
~~~

`Cont r`是Monad的一个实例类型。`return`操作相当于将一个值转化为一个Context，`bind`操作表示将两个Continuation串连在一起。

~~~haskell
instance Monad (Cont r) where
    m >>= k = Cont $ \(x :: b -> r) -> (runCont m) (\a -> runCont (k a) x)
    return a = Cont $ \(f :: a -> r) -> f a
~~~

使用Equation Reasoning不难证明`Cont r`上的`return`和`bind`操作是满足Moand laws的。有了Continuation，就可以在其基础上更进
一步，实现call/cc。call/cc即Call with current continuation，是起源于Scheme中的技术，可以用来获得程序当前的上下文，
即current continuation。call/cc的参数是一个高阶函数，这个这个函数的参数`k`是一个逃逸函数，在call/cc的参数中调用`k`，
就会打断程序的执行流，将当前的值传给`k`，并跳转到call/cc捕获得current continuation中去。

~~~haskell
callcc :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callcc f = Cont $ \(k :: a -> r) -> runCont (f (\vara -> Cont (\(_ :: b -> r) -> k vara))) k
~~~

考虑参数`k`的类型：a -> Cont r b，

接下来从Haskell的Cont实现的角度分析在call/cc的参数中调用`k`为什么能够做到打断当前按执行流(early exit)。例如以下的例子：

~~~haskell
example1, example2 :: Num a => Cont r a
example1 = callcc $ \cc -> do { cc 1;     return 2 }
example2 = callcc $ \cc -> do { return 1; return 2 }
~~~

给出一个函数`id`做参数来表达rest of computation(continuation)，放入到call/cc捕获的上下文中(suspend computation)执行：

~~~haskell
r1 = runCont example1 id  -- 1
r2 = runCont example2 id  -- 2
~~~

从`r1`和`r2`的值可以看出，在call/cc的参数的函数体中调用current continuation，确实达到了early exit的效果。接下来，利用Equation
Reasoning的方法来分析为什么会出现这样的运行结果。考虑更通常的情形，

~~~haskell
r1 = runCont (callcc (\cc -> return v >>= T)) f
r2 = runCont (callcc (\cc -> cc v >>= T)) f
~~~

带入`callcc`以及Continuation Monad的`return`和`>>=`的实现，得到下面的推导序列。对于`r1`，有：

~~~haskell
r1 = runCont (callcc (\cc -> return v >>= T)) f
   = runCont (callcc (\_ -> (\x -> (runCont (return v)) (\a -> runCont (T a) x)))) f
   = runCont (callcc (\_ -> (\x -> (\f -> f v) (\a -> runCont (T a) x)))) f
   = runCont (callcc (\_ -> (\x -> runCont (T v) x))) f
   = runCont (Cont (\k -> runCont ((\_ -> (\x -> runCont (T v) x)) (\a -> Cont (\_ -> k a))) k)) f
   = ((\_ -> (\x -> runCont (T v) x)) (\a -> Cont (\_ -> f a))) f
   = (\x -> runCont (T v) x) f
   = runCont (T v) f
~~~

而对于`r2`，有：

~~~haskell
r2 = runCont (callcc (\cc -> cc v >>= T)) f
   = runCont (callcc (\cc -> (\x -> (runCont (cc v)) (\a -> runCont (T a) x)))) f
   = runCont (Cont (\k -> runCont ((\cc -> (\x -> (runCont (cc v)) (\a -> runCont (T a) x))) (\a -> Cont (\_ -> k a))) k)) f
   = ((\cc -> (\x -> (runCont (cc v)) (\a -> runCont (T a) x))) (\a -> Cont (\_ -> f a))) f
   = (\x -> (runCont ((\a -> Cont (\_ -> f a)) v)) (\a -> runCont (T a) x)) f
   = (runCont ((\a -> Cont (\_ -> f a)) v)) f
   = runCont (Cont (\_ -> f v)) f
   = v
~~~

程序运行的结果与Equation Reasoning的结果相符。进一步思考，之所以会出现这样的结果，是因为，调用`cc`就意味着把rest of computation
放到call/cc捕获的上下文中，执行之后，就能得到结果，自然也就与后续的其他的rest of computation没关系了。`callcc`函数中的
`Cont (\_ k a)`也能体现出这一点。

用途
----

Continuation最大的用途就在于可以显式地、灵活地操作程序的控制流，通过捕获current continuation和调用continuation来实现在不同
上下文中的灵活切换。例如，使用Continuation来实现对异步任务的控制，程序逻辑要比NodeJS风格的callback方式清晰很多，composable
continuation通过定义continuation之间的运算符，进一步提高程序的程度，简化程序设计和软件构建。

一个利用call/cc来捕获当前上下文然后从程序的其他地方返回捕获的上下文中执行的例子：

~~~haskell
import Control.Monad.Cont
import Data.IORef

main = (`runContT` return) $ do
    ref <- lift $ newIORef undefined
    callCC $ \k -> lift $ writeIORef ref k
    lift $ putStrLn "hello world"
    continue <- lift $ readIORef ref
    continue ()
~~~

TSPL(The Scheme Programming Language)一书中有一个使用Continuation来模拟多进程调度的例子[^1]：

~~~scheme
(define proc-list '())
(define proc (lambda (thunk) (set! proc-list (append proc-list (list thunk)))))

(define pause (lambda () (call/cc (lambda (k) (proc (lambda () (k))) (start)))))
(define start (lambda () (let ([p (car proc-list)]) (set! proc-list (cdr proc-list)) (p))))

(proc (lambda () (let f () (pause) (display "hello ") (f))))
(proc (lambda () (let f () (pause) (display "world\n") (f))))
(start)
~~~

将这段程序直接翻译成Haskell：

~~~haskell
proc proclist p = lift $ modifyIORef proclist $ \ps -> ps ++ [p]
pause proclist = callCC $ \k -> proc proclist (k ()) >> start proclist
start proclist = lift (readIORef proclist) >>= \(p:ps) -> lift (writeIORef proclist ps) >> p

sched = do
    proclist <- lift $ newIORef []
    proc proclist . forever $ pause proclist >> lift (putStr "hello ")
    proc proclist . forever $ pause proclist >> lift (putStrLn "world")
    start proclist

main = runContT sched return
~~~

此外，Continuation Moand还可以用于分派、调度和同步异步任务。事实上，即使使用C/C++这类典型的命令式语言，有些场景下仍然
无法做到“programming in a straightforward imperative style”，例如状态机，就必须要用到回调。而COntinuation提供了另一个
中表达程序的控制流的新思路。Bartosz Milewski的文章[^2]中给出了一个如何使用Continuation Monad去调度异步任务的例子。

另一个与Continuation相关的实际应用是coroutines.

The mother of all monads
------------------------

对于Moand的bind操作`(>>=) :: Monad m => m a -> (a -> m b) -> m b`，可以把第二个参数视为一个Continuation，将第一个参数所
代表的Computation的结果作为参数，然后返回此时的Computation。直观上，使用`>>=`来表达Monad的写法与Continuation-passing 
Style的写法非常相似，实际上，定义了Continuation Monad之后，其他的Monad都可以使用Continuation Monad来定义。Dan Diponi的
The mother of all monads[^3] 一文详细地描述了如何使用Continuation Monad来表达Maybe Monad，List Monad，以及如何表达`(>>=)`。

正是由于Continuation Monad强大的表达能力，如果实现了Continuation Monad，就以为能够实现其他所有的Monad。对于C++14，标准
范围内的语法是足够实现Continuation的，因此，也就具备了Monad的表达能力。


References
----------

[^1]: [Going Further](http://scheme.com/tspl4/further.html#./further:h3), TSPL chapter 3.
[^2]: [Asynchronous API in C++ and the Continuation Monad](https://www.fpcomplete.com/blog/2012/06/asynchronous-api-in-c-and-the-continuation-monad), Bartosz Milewski.
[^3]: [The Mother of all Monads](http://blog.sigfpe.com/2008/12/mother-of-all-monads.html), Dan Diponi.
