---
title: Variadic Functions
author: He Tao
date: 2016-01-24
tag: [Haskell]
category: 编程语言
layout: post
---

编程语言对于 Variadic function 的支持极大地扩展了语言的表达能力，对Haskell这样类型系统及其强大的语言，Variadic Function 的实现更是优雅。

<!--more-->

Variadic Function
-----------------

对于多态函数，具体实例的选择取决于调用函数的上下文，根据这一原理，可以使用多态来构造支持变长参数的函数。Haskell 中`Text.Printf`包中的`printf`函数是一个典型的
支持 Variadic Arguments 的例子，用于类似于C语言中的`printf`函数的用法，例如：

    printf "%d %c" 10 'x'

具体实现中的核心代码：

~~~haskell
printf :: (PrintfType r) => String ‐> r
printf fmts = spr fmts []

class PrintfType t where
    spr :: String ‐> [UPrintf] ‐> t

instance (IsChar c) => PrintfType [c] where
    spr fmts args = map fromChar (uprintf fmts (reverse args))

instance (a ~ ()) => PrintfType (IO a) where
    spr fmts args = putStr $ map fromChar $ uprintf fmts $ reverse args

instance (PrintfArg a, PrintfType r) => PrintfType (a -> r) where
    spr fmts args = \a ‐> spr fmts (toUPrintf a : args)
~~~

其巧妙之处在于，将`(IsChar c) => [c]`和`(a -> r)`同时作为`PrintfType`类型类的实例类型，对于不同参数的调用，选择不同的函数，`printf`函数的类型，既可以是
一个`(IsChar c) => [c]`，也可以是一个函数`(a -> r)`，或者是一个`IO a`。这种多态机制，实现了对边长参数的支持。

从`printf`的实现，不难构造出Variadic Function的一般框架如下：

~~~haskell
variadic :: VariadicRetClass r => RequiredArgs -> r
variadic args = variadicImpl args mempty

class VariadicRetClass r where
    variadicImpl :: RequiredArgs -> AccType -> r

class VariadicRetClass ActualRetType where
    variadicImpl args acc = constructActualRes args acc

class (ArgClass a, VariadicRetClass r) => VariadicRetClass (a -> r) where
    variadicImpl args acc = \a -> variadicImpl args (acc `mappend` (specialize a))
~~~

[Polyvariadic functions and keyword arguments: pattern-matching on the type of the context](http://okmij.org/ftp/Haskell/polyvariadic.html) 一文给出了更加泛化的例子和说明。

仿照上面提到的一般性模板，实现一个Variadic 版本的 printAll：

~~~haskell
class PrintType t where
    printAll' :: [String] -> t

instance (a ~ ()) => PrintType (IO a) where
    printAll' args = do
        mapM_ putStrLn args
        return undefined -- return undefined rather than () make sure nobody uses it.

instance (Show a, PrintType r) => PrintType (a -> r) where
    printAll' acc = \x -> printAll' (acc ++ [show x])

printAll :: (PrintType t) => t
printAll = printAll' []
~~~

其他语言
------

#### c

C 语言中，`printf` 函数也是典型的边长参数函数，实现原理是在参数压栈是，先将参数从右至左压入栈，然后，将各个参数的位置指针压栈，然后，将参数个数压栈。对调用者来说，通过参数个数
和每个参数的位置指针，边可以按照特性的类型解析各个参数。`stdarg.h`中定义了`va_start/va_arg/va_end`这一系列函数用于解析变长参数。

#### c++

C++11 开始支持 Variadic Template，因此，很容易实现边长参数函数。C++版的`Variadic printAll`：

~~~cpp
#include <iostream>

template<typename T>
void printAll(T const & t) {
    std::cout << t;
}

template<typename First, typename... Rest>
void printAll(First const & first, Rest const &... rest) {
    std::cout << first;
    printAll(rest...);
}

int main() {
    int i = 10;
    std::string s = "Hello world";
    printAll("i = ", i, " and s = \"", s, "\"\n");
}
~~~

#### scheme

~~~scheme
(define print‐all
  (lambda (things)
    (for‐each (lambda (x) (display x) (newline)) things)))

(print‐all 4 3 5 6 4 3)
~~~

