---
title: Haskell中FFI的使用
author: sighingnow
date: 2016-05-15
tag: [Haskell]
category: 编程语言
layout: post
---

FFI, Foreign Function Interface, 用于在一种编程语言调用另一种编程语言编写的函数。Haskell的FFI提供了
Haskell编写的程序调用其它语言编写的函数或者在其他语言中调用Haskell的函数，以进行协作的能力。

<!--more-->

Haskell调用C函数
---------------

在Haskell中调用libc中的函数，只需要在Haskell代码中声明对应的函数原型，就可以跟正常的Haskell函数一样进行调用：

~~~haskell
foreign import ccall "exp" c_exp :: Double -> Double

e_value = c_exp 1
~~~

GHC在编译Haskell源代码是默认会链接libc，并且GHCi在启动时也会载入libc。对于自己编写的函数，例如：

+ C代码

~~~c
float f_add(float x, float y) {
    return x+y;
}
~~~

+ Haskell代码

~~~haskell
foreign import ccall "f_add" f_add :: Float -> Float -> Float
~~~

编译的时候，除了编译Haskell源文件外，还需要编译或者链接C语言源文件。

+ 直接编译并链接

        ghc --make ffi-example-hs.hs ffi-example-c.c

+ 动态链接库

        gcc -shared ffi-example-c.c -o ffi-example-c.dll  ## on linux: -fPIC -rdynamic
                                                          ## optional: -Wl,--out-implib,libffi-example-c.a
        ghc --make ffi-example-hs.hs -L . -lffi-example-c

+ 静态链接库

        gcc -c ffi-example-c.c
        ar rcs libffi-example-c.a ffi-example-c.o
        ghc --make ffi-example-hs.hs -L . -lffi-example-c

在GHCi中，可以通过动态链接库的形式载入C语言编写的模块：

    ghci > :set -lffi-example-c
    ghci > :l ffi-example-hs.hs

除了这两种方式，还可以通过手动加载动态链接库的方式进行。在Linux上，可以使用[libdl](http://linux.die.net/man/3/dlopen)的
封装，[System.Posix.DynamicLinker](https://hackage.haskell.org/package/unix/docs/System-Posix-DynamicLinker.html)中的函数。
在Windows上，可以使用[System.Win32.DLL](http://hackage.haskell.org/package/Win32/docs/System-Win32-DLL.html)中的函数加载动态
链接库，使用`getProcAddress`得到函数地址，使用[Foreign.Ptr](http://hackage.haskell.org/package/base/docs/Foreign-Ptr.html)中
的函数`castPtrToFunPtr`将指针转换成对应类型的函数指针。

C代码中调用Haskell函数
---------------------

+ Haskell代码

~~~haskell
module FFIExample where  -- module declaration is required.

foreign export ccall "fibonacci" fibonacci :: Int -> Int

fibonacci :: Int -> Int
fibonacci = (memo !!) where memo = 0 : 1 : zipWith (+) memo (tail memo)
~~~

+ C代码

~~~c
#include <stdio.h>
#include "ffi-example-hs_stub.h"

int main(int argc, char **argv) {
    hs_init(&argc, &argv);

    printf("%d\n", fibonacci(10));

    hs_exit();
}
~~~

### 使用GHC编译、链接

#### 完全静态链接

生成的可执行文件不依赖额外的动态链接库：

    ghc -c ffi-example-hs.hs
    ghc --make -no-hs-main ffi-example-c.c ffi-example-hs -o ffi-example.exe

#### 使用动态链接库

增加一个C语言编写的loader，用于初始化和退出Haskell运行时：

~~~c
#include "ffi-example-hs_stub.h"
void haskell_init(int *argc, char ***argv) {
    // int argc = 1
    // char *argv[] = {"GHC Runtime", NULL}; // argv must end with NULL.
    hs_init(argc, argv);
}

void haskell_exit() {
    hs_exit();
}
~~~

在C语言程序中使用Loader提供的函数，并调用Haskell实现的函数：

~~~c
#include <stdio.h>

// int fibonacci(int);
#include "ffi-example-hs_stub.h"

#if defined(__cplusplus)
extern "C" {
#endif
    void haskell_init(int *argc, char ***argv);
    void haskell_exit();
#if defined(__cplusplus)
}
#endif

int main(int argc, char **argv) {
    haskell_init(&argc, &argv);
    printf("%d\n", fibonacci(10));
    haskell_exit();
}
~~~

编译链接：

~~~shell
ghc -c ffi-example-hs.hs
ghc -c ffi-example-loader.c
ghc -shared -o ffi-example.dll ffi-example-loader.o ffi-example-hs.o
ghc -no-hs-main ffi-example-c.c ffi-example.dll.a -o ffi-example.exe
## or gcc ffi-example-c -lffi-example
## or gcc ffi-example-c ffi-example.dll.a
## when use gcc, the target file will be more small, but can't include "***stub.h",
## haskell functions must be declared manually.
~~~

通过将Haskell模块打包成动态链接库，还可以用于被Python、Java程序调用。

复杂数据结构
-----------

Haskell中的`data`可以与C语言中的结构体对应，并可以通过指针的方式在两种语言的代码中交换数据。
与C语言进行数据交换的类型必须是`Foreign.Storable`类型类的实例类型。

使用DSL
------

Haskell库[inline-c](https://hackage.haskell.org/package/inline-c)使用TemplateHaskell和QuasiQuotes提供了一套DSL，能够方便地以inline的形式在Haskell程序中调用C语言代码。一个简单的例子：

~~~haskell
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Language.C.Inline as C

C.include "<math.h>"

main :: IO ()
main = [C.exp| double{ cos(1) } |] >>= print
~~~

