---
title: DLL的初始化和退出处理
author: Tao He
date: 2016-10-08
tags: C/C++
categories: 编程语言
layout: post
---

将Haskell代码编译成动态链接库供其他语言调用时，需要调用`hs_init`函数初始化GHC的运行时环境，退出时
需要调用`hs_exit`函数来完成释放内存等清理工作[^1]。如果把初始化和退出处理交给用户程序去做，不仅会给
用户程序代码带来额外的麻烦，而且错误的初始化和退出处理还会造成调用动态链接库时的错误。DLL本身的
处理化和退出清理机制可以很好地解决这一问题。

<!--more-->

Linux
-----

Linux上使用GCC构建动态链接库时，可以使用`__attribute__((constructor))`和`__attribute__((destructor))`
函数属性来输出DLL的构造和析构函数。构造函数会在`dlopen`返回前或库被装载时调用，析构函数会在
`dlclose`返回前或者`main`函数返回后，或者卸载库过程中被调用。

对于加载Haskell代码编写的动态链接库，使用示例如下：

~~~c
__attribute__((constructor)) static void hslib_init() {
    static int argc = 1;
    static char *argv[] = {"HsLib.so", NULL};
    hs_init(&argc, &argv);
}

__attribute__((destructor)) static void hslib_final() {
    hs_exit();
}
~~~

此外，在Linux系统中，还可以通过`.init`和`.fini`机制来完成动态链接库的初始化和退出处理，在
使用`ld`构建动态链接库时，通过`-init <function name>`和`-fini <function name>`参数指定构造和
析构函数。如果直接使用`gcc`完成链接工作，相应的命令行参数为`-Wl,-init,<function name>`
和`-Wl,-fini,<function name>`。但是，这种方式已经呗标为`OBSOLETE/DANGEROUS`[^2]。某些环境下，
程序可能会非常依赖GCC自动添加的`.init`函数和`.fini`函数，因此这种方式可能会导致一些无法预测
和复现的运行现象，甚至导致程序崩溃。

Windows
-------

Windows系统上，使用`LoadLibrary`和`FreeLibrary`函数来加载和卸载动态链接库，都会调用`DllMain`函数。这个函数的原型为[^3]：

~~~c
BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpvReserved);
~~~

其中，`reason`参数指明了调用`DllMain`函数的原因，可选值包括：

+ `DLL_PROCESS_ATTACH`：第一次加载DLL、将DLL文件映射到进程的地址空间时。
+ `DLL_PROCESS_DETACH`：当调用`FreeLibrary`函数，DLL被从进程的地址空间解除映射时，通过DLL进行清理
工作。
+ `DLL_THREAD_ATTACH`：当进程创建一个新的线程时系统会查看当前映射到进程地址空间的所有DLL文件映像、
并调用`DllMain`函数。
+ `DLL_THREAD_DETACH`：当线程函数返回时系统会自动调用`ExitThread`函数，系统会查看当前映射到进程
地址空间的所有DLL文件映像、并调用`DllMain`函数，通知DLL进行线程级的清理工作。

对于加载Haskell代码编写的动态链接库，`DllMain`函数如下例：

~~~c
BOOL APIENTRY DllMain(HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpvReserved)
{
    static int argc = 1;
    static char *argv[] = {"HsLib.dll", NULL};
    switch (fdwReason) {
        case DLL_PROCESS_ATTACH:
            hs_init(&argc, &argv);
            break;
        case DLL_PROCESS_DETACH:
            hs_exit();
            break;
    }
    return 0;
}
~~~

References
----------

[^1]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ffi-chap.html#using-the-ffi-with-ghc
[^2]: http://www.faqs.org/docs/Linux-HOWTO/Program-Library-HOWTO.html#INIT-AND-CLEANUP
[^3]: https://msdn.microsoft.com/en-us/library/windows/desktop/ms682583(v=vs.85).aspx


