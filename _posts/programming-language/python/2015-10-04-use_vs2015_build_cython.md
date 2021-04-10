---
title: 使用 VS2015 编译 Cython 程序
author: Tao He
date: 2015-10-04
tag: Python
category: 编程语言
layout: post
---

Cython, C-Extensions for Python, 通过 Cython，可以把 Python代码编译到 C 语言代码，甚至在 Python 代码中混用 C 语言的 native 数据类型，
进一步编译成动态链接库，从而极大地提升程序的运行效率。

<!--more-->

Hello world
-----------

创建一个后缀为 `.pyx` 的文件，在其中写入：

~~~python
print("Hello World")
~~~

然后创建一个 setuptools 的构建脚本，指定使用 Cython 编译扩展模块：

~~~python
from distutils.core import setup
from Cython.Build import cythonize

setup(
    ext_modules = cythonize("helloworld.pyx")
)
~~~

运行编译命令：

    python setup.py build

便会在当前目录下得到一个 helloworld.c 的文件，这便是 Cython 根据 Python 代码转换出来的 C 语言程序。此外，当前目录下还有一个 build 文件夹，里面的 lib.xxx.3.4 目录下有一个 helloworld.pyd 的文件，这便是我们得到的动态链接库。使用这个库，直接 import 即可：

~~~python
import helloworld
~~~

便会得到 `hello world` 的输出。

在 Cython 的扩展模块中编写函数以及调用这些函数同样很简单：

~~~python
def greet():
    print("Hello World")
~~~

调用：

~~~python
import helloworld

helloworld.greet()
~~~

编译 Cython 遇到的问题
--------------------

在 Linux 上，编译 Cython 模块，只要有 gcc 即可，在 Windows 上，可以使用 Mingw 或者 MSVC 作为 C 语言代码的编译器。关于如何指定具体的编译器，可以参考[Building Extensions: Tips and Tricks](https://docs.python.org/3/install/#building-extensions-tips-and-tricks)。具体做法：

在 Windows 平台上，Python 默认使用的编译器是 MSVC，如果要使用 mingw 作为编译器，在 Python 安装目录下的 `Lib\distutils` 下面的 distutils.cfg 文件(如果没有这个文件，创建一个即可)中加入一下内容：

~~~
[build]
compiler = mingw32
~~~

而想要指定使用 MSCV 编译器，在 distutils.cfg 中加入:

~~~
[build]
compiler = msvc
~~~

即可。

由于 MinGW 并没有官方发行的 64 位版本，在 Cython 的Wiki中也强调 **Do not use MinGW-w64**。在 Windows 平台上，如果我们安装的是 64 位版本的 Python，当我们使用设置 `compiler=mingw32` 来构建上面的
helloworld的例子时，只要我们 import 得到的动态链接库，Python 就会 Crash，这是由于库的二进制不兼容的问题。因此，需要使用 MSVS 编译器。

而是用 MSVC 编译，就需要安装 MSVC  编译器，在 Windows 7 上可以通过安装 Windows SDK 来解决，但在 Windows 8 及之后的版本中，已经没有独立的 SDK 安装包了，这就需要安装 Visual Studio。

而安装了 MSVC 的可能会遇到

    Unable to find vcvarsall.bat

的问题，这是因为 VS 的版本问题，找到 Python 安装目录下的 `Lib\distutils\msvc9compiler.py`，找到第 292 行(Python 3.4.3)：

    VERSION = get_build_version()

改为安装的 VS 中 VC 编译器的版本，例如，VS 2015 就对应着 14.0，

    VERSION = 14.0

这样就解决了这个问题。

参考
---

1. [CythonExtensionsOnWindows](https://github.com/cython/cython/wiki/CythonExtensionsOnWindows)
2. [Building Extensions: Tips and Tricks](https://docs.python.org/3/install/#building-extensions-tips-and-tricks)
