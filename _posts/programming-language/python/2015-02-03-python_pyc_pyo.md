---
title: Python编译成Pyc和Pyo文件
author: He Tao
date: 2015-02-03
tag: Python
category: 编程语言
layout: post
---

什么是pyc文件和pyo文件
----------------------

pyc文件是Python的字节码(byte code)文件，是一种二进制文件。pyc文件跨平台，由python的虚拟机加载执行。pyc文件与Python的版本有关，不同版本的Python编译出的pyc文件不同。

pyo文件是优化(optimize)后的字节码文件。

编译成pyc文件
-------------

可以在命令行执行以下命令来将Python源码文件编译成pyc文件：

    python -m py_compile $filename

其中，`$filename`是要编译的Python源码文件名。

也可以编写一下脚本来将Python源码文件编译成pyc文件：

<!--more-->

```python
import py_compile
py_compile.compile('$filename') # $filename is the file name of python source code.
```

编译成pyo文件
-------------

使用以下命令将Python源码文件编译成pyo文件：

    python -O -m py_compile $filename

或者

    python -OO -m py_compile $filename

其中，
+ -O选项表示优化产生的字节码，优化程度由PYTHONOPTIMIZE(environment)的值来决定。
+ -OO选项表示在-O优化的基础上移除所有的doc-strings(文档文本)。

py_compile
-----------

py_compile模块提供了一组函数用于由源码文件生成字节码文件。

    py_compile.compile(file, cfile=None, dfile=None, doraise=False, optimize=1)

参数含义:
+ file: Python源码文件(source code) 
+ cfile: 指定编译得到的pyc文件的路径，cfile的默认位置遵循`PEP 3147`的约定
+ dfile: 如果指定了dfile，在error message中使用dfile的值来作为源文件的文件名而非file参数的文件名
+ doraise: 是否抛出警告
+ optimize: 优化级别

批量生成pyc文件
---------------

使用compileall模块可以批量编译整个目录下的Python源码文件。

函数定义：

    compileall.compile_dir(dir, maxlevels=10, ddir=None, force=False, rx=None, quiet=False, legacy=False, optimize=-1) 

用法：

    python -m compileall $dir

或者编写以下脚本来实现此功能：

```python
import compileall
compileall.compile_dir('$dir')
```

其中，$dir为Python源代码所在的目录。





