---
title: Python 的默认参数
author: He Tao
date: 2015-02-10
tag: Python
category: 编程语言
layout: post
---

Python的默认参数为编程带来了极大地便利，但同时误用默认参数也产生了不小的陷进。

默认参数的求值
---------------

在Python(34)的Documentation中，有这样一段叙述：

> Default parameter values are evaluated from left to right when the function definition is executed. 

Python的默认参数是从左向右求值。看下面这段代码：

```python
param = [1, 3, 2, 4]

def func(a=param.copy(), b=param.sort(), c=param.copy()):
    print(a, b, c)

if __name__ == '__main__':
    func()
```

<!--more-->

按照Python默认参数从左到右求值的规则，有如下的输出结果：

    [1, 3, 2, 4] None [1, 2, 3, 4]

再看如下一段代码：

```python
param = [1, 3, 2, 4]

if __name__ == '__main__':
    print(param)

def func(a=param.copy(), b=param.sort(), c=param.copy()):
    print(a, b, c)

if __name__ == '__main__':
    print(param)
```

运行，得到如下输出：

    [1, 3, 2, 4]
    [1, 2, 3, 4]
    
出现以上现象的原因在于Python的默认参数是在运行`def`语句是进行求值的。默认参数只进行一次求值，之后每次调用函数是，都是用已经算出来的值("pre-computed" value)。

可变对象作为默认参数
---------------------

将可变对象如`list`, `dict`等作为默认参数时，可能会改变对象的值，从而引起一些错误。在使用可变对象作为默认参数时，应当注意以下两个问题。

### 可变对象的求值

正如上文所讲到的，默认参数在函数定义时（`def`语句中）进行求值。同时，其求值顺序为从左至右。因此，要注意对默认参数求值时因可变对象的值改变而可能引起的错误。

### 不会创建新对象

使用可变对象作为默认参数时，参数可以在不创建新对象的情况下对参数对象进行修改。考虑如下一段代码的运行情况：

```python
>>> def func(param=[]):
	param.append(1)
	return param

>>> func()
[1]
>>> func()
[1, 1]
>>> func()
[1, 1, 1]
```

可见，每次调用函数时并没有重新将`param`的值赋为`[]`，而是直接在原来的对象的基础上修改。这也正是默认参数仅仅在函数定义时求值的原因。

```python
>>> def func(param=[]):
	param.append(1)
	print(id(param))
	return param

>>> func()
29548216
[1]
>>> func()
29548216
[1, 1]
>>> func()
29548216
[1, 1, 1]
>>> 
```

从上面这段代码的运行结果不难看出，`param`始终是同一对象。

### 解决方法

为了避免程序中出现此类问题，应当用`None`作为默认参数，并且在函数开始的时候检查参数是否为`None`即可。

```python
def func(param=None):
    if param == None:
        ...
    ...
```

有些情形下，直接将对象与`None`比较是否相等可能会导致一些警告信息(例如`Numpy`库中的某些对象)。可以用以下方式来解决这一问题：

```python
def func(param=None):
    if param is None:
        ...
    ...
```

在Python(34)的Documentation中，有这样的叙述：

> This is generally not what was intended. A way around this is to use None as the default, and explicitly test for it in the body of the function.

并给出了如下的一个示例：

```python
def whats_on_the_telly(penguin=None):
    if penguin is None:
        penguin = []
    penguin.append("property of the zoo")
    return penguin
```

默认参数的修改
-----------------

默认参数也是函数的一个属性(attribute)，可以像修改普通的函数属性那样修改默认参数的值。Python中国，函数的`__defaults__`属性是一个包含函数所有的默认参数的元组。

```python
>>> def func(param=10):
	print(param)

	
>>> func()
10
>>> func.__defaults__
(10,)
>>> func.__defaults__ = ([1, 2, 4],)
>>> func()
[1, 2, 4]
>>> func.__defaults__ = ()
>>> func.__defaults__
()
>>> func()
Traceback (most recent call last):
  File "<pyshell#25>", line 1, in <module>
    func()
TypeError: func() missing 1 required positional argument: 'param'
>>>
```

可见，通过修改默认参数来改变函数的性质的方法也是可行的。

参考
------

1. [http://effbot.org/zone/default-values.htm](http://effbot.org/zone/default-values.htm "default-values")

2. [Python Documentation](https://docs.python.org/3/)



