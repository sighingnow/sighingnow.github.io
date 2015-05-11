---
title: Python Decorator
author: He Tao
date: 2015-02-01
tag: Python
category: 编程语言
layout: post
---

Decorator, 装饰器。Decorator的用途在于允许在函数和类中嵌入或者修改代码。

classmethod and staticmethod
----------------------------

    staticmethod(function)

这将返回一个function的静态方法。把类的一个方法的声明为静态方法，具体用法如下:

```python
class C:
    @staticmethod
    def f(arg1, arg2, ...): ...
```

或者

```python
class C:
    def f(arg1, arg2, ...): ...
    f = staticmethod(f)
```

<!--more-->

类的静态方法既可以被类调用也可以被类的实例调用，例如:

    C.f(...)

或者:

    C(...).f(...)

**注意**，staticmethod 并不将类自身或其示例接收为隐含的第一个参数，在这一点上staticmethod 和 classmethod 存在区别。

    classmethod(function)

这将返回function的一个类方法。具体用法如下：

```python
class C:
    @classmethod
    def f(cls, arg1, arg2, ...): ...
```

或者：

```python
class C:
    def f(cls, arg1, arg2, ...): ...
    f = classmethod(f)
```

类的classmethod既可以被类调用也可以被类的实例调用，例如:

    C.f(...)

或者:

    C(...).f(...)

**注意**，classmethod 像将类的示例函数一样，将类自身接收为隐含的第一个参数。

Python Decorators for Functions and Methods
-------------------------------------------

### 函数decorator

一个函数decorators用于函数定义，它位于在函数定义之前的一行。例如：

```python
@decoFunc
def aFunc():
    print('a Func is called')
```

当编译器经过这段代码时，`aFunc()`被编译然后将结果函数对象传递给`decoFunc`代码，后者创建一个类函数对象并取代原来的`aFunc()`。根据这一说法，那么：

```python
@A
@B
@C
def func(arg1, arg2, ...): ...
```

等价于

```python
def func(arg1, arg2, ...): ...
func = A(B(C(func)))
```

例如：

```python
def decFunc(f):
    def inner(f):
        return 100
    return inner

@decFunc
def aFunc(x):
    print('value x is %d'%(x))
    print('aFunc called')

print(aFunc(10))
```

程序的输出结果为

    100

并没有输出

    aFunc called

也就是说，在装饰器函数中，函数`aFunc`的行为被改变了。

### 类decorator

与函数decorator类似，也可以将类作为decorator，只要实现`__call__`方法即可。

一个类如果实现了`__call__`方法，那么这个类的对象便是可调用的。例如：

```python
class decCls():
    def __call__():
        print('__call__ of decCls called.')

decCls()()
```

运行，输出：

    __call__ of decCls called.

下来举一个将类作为decorator的例子：

```python
class decCls():
    def __init__(self, f):
        self.f = f
    def __call__(self, x):
        print('decorator called')
        return 10000

@decCls
def aFunc(x):
    print('value x is %d'%(x))
    print('aFunc called')

print(aFunc(10))
```

此处就相当于

```python
def aFunc(x):
    print('value x is %d'%(x))
    print('aFunc called')
aFunc = decCls(aFunc)
```

那么在调用`aFunc`的时候，其实是在调用`decCls(aFunc)`，也就是运行的是

    decCls(aFunc)(10)

最后相当于调用的是装饰器类`decCls`的`__call__`方法。

decorator的用途
---------------

decorator的一个很大的用途在于可以“掌控”一个函数的运行。看下面这段代码：

```python
def fn_decorator(fn):
    def wrapper(*args):
        print('call wrapper')
        return fn(*args)
    return wrapper

@fn_decorator
def fn(...):
    # ...
```

在这儿，调用`fn`时其实是在调用`wrapper`，因此，`wrapper`是可以控制`fn`是否运行的。

Decorator带参数
---------------

之前的Decorator都是没有参数的，那么，有参数的Decorator又是如何工作的呢？

如果是单个带参数的Decorator，那么：

```python
@dec(param)
def aFunc():
    #...
```

等价于

```
def aFunc():
aFunc = dec(param)(aFunc)
```

如果是多重Decorator，并且带参数，那么有：

```python
@dec_a(params1)
@dec_b(params2)
@dec_c(params3)
def method(args):
    pass
```

等价于

```python
def method(args):
    pass
method = dec_a(params1)(dec_b(params2)(dec_c(params)(method))) 
```

可见，Decorator还是很灵活很强大的。

应用举例
--------

定义一个装饰器(Decorator)来测量函数的执行时间：

```python

import time

def fn_timer(f):
    def wrapper(*args):
        t0 = time.time()
        ans = f(*args)
        t1 = time.time()
        print('f start at %f, end at %f, running %f'%(t0,t1,t1-t0))
        return ans
    return wrapper
```

使用这个装饰器的例子：

```python
@fn_timer
def sum_fn(n):
    ans = 0
    for i in range(0, n):
        ans += i
    return ans

print(sum_fn(10000000))
```

这样，便可以在不改变`sum_fn`的接口和内部实现的前提下实现新的功能。

参考
-----

1. [PEP 318](https://www.python.org/dev/peps/pep-0318/)

