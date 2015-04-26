---
title: Python Decorator
author: He Tao
date: 2015-02-01
tag: Python
category: 编程语言
layout: post
---

Decorator, 装饰器。

classmethod and staticmethod
----------------------------

    staticmethod(function)

这将返回一个function的静态方法。把类的一个方法的声明为静态方法，具体用法如下:

    class C:
        @staticmethod
        def f(arg1, arg2, ...): ...

或者

    class C:
        def f(arg1, arg2, ...): ...
        f = staticmethod(f)

<!--more-->

类的静态方法既可以被类调用也可以被类的实例调用，例如:

    C.f(...)

或者:

    C(...).f(...)

**注意**，staticmethod 并不将类自身或其示例接收为隐含的第一个参数，在这一点上staticmethod 和 classmethod 存在区别。

    classmethod(function)

这将返回function的一个类方法。具体用法如下：

    class C:
        @classmethod
        def f(cls, arg1, arg2, ...): ...

或者：

    class C:
        def f(cls, arg1, arg2, ...): ...
        f = classmethod(f)

类的classmethod既可以被类调用也可以被类的实例调用，例如:

    C.f(...)

或者:

    C(...).f(...)

**注意**，classmethod 像将类的示例函数一样，将类自身接收为隐含的第一个参数。

Python Decorators for Functions and Methods
-------------------------------------------

    @A
    @B
    @C
    def func(arg1, arg2, ...): ...

等价于

    def func(arg1, arg2, ...): ...
    func = A(B(C(func)))

<!--
有待思考：

```python
def decorator(fn):
    def wrapper(*args):
        print('call wrapper')
        return fn(*args)
    return wrapper
```
-->


