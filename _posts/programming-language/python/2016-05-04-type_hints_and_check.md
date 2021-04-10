---
title: Python的类型标注与检查
author: Tao He
date: 2016-05-04
tag: Python
category: 编程语言
layout: post
---

Python作为一个动态类型语言，支持Subtyping和Duck Typing，一些Lint工具可以对Python代码静态进行类型
检查来提高程序质量，还有工具可以支持类型标注和检查，并编译代码以提升性能。Python 3.5又引入了
Type Hints机制来提供统一的类型标注协议。

<!--more-->

`__class__` and `isinstance`
----------------------------

Python中，判断一个对象是否是某个类的实例的做法：

+ 判断直接实例使用`a.__class__ is A`
+ 判断间接实例使用`isinstance(a, A)`

`__class__`可以用来获取一个对象的类对象，`is`用于比较是否是内存中的同一个对象。`isinstance`判断
给出的对象是否是给出的类的直接或间接实例类型。举例：

~~~python
In []: class A: pass
In []: class B(A): pass
In []: b = B()

In []: b.__class__ is B
Out[]: True
In []: b.__class__ is A
Out[]: False

In []: isinstance(b, B)
Out[]: True
In []: isinstance(b, A)
Out[]: True
~~~

Duck Typing
-----------

Duck Typing，鸭子类型，是动态类型的一种实现风格，指的是一个对象的语义不取决于其继承
自特定的类或实现特定的接口，而是关注这个对象有哪些可以被调用的方法和可以被访问的属性。
当被调用的方法或被访问的属性不存在时，会产生一个运行时错误。Python使用了Duck Typing
来实现多态(Run Time Polymorphism)的能力。示例：

~~~python
class A:
    def f(): pass
    def g(): pass
class B:
    def g(): pass
    def h(): pass

def func(duck):
    g()
~~~

Duck Typing放弃了任何类型检查，完全没有语言本身的机制来检查，这种方式非常灵活，
完全依靠程序员来通过注释、文档、测试来强制约束。除了Python之外，Java(通过reflection)、
Javascript，Common Lisp等编程语言也支持Duck Typing。

Type Hints
-----------

PEP 482、PEP 483和PEP 484这三篇Proposal描述了Python 3.5引入的Type Hints机制。已经有mypy、
Reticulated Python和numba等实现了一定程度的静态类型检查，PyCharm这类IDE也通过类型推导
和检查机制来静态检查代码中的错误以及给出改进建议。其他的一些动态类型语言如PHP、TypeScript
等也引入了Type Hints来进行运行时的类型推导和检查。

值得注意的是，Type Hints**只是提供了一个统一的类型标注的协议，而并没有提供类型检查机制。**

~~~python
def greeting(name: str) -> str:
    return 1

if __name__ == '__main__':
    print(greeting(1))
~~~

很明显这段代码中函数的实际返回值类型标注不符，且调用函数时参数类型也与标注不符，但仍然
能正常运行。

早在2006年，PEP 3107 Function Annotations就已经允许对函数的参数和返回值进行类型和注释
的标注。PEP 3107已经提供了Type Hints所需要的语法，PEP 484提供了一个额外的模块`typing`，提供
了`Callable`、`Generic``Sequence`，`TypeVar`等GenericMeta和TypingMeta来构造一些更复杂的组合类型，
对于TypingMeta类型的构造器还提供了Covariance和Contravariance选项。`typing`同时
以[库](https://github.com/python/typing)的形式发布，可以通过`pip3 install typing`来安装，
用于支持Python 3.5以前版本的Python。`typing`模块还提供了`typing.TYPE_CHECKING`这一常量来
对不会被解释执行的代码执行类型检查，在类型检查阶段或静态分析阶段，`type.TYPE_CHECKING`的
值为`True`。PEP 484还定义了Type Comments，由于在Python中并没有First-class syntax用来支持
Type Annotation，对于某些比较复杂的表达式，可以用来Type Comments来进行类型标注：

~~~python
x = []   # type: List[float]
x, y, z = [], [], []  # type: List[int], List[int], List[str]
x, y, z = [], [], []  # type: (List[int], List[int], List[str])
x = [
   1,
   2,
]  # type: List[int]
~~~

在可以预见的未来，CPython本身不会引入原生的类型推导和检查的支持，PEP 484中有
这样一段话：

> Python will remain a dynamically typed language, and the authors have no desire
> to ever make type hints mandatory, even by convention.

Type Hints的另一个可能的用途是用来辅助实现Function/Method Overloading，提供Multiple
Dispatch的能力，目前`typing.overload`仅能用于stub files。PEP 484中表示，Overloading
Syntax与Multiple Dispatch由于而这目的和用途不同，应该各自独立实现。[overloading.py](https://github.com/bintoro/overloading.py)
以库的形式通过Decorator的形式实现了基本的Function/Method Overloading的支持，并且
支持使用PEP 484定义的类型标注协议以及`typing`里的构造器定义的复合类型。

想要真正做静态类型推导检查，仍然需要使用[mypy](http://mypy.readthedocs.io/en/latest/index.html)
之类的工具。mypy已经支持PEP 484，mypy发现类型错误是的错误信息也非常友善，在实际项目
中使用mypy非常有利于提升项目质量。

Gradual Typing
--------------

类型决定了一个程序中一个符号的取值范围。Indiana大学的Jeremy Siek提出的Gradual Typing
可以作为一种弥补的方案。Gradual Typing也是Python的Type Hints以及mypy等类型检查工具的理论基础。

> Gradual typing allows one to annotate only part of a program, thus leverage desirable
> aspects of both dynamic and static typing.

具体来说，如果变量`a_value`的类型和标识符`a_variable`一致，那么就可以将`a_value`赋值给`a_variable`。
对于两个不同的类型，“一致(consistant)”包含三种含义：

+ 如果类型`t1`是类型`t2`的子类型，则`t1`和`t2`一致。
+ `Any`类型与其他任何类型一致。
+ 任何类型都可以视作`Any`类型的子类型，也就意味着任何类型都与`Any`类型一致。

具体举例，定义两个类型：

    class A: ...
    class B(A): ...

定义一个`A`类型的变量`a`，则`a`具有类型`A`，由于`B`类型是`A`类型的子类型，与`A`类型一致，因此
标识符`a`可以被赋一个`B`类型的值：

    a = A() # type: A
    a = B()

反过来，定义一个`B`类型的变量`b`，不能给标识符`b`赋一个`A`类型的值：

    b = B() # type: B
    b = A() # error.

`Any`类型可以视为类型继承图的根节点，因此，可以给`Any`类型的标识符赋任何类型的值：

    c = ... # Any
    c = A()
    c = B()



