---
title: Python yield 关键字
author: He Tao
date: 2015-01-31
tag: Python
category: 编程语言
layout: post
---

Yield 表达式和声明仅仅用于定义一个“生成器(`generator`)”函数，并且仅仅用在“生成器”函数的函数体中。使用yield声明足以使得函数定义产生一个`generator`函数而非一个普通的函数。

> Yield expressions and statements are only used when defining a `generator` function, and are only used in the body of the generator function. Using yield in a function definition is sufficient to cause that definition to create a generator fun instead of a normal function.

The `yield` statement
---------------------

    yield_stmt ::= yield_expression

`yield` 声明与yield表达式(`yield expression`) 在语义上等价。yield声明通常可以省略括号使用然而在yield表倒是中括号是必须的(required)。例如

<!--more-->

    yield <expr>
    yield from <expr>

和

    (yield <expr>)
    (yield from <expr>)

是等价的。

Yield expressions
-----------------

    yield_atom          ::= "(" yield_expression ")"
    yield_expression    ::= "yield" [expression_list] | "from" expression]

当调用一个生成器函数时，返回一个生成器（迭代器）。然后这个生成器会控制生成器函数的执行。当生成器的一个方法被调用时生成器函数开始执行。

Iterator和Generators
--------------------

Iterator，迭代器，可以逐个地读取每一项。可以使用`for ... in ...`语句来操作可迭代对象。如list, str, tuple 等。

Generator，生成器，生成器同样是可迭代对象。但由于生成器是动态地生成值，并没有把所有制都放在内存中。因此，只能读取一次。

yield关键字产生生成器函数，其对象是生成器，可迭代。

Generator-iterator methods
--------------------------

class generator

+ generator.__next__()
+ generator.send()
+ generator.throw(type[, value[, traceback]])
+ generator.close()

当第一次调用生成器的__next__() 方法时，函数执行并生成迭代器。如果调用生成器的__next__()方法，将会得到该生成器函数的下一个值。如果已经到达末尾，则抛出`StopIteration`异常。

如果调用生成器的send()方法，结果将会是send()方法传入的值。

yield 表达式可以用在 `try ... finally `结构的`try`部分中。当生成器的close()函数被调用时，`finally`语句块中的表达式会执行。

Example
-------

```python
>>> def fib(bound):
	a, b, n = 0, 1, 1
	while n < bound:
	    yield a
	    a, b = b, a+b
	    n += 1
	    
>>> g = fib(5)
>>> g.__next__()
0
>>> g.__next__()
1
>>> g.__next__()
1
>>> g.__next__()
2
>>> for i in g:
	print(i)
	
0
1
1
2
>>> 
```

由此，yield声明可以得到一个生成器函数。其类型为`<class 'generator'>`。

yield 表示与 `try ... finally `语句块的运用:

```python
>>> def func(value = None):
        print("Execution starts when 'next()' is called for the first time.")
        try:
            while True:
                try:
                    value = (yield value)
                except Exception as e:
                    value = e
        finally:
            print("Don't forget to clean up when 'close()' is called.")

>>> g = func(1)
>>> g.__next__()
Execution starts when 'next()' is called for the first time.
1
>>> print(g.__next__())
None
>>> g.close()
Don't forget to clean up when 'close()' is called.'
>>> 
```

读取文件时，如果直接对文件对象调用`read()`方法，会导致不可预测的内存占用，因此，常常需要用固定长度的缓冲区来不断读取文件内容。通过yield关键字，很容易实现文件的块读取。具体实现如下：

```python
def block_read(fpath, BLOCK_SIZE = 1024):
    with open(fpath, mode = 'rd') as fp:
        while True:
            block = f.read(BLOCK_SIZE)
            if block:
                yield block
            else:
                return
```

Reference
---------

1. [Python 34 Documentation](https://docs.python.org/3/)


