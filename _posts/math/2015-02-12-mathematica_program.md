---
title: Mathematica 编程
author: He Tao
date: 2015-02-12
tag: Mathematica
category: Math
layout: post
---

赋值方式
---------------------

Mathematica提供了两种类型的赋值方式。

### 即时赋值

即时赋值是指在赋值时就对值进行计算，即时赋值用`=`表示。例如：

    var = value

在赋值时就计算出`value`的值。

### 延迟赋值

延迟赋值是指调用赋值语句时（使用被赋值的变量时）才对值进行计算，并且在每次调用时，其值都会重新计算。延迟赋值用`:=`表示。例如：

    var := value

在调用`var`是才计算`value`的值。

### 清除赋值

使用如下语法可以做到清除赋值：

    x = .
    t = .

这样处理以后，`x`, `t`都会重新成为一个没有值得符号。

<!--more-->

### 赋值与计算

如果想要计算表达式的值，但又不想给变量赋值，可以使用`/.`符号。例如：

    x^2+2*x+1 /. x->2
    9
    ?x
    Global `x

表明，表达式`x^2+2*x+1`的值已经计算出来，但`x`仍未被赋值。

`/.`的详细用法：

    expr/.rules
        应用一个规则或规则列表尽可能转换一个表达式 expr 的每个子部分.

`/.`符号也可以被用于表达式的整体替换。

### 立即赋值与延迟赋值的例子

以如下代码为例：

```mma
a = Expand[x^2]
b := Expand[x^2]
x = u+v
Print[a]
Print[b]
```

这段程序运行的输出如下所示：

    a = (u+v)^2
    b = u^2+2uv+v^2

可见，在用`u+v`替换`x`之前，`a`就已经展开了，而`b`还没有展开。展开`b`时，已经用`(u+v)`替换了`x`，所以实际上是展开`(u+v)^2`。因此，会看到这样的结果。


循环语句
----------

Mathematica中，循环结构有Do命令，For命令，While，Table 命令三种实现方式，此外，Sum 函数和 Product 函数也具有对指定序列循环求值（和或差）的功能。

### Sum 和 Product

 `Sum` 和 `Product` 函数都可以通过一组指定的首尾和步长(可选)求出一个序列的和或积。并且支持符号运算。

例如：

求前100个素数的和：

    Sum[Prime[k], {k, 1, 100, 1}]

求从1-n的整数的平方和的一般公式：

    Sum[k^2, {k, 1, n, 1}]
    (* 可以得到结果：n*(n+1)*(2*n+1)/6 *)

求无穷和 1+1/2+1/4+1/8+1/16+...

    Sum[1/(2^i), {i, 0, Infinity, 1}]
    (* 可以得到结果为 2 *)

    (* Infinity 表示正的无穷大 *)

### Do 命令

Do命令的用法同Sum和Product函数类似（可以认为Sum和Product是Do命令的特例），给出文档中Do命令的用法：

+ Do[expr,{Subscript[i, max]}] 
    
    对 expr 计算 imax 次.

+ Do[expr,{i,Subscript[i, max]}] 

    将变量 i 从 1 递增到 Subscript[i, max]（步长为 1），计算 expr. 

+ Do[expr,{i,Subscript[i, min],Subscript[i, max]}] 

    从 i=Subscript[i, min] 开始.

+ Do[expr,{i,Subscript[i, min],Subscript[i, max],di}] 

    使用步长 di. 

+ Do[expr,{i,{Subscript[i, 1],Subscript[i, 2],\[Ellipsis]} }] 

    使用连续的值 Subscript[i, 1]，Subscript[i, 2]，\[Ellipsis].

+ Do[expr,{i,Subscript[i, min],Subscript[i, max]},{j,Subscript[j, min],Subscript[j, max]},\[Ellipsis]] 

    对每一个 i 循环地根据不同的 j 等，计算 expr. 


### While 命令

While命令的用法如下：

+ While[test,body] 

    重复计算 test，然后是 body，直到 test 第一次不能给出 True.


### For 命令

For命令的用法如下：

+ For[start,test,incr,body] 

    执行 start，然后重复计算 body 和 incr，直到 test 不能给出 True. 

### Table 命令

Table 命令用于生产表达式的列表。Table 命令的用法如下：

+ Table[expr,{Subscript[i, max]}] 

    产生一个 expr 的 Subscript[i, max] 拷贝的列表.

+ Table[expr,{i,Subscript[i, max]}] 

    产生i  从1到 Subscript[i, max] 的一个 expr 的值的列表.

+ Table[expr,{i,Subscript[i, min],Subscript[i, max]}] 

    以 i=Subscript[i, min] 开始.

+ Table[expr,{i,Subscript[i, min],Subscript[i, max],di}] 

    使用步长 di. 

+ Table[expr,{i,{Subscript[i, 1],Subscript[i, 2],\[Ellipsis]} }] 

    使用连续值 Subscript[i, 1], Subscript[i, 2], \[Ellipsis]. 

+ Table[expr,{i,Subscript[i, min],Subscript[i, max]},{j,Subscript[j, min],Subscript[j, max]},\[Ellipsis]] 

    给出一个嵌套列表. 和 i 相关联的列表是最外的列表. 


分支结构
---------

Mathematica中，分支控制主要有If、Which、Switch 等命令实现。

### If命令的用法如下：

+ If[condition,t,f] 

    如果 condition 计算为 True 给出 t，如果它计算为 False 给出 f.

+ If[condition,t,f,u] 

    如果 condition 计算既不为 True 也不为 False 给出 u.


### Which 命令

Which命令的用法如下：

+ Which[test1,value1,test2,value,...

    依次计算每个 testi ，返回相应于产生 True 的第一个valuei的值.

### Switch 命令

+ Switch[expr, form1, value1, form2, value2, ...]

    计算 expr，然后依次和每个 formi 比较，计算并返回相应于找到的第一个匹配的valuei].

函数与模式
-----------

1. `Cases`命令，查找列表中匹配模式的表达式。

```mma
In:= Cases[{f[1], g[2], f[5], g[4]}, f[_]]
Out= {f[1], f[5]}
```

此处，`_`(blank)用来代表任意表达式。

2. `x_`(`x:_`的缩写)，代表一个模式，将其值命名为`x`:

```mma
In:= Replace[f[100], f[x_] -> x+5]
Out= 105
```

`->`用来指定规则。

3. `./`表示匹配模式并全部替代。

```mma
In:= {f[1], g[2], f[4], g[4]} ./ f[x_] -> x+5
Out= {6, g[2], 9, g[4]}
```
2
4. `__`(两个blank)代表**任意表达式**。例如：

```mma
fIn:= Cases[{f[1,2],f[1],g[3]}, f[__]]
Out= {f[1,2],f[1]}
```

5. `a|b|c`代表`a`、`b`或`c`。例如：

```mma
In:= Cases[{f[1],g[2],f[3],f[5]}, f[1|3]]
Out= {f[1],f[3]}
In:= Cases[{f[1],g[1],f[3],f[5]}, (f|g)[1]]
Out= {f[1],g[1]}
```

6. `_h`代表任何具有标头`h`的表达式。例如：<!--没太看明白这个“标头”的含义-->

```mma
In:= Cases[{1, 2, 3.5, 6}, _Real]
Out= {1, 2, 6}
```

7. `:>`是一个延迟规则，类似于规则中的`:=`。

从上面几个例子中可以看出函数式编程中模式匹配的强大威力。

定义数学函数
------------

### 定义普通单变量函数


定义函数是实现编程的重要基础性功能。在Mathematica中，可以通过以下方式定义函数。

同样，在函数定义中，也可以使用“即时赋值”和“延迟赋值”这两种赋值方式。

加入需要定义一个单变量函数`f`，以`x`为变量，可以写为如下形式：

    f[x_] = ...

或者

    f[x_] := ...

### 分段函数


分段函数可以用`/;condition`来定义。即输入：

    f[x_] := expr /; condition

就会使当且仅当条件值为真时，`f[x]`的值为表达式。

**注意**：在此情形下，必须使用`:=`赋值符号。

### 定义多变量函数

在Mathematica中，定义多变量函数的方式同定义单变量函数类似：

    f[x_, y_] = ...

例如：

    f[x_, y_] = x^2 + y^2

数学函数的运算
---------------

如果 `f`,`g`为具有相同定义域`D`的函数，那么就可以逐点定义他们的和、差、积、商。

数学函数的复合
---------------

如果`x`在`g`的定义域内，而`g(x)`在`f`的定义域内，那么就可以定义复合函数

    f(g(x))

此外，函数的复合还可以用 `Composition` 命令实现，用法如下：

    Composition[f, g, h, ... ] 表示函数 f、g、h、... 的复合. 

还有如下两个命令也可以用于函数的复合：

+ `Nest` 命令

        Nest[f,expr,n] 返回一个将 f 作用于 expr 上 n 次后得到的表达式.

+ `NestList` 命令

        NestList[f,expr,n] 将 f 作用于 expr 上 0 到 n 次，给出结果列表.

通过以上几种方式，就可以实现函数的复合。


Mathematica编程中的范围结构
------------------------------

Mathematica中，程序的范围结构可以由`Module`、`With`、`Block`这三种方式实现。

### Module

在Mathematica中，缺省情况下，所有对象（变量等）都是全局性的。通过模块可以定义局部变量，局部变量只在模块内有定义，在模块外面，同名对象可能就没有定义，或者具有完全不同的值。

通过以下语法定义模块：

+ Module[{x,y,...},expr] 

    指定在 expr 中出现的符号 x、y、... 应被当作局部值. 

+ Module[{x=x0, y=y0, ... expr] 

    用来定义 x, y, ... 的初始值. 

### With

With命令的用法如下：

+ With[{x=x0, y=y0, ...},expr] 

    指定在 expr 中出现的符号 x、y、... 应当由 x0、y0、... 替换. 

### Block

+ Block[{x,y,...,expr] 

    指定用符号 x、y、... 的局部值计算 expr. 

+ Block[{x=x0,...},expr] 

    给 x，... 赋初始局部值. 

### Block 与 Module的区别

+ `Module`: 使用词法作用域（局部化名称）
+ `Block`: 使用动态作用域（局部化值）
+ `DynamicModule`: 使用文档中的作用域。

以下关于`Block`和`Module`的区别，引述下面一段Mathematica文档中的陈述和示例：


> 块与模块的比较

> 当进行 Mathematica 编程时，应当尽量使它的项相互独立，这样程序就容易理解、维护和扩充. 

> 保证程序中不同相相互不影响的一个重要途径是给它的变量一定的"范围". Mathematica 用模块和块这两种机制来限制变量的范围. 

> 在实际编程时，模块远远比块常用，而在相互作用的计算中需要确定范围时，往往是块比较方便. 

    Module[vars,body]   词汇（lexical）定界
    Block[vars,body]    动态定界

> Mathematica 变量的定界机理. 

> 大部分计算机语言使用与 Mathematica 模块类似的词汇定界机理. 一些像LISP等符号计算语言与 Mathematica 块类似的动态定界机理. 

> 在使用词汇定界时，变量在一个程序中的一个代码段被作为局部变量. 在动态定界时，在程序执行历史的一部分被作为局部值. 

> 在 C 和 Java 等编译语言中， 它们的变量在使用之前就要声明类型，因此在编译前就已经确定了变量的类型；"代码"和"执行历史"之间的区分非常明显. 而 Mathematica 属于动态类型语言，它的符号特性使这个区别不明显，其原因是代码在程序的执行过程中可以动态地生成.

> Module[vars,body] 的作用是在模块作为 Mathematica 的代码被执行时处理表达式 body 的形式，当任何 vars 明显地出现在代码中时，就被当作局部变量. 

> Block[vars,body] 不注意表达式 body 的形式. 而是，在 body 的全局计算过程中使用 vars 的局部值. 

> 通过 i 来定义 m. 

    In[1]:= m = i^2

    Out[1]=  i^2

> 在块内 i+m 的计算过程中，i 用了局部值. 

    In[2]:= Block[{i = a}, i + m]

    Out[2]=  a + a^2

> 这里仅明显出现在 i+m 中的 i 被当作局部变量处理. 

    In[3]:= Module[{i = a}, i + m]

    Out[3]=  a + i^2



由上面一段文档，可以看出，`Module` 比 `Block` 对局部变量的屏蔽程度更高。


使用纯函数
-----------

如果必须对任何一种无论多小的运算所用的函数显式命名，往往将很不方便. 在 Mathematica 中，可以通过声明内联函数（称作纯函数）来避开这一问题.

纯函数的定义方法如下：

+ Function[x,body]

    纯函数中的 x 可用任何变量代替

+ Function[x1, x2, ... , body]

    多变量的纯函数

+ body `&`

    自变量为 # 或 #1、#2、#3 等的纯函数.

对于纯函数，可以不命名就使用：

    Function[{x, y}, x+y][3,4]
    7

使用一个`&`符号标记在末尾为简写符号。参数位置用`#1`、`#2`、`#3`等指定。

    g = (#1^#2) &;
    g[2, 3]
    8

纯函数不要求有单独的定义或名称，例如：

    (#1 ^ #2) & [3, 4]
    8

如果纯函数只有一个参数，那么可以直接使用`#`来指定该参数。

将函数应用于多个表达式/多个参数
-----------------------

可以使用`Map`命令来将一个函数应用于多个表达式，例如：

```mma
In:= Map[f, {a,b,c,d}]
Out= {f[a],f[b],f[c],f[d]}
```

此外，还可以使用`Map`命令的简写形式`/@`：

```mma
In:= f/@{a,b,c,d}
Out= {f[a],f[b],f[c],f[d]}
```

与将函数应用于多个表达式类似，还可以将函数应用于多个参数，使用`Apply`命令或者其简写形式`@@`：

```mma
In:= Apply[f, {a,b,c,d}]
Out= f[a,b,c,d]

In:= f@@{ {a,b},{c,d} }
Out= f[{a,b},{c,d}]
```

使用函数时除了使用`[]`以外，还可以使用`@`符号：

```mma
In:= f@a
Out= f[a]
```

函数的“部分应用”
---------------

WolframAlpha语言具有函数式编程的特性，Mathematica也支持函数的部分应用。如下例：

```
In:= Nearest[{1, 2, 3, 4, 5}, 2.7]
Out= {3}

In:= f = Nearest[{1, 2, 3, 4, 5}]
Out= NearestFunction[{5,1},<>]

In:= %[2.7]
Out= {3}

In:= f[2.7]
Out= {3}
```

Mathematica的函数与参数占位符
------------------------------

使用参数占位符，可以定义参数个数不定的函数。

占位符用法表如下：

| 占位符    | 含义                              |
|-----------|-----------------------------------|
| `_`       | 单一表达式                        |
| `x_`      | 名为 x 的表达式                   |
| `__`      | 一个或多个表达式序列              |
| `x__`     | 名为 x 的表达式列                 |
| `x__ h`   | 头部为 h 的表达式列               |
| `___`     | 零个或多个表达式序列              |
| `x___`    | 名为 x 的零个或多个表达式序列     |
| `x___ h`  | 头部为 h 的零个或多个表达式序列   |

**注意**: 使用三空位`___`来表示0个或多个表达式序列时，某些情况下很容易导致`___`反复与零元素进行匹配，导致死循环。因此，应当尽量避免使用三空位`___`。

Mathematica程序的控制流
------------------------

引自[Wolfram语言快速入门](http://www.wolfram.com/language/fast-introduction-for-programmers/procedures/)中的一句话：

> 在 Wolfram 语言中通常只需要小剂量的过程式编程.

>（如果你的大型程序中充满了 If、Do、Return 等，那你的程序可能需要改进.）

Mathematica中，通过`Return`、`Throw`、`Catch`等命令来实现对程序跳转和返回值等的控制。

### Return 命令

`Return` 命令的用法：

+ Return[expr] 

    从函数中返回 expr 的值.

+ Return[] 

    返回 Null 值. 

关于`Return`命令的用法，需要**注意**: `Return` 仅退出调用它的最内层结构。

### Throw和Catch

`Sow/Reap`和`Throw/Catch`是过程式编程中传递数据和控制的有效方式。`Throw`和`Catch`命令可以比Return 更明确地实现程序的控制流。命令的具体用法：

`Throw`命令：

+ Throw[value] 

    停止计算并将 value 作为最接近 Catch 的返回值.

+ Throw[value,tag] 

    仅由 Catch[expr,form] 返回，其中 form 是匹配 tag 的模式.

`Catch`命令：

+ Catch[expr] 

    返回在运行 expr 时产生的第一个 Throw 的参数.

+ Catch[expr,form] 

    返回 form 匹配 tag 的第一个 Throw[value,tag] 中的 value.

+ Catch[expr,form,f] 

    返回 f[value,tag]. 

TimeConstrained 和 Pause 命令
------------------------------

TimeConstrained 命令用来实现定时停止计算。

Puase 命令用来实现暂停。

TimeConstrained 的用法

+ TimeConstrained[expr,t] 

    计算 expr，在 t 秒后停止计算. 

+ TimeConstrained[expr,t,failexpr] 

    如果没有达到时间限制，返回 failexpr.

Puase 的用法：

+ Pause[n] 

    至少暂停 n 秒.

TimeConstrained的一个显著用途在于限制迭代时间。使得相关的计算能够在限定之间内给出某个不够精确的解。

关于Puase命令，应当**注意**：

+ Pause 仅在向下到至少`$TimeUnit`秒的间隔时是准确的. `$TimeUnit`的值可以通过在Mathematica的交互式命令行输入`$TimeUnit`查看。

        $TimeUnit: 给出您计算机系统上记录的以秒计的最小时间间隔.

+ 在执行Pause的过程中，过去的时间在SessionTime 和 AbsoluteTiming中统计，而不在TimeUsed或Timing中统计.

具体举例：

    $TimeUnit
    1/1000

    Timing[Pause[1]]
    {0., Null}
    AbsoluteTiming[Puase[1]]
    {1.000066, Null}

FullForm 命令
--------------

FullForm 命令用来查看变量的具体形式，包括类型、值等等。

例如：

    CharacterRange["a", "c"]
    {a, b, c}
    FullForm[%]
    List["a", "b", "c"]





