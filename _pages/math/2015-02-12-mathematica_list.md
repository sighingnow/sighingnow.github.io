---
title: Mathematica中的列表(List)
author: He Tao
date: 2015-02-12
tag: Mathematica
category: Math
layout: post
---

List是Mathematica中的一种重要的数据结构。

** 在Mathematica中使用`List`时必须注意：Mathematica的`List`的索引是从`1`开始的，不是`0`! **

List的表示
-----------

List用`{}`来表示。如：

    {1, 2, 3, 4}

List的生成
-----------

Mathematica中，有以下三种方式来生成List。

1. Range 命令

```mma
Range[Subscript[i, max]] 
    生成列表 {1,2,\[Ellipsis],Subscript[i, max]}.
Range[Subscript[i, min],Subscript[i, max]] 
    生成列表 {Subscript[i, min],\[Ellipsis],Subscript[i, max]}.
Range[Subscript[i, min],Subscript[i, max],di] 
    使用步长 di 生成列表. 
```

<!--more-->

2. Table 命令

```mma
Table[expr,{Subscript[i, max]}] 
    产生一个 expr 的 Subscript[i, max] 拷贝的列表.
Table[expr,{i,Subscript[i, max]}] 
    产生i  从1到 Subscript[i, max] 的一个 expr 的值的列表.
Table[expr,{i,Subscript[i, min],Subscript[i, max]}] 
    以 i=Subscript[i, min] 开始.
Table[expr,{i,Subscript[i, min],Subscript[i, max],di}] 
    使用步长 di. 
Table[expr,{i,{Subscript[i, 1],Subscript[i, 2],\[Ellipsis]}}] 
    使用连续值 Subscript[i, 1], Subscript[i, 2], \[Ellipsis]. 
Table[expr,{i,Subscript[i, min],Subscript[i, max]},{j,Subscript[j, min],Subscript[j, max]},\[Ellipsis]] 
    给出一个嵌套列表. 和 i 相关联的列表是最外的列表.  
```

3. Array 命令

```mma
Array[f,n] 
    生成长度为 n、元素为 f[i] 的列表. 
Array[f,n,r] 
    生成使用索引原点 r 的列表.
Array[f,n,{a,b}] 
    生成使用 n 个从 a 到 b 的数值组成的列表.
Array[f,{Subscript[n, 1],Subscript[n, 2],\[Ellipsis]}] 
    生成嵌套列表的 Subscript[n, 1]*Subscript[n, 2]*\[Ellipsis] 数组，元素为 f[Subscript[i, 1],Subscript[i, 2],\[Ellipsis]]. 
Array[f,{Subscript[n, 1],Subscript[n, 2],\[Ellipsis]},{Subscript[r, 1],Subscript[r, 2],\[Ellipsis]}] 
    生成一个列表，该列表使用指标起点 Subscript[r, i],(缺省为 1). 
Array[f,{Subscript[n, 1],Subscript[n, 2],\[Ellipsis]},{ {Subscript[a, 1],Subscript[b, 1]},{Subscript[a, 2],Subscript[b, 2]},\[Ellipsis]}] 
    生成使用 Subscript[n, i] 个从 Subscript[a, i] 到 Subscript[b, i] 的数值组成的列表.
Array[f,dims,origin,h] 
    对数组的每一层使用头部 h，而不是 List. 
```

字符型List的生成
------------------

生成字符型的List，有如下两种方式：

1. Characters 命令

```mma
Characters["string"] 
    给出了字符串中字符的列表. 
```

2. CharacterRange 命令

```mma
CharacterRange[Subscript[c, 1],Subscript[c, 2]] 
    产生 "Subscript[c, 1]" 到 "Subscript[c, 2]" 范围之内的字符列表. 
```

    具体的字符顺序为起始字符之间 ASCII 码表中的字符顺序。

    例如：

```mma
CharacterRange["a", "c"]
{a, b, c}
FullForm[%]
List["a", "b", "c"]  
```

List的运算
------------

可以对 List 进行加、减、乘、除、阶乘、乘方等运算，具体运算过程为对列表进行逐项计算。

例如：

```mma
2 ^ Range[10]
{2, 4, 8, 16, 32, 64, 128, 256, 512, 1024}
Prime[Range[10]]
{2, 3, 5, 7, 11, 13, 17, 19, 23, 29}
```

List取得指定位置元素
------------------------

Mathematica中，用`[[]]`符号来根据索引取得列表的值。在使用列表索引时，应当注意一下两个问题：

1. 列表的索引从**1**开始。
2. 列表可以逆向索引，也就是说，最后一项的索引可以为`-1`。

List的线性操作
--------------

在Mathematica中，很多线性操作都可以直接用于列表。例如：

    {a,b,c}+{d,e,f}

的结果为

    {a+d,b+e,c+f}

List的操作
-----------

+ List的列表切片操作

    List使用`;;`运算符可以实现切片操作，例如：

        {1,2,3,4,5,6}[[2;;4]]

    的结果为：

        {2,3,4}

+ Length[lst] 

    求出列表lst的长度（lst中的元素个数）。

+ First[lst] 

    得到lst中的第一个位置的元素。

    如果lst的长度为0（没有元素），会得到如下输出：

```mma
    First::first: "{} 长度为零，并且没有第一个元素."
```

+ Last[lst]

    得到lst中的最后一个位置的元素。

    如果lst的长度为0（没有元素），会得到如下输出：

```mma
    Last::nolast: "{} 的长度为零，并且没有最后一个元素".
```

+ Part[lst, k] 或 lst[[k]]

    返回lst中的第`k`个元素。如果lst中没有第`k`个元素，会得到如下输出：

```mma
    Part::partw: {} 的部分 1 不存在.
```

+ Part[lst, -k] 或 lst[[-k]]

    返回lst中的倒数第`k`个元素。如果lst中没有倒数第`k`个元素，会得到如下输出：

```mma
    Part::partw: "{} 的部分 -1 不存在.
```

+ Rest[lst] 

    返回删除`lst`中第一个对象后的结果。

+ Take 命令

    Take 命令用于从 List 中取得一部分元素，返回一个新的列表。

+ Delete 命令

    Delete 命令用于从列表中删除元素。根据索引的正负来删除整数第`k`个元素或者第`-k`个元素。

+ Drop 命令

    Drop命令和Take命令的格式基本一致，用于从列表中删除多个元素（连续的）。

+ Append 命令

    Append命令用于在原来的列表末尾追加元素。

+ Prepend 命令

    Prepend命令用于在原来的列表开头处追加元素。

+ Insert 命令

    Insert 命令用于将元素插入到列表中，位置索引既可以为正值也可以为负值。

+ ReplacePart 命令

    ReplacePart 命令用于替换List某个位置的元素。

+ Sort 命令

    Sort命令用于对列表进行排序，具体标准顺序的定义如下例：

```mma
    a = {1, 2, "a", "b", "c", "A", "B", "C"}
    {1, 2, "a", "b", "c", "A", "B", "C"}
    Sort[a]
    {1, 2, "a", "A", "b", "B", "c", "C"}
```

    Mathematica中标准顺序的定义同其他编程语言有所不同，需格外注意。

+ Reverse 命令

    Reverse命令用于将列表逆序排序。

    **注意**：`Sort` 命令和 `Reverse` 命令并不改变原来的列表，只是返回一个排好序的列表。

+ RotateRight 和 RotateLeft 命令

    RotateRight 和 RotateLeft 命令用于对列表进行循环旋转操作。可以指定循环旋转的距离。

+ Join 和 Union 命令

    Join 命令和 Union 命令用于列表的合并。区别在于 Join 命令仅仅合并列表，并不去除重复的元素。而 Union 命令则采取的是集合的并集操作方式，重复的元素仅仅保留一个。

+ Depth 命令

    Depth命令可以得到多层嵌套列表的嵌套层数。**注意** Depth命令得到的值为列表的嵌套层数加 1。 

+ IntegerDigits 命令

    IntegerDigits 命令可以得到一个整数各个位上的数字组成的列表。

    类似，RealDigits 命令可以得到一个实数（有限位近似值）的各个位上的数字组成的列表。并且能够得到小数点左边的数字位数。例如:

```mma
    RealDigits[N[Pi, 10]]
    { {3, 1, 4, 1, 5, 9, 2, 6, 5, 3}, 1}
```

    上述结果中包含两个子列表，其中，第一个子列表为各个位上的数字组成的列表，第二个子列表的值为小数点左侧的数字的位数。

List与集合
------------

集合在Mathematica中是用列表表示的。集合的基本操作：

+ Union[lst1, lst2]

    求并集

+ Intersection

    求交集

+ Complement[universe, lst]

    求全集

+ Subsets[lst]

    返回一个集合的所有子集构成的列表（幂集）

+ KSubsets[lst]

    返回lst的所有包含K个元素的子集构成的列表。

List 与矩阵(Matrix)
----------------------

Mathematica中，用多层List来表示矩阵。设计矩阵的常用运算有：

+ MatrixForm

    以矩阵的形式显示多层List。

+ IdentityMatrix[n]

    返回一个`n`阶单位阵。

+ DiagonalMatrix[lst]

    返回一个主对角线值为`lst`的元素、其他元素为`0`的矩阵。





