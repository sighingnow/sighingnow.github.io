---
title: Mathematica 求根与求最值
author: He Tao
date: 2015-03-23
tag: Mathematica
category: Math
layout: post
---

Mathematica具有强大的数值计算系统，可以用来进行求解方程的根、对函数求最值等数值计算操作。与求根和求最值的相关操作如下：

<!--more-->

对方程（组）求根
-----------------

#### 1. FindRoot

用法：

    FindRoot[f,{x,x_0}] 
        搜索 f 的一个数值根，初始值是 x=x_0.
    FindRoot[lhs==rhs,{x,x_0}] 
        搜索方程 lhs==rhs 的一个数值解.
    FindRoot[{f_1,f_2, ... },{ {x,x_0},{y,y_0},...}] 
        搜索所有 Subscript[f, i] 的一个同步数值解.
    FindRoot[eqn_1,eqn_2,...},{ {x,x_0},{y,y_0},...}] 
        搜索同步方程 eqn_i 的一个数值解.

`FindRoot`使用迭代搜索的方法来提供方程的数值近似解，`FindRoot`方法的参数`Method`可以用于指定数值求解的方法，缺省为Newton方法，此外，还有Brent方法和Secant方法。例如：

    FindRoot[x^5-2, {x, 1}, Method->"Brent"]

`FindRoot`所求的是哪个解依赖于选择的起点，只要起点充分靠近某个解，`FindRoot`总是返回这个解。如果要想使`FindRoot`在复数域内求解复数根，那么就必须给出一个复数初始值。

#### 2. Solve

用法：

    Solve[expr, vars]
        试图求解以 vars 为变量的方程组或不等式组 expr
    Solve[expr, vars, dom]
        在定义域dom上求解。
        dom的常用选择为Reals, Integers和Complexes。

`Solve`函数从理论上对方程（组）进行求解，给出结果是给出解的表达式而不是解的数值值。可以指定解的定义域，

#### 3. NSolve

用法：

    NSolve[expr, vars]
        试图求解以 vars 为变量的方程组或不等式组 expr 的解的数值近似。
    NSolve[expr, vars, Reals]
        在实数域内求解。

`NSolve`函数通过数值方法求解方程（组）的数值解。`NSolve`函数给出了多项式方程数值求解根的一般方法，而`FindRoot`函数给出了求解任意方程或方程组的数值解的方法。对于更一般的方程，求解数值解时需要用到`FindRoot`函数。

求解函数的极值
--------------

#### 1. FindMinimum

用法：

    FindMinimum[f,x] 
        搜索 f 的局部极小值，从一个自动选定的点开始.
    FindMinimum[f,{x, x_0}] 
        搜索 f 的局部最小值，初始值是 x=x_0.
    FindMinimum[f,{ {x, x_0},{y, y_0]}, ...}] 
        搜索多元函数的局部最小值.
    FindMinimum[{f,cons},{ {x, x_0},{y,y_0},...}] 
        搜索约束条件 cons 下局部最小值.
    FindMinimum[{f,cons},{x,y, ...}] 
        初始值在约束条件定义的区域内.

`FindMinimum`搜索求解函数在给定初值附近的最小值。

#### 2. FindMaximum

用法：

    FindMaximum[f,x] 
        搜索 f 的局部极大值，从一个自动选定的点开始.
    FindMaximum[f,{x, x_0}] 
        搜索 f 的局部最大值，初始值是 x=x_0.
    FindMaximum[f,{ {x, x_0},{y, y_0]}, ...}] 
        搜索多元函数的局部最大值.
    FindMaximum[{f,cons},{ {x, x_0},{y,y_0},...}] 
        搜索约束条件 cons 下局部最大值.
    FindMaximum[{f,cons},{x,y, ...}] 
        初始值在约束条件定义的区域内.

`FindMaximum`搜索求解函数在给定初值附近的最大值。

`FindMinimum`和`FindMaximum`函数的参数`Method`可以用来指定搜索最小值的方式，缺省为基于导数的方法。有些情形下，寻找区间最值（极值）可能无法通过求导的方式进行，此时，可以指定`Method`为Newton方法、ConjugateGradient方法或PrincipalAxis方法(不需要导数直接搜索)。

`FindMinimum`与`FindMaximum`从一个点开始，循序渐进地寻找极大（小）值。他们一旦找到一个极值，就会返回结果，这两个函数只能给出函数的局部极小值或极大值，而非函数的全局极大（小）值。

#### 3. NMinValue

用法：

    NMinValue[f,x] 
        给出关于 x 的 f 的最小值.
    NMinValue[f,{x,y,...}] 
        给出关于 x，y， \[Ellipsis] 的 f 的最小值.
    NMinValue[{f,cons},{x,y,...}] 
        给出在约束条件 cons 下 f 的最小值. 

#### 4. NMaxValue

用法：

    NMaxValue[f,x] 
        给出关于 x 的 f 的最大值.
    NMaxValue[f,{x,y,...}] 
        给出关于 x,y,... 的 f 的最大值.
    NMaxValue[{f,cons},{x,y,...}] 
        给出在约束条件 cons 下 f 的最大值. 

#### 5. Minimize
用法：

    Minimize[f,x] 
        得出以 x 为自变量的 f 的最小值.
    Minimize[f,{x,y,\[Ellipsis]}] 得出以 x，y，\[Ellipsis]为自变量的函数 f 的最小值.
    Minimize[{f,cons},{x,y,\[Ellipsis]}] 根据约束条件 cons，得出 f 的最小值.
    Minimize[{f,cons},{x,y,\[Ellipsis]},dom] 得出函数的最小值，函数含有域 dom 上的变量，典型的有 Reals 或 Integers. 
    
    Maximize[f,x] 得出函数 f 关于 x 的最大值.
    Maximize[f,{x,y,\[Ellipsis]}] 得出函数 f 关于 x，y ，\[Ellipsis] 的最大值.
    Maximize[{f,cons},{x,y,\[Ellipsis]}] 根据约束条件 cons，得出 f 的最大值.
    Maximize[{f,cons},{x,y,\[Ellipsis]},dom] 得出函数的最大值，函数含有域 dom 上的变量，一般有 Reals 和 Integers. 

#### 5. NMinimize

用法：

    NMinimize[f,x] 
        关于 x，最小化 f. 不含约束条件的全局最小值。
    NMinimize[f,{x,y,...}] 
        关于 x、y、...，最小化 f.
    NMinimize[{f,cons},{x,y,...}] 
        在约束条件 cons 下，最小化 f.

#### 6. NMaximize

用法：

    NMaximize[f,x] 
        关于 x 最大化 f. 不含约束条件的全局最大值。
    NMaximize[f,{x,y,...}] 
        关于 x,y,...，最大化 f.
    NMaximize[{f,cons},{x,y,...}] 
        在约束条件 cons 下，最大化 f.

`Minimize`与`Maximize`用于寻找函数的全局极大（小）值。`NMinValue`与`NMaxValue`是`Minimize`与`Maximize`的数值模拟，但他们可能找不到全局极大（小）值。当函数很平滑并且有有限个局部极大（小）值时，函数性能会更好。




