---
title: Javascript 变量作用域与闭包
author: He Tao
date: 2015-02-18
tag: Javascript
category: 编程语言
layout: post
---

Javascript 中的变量
--------------------

Javascript 中的变量可分为全局变量和局部变量两种。

Javascript 中使用 `var` 语句声明变量。例如：

```javascript
var x = 10;
var y = ["a", 20, true]
```

变量相等性比较
---------------

Javascript 中，比较两个变量是否想等是，必须注意 `==`和`===` 之间的区别。

`==`只比较变量的值，而 `===`既比较变量的值，又比较变量的类型。如下例：

<!--more-->

```javascript
console.log(5 == "5");
console.log(5 === "5");
```

得到的输出如下：

    true
    false

变量生命期
------------

Javascript 中的变量在声明时初始化。

+ 局部变量在函数执行完毕后销毁。
+ 全局变量在页面关闭后销毁。

函数参数变量
-------------

函数的参数只在函数内起作用，是局部变量。

变量作用域
------------

### 全局变量与局部变量

函数内部可以访问全局变量，但在函数外部无法访问函数的局部变量。

局部变量会在局部作用域中覆盖全局变量。

Javascript 中，`function`中内容相对于外部的内容形成新的作用域(scope)。

在同一作用域中，同名变量可以重复声明。重复声明的作用相当于赋值操作。例如：

```javascript
(function() {
    var a = 10;
    console.log(a);
    var a = 20;
    console.log(a);
    var a;
    console.log(a);
})();
```

运行后得到：

    10
    20
    20

Javascript 中，如果不用`var`声明变量，则该变量为全局变量。如：

```javascript
var n = 100;
(function(){
    console.log(n);
    n=200;
    console.log(n);
})();
```

得到输出：

    100
    200

在函数体内部，对于每一个变量，Javascript 会首先在函数体内寻找是否用`var`语句声明了局部变量，如果有（**无论是在该变量使用前声明还是使用后声明**），都会认为该变量是局部变量。如果找不到，在逐层向外寻找该变量(一级一级地向上寻找父对象的变量)。例如下面两段代码：

```javascript
var n = 100;
(function(){
    console.log(n); 
    return n;
})();
```

得到输出：

    100

而代码：

```javascript
var n = 100;
(function(){
    console.log(n); 
    return n; 
    var n;
})();

// 注意 var 语句在 return 语句之后！！！
```

运行后得到如下输出：

    undefined

如果写成这样：

```javascript
var n = 100;
(function(){
    n = 200; 
    console.log(n); 
    return n; 
    var n;
})();
console.log(n);
```

得到输出：

    200
    100

这便是 Javascript 的作用域的运行机制。

从外部读取局部变量
--------------------

我们知道，在Javascript中，无法直接从外部读取局部变量，但是，Javascript函数(Function对象)的子函数(子对象)是可以读取函数的局部变量的。因此，通过如下方法可以从外部读取和操作局部变量。

```javascript
function func() {
    var innerVar = 100;

    function setInner(value) {
        innerVar = value;
    }

    return setInner;
}
```



闭包
------

当内部函数在定义它的作用域外部被引用时，就会创建该内部函数的闭包。如果内部函数引用了位于外部函数的变量，当外部函数调用完毕后，这些闭包中的变量不会被释放，会保持在内存中。

闭包对应函数示例，当函数示例销毁时才会销毁闭包，并释放其占用的内存。




