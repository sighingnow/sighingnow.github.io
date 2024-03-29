---
title: 正则表达式检测质数
author: Tao He
date: 2015-07-10
tags: [Regular Expression]
category: Programming Languages
layout: post
---

一个可以监测所有质数的正则表达式：

    ^1?$|^(11+?)\1+$

<!--more-->

这个正则表达式可以匹配所有不是质数的数（合数，以及0，1），模式如下图所示：

![]({{site.url}}/resource/regular_match_primes/pic1.png)

解释
----

这个正则表达式主要是通过将数值转化为字符`1`的个数来实现监测质数的功能的。此正则表达式分成
由"|"分割两部分: `/^1?$/`和`/^(11+?)\1+$/`。

前一部分匹配0或1，这两个数不是质数。

后一部分`(11+?)`表示至少有两个1，`(11+?)\1`中的`\1`表示对前面的匹配的引用。`\1+`表示前面的`(11+?)`出现至少一次，相当于`(11+?){2,}`。整个式子的含义为**至少出现两次(11+?)**。也就是说，所匹配的数可以写成两个大于2的整数的乘积。而`?`通过backtrace穷举所有的可能行，从而完成匹配所有的合数。

因此，两部分使用`|`连接，不匹配的就只能是质数了。

示例
----

通过此正则表达式输出1-100之间所有的质数(Python)：

~~~python
import re
print([i for i in range(1, 101) if not re.compile(r'^1?$|^(11+?)\1+$').match('1'*i)])
~~~

得到结果：

~~~python
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
~~~

参考
----

1. [A regular expression to check for prime numbers](http://www.noulakaz.net/2007/03/18/a-regular-expression-to-check-for-prime-numbers/).

