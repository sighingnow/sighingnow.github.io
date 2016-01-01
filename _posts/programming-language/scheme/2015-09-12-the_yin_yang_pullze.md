---
title: The yin-yang puzzle
author: He Tao
date: 2015-09-12
tag: [Scheme]
category: 编程语言
layout: post
---

所谓的yin-yang puzzle，指的是这样一段Scheme代码：

~~~scheme
(let* ((yin
         ((lambda (cc) (display #\@) cc) (call-with-current-continuation (lambda (c) c))))
       (yang
         ((lambda (cc) (display #\*) cc) (call-with-current-continuation (lambda (c) c)))) )
    (yin yang))
~~~

执行这段代码的输出：

    @*@**@***@****@*****@******@*******@********@********* ...

<!--more-->