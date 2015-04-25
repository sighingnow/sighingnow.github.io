---
title: C和C++中的回调函数
author: He Tao
date: 2015-02-02
tag: C/C++
category: 编程语言
layout: post
---

回调函数(Callback Functions)
----------------------------

回调函数是指一个通过函数指针调用的函数。回掉函数不是由该函数的实现方法直接调用，而是在特定时间或条件发生时由另一方调用的，用于对该事件或条件进行响应。

回调函数的简单实现
------------------

C和C++中，可以通过函数指针的方式实现回调函数。如下例：

<!--more-->

```cpp
#include <stdio.h>

typedef int (*callf)(int a);

/* 回调函数 */
int fa(int a) {
    printf("a is called, a is %d\n", a);
    return a;
}

/* 回调函数 */
int fb(int b) {
    printf("b is called, b is %d\n", b);
    return b;
}

int call1(callf pcf, int arg) { /* 通过typedef函数指针的方式 */
    printf("call1 is called, arg is %d\n", arg);
    pcf(arg); // 调用回调函数
    return arg;
}

int call2(int (*ptr)(int ), int arg) { /* 通过直接定义函数指针的方式 */
    printf("call2 is called, arg is %d\n", arg);
    ptr(arg); // 调用回调函数
    return arg;
}

int main(int argc, char *argv[])
{
    int arg = 10;
    call1(&fa, arg);
    call2(&fb, arg);
    return 0;
}
```

在上例中，我们可以看到，在C/C++中可以直接将函数指针作为参数传参，并通过函数指针进行函数调用和传递参数，从而实现了回调函数。

回调函数与事件模型
------------------

首先声明时间模型、回调函数和事件注册函数。

```cpp
// 事件模型声明
struct Event;
// 回调函数声明
typedef void (*pEvent_cbF)(const struct Event *e, void *data);
// 事件注册函数声明
void event_cbF_register(pEvent_cbF pf, void *data);
```

在事件调度器(event dispather)中，通常将回调函数放在结构体中。

```cpp
struct Event {
    pEvent_cbF callback;
    void *data;
};
```

接下来，实现事件注册函数和回掉函数。

```
void event_cbF_register(Event *e, pEvent_cbF callback, void *data) {
    e->callback = callback;
    e->data = data;
}
void my_event_cbF(const struct Event *e, void *data) {
    printf("my event callback function is called.\n");
    printf("data: %s\n", (const char *)data);
    printf("event data: %s\n", (const char *)(e->data));
}

```

在main函数中测试：

```cpp
int main(int argc, char *argv[])
{
    Event curtom_event;
    char custom_data[15] = "Event occur !";
    event_cbF_register(&curtom_event, &my_event_cbF, custom_data);
    curtom_event.callback(&curtom_event, curtom_event.data);

    return 0;
}
```

运行上述程序，将获得如下输出：

    ht@debian ~
    $ ./callback
    my event callback function is called.
    data: Event occur !
    event data: Event occur !

回调函数与异步非阻塞
---------------------

将回调函数放在单独的线程中执行，便可以做到异步非阻塞处理。




