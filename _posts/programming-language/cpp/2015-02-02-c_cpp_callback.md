---
title: C和C++中的回调函数
author: Tao He
date: 2015-02-02
tag: C/C++
category: Programming Languages
layout: post
---

回调函数(Callback Functions)
----------------------------

回调函数是指一个通过函数指针调用的函数。回掉函数不是由该函数的实现方法直接调用，而是在特定时间或条件发生时由另一方调用的，用于对该事件或条件进行响应。

回调函数的简单实现
------------------

C和C++中，可以通过函数指针的方式实现回调函数。如下例：

<!--more-->

~~~cpp
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
~~~

在上例中，我们可以看到，在C/C++中可以直接将函数指针作为参数传参，并通过函数指针进行函数调用和
传递参数，从而实现了回调函数。

回调函数与事件模型
---------------

首先声明时间模型、回调函数和事件注册函数。

~~~cpp
// 事件模型声明
struct Event;
// 回调函数声明
typedef void (*pEvent_cbF)(const struct Event *e, void *extra_data);
// 事件注册函数声明
void event_cbF_register(Event *e, pEvent_cbF callback, void *data);
~~~

在事件调度器(event dispather)中，通常将回调函数放在结构体中。`trigger`函数供外部调用来触发
事件，当时间发生时，会调用回调函数。

~~~cpp
struct Event {
    pEvent_cbF callback;
    void *data;
    void trigger() {
        printf("Event occur!\n");
        this->callback(this, nullptr);
    }
    void trigger(void *extra_data) {
        printf("Event occur with extra data: %s\n", (const char *)extra_data);
        this->callback(this, extra_data);
    }
};
~~~

接下来，实现事件注册函数和回掉函数。

~~~cpp
void event_cbF_register(Event *e, pEvent_cbF callback, void *data) {
    e->callback = callback;
    e->data = data;
}

void my_event_cbF(const struct Event *e, void *extra_data) {
    printf("my event callback function is called.\n");
    printf("event data: %s\n", (const char *)(e->data));
    if (extra_data != nullptr) {
        printf("extra data: %s\n", (const char *)(extra_data));
    }
}
~~~

在 main 函数中创建事件，并通过 `trigger` 触发。测试：

~~~cpp
int main(int argc, char *argv[])
{
    Event custom_event;
    char custom_data[50] = "Event message of custom_event.";
    event_cbF_register(&custom_event, &my_event_cbF, custom_data);
    custom_event.trigger();
    
    printf("\n");
    char extra_data[50] = "Extra data when trigger custom_event.";
    custom_event.trigger(extra_data);

    return 0;
}
~~~

运行上述程序，将获得如下输出：

~~~
    Event occur!
    my event callback function is called.
    event data: Event message of custom_event.

    Event occur with extra data: Extra data when trigger custom_event.
    my event callback function is called.
    event data: Event message of custom_event.
    extra data: Extra data when trigger custom_event.
~~~

回调函数与异步非阻塞
---------------------

将回调函数放在单独的线程中执行，便可以做到异步非阻塞处理。




