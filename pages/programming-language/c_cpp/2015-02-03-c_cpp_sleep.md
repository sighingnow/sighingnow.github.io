---
title: C和C++中的sleep函数
author: He Tao
date: 2015-02-03
tag: C/C++
category: 编程语言
layout: post
---

`sleep`这一功能在不同的编译器中实现不同，API也有差别。

GCC编译器的实现
---------------

GCC编译器中，sleep定义在`<unistd.h>`中。函数原型：

```cpp
/* The sleep() function is, perhaps, the most commonly used of all the
 * process/thread suspension APIs; it provides support for specification
 * of suspension periods ranging from 1 second to ~136 years.  (However,
 * POSIX recommends limiting the maximum period to 65535 seconds, to
 * maintain portability to platforms with only 16-bit ints).
 */
unsigned _cdecl __MINGW_NOTHROW sleep( unsigned );
```

注意，在GCC的实现中，`sleep`的单位为秒(second)。

<!--more-->

VC编译器的实现
---------------

VC编译器中，Sleep定义在`<windows.h>`中(具体定义在`<winbase.h>`中)，函数原型为：

```cpp
WINBASEAPI void WINAPI Sleep(DWORD);
```

注意，在VC编译器的实现中，`Sleep`的首字母"S"要**大写**，并且其时间单位为毫秒(millisecond)。

MinGW
------

在MinGW中，如果安装了win32api，则既可以使用`sleep()`，也可以使用`Sleep()`。

C++11
-----

在C++11中，可以用`this_thread`命名空间下的`sleep_for`函数来实现线程休眠的功能。具体函数定义如下：

```cpp
template< class Rep, class Period >
void sleep_for( const std::chrono::duration<Rep, Period>& sleep_duration );
```

使用示例：

```cpp
#include <iostream>
#include <chrono>
#include <thread>

void sleep(int milliseconds) {
    std::chrono::milliseconds dura(milliseconds);
    std::this_thread::sleep_for(dura);
} 

int main()
{
    std::cout << "test sleep_for in c++11..." << std::endl;
    sleep(2000);
    std::cout << "sleep for 2000 ms\n";
}
```


