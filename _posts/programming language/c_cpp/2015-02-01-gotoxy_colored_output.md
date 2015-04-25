---
title: C语言控制台程序光标位置与文字颜色
author: He Tao
date: 2015-02-01
tag: C/C++
category: 编程语言
layout: post
---

Windows平台
-----------

### 控制光标定位

Windows环境下可以通过调用WIN32 API来实现光标定位，具体实现如下:

```cpp
#include <windows.h>
void gotoxy(int x, int y) {
    COORD cursorPosition;
    cursorPosition.X = x;
    cursorPosition.Y = y;
    // COORD cursorPosition = {x, y};
    SetConsoleCursorPosition(GetStdHandle(STD_OUTPUT_HANDLE), cursorPosition);
}
```

**注意**：X, Y 的值都是从 `0` 开始的。

<!--more-->

### 隐藏光标

有了光标定位的基础，只需要将光标定位于控制台之外的为，便可以实现隐藏光标，例如，通过上文中实现的`gotoxy`函数将光标定位于`(-1, -1)` 位置便可以实现隐藏光标。

### 清屏

Windows环境下可以通过执行cls命令来实现清屏，具体实现如下：

```cpp
#include <windows.h>
void clear() {
    system("cls");
}
```

### color命令

在cmd命令行中执行 `color /?`，可以得到color命令的用法如下：

    设置默认的控制台前景和背景颜色。
    COLOR [attr]
        attr        指定控制台输出的颜色属性
    颜色属性由两个十六进制数字指定 -- 第一个为背景，第二个则为
    前景。每个数字可以为以下任何值之一:

        0 = 黑色       8 = 灰色
        1 = 蓝色       9 = 淡蓝色
        2 = 绿色       A = 淡绿色
        3 = 浅绿色     B = 淡浅绿色
        4 = 红色       C = 淡红色
        5 = 紫色       D = 淡紫色
        6 = 黄色       E = 淡黄色
        7 = 白色       F = 亮白色

    如果没有给定任何参数，该命令会将颜色还原到 CMD.EXE 启动时
    的颜色。这个值来自当前控制台窗口、/T 命令行开关或 
    DefaultColor 注册表值。

    如果用相同的前景和背景颜色来执行 COLOR 命令，COLOR 命令
    会将 ERRORLEVEL 设置为 1。

    例如: "COLOR fc" 在亮白色上产生亮红色

程序中，也可以通过 `system("color ... ")` 调用color命令来实现改变控制台颜色的功能。


### 彩色输出

在Windows环境下，同样是通过调用WIN32 API来实现控制台程序的彩色输出。具体实现如下：

```cpp
#include <windows.h>
void settextcolor(int color)
{
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), color);
}
```

其中，color的值定义在 wincon.h 中。多个颜色值可以综合在一起使用。具体颜色定义如下:

```cpp
#define FOREGROUND_BLUE	1
#define FOREGROUND_GREEN	2
#define FOREGROUND_RED	4
#define FOREGROUND_INTENSITY	8
#define BACKGROUND_BLUE	16
#define BACKGROUND_GREEN	32
#define BACKGROUND_RED	64
#define BACKGROUND_INTENSITY	128
```

多个值综合使用示例：(本质上为多个值得或运算)

```cpp
SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 
        FOREGROUND_GREEN|FOREGROUND_RED|FOREGROUND_INTENSITY);
```

便可以在控制台将输出字符的颜色设置为黄色。

Linux 平台
----------

### 控制光标定位

Linux环境下，控制台会解释ANSI转义序列，转义符是ESC，ASCII码为0x1b，因此，可以通过在中断输出转义序列来实现光标的定位，具体序列如下：

    Esc[Line;Columnf

或

    Esc[Line;ColumnH

应用举例：

```cpp
void gotoxy(int x, int y) {
    printf("%c[%d;%df", 0x1b, y, x);
    // printf("%c[%d;%dH", 0x1b, y, x);
}
```

**注意**: 光标位置的行和列都是从 `0` 开始索引。

转义序列列表：

+ Esc[PnA 光标向上移动Pn行，如果光标已经处于最上方(the top line)，则忽略该转义序列。
+ Esc[PnB 光标向下移动Pn行，如果光标已经处于最下方(the buttom line)，则忽略该转义序列。
+ Esc[PnC 光标向右移动Pn行，如果光标已经处于最右处(the rightmost column)，则忽略该转义序列。
+ Esc[PnD 光标向左移动Pn行，如果光标已经处于最左处(the leftmost column)，则忽略该转义序列。
+ Esc[s 保存当前光标位置，之后可以通过恢复光标位置的转义序列回到被保存的光标位置。
+ Esc[u 恢复光标位置，将光标置于 Esc[s 序列保存的光标位置。
+ Esc[2J 清屏，将光标置于(0, 0)处(第一行第一列)。
+ Esc[K 删除从光标所在位置至当前行行末的所有字符，包括当前光标所在位置处的字符。
+ Esc[?25l 隐藏光标。
+ Esc[?25h 显示光标。

### 设置输出字符颜色

同样，也可以通过输出控制符来控制输出字符的颜色，用法如下：

+ Esc[Ps;Psm 设置图形模式(Set Graphics Mode)。第一个Ps的值为背景色的值，第二个Ps的值为前景色的值。

背景色的颜色值范围为`40~49`，前景色的颜色值范围为`30-39`，具体颜色值如下：

| Color      | Background | Foreground | 
|:----------:|:----------:|:----------:|
| black      | 40         | 30         |
| red        | 41         | 31         |
| green      | 42         | 32         |
| yellow     | 43         | 33         |
| blue       | 44         | 34         |
| purple     | 45         | 35         |
| deep green | 46         | 36         |
| white      | 47         | 37         |

### 其他转义序列控制

+ Esc[0m 关闭所有属性。
+ Esc[1m 设置高亮度。
+ Esc[4m 下划线。
+ Esc[5m 闪烁。
+ Esc[7m 反显。
+ Esc[8m 消隐。








