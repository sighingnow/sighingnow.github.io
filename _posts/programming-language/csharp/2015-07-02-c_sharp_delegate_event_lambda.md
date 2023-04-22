---
title: C# 委托、事件与Lambda表达式
author: Tao He
date: 2015-07-02
tags: [CSharp]
category: Programming Languages
layout: post
---

在.NET中，在大部分时间里都没有指针的身影，因为指针被封闭在内部函数当中。可是回调函数却依然存在，它是以委托的方式来完成的。委托可以被视为一个更高级的指针，它不仅仅能把地址指向另一个函数，而且还能传递参数，返回值等多个信息。系统还为委托对象自动生成了同步、异步的调用方式。

<!--more-->

C#委托与函数指针
--------------

委托是安全封装方法的类型，类似于 C 和 C++ 中的函数指针。

~~~csharp
class Program
{
    delegate void pFunc(int x);
    static void Main(string[] args)
    {
        pFunc f = delegate(int x)
        {
            Console.WriteLine(x * 2);
        };
        pFunc g = test;
        f(20); // f.Invoke(20);
        g(100); // g.Invoke(100);
        Console.ReadKey();
    }
    static void test(int x) 
    {
        Console.WriteLine("test {0}", x);
    }
}
~~~

C#中的委托类型
------------

委托类型派生自 .NET Framework 中的 Delegate 类。 委托类型是封装的，它们不能派生出其他类，也不能从 Delegate 派生出自定义类。 

由于实例化的委托是一个对象，因此可以作为参数传递或分配给一个属性。 这允许方法作为参数接受委托并在稍后调用委托。 这被称为异步回调，是在长进程完成时通知调用方的常用方法。 当以这种方式使用委托时，使用委托的代码不需要知道要使用的实现方法。 功能类似于封装接口提供的功能。

委托作为参数的一个例子：在快速排序函数中使用一个cmp函数作为参数来决定排序的方式。通过委托调用方法时既可以直接调用也可以通过delegate的Invoke方法调用。


匿名委托
-------

通过使用匿名方法，由于不必创建单独的方法，因此减少了实例化委托所需的编码系统开销。

例如，如果创建方法所需的系统开销是不必要的，则指定代码块（而不是委托）可能非常有用。 启动新线程即是一个很好的示例。 无需为委托创建更多方法，线程类即可创建一个线程并且包含该线程执行的代码。

~~~csharp
void StartThread() 
{ 
    System.Threading.Thread t = new System.Threading.Thread (delegate() 
    {
        System.Console.Write("new Thread is running."); 
    }); 
    t.Start(); 
}
~~~

多播委托(MulticastDelegate)
-------------------------

一次委托调用多个方法，通过+和-运算符实现多播的增加或减少。多播委托包含已分配委托的列表。在调用多播委托时，它会按顺序调用列表中的委托。只能合并相同类型的委托。一个多播委托的例子：

~~~csharp
using System;

namespace SharpWork
{
    class Program
    {
        delegate void funcdg(int arg);
        public static void Main(string[] args)
        {
            Console.WriteLine("Hello World!");
            
            funcdg f = delegate(int arg)
            {
                Console.WriteLine(arg);
            };
            
            Program prog = new Program();
            
            f += prog.Show10;
            f += prog.Show100;
            f += prog.Show1000;
            
            f.Invoke(1);

            Console.ReadKey(true);
        }
        
        void Show10(int arg) { Console.WriteLine(arg+10); }
        void Show100(int arg) { Console.WriteLine(arg+100); }
        void Show1000(int arg) { Console.WriteLine(arg+1000); }
    }
}
~~~

委托、事件与Observer模式
----------------------

Observer设计模式是为了定义对象间的一种一对多的依赖关系，以便于当一个对象的状态改变时，其他依赖于它的对象会被自动告知并更新。Observer模式是一种松耦合的设计模式。Observer设计模式中主要包括如下两类对象：

1. Subject：监视对象，它往往包含着其他对象所感兴趣的内容。会不断把数据发给监视它的对象。
2. Observer：监视者，它监视Subject，当Subject中的某件事发生的时候，会告知Observer，而Observer则会采取相应的行动。

假设我们有个高档的热水器，我们给它通上电，当水温超过95度的时候：
1. 扬声器会开始发出语音，告诉你水的温度；
2. 液晶屏也会改变水温的显示，来提示水已经快烧开了。

现在需要写个程序来模拟这个烧水的过程。在本范例中，热水器就是一个监视对象，它包含的其他对象所感兴趣的内容，就是 temprature字段，当这个字段的值快到100时，会不断把数据发给监视它的对象。Observer有警报器和显示器，它们采取的行动分别是发出警报和显示水温。

在本例中，事情发生的顺序应该是这样的：

+ 警报器和显示器告诉热水器，它对它的温度比较感兴趣(注册)。
+ 热水器知道后保留对警报器和显示器的引用。
+ 热水器进行烧水这一动作，当水温超过95度时，通过对警报器和显示器的引用，自动调用警报器的MakeAlert()方法、显示器的 ShowMsg()方法。

具体实现代码：

~~~csharp
/*
 * Created by SharpDevelop.
 * User: He Tao
 * Date: 2015/7/2
 * Time: 15:13
 * 
 * To change this template use Tools | Options | Coding | Edit Standard Headers.
 */
using System;
using System.Threading;

namespace Delegate
{
    // 热水器
    public class Heater
    {
        private int temperature;
        //声明委托
        public delegate void BoilHandler(int param);
        //声明事件
        public event BoilHandler BoilEvent;
        // 烧水
        public void BoilWater()
        {
            for (int i = 0; i <= 100; i++) {
                temperature = i;
                if (temperature > 95) {
                    if (BoilEvent != null) {       //如果有对象注册
                        BoilEvent(temperature);       //调用所有注册对象的方法
                    } 
                }
            }
        }
    }
    // 警报器
    public class Alarm
    {
        public void MakeAlert(int param)
        {
            Console.WriteLine("Alarm：嘀嘀嘀，水已经 {0} 度了：", param);
        }
    }
    // 显示器
    public class Display
    {
        public static void ShowMsg(int param)
        {       //静态方法
            Console.WriteLine("Display：水快烧开了，当前温度：{0}度。", param);
        }
    }
      
    class Program
    {
        static void Main()
        {
            Heater heater = new Heater();
            Alarm alarm = new Alarm();
            heater.BoilEvent += alarm.MakeAlert;       //注册方法
            // heater.BoilEvent += (new Alarm()).MakeAlert;       //给匿名对象注册方法
            heater.BoilEvent += Display.ShowMsg;              //注册静态方法 .
            heater.BoilWater();       //烧水，会自动调用注册过对象的方法
            
            Console.ReadKey();
        }
    }
}
~~~

.Net Framework中的委托与事件
--------------------------

.Net Framework的编码规范：

+ 委托类型的名称都应该以EventHandler结束。
+ 委托的原型定义：有一个void返回值，并接受两个输入参数：一个Object 类型，一个 EventArgs类型(或继承自EventArgs)。
+ 事件的命名为 委托去掉 EventHandler之后剩余的部分。
+ 继承自EventArgs的类型应该以EventArgs结尾。

按照这一个规范，将上述代码改下为如下形式：

~~~csharp
/*
 * Created by SharpDevelop.
 * User: He Tao
 * Date: 2015/7/2
 * Time: 15:13
 * 
 * To change this template use Tools | Options | Coding | Edit Standard Headers.
 */
using System;

namespace Delegate
{
    // 热水器
    public class Heater
    {
        private int temperature;
        public string type = "RealFire 001"; // 添加型号作为演示
        public string area = "China Xian"; // 添加产地作为演示
        //声明委托
        public delegate void BoiledEventHandler(Object sender, BoliedEventArgs e);
        public event BoiledEventHandler Boiled;
        //声明事件
        // 定义BoliedEventArgs类，传递给Observer所感兴趣的信息
        public class BoliedEventArgs : EventArgs
        {
            public readonly int temperature;
            public BoliedEventArgs(int temperature)
            {
                this.temperature = temperature;
            }
        }
        // 可以供继承自 Heater 的类重写，以便继承类拒绝其他对象对它的监视
        protected virtual void OnBolied(BoliedEventArgs e)
        {
            if (Boiled != null) {       // 如果有对象注册
                Boiled(this, e);       // 调用所有注册对象的方法
            }
        }
              
        // 烧水。
        public void BoilWater()
        {
            for (int i = 0; i <= 100; i++) {
                temperature = i;
                if (temperature > 95) {
                    //建立BoliedEventArgs 对象。
                    BoliedEventArgs e = new BoliedEventArgs(temperature);
                    OnBolied(e);       // 调用 OnBolied方法
                }
            }
        }
    }
    // 警报器
    public class Alarm
    {
        public void MakeAlert(Object sender, Heater.BoliedEventArgs e)
        {
            Heater heater = (Heater)sender;              //这里是不是很熟悉呢？
            //访问 sender 中的公共字段
            Console.WriteLine("Alarm：{0} - {1}: ", heater.area, heater.type);
            Console.WriteLine("Alarm: 嘀嘀嘀，水已经 {0} 度了：", e.temperature); 
        }
    }
    // 显示器
    public class Display
    {
        public static void ShowMsg(Object sender, Heater.BoliedEventArgs e)
        {       //静态方法
            Heater heater = (Heater)sender;
            Console.WriteLine("Display：{0} - {1}: ", heater.area, heater.type);
            Console.WriteLine("Display：水快烧开了，当前温度：{0}度。", e.temperature);
        }
    }
    class Program
    {
        static void Main()
        {
            Heater heater = new Heater();
            Alarm alarm = new Alarm();
            heater.Boiled += alarm.MakeAlert;       //注册方法
            // heater.Boiled += (new Alarm()).MakeAlert;              //给匿名对象注册方法
            // heater.Boiled += new Heater.BoiledEventHandler(alarm.MakeAlert);   //也可以这么注册
            heater.Boiled += Display.ShowMsg;              //注册静态方法
            heater.BoilWater();       //烧水，会自动调用注册过对象的方法
            Console.ReadKey();
        }
    }
}
~~~

`Action<T>`和`Func<T>`委托
---------------------

`Action<T>`和`Func<T>`委托进一步简化了委托的定义,而不用显式声明自定义委托。

泛型`Action<T>`委托表示引用一个void返回类型的方法，这个委托类存在不同的变体，最多可传递16种不同的参数类型。

~~~csharp
Action<int> f = delegate(int arg)
{
    Console.WriteLine(arg);
};
f.Invoke(10);
~~~

`Func<T>`允许调用带返回类型的方法，与`Action<T>`类似，也定义了不同的变体，最多也可传递16种不同的参数类型和一个返回类型。

~~~csharp
Func<int, int> f = delegate(int arg)
{
    Console.WriteLine(arg);
    return arg+10;
};
Console.WriteLine(f.Invoke(10));
~~~

`Func<T>`的类型参数列表中**最后一个**类型为返回值的类型。

`Predicate<T>`委托
----------------

`Predicate<T>`委托是C#类库中另一个常用的匿名委托，其定义如下:

    public delegate bool Predicate<T>(T obj);

此委托引用一个返回bool 值的方法，在实际开发中，通常使用`Predicate<T>`委托变量引用一个“判断条件函数”，在判断条件函数内部书写代码表明函数参数所引用的对象应满足的条件，条件满足时，函数返回true.

例如，在类库中，`Array<T>` 里面的Find方法，定义如下：

    public T Find(Predicate<T> match);

一个使用`Predicate<T>`委托的例子：

~~~csharp
using System;
using System.Drawing;

public class Example
{
    public static void Main()
    {
        // Create an array of Point structures.
        Point[] points = { new Point(100, 200), 
            new Point(150, 250), new Point(250, 375), 
            new Point(275, 395), new Point(295, 450)
        };
        
        // Define the Predicate<T> delegate.
        Predicate<Point> predicate = FindPoints;

        // Find the first Point structure for which X times Y  
        // is greater than 100000. 
        Point first = Array.Find(points, predicate);

        // Display the first structure found.
        Console.WriteLine("Found: X = {0}, Y = {1}", first.X, first.Y);
        Console.ReadKey();
    }

    private static bool FindPoints(Point p)
    {
        return p.X * p.Y > 100000;
    }
}
~~~

Lambda表达式
------------

前文中提到了如何通过命名方法和匿名方法创建委托(delegate)，接下来给出一个使用Lambda表达式来创建委托的例子：

~~~csharp
using System;

namespace SharpWork
{
    class Program
    {
        delegate void funcdg(int arg);
        public static void Main(string[] args)
        {
            funcdg f = x => Console.WriteLine(x);
            f.Invoke(100);
            Console.Write("Press any key to continue . . . ");
            Console.ReadKey(true);
        }
    }
}
~~~

上文中使用`Predicate<T>`委托的例子可以直接改成使用Lambda的例子：

~~~csharp
using System;
using System.Drawing;

public class Example
{
    public static void Main()
    {
        // Create an array of Point structures.
        Point[] points = { new Point(100, 200), 
            new Point(150, 250), new Point(250, 375), 
            new Point(275, 395), new Point(295, 450)
        };

        // Find the first Point structure for which X times Y  
        // is greater than 100000. 
        Point first = Array.Find(points, p => p.X * p.Y > 100000);

        // Display the first structure found.
        Console.WriteLine("Found: X = {0}, Y = {1}", first.X, first.Y);
        Console.ReadKey();
    }
}
~~~
