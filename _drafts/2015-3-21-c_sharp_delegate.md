---
title: C# 委托
author: DHDave
date: 2015-02-01
tags: [C Sharp]
category: 编程语言
layout: post
---

1. C#委托与函数指针
委托是安全封装方法的类型，类似于 C 和 C++ 中的函数指针。
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

            f(20);
            g(100);

            Console.ReadKey();


        }

        static void test(int x) 
        {
            Console.WriteLine("test {0}", x);
        }
    }
2. 委托类型派生自 .NET Framework 中的 Delegate 类。 委托类型是封装的，它们不能派生出其他类，也不能从 Delegate 派生出自定义类。 由于实例化的委托是一个对象，因此可以作为参数传递或分配给一个属性。 这允许方法作为参数接受委托并在稍后调用委托。 这被称为异步回调，是在长进程完成时通知调用方的常用方法。 当以这种方式使用委托时，使用委托的代码不需要知道要使用的实现方法。 功能类似于封装接口提供的功能。

委托作为参数：快速排序函数，cmp


3. 匿名委托
通过使用匿名方法，由于您不必创建单独的方法，因此减少了实例化委托所需的编码系统开销。
例如，如果创建方法所需的系统开销是不必要的，则指定代码块（而不是委托）可能非常有用。 启动新线程即是一个很好的示例。 无需为委托创建更多方法，线程类即可创建一个线程并且包含该线程执行的代码。
void StartThread() 
{ 
    System.Threading.Thread t1 = new System.Threading.Thread 
      (delegate() 
            { 
                System.Console.Write("Hello, "); 
                System.Console.WriteLine("World!"); 
            }); 
    t1.Start(); 
} 
