---
title: Java 8 Lambda 代码片段
author: He Tao
date: 2015-05-28
category: 编程语言
tags: Java
layout: post
---

Java 8引入了lambda表达式，可以用来完成很多函数式编程的目的。本文的内容主要用来积累一些有用的Lambda表达式的应用代码片段。

可以使用下面语法实现Lambda表达式:

    (params) -> expression
    (params) -> statement
    (params) -> { statements }

如果方法并不改变任何方法参数，比如只是输出，那么可以简写如下：

    () -> System.out.println("Hello Lambda Expressions");

如果方法接受两个方法参数，如下：

    (int even, int odd) -> even + odd

<!--more-->

Functional Interface
--------------------

Functional Interface指的是只有一个抽象方法的接口。在Java 8中，使用@FunctionalInterface注解来标记(注解是可选的)。可以使用lambda表达式来实例化接口的子类。

实现Runnable线程，使用() -> {}替代匿名类：

```java
@FunctionalInterface
interface M {
    public abstract void A();
}
// ...
M m = ()->System.out.println("a");

@FunctionalInterface
interface N {
    public abstract void A(String arg);
}
// ...
N n = ("lambda expression")->{
    System.out.println("class N"); 
    System.out.println(arg);
};
```

一个更加实用的例子：使用Lambda表达式来创建匿名Runnable线程对象：

```java
//Before Java 8:
new Thread(new Runnable() {
    @Override
    public void run() {
        System.out.println("Before Java8");
    }
}).start();

//Java 8 way:
new Thread(() -> System.out.println("In Java8!")).start();
```


仿照Runnable的例子，也可以使用lambda表达式来实现事件处理。使用lambda表达式如下所示写出更好的事件侦听器的代码。

```java
// Before Java 8:
JButton show =  new JButton("Show");
show.addActionListener(new ActionListener() {
     @Override
     public void actionPerformed(ActionEvent e) {
           System.out.println("without lambda expression is boring");
        }
     });

// Java 8 way:
show.addActionListener((e) -> {
    System.out.println("Action !! Lambda expressions Rocks");
});
```

使用lambda表达式实现排序：

```java
// Old style.
List<String> l = new LinkedList<>();
// ...
l.sort(new Comparator<String>() {
    @Override
    public int compare(String s1, String s2) {
        //...
        return s1.compareTo(s2);
    }
})

// Lambda style.
l.sort((String s1, String s2)->s1.compareTo(s2));
```

更复杂的情形中，也许需要进行多个属性的比较，此时，我们可以考虑使用`thenComparing`方法：

```java
class A {
    int a, b;
    A(int a, int b) {
        this.a = a; this.b = b;
    }
}
List<A> aList = Arrays.asList(new A(1, 2), new A(3, 4), new A(1, 5));
    aList.sort(new Comparator<A>() {
        @Override
        public int compare(A o1, A o2) {
            // TODO Auto-generated method stub
            return 0;
        }
    }.thenComparing((o1, o2) -> o1.a - o2.a)
     .thenComparing((o1, o2) -> o1.b - o2.b));

// 或者写成：
class Utils {
    static <E> Comparator<E> compare() {
        return (e1, e2) -> 0;
    }
}
aList.sort(Utils.<A> compare().thenComparing((o1, o2) -> o1.a - o2.a)
                              .thenComparing((o1, o2) -> o1.b - o2.b));
```

在java 8中你可以使用Lambda表达式替代丑陋的匿名类。

**需要注意的是，在Lambda表达式中使用的外部变量在效果上必须是`final`的。**

> Local variable a defined in an enclosing scope must be final or effectively final.

**也就是说，这些变量是不可以被改变的，尽管在声明变量时不是必须使用`final`关键字。**

接口的默认方法
---------------

Java 8中，接口可以有默认方法。如下：

```java
interface A {
    default void funcA() {
        /...
    }
    void funcB();
}
```

在实现接口时，只需要实现接口的非默认方法即可。但是，使用lambda表达式去实现一个`FunctionalInterface`的时候，不能调用接口的默认方法，否则会有编译错误。

遍历集合
---------

使用Lambda表达式遍历List集合

```java
//Prior Java 8 :
List features = Arrays.asList("Lambdas", "Default Method", "Stream API", 
        "Date and Time API");
for(String feature : features) {
    System.out.println(feature);
}

//In Java 8:
List features = Arrays.asList("Lambdas", "Default Method", "Stream API",
        "Date and Time API");
features.forEach(n -> System.out.println(n));

// Even better use Method reference feature of Java 8
// method reference is denoted by :: (double colon) operator
// looks similar to score resolution operator of C++
features.forEach(System.out::println);
```

方法引用是使用两个冒号`::`这个操作符号。

Lambda表达式和函数接口
----------------------

为了支持函数编程，Java 8加入了一个新的包`java.util.function`，其中有一个接口`java.util.function.Predicate`是支持Lambda函数编程：

```java
public static void main(args[]){
    List languages = Arrays.asList("Java", "Scala", "C++", "Haskell", "Lisp");

    System.out.println("Languages which starts with J :");
    filter(languages, (str)->str.startsWith("J"));

    System.out.println("Languages which ends with a ");
    filter(languages, (str)->str.endsWith("a"));

    System.out.println("Print all languages :");
    filter(languages, (str)->true);

    System.out.println("Print no language : ");
    filter(languages, (str)->false);

    System.out.println("Print language whose length greater than 4:");
    filter(languages, (str)->str.length() > 4);
}

public static void filter(List names, Predicate condition) {
    for(String name: names) {
        if(condition.test(name)) {
            System.out.println(name + " ");
        }
    }
}
```

程序运行后的输出结果为：

    Languages which starts with J :
    Java
    Languages which ends with a
    Java
    Scala
    Print all languages :
    Java
    Scala
    C++
    Haskell
    Lisp
    Print no language :
    Print language whose length greater than 4:
    Scala
    Haskell

```java
//Even better
public static void filter(List names, Predicate condition) {
    names.stream()
        .filter((name) -> (condition.test(name)))
        .forEach((name) -> {System.out.println(name + " ");
    });
}
```

从这个例子中可以看出，来自Stream API 的filter方法能够接受 Predicate参数, 能够允许测试多个条件。

复杂的结合Predicate使用
-----------------------

`java.util.function.Predicate`提供`and()`, `or()`和`xor()`可以进行逻辑操作，比如为了得到一串字符串中以"J"开头的4个长度：

```java
// We can even combine Predicate using and(), or() And xor() logical functions
// for example to find names, which starts with J and four letters long, you
// can pass combination of two Predicate

Predicate<String> startsWithJ = (n) -> n.startsWith("J");
Predicate<String> fourLetterLong = (n) -> n.length() == 4;

names.stream().filter(startsWithJ.and(fourLetterLong))
              .forEach((n) -> System.out.print("\nName, which starts with 'J' 
                        and four letter long is : " + n));
```

其中`startsWithJ.and(fourLetterLong)`是使用了AND逻辑操作。

Lambda实现Map和Reduce
---------------------

最流行的函数编程概念是map，它允许你改变你的对象，在这个案例中，我们将`costBeforeTeax`集合中每个元素改变了增加一定的数值，我们将Lambda表达式 `x -> x*x` 传送`map()`方法，这将应用到stream中所有元素。然后我们使用 `forEach()` 打印出这个集合的元素.

```java
// applying 12% VAT on each purchase
// Without lambda expressions:
List costBeforeTax = Arrays.asList(100, 200, 300, 400, 500);
for (Integer cost : costBeforeTax) {
    double price = cost + .12*cost;
    System.out.println(price);
}

// With Lambda expression:
List costBeforeTax = Arrays.asList(100, 200, 300, 400, 500);
costBeforeTax.stream().map((cost) -> cost + .12*cost)
                      .forEach(System.out::println);
```

程序的输出为：

    112.0
    224.0
    336.0
    448.0
    560.0

`reduce()`是将集合中所有值结合进一个，Reduce类似SQL语句中的`sum()`, `avg()` 或`count()`, 如下面的例子：

```java
// Applying 12% VAT on each purchase
// Old way:
List costBeforeTax = Arrays.asList(100, 200, 300, 400, 500);
double total = 0;
for (Integer cost : costBeforeTax) {
    double price = cost + .12*cost;
    total = total + price;
}
System.out.println("Total : " + total);

// New way:
List costBeforeTax = Arrays.asList(100, 200, 300, 400, 500);
double bill = costBeforeTax.stream().map((cost) -> cost + .12*cost)
                                    .reduce((sum, cost) -> sum + cost)
                                    .get();
System.out.println("Total : " + bill);
```

程序运行的输出为：

    Total : 1680.0

filter和filtering
-----------------

Filtering是对大型Collection操作的一个通用操作，Stream提供`filter()`方法，接受一个Predicate对象，意味着你能传送lambda表达式作为一个过滤逻辑进入这个方法。例如，通过filtering 创建一个字符串`String`的集合:

```java
// Create a List with String more than 2 characters
List<String> strList = Arrays.asList("abc", "", "bcd", "", "defg", "jk");
List<String> filtered = strList.stream().filter(x -> x.length()> 2)
                                        .collect(Collectors.toList());
System.out.printf("Original List : %s, filtered list : %s %n", 
        strList, filtered);
```

程序运行后的输出为：

    Original List : [abc, , bcd, , defg, jk], filtered list : [abc, bcd, defg]

map与List的结合使用
-------------------

1. 对集合中每个元素应用函数。我们经常需要对集合中元素运用一定的功能，如表中的每个元素乘以或除以一个值等等.

```java
// Convert String to Uppercase and join them using coma
List<String> G7 = Arrays.asList("USA", "Japan", "France", "Germany", 
                                "Italy", "U.K.","Canada");
String G7Countries = G7.stream().map(x -> x.toUpperCase())
                                .collect(Collectors.joining(", "));
System.out.println(G7Countries);
```

Output : 

    USA, JAPAN, FRANCE, GERMANY, ITALY, U.K., CANADA

上面是将字符串转换为大写，然后使用逗号串起来。

2. 通过复制不同的值创建一个子列表。使用Stream的distinct()方法过滤集合中重复元素。

```java
// Create List of square of all distinct numbers
List<Integer> numbers = Arrays.asList(9, 10, 3, 4, 7, 3, 4);
List<Integer> distinct = numbers.stream().map(i -> i*i).distinct()
                                         .collect(Collectors.toList());
System.out.printf("Original List : %s,\nSquare Without duplicates: %s %n",
        numbers, distinct);
```

程序的输出为：

    Original List : [9, 10, 3, 4, 7, 3, 4], 
    Square Without duplicates : [81, 100, 9, 16, 49]

3. 计算List中的元素的最大值，最小值，总和及平均值

```java
//Get count, min, max, sum, and average for numbers
List<Integer> primes = Arrays.asList(2, 3, 5, 7, 11, 13, 17, 19, 23, 29);
IntSummaryStatistics stats = primes.stream().mapToInt((x) -> x)
                                            .summaryStatistics();
System.out.println("Highest prime number in List : " + stats.getMax());
System.out.println("Lowest prime number in List : " + stats.getMin());
System.out.println("Sum of all prime numbers : " + stats.getSum());
System.out.println("Average of all prime numbers : " + stats.getAverage());
```

程序运行后的输出为: 

    Highest prime number in List : 29
    Lowest prime number in List : 2
    Sum of all prime numbers : 129
    Average of all prime numbers : 12.9

