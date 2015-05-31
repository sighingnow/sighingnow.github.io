---
title: Dive Into Haskell(5) 输入与输出(IO)
author: He Tao
date: 2015-03-22
tag: [Haskell]
category: 编程语言
layout: post
---

Haskell拥有惰性求值和一切皆函数的设计，在Haskell中，函数仅仅负责根据提供的参数返回特定的结果，并且，函数的结果不受外部环境的影响，仅仅与参数有关。但IO却拥有副作用，IO环境的变化使得函数的运行状态不可预测，这对程序的流程造成了严重的潜在影响。在Haskell中，使用了一套**Monadic I/O**的模型来弱化和解除程序对IO操作的依赖，其基本思想是控制构造具有IO操作的程序和限制IO运算对函数的影响。

<!--more-->

stdio的IO操作
--------------

Haskell提供类型`IO a`，其成员称为**类型a**的**I/O动作**。这种类型提供了一种在Haskell之上书写涉及到IO的有副作用的程序(按顺序执行IO操作)，并且不破坏Haskell的函数模型。以下函数用于标准IO(stdin/stdout)的输入与输出：

| Function     | Type                    | Action | Usage                                    |
|:------------:|-------------------------|:------:|------------------------------------------|
| getChar      | :: IO Char              | I      | 从stdin读入一个字符                      |
| getContents  | :: IO String            | I      | 从stdin读入多行字符串                    |
| getLine      | :: IO String            | I      | 从stdin读入一行字符串                    |
| putChar      | :: Char -> IO ()        | O      | 向stdout输出一个字符                     |
| putStr       | :: String -> IO ()      | O      | 向stdout输出一个字符串                   |
| putStrLn     | :: String -> IO ()      | O      | 向stdout输出一个字符串，再输出一个换行符 |
| print        | :: Show a => a -> IO () | O      | 向stdout输出任何`Show`类型的参数         |

从这些IO函数的类型中可以看出，对于输入，函数返回一个`IO a`类型的值，对于输出，返回值的类型为`IO ()`，其中`()`表示空元组，这仅仅意味着这个IO操作的完成。那么，又如何从IO操作的返回值中得到对应类型的内容呢？Haskell提供了一个操作符`<-`用于从`IO a`中得到`a`类型的值。如下例：

```haskell
main :: IO ()
main = do
    a <- getLine       -- 用 <- 获取IO操作的值
    b <- print a       -- 将输出操作的返回值的内容绑定到 b
    print b            -- 输出操作的值为 IO()
```

用GHC编译上述代码，运行，输入

    hello

会得到输入：

    "hello"
    ()

IO Action可以在任何地方创建、赋值、传递。任何表达式都可以返回一个IO Action，但只有从另一个IO动作中（或main中）才能执行。举个例子：

```haskell
func :: IO ()
func = do
    let a = putStr "12345"
    return ()
main = do
    func
```

执行这一段代码不会获得任何输出，因为这仅仅将一个IO Action（此处为`putStr "12345"`）绑定到变量`a`而已，并没有执行。在`return ()`语句之前加上一句：

    a

再编译运行，会得到如下输出：

    12345

那么，上述代码中的`return ()`又是干嘛的呢？`return ()`的意思是构造了一个IO Action，其值为`()`。再看这样一段代码：

```haskell
func :: IO String
func = do
    return "1234567"

main = do
    str <- func
    print str
```

编译，运行，可以看到如下输出：

    "1234567"

在Haskell中，`return`记号可以用来构造一个IO Action，并且，与C、Java、Python等语言不同，`return`不会终止函数的运行，`return`语句仅仅是构造了一个IO Action而已。在上例中，`return`构造的`IO String`类型的值与`getLine`函数的`IO String`类型的返回值具有一样的特征（例如都可以用`<-`记号来取得IO Action的值）。

组合IO
-------

我们注意到在前面的程序里用到了`do`记号(notation)，`do`几号用于组合IO，将多个IO操作顺序执行。`do`隐藏了函数式的细节，从而使得一系列IO操作像命令式程序那样执行，同时将IO操作独立出来，以函数式的方式封装，避免IO操作影响其他的“纯函数”的执行。

应当注意到，组合IO块必须返回一个IO Action，可以用`return`来构造一个IO Action。haskell会将组合IO块的最后一条语句作为返回值。例如：

```haskell
func :: IO String
    getLine
    getLine
    getLine
```

这一组合IO块的值便是最后一个`getLine`语句的值。同样，也可以通过绑定获取这一`IO String`类型的值中的`String`。

文件IO
-------

Haskell中，与文件操作有关的函数定义在`System.IO`包中，使用时需要先导入：

    import System.IO

打开与关闭文件：

    openBinaryFile
    openBinaryTempFile
    openBinaryTempFileWithDefaultPermissions
    openFile
    openTempFile
    openTempFileWithDefaultPermissions

这些函数的参数都是相同的：

    :: FilePath -> IOMode -> IO Handle

其中，`IOMode`有以下几种值：

    ReadMode | WriteMode | AppendMode | ReadWriteMode

`AppendMode`与`ReadWriteMode`这两种模式的区别在于`AppendMode`的起始位置是文件结尾处，而`ReadWriteMode`的起始位置是文件开头。

值得**注意**的是，以`ReadMode`打开一个文件时，程序在编译阶段便会检查文件是否存在，如果不存在，那么程序不能通过编译。而以`WriteMode`、`AppendMode`和`ReadWriteMode`模式打开文件是，如果文件不存在，那么回自动根据`FilePath`来创建一个文件。

如果对以`ReadMode`打开的文件进行写操作，或者对以`WriteMode`打开的文件进行都操作，在编译阶段便会产生错误。

打开文件的函数如`openFile`会返回一个`IO Handle`，可以同`<-`来获得这个`IO Handle`的值。如：

```haskell
main = do
    fh = openFile "output.txt" WriteMode
    s <- hPutStr fh "output content"
    print s
    hClose fh
```

在`System.IO`中，普通的对`stdio`的操作的函数都有对应的操作文件的版本，具体构成为将原函数的首字母变成大写，再在前面加上一个`h`字符。这些函数都有一个额外的参数：`IO Handle`。

在完成文件操作后，要使用`hClose`函数来关闭文件(文件句柄)。

与C/C++、Java、Python等语言的文件操作类似，Haskell中也有`hTell`和`hSeek`函数，用来获得当前文件操作所在的位置（精确位置）和移动文件操作的位置。`hSeek`函数的定义如下：

    hSeek :: Handle -> SeekMode -> Integer -> IO ()

三个参数的含义分别为文件句柄、偏移模式和偏移量。返回值为一个`IO ()`类型的值。`SeekMode`又有三种：

    AbsoluteSeek | RelativeSeek | SeekFromEnd

三种模式的含义分别为：

+ `AbsoluteSeek`: 通过指定精确位置的的方式来偏移操作位置；
+ `RelativeSeek`: 以当前位置为原点，偏移一个相对位置，正数表示向前偏移，而负数表示向后偏移；
+ `SeekFromEnd`: 从文件末尾向前偏移指定的字节数。例如`hSeek handle SeekFromEnd 0`的含义便是将操作位置设为文件末尾。

例如

    stdin, stdout, stderr

这样的IO文件是无法偏移的，Haskell中提供了`hIsSeekable`函数来判断文件是否支持偏移。

为了避免每次操作文件时都指定`IO handle`，可以将这些与文件IO相关的函数柯里化（部分函数），以此来实现更灵活、更简洁的IO操作。

删除与重命名文件
-----------------

在包`System.Directory`中定义了删除和重命名文件的函数：

+ `removeFile`: `:: FilePath -> IO ()`，
+ `renameFile`: `:: FilePath -> FilePath -> IO ()`，两个`FilePath`分别为旧、新文件名，
+ `removeDirectory`: 删除目录，
+ `renameDirectory`: 重命名目录，
+ `removeDirectoryRecursive`: 递归地删除目录。

