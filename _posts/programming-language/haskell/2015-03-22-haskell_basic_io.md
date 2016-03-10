---
title: Haskell 基本输入与输出(Basic IO)
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

| Function     | Type                    | Action | Usage                                       |
|:------------:|-------------------------|:------:|---------------------------------------------|
| getChar      | :: IO Char              | I      | 从stdin读入一个字符                         |
| getContents  | :: IO String            | I      | 从stdin读入多行字符串                       |
| getLine      | :: IO String            | I      | 从stdin读入一行字符串                       |
| putChar      | :: Char -> IO ()        | O      | 向stdout输出一个字符                        |
| putStr       | :: String -> IO ()      | O      | 向stdout输出一个字符串                      |
| putStrLn     | :: String -> IO ()      | O      | 向stdout输出一个字符串，再输出一个换行符    |
| print        | :: Show a => a -> IO () | O      | 向stdout输出任何`Show`类型的参数            |

从这些IO函数的类型中可以看出，对于输入，函数返回一个`IO a`类型的值，对于输出，返回值的类型为`IO ()`，其中`()`表示空元组，这仅仅意味着这个IO操作的完成。那么，又如何从IO操作的返回值中得到对应类型的内容呢？Haskell提供了一个操作符`<-`用于从`IO a`中得到`a`类型的值。如下例：

~~~haskell
main :: IO ()
main = do
    a <- getLine       -- 用 <- 获取IO操作的值
    b <- print a       -- 将输出操作的返回值的内容绑定到 b
    print b            -- 输出操作的值为 IO()
~~~

用GHC编译上述代码，运行，输入

    hello

会得到输入：

    "hello"
    ()

IO Action可以在任何地方创建、赋值、传递。任何表达式都可以返回一个IO Action，但只有从另一个IO动作中（或main中）才能执行。举个例子：

~~~haskell
func :: IO ()
func = do
    let a = putStr "12345"
    return ()
main = do
    func
~~~

执行这一段代码不会获得任何输出，因为这仅仅将一个IO Action（此处为`putStr "12345"`）绑定到变量`a`而已，并没有执行。在`return ()`语句之前加上一句：

    a

再编译运行，会得到如下输出：

    12345

那么，上述代码中的`return ()`又是干嘛的呢？`return ()`的意思是构造了一个IO Action，其值为`()`。再看这样一段代码：

~~~haskell
func :: IO String
func = do
    return "1234567"

main = do
    str <- func
    print str
~~~

编译，运行，可以看到如下输出：

    "1234567"

在Haskell中，`return`记号可以用来构造一个IO Action，并且，**与C、Java、Python等语言不同，`return`不会终止函数的运行，`return`语句仅仅是构造了一个IO Action而已**。在上例中，`return`构造的`IO String`类型的值与`getLine`函数的`IO String`类型的返回值具有一样的特征（例如都可以用`<-`记号来取得IO Action的值）。

组合IO
-------

我们注意到在前面的程序里用到了`do`记号(notation)，`do`几号用于组合IO，将多个IO操作顺序执行。`do`隐藏了函数式的细节，从而使得一系列IO操作像命令式程序那样执行，同时将IO操作独立出来，以函数式的方式封装，避免IO操作影响其他的“纯函数”的执行。

应当注意到，组合IO块必须返回一个IO Action，可以用`return`来构造一个IO Action。haskell会将组合IO块的最后一条语句作为返回值。例如：

~~~haskell
func :: IO String
    getLine
    getLine
    getLine
~~~

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

~~~haskell
main = do
    fh = openFile "output.txt" WriteMode
    s <- hPutStr fh "output content"
    print s
    hClose fh
~~~

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

sequence
--------

`sequence` 接受一串 I/O action，并回传一个会依序执行他们的 I/O action。运算的结果是包在一个 I/O action 的一连串 I/O action 的运算结果。他的 type signature 是 `sequence :: [IO a] -> IO [a]`

~~~haskell
main :: IO()
main = do
    rs <- sequence [getLine, getLine, getLine]
    print rs
~~~

就等价于：

~~~haskell
main :: IO()
main = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a,b,c]
~~~


when
----

这函数定义在 `Control.Monad` 中。`when`在一个 do block 中看起来就像一个控制流程的 statement，但实际上他的确是一个普通的函数。

他接受一个 boolean 值跟一个 I/O action。如果 boolean 值是 True，便回传我们传给他的 I/O action。如果 boolean 值是 False，便回传 return ()，即什么都不做的 I/O action。

~~~haskell
import Control.Monad

main :: IO()
main = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        main
~~~

mapM
-----

`mapM` 接受一个函数跟一个串列，将对串列用函数 `map` 然后 `sequence` 结果。`mapM_` 也作同样的事，只是他把运算的结果丢掉而已。在不关心 I/O action 结果的情况下，`mapM_` 是最常被使用的。

`map print [1,2,3,4]` 这个动作并不会产生一个 I/O action，而是一串 I/O action，就像是 `[print 1, print 2, print 3, print 4]`。如果我们将一串 I/O action 变成一个 I/O action，我们必须用 `sequence`:

    sequence $ map print [1,2,3,4]

等价于：

    mapM print [1,2,3,4]

如果忽略结果，使用：

    mapM_ print [1,2,3,4]

interact
--------

`interact` 接受一个 `String -> String` 的函数，并回传一个 I/O action。那个 I/O action 会读取一些输入(直到读到EOF)，调用提供的函数，然后把函数的结果打印出来。

能应用 `interact` 的情况有几种，像是从输入 pipe 读进一些内容，然后丢出一些结果的程序；或是从用户获取一行一行的输入，然后丢回根据那一行运算的结果，再拿取另一行。

~~~haskell
import Control.Monad
import System.IO

main :: IO()
main = interact reverseLine

reverseLine :: String -> String
reverseLine contents = unlines (map (\s -> reverse s) (lines contents))

-- point-free style:
-- reverseLine = unlines . map reverse . lines
~~~

这段程序的作用在于每读入一行就reverse，然后输出。

`lines`和`unlines`的用法： 例如有字符串`"short\nlooooooooooooooong\nshort again"`。这字串有三行。用 `lines` 把字串分成 `["short", "looooooooooooooong", "short again"]`，并返回结果。最后用 `unlines` 把这些字串用换行接起来，形成 `"short\nlooooooooooooooong\nshort again"`。

命令行参数
---------

在 `System.Environment` 模块当中有两个 I/O actions:

+ `getArgs`，他的 type 是 `getArgs :: IO [String]`，他是一个拿取命令行引数的 I/O action，并把结果放在包含的一个串列中。
+ `getProgName` 的型态是 `getProgName :: IO String`，他则是一个 I/O action 包含了程序的名称。

~~~haskell
import System.IO
import System.Environment

main :: IO()
main = do
    args <- getArgs
    progName <- getProgName
    mapM_ print args
    print progName
~~~

随机数(Random)
-------------

Haskell 是一个纯粹函数式语言。代表任何东西都具有 **referential transparency**。那代表你喂给一个函数相同的参数，不管怎么调用都是回传相同的结果。在 `System.Random` 模块中，包含生成随机数所需要的函数：

    random :: (RandomGen g, Random a) => g -> (a, g)

接受一个 random generator ，然后回传一个随机值以及一个新的 random generator。

在 `System.Random` 中有一个很酷的型态，叫做 `StdGen`， 是 `RandomGen` 的一个 instance。 要自己做一个 random generator，要使用 `mkStdGen` 这个函数。他的型态是 `mkStdGen :: Int -> StdGen`, 接受一个整数(随机数种子)，然后根据这个整数会给一个 random generator。

生成随机数的代码：

    random (mkStdGen 100)

由于 random 函数会回传 Random typeclass 中任何一种型态，所以我们必须告诉 Haskell 我们是要哪一种型态。需要回传 random value 跟 random generator 的一个 pair

    ghci> random (mkStdGen 100) :: (Int, StdGen)
    (-1352021624,651872571 1655838864)

有一个函数叫 `randoms`，他接受一个 generator 并回传一个无穷串行。

    take 5 $ randoms (mkStdGen 11) :: [Int]
    take 5 $ randoms (mkStdGen 11) :: [Bool]

`randoms` 不另外多回传一个新的 generator, `randoms`的**一个实现**:

    randoms' :: (RandomGen g, Random a) => g -> [a]
    randoms' gen = let (value, newGen) = random gen in value:randoms' newGen

对于同样的`StdGen`, 程序永远都会回传同样的随机数。这在真实世界中的程序是不能接受的。`System.Random` 要提供 `getStdGen` 这个 I/O action，他的型态是 `IO StdGen`。当你的程序执行时，他会跟系统要一个 random generator，并存成一个 global generator。getStdGen 会替你拿那个 global random generator 并把他绑定到某个名称上。

~~~haskell
import System.Random

main = do
    gen <- getStdGen
    putStr $ take 20 (randomRs (1, 10) gen) -- randomRs: produce random number in specific range.
~~~

在haskell中生成随机数最常用的方法是使用`getStdRandom`函数，函数的类型签名是：

    getStdRandom :: (StdGen -> (a, StdGen)) -> IO a

使用示例：

    > getStdRandom (randomR (1, 3))
    1
    > getStdRandom (randomR (1, 3))
    3
    > getStdRandom (randomR (1, 3))
    2
    > getStdRandom (randomR (1, 3))

也可以使用`getStdRandom`函数来连续生成多个随机数：

    > sequence $ replicate 10 (getStdRandom (randomR (1, 3)))
    [2,3,1,3,3,1,2,1,3,3]

Exceptions
----------

当一个 exception 被丢出的时候，控制流程就会跳到我们做一些清理动作的地方，做完清理后 exception 被重新丢出，这样一些处理错误的代码可以完成他们的工作。 Haskell 有一个很棒的型态系统。Algebraic data types 允许像是 `Maybe` 或 `Either` 这种型态，我们能用这些型态来代表一些可能有或没有的结果。

**pure code 能丢出 Exception，但 Exception 只能在 I/O section 中被接到（也就是在 main 的 do block 中）这是因为在 pure code 中你不知道什么东西什么时候会被 evaluate。因为 lazy 特性的缘故，程序没有一个特定的执行顺序，但 I/O code 有。**

要这样使用 Exception，我们必须使用 `System.IO.Error` 中的 `catch` 函数。他的型态是 `catch :: IO a -> (IOError -> IO a) -> IO a`。他接受两个参数，第一个是一个 I/O action。像是他可以接受一个打开文件的 I/O action。第二个是 handler。
