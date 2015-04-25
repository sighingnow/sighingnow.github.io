---
title: Mathematica 输入与输出
author: He Tao
date: 2015-02-07
tag: Mathematica
category: Math
layout: post
---

基本输出
--------

Mathematica的基本输出命令是`Print`，具体用法为：

+ Print[expr] 

    输出 expr. 

例如：

    Print["Hello world"]
    Hello world
    Print[2^10]
    1024

`Print`命令也可以一次输出多项内容(用逗号`,`隔开)，输出时每项内容之间无间隔：

    Print["Hello world", 2^10]
    Hello world1024

<!--more-->

基本输入
--------

<!--more-->

### Input

Mathematica的基本输入命令是`Input`，具体用法为:

+ Input[] 

    在一个 Mathematica 表达式中交互读入.

+ Input[prompt] 

    提示输入，显示 prompt 为一个 " 提示符".

+ Input[prompt,init] 

    在笔记本前端用 init 作为输入域的初始内容.

`Input`命令得到的结果是表达式，值为表达式进行计算后的结果。


还有这些命令也可是实现输入：

### InputString

`InputString`用于交互式地读入字符串。具体用法：

+ InputString[] 

    以交互方式读入一个字符串.

+ InputString[prompt] 

    提示输入，把 prompt 显示为一个 "提示".

+ InputString[prompt,init] 

    在一个笔记本前端用 init 作为输入域的初始内容. 


### Get

`Get`用于读取文件中的表达式并计算。具体用法：

+ `<<name` 
    
    读取一个文件，计算其中的每个表达式，并且返回最后一个.

+ `Get[stream]` 

    读取一个文件流，计算其中的每个表达式，并且返回最后一个.

### Read

`Read`用于从输入流(stream)中读取表达式或对象。具体用法为：

+ Read[stream] 

    从一个输入流读取表达式，并返回该表达式.

+ Read[stream,type] 

    读取指定类型的对象.

+ Read[stream,type1,type2,...}] 
    
    读取指定类型的一系列对象. 

`Get`命令可读取的类型有：

| 类型       | 说明                                      |
|------------|-------------------------------------------|
| Byte	     | 单字节，返回一个整数编码                  |
| Character  | 单字符，返回以一个由单字符组成的字符串    | 
| Expression | 完整的 Mathematica 表达式                 |
| Number	 | 整数或近似数，以 "E" 格式给出             |
| Real	     | 近似数，以 "E" 格式给出                   |
| Record     | 用记录分隔符分隔的字符序列                |
| String     | 以换行符结束的字符串                      |
| Word       | 用单词分隔符分隔的字符序列                |

Import 命令和文件导入
----------------------

`Import`命令用来实现文件中的数据的导入。具体用法为：

+ Import["file"] 

    从文件中导入数据，将返回相应的 Mathematica 格式. 

+ Import["file",elements] 

    从文件中导入指定元素. 

+ Import["http://url",...] 和 Import["ftp://url",...] 

    从任何可访问网址导入. 




