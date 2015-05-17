---
title: Python实现docx2txt
author: He Tao
date: 2015-05-17
tag: Python
category: 编程语言
layout: post
---

对于一些Linux平台，特别是没有安装桌面的Linux系统，查看MS Office的文档是一件非常痛苦的事情，如果能够将.dox格式的文件转化成一个文本文件，将会解决很多问题。我们知道，.docx文件实质上是一个.zip压缩包，里面包含了各种各样的XML结构化文本文件、资源文件、XML样式表，等等。文档中的所有内容都集中在压缩包中的`word/document.xml`文件中，然而这是一个复杂的XML文件，并不适合直接阅读。因此，需要将其中的文本提取出来。

我们知道，Python作为一门强大的脚本语言，很擅长处理与字符串相关的问题。因此使用Python来实现一个docx2txt的小工具。

<!--more-->

获取XML源文件
-------------

第一步，我们需要解压docx包，读取其中的XML文件。Python提供了`ZipFile`可供读取.zip文件。

```python
from zipfile import ZipFile

zf = Zipfile(filename)
```

那么，就如何判断一个文件是不是MS Office Word(.docx)文件呢？我们可以通过判断压缩包文件中是否存在`word/document.xml`文件来实现。

```python
def isdocx(zf):
    if not zf.namelist().__contains__('word/document.xml'):
        # print('invalid MS word file.')
        return False
    return True
```

接下来，读取`word/document.xml`文件：

```python
text = str(zf.read('word/document.xml'), encoding='utf-8')
```

读取之后，还需要解析这个XML文件，以提取出其中的所有文字信息。因为此处并不关心XML文件的样式以及其他信息，为方便起见，直接使用标准库中的`html.parser.HTMLParser`来解析XML即可。

首先，继承HTMLParser类：

```python
class Parser(HTMLParser):
    def __init__(self):
        HTMLParser.__init__(self)
        self.text = ''
```

接下来，提取出所有的data，这就是docx文件中的文本内容。这一步需要重写`handle_data`方法：

```python
def handle_data(self, data):
    self.text += data
```

然而仅仅通过`handle_data`并无法处理换行符的问题。通过观察`word/document.xml`文件的特点和查阅DOXC文件规范得知Word中每一个新段落都是通过`<w:p>`Tag开始的，因此，只需要在每次`<w:p>`标签结束的时候加上一个`\n`即可实现段落的划分。

```
def handle_endtag(self, tag):
    if tag == 'w:p':  # new line
        self.text += '\n'
```

更多
-----

目前还无法处理docx文件中的图片等等信息，样式表信息也忽略，进一步开发的话，希望能够做到将.docx文件转换成markdown文件。

项目地址
--------

[Programming/docx2txt](https://github.com/He-Tao/Programming/tree/master/docx2txt)

参考资料
--------

1. [Word Extensions to the Office Open XML (.docx) File Format](https://msdn.microsoft.com/en-us/library/dd773189(v=office.12).aspx)

