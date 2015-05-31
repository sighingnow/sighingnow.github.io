---
title: Cygwin rebaseall
author: He Tao
date: 2015-01-31
tag: Cygwin
category: Linux/Unix
layout: post
---

Overview
--------

很多时候在升级Cygwin或安装Cygwin的软件包之后，加载dll会出错，或“fork()”出现错误，并提示“try rebaseall” , 此时，可以通过执行rebaseall命令来解决这些错误。

Required
--------

执行rebaseall命令，需要安装以下软件包

1. dash (or ash)
2. rebase

Usage
-----

<!--more-->

首先，需要暂停所有正在运行的Cygwin服务，可以通过在Cygwin的shell中执行如下命令获取当前正在运行的服务列表:

    cygrunsrv --list 

关闭所有的Cygwin服务后，在Cygwin的安装目录下，找到/bin目录下的 `dash.exe` 或者 `ash.exe` 程序，右键，以管理员权限运行，然后在打开的shel中运行如下命令:

    /usr/bin/rebaseall -v
    exit

**Note** 在运行rebasell时，可能会由于文件的权限问题而失败，可以将相关文件的权限设置为“可写”后重试。

Reference
---------

1. [Cygwin Wiki](http://cygwin.wikia.com/wiki/Rebaseall)

