---
title: Linux 文件解压及压缩命令
author: He Tao
date: 2015-02-17
tag: Linux
category: Linux/Unix
layout: post
---

Linux下，通常需要用tar命令进行解压和压缩操作，针对不同的压缩文件格式，tar命令的参数也不相同。

tar 命令的常用参数含义:
------------------------

```
Main operation mode:

 -A, --catenate, --concatenate   append tar files to an archive
 -c, --create               create a new archive
 -d, --diff, --compare      find differences between archive and file system
     --delete               delete from the archive (not on mag tapes!)
 -r, --append               append files to the end of an archive
 -t, --list                 list the contents of an archive
     --test-label           test the archive volume label and exit
 -u, --update               only append files newer than copy in archive
 -x, --extract, --get       extract files from an archive
```

常见压缩文件格式解压参数
------------------------

1. .tar.Z 格式

        解压: tar Zxvf FileName.tar.Z
        压缩: tar Zcvf FileName.tar.Z

2. .zip 格式

<!--more-->

        压缩: unzip FileName.zip
        解压: zip 

3. .gz 格式

        解压：gunzip FileName.gz
          或：gzip -d FileName.gz
        压缩：gzip FileName

4. .tar.gz 格式

        解压：tar zxvf FileName.tar.gz
        压缩：tar zcvf FileName.tar.gz DirName

5. .bz2 格式

        解压：bzip2 -d FileName.bz2
          或：bunzip2 FileName.bz2
        压缩：bzip2 -z FileName

6. .tar.bz2 格式

        解压：tar jxvf FileName.tar.bz2
        压缩：tar jcvf FileName.tar.bz2 DirName

7. .bz 格式

        解压：bzip2 -d FileName.bz
          或：bunzip2 FileName.bz

8. .tar.bz 格式

        解压：tar jxvf FileName.tar.bz

9. .Z 格式

        解压：uncompress FileName.Z
        压缩：compress FileName

10. .tgz 格式

        解压：tar zxvf FileName.tgz

11. .tar.tgz 格式

        解压：tar zxvf FileName.tar.tgz
        压缩：tar zcvf FileName.tar.tgz FileName

12. .zip 格式

        解压：unzip FileName.zip
        压缩：zip FileName.zip DirName

13. .rar 格式

        解压：rar e FileDir
        压缩：rar a FileName.rar 

14. .tar.lz 格式

        解压：tar xvf filename.tar.lz

15. .tar.xz 格式

        解压：xz -d filename.tar.xz
        压缩：xz -Z FileDir

16. .lzma格式

        解压：lzma -d filename.lzma
        压缩: lzma -k FileDir



