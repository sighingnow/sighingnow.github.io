---
title: 决策树(ID3)
author: He Tao
date: 2015-02-20
tag: [机器学习]
category: 机器学习
layout: post
---

决策树(Decision Tree)是一个分类和预测模型，代表对象属性和对象值之间的一种映射关系。在决策树中，每个节点代表对象，而每个分叉都代表某方面特征的可能取值。最终，叶节点表示该对象所属的种类。

常用的决策树算法有ID3，C4.5等，本文以ID3算法为例进行分析。

<!--more-->

创建分支
---------

上文中已经提到决策树是根据某一特征的不同取值来对元素进行划分，不难想到很容易用递归的方法来实现数的建立。

    Check if every item in the dataset is in the same class:
        If so 
            return the class label
        Else
            find the best feature to split the data
            split the dataset
            create a branch node
            for each split
                call createBranch and add the result to the branch node
            return branch node

特征选取和信息增益
------------------

一般来说，决策树算法通常会采用二分法划分数据集，另一种划分方法是根据选取的特征分量的属性个数来划分。划分数据集的原则是将无序的数据变得有序，可以从信息论的观点出发来衡量数据的杂乱程度。

信息增益是指划分数据集之前/之后信息发生的变化。在选择划分数据集的特征时，获得信息增益最高的特征就是最好的选择。


构造决策树
----------

分类
----

决策树的优缺点
--------------

决策树算法的计算复杂度不高，并且能较好地适应有缺失的数据。与kNN算法相比，决策树算法的优势在于能够描述数据形式的含义，而kNN算法仅仅基于统计，无法给出其数据的内在含义。

但同时决策树也容易产生过度匹配的问题。


1. [ID3 Algorithm](http://en.wikipedia.org/wiki/ID3_algorithm)
