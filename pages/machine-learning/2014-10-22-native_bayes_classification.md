---
title: Native Bayes Classification
author: He Tao
date: 2014-10-22
tag: [机器学习]
category: 机器学习
layout: post
---

一、概述
----------

朴素贝叶斯分类基于贝叶斯定理，属于有监督的学习过程。在选取特征值恰当的情况下，朴素贝叶斯分类算法有很好的准确率。

二、定义
--------

<!--more-->

1、设$$x = \{a_1, a_2, \dots, a_m\}$$为一个待分类项，而每个a为x的一个特征属性。

2、有类别集合$$C = \{y_1, y_2, \dots, y_n\}$$

3、计算$$P(y_1|x), P(y_2|x), \dots, P(y_n|x)$$

4、如果$$P(y_k|n) = max\{P(y_1|x), P(y_2|x), \dots,P(y_n|x)\}$$则$$x\in y_k$$

那么现在的关键就是如何计算第3步中的各个条件概率。我们可以这么做：

1. 找到一个已知分类的待分类项集合，这个集合叫做训练样本集。
2. 统计得到在各类别下各个特征属性的条件概率估计。即
$$P(a_1|y_1),P(a_2|y_1),\dots,P(a_m|y_1),\dots,P(a_m|y_n)$$
3. 如果各个特征属性是条件独立的，则根据贝叶斯定理有如下推导：
$$P(y_i|x) = \frac{P(x|y_i)P(p_i)}{P(x)}$$
因为分母对于所有类别为常数，因为我们只要将分子最大化皆可。又因为各特征属性是条件独立的，所以有：
$$P(x|y_i)P(y_i) = P(a_1|y_i)P(a_2|y_i) \dots P(a_m|y_i) = P(y_i) \prod_{j=1}^m P(a_j|y_i) $$

三、基本实现步骤
-----------------

1. 整理数据， 对一些连续值字段离散化。
2. 通过已知数据统计计算$P(y_i)$，$P(a_j|y_i)$。
3. 对待分类数据进行分类

四、优化
--------

1. 通过相关性检验和无关检验，选取恰当的特征项作为朴素贝叶斯学习的特征值。
2. 采用更合理的连续数据离散化方法和更合理的噪声去除方法，均有助于提高朴素贝叶斯分类算法的精确度和适应性。
3. 通过对已经数据惊醒分类，检验结果，校正误差。

五、练习实现：

```python
#! /usr/bin/env python3
# -*- encoding: utf-8 -*-
'''
Using Native Bayes method to classify.
Learn data: adult.data
Test data: adult.test
Ans: error: 0.221792
properties = ['age',
              'type_employer',
              'fnlwgt',
              'education',
              'euucation_num',
              'marital',
              'occupation',
              'relationship',
              'race',
              'capital_gain',
              'capital_loss',
              'hr_per_week',
              'country']
'''
import sqlite3

frequency = ([{} for i in range(0, 16)], [{} for i in range(0, 16)])
probability = ([{} for i in range(0, 16)], [{} for i in range(0, 16)])
feature = [1, 2, 4, 7, 8, 11, 12] # properties used to test classify.
cnt = [0, 0]
total = [0.0, 0.0]

conn = sqlite3.connect('adult.db')
point = conn.cursor()

def main():
    learn()
    test()
    conn.close()

def test():
    sql = 'select * from test_data'
    correct = 0
    cnt0 = 0
    cnt1 = 0
    wrong = 0
    for row in point.execute(sql):
        if row[15] == 1:
            cnt1 += 1
        else:
            cnt0 += 1
        if classify(row) == row[15]:
            correct += 1
        else:
            wrong += 1
    print('correct: %d, wrong: %d, total: %d' %(correct, wrong, correct+wrong))
    print('correct rate: %f' %(correct/(correct+wrong)))
    print('correct rate: %f' %(wrong/(correct+wrong)))
    
def classify(data):
    data = list(data)
    p = [0.0, 0.0]
    data[3] = int(data[3]/50000)
    nodata = 0
    for i in [0, 1]:
        for j in feature:
            ans = 1.0
            rate = probability[i][j].get(data[j])
            if rate == None: # imcomplete data.
                rate = 1.0
                nodata += 1
            ans *= rate
        p[i] = ans * total[i]
    if p[0] > p[1]:
        return 0
    else:
        return 1

def learn():
    sql = 'select * from adult'
            
    for row in point.execute(sql):
        row = list(row) # transfer from tuple to list
        type = row[15]
        cnt[type] += 1
        row[3] = int(row[3]/50000)
        for i in range(1, 15):
            if frequency[type][i].get(row[i]):
                frequency[type][i][row[i]] += 1
            else:
                frequency[type][i][row[i]] = 1
    
    total[0] = cnt[0] / (cnt[0]+cnt[1])
    total[1] = cnt[1] / (cnt[0]+cnt[1])
    for type in range(0, 2):
        for i in range(1, 16):
            for (u, v) in frequency[type][i].items():
                probability[type][i][u] = v / cnt[type]
            
if __name__ == '__main__':
    main() 
```

测试用数据来自[UCI Machine Learning Repository](http://archive.ics.uci.edu/ml/datasets/Adult)。

实现的不足之处
--------------

1. 未能形成模块化，应该封装成可重用的接口。
2. 预处理不够仔细。
