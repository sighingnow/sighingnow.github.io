---
title: 先验和后验概率以及估计
author: sighingnow
date: 2015-03-18
tag: [机器学习]
category: 机器学习
layout: post
---

<!--more-->

先验概率与后验概率
----------------

先验概率是指事件尚未发生，对该事件发生的概率的估计，是在缺乏某个事情的情况下描述一个变量。后验概率是指在
事件已经发生的条件下，求该事件发生原因是由某个因素引起的可能性的大小，是考虑一个事件之后的条件概率。

先验概率可以通过已知的关于事件本身的先验知识得到，蒙特卡洛方法也可以用于计算先验概率。后验概率可以基于
贝叶斯定理，通过先验概率乘以似然度，再归一化得到。具体来说，贝叶斯公式：

$$P(h|D) = \frac{P(D\|h)P(h)}{P(D)}$$

其中$P(h)$为$h$的先验概率，$P(h\|D)$为$h$的后验概率。

最大似然估计与最大后验估计
-----------------------

似然函数(likelihood function)是关于参数的函数，表示在参数给定的条件下，随机变量的值的条件概率。对于随机变量$X$，
给定模型中$X$的概率分布函数为$f$，$\theta$为参数，则参数的似然函数为：

$$\mathcal{L}(\theta\|x) = P(x\|\theta)$$

假设$x_1, x_2, \dots, x_n$为该模型独立同分布的采样，那么

$$f(x_1, x_2, \dots, x_n \| \theta) = f(x_1\|\theta) \times f(x_2\|\theta) \times \dots \times f(x_n \| \theta)$$

因此，似然函数以及对数似然函数为

$$\begin{aligned}\mathcal{L}(\theta\|x_1, x_2, \dots, x_n) &= \prod_{i=1}^{n} f(x_i\|\theta) \\
                 \ln{\mathcal{L}(\theta\|x_1, x_2, \dots, x_n)} &= \sum_{i=1}^{n}\ln{f(x_i\|\theta)}
\end{aligned}$$

并且取

$$\mathcal{l} = \frac{1}{n}\ln{\mathcal{L}}$$


### 最大似然估计

最大似然估计是指求当似然函数取得最大时参数$\theta$的值，

$$\theta_{MLE} = \mathop{\arg\,\max}\limits_{\theta \in \Theta} f(x\|\theta) = \mathop{\arg\,\max}\limits_{\theta \in \Theta} \mathcal{L}(\theta|x)$$

想要求解参数$\theta$的值，只需要对$\mathcal{L}$取导数，令导数等于$0$，得到似然方程，求解似然方程即可。

可见，似然函数的值表达式是某个模型能够产生某个给定观察序列的概率，最大似然即能产生该给定观察序列的最大概率。
最大似然估计是在参数$\theta$的整个取值空间$\Theta$上求解使得似然最大的参数值，并没有考虑该模型本身的概率。

<!--TODO：举例：一次直线拟合和二次曲线拟合。-->

### 最大后验估计

最大后验估计引入了参数的先验分布$g$，求解对于已有的观测序列，能使得后验概率最大的参数的值。根据贝叶斯公式，后验概率

$$f(\theta|x) = \frac{f(x|\theta)f(\theta)}{\sum_{\theta_i \in \Theta}{f(x|\theta_i)f(\theta_i)}} = \frac{f(x|\theta)f(\theta)}{f(x)}$$

同时$f(x)$的具体值与参数$\theta$无关，不影响求解参数$\theta$的最大估计，最大后验估计可以表示为

$$\theta_{MAP} = \mathop{\arg\,\max}\limits_{\theta \in \Theta} f(\theta|x) = \mathop{\arg\,\max}\limits_{\theta \in \Theta} f(x|\theta)f(\theta)$$

而由贝叶斯定理，后验概率可以表示为

$$f(x|\theta)g(\theta) = \frac{1}{2}$$

### 对比

最大似然估计与最大后验估计最大的区别在于最大后验估计引入了模型参数本身的概率分布，或者说最大似然估计认为参数本身满足均匀分布。
当数据量足够大时，最大似然估计和最大后验估计趋于一致，当数据量为0时，后验概率仅有先验概率决定，二者一致。

统计推断理论体系中，频率学派把需要推断的参数$\theta$视作固定且未知的常数，而样本$X$是随机的，其着眼点在样本空间，有关的
概率计算都是针对$X$的分布。贝叶斯学派把参数$\theta$视作随机变量，而样本$X$是固定的，其着眼点在参数空间，重视参数$\theta$的
分布。最大似然估计体现是的频率学派的观点，而最大后验估计体现的是贝叶斯学派的观点。

