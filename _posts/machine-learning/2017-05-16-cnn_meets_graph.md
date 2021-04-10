---
title: CNN Meets Graphs
author: Tao He
date: 2017-05-16
tags: [GCN, Graph Theory, 机器学习]
category: 机器学习
layout: post
---

Graph convolutional network is a kind of neural network that use graph data as input,
developing a serials of _Conv_ operator and `Pooling` theory based on graph theory,
mainly the spectral graph theory.

<!--more-->

数学基础
-------

### Spectral Graph Theory

对于无向图$G$的链接矩阵$A$，按如下方式定义度矩阵$D$（$D$是一个对角阵）

$$ D_{ii} = \sum_{j}A_{ij} $$

在此基础上定义图的Laplacian矩阵

$$ L = D - A $$

$L$是一个实对称矩阵，因此$L$必然可以对角化，可以分解为$L = U \Lambda U^{-1}$的形式，
其中$\Lambda$是$L$的特征值组成的对角阵，$U$是$L$的特征向量矩阵，并且$U$是一个单位正交
矩阵，有$U^T = U^{-1}$。因此，$L$分解可以进一步表达为

$$L = U \Lambda U^{-1} = U \Lambda U^T$$

对 Laplacian 矩阵进行正规化，得到

$$L = I - D^{-1/2} A D^{-1/2}$$

### Spectral and Convolution

_符号约定_：
+ $f$，$g$，$h$是一维或二维连续函数
+ $x$，$m$，$n$，$i$，$j$是自变量
+ $\star$ 表示卷积算符，$\cdot$表示普通的乘法算符，$\odot$表示两个向量之间的ElementWise乘积
+ $\mathcal{F}$表示离散傅里叶变换或连续傅里叶变换，$\mathcal{F}^{-1}$表示相应的傅里叶逆变换

#### 卷积的定义

一维连续卷积的定义，若$h$是$f$和$g$的卷积，即$h = f * g$，那么

$$h(x) = \int_{-\infty}^{\infty} f(t) \cdot g(x-t) \,dt$$

二维连续卷积的定义，若$h$是$f$和$g$的卷积，即$h = f * g$，那么

$$h(i, j) = \int_{\infty}^{infty}
    {\int_{\infty}^{\infty} f(m, n) \cdot g(i-m, j-n) \,dm}
\,dn$$

接下来考虑有限尺度上的二维离散卷积，例如图片上的卷积，假定 $f$ 表示二维图像，$g$
表示卷积核（尺度小于$f$），仍然有$h = f * g$，那么

$$h(i, j) = \sum_{m=-M}^{M}
    {\sum_{n=-N}^{N} f(m, n) \cdot g(i-m, j-n)}
$$

其中，$M = \lfloor R(g)/2 \rfloor，N = \lfloor C(g)/2 \rfloor$。

#### 卷积定理

卷积定理：函数卷积的傅里叶变换是函数傅里叶变换的乘积，即一个域中的卷积对应于
另一个域的乘积，因此，时（空）域上的卷积运算可以转换为其在频域上的乘积，在对结果
做傅里叶逆变换，

$$f * g = \mathcal{F}^{-1} \{\mathcal{F}\{f\} \cdot \mathcal{F}\{g\} \}$$

#### Laplacian矩阵与傅里叶变换

拉普拉斯算子是欧式空间上的一个二维微分算子，定义为

$$\Delta f = \sum_{i=1}^n \frac{\partial^2 f}{\partial x_i^2}$$

因此，Laplacian矩阵的特征矩阵可以作为傅里叶变换的基，即 $\{\mathcal{F}\{f\} = U^T f$。

考虑向量$f$, $g$，不妨假定$f$代表图上的点，$g$代表卷积核，则卷积操作

$$\begin{aligned}
    f * g &= \mathcal{F}^{-1}(\mathcal{F} f \odot \mathcal{F} g) \\
          &= U((U^T f) \odot (U^T g)) \\
          &= U((U^T g) \odot (U^T f)) \\
          &= U({\hat{g}} \odot (U^T f)) \\
          &= U diag(\hat{g}) U^T f
\end{aligned}$$

将$diag(\hat{g})$的对角线上的$n$个元素$\hat{g}\_i$分别视为$L$的$n$个特征值$\lambda\_i$
的函数，那么，下列公式中的卷积可以表达为

$$\begin{aligned}
    f * g &= U diag(\hat{g}) U^T f
          &= U {\begin{bmatrix}
                    \hat{g}(\lambda_1) & & \\
                    & \ddots & \\
                    & & \hat{g}(\lambda_n) \end{bmatrix}}
                U^T f \\
          &= U \hat{g}(\Lambda)U^F f
\end{aligned}$$

其中，$\hat{g}\_i$是需要训练的参数，规模为$O(n)$，并且，对于每个卷积层，都需要预先计算好Laplacian矩阵
的特征向量矩阵。

#### 多项式近似

根据[^3]中的结论，$\hat{g}(\Lambda)$可以有在$K$阶Chebyshev多项式上的有限项展开近似，

$$\hat{g}(\Lambda) \approx \sum_{k=0}^K \theta_k T_k(\tilde{\Lambda})$$

其中，$\tilde{\Lambda} = \frac{2}{\lambda_{max}}\Lambda-I_N$，$\lambda_{max}$表示$L$的最大
的特征值（实际实验中近似处理为2[^4][^5]，省去求Laplacian矩阵最大特征值的计算步骤）。Chebyshev多
项式的递归定义为

$$T_k(x) = 2xT_{k-1}(x)-T_{k-2}(x)$$

其中，$T_0(x) = 1$，$T_1(x) = x$。

假定对于非负整数$k$，$h = \theta_k x^k$，不难得出关于$h(\Lambda)$的等式

$$\begin{aligned}
U h(\Lambda) U^T &= U \theta_k \Lambda^k U^T
                 &= \theta_k (U \Lambda U^T)^k
                 &= \theta_k L^k
\end{aligned}$$

也就意味着$\Lambda$的多项式可以转换为$L$的多项式，约等式中$\hat{g}(\Lambda)$的近似
多项式展开可以表示为

$$\hat{g}(\Lambda) \approx \sum_{k=0}^K \theta_k T_k(\tilde{\Lambda}) = \sum_{k=0}^K \theta_k T_k(\tilde{L})$$

其中，$\tilde{L} = \frac{2}{\lambda_{max}}\,L - I_N$。此时Laplacian矩阵的$K$阶多项式包含了$K$近邻的含义[^4]，
$\theta_k$为需要训练的参数，规模为$O(K)$，为常数复杂度。

图的粗化
--------

对于一个图，通过点与点之间的近邻信息可以对图进行聚类，将每个类用一个新的点表示，得到图的粗化结果。
位于不同类的两个点之间有连接关系，则对聚类结果中代表这两个类的新点之间建立连接关系。

1. 下图是[^1]中提到的多层级聚类方法。

    ![Hierarchical Clustering of Graph]({{site.url}}/resource/cnn_meets_graph/multi-resolution-clustering.png "Hierarchical Clustering of Graph")

2. 下图是[^2]中提到的每次使图的大小减小一般的粗化方法。

    ![Graph Coarsening and Pooling]({{site.url}}/resource/cnn_meets_graph/half-pooling.png "Graph Coarsening and Pooling")

模型
----

+ 输入：图的邻接矩阵$A$及每个点的特征$F_{ij}^{0}$
+ 输出：图的特征

### 图的粗化与多层CNN

对于多层CNN模型，每一个卷积层都会用到该层输入的图的Laplacian矩阵，需要对原始图进行预处理，对每个
池化层，都根据池化层输入输出的规模对图做一次粗化。在预处理数据阶段，要根据设计好的神经网络的结构，
按照每一层的大小对图进行粗化处理，算出每一次粗化处理得到的图的Laplacian矩阵。之后做池化操作时，
使用粗化时预先计算好的近邻关系，做卷积操作时，使用粗化时预先计算好的Laplacian矩阵。

模型的改进
----------

### 图片采样为图

+ 输入：图片
+ 输出：图的邻接矩阵$A$、每个点的特征 $F_{ij}^{0}$

### 高阶模式的应用

+ 输入：基本图案（Motif）、图的邻接矩阵$A$、每个点的特征$F_{ij}^{0}$
+ 输出：新的图的邻接矩阵$A'$、每个点的特征${F^{'}}\_{ij}^{0}$

### Higher Order Originazation

论文[^6]通过利用一个图的某种特定模式的图案（Motif）定义了新的RatioCut

$$\phi_M(S) = \frac{cut_M(S, \bar{S})}{min[vol_M(S), vol_M(\bar{S}]}$$

并且从理论上证明了新定义的RatioCut仍然满足Cheeger不等式[^7]：

$$\phi_M(S) \le 4 \sqrt{\phi_M^{*}}$$

其中，$\phi_M(S)$是实际求得的最优解，$\phi_M^{*}$是理论最优解。


References
----------

[^1]: Spectral Networks and Locally Connected Networks on Graphs[iclr2014]
[^2]: Convolutional Neural Networks on Graphs with Fast Localized Spectral Filtering[nips2016]
[^3]: Wavelets on Graphs via Spectral Graph Theory
[^4]: Semi-supervised Classification with Graph Convolutional Networks[iclr2017]
[^5]: https://github.com/mdeff/cnn_graph/blob/master/lib/graph.py#L232
[^6]: Higher-order Organization of Complex Networks
[^7]: 原始的通过图的点之间的邻接矩阵定义的Laplacian矩阵的特征值满足 $$\frac{\phi_{*}^2}{2} \le \lambda_2 \le 2 \phi_{*}$$
