---
title: 包含根节点的最大权值子树
author: Tao He
date: 2015-01-25
tag: Algorithm
category: Algorithm
layout: post
---

树上每个点都有一个权值，找出与根节点连通的m个点，使得这m个点的权值之和最大。

题目链接: [HihoCoder 1055 刷油漆](http://hihocoder.com/problemset/problem/1055)

<!--more-->

分析
---------

### 基本思路

树上的动态规划。

### 具体解法

设 $f[t][m]$ 表示以 $t$ 为根节点，包括 $t$ 节点在内的 $m$ 个节点的权值总和的最大值，那么题目要求的便是 $f[1][m]$ 的值。

假设t节点有孩子节点 $tc$, 那么以 $tc$ 为根又可以构成一棵子树，并且在该子树上最多取 $m-1$ 个节点，对于 $f[t][m]$ 来说，有如下的状态转移方程:

$$f[t][m] = max\{f[t][m], f[t][m-m_{tc}]+f[tc][m_{tc}]\}$$

其中，$m_{tc}$ 的取值范围为

$$1, 2, 3, \dots, m-1$$

$m$ 的取值范围为

$$m, m-1, m-2, \dots, 2$$

由此，对整棵树进行一次后序遍历，每遍历玩一个根节点，便对该根节点做一次dp，由此，便可以得到 $f[1][m]$ 的值。

代码实现
-------

~~~cpp
#include <iostream>
#include <cstdio>
#include <algorithm>
using namespace std;

/**
 * author: Tao He
 */

const int max_n = 110;
int v[max_n] = {0};
int graph[max_n][max_n];
int f[max_n][max_n]; // f[t][j]以t为根的包括t在内的j个节点的得分
int n, m;

void dp(int pos, int father) {
    f[pos][1] = v[pos];
    for(int i = 1; i <= n; ++i) {
        if(i != father && graph[pos][i] == 1) {
            for(int mm = m; mm >= 2; --mm) {
                for(int m_tc = 1; m_tc < mm; m_tc++) {
                    f[pos][mm] = max(f[pos][mm],
                            f[pos][mm-m_tc] + f[i][m_tc]);
                }
            }
        }
    }
}

void post_order(int pos, int father) {
    for(int i = 1; i <= n; ++i) {
        if(i != father && graph[pos][i] == 1) {
            post_order(i, pos);
        }
    }
    dp(pos, father);
}

int main(int argc, char *argv[])
{
	scanf("%d %d", &n, &m);
    int x, y;
    for(int i = 1; i <= n; ++i) {
        scanf("%d", v+i);
    }
    for(int i = 1; i < n; ++i) {
        scanf("%d %d", &x, &y);
        graph[x][y] = graph[y][x] = 1;
    }
    post_order(1, -1);
    printf("%d\n", f[1][m]);
    return 0;
}

/* vim: set ts=4, sw = 4 */
~~~
