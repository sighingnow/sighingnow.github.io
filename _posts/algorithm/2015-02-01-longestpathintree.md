---
title: 树的最长路径
author: DHDave
date: 2015-02-01
tag: Algorithm
category: Algorithm
layout: post
---

## 一、概述

树的最长路问题是一类求解树上两点之间最长距离的问题。针对此问题，有这样两类算法：DFS求解和树形DP。本文将以 [HihoCoder 1050 : 树中的最长路][1] 一题为例，详细阐述这两种解法。

<!--more-->

## 二、树形DP

树形DP的基本思路为由子节点的情况推出父节点的情况，针对树的最长路径这一问题，分别记录每个节点的子节点的最大深度和次大深度，父节点的最大深度等于所有子节点的最大深度和次大深度的最大值加1，父节点的次大深度等于所有子节点的最大深度和次大深度的次大值加1，最后，每个节点对应的最长路径值为该节点的`最大深度-次大深度+1`,由此可得到树的最长路径长度。伪代码描述如下：

```cpp
初始值：
    depth_0[leaf node] = depth_1[leaf node] = 1
递推：
    depth_0[parent] = max({depth_0[{sons}], depth_1[{sons}]})
    depth_1[parent] = max({depth_0[{sons}], depth_1[{sons}]} - depth_0[parent])
求解：
    length[i] = depth_0[i] + depth_1[i] - 1
    longest = max({length[i]})
```

由此，得到结果。

## 三、DFS求解

此题通过DFS求解的算法正确性基于以下性质：

以树上的任意一点为根节点，距离根节点最远的点一定是树的最长路径的一个端点。

证明：假设 s-t这条路径为树的最长路径，分以下两种情况证明：
1. 设u为s-t路径上的一点，结论显然成立，否则设搜到的最远点为T则`dis(u,T) >dis(u,s)`且`dis(u,T)>dis(u,t)`，则最长路不是s-t了，与假设矛盾。
2. 设u不为s-t路径上的点，首先明确，假如u走到了s-t路径上的一点，那么接下来的路径肯定都在s-t上了，而且终点为s或t，在1中已经证明过了。
所以现在又有两种情况了：
1：u走到了s-t路径上的某点，假设为X，最后肯定走到某个端点，假设是t ，则路径总长度为`dis(u,X)+dis(X,t)`
2：u走到最远点的路径u-T与s-t无交点，则`dis(u-T)>dis(u,X)+dis(X,t)`显然，如果这个式子成立，
则`dis(u,T)+dis(s,X)+dis(u,X)>dis(s,X)+dis(X,t)=dis(s,t)`最长路不是s-t矛盾。

由此上性质，得到如下解法：从任意一点对树DFS，找出深度最大的点，该点即为树的最长路径的一个端点。再从该点出发，对树进行一次DFS，此时得到的深度最大的点，该点即为树的最长路径的另一个端点，此时改点的深度值即为树的最长路径的长度。

## 四、代码实现

[HihoCoder 1050][2]

```cpp
#include <cstdio>
#include <vector>
#include <cstring>
#include <queue>
using namespace std;

vector<int> e[100100];
bool flag[100100];
int depth[100100], ans, n;

int bfs(int start) {
    memset(flag, 0x00, sizeof(flag));
    memset(depth, 0x00, sizeof(depth));
    queue<int> Q;
    Q.push(start);
    flag[start] = true;
    while(!Q.empty()) { 
        int u = Q.front();
        Q.pop();
        for(int i = 0; i < e[u].size(); ++i) {
            if(!flag[e[u][i]]) {
                depth[e[u][i]] = depth[u]+1;
                flag[e[u][i]] = true;
                Q.push(e[u][i]);
            }
        }
    }
    int point = -1;
    ans = -1;
    for(int i = 1; i <= n; ++i) {
        if(depth[i] > ans) {
            ans = depth[i];
            point = i;
        }
    }
    return point; // 返回距离最远的点的编号
}

int main(int argc, char **argv) {
    int x, y;
    scanf("%d", &n);
    for(int i = 1; i < n; ++i) {
        scanf("%d %d", &x, &y);
        e[x].push_back(y);
        e[y].push_back(x);
    }
    bfs(bfs(1));
    printf("%d", ans);

    return 0;
}
```

[1]: http://hihocoder.com/problemset/problem/1050
[2]: http://hihocoder.com/problemset/problem/1050