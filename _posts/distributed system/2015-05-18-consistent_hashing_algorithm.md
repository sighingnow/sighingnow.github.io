---
title: 一致性哈希算法
author: He Tao
date: 2015-05-18
tag: 分布式系统
category: 分布式系统
layout: post
---

在一个分布式服务系统中，当有一台Server宕机时，需要将其数据、服务等迁移到别的主机上。不难想到，可以对整个集群的任务重新做一次Hash，使得数据重复分布，但这样往往会造成大规模的数据迁移，并且在数据迁移完成之前所有的服务都不可用。一致性Hash算法很好的解决了这一问题。

<!--more-->

一致性Hash
-----------

针对ReHash的弊端，Karger提出了一种算法，即一致性哈希算法(Consistent Hashing Algorithm)，算法的核心是”虚拟节点”。其相比普通hash的主要优势在于在添加或移除节点时，保证尽量少的cache失效（数据迁移及均衡）。使用一致性Hash算法时，处理服务器的选择不再仅仅依赖key的hash本身而是将服务实例（节点）的配置也进行hash运算。在一致性Hash中，虚拟节点的过渡作用起到了很好的效果。

算法流程
--------

算法流程如下：

1. 假设Hash范围为`N`，首先求出每个服务节点的hash，并将其配置到一个0~N的圆环（continuum）区间上。
2. 其次使用同样的方法求出你所需要存储的key的hash，也将其配置到这个圆环（continuum）上。
3. 然后从数据映射到的位置开始顺时针查找，将数据保存到找到的第一个服务节点上。如果超过N仍然找不到服务节点，就会保存到第一个memcached服务节点上。

算法的改进
----------

### 负载均衡的考量

在经典一致性Hash算法中，只需要将服务不可用的虚拟节点重新迁移到下一台对应的可用的虚拟主机上即可。但这样的方便存在很大的缺陷，一台服务器宕机将会使得接收其数据的主机负载加倍，显然，这对于负载均衡是很不利的。

将故障节点Rehash，然后按照一致性Hash算法去寻找好几台主机共同接收故障节点的负载，这能够使得整个系统的负载重新做到相对均衡。

### 虚拟节点的扩展

在真实的系统情况下，相同部署的两套系统可能不能提供相同的服务，主要原因：

1. 硬件个体差异导致服务器性能不同。
2. 机房交换机和网络带宽导致IDC服务器之间的网络通信效率不同。
3. 用户使用不同的网络运营商导致电信IDC和联通IDC提供的服务性能不同。
4. 服务器所在网络或机房遭遇攻击。

所以完全相同的两套系统可能也需要提供差异化的服务，通过使用虚拟节点可以灵活的动态调整，达到系统服务的最优化。

一致性Hash的简单实现
--------------------

### 代码实现

```python
import random

def con_hash():
    '''
    Consistent Hashing Algorithm

    node: physical machine. count: 10
    vnode: virtual node. count: 7920
    '''
    node = [0 for i in range(0, 11)]
    vnode = [0 for j in range(0, 7921)]
    def hashcode(s):
        result, base = 0, 97
        for c in s:
            result += base*ord(c)
            base *= 97
        return result%7919

    def init_maps():
        for i in range(1, 11):
            for j in range(1, 792):
                vnode[random.randint(1,7920)] = random.randint(1, 10)
                # vnode[hashcode(str((i-1)*792+j))] = i
        for i in range(0, 7920):
            node[vnode[i]] += 1

    def add_test():
        node = [0 for i in range(0, 11)]
        for i in range(1235, 10000):
            target = hashcode(str(i))
            while vnode[target] == 0:
                target = (target+1)%7920
            node[vnode[target]] += 1
        print(node)

    def del_test():
        node = [0 for i in range(0, 11)]
        for i in range(4324, 12349):
            data = str(random.randint(0, 123456789))
            badnode = random.randint(1, 10)
            target = hashcode(data)
            while vnode[target] == 0 or vnode[target] == badnode:
                target = (target+1)%7920
            node[vnode[target]] += 1
        print(node)

    init_maps()
    add_test()
    del_test()

con_hash()
```

测试结果表明，算法效果确实很不错，在负载均衡上做的很好。

MurMurHash算法
---------------

MurmurHash 是一种非加密型哈希函数，适用于一般的哈希检索操作。由Austin
Appleby在2008年发明，并出现了多个变种，都已经发布到了公有领域(public
domain)。与其它流行的哈希函数相比，对于规律性较强的key，MurmurHash的随机分布特征表现更良好。这个算法已经被若干开源计划所采纳，最重要的有libstdc++
(4.6版)、Perl、nginx (不早于1.0.1版)、Rubinius、 libmemcached
(Memcached的C语言客户端驱动)、maatkit、Hadoop、Kyoto
Cabinet以及RaptorDB。与CRC32，MD5，SHA-1等加密算法相比，MurmurHash算法的效率较高，碰撞率也很低。算法的Java实现：

```java
private Long hash(String key) {
    ByteBuffer buf = ByteBuffer.wrap(key.getBytes());
    int seed = 0x1234ABCD;
    ByteOrder byteOrder = buf.order();
    buf.order(ByteOrder.LITTLE_ENDIAN);
    long m = 0xc6a4a7935bd1e995L;
    int r = 47;

    long h = seed ^ (buf.remaining() * m);
    long k;

    while (buf.remaining() >= 8) {
        k = buf.getLong();
        k *= m;
        k ^= k >>> r;
        k *= m;
        h ^= k;
        h *= m;
    }
    if (buf.remaining() > 0) {
        ByteBuffer finish = ByteBuffer.allocate(8).order(ByteOrder.LITTLE_ENDIAN);
        // for big-endian version, do this first:
        // finish.position(8-buf.remaining());
        finish.put(buf).rewind();
        h ^= finish.getLong();
        h *= m;
    }
    h ^= h >>> r;
    h *= m;
    h ^= h >>> r;

    buf.order(byteOrder);
    return h;
}
```

参考
----

1. [一致性哈希算法原理设计](http://blog.jobbole.com/80334/)
2. [Dynamo: Amazon’s Highly Available Key-value Store](http://www.allthingsdistributed.com/files/amazon-dynamo-sosp2007.pdf)
3. [MurmurHash](https://sites.google.com/site/murmurhash/)
4. [Murmur哈希](http://zh.wikipedia.org/wiki/Murmur%E5%93%88%E5%B8%8C)
5. [理解一致性哈希算法(consistent hashing)](http://blog.csdn.net/cywosp/article/details/23397179)

