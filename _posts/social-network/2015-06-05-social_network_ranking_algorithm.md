---
title: 社交网站站内排名与社区演化
author: He Tao
date: 2015-05-13
tag: [排名, 社会网络]
category: 社会网络
layout: post
---

社交网站站内排名一直是一个热门的话题，一套好的排名机制有助于将社区中精华的内容(如博客、帖子、评论等)优先推送给网站用户，从而有效促进社区的良性发展。本文将以[Reddit][1]和[知乎][2]这两个热门社交网站为例，分析其站内排名算法的优缺点，并探讨其对整个社区内社团形成以及同质化的作用和影响。

知乎排名算法
------------

知乎是一个基于问答的社交网站，以“创造有价值的内容”和“保持友善和尊重”为核心原则：

> 创造有价值的内容：知乎的初衷是帮助人们可以更好的分享彼此的知识、经验和见解，发表有用、有帮助、有质量的内容，不仅可以帮助他人，也会让自己获益。

> 保持友善和尊重：不要攻击、故意贬低用户或者他写的内容，尊重不同的观点，不恶意揣测动机。

<!--more-->

如今，知乎已经是一个有着几百万活跃用户、几万个话题和几千万个问题的大型社交网站，知乎的话题涵盖了IT、自然科学、工程、数学、历史、政治、经济等多方面的话题，也有很多非常优秀的用户在这儿贡献自己的才华和智慧。对于这样一个网站来说，一套优秀的站内排名算法是至关重要的。

知乎采取的是基于用户投票的[威尔逊算法][3]。算法描述如下：

$$n = u+v$$
$$p = u/n$$
$$Score = (p+\frac{z_{\alpha}^{2}}{2n}-\frac{z_{\alpha}}{2n}\sqrt{4n(1-p)p+z_{\alpha}^{2}})/(1+\frac{z_{\alpha}^{2}}{n})$$

其中，$u$为加权赞同票数，$v$为加权反对票数，$z_{\alpha}$为参数。

这套算法遵循了一下原则：

1. 所有用户看到的排序是相同的
2. 获得赞同会使回答的排序上升，获得反对则会下降
3. 在某个领域下（根据问题添加的话题区分）的好回答会提高用户在该领域下的投票权重
4. 领域下高权重用户的投票对排序有更大影响，他们的回答排序也更高

从这套算法的描述中，我们不难看出，用户的权重对于最终回答的得分有着很大的影响。那么，用户的权重又是如何计算出来的呢？

知乎对于用户权重的计算遵循以下两条原则：

1. 在知乎上创作了专业、严谨、认真的高质量回答的人，应该在他/她擅长的领域里，有更大的判断力。权重则是这个判断力的体现。
2. 用户在一系列相关话题下发布的全部回答所得到赞同、反对、没有帮助票数决定用户在该领域下的权重

这也就是说，用户在某个问题下的权重，是根据他过去在相关话题下的回答得到的赞同、反对和没有帮助票数计算的。**用户过去回答的得票，体现了其他用户在相关领域下对他能力的认可程序**。这样的排名系统，更加有利于将行业内(话题)的高手显现出来，并获得更多用户的关注。而在这样一个趋势下，用户会重新定位自己在整个社交平台甚至是在自己所在行业里的水平和地位。大多数用户最终会团聚在一些大牛周围，并根据专业和兴趣形成一个一个的圈子，圈子的人会慢慢同质化，并在知乎这个大平台下形成较小的、相对封闭的群落。

Reddit排名算法
--------------

与知乎类似，Reddit是全美最大的问答社区，其主题同样涵盖了各个学科的内容。Reddit的站内排名算法是根据赞成票与反对票的相对比例来决定的，Reddit采用的算法并没有过多考虑用户权重，却加上了时间对排名的影响，或许是因为Reddit更倾向于想用户呈现较新的内容吧。Reddit所使用的排名算法如下：

```python
#Rewritten code from /r2/r2/lib/db/_sorts.pyx
 
from datetime import datetime, timedelta
from math import log
from pylons import g

epoch = datetime(1970, 1, 1, tzinfo = g.tz)
 
def epoch_seconds(date):
    """Returns the number of seconds from the epoch to date."""
    td = date - epoch
    return td.days * 86400 + td.seconds + (float(td.microseconds) / 1000000)
 
def score(ups, downs):
    return ups - downs
 
def hot(ups, downs, date):
    """The hot formula. Should match the equivalent function in postgres."""
    s = score(ups, downs)
    order = log(max(abs(s), 1), 10)
    sign = 1 if s > 0 else -1 if s < 0 else 0
    seconds = epoch_seconds(date) - 1134028003
    return round(order + sign * seconds / 45000, 7)
```

(代码来源：[https://github.com/reddit/reddit/blob/master/r2/r2/lib/db/_sorts.pyx][5])

Reddit采用的算法考虑了这样几个因素:

1. 帖子的新旧程度

        t = 发贴时间 – 2005年12月8日7:46:43 (1134028003)

2. 赞成票与反对票之间的差值

        x = 赞成票 - 反对票

3. 投票方向(符号变量，对文章的总体看法，如果)

        y = 1 if x > 0 else -1 if x < 0 else 0

4. 帖子受肯定(否定)的程度

        z = abs(x) if x != 0 else 1

帖子最终的得分为：

$$Score = log_{10}{z} + \frac{yt}{45000}$$

从这个公式中，我们可以看出以下两点：

1. 赞成票与反对票的差额$z$越大，得分越高。
2. 新帖子得分高于老帖子。
3. 对于那些有争议的帖子(赞同数接近反对数)的帖子不可能拍到前面。

从上述分析中，我们不难看出这套排名系统中还有很多考虑并不是很周到的地方，例如，这个算法基本没有考虑机器批量投票、刷票、拉票等行为对社会的负面影响，也有很多评论在分析Reddit在这方面的不足之处。

排名算法的作用
--------------

诸如知乎、Reddit等社交网站之所以要费心思设计排名算法，根源在于维护社区内容的优质性。如今，诸如贴吧、论坛等社交平台早已水军泛滥，而作为一个知识共享平台，只有不断提升真正有实力的用户的地位，防止拉票、机器自动投票等行为，才能不断吸引更多的用户加入社区，不断壮大社区。

排名机制在重新塑造着一个社交网站的秩序，而由此引发的网络动力学特征也值得我们关注。网络打破了物理隔离，而这样的平台能帮助我们去找到水平相近、志同道合的伙伴，有这些交际引发的连锁反应也必将影响真实社会(线下)网络结构的演变。

参考
----

1. [威尔逊得分][3]
2. [How Reddit ranking algorithms work][4]
3. [Reddit’s empire is founded on a flawed algorithm][6]

<!-------------------links------------------------>

[1]: http://www.reddit.com
[2]: http://www.zhihu.com
[3]: http://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Wilson_score_interval
[4]: http://amix.dk/blog/post/19588
[5]: https://github.com/reddit/reddit/blob/master/r2/r2/lib/db/_sorts.pyx
[6]: http://technotes.iangreenleaf.com/posts/2013-12-09-reddits-empire-is-built-on-a-flawed-algorithm.html

