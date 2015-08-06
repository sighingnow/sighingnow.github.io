---
title: Haskell Functors(函子)
author: He Tao
date: 2015-08-03
tag: [Haskell]
category: 编程语言
layout: post
---

Functors 是可以被 **map over** 的对象，像是 lists，Maybe，trees 等等。在 Haskell 中我们是用 Functor 这个 typeclass 来描述他。这个 typeclass 只有一个 method，叫做 fmap，他的型态是 `fmap :: (a -> b) -> f a -> f b`。

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

<!--more-->

如果一个 type constructor 要是 Functor 的 instance，那

