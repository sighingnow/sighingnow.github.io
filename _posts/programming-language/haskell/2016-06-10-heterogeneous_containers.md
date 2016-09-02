
---
title: Heterogeneous containers in Haskell
author: sighingnow
date: 2016-06-10
tag: [Haskell]
category: 编程语言
layout: post
---

<!--more-->

HList
-----

Extensions:

~~~haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
~~~

Definition:

~~~haskell
data HList :: [*] -> * where
    HNil :: HList '[]
    HCons :: a -> HList as -> HList (a ': as)
~~~

Usage:

~~~haskell
xs :: HList [Int, Bool]
xs = HCons 3 . HCons True $ HNil
~~~


