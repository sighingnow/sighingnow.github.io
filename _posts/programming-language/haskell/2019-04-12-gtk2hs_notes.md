---
title: Notes on Gtk2Hs
author: Tao He
date: 2019-04-12
tag: [Haskell, GUI, GSoC]
category: 编程语言
layout: post
---

Gtk2Hs is a GUI library for Haskell, based on GTK+, for creating graphical user interfaces.
This page records some (trivial) notes about GUI programming in Haskell with the Gtk2Hs library.

<!--more-->

Hello World with GTK
--------------------

```haskell
import Graphics.UI.Gtk

main :: IO ()
main = do
    initGUI
    window <- windowNew

    widgetShowAll window
    -- onDestroy window mainQuit
    onDelete window (const (mainQuit >> return True))
    mainGUI
```

The `initGUI` must be called *once* before any other Gtk2Hs functions. `widgetsShowAll window`
completes the internal steps and allocations, and show `window`, as well as its children.
`mainGUI` is the infinite event loop, and event loop will block until the window get destroyed
since there's a callback `mainQuit` for `onDelete` event of the `window`.

The Gtk main event loop can be put into another separate OS thread. In such scenarios, something
like `MVar` may be involved to synchronize the Gtk thread and the main thread of the program.

Signal
------

In GTK+, events are called _signals_. The object, event and event handler can be connected with
`on`:

```haskell
on ::
  object
  -> Signal object callback -> callback -> IO (ConnectId object)
    -- Defined in ‘glib-0.13.6.0:System.Glib.Signals’
```

Unlike `on`, `after` has the same type signature and functionality with `on`, except that the
`callback` is executed after Gtk's default handler has run.

Packing Boxes
-------------

The horizontal box or vertical box can be created by `hBoxNew` or `vBoxNew`. A `Bool` parameter
indicates whether gives all children equal space allotments. `boxPackStart` place objects into the
container start from the top, or left, to the bottom, or right and `boxPackEnd` works along the
reverse direction. A parameter with type `Packing` is required to determine how the child scales.

+ `PackNatural` means the child is as big as it requests.
+ `PackRepel` will be padded on both sides with additional space.
+ `PackGrow` increases the size of widgets so that it covers the available space.


