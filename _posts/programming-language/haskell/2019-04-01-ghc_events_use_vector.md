---
title: Use vector in ThreadScope to Reduce the Memory Usage
author: Tao He
date: 2019-04-01
tag: [Haskell, GSoC, Profiling]
category: 编程语言
layout: post
---

As described in the issue [ThreadScope#29][1], the ThreadScope is really slow
when loading a relatively huge eventlog file, and consumed an unacceptable volume
of memory. High memory usage caused by using incorrect data structure is
fairly common in many Haskell applications, thus at first glance, I focused
on the internal data structure that ThreadScope uses when browsing the source code.
I found that replacing `List` using `Vector` could bring a significant improvement.

<!--more-->

How ThreadScope Processing Events
---------------------------------

At first, we need to know how ThreadScope processing event logs. After the user
selects an eventlog file in the dialog window, ThreadScope starts reading the
event log using the `readEventLogFromFile` from the ghc-events package to load
the whole file and parse it into a sequence of `Event` data. ThreadScope will
sort those events by their `cap` field and partition events by their `cap`,
then build views for UI using `mkDurationTree`, `mkEventTree`, and `mkSparkTree`.

The processing on events is a serials of `map`, `filter`, `groupBy` and `sort`
operations. For a list that contains millions of events, the operation cannot
be every memory efficient and the performance won't be very good as well.
Therefore more efficient arrays should be used.

When the user drags the timeline bar, the state of timeline will be updated, and
corresponding parts of event information will be shown in the current view. All
events will always be kept in memory not matter what time range the user have
selected on timeline bar. This is the root cause that ThreadScope cannot handle
large event logs within limit memory.

Why List is Bad
---------------

`List` (or `[]`) is the most commonly used container to represent sequential data
in Haskell. The `List` is implemented using the _Linked List_ data structure. Its
implementation has been heavily optimized and the performance could even beat
_Linked List_ implemented using C! But the overhead is also obvious that many
there are more extra field and more indirections in `List`, compared to `Vector`,
especially when meets a large amount of elements.

From the last section, we know that ThreadScope needs to do many mapping, filtering,
grouping by, and even sorting operations on the event logs. It is data intensive.
Under such settings, the `List` is not a good choice. A compact data structure
could save a lot of memory and bump up the speed. Then I tried with the efficient
array package [`vector`][4].

ThreadScope uses the `ghc-events` package to read events from eventlog file and parse
the serialized bytes to structured data. `ghc-events` returns events as `[Event]`
thus, the first task is returning `Vector Event`, rather than `[Event]`, in ghc-events.
The work is straight forward and can be found in [2067776d64][2]:

+ Update the type signatures to use `Vector`
+ Replacing operations on `List` with its counterpart in the `vector` package
+ Resolve all compiler errors (the powerful type system of Haskell makes the refactor
  job so easy)

The main event processing logic is in ThreadScope, in `Events/ReadEvents.hs`. Thanks
to the power type checking of GHC again, now the task is to eliminate all type mismatch
errors after upgrade to the `vector` version of `ghc-events`. By replacing the
`map/reduce/groupBy/sort` on `List` with the same thing from the `vector` package, we
made a ThreadScope that backed by the more efficient array `Vector` without many difficulties.
The work can be found in [1ce3cde310][3].

Comparison and Conclusion
-------------------------

From the experiment on the prototype with commit [2067776d64][2] and [1ce3cde310][3],
for an eventlog file of size about 50MB, containing about 2,000,000 event records,
using `Vector` can reduce the memory usage from about 1.3 GB to 800 MB, that is a
great win!

The implementation is very preliminary and can be improved, indicates that there is
still a chance to optimize ThreadScope by using a more suitable data structure. However,
just doing that is not enough, since for eventlog file with GBs size, keeping all
event records all the time is a bad idea and the correct fix for it must be partial
loading, processing and rendering, and that will be the key of this GSoC project.

[1]: https://github.com/haskell/ThreadScope/issues/29
[2]: https://github.com/sighingnow/ghc-events/commit/2067776d64ea34a4300e18c6d5d717333a8efcb3
[3]: https://github.com/sighingnow/ThreadScope/commit/1ce3cde310eb3b9a678bcffda4d5854db005afb8
[4]: http://hackage.haskell.org/package/vector
