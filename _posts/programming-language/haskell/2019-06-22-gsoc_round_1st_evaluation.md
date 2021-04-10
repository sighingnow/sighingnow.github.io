---
title: GSoC 2019 Round 1st Evaluation
author: Tao He
date: 2019-06-22
tag: [Haskell, GUI, GSoC]
category: 编程语言
layout: post
---

It's time to summarize current progress during the first round of GSoC 2019. I have finished
a tool to indexing event logs into SQLite at a relative acceptable speed and worked out a
prototype that can leverage SQLite to reduce the burden of memory management for large
eventlog files, achieving the goal of the first round in my original proposal.

<!--more-->

Fixing the Parser of ghc-events
-------------------------------

There was a long-standing [bug][1] in ghc-events that this package cannot handle the
`par_balanced_copied` field of the `EVENT_GC_STATS_GHC` event. This field was added
in ghc-8.4.1, that means ghc-events and ThreadScope has been broken for every version
of ghc compiler since 8.4.1.

The fix for the bug is trivial after understanding how `mkEventTypeParsers` works: adding
another `FixedSizeParser` for `EVENT_GC_STATS_GHC` event that consumes 58 bits, and declare
a new `Maybe` field in `GCStatsGHC` to contain the newly added `par_balanced_copied` value,
and when the decoder meets an eventlog file that claims the `EVENT_GC_STATS_GHC` event has
58 bits, the new parser will be chose correctly.

```
(FixedSizeParser EVENT_GC_STATS_GHC (sz_capset + 2 + 5*8 + 4 + 8) (do  -- (heap_capset, generation, copied_bytes, slop_bytes, frag_bytes, par_n_threads, par_max_copied, par_tot_copied, par_balanced_copied)
    heapCapset   <- get
    gen          <- get :: Get Word16
    copied       <- get :: Get Word64
    slop         <- get :: Get Word64
    frag         <- get :: Get Word64
    parNThreads  <- get :: Get Word32
    parMaxCopied <- get :: Get Word64
    parTotCopied <- get :: Get Word64
    parBalancedCopied <- get :: Get Word64
    return GCStatsGHC{ gen = fromIntegral gen
                     , parNThreads = fromIntegral parNThreads
                     , parBalancedCopied = Just parBalancedCopied
                     , ..}
)),
```

This patch has been organized as a pull request ([ghc-events#47][2]) and has been merged
by the maintainer. After the fix, a new version of ghc-events (v0.9.0) has been released.

SQLite and ghc-events
---------------------

In the proposal, we have a plan that leveraging the SQLite to managing events for us and
ThreadScope can read parts of events from SQLite in a specified order, then build the view
for users. Currently, I have resolved the challenge of indexing huge eventlog file into
the SQLite database at a reasonable speed and have worked out a prototype of ThreadScope
that loading events from SQLite. This part of work can be split into two subtasks:
indexing event logs and query from SQLite. The former has been organized into a separate
repository [ghc-events-sqlite][3]. The later has been pushed to my fork but hasn't
been submitted to the upstream repository yet.

#### The Performance of SQLite

In the beginning, the performance of SQLite is not very promising. Our preliminary test
shows that it requires about 2 minutes to indexing about 2,000,000 event logs into a
temporary SQLite database in ramdisk. However, the size of the eventlog file is only about
50MB, and the performance is even worse than loading the whole eventlog into ThreadScope!

After detailed debugging and experiments, I found that the speed of indexing could be
optimized by tweaking the settings of ThreadScope database. After adjusting the parameter
`journal_mode` as `MEMORY` and turning `synchronous` OFF, now only about 4 seconds is
used to indexing a 50-MB eventlog to SQLite DB. Compared the previous result (2 minutes),
the performance of indexing is acceptable now.

#### Query Events from SQLite

In the previous implementation, a list of events will be sorted and partitioned with their
`cap` property and will be displayed in GUI along with their timestamps. When querying
events from SQLite, we can leverage the database engine to perform sorting and
return the sorted result set directly.

I set up an index in SQLite when creating the table, indexing on the `cap` and `timestamp`
property.

```haskell
DB.execute_ conn "CREATE TABLE evts (id INTEGER PRIMARY KEY, timestamp INTEGER, cap INTEGER, etype INTEGER, einfo TEXT);"
DB.execute_ conn "CREATE INDEX evts_cap_timestamp ON evts (cap, timestamp);"
```

In ThreadScope, when we need to load events on a given capability within the given range
of time, we need to do something like

```haskell
DB.query conn "select timestamp, etype, einfo from evts where cap = ? and timestamp >= ? and timestamp <= ? ORDER by timestamp ASC" (cap, start_ts, end_ts)
```

The result set is guaranteed sorted by the timestamp field, and there is no need sorting
the `List` (or `Vector`) again in Haskell code, thus reduce the memory usage.
My experiments shows that when query events on a given capability, the index could
reduce the time from about 0.5 seconds to about 0.15 seconds. By using the `EXPLAIN QUERY
PLAN` query I can confirm that the index is used when executing the event query.

#### Put Things Together

There is already a prototype that loading events from the SQLite database and build the
view for users, but there are still many problems with the current design. The 0.1 second
that consumed by the query is still much slower, and the user can feel a pause when drag
the timeline bar, compared with the original design that keeping all event records in
memory.

The view is still incorrect in this prototype. The biggest challenge is about how to
build a partial view on GUI correctly. Many kinds of events, like `ThreadStart` and
`ThreadStop`, need to be grouped as pairs when displaying them on GUI. However, when
constructing the view with events that within the given range directly, such context
may be lost because of the corresponding _start_ or _stop_ lies out of the selected time range.

Plan for the Next Round
-----------------------

My work during the first round shows that our SQLite plan is promising to manage
event logs and could reducing the burden like partial file reading (and parsing) as
well as memory management. Basically, I have achieved the goal of the first round
development in my proposal.

I will address the issue of incorrect partial view during the second round development.
Another challenge is that we still need to improve the performance of querying
from SQLite. Querying events for a time span within about 0.15 seconds sounds promising,
but we still could feel a significant pause when dragging the timeline bar to change the
view. Something like caching may help, and the settings of SQLite may need more
careful tweaks.

[1]: https://github.com/haskell/ghc-events/issues/31
[2]: https://github.com/haskell/ghc-events/pull/47
[3]: https://github.com/sighingnow/ghc-events-sqlite
