---
title: Hello World using the eBPF
author: Tao He
date: 2021-04-09
tag: [Linux, eBPF]
category: Linux
layout: post
---

eBPF is a in-kernel register-based virtual machine that is capable to executing native-fast
JIT-compiled BPF programs inside the Linux kernel with access to a subset of kernel functions
and memory.

<!--more-->

History of eBPF
---------------

BPF (aka. BSD Packet Filter) was originally designed and developed to capture and filter
network packets that matched specific rules[^1]. The process is done in kernel without copying the
packet to the user space and then run filters. The BPF then was extended to more than packet
filtering, as eBPF, to monitor, trace and monitor the behavior of the kernel[^2][^3], and has
been exposed to user space.

eBPF provides a way to run mini programs on system and application events, such as IO. It makes
the kernel fully programmable and the eBPF instructions are executed by the Linux kernel, which
includes an interpreter and a JIT compiler.

Classical BPF arrived in Linux in 1997. A BPF JIT compiler was added in 2011, since Linux 3.0.
BPF was first used outside of networking in 2012, when BPF filters was added for `seccomp` syscall
policies. In 2013, the "extended BFP" patchset came out, and been merged, and today's eBPF has
little to do with Berkeley, packets, or filtering.

eBPF Tooling
------------

BCC is a toolkit that makes use of eBPF for creating kernel tracing and manipulation programs,
providing a C programming environment for writing kernel BPF code.

Note that BCC requires the `linux-headers` package available thus on WSL2 (as to Apr, 2021),
developers need to manually build the kernel to make it work, with enabling the configuration
`CONFIG_IKHEADERS=m`. The `debugfs` is also needed and can be mounted by

```bash
sudo mount -t debugfs debugfs /sys/kernel/debug
```

Or, by adding the configuration to `/etcd/fstab`:

```
debugfs    /sys/kernel/debug      debugfs  defaults  0 0
```

bpftrace is another BPF frontend that it ideal for powerful one-liners scripts, as well as ply.

Tracing using eBFP
-------------------

Dynamic tracing was first created in the 1990s, was first developed for Linux and was finally
added to Linux kernel in 2004. Sun developed the DTrace in the Solaris operating system and
made it a well-known and in-demand features. Linux supports dynamic instrumentation for both
kernel-level functions and user-level functions, via `kprobe` and `uprobe` respectively, as
demonstrated in the following examples[^4]:

| probe                          | affect                                                                         |
|:------------------------------:|:------------------------------------------------------------------------------:|
| `kprobe:vfs_read`              | instrument at the beginning of `vfs_read()`                                    |
| `kretprobe:vfs_read`           | instrument at the end of `vfs_read()`                                          |
| `uprobe:/bin/bash:readline`    | instrument at the beginning of user-level function `readline()` in `/bin/bash` |
| `uretprobe:/bin/bash:readline` | instrument at the end of user-level function `readline()` in `/bin/bash`       |

Static tracing in Linux is supported by tracepoint (for kernel static instrumentation) and USDT
(user-level static defined tracing) for user-level static instrumentation. Static tracing requires
extra definition in the source code thus becomes a maintenance burden for the developers. The
tracepoint and USDT can be used as the following examples[^4]:

|:-----------------------------------------:|:----------------------------------------------:|
| `tracepoint:syscalls:sys_enter_open`      | instrument the `open(2)` system call           |
| `usdt:/usr/sbin/mysqld:mysql:query_start` | instrument the `query_start` function in MySQL |

The tracepoints and USDTs are only activated and running after the BPF program was attached
and are deactivated once the BPF program was removed.


References
----------

[^1]: https://www.tcpdump.org/papers/bpf-usenix93.pdf
[^2]: A thorough introduction to eBPF, [https://lwn.net/Articles/740157/](https://lwn.net/Articles/740157/)
[^3]: BPF: the universal in-kernel virtual machine, [https://lwn.net/Articles/599755/](https://lwn.net/Articles/599755/)
[^4]: These examples are referred from the book _BFP Performances Tools_, by Brendan Gregg.
