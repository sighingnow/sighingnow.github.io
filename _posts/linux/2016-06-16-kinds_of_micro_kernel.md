---
title: Kinds of (Micro)Kernel
author: sighingnow
date: 2016-06-16
tags: [Linux, Kernel]
category: Linux
layout: post
---

There are many different types of Operating System kernels: the monolithic kernel, the microkernel, and
the newly appeared Exokernel as well as Unikernel.

<!--more-->

+ Monolithic kernel

    Example: Unix, Linux, DOS, Windows

    A monolithic kernel is an operating system architecture where the entire operating system is working in
    kernel space and is alone in supervisor mode.

    - High-level virtual interface over computer hardware.
    - A set of primitives or system calls.
    - Device drivers as modules(loadable modules) added to the kernel.

+ Microkernel

    Example: Mach, Minix, L4, seL4

    A microkernel is the near-minimum amount of software that can provide the machanisms(low-level address
    space management, thread management, inter-process communication(IPC)) needed to implement an operating
    system.

    - Tradictional operating system functions, such as device drivers, protocol stacks and file systems, are
    run in user space as services.

+ Nanokernel

    Example: KeyKOS

    The term _nano_, refers to one that supports a nanosecond clcok resolution.

    - The total amount of kernel code (executing in the privileged mode of hardware) is very small.
    - A virtualization layer underneath an operating system.
    - A hardware abstration layer that forms the lowest-level part of a kernel, can provide real-time
    functionlity to normal operating system.

+ Exokernel

    Example: JOS (MIT exokernel), Memesis, Minix 3 (some ideas of exokernel)

    - Operating systems generally present hardware resources to applications through high-level abstration
    such as **virtual file system**. Idea: force as few abstrations as possible on application developers,
    enabling them to make as many decisions as possible about hardware abstrations. Simpler than conventional
    microkernels' implementation of message passing and monolithic kernels implementation of high-level
    abstrations.

+ Unikernel

    [http://unikernel.org/](http://unikernel.org/)

    Specialised, single address apce machine images constructed by using **library operating systems**.

    Unikernel是通过使用专门的库操作系统来构建的单地址空间机器镜像。开发者通过选择栈模块和一系列最小依赖库来
    运行应用，而这些栈和库对应于操作系统中运行应用所必需的依赖。这些库负责应用和配置代码编译，构建成封闭的、
    固定用途的镜像（Unikernel）可以直接在虚拟机管理程序（hypervisor）或硬件上运行，不需要类似Linux或Windows
    的操作系统介于其中。

    删除应用与硬件中间臃肿的部分。让最“精简”的操作系统运行你的代码。

+ Hybrid kernel

    Example: ReactOS Kernel, XNU kernel, NT Kernel (emulatin subsystems run in user-mode server processes,
    rather than kernle mode (the large number of design goals which resemble design goals of Mach))

    A hybrid kernel is an operating system kernel architecture that attempts to combine aspects and benefits
    of microkernel and monolithic kernel architectures used in computer operating systems.


