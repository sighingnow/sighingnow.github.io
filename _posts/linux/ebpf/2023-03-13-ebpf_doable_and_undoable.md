---
title: eBPF Doable and Undoable
author: Tao He
date: 2023-03-13
tag: [Linux, eBPF]
category: Linux
layout: post
---

eBPF is a powerful tool for Linux kernel tracing and observability. Compared with
kernel module, eBPF is more flexible and easier to develop and deploy. However,
eBPF lacks many key features that kernel module has, and cannot be used as the
replacement for kernel module framework.

<!--more-->

Doable
------

One of the most important features of eBPF is tracing, including tracepoints and kprobe
for kernel functions, and userspace tracepoints and uprobes for user-space functions.
The measurement recorded in the eBPF program can be exposed to userspace driver programs
using channels like BPF maps.

### kernel-space tracepoints

Linux provides a set of tracepoints to trace kernel functions, e.g., `sys_enter_execve`
and `sys_exit_execve`.

```c
SEC("tracepoint/syscalls/sys_enter_execve")
int tracepoint_sys_enter_execve(struct sys_enter_execve_args *ctx) {
    char comm[TASK_COMM_LEN] = {0};
    bpf_get_current_comm(comm, sizeof(comm));

    bpf_printk("tracepoint/sys_enter_execve: comm=%s (%s)", comm,
               ctx->filename);
    return 0;
}
```

The full list of tracepoints can be found by inspecting `/sys/kernel/debug/tracing/events`.

### Kernel-space probes

Tracepoints requires being exported from the kernel source code. To observe
arbitrary kernel functions, eBPF provides kprobes. The kprobe can be attached
to any kernel functions and will be triggered when the function is called.

```c
SEC("kprobe/__x64_sys_execve")
int kprobe_sys_execve(struct pt_regs *ctx) {
    char comm[TASK_COMM_LEN] = {0};
    bpf_get_current_comm(comm, sizeof(comm));
    bpf_printk("kprobe/sys_execve: comm=%s (%s)", comm);
    return 0;
}
```

Compared with predefined tracepoints, kprobes passes the kernel function
arguments in a `struct pt_regs *` struct and requires extra work to extract
the arguments using `bpf_probe_read` as it requires accessing kernel memory.

```c
    const char *filename = NULL;
    const char *const *argv = NULL;
    const char *const *envp = NULL;

    bpf_probe_read(&filename, sizeof(filename) /* read the pointer */,
                   &PT_REGS_PARM1(__ctx));
    bpf_probe_read(&argv, sizeof(argv) /* read the pointer */,
                   &PT_REGS_PARM2(__ctx));
    bpf_probe_read(&envp, sizeof(envp) /* read the pointer */,
                   &PT_REGS_PARM3(__ctx));
```

In the example above, pointers in `argv` and `envp` still in the kernel
memory and requires `bpf_probe_read` to access them.

```c
    for (int i = 0; i < 128; i++) {
        const char *arg = NULL;
        bpf_probe_read(&arg, sizeof(arg) /* read the pointer */, &argv[i]);
        if (!arg) {
            break;
        }
        bpf_printk("kprobe/sys_execve: argv[%d] = %s", i, arg);
    }
```

Linux 4.17 [added a configuration entry `CONFIG_ARCH_HAS_SYSCALL_WRAPPER`][1] and
defaults to `y` for x86_64, adding an extra indirection to syscall function
arguments,

```c
    struct pt_regs *__ctx = (struct pt_regs *) PT_REGS_PARM1(ctx);
```

See also the effort to automatically generate the function argument extraction code
in [the BCC project][2].

### Userspace tracepoints

Userspace tracepoints are similar to kernel-space tracepoints, but they are
defined in userspace and can be triggered by userspace programs. The userspace
tracepoints can be listed using the `tplist.py` tools in BCC:

```bash
$ ~/bcc/tools/tplist.py -l /lib/x86_64-linux-gnu/libc.so.6
/lib/x86_64-linux-gnu/libc.so.6 libc:setjmp
/lib/x86_64-linux-gnu/libc.so.6 libc:longjmp
/lib/x86_64-linux-gnu/libc.so.6 libc:longjmp_target
...
```

The userspace tracepoints can be hooked by eBPF programs like kernel-space
tracepoints.

```c
SEC("usdt/libc.so.6:libc:mutex_acquired")
int usdt_libc_mutex_acquired(struct pt_regs *ctx) {
    bpf_printk("usdt/libc:mutex_acquired: process=%d",
               bpf_get_current_pid_tgid());
    return 0;
}

SEC("usdt/libc.so.6:libc:mutex_release")
int usdt_libc_mutex_released(struct pt_regs *ctx) {
    bpf_printk("usdt/libc:mutex_release: process=%d",
               bpf_get_current_pid_tgid());
    return 0;
}
```

The program above will react to events that `mutex_acquired` and `mutex_release`
been called.

### Userspace probes

Like kprobe, userspace probes can be attached to any userspace functions and
will be triggered when the function is called.

```c
SEC("uprobe//proc/self/exe:random_gen")
int uprobe_random_gen(struct pt_regs *ctx) {
    int argument = (int) PT_REGS_PARM1(ctx);
    bpf_printk("uprobe/random_gen: argument=%d", argument);
    return 0;
}

SEC("uretprobe//proc/self/exe:random_gen")
int uretprobe_random_gen(struct pt_regs *ctx) {
    bpf_printk("uretprobe/random_gen: output=%d", PT_REGS_RC(ctx));
    return 0;
}
```

Note that target functions cannot be inlined and is a mangled name in the
`uprobe/uretprobe` entry, e.g.,

```c
extern "C" {
int __attribute__((noinline)) random_gen(int argument) {
    std::srand(argument);
    return std::rand();
}
}
```

Undoable
--------

eBPF provides extensible, convenient, and powerful tools to trace kernel and
userspace programs. However, as eBPF is executed in a virtual machine, the
kernel space functions that eBPF program can access is fairly limited. The
make the kernel function accessible to eBPF programs, the function needs to
be encoded in the eBPF VM or exported using `kfuncs`.

### Exported helper functions

The Linux kernel encoded supported kernel function calls in the VM and exposed
in [`include/uapi/linux/bpf.h`][3]. For eBPF developers, there's a
[`bpf_helper_defs.h`][4] header that exposed those help function prototypes in C,
generated by the [`bpf_doc.py`][5] script in Linux Kernel.

### BFP Kernel Functions (kfuncs)

Besides predefined functions, there's a new mechanism called
["BFP Kernel Function (kfuncs)"][6] that allows exposed any kernel functions to eBPF
programs. The `kfuncs` is exposed by [the macro `BTF_ID_FLAGS`][7], and can be
first declared as `__ksym` before being used in eBPF programs.

```c
int bpf_kfunc_call_test2(struct sock *sk, __u32 a, __u32 b) __ksym;

SEC("classifier")
int kprobe_drop_cache_impl(struct __sk_buff *skb) {
    struct bpf_sock *sk = skb->sk;

    // ....
	return bpf_kfunc_call_test2((struct sock *)sk, 1, 2);
}
```

At the time of writing, the exported function set is still very limited. It is
impossible to access arbitrary kernel functions and arbitrary kernel data structures
from eBPF programs so for complex tasks beyond simple tracing, eBPF is not a
replacement for kernel module for extending the kernel's capability safely and
efficiently.

[1]: https://github.com/torvalds/linux/commit/fa697140f9a20119a9ec8fd7460cc4314fbdaff3
[2]: https://github.com/iovisor/bcc/issues/1802
[3]: https://github.com/torvalds/linux/blob/master/include/uapi/linux/bpf.h
[4]: https://github.com/libbpf/libbpf/blob/master/src/bpf_helper_defs.h
[5]: https://github.com/torvalds/linux/blob/master/scripts/bpf_doc.py
[6]: https://docs.kernel.org/bpf/kfuncs.html
[7]: https://elixir.bootlin.com/linux/latest/C/ident/BTF_ID_FLAGS
