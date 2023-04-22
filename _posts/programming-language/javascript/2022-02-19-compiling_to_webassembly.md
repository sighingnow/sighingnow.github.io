---
title: Compiling to WebAssembly
author: Tao He
date: 2022-02-19
tag: [WebAssembly, LLVM]
category: Programming Languages
layout: post
---

WebAssembly, a relatively new and rapidly-evolving low-level language, is designed
as an assembly language that run on the Web, and gains support from all major browsers.
High-level languages like C++ and Rust can use WebAssembly as a compilation target
to be deployed to the web.

<!--more-->

Set up the Toolchain
-------------------

C/C++ source code can be compiled to WebAssembly by either clang or emscripten.
Installing the [emscripten toolchain][1] is fairly straightforward,

+ Clone the emsdk,

        git clone https://github.com/emscripten-core/emsdk.git
        cd emsdk/

+ Install the latest release, or the tip-of-tree build,

        ./emsdk install tot
        ./emsdk activate tot

+ Finally, populate the environment variables,

        source ./emsdk_env.sh

Now `emcc` should works,


```bash
$ emcc --version
emcc (Emscripten gcc/clang-like replacement + linker emulating GNU ld) 3.1.6-git (e1bfc04f8234eff40739c7932402f2148487c192)
Copyright (C) 2014 the Emscripten authors (see AUTHORS.txt)
This is free and open source software under the MIT license.
There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
```

Besides compiling C/C++ source code to WebAssembly. Emscripten supports a set of polyfill
libraries, e.g., SDL, glfw, to emulate native behaviors in the web browsers.

To inspect and manipulate the WebAssembly language, we need to install the [binaryen][2] tools,

    git clone https://github.com/WebAssembly/binaryen.git
    cd binaryen/
    git submodule update
    mkdir build && cd build && cmake ..
    make install -j`nproc`

Compiling with Emscripten
------------------------

Let's try a "hello world" to get a glance at the emscripten toolchain,

```c
#include <stdio.h>

int main() {
  printf("hello world, webassembly!\n");
  return 0;
}
```

Compiling the C source code with `emcc`,


```bash
$ emcc hello.cc
```

A `a.out.js` and `a.out.wasm` was generated, and the `a.out.js` is the entry file,

```bash
$ node ./a.out.js
hello world, webassembly!
```

Compiling with LLVM
------------------

Emscripten is not a necessity for compiling C/C++ source to WebAssembly, and LLVM
supports natively WebAssembly code generation,

```bash
$ clang --print-targets
  Registered Targets:
    ...
    wasm32     - WebAssembly 32-bit
    wasm64     - WebAssembly 64-bit
```

We actually do could compile to WebAssembly using clang,

```cpp
int add (int first, int second) {
  return first + second;
}
```

Compiling the code above to WebAssembly,

```bash
$ clang -O2 --target=wasm32 --no-standard-libraries -Wl,--export-all -Wl,--no-entry -o add.wasm add.cc
```

We use `--target=wasm32` for the clang compiler, as `wasm64` is not supported well by
binaryen at the time of writing, see [WebAssembly/binary#][3].

The `wasm2js` tool in binaryen can translate the wasm module to js module (see [difference between .js and .mjs][4]),

```bash
$ wasm2js add.wasm -o add.mjs
```

And we can import and evaluate the js module in node,

```javascript
$ node --experimental-repl-await
Welcome to Node.js v14.18.2.
Type ".help" for more information.
>
> let addModule = await import("./add.mjs")

> addModule
[Module: null prototype] {
  __wasm_call_ctors: [Function: __wasm_call_ctors],
  add: [Function: add],
  memory: {}
}

> addModule.add(1, 2)
3
```

Actually we can import the WebAssembly into node using `WebAssembly.instantiate()`,

```javascript
$ node --experimental-repl-await
Welcome to Node.js v14.18.2.
Type ".help" for more information.
>
> let addModule = await WebAssembly.instantiate(fs.readFileSync("add.wasm"));
{
  instance: Instance [WebAssembly.Instance] {},
  module: Module [WebAssembly.Module] {}
}

> addModule.instance.exports
[Object: null prototype] {
  memory: Memory [WebAssembly.Memory] {},
  __wasm_call_ctors: [Function: 0],
  add: [Function: 1],
  __dso_handle: Global [WebAssembly.Global] {},
  __data_end: Global [WebAssembly.Global] {},
  __global_base: Global [WebAssembly.Global] {},
  __heap_base: Global [WebAssembly.Global] {},
  __memory_base: Global [WebAssembly.Global] {},
  __table_base: Global [WebAssembly.Global] {}
}

> addModule.instance.exports.add(1, 2)
3
```

### Link standard libraries in WebAssembly

We use `--no-standard-libraries` to compile `.cc` to WebAssembly, to avoid linking the
standard libc/libc++ libraries (namely `libc.a`, `libc++.a`, `libc++abi.a` and `libclang_rt.builtins-wasm32.a`),
which are usually not available in typical LLVM/Clang distributions, e.g., `apt.llvm.org`.
Fortunately, the project [wasm-libc][5] provides a libc compatible layer for WebAssembly
and the project [wasm-sdk][6] provides a pre-built cross-compiled bundle of above standard
libraries with _sys-root_ correctly configured.

The wasm-sdk toolchain is available at [https://github.com/WebAssembly/wasi-sdk/releases][7].

Once we have downloaded and unpacked the wasm-sdk, we can verify the support of WebAssembly
by

```bash
$ $WASM/bin/clang --print-targets
  Registered Targets:
    wasm32 - WebAssembly 32-bit
    wasm64 - WebAssembly 64-bit
```

where the `$WASM` is the location of the wasm-sdk installation.

Lets try compile C/C++ source to WebAssembly in which standard libraries are involved,

```cpp
#include <math.h>

extern "C" double add(int first, int second) {
  return sin(first + second);
}
```

Compiling using `$WASM/bin/clang`,

```bash
$ $WASM/bin/clang++ -O3 --target=wasm32-unknown-wasi -nostartfiles -Wl,--export-all -Wl,--no-entry -o add.wasm add.cc
```

Note that we use `-nostartfiles` as it doesn't have a `main` entry function, and use
`wasm32-unknown-wasi` target triple as the ABI.

Evaluating WebAssembly binary that WASI involves requires an implementation of the WASI
APIs (namely `wasi_snapshot_preview1`), and there are some polyfills provides such
functionalities, e.g., [deno.std][8] and [lib/wasi.js][9].

```javascript
$ node --experimental-repl-await --experimental-wasi-unstable-preview
Welcome to Node.js v14.18.2.
Type ".help" for more information.
>
> const WASI = await import("wasi");

> let wasi = new WASI.WASI();

> const importObject = { wasi_snapshot_preview1: wasi.wasiImport };

> let addModule = await WebAssembly.instantiate(fs.readFileSync("add.wasm"), importObject);

> addModule.instance.exports.add(1, 2)
0.141120008059867
```

### Memory Pointers in WASM

Consider the following case, where we need to share some memory pointers between
WebAssembly (i.e., the C/C++ source code) and JavaScript[^2],

```cpp
#include <math.h>
#include <string.h>

extern "C" double add(int first, int second) {
  return sin(first + second);
}

extern "C" int add_string(char *input, char *output) {
    int nlen = strlen(input);
    strncpy(output, input, nlen);
    return nlen;
}
```

Compile the above C source code with `-Wl,--import-memory` to makes the heap in WASM
sharable with the environment[^1],

```bash
$WASM/bin/clang++ -O3 --target=wasm32-unknown-wasi \
    -nostartfiles \
    -Wl,--export-all \
    -Wl,--no-entry \
    -Wl,--import-memory \
    -o add.wasm add.cc
```

Inspecting the generated `add.wasm` with `wasm-dis` from _Binaryen_, there're some
"import" statements in the wat,

```wasm
(import "env" "memory" (memory $mimport$0 2))
```

We need to provide a `env.memory` entry in the `importObject`,

```javascript
> let memory = new WebAssembly.Memory({ initial: 2 });

> let importObject = { wasi_snapshot_preview1: wasi.wasiImport, env: { memory: memory } };
> let addModule = await WebAssembly.instantiate(fs.readFileSync("add.wasm"), importObject);

> let view = new Uint8Array(memory.buffer);
> let base = addModule.instance.exports.__heap_base;
```

The block of memory can be accessed from both the two environments, as a typed buffer
in JavaScript and a raw pointer in WebAssembly (i.e., C/C++).

Next, we define two functions to peek JavaScript value from the typed buffer and poke
the JavaScript value into the memory view,

```javascript
> function fromJSString(memory, base, text) {
      for (let i = 0; i < text.length; ++i) {
          memory[base + i] = text.charCodeAt(i);
      }
      memory[base + text.length] = 0;
  }

> function toJSString(memory, base) {
      let p = base + 0;
      let result = '';
      while (memory[p] !== 0) {
          result += String.fromCharCode(memory[p++]);
      }
      return result;
  }
```

Combining the above pieces as a whole, we can verify that the `add_string` defined in C
does work expectedly,

```javascript
$ node --experimental-repl-await --experimental-wasi-unstable-preview
Welcome to Node.js v14.18.2.
Type ".help" for more information.
>
> const WASI = await import("wasi");
> const wasi = new WASI.WASI();

> function fromJSString(memory, base, text) {
      for (let i = 0; i < text.length; ++i) {
          memory[base + i] = text.charCodeAt(i);
      }
      memory[base + text.length] = 0;
  }

> function toJSString(memory, base) {
      let p = base + 0;
      let result = '';
      while (memory[p] !== 0) {
          result += String.fromCharCode(memory[p++]);
      }
      return result;
  }

> let memory = new WebAssembly.Memory({ initial: 2 });

> let importObject = { wasi_snapshot_preview1: wasi.wasiImport, env: { memory: memory } };
> let addModule = await WebAssembly.instantiate(fs.readFileSync("add.wasm"), importObject);

> let view = new Uint8Array(memory.buffer);
> let base = addModule.instance.exports.__heap_base;

> let text = "hello world, webassembly!";
> fromJSString(view, base, text);
> toJSString(view, base);
'hello world, webassembly!'

> addModule.instance.exports.add_string(base, base + text.length);
25
> toJSString(view, base);
'hello world, webassembly!hello world, webassembly!'
```

Note that the `view` **will be invalidated** once the underlying memory buffer changed,
e.g., `malloc` happens in the WebAssembly functions, the a new view will be needed
after returning from the WebAssembly functions.


[^1]: https://lld.llvm.org/WebAssembly.html#cmdoption-import-memory
[^2]: https://hacks.mozilla.org/2017/07/memory-in-webassembly-and-why-its-safer-than-you-think/

[1]: https://emscripten.org/docs/getting_started/downloads.html
[2]: https://github.com/WebAssembly/binaryen
[3]: https://github.com/WebAssembly/binaryen/issues/4518
[4]: https://stackoverflow.com/a/57492606/5080177
[5]: https://github.com/WebAssembly/wasi-libc
[6]: https://github.com/WebAssembly/wasi-sdk
[7]: https://github.com/WebAssembly/wasi-sdk/releases
[8]: https://github.com/denoland/deno_std/blob/main/wasi/snapshot_preview1.ts
[9]: https://nodejs.org/api/wasi.html
