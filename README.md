<div align="center">

# ![sclin](./logo.svg)

[![maven](https://img.shields.io/maven-central/v/io.github.molarmanful/sclin_3?style=flat-square)](https://central.sonatype.dev/artifact/io.github.molarmanful/sclin_3)
[![build](https://img.shields.io/github/actions/workflow/status/molarmanful/sclin/release.yml?style=flat-square)](https://github.com/molarmanful/sclin/actions)
[![license](https://img.shields.io/github/license/molarmanful/sclin?style=flat-square)](./LICENSE)

</div>

```sclin
sclin 100I-a \; tap "Fizz""Buzz", over 3 5, % ! *` \pop ||# n>o
```

[Try it on
scline!](https://scline.fly.dev/##H4sIAAY3AWUCAzM0MPS0S1SIsVYoSSzgUlBQcsusqlJScioFkjoK.WWpRQrGCqY6CqoKigpaCQoppQUKMQX5BQo1ygp5dvkAKdsWfD4AAAA#)

> A concatenative cyborg chimera.

**sclin** is a programming language that leans heavily into its weirdness. Some
of its features include:

- **Postfix notation** - sclin reads left-to-right with no precedence rules.
  _Concatenation is composition!_
- **The Stack** - sclin's FIFO stack serves as both storage and a "staging
  ground" for function arguments. This - in tandem with stack manipulation
  commands - enables an intuitive point-free style that mirrors those found in
  functional languages.
- **Line-jumping** - sclin treats each line as a separate function, enabling
  "GOTO"-style line execution commands to create recursion and other control flow
  structures.
- **Vectorization** - Most of sclin's built-in commands vectorize, which allow
  those commands to operate on arbitrarily nested data structures.
- **Concise symbols** - sclin's commands follow a pictographic language to
  convey and distinguish properties.
- **Type fluidity** - Most (if not all) sclin commands auto-convert types as
  necessary, eschewing type safety for flexibility and expressiveness.

For more info/docs, please visit the
[wiki](https://github.com/molarmanful/sclin/wiki).

## Installation

### Browser Interface

[Try it on scline!](https://scline.fly.dev) The official online interpreter for
sclin.

### Requirements

- JRE/JDK 21+

### Installation with [Coursier](https://get-coursier.io)

```sh
sh cs install --contrib sclin
```

### Prebuilt Executable

Download the [latest executable JAR from
releases](https://github.com/molarmanful/sclin/releases) and place it on your
`$PATH`. Verify that it works with `sclin --help`.

### Building from Scratch

Clone this repo, `cd` into it, and run `./mill sclin.assembly` (use `mill.bat`
instead of `mill` if on Windows). The built JAR is at
`out/sclin/assembly.dest/out.jar`.

## Tools

- [tree-sitter-sclin](https://www.npmjs.com/package/tree-sitter-sclin) -
  Ultra-primitive tree-sitter grammar for syntax highlighting.

## Licensing

Made with ♥ by Ben Pang. Released under the MIT License.
