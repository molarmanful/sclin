# ![sclin](./logo.svg)

[![maven](https://img.shields.io/maven-central/v/io.github.molarmanful/sclin_3?style=flat-square)](https://central.sonatype.dev/artifact/io.github.molarmanful/sclin_3)
[![build](https://img.shields.io/github/actions/workflow/status/molarmanful/sclin/release.yml?style=flat-square)](https://github.com/molarmanful/sclin/actions)
[![license](https://img.shields.io/github/license/molarmanful/sclin?style=flat-square)](./LICENSE)

Scala implementation and rework of [lin](https://github.com/molarmanful/lin).

```
100I-a \; tap
  "Fizz""Buzz", over 3 5, % ! *` dup \pop |# n>o
```

[Try it on scline!](https://scline.fly.dev/##H4sIAAY3AWUCAzM0MPS0S1SIsVYoSSzgUlBQcsusqlJScioFkjoK.WWpRQrGCqY6CqoKigpaCQoppQUKMQX5BQo1ygp5dvkAKdsWfD4AAAA#)

## Installation

### Browser Interface

[Try it on scline!](https://scline.fly.dev) The official online interpreter for sclin.

### Requirements

- JRE/JDK 11+

### Installation with [Coursier](https://get-coursier.io)

```
cs install --contrib sclin
```

### Prebuilt Executable

Download the [latest executable JAR from releases](https://github.com/molarmanful/sclin/releases) and place it on your `$PATH`. Verify that it works with `sclin --help`.

### Building from Scratch

Clone this repo, `cd` into it, and run `./mill sclin.assembly` (use `mill.bat` instead of `mill` if on Windows). The built JAR can be found at `out/sclin/assembly.dest/out.jar`.

## Licensing

Made with ♥ by Ben Pang. Released under the MIT License.