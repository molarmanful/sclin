# ![sclin](./logo.svg)

[![maven](https://img.shields.io/maven-central/v/io.github.molarmanful/sclin_3?style=flat-square)](https://central.sonatype.dev/artifact/io.github.molarmanful/sclin_3)
[![build](https://img.shields.io/github/actions/workflow/status/molarmanful/sclin/release.yml?style=flat-square)](https://github.com/molarmanful/sclin/actions)
[![license](https://img.shields.io/github/license/molarmanful/sclin?style=flat-square)](./LICENSE)

Scala implementation and rework of [lin](https://github.com/molarmanful/lin).

```
101I>a >A \; tap
  "Fizz""Buzz", over 3 5, % ! *` dup \pop |# n>o
```

## Installation

### Browser Interface

[Try sclin in your browser!](https://replit.com/@molarmanful/try-sclin)

### Requirements

- JRE/JDK 11+

### Installation with [Coursier](https://get-coursier.io)

```
cs install --contrib sclin
```

### Prebuilt Executable

Download the [latest executable JAR from releases](https://github.com/molarmanful/sclin/releases) and place it on your `$PATH`. Verify that it works with `sclin --help`.

### Building from Scratch

Clone this repo, `cd` into it, and run `./mill sclin.jvm.assembly` (use `mill.bat` instead of `mill` if on Windows). The built JAR can be found at `out/sclin/jvm/assembly.dest/out.jar`.

## Licensing

Made with ♥ by Ben Pang. Released under the MIT License.