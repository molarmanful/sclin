# sclin

Scala implementation and rework of [lin](https://github.com/molarmanful/lin).

## Browser Interface

[Try sclin in your browser!](https://replit.com/@molarmanful/try-sclin)

## Installation

### Requirements

- JRE/JDK 11+

### Installation with [Coursier](https://get-coursier.io)

```
cs install --contrib sclin
```

### Prebuilt Installation

Download the [latest executable JAR from releases](https://github.com/molarmanful/sclin/releases) and place it on your `$PATH`. Verify that it works with `sclin --help`.

### Building from Scratch

Clone this repo, `cd` into it, and run `./mill sclin.assembly` (use `mill.bat` instead of `mill` if on Windows). The built JAR can be found at `out/sclin/assembly.dest/out.jar`.

## Licensing

Made with ♥ by Ben Pang. Released under the MIT License.