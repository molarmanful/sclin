# sclin
Scala implementation of lin.

## Installation

### Requirements

- JRE/JDK 19+

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