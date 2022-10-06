# sclin
Scala implementation of lin.

## Installation

### Requirements

- JRE/JDK 19+

### Prebuilt Installation

Download the [latest executable JAR from releases](https://github.com/molarmanful/sclin/releases) and place it on your `$PATH`. Verify that it works with `sclin --help`.

### Building from Scratch

Requires JRE/JDK 19+. Clone this repo, `cd` into it, and run `./mill main.assembly` (use `mill.bat` instead of `mill` if on Windows). The built JAR can be found at `out/main/assembly.dest/out.jar`.