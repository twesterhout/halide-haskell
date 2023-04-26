<h1 align="center">
halide-JuicyPixels
</h1>

<div align="center">
<br />

[![license](https://img.shields.io/github/license/twesterhout/halide-haskell.svg?style=flat-square)](LICENSE)

[![build](https://img.shields.io/github/actions/workflow/status/twesterhout/halide-haskell/ci.yml?style=flat-square)](https://github.com/twesterhout/halide-haskell/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/halide-JuicyPixels?style=flat-square)](https://hackage.haskell.org/package/halide-Julide-JuicyPixels)

</div>

This package integrates
[halide-haskell](https://github.com/twesterhout/halide-haskell/) with
[JuicyPixels](https://github.com/Twinside/Juicy.Pixels) by implementing
instances of `IsHalideBuffer` for `Image` and `MutableImage` types. See [this
test](test/Main.hs) for an example usage.
