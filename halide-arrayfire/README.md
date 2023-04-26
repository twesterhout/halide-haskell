<h1 align="center">
halide-arrayfire
</h1>

<div align="center">
<br />

[![license](https://img.shields.io/github/license/twesterhout/halide-haskell.svg?style=flat-square)](LICENSE)

[![build](https://img.shields.io/github/actions/workflow/status/twesterhout/halide-haskell/ci.yml?style=flat-square)](https://github.com/twesterhout/halide-haskell/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/halide-arrayfire?style=flat-square)](https://hackage.haskell.org/package/halide-arrayfire)

</div>

This package integrates
[halide-haskell](https://github.com/twesterhout/halide-haskell/) with
[arrayfire](https://github.com/arrayfire/arrayfire-haskell) by implementing
an instance of `IsHalideBuffer` for the `Array` data type.

  - [X] CPU
  - [ ] CUDA (know how to do it, just need a bit of time)
  - [ ] OpenCL (no idea how, contributions are welcome!)
