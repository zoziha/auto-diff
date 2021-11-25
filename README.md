# Auto-Diff

[![MIT](https://img.shields.io/github/license/zoziha/Auto-Diff?color=pink)](LICENSE)

`Auto-Diff` is an implementation of Modern Fortran's backward mode automatic differentiation.

## Get Started

### Dependencies

- Git
- [Fortran-lang/FPM](https://github.com/fortran-lang/fpm)

### Get the Code

```sh
git clone https://github.com/zoziha/Auto-Diff.git
cd Auto-Diff
```

### Build with Fortran-lang/FPM

Fortran Package Manager (FPM) is a package manager and build system for Fortran. <br>
You can build `Auto-Diff` using the provided `fpm.toml`:

```sh
fpm build
fpm run --example --list
```

To use `Auto-Diff` within your `fpm` project, add the following lines to your `fpm.toml` file:

```toml
[dependencies]
Auto-Diff = { git="https://github.com/zoziha/Auto-Diff" }
```

## Links

- [St-Maxwell/backward(F90)](https://gist.github.com/St-Maxwell/0a936b03ecf99e284a05d10dd994516e)
- [李理的博客/自动微分](http://fancyerii.github.io/books/autodiff/)
- [joddlehod/DNAD](https://github.com/joddlehod/dnad)