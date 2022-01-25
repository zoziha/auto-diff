# Auto-Diff

`Auto-Diff` is an implementation of Modern Fortran's backward mode automatic differentiation.

*This project is still in the experimental stage, feedback is welcome!*

[![MIT](https://img.shields.io/github/license/zoziha/Auto-Diff?color=pink)](LICENSE)

## Getting Started

### Get the Code

```sh
git clone https://github.com/zoziha/Auto-Diff.git
cd Auto-Diff
```

### Build with [Fortran-lang/fpm](https://github.com/fortran-lang/fpm)

Fortran Package Manager (fpm) is a package manager and build system for Fortran. <br>
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

### Demo3

```fortran
!> Staged solution, run this code: fpm run --example demo3
program main

    use auto_diff
    implicit none
    type(tree_t) :: x1, x2
    type(tree_t) :: y

    x1 = 3.0_rk
    x2 = -4.0_rk

    print *, "staged demo: y = (x1 + sigmoid(x2))/(sigmoid(x1) + (x1 + x2)**2)"
    y = (x1 + sigmoid(x2))/(sigmoid(x1) + (x1 + x2)**2.0_rk)

    print *, "y      = ", y%get_value()
    call y%backward()

    print *, "dy/dx1 = ", x1%get_grad()
    print *, "dy/dx2 = ", x2%get_grad()

end program main

!> staged demo: y = (x1 + sigmoid(x2))/(sigmoid(x1) + (x1 + x2)**2)
!> y      =    1.5456448841066441     
!> dy/dx1 =   -1.1068039935182090
!> dy/dx2 =   -1.5741410376065648
```

### Bench1

```sh
$ fpm run --example bench1 --profile debug
 Forward
 Elapsed time (seconds):   1.9218750000000000
 Ordinary arithmetic
 Elapsed time (seconds):  0.15625000000000000
 Backward
 Elapsed time (seconds):  0.29687500000000000
 
$ fpm run --example bench1 --profile release
 Forward
 Elapsed time (seconds):   1.6093750000000000 
 Ordinary arithmetic
 Elapsed time (seconds):   0.0000000000000000
 Backward
 Elapsed time (seconds):  0.21875000000000000  
```

The `Bench1` code for arrays (1000*1000) is [here](./example/bench1.f90).

## Links

- [St-Maxwell/backward(F90)](https://gist.github.com/St-Maxwell/0a936b03ecf99e284a05d10dd994516e)
- [KT19/automatic_differentiation](https://github.com/KT19/automatic_differentiation)
- [李理的博客/自动微分](http://fancyerii.github.io/books/autodiff/)
- [joddlehod/DNAD](https://github.com/joddlehod/dnad)
- [SCM-NV/ftl](https://github.com/SCM-NV/ftl/blob/master/src/ftlList.F90_template)
- [reverse-mode-automatic-differentiation](https://rufflewind.com/2016-12-30/reverse-mode-automatic-differentiation)
