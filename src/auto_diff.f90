module auto_diff

    use ad_kinds, only: rk
    use ad_types, only: tree_t
    use ad_operator, only: operator(+), operator(-), operator(*), operator(/), &
                           operator(**)
    use ad_intrinsic, only: abs, max, min, &
                            sin, cos, tan, &
                            exp, sqrt, log, log10
    use ad_usr_func, only: sigmoid
    implicit none

end module auto_diff
