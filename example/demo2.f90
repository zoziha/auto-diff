!> Sigmoid func & gate
program main

    use auto_diff, only: sigmoid
    use auto_diff, only: tree_t, rk
    use auto_diff, only: operator(*), operator(+)
    implicit none
    type(tree_t) :: w0, w1, w2, x0, x1
    type(tree_t) :: y

    call w0%constructor(value=2.0_rk)
    call w1%constructor(value=-3.0_rk)
    call w2%constructor(value=-3.0_rk)
    call x0%constructor(value=-1.0_rk)
    call x1%constructor(value=-2.0_rk)

    print *, "sigmoid demo: y = 1/(1 + exp(-z)), z = w0*x0 + w1*x1 + w2"
    y = sigmoid(w0*x0 + w1*x1 + w2)

    print *, "y      = ", y%get_value() ! should be  0.73
    call y%backward()

    print *, "dy/dw0 = ", w0%get_grad() ! should be -0.20
    print *, "dy/dw1 = ", w1%get_grad() ! should be -0.39
    print *, "dy/dw2 = ", w2%get_grad() ! should be  0.20
    print *, "dy/dx0 = ", x0%get_grad() ! should be  0.39
    print *, "dy/dx1 = ", x1%get_grad() ! should be -0.59

end program main

!> sigmoid demo: y = 1/(1 + exp(-z), z = w0*x0 + w1*x1 + w2
!> y      =   0.73105857863000490     
!> dy/dw0 =  -0.19661193324148185
!> dy/dw1 =  -0.39322386648296370
!> dy/dw2 =   0.19661193324148185
!> dy/dx0 =   0.39322386648296370
!> dy/dx1 =  -0.58983579972444555
