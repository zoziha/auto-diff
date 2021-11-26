!> Staged solution
program main

    use auto_diff, only: sigmoid
    use auto_diff, only: node_t, dp
    use auto_diff, only: operator(*), operator(+), operator(/), operator(**)
    implicit none
    type(node_t) :: x1, x2
    type(node_t), pointer :: y

    call x1%constructor(value=3.0_dp)
    call x2%constructor(value=-4.0_dp)

    print *, "staged demo: y = (x1 + sigmoid(x2))/(sigmoid(x1) + (x1 + x2)**2)"
    y => (x1 + sigmoid(x2))/(sigmoid(x1) + (x1 + x2)**2.0_dp)

    print *, "y      = ", y%get_value()
    call y%backward()

    print *, "dy/dx1 = ", x1%get_grad()
    print *, "dy/dx2 = ", x2%get_grad()

end program main

!> staged demo: y = (x1 + sigmoid(x2))/(sigmoid(x1) + (x1 + x2)**2)
!> y      =    1.5456448841066441     
!> dy/dx1 =   -1.1068039935182090
!> dy/dx2 =   -1.5741410376065648