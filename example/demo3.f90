!> Staged solution
program main

    use auto_diff, only: sigmoid
    use auto_diff, only: tree_t, rk, assignment(=)
    use auto_diff, only: operator(*), operator(+), operator(/), operator(**)
    implicit none
    type(tree_t) :: x1, x2
    type(tree_t) :: y
    
    x1 = 3.0_rk
    x2 = -4.0_rk

    print *, "staged demo: y = (x1 + sigmoid(x2))/(sigmoid(x1) + (x1 + x2)**2)"
    y = (x1 + sigmoid(x2))/(sigmoid(x1) + (x1 + x2)**2)

    print *, "y      = ", y%get_value()
    call y%backward()

    print *, "dy/dx1 = ", x1%get_grad()
    print *, "dy/dx2 = ", x2%get_grad()

end program main

!> staged demo: y = (x1 + sigmoid(x2))/(sigmoid(x1) + (x1 + x2)**2)
!> y      =    1.5456448841066441     
!> dy/dx1 =   -1.1068039935182090
!> dy/dx2 =   -1.5741410376065648