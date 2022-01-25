!> Backward auto diff
program main

    use auto_diff, only: tree_t, operator(*), operator(+), exp, rk, assignment(=)
    implicit none
    type(tree_t) :: a, b, c
    type(tree_t) :: y

    a = 2.0_rk
    b = 1.0_rk
    c = 0.0_rk

    print *, "demo1: y = (a + b*b)*exp(c)"

    y = (a + b*b)*exp(c)

    print *, "y     = ", y%get_value()
    call y%backward()

    print *, "dy/da = ", a%get_grad()
    print *, "dy/db = ", b%get_grad()
    print *, "dy/dc = ", c%get_grad()

    ! - - -
    
    a = 2.0_rk
    b = 1.0_rk
    
    print *, ""
    print *, "demo2: y = (a + b)*(b + 1.0)"
    
    y = (a + b)*(b + 1.0_rk)

    print *, "y     = ", y%get_value()
    
    call y%backward()

    print *, "dy/da = ", a%get_grad()
    print *, "dy/db = ", b%get_grad()

end program main

!>  demo1: y = (a + b*b)*exp(c)
!>  y     =    3.0000000000000000     
!>  dy/da =    1.0000000000000000
!>  dy/db =    2.0000000000000000
!>  dy/dc =    3.0000000000000000
!> 
!>  demo2: y = (a + b)*(b + 1.0)
!>  y     =    6.0000000000000000
!>  dy/da =    2.0000000000000000
!>  dy/db =    5.0000000000000000