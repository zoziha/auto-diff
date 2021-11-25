!> Backward auto diff
program main
    use auto_diff, only: node_t, operator(*), operator(+), exp, dp
    implicit none
    type(node_t) :: a, b, c
    type(node_t), pointer :: y

    call a%constructor(value=2.0_dp)
    call b%constructor(value=1.0_dp)
    call c%constructor(value=0.0_dp)

    print *, "demo1: y = (a + b*b)*exp(c)"

    y => (a + b*b)*exp(c)

    print *, "y     = ", y%get_value()
    call y%backward()

    print *, "dy/da = ", a%get_grad()
    print *, "dy/db = ", b%get_grad()
    print *, "dy/dc = ", c%get_grad()

    ! - - -
    
    call a%constructor(value=2.0_dp)
    call b%constructor(value=1.0_dp)
    
    print *, ""
    print *, "demo2: y = (a + b)*(b + 1.0)"
    
    y => (a + b)*(b + 1.0_dp)

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