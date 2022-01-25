!> Matrix
program main

    use auto_diff
    implicit none
    type(tree_t) :: a(2, 2), b(2, 2)
    type(tree_t) :: y(2, 2)

    a = 1.0_rk
    b = 2.0_rk

    print *, "demo4: y = a + a * b"

    y = a + a*b

    print *, "y     = ", y%get_value()
    call y%backward()

    print *, "dy/da = ", a%get_grad()
    print *, "dy/db = ", b%get_grad()

end program main

! demo4: y = a + a * b
! y     =    3.0000000000000000        3.0000000000000000        3.0000000000000000        3.0000000000000000     
! dy/da =    3.0000000000000000        3.0000000000000000        3.0000000000000000        3.0000000000000000
! dy/db =    1.0000000000000000        1.0000000000000000        1.0000000000000000        1.0000000000000000