!> Matrix
program main

    use auto_diff
    implicit none
    type(tree_t) :: a(2, 2), b(2, 2)
    type(tree_t) :: y(2, 2)

    call a%constructor(value=1.0_rk)
    call b%constructor(value=2.0_rk)

    print *, "demo4: y = a + a * b"

    y = a + a*b

    print *, "y     = ", y%get_value()
    call y%backward()

    print *, "dy/da = ", a%get_grad()
    print *, "dy/db = ", b%get_grad()

end program main
