program main

    use auto_diff
    implicit none

    integer, parameter :: n = 1000
    type(tree_t) :: x1(n, n), x2(n, n)
    type(tree_t) :: y(n, n)
    real(rk) :: r1(n, n), r2(n, n)
    real(rk) :: z(n, n)

    real(rk) :: t1, t2

    x1 = randu(n, n)
    x2 = randu(n, n)

    r1 = randu(n, n)
    r2 = randu(n, n)

    print *, "Forward"
    call cpu_time(t1)
    y = (x1 + sigmoid(x2))/(sigmoid(x1) + (x1 + x2)**2.0_rk)
    call cpu_time(t2)
    print *, "Elapsed time (seconds):", t2 - t1

    print *, "Ordinary arithmetic"
    call cpu_time(t1)
    z = (r1 + sigmoid_local(r2))/(sigmoid_local(r1) + (r1 + r2)**2.0_rk)
    call cpu_time(t2)
    print *, "Elapsed time (seconds):", t2 - t1

    print *, "Backward"
    call cpu_time(t1)
    call y%backward()
    call cpu_time(t2)
    print *, "Elapsed time (seconds):", t2 - t1

contains

    function randu(m, n) result(out)
        integer, intent(in) :: m, n
        real(rk) :: out(m, n)

        call random_number(out)

    end function randu

    elemental function sigmoid_local(x) result(y)
        real(rk), intent(in) :: x
        real(rk) :: y
        y = 1.0_rk/(1.0_rk + exp(-x))
    end function sigmoid_local

end program main

! fpm run --example bench1 --profile debug:
! Forward
! Elapsed time (seconds):   1.9218750000000000     
! Ordinary arithmetic
! Elapsed time (seconds):  0.15625000000000000     
! Backward
! Elapsed time (seconds):  0.29687500000000000

! fpm run --example bench1 --profile release:
! Forward
! Elapsed time (seconds):   1.6093750000000000     
! Ordinary arithmetic
! Elapsed time (seconds):   0.0000000000000000
! Backward
! Elapsed time (seconds):  0.21875000000000000   
