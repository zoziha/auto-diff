module test_operator
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use auto_diff, only: operator(+), operator(-), operator(*), operator(/), operator(**)
    use auto_diff, only: rk, tree_t
    implicit none
    private

    public :: collect_suite_operator

contains

    !> Collect all exported unit tests
    subroutine collect_suite_operator(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("operator(+) valid", test_add_valid), &
                    new_unittest("operator(-) valid", test_sub_valid), &
                    new_unittest("operator(*) valid", test_mult_valid), &
                    new_unittest("operator(/) valid", test_div_valid), &
                    new_unittest("operator(**) valid", test_pow_valid) &
                    ]

    end subroutine collect_suite_operator

    subroutine test_add_valid(error)
        type(error_type), allocatable, intent(out) :: error
        
        type(tree_t) :: a, b
        type(tree_t) :: c
        
        call a%constructor(value=1.0_rk)
        call b%constructor(value=2.0_rk)
        
        c = a + b
        call c%backward()
        
        call check(error, c%get_value(), 3.0_rk); if (allocated(error)) return
        call check(error, a%get_grad(), 1.0_rk); if (allocated(error)) return
        call check(error, b%get_grad(), 1.0_rk); if (allocated(error)) return
        call c%destructor()
        
        call a%constructor(value=1.0_rk)
        c = a + 1.0_rk
        call c%backward()
        
        call check(error, c%get_value(), 2.0_rk); if (allocated(error)) return
        call check(error, a%get_grad(), 1.0_rk); if (allocated(error)) return
        call c%destructor()
        
        call a%constructor(value=1.0_rk)
        c = 1.0_rk + a
        call c%backward()
        
        call check(error, c%get_value(), 2.0_rk); if (allocated(error)) return
        call check(error, a%get_grad(), 1.0_rk)
        
    end subroutine test_add_valid
    
    subroutine test_sub_valid(error)
        type(error_type), allocatable, intent(out) :: error
        
        type(tree_t) :: a, b
        type(tree_t) :: c
        
        call a%constructor(value=1.0_rk)
        call b%constructor(value=2.0_rk)
        
        c = a - b
        call c%backward()
        
        call check(error, c%get_value(), -1.0_rk); if (allocated(error)) return
        call check(error, a%get_grad(), 1.0_rk); if (allocated(error)) return
        call check(error, b%get_grad(), -1.0_rk); if (allocated(error)) return
        
        call a%constructor(value=1.0_rk)
        c = a - 1.0_rk
        call c%backward()
        
        call check(error, c%get_value(), 0.0_rk); if (allocated(error)) return
        call check(error, a%get_grad(), 1.0_rk); if (allocated(error)) return
        
        call a%constructor(value=1.0_rk)
        c = 1.0_rk - a
        call c%backward()
        
        call check(error, c%get_value(), 0.0_rk); if (allocated(error)) return
        call check(error, a%get_grad(), -1.0_rk)
        
    end subroutine test_sub_valid
    
    subroutine test_mult_valid(error)
        type(error_type), allocatable, intent(out) :: error
        
        type(tree_t) :: a, b
        type(tree_t) :: c
        
        call a%constructor(value=1.0_rk)
        call b%constructor(value=2.0_rk)
        
        c = a * b
        call c%backward()
        
        call check(error, c%get_value(), 2.0_rk); if (allocated(error)) return
        call check(error, a%get_grad(), 2.0_rk); if (allocated(error)) return
        call check(error, b%get_grad(), 1.0_rk); if (allocated(error)) return
        
        call a%constructor(value=1.0_rk)
        c = a * 1.0_rk
        call c%backward()
        
        call check(error, c%get_value(), 1.0_rk); if (allocated(error)) return
        call check(error, a%get_grad(), 1.0_rk); if (allocated(error)) return
        
        call a%constructor(value=1.0_rk)
        c = 1.0_rk * a
        call c%backward()
        
        call check(error, c%get_value(), 1.0_rk); if (allocated(error)) return
        call check(error, a%get_grad(), 1.0_rk)
        
    end subroutine test_mult_valid
    
    subroutine test_div_valid(error)
        type(error_type), allocatable, intent(out) :: error
        
        type(tree_t) :: a, b
        type(tree_t) :: c
        
        call a%constructor(value=1.0_rk)
        call b%constructor(value=2.0_rk)
        
        c = a / b
        call c%backward()
        
        call check(error, c%get_value(), 0.5_rk); if (allocated(error)) return
        call check(error, a%get_grad(), 0.5_rk); if (allocated(error)) return
        call check(error, b%get_grad(), -0.25_rk); if (allocated(error)) return
        
        call a%constructor(value=1.0_rk)
        c = a / 1.0_rk
        call c%backward()
        
        call check(error, c%get_value(), 1.0_rk); if (allocated(error)) return
        call check(error, a%get_grad(), 1.0_rk); if (allocated(error)) return
        
        call a%constructor(value=1.0_rk)
        c = 1.0_rk / a
        call c%backward()
        
        call check(error, c%get_value(), 1.0_rk); if (allocated(error)) return
        call check(error, a%get_grad(), -1.0_rk)
        
    end subroutine test_div_valid
    
    subroutine test_pow_valid(error)
        type(error_type), allocatable, intent(out) :: error
        
        type(tree_t) :: a, b
        type(tree_t) :: c
        
        call a%constructor(value=1.0_rk)
        call b%constructor(value=2.0_rk)
        
        c = a ** b
        call c%backward()
        
        call check(error, c%get_value(), 1.0_rk); if (allocated(error)) return
        call check(error, a%get_grad(), 2.0_rk); if (allocated(error)) return
        call check(error, b%get_grad(), 2.0_rk); if (allocated(error)) return
        
        call a%constructor(value=1.0_rk)
        c = a ** 1.0_rk
        call c%backward()
        
        call check(error, c%get_value(), 1.0_rk); if (allocated(error)) return
        call check(error, a%get_grad(), 1.0_rk); if (allocated(error)) return
        
        call a%constructor(value=1.0_rk)
        c = 1.0_rk ** a
        call c%backward()
        
        call check(error, c%get_value(), 1.0_rk); if (allocated(error)) return
        call check(error, a%get_grad(), 0.0_rk)
        
    end subroutine test_pow_valid

end module test_operator
