module test_func

    use testdrive, only: new_unittest, unittest_type, error_type, check
    use auto_diff, only: abs, exp, sqrt, sin, cos, tan, log, log10
    use auto_diff, only: max, min, sigmoid
    use auto_diff, only: tree_t, rk, assignment(=)
    implicit none
    private

    public :: collect_suite_func

contains

    !> Collect all exported unit tests
    subroutine collect_suite_func(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("func abs valid", test_abs_valid), &
                    new_unittest("func exp valid", test_exp_valid), &
                    new_unittest("func sqrt valid", test_sqrt_valid), &
                    new_unittest("func sin valid", test_sin_valid), &
                    new_unittest("func cos valid", test_cos_valid), &
                    new_unittest("func tan valid", test_tan_valid), &
                    new_unittest("func log valid", test_log_valid), &
                    new_unittest("func log10 valid", test_log10_valid), &
                    new_unittest("func max valid", test_max_valid), &
                    new_unittest("func min valid", test_min_valid), &
                    new_unittest("func sigmoid valid", test_sigmoid_valid) &
                    ]

    end subroutine collect_suite_func

    subroutine test_abs_valid(error)
        type(error_type), allocatable, intent(out) :: error
        
        type(tree_t) :: a
        type(tree_t) :: b
        
        a = 2.0_rk
        b = abs(a)
        call b%backward()
        
        call check(error, b%get_value(), 2.0_rk); if (allocated(error)) return
        call check(error, a%get_grad(), 1.0_rk)
        
    end subroutine test_abs_valid

    subroutine test_sqrt_valid(error)
        type(error_type), allocatable, intent(out) :: error
        
        type(tree_t) :: a
        type(tree_t) :: b
        
        a = 2.0_rk
        b = sqrt(a)
        call b%backward()
        
        call check(error, b%get_value(), sqrt(2.0_rk)); if (allocated(error)) return
        call check(error, a%get_grad(), 1.0_rk/(2.0_rk*sqrt(2.0_rk)))
        
    end subroutine test_sqrt_valid
    
    subroutine test_exp_valid(error)
        type(error_type), allocatable, intent(out) :: error
        
        type(tree_t) :: a
        type(tree_t) :: b
        
        a = 2.0_rk
        b = exp(a)
        call b%backward()
        
        call check(error, b%get_value(), exp(2.0_rk)); if (allocated(error)) return
        call check(error, a%get_grad(), exp(2.0_rk))
        
    end subroutine test_exp_valid
    
    subroutine test_sin_valid(error)
        type(error_type), allocatable, intent(out) :: error
        
        type(tree_t) :: a
        type(tree_t) :: b
        
        a = 2.0_rk
        b = sin(a)
        call b%backward()
        
        call check(error, b%get_value(), sin(2.0_rk)); if (allocated(error)) return
        call check(error, a%get_grad(), cos(2.0_rk))
        
    end subroutine test_sin_valid
    
    subroutine test_cos_valid(error)
        type(error_type), allocatable, intent(out) :: error
        
        type(tree_t) :: a
        type(tree_t) :: b
        
        a = 2.0_rk
        b = cos(a)
        call b%backward()
        
        call check(error, b%get_value(), cos(2.0_rk)); if (allocated(error)) return
        call check(error, a%get_grad(), -sin(2.0_rk))
        
    end subroutine test_cos_valid
    
    subroutine test_tan_valid(error)
        type(error_type), allocatable, intent(out) :: error
        
        type(tree_t) :: a
        type(tree_t) :: b
        
        a = 2.0_rk
        b = tan(a)
        call b%backward()
        
        call check(error, b%get_value(), tan(2.0_rk)); if (allocated(error)) return
        call check(error, a%get_grad(), 1.0_rk/(cos(2.0_rk)**2))
        
    end subroutine test_tan_valid
    
    subroutine test_log_valid(error)
        type(error_type), allocatable, intent(out) :: error
        
        type(tree_t) :: a
        type(tree_t) :: b
        
        a = 2.0_rk
        b = log(a)
        call b%backward()
        
        call check(error, b%get_value(), log(2.0_rk)); if (allocated(error)) return
        call check(error, a%get_grad(), 1.0_rk/2.0_rk)
        
    end subroutine test_log_valid
    
    subroutine test_log10_valid(error)
        type(error_type), allocatable, intent(out) :: error
        
        type(tree_t) :: a
        type(tree_t) :: b
        
        a = 2.0_rk
        b = log10(a)
        call b%backward()
        
        call check(error, b%get_value(), log10(2.0_rk)); if (allocated(error)) return
        call check(error, a%get_grad(), 1.0_rk/log(10.0_rk)/2.0_rk)
        
    end subroutine test_log10_valid
    
    subroutine test_max_valid(error)
        type(error_type), allocatable, intent(out) :: error
        
        type(tree_t) :: a
        type(tree_t) :: b
        
        a = 2.0_rk
        b = max(a, a)
        call b%backward()
        
        call check(error, b%get_value(), 2.0_rk); if (allocated(error)) return
        call check(error, a%get_grad(), 1.0_rk)
        
    end subroutine test_max_valid
    
    subroutine test_min_valid(error)
        type(error_type), allocatable, intent(out) :: error
        
        type(tree_t) :: a
        type(tree_t) :: b
        
        a = 2.0_rk
        b = min(a, a)
        call b%backward()
        
        call check(error, b%get_value(), 2.0_rk); if (allocated(error)) return
        call check(error, a%get_grad(), 1.0_rk)
        
        a = 1.0_rk
        b = min(a, 2.0_rk)
        call b%backward()
        
        call check(error, b%get_value(), 1.0_rk); if (allocated(error)) return
        call check(error, a%get_grad(), 1.0_rk)
        
        !> Not implemented yet
        ! call a%constructor(value=1.0_rk)
        ! b = min(2.0_rk, a)
        ! call b%backward()
        
        ! call check(error, b%get_value(), 1.0_rk)
        ! call check(error, a%get_grad(), 1.0_rk)
        
    end subroutine test_min_valid
    
    subroutine test_sigmoid_valid(error)
        type(error_type), allocatable, intent(out) :: error
        
        type(tree_t) :: a
        type(tree_t) :: b
        
        a = 2.0_rk
        b = sigmoid(a)
        call b%backward()
        
        call check(error, b%get_value(), 1.0_rk/(1.0_rk+exp(-2.0_rk))); if (allocated(error)) return
        call check(error, a%get_grad(), 0.10499358540350662_rk)
        
    end subroutine test_sigmoid_valid

end module test_func
