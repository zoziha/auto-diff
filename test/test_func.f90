module test_func
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use auto_diff, only: abs, exp, sqrt
    use auto_diff, only: node_t, dp
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
                    new_unittest("func sqrt valid", test_sqrt_valid) &
                    ]

    end subroutine collect_suite_func

    subroutine test_abs_valid(error)
        type(error_type), allocatable, intent(out) :: error
        
        type(node_t) :: a
        type(node_t), pointer :: b
        
        call a%constructor(value=2.0_dp)
        b => abs(a)
        call b%backward()
        
        call check(error, b%get_value(), 2.0_dp)
        call check(error, a%get_grad(), 1.0_dp)
        
    end subroutine test_abs_valid

    subroutine test_sqrt_valid(error)
        type(error_type), allocatable, intent(out) :: error
        
        type(node_t) :: a
        type(node_t), pointer :: b
        
        call a%constructor(value=2.0_dp)
        b => sqrt(a)
        call b%backward()
        
        call check(error, b%get_value(), sqrt(2.0_dp))
        call check(error, a%get_grad(), 1.0_dp/(2.0_dp*sqrt(2.0_dp)))
        
    end subroutine test_sqrt_valid
    
    subroutine test_exp_valid(error)
        type(error_type), allocatable, intent(out) :: error
        
        type(node_t) :: a
        type(node_t), pointer :: b
        
        call a%constructor(value=2.0_dp)
        b => exp(a)
        call b%backward()
        
        call check(error, b%get_value(), exp(2.0_dp))
        call check(error, a%get_grad(), exp(2.0_dp))
        
    end subroutine test_exp_valid

end module test_func
