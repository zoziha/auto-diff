!> reference: https://github.com/KT19/automatic_differentiation

module auto_diff

    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    private

    public :: node_t, dp
    public :: operator(+), operator(-), operator(*), operator(/), operator(**)
    public :: abs, exp, sqrt

    type node_t

        private
        real(dp) :: value           !! value of the node
        real(dp) :: grad = 0.0_dp   !! gradient of the node
        type(pair_t), pointer :: parents(:)

    contains

        !> Constructor
        procedure :: constructor => node_t_constructor

        !> Node value
        procedure :: get_value => node_t_get_value

        !> Node grad
        procedure :: get_grad => node_t_get_grad
        procedure, private :: backward_head
        procedure, private :: backward_next
        generic :: backward => backward_head, backward_next

    end type node_t

    type pair_t
        type(node_t), pointer :: node
        real(dp) :: local_grad
    end type pair_t

    interface operator(+)
        module procedure :: add_nn
        module procedure :: add_nr
        module procedure :: add_rn
    end interface

    interface operator(-)
        module procedure :: sub_nn
        module procedure :: sub_nr
        module procedure :: sub_rn
    end interface

    interface operator(*)
        module procedure :: mult_nn
        module procedure :: mult_nr
        module procedure :: mult_rn
    end interface

    interface operator(/)
        module procedure :: div_nn
        module procedure :: div_nr
        module procedure :: div_rn
    end interface

    interface operator(**)
        module procedure :: pow_nn
        module procedure :: pow_nr
        module procedure :: pow_rn
    end interface

    interface abs
        module procedure :: abs_n
    end interface

    interface exp
        module procedure :: exp_n
    end interface

    interface sin
        module procedure :: sin_n
    end interface

    interface cos
        module procedure :: cos_n
    end interface

    interface tan
        module procedure :: tan_n
    end interface

    interface log
        module procedure :: log_n
    end interface

    interface log10
        module procedure :: log10_n
    end interface

    interface sqrt
        module procedure :: sqrt_n
    end interface

contains

    !> Node constructor
    subroutine node_t_constructor(self, value)
        class(node_t), intent(inout) :: self
        real(dp), intent(in) :: value

        self%value = value
        self%grad = 0.0_dp
        allocate (self%parents(0))

    end subroutine node_t_constructor

    !> Node value
    function node_t_get_value(self) result(value)
        class(node_t), intent(in) :: self
        real(dp) :: value

        value = self%value

    end function node_t_get_value

    !> Node grad
    function node_t_get_grad(self) result(grad)
        class(node_t), intent(in) :: self
        real(dp) :: grad

        grad = self%grad

    end function node_t_get_grad

    !> num + node
    function add_rn(num, node1) result(new_node)
        real(dp), intent(in) :: num
        type(node_t), intent(in), target :: node1
        type(node_t), pointer :: new_node

        allocate (new_node)

        new_node%value = num + node1%value
        allocate (new_node%parents(1))

        new_node%parents(1)%node => node1
        new_node%parents(1)%local_grad = 1.0_dp

    end function add_rn

    !> node + num
    function add_nr(node1, num) result(new_node)
        type(node_t), intent(in), target :: node1
        real(dp), intent(in) :: num
        type(node_t), pointer :: new_node

        new_node => add_rn(num, node1)

    end function add_nr

    !> node + node
    function add_nn(node1, node2) result(new_node)
        type(node_t), intent(in), target :: node1
        type(node_t), intent(in), target :: node2
        type(node_t), pointer :: new_node

        allocate (new_node)

        new_node%value = node1%value + node2%value
        allocate (new_node%parents(2))

        new_node%parents(1)%node => node1
        new_node%parents(1)%local_grad = 1.0_dp

        new_node%parents(2)%node => node2
        new_node%parents(2)%local_grad = 1.0_dp

    end function add_nn

    !> real x node
    function mult_rn(num, node1) result(new_node)
        real(dp), intent(in) :: num
        type(node_t), intent(in), target :: node1
        type(node_t), pointer :: new_node

        allocate (new_node)

        new_node%value = num*node1%value
        allocate (new_node%parents(1))

        new_node%parents(1)%node => node1
        new_node%parents(1)%local_grad = num

    end function mult_rn

    !> node x real
    function mult_nr(node1, num) result(new_node)
        type(node_t), intent(in), target :: node1
        real(dp), intent(in) :: num
        type(node_t), pointer :: new_node

        new_node => mult_rn(num, node1)

    end function mult_nr

    !> node x node
    function mult_nn(node1, node2) result(new_node)
        type(node_t), intent(in), target :: node1
        type(node_t), intent(in), target :: node2
        type(node_t), pointer :: new_node

        allocate (new_node)

        new_node%value = node1%value*node2%value
        allocate (new_node%parents(2))

        new_node%parents(1)%node => node1
        new_node%parents(1)%local_grad = node2%value

        new_node%parents(2)%node => node2
        new_node%parents(2)%local_grad = node1%value

    end function mult_nn

    !> node exp
    function exp_n(node1) result(new_node)
        type(node_t), intent(in), target :: node1
        type(node_t), pointer :: new_node

        allocate (new_node)

        new_node%value = exp(node1%value)
        allocate (new_node%parents(1))

        new_node%parents(1)%node => node1
        new_node%parents(1)%local_grad = new_node%value

    end function exp_n

    !> Init backward: head
    subroutine backward_head(self)
        class(node_t), intent(inout) :: self

        self%grad = 1.0_dp

        select case (size(self%parents))
        case (0)
        case (1)
            call self%parents(1)%node%backward(self%parents(1)%local_grad)
        case (2)
            call self%parents(1)%node%backward(self%parents(1)%local_grad)
            call self%parents(2)%node%backward(self%parents(2)%local_grad)
        end select

    end subroutine backward_head

    !> Continue backward
    recursive subroutine backward_next(self, out)
        class(node_t), intent(inout) :: self
        real(dp), intent(in) :: out

        !! locals
        real(dp) :: local_grad
        integer :: i

        self%grad = self%grad + out

        select case (size(self%parents))
        case (0)
        case (1)
            local_grad = out*self%parents(1)%local_grad
            call self%parents(1)%node%backward(local_grad)
        case (2)

            do i = 1, 2
                local_grad = out*self%parents(i)%local_grad
                call self%parents(i)%node%backward(local_grad)
            end do

        end select

    end subroutine backward_next

    function sub_nn(node1, node2) result(new_node)
        type(node_t), intent(in), target :: node1
        type(node_t), intent(in), target :: node2
        type(node_t), pointer :: new_node

        new_node => add_nn(node1, mult_rn(-1.0_dp, node2))

    end function sub_nn

    function sub_nr(node1, num) result(new_node)
        type(node_t), intent(in), target :: node1
        real(dp), intent(in) :: num
        type(node_t), pointer :: new_node

        new_node => add_nr(node1, -num)

    end function sub_nr

    function sub_rn(num, node1) result(new_node)
        real(dp), intent(in) :: num
        type(node_t), intent(in), target :: node1
        type(node_t), pointer :: new_node

        new_node => add_rn(num, mult_rn(-1.0_dp, node1))

    end function sub_rn

    function div_nn(node1, node2) result(new_node)
        type(node_t), intent(in), target :: node1
        type(node_t), intent(in), target :: node2
        type(node_t), pointer :: new_node

        new_node => mult_nn(node1, div_rn(1.0_dp, node2))

    end function div_nn

    function div_nr(node1, num) result(new_node)
        type(node_t), intent(in), target :: node1
        real(dp), intent(in) :: num
        type(node_t), pointer :: new_node

        new_node => mult_nr(node1, 1.0_dp/num)

    end function div_nr

    function div_rn(num, node1) result(new_node)
        real(dp), intent(in) :: num
        type(node_t), intent(in), target :: node1
        type(node_t), pointer :: new_node

        allocate (new_node)

        new_node%value = num/node1%value
        allocate (new_node%parents(1))

        new_node%parents(1)%node => node1
        new_node%parents(1)%local_grad = -num/node1%value**2

    end function div_rn

    !> TO CHECK:
    function pow_nn(node1, node2) result(new_node)
        type(node_t), intent(in), target :: node1
        type(node_t), intent(in), target :: node2
        type(node_t), pointer :: new_node

        allocate (new_node)

        new_node%value = node1%value**node2%value
        allocate (new_node%parents(2))

        new_node%parents(1)%node => node1
        new_node%parents(1)%local_grad = new_node%value*(log(node1%value) + node2%value/node1%value)

        new_node%parents(2)%node => node2
        new_node%parents(2)%local_grad = new_node%value*(log(node1%value) + node2%value/node1%value)

    end function pow_nn

    function pow_nr(node1, num) result(new_node)
        type(node_t), intent(in), target :: node1
        real(dp), intent(in) :: num
        type(node_t), pointer :: new_node

        allocate (new_node)

        new_node%value = node1%value**num
        allocate (new_node%parents(1))

        new_node%parents(1)%node => node1
        new_node%parents(1)%local_grad = num*new_node%value**(num - 1.0_dp)

    end function pow_nr

    function pow_rn(num, node1) result(new_node)
        real(dp), intent(in) :: num
        type(node_t), intent(in), target :: node1
        type(node_t), pointer :: new_node

        allocate (new_node)

        new_node%value = num**node1%value
        allocate (new_node%parents(1))

        new_node%parents(1)%node => node1
        new_node%parents(1)%local_grad = num**node1%value*log(num)

    end function pow_rn

    function abs_n(node1) result(new_node)
        type(node_t), intent(in), target :: node1
        type(node_t), pointer :: new_node

        allocate (new_node)

        new_node%value = abs(node1%value)
        allocate (new_node%parents(1))

        new_node%parents(1)%node => node1
        new_node%parents(1)%local_grad = merge(-1.0_dp, 1.0_dp, node1%value < 0.0_dp)

    end function abs_n
    
    function sin_n(node1) result(new_node)
        type(node_t), intent(in), target :: node1
        type(node_t), pointer :: new_node

        allocate (new_node)

        new_node%value = sin(node1%value)
        allocate (new_node%parents(1))

        new_node%parents(1)%node => node1
        new_node%parents(1)%local_grad = cos(node1%value)

    end function sin_n
    
    function cos_n(node1) result(new_node)
        type(node_t), intent(in), target :: node1
        type(node_t), pointer :: new_node

        allocate (new_node)

        new_node%value = cos(node1%value)
        allocate (new_node%parents(1))

        new_node%parents(1)%node => node1
        new_node%parents(1)%local_grad = -sin(node1%value)

    end function cos_n
    
    function tan_n(node1) result(new_node)
        type(node_t), intent(in), target :: node1
        type(node_t), pointer :: new_node

        allocate (new_node)

        new_node%value = tan(node1%value)
        allocate (new_node%parents(1))

        new_node%parents(1)%node => node1
        new_node%parents(1)%local_grad = 1.0_dp/(cos(node1%value)**2)

    end function tan_n
    
    function log_n(node1) result(new_node)
        type(node_t), intent(in), target :: node1
        type(node_t), pointer :: new_node
        
        allocate (new_node)
        
        new_node%value = log(node1%value)
        allocate (new_node%parents(1))
        
        new_node%parents(1)%node => node1
        new_node%parents(1)%local_grad = 1.0_dp/node1%value
        
    end function log_n
    
    function log10_n(node1) result(new_node)
        type(node_t), intent(in), target :: node1
        type(node_t), pointer :: new_node
        
        allocate (new_node)
        
        new_node%value = log10(node1%value)
        allocate (new_node%parents(1))
        
        new_node%parents(1)%node => node1
        new_node%parents(1)%local_grad = 1.0_dp/node1%value/log(10.0_dp)
        
    end function log10_n
    
    function sqrt_n(node1) result(new_node)
        use, intrinsic :: ieee_arithmetic, only: ieee_value, NAN => ieee_quiet_nan
        type(node_t), intent(in), target :: node1
        type(node_t), pointer :: new_node
        
        allocate (new_node)
        
        new_node%value = sqrt(node1%value)
        allocate (new_node%parents(1))
        
        new_node%parents(1)%node => node1
        new_node%parents(1)%local_grad = merge(0.5_dp/new_node%value, ieee_value(1.0_dp, NAN), node1%value >= 0.0_dp) !TODO: NAN
        
    end function sqrt_n

end module auto_diff
