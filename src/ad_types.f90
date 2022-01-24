module ad_types

    use ad_kinds, only: rk
    implicit none
    private

    public :: tree_t

    !> Linked list node
    type node_t

        real(rk) :: value  !! value of the node
        real(rk) :: grad = 0.0_rk   !! gradient of this node

        type(node_t), pointer :: left => null()
        real(rk) :: left_grad  !! gradient of the left node

        type(node_t), pointer :: right => null()
        real(rk), allocatable :: right_grad  !! gradient of the right node

    contains

        procedure :: backward => node_t_backward
        procedure :: destructor => node_t_destructor

    end type node_t

    !> Linked tree
    type tree_t
        type(node_t), pointer :: node => null()
    contains
        procedure :: constructor => tree_t_constructor
        procedure :: backward => tree_t_backward
        procedure :: get_value => tree_t_get_value
        procedure :: get_grad => tree_t_get_grad
        procedure :: destructor => tree_t_destructor
    end type tree_t

contains

    elemental subroutine tree_t_constructor(self, value)
        class(tree_t), intent(inout) :: self
        real(rk), intent(in) :: value

        allocate (self%node)
        self%node%value = value

    end subroutine tree_t_constructor

    elemental subroutine tree_t_backward(self)
        class(tree_t), intent(inout) :: self

        associate (node => self%node)

            self%node%grad = 1.0_rk

            if (associated(node%left)) &
                call node%left%backward(node%left_grad)
            if (associated(node%right)) &
                call node%right%backward(node%right_grad)

        end associate

    end subroutine tree_t_backward

    elemental subroutine node_t_backward(self, out)
        class(node_t), intent(inout) :: self
        real(rk), intent(in) :: out

        self%grad = self%grad + out

        if (associated(self%left)) call self%left%backward(out*self%left_grad)
        if (associated(self%right)) call self%right%backward(out*self%right_grad)

    end subroutine node_t_backward

    elemental function tree_t_get_value(self) result(value)
        class(tree_t), intent(in) :: self
        real(rk) :: value

        value = self%node%value

    end function tree_t_get_value
    
    elemental function tree_t_get_grad(self) result(grad)
        class(tree_t), intent(in) :: self
        real(rk) :: grad

        grad = self%node%grad

    end function tree_t_get_grad

    elemental subroutine tree_t_destructor(self)
        class(tree_t), intent(inout) :: self

        if (associated(self%node)) then
            call self%node%destructor()
            deallocate (self%node)
        end if

    end subroutine tree_t_destructor

    ! - GFortran >= 11.0
    ! - Intel Fortran >= 2019
    ! or report an error: ELEMENTAL attribute conflicts with RECURSIVE attribute.
    elemental recursive subroutine node_t_destructor(self)
        class(node_t), intent(inout) :: self

        if (associated(self%left)) then
            call self%left%destructor()
            deallocate (self%left)
        end if

        if (associated(self%right)) then
            call self%right%destructor()
            deallocate (self%right)
        end if

    end subroutine node_t_destructor

end module ad_types
