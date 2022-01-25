module ad_usr_func

    use ad_kinds, only: rk
    use ad_types, only: tree_t, assignment(=)
    implicit none
    private
    
    public :: sigmoid
    
contains
    
    impure elemental function sigmoid(t1) result(t)
        type(tree_t), intent(in) :: t1
        type(tree_t) :: t
        
        t = 1.0_rk/(1.0_rk + exp(-t1%node%value))
        
        t%node%left => t1%node
        t%node%left_grad = t%node%value*(1.0_rk - t%node%value)
        
    end function sigmoid

end module ad_usr_func