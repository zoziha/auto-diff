module ad_operator

    use ad_kinds, only: rk
    use ad_types, only: tree_t
    implicit none
    private

    public :: operator(+), operator(-), operator(*), operator(/), operator(**)

    interface operator(+)
        module procedure :: add_tt
        module procedure :: add_tr
        module procedure :: add_rt
    end interface operator(+)

    interface operator(-)
        module procedure :: sub_tt
        module procedure :: sub_tr
        module procedure :: sub_rt
    end interface operator(-)

    interface operator(*)
        module procedure :: mul_tt
        module procedure :: mul_tr
        module procedure :: mul_rt
    end interface operator(*)

    interface operator(/)
        module procedure :: div_tt
        module procedure :: div_tr
        module procedure :: div_rt
    end interface operator(/)

    interface operator(**)
        module procedure :: pow_tt
        module procedure :: pow_tr
        module procedure :: pow_rt
    end interface operator(**)

contains

    impure elemental function add_tt(t1, t2) result(t)
        type(tree_t), intent(in) :: t1, t2
        type(tree_t) :: t

        allocate (t%node)
        t%node%value = t1%node%value + t2%node%value

        t%node%left => t1%node
        t%node%right => t2%node

        t%node%left_grad = 1.0_rk
        t%node%right_grad = 1.0_rk

    end function add_tt

    impure elemental function add_tr(t1, r) result(t)
        type(tree_t), intent(in) :: t1
        real(rk), intent(in) :: r
        type(tree_t) :: t

        t = add_rt(r, t1)

    end function add_tr

    impure elemental function add_rt(r, t1) result(t)
        real(rk), intent(in) :: r
        type(tree_t), intent(in) :: t1
        type(tree_t) :: t

        allocate (t%node)
        t%node%value = t1%node%value + r

        t%node%left => t1%node
        t%node%left_grad = 1.0_rk

    end function add_rt

    impure elemental function sub_tt(t1, t2) result(t)
        type(tree_t), intent(in) :: t1, t2
        type(tree_t) :: t

        t = add_tt(t1, mul_rt(-1.0_rk, t2))

    end function sub_tt

    impure elemental function sub_tr(t1, r) result(t)
        type(tree_t), intent(in) :: t1
        real(rk), intent(in) :: r
        type(tree_t) :: t

        t = add_tr(t1, -r)

    end function sub_tr

    impure elemental function sub_rt(r, t1) result(t)
        real(rk), intent(in) :: r
        type(tree_t), intent(in) :: t1
        type(tree_t) :: t

        t = add_rt(r, mul_rt(-1.0_rk, t1))

    end function sub_rt

    impure elemental function mul_tt(t1, t2) result(t)
        type(tree_t), intent(in) :: t1, t2
        type(tree_t) :: t

        allocate (t%node)
        t%node%value = t1%node%value*t2%node%value

        t%node%left => t1%node
        t%node%right => t2%node

        t%node%left_grad = t2%node%value
        t%node%right_grad = t1%node%value

    end function mul_tt

    impure elemental function mul_tr(t1, r) result(t)
        type(tree_t), intent(in) :: t1
        real(rk), intent(in) :: r
        type(tree_t) :: t

        t = mul_rt(r, t1)

    end function mul_tr

    impure elemental function mul_rt(r, t1) result(t)
        real(rk), intent(in) :: r
        type(tree_t), intent(in) :: t1
        type(tree_t) :: t

        allocate (t%node)
        t%node%value = t1%node%value*r

        t%node%left => t1%node
        t%node%left_grad = r

    end function mul_rt

    impure elemental function div_tt(t1, t2) result(t)
        type(tree_t), intent(in) :: t1, t2
        type(tree_t) :: t

        t = mul_tt(t1, div_rt(1.0_rk, t2))

    end function div_tt

    impure elemental function div_tr(t1, r) result(t)
        type(tree_t), intent(in) :: t1
        real(rk), intent(in) :: r
        type(tree_t) :: t

        t = mul_tr(t1, 1.0_rk/r)

    end function div_tr

    impure elemental function div_rt(r, t1) result(t)
        real(rk), intent(in) :: r
        type(tree_t), intent(in) :: t1
        type(tree_t) :: t

        allocate (t%node)

        t%node%value = r/t1%node%value

        t%node%left => t1%node
        t%node%left_grad = -r/t1%node%value**2

    end function div_rt

    impure elemental function pow_tt(t1, t2) result(t)
        type(tree_t), intent(in) :: t1, t2
        type(tree_t) :: t

        allocate (t%node)
        t%node%value = t1%node%value**t2%node%value

        t%node%left => t1%node
        t%node%right => t2%node

        ! t%node%left_grad = t2%node%value*t1%node%value**(t2%node%value-1.0_rk)
        ! t%node%right_grad = t1%node%value**t2%node%value*log(t1%node%value)
        t%node%left_grad = t%node%value*(log(t1%node%value) + t2%node%value/t1%node%value)
        t%node%right_grad = t%node%value*(log(t1%node%value) + t2%node%value/t1%node%value)

    end function pow_tt
    
    impure elemental function pow_tr(t1, r) result(t)
        type(tree_t), intent(in) :: t1
        real(rk), intent(in) :: r
        type(tree_t) :: t

        allocate(t%node)
        t%node%value = t1%node%value**r
        
        t%node%left => t1%node
        t%node%left_grad = r*t%node%value**(r-1.0_rk)

    end function pow_tr
    
    impure elemental function pow_rt(r, t1) result(t)
        real(rk), intent(in) :: r
        type(tree_t), intent(in) :: t1
        type(tree_t) :: t

        allocate(t%node)
        t%node%value = r**t1%node%value
        
        t%node%left => t1%node
        t%node%left_grad = r**t1%node%value*log(r)

    end function pow_rt

end module ad_operator
