module ad_intrinsic

    use ad_kinds, only: rk
    use ad_types, only: tree_t, assignment(=)
    implicit none
    private

    public :: abs, max, min
    public :: sin, cos, tan
    public :: exp, sqrt, log, log10

    interface abs
        module procedure :: abs_t
    end interface abs

    interface max
        module procedure :: max_tt
        module procedure :: max_tr
        ! Ambiguous interfaces in generic interface 'max' for 'max_tr' at (1) and 'max_rt' here
        ! module procedure :: max_rt
    end interface max

    interface min
        module procedure :: min_tt
        module procedure :: min_tr
        ! module procedure :: min_rt
    end interface min

    interface sin
        module procedure :: sin_t
    end interface sin

    interface cos
        module procedure :: cos_t
    end interface cos

    interface tan
        module procedure :: tan_t
    end interface tan

    interface exp
        module procedure :: exp_t
    end interface exp

    interface sqrt
        module procedure :: sqrt_t
    end interface sqrt

    interface log
        module procedure :: log_t
    end interface log

    interface log10
        module procedure :: log10_t
    end interface log10

contains

    impure elemental function abs_t(t1) result(t)
        type(tree_t), intent(in) :: t1
        type(tree_t) :: t

        t = abs(t1%node%value)

        t%node%left => t1%node
        t%node%left_grad = merge(-1.0_rk, 1.0_rk, t1%node%value < 0.0_rk)

    end function abs_t

    impure elemental function max_tt(t1, t2) result(t)
        type(tree_t), intent(in) :: t1, t2
        type(tree_t) :: t

        t = max(t1%node%value, t2%node%value)

        t%node%left => t1%node
        t%node%left_grad = 1.0_rk

    end function max_tt

    ! impure elemental function max_rt(r, t1) result(t)
    !     real(rk), intent(in) :: r
    !     type(tree_t), intent(in) :: t1
    !     type(tree_t) :: t

    !     allocate (t%node)
    !     t%node%value = max(r, t1%node%value)

    !     t%node%left => t1%node
    !     t%node%left_grad = 1.0_rk

    ! end function max_rt

    impure elemental function max_tr(t1, r) result(t)
        type(tree_t), intent(in) :: t1
        real(rk), intent(in) :: r
        type(tree_t) :: t

        t = max(t1%node%value, r)

        t%node%left => t1%node
        t%node%left_grad = 1.0_rk

    end function max_tr

    impure elemental function min_tt(t1, t2) result(t)
        type(tree_t), intent(in) :: t1, t2
        type(tree_t) :: t

        t = min(t1%node%value, t2%node%value)

        t%node%left => t1%node
        t%node%left_grad = 1.0_rk

    end function min_tt

    ! impure elemental function min_rt(r, t1) result(t)
    !     real(rk), intent(in) :: r
    !     type(tree_t), intent(in) :: t1
    !     type(tree_t) :: t

    !     allocate (t%node)
    !     t%node%value = min(r, t1%node%value)

    !     t%node%left => t1%node
    !     t%node%left_grad = 1.0_rk

    ! end function min_rt

    impure elemental function min_tr(t1, r) result(t)
        type(tree_t), intent(in) :: t1
        real(rk), intent(in) :: r
        type(tree_t) :: t

        t = min(t1%node%value, r)

        t%node%left => t1%node
        t%node%left_grad = 1.0_rk

    end function min_tr

    impure elemental function sin_t(t1) result(t)
        type(tree_t), intent(in) :: t1
        type(tree_t) :: t

        t = sin(t1%node%value)

        t%node%left => t1%node
        t%node%left_grad = cos(t1%node%value)

    end function sin_t

    impure elemental function cos_t(t1) result(t)
        type(tree_t), intent(in) :: t1
        type(tree_t) :: t

        t = cos(t1%node%value)

        t%node%left => t1%node
        t%node%left_grad = -sin(t1%node%value)

    end function cos_t

    impure elemental function tan_t(t1) result(t)
        type(tree_t), intent(in) :: t1
        type(tree_t) :: t

        t = tan(t1%node%value)

        t%node%left => t1%node
        t%node%left_grad = 1.0_rk/(cos(t1%node%value)*cos(t1%node%value))

    end function tan_t

    impure elemental function exp_t(t1) result(t)
        type(tree_t), intent(in) :: t1
        type(tree_t) :: t

        t = exp(t1%node%value)

        t%node%left => t1%node
        t%node%left_grad = t%node%value

    end function exp_t

    impure elemental function sqrt_t(t1) result(t)
        use, intrinsic :: ieee_arithmetic, only: ieee_value, NAN => ieee_quiet_nan
        type(tree_t), intent(in) :: t1
        type(tree_t) :: t

        t = sqrt(t1%node%value)

        t%node%left => t1%node
        t%node%left_grad = merge(0.5_rk/t%node%value, ieee_value(1.0_rk, NAN), t1%node%value >= 0.0_rk) !TODO: NAN

    end function sqrt_t
    
    impure elemental function log_t(t1) result(t)
        type(tree_t), intent(in) :: t1
        type(tree_t) :: t

        t = log(t1%node%value)

        t%node%left => t1%node
        t%node%left_grad = 1.0_rk/t1%node%value

    end function log_t
    
    impure elemental function log10_t(t1) result(t)
        type(tree_t), intent(in) :: t1
        type(tree_t) :: t

        t = log10(t1%node%value)

        t%node%left => t1%node
        t%node%left_grad = 1.0_rk/t1%node%value/log(10.0_rk)

    end function log10_t

end module ad_intrinsic
