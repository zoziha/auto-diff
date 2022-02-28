module ad_kinds

    use, intrinsic :: iso_fortran_env, only: real64, real32
    implicit none
    private
    
    public :: rk
    
    integer, parameter :: rk = real32

end module ad_kinds
