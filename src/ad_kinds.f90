module ad_kinds

    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private
    
    public :: rk
    
    integer, parameter :: rk = real64

end module ad_kinds