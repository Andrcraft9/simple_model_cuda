module kind_module
    ! All platform-independent data types

    implicit none
    save
    public

    ! Parameters (from E3SM)
    integer, parameter :: SHR_KIND_R8 = selected_real_kind(12) ! 8 byte real
    integer, parameter :: SHR_KIND_R4 = selected_real_kind( 6) ! 4 byte real
    integer, parameter :: SHR_KIND_RN = kind(1.0)              ! native real
    integer, parameter :: SHR_KIND_I8 = selected_int_kind (13) ! 8 byte integer
    integer, parameter :: SHR_KIND_I4 = selected_int_kind ( 6) ! 4 byte integer
    integer, parameter :: SHR_KIND_IN = kind(1)                ! native integer
    integer, parameter :: SHR_KIND_CL = 256                    ! long char
    integer, parameter :: SHR_KIND_CS = 80                     ! short char

end module kind_module
