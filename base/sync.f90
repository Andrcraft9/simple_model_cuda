module mpp_sync_module
    ! Sync data of different types, depends on domain
    use kind_module, only: wp8 => SHR_KIND_R8, wp4 => SHR_KIND_R4
    use mpp_module
    use decomposition_module, only: domain_type

    implicit none
    save
    private

    type, public :: sync_parameters_type
        integer :: sync_mode
    end type sync_parameters_type

    integer, allocatable :: sync_gpu_streams(:)

contains

subroutine mpp_sync_init(domain)
    type(domain_type), intent(in) :: domain
end subroutine 

end module mpp_sync_module