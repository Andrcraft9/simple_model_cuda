module mpp_sync_module
    ! Sync data of different types, depends on domain
    use kind_module, only: wp8 => SHR_KIND_R8, wp4 => SHR_KIND_R4
    use cudafor
    use mpp_module
    use decomposition_module, only: domain_type

    implicit none
    save
    public

    type, public :: sync_parameters_type
        integer :: sync_mode
    end type sync_parameters_type

    integer(kind=cuda_stream_kind), allocatable :: sync_gpu_streams(:)

contains

subroutine mpp_sync_init(domain)
    type(domain_type), intent(in) :: domain
    integer :: k, istat

    allocate(sync_gpu_streams(domain%bcount))
    do k = 1, domain%bcount
        istat = cudaStreamCreate(sync_gpu_streams(k))
    enddo
end subroutine 

subroutine mpp_sync_finalize(domain)
    type(domain_type), intent(in) :: domain
    integer :: k, istat

    do k = 1, domain%bcount
        istat = cudaStreamDestroy(sync_gpu_streams(k))
    enddo
    deallocate(sync_gpu_streams)
end subroutine

end module mpp_sync_module