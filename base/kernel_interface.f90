module kernel_interface_module
    
    use kind_module, only: wp8 => SHR_KIND_R8, wp4 => SHR_KIND_R4
    use mpp_module
    use mpp_sync_module
    use decomposition_module, only: domain_type, domain => domain_data

    implicit none
    save
    public

contains

!-----------------------------------------------------------------------------!
!-------------------------- Interface subroutines ----------------------------!
!-----------------------------------------------------------------------------!
    subroutine envoke_empty_kernel(k, it)
        integer, intent(in) :: k
        integer, intent(in) :: it
    end subroutine

    subroutine envoke_empty_sync(k, sync_parameters)
        integer, intent(in) :: k
        type(sync_parameters_type), intent(in) :: sync_parameters
    end subroutine 

    subroutine envoke(sub_kernel, sub_sync, it)
        procedure(envoke_empty_kernel), pointer :: sub_kernel
        procedure(envoke_empty_sync), pointer :: sub_sync
        integer, intent(in) :: it 
        
        integer :: k
        type(sync_parameters_type) :: sync_params_dummy, sync_params_htod, sync_params_dtoh

        sync_params_dummy%sync_mode = -1
        sync_params_dtoh%sync_mode = 0
        sync_params_htod%sync_mode = 1

        do k = 1, domain%bcount
            call sub_sync(k, sync_params_htod)
            call sub_kernel(k, it)
            call sub_sync(k, sync_params_dtoh)
        enddo

    end subroutine

    subroutine envoke_async_gpu(sub_kernel, sub_sync, it)
        procedure(envoke_empty_kernel), pointer :: sub_kernel
        procedure(envoke_empty_sync), pointer :: sub_sync
        integer, intent(in) :: it

        integer :: k
        integer :: istat
        type(sync_parameters_type) :: sync_params_dummy, sync_params_htod, sync_params_dtoh, async_params_htod, async_params_dtoh
        
        sync_params_dummy%sync_mode = -1
        sync_params_dtoh%sync_mode = 0
        sync_params_htod%sync_mode = 1
        async_params_dtoh%sync_mode = 2
        async_params_htod%sync_mode = 3

        do k = 1, domain%bcount
            call sub_sync(k, async_params_htod)
            call sub_kernel(k, it)
            call sub_sync(k, async_params_dtoh)
        enddo

    end subroutine

end module kernel_interface_module