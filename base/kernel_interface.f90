module kernel_interface_module
    
    use kind_module, only: wp8 => SHR_KIND_R8, wp4 => SHR_KIND_R4
    use mpp_module
    use decomposition_module, only: domain_type, domain => domain_data

    implicit none
    save
    public

contains

!-----------------------------------------------------------------------------!
!-------------------------- Interface subroutines ----------------------------!
!-----------------------------------------------------------------------------!
    subroutine envoke_empty_kernel(k)
        integer, intent(in) :: k
    end subroutine 

    subroutine envoke(sub_kernel)
        procedure(envoke_empty_kernel), pointer :: sub_kernel
    
        integer :: k

        do k = 1, domain%bcount
            call sub_kernel(k)
        enddo

    end subroutine

end module kernel_interface_module