module solver_module

    use kind_module, only: wp8 => SHR_KIND_R8, wp4 => SHR_KIND_R4
    use kernel_interface_module
    use decomposition_module, only: domain_type, domain => domain_data
    use ocean_module, only: ocean_type, ocean_data
    use grid_module, only: grid_type, grid_data

    implicit none
    save
    public

contains

subroutine envoke_sw_simple_kernel(k)
    integer, intent(in) :: k

    call sw_simple_kernel(domain%bnx_start(k), domain%bnx_end(k), domain%bny_start(k), domain%bny_end(k),  &
                          ocean_data%ssh%block(k)%field)

end subroutine

subroutine sw_simple_kernel(nx_start, nx_end, ny_start, ny_end, ssh)

    integer, intent(in) :: nx_start, nx_end, ny_start, ny_end
    real(wp8), intent(inout) :: ssh(nx_start:nx_end, ny_start:ny_end)

    integer :: m, n

    do n=ny_start,ny_end
        do m=nx_start,nx_end
            ssh(m,n) = ssh(m,n) + 1.0d0
        enddo
    enddo

end subroutine

end module solver_module