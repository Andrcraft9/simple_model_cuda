module solver_module

    use kind_module, only: wp8 => SHR_KIND_R8, wp4 => SHR_KIND_R4
    use kernel_interface_module
    use mpp_sync_module
    use decomposition_module, only: domain_type, domain => domain_data
    use ocean_module, only: ocean_type, ocean_data
    use grid_module, only: grid_type, grid_data

    implicit none
    save
    public

contains

subroutine envoke_sw_simple_kernel(k, it)
    integer, intent(in) :: k
    integer, intent(in) :: it

    call sw_simple_kernel(domain%bnx_start(k), domain%bnx_end(k), domain%bny_start(k), domain%bny_end(k),  &
                          ocean_data%ssh%block(k)%field)

end subroutine

subroutine envoke_sw_simple_sync(k, sync_parameters)
    integer, intent(in) :: k
    type(sync_parameters_type), intent(in) :: sync_parameters
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

subroutine envoke_sw_update_ssh_kernel(k, it)
    integer, intent(in) :: k
    integer, intent(in) :: it

    call sw_update_ssh_kernel(domain%bnx_start(k), domain%bnx_end(k), domain%bny_start(k), domain%bny_end(k),  &
                              1.0d0,  &
                              grid_data%lu %block(k)%field,  &
                              grid_data%dxt%block(k)%field,  &
                              grid_data%dyt%block(k)%field,  &
                              grid_data%hhu%block(k)%field,  &
                              grid_data%hhv%block(k)%field,  &
                              ocean_data%ssh   %block(k)%field,  &
                              ocean_data%ubrtr %block(k)%field,  &
                              ocean_data%vbrtr %block(k)%field)

end subroutine

subroutine envoke_sw_update_ssh_sync(k, sync_parameters)
    integer, intent(in) :: k
    type(sync_parameters_type), intent(in) :: sync_parameters
end subroutine 

subroutine sw_update_ssh_kernel(nx_start, nx_end, ny_start, ny_end,  &
                                tau, lu, dx, dy, hhu, hhv, ssh, ubrtr, vbrtr)

  integer, intent(in) :: nx_start, nx_end, ny_start, ny_end
  real(wp8), intent(in) :: tau
  real(wp8), intent(in) :: lu(nx_start:nx_end, ny_start:ny_end)
  real(wp8), intent(in) :: dx(nx_start:nx_end, ny_start:ny_end),   &
                           dy(nx_start:nx_end, ny_start:ny_end)
  real(wp8), intent(in) :: hhu(nx_start:nx_end, ny_start:ny_end),  &
                           hhv(nx_start:nx_end, ny_start:ny_end)
  real(wp8), intent(in) :: ubrtr(nx_start:nx_end, ny_start:ny_end),  &
                           vbrtr(nx_start:nx_end, ny_start:ny_end)
  real(wp8), intent(inout) :: ssh(nx_start:nx_end, ny_start:ny_end)

  integer :: m, n

  do n=ny_start + 1, ny_end - 1
    do m=nx_start + 1, nx_end - 1

        if(lu(m,n)>0.5) then
            ssh(m,n) = ssh(m,n) + 2.0d0*tau*(  &
            - ( ubrtr(m,n)*hhu(m,n)*dy(m,n) - ubrtr(m-1,n)*hhu(m-1,n)*dy(m-1,n)             &
              + vbrtr(m,n)*hhv(m,n)*dx(m,n) - vbrtr(m,n-1)*hhv(m,n-1)*dx(m,n-1) )/(dx(m,n)*dy(m,n))  )
        endif

    enddo
  enddo

end subroutine

end module solver_module