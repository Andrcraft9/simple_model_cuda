program model
    use mpp_module
    use errors_module
    use decomposition_module
    use grid_module
    use ocean_module
    use init_data_module
    use kernel_interface_module
    use solver_module

    implicit none

    real(wp8) :: t_local
    integer :: k, m, n, ierr
    procedure(envoke_empty_kernel), pointer :: sub_kernel

    ! Make decomposition
    call mpp_init
    call domain_data%init(2, 2)

    ! Allocate data
    call ocean_data%init(domain_data)
    call grid_data%init(domain_data)

    ! Init data (read/set)
    call init_ocean_data
    call init_grid_data

    ! Solver
    if (mpp_is_master())  then
        print *, "MODEL: Start Solver"
        call start_timer(t_local)
    endif
    
    sub_kernel => envoke_sw_simple_kernel
    call envoke(sub_kernel)

    if (mpp_is_master())  then
        call end_timer(t_local)
        mpp_time_model_step =  t_local
        print *, "MODEL: Solver Time:", mpp_time_model_step
    endif

    ! Check Answer
    ierr = 0
    do k = 1, domain_data%bcount
        do n = domain_data%bny_start(k), domain_data%bny_end(k)
            do m = domain_data%bnx_start(k), domain_data%bnx_end(k)
                if (ocean_data%ssh%block(k)%field(m, n) < 1.0d0) then
                    ierr = 1
                endif
            enddo
        enddo
    enddo
    call check_error(ierr, "Wrong Answer!")

    ! Clear data
    call ocean_data%clear(domain_data)
    call grid_data%clear(domain_data)
    ! Clear decomposition
    call domain_data%clear()

    call mpp_finalize

end program model