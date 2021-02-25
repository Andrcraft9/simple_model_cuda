program model
    use kind_module, only: wp8 => SHR_KIND_R8, wp4 => SHR_KIND_R4
    use mpp_module
    use mpp_sync_module
    use basinpar_module, only: nx, ny
    use errors_module
    use decomposition_module
    use grid_module
    use ocean_module
    use init_data_module
    use kernel_interface_module
    use solver_module
    use solver_gpu_module

    implicit none

    real(wp8) :: t_local
    real(wp4) :: t_local_real
    integer :: it
    procedure(envoke_empty_kernel), pointer :: sub_kernel
    procedure(envoke_empty_sync), pointer :: sub_sync
    character(len=32) :: arg
    integer :: bppnx, bppny, iters

    ! Get arguments from CMD:
    if (command_argument_count() == 5) then
        call get_command_argument(1, arg)
        read(arg, *) nx
        call get_command_argument(2, arg)
        read(arg, *) ny
        call get_command_argument(3, arg)
        read(arg, *) bppnx
        call get_command_argument(4, arg)
        read(arg, *) bppny
        call get_command_argument(5, arg)
        read(arg, *) iters
        print *, 'MODEL:   nx ,  ny   = ', nx, ny
        print *, 'MODEL: bppnx, bppny = ', bppnx, bppny
        print *, 'MODEL:     iters    = ', iters
    else
        print *, 'Please, pass nx, ny, bppnx, bppny, itersto CMD arguments'
        return
    endif

    !
    ! Init
    !

    ! Make decomposition
    call mpp_init
    call domain_data%init(bppnx, bppny)
    ! Allocate data
    call ocean_data%init(domain_data)
    call grid_data%init(domain_data)

    !
    ! CPU Solver
    !

    ! Init data (read/set)
    call init_ocean_data
    call init_grid_data

    ! Solver
    if (mpp_is_master())  then
        print *, "MODEL: Start CPU Solver"
        mpp_time_model_step = 0.0d0
        call start_timer(t_local)
    endif
    
    do it = 1, iters
        sub_kernel => envoke_sw_update_ssh_kernel
        sub_sync   => envoke_sw_update_ssh_sync
        call envoke(sub_kernel, sub_sync, it)
    enddo

    if (mpp_is_master())  then
        call end_timer(t_local)
        mpp_time_model_step =  t_local
        print *, "MODEL: CPU Solver Time:", mpp_time_model_step
    endif
    !call check_answer(iters)

    !
    ! GPU Solver
    !

    ! Init data (read/set)
    call init_ocean_data
    call init_grid_data

    ! Solver
    if (mpp_is_master())  then
        print *, "MODEL: Start GPU Solver"
        gpu_time_model_step = 0.0
        call start_gpu_timer()
    endif
    
    do it = 1, iters
        sub_kernel => envoke_sw_update_ssh_kernel_gpu
        sub_sync   => envoke_sw_update_ssh_sync_gpu
        call envoke(sub_kernel, sub_sync, it)
    enddo

    if (mpp_is_master())  then
        call end_gpu_timer(t_local_real)
        gpu_time_model_step =  t_local_real
        print *, "MODEL: GPU Solver Time:", gpu_time_model_step
    endif
    !call check_answer(iters)

    !
    ! Finalize
    !

    ! Clear data
    call ocean_data%clear(domain_data)
    call grid_data%clear(domain_data)
    ! Clear decomposition
    call domain_data%clear()
    call mpp_finalize

end program model