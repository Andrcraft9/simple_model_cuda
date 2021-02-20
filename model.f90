program model
    use mpp_module
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
    integer :: it
    procedure(envoke_empty_kernel), pointer :: sub_kernel
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
        sub_kernel => envoke_sw_simple_kernel
        call envoke(sub_kernel, it)
    enddo

    if (mpp_is_master())  then
        call end_timer(t_local)
        mpp_time_model_step =  t_local
        print *, "MODEL: CPU Solver Time:", mpp_time_model_step
    endif
    call check_answer(iters)

    !
    ! GPU Solver
    !

    ! Init data (read/set)
    call init_ocean_data
    call init_grid_data

    ! Solver
    if (mpp_is_master())  then
        print *, "MODEL: Start GPU Solver"
        mpp_time_model_step = 0.0d0
        call start_timer(t_local)
    endif
    
    do it = 1, iters
        sub_kernel => envoke_sw_simple_kernel_gpu
        call envoke(sub_kernel, it)
    enddo

    if (mpp_is_master())  then
        call end_timer(t_local)
        mpp_time_model_step =  t_local
        print *, "MODEL: GPU Solver Time:", mpp_time_model_step
    endif
    call check_answer(iters)

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