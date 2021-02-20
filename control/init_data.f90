module init_data_module
    ! Grid data

    use mpp_module
    use basinpar_module, only: dxst, dyst
    use errors_module
    use decomposition_module, only: domain_type, domain => domain_data
    use ocean_module, only: ocean_type, ocean_data
    use grid_module, only: grid_type, grid_data

    implicit none
    save
    private

    public :: init_ocean_data
    public :: init_grid_data

    public :: check_answer

contains

    subroutine init_ocean_data()

        call ocean_data%ssh%fill(domain, 0.0d0)
        call ocean_data%ubrtr%fill(domain, 0.0d0)
        call ocean_data%vbrtr%fill(domain, 0.0d0)

    end subroutine init_ocean_data

    subroutine init_grid_data()

        call grid_data%dxt%fill(domain, dxst)
        call grid_data%dyt%fill(domain, dyst)
        call grid_data%hhq%fill(domain, 500.0d0)
        call grid_data%hhu%fill(domain, 500.0d0)
        call grid_data%hhv%fill(domain, 500.0d0)
        call grid_data%lu%fill(domain, 1.0d0)

    end subroutine init_grid_data

    subroutine check_answer(iters)
        integer, intent(in) :: iters
        integer :: k, m, n, ierr
        ierr = 0
        do k = 1, domain%bcount
            do n = domain%bny_start(k), domain%bny_end(k)
                do m = domain%bnx_start(k), domain%bnx_end(k)
                    if (ocean_data%ssh%block(k)%field(m, n) /= iters) then
                        ierr = 1
                    endif
                enddo
            enddo
        enddo
        call check_error(ierr, "Wrong Answer!")
        if (mpp_is_master())  then
            print *, "CHECK: Ok answer"
        endif
    end subroutine

end module init_data_module