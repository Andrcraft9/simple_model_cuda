module init_data_module
    ! Grid data

    use basinpar_module, only: dxst, dyst
    use decomposition_module, only: domain_type, domain => domain_data
    use ocean_module, only: ocean_type, ocean_data
    use grid_module, only: grid_type, grid_data

    implicit none
    save
    private

    public :: init_ocean_data
    public :: init_grid_data

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

end module init_data_module