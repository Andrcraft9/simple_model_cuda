module ocean_module
    ! Ocean data

    use mpp_module
    use decomposition_module, only: domain_type
    use data_types_module, only: data2D_real8_type

    implicit none
    save
    private

    type, public :: ocean_type
        type(data2D_real8_type) :: ssh,    &  ! sea surface height (SSH) at current  time step [m] (internal mode)
                                   ubrtr,  &  ! barotropic velocity      zonal[m/s] at current time step (internal mode)
                                   vbrtr      ! barotropic velocity meridional[m/s] at current time step (internal mode)
    contains
        procedure, public  :: init
        procedure, public  :: clear
    end type ocean_type

    type(ocean_type), public, target :: ocean_data

contains

    subroutine init(this, domain)
        ! Initialization of grid data
        class(ocean_type), intent(inout) :: this
        type(domain_type), intent(in) :: domain
        real(8) :: r8

        ! CPU: init first
        call this%ssh%init(domain)
        call this%ubrtr%init(domain)
        call this%vbrtr%init(domain)

        if (mpp_is_master()) then
            print *, "INIT: CPU OK"
        endif

        ! GPU: init last
        call this%ssh%init_gpu(domain)

        if (mpp_is_master()) then
            print *, "INIT: GPU OK"
        endif

        if (mpp_is_master()) then
            print *, "INIT: SIZEOF OF FIRST SSH BLOCK  (B): ", sizeof(this%ssh%block(1)%field)
            print *, "INIT: SIZEOF OF FIRST SSH BLOCK (KB): ", sizeof(this%ssh%block(1)%field) / 1024.0
            print *, "INIT: SIZEOF OF FIRST SSH BLOCK (MB): ", sizeof(this%ssh%block(1)%field) / 1024.0 / 1024.0
            print *, "INIT: SIZEOF OF FIRST SSH BLOCK (GB): ", sizeof(this%ssh%block(1)%field) / 1024.0 / 1024.0 / 1024.0
        endif
    end subroutine

    subroutine clear(this, domain)
        ! Clear grid data
        class(ocean_type), intent(inout) :: this
        type(domain_type), intent(in) :: domain

        ! GPU: clear first
        call this%ssh%clear_gpu(domain)

        ! CPU: clear last
        call this%ssh%clear(domain)
        call this%ubrtr%clear(domain)
        call this%vbrtr%clear(domain)
    end subroutine

endmodule ocean_module
