module data_types_module

    use kind_module, only: wp8 => SHR_KIND_R8, wp4 => SHR_KIND_R4
    use mpp_module
    use decomposition_module

    implicit none
    save
    private

    ! Data types for Parallel System layer
    type, public :: block2D_real4_type
        real(wp4), allocatable :: field(:, :)
        real(wp4), allocatable, device :: field_gpu(:, :)
    end type block2D_real4_type

    type, public :: block2D_real8_type
        real(wp8), allocatable :: field(:, :)
        real(wp8), allocatable, device :: field_gpu(:, :)
    end type block2D_real8_type

    ! Data types for Algorithm layer
    type, public :: data2D_real4_type
        type(block2D_real4_type), allocatable :: block(:)
    contains
        procedure, public :: init => init_data2D_real4
        procedure, public :: clear => clear_data2D_real4
        procedure, public :: fill => fill_data2D_real4
    end type data2D_real4_type

    type, public :: data2D_real8_type
        type(block2D_real8_type), allocatable :: block(:)
    contains
        procedure, public :: init => init_data2D_real8
        procedure, public :: clear => clear_data2D_real8
        procedure, public :: fill => fill_data2D_real8
    end type data2D_real8_type

contains

    subroutine init_data2D_real4(this, domain)
        class(data2D_real4_type), intent(inout) :: this
        type(domain_type), intent(in) :: domain
        integer :: k

        ! CPU: init first
        allocate(this%block(domain%bcount))
        do k = 1, domain%bcount
            allocate(this%block(k)%field(domain%bnx_start(k) : domain%bnx_end(k), domain%bny_start(k) : domain%bny_end(k)))
            this%block(k)%field = 0.0
        enddo

        ! GPU: init last
        do k = 1, domain%bcount
            allocate(this%block(k)%field_gpu(domain%bnx_start(k) : domain%bnx_end(k), domain%bny_start(k) : domain%bny_end(k)))
            this%block(k)%field_gpu = 0.0
        enddo
    end subroutine

    subroutine clear_data2D_real4(this, domain)
        class(data2D_real4_type), intent(inout) :: this
        type(domain_type), intent(in) :: domain
        integer :: k

        ! GPU: clear first
        do k = 1, domain%bcount
            deallocate(this%block(k)%field_gpu)
        enddo

        ! CPU: clear last
        do k = 1, domain%bcount
            deallocate(this%block(k)%field)
        enddo
        deallocate(this%block)
    end subroutine

    subroutine init_data2D_real8(this, domain)
        class(data2D_real8_type), intent(inout) :: this
        type(domain_type), intent(in) :: domain
        integer :: k

        ! CPU: init first
        allocate(this%block(domain%bcount))
        do k = 1, domain%bcount
            allocate(this%block(k)%field(domain%bnx_start(k) : domain%bnx_end(k), domain%bny_start(k) : domain%bny_end(k)))
            this%block(k)%field = 0.0d0
        enddo

        ! GPU: init last
        do k = 1, domain%bcount
            allocate(this%block(k)%field_gpu(domain%bnx_start(k) : domain%bnx_end(k), domain%bny_start(k) : domain%bny_end(k)))
            this%block(k)%field_gpu = 0.0d0
        enddo
    end subroutine

    subroutine clear_data2D_real8(this, domain)
        class(data2D_real8_type), intent(inout) :: this
        type(domain_type), intent(in) :: domain
        integer :: k

        ! GPU: clear first
        do k = 1, domain%bcount
            deallocate(this%block(k)%field_gpu)
        enddo

        ! CPU: clear last
        do k = 1, domain%bcount
            deallocate(this%block(k)%field)
        enddo
        deallocate(this%block)
    end subroutine

    subroutine fill_data2D_real4(this, domain, val)
        class(data2D_real4_type), intent(inout) :: this
        type(domain_type), intent(in) :: domain
        real(wp4), intent(in) :: val
        integer :: k, m, n

        do k = 1, domain%bcount
            do n = domain%bny_start(k), domain%bny_end(k)
                do m = domain%bnx_start(k), domain%bnx_end(k)
                    this%block(k)%field(m, n) = val
                enddo
            enddo
        enddo

        ! Copy to GPU
        this%block(k)%field_gpu = this%block(k)%field
    end subroutine

    subroutine fill_data2D_real8(this, domain, val)
        class(data2D_real8_type), intent(inout) :: this
        type(domain_type), intent(in) :: domain
        real(wp8), intent(in) :: val
        integer :: k, m, n

        do k = 1, domain%bcount
            do n = domain%bny_start(k), domain%bny_end(k)
                do m = domain%bnx_start(k), domain%bnx_end(k)
                    this%block(k)%field(m, n) = val
                enddo
            enddo
        enddo

        ! Copy to GPU
        this%block(k)%field_gpu = this%block(k)%field
    end subroutine

end module data_types_module