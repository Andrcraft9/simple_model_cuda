module mpp_module
    ! MPI massively parallel processing library

    !use mpi
    use kind_module, only: wp8 => SHR_KIND_R8, wp4 => SHR_KIND_R4

    implicit none
    save
    public

    include 'mpif.h'
    include "omp_lib.h"

    integer, public :: mpp_rank, mpp_count
    integer, public :: mpp_cart_comm
    integer, dimension(2), public :: mpp_size, mpp_coord
    logical, dimension(2), public :: mpp_period

    integer, public :: mpp_count_threads

    ! Timers for master thread
    real(wp8), public :: mpp_time_model_step

contains

    subroutine mpp_init()
        integer :: ierr, req, provided, tmp
        integer :: rank_cart
        character(len=mpi_max_library_version_string) :: version

        mpp_period = (/.true., .true./)
        mpp_size = (/0,0/)
        !req = MPI_THREAD_MULTIPLE
        req = MPI_THREAD_FUNNELED
        ierr = 0

        !call mpi_init(ierr)
        call mpi_init_thread(req, provided, ierr)

        call mpi_get_library_version(version, tmp, ierr)

        call mpi_comm_rank(mpi_comm_world, mpp_rank, ierr)
        call mpi_comm_size(mpi_comm_world, mpp_count, ierr)
        call mpi_dims_create(mpp_count, 2, mpp_size, ierr)
        call mpi_cart_create(mpi_comm_world, 2, mpp_size, mpp_period, .false., mpp_cart_comm, ierr)
        call mpi_cart_coords(mpp_cart_comm, mpp_rank, 2, mpp_coord, ierr)
        
        call mpi_comm_rank(mpp_cart_comm, rank_cart, ierr)

        !$omp parallel
        mpp_count_threads = omp_get_num_threads()
        !$omp end parallel

        ! MPP INFO
        if (mpp_is_master()) then
            print *, "MPP INFO: Total Processes = ", mpp_count
            print *, "MPP INFO: Total Threads   = ", mpp_count_threads
            print *, "MPP INFO: required and provided thread level for mpi : ", req, provided
            print *, "MPP INFO: MPI byte, MPI real, MPI real8 : ", MPI_BYTE, MPI_REAL, MPI_REAL8
            print *, "MPP INFO: kind 4 and kind 8 : ", wp4, wp8
            print *, "MPP INFO: MPI version: ", trim(version)

            print *, "MPP INFO: Version 2.0, only NO_PARALLEL, BLOCK mode:"
            print *, "MPP INFO: Single OMP PARALLEL, OMP for pack/unpack MPI buffers, nowait loops, one global sync at mpi_waitall"
            print *, "MPP INFO: dont use SIMUL syncs"
            print *, "------------------------------------------------------------"
        endif
        call mpi_barrier(mpp_cart_comm, ierr)

        !$omp parallel
        if (omp_get_thread_num() == 0) then 
            print *, 'rank_world, rank_cart, cord, provided, threads:', mpp_rank, rank_cart, mpp_coord, provided, omp_get_num_threads()
        endif
        !$omp end parallel
        call mpi_barrier(mpp_cart_comm, ierr)

        mpp_time_model_step = 0.0d0
    end subroutine

    subroutine mpp_finalize()
        integer :: ierr
        real(wp8) :: maxtime_model_step, mintime_model_step

        if (mpp_is_master()) then
            write(*,'(a50)') 'Times for master thread:'
        endif

        ! Timers for master thread
        call mpi_allreduce(mpp_time_model_step, maxtime_model_step, 1, mpi_real8, mpi_max, mpp_cart_comm, ierr)
        call mpi_allreduce(mpp_time_model_step, mintime_model_step, 1, mpi_real8, mpi_min, mpp_cart_comm, ierr)
        if (mpp_rank == 0) write(*,'(a50, F12.2, F12.2)') "Time full of model step (max and min): ", maxtime_model_step, mintime_model_step

        call mpi_finalize(ierr)
    end subroutine

    subroutine start_timer(time)
        real(wp8), intent(inout) :: time
        time = mpi_wtime()
        return
    end subroutine

    subroutine end_timer(time)
        real(wp8), intent(inout) :: time
        time = mpi_wtime() - time
        return
    end subroutine

    function mpp_is_master() result(is)
        logical :: is

        if (mpp_rank == 0 .and. omp_get_thread_num() == 0) then
            is = .true.
        else
            is = .false.
        endif

        return
    end function

end module mpp_module
