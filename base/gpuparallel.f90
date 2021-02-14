module mpp_module
    ! Fake wrapper for MPI, CUDA - only

    use cudafor
    use kind_module, only: wp8 => SHR_KIND_R8, wp4 => SHR_KIND_R4

    implicit none
    save
    public

    include "omp_lib.h"

    integer, parameter:: mpi_integer = 0, mpi_sum = 0

    integer, public :: mpp_rank, mpp_count
    integer, public :: mpp_cart_comm
    integer, dimension(2), public :: mpp_size, mpp_coord
    logical, dimension(2), public :: mpp_period

    integer, public :: mpp_count_threads

    ! Timers for master thread
    real(wp8), public :: mpp_time_model_step

contains

    subroutine mpp_init()

        type (cudaDeviceProp) :: prop
        integer :: nDevices=0, i, ierr

        mpp_period = (/.true., .true./)
        mpp_size = (/1, 1/)
        mpp_coord = (/0, 0/)
        mpp_cart_comm = 0
        mpp_rank = 0
        mpp_count = 1

        !$omp parallel
        mpp_count_threads = omp_get_num_threads()
        !$omp end parallel

        ! MPP INFO
        if (mpp_is_master()) then
            print *, "MPP INFO: VERSION WITHOUT MPI!"
            print *, "MPP INFO: Total Processes = ", mpp_count
            print *, "MPP INFO: Total Threads   = ", mpp_count_threads
            print *, "MPP INFO: kind 4 and kind 8 : ", wp4, wp8
            print *, "------------------------------------------------------------"
        endif

        ! CUDA INFO
        ! Number of CUDA-capable devices
        ierr = cudaGetDeviceCount(nDevices)
        if (nDevices == 0) then
            write(*,"(/,'No CUDA devices found',/)")
            stop
        else if (nDevices == 1) then
            write(*,"(/,'One CUDA device found',/)")
        else 
            write(*,"(/,i0,' CUDA devices found',/)") nDevices
        end if
        ! Loop over devices        
        do i = 0, nDevices-1
            write(*,"('Device Number: ',i0)") i
            ierr = cudaGetDeviceProperties(prop, i)
            if (ierr .eq. 0) then
                write(*,"('  GetDeviceProperties for device ',i0,': Passed')") i
            else
                write(*,"('  GetDeviceProperties for device ',i0,': Failed')") i
            endif
            ! General device info
            write(*,"('  Device Name: ',a)") trim(prop%name)
            write(*,"('  Compute Capability: ',i0,'.',i0)") &
                prop%major, prop%minor
            write(*,"('  Number of Multiprocessors: ',i0)") &
                prop%multiProcessorCount
            write(*,"('  Max Threads per Multiprocessor: ',i0)") &
                prop%maxThreadsPerMultiprocessor
            write(*,"('  Global Memory (GB): ',f9.3,/)") &
                prop%totalGlobalMem/1024.0**3
            ! Execution Configuration
            write(*,"('  Execution Configuration Limits')")
            write(*,"('    Max Grid Dims: ',2(i0,' x '),i0)") &
                prop%maxGridSize
            write(*,"('    Max Block Dims: ',2(i0,' x '),i0)") &
                prop%maxThreadsDim
            write(*,"('    Max Threads per Block: ',i0,/)") &
                prop%maxThreadsPerBlock
        enddo

        mpp_time_model_step = 0.0d0
    end subroutine

    subroutine mpp_finalize()
        integer :: ierr
        real(wp8) :: maxtime_model_step, mintime_model_step

        if (mpp_is_master()) then
            write(*,'(a50)') 'Times for master thread:'
        endif

        ! Timers for master thread
        if (mpp_rank == 0) write(*,'(a50, F12.2, F12.2)') "Time full of model step (max and min): ", mpp_time_model_step, mpp_time_model_step
    end subroutine

    subroutine start_timer(time)
        real(wp8), intent(inout) :: time
        call cpu_time(time)
        return
    end subroutine

    subroutine end_timer(time)
        real(wp8), intent(inout) :: time
        real(wp8) :: time_now
        call cpu_time(time_now)
        time = time_now - time
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

    subroutine mpi_allreduce(loc, tot, count, t, op, comm, ierr)
        integer :: loc, tot, count, t, op, comm, ierr
        tot = loc
    end subroutine

    subroutine mpi_abort(comm, c, ierr)
        integer :: comm, c, ierr
    end subroutine
    
    subroutine mpi_barrier(comm, ierr)
        integer :: comm, ierr
    end subroutine

end module mpp_module
