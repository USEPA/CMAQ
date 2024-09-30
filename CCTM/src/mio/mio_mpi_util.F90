! Purpose: Define a collection of MPI routines that are initiated on the
!          MIO side and make these MPI information available to all MIO
!          routines.

       subroutine mio_setup_rank (mype)

         implicit none

         integer, intent(out) :: mype

#ifdef parallel
         include 'mpif.h'

         integer :: stat

         call mpi_comm_rank (mpi_comm_world, mype, stat)
#else
         mype = 0
#endif

       end subroutine mio_setup_rank

! ----------------------------------------------------------------------
       subroutine mio_set_barrier

         implicit none

#ifdef parallel
         include 'mpif.h'

         integer :: stat

         call mpi_barrier (mpi_comm_world, stat)
#endif

       end subroutine mio_set_barrier
