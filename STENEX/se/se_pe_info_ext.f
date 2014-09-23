
C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/work/rep/STENEX/src/se_snl/se_pe_info_ext.f,v 1.2 2006/02/17 12:55:04 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C --------------------------------------------------------------------------
C Note: all these variables with prefix se_ are for stencil exchange library 
C       only
C
C to define processor info variables:
C
C   se_numprocs       -- number of processors allocated
C   se_npcol          -- number of processors allocated along column dimension
C   se_nprow          -- number of processors allocated along row dimension
C   se_my_pe          -- my logical global processor number
C   se_numworkers     -- number of processors in worker partition
C   se_numiopes       -- number of processors in I/O partition
C   se_myworker_pe    -- my logical worker processor number
C   se_myio_pe        -- my logical I/O processor number
C   se_partition_mode -- processor parition mode: 1 -- no parition
C                                                 2 -- worker + I/O parition
C   se_world_comm     -- MPI global communicator
C   se_worker_comm    -- MPI worker communicator
C   se_io_comm        -- MPI I/O communicator
C --------------------------------------------------------------------------

	module se_pe_info_ext

          integer :: se_numprocs
          integer :: se_npcol
          integer :: se_nprow
          integer :: se_my_pe

          integer :: se_numworkers
          integer :: se_numiopes
          integer :: se_myworker_pe
          integer :: se_myio_pe

          integer :: se_partition_mode

          integer :: se_world_comm
          integer :: se_worker_comm
          integer :: se_io_comm

          integer, allocatable :: ranks(:)

        end module se_pe_info_ext
