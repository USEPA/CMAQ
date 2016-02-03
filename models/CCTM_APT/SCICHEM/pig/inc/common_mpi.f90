module common_mpi
!******************************************************************************
!Jan 2007: Add new module for multiprocessor code, -BC
!******************************************************************************

  save

  integer                              :: MPI_PUFFSTRUC
  integer                              :: MPI_WORKSPEC
  integer                              :: MPI_MCDAT
  integer                              :: MPI_REACTSTRUC
  integer                              :: MPI_CSPEC

  integer                              :: ierr, myid, numprocs
  integer                              :: source, dest
  integer,  dimension(:), allocatable  :: nchunk, ipuf_start
  character,dimension(:), allocatable  :: MPI_MCHEAD
  integer                              :: npuf_chunk
  integer                              :: size_int, size_real
  integer                              :: size_char, size_logic
  integer                              :: size_mchead
  integer                              :: size_mcupdate

  real (8)                             :: start_time, end_time

end module common_mpi
  
