!===============================================================================
! Purpose:  To capture the variation of CGRID in a pre-defined sub domain
!
! Revised:  May 2010  Original version.  David Wong
!           31 Jan 2019  (David Wong)
!              -- adopted the idea to process all twoway related environment
!                 variables in one place
!           01 Aug 2019  (David Wong)
!              -- removed interface block for get_envlist
!===============================================================================

module sd_time_series_module

  implicit none

  integer :: n_sd_spcs, sd_scol, sd_ecol, sd_srow, sd_erow
  character (len = 16), allocatable :: sd_spcs(:)
  integer, allocatable :: sd_spcs_index(:)
  real, allocatable :: sd_ts_data(:,:,:,:)

  contains

! --------------------------------------------------------------------------------
  subroutine sd_time_series_init (in_logdev, tstep)

    use hgrd_defn, only : mype
    use get_env_module

    use utilio_defn
!   include 'PARMS3.EXT'
!   include 'FDESC3.EXT'
!   include 'IODECL3.EXT'
    include SUBST_FILES_ID    ! I/O definitions and declarations

    integer, intent(in) :: in_logdev, tstep

    character (len = 80), allocatable :: temp(:,:)
    integer :: stat, n
!   integer, external :: index1

    character (len = 16), parameter :: pname = 'sd_time_series_i'

    if ( .not. desc3( ctm_conc_1 ) ) then
       write (in_logdev, '(a14, a16, a17)') 'Could not get ', CTM_CONC_1, ' file description'
       stop
    end if

    allocate (sd_spcs(nvars3d), stat=stat)

    call get_envlist ('SD_CONC_SPCS', n_sd_spcs, sd_spcs)

    allocate (sd_spcs_index(n_sd_spcs), temp(n_sd_spcs,3), stat=stat)

    do n = 1, n_sd_spcs
       sd_spcs_index(n) = index1 (sd_spcs(n), nvars3d, vname3d)
    end do

    nvars3d = n_sd_spcs
    ncols3d = sd_ecol - sd_scol + 1
    nrows3d = sd_erow - sd_srow + 1

    do n = 1, n_sd_spcs
       temp(n,1)(1:16) = vname3d(sd_spcs_index(n))
       temp(n,2)(1:16) = units3d(sd_spcs_index(n))
       temp(n,3)       = vdesc3d(sd_spcs_index(n))
    end do
    do n = 1, n_sd_spcs
       vname3d(n) = temp(n,1)(1:16)
       units3d(n) = temp(n,2)(1:16)
       vdesc3d(n) = temp(n,3)
    end do

    xorig3d = xorig3d + (sd_scol - 1) * xcell3d
    yorig3d = yorig3d + (sd_srow - 1) * ycell3d

    tstep3d = tstep

    if (mype .eq. 0) then
       if ( .not. open3 (ctm_sd_ts, FSRDWR3, pname) ) then
          write (in_logdev, '(a30, a16, a11)') ' Warning: Could not open file ', ctm_sd_ts, ' for update'
          if ( .not. open3 (ctm_sd_ts, FSNEW3, pname) ) then
             write (in_logdev, '(a30, a16)') ' Warning: Could not open file ', ctm_sd_ts
          end if
       end if
    end if

    allocate (sd_ts_data(ncols3d, nrows3d, nlays3d, nvars3d), stat=stat)

    deallocate (temp)

  end subroutine sd_time_series_init

! --------------------------------------------------------------------------------
  subroutine sd_ts_data_ext (cgrid, sd_ts_data, send_to, send_index,        &
                             recv_from, recv_index, n_recv, loc_n_sd_spcs,  &  
                             var_index, jtime, mype)

  include 'mpif.h'

  real, pointer, intent(in)  :: cgrid(:,:,:,:)
  real, intent(out) :: sd_ts_data(:,:,:,:)
  integer, intent(in) :: send_to, send_index(:,:), recv_from(:),     &
                         recv_index(:,:,:), n_recv, loc_n_sd_spcs,   &
                         var_index(:), jtime, mype

  real, allocatable, save :: sdata(:,:,:,:)
  integer :: stat, n, data_size, tag, status(MPI_STATUS_SIZE), s_index, e_index

  if (.not. allocated(sdata)) then
     allocate(sdata(send_index(2,1)-send_index(1,1)+1,   &
                    send_index(2,2)-send_index(1,2)+1,   &
                    size(sd_ts_data,3), loc_n_sd_spcs),      &
              stat=stat)
  end if

  if (send_to >= 0) then
     do n = 1, loc_n_sd_spcs
        sdata(:,:,:,n) = cgrid(send_index(1,1):send_index(2,1), send_index(1,2):send_index(2,2),:,var_index(n))
     end do
  end if

  if (mype .eq. 0) then
 
     if (send_to >= 0) then
        s_index = 2
        sd_ts_data(recv_index(1,1,1):recv_index(2,1,1), recv_index(1,2,1):recv_index(2,2,1),:,:) = sdata
     else
        s_index = 1
     end if
     e_index = n_recv

     do n = s_index, e_index
        tag = jtime * 1000 + recv_from(n)
        data_size = (recv_index(2,1,n) - recv_index(1,1,n) + 1) * &
                    (recv_index(2,2,n) - recv_index(1,2,n) + 1) * &
                    size(sd_ts_data,3) * loc_n_sd_spcs


        call mpi_recv(sd_ts_data(recv_index(1,1,n):recv_index(2,1,n),      &
                                 recv_index(1,2,n):recv_index(2,2,n),:,:), &
                      data_size, mpi_real, recv_from(n), tag,              &
                      mpi_comm_world, status, stat)
     end do
  else
     if (send_to >= 0) then
        data_size = size(sdata)
        tag = jtime * 1000 + mype
        call mpi_send (sdata, data_size, mpi_real, send_to, tag, mpi_comm_world, stat)
     end if
  end if

  end subroutine sd_ts_data_ext

! ------------------------------------------------------------------------------
  subroutine output_sd_time_series (cgrid, jdate, jtime)

    use HGRD_DEFN

    use utilio_defn
!   include 'PARMS3.EXT'
!   include 'FDESC3.EXT'
!   include 'IODECL3.EXT'
    include SUBST_FILES_ID    ! I/O definitions and declarations

    real, pointer :: cgrid(:,:,:,:)
    integer, intent(in) :: jdate, jtime

    character (len = 16), parameter :: pname = 'output_sd_time_s'
    integer :: stat, n
    integer, save :: send_to, n_recv, send_index(2,2)
    logical, save :: firstime = .true.
    character (len = 80) :: xmsg
    integer, allocatable, save :: recv_from(:), recv_index(:,:,:)
    logical :: x_intercepted, y_intercepted 

    if (firstime) then
       allocate (recv_from(nprow*npcol), recv_index(2,2,nprow*npcol), stat=stat)

       allocate (sd_ts_data(ncols3d, nrows3d, nlays3d, nvars3d), stat=stat)

       send_to = -1
       recv_from = -1
       n_recv = 0
       do n = 1, NPCOL*NPROW

           x_intercepted = (( ((sd_scol <= colsx_pe(1,n)) .and. (colsx_pe(1,n) <= sd_ecol)) .or.          &
                              ((sd_scol <= colsx_pe(2,n)) .and. (colsx_pe(2,n) <= sd_ecol))      ) .or.   &
                            ( ((colsx_pe(1,n) <= sd_scol) .and. (sd_scol <= colsx_pe(2,n))) .or.          &
                              ((colsx_pe(1,n) <= sd_ecol) .and. (sd_ecol <= colsx_pe(2,n)))      ))
           y_intercepted = (( ((sd_srow <= rowsx_pe(1,n)) .and. (rowsx_pe(1,n) <= sd_erow)) .or.          &
                              ((sd_srow <= rowsx_pe(2,n)) .and. (rowsx_pe(2,n) <= sd_erow))      ) .or.   &
                            ( ((rowsx_pe(1,n) <= sd_srow) .and. (sd_srow <= rowsx_pe(2,n))) .or.          &
                              ((rowsx_pe(1,n) <= sd_erow) .and. (sd_erow <= rowsx_pe(2,n)))      ))

          if (x_intercepted .and. y_intercepted) then

              n_recv = n_recv + 1
              recv_from(n_recv) = n - 1
              if (n .eq. mype + 1) then
                 send_to = 0
                 send_index(1,1) = max(sd_scol, colsx_pe(1,n)) - colsx_pe(1,n) + 1
                 send_index(2,1) = min(sd_ecol, colsx_pe(2,n)) - colsx_pe(1,n) + 1
                 send_index(1,2) = max(sd_srow, rowsx_pe(1,n)) - rowsx_pe(1,n) + 1
                 send_index(2,2) = min(sd_erow, rowsx_pe(2,n)) - rowsx_pe(1,n) + 1
              end if

              recv_index(1,1,n_recv) = max(sd_scol, colsx_pe(1,n)) - sd_scol + 1
              recv_index(2,1,n_recv) = min(sd_ecol, colsx_pe(2,n)) - sd_scol + 1
              recv_index(1,2,n_recv) = max(sd_srow, rowsx_pe(1,n)) - sd_srow + 1
              recv_index(2,2,n_recv) = min(sd_erow, rowsx_pe(2,n)) - sd_srow + 1
          end if
       end do

       firstime = .false.

    end if

    call sd_ts_data_ext (cgrid, sd_ts_data, send_to, send_index,    &
                         recv_from, recv_index, n_recv, n_sd_spcs,  &
                         sd_spcs_index, jtime, mype)

    if (mype .eq. 0) then
       if (.not. write3(ctm_sd_ts, allvar3, jdate, jtime, sd_ts_data)) then
          xmsg = 'Could not write to ' // ctm_sd_ts
          call m3exit(pname, jdate, jtime, xmsg, stat)
       end if
    end if

  end subroutine output_sd_time_series

end module sd_time_series_module
