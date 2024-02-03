!===============================================================================
! Purpose:  Various utility programs to faciliate the twoway model implementation
! Revised:  11 Apr 2007  Original version.  (David Wong)
!           11 Jan 2016  David wong
!              -- increased the string length of griddesc_fname to 500 in routine
!                 setup_griddesc_file to accommondate long path name
!           31 Jan 2019  (David Wong)
!              -- adopted the idea to process all twoway related environment
!                 variables in one place
!           30 Jun 2021  (David Wong)
!              -- replaced 3i4 with 3i6 in the format statement 16 in subroutine 
!                 setup_griddesc_file to handle ncols or nrow up to 99999
!===============================================================================

  module twoway_util_module

  implicit none

  contains

  SUBROUTINE aq_set_ioapi_header (file_type, ncols, nrows)

    USE twoway_header_data_module

    use utilio_defn
!   INCLUDE 'PARMS3.EXT'
!   INCLUDE 'FDESC3.EXT'

    IMPLICIT NONE

    CHARACTER (LEN = 1), INTENT(IN) :: file_type
    INTEGER,             INTENT(IN) :: ncols, nrows

    ncols3d = ncols
    nrows3d = nrows

    if (file_type .eq. 'C') then
!      ncols3d = ioapi_header%ncols
!      nrows3d = ioapi_header%nrows
       xorig3d = ioapi_header%xorig
       yorig3d = ioapi_header%yorig
    else if (file_type .eq. 'D') then
!      ncols3d = ioapi_header%ncols + 1
!      nrows3d = ioapi_header%nrows + 1
       xorig3d = ioapi_header%xorig - ioapi_header%xcell / 2.0
       yorig3d = ioapi_header%yorig - ioapi_header%ycell / 2.0
    end if

    xcent3d = ioapi_header%xcent
    ycent3d = ioapi_header%ycent

    sdate3d = ioapi_header%sdate
    stime3d = ioapi_header%stime

    gdtyp3d = ioapi_header%gdtyp
    p_alp3d = ioapi_header%p_alp
    p_bet3d = ioapi_header%p_bet
    p_gam3d = ioapi_header%p_gam

    xcell3d = ioapi_header%xcell
    ycell3d = ioapi_header%ycell

    vgtyp3d = ioapi_header%vgtyp

    vgtop3d = ioapi_header%vgtop

    vglvs3d = ioapi_header%vglvs

    gdnam3d = ioapi_header%gdnam

    gdnam3d = ioapi_header%grid_name

    nthik3d = 1
    ftype3d = 1

  END SUBROUTINE aq_set_ioapi_header

!-------------------------------------------------------------------------------
  SUBROUTINE setup_griddesc_file (g_ncols, g_nrows)

    USE twoway_header_data_module
    USE twoway_data_module, only : griddesc_fname, grid_name_str

    integer, intent(in) :: g_ncols, g_nrows

    character (len = 100) :: buffer, myfmt
    character (len = 4)   :: projection_type

    open (unit = 10, file = griddesc_fname, status = 'unknown')
    write (10, 11) "' '"
 11 format (a3)
    if (ioapi_header%gdtyp .eq. 2) then
       projection_type = 'LAM_'
    else if (ioapi_header%gdtyp .eq. 6) then
       projection_type = 'POL_'
    end if

    if (ioapi_header%xcent .lt. 0.0) then
       write (buffer, 12) "'", projection_type, int(ioapi_header%ycent), 'N', int(ioapi_header%xcent*-1), "W'"
    else
       write (buffer, 12) "'", projection_type, int(ioapi_header%ycent), 'N', int(ioapi_header%xcent), "E'"
    end if
 12 format (a1, a4, i3.3, a1, i3.3, a2)
    write (10, 13) buffer
 13 format (a14)
    write (10, 14) ioapi_header%gdtyp, ioapi_header%p_alp, ioapi_header%p_bet, ioapi_header%p_gam, &
                   ioapi_header%xcent, ioapi_header%ycent
 14 format (i2, 5f14.3)
    write (10, 11) "' '"
    write (myfmt, 15) '(a1, a', len(trim(grid_name_str)), ', a1)'
 15 format (a6, i2.2, a5)
    write (10, myfmt) "'", trim(grid_name_str), "'"
    write (10, 16) buffer, ioapi_header%xorig, ioapi_header%yorig, ioapi_header%xcell, ioapi_header%ycell, &
                   g_ncols, g_nrows, ioapi_header%nthik
!   write (10, 16) buffer, ioapi_header%xorig, ioapi_header%yorig, ioapi_header%xcell, ioapi_header%ycell, &
!                  ioapi_header%nrows, ioapi_header%ncols, ioapi_header%nthik
 16 format (a14, 4f14.3, 3i6)
    write (10, 11) "' '"
    close (10)

    ioapi_header%grid_name = grid_name_str

  END SUBROUTINE setup_griddesc_file

! --------------------------------------------------------------------------------
  subroutine compute_decomp (dim, npe1, npe2, domain_type, orientation, domain_map, delta)

    implicit none

    integer, intent(in) :: dim, npe1, npe2
    character (len = 4), intent(in) :: domain_type
    character (len = 1), intent(in) :: orientation
    integer, intent(out) :: domain_map(:,:)
    integer, intent(in), optional :: delta

    integer :: quotient, remainder, stat, i, j, pe, loc_delta
    integer, allocatable :: loc_decomp_map(:,:)
    character (len = 1) :: loc_orientation

    if (present(delta)) then
       loc_delta = delta
    else
       loc_delta = 0
    end if

    quotient = dim / npe1
    remainder = mod(dim, npe1)

    allocate (loc_decomp_map(3, npe1), stat=stat)
    if (stat .ne. 0) then
       print *, ' Error: Allocating loc_decomp_map'
       stop
    end if

    loc_decomp_map(3, :) = quotient

    if (remainder .gt. 0) then
       if (domain_type .eq. 'wrf') then
          do i = 1, (remainder - 1) / 2 + 1
             loc_decomp_map(3, i) = loc_decomp_map(3, i) + 1
          end do
          do i = npe1, npe1 - remainder / 2 + 1, -1
             loc_decomp_map(3, i) = loc_decomp_map(3, i) + 1
          end do
       else
          do i = 1, remainder
             loc_decomp_map(3, i) = loc_decomp_map(3, i) + 1
          end do
       end if
    end if

    do i = 1, npe1
       if (i .eq. 1) then
          loc_decomp_map(1, i) = 1 + loc_delta
       else
          loc_decomp_map(1, i) = loc_decomp_map(2, i-1) + 1
       end if
       loc_decomp_map(2, i) = loc_decomp_map(1, i) + loc_decomp_map(3, i) - 1
    end do

    if (domain_type .eq. 'wrf') then
       loc_orientation = orientation
    else
       if (orientation .eq. 'c') then
          loc_orientation = 'r'
       else
          loc_orientation = 'c'
       end if
    end if

    if (loc_orientation .eq. 'c') then
       pe = 1
       do i = 1, npe1
          do j = 1, npe2
             domain_map(:,pe+j-1) = loc_decomp_map(:,i)
          end do
          pe = pe + npe2
       end do
    else
       pe = 1
       do i = 1, npe1
          do j = 1, npe2
             domain_map(:,pe+(j-1)*npe1) = loc_decomp_map(:,i)
          end do
          pe = pe + 1
       end do
    end if

    deallocate (loc_decomp_map)

  end subroutine compute_decomp

! --------------------------------------------------------------------------------
  subroutine compute_comm_indices (nprocs, source_domain_map, dest_domain_map, &
                                   send_to, recv_from,                         &
                                   send_index_g, send_index_l,                 &
                                   recv_index_g, recv_index_l                   )

    implicit none

    integer, intent(in)  :: nprocs
    integer, intent(in)  :: source_domain_map (3, 2, 0:nprocs-1)
    integer, intent(in)  :: dest_domain_map (3, 2, 0:nprocs-1)
    integer, intent(out) :: send_to (0:,0:), recv_from (0:,0:)
    integer, intent(out) :: send_index_g (:,:,0:), recv_index_g (:,:,0:)
    integer, intent(out) :: send_index_l (:,:,0:), recv_index_l (:,:,0:)

    integer :: i, j, k, wrf_pe, cmaq_pe, x_s, x_e, y_s, y_e
    logical :: x_intercept, y_intercept

    send_to = -1
    recv_from = -1
    send_to(0,:) = 0
    recv_from(0,:) = 0

    send_index_g = -1
    send_index_l = -1
    recv_index_g = -1
    recv_index_l = -1

    do wrf_pe = 0, nprocs-1
       i = 0
       do cmaq_pe = 0, nprocs-1
          x_s = max(source_domain_map(1,1,wrf_pe), dest_domain_map(1,1,cmaq_pe))
          x_e = min(source_domain_map(2,1,wrf_pe), dest_domain_map(2,1,cmaq_pe))
          x_intercept = ( ((dest_domain_map(1,1,cmaq_pe) .le. x_s) .and. (x_s .le. dest_domain_map(2,1,cmaq_pe))) &
                          .and.                                                                                   &
                          ((dest_domain_map(1,1,cmaq_pe) .le. x_e) .and. (x_e .le. dest_domain_map(2,1,cmaq_pe))) )

          y_s = max(source_domain_map(1,2,wrf_pe), dest_domain_map(1,2,cmaq_pe))
          y_e = min(source_domain_map(2,2,wrf_pe), dest_domain_map(2,2,cmaq_pe))
          y_intercept = ( ((dest_domain_map(1,2,cmaq_pe) .le. y_s) .and. (y_s .le. dest_domain_map(2,2,cmaq_pe))) &
                          .and.                                                                                   &
                          ((dest_domain_map(1,2,cmaq_pe) .le. y_e) .and. (y_e .le. dest_domain_map(2,2,cmaq_pe))) )

          if (x_intercept .and. y_intercept) then
             i = i + 1  ! count number of current wrf_pe intercept with cmaq_pe
             send_to(0, wrf_pe) = i
             send_to(i, wrf_pe) = cmaq_pe
             recv_from(0, cmaq_pe) = recv_from(0, cmaq_pe) + 1
             recv_from(recv_from(0, cmaq_pe), cmaq_pe) = wrf_pe
             j = (i - 1) * 3 + 1
             send_index_g(j,   1, wrf_pe) = x_s
             send_index_g(j+1, 1, wrf_pe) = x_e
             send_index_g(j+2, 1, wrf_pe) = x_e - x_s + 1
             send_index_g(j,   2, wrf_pe) = y_s
             send_index_g(j+1, 2, wrf_pe) = y_e
             send_index_g(j+2, 2, wrf_pe) = y_e - y_s + 1

             send_index_l(j,   1, wrf_pe) = x_s - source_domain_map(1, 1, wrf_pe) + 1
             send_index_l(j+1, 1, wrf_pe) = x_e - source_domain_map(1, 1, wrf_pe) + 1
             send_index_l(j+2, 1, wrf_pe) = x_e - x_s + 1
             send_index_l(j,   2, wrf_pe) = y_s - source_domain_map(1, 2, wrf_pe) + 1
             send_index_l(j+1, 2, wrf_pe) = y_e - source_domain_map(1, 2, wrf_pe) + 1
             send_index_l(j+2, 2, wrf_pe ) = y_e - y_s + 1

             k = (recv_from(0, cmaq_pe) - 1) * 3 + 1
             recv_index_g(k,   1, cmaq_pe) = x_s
             recv_index_g(k+1, 1, cmaq_pe) = x_e
             recv_index_g(k+2, 1, cmaq_pe) = x_e - x_s + 1
             recv_index_g(k,   2, cmaq_pe) = y_s
             recv_index_g(k+1, 2, cmaq_pe) = y_e
             recv_index_g(k+2, 2, cmaq_pe) = y_e - y_s + 1

             recv_index_l(k,   1, cmaq_pe) = x_s - dest_domain_map(1,1,cmaq_pe) + 1
             recv_index_l(k+1, 1, cmaq_pe) = x_e - dest_domain_map(1,1,cmaq_pe) + 1
             recv_index_l(k+2, 1, cmaq_pe) = x_e - x_s + 1
             recv_index_l(k,   2, cmaq_pe) = y_s - dest_domain_map(1,2,cmaq_pe) + 1
             recv_index_l(k+1, 2, cmaq_pe) = y_e - dest_domain_map(1,2,cmaq_pe) + 1
             recv_index_l(k+2, 2, cmaq_pe) = y_e - y_s + 1

          end if
       end do
    end do

  end subroutine compute_comm_indices

  end module twoway_util_module
