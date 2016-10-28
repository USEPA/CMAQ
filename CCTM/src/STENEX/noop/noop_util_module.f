
!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !
!  continuous development by various groups and is based on information  !
!  from these groups: Federal Government employees, contractors working  !
!  within a United States Government contract, and non-Federal sources   !
!  including research institutions.  These groups give the Government    !
!  permission to use, prepare derivative works of, and distribute copies !
!  of their work in the CMAQ system to the public and to permit others   !
!  to do so.  The United States Environmental Protection Agency          !
!  therefore grants similar permission to use the CMAQ system software,  !
!  but users are requested to provide copies of derivative works or      !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software        !
!  that is used with the CMAQ system but distributed under the GNU       !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                              !
!------------------------------------------------------------------------!

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/work/rep/STENEX/src/noop_f90/noop_util_module.f,v 1.5 2002/02/28 15:22:46 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C --------------------------------------------------------------------------
C Purpose:
C
C   use F90 interface feature to achieve "faked" polymorphism for noop data
C   initialization routine and use F90 module feature to collect a set of 
C   noop utility routines which consists of:
C
C       noop_init_array.f
C       noop_hi_lo_bnd_pe.f
C       noop_loop_index.f
C       noop_barrier.f
C       noop_global_min_data.f
C       noop_global_to_local_coord.f
C       noop_subgrid_index.f
C       noop_sum_chk.f
C       noop_my_region.f
C       noop_global_logical.f
C       noop_in_syn 
C
C that are used directly from the application
C
C Revision history:
C
C   Orginal version: 11/05/99 by David Wong
C                    04/24/00 by David Wong
C                      -- include a new module, se_global_logical.f
C -----------------------------------------------------------------------------

        module noop_util_module

        implicit none

        interface noop_init_array
          module procedure noop_init_iarray, noop_init_rarray
        end interface

        interface noop_sum_chk
          module procedure noop_isum_chk, noop_rsum_chk
        end interface

        interface noop_subgrid_index
          module procedure noop_subgrid_index_2,  noop_subgrid_index_3,
     &                     noop_subgrid_index_n2, noop_subgrid_index_n3
        end interface

        contains

C -----------------------------------------------------------------------------
C Purpose:
C
C   to provide a no-op noop_init_iarray.f counter part for serial code
C
C Revision history:
C
C   Orginal version: 5/20/99 by David Wong
C                    11/05/99 by David Wong
C                      -- recode using F90 syntax
C -----------------------------------------------------------------------------

        subroutine noop_init_iarray ( wpe, val )

        implicit none

        integer, intent(out) :: wpe(:)
        integer, intent(in) :: val

        integer i

        do i = 1, size(wpe,1)
           wpe(i) = val
        end do

        end subroutine noop_init_iarray 

C -----------------------------------------------------------------------------
C Purpose:
C
C   to provide a no-op noop_init_rarray.f counter part for serial code
C
C Revision history:
C
C   Orginal version: 10/05/00 by David Wong
C -----------------------------------------------------------------------------

        subroutine noop_init_rarray ( wpe, val )

        implicit none

        real, intent(out) :: wpe(:)
        real, intent(in) :: val

        integer i

        do i = 1, size(wpe,1)
           wpe(i) = val
        end do

        end subroutine noop_init_rarray

C -----------------------------------------------------------------------------
C Purpose:
C
C   to provide a no-op hi_lo_bnd_pe.f counter part for serial code
C
C Revision history:
C
C   Orginal version: 1/19/99 by David Wong
C                    11/05/99 by David Wong
C                      -- recode using F90 syntax
C -----------------------------------------------------------------------------

        subroutine noop_hi_lo_bnd_pe (ori, low_index, high_index)

        implicit none

        character, intent(in) :: ori
        logical, intent(out) :: low_index, high_index

        low_index = .true.
        high_index = .true.

        end subroutine noop_hi_lo_bnd_pe

C -----------------------------------------------------------------------------
C Purpose:
C
C   to provide a no-op loop_index.f counter part for serial code
C
C Revision history:
C
C   Orginal version: 10/06/98 by David Wong
C                    01/19/99 by David Wong
C                    removed the top_pe and bot_pe logical part which is 
C                    computed in a seperate subroutine noop_hi_lo_bnd_pe.f
C                    11/05/99 by David Wong
C                      -- recode using F90 syntax
C -----------------------------------------------------------------------------

        subroutine noop_loop_index (ori, gstart, gend, dif, my_nn, start, end)

        implicit none

        character, intent(in) :: ori
        integer, intent(in) :: gstart, gend, dif
        integer, intent(out) :: my_nn, start, end

        start = gstart
        end = gend + dif

        my_nn = gend

        end subroutine noop_loop_index 

C --------------------------------------------------------------------------
C Purpose:
C
C   to provide a no-op noop_barrier.f counter part for serial code
C
C Revision history:
C
C   Orginal version: 10/6/98 by David Wong
C                    11/05/99 by David Wong
C                      -- recode using F90 syntax
C --------------------------------------------------------------------------

        subroutine noop_barrier

        implicit none

        end subroutine noop_barrier

C -----------------------------------------------------------------------------
C Purpose:
C
C   to provide a no-op noop_global_min_data.f counter part for serial code
C
C Revision history:
C
C   Orginal version: 2/15/98 by David Wong
C                    11/05/99 by David Wong
C                      -- recode using F90 syntax
C -----------------------------------------------------------------------------

        subroutine noop_global_min_data (var, col, row, lvl, meddyv, never,
     &                                   mtrop, ftrop, mth1, mth2, mrib, mws)

        implicit none

        real, intent(in) :: var
        real, intent(inout) :: meddyv, mth1, mth2, mrib, mws
        integer, intent(inout) :: col, row, lvl, mtrop
        logical, intent(inout) :: never, ftrop

        end subroutine noop_global_min_data 

C -----------------------------------------------------------------------------
C Purpose:
C
C   to provide a no-op noop_global_to_local_coord.f counter part for serial code
C
C Revision history:
C
C   Orginal version: 6/18/99 by David Wong
C                    11/05/99 by David Wong
C                      -- recode using F90 syntax
C -----------------------------------------------------------------------------

        subroutine noop_global_to_local_coord (x, y, my_x, my_y, nthick)

        implicit none

        integer, intent(in) :: x, y, nthick
        integer, intent(inout) :: my_x, my_y

        my_x = x
        my_y = y

        end subroutine noop_global_to_local_coord 

C -----------------------------------------------------------------------------

        subroutine noop_subgrid_index_2 (begrow, endrow, begcol, endcol,
     &                                   my_begrow, my_endrow,
     &                                   my_begcol, my_endcol,
     &                                   my_subgrid_rows, my_subgrid_cols,
     &                                   nrows_in, ncols_in)

        implicit none

        integer, intent(in) :: begrow, endrow, begcol, endcol
        integer, intent(out) :: my_begrow, my_endrow, my_begcol, my_endcol
        integer, intent(out) :: my_subgrid_rows, my_subgrid_cols
        integer, optional, intent(in) :: nrows_in, ncols_in

        my_begrow = begrow
        my_endrow = endrow
        my_begcol = begcol
        my_endcol = endcol
        my_subgrid_rows  = endrow - begrow + 1
        my_subgrid_cols  = endcol - begcol + 1

        end subroutine noop_subgrid_index_2 


C Revision history:
C
C   Orginal version: 10/26/00 by David Wong
C                    12/18/00 by Jeff Young
C                    -- remove my_subgrid_levs to make backwardly compatible w/
C                       CCTM code - can't decompose in this dimension now.
C

C -----------------------------------------------------------------------------
        subroutine noop_subgrid_index_3
     &      ( begrow, endrow, begcol, endcol, beglev, endlev,
     &        my_begrow, my_endrow, my_begcol, my_endcol, my_beglev, my_endlev,
!    &        my_subgrid_rows, my_subgrid_cols, my_subgrid_levs,
     &        my_subgrid_rows, my_subgrid_cols,
     &        nrows_in, ncols_in, nlevs_in )

        implicit none

        integer, intent(in) :: begrow, endrow, begcol, endcol, beglev, endlev
        integer, intent(out) :: my_begrow, my_endrow, my_begcol, my_endcol
        integer, intent(out) :: my_beglev, my_endlev
        integer, intent(out) :: my_subgrid_rows, my_subgrid_cols
!    &                          my_subgrid_levs
        integer, optional, intent(in) :: nrows_in, ncols_in, nlevs_in

        my_begrow = begrow
        my_endrow = endrow
        my_begcol = begcol
        my_endcol = endcol
        my_beglev = beglev
        my_endlev = endlev
        my_subgrid_rows  = endrow - begrow + 1
        my_subgrid_cols  = endcol - begcol + 1
!       my_subgrid_levs  = endlev - beglev + 1

        end subroutine noop_subgrid_index_3

C -----------------------------------------------------------------------------
        subroutine noop_subgrid_index_n2 (begrow, endrow, begcol, endcol,
     &                                    my_begrow, my_endrow,
     &                                    my_begcol, my_endcol, flag)

        implicit none

        integer, intent(in) :: begrow, endrow, begcol, endcol
        integer, intent(out) ::  my_begrow, my_endrow, my_begcol, my_endcol
        character, intent(in) :: flag

        my_begrow = begrow
        my_endrow = endrow
        my_begcol = begcol
        my_endcol = endcol

        end subroutine noop_subgrid_index_n2 

C -----------------------------------------------------------------------------
        subroutine noop_subgrid_index_n3 (begrow, endrow, begcol, endcol,
     &                                    beglev, endlev,
     &                                    my_begrow, my_endrow,
     &                                    my_begcol, my_endcol,
     &                                    my_beglev, my_endlev, flag)

        implicit none

        integer, intent(in) :: begrow, endrow, begcol, endcol, beglev, endlev
        integer, intent(out) ::  my_begrow, my_endrow, my_begcol,
     &                           my_endcol, my_beglev, my_endlev
        character, intent(in) :: flag

        my_begrow = begrow
        my_endrow = endrow
        my_begcol = begcol
        my_endcol = endcol
        my_beglev = beglev
        my_endlev = endlev

        end subroutine noop_subgrid_index_n3

C -- Fortran functions

C -----------------------------------------------------------------------------
C Purpose: 
C
C   to provide a no-op noop_isum_ck.f counter part for serial code
C
C Revision history:
C
C   Orginal version: 10/6/98 by David Wong
C                    11/05/99 by David Wong
C                      -- recode using F90 syntax
C --------------------------------------------------------------------------

        function noop_isum_chk (var1, op, var2) result (noop_isum_chk_result)

        implicit none

        logical :: noop_isum_chk_result
        integer, intent(in) :: var1, var2
        character (len = 2), intent(in) :: op

        logical :: lnoop_isum_chk

        lnoop_isum_chk = .false.
        if (op .eq. 'EQ') then
           if (var1 .eq. var2) then
              lnoop_isum_chk = .true.
           end if
        else if (op .eq. 'GT') then
           if (var1 .gt. var2) then
              lnoop_isum_chk = .true.
           end if
        else if (op .eq. 'GE') then
           if (var1 .ge. var2) then
              lnoop_isum_chk = .true.
           end if
        else if (op .eq. 'LT') then
           if (var1 .lt. var2) then
              lnoop_isum_chk = .true.
           end if
        else 
           if (var1 .le. var2) then
              lnoop_isum_chk = .true.
           end if
        end if

        noop_isum_chk_result = lnoop_isum_chk

        end function noop_isum_chk 

C -----------------------------------------------------------------------------
C Purpose: 
C
C   to provide a no-op noop_rsum_ck.f counter part for serial code
C
C Revision history:
C
C   Orginal version: 10/6/98 by David Wong
C                    11/05/99 by David Wong
C                      -- recode using F90 syntax
C --------------------------------------------------------------------------

        function noop_rsum_chk (var1, op, var2) result (noop_rsum_chk_result)

        implicit none

        logical :: noop_rsum_chk_result
        real, intent(in) :: var1, var2
        character (len = 2), intent(in) :: op

        logical :: lnoop_rsum_chk

        lnoop_rsum_chk = .false.
        if (op .eq. 'EQ') then
           if (var1 .eq. var2) then
              lnoop_rsum_chk = .true.
           end if
        else if (op .eq. 'GT') then
           if (var1 .gt. var2) then
              lnoop_rsum_chk = .true.
           end if
        else if (op .eq. 'GE') then
           if (var1 .ge. var2) then
              lnoop_rsum_chk = .true.
           end if
        else if (op .eq. 'LT') then
           if (var1 .lt. var2) then
              lnoop_rsum_chk = .true.
           end if
        else 
           if (var1 .le. var2) then
              lnoop_rsum_chk = .true.
           end if
        end if

        noop_rsum_chk_result = lnoop_rsum_chk

        end function noop_rsum_chk 

C -----------------------------------------------------------------------------
C Purpose: 
C
C   to provide a no-op noop_my_region.f counter part for serial code
C
C Revision history:
C
C   Orginal version: 5/19/99 by David Wong
C                    11/05/99 by David Wong
C                      -- recode using F90 syntax
C -----------------------------------------------------------------------------

        function noop_my_region (x, y, wpe) 
     &           result (noop_my_region_result)

        implicit none

        integer, intent(in) :: x, y
        integer, intent(out) :: wpe
        logical :: noop_my_region_result

        noop_my_region_result = .true.
        wpe = 0

        end function noop_my_region 

C -----------------------------------------------------------------------------
C Purpose:
C
C   to provide a no-op counter part for serial code se_global_logical.f
C
C Revision history:
C
C   Orginal version: 04/24/00 by David Wong
C
C Parameter List:
C
C   InOut: var      -- input variable
C   In   : operator -- operator type
C
C -----------------------------------------------------------------------------

        subroutine noop_global_logical (var, operator)

        implicit none

        logical var
        character*3 operator

        return
        end subroutine noop_global_logical

C -----------------------------------------------------------------------------
C Purpose:
C
C   to provide a no-op counter part for serial code se_in_syn
C
C Revision history:
C
C   Orginal version: 12/04/01 by David Wong
C
C Parameter List:
C
C   InOut: var -- input variable
C   In   : pe  -- PE number
C
C -----------------------------------------------------------------------------

        subroutine noop_in_syn (var, pe)

        implicit none

        logical, intent(inout) :: var
        integer, intent(in) :: pe

        return
        end subroutine noop_in_syn

        end module noop_util_module
