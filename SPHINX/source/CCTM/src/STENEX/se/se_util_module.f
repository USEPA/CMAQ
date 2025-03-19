
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
C $Header: /project/work/rep/STENEX/src/se_snl/se_util_module.f,v 1.3 2006/02/17 12:54:05 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C --------------------------------------------------------------------------
C Purpose:
C
C   use F90 interface feature to achieve "faked" polymorphism for data
C   initialization routine and use F90 module feature to collect a set of 
C   utility routines which consists of:
C
C       se_init_array
C       se_hi_lo_bnd_pe
C       se_loop_index
C       se_barrier
C       se_global_min_data
C       se_global_to_local_coord
C       se_subgrid_index
C       se_sum_chk
C       se_my_region
C       se_global_logical
C       se_in_syn
C
C that are used directly from the application
C
C Revision history:
C
C   Orginal version: 11/05/99 by David Wong
C                    02/11/00 by David Wong
C                      -- include a new module, se_csg_index.f
C                    04/24/00 by David Wong
C                      -- include a new module, se_global_logical.f
C                    11/05/00 by David Wong
C                      -- use F90 interface construct to distinguish integer or
C                         real array initialization
C                      -- use F90 interface construct to distinguish integer or
C                         real check sum
C                    08/24/11 by David Wong
C                      -- eliminated data orientation
C -----------------------------------------------------------------------------

        module se_util_module

        implicit none

        interface se_init_array
          module procedure se_init_iarray, se_init_rarray
        end interface

        interface se_sum_chk
          module procedure se_isum_chk, se_rsum_chk
        end interface

        interface se_subgrid_index
          module procedure se_subgrid_index_2,  se_subgrid_index_3,
     &                     se_subgrid_index_n2, se_subgrid_index_n3
        end interface

        contains

C -----------------------------------------------------------------------------
C Purpose:
C
C   initialize an array with a specific integer value 
C
C Subroutine parameter description:
C
C   In: wpe -- input array
C       val -- initial value
C
C  Out: wpe
C
C  Local variable: i -- loop index
C
C Revision history:
C
C   Orginal version: 6/18/99 by David Wong
C                    11/05/99 by David Wong
C                      -- recode the code using F90 syntax
C -----------------------------------------------------------------------------

        subroutine se_init_iarray ( wpe, val )

        implicit none

        integer, intent(out) :: wpe(:) 
        integer, intent(in) :: val

        integer i

        do i = 1, size(wpe,1)
           wpe(i) = val
        end do

        return
        end subroutine se_init_iarray

C -----------------------------------------------------------------------------
C Purpose:
C
C   initialize an array with a specific real value
C
C Subroutine parameter description:
C
C   In: wpe -- input array
C       val -- initial value
C
C  Out: wpe
C
C  Local variable: i -- loop index
C
C Revision history:
C
C   Orginal version: 10/05/00 by David Wong
C -----------------------------------------------------------------------------

        subroutine se_init_rarray ( wpe, val )

        implicit none

        real, intent(out) :: wpe(:)
        real, intent(in) :: val

        integer i

        do i = 1, size(wpe,1)
           wpe(i) = val
        end do

        return
        end subroutine se_init_rarray

C -----------------------------------------------------------------------------
C Purpose:
C
C   To determine if a processor contains global boundary cells and if the
C   processor is a low boundary or high boundary processor according to the
C   domain decomposition.
C
C   For example, suppose there are 4 processors with 1 x 4 processor
C    configuration as follows:
C
C     0  1  2  3
C
C   In row orientation, PE 0 is a low boundary processor, and PE 3 is a high 
C   boundary processor.
C
C   In column orientation, PE 0 is both a low and a high boundary processor.
C
C Revision history:
C
C   Orginal version: 1/19/99 by David Wong
C                    11/05/99 by David Wong
C                      -- recode the code using F90 syntax
C                    11/05/00 by David Wong
C                      -- replaced 'R' with 'Y' (Y-axis) and 'C' with 'X' 
C                         (X-axis) to provide a better description of variable 
C                         ori
C                    12/20/00 by Jeff Young
C                      -- add optional 'R' or 'Y' and 'C' or 'X'
C                    08/24/11 by David Wong
C                      -- removed se_ori_ext to eliminate data and geo orientation
C
C Subroutine parameter description:
C
C   In:  ori    -- orientation of 1-D data from the grid domain, row or column
C        
C   Out: low_index  -- logical variable to indicate processor along North or 
C                      West boundary depends on the orientation
C        high_index -- logical variable to indicate processor along South or 
C                      East boundary depends on the orientation
C
C Local variable description:
C
C   top -- position of the North of West direction in the se_nbg_pe array
C   bot -- position of the South of East direction in the se_nbg_pe array
C
C Include files:
C
C   se_comm_info_ext
C -----------------------------------------------------------------------------

        subroutine se_hi_lo_bnd_pe (ori, low_index, high_index)

        use se_comm_info_ext
!       use se_ori_ext

        implicit none

        character, intent(in) :: ori
        logical, intent(out) :: low_index, high_index

        integer :: top, bot

        if (ori .eq. 'Y' .or. ori .eq. 'R') then
           top = 1
           bot = 5
        else
           top = 7
           bot = 3
        end if

        low_index  = .false.
        high_index = .false.

        if (se_ngb_pe(top) .eq. -1) then
           low_index = .true.
        end if

        if (se_ngb_pe(bot) .eq. -1) then
           high_index = .true.
        end if

        return
        end subroutine se_hi_lo_bnd_pe

C -----------------------------------------------------------------------------
C Purpose:
C
C   To map loop indexes INDEX1..INDEX2 in the serial code to the corresponding
C   starting and ending loop indexes in the parallel code, when a 1-D array,
C   extracted for a row or a column of the grid domain is passed into an
C   advection subroutine.  These indexes depend on processor location in the
C   domain decomposition  processor map and whether the processor contains an
C   exterior boundary.
C   The following description illustrates mapping serial loop indexes to
C   parallel code loop indexes:
C
C   serial loop       parallel local
C      index            loop index
C
C        a                   1
C       a+1                  2
C        .                   .
C        .                   .
C        .                   .
C       a+i                 i+1
C  ------------ processor boundary ----------
C      a+i+1                 1
C        .                   .
C        .                   .
C        .                   .
C       a+j                 j-i
C  ------------ processor boundary ----------
C      a+j+1                 1
C        .                   .
C        .                   .
C        .                   .
C      a+n-1               n-j-1
C       a+n                 n-j
C 
C
C   Here is an example: given 16 processors with a 4 x 4 processor map, and a 
C   domain grid size 18 x 25 (row x column), so
C 
C   processor map:  12  13  14  15
C                    8   9  10  11
C                    4   5   6   7
C                    0   1   2   3
C
C   proc #           nrows x ncols      my_nrows x my_ncols
C   -------------------------------------------------------
C   0,4                  5 x 7                 5 x 7 
C   1,2,3,5,6,7          5 x 7                 5 x 6 
C   8,12                 5 x 7                 4 x 7 
C   9,10,11,13,14,15     5 x 7                 4 x 6
C
C   For a row of the 2D domain (in xadv*.F), processors 0, 4, 8, and 12 are
C   BNDY_LO_PE's, processors 3, 7, 11, and 15 are BNDY_HI_PE's, and the rest
C   of the processors are interior processors. If for the serial code, a loop
C   runs from 3 to 22, then the starting and ending indexes are:
C
C   proc #                starting index      ending index
C   ------------------------------------------------------
C   0,4,8,12                    3                  7
C   1,2,5,6,9,10,13,14          1                  6
C   3,7,11,15                   1                  3
C
C   For a column of the 2D domain (in yadv*.F), processors 0, 1, 2, and 3 are
C   BNDY_LO_PE's, processors 12, 13, 14, and 15 are BNDY_HI_PE's, and the rest
C   of the processors are interior processors. If for the serial code, a loop
C   runs from 2 to 17, then the starting and ending indexes are:
C
C   proc #                starting index      ending index
C   ------------------------------------------------------
C   0,1,2,3                     2                  5
C   4,5,6,7                     1                  5
C   8,9,10,11                   1                  4
C   12,13,14,15                 1                  3
C
C
C Revision history:
C
C   Orginal version: 9/14/98 by David Wong
C                  : 1/19/99 by David Wong
C                    took away lo and hi boundary pe calculation, calculate dif
C                    locally
C                    11/05/99 by David Wong
C                      -- recode the code using F90 syntax
C                    12/20/00 by Jeff Young
C                      -- add optional 'R' or 'Y' and 'C' or 'X'
C                    08/24/11 by David Wong
C                      -- removed se_ori_ext to eliminate data and geo orientation
C
C Subroutine parameter description:
C
C   In:  ori    -- orientation of 1-D data from the grid domain, row or column
C        gstart -- original starting point of the loop
C        gend   -- original data dimension
C        dif    -- difference in ending dimension for a loop, such that
C                  GEND + DIF is the orginal ending point
C        
C   Out: my_nn  -- number of data points in the processor
C        start  -- loop starting index of data in the processor
C        end    -- loop ending index of data in the processor
C
C Local variable description:
C
C   top -- position of the North of West direction in the se_nbg_pe array
C   bot -- position of the South of East direction in the se_nbg_pe array
C   dif -- difference in the original loop ending count
C
C Include files:
C
C   se_domain_info_ext
C -----------------------------------------------------------------------------

        subroutine se_loop_index (ori, gstart, gend, dif, my_nn, start, end)

        use se_domain_info_ext
        use se_comm_info_ext
!       use se_ori_ext

        implicit none

        integer, intent(out) :: my_nn, start, end
        character, intent(in) :: ori
        integer, intent(in) :: gstart, gend, dif

        integer :: top, bot

        if (ori .eq. 'Y' .or. ori .eq. 'R') then
           top = 1
           bot = 5
           my_nn = se_my_nrows
        else
           top = 7
           bot = 3
           my_nn = se_my_ncols
        end if

        start = 1
        end = my_nn

        if (se_ngb_pe(top) .eq. -1) then
           start = gstart
        end if

        if (se_ngb_pe(bot) .eq. -1) then
           end = my_nn + dif
        end if

        return
        end subroutine se_loop_index

C --------------------------------------------------------------------------
C Purpose:
C
C   to set a barrier for synchronization purpose (hiding details from user and
C   avoid to include any MPI header files in the original code).
C
C Local variable description:
C
C   error -- return error of calling MPI subroutine
C
C Revision history:
C
C   Orginal version: 9/14/98 by David Wong
C
C                    11/05/99 by David Wong
C                      -- recode the code using F90 syntax
C                    12/04/02 by David Wong
C                      -- modified the routine to accommodate worker and 
C                         I/O processors partition scheme
C                    01/30/17 by David Wong
C                      -- modified the routine to avoid unnecessary creating
C                         and destroying communicator
C --------------------------------------------------------------------------

        subroutine se_barrier (flag)

        use se_pe_info_ext

        implicit none

        character, optional, intent(in) :: flag

        include "mpif.h"

        integer :: local_comm, error

        if (present(flag)) then
           call mpi_comm_dup (se_worker_comm, local_comm, error)
           call mpi_barrier (local_comm, error)
           call mpi_comm_free (local_comm, error)
        else
           call mpi_barrier (mpi_comm_world, error)
        end if

        return
        end subroutine se_barrier

C -----------------------------------------------------------------------------
C Purpose: 
C
C   to determine the global minimum of variable VAR and broadcast data which are
C associated with the minimum VAR
C
C Note: this is a specific subroutine for 11 associated data. If the number of
C       associated data is changed, a new subroutin is needed.
C
C Revision history:
C
C   Orginal version: 2/15/99 by David Wong
C 
C                    11/05/99 by David Wong
C                      -- recode the code using F90 syntax
C 
C                    12/04/02 by David Wong
C                       -- modified the routine to accommodate worker and 
C                          I/O processors partition scheme
C 
C Parameter List:
C
C   In: var -- distributed variable which needs be determined the min value
C              among all processors
C
C  Out: col,
C       row, 
C       lvl, 
C       meddyv, 
C       never,
C       mtrop,
C       ftrop,
C       mth1,
C       mth2,
C       mrib,
C       mws     -- data which are associated with the minimum var
C
C Local Variable:
C
C   num_item -- number of associated variables
C   datain   -- an array holds var and its associated PE number
C   dataout  -- an array holds the minimum of var and its associated PE number
C               in PE 0 only
C   packdata -- an array holds all the associated data
C   min_pe   -- PE number assoicated with minimum var
C   error    -- error code of mpi call
C
C Include Files:
C
C   mpif.h
C   se_pe_info_ext
C   se_domain_info_ext
C -----------------------------------------------------------------------------

        subroutine se_global_min_data (var, col, row, lvl, meddyv, never,
     &                                 mtrop, ftrop, mth1, mth2, mrib, mws)

        use se_pe_info_ext
        use se_domain_info_ext

        implicit none

        real, intent(in) :: var 
        real, intent(inout) :: meddyv, mth1, mth2, mrib, mws
        integer, intent(inout) :: col, row, lvl, mtrop
        logical, intent(inout) :: never, ftrop

        include "mpif.h"

        integer :: num_item = 11

        real :: datain(2), dataout(2) 
        real, allocatable :: packdata(:)
        integer :: min_pe, error, allocate_status

C -- allocate data
        allocate (packdata(num_item), stat=allocate_status)
        if (allocate_status .ne. 0) then
           print *, ' Allocation error in subroutine SE_GLOBAL_MIN_DATA'
           stop
        end if

        datain(1) = var
        datain(2) = float(se_myworker_pe)

        call mpi_reduce (datain, dataout, 1, mpi_2real, MPI_MINLOC, 0, 
     &                   se_worker_comm, error)

        call mpi_bcast (dataout(2), 1, mpi_real, 0, se_worker_comm, error)

        min_pe = dataout(2)

        packdata(1) = col + se_gl_ind(1, 2, min_pe) - 1
        packdata(2) = row + se_gl_ind(1, 1, min_pe) - 1
        packdata(3) = lvl
        packdata(4) = meddyv
        if (never) then
           packdata(5) = 0.0
        else
           packdata(5) = 1.0
        end if
        packdata(6) = float(mtrop)
        if (ftrop) then
           packdata(7) = 0.0
        else
           packdata(7) = 1.0
        end if
        packdata(8) = mth1
        packdata(9) = mth2
        packdata(10) = mrib
        packdata(11) = mws

        call mpi_bcast (packdata, num_item, mpi_real, min_pe, se_worker_comm, 
     &                  error)

        col  = packdata(1)
        row  = packdata(2)
        lvl  = packdata(3)
        meddyv = packdata(4)
        if (packdata(5) .eq. 0.0) then
           never = .true.
        else
           never = .false.
        end if
        mtrop = packdata(6)
        if (packdata(7) .eq. 0.0) then
           ftrop = .true. 
        else
           ftrop = .false. 
        end if
        mth1 = packdata(8) 
        mth2 = packdata(9)
        mrib = packdata(10)
        mws  = packdata(11)
        
        deallocate(packdata)
 
        return
        end subroutine se_global_min_data

C --------------------------------------------------------------------------
C Purpose:
C
C   to convert global grid coordinate to local grid coordinate with respect to
C the thickness of ghost zone. The subroutine will return (-999, -999) if the 
C global grid point does not fall in the sub domain that a PE owns.
C
C Revision history:
C
C   Orginal version: 6/18/99 by David Wong 
C                    11/05/99 by David Wong
C                      -- recode the code using F90 syntax
C                      -- moved the data orientation dependency to higher level
C                    12/04/02 by David Wong
C                       -- modified the routine to accommodate worker and 
C                          I/O processors partition scheme
C                    12/27/05 by David Wong
C                       -- made the routine more robust to avoid IO processor
C                          to access non-existence memory
C
C Subroutine parameter description:
C
C   In:  x     -- global grid x coordiate
C        y     -- global grid y coordiate
C        nthick -- thickness of ghost zone, if nthick = 0, there is no ghost 
C                  zone
C
C   Out: my_x  -- local grid x coordiate
C        my_y  -- local grid y coordiate
C
C Local variable description:
C
C    not_in_my_pe   -- a constant to indicate the grid point is not in the
C                      local processor grid region
C
C    loc_x, loc_y -- output copy the local grid coordination 
C
C Example:
C
C   given 21 x 21 data domain with 4 x 2 PE configuration and "cr" orientation, 
C let global grid point be (8,15), local grid point will be (2, 4) in PE 5 and
C (0, 0) elsewhere. 
C
C Include file:
C
C    se_pe_info_ext
C    se_domain_info_ext
C
C --------------------------------------------------------------------------

        subroutine se_global_to_local_coord (x, y, my_x, my_y, nthick)

        use se_pe_info_ext
        use se_domain_info_ext

        implicit none

        integer, intent(in) :: x, y, nthick
        integer, intent(inout) :: my_x, my_y

        integer, parameter :: not_in_my_pe = -999

        integer :: loc_x, loc_y

        loc_x = not_in_my_pe
        loc_y = not_in_my_pe

        if (se_myworker_pe .ge. 0) then
           if (((se_gl_ind(1,1,se_myworker_pe) - nthick) .le. x) .and. 
     &         (x .le. (se_gl_ind(2,1,se_myworker_pe) + nthick))) then
              loc_x = x - se_gl_ind(1,1,se_myworker_pe) + 1
           end if

           if (((se_gl_ind(1,2,se_myworker_pe) - nthick) .le. y) .and. 
     &         (y .le. (se_gl_ind(2,2,se_myworker_pe) + nthick))) then
              loc_y = y - se_gl_ind(1,2,se_myworker_pe) + 1
           end if

           if (loc_x .eq. not_in_my_pe) then
              loc_y = not_in_my_pe
           else if (loc_y .eq. not_in_my_pe) then
              loc_x = not_in_my_pe
           end if
        end if

        my_x = loc_x
        my_y = loc_y

        return
        end subroutine se_global_to_local_coord

C -- Fortran functions

C -----------------------------------------------------------------------------
C Purpose: determine the logical relationship between var1 and var2 according
C          to the operator, op, i.e. checking var1 op var2 is true or not
C
C   Orginal version: 2/15/99 by David Wong
C
C                    11/05/99 by David Wong
C                      -- recode the code using F90 syntax
C
C                    10/05/00 by David Wong
C                      -- expanded to distinguish integer or real variable
C
C                    12/04/02 by David Wong
C                       -- modified the routine to accommodate worker and 
C                          I/O processors partition scheme
C
C Parameter List:
C
C   In: var1 -- variable one
C       var2 -- variable two
C       op   -- logical operator, EQ, GT, GE, LT, and LE
C
C Local Variable:
C
C   sum          -- local variable for computing global sum
C   lse_isum_chk -- local variable for holding the logical relationship
C   error        -- error code for mpi call
C
C Include Files:
C
C   se_pe_info.ext:
C -----------------------------------------------------------------------------

        function se_isum_chk (var1, op, var2) result (se_isum_chk_result)

        use se_pe_info_ext

        implicit none

        logical :: se_isum_chk_result
        integer, intent(in) :: var1, var2
        character (len = 2), intent(in) :: op

        include "mpif.h"

        integer :: sum, error
        logical :: lse_isum_chk

        call mpi_reduce (var1, sum, 1, mpi_integer, mpi_sum, 0,
     &                   se_worker_comm, error)

        if (se_myworker_pe .eq. 0) then
           lse_isum_chk = .false.
           if (op .eq. 'EQ') then
              if (sum .eq. var2) then
                 lse_isum_chk = .true.
              end if
           else if (op .eq. 'GT') then
              if (sum .gt. var2) then
                 lse_isum_chk = .true.
              end if
           else if (op .eq. 'GE') then
              if (sum .ge. var2) then
                 lse_isum_chk = .true.
              end if
           else if (op .eq. 'LT') then
              if (sum .lt. var2) then
                 lse_isum_chk = .true.
              end if
           else 
              if (sum .le. var2) then
                 lse_isum_chk = .true.
              end if
           end if
           se_isum_chk_result = lse_isum_chk
        end if

        call mpi_bcast (se_isum_chk_result, 1, mpi_logical, 0, 
     &                  se_worker_comm, error)

        return
        end function se_isum_chk

C -----------------------------------------------------------------------------
C Purpose: determine the logical relationship between var1 and var2 according
C          to the operator, op, i.e. checking var1 op var2 is true or not
C
C   Orginal version: 2/15/99 by David Wong
C
C                    11/05/99 by David Wong
C                      -- recode the code using F90 syntax
C
C                    10/05/00 by David Wong
C                      -- expanded to distinguish integer or real variable
C
C                    12/04/02 by David Wong
C                       -- modified the routine to accommodate worker and 
C                          I/O processors partition scheme
C
C                    05/19/03 by David Wong
C                       -- fixed a bug which declared sum in the wrong type
C
C Parameter List:
C
C   In: var1 -- variable one
C       var2 -- variable two
C       op   -- logical operator, EQ, GT, GE, LT, and LE
C
C Local Variable:
C
C   sum          -- local variable for computing global sum
C   lse_rsum_chk -- local variable for holding the logical relationship
C   error        -- error code for mpi call
C
C Include Files:
C
C   se_pe_info.ext:
C -----------------------------------------------------------------------------

        function se_rsum_chk (var1, op, var2) result (se_rsum_chk_result)

        use se_pe_info_ext

        implicit none

        logical :: se_rsum_chk_result
        real, intent(in) :: var1, var2
        character (len = 2), intent(in) :: op

        include "mpif.h"

        real    :: sum
        integer :: error
        logical :: lse_rsum_chk

        call mpi_reduce (var1, sum, 1, mpi_real, mpi_sum, 0,
     &                   se_worker_comm, error)

        if (se_myworker_pe .eq. 0) then
           lse_rsum_chk = .false.
           if (op .eq. 'EQ') then
              if (sum .eq. var2) then
                 lse_rsum_chk = .true.
              end if
           else if (op .eq. 'GT') then
              if (sum .gt. var2) then
                 lse_rsum_chk = .true.
              end if
           else if (op .eq. 'GE') then
              if (sum .ge. var2) then
                 lse_rsum_chk = .true.
              end if
           else if (op .eq. 'LT') then
              if (sum .lt. var2) then
                 lse_rsum_chk = .true.
              end if
           else
              if (sum .le. var2) then
                 lse_rsum_chk = .true.
              end if
           end if
           se_rsum_chk_result = lse_rsum_chk
        end if

        call mpi_bcast (se_rsum_chk_result, 1, mpi_logical, 0,
     &                  se_worker_comm, error)

        return
        end function se_rsum_chk

C -----------------------------------------------------------------------------
C Purpose: 
C
C   to determine where a global grid point is reside in local processor when
C the global domain is decomposed
C
C  Revision history:
C
C   Orginal version: 6/18/99 by David Wong
C                    11/05/99 by David Wong
C                      -- recode the code using F90 syntax
C                      -- moved the data orientation dependency to higher level
C                    12/03/01 by David Wong
C                      -- modified the algorithm to set variable WPE negative
C                         if coordinate (x, y) is not in the domain
C                    12/04/02 by David Wong
C                       -- modified the routine to accommodate worker and 
C                          I/O processors partition scheme
C                    12/27/05 by David Wong
C                       -- made the routine more robust to avoid IO processor
C                          to access non-existence memory
C
C Parameter List:
C
C   In: x -- x coordinate of a global grid point
C       y -- y coordinate of a global grid point
C
C  Out: wpe -- PE number
C
C Local Variable:
C
C   lwpe   -- local PE number
C
C -----------------------------------------------------------------------------

        function se_my_region (x, y, wpe) result (se_my_region_result)

        use se_pe_info_ext
        use se_domain_info_ext

        implicit none

        include "mpif.h"

        integer, intent(in) :: x, y
        integer, intent(out) :: wpe
        logical :: se_my_region_result

        integer :: lwpe, error

        se_my_region_result = .false.
        lwpe = 0

        if (se_myworker_pe .ge. 0) then
           if ((se_gl_ind(1,1,se_myworker_pe) .le. x) .and.
     &         (x .le. se_gl_ind(2,1,se_myworker_pe)) .and.
     &         (se_gl_ind(1,2,se_myworker_pe) .le. y) .and.
     &         (y .le. se_gl_ind(2,2,se_myworker_pe))) then
              se_my_region_result = .true.
              lwpe = se_myworker_pe + 1
           end if
        end if

        call mpi_reduce (lwpe, wpe, 1, mpi_integer, mpi_sum, 0,
     &                   se_worker_comm, error)

        if (wpe .eq. 0) then
           wpe = -1
        else
           wpe = wpe - 1
        end if

        call mpi_bcast (wpe, 1, mpi_integer, 0, se_worker_comm, error)

        return
        end function se_my_region

C -----------------------------------------------------------------------------
C Purpose:
C
C   determine the global logical operation which can be AND, OR
C
C Revision history:
C
C   Orginal version: 04/24/00 by David Wong
C
C                    12/04/02 by David Wong
C                       -- modified the routine to accommodate worker and 
C                          I/O processors partition scheme
C
C Parameter List:
C
C   InOut: var      -- distributed variable which needs be determined the
C                      logical value among all processors
C   In   : operator -- operator type
C
C Local Variable:
C
C   temp_var -- local temporary copy of var
C   error    -- error code of mpi call
C
C Include Files:
C
C   mpif.h
C -----------------------------------------------------------------------------

        subroutine se_global_logical (var, op)

        use se_pe_info_ext

        implicit none

        logical var
        character*3 op

        include "mpif.h"

        logical temp_var
        integer error

        temp_var = var

        if ((op .eq. 'AND') .or. (op .eq. 'and')) then
           call mpi_reduce (var, temp_var, 1, mpi_logical, MPI_LAND, 0,
     &                      se_worker_comm, error)
        else if ((op .eq. 'OR') .or. (op .eq. 'or')) then
           call mpi_reduce (var, temp_var, 1, mpi_logical, MPI_LOR, 0,
     &                      se_worker_comm, error)
        end if

        call mpi_bcast (temp_var, 1, mpi_logical, 0, se_worker_comm, error)

        var = temp_var

        return
        end subroutine se_global_logical

C -----------------------------------------------------------------------------
C Purpose:
C
C   given starting point and ending point in terms of column and row number in
C original grid domain, determine a sub-grid mapping with re-distribution of
C data for a 2-D data structure. The routine provides a flexibility that the
C sub-grid has a different resolution indicates with optional variables: 
C nrows_in and ncols_in.
C
C Revision history:
C
C   Orginal version: 10/26/00 by David Wong
C
C                    1/23/01  by David Wong
C                      -- deallocate se_lgl_ind
C
C                    12/04/02 by David Wong
C                       -- modified the routine to accommodate worker and 
C                          I/O processors partition scheme
C
C                    12/27/05 by David Wong
C                       -- made the routine more robust to avoid IO processor
C                          to access non-existence memory
C
C                    08/24/11 by David Wong
C                      -- removed se_ori_ext to eliminate data and geo orientation
C
C Parameter List:
C
C   In  : begrow -- starting row number in the original grid
C         endrow -- ending row number in the original grid
C         begcol -- starting column number in the original grid
C         endcol -- ending column number in the original grid
C
C   Out : my_begrow -- starting row number of the sub-grid in a processor
C         my_endrow -- end row number of the sub-grid in a processor
C         my_begcol -- starting column number of the sub-grid in a processor
C         my_endcol -- end column number of the sub-grid in a processor
C         my_subgrid_rows -- number of sub-grid rows in a processor
C         my_subgrid_cols -- number of sub-grid columns in a processor
C
C  In (optional) : nrows_in -- number of rows in the sub-grid
C                  ncols_in -- number of columns in the sub-grid
C
C Local Variable:
C
C   intersect       -- intersection indicator
C   se_lgl_ind      -- local global index
C   se_lgl_ind_ptr  -- local global index pointer
C   i               -- loop index
C   allocate_status -- memory allocation error indicator
C
C -----------------------------------------------------------------------------

        subroutine se_subgrid_index_2 (begrow, endrow, begcol, endcol, 
     &                                 my_begrow, my_endrow, 
     &                                 my_begcol, my_endcol,
     &                                 my_subgrid_rows, my_subgrid_cols,
     &                                 nrows_in, ncols_in)

        use se_pe_info_ext
        use se_domain_info_ext
        use se_subgrid_info_ext
!       use se_ori_ext
        use se_internal_util_module

        implicit none

        integer, intent(in) :: begrow, endrow, begcol, endcol
        integer, intent(out) :: my_begrow, my_endrow, my_begcol, my_endcol
        integer, intent(out) :: my_subgrid_rows, my_subgrid_cols
        integer, optional, intent(in) :: nrows_in, ncols_in

        integer :: i, allocate_status

        integer, allocatable, target :: se_lgl_ind(:,:,:)
        integer, pointer :: se_lgl_ind_ptr(:,:,:)

        logical :: intersect

C -- allocate data
        allocate (se_subgrid_send_ind(2, 4, 0:se_numworkers-1), 
     &            stat=allocate_status)
        if (allocate_status .ne. 0) then
           print *, ' Subroutine SE_SUBGD_INDEX: ',
     &              ' allocation erorr in se_subgrid_send_ind'
           stop
        end if

        allocate (se_subgrid_recv_ind(2, 4, 0:se_numworkers-1), 
     &            stat=allocate_status)
        if (allocate_status .ne. 0) then
           print *, ' Subroutine SE_SUBGD_INDEX: ',
     &              ' allocation erorr in se_subgrid_recv_ind'
           stop
        end if

        allocate (se_subgrid_send(0:se_numworkers-1), stat=allocate_status)
        if (allocate_status .ne. 0) then
           print *, ' Subroutine SE_SUBGD_INDEX: ',
     &              'allocation erorr in se_subgrid_send'
           stop
        end if

        allocate (se_subgrid_recv(0:se_numworkers-1), stat=allocate_status)
        if (allocate_status .ne. 0) then
           print *, ' Subroutine SE_SUBGD_INDEX: ',
     &              'allocation erorr in se_subgrid_recv'
           stop
        end if

        se_subgrid_send_ind_ptr => se_subgrid_send_ind
        se_subgrid_recv_ind_ptr => se_subgrid_recv_ind
        se_subgrid_send_ptr => se_subgrid_send
        se_subgrid_recv_ptr => se_subgrid_recv

C -- allocate data
        allocate (se_subgrid_ind(2, 2, 0:se_numworkers-1), stat=allocate_status)
        if (allocate_status .ne. 0) then
           print *, ' Allocation error in subroutine SE_SUBGD_INDEX'
           stop
        end if

        se_subgrid_ind_ptr => se_subgrid_ind

        allocate (se_lgl_ind(2, 2, 0:se_numworkers-1), stat=allocate_status)
        if (allocate_status .ne. 0) then
           print *, ' Allocation error in subroutine SE_SUBGD_INDEX'
           stop
        end if
        se_lgl_ind_ptr => se_lgl_ind

        if (present(nrows_in)) then
!          if ((se_data_ori .eq. "cr") .or. (se_data_ori .eq. "CR")) then
              call se_generate_map (1, ncols_in, 1, nrows_in, 
     &                              se_nprow, se_npcol, se_lgl_ind_ptr)
!          else
!             call se_generate_map (1, nrows_in, 1, ncols_in, 
!    &                              se_nprow, se_npcol, se_lgl_ind_ptr)
!          end if

C -- figure out the low and high column and row index of the
C    processor analysis (PA) grid, respectively

!          if ((se_data_ori .eq. "cr") .or. (se_data_ori .eq. "CR")) then
              call se_generate_map (begcol, endcol, begrow, endrow,
     &                              se_nprow, se_npcol, se_subgrid_ind_ptr, 0)
!          else
!             call se_generate_map (begrow, endrow, begcol, endcol,
!    &                              se_nprow, se_npcol, se_subgrid_ind_ptr, 0)
!          end if

        else

           se_lgl_ind = se_gl_ind

C -- figure out the low and high column and row index of the
C    fine sub grid, respectively

!          if ((se_data_ori .eq. "cr") .or. (se_data_ori .eq. "CR")) then
              call se_generate_map (begcol, endcol, begrow, endrow,
     &                              se_nprow, se_npcol, se_subgrid_ind_ptr)
!          else
!             call se_generate_map (begrow, endrow, begcol, endcol,
!    &                              se_nprow, se_npcol, se_subgrid_ind_ptr)
!          end if
        end if

        if (se_myworker_pe .ge. 0) then
           do i = 0, se_numworkers-1
              intersect = .true.
              if (     (se_subgrid_ind(1,1,i) .gt. se_subgrid_ind(2,1,i))
     &            .or. (se_subgrid_ind(1,2,i) .gt. se_subgrid_ind(2,2,i))
     &            .or. (se_lgl_ind(1,1,se_myworker_pe) .gt. se_subgrid_ind(2,1,i))
     &            .or. (se_lgl_ind(2,1,se_myworker_pe) .lt. se_subgrid_ind(1,1,i))) then
                 intersect = .false.
              else
                 if (     (se_lgl_ind(1,2,se_myworker_pe) .gt. se_subgrid_ind(2,2,i))
     &               .or. (se_lgl_ind(2,2,se_myworker_pe) .lt. se_subgrid_ind(1,2,i))) 
     &                    then
                    intersect = .false.
                 end if
              end if

              if (intersect) then
                 se_subgrid_send_ind(1,1,i) = max(se_lgl_ind(1,1,se_myworker_pe),
     &                                        se_subgrid_ind(1,1,i))
     &                                   - se_lgl_ind(1,1,se_myworker_pe) + 1
                 se_subgrid_send_ind(2,1,i) = min(se_lgl_ind(2,1,se_myworker_pe),
     &                                        se_subgrid_ind(2,1,i))
     &                                   - se_lgl_ind(1,1,se_myworker_pe) + 1
                 se_subgrid_send_ind(1,2,i) = max(se_lgl_ind(1,2,se_myworker_pe),
     &                                        se_subgrid_ind(1,2,i))
     &                                   - se_lgl_ind(1,2,se_myworker_pe) + 1
                 se_subgrid_send_ind(2,2,i) = min(se_lgl_ind(2,2,se_myworker_pe),
     &                                        se_subgrid_ind(2,2,i))
     &                                   - se_lgl_ind(1,2,se_myworker_pe) + 1
                 se_subgrid_send(i) = i
              else
                 se_subgrid_send(i) = -1
              end if
           end do

C -- determine data is going to receive from which PE, and corresponding local
C    index

           do i = 0, se_numworkers-1
              intersect = .true.
              if (     (se_subgrid_ind(1,1,se_myworker_pe) .gt. 
     &                     se_subgrid_ind(2,1,se_myworker_pe))
     &            .or. (se_subgrid_ind(1,2,se_myworker_pe) .gt. 
     &                     se_subgrid_ind(2,2,se_myworker_pe))
     &            .or. (se_lgl_ind(1,1,i) .gt. se_subgrid_ind(2,1,se_myworker_pe))
     &            .or. (se_lgl_ind(2,1,i) .lt. se_subgrid_ind(1,1,se_myworker_pe))) then
                 intersect = .false.
              else
                 if (     (se_lgl_ind(1,2,i) .gt. se_subgrid_ind(2,2,se_myworker_pe))
     &               .or. (se_lgl_ind(2,2,i) .lt. se_subgrid_ind(1,2,se_myworker_pe))) 
     &                    then
                    intersect = .false.
                 end if
              end if

              if (intersect) then
                 se_subgrid_recv_ind(1,1,i) = max(se_lgl_ind(1,1,i),
     &                                        se_subgrid_ind(1,1,se_myworker_pe))
     &                                      - se_subgrid_ind(1,1,se_myworker_pe) + 1
                    se_subgrid_recv_ind(2,1,i) = min(se_lgl_ind(2,1,i),
     &                                        se_subgrid_ind(2,1,se_myworker_pe))
     &                                   - se_subgrid_ind(1,1,se_myworker_pe) + 1
                 se_subgrid_recv_ind(1,2,i) = max(se_lgl_ind(1,2,i),
     &                                        se_subgrid_ind(1,2,se_myworker_pe))
     &                                   - se_subgrid_ind(1,2,se_myworker_pe) + 1
                 se_subgrid_recv_ind(2,2,i) = min(se_lgl_ind(2,2,i),
     &                                        se_subgrid_ind(2,2,se_myworker_pe))
     &                                   - se_subgrid_ind(1,2,se_myworker_pe) + 1
                 se_subgrid_recv(i) = i
              else
                 se_subgrid_recv(i) = -1
              end if
           end do

C -- determine begining and ending row and column indexes, respectively

           my_begrow = 1
           my_begcol = 1
           my_endrow = se_subgrid_ind(2,1,se_myworker_pe) - 
     &                 se_subgrid_ind(1,1,se_myworker_pe) + 1
           my_endcol = se_subgrid_ind(2,2,se_myworker_pe) - 
     &                 se_subgrid_ind(1,2,se_myworker_pe) + 1

C -- if row or column is not included in a processor, set column or row
C    outside the range, respectively

           if (my_endrow .eq. 0) then
              my_endcol = 0
           else if (my_endcol .eq. 0) then
              my_endrow = 0
           end if

           my_subgrid_rows = my_endrow - my_begrow + 1
           my_subgrid_cols = my_endcol - my_begcol + 1
        else
           my_begrow = -1
           my_begcol = -1
           my_endcol = -1
           my_endrow = -1
           my_subgrid_rows = -1
           my_subgrid_cols = -1
        end if

        deallocate (se_lgl_ind)

        return
        end subroutine se_subgrid_index_2

C -----------------------------------------------------------------------------
C Purpose:
C
C   given starting point and ending point in terms of column, row, and level 
C number in original grid domain, determine a sub-grid mapping with 
C re-distribution of data for a 3-D data structure. The routine provides a 
C flexibility that the sub-grid has a different resolution indicates with 
C optional variables: nrows_in, ncols_in, and nlevs_in.
C
C Revision history:
C
C   Orginal version: 10/26/00 by David Wong
C                    12/18/00 by Jeff Young
C                    -- remove my_subgrid_levs to make backwardly compatible w/
C                       CCTM code - can't decompose in this dimension now.
C
C                    1/23/01  by David Wong
C                      -- deallocate se_lgl_ind
C
C                    12/04/02 by David Wong
C                       -- modified the routine to accommodate worker and 
C                          I/O processors partition scheme
C
C                    12/27/05 by David Wong
C                       -- made the routine more robust to avoid IO processor
C                          to access non-existence memory
C
C                    08/24/11 by David Wong
C                      -- removed se_ori_ext to eliminate data and geo orientation
C
C Parameter List:
C
C   In  : begrow -- starting row number in the original grid
C         endrow -- ending row number in the original grid
C         begcol -- starting column number in the original grid
C         endcol -- ending column number in the original grid
C         beglev -- starting level number in the original grid
C         endlev -- ending level number in the original grid
C
C  In (optional) : nrows_in -- number of rows in the sub-grid
C                  ncols_in -- number of columns in the sub-grid
C                  nlevs_in -- number of levels in the sub-grid
C
C   Out : my_begrow -- starting row number of the sub-grid in a processor
C         my_endrow -- end row number of the sub-grid in a processor
C         my_begcol -- starting column number of the sub-grid in a processor
C         my_endcol -- end column number of the sub-grid in a processor
C         my_beglev -- starting level number of the sub-grid in a processor
C         my_endlev -- end level number of the sub-grid in a processor
C         my_subgrid_rows -- number of sub-grid rows in a processor
C         my_subgrid_cols -- number of sub-grid columns in a processor
C
C         my_subgrid_levs -- number of sub-grid levels in a processor
C         ^^^^^^^^^^^^^^^  eliminated for now, maybe implement later
C
C Local Variable:
C
C   intersect       -- intersection indicator
C   se_lgl_ind      -- local global index
C   se_lgl_ind_ptr  -- local global index pointer
C   i               -- loop index
C   allocate_status -- memory allocation error indicator
C
C -----------------------------------------------------------------------------

        subroutine se_subgrid_index_3
     &      ( begrow, endrow, begcol, endcol, beglev, endlev,
     &        my_begrow, my_endrow, my_begcol, my_endcol, my_beglev, my_endlev,
!    &        my_subgrid_rows, my_subgrid_cols, my_subgrid_levs, 
     &        my_subgrid_rows, my_subgrid_cols, 
     &        nrows_in, ncols_in, nlevs_in )

        use se_pe_info_ext
        use se_domain_info_ext
        use se_subgrid_info_ext
!       use se_ori_ext
        use se_internal_util_module

        implicit none

        integer, intent(in) :: begrow, endrow, begcol, endcol, beglev, endlev
        integer, intent(out) :: my_begrow, my_endrow, my_begcol, my_endcol
        integer, intent(out) :: my_beglev, my_endlev
        integer, intent(out) :: my_subgrid_rows, my_subgrid_cols
!    &                          my_subgrid_levs
        integer, optional, intent(in) :: nrows_in, ncols_in, nlevs_in

        integer :: i, allocate_status

        integer, allocatable, target :: se_lgl_ind(:,:,:)
        integer, pointer :: se_lgl_ind_ptr(:,:,:)

        logical :: intersect

C -- allocate data
        allocate (se_subgrid_send_ind(2, 4, 0:se_numworkers-1), 
     &            stat=allocate_status)
        if (allocate_status .ne. 0) then
           print *, ' Subroutine SE_SUBGD_INDEX: ',
     &              ' allocation erorr in se_subgrid_send_ind'
           stop
        end if

        allocate (se_subgrid_recv_ind(2, 4, 0:se_numworkers-1), 
     &            stat=allocate_status)
        if (allocate_status .ne. 0) then
           print *, ' Subroutine SE_SUBGD_INDEX: ',
     &              ' allocation erorr in se_subgrid_recv_ind'
           stop
        end if

        allocate (se_subgrid_send(0:se_numworkers-1), stat=allocate_status)
        if (allocate_status .ne. 0) then
           print *, ' Subroutine SE_SUBGD_INDEX: ',
     &              'allocation erorr in se_subgrid_send'
           stop
        end if

        allocate (se_subgrid_recv(0:se_numworkers-1), stat=allocate_status)
        if (allocate_status .ne. 0) then
           print *, ' Subroutine SE_SUBGD_INDEX: ',
     &              'allocation erorr in se_subgrid_recv'
           stop
        end if

        se_subgrid_send_ind_ptr => se_subgrid_send_ind
        se_subgrid_recv_ind_ptr => se_subgrid_recv_ind
        se_subgrid_send_ptr => se_subgrid_send
        se_subgrid_recv_ptr => se_subgrid_recv

C -- allocate data
        allocate (se_subgrid_ind(2, 2, 0:se_numworkers-1), stat=allocate_status)
        if (allocate_status .ne. 0) then
           print *, ' Allocation error in subroutine SE_SUBGD_INDEX'
           stop
        end if

        se_subgrid_ind_ptr => se_subgrid_ind

        allocate (se_lgl_ind(2, 2, 0:se_numworkers-1), stat=allocate_status)
        if (allocate_status .ne. 0) then
           print *, ' Allocation error in subroutine SE_SUBGD_INDEX'
           stop
        end if
        se_lgl_ind_ptr => se_lgl_ind

        if (present(nrows_in)) then
!          if ((se_data_ori .eq. "cr") .or. (se_data_ori .eq. "CR")) then
              call se_generate_map (1, ncols_in, 1, nrows_in, 
     &                              se_nprow, se_npcol, se_lgl_ind_ptr)
!          else
!             call se_generate_map (1, nrows_in, 1, ncols_in, 
!    &                              se_nprow, se_npcol, se_lgl_ind_ptr)
!          end if

C -- figure out the low and high column and row index of the
C    processor analysis (PA) grid, respectively

!          if ((se_data_ori .eq. "cr") .or. (se_data_ori .eq. "CR")) then
              call se_generate_map (begcol, endcol, begrow, endrow,
     &                              se_nprow, se_npcol, se_subgrid_ind_ptr, 0)
!          else
!             call se_generate_map (begrow, endrow, begcol, endcol,
!    &                              se_nprow, se_npcol, se_subgrid_ind_ptr, 0)
!          end if

        else

           se_lgl_ind = se_gl_ind

C -- figure out the low and high column and row index of the
C    fine sub grid, respectively

!          if ((se_data_ori .eq. "cr") .or. (se_data_ori .eq. "CR")) then
              call se_generate_map (begcol, endcol, begrow, endrow,
     &                              se_nprow, se_npcol, se_subgrid_ind_ptr)
!          else
!             call se_generate_map (begrow, endrow, begcol, endcol,
!    &                              se_nprow, se_npcol, se_subgrid_ind_ptr)
!          end if
        end if

        if (se_myworker_pe .ge. 0) then
           do i = 0, se_numworkers-1
              intersect = .true.
              if (     (se_subgrid_ind(1,1,i) .gt. se_subgrid_ind(2,1,i))
     &            .or. (se_subgrid_ind(1,2,i) .gt. se_subgrid_ind(2,2,i))
     &            .or. (se_lgl_ind(1,1,se_myworker_pe) .gt. se_subgrid_ind(2,1,i))
     &            .or. (se_lgl_ind(2,1,se_myworker_pe) .lt. se_subgrid_ind(1,1,i))) then
                 intersect = .false.
              else
                 if (     (se_lgl_ind(1,2,se_myworker_pe) .gt. se_subgrid_ind(2,2,i))
     &               .or. (se_lgl_ind(2,2,se_myworker_pe) .lt. se_subgrid_ind(1,2,i))) 
     &                    then
                    intersect = .false.
                 end if
              end if

              if (intersect) then
                 se_subgrid_send_ind(1,1,i) = max(se_lgl_ind(1,1,se_myworker_pe),
     &                                        se_subgrid_ind(1,1,i))
     &                                   - se_lgl_ind(1,1,se_myworker_pe) + 1
                 se_subgrid_send_ind(2,1,i) = min(se_lgl_ind(2,1,se_myworker_pe),
     &                                        se_subgrid_ind(2,1,i))
     &                                      - se_lgl_ind(1,1,se_myworker_pe) + 1
                 se_subgrid_send_ind(1,2,i) = max(se_lgl_ind(1,2,se_myworker_pe),
     &                                        se_subgrid_ind(1,2,i))
     &                                   - se_lgl_ind(1,2,se_myworker_pe) + 1
                 se_subgrid_send_ind(2,2,i) = min(se_lgl_ind(2,2,se_myworker_pe),
     &                                        se_subgrid_ind(2,2,i))
     &                                   - se_lgl_ind(1,2,se_myworker_pe) + 1
                 se_subgrid_send(i) = i
              else
                 se_subgrid_send(i) = -1
              end if
           end do

C -- determine data is going to receive from which PE, and corresponding local
C    index

           do i = 0, se_numworkers-1
              intersect = .true.
              if (     (se_subgrid_ind(1,1,se_myworker_pe) .gt. 
     &                     se_subgrid_ind(2,1,se_myworker_pe))
     &            .or. (se_subgrid_ind(1,2,se_myworker_pe) .gt. 
     &                     se_subgrid_ind(2,2,se_myworker_pe))
     &            .or. (se_lgl_ind(1,1,i) .gt. se_subgrid_ind(2,1,se_myworker_pe))
     &            .or. (se_lgl_ind(2,1,i) .lt. se_subgrid_ind(1,1,se_myworker_pe))) then
                 intersect = .false.
              else
                 if (     (se_lgl_ind(1,2,i) .gt. se_subgrid_ind(2,2,se_myworker_pe))
     &               .or. (se_lgl_ind(2,2,i) .lt. se_subgrid_ind(1,2,se_myworker_pe))) 
     &                    then
                    intersect = .false.
                 end if
              end if

              if (intersect) then
                 se_subgrid_recv_ind(1,1,i) = max(se_lgl_ind(1,1,i),
     &                                        se_subgrid_ind(1,1,se_myworker_pe))
     &                                   - se_subgrid_ind(1,1,se_myworker_pe) + 1
                 se_subgrid_recv_ind(2,1,i) = min(se_lgl_ind(2,1,i),
     &                                        se_subgrid_ind(2,1,se_myworker_pe))
     &                                   - se_subgrid_ind(1,1,se_myworker_pe) + 1
                 se_subgrid_recv_ind(1,2,i) = max(se_lgl_ind(1,2,i),
     &                                        se_subgrid_ind(1,2,se_myworker_pe))
     &                                   - se_subgrid_ind(1,2,se_myworker_pe) + 1
                 se_subgrid_recv_ind(2,2,i) = min(se_lgl_ind(2,2,i),
     &                                        se_subgrid_ind(2,2,se_myworker_pe))
     &                                   - se_subgrid_ind(1,2,se_myworker_pe) + 1
                 se_subgrid_recv(i) = i
              else
                 se_subgrid_recv(i) = -1
              end if
           end do

C -- determine begining and ending row and column indexes, respectively

           my_begrow = 1
           my_begcol = 1
           my_endrow = se_subgrid_ind(2,1,se_myworker_pe) - 
     &                 se_subgrid_ind(1,1,se_myworker_pe) + 1
           my_endcol = se_subgrid_ind(2,2,se_myworker_pe) - 
     &                 se_subgrid_ind(1,2,se_myworker_pe) + 1

C -- if row or column is not included in a processor, set column or row
C    outside the range, respectively

           if (my_endrow .eq. 0) then
              my_endcol = 0
           else if (my_endcol .eq. 0) then
              my_endrow = 0
           end if

           my_subgrid_rows = my_endrow - my_begrow + 1
           my_subgrid_cols = my_endcol - my_begcol + 1

           my_beglev = beglev
           my_endlev = endlev
!          my_subgrid_levs = my_endlev - my_beglev + 1

           if (present(nrows_in)) then
              se_my_subgrid_beglev = 1
              se_my_subgrid_endlev = se_my_nlays
           else
              se_my_subgrid_beglev = beglev
              se_my_subgrid_endlev = endlev
           end if
c          my_subgrid_levs = se_my_subgrid_endlev - se_my_subgrid_beglev + 1
        else
           my_begrow = -1
           my_begcol = -1
           my_endrow = -1
           my_endcol = -1
           my_beglev = -1
           my_endlev = -1
           my_subgrid_rows = -1
           my_subgrid_cols = -1
c          my_subgrid_levs = -1
        end if

!       print*, '  myworker_pe: ', se_myworker_pe
!       do i = 0, se_numworkers-1
!          print*, ' i, se_lgl_ind( 1,1,i ): ',          i, se_lgl_ind( 1,1,i )
!          print*, ' i, se_lgl_ind( 2,1,i ): ',          i, se_lgl_ind( 2,1,i )
!          print*, ' i, se_lgl_ind( 1,2,i ): ',          i, se_lgl_ind( 1,2,i )
!          print*, ' i, se_lgl_ind( 2,2,i ): ',          i, se_lgl_ind( 2,2,i )
!          print*, ' i, se_subgrid_ind( 1,1,i ): ',      i, se_subgrid_ind( 1,1,i )
!          print*, ' i, se_subgrid_ind( 2,1,i ): ',      i, se_subgrid_ind( 2,1,i )
!          print*, ' i, se_subgrid_ind( 1,2,i ): ',      i, se_subgrid_ind( 1,2,i )
!          print*, ' i, se_subgrid_ind( 2,2,i ): ',      i, se_subgrid_ind( 2,2,i )
!          print*, ' i, se_subgrid_send( i ): ',         i, se_subgrid_send( i )
!          print*, ' i, se_subgrid_recv( i ): ',         i, se_subgrid_recv( i )
!          print*, ' i, se_subgrid_send_ind( 1,1,i ): ', i, se_subgrid_send_ind( 1,1,i )
!          print*, ' i, se_subgrid_send_ind( 2,1,i ): ', i, se_subgrid_send_ind( 2,1,i )
!          print*, ' i, se_subgrid_send_ind( 1,2,i ): ', i, se_subgrid_send_ind( 1,2,i )
!          print*, ' i, se_subgrid_send_ind( 2,2,i ): ', i, se_subgrid_send_ind( 2,2,i )
!          print*, ' i, se_subgrid_recv_ind( 1,1,i ): ', i, se_subgrid_recv_ind( 1,1,i )
!          print*, ' i, se_subgrid_recv_ind( 2,1,i ): ', i, se_subgrid_recv_ind( 2,1,i )
!          print*, ' i, se_subgrid_recv_ind( 1,2,i ): ', i, se_subgrid_recv_ind( 1,2,i )
!          print*, ' i, se_subgrid_recv_ind( 2,2,i ): ', i, se_subgrid_recv_ind( 2,2,i )
!       end do

        deallocate (se_lgl_ind)

        return
        end subroutine se_subgrid_index_3

C -----------------------------------------------------------------------------
C Purpose:
C
C   given starting point and ending point in terms of column and row number in
C original grid domain, determine a sub-grid mapping without re-distribution of
C data for a 2-D data structure.
C
C Revision history:
C
C   Orginal version: 10/26/00 by David Wong
C
C                    12/04/02 by David Wong
C                       -- modified the routine to accommodate worker and 
C                          I/O processors partition scheme
C
C                    08/24/11 by David Wong
C                      -- removed se_ori_ext to eliminate data and geo orientation
C
C Parameter List:
C
C   In  : begrow -- starting row number in the original grid
C         endrow -- ending row number in the original grid
C         begcol -- starting column number in the original grid
C         endcol -- ending column number in the original grid
C         flag   -- an indicator of no re-distribution
C
C   Out : my_begrow -- starting row number of the sub-grid in a processor
C         my_endrow -- end row number of the sub-grid in a processor
C         my_begcol -- starting column number of the sub-grid in a processor
C         my_endcol -- end column number of the sub-grid in a processor
C
C Local Variable:
C
C   tpe           -- temporary processor number
C   mpe           -- processor number form by modulus arithmetic
C   dpe           -- processor number form by integer division arithmetic
C   col_ind       -- column index
C   row_ind       -- row index
C   loc_begrow    -- local starting row number
C   loc_endrow    -- local ending row number
C   loc_begcol    -- local starting column number
C   loc_endcol    -- local ending column number
C   loc_my_begrow -- local starting row number in a processor
C   loc_my_endrow -- local ending row number in a processor
C   loc_my_begcol -- local starting column number in a processor
C   loc_my_endcol -- local ending column number in a processor
C
C -----------------------------------------------------------------------------

        subroutine se_subgrid_index_n2 (begrow, endrow, begcol, endcol, 
     &                                  my_begrow, my_endrow, 
     &                                  my_begcol, my_endcol, flag)

        use se_pe_info_ext
!       use se_ori_ext
        use se_domain_info_ext

        implicit none

        integer, intent(in) :: begrow, endrow, begcol, endcol
        integer, intent(out) ::  my_begrow, my_endrow, my_begcol, my_endcol
        character, intent(in) :: flag

        integer :: col_ind(2), row_ind(2)
        integer :: tpe, mpe, dpe
        integer :: loc_my_begrow, loc_my_endrow,
     &             loc_my_begcol, loc_my_endcol
        integer :: loc_begrow, loc_endrow, loc_begcol, loc_endcol

!       if ((se_data_ori .eq. "cr") .or. (se_data_ori .eq. "CR")) then
           loc_begcol = begrow
           loc_endcol = endrow
           loc_begrow = begcol
           loc_endrow = endcol
!       else
!          loc_begcol = begcol
!          loc_endcol = endcol
!          loc_begrow = begrow
!          loc_endrow = endrow
!       end if

C -- figure out the low and high column index of the original grid

        tpe = mod(se_gl_ncols, se_npcol)
        mpe = mod(se_myworker_pe, se_npcol)
        col_ind(1) = 1 + min(mpe, tpe) * se_ncols + max(mpe-tpe,0) * se_my_ncols
        col_ind(2) = min(mpe+1, tpe) * se_ncols + max(mpe-tpe+1,0) * se_my_ncols
C -- determine the mapping of column of the PA grid to each processor

        if ((col_ind(2) .lt. loc_begcol) .or. (col_ind(1) .gt. loc_endcol)) then
           loc_my_begcol = 0
           loc_my_endcol = -1
        else if ((col_ind(1) .lt. loc_begcol) .and. (loc_begcol .le. col_ind(2))
     &           .and. (loc_endcol .gt. col_ind(2))) then
           loc_my_begcol = loc_begcol - col_ind(1) + 1
           loc_my_endcol = col_ind(2) - col_ind(1) + 1
        else if ((col_ind(1) .le. loc_endcol) .and. (loc_endcol .lt. col_ind(2))
     &           .and. (loc_begcol .lt. col_ind(1))) then
           loc_my_begcol = 1
           loc_my_endcol = loc_endcol - col_ind(1) + 1
        else
           loc_my_begcol = max(loc_begcol-col_ind(1)+1, 1)
           loc_my_endcol = loc_my_begcol + min(col_ind(2)-col_ind(1),
     &                                         loc_endcol-loc_begcol)
        end if

C -- figure out the low and high row index of the original grid

        tpe = mod(se_gl_nrows, se_nprow)
        dpe = se_myworker_pe / se_npcol
        row_ind(1) = 1 + min(dpe, tpe) * se_nrows + max(dpe-tpe,0) * se_my_nrows
        row_ind(2) = min(dpe+1, tpe) * se_nrows + max(dpe-tpe+1,0) * se_my_nrows
C -- determine the mapping of row of the PA grid to each processor

        if ((row_ind(2) .lt. loc_begrow) .or. (row_ind(1) .gt. loc_endrow)) then
           loc_my_begrow = 0
           loc_my_endrow = -1
        else if ((row_ind(1) .lt. loc_begrow) .and. (loc_begrow .le. row_ind(2))
     &           .and. (loc_endrow .gt. row_ind(2))) then
           loc_my_begrow = loc_begrow - row_ind(1) + 1
           loc_my_endrow = row_ind(2) - row_ind(1) + 1
        else if ((row_ind(1) .le. loc_endrow) .and. (loc_endrow .lt. row_ind(2))
     &           .and. (loc_begrow .lt. row_ind(1))) then
           loc_my_begrow = 1
           loc_my_endrow = loc_endrow - row_ind(1) + 1
        else
           loc_my_begrow = max(loc_begrow-row_ind(1)+1, 1)
           loc_my_endrow = loc_my_begrow + min(row_ind(2)-row_ind(1),
     &                                         loc_endrow-loc_begrow)
        end if

C -- if row or column is not within the PA grid dimensions, set column or row
C    outside the range, respectively

        if (loc_my_begrow .eq. 0) then
           loc_my_begcol = 0
           loc_my_endcol = -1
        else if (loc_my_begcol .eq. 0) then
           loc_my_begrow = 0
           loc_my_endrow = -1
        end if

!       if ((se_data_ori .eq. "cr") .or. (se_data_ori .eq. "CR")) then
           my_begrow = loc_my_begcol
           my_endrow = loc_my_endcol
           my_begcol = loc_my_begrow
           my_endcol = loc_my_endrow
!       else
!          my_begrow = loc_my_begrow
!          my_endrow = loc_my_endrow
!          my_begcol = loc_my_begcol
!          my_endcol = loc_my_endcol
!       end if

        return
        end subroutine se_subgrid_index_n2

C -----------------------------------------------------------------------------
C Purpose:
C
C   given starting point and ending point in terms of column, row, and level 
C number in original grid domain, determine a sub-grid mapping without 
C re-distribution of data for a 3-D data structure.
C
C Revision history:
C
C   Orginal version: 10/26/00 by David Wong
C
C                    12/04/02 by David Wong
C                       -- modified the routine to accommodate worker and 
C                          I/O processors partition scheme
C
C                    08/24/11 by David Wong
C                      -- removed se_ori_ext to eliminate data and geo orientation
C
C Parameter List:
C
C   In  : begrow -- starting row number in the original grid
C         endrow -- ending row number in the original grid
C         begcol -- starting column number in the original grid
C         endcol -- ending column number in the original grid
C         beglev -- starting level number in the original grid
C         endlev -- ending level number in the original grid
C         flag   -- an indicator of no re-distribution
C
C   Out : my_begrow -- starting row number of the sub-grid in a processor
C         my_endrow -- end row number of the sub-grid in a processor
C         my_begcol -- starting column number of the sub-grid in a processor
C         my_endcol -- end column number of the sub-grid in a processor
C         my_beglev -- starting level number of the sub-grid in a processor
C         my_endlev -- end level number of the sub-grid in a processor
C
C Local Variable:
C
C   tpe           -- temporary processor number
C   mpe           -- processor number form by modulus arithmetic
C   dpe           -- processor number form by integer division arithmetic
C   col_ind       -- column index
C   row_ind       -- row index
C   loc_begrow    -- local starting row number
C   loc_endrow    -- local ending row number
C   loc_begcol    -- local starting column number
C   loc_endcol    -- local ending column number
C   loc_my_begrow -- local starting row number in a processor
C   loc_my_endrow -- local ending row number in a processor
C   loc_my_begcol -- local starting column number in a processor
C   loc_my_endcol -- local ending column number in a processor
C
C -----------------------------------------------------------------------------

        subroutine se_subgrid_index_n3 (begrow, endrow, begcol, endcol, 
     &                                  beglev, endlev,
     &                                  my_begrow, my_endrow, 
     &                                  my_begcol, my_endcol, 
     &                                  my_beglev, my_endlev, flag)

        use se_pe_info_ext
!       use se_ori_ext
        use se_domain_info_ext

        implicit none

        integer, intent(in) :: begrow, endrow, begcol, endcol, beglev, endlev
        integer, intent(out) ::  my_begrow, my_endrow, my_begcol,
     &                           my_endcol, my_beglev, my_endlev
        character, intent(in) :: flag

        integer :: col_ind(2), row_ind(2)
        integer :: tpe, mpe, dpe
        integer :: loc_my_begrow, loc_my_endrow,
     &             loc_my_begcol, loc_my_endcol
        integer :: loc_begrow, loc_endrow, loc_begcol, loc_endcol

!       if ((se_data_ori .eq. "cr") .or. (se_data_ori .eq. "CR")) then
           loc_begcol = begrow
           loc_endcol = endrow
           loc_begrow = begcol
           loc_endrow = endcol
!       else
!          loc_begcol = begcol
!          loc_endcol = endcol
!          loc_begrow = begrow
!          loc_endrow = endrow
!       end if

C -- figure out the low and high column index of the original grid

        tpe = mod(se_gl_ncols, se_npcol)
        mpe = mod(se_myworker_pe, se_npcol)
        col_ind(1) = 1 + min(mpe, tpe) * se_ncols + max(mpe-tpe,0) * se_my_ncols
        col_ind(2) = min(mpe+1, tpe) * se_ncols + max(mpe-tpe+1,0) * se_my_ncols
C -- determine the mapping of column of the PA grid to each processor

        if ((col_ind(2) .lt. loc_begcol) .or. (col_ind(1) .gt. loc_endcol)) then
           loc_my_begcol = 0
           loc_my_endcol = -1
        else if ((col_ind(1) .lt. loc_begcol) .and. (loc_begcol .le. col_ind(2))
     &           .and. (loc_endcol .gt. col_ind(2))) then
           loc_my_begcol = loc_begcol - col_ind(1) + 1
           loc_my_endcol = col_ind(2) - col_ind(1) + 1
        else if ((col_ind(1) .le. loc_endcol) .and. (loc_endcol .lt. col_ind(2))
     &           .and. (loc_begcol .lt. col_ind(1))) then
           loc_my_begcol = 1
           loc_my_endcol = loc_endcol - col_ind(1) + 1
        else
           loc_my_begcol = max(loc_begcol-col_ind(1)+1, 1)
           loc_my_endcol = loc_my_begcol + min(col_ind(2)-col_ind(1),
     &                                         loc_endcol-loc_begcol)
        end if

C -- figure out the low and high row index of the original grid

        tpe = mod(se_gl_nrows, se_nprow)
        dpe = se_myworker_pe / se_npcol
        row_ind(1) = 1 + min(dpe, tpe) * se_nrows + max(dpe-tpe,0) * se_my_nrows
        row_ind(2) = min(dpe+1, tpe) * se_nrows + max(dpe-tpe+1,0) * se_my_nrows
C -- determine the mapping of row of the PA grid to each processor

        if ((row_ind(2) .lt. loc_begrow) .or. (row_ind(1) .gt. loc_endrow)) then
           loc_my_begrow = 0
           loc_my_endrow = -1
        else if ((row_ind(1) .lt. loc_begrow) .and. (loc_begrow .le. row_ind(2))
     &           .and. (loc_endrow .gt. row_ind(2))) then
           loc_my_begrow = loc_begrow - row_ind(1) + 1
           loc_my_endrow = row_ind(2) - row_ind(1) + 1
        else if ((row_ind(1) .le. loc_endrow) .and. (loc_endrow .lt. row_ind(2))
     &           .and. (loc_begrow .lt. row_ind(1))) then
           loc_my_begrow = 1
           loc_my_endrow = loc_endrow - row_ind(1) + 1
        else
           loc_my_begrow = max(loc_begrow-row_ind(1)+1, 1)
           loc_my_endrow = loc_my_begrow + min(row_ind(2)-row_ind(1),
     &                                         loc_endrow-loc_begrow)
        end if

C -- if row or column is not within the PA grid dimensions, set column or row
C    outside the range, respectively

        if (loc_my_begrow .eq. 0) then
           loc_my_begcol = 0
           loc_my_endcol = -1
        else if (loc_my_begcol .eq. 0) then
           loc_my_begrow = 0
           loc_my_endrow = -1
        end if

!       if ((se_data_ori .eq. "cr") .or. (se_data_ori .eq. "CR")) then
           my_begrow = loc_my_begcol
           my_endrow = loc_my_endcol
           my_begcol = loc_my_begrow
           my_endcol = loc_my_endrow
!       else
!          my_begrow = loc_my_begrow
!          my_endrow = loc_my_endrow
!          my_begcol = loc_my_begcol
!          my_endcol = loc_my_endcol
!       end if

        my_beglev = beglev
        my_endlev = endlev

        return
        end subroutine se_subgrid_index_n3

C -----------------------------------------------------------------------------
C Purpose:
C
C   synchronizes the value of var in all the processors according to the value
C   in processor pe
C
C Revision history:
C
C   Orginal version: 12/04/01 by David Wong
C
C                    12/04/02 by David Wong
C                       -- modified the routine to accommodate worker and 
C                          I/O processors partition scheme
C
C Parameter List:
C
C   InOut : var -- logical value
C   In    : pe  -- processor number
C -----------------------------------------------------------------------------

        subroutine se_in_syn (var, pe)

        use se_pe_info_ext

        implicit none

        logical, intent(inout) :: var
        integer, intent(in) :: pe

        integer :: error
 
        include "mpif.h"

        if (pe .ge. 0) then
           call mpi_bcast (var, 1, mpi_logical, pe, se_worker_comm, error)
        end if

        return
        end subroutine se_in_syn

C -----------------------------------------------------------------------------
C Purpose:
C
C   synchronizes the value of var in all the processors according to the value
C   in processor pe
C
C Revision history:
C
C   Orginal version: 12/04/01 by David Wong
C
C                    12/04/02 by David Wong
C                       -- modified the routine to accommodate worker and 
C                          I/O processors partition scheme
C
C Parameter List:
C
C   InOut : var -- logical value
C   In    : pe  -- processor number
C -----------------------------------------------------------------------------

        subroutine se_collect (ncols, nrows, ncols_pe, nrows_pe)

        use se_pe_info_ext

        implicit none

        include "mpif.h"

        integer, intent(inout) :: ncols, nrows
        integer, intent(out) :: ncols_pe(:), nrows_pe(:)

        integer :: sdata(2), rdata(2)
        integer :: i, error, status(MPI_STATUS_SIZE)

        if (se_my_pe .eq. 0) then
           ncols_pe = 0
           nrows_pe = 0
           do i = 1, se_numprocs-1
              call mpi_recv (rdata, 2, mpi_integer, i,
     &                       i, se_world_comm, status, error)
              ncols_pe(i+1) = rdata(1)
              nrows_pe(i+1) = rdata(2)
           end do
           ncols = maxval(ncols_pe)
           nrows = maxval(nrows_pe)
        else
           sdata(1) = ncols
           sdata(2) = nrows
           call mpi_send (sdata, 2, mpi_integer, 0, se_my_pe, se_world_comm, error)
        end if
        
        end subroutine se_collect

        end module se_util_module
