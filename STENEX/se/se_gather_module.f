
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
C $Header: /project/work/rep/STENEX/src/se_snl/se_gather_module.f,v 1.2 2006/02/15 14:41:56 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C --------------------------------------------------------------------------
C Purpose:
C
C   use F90 module feature to include all routines to perform data gather 
C function in one module and use F90 interface feature to achieve "faked" 
C polymorphism for data gather routine
C
C   there are two major routines: se_setup_gather and se_gather[n] where [n] 
C denotes the dimensionality of the data
C
C   se_setup_gather:
C      -- to setup source PE of communication for I/O PE, namely PE 0
C
C      details description is provided with the routine
C
C   se_gather[n]:
C      -- to gather n-dimensional array data from all PEs who hold information 
C         that need to be output, and store in the I/O PE namely PE 0
C
C   Subroutine parameter description:
C
C     In:  data     -- original data
C          pec      -- indicator of where data is locator in
C          ptr      -- address of data that need to be gathered
C          n        -- number of data need to be gethered in a processor
C          sdim     -- slicing data dimension, i.e. the dimenison of the data
C                      that are located in various processor
C
C     Out: data     -- original data after communication
C
C   Local variable description:
C
C      locpec     -- local copy of pec
C      locptr     -- local copy of ptr
C      i, j       -- loop indexes
C      gn         -- global n
C      locn       -- local copy of n
C
C   Example:
C
C     given four PEs, a 3D data array A(m, n, 6) where m and n are integers, and
C   the last dimension was distributed: PE 0 has 5, PE 1 has 1, 2, 3, 4, PE 2 
C   has 6, and PE 3 has nothing. Let data 1, 3, 6 be the ones that need output. 
C   So PEC will be 0 1 1 1 1 2; PRT for PE 1 and PE 2 are 1, 2 and 6, 
C   respectively, PE 0 and PE 3's PRT will be all -1; N for PE 0 to 3 are 0, 2, 
C   1, and 0, respectively. In this example, sdim is 3. Overall picture is, 
C   PE 0 will collect data from PE 1 and 2 in this example.
C
C   Include file:
C
C      se_pe_info_ext
C      se_slice_module
C      se_global_sum_module
C
C   Subroutine/Function call:
C
C     se_global_isum
C     se_slice[n]
C     setup_info
C
C Revision history:
C
C   Orginal version: 11/05/99 by David Wong
C   Add integer data 12/16/00 by Jeff Young
C                    12/04/02 by David Wong
C                       -- modified the routine to accommodate worker and
C                          I/O processors partition scheme
C --------------------------------------------------------------------------

        module se_gather_module

        implicit none

        private :: se_setup_gather

        interface se_gather
          module procedure se_gather1i, se_gather1r, 
     &                     se_gather2i, se_gather2r,
     &                     se_gather3i, se_gather3r, 
     &                     se_gather4i, se_gather4r
        end interface

        contains

C --------------------------------------------------------------------------
C Purpose:
C
C   to setup source PE of communication for I/O PE, namely PE 0
C
C Revision history:
C
C   Orginal version: 7/14/99 by David Wong 
C
C                    11/05/99 by David Wong
C                      -- redesign the code using OO-Based approach in F90
C
C                    12/04/02 by David Wong
C                       -- modified the routine to accommodate worker and
C                          I/O processors partition scheme
C
C Subroutine parameter description:
C
C   In:  ptr      -- address of data that need to be gathered
C        pec      -- indicator of where data is locator in
C        n        -- number of data point in a processor
C        gn       -- total number of data point among all processors
C
C   Out: locptr   -- local ptr
C        locpec   -- local pec
C        locn     -- local n
C
C Local variable description:
C
C    bufptr     -- buffer to hold ptr from all PEs
C    locnarray  -- an array to hold n from all PEs
C    i, k       -- loop index
C    j          -- position of locptr in PE 0 during compact process
C    low, high  -- starting and ending point in bufptr for each PE
C    error      -- error code for mpi call
C
C Example: Given 4 PEs, and only PE 1 and 2 have two and one data point,
C          respectively. Let prt in each PE are:
C
C          0: 0 0 0 0 ...
C          1: 1 3 0 0 ...
C          2: 6 0 0 0 ...
C          3: 0 0 0 0 ...
C
C          Then bufptr in PE 0 will be:
C
C          0: 0 0 0 1 3 0 6 0 0 0 0 0 ...
C
C Subroutine/Function call:
C
C   mpi_gather
C
C --------------------------------------------------------------------------

        subroutine se_setup_gather (ptr, locptr, pec, locpec, gn, n, locn)

        use se_pe_info_ext

        integer, intent(in) :: ptr(:), pec(:), gn, n 
        integer, intent(out) :: locptr(:), locpec(:), locn

        include "mpif.h"

        integer, allocatable :: bufptr(:), locnarray(:)
        integer :: i, j, k, low, high, error, allocate_status

C -- allocate data
        allocate (bufptr(gn*se_numworkers), stat=allocate_status)
        if (allocate_status .ne. 0) then
           print *, ' Subroutine SE_SETUP_GATHER: ',
     &              ' allocation error in bufptr'
           stop
        end if

C -- allocate data
        allocate (locnarray(0:se_numworkers-1), stat=allocate_status)
        if (allocate_status .ne. 0) then
           print *, ' Subroutine SE_SETUP_GATHER: ',
     &              ' allocation error in locnarray'
           stop
        end if

C -- PE 0 colloect ptr information from each PE
        call mpi_gather (ptr, gn, mpi_integer, bufptr, gn,
     &                   mpi_integer, 0, se_worker_comm, error)

C -- PE 0 colloect n from each PE
        call mpi_gather (n, 1, mpi_integer, locnarray(se_myworker_pe), 1,
     &                   mpi_integer, 0, se_worker_comm, error)

        if (se_myworker_pe .eq. 0) then

C -- PE 0 filters and compacts PEC and PTR array data. 

           j = 0
           do i = 0, se_numworkers-1
              low = i * gn + 1
              high = low + locnarray(i) - 1
              do k = low, high
                 j = j + 1
                 locptr(j) = bufptr(k)
                 locpec(j) = i
              end do
           end do
           locn = gn
        else
           do i = 1, gn
              if (i .le. n) then
                 locptr(i) = ptr(i)
                 locpec(i) = pec(ptr(i))
              else
                 locptr(i) = -1
                 locpec(i) = -1
              end if
           end do
           locn = n
        end if

        deallocate (bufptr)
        deallocate (locnarray)

        return
        end subroutine se_setup_gather

C --------------------------------------------------------------------------
        subroutine se_gather1r (data, pec, ptr, n, sdim)

        use se_pe_info_ext
        use se_slice_module
        use se_global_sum_module

        implicit none

        real, intent(inout) :: data(:)
        integer, intent(in) :: pec(:), ptr(:), n, sdim

        integer :: locpec(size(pec))
        integer :: locptr(size(ptr))

        integer :: i, j, gn, locn

        gn = se_global_sum(n)

        call se_setup_gather (ptr, locptr, pec, locpec, gn, n, locn)

C -- gathering data by using se_slice function
        do j = 1, locn
           i = locptr(j) 
           if ((locpec(j) .ge. 0) .or. (se_myworker_pe .eq. 0)) then
              call se_slice (data, locpec(j), 0, sdim, i, i)
           end if
        end do

        return
        end subroutine se_gather1r

C --------------------------------------------------------------------------
        subroutine se_gather2r (data, pec, ptr, n, sdim)

        use se_pe_info_ext
        use se_slice_module
        use se_global_sum_module

        implicit none

        real, intent(inout) :: data(:,:)
        integer, intent(in) :: pec(:), ptr(:), n, sdim

        integer :: locpec(size(pec))
        integer :: locptr(size(ptr))

        integer :: i, j, gn, locn

        gn = se_global_sum(n)

        call se_setup_gather (ptr, locptr, pec, locpec, gn, n, locn)

C -- gathering data by using se_slice function
        do j = 1, locn
           i = locptr(j) 
           if ((locpec(j) .ge. 0) .or. (se_myworker_pe .eq. 0)) then
              call se_slice (data, locpec(j), 0, sdim, i, i)
           end if
        end do

        return
        end subroutine se_gather2r

C --------------------------------------------------------------------------
        subroutine se_gather3r (data, pec, ptr, n, sdim)

        use se_pe_info_ext
        use se_slice_module
        use se_global_sum_module

        implicit none

        real, intent(inout) :: data(:,:,:)
        integer, intent(in) :: pec(:), ptr(:), n, sdim

        integer :: locpec(size(pec))
        integer :: locptr(size(ptr))

        integer :: i, j, gn, locn

        gn = se_global_sum(n)

        call se_setup_gather (ptr, locptr, pec, locpec, gn, n, locn)

C -- gathering data by using se_slice function
        do j = 1, locn
           i = locptr(j) 
           if ((locpec(j) .ge. 0) .or. (se_myworker_pe .eq. 0)) then
              call se_slice (data, locpec(j), 0, sdim, i, i)
           end if
        end do

        return
        end subroutine se_gather3r

C --------------------------------------------------------------------------
        subroutine se_gather4r (data, pec, ptr, n, sdim)

        use se_pe_info_ext
        use se_slice_module
        use se_global_sum_module

        implicit none

        real, intent(inout) :: data(:,:,:,:)
        integer, intent(in) :: pec(:), ptr(:), n, sdim

        integer :: locpec(size(pec))
        integer :: locptr(size(ptr))

        integer :: i, j, gn, locn

        gn = se_global_sum(n)

        call se_setup_gather (ptr, locptr, pec, locpec, gn, n, locn)

C -- gathering data by using se_slice function
        do j = 1, locn
           i = locptr(j) 
           if ((locpec(j) .ge. 0) .or. (se_myworker_pe .eq. 0)) then
              call se_slice (data, locpec(j), 0, sdim, i, i)
           end if
        end do

        return
        end subroutine se_gather4r

C --------------------------------------------------------------------------
        subroutine se_gather1i (data, pec, ptr, n, sdim)

        use se_pe_info_ext
        use se_slice_module
        use se_global_sum_module

        implicit none

        integer, intent(inout) :: data(:)
        integer, intent(in) :: pec(:), ptr(:), n, sdim

        integer :: locpec(size(pec))
        integer :: locptr(size(ptr))

        integer :: i, j, gn, locn

        gn = se_global_sum(n)

        call se_setup_gather (ptr, locptr, pec, locpec, gn, n, locn)

C -- gathering data by using se_slice function
        do j = 1, locn
           i = locptr(j) 
           if ((locpec(j) .ge. 0) .or. (se_myworker_pe .eq. 0)) then
              call se_slice (data, locpec(j), 0, sdim, i, i)
           end if
        end do

        return
        end subroutine se_gather1i

C --------------------------------------------------------------------------
        subroutine se_gather2i (data, pec, ptr, n, sdim)

        use se_pe_info_ext
        use se_slice_module
        use se_global_sum_module

        implicit none

        integer, intent(inout) :: data(:,:)
        integer, intent(in) :: pec(:), ptr(:), n, sdim

        integer :: locpec(size(pec))
        integer :: locptr(size(ptr))

        integer :: i, j, gn, locn

        gn = se_global_sum(n)

        call se_setup_gather (ptr, locptr, pec, locpec, gn, n, locn)

C -- gathering data by using se_slice function
        do j = 1, locn
           i = locptr(j) 
           if ((locpec(j) .ge. 0) .or. (se_myworker_pe .eq. 0)) then
              call se_slice (data, locpec(j), 0, sdim, i, i)
           end if
        end do

        return
        end subroutine se_gather2i

C --------------------------------------------------------------------------
        subroutine se_gather3i (data, pec, ptr, n, sdim)

        use se_pe_info_ext
        use se_slice_module
        use se_global_sum_module

        implicit none

        integer, intent(inout) :: data(:,:,:)
        integer, intent(in) :: pec(:), ptr(:), n, sdim

        integer :: locpec(size(pec))
        integer :: locptr(size(ptr))

        integer :: i, j, gn, locn

        gn = se_global_sum(n)

        call se_setup_gather (ptr, locptr, pec, locpec, gn, n, locn)

C -- gathering data by using se_slice function
        do j = 1, locn
           i = locptr(j) 
           if ((locpec(j) .ge. 0) .or. (se_myworker_pe .eq. 0)) then
              call se_slice (data, locpec(j), 0, sdim, i, i)
           end if
        end do

        return
        end subroutine se_gather3i

C --------------------------------------------------------------------------
        subroutine se_gather4i (data, pec, ptr, n, sdim)

        use se_pe_info_ext
        use se_slice_module
        use se_global_sum_module

        implicit none

        integer, intent(inout) :: data(:,:,:,:)
        integer, intent(in) :: pec(:), ptr(:), n, sdim

        integer :: locpec(size(pec))
        integer :: locptr(size(ptr))

        integer :: i, j, gn, locn

        gn = se_global_sum(n)

        call se_setup_gather (ptr, locptr, pec, locpec, gn, n, locn)

C -- gathering data by using se_slice function
        do j = 1, locn
           i = locptr(j) 
           if ((locpec(j) .ge. 0) .or. (se_myworker_pe .eq. 0)) then
              call se_slice (data, locpec(j), 0, sdim, i, i)
           end if
        end do

        return
        end subroutine se_gather4i

        end module se_gather_module
