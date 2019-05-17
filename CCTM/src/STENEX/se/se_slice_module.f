
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
C $Header: /project/work/rep/STENEX/src/se_snl/se_slice_module.f,v 1.2 2006/02/15 14:41:56 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C --------------------------------------------------------------------------
C Purpose:
C
C   use F90 interface feature to achieve "faked" polymorphism for data
C   slicing routine
C
C Revision history:
C
C   Orginal version: 11/05/99 by David Wong
C   Add integer data 12/16/00 by Jeff Young
C --------------------------------------------------------------------------

        module se_slice_module

        implicit none

        interface se_slice
          module procedure se_slice1i, se_slice1r,
     &                     se_slice2i, se_slice2r,
     &                     se_slice3i, se_slice3r,
     &                     se_slice4i, se_slice4r
        end interface

        contains

C --------------------------------------------------------------------------
C Purpose:
C
C   to perform transferring a slice of a 1-D integer data 
C
C Revision history:
C
C   Orginal version: 5/26/99 by David Wong 
C                    11/05/99 by David Wong
C                      -- recode the code using F90 syntax
C                    12/04/02 by David Wong
C                       -- modified the routine to accommodate worker and 
C                          I/O processors partition scheme
C
C Subroutine parameter description:
C
C   In:  data     -- original data
C        sourcepe -- source PE
C        destpe   -- target PE
C        sdim     -- slicing dimension
C        from     -- index of the slicing source
C        to       -- index of the slicing destination
C
C   Out: data     -- original data after communication
C
C Local variable description:
C
C    status     -- return status of MPI_RECEIVING call
C    error      -- error code of invoking MPI calls
C
C Include file:
C
C    se_pe_info.ext
C
C Subroutine/Function call:
C
C   mpi_send
C   mpi_recv
C
C --------------------------------------------------------------------------

        subroutine se_slice1i (data, sourcepe, destpe, sdim, from, to)

        use se_pe_info_ext

        implicit none

        include "mpif.h"

        integer, intent(inout) :: data(:)
        integer, intent(in) :: sourcepe, destpe, sdim, from, to

        integer :: error
        integer :: status(MPI_STATUS_SIZE)

        if ((sourcepe .ge. 0) .and. (destpe .ge. 0) .and. 
     &      (sourcepe .ne. destpe)) then
           if (se_myworker_pe .eq. sourcepe) then

C -- send data to corresponding processor 

              call mpi_send (data(from), 1, mpi_integer, destpe,
     &                       sourcepe, se_worker_comm, error)

           else if (se_myworker_pe .eq. destpe) then

C -- receive data from corresponding processor

              call mpi_recv (data(to), 1, mpi_integer, sourcepe,
     &                       sourcepe, se_worker_comm, status, error)

           end if
        end if

        return
        end subroutine se_slice1i

C --------------------------------------------------------------------------
C Purpose:
C
C   to perform transferring a slice of a 1-D real data 
C
C Revision history:
C
C   Orginal version: 5/26/99 by David Wong 
C                    11/05/99 by David Wong
C                      -- recode the code using F90 syntax
C                    12/04/02 by David Wong
C                       -- modified the routine to accommodate worker and
C                          I/O processors partition scheme
C
C Subroutine parameter description:
C
C   In:  data     -- original data
C        sourcepe -- source PE
C        destpe   -- target PE
C        sdim     -- slicing dimension
C        from     -- index of the slicing source
C        to       -- index of the slicing destination
C
C   Out: data     -- original data after communication
C
C Local variable description:
C
C    status     -- return status of MPI_RECEIVING call
C    error      -- error code of invoking MPI calls
C
C Include file:
C
C    se_pe_info.ext
C
C Subroutine/Function call:
C
C   mpi_send
C   mpi_recv
C
C --------------------------------------------------------------------------

        subroutine se_slice1r (data, sourcepe, destpe, sdim, from, to)

        use se_pe_info_ext

        implicit none

        include "mpif.h"

        real, intent(inout) :: data(:)
        integer, intent(in) :: sourcepe, destpe, sdim, from, to

        integer :: error
        integer :: status(MPI_STATUS_SIZE)

        if ((sourcepe .ge. 0) .and. (destpe .ge. 0) .and. 
     &      (sourcepe .ne. destpe)) then
           if (se_myworker_pe .eq. sourcepe) then

C -- send data to corresponding processor 

              call mpi_send (data(from), 1, mpi_real, destpe,
     &                       sourcepe, se_worker_comm, error)

           else if (se_myworker_pe .eq. destpe) then

C -- receive data from corresponding processor

              call mpi_recv (data(to), 1, mpi_real, sourcepe,
     &                       sourcepe, se_worker_comm, status, error)

           end if
        end if

        return
        end subroutine se_slice1r

C --------------------------------------------------------------------------
C Purpose:
C
C   transfer a slice of a 2-D integer array 
C
C Revision history:
C
C   Orginal version: 5/26/99 by David Wong 
C                    11/05/99 by David Wong
C                      -- recode the code using F90 syntax
C                    12/16/00 by Jeff Young
C                      -- mod for integer data
C                    12/04/02 by David Wong
C                       -- modified the routine to accommodate worker and
C                          I/O processors partition scheme
C                    09/19/18 by David Wong
C                       -- removed unused variable
C
C Subroutine parameter description:
C
C   In:  data     -- original data
C        sourcepe -- source PE
C        destpe   -- target PE
C        sdim     -- slicing dimension
C        from     -- index of the slicing source
C        to       -- index of the slicing destination
C
C   Out: data     -- original data after communication
C
C Local variable description:
C
C    i, j       -- loop indexes
C    li, ui     -- local low and upper index of i dimension
C    lj, uj     -- local low and upper index of j dimension
C    status     -- return status of MPI_RECEIVING call
C    error      -- error code of invoking MPI calls
C    scount     -- number of items need to be sent
C    rcount     -- number of items are expected to receive
C    sarray     -- array to hold sending data
C    rarray     -- array to hold receiving data
C
C Include file:
C
C    se_pe_info.ext
C
C Subroutine/Function call:
C
C   mpi_send
C   mpi_recv
C
C --------------------------------------------------------------------------

        subroutine se_slice2i (data, sourcepe, destpe, sdim, from, to)

        use se_pe_info_ext

        implicit none

        include "mpif.h"
        
        integer, intent(inout) :: data(:,:)
        integer, intent(in) :: sourcepe, destpe, sdim, from, to

        integer :: error
        integer :: i, j, li, lj, ui, uj
        integer :: status(MPI_STATUS_SIZE)
        integer :: scount, rcount
        integer :: sarray(size(data)), rarray(size(data))

C -- send data to corresponding processor 

        if ((sourcepe .ge. 0) .and. (destpe .ge. 0) .and.
     &      (sourcepe .ne. destpe)) then

           li = lbound(data,1)
           ui = ubound(data,1)
           lj = lbound(data,2)
           uj = ubound(data,2)

           if (se_myworker_pe .eq. sourcepe) then

                if (sdim .eq. 1) then
                 li = from
                 ui = from
                else
                 lj = from
                 uj = from
              end if

              scount = 0
C -- pack data for sending
              do j = lj, uj
                 do i = li, ui
                    scount = scount + 1
                    sarray(scount) = data(i,j)
                 end do
              end do

              call mpi_send (sarray, scount, mpi_integer, destpe,
     &                       sourcepe, se_worker_comm, error)

           else if (se_myworker_pe .eq. destpe) then

                   if (sdim .eq. 1) then
                 li = to
                 ui = to
                else
                 lj = to
                 uj = to
              end if

C -- receive data from corresponding processor

              rcount = (ui - li + 1) * (uj - lj + 1)

              call mpi_recv (rarray, rcount, mpi_integer, sourcepe,
     &                       sourcepe, se_worker_comm, status, error)

C -- unpack received data
              rcount = 0
              do j = lj, uj
                  do i = li, ui
                     rcount = rcount + 1
                     data(i,j) = rarray(rcount) 
                  end do
              end do

           end if
        end if

        return
        end subroutine se_slice2i

C --------------------------------------------------------------------------
C Purpose:
C
C   to perform transferring a slice of a 2-D real data 
C
C Revision history:
C
C   Orginal version: 5/26/99 by David Wong 
C                    11/05/99 by David Wong
C                      -- recode the code using F90 syntax
C                    12/04/02 by David Wong
C                       -- modified the routine to accommodate worker and
C                          I/O processors partition scheme
C                    09/19/18 by David Wong
C                       -- removed unused variable
C
C Subroutine parameter description:
C
C   In:  data     -- original data
C        sourcepe -- source PE
C        destpe   -- target PE
C        sdim     -- slicing dimension
C        from     -- index of the slicing source
C        to       -- index of the slicing destination
C
C   Out: data     -- original data after communication
C
C Local variable description:
C
C    i, j       -- loop indexes
C    li, ui     -- local low and upper index of i dimension
C    lj, uj     -- local low and upper index of j dimension
C    status     -- return status of MPI_RECEIVING call
C    error      -- error code of invoking MPI calls
C    scount     -- number of items need to be sent
C    rcount     -- number of items are expected to receive
C    sarray     -- array to hold sending data
C    rarray     -- array to hold receiving data
C
C Include file:
C
C    se_pe_info.ext
C
C Subroutine/Function call:
C
C   mpi_send
C   mpi_recv
C
C --------------------------------------------------------------------------

        subroutine se_slice2r (data, sourcepe, destpe, sdim, from, to)

        use se_pe_info_ext

        implicit none

        include "mpif.h"
        
        real, intent(inout) :: data(:,:)
        integer, intent(in) :: sourcepe, destpe, sdim, from, to

        integer :: error
        integer :: i, j, li, lj, ui, uj
        integer :: status(MPI_STATUS_SIZE)
        integer :: scount, rcount
        real :: sarray(size(data)), rarray(size(data))

C -- send data to corresponding processor 

        if ((sourcepe .ge. 0) .and. (destpe .ge. 0) .and.
     &      (sourcepe .ne. destpe)) then

           li = lbound(data,1)
           ui = ubound(data,1)
           lj = lbound(data,2)
           uj = ubound(data,2)

           if (se_myworker_pe .eq. sourcepe) then

                if (sdim .eq. 1) then
                 li = from
                 ui = from
                else
                 lj = from
                 uj = from
              end if

              scount = 0
C -- pack data for sending
              do j = lj, uj
                 do i = li, ui
                    scount = scount + 1
                    sarray(scount) = data(i,j)
                 end do
              end do

              call mpi_send (sarray, scount, mpi_real, destpe,
     &                       sourcepe, se_worker_comm, error)

           else if (se_myworker_pe .eq. destpe) then

                   if (sdim .eq. 1) then
                 li = to
                 ui = to
                else
                 lj = to
                 uj = to
              end if

C -- receive data from corresponding processor

              rcount = (ui - li + 1) * (uj - lj + 1)

              call mpi_recv (rarray, rcount, mpi_real, sourcepe,
     &                       sourcepe, se_worker_comm, status, error)

C -- unpack received data
              rcount = 0
              do j = lj, uj
                  do i = li, ui
                     rcount = rcount + 1
                     data(i,j) = rarray(rcount) 
                  end do
              end do

           end if
        end if

        return
        end subroutine se_slice2r

C --------------------------------------------------------------------------
C Purpose:
C
C   transfer a slice of a 3-D integer array 
C
C Revision history:
C
C   Orginal version: 5/26/99 by David Wong 
C                    11/05/99 by David Wong
C                      -- recode the code using F90 syntax
C                    12/16/00 by Jeff Young
C                      -- mod for integer data
C                    12/04/02 by David Wong
C                       -- modified the routine to accommodate worker and
C                          I/O processors partition scheme
C
C Subroutine parameter description:
C
C   In:  data     -- original data
C        sourcepe -- source PE
C        destpe   -- target PE
C        sdim     -- slicing dimension
C        from     -- index of the slicing source
C        to       -- index of the slicing destination
C
C   Out: data     -- original data after communication
C
C Local variable description:
C
C    i, j, k    -- loop indexes
C    li, ui     -- local low and upper index of i dimension
C    lj, uj     -- local low and upper index of j dimension
C    lk, uk     -- local low and upper index of k dimension
C    status     -- return status of MPI_RECEIVING call
C    error      -- error code of invoking MPI calls
C    scount     -- number of items need to be sent
C    rcount     -- number of items are expected to receive
C    sarray     -- array to hold sending data
C    rarray     -- array to hold receiving data
C
C Include file:
C
C    se_pe_info.ext
C
C Subroutine/Function call:
C
C   mpi_send
C   mpi_recv
C
C --------------------------------------------------------------------------

        subroutine se_slice3i (data, sourcepe, destpe, sdim, from, to)

        use se_pe_info_ext

        implicit none

        integer, intent(inout) :: data(:,:,:)
        integer, intent(in) :: sourcepe, destpe, sdim, from, to

        include "mpif.h"
        
        integer :: error
        integer :: i, j, k, li, lj, lk, ui, uj, uk
        integer :: status(MPI_STATUS_SIZE)
        integer :: scount, rcount
        integer :: sarray(size(data)), rarray(size(data))

        if ((sourcepe .ge. 0) .and. (destpe .ge. 0) .and.
     &      (sourcepe .ne. destpe)) then

C -- send data to corresponding processor 

           li = lbound(data,1)
           ui = ubound(data,1)
           lj = lbound(data,2)
           uj = ubound(data,2)
           lk = lbound(data,3)
           uk = ubound(data,3)

           if (se_myworker_pe .eq. sourcepe) then

                if (sdim .eq. 1) then
                 li = from
                 ui = from
                 else if (sdim .eq. 2) then
                 lj = from
                 uj = from
              else
                 lk = from
                 uk = from
              end if

              scount = 0
C -- pack data for sending
              do k = lk, uk
                 do j = lj, uj
                    do i = li, ui
                       scount = scount + 1
                       sarray(scount) = data(i,j,k)
                    end do
                 end do
              end do

              call mpi_send (sarray, scount, mpi_integer, destpe,
     &                       sourcepe, se_worker_comm, error)

           else if (se_myworker_pe .eq. destpe) then

                if (sdim .eq. 1) then
                 li = to
                 ui = to
                else if (sdim .eq. 2) then
                 lj = to
                 uj = to
              else
                 lk = to
                 uk = to
              end if

C -- receive data from corresponding processor

              rcount = (ui - li + 1) * (uj - lj + 1) * (uk - lk + 1)

              call mpi_recv (rarray, rcount, mpi_integer, sourcepe,
     &                       sourcepe, se_worker_comm, status, error)

C -- unpack received data
              rcount = 0
              do k = lk, uk
                 do j = lj, uj
                    do i = li, ui
                       rcount = rcount + 1
                       data(i,j,k) = rarray(rcount) 
                    end do
                 end do
              end do

           end if
        end if

        return
        end subroutine se_slice3i

C --------------------------------------------------------------------------
C Purpose:
C
C   to perform transferring a slice of a 3-D data 
C
C Revision history:
C
C   Orginal version: 5/26/99 by David Wong 
C                    11/05/99 by David Wong
C                      -- recode the code using F90 syntax
C                    12/04/02 by David Wong
C                       -- modified the routine to accommodate worker and
C                          I/O processors partition scheme
C
C Subroutine parameter description:
C
C   In:  data     -- original data
C        sourcepe -- source PE
C        destpe   -- target PE
C        sdim     -- slicing dimension
C        from     -- index of the slicing source
C        to       -- index of the slicing destination
C
C   Out: data     -- original data after communication
C
C Local variable description:
C
C    i, j, k    -- loop indexes
C    li, ui     -- local low and upper index of i dimension
C    lj, uj     -- local low and upper index of j dimension
C    lk, uk     -- local low and upper index of k dimension
C    status     -- return status of MPI_RECEIVING call
C    error      -- error code of invoking MPI calls
C    scount     -- number of items need to be sent
C    rcount     -- number of items are expected to receive
C    sarray     -- array to hold sending data
C    rarray     -- array to hold receiving data
C
C Include file:
C
C    se_pe_info.ext
C
C Subroutine/Function call:
C
C   mpi_send
C   mpi_recv
C
C --------------------------------------------------------------------------

        subroutine se_slice3r (data, sourcepe, destpe, sdim, from, to)

        use se_pe_info_ext

        implicit none

        real, intent(inout) :: data(:,:,:)
        integer, intent(in) :: sourcepe, destpe, sdim, from, to

        include "mpif.h"
        
        integer :: error
        integer :: i, j, k, li, lj, lk, ui, uj, uk
        integer :: status(MPI_STATUS_SIZE)
        integer :: scount, rcount
        real :: sarray(size(data)), rarray(size(data))

        if ((sourcepe .ge. 0) .and. (destpe .ge. 0) .and.
     &      (sourcepe .ne. destpe)) then

C -- send data to corresponding processor 

           li = lbound(data,1)
           ui = ubound(data,1)
           lj = lbound(data,2)
           uj = ubound(data,2)
           lk = lbound(data,3)
           uk = ubound(data,3)

           if (se_myworker_pe .eq. sourcepe) then

                if (sdim .eq. 1) then
                 li = from
                 ui = from
                 else if (sdim .eq. 2) then
                 lj = from
                 uj = from
              else
                 lk = from
                 uk = from
              end if

              scount = 0
C -- pack data for sending
              do k = lk, uk
                 do j = lj, uj
                    do i = li, ui
                       scount = scount + 1
                       sarray(scount) = data(i,j,k)
                    end do
                 end do
              end do

              call mpi_send (sarray, scount, mpi_real, destpe,
     &                       sourcepe, se_worker_comm, error)

           else if (se_myworker_pe .eq. destpe) then

                if (sdim .eq. 1) then
                 li = to
                 ui = to
                else if (sdim .eq. 2) then
                 lj = to
                 uj = to
              else
                 lk = to
                 uk = to
              end if

C -- receive data from corresponding processor

              rcount = (ui - li + 1) * (uj - lj + 1) * (uk - lk + 1)

              call mpi_recv (rarray, rcount, mpi_real, sourcepe,
     &                       sourcepe, se_worker_comm, status, error)

C -- unpack received data
              rcount = 0
              do k = lk, uk
                 do j = lj, uj
                    do i = li, ui
                       rcount = rcount + 1
                       data(i,j,k) = rarray(rcount) 
                    end do
                 end do
              end do

           end if
        end if

        return
        end subroutine se_slice3r

C --------------------------------------------------------------------------
C Purpose:
C
C   transfer a slice of a 4-D integer array
C
C Revision history:
C
C   Orginal version: 5/26/99 by David Wong 
C                    11/05/99 by David Wong
C                      -- recode the code using F90 syntax
C                    12/16/00 by Jeff Young
C                      -- mod for integer data
C                    12/04/02 by David Wong
C                       -- modified the routine to accommodate worker and
C                          I/O processors partition scheme
C
C Subroutine parameter description:
C
C   In:  data     -- original data
C        sourcepe -- source PE
C        destpe   -- target PE
C        sdim     -- slicing dimension
C        from     -- index of the slicing source
C        to       -- index of the slicing destination
C
C   Out: data     -- original data after communication
C
C Local variable description:
C
C    i, j, k, l -- loop indexes
C    li, ui     -- local low and upper index of i dimension
C    lj, uj     -- local low and upper index of j dimension
C    lk, uk     -- local low and upper index of k dimension
C    ll, ul     -- local low and upper index of l dimension
C    status     -- return status of MPI_RECEIVING call
C    error      -- error code of invoking MPI calls
C    scount     -- number of items need to be sent
C    rcount     -- number of items are expected to receive
C    sarray     -- array to hold sending data
C    rarray     -- array to hold receiving data
C
C Include file:
C
C    se_pe_info.ext
C
C Subroutine/Function call:
C
C   mpi_send
C   mpi_recv
C
C --------------------------------------------------------------------------

        subroutine se_slice4i (data, sourcepe, destpe, sdim, from, to)

        use se_pe_info_ext

        implicit none

        include "mpif.h"
        
        integer, intent(inout) :: data(:,:,:,:)
        integer, intent(in) :: sourcepe, destpe, sdim, from, to

        integer :: error
        integer :: i, j, k, l, li, lj, lk, ll, ui, uj, uk, ul
        integer :: status(MPI_STATUS_SIZE)
        integer :: scount, rcount
        integer :: sarray(size(data)), rarray(size(data))

        if ((sourcepe .ge. 0) .and. (destpe .ge. 0) .and.
     &      (sourcepe .ne. destpe)) then

C -- send data to corresponding processor 

           li = lbound(data,1)
           ui = ubound(data,1)
           lj = lbound(data,2)
           uj = ubound(data,2)
           lk = lbound(data,3)
           uk = ubound(data,3)
           ll = lbound(data,4)
           ul = ubound(data,4)

           if (se_myworker_pe .eq. sourcepe) then

                  if (sdim .eq. 1) then
                 li = from
                 ui = from
                else if (sdim .eq. 2) then
                 lj = from
                 uj = from
                else if (sdim .eq. 3) then
                 lk = from
                 uk = from
              else
                 ll = from
                 ul = from
              end if

              scount = 0
C -- pack data for sending
              do l = ll, ul
                 do k = lk, uk
                    do j = lj, uj
                       do i = li, ui
                          scount = scount + 1
                          sarray(scount) = data(i,j,k,l)
                       end do
                    end do
                 end do
              end do

              call mpi_send (sarray, scount, mpi_integer, destpe,
     &                       sourcepe, se_worker_comm, error)

           else if (se_myworker_pe .eq. destpe) then

                if (sdim .eq. 1) then
                 li = to
                 ui = to
                else if (sdim .eq. 2) then
                 lj = to
                 uj = to
                else if (sdim .eq. 3) then
                 lk = to
                 uk = to
              else
                 ll = to
                 ul = to
              end if

C -- receive data from corresponding processor

              rcount =   (ui - li + 1) * (uj - lj + 1)
     &                 * (uk - lk + 1) * (ul - ll + 1) 

              call mpi_recv (rarray, rcount, mpi_integer, sourcepe,
     &                       sourcepe, se_worker_comm, status, error)

C -- unpack received data
              rcount = 0
              do l = ll, ul
                 do k = lk, uk
                    do j = lj, uj
                       do i = li, ui
                          rcount = rcount + 1
                          data(i,j,k,l) = rarray(rcount) 
                       end do
                    end do
                 end do
              end do

           end if
        end if

        return
        end subroutine se_slice4i

C --------------------------------------------------------------------------
C Purpose:
C
C   to perform transferring a slice of a 4-D real data 
C
C Revision history:
C
C   Orginal version: 5/26/99 by David Wong 
C                    11/05/99 by David Wong
C                      -- recode the code using F90 syntax
C                    12/04/02 by David Wong
C                       -- modified the routine to accommodate worker and
C                          I/O processors partition scheme
C
C Subroutine parameter description:
C
C   In:  data     -- original data
C        sourcepe -- source PE
C        destpe   -- target PE
C        sdim     -- slicing dimension
C        from     -- index of the slicing source
C        to       -- index of the slicing destination
C
C   Out: data     -- original data after communication
C
C Local variable description:
C
C    i, j, k, l -- loop indexes
C    li, ui     -- local low and upper index of i dimension
C    lj, uj     -- local low and upper index of j dimension
C    lk, uk     -- local low and upper index of k dimension
C    ll, ul     -- local low and upper index of l dimension
C    status     -- return status of MPI_RECEIVING call
C    error      -- error code of invoking MPI calls
C    scount     -- number of items need to be sent
C    rcount     -- number of items are expected to receive
C    sarray     -- array to hold sending data
C    rarray     -- array to hold receiving data
C
C Include file:
C
C    se_pe_info.ext
C
C Subroutine/Function call:
C
C   mpi_send
C   mpi_recv
C
C --------------------------------------------------------------------------

        subroutine se_slice4r (data, sourcepe, destpe, sdim, from, to)

        use se_pe_info_ext

        implicit none

        include "mpif.h"
        
        real, intent(inout) :: data(:,:,:,:)
        integer, intent(in) :: sourcepe, destpe, sdim, from, to

        integer :: error
        integer :: i, j, k, l, li, lj, lk, ll, ui, uj, uk, ul
        integer :: status(MPI_STATUS_SIZE)
        integer :: scount, rcount
        real :: sarray(size(data)), rarray(size(data))

        if ((sourcepe .ge. 0) .and. (destpe .ge. 0) .and.
     &      (sourcepe .ne. destpe)) then

C -- send data to corresponding processor 

           li = lbound(data,1)
           ui = ubound(data,1)
           lj = lbound(data,2)
           uj = ubound(data,2)
           lk = lbound(data,3)
           uk = ubound(data,3)
           ll = lbound(data,4)
           ul = ubound(data,4)

           if (se_myworker_pe .eq. sourcepe) then

                  if (sdim .eq. 1) then
                 li = from
                 ui = from
                else if (sdim .eq. 2) then
                 lj = from
                 uj = from
                else if (sdim .eq. 3) then
                 lk = from
                 uk = from
              else
                 ll = from
                 ul = from
              end if

              scount = 0
C -- pack data for sending
              do l = ll, ul
                 do k = lk, uk
                    do j = lj, uj
                       do i = li, ui
                          scount = scount + 1
                          sarray(scount) = data(i,j,k,l)
                       end do
                    end do
                 end do
              end do

              call mpi_send (sarray, scount, mpi_real, destpe,
     &                       sourcepe, se_worker_comm, error)

           else if (se_myworker_pe .eq. destpe) then

                if (sdim .eq. 1) then
                 li = to
                 ui = to
                else if (sdim .eq. 2) then
                 lj = to
                 uj = to
                else if (sdim .eq. 3) then
                 lk = to
                 uk = to
              else
                 ll = to
                 ul = to
              end if

C -- receive data from corresponding processor

              rcount =   (ui - li + 1) * (uj - lj + 1)
     &                 * (uk - lk + 1) * (ul - ll + 1) 

              call mpi_recv (rarray, rcount, mpi_real, sourcepe,
     &                       sourcepe, se_worker_comm, status, error)

C -- unpack received data
              rcount = 0
              do l = ll, ul
                 do k = lk, uk
                    do j = lj, uj
                       do i = li, ui
                          rcount = rcount + 1
                          data(i,j,k,l) = rarray(rcount) 
                       end do
                    end do
                 end do
              end do

           end if
        end if

        return
        end subroutine se_slice4r

        end module se_slice_module
