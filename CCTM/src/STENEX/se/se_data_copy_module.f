
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
C $Header: /project/work/rep/STENEX/src/se_snl/se_data_copy_module.f,v 1.2 2006/02/15 14:41:56 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C --------------------------------------------------------------------------
C Purpose:
C
C   use F90 interface feature to achieve "faked" polymorphism for data
C   copy routine which performs interior sub-grid data communication (data 
C   re-distribution)
C
C Revision history:
C
C   Orginal version: 11/05/99 by David Wong
C
C                    10/09/00 by David Wong
C                      -- unified fsg and csg data structures into one,
C                         subgrid
C
C                    07/23/01 by David Wong
C                      -- redesign the message sending and receiving algorithm
C
C                    03/06/02 David Wong
C                      -- use blocking communication scheme
C                      -- use array copy mechanism when communicates to itself
C
C                    12/04/02 by David Wong
C                       -- modified the routine to accommodate worker and
C                          I/O processors partition scheme
C
C                    01/04/23 by David Wong
C                       -- modified routines to handle any level range setting
C                          defined by PA_BLEV PA_ELEV in the run script properly
C
C                    01/17/24 by David Wong
C                       -- fixed a bug that IRR/IPR process alllows a subset of
C                          level, a..b, where 1 <= a <= b <= NLAYS
C
C Note:
C
C   se_[n]d[e]_data_copy where [n] denotes the dimensionality of the data
C   and [e] is optional, indicates the first two dimensions are both decomposed
C   or not
C
C Subroutine parameter description:
C
C   In:  data1     -- original data
C        spc       -- (use as needed) a specific species in the original data
C        des       -- (use as needed) species number in the recipient data
C        grid_type -- optional input, to indicate the data is from coarse
C                     grid 'c', 'C' or not
C
C   Out: data2   -- recipient of original data
C
C Local variable description:
C
C    sdir, rdir -- loop indexes which indicate send to or recvd from
C    tag        -- message tag
C    i          -- local loop index
C
C Include file:
C
C   se_pe_info_ext
C   se_data_send_module
C   se_data_recv_module
C
C --------------------------------------------------------------------------

        module se_data_copy_module

        implicit none

        interface se_data_copy
          module procedure se_2d_data_copy, se_2de_data_copy, 
     &                     se_3d_data_copy, se_3de_data_copy, 
     &                     se_4d_data_copy, se_4de_data_copy
        end interface

        contains

C --------------------------------------------------------------------------
        subroutine se_2d_data_copy (data1, data2)

        use se_subgrid_info_ext
        use se_pe_info_ext
        use se_data_send_module
        use se_data_recv_module

        implicit none

        include "mpif.h"

        real, intent(in) :: data1(:, :)
        real, intent(out) :: data2(:, :)

        integer :: sdir, rdir, tag
        integer :: request, status(MPI_STATUS_SIZE), error

        do sdir = 0, se_numworkers-1

           if (se_subgrid_send(sdir) .eq. se_myworker_pe) then

              data2(se_subgrid_recv_ind(1,1,sdir):se_subgrid_recv_ind(2,1,sdir),
     $              se_subgrid_recv_ind(1,2,sdir):se_subgrid_recv_ind(2,2,sdir))
     $        =
     $        data1(se_subgrid_send_ind(1,1,sdir):se_subgrid_send_ind(2,1,sdir),
     $              se_subgrid_send_ind(1,2,sdir):se_subgrid_send_ind(2,2,sdir))

           else

              if (se_subgrid_send(sdir) .ge. 0) then
                 tag = sdir+se_myworker_pe
                 call se_data_send (data1, se_subgrid_send_ind_ptr, 
     &                              se_subgrid_send_ptr, sdir, tag, request)
              end if

              if ((se_subgrid_recv(sdir) .ge. 0) .and.
     &            (se_subgrid_recv(sdir) .ne. se_myworker_pe)) then
                 tag = sdir+se_myworker_pe
                 call se_data_recv (data2, se_subgrid_recv_ind_ptr, 
     &                              se_subgrid_recv_ptr, sdir, tag)
              end if

c             if (se_subgrid_send(sdir) .ge. 0) then
c                call mpi_wait (request, status, error)
c             end if

           end if
        end do

        return
        end subroutine se_2d_data_copy

C --------------------------------------------------------------------------
        subroutine se_2de_data_copy (data1, data2, spc)

        use se_subgrid_info_ext
        use se_pe_info_ext
        use se_data_send_module
        use se_data_recv_module

        implicit none

        include "mpif.h"

        real, intent(in) :: data1(:, :, :)
        real, intent(out) :: data2(:, :)
        integer, intent(in) :: spc

        integer :: i, sdir, rdir, tag
        integer :: request, status(MPI_STATUS_SIZE), error

        do i = 0, se_numworkers-1
           se_subgrid_send_ind(1,3,i) = spc
           se_subgrid_send_ind(2,3,i) = spc
        end do

        do sdir = 0, se_numworkers-1

           if (se_subgrid_send(sdir) .eq. se_myworker_pe) then

              data2(se_subgrid_recv_ind(1,1,sdir):se_subgrid_recv_ind(2,1,sdir),
     $              se_subgrid_recv_ind(1,2,sdir):se_subgrid_recv_ind(2,2,sdir))
     $        =
     $        data1(se_subgrid_send_ind(1,1,sdir):se_subgrid_send_ind(2,1,sdir),
     $              se_subgrid_send_ind(1,2,sdir):se_subgrid_send_ind(2,2,sdir),
     $              se_subgrid_send_ind(1,3,sdir))

           else

              if (se_subgrid_send(sdir) .ge. 0) then
                 tag = sdir+se_myworker_pe
                 call se_data_send (data1, se_subgrid_send_ind_ptr, 
     &                              se_subgrid_send_ptr, sdir, tag, request)
              end if

              if ((se_subgrid_recv(sdir) .ge. 0) .and.
     &            (se_subgrid_recv(sdir) .ne. se_myworker_pe)) then
                 tag = sdir+se_myworker_pe
                 call se_data_recv (data2, se_subgrid_recv_ind_ptr, 
     &                              se_subgrid_recv_ptr, sdir, tag)
              end if

c             if (se_subgrid_send(sdir) .ge. 0) then
c                call mpi_wait (request, status, error)
c             end if

           end if
        end do

        return
        end subroutine se_2de_data_copy

C --------------------------------------------------------------------------
        subroutine se_3d_data_copy (data1, data2)

        use se_subgrid_info_ext
        use se_domain_info_ext
        use se_pe_info_ext
        use se_data_send_module
        use se_data_recv_module

        implicit none

        include "mpif.h"

        real, intent(in) :: data1(:, :, :)
        real, intent(out) :: data2(:, :, :)

        integer :: i, sdir, rdir, tag
        integer :: request, status(MPI_STATUS_SIZE), error

        do i = 0, se_numworkers-1
           se_subgrid_send_ind(1,3,i) = se_my_subgrid_beglev
           se_subgrid_send_ind(2,3,i) = se_my_subgrid_endlev
           se_subgrid_recv_ind(1,3,i) = 1
           se_subgrid_recv_ind(2,3,i) = se_my_subgrid_endlev - se_my_subgrid_beglev + 1
        end do

        do sdir = 0, se_numworkers-1

           if (se_subgrid_send(sdir) .eq. se_myworker_pe) then

              data2(se_subgrid_recv_ind(1,1,sdir):se_subgrid_recv_ind(2,1,sdir),
     &              se_subgrid_recv_ind(1,2,sdir):se_subgrid_recv_ind(2,2,sdir),
     &              se_subgrid_recv_ind(1,3,sdir):se_subgrid_recv_ind(2,3,sdir))
     &        =
     &        data1(se_subgrid_send_ind(1,1,sdir):se_subgrid_send_ind(2,1,sdir),
     &              se_subgrid_send_ind(1,2,sdir):se_subgrid_send_ind(2,2,sdir),
     &              se_subgrid_send_ind(1,3,sdir):se_subgrid_send_ind(2,3,sdir))

           else

              if (se_subgrid_send(sdir) .ge. 0) then
                 tag = sdir+se_myworker_pe
                 call se_data_send (data1, se_subgrid_send_ind_ptr,
     &                              se_subgrid_send_ptr, sdir, tag, request)
              end if

              if ((se_subgrid_recv(sdir) .ge. 0) .and.
     &            (se_subgrid_recv(sdir) .ne. se_myworker_pe)) then
                 tag = sdir+se_myworker_pe
                 call se_data_recv (data2, se_subgrid_recv_ind_ptr,
     &                              se_subgrid_recv_ptr, sdir, tag)
              end if

c             if (se_subgrid_send(sdir) .ge. 0) then
c                call mpi_wait (request, status, error)
c             end if

           end if

        end do

        return
        end subroutine se_3d_data_copy

C --------------------------------------------------------------------------
        subroutine se_3de_data_copy (data1, data2, spc, flag)

        use se_subgrid_info_ext
        use se_pe_info_ext
        use se_data_send_module
        use se_data_recv_module

        implicit none

        include "mpif.h"

        real, intent(in) :: data1(:, :, :, :)
        real, intent(out) :: data2(:, :, :)
        integer, intent(in) :: spc
        integer, intent(in), optional :: flag

        integer :: i, sdir, rdir, tag, loc_beglev, loc_endlev
        integer :: request, status(MPI_STATUS_SIZE), error

        if (present(flag)) then
           ! this indicates data1 is in subset level configuration and
           ! indiex starting from 1
           loc_beglev = 1
           loc_endlev = se_my_subgrid_endlev - se_my_subgrid_beglev + 1
        else
           loc_beglev = se_my_subgrid_beglev
           loc_endlev = se_my_subgrid_endlev
        end if

        do i = 0, se_numworkers-1
           se_subgrid_send_ind(1,3,i) = loc_beglev
           se_subgrid_send_ind(2,3,i) = loc_endlev
           se_subgrid_send_ind(1,4,i) = spc
           se_subgrid_send_ind(2,4,i) = spc

           se_subgrid_recv_ind(1,3,i) = 1
           se_subgrid_recv_ind(2,3,i) = se_my_subgrid_endlev - se_my_subgrid_beglev + 1
        end do

        do sdir = 0, se_numworkers-1

           if (se_subgrid_send(sdir) .eq. se_myworker_pe) then

              data2(se_subgrid_recv_ind(1,1,sdir):se_subgrid_recv_ind(2,1,sdir),
     &              se_subgrid_recv_ind(1,2,sdir):se_subgrid_recv_ind(2,2,sdir),
     &              se_subgrid_recv_ind(1,3,sdir):se_subgrid_recv_ind(2,3,sdir))
     &        =
     &        data1(se_subgrid_send_ind(1,1,sdir):se_subgrid_send_ind(2,1,sdir),
     &              se_subgrid_send_ind(1,2,sdir):se_subgrid_send_ind(2,2,sdir),
     &              se_subgrid_send_ind(1,3,sdir):se_subgrid_send_ind(2,3,sdir),
     &              se_subgrid_send_ind(1,4,sdir))

           else

              if (se_subgrid_send(sdir) .ge. 0) then
                 tag = sdir+se_myworker_pe
                 call se_data_send (data1, se_subgrid_send_ind_ptr, 
     &                              se_subgrid_send_ptr, sdir, tag, request)
              end if

              if ((se_subgrid_recv(sdir) .ge. 0) .and.
     &            (se_subgrid_recv(sdir) .ne. se_myworker_pe)) then
                 tag = sdir+se_myworker_pe
                 call se_data_recv (data2, se_subgrid_recv_ind_ptr, 
     &                              se_subgrid_recv_ptr, sdir, tag)
              end if

c             if (se_subgrid_send(sdir) .ge. 0) then
c                call mpi_wait (request, status, error)
c             end if

           end if
        end do

        return
        end subroutine se_3de_data_copy

C --------------------------------------------------------------------------
        subroutine se_4d_data_copy (data1, data2)

        use se_subgrid_info_ext
        use se_pe_info_ext
        use se_domain_info_ext
        use se_data_send_module
        use se_data_recv_module

        implicit none

        include "mpif.h"

        real, intent(in) :: data1(:, :, :, :)
        real, intent(out) :: data2(:, :, :, :)

        integer :: i, sdir, rdir, tag
        integer :: request, status(MPI_STATUS_SIZE), error

        do i = 0, se_numworkers-1
           se_subgrid_send_ind(1,3,i) = 1
           se_subgrid_send_ind(2,3,i) = se_my_nlays
           se_subgrid_send_ind(1,4,i) = 1
           se_subgrid_send_ind(2,4,i) = se_my_nspcs
           se_subgrid_recv_ind(1,3,i) = 1
           se_subgrid_recv_ind(2,3,i) = se_my_nlays
           se_subgrid_recv_ind(1,4,i) = 1
           se_subgrid_recv_ind(2,4,i) = se_my_nspcs
        end do

        do sdir = 0, se_numworkers-1

           if (se_subgrid_send(sdir) .eq. se_myworker_pe) then

              data2(se_subgrid_recv_ind(1,1,sdir):se_subgrid_recv_ind(2,1,sdir),
     &              se_subgrid_recv_ind(1,2,sdir):se_subgrid_recv_ind(2,2,sdir),
     &              se_subgrid_recv_ind(1,3,sdir):se_subgrid_recv_ind(2,3,sdir),
     &              se_subgrid_recv_ind(1,4,sdir):se_subgrid_recv_ind(2,4,sdir))
     &        =
     &        data1(se_subgrid_send_ind(1,1,sdir):se_subgrid_send_ind(2,1,sdir),
     &              se_subgrid_send_ind(1,2,sdir):se_subgrid_send_ind(2,2,sdir),
     &              se_subgrid_send_ind(1,3,sdir):se_subgrid_send_ind(2,3,sdir),
     &              se_subgrid_send_ind(1,4,sdir):se_subgrid_send_ind(2,4,sdir))

           else

              if (se_subgrid_send(sdir) .ge. 0) then
                 tag = sdir+se_myworker_pe
                 call se_data_send (data1, se_subgrid_send_ind_ptr,
     &                              se_subgrid_send_ptr, sdir, tag, request)
              end if

              if ((se_subgrid_recv(sdir) .ge. 0) .and.
     &            (se_subgrid_recv(sdir) .ne. se_myworker_pe)) then
                 tag = sdir+se_myworker_pe
                 call se_data_recv (data2, se_subgrid_recv_ind_ptr,
     &                              se_subgrid_recv_ptr, sdir, tag)
              end if

c             if (se_subgrid_send .ge. 0) then
c                call mpi_wait (request, status, error)
c             end if

           end if
        end do

        return
        end subroutine se_4d_data_copy

C --------------------------------------------------------------------------
        subroutine se_4de_data_copy (data1, data2, spc, des)

        use se_subgrid_info_ext
        use se_pe_info_ext
        use se_data_send_module
        use se_data_recv_module

        implicit none

        include "mpif.h"

        real, intent(in) :: data1(:, :, :, :)
        real, intent(out) :: data2(:, :, :, :)
        integer, intent(in) :: spc, des

        integer :: i, sdir, rdir, tag
        integer :: request, status(MPI_STATUS_SIZE), error

        do i = 0, se_numworkers-1
           se_subgrid_send_ind(1,3,i) = se_my_subgrid_beglev
           se_subgrid_send_ind(2,3,i) = se_my_subgrid_endlev
           se_subgrid_send_ind(1,4,i) = spc
           se_subgrid_send_ind(2,4,i) = spc

           se_subgrid_recv_ind(1,3,i) = 1
           se_subgrid_recv_ind(2,3,i) = se_my_subgrid_endlev - se_my_subgrid_beglev + 1
           se_subgrid_recv_ind(1,4,i) = des
           se_subgrid_recv_ind(2,4,i) = des
        end do

        do sdir = 0, se_numworkers-1

           if (se_subgrid_send(sdir) .eq. se_myworker_pe) then

              data2(se_subgrid_recv_ind(1,1,sdir):se_subgrid_recv_ind(2,1,sdir),
     &              se_subgrid_recv_ind(1,2,sdir):se_subgrid_recv_ind(2,2,sdir),
     &              se_subgrid_recv_ind(1,3,sdir):se_subgrid_recv_ind(2,3,sdir),
     &              se_subgrid_recv_ind(1,4,sdir))
     &        =
     &        data1(se_subgrid_send_ind(1,1,sdir):se_subgrid_send_ind(2,1,sdir),
     &              se_subgrid_send_ind(1,2,sdir):se_subgrid_send_ind(2,2,sdir),
     &              se_subgrid_send_ind(1,3,sdir):se_subgrid_send_ind(2,3,sdir),
     &              se_subgrid_send_ind(1,4,sdir))

           else

              if (se_subgrid_send(sdir) .ge. 0) then
                 tag = sdir+se_myworker_pe
                 call se_data_send (data1, se_subgrid_send_ind_ptr, 
     &                              se_subgrid_send_ptr, sdir, tag, request)
              end if

              if ((se_subgrid_recv(sdir) .ge. 0) .and.
     &            (se_subgrid_recv(sdir) .ne. se_myworker_pe)) then
                 tag = sdir+se_myworker_pe
                 call se_data_recv (data2, se_subgrid_recv_ind_ptr, 
     &                              se_subgrid_recv_ptr, sdir, tag)
              end if

c             if (se_subgrid_send(sdir) .ge. 0) then
c                call mpi_wait (request, status, error)
c             end if

           end if
        end do

        return
        end subroutine se_4de_data_copy

        end module se_data_copy_module
