
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

! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header: /project/work/rep/STENEX/src/se_snl/se_data_send_module.f,v 1.2 2006/02/15 14:41:56 yoj Exp $

! what(1) key, module and SID; SCCS file; date and time of last delta:
! %W% %P% %G% %U%

! --------------------------------------------------------------------------
! Purpose:
!
!   use F90 interface feature to achieve "faked" polymorphism for data
!   sending routine
!
! Revision history:
!
!   Orginal version: 11/05/99 by David Wong
!                    07/23/01 by David Wong
!                      -- use mpi_isend rather than mpi_send to send messages
!                    03/06/02 David Wong
!                      -- use blocking communication scheme due to non-blocking
!                         timing problems on IBM SP
!                    12/04/02 by David Wong
!                       -- modified the routine to accommodate worker and
!                          I/O processors partition scheme
!                    10/01/12 by David Wong
!                       -- reduced memory usage
!                       -- extended to five dimensional array
!                       -- used block sending mechanism
!
! Note:
!
!   se_[n]d[e]_data_send where [n] denotes the dimensionality of the data
!   and [e] is optional, indicates the first two dimensions are not both
!   decomposed
!
! Subroutine parameter description:
!
!   In:    sind    -- stores low and high index of each dimension for
!                     sending process
!          send_to -- stores processor number which data is sent to
!          dir_ind -- one of those eight major communication directions
!          tag     -- message tag
!          data    -- variable that sends data to other processors
! --------------------------------------------------------------------------

        module se_data_send_module

          use se_pe_info_ext

          implicit none

          interface se_data_send
            module procedure se_1d_data_send,
     &                       se_2d_data_send, se_2de_data_send,
     &                       se_3d_data_send, se_3de_data_send,
     &                       se_4d_data_send, se_4de_data_send,
     &                       se_5d_data_send
          end interface

          contains

! --------------------------------------------------------------------------
        subroutine se_1d_data_send (data, sind, send_to, dir_ind, tag, request)

        implicit none
        include "mpif.h"

        real, intent(in) :: data(:)
        integer, pointer :: sind(:, :), send_to(:)
        integer, intent(in) :: dir_ind, tag
        integer, intent(out) :: request

        integer :: i, scount, error
!       real :: sarray(size(data))
        real, allocatable :: sarray(:)

        allocate (sarray(sind(2,dir_ind) - sind(1,dir_ind) + 1), stat=error)

! -- pack data for sending
        scount = 0
        do i = sind(1,dir_ind), sind(2,dir_ind)
           scount = scount + 1
           sarray(scount) = data(i)
        end do

!       call mpi_isend (sarray, scount, mpi_real, send_to(dir_ind),
!    &                  tag, se_worker_comm, request, error)
        call mpi_send (sarray, scount, mpi_real, send_to(dir_ind),
     &                  tag, se_worker_comm, error)

        deallocate (sarray)

        end subroutine se_1d_data_send

! -----------------------------------------------------------------------------
        subroutine se_2d_data_send (data, sind, send_to, dir_ind, tag, request)

        implicit none
        include "mpif.h"

        real, intent(in) :: data(:,:)
        integer, pointer :: sind(:, :, :), send_to(:)
        integer, intent(in) :: dir_ind, tag
        integer, intent(out) :: request

        integer :: i, j, scount, error
!       real :: sarray(size(data))
        real, allocatable :: sarray(:)

        allocate (sarray( (sind(2,1,dir_ind) - sind(1,1,dir_ind) + 1) *
     $                    (sind(2,2,dir_ind) - sind(1,2,dir_ind) + 1)), stat=error)

        scount = 0

! -- pack data for sending
        do j = sind(1,2,dir_ind), sind(2,2,dir_ind)
           do i = sind(1,1,dir_ind), sind(2,1,dir_ind)
              scount = scount + 1
              sarray(scount) = data(i,j)
           end do
        end do

!       call mpi_isend (sarray, scount, mpi_real, send_to(dir_ind),
!    &                  tag, se_worker_comm, request, error)
        call mpi_send (sarray, scount, mpi_real, send_to(dir_ind),
     &                  tag, se_worker_comm, error)

        deallocate (sarray)

        end subroutine se_2d_data_send

! -----------------------------------------------------------------------------
        subroutine se_2de_data_send (data, sind, send_to, dir_ind, tag, request)

        implicit none
        include "mpif.h"

        real, intent(in) :: data(:,:)
        integer, pointer :: sind(:, :), send_to(:)
        integer, intent(in) :: dir_ind, tag
        integer, intent(out) :: request

        integer :: i, j, scount, error
!       real :: sarray(size(data))
        real, allocatable :: sarray(:)

        allocate (sarray( (sind(2,dir_ind) - sind(1,dir_ind) + 1) *
     $                    size(data, 2)), stat=error)

        scount = 0

! -- pack data for sending
        do j = lbound(data,2), ubound(data,2)
           do i = sind(1,dir_ind), sind(2,dir_ind)
              scount = scount + 1
              sarray(scount) = data(i,j)
           end do
        end do

!       call mpi_isend (sarray, scount, mpi_real, send_to(dir_ind),
!    &                  tag, se_worker_comm, request, error)
        call mpi_send (sarray, scount, mpi_real, send_to(dir_ind),
     &                  tag, se_worker_comm, error)

        deallocate (sarray)

        end subroutine se_2de_data_send

! -----------------------------------------------------------------------------
        subroutine se_3d_data_send (data, sind, send_to, dir_ind, tag, request)

        implicit none
        include "mpif.h"

        real, intent(in) :: data(:,:,:)
        integer, pointer :: sind(:, :, :), send_to(:)
        integer, intent(in) :: dir_ind, tag
        integer, intent(out) :: request

        integer :: i, j, k, scount, error
!       real :: sarray(size(data))
        real, allocatable :: sarray(:)

        allocate (sarray( (sind(2,1,dir_ind) - sind(1,1,dir_ind) + 1) *
     $                    (sind(2,2,dir_ind) - sind(1,2,dir_ind) + 1) *
     $                    (sind(2,3,dir_ind) - sind(1,3,dir_ind) + 1)), stat=error)

        scount = 0

! -- pack data for sending
        do k = sind(1,3,dir_ind), sind(2,3,dir_ind)
           do j = sind(1,2,dir_ind), sind(2,2,dir_ind)
              do i = sind(1,1,dir_ind), sind(2,1,dir_ind)
                 scount = scount + 1
                 sarray(scount) = data(i,j,k)
              end do
           end do
        end do

!       call mpi_isend (sarray, scount, mpi_real, send_to(dir_ind),
!    &                  tag, se_worker_comm, request, error)
        call mpi_send (sarray, scount, mpi_real, send_to(dir_ind),
     &                  tag, se_worker_comm, error)

        deallocate (sarray)

        end subroutine se_3d_data_send

! -----------------------------------------------------------------------------
        subroutine se_3de_data_send (data, sind, send_to, dir_ind, tag, request)

        implicit none
        include "mpif.h"

        real, intent(in) :: data(:,:,:)
        integer, pointer :: sind(:, :), send_to(:)
        integer, intent(in) :: dir_ind, tag
        integer, intent(out) :: request

        integer :: i, j, k, scount, error
!       real :: sarray(size(data))
        real, allocatable :: sarray(:)

        allocate (sarray( (sind(2,dir_ind) - sind(1,dir_ind) + 1) *
     $                    size(data,2) * size(data,3)), stat=error)

        scount = 0

! -- pack data for sending
        do k = lbound(data,3), ubound(data,3)
           do j = lbound(data,2), ubound(data,2)
              do i = sind(1,dir_ind), sind(2,dir_ind)
                 scount = scount + 1
                 sarray(scount) = data(i,j,k)
              end do
           end do
        end do

!       call mpi_isend (sarray, scount, mpi_real, send_to(dir_ind),
!    &                  tag, se_worker_comm, request, error)
        call mpi_send (sarray, scount, mpi_real, send_to(dir_ind),
     &                  tag, se_worker_comm, error)

        deallocate (sarray)

        end subroutine se_3de_data_send

! -----------------------------------------------------------------------------
        subroutine se_4d_data_send (data, sind, send_to, dir_ind, tag, request)

        implicit none
        include "mpif.h"

        real, intent(in) :: data(:,:,:,:)
        integer, pointer :: sind(:, :, :), send_to(:)
        integer, intent(in) :: dir_ind, tag
        integer, intent(out) :: request

        integer :: i, j, k, l, scount, error
!       real :: sarray(size(data))
        real, allocatable :: sarray(:)

        scount = 0

        allocate (sarray( (sind(2,1,dir_ind) - sind(1,1,dir_ind) + 1) *
     $                    (sind(2,2,dir_ind) - sind(1,2,dir_ind) + 1) *
     $                    (sind(2,3,dir_ind) - sind(1,3,dir_ind) + 1) *
     $                    (sind(2,4,dir_ind) - sind(1,4,dir_ind) + 1)), stat=error)

! -- pack data for sending
        do l = sind(1,4,dir_ind), sind(2,4,dir_ind)
           do k = sind(1,3,dir_ind), sind(2,3,dir_ind)
              do j = sind(1,2,dir_ind), sind(2,2,dir_ind)
                 do i = sind(1,1,dir_ind), sind(2,1,dir_ind)
                    scount = scount + 1
                    sarray(scount) = data(i,j,k,l)
                 end do
              end do
           end do
        end do

!       call mpi_isend (sarray, scount, mpi_real, send_to(dir_ind),
!    &                  tag, se_worker_comm, request, error)
        call mpi_send (sarray, scount, mpi_real, send_to(dir_ind),
     &                  tag, se_worker_comm, error)

        deallocate (sarray)

        end subroutine se_4d_data_send

! -----------------------------------------------------------------------------
        subroutine se_4de_data_send (data, sind, send_to, dir_ind, tag, request)

        implicit none
        include "mpif.h"

        real, intent(in) :: data(:,:,:,:)
        integer, pointer :: sind(:, :), send_to(:)
        integer, intent(in) :: dir_ind, tag
        integer, intent(out) :: request

        integer :: i, j, k, s, scount, error
!       real :: sarray(size(data))
        real, allocatable :: sarray(:)

        allocate (sarray( (sind(2,dir_ind) - sind(1,dir_ind) + 1) *
     $                    size(data,2) * size(data,3) * size(data,4)), stat=error)

        scount = 0

! -- pack data for sending
        do s = lbound(data,4), ubound(data,4)
           do k = lbound(data,3), ubound(data,3)
              do j = lbound(data,2), ubound(data,2)
                 do i = sind(1,dir_ind), sind(2,dir_ind)
                    scount = scount + 1
                    sarray(scount) = data(i,j,k,s)
                 end do
              end do
           end do
        end do

!       call mpi_isend (sarray, scount, mpi_real, send_to(dir_ind),
!    &                  tag, se_worker_comm, request, error)
        call mpi_send (sarray, scount, mpi_real, send_to(dir_ind),
     &                  tag, se_worker_comm, error)

        deallocate (sarray)

        end subroutine se_4de_data_send

! -----------------------------------------------------------------------------
        subroutine se_5d_data_send (data, sind, send_to, dir_ind, tag, request)

        implicit none
        include "mpif.h"

        real, intent(in) :: data(:,:,:,:,:)
        integer, pointer :: sind(:, :, :), send_to(:)
        integer, intent(in) :: dir_ind, tag
        integer, intent(out) :: request

        integer :: i, j, k, l, m, scount, error
!       real :: sarray(size(data))
        real, allocatable :: sarray(:)

        allocate (sarray( (sind(2,1,dir_ind) - sind(1,1,dir_ind) + 1) *
     $                    (sind(2,2,dir_ind) - sind(1,2,dir_ind) + 1) *
     $                    (sind(2,3,dir_ind) - sind(1,3,dir_ind) + 1) *
     $                    (sind(2,4,dir_ind) - sind(1,4,dir_ind) + 1) *
     $                    (sind(2,5,dir_ind) - sind(1,5,dir_ind) + 1)), stat=error)

        scount = 0

! -- pack data for sending
        do m = sind(1,5,dir_ind), sind(2,5,dir_ind)
           do l = sind(1,4,dir_ind), sind(2,4,dir_ind)
              do k = sind(1,3,dir_ind), sind(2,3,dir_ind)
                 do j = sind(1,2,dir_ind), sind(2,2,dir_ind)
                    do i = sind(1,1,dir_ind), sind(2,1,dir_ind)
                       scount = scount + 1
                       sarray(scount) = data(i,j,k,l,m)
                    end do
                 end do
              end do
           end do
        end do

!       call mpi_isend (sarray, scount, mpi_real, send_to(dir_ind),
!    &                  tag, se_worker_comm, request, error)
        call mpi_send (sarray, scount, mpi_real, send_to(dir_ind),
     &                  tag, se_worker_comm, error)

        deallocate (sarray)

        end subroutine se_5d_data_send

        end module se_data_send_module
