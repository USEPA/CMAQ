
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
! $Header: /project/work/rep/STENEX/src/se_snl/se_data_recv_module.f,v 1.2 2006/02/15 14:41:56 yoj Exp $

! what(1) key, module and SID; SCCS file; date and time of last delta:
! %W% %P% %G% %U%

! --------------------------------------------------------------------------
! Purpose:
!
!   use F90 interface feature to achieve "faked" polymorphism for data 
!   receiving routine
!
! Revision history:
!
!   Orginal version: 11/05/99 by David Wong
!
!                    12/04/02 by David Wong
!                       -- modified the routine to accommodate worker and
!                          I/O processors partition scheme
!                    10/01/12 by David Wong
!                       -- reduced memory usage
!                       -- extended to five dimensional array
!
! Note:
!
!   se_[n]d[e]_data_recv where [n] denotes the dimensionality of the data 
!   and [e] is optional, indicates the first two dimensions are not both 
!   decomposed
!
! Subroutine parameter description:
!
!   In:    rind      -- stores low and high index of each dimension for 
!                       receiving process
!          recv_from -- stores processor number which data is received from
!          dir_ind   -- one of those eight major communication directions
!          tag       -- message tag
!
!   InOut: data      -- variable that receives data from other processors
! --------------------------------------------------------------------------

        module se_data_recv_module

          use se_pe_info_ext

          implicit none

          interface se_data_recv
            module procedure se_1d_data_recv,
     &                       se_2d_data_recv, se_2de_data_recv,
     &                       se_3d_data_recv, se_3de_data_recv,
     &                       se_4d_data_recv, se_4de_data_recv,
     &                       se_5d_data_recv
          end interface

          contains

! -----------------------------------------------------------------------------
        subroutine se_1d_data_recv (data, rind, recv_from, dir_ind, tag)

        implicit none
        include "mpif.h"

        real, intent(inout) :: data(:)
        integer, pointer :: rind(:, :), recv_from(:)
        integer, intent(in) :: dir_ind, tag

        integer :: i, rcount, error
        integer :: status(MPI_STATUS_SIZE)
!       real :: rarray(size(data))
        real, allocatable :: rarray(:)

! -- receive data from corresponding processor

        rcount = rind(2,dir_ind) - rind(1,dir_ind) + 1

        allocate (rarray(rcount), stat=error)

        call mpi_recv (rarray, rcount, mpi_real, recv_from(dir_ind),
     &                 tag, se_worker_comm, status, error)

! -- unpack received data
        rcount = 0
        do i = rind(1,dir_ind), rind(2,dir_ind)
           rcount = rcount + 1
           data(i) = rarray(rcount)
        end do

        deallocate (rarray)

        end subroutine se_1d_data_recv

! -----------------------------------------------------------------------------
        subroutine se_2d_data_recv (data, rind, recv_from, dir_ind, tag)

        implicit none
        include "mpif.h"

        real, intent(inout) :: data(:,:)
        integer, pointer :: rind(:, :, :), recv_from(:)
        integer, intent(in) :: dir_ind, tag

        integer :: i, j, rcount, error
        integer :: status(MPI_STATUS_SIZE)
!       real :: rarray(size(data))
        real, allocatable :: rarray(:)

        rcount = (rind(2,1,dir_ind) - rind(1,1,dir_ind) + 1) *
     &           (rind(2,2,dir_ind) - rind(1,2,dir_ind) + 1)

        allocate (rarray(rcount), stat=error)

        call mpi_recv (rarray, rcount, mpi_real, recv_from(dir_ind),
     &                 tag, se_worker_comm, status, error)

! -- unpack received data
        rcount = 0
        do j = rind(1,2,dir_ind), rind(2,2,dir_ind)
           do i = rind(1,1,dir_ind), rind(2,1,dir_ind)
              rcount = rcount + 1
              data(i,j) = rarray(rcount)
           end do
        end do

        deallocate (rarray)

        end subroutine se_2d_data_recv

! -----------------------------------------------------------------------------
        subroutine se_2de_data_recv (data, rind, recv_from, dir_ind, tag)

        implicit none
        include "mpif.h"

        real, intent(inout) :: data(:,:)
        integer, pointer :: rind(:, :), recv_from(:)
        integer, intent(in) :: dir_ind, tag

        integer :: i, j, rcount, error
        integer :: status(MPI_STATUS_SIZE)
!       real :: rarray(size(data))
        real, allocatable :: rarray(:)

        rcount = (rind(2,dir_ind) - rind(1,dir_ind) + 1) *
     &           (ubound(data,2) - lbound(data,2) + 1)

        allocate (rarray(rcount), stat=error)

        call mpi_recv (rarray, rcount, mpi_real, recv_from(dir_ind),
     &                 tag, se_worker_comm, status, error)

! -- unpack received data
        rcount = 0
        do j = lbound(data,2), ubound(data,2)
           do i = rind(1,dir_ind), rind(2,dir_ind)
              rcount = rcount + 1
              data(i,j) = rarray(rcount)
           end do
        end do

        deallocate (rarray)

        end subroutine se_2de_data_recv

! -----------------------------------------------------------------------------
        subroutine se_3d_data_recv (data, rind, recv_from, dir_ind, tag)

        implicit none
        include "mpif.h"

        real, intent(inout) :: data(:,:,:)
        integer, pointer :: rind(:, :, :), recv_from(:)
        integer, intent(in) :: dir_ind, tag

        integer :: i, j, k, rcount, error
        integer :: status(MPI_STATUS_SIZE)
!       real :: rarray(size(data))
        real, allocatable :: rarray(:)

        rcount = (rind(2,1,dir_ind) - rind(1,1,dir_ind) + 1) *
     &           (rind(2,2,dir_ind) - rind(1,2,dir_ind) + 1) *
     &           (rind(2,3,dir_ind) - rind(1,3,dir_ind) + 1)

        allocate (rarray(rcount), stat=error)

        call mpi_recv (rarray, rcount, mpi_real, recv_from(dir_ind),
     &                 tag, se_worker_comm, status, error)

! -- unpack received data
        rcount = 0
        do k = rind(1,3,dir_ind), rind(2,3,dir_ind)
           do j = rind(1,2,dir_ind), rind(2,2,dir_ind)
              do i = rind(1,1,dir_ind), rind(2,1,dir_ind)
                 rcount = rcount + 1
                 data(i,j,k) = rarray(rcount)
              end do
           end do
        end do

        deallocate (rarray)

        end subroutine se_3d_data_recv

! -----------------------------------------------------------------------------
        subroutine se_3de_data_recv (data, rind, recv_from, dir_ind, tag)

        implicit none
        include "mpif.h"

        real, intent(inout) :: data(:,:,:)
        integer, pointer :: rind(:, :), recv_from(:)
        integer, intent(in) :: dir_ind, tag

        integer :: i, j, k, rcount, error
        integer :: status(MPI_STATUS_SIZE)
!       real :: rarray(size(data))
        real, allocatable :: rarray(:)

        rcount = (rind(2,dir_ind) - rind(1,dir_ind) + 1) *
     &           (ubound(data,2) - lbound(data,2) + 1) *
     &           (ubound(data,3) - lbound(data,3) + 1)

        allocate (rarray(rcount), stat=error)

        call mpi_recv (rarray, rcount, mpi_real, recv_from(dir_ind),
     &                 tag, se_worker_comm, status, error)

! -- unpack received data
        rcount = 0
        do k = lbound(data,3), ubound(data,3)
           do j = lbound(data,2), ubound(data,2)
              do i = rind(1,dir_ind), rind(2,dir_ind)
                 rcount = rcount + 1
                 data(i,j,k) = rarray(rcount)
              end do
           end do
        end do

        deallocate (rarray)

        end subroutine se_3de_data_recv

! -----------------------------------------------------------------------------
        subroutine se_4d_data_recv (data, rind, recv_from, dir_ind, tag)

        implicit none
        include "mpif.h"

        real, intent(inout) :: data(:,:,:,:)
        integer, pointer :: rind(:, :, :), recv_from(:)
        integer, intent(in) :: dir_ind, tag

        integer :: i, j, k, l, rcount, error
        integer :: status(MPI_STATUS_SIZE)
!       real :: rarray(size(data))
        real, allocatable :: rarray(:)

        rcount = (rind(2,1,dir_ind) - rind(1,1,dir_ind) + 1) *
     &           (rind(2,2,dir_ind) - rind(1,2,dir_ind) + 1) *
     &           (rind(2,3,dir_ind) - rind(1,3,dir_ind) + 1) *
     &           (rind(2,4,dir_ind) - rind(1,4,dir_ind) + 1)

        allocate (rarray(rcount), stat=error)

        call mpi_recv (rarray, rcount, mpi_real, recv_from(dir_ind),
     &                 tag, se_worker_comm, status, error)

! -- unpack received data
        rcount = 0
        do l = rind(1,4,dir_ind), rind(2,4,dir_ind)
           do k = rind(1,3,dir_ind), rind(2,3,dir_ind)
              do j = rind(1,2,dir_ind), rind(2,2,dir_ind)
                 do i = rind(1,1,dir_ind), rind(2,1,dir_ind)
                    rcount = rcount + 1
                    data(i,j,k,l) = rarray(rcount)
                 end do
              end do
           end do
        end do

        deallocate (rarray)

        end subroutine se_4d_data_recv

! -----------------------------------------------------------------------------
        subroutine se_4de_data_recv (data, rind, recv_from, dir_ind, tag)

        implicit none
        include "mpif.h"

        real, intent(inout) :: data(:,:,:,:)
        integer, pointer :: rind(:, :), recv_from(:)
        integer, intent(in) :: dir_ind, tag

        integer :: i, j, k, s, rcount, error
        integer :: status(MPI_STATUS_SIZE)
!       real :: rarray(size(data))
        real, allocatable :: rarray(:)

        rcount = (rind(2,dir_ind) - rind(1,dir_ind) + 1) *
     &           (ubound(data,2) - lbound(data,2) + 1) *
     &           (ubound(data,3) - lbound(data,3) + 1) *
     &           (ubound(data,4) - lbound(data,4) + 1)

        allocate (rarray(rcount), stat=error)

        call mpi_recv (rarray, rcount, mpi_real, recv_from(dir_ind),
     &                 tag, se_worker_comm, status, error)

! -- unpack received data
        rcount = 0
        do s = lbound(data,4), ubound(data,4)
           do k = lbound(data,3), ubound(data,3)
              do j = lbound(data,2), ubound(data,2)
                 do i = rind(1,dir_ind), rind(2,dir_ind)
                    rcount = rcount + 1
                    data(i,j,k,s) = rarray(rcount)
                 end do
              end do
           end do
        end do

        deallocate (rarray)

        end subroutine se_4de_data_recv

! -----------------------------------------------------------------------------
        subroutine se_5d_data_recv (data, rind, recv_from, dir_ind, tag)

        implicit none
        include "mpif.h"

        real, intent(inout) :: data(:,:,:,:,:)
        integer, pointer :: rind(:, :, :), recv_from(:)
        integer, intent(in) :: dir_ind, tag

        integer :: i, j, k, l, m, rcount, error
        integer :: status(MPI_STATUS_SIZE)
!       real :: rarray(size(data))
        real, allocatable :: rarray(:)

        rcount = (rind(2,1,dir_ind) - rind(1,1,dir_ind) + 1) *
     &           (rind(2,2,dir_ind) - rind(1,2,dir_ind) + 1) *
     &           (rind(2,3,dir_ind) - rind(1,3,dir_ind) + 1) *
     &           (rind(2,4,dir_ind) - rind(1,4,dir_ind) + 1) *
     &           (rind(2,5,dir_ind) - rind(1,5,dir_ind) + 1)

        allocate (rarray(rcount), stat=error)

        call mpi_recv (rarray, rcount, mpi_real, recv_from(dir_ind),
     &                 tag, se_worker_comm, status, error)

! -- unpack received data
        rcount = 0
        do m = rind(1,5,dir_ind), rind(2,5,dir_ind)
           do l = rind(1,4,dir_ind), rind(2,4,dir_ind)
              do k = rind(1,3,dir_ind), rind(2,3,dir_ind)
                 do j = rind(1,2,dir_ind), rind(2,2,dir_ind)
                    do i = rind(1,1,dir_ind), rind(2,1,dir_ind)
                       rcount = rcount + 1
                       data(i,j,k,l,m) = rarray(rcount)
                    end do
                 end do
              end do
           end do
        end do

        deallocate (rarray)

        end subroutine se_5d_data_recv

        end module se_data_recv_module
