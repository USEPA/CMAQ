
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
! $Header: /home/wdx/lib/src/junk/junk/CMAQv5.0.1/models/STENEX/src/se_snl/se_twoway_comm_module.f,v 1.1.1.1 2012/04/19 19:48:15 sjr Exp $

! what(1) key, module and SID; SCCS file; date and time of last delta:
! %W% %P% %G% %U%

!-----------------------------------------------------------------------
! Purpose:
!   implement data exchange between WRF and CMAQ to accommodate different
!   domain sizes and domain decomposition strategies

! Revision history:
!   Orginal version: 4/10/07 by David Wong
!          Modified: 5/21/12 by David Wong
!                      -- used a more strict hand-shake paradiagm for communication
!          Modified: 9/30/15 by David Wong
!                      -- reduced the value of tag to accommodate various MPI 
!                         implementation
!-----------------------------------------------------------------------

        module se_twoway_comm_module

        use se_comm_info_ext

        implicit none

        integer, private :: wrf_cmaq_loc_ngb(0:8) = -1
        integer, private :: wrf_cmaq_loc_ngb_send_to(0:8, 2) = -1
        integer, private :: wrf_cmaq_loc_ngb_recv_from(0:8, 2) = -1
        logical, private :: wrf_cmaq_computed = .false.

        integer, private :: cmaq_wrf_loc_ngb(0:8) = -1
        integer, private :: cmaq_wrf_loc_ngb_send_to(0:8, 2) = -1
        integer, private :: cmaq_wrf_loc_ngb_recv_from(0:8, 2) = -1
        logical, private :: cmaq_wrf_computed = .false.

        interface se_wrf_cmaq_comm
          module procedure se_wrf_cmaq_comm2, se_wrf_cmaq_comm3, se_wrf_cmaq_comm4
        end interface

        interface se_cmaq_wrf_comm
          module procedure se_cmaq_wrf_comm2, se_cmaq_wrf_comm3, se_cmaq_wrf_comm4
        end interface

        contains

! -----------------------------------------------------------------------------

        subroutine se_wrf_cmaq_comm2 (mype, wrf_data, cmaq_data, 
     &                                wrf_cmaq_send_to, wrf_cmaq_recv_from,
     &                                wrf_cmaq_send_index_l, wrf_cmaq_recv_index_l, 
     &                                flag)

        implicit none

        integer, intent(in) :: mype
        real, intent(in)  :: wrf_data(:,:)
        real, intent(out) :: cmaq_data(:,:)
        integer, pointer :: wrf_cmaq_send_to(:,:), wrf_cmaq_recv_from(:,:)
        integer, pointer :: wrf_cmaq_send_index_l(:,:,:), wrf_cmaq_recv_index_l(:,:,:)
        integer, intent(in) :: flag

        include "mpif.h"

        integer :: c, r, lc, lr, i, j, k, index, sdir, rdir, 
     $             request, status(MPI_STATUS_SIZE), error
        integer :: data_size, tag, li, lj, s_ind, r_ind
        real,allocatable :: sarray(:), rarray(:)
        logical :: found

           call compute_ngb (mype, wrf_cmaq_loc_ngb, se_twoway_npcol,
     $                       se_twoway_nprow)

           do k = 0, 8

              call loc_index (wrf_cmaq_loc_ngb(k), wrf_cmaq_send_to(:,mype), 
     $                        wrf_cmaq_loc_ngb_send_to(k,1),
     $                        wrf_cmaq_loc_ngb_send_to(k,2))
              call loc_index (wrf_cmaq_loc_ngb(8-k), wrf_cmaq_recv_from(:,mype), 
     $                        wrf_cmaq_loc_ngb_recv_from(8-k,1),
     $                        wrf_cmaq_loc_ngb_recv_from(8-k,2))
           end do

        do k = 0, 8

           sdir = wrf_cmaq_loc_ngb_send_to(k,1)
           rdir = wrf_cmaq_loc_ngb_recv_from(8-k,1)

           if (sdir > -1) then
              s_ind = wrf_cmaq_loc_ngb_send_to(k,2)
              j = (s_ind - 1) * 3 + 1

              if (mype .eq. sdir) then

                 found = .false.
                 li = 0
                 do while (.not. found)
                    li = li + 1
                    if (mype .eq. wrf_cmaq_recv_from(li, mype)) then
                       found = .true.
                       lj = (li - 1) * 3 + 1
                    end if
                 end do

                 lr = wrf_cmaq_recv_index_l(lj,2,mype) - 1
                 do r = wrf_cmaq_send_index_l(j,2,mype), wrf_cmaq_send_index_l(j+1,2,mype)
                    lr = lr + 1
                    lc = wrf_cmaq_recv_index_l(lj,1,mype) - 1
                    do c = wrf_cmaq_send_index_l(j,1,mype), wrf_cmaq_send_index_l(j+1,1,mype)
                       lc = lc + 1
                       cmaq_data(lc,lr) = wrf_data(c,r)
                    end do
                 end do
              else
                 data_size = wrf_cmaq_send_index_l(j+2,1,mype) * wrf_cmaq_send_index_l(j+2,2,mype)

                 allocate (sarray(data_size), stat=error)
                 index = 0
                 do r = wrf_cmaq_send_index_l(j,2,mype), wrf_cmaq_send_index_l(j+1,2,mype)
                    do c = wrf_cmaq_send_index_l(j,1,mype), wrf_cmaq_send_index_l(j+1,1,mype)
                       index = index + 1
                       sarray(index) = wrf_data(c,r)
                    end do
                 end do
!                tag = flag * 1000000 + mype * 1000 + sdir
                 tag = flag * 1000 + sdir

                 call mpi_send (sarray, data_size, mpi_real, sdir,
     &                          tag, mpi_comm_world, error)

                 deallocate (sarray)

              end if
           end if

           if (rdir > -1) then

              if (mype .ne. rdir) then

                 r_ind = wrf_cmaq_loc_ngb_recv_from(8-k,2)
                 j = (r_ind - 1) * 3 + 1

                 data_size = wrf_cmaq_recv_index_l(j+2,1,mype) * wrf_cmaq_recv_index_l(j+2,2,mype)

                 allocate (rarray(data_size), stat=error)
!                tag = flag * 1000000 + rdir * 1000 + mype
                 tag = flag * 1000 + mype

                 call mpi_recv (rarray, data_size, mpi_real, rdir, tag,
     $                          mpi_comm_world, status, error)

                 index = 0
                 do r = wrf_cmaq_recv_index_l(j,2,mype), wrf_cmaq_recv_index_l(j+1,2,mype)
                    do c = wrf_cmaq_recv_index_l(j,1,mype), wrf_cmaq_recv_index_l(j+1,1,mype)
                       index = index + 1
                       cmaq_data(c,r) = rarray(index)
                    end do
                 end do
                 deallocate (rarray)
              end if
           end if

        end do

        return
        end subroutine se_wrf_cmaq_comm2

! -----------------------------------------------------------------------------

        subroutine se_wrf_cmaq_comm3 (mype, wrf_data, cmaq_data, 
     &                                wrf_cmaq_send_to, wrf_cmaq_recv_from,
     &                                wrf_cmaq_send_index_l, wrf_cmaq_recv_index_l, 
     &                                flag)

        implicit none

        integer, intent(in) :: mype
        real, intent(in)  :: wrf_data(:,:,:)
        real, intent(out) :: cmaq_data(:,:,:)
        integer, pointer :: wrf_cmaq_send_to(:,:), wrf_cmaq_recv_from(:,:)
!       integer, intent(in) :: wrf_cmaq_send_to(:,:), wrf_cmaq_recv_from(:,:)
        integer, pointer :: wrf_cmaq_send_index_l(:,:,:), wrf_cmaq_recv_index_l(:,:,:)
        integer, intent(in) :: flag

        include "mpif.h"

        integer :: c, r, lc, lr, d, i, j, k, index, sdir, rdir, 
     $             request, status(MPI_STATUS_SIZE), error
        integer :: data_size, size_3d, tag, li, lj, s_ind, r_ind
        real,allocatable :: sarray(:), rarray(:)
!       real :: sarray(size(wrf_data))  ! the second dimension is needed to keep the send
                                          ! data since all the send instructions are done
                                          ! before any receive instruction
!       real :: rarray(size(cmaq_data))
        logical :: found

        integer :: total_s, total_r, n_s, n_r
        integer :: total_ss, total_rr, n_ss, n_rr

!       if (.not. wrf_cmaq_computed) then
           call  compute_ngb (mype, wrf_cmaq_loc_ngb, se_twoway_npcol, 
     $                        se_twoway_nprow)

!         write (6, '(a13, 9i5)') ' ==dw3 ngb a ', wrf_cmaq_loc_ngb

!         write (6, '(a15, 20i5)') ' ==dw3 send to ', 
!    $      wrf_cmaq_send_to(0:wrf_cmaq_send_to(0,mype), mype)

!         write (6, '(a15, 20i5)') ' ==dw3 recv fr ', 
!    $      wrf_cmaq_recv_from(0:wrf_cmaq_recv_from(0,mype), mype)

           wrf_cmaq_computed = .true.
           do k = 0, 8

              call loc_index (wrf_cmaq_loc_ngb(k), wrf_cmaq_send_to(:,mype), 
     $                        wrf_cmaq_loc_ngb_send_to(k,1), 
     $                        wrf_cmaq_loc_ngb_send_to(k,2)) 
              call loc_index (wrf_cmaq_loc_ngb(8-k), wrf_cmaq_recv_from(:,mype), 
     $                        wrf_cmaq_loc_ngb_recv_from(8-k,1), 
     $                        wrf_cmaq_loc_ngb_recv_from(8-k,2)) 
           end do

!          do k = 0, 8
!         write (6, '(a13, 9i5)') ' ==dw3 ngb b ', k, wrf_cmaq_loc_ngb_send_to(k,:)
!          end do
!          do k = 0, 8
!         write (6, '(a13, 9i5)') ' ==dw3 ngb c ', k, wrf_cmaq_loc_ngb_recv_from(k,:)
!          end do

!       end if

        size_3d = size(wrf_data, 3)

        total_s = 0
        total_r = 0
        n_s = 0
        n_r = 0
        do k = 0, 8
!          call loc_index (loc_ngb(k), wrf_cmaq_send_to(:,mype), sdir, s_ind)
!          call loc_index (loc_ngb(8-k), wrf_cmaq_recv_from(:,mype), rdir, r_ind)

           sdir = wrf_cmaq_loc_ngb_send_to(k,1) 
           rdir = wrf_cmaq_loc_ngb_recv_from(8-k,1) 
 
!          write (6, '(a7, 30i5)') ' ==d== ', k, mype, loc_ngb(k), wrf_cmaq_send_to(:,mype), sdir, s_ind
!          write (6, '(a7, 30i5)') ' ==d== ', k, mype, loc_ngb(8-k), wrf_cmaq_recv_from(:,mype), rdir, r_ind

!          goto 999

           if (sdir > -1) then
              total_s = total_s + 1 
              s_ind = wrf_cmaq_loc_ngb_send_to(k,2)
              j = (s_ind - 1) * 3 + 1

              if (mype .eq. sdir) then

                 found = .false.
                 li = 0
                 do while (.not. found)
                    li = li + 1
                    if (mype .eq. wrf_cmaq_recv_from(li, mype)) then
                       found = .true.
                       lj = (li - 1) * 3 + 1
                    end if
                 end do

                 lr = wrf_cmaq_recv_index_l(lj,2,mype) - 1
                 do r = wrf_cmaq_send_index_l(j,2,mype), wrf_cmaq_send_index_l(j+1,2,mype)
                    lr = lr + 1
                    lc = wrf_cmaq_recv_index_l(lj,1,mype) - 1
                    do c = wrf_cmaq_send_index_l(j,1,mype), wrf_cmaq_send_index_l(j+1,1,mype)
                       lc = lc + 1
                       cmaq_data(lc,lr,:) = wrf_data(c,r,:)
                    end do
                 end do
              else
                 data_size = size_3d * wrf_cmaq_send_index_l(j+2,1,mype) * wrf_cmaq_send_index_l(j+2,2,mype)

                 allocate (sarray(data_size), stat=error)
                 index = 0
                 do d = 1, size_3d
                    do r = wrf_cmaq_send_index_l(j,2,mype), wrf_cmaq_send_index_l(j+1,2,mype)
                       do c = wrf_cmaq_send_index_l(j,1,mype), wrf_cmaq_send_index_l(j+1,1,mype)
                          index = index + 1
                          sarray(index) = wrf_data(c,r,d)
                       end do
                    end do
                 end do
!                tag = flag * 1000000 + mype * 1000 + sdir
                 tag = flag * 1000 + sdir

!        write (6, '(a12, 2i5, 8i10)') ' ==dw3 send ', j, sdir, tag, data_size, 
!    $     size_3d, wrf_cmaq_send_index_l(j+2,1,mype),
!    $     wrf_cmaq_send_index_l(j+2,2,mype), size(sarray)

                 call mpi_send (sarray, data_size, mpi_real, sdir,
     &                          tag, mpi_comm_world, error)

!                n_s = n_s + 1 
!                call mpi_send (mype, 1, mpi_integer, sdir,
!    &                          tag, mpi_comm_world, error)
                 deallocate (sarray)

              end if
           end if

           if (rdir > -1) then

              total_r = total_r + 1 
              if (mype .ne. rdir) then

                 r_ind = wrf_cmaq_loc_ngb_recv_from(8-k,2)
                 j = (r_ind - 1) * 3 + 1

                 data_size = size_3d * wrf_cmaq_recv_index_l(j+2,1,mype) * wrf_cmaq_recv_index_l(j+2,2,mype)

                 allocate (rarray(data_size), stat=error)
!                tag = flag * 1000000 + rdir * 1000 + mype
                 tag = flag * 1000 + mype

                 call mpi_recv (rarray, data_size, mpi_real, rdir, tag, 
     $                          mpi_comm_world, status, error)

!                n_r = n_r + 1 
!                call mpi_recv (request, 1, mpi_integer, rdir, tag, mpi_comm_world, status, error)

!        write (6, '(a12, 2i5, 10i8)') ' ==dw3 recv ', j, rdir, tag,
!    $       data_size, size_3d, wrf_cmaq_recv_index_l(j+2,1,mype), 
!    $       wrf_cmaq_recv_index_l(j+2,2,mype), size(rarray)

                 index = 0
                 do d = 1, size_3d
                    do r = wrf_cmaq_recv_index_l(j,2,mype), wrf_cmaq_recv_index_l(j+1,2,mype)
                       do c = wrf_cmaq_recv_index_l(j,1,mype), wrf_cmaq_recv_index_l(j+1,1,mype)
                          index = index + 1
                          cmaq_data(c,r,d) = rarray(index)
                       end do
                     end do
                 end do
                 deallocate (rarray)
              end if
           end if
 999       continue

        end do

        end subroutine se_wrf_cmaq_comm3

! -----------------------------------------------------------------------------

        subroutine se_wrf_cmaq_comm4 (mype, wrf_data, cmaq_data, 
     &                                wrf_cmaq_send_to, wrf_cmaq_recv_from,
     &                                wrf_cmaq_send_index_l, wrf_cmaq_recv_index_l, 
     &                                flag)

        implicit none

        integer, intent(in) :: mype
        real, intent(in)  :: wrf_data(:,:,:,:)
        real, intent(out) :: cmaq_data(:,:,:,:)
        integer, pointer :: wrf_cmaq_send_to(:,:), wrf_cmaq_recv_from(:,:)
        integer, pointer :: wrf_cmaq_send_index_l(:,:,:), wrf_cmaq_recv_index_l(:,:,:)
        integer, intent(in) :: flag

        include "mpif.h"

        integer :: sdir, rdir, size_l_v, nlays, nvars, si, ri
        logical :: done
        integer :: c, r, l, v, i, j, k, data_size, tag, li, lj, s_ind, 
     $             r_ind, index, lr, lc,
     $             request, status(MPI_STATUS_SIZE), error
        real,allocatable :: sarray(:), rarray(:)
        logical :: found

           call  compute_ngb (mype, wrf_cmaq_loc_ngb, se_twoway_npcol,
     $                        se_twoway_nprow)

           do k = 0, 8

              call loc_index (wrf_cmaq_loc_ngb(k), wrf_cmaq_send_to(:,mype), 
     $                        wrf_cmaq_loc_ngb_send_to(k,1),
     $                        wrf_cmaq_loc_ngb_send_to(k,2))
              call loc_index (wrf_cmaq_loc_ngb(8-k), wrf_cmaq_recv_from(:,mype), 
     $                        wrf_cmaq_loc_ngb_recv_from(8-k,1),
     $                        wrf_cmaq_loc_ngb_recv_from(8-k,2))
           end do

        nlays = size(wrf_data, 3)
        nvars = size(wrf_data, 4)
        size_l_v = nlays * nvars

        do k = 0, 8

           sdir = wrf_cmaq_loc_ngb_send_to(k,1)
           rdir = wrf_cmaq_loc_ngb_recv_from(8-k,1)

           if (sdir > -1) then
              s_ind = wrf_cmaq_loc_ngb_send_to(k,2)
              j = (s_ind - 1) * 3 + 1

              if (mype .eq. sdir) then

                 found = .false.
                 li = 0
                 do while (.not. found)
                    li = li + 1
                    if (mype .eq. wrf_cmaq_recv_from(li, mype)) then
                       found = .true.
                       lj = (li - 1) * 3 + 1
                    end if
                 end do

                 lr = wrf_cmaq_recv_index_l(lj,2,mype) - 1
                 do r = wrf_cmaq_send_index_l(j,2,mype), wrf_cmaq_send_index_l(j+1,2,mype)
                    lr = lr + 1
                    lc = wrf_cmaq_recv_index_l(lj,1,mype) - 1
                    do c = wrf_cmaq_send_index_l(j,1,mype), wrf_cmaq_send_index_l(j+1,1,mype)
                       lc = lc + 1
                       cmaq_data(lc,lr,:,:) = wrf_data(c,r,:,:)
                    end do
                 end do
              else
                 data_size = size_l_v * wrf_cmaq_send_index_l(j+2,1,mype) * wrf_cmaq_send_index_l(j+2,2,mype)

                 allocate (sarray(data_size), stat=error)
                 index = 0
                 do v = 1, nvars
                    do l = 1, nlays
                       do r = wrf_cmaq_send_index_l(j,2,mype), wrf_cmaq_send_index_l(j+1,2,mype)
                          do c = wrf_cmaq_send_index_l(j,1,mype), wrf_cmaq_send_index_l(j+1,1,mype)
                             index = index + 1
                             sarray(index) = wrf_data(c,r,l,v)
                          end do
                       end do
                    end do
                 end do
!                tag = flag * 1000000 + mype * 1000 + sdir
                 tag = flag * 1000 + sdir

                 call mpi_send (sarray, data_size, mpi_real, sdir,
     &                          tag, mpi_comm_world, error)

                 deallocate (sarray)

              end if
           end if

           if (rdir > -1) then

              if (mype .ne. rdir) then

                 r_ind = wrf_cmaq_loc_ngb_recv_from(8-k,2)
                 j = (r_ind - 1) * 3 + 1

                 data_size = size_l_v * wrf_cmaq_recv_index_l(j+2,1,mype) * wrf_cmaq_recv_index_l(j+2,2,mype)

                 allocate (rarray(data_size), stat=error)
!                tag = flag * 1000000 + rdir * 1000 + mype
                 tag = flag * 1000 + mype

                 call mpi_recv (rarray, data_size, mpi_real, rdir, tag,
     $                          mpi_comm_world, status, error)

                 index = 0
                 do v = 1, nvars
                    do l = 1, nlays
                       do r = wrf_cmaq_recv_index_l(j,2,mype), wrf_cmaq_recv_index_l(j+1,2,mype)
                          do c = wrf_cmaq_recv_index_l(j,1,mype), wrf_cmaq_recv_index_l(j+1,1,mype)
                             index = index + 1
                             cmaq_data(c,r,l,v) = rarray(index)
                          end do
                       end do
                     end do
                 end do
                 deallocate (rarray)
              end if
           end if

        end do

        return
        end subroutine se_wrf_cmaq_comm4

! -----------------------------------------------------------------------------

        subroutine se_cmaq_wrf_comm2 (mype, cmaq_data, wrf_data,
     &                                cmaq_wrf_send_to, cmaq_wrf_recv_from,
     &                                cmaq_wrf_send_index_l, cmaq_wrf_recv_index_l, 
     &                                flag)

        implicit none

        integer, intent(in) :: mype
        real, intent(in)  :: cmaq_data(:,:)
        real, intent(out) :: wrf_data(:,:)
        integer, pointer :: cmaq_wrf_send_to(:,:), cmaq_wrf_recv_from(:,:)
        integer, pointer :: cmaq_wrf_send_index_l(:,:,:), cmaq_wrf_recv_index_l(:,:,:)
        integer, intent(in) :: flag

        include "mpif.h"

        integer :: c, r, lc, lr, i, j, k, index, sdir, rdir, 
     $             request, status(MPI_STATUS_SIZE), error
        integer :: data_size, tag, li, lj, s_ind, r_ind
        real,allocatable :: sarray(:), rarray(:)
        logical :: found

           call compute_ngb (mype, cmaq_wrf_loc_ngb, se_twoway_npcol,
     $                       se_twoway_nprow)

           do k = 0, 8

              call loc_index (cmaq_wrf_loc_ngb(k), cmaq_wrf_send_to(:,mype), 
     $                        cmaq_wrf_loc_ngb_send_to(k,1),
     $                        cmaq_wrf_loc_ngb_send_to(k,2))
              call loc_index (cmaq_wrf_loc_ngb(8-k), cmaq_wrf_recv_from(:,mype), 
     $                        cmaq_wrf_loc_ngb_recv_from(8-k,1),
     $                        cmaq_wrf_loc_ngb_recv_from(8-k,2))
           end do

        do k = 0, 8

           sdir = cmaq_wrf_loc_ngb_send_to(k,1)
           rdir = cmaq_wrf_loc_ngb_recv_from(8-k,1)

           if (sdir > -1) then
              s_ind = cmaq_wrf_loc_ngb_send_to(k,2)
              j = (s_ind - 1) * 3 + 1

              if (mype .eq. sdir) then

                 found = .false.
                 li = 0
                 do while (.not. found)
                    li = li + 1
                    if (mype .eq. cmaq_wrf_recv_from(li, mype)) then
                       found = .true.
                       lj = (li - 1) * 3 + 1
                    end if
                 end do

                 lr = cmaq_wrf_recv_index_l(lj,2,mype) - 1
                 do r = cmaq_wrf_send_index_l(j,2,mype), cmaq_wrf_send_index_l(j+1,2,mype)
                    lr = lr + 1
                    lc = cmaq_wrf_recv_index_l(lj,1,mype) - 1
                    do c = cmaq_wrf_send_index_l(j,1,mype), cmaq_wrf_send_index_l(j+1,1,mype)
                       lc = lc + 1
                       wrf_data(lc,lr) = cmaq_data(c,r)
                    end do
                 end do
              else
                 data_size = cmaq_wrf_send_index_l(j+2,1,mype) * cmaq_wrf_send_index_l(j+2,2,mype)

                 allocate (sarray(data_size), stat=error)
                 index = 0
                 do r = cmaq_wrf_send_index_l(j,2,mype), cmaq_wrf_send_index_l(j+1,2,mype)
                    do c = cmaq_wrf_send_index_l(j,1,mype), cmaq_wrf_send_index_l(j+1,1,mype)
                       index = index + 1
                       sarray(index) = cmaq_data(c,r)
                    end do
                 end do
!                tag = flag * 1000000 + mype * 1000 + sdir
                 tag = flag * 1000 + sdir

                 call mpi_send (sarray, data_size, mpi_real, sdir,
     &                          tag, mpi_comm_world, error)

                 deallocate (sarray)

              end if
           end if

           if (rdir > -1) then

              if (mype .ne. rdir) then

                 r_ind = cmaq_wrf_loc_ngb_recv_from(8-k,2)
                 j = (r_ind - 1) * 3 + 1

                 data_size = cmaq_wrf_recv_index_l(j+2,1,mype) * cmaq_wrf_recv_index_l(j+2,2,mype)

                 allocate (rarray(data_size), stat=error)
!                tag = flag * 1000000 + rdir * 1000 + mype
                 tag = flag * 1000 + mype

                 call mpi_recv (rarray, data_size, mpi_real, rdir, tag,
     $                          mpi_comm_world, status, error)

                 index = 0
                 do r = cmaq_wrf_recv_index_l(j,2,mype), cmaq_wrf_recv_index_l(j+1,2,mype)
                    do c = cmaq_wrf_recv_index_l(j,1,mype), cmaq_wrf_recv_index_l(j+1,1,mype)
                       index = index + 1
                       wrf_data(c,r) = rarray(index)
                    end do
                 end do
                 deallocate (rarray)
              end if
           end if

        end do

        return
        end subroutine se_cmaq_wrf_comm2

! -----------------------------------------------------------------------------

        subroutine se_cmaq_wrf_comm3 (mype, cmaq_data, wrf_data,
     &                                cmaq_wrf_send_to, cmaq_wrf_recv_from,
     &                                cmaq_wrf_send_index_l, cmaq_wrf_recv_index_l, 
     &                                flag)

        implicit none

        integer, intent(in) :: mype
        real, intent(in)  :: cmaq_data(:,:,:)
        real, intent(out) :: wrf_data(:,:,:)
        integer, pointer :: cmaq_wrf_send_to(:,:), cmaq_wrf_recv_from(:,:)
        integer, pointer :: cmaq_wrf_send_index_l(:,:,:), cmaq_wrf_recv_index_l(:,:,:)
        integer, intent(in) :: flag

        include "mpif.h"

        integer :: c, r, lc, lr, d, i, j, k, index, sdir, rdir, 
     $             request, status(MPI_STATUS_SIZE), error
        integer :: data_size, size_3d, tag, li, lj, s_ind, r_ind
        real,allocatable :: sarray(:), rarray(:)
        logical :: found

        integer :: total_s, total_r, n_s, n_r
        integer :: total_ss, total_rr, n_ss, n_rr

!       if (.not. cmaq_wrf_computed) then
           call  compute_ngb (mype, cmaq_wrf_loc_ngb, se_twoway_npcol, 
     $                        se_twoway_nprow)

!         write (6, '(a13, 9i5)') ' ==dc3 ngb a ', cmaq_wrf_loc_ngb

!         write (6, '(a15, 20i5)') ' ==dc3 send to ', 
!    $      cmaq_wrf_send_to(0:cmaq_wrf_send_to(0,mype), mype)

!         write (6, '(a15, 20i5)') ' ==dc3 recv fr ', 
!    $      cmaq_wrf_recv_from(0:cmaq_wrf_recv_from(0,mype), mype)

           cmaq_wrf_computed = .true.
           do k = 0, 8

              call loc_index (cmaq_wrf_loc_ngb(k), cmaq_wrf_send_to(:,mype), 
     $                        cmaq_wrf_loc_ngb_send_to(k,1), 
     $                        cmaq_wrf_loc_ngb_send_to(k,2)) 
              call loc_index (cmaq_wrf_loc_ngb(8-k), cmaq_wrf_recv_from(:,mype), 
     $                        cmaq_wrf_loc_ngb_recv_from(8-k,1), 
     $                        cmaq_wrf_loc_ngb_recv_from(8-k,2)) 
           end do

!          do k = 0, 8
!         write (6, '(a13, 9i5)') ' ==dc3 ngb b ', k, cmaq_wrf_loc_ngb_send_to(k,:)
!          end do
!          do k = 0, 8
!         write (6, '(a13, 9i5)') ' ==dc3 ngb c ', k, cmaq_wrf_loc_ngb_recv_from(k,:)
!          end do

!       end if

        size_3d = size(cmaq_data, 3)

        n_s = 0
        n_r = 0
        do k = 0, 8

           sdir = cmaq_wrf_loc_ngb_send_to(k,1) 
           rdir = cmaq_wrf_loc_ngb_recv_from(8-k,1) 
 
           if (sdir > -1) then
              total_s = total_s + 1 
              s_ind = cmaq_wrf_loc_ngb_send_to(k,2)
              j = (s_ind - 1) * 3 + 1

              if (mype .eq. sdir) then

                 found = .false.
                 li = 0
                 do while (.not. found)
                    li = li + 1
                    if (mype .eq. cmaq_wrf_recv_from(li, mype)) then
                       found = .true.
                       lj = (li - 1) * 3 + 1
                    end if
                 end do

                 lr = cmaq_wrf_recv_index_l(lj,2,mype) - 1
                 do r = cmaq_wrf_send_index_l(j,2,mype), cmaq_wrf_send_index_l(j+1,2,mype)
                    lr = lr + 1
                    lc = cmaq_wrf_recv_index_l(lj,1,mype) - 1
                    do c = cmaq_wrf_send_index_l(j,1,mype), cmaq_wrf_send_index_l(j+1,1,mype)
                       lc = lc + 1
                       wrf_data(lc,lr,:) = cmaq_data(c,r,:)
                    end do
                 end do
              else
                 data_size = size_3d * cmaq_wrf_send_index_l(j+2,1,mype) * cmaq_wrf_send_index_l(j+2,2,mype)

                 allocate (sarray(data_size), stat=error)
                 index = 0
                 do d = 1, size_3d
                    do r = cmaq_wrf_send_index_l(j,2,mype), cmaq_wrf_send_index_l(j+1,2,mype)
                       do c = cmaq_wrf_send_index_l(j,1,mype), cmaq_wrf_send_index_l(j+1,1,mype)
                          index = index + 1
                          sarray(index) = cmaq_data(c,r,d)
                       end do
                    end do
                 end do
!                tag = flag * 1000000 + mype * 1000 + sdir
                 tag = flag * 1000 + sdir

!        write (6, '(a12, 2i5, 8i10)') ' ==dc3 send ', j, sdir, tag, data_size, 
!    $     size_3d, cmaq_wrf_send_index_l(j+2,1,mype),
!    $     cmaq_wrf_send_index_l(j+2,2,mype), size(sarray)

                 call mpi_send (sarray, data_size, mpi_real, sdir,
     &                          tag, mpi_comm_world, error)

                 deallocate (sarray)

              end if
           end if

           if (rdir > -1) then

              total_r = total_r + 1 
              if (mype .ne. rdir) then

                 r_ind = cmaq_wrf_loc_ngb_recv_from(8-k,2)
                 j = (r_ind - 1) * 3 + 1

                 data_size = size_3d * cmaq_wrf_recv_index_l(j+2,1,mype) * cmaq_wrf_recv_index_l(j+2,2,mype)

                 allocate (rarray(data_size), stat=error)
!                tag = flag * 1000000 + rdir * 1000 + mype
                 tag = flag * 1000 + mype

                 call mpi_recv (rarray, data_size, mpi_real, rdir, tag, 
     $                          mpi_comm_world, status, error)

!        write (6, '(a12, 2i5, 10i8)') ' ==dc3 recv ', j, rdir, tag,
!    $       data_size, size_3d, cmaq_wrf_recv_index_l(j+2,1,mype), 
!    $       cmaq_wrf_recv_index_l(j+2,2,mype), size(rarray)

                 index = 0
                 do d = 1, size_3d
                    do r = cmaq_wrf_recv_index_l(j,2,mype), cmaq_wrf_recv_index_l(j+1,2,mype)
                       do c = cmaq_wrf_recv_index_l(j,1,mype), cmaq_wrf_recv_index_l(j+1,1,mype)
                          index = index + 1
                          wrf_data(c,r,d) = rarray(index)
                       end do
                     end do
                 end do
                 deallocate (rarray)
              end if
           end if

        end do

        end subroutine se_cmaq_wrf_comm3

! -----------------------------------------------------------------------------

        subroutine se_cmaq_wrf_comm4 (mype, cmaq_data, wrf_data,
     &                                cmaq_wrf_send_to, cmaq_wrf_recv_from,
     &                                cmaq_wrf_send_index_l, cmaq_wrf_recv_index_l, 
     &                                flag)

        implicit none

        integer, intent(in) :: mype
        real, intent(in)  :: cmaq_data(:,:,:,:)
        real, intent(out) :: wrf_data(:,:,:,:)
        integer, pointer :: cmaq_wrf_send_to(:,:), cmaq_wrf_recv_from(:,:)
        integer, pointer :: cmaq_wrf_send_index_l(:,:,:), cmaq_wrf_recv_index_l(:,:,:)
        integer, intent(in) :: flag

        include "mpif.h"

        integer :: sdir, rdir, size_l_v, nlays, nvars, si, ri
        logical :: done
        integer :: c, r, l, v, i, j, k, data_size, tag, li, lj, s_ind, 
     $             r_ind, index, lr, lc,
     $             request, status(MPI_STATUS_SIZE), error
        real,allocatable :: sarray(:), rarray(:)
        logical :: found

           call  compute_ngb (mype, cmaq_wrf_loc_ngb, se_twoway_npcol,
     $                        se_twoway_nprow)

!         write (6, '(a13, 9i5)') ' ==dc4 ngb a ', cmaq_wrf_loc_ngb

!         write (6, '(a15, 20i5)') ' ==dc4 send to ', 
!    $      cmaq_wrf_send_to(0:cmaq_wrf_send_to(0,mype), mype)

!         write (6, '(a15, 20i5)') ' ==dc4 recv fr ', 
!    $      cmaq_wrf_recv_from(0:cmaq_wrf_recv_from(0,mype), mype)

           do k = 0, 8

              call loc_index (cmaq_wrf_loc_ngb(k), cmaq_wrf_send_to(:,mype), 
     $                        cmaq_wrf_loc_ngb_send_to(k,1),
     $                        cmaq_wrf_loc_ngb_send_to(k,2))
              call loc_index (cmaq_wrf_loc_ngb(8-k), cmaq_wrf_recv_from(:,mype), 
     $                        cmaq_wrf_loc_ngb_recv_from(8-k,1),
     $                        cmaq_wrf_loc_ngb_recv_from(8-k,2))
           end do

!          do k = 0, 8
!         write (6, '(a13, 9i5)') ' ==dc4 ngb b ', k, cmaq_wrf_loc_ngb_send_to(k,:)
!          end do
!          do k = 0, 8
!         write (6, '(a13, 9i5)') ' ==dc4 ngb c ', k, cmaq_wrf_loc_ngb_recv_from(k,:)
!          end do

        nlays = size(cmaq_data, 3)
        nvars = size(cmaq_data, 4)
        size_l_v = nlays * nvars

        do k = 0, 8

           sdir = cmaq_wrf_loc_ngb_send_to(k,1)
           rdir = cmaq_wrf_loc_ngb_recv_from(8-k,1)

           if (sdir > -1) then
              s_ind = cmaq_wrf_loc_ngb_send_to(k,2)
              j = (s_ind - 1) * 3 + 1

              if (mype .eq. sdir) then

                 found = .false.
                 li = 0
                 do while (.not. found)
                    li = li + 1
                    if (mype .eq. cmaq_wrf_recv_from(li, mype)) then
                       found = .true.
                       lj = (li - 1) * 3 + 1
                    end if
                 end do

                 lr = cmaq_wrf_recv_index_l(lj,2,mype) - 1
                 do r = cmaq_wrf_send_index_l(j,2,mype), cmaq_wrf_send_index_l(j+1,2,mype)
                    lr = lr + 1
                    lc = cmaq_wrf_recv_index_l(lj,1,mype) - 1
                    do c = cmaq_wrf_send_index_l(j,1,mype), cmaq_wrf_send_index_l(j+1,1,mype)
                       lc = lc + 1
                       wrf_data(lc,lr,:,:) = cmaq_data(c,r,:,:)
                    end do
                 end do
              else
                 data_size = size_l_v * cmaq_wrf_send_index_l(j+2,1,mype) * cmaq_wrf_send_index_l(j+2,2,mype)

                 allocate (sarray(data_size), stat=error)
                 index = 0
                 do v = 1, nvars
                    do l = 1, nlays
                       do r = cmaq_wrf_send_index_l(j,2,mype), cmaq_wrf_send_index_l(j+1,2,mype)
                          do c = cmaq_wrf_send_index_l(j,1,mype), cmaq_wrf_send_index_l(j+1,1,mype)
                             index = index + 1
                             sarray(index) = cmaq_data(c,r,l,v)
                          end do
                       end do
                    end do
                 end do
!                tag = flag * 1000000 + mype * 1000 + sdir
                 tag = flag * 1000 + sdir

!        write (6, '(a12, 2i5, 8i10)') ' ==dc4 send ', j, sdir, tag, data_size, 
!    $     size_l_v, cmaq_wrf_send_index_l(j+2,1,mype),
!    $     cmaq_wrf_send_index_l(j+2,2,mype), size(sarray)

                 call mpi_send (sarray, data_size, mpi_real, sdir,
     &                          tag, mpi_comm_world, error)

                 deallocate (sarray)

              end if
           end if

           if (rdir > -1) then

              if (mype .ne. rdir) then

                 r_ind = cmaq_wrf_loc_ngb_recv_from(8-k,2)
                 j = (r_ind - 1) * 3 + 1

                 data_size = size_l_v * cmaq_wrf_recv_index_l(j+2,1,mype) * cmaq_wrf_recv_index_l(j+2,2,mype)

                 allocate (rarray(data_size), stat=error)
!                tag = flag * 1000000 + rdir * 1000 + mype
                 tag = flag * 1000 + mype

!        write (6, '(a12, 2i5, 10i8)') ' ==dc4 recv ', j, rdir, tag,
!    $       data_size, size_l_v, cmaq_wrf_recv_index_l(j+2,1,mype), 
!    $       cmaq_wrf_recv_index_l(j+2,2,mype), size(rarray)

                 call mpi_recv (rarray, data_size, mpi_real, rdir, tag,
     $                          mpi_comm_world, status, error)

                 index = 0
                 do v = 1, nvars
                    do l = 1, nlays
                       do r = cmaq_wrf_recv_index_l(j,2,mype), cmaq_wrf_recv_index_l(j+1,2,mype)
                          do c = cmaq_wrf_recv_index_l(j,1,mype), cmaq_wrf_recv_index_l(j+1,1,mype)
                             index = index + 1
                             wrf_data(c,r,l,v) = rarray(index)
                          end do
                       end do
                     end do
                 end do
                 deallocate (rarray)
              end if
           end if

        end do

        return
        end subroutine se_cmaq_wrf_comm4

! -----------------------------------------------------------------------------

        subroutine se_twoway_send4 (mype, si, sdir, source_data, dest_data, 
     &                              recv_from, send_index_l, 
     &                              recv_index_l, flag, size_l_v,
     &                              nvars, nlays)

        integer, intent(in) :: mype, si, sdir, size_l_v, nvars, nlays
        real, intent(in)  :: source_data(:,:,:,:)
        real, intent(out) :: dest_data(:,:,:,:)
        integer, pointer :: recv_from(:,:)
        integer, pointer :: send_index_l(:,:,:), recv_index_l(:,:,:)
        integer, intent(in) :: flag

        include "mpif.h" 

        integer :: c, r, lc, lr, l, v, j, index, request, status(MPI_STATUS_SIZE), error
        integer :: data_size, tag, lj, li 
        real, allocatable :: sarray(:)
        logical :: found

        allocate (sarray(size(source_data)), stat=error)

           j = (si - 1) * 3 + 1            ! 1st diminsion of *_index_l array has three components: 
                                           ! starting index, ending index, and distance
                                           ! between starting and ending indices

           if (mype .eq. sdir) then

              found = .false.
              li = 0
              do while (.not. found)
                 li = li + 1
                 if (mype .eq. recv_from(li, mype)) then
                    found = .true.
                    lj = (li - 1) * 3 + 1
                 end if
              end do

              do v = 1, nvars
                 do l = 1, nlays
                    lr = recv_index_l(lj,2,mype) - 1
                    do r = send_index_l(j,2,mype), send_index_l(j+1,2,mype)
                       lr = lr + 1
                       lc = recv_index_l(lj,1,mype) - 1
                       do c = send_index_l(j,1,mype), send_index_l(j+1,1,mype)
                          lc = lc + 1
                          dest_data(lc,lr,l,v) = source_data(c,r,l,v)
                       end do
                    end do
                 end do
              end do

           else

              data_size = size_l_v * send_index_l(j+2,1,mype) * send_index_l(j+2,2,mype)

              index = 0
              do v = 1, nvars
                 do l = 1, nlays
                    do r = send_index_l(j,2,mype), send_index_l(j+1,2,mype)
                       do c = send_index_l(j,1,mype), send_index_l(j+1,1,mype)
                          index = index + 1
                          sarray(index) = source_data(c,r,l,v)
                       end do
                    end do
                 end do
              end do
!             tag = flag * 1000000 + mype * 1000 + sdir
              tag = flag * 1000 + sdir

!             call mpi_issend (sarray(1:data_size), data_size, mpi_real, sdir,
!    &                         tag, mpi_comm_world, request, error)
              call mpi_send (sarray(1:data_size), data_size, mpi_real, sdir,
     &                         tag, mpi_comm_world, error)

!             call mpi_wait (request, status, error)
!             call mpi_request_free (request, error)
           end if

        deallocate (sarray)

        end subroutine se_twoway_send4

! -----------------------------------------------------------------------------

        subroutine se_twoway_recv4 (mype, ri, rdir, dest_data, 
     &                              recv_index_l, flag, size_l_v,
     &                              nvars, nlays)

        integer, intent(in) :: mype, ri, rdir, size_l_v, nvars, nlays
        real, intent(out) :: dest_data(:,:,:,:)
        integer, pointer :: recv_index_l(:,:,:)
        integer, intent(in) :: flag

        include "mpif.h"

        integer :: c, r, lc, lr, l, v, j, index, request, status(MPI_STATUS_SIZE), error
        integer :: data_size, tag
        real, allocatable :: rarray(:)
        logical :: found

        allocate (rarray(size(dest_data)), stat=error)

              j = (ri - 1) * 3 + 1            ! 1st diminsion of *_index_l array has three components: 
                                              ! starting index, ending index, and distance
                                              ! between starting and ending indices

              data_size = size_l_v * recv_index_l(j+2,1,mype) * recv_index_l(j+2,2,mype)

!             tag = flag * 1000000 + rdir * 1000 + mype
              tag = flag * 1000 + mype

              call mpi_recv (rarray(1:data_size), data_size, mpi_real, rdir, tag, mpi_comm_world, status, error)

              index = 0
              do v = 1, nvars
                 do l = 1, nlays
                    do r = recv_index_l(j,2,mype), recv_index_l(j+1,2,mype)
                       do c = recv_index_l(j,1,mype), recv_index_l(j+1,1,mype)
                          index = index + 1
                          dest_data(c,r,l,v) = rarray(index)
                       end do
                    end do
                  end do
              end do

        deallocate (rarray)

        end subroutine se_twoway_recv4 

! -----------------------------------------------------------------------------
        subroutine compute_ngb (mype, loc_ngb, npcol, nprow)

        integer, intent(in)  :: mype, npcol, nprow
        integer, intent(out) :: loc_ngb(0:)

        integer :: loci, locj, pos, i, j
        integer, parameter :: conv(9) = (/ 0, 1, 2, 3, 5, 6, 7, 8, 4 /)

        loci = mype / npcol
        locj = mod ( mype, npcol )
        pos = 0
        do i = loci-1, loci+1
           do j = locj-1, locj+1
              if ( ( i .ne. loci ) .or. ( j .ne. locj ) ) then
                 pos = pos + 1
                 if ( ( i .ge. 0 ) .and. ( i .lt. nprow ) .and.
     &                ( j .ge. 0 ) .and. ( j .lt. npcol ) ) then
                    loc_ngb( conv( pos ) ) = i * npcol + j
                 else
                    loc_ngb( conv( pos ) ) = -1
                 end if
              else
                 loc_ngb( 4 ) = mype
              end if
           end do
        end do

        end subroutine compute_ngb

! -----------------------------------------------------------------------------
        subroutine loc_index (pe, list, dir, ind)

        integer, intent(in)  :: pe, list(0:)
        integer, intent(out) :: dir, ind

        logical :: found
        integer :: n

        found = .false.
        n = 0
        dir = -1
        ind = -1
        do while ((.not. found) .and. (n < list(0)))
           n = n + 1
           if (list(n) == pe) then
              found = .true.
              dir = list(n)
              ind = n
           end if
        end do

        end subroutine loc_index

      end module se_twoway_comm_module
