
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
! $Header: /project/work/rep/STENEX/src/se_snl/se_reconfig_grid_module.f,v 1.2 2006/02/15 15:04:29 yoj Exp $

! what(1) key, module and SID; SCCS file; date and time of last delta:
! %W% %P% %G% %U%

! --------------------------------------------------------------------------
! Purpose:
!
!   use F90 interface feature to achieve "faked" polymorphism for grid
! reconfigureation routines which consists of two major parts: se_grid_to_grid
! and se_reconfig_data_copy. The former one is to determine data mapping with
! respect of grid reconfiguration. The latter routine is to conduct messages
! sending and receiving.
!
! Revision history:
!
!   Orginal version: 02/27/01 by David Wong
!                    03/06/02 David Wong
!                      -- use blocking communication scheme
!                      -- use array copy mechanism when communicates to itself
! --------------------------------------------------------------------------

        module se_reconfig_grid_module

        implicit none

        interface se_grid_to_grid
          module procedure se_grid_to_grid2,
     &                     se_grid_to_grid3,
     &                     se_grid_to_grid4
        end interface

        interface se_reconfig_data_copy
          module procedure se_reconfig_data_copy2,
     &                     se_reconfig_data_copy3,
     &                     se_reconfig_data_copy4
        end interface

        contains

C -----------------------------------------------------------------------------
        subroutine se_grid_to_grid2 (nprow1, npcol1, map1, data1,
     &                               nprow2, npcol2, map2, data2)

        use se_reconfig_grid_info_ext
        use se_pe_info_ext

        implicit none

        include "mpif.h"

        integer, intent(in) :: map1(:,:,:), map2(:,:,:)
        real, intent(inout) :: data1(:,:)
        real, intent(inout) :: data2(:,:)
        integer, intent(in) :: nprow1, npcol1, nprow2, npcol2

        integer :: mype
        integer :: tpe, mpe, dpe, error, block, i, j, allocate_status
        integer :: nprow1npcol1, nprow2npcol2

        logical :: intersect

        nprow1npcol1 = nprow1 * npcol1
        nprow2npcol2 = nprow2 * npcol2
c       mype = se_my_pe
        mype = se_myworker_pe

C -- allocate data
        allocate (se_grid1_map(2,2,0:nprow1npcol1-1),stat=allocate_status)
        if (allocate_status .ne. 0) then
           print *, ' Allocation error '
           stop
        end if

        allocate (se_grid2_map(2,2,0:nprow2npcol2-1),stat=allocate_status)
        if (allocate_status .ne. 0) then
           print *, ' Allocation error '
           stop
        end if

        se_grid1_map = map1
        se_grid2_map = map2

C -- allocate data
        allocate (se_reconfig_grid_send_ind(2, 4, 0:nprow2npcol2-1), 
     &            stat=allocate_status)
        if (allocate_status .ne. 0) then
           print *, ' Subroutine SE_SUBGD_INDEX: ',
     &              ' allocation erorr in se_reconfig_grid_send_ind'
           stop
        end if

        allocate (se_reconfig_grid_recv_ind(2, 4, 0:nprow1npcol1-1), 
     &            stat=allocate_status)
        if (allocate_status .ne. 0) then
           print *, ' Subroutine SE_SUBGD_INDEX: ',
     &              ' allocation erorr in se_reconfig_grid_recv_ind'
           stop
        end if

        allocate (se_reconfig_grid_send(0:nprow2npcol2-1), 
     &            stat=allocate_status)
        if (allocate_status .ne. 0) then
           print *, ' Subroutine SE_SUBGD_INDEX: ',
     &              'allocation erorr in se_reconfig_grid_send'
           stop
        end if

        allocate (se_reconfig_grid_recv(0:nprow1npcol1-1), 
     &            stat=allocate_status)
        if (allocate_status .ne. 0) then
           print *, ' Subroutine SE_SUBGD_INDEX: ',
     &              'allocation erorr in se_reconfig_grid_recv'
           stop
        end if

        se_reconfig_grid_send_ind_ptr => se_reconfig_grid_send_ind
        se_reconfig_grid_recv_ind_ptr => se_reconfig_grid_recv_ind
        se_reconfig_grid_send_ptr => se_reconfig_grid_send
        se_reconfig_grid_recv_ptr => se_reconfig_grid_recv

        se_reconfig_grid_send = -1
        if (mype .lt. nprow1npcol1) then
           do i = 0, nprow2npcol2-1
              intersect = .true.

              if (     (se_grid1_map(1,1,mype) .gt. se_grid2_map(2,1,i))
     &            .or. (se_grid1_map(2,1,mype) .lt. se_grid2_map(1,1,i))) then
                 intersect = .false.
              else
                  if (     (se_grid1_map(1,2,mype) .gt. se_grid2_map(2,2,i))
     &                .or. (se_grid1_map(2,2,mype) .lt. se_grid2_map(1,2,i))) 
     &               then
                     intersect = .false.
                  end if
              end if

              if (intersect) then
                 se_reconfig_grid_send_ind(1,1,i) = max(se_grid1_map(1,1,mype),
     &                         se_grid2_map(1,1,i)) - se_grid1_map(1,1,mype) + 1
                 se_reconfig_grid_send_ind(2,1,i) = min(se_grid1_map(2,1,mype),
     &                         se_grid2_map(2,1,i)) - se_grid1_map(1,1,mype) + 1
                 se_reconfig_grid_send_ind(1,2,i) = max(se_grid1_map(1,2,mype),
     &                         se_grid2_map(1,2,i)) - se_grid1_map(1,2,mype) + 1
                 se_reconfig_grid_send_ind(2,2,i) = min(se_grid1_map(2,2,mype),
     &                         se_grid2_map(2,2,i)) - se_grid1_map(1,2,mype) + 1
                 se_reconfig_grid_send(i) = i
              else
                 se_reconfig_grid_send(i) = -1
              end if
           end do
         end if

C -- determine data is going to receive from which PE, and corresponding local
C    index

        se_reconfig_grid_recv = -1

        if (mype .lt. nprow2npcol2) then
           do i = 0, nprow1npcol1-1
              intersect = .true.

              if (     (se_grid1_map(1,1,i) .gt. se_grid2_map(2,1,mype))
     &            .or. (se_grid1_map(2,1,i) .lt. se_grid2_map(1,1,mype))) then
                 intersect = .false.
              else
                 if (     (se_grid1_map(1,2,i) .gt. se_grid2_map(2,2,mype))
     &               .or. (se_grid1_map(2,2,i) .lt. se_grid2_map(1,2,mype)))then
                    intersect = .false.
                 end if
              end if

              if (intersect) then
                 se_reconfig_grid_recv_ind(1,1,i) = max(se_grid1_map(1,1,i), 
     &                      se_grid2_map(1,1,mype)) - se_grid2_map(1,1,mype) + 1
                 se_reconfig_grid_recv_ind(2,1,i) = min(se_grid1_map(2,1,i), 
     &                      se_grid2_map(2,1,mype)) - se_grid2_map(1,1,mype) + 1
                 se_reconfig_grid_recv_ind(1,2,i) = max(se_grid1_map(1,2,i),
     &                      se_grid2_map(1,2,mype)) - se_grid2_map(1,2,mype) + 1
                 se_reconfig_grid_recv_ind(2,2,i) = min(se_grid1_map(2,2,i),
     &                      se_grid2_map(2,2,mype)) - se_grid2_map(1,2,mype) + 1
                 se_reconfig_grid_recv(i) = i
              end if
           end do
        end if

        call se_reconfig_data_copy2 (npcol1, nprow1, data1, 
     &                               npcol2, nprow2, data2)

        deallocate (se_grid1_map)
        deallocate (se_grid2_map)

        return
        end subroutine se_grid_to_grid2

C -----------------------------------------------------------------------------
        subroutine se_grid_to_grid3 (nprow1, npcol1, map1, data1,
     &                               nprow2, npcol2, map2, data2)

        use se_reconfig_grid_info_ext
        use se_pe_info_ext

        implicit none

        include "mpif.h"

        integer, intent(in) :: map1(:,:,:), map2(:,:,:)
        real, intent(inout) :: data1(:,:,:)
        real, intent(inout) :: data2(:,:,:)
        integer, intent(in) :: nprow1, npcol1, nprow2, npcol2

        integer :: mype
        integer :: tpe, mpe, dpe, error, block, i, j, allocate_status
        integer :: nprow1npcol1, nprow2npcol2

        logical :: intersect

        nprow1npcol1 = nprow1 * npcol1
        nprow2npcol2 = nprow2 * npcol2
c       mype = se_my_pe
        mype = se_myworker_pe

C -- allocate data
        allocate (se_grid1_map(2,2,0:nprow1npcol1-1),stat=allocate_status)
        if (allocate_status .ne. 0) then
           print *, ' Allocation error '
           stop
        end if

        allocate (se_grid2_map(2,2,0:nprow2npcol2-1),stat=allocate_status)
        if (allocate_status .ne. 0) then
           print *, ' Allocation error '
           stop
        end if

        se_grid1_map = map1
        se_grid2_map = map2

C -- allocate data
        if (.not. allocated(se_reconfig_grid_send_ind)) then
           allocate (se_reconfig_grid_send_ind(2, 4, 0:nprow2npcol2-1), 
     &               stat=allocate_status)
           if (allocate_status .ne. 0) then
              print *, ' Subroutine SE_SUBGD_INDEX: ',
     &                 ' allocation erorr in se_reconfig_grid_send_ind'
              stop
           end if

           allocate (se_reconfig_grid_recv_ind(2, 4, 0:nprow1npcol1-1), 
     &               stat=allocate_status)
           if (allocate_status .ne. 0) then
              print *, ' Subroutine SE_SUBGD_INDEX: ',
     &                 ' allocation erorr in se_reconfig_grid_recv_ind'
              stop
           end if

           allocate (se_reconfig_grid_send(0:nprow2npcol2-1), 
     &               stat=allocate_status)
           if (allocate_status .ne. 0) then
              print *, ' Subroutine SE_SUBGD_INDEX: ',
     &                 'allocation erorr in se_reconfig_grid_send'
              stop
           end if

           allocate (se_reconfig_grid_recv(0:nprow1npcol1-1), 
     &               stat=allocate_status)
           if (allocate_status .ne. 0) then
              print *, ' Subroutine SE_SUBGD_INDEX: ',
     &                 'allocation erorr in se_reconfig_grid_recv'
              stop
           end if

           se_reconfig_grid_send_ind_ptr => se_reconfig_grid_send_ind
           se_reconfig_grid_recv_ind_ptr => se_reconfig_grid_recv_ind
           se_reconfig_grid_send_ptr => se_reconfig_grid_send
           se_reconfig_grid_recv_ptr => se_reconfig_grid_recv

        end if

        se_reconfig_grid_send = -1
        if (mype .lt. nprow1npcol1) then
           do i = 0, nprow2npcol2-1
              intersect = .true.

              if (     (se_grid1_map(1,1,mype) .gt. se_grid2_map(2,1,i))
     &            .or. (se_grid1_map(2,1,mype) .lt. se_grid2_map(1,1,i))) then
                 intersect = .false.
              else
                  if (     (se_grid1_map(1,2,mype) .gt. se_grid2_map(2,2,i))
     &                .or. (se_grid1_map(2,2,mype) .lt. se_grid2_map(1,2,i))) 
     &                then
                     intersect = .false.
                  end if
              end if

              if (intersect) then
                 se_reconfig_grid_send_ind(1,1,i) = max(se_grid1_map(1,1,mype),
     &                         se_grid2_map(1,1,i)) - se_grid1_map(1,1,mype) + 1
                 se_reconfig_grid_send_ind(2,1,i) = min(se_grid1_map(2,1,mype),
     &                         se_grid2_map(2,1,i)) - se_grid1_map(1,1,mype) + 1
                 se_reconfig_grid_send_ind(1,2,i) = max(se_grid1_map(1,2,mype),
     &                         se_grid2_map(1,2,i)) - se_grid1_map(1,2,mype) + 1
                 se_reconfig_grid_send_ind(2,2,i) = min(se_grid1_map(2,2,mype),
     &                         se_grid2_map(2,2,i)) - se_grid1_map(1,2,mype) + 1
                 se_reconfig_grid_send(i) = i
              else
                 se_reconfig_grid_send(i) = -1
              end if
           end do
        end if

C -- determine data is going to receive from which PE, and corresponding local
C    index

        se_reconfig_grid_recv = -1

        if (mype .lt. nprow2npcol2) then
           do i = 0, nprow1npcol1-1
              intersect = .true.

              if (     (se_grid1_map(1,1,i) .gt. se_grid2_map(2,1,mype))
     &            .or. (se_grid1_map(2,1,i) .lt. se_grid2_map(1,1,mype))) then
                 intersect = .false.
              else
                 if (     (se_grid1_map(1,2,i) .gt. se_grid2_map(2,2,mype))
     &               .or. (se_grid1_map(2,2,i) .lt. se_grid2_map(1,2,mype)))
     &                then
                    intersect = .false.
                 end if
              end if

              if (intersect) then
                 se_reconfig_grid_recv_ind(1,1,i) = max(se_grid1_map(1,1,i), 
     &                      se_grid2_map(1,1,mype)) - se_grid2_map(1,1,mype) + 1
                 se_reconfig_grid_recv_ind(2,1,i) = min(se_grid1_map(2,1,i), 
     &                      se_grid2_map(2,1,mype)) - se_grid2_map(1,1,mype) + 1
                 se_reconfig_grid_recv_ind(1,2,i) = max(se_grid1_map(1,2,i),
     &                      se_grid2_map(1,2,mype)) - se_grid2_map(1,2,mype) + 1
                 se_reconfig_grid_recv_ind(2,2,i) = min(se_grid1_map(2,2,i),
     &                      se_grid2_map(2,2,mype)) - se_grid2_map(1,2,mype) + 1
                 se_reconfig_grid_recv(i) = i
              end if
           end do
        end if

        call se_reconfig_data_copy3 (npcol1, nprow1, data1, 
     &                               npcol2, nprow2, data2)

        deallocate (se_grid1_map)
        deallocate (se_grid2_map)

        return
        end subroutine se_grid_to_grid3

C -----------------------------------------------------------------------------
        subroutine se_grid_to_grid4 (nprow1, npcol1, map1, data1,
     &                               nprow2, npcol2, map2, data2)

        use se_reconfig_grid_info_ext
        use se_pe_info_ext

        implicit none

        include "mpif.h"

        integer, intent(in) :: map1(:,:,:), map2(:,:,:)
        real, intent(inout) :: data1(:,:,:,:)
        real, intent(inout) :: data2(:,:,:,:)
        integer, intent(in) :: nprow1, npcol1, nprow2, npcol2

        integer :: mype
        integer :: tpe, mpe, dpe, error, block, i, j, allocate_status
        integer :: nprow1npcol1, nprow2npcol2

        logical, save :: firstime
        data firstime / .true. /

        logical :: intersect

        nprow1npcol1 = nprow1 * npcol1
        nprow2npcol2 = nprow2 * npcol2
c       mype = se_my_pe
        mype = se_myworker_pe

C -- allocate data
        allocate (se_grid1_map(2,2,0:nprow1npcol1-1),stat=allocate_status)
        if (allocate_status .ne. 0) then
           print *, ' Allocation error '
           stop
        end if

        allocate (se_grid2_map(2,2,0:nprow2npcol2-1),stat=allocate_status)
        if (allocate_status .ne. 0) then
           print *, ' Allocation error '
           stop
        end if

        se_grid1_map = map1
        se_grid2_map = map2

        if (firstime) then
           firstime = .false.
C -- allocate data
           allocate (se_reconfig_grid_send_ind(2, 4, 0:nprow2npcol2-1),
     &               stat=allocate_status)
           if (allocate_status .ne. 0) then
              print *, ' Subroutine se_grid_to_grid4: ',
     &                 ' allocation erorr in se_reconfig_grid_send_ind'
              stop
           end if

           allocate (se_reconfig_grid_recv_ind(2, 4, 0:nprow1npcol1-1),
     &               stat=allocate_status)
           if (allocate_status .ne. 0) then
              print *, ' Subroutine se_grid_to_grid4: ',
     &                 ' allocation erorr in se_reconfig_grid_recv_ind'
              stop
           end if

           allocate (se_reconfig_grid_send(0:nprow2npcol2-1),
     &               stat=allocate_status)
           if (allocate_status .ne. 0) then
              print *, ' Subroutine SE_SUBGD_INDEX: ',
     &                 'allocation erorr in se_reconfig_grid_send'
              stop
           end if

           allocate (se_reconfig_grid_recv(0:nprow1npcol1-1),
     &               stat=allocate_status)
           if (allocate_status .ne. 0) then
              print *, ' Subroutine SE_SUBGD_INDEX: ',
     &                 'allocation erorr in se_reconfig_grid_recv'
              stop
           end if

           se_reconfig_grid_send_ind_ptr => se_reconfig_grid_send_ind
           se_reconfig_grid_recv_ind_ptr => se_reconfig_grid_recv_ind
           se_reconfig_grid_send_ptr => se_reconfig_grid_send
           se_reconfig_grid_recv_ptr => se_reconfig_grid_recv
        end if

        se_reconfig_grid_send = -1
        if (mype .lt. nprow1npcol1) then
           do i = 0, nprow2npcol2-1
              intersect = .true.

              if (     (se_grid1_map(1,1,mype) .gt. se_grid2_map(2,1,i))
     &            .or. (se_grid1_map(2,1,mype) .lt. se_grid2_map(1,1,i))) then
                 intersect = .false.
              else
                  if (     (se_grid1_map(1,2,mype) .gt. se_grid2_map(2,2,i))
     &                .or. (se_grid1_map(2,2,mype) .lt. se_grid2_map(1,2,i))) 
     &                then
                     intersect = .false.
                  end if
              end if

              if (intersect) then
                 se_reconfig_grid_send_ind(1,1,i) = max(se_grid1_map(1,1,mype),
     &                         se_grid2_map(1,1,i)) - se_grid1_map(1,1,mype) + 1
                 se_reconfig_grid_send_ind(2,1,i) = min(se_grid1_map(2,1,mype),
     &                         se_grid2_map(2,1,i)) - se_grid1_map(1,1,mype) + 1
                 se_reconfig_grid_send_ind(1,2,i) = max(se_grid1_map(1,2,mype),
     &                         se_grid2_map(1,2,i)) - se_grid1_map(1,2,mype) + 1
                 se_reconfig_grid_send_ind(2,2,i) = min(se_grid1_map(2,2,mype),
     &                         se_grid2_map(2,2,i)) - se_grid1_map(1,2,mype) + 1
                 se_reconfig_grid_send(i) = i
              else
                 se_reconfig_grid_send(i) = -1
              end if
           end do
         end if

C -- determine data is going to receive from which PE, and corresponding local
C    index

        se_reconfig_grid_recv = -1

        if (mype .lt. nprow2npcol2) then
           do i = 0, nprow1npcol1-1
              intersect = .true.

              if (     (se_grid1_map(1,1,i) .gt. se_grid2_map(2,1,mype))
     &            .or. (se_grid1_map(2,1,i) .lt. se_grid2_map(1,1,mype))) then
                 intersect = .false.
              else
                 if (     (se_grid1_map(1,2,i) .gt. se_grid2_map(2,2,mype))
     &               .or. (se_grid1_map(2,2,i) .lt. se_grid2_map(1,2,mype)))
     &               then
                    intersect = .false.
                 end if
              end if

              if (intersect) then
                 se_reconfig_grid_recv_ind(1,1,i) = max(se_grid1_map(1,1,i),
     &                      se_grid2_map(1,1,mype)) - se_grid2_map(1,1,mype) + 1
                 se_reconfig_grid_recv_ind(2,1,i) = min(se_grid1_map(2,1,i),
     &                      se_grid2_map(2,1,mype)) - se_grid2_map(1,1,mype) + 1
                 se_reconfig_grid_recv_ind(1,2,i) = max(se_grid1_map(1,2,i),
     &                      se_grid2_map(1,2,mype)) - se_grid2_map(1,2,mype) + 1
                 se_reconfig_grid_recv_ind(2,2,i) = min(se_grid1_map(2,2,i),
     &                      se_grid2_map(2,2,mype)) - se_grid2_map(1,2,mype) + 1
                 se_reconfig_grid_recv(i) = i
              end if
           end do
        end if

        call se_reconfig_data_copy4 (npcol1, nprow1, data1,
     &                               npcol2, nprow2, data2)

        deallocate (se_grid1_map)
        deallocate (se_grid2_map)

        return
        end subroutine se_grid_to_grid4

C --------------------------------------------------------------------------
        subroutine se_reconfig_data_copy2 (nprow1, npcol1, data1, 
     &                                     nprow2, npcol2, data2)

        use se_reconfig_grid_info_ext
        use se_pe_info_ext
        use se_data_send_module
        use se_data_recv_module

        implicit none

        include "mpif.h"

        integer, intent(in) :: nprow1, npcol1, nprow2, npcol2
        real, intent(in)  :: data1(:, :)
        real, intent(out) :: data2(:, :)

        integer :: dir, sdir, rdir, tag
        integer :: request, status(MPI_STATUS_SIZE), error

        do dir = 0, max(nprow1*npcol1-1, nprow2*npcol2-1)

           if (dir .lt. nprow2*npcol2) then
              sdir = dir

c             if ((se_reconfig_grid_send(sdir) .eq. se_my_pe) .and.
              if ((se_reconfig_grid_send(sdir) .eq. se_myworker_pe) .and.
     &            (dir .lt. nprow1*npcol1)) then

                 data2( se_reconfig_grid_recv_ind(1,1,sdir)
     &                 :se_reconfig_grid_recv_ind(2,1,sdir),
     &                  se_reconfig_grid_recv_ind(1,2,sdir)
     &                 :se_reconfig_grid_recv_ind(2,2,sdir))
     &           =
     &           data1( se_reconfig_grid_send_ind(1,1,sdir)
     &                 :se_reconfig_grid_send_ind(2,1,sdir),
     &                  se_reconfig_grid_send_ind(1,2,sdir)
     &                 :se_reconfig_grid_send_ind(2,2,sdir))

              else

                 if (se_reconfig_grid_send(sdir) .ge. 0) then
c                   tag = (sdir + 1) * 10000 + se_my_pe
                    tag = (sdir + 1) * 10000 + se_myworker_pe
                    call se_data_send (data1, se_reconfig_grid_send_ind_ptr,
     &                                 se_reconfig_grid_send_ptr, sdir, tag, 
     &                                 request)
                 end if
              end if
           end if

           if (dir .lt. nprow1*npcol1) then
              rdir = dir

              if ((se_reconfig_grid_recv(rdir) .ge. 0) .and.
     &            (se_reconfig_grid_recv(rdir) .ne. se_myworker_pe)) then
c    &            (se_reconfig_grid_recv(rdir) .ne. se_my_pe)) then
                 tag = (se_myworker_pe + 1) * 10000 + rdir
c                tag = (se_my_pe + 1) * 10000 + rdir
                 call se_data_recv (data2, se_reconfig_grid_recv_ind_ptr,
     &                              se_reconfig_grid_recv_ptr, rdir, tag)
              end if
           end if

c          if ((dir .lt. nprow2*npcol2) .and.
c    &         (se_reconfig_grid_send(sdir) .ge. 0)) then
c             call mpi_wait (request, status, error)
c          end if

        end do

        return
        end subroutine se_reconfig_data_copy2

C --------------------------------------------------------------------------
        subroutine se_reconfig_data_copy3 (nprow1, npcol1, data1, 
     &                                     nprow2, npcol2, data2)

        use se_reconfig_grid_info_ext
        use se_pe_info_ext
        use se_domain_info_ext
        use se_data_send_module
        use se_data_recv_module

        implicit none

        include "mpif.h"

        integer, intent(in) :: nprow1, npcol1, nprow2, npcol2
        real, intent(in)  :: data1(:, :, :)
        real, intent(out) :: data2(:, :, :)

        integer :: i, dir, sdir, rdir, tag
        integer :: request, status(MPI_STATUS_SIZE), error

        do i = 0, nprow2*npcol2-1
           se_reconfig_grid_send_ind(1,3,i) = 1
           se_reconfig_grid_send_ind(2,3,i) = size(data1,3)
        end do

        do i = 0, nprow1*npcol1-1
           se_reconfig_grid_recv_ind(1,3,i) = 1
           se_reconfig_grid_recv_ind(2,3,i) = size(data1,3)
        end do

        do dir = 0, max(nprow1*npcol1-1, nprow2*npcol2-1)

           if (dir .lt. nprow2*npcol2) then
              sdir = dir

c             if ((se_reconfig_grid_send(sdir) .eq. se_my_pe) .and.
              if ((se_reconfig_grid_send(sdir) .eq. se_myworker_pe) .and.
     &            (dir .lt. nprow1*npcol1)) then

                 data2( se_reconfig_grid_recv_ind(1,1,sdir)
     &                 :se_reconfig_grid_recv_ind(2,1,sdir),
     &                  se_reconfig_grid_recv_ind(1,2,sdir)
     &                 :se_reconfig_grid_recv_ind(2,2,sdir),
     &                  se_reconfig_grid_recv_ind(1,3,sdir)
     &                 :se_reconfig_grid_recv_ind(2,3,sdir))
     &           =
     &           data1( se_reconfig_grid_send_ind(1,1,sdir)
     &                 :se_reconfig_grid_send_ind(2,1,sdir),
     &                  se_reconfig_grid_send_ind(1,2,sdir)
     &                 :se_reconfig_grid_send_ind(2,2,sdir),
     &                  se_reconfig_grid_send_ind(1,3,sdir)
     &                 :se_reconfig_grid_send_ind(2,3,sdir))

              else

                 if (se_reconfig_grid_send(sdir) .ge. 0) then
c                   tag = (sdir + 1) * 10000 + se_my_pe
                    tag = (sdir + 1) * 10000 + se_myworker_pe

                    call se_data_send (data1, se_reconfig_grid_send_ind_ptr,
     &                                 se_reconfig_grid_send_ptr, sdir, tag, 
     &                                 request)
                 end if

              end if
           end if

           if (dir .lt. nprow1*npcol1) then
              rdir = dir
              if ((se_reconfig_grid_recv(rdir) .ge. 0) .and.
     &            (se_reconfig_grid_recv(rdir) .ne. se_myworker_pe)) then
c    &            (se_reconfig_grid_recv(rdir) .ne. se_my_pe)) then
c                tag = (se_my_pe + 1) * 10000 + rdir
                 tag = (se_myworker_pe + 1) * 10000 + rdir

                 call se_data_recv (data2, se_reconfig_grid_recv_ind_ptr,
     &                              se_reconfig_grid_recv_ptr, rdir, tag)
              end if
           end if

c          if ((dir .lt. nprow2*npcol2) .and. 
c    &         (se_reconfig_grid_send(sdir) .ge. 0)) then
c             call mpi_wait (request, status, error)
c          end if

        end do

        return
        end subroutine se_reconfig_data_copy3

C --------------------------------------------------------------------------
        subroutine se_reconfig_data_copy4 (nprow1, npcol1, data1, 
     &                                     nprow2, npcol2, data2)

        use se_reconfig_grid_info_ext
        use se_pe_info_ext
        use se_domain_info_ext
        use se_data_send_module
        use se_data_recv_module

        implicit none

        include "mpif.h"

        integer, intent(in) :: nprow1, npcol1, nprow2, npcol2
        real, intent(in)  :: data1(:, :, :, :)
        real, intent(out) :: data2(:, :, :, :)

        integer :: i, dir, sdir, rdir, tag
        integer :: request, status(MPI_STATUS_SIZE), error

        do i = 0, nprow2*npcol2-1
           se_reconfig_grid_send_ind(1,3,i) = 1
           se_reconfig_grid_send_ind(2,3,i) = size(data1,3)
           se_reconfig_grid_send_ind(1,4,i) = 1
           se_reconfig_grid_send_ind(2,4,i) = size(data1,4)
        end do

        do i = 0, nprow1*npcol1-1
           se_reconfig_grid_recv_ind(1,3,i) = 1
           se_reconfig_grid_recv_ind(2,3,i) = size(data1,3)
           se_reconfig_grid_recv_ind(1,4,i) = 1
           se_reconfig_grid_recv_ind(2,4,i) = size(data1,4)
        end do

        do dir = 0, max(nprow1*npcol1-1, nprow2*npcol2-1)

           if (dir .lt. nprow2*npcol2) then
              sdir = dir

c             if ((se_reconfig_grid_send(sdir) .eq. se_my_pe) .and.
              if ((se_reconfig_grid_send(sdir) .eq. se_myworker_pe) .and.
     &            (dir .lt. nprow1*npcol1)) then

                 data2( se_reconfig_grid_recv_ind(1,1,sdir)
     &                 :se_reconfig_grid_recv_ind(2,1,sdir),
     &                  se_reconfig_grid_recv_ind(1,2,sdir)
     &                 :se_reconfig_grid_recv_ind(2,2,sdir),
     &                  se_reconfig_grid_recv_ind(1,3,sdir)
     &                 :se_reconfig_grid_recv_ind(2,3,sdir),
     &                  se_reconfig_grid_recv_ind(1,4,sdir)
     &                 :se_reconfig_grid_recv_ind(2,4,sdir))
     &           =
     &           data1( se_reconfig_grid_send_ind(1,1,sdir)
     &                 :se_reconfig_grid_send_ind(2,1,sdir),
     &                  se_reconfig_grid_send_ind(1,2,sdir)
     &                 :se_reconfig_grid_send_ind(2,2,sdir),
     &                  se_reconfig_grid_send_ind(1,3,sdir)
     &                 :se_reconfig_grid_send_ind(2,3,sdir),
     &                  se_reconfig_grid_send_ind(1,4,sdir)
     &                 :se_reconfig_grid_send_ind(2,4,sdir))

              else

                 if (se_reconfig_grid_send(sdir) .ge. 0) then
c                   tag = (sdir + 1) * 10000 + se_my_pe
                    tag = (sdir + 1) * 10000 + se_myworker_pe

                    call se_data_send (data1, se_reconfig_grid_send_ind_ptr,
     &                                 se_reconfig_grid_send_ptr, sdir, tag, 
     &                                 request)
                 end if

              end if
           end if

           if (dir .lt. nprow1*npcol1) then
              rdir = dir

              if ((se_reconfig_grid_recv(rdir) .ge. 0) .and.
     &            (se_reconfig_grid_recv(rdir) .ne. se_myworker_pe)) then
c    &            (se_reconfig_grid_recv(rdir) .ne. se_my_pe)) then
c                tag = (se_my_pe + 1) * 10000 + rdir
                 tag = (se_myworker_pe + 1) * 10000 + rdir

                 call se_data_recv (data2, se_reconfig_grid_recv_ind_ptr,
     &                              se_reconfig_grid_recv_ptr, rdir, tag)
              end if
           end if

c          if ((dir .lt. nprow2*npcol2) .and.
c    &         (se_reconfig_grid_send(sdir) .ge. 0)) then
c             call mpi_wait (request, status, error)
c          end if

        end do

        return
        end subroutine se_reconfig_data_copy4

        end module se_reconfig_grid_module
