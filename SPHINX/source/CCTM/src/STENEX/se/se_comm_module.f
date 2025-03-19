
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
! $Header: /project/work/rep/STENEX/src/se_snl/se_comm_module.f,v 1.1 2004/03/26 16:16:47 yoj Exp $

! what(1) key, module and SID; SCCS file; date and time of last delta:
! %W% %P% %G% %U%

! --------------------------------------------------------------------------
! Purpose:
!
!   use F90 interface feature to achieve "faked" polymorphism for pe 
! communication routine and use F90 module feature to modulize pe communication
! functionality of the stencil exchange library
!
! Revision history:
!
!   Orginal version: 11/05/99 by David Wong
!          Modified: 08/24/11 by David Wong
!                      -- elminated data and geo orientation
!          Modified: 10/04/12 by David Wong
!                      -- extended to five dimensional array and made the third,
!                         and fourth dimension more flexible and only limited to
!                         layer and species
!          Modified: 11/01/18 by David Wong
!                      -- remove TAB character
! --------------------------------------------------------------------------

        module se_comm_module

        implicit none

        interface se_comm
          module procedure se_pe_comm1, 
     &                     se_pe_comm2, se_pe_comm2e, 
     &                     se_pe_comm3, se_pe_comm3e, se_pe_comm3s,
     &                     se_pe_comm4,
     &                     se_pe_comm5
        end interface

        contains

! -----------------------------------------------------------------------------
! Purpose:
!
!   determine the set of neighbouring processor(s) that my own processor needs 
!   to communicate with, base upon the input data dependency.
!
! Revision history:
!
!   Orginal version: 7/29/98 by David Wong 
!
!                    11/05/99 by David Wong
!                      -- recode the code using F90 syntax
!                    02/06/02 by David Wong
!                      -- adjust the communication direction if it is NE, SE,
!                         SW, or NW
!                    08/24/11 by David Wong
!                      -- removed se_ori_ext to eliminate data and geo orientation
!
! Subroutine parameter description:
!
!   In: dirstr    -- indicator of communication direction associated with
!                    the data dependency: 0 (without communication), 
!                                         1 (with communication)
!       send_to   -- processor number which data needs to be sent to
!       recv_from -- processor number which data is received from
!
! Local variable description:
!
!   rdirection -- input communication direction for receiveing
!   sdirection -- communication direction for sending
!   i, j, k    -- loop indexes
!
! Include file:
!
!   se_comm_info_ext
!
!     se_ngb_pe -- an array to indicate a communication with a certain 
!                  processor is required base upon near-neighbour communication 
!                  pattern: -1 denotes no communication is needed, and a non 
!                  -1 number denotes processor number with which communication 
!                  is formed
!
!   se_ori_ext
!   se_internal_util_module
! -----------------------------------------------------------------------------

        subroutine se_comm_pat (dirstr, send_to, recv_from)

        use se_comm_info_ext
!       use se_ori_ext
        use se_internal_util_module

        implicit none

        character (len = 16), intent(in) :: dirstr
        integer, intent(out) :: send_to (8), recv_from (8)

        integer :: i, j, k
        integer :: rdirection (8), sdirection(8)

! -- extract inform from input strings

        read (dirstr, 10) (rdirection(i), i=1, 8)
 10     format (8i2)

! -- make adjustment if communication direction is: NE, SE, SW, or NW

        do i = 2, 8, 2
           if (rdirection(i) .eq. 1) then
              rdirection(mod(i-1,8)) = 1
              rdirection(mod(i+1,8)) = 1
           end if
        end do

!       if (se_geo_ori .eq. 0) then
           call swap (rdirection(1), rdirection(5))
           call swap (rdirection(2), rdirection(4))
           call swap (rdirection(8), rdirection(6))
!       end if

! -- figuring out send direction pattern
        do i = 1, 8
           sdirection(i) = rdirection(mod(i+3,8)+1)
        end do

! -- determine where data is receiving from
! -- first: N, E, S, and W
        do i = 1, 7, 2
           if ((rdirection (i) .gt. 0) .and.
     &         (se_ngb_pe(i) .ge. 0)) then
              recv_from(i) = se_ngb_pe(i)
           else
              recv_from(i) = -1
           end if
        end do

! -- second: NE, SE, SW, NW, and their immediate neighbours
! --         for instance, NE's immediate neighbours are N, and E
        do i = 2, 8, 2
           if (rdirection(i) .gt. 0) then
              do j = i-1, i+1
                 k = j
                 if (k .eq. 9) then
                    k = 1
                  end if 
                  if (se_ngb_pe(k) .ge. 0) then
                     recv_from(k) = se_ngb_pe(k)
                  else
                     recv_from(k) = -1
                  end if
              end do
           else
              recv_from(i) = -1
           end if
        end do

! -- determine where data is sending to
! -- first: N, E, S, and W
        do i = 1, 7, 2
           if ((sdirection (i) .gt. 0) .and.
     &         (se_ngb_pe(i) .ge. 0)) then
              send_to(i) = se_ngb_pe(i)
           else
              send_to(i) = -1
           end if
        end do

! -- second: NE, SE, SW, NW, and their immediate neighbours
! --         for instance, NE's immediate neighbours are N, and E
        do i = 2, 8, 2
           if (sdirection(i) .gt. 0) then
              do j = i-1, i+1
                 k = j
                 if (k .eq. 9) then
                    k = 1
                  end if 
                  if (se_ngb_pe(k) .ge. 0) then
                     send_to(k) = se_ngb_pe(k)
                  else
                     send_to(k) = -1
                  end if
              end do
           else
              send_to(i) = -1
           end if
        end do

        return
        end subroutine se_comm_pat

! --------------------------------------------------------------------------
! Purpose:
!
!   program se_pe_comm[n][e] performs near-neighbour communication for a 
! n-dimensional data structure
!
! Revision history:
!
!   Orginal version: 9/15/98 by David Wong
!                    11/05/99 by David Wong
!                      -- recode the code using F90 syntax
!                    07/23/01 by David Wong
!                      -- redesign the message sending and receiving algorithm
!                    03/06/02 David Wong
!                      -- use blocking communication scheme
!                      -- use array copy mechanism when communicates to itself
!
! Subroutine parameter description:
!
!   In:  data    -- original data
!        dispstr -- displacement string
!        dirstr  -- indicator of communication direction
!                   0 (without communication), 1 (with communication)
!        str     -- an optional argument to indicate the starting index of 
!                   certain dimension
!        flag    -- to indicate se_pe_comm[n]e is used
!
!   Out: data    -- original data after communication
!
! Local variable description:
!
!    send_to       -- processor number which data needs to be sent to
!    send_to_ptr   -- a F90 pointer (alias) of send_to
!    recv_from     -- processor number which data is recvd from
!    recv_from_ptr -- a F90 pointer (alias) of recv_from
!    sdir, rdir    -- loop indexes which indicate send to or recvd from
!    sind          -- store low and high index of each dimension for sending
!                     process
!    sind_ptr      -- a F90 pointer (alias) of sind
!    rind          -- store low and high index of each dimension for receiving
!                     process
!    rind_ptr      -- a F90 pointer (alias) of rind
!    shift         -- an array to hold the amount of index shifting due to
!                     starting index is 1 in a subroutine
!    num_shift     -- number of shifting
!    loc_str       -- a local copy of str
!
! Include file:
!
!   se_data_send_module
!   se_data_recv_module
!   se_internal_util_module
!
! Subroutine/Function call:
!
!   se_comm_pat
!   se_up_low[1]
!
! --------------------------------------------------------------------------

        subroutine se_pe_comm1 (data, dispstr, dirstr, str)

        use se_data_send_module
        use se_data_recv_module
        use se_internal_util_module
        use se_pe_info_ext

        implicit none

        include "mpif.h"

        real, intent(inout) :: data(:)
        character (len = 16), intent(in) :: dirstr
        character (len = 12), intent(in) :: dispstr
        character (len = *), optional, intent(in) :: str

        integer, target :: sind(2,8), rind(2,8)
        integer, pointer :: sind_ptr(:,:), rind_ptr(:,:)
        integer, target :: send_to(8), recv_from(8)
        integer, pointer :: send_to_ptr(:), recv_from_ptr(:)
        integer :: sdir, rdir
        integer :: shift(2), num_shift
        character (len = 80) :: loc_str
        integer :: request, status(MPI_STATUS_SIZE), error

        if (present(str)) then
           loc_str = str
           call se_string_to_integer (loc_str, shift, num_shift)
        else
           num_shift = 0
        end if

        call se_comm_pat (dirstr, send_to, recv_from)

        call se_up_low1 (dispstr, sind, rind, shift, num_shift)

        send_to_ptr => send_to
        recv_from_ptr => recv_from
        sind_ptr => sind
        rind_ptr => rind

        do sdir = 1, 8, 2

           rdir = mod((sdir + 3), 8) + 1

           if (send_to(sdir) .eq. se_my_pe) then

              data(rind(1,rdir):rind(2,rdir)) = data(sind(1,sdir):sind(2,sdir))

           else 

              if (send_to(sdir) .ge. 0) then
                 call se_data_send (data, sind_ptr, send_to_ptr, sdir, sdir, 
     $                              request)
              end if

              if ((recv_from(rdir) .ge. 0) .and. 
     $            (recv_from(rdir) .ne. se_my_pe)) then
                 call se_data_recv (data, rind_ptr, recv_from_ptr, rdir, sdir)
              end if

!             if (send_to(sdir) .ge. 0) then
!                call mpi_wait (request, status, error)
!             end if

           end if
        end do

        return
        end subroutine se_pe_comm1

! --------------------------------------------------------------------------
! Purpose:
!
!   perform near-neighbour communication for a 2-D data structure with 2-D
! decomposition (se_pe_comm2) or 1-D decomposition (se_pe_comm2e)
!
! Revision history:
!
!   Orginal version: 8/3/98 by David Wong
!                    11/05/99 by David Wong
!                      -- recode the code using F90 syntax
!                    07/23/01 by David Wong
!                      -- redesign the message sending and receiving algorithm
!                    03/06/02 David Wong
!                      -- use blocking communication scheme
!                      -- use array copy mechanism when communicates to itself
!
! Subroutine parameter description:
!
!   In:  data    -- original data
!        dispstr -- displacement string
!        dirstr  -- indicator of communication direction
!                   0 (without communication), 1 (with communication)
!        flag    -- to indicate se_pe_comm2e is used
!        str     -- an optional argument to indicate the starting index of
!                   certain dimension
!
!   Out: data    -- original data after communication
!
! Local variable description:
!
!    send_to       -- processor number which data needs to be sent to
!    send_to_ptr   -- a F90 pointer (alias) of send_to
!    recv_from     -- processor number which data is recvd from
!    recv_from_ptr -- a F90 pointer (alias) of recv_from
!    sdir, rdir    -- loop indexes which indicate send to or recvd from
!    sind          -- store low and high index of each dimension for sending
!                     process
!    sind_ptr      -- a F90 pointer (alias) of sind
!    rind          -- store low and high index of each dimension for receiving
!                     process
!    rind_ptr      -- a F90 pointer (alias) of rind
!    shift         -- an array to hold the amount of index shifting due to
!                     starting index is 1 in a subroutine
!    num_shift     -- number of shifting
!    loc_str       -- a local copy of str
!
! Include file:
!
!    se_data_send_module
!    se_data_recv_module
!
! Subroutine/Function call:
!
!   se_comm_pat 
!   se_up_low2 
!
! --------------------------------------------------------------------------

        subroutine se_pe_comm2 (data, dispstr, dirstr, str)

        use se_data_send_module
        use se_data_recv_module
        use se_internal_util_module
        use se_pe_info_ext

        implicit none

        include "mpif.h"

        real, intent(inout) :: data(:,:)
        character (len = 16), intent(in) :: dirstr
        character (len = 12), intent(in) :: dispstr
        character (len = *), optional, intent(in) :: str

        integer, target :: send_to(8), recv_from(8)
        integer, pointer :: send_to_ptr(:), recv_from_ptr(:)
        integer, target :: sind(2,2,8), rind(2,2,8)
        integer, pointer :: sind_ptr(:,:,:), rind_ptr(:,:,:)
        integer :: sdir, rdir
        integer :: shift(4), num_shift
        character (len = 80) :: loc_str
        integer :: request, status(MPI_STATUS_SIZE), error

        if (present(str)) then
           loc_str = str
           shift(2:4:2) = 1
           call se_string_to_integer (loc_str, shift, num_shift)
        else
           num_shift = 0
        end if

        call se_comm_pat (dirstr, send_to, recv_from)

        send_to_ptr => send_to
        recv_from_ptr => recv_from

        call se_up_low2 (dispstr, sind, rind, shift, num_shift)

        sind_ptr => sind
        rind_ptr => rind

        do sdir = 1, 8

           rdir = mod((sdir + 3), 8) + 1

           if (send_to(sdir) .eq. se_my_pe) then

              data(rind(1,1,rdir):rind(2,1,rdir),rind(1,2,rdir):rind(2,2,rdir))
     $        =
     $        data(sind(1,1,sdir):sind(2,1,sdir),sind(1,2,sdir):sind(2,2,sdir))

           else

              if (send_to(sdir) .ge. 0) then
                 call se_data_send (data, sind_ptr, send_to_ptr, sdir, sdir, 
     $                              request)
              end if

              if ((recv_from(rdir) .ge. 0) .and.
     $            (recv_from(rdir) .ne. se_my_pe)) then
                 call se_data_recv (data, rind_ptr, recv_from_ptr, rdir, sdir)
              end if

!             if (send_to(sdir) .ge. 0) then
!                call mpi_wait (request, status, error)
!             end if

           end if

        end do

        return
        end subroutine se_pe_comm2

! -----------------------------------------------------------------------------
        subroutine se_pe_comm2e (data, dispstr, dirstr, flag, str)

        use se_data_send_module
        use se_data_recv_module
        use se_internal_util_module
        use se_pe_info_ext

        implicit none

        include "mpif.h"

        real, intent(inout) :: data(:,:)
        character (len = 16), intent(in) :: dirstr
        character (len = 12), intent(in) :: dispstr
        integer, intent(in) :: flag
        character (len = *), optional, intent(in) :: str

        integer, target :: send_to(8), recv_from(8)
        integer, pointer :: send_to_ptr(:), recv_from_ptr(:)
        integer, target :: sind(2,8), rind(2,8)
        integer, pointer :: sind_ptr(:,:), rind_ptr(:,:)
        integer :: sdir, rdir
        integer :: shift(4), num_shift
        character (len = 80) :: loc_str
        integer :: request, status(MPI_STATUS_SIZE), error

        if (present(str)) then
           loc_str = str
           shift(2:4:2) = 1
           call se_string_to_integer (loc_str, shift, num_shift)
        else
           num_shift = 0
        end if

        call se_comm_pat (dirstr, send_to, recv_from)

        send_to_ptr => send_to
        recv_from_ptr => recv_from

        call se_up_low1 (dispstr, sind, rind, shift, num_shift)

        sind_ptr => sind
        rind_ptr => rind

        do sdir = 1, 8, 2

           rdir = mod((sdir + 3), 8) + 1

           if (send_to(sdir) .eq. se_my_pe) then

              data(rind(1,rdir):rind(2,rdir),:) 
     $        =
     $        data(sind(1,sdir):sind(2,sdir),:)

           else

              if (send_to(sdir) .ge. 0) then
                 call se_data_send (data, sind_ptr, send_to_ptr, sdir, sdir,
     $                              request)
              end if

              if ((recv_from(rdir) .ge. 0) .and.
     $            (recv_from(rdir) .ne. se_my_pe)) then
                 call se_data_recv (data, rind_ptr, recv_from_ptr, rdir, sdir)
              end if

!             if (send_to(sdir) .ge. 0) then
!                call mpi_wait (request, status, error)
!             end if

           end if
        end do

        return
        end subroutine se_pe_comm2e

! --------------------------------------------------------------------------
! Purpose:
!
!   perform near-neighbour communication for a 3-D data structure with 2-D
! decomposition (se_pe_comm3) or 1-D decomposition (se_pe_comm3e)
!
! Revision history:
!
!   Orginal version: 7/29/98 by David Wong 
!                    11/05/99 by David Wong
!                      -- recode the code using F90 syntax
!                    07/23/01 by David Wong
!                      -- redesign the message sending and receiving algorithm
!                    03/06/02 David Wong
!                      -- use blocking communication scheme
!                      -- use array copy mechanism when communicates to itself
!
! Subroutine parameter description:
!
!   In:  data    -- original data
!        dispstr -- displacement string
!        dirstr  -- indicator of communication direction
!                   0 (without communication), 1 (with communication)
!        flag    -- to indicate se_pe_comm2e is used
!        str     -- an optional argument to indicate the starting index of
!                   certain dimension
!
!   Out: data     -- original data after communication
!
! Local variable description:
!
!    send_to       -- processor number which data needs to be sent to
!    send_to_ptr   -- a F90 pointer (alias) of send_to
!    recv_from     -- processor number which data is recvd from
!    recv_from_ptr -- a F90 pointer (alias) of recv_from
!    sdir, rdir    -- loop indexes which indicate send to or recvd from
!    rdirection    -- input communication direction for receiveing
!    sdirection    -- communication direction for sending
!    ldecomp       -- domain decomposition indicator
!    sind          -- store low and high index of each dimension for sending
!                     process
!    sind_ptr      -- a F90 pointer (alias) of sind
!    rind          -- store low and high index of each dimension for receiving
!                     process
!    rind_ptr      -- a F90 pointer (alias) of rind
!    shift         -- an array to hold the amount of index shifting due to
!                     starting index is 1 in a subroutine
!    num_shift     -- number of shifting
!    loc_str       -- a local copy of str
!
! Include file:
!
!    se_data_send_module
!    se_data_recv_module
!
! Subroutine/Function call:
!
!   se_comm_pat
!   se_up_low3
!
! --------------------------------------------------------------------------

        subroutine se_pe_comm3 (data, dispstr, dirstr, str)

        use se_data_send_module
        use se_data_recv_module
        use se_internal_util_module
        use se_pe_info_ext

        implicit none

        include "mpif.h"

        real, intent(inout) ::  data(:,:,:)
        character (len = 16), intent(in) :: dirstr
        character (len = 12), intent(in) :: dispstr
        character (len = *), optional, intent(in) :: str

        integer, target :: send_to(8), recv_from(8)
        integer, pointer :: send_to_ptr(:), recv_from_ptr(:)
        integer, target :: sind(2,3,8), rind(2,3,8)
        integer, pointer :: sind_ptr(:,:,:), rind_ptr(:,:,:)
        integer :: sdir, rdir
        integer :: shift(6), num_shift
        character (len = 80) :: loc_str
        integer :: request, status(MPI_STATUS_SIZE), error

        if (present(str)) then
           loc_str = str
           shift(2:6:2) = 1
           call se_string_to_integer (loc_str, shift, num_shift)
        else
           num_shift = 0
        end if

        call se_comm_pat (dirstr, send_to, recv_from)

        send_to_ptr => send_to
        recv_from_ptr => recv_from

        call se_up_low3 (dispstr, sind, rind, shift, num_shift, size(data,3))

        sind_ptr => sind
        rind_ptr => rind

        do sdir = 1, 8

           rdir = mod((sdir + 3), 8) + 1

           if (send_to(sdir) .eq. se_my_pe) then

              data(rind(1,1,rdir):rind(2,1,rdir),
     $             rind(1,2,rdir):rind(2,2,rdir),
     $             rind(1,3,rdir):rind(2,3,rdir))
     $        =
     $        data(sind(1,1,sdir):sind(2,1,sdir),
     $             sind(1,2,sdir):sind(2,2,sdir),
     $             sind(1,3,sdir):sind(2,3,sdir))

           else

              if (send_to(sdir) .ge. 0) then
                 call se_data_send (data, sind_ptr, send_to_ptr, sdir, sdir,
     $                              request)
              end if

              if ((recv_from(rdir) .ge. 0) .and.
     $            (recv_from(rdir) .ne. se_my_pe)) then
                 call se_data_recv (data, rind_ptr, recv_from_ptr, rdir, sdir)
              end if

!             if (send_to(sdir) .ge. 0) then
!                call mpi_wait (request, status, error)
!             end if

           end if
        end do
 
        return
        end subroutine se_pe_comm3

! --------------------------------------------------------------------------
        subroutine se_pe_comm3e (data, dispstr, dirstr, flag, str)

        use se_data_send_module
        use se_data_recv_module
        use se_internal_util_module
        use se_pe_info_ext

        implicit none

        include "mpif.h"

        real, intent(inout) ::  data(:,:,:)
        character (len = 16), intent(in) :: dirstr
        character (len = 12), intent(in) :: dispstr
        integer, intent(in) :: flag
        character (len = *), optional, intent(in) :: str

        integer, target :: send_to(8), recv_from(8)
        integer, pointer :: send_to_ptr(:), recv_from_ptr(:)
        integer, target :: sind(2,8), rind(2,8)
        integer, pointer :: sind_ptr(:,:), rind_ptr(:,:)
        integer :: sdir, rdir
        integer :: shift(6), num_shift
        character (len = 80) :: loc_str
        integer :: request, status(MPI_STATUS_SIZE), error

        if (present(str)) then
           loc_str = str
           shift(2:6:2) = 1
           call se_string_to_integer (loc_str, shift, num_shift)
        else
           num_shift = 0
        end if

        call se_comm_pat (dirstr, send_to, recv_from)

        send_to_ptr => send_to
        recv_from_ptr => recv_from

        call se_up_low1 (dispstr, sind, rind, shift, num_shift)

        sind_ptr => sind
        rind_ptr => rind

        do sdir = 1, 8, 2

           rdir = mod((sdir + 3), 8) + 1

           if (send_to(sdir) .eq. se_my_pe) then

              data(rind(1,rdir):rind(2,rdir),:,:)
     $        =
     $        data(sind(1,sdir):sind(2,sdir),:,:)

           else
              if (send_to(sdir) .ge. 0) then
                 call se_data_send (data, sind_ptr, send_to_ptr, sdir, sdir,
     $                              request)
              end if

              if ((recv_from(rdir) .ge. 0) .and.
     $            (recv_from(rdir) .ne. se_my_pe)) then
                 call se_data_recv (data, rind_ptr, recv_from_ptr, rdir, sdir)
              end if

!             if (send_to(sdir) .ge. 0) then
!                call mpi_wait (request, status, error)
!             end if

           end if
        end do
 
        return
        end subroutine se_pe_comm3e

! --------------------------------------------------------------------------
! Purpose:
!
!   perform near-neighbour communication for transfer data between two 3-D arrays
!
! Revision history:
!
!   Orginal version: 10/29/17 by David Wong
!
! Subroutine parameter description:
!
!   In:  sdata   -- input data
!        dispstr -- displacement string
!        dirstr  -- indicator of communication direction
!                   0 (without communication), 1 (with communication)
!        str     -- an optional argument to indicate the starting index of
!                   certain dimension
!
!   Out: ddata   -- output data after communication
!
! Local variable description:
!
!    send_to       -- processor number which data needs to be sent to
!    send_to_ptr   -- a F90 pointer (alias) of send_to
!    recv_from     -- processor number which data is recvd from
!    recv_from_ptr -- a F90 pointer (alias) of recv_from
!    sdir, rdir    -- loop indexes which indicate send to or recvd from
!    sind          -- store low and high index of each dimension for sending
!                     process
!    sind_ptr      -- a F90 pointer (alias) of sind
!    rind          -- store low and high index of each dimension for receiving
!                     process
!    rind_ptr      -- a F90 pointer (alias) of rind
!    shift         -- an array to hold the amount of index shifting due to
!                     starting index is 1 in a subroutine
!    num_shift     -- number of shifting
!    loc_str       -- a local copy of str
!
! Include file:
!
!    se_data_send_module
!    se_data_recv_module
!
! Subroutine/Function call:
!
!   se_comm_pat
!   se_up_low3
!
! --------------------------------------------------------------------------
        subroutine se_pe_comm3s (sdata, ddata, dispstr, dirstr, str)

        use se_data_send_module
        use se_data_recv_module
        use se_internal_util_module
        use se_pe_info_ext

        implicit none

        include "mpif.h"

        real, intent(in)  ::  sdata(:,:,:)
        real, intent(out) ::  ddata(:,:,:)
        character (len = 16), intent(in) :: dirstr
        character (len = 12), intent(in) :: dispstr
        character (len = *), optional, intent(in) :: str

        integer, target :: send_to(8), recv_from(8)
        integer, pointer :: send_to_ptr(:), recv_from_ptr(:)
        integer, target :: sind(2,8), rind(2,8)
        integer, pointer :: sind_ptr(:,:), rind_ptr(:,:)
        integer :: sdir, rdir
        integer :: shift(6), num_shift
        character (len = 80) :: loc_str
        integer :: request, status(MPI_STATUS_SIZE), error, dsize

        if (present(str)) then
           loc_str = str
           shift(2:6:2) = 1
           call se_string_to_integer (loc_str, shift, num_shift)
        else
           num_shift = 0
        end if

        call se_comm_pat (dirstr, send_to, recv_from)

        send_to_ptr => send_to
        recv_from_ptr => recv_from

        call se_up_low1 (dispstr, sind, rind, shift, num_shift)

        sind_ptr => sind
        rind_ptr => rind

        dsize = size(sdata)

        do sdir = 1, 8, 2

           rdir = mod((sdir + 3), 8) + 1

           if (send_to(sdir) .eq. se_my_pe) then

              ddata = sdata

           else
              if (send_to(sdir) .ge. 0) then
                 call mpi_send (sdata, dsize, mpi_real, send_to_ptr(sdir), 
     $                          sdir, se_worker_comm, error)
              end if

              if ((recv_from(rdir) .ge. 0) .and.
     $            (recv_from(rdir) .ne. se_my_pe)) then
                 call mpi_recv (ddata, dsize, mpi_real, recv_from_ptr(rdir), sdir,
     &                          se_worker_comm, status, error)
              end if

!             if (send_to(sdir) .ge. 0) then
!                call mpi_wait (request, status, error)
!             end if

           end if
        end do
 
        end subroutine se_pe_comm3s

! --------------------------------------------------------------------------
! Purpose:
!
!   perform near-neighbour communication for a 4-D data structure with 2-D
! decomposition 
!
! Revision history:
!
!   Orginal version: 7/31/98 by David Wong
!                    11/05/99 by David Wong
!                      -- recode the code using F90 syntax
!                    07/23/01 by David Wong
!                      -- redesign the message sending and receiving algorithm
!                    03/06/02 David Wong
!                      -- use blocking communication scheme
!                      -- use array copy mechanism when communicates to itself
!
! Subroutine parameter description:
!
!   In:  data    -- original data
!        dispstr -- displacement string
!        dirstr  -- indicator of communication direction
!                   0 (without communication), 1 (with communication)
!        str     -- an optional argument to indicate the starting index of
!                   certain dimension
!
!   Out: data    -- original data after communication
!
! Local variable description:
!
!    send_to       -- processor number which data needs to be sent to
!    send_to_ptr   -- a F90 pointer (alias) of send_to
!    recv_from     -- processor number which data is recvd from
!    recv_from_ptr -- a F90 pointer (alias) of recv_from
!    sdir, rdir    -- loop indexes which indicate send to or recvd from
!    sind          -- store low and high index of each dimension for sending
!                     process
!    sind_ptr      -- a F90 pointer (alias) of sind
!    rind          -- store low and high index of each dimension for receiving
!                     process
!    rind_ptr      -- a F90 pointer (alias) of rind
!    shift         -- an array to hold the amount of index shifting due to
!                     starting index is 1 in a subroutine
!    num_shift     -- number of shifting
!    loc_str       -- a local copy of str
!
! Include file:
!
!    se_data_send_module
!    se_data_recv_module
!
! Subroutine/Function call:
!
!   se_comm_pat
!   se_up_low4
!
! --------------------------------------------------------------------------

        subroutine se_pe_comm4 (data, dispstr, dirstr, str)

        use se_data_send_module
        use se_data_recv_module
        use se_internal_util_module
        use se_pe_info_ext

        implicit none

        include "mpif.h"

        real, intent(inout) :: data(:,:,:,:)
        character (len = 16), intent(in) :: dirstr
        character (len = 12), intent(in) :: dispstr
        character (len = *), optional, intent(in) :: str

        integer, target :: send_to(8), recv_from(8)
        integer, pointer :: send_to_ptr(:), recv_from_ptr(:)
        integer, target :: sind(2,4,8), rind(2,4,8)
        integer, pointer :: sind_ptr(:,:,:), rind_ptr(:,:,:)
        integer :: sdir, rdir
        integer :: shift(8), num_shift
        character (len = 80) :: loc_str
        integer :: request, status(MPI_STATUS_SIZE), error

        if (present(str)) then
           loc_str = str
           shift(2:8:2) = 1
           call se_string_to_integer (loc_str, shift, num_shift)
        else
           num_shift = 0
        end if

        call se_comm_pat (dirstr, send_to, recv_from)

        call se_up_low4 (dispstr, sind, rind, shift, num_shift, size(data,3), size(data,4))

        send_to_ptr => send_to
        recv_from_ptr => recv_from
        sind_ptr => sind
        rind_ptr => rind

        do sdir = 1, 8

           rdir = mod((sdir + 3), 8) + 1

           if (send_to(sdir) .eq. se_my_pe) then

              data(rind(1,1,rdir):rind(2,1,rdir),
     $             rind(1,2,rdir):rind(2,2,rdir),
     $             rind(1,3,rdir):rind(2,3,rdir),
     $             rind(1,4,rdir):rind(2,4,rdir))
     $        =
     $        data(sind(1,1,sdir):sind(2,1,sdir),
     $             sind(1,2,sdir):sind(2,2,sdir),
     $             sind(1,3,sdir):sind(2,3,sdir),
     $             sind(1,4,sdir):sind(2,4,sdir))

           else

              if (send_to(sdir) .ge. 0) then
                 call se_data_send (data, sind_ptr, send_to_ptr, sdir, sdir,
     $                              request)
              end if

              if ((recv_from(rdir) .ge. 0) .and.
     $            (recv_from(rdir) .ne. se_my_pe)) then
                 call se_data_recv (data, rind_ptr, recv_from_ptr, rdir, sdir)
              end if

!             if (send_to(sdir) .ge. 0) then
!                call mpi_wait (request, status, error)
!             end if

           end if
        end do

        return
        end subroutine se_pe_comm4

! --------------------------------------------------------------------------
        subroutine se_pe_comm4e (data, dispstr, dirstr, flag, str)

        use se_data_send_module
        use se_data_recv_module
        use se_internal_util_module
        use se_pe_info_ext

        implicit none

        include "mpif.h"

        real, intent(inout) ::  data(:,:,:,:)
        character (len = 16), intent(in) :: dirstr
        character (len = 12), intent(in) :: dispstr
        integer, intent(in) :: flag
        character (len = *), optional, intent(in) :: str

        integer, target :: send_to(8), recv_from(8)
        integer, pointer :: send_to_ptr(:), recv_from_ptr(:)
        integer, target :: sind(2,8), rind(2,8)
        integer, pointer :: sind_ptr(:,:), rind_ptr(:,:)
        integer :: sdir, rdir
        integer :: shift(8), num_shift
        character (len = 80) :: loc_str
        integer :: request, status(MPI_STATUS_SIZE), error

        if (present(str)) then
           loc_str = str
           shift(2:8:2) = 1
           call se_string_to_integer (loc_str, shift, num_shift)
        else
           num_shift = 0
        end if

        call se_comm_pat (dirstr, send_to, recv_from)

        send_to_ptr => send_to
        recv_from_ptr => recv_from

        call se_up_low1 (dispstr, sind, rind, shift, num_shift)

        sind_ptr => sind
        rind_ptr => rind

        do sdir = 1, 8, 2

           rdir = mod((sdir + 3), 8) + 1

           if (send_to(sdir) .eq. se_my_pe) then

              data(rind(1,rdir):rind(2,rdir),:,:,:)
     $        =
     $        data(sind(1,sdir):sind(2,sdir),:,:,:)

           else
              if (send_to(sdir) .ge. 0) then
                 call se_data_send (data, sind_ptr, send_to_ptr, sdir, sdir,
     $                              request)
              end if

              if ((recv_from(rdir) .ge. 0) .and.
     $            (recv_from(rdir) .ne. se_my_pe)) then
                 call se_data_recv (data, rind_ptr, recv_from_ptr, rdir, sdir)
              end if

!             if (send_to(sdir) .ge. 0) then
!                call mpi_wait (request, status, error)
!             end if

           end if
        end do
 
        return
        end subroutine se_pe_comm4e

! --------------------------------------------------------------------------

        subroutine se_pe_comm5 (data, dispstr, dirstr, str)

        use se_data_send_module
        use se_data_recv_module
        use se_internal_util_module
        use se_pe_info_ext

        implicit none

        include "mpif.h"

        real, intent(inout) :: data(:,:,:,:,:)
        character (len = 16), intent(in) :: dirstr
        character (len = 12), intent(in) :: dispstr
        character (len = *), optional, intent(in) :: str

        integer, target :: send_to(8), recv_from(8)
        integer, pointer :: send_to_ptr(:), recv_from_ptr(:)
        integer, target :: sind(2,5,8), rind(2,5,8)
        integer, pointer :: sind_ptr(:,:,:), rind_ptr(:,:,:)
        integer :: sdir, rdir
        integer :: shift(10), num_shift
        character (len = 80) :: loc_str
        integer :: request, status(MPI_STATUS_SIZE), error

        if (present(str)) then
           loc_str = str
           shift(2:10:2) = 1
           call se_string_to_integer (loc_str, shift, num_shift)
        else
           num_shift = 0
        end if

        call se_comm_pat (dirstr, send_to, recv_from)

        call se_up_low5 (dispstr, sind, rind, shift, num_shift, size(data,3), size(data,4), size(data,5))

        send_to_ptr => send_to
        recv_from_ptr => recv_from
        sind_ptr => sind
        rind_ptr => rind

        do sdir = 1, 8

           rdir = mod((sdir + 3), 8) + 1

           if (send_to(sdir) .eq. se_my_pe) then

              data(rind(1,1,rdir):rind(2,1,rdir),
     $             rind(1,2,rdir):rind(2,2,rdir),
     $             rind(1,3,rdir):rind(2,3,rdir),
     $             rind(1,4,rdir):rind(2,4,rdir),
     $             rind(1,5,rdir):rind(2,5,rdir))
     $        =
     $        data(sind(1,1,sdir):sind(2,1,sdir),
     $             sind(1,2,sdir):sind(2,2,sdir),
     $             sind(1,3,sdir):sind(2,3,sdir),
     $             sind(1,4,sdir):sind(2,4,sdir),
     $             sind(1,5,sdir):sind(2,5,sdir))

           else

              if (send_to(sdir) .ge. 0) then
                 call se_data_send (data, sind_ptr, send_to_ptr, sdir, sdir,
     $                              request)
              end if

              if ((recv_from(rdir) .ge. 0) .and.
     $            (recv_from(rdir) .ne. se_my_pe)) then
                 call se_data_recv (data, rind_ptr, recv_from_ptr, rdir, sdir)
              end if

!             if (send_to(sdir) .ge. 0) then
!                call mpi_wait (request, status, error)
!             end if

           end if
        end do

        end subroutine se_pe_comm5

! --------------------------------------------------------------------------
! Purpose:
!
!   initialize the lower and upper index of each dimension of a local
!   1-D array
!
! Revision history:
!
!   Orginal version: 9/9/98 by David Wong
!                    11/05/99 by David Wong
!                      -- recode the code using F90 syntax
!                    08/24/11 by David Wong
!                      -- removed se_ori_ext to eliminate data and geo orientation
!
! Subroutine parameter description:
!
!   In:  dispstr   -- displacement string
!        shift     -- an array to hold the amount of index shifting due to
!                     starting index is 1 in a subroutine
!        num_shift -- number of shifting
!
!   Out: sind    -- store low and high index of each dimension for sending
!                   process
!        rind    -- store low and high index of each dimension for receiving
!                   process
!
! Local variable description:
!
!    ndis      -- North displacement
!    edis      -- East displacement
!    sdis      -- South displacement
!    wdis      -- West displacement
!    loc_shift -- local adjustment of dummy argument shift
!
! Include file:
! 
!   se_domain_info_ext
!   se_ori_ext
!
! Subroutine/Function call:
!
!   store1 
!
! --------------------------------------------------------------------------

        subroutine se_up_low1 (dispstr, sind, rind, shift, num_shift)

        use se_domain_info_ext
!       use se_ori_ext

        implicit none

        integer, intent(out) :: sind(2,8), rind(2,8)
        character (len = 12), intent(in) :: dispstr
        integer, intent(in) :: shift(2), num_shift
        integer :: ndis, edis, sdis, wdis, loc_shift

        if (num_shift .gt. 0) then
           loc_shift = 1 - shift(2)
        else
           loc_shift = 0
        end if

!       if (se_geo_ori .eq. 0) then
           read (dispstr, 20) sdis, edis, ndis, wdis
!       else
!          read (dispstr, 20) ndis, edis, sdis, wdis
!       end if
 20     format (4i3)

! -- ( sending ) determine ghost cells indexes configuration
        if (sdis .gt. 0) then
           call store1 (sind, 1, 1+loc_shift, sdis+loc_shift)
        end if
        if (ndis .gt. 0) then
           call store1 (sind, 5, se_my_nrows-ndis+1+loc_shift, 
     &                  se_my_nrows+loc_shift)
        end if
        if (wdis .gt. 0) then
           call store1 (sind, 3, se_my_ncols-wdis+1+loc_shift, 
     &                  se_my_ncols+loc_shift)
        end if
        if (edis .gt. 0) then
           call store1 (sind, 7, 1+loc_shift, edis+loc_shift)
        end if
 
! -- ( receiving ) determine ghost cells indexes configuration
        if (ndis .gt. 0) then
           call store1 (rind, 1, 1-ndis+loc_shift, loc_shift)
        end if
        if (sdis .gt. 0) then
           call store1 (rind, 5, se_my_nrows+1+loc_shift, 
     &                  se_my_nrows+sdis+loc_shift)
        end if
        if (edis .gt. 0) then
           call store1 (rind, 3, se_my_ncols+1+loc_shift, 
     &                  se_my_ncols+edis+loc_shift)
        end if
        if (wdis .gt. 0) then
           call store1 (rind, 7, 1-wdis+loc_shift, loc_shift)
        end if

        return
        end subroutine se_up_low1

! --------------------------------------------------------------------------
! Purpose:
!
!   to store low and high array indexes of a given domain with ghost cells
!
! Revision history:
!
!   Orginal version: 9/9/98 by David Wong
!                    11/05/99 by David Wong
!                      -- recode the code using F90 syntax
!
! Subroutine parameter description:
!
!   In:  direction -- communication direction
!        i1        -- low index of I dimension
!        i2        -- high index of I dimension
!
!   Out: array     -- array with low and high indexes of each dimension
! --------------------------------------------------------------------------

        subroutine store1 (array, direction, i1, i2)

        integer, intent(out) :: array (2,8) 
        integer, intent(in) :: direction, i1, i2

          array(1,direction) = i1
          array(2,direction) = i2

        return
        end subroutine store1

! --------------------------------------------------------------------------
! Purpose:
!
!   initialize the lower and upper index of each dimension of a local
!   2-D array 
!
! Revision history:
!
!   Orginal version: 8/3/98 by David Wong
!                    11/05/99 by David Wong
!                      -- recode the code using F90 syntax
!                    1/17/01 by David Wong
!                      -- use a new function se_corner_adjust to adjust stencil
!                         in the NE, SE, SW, and NW direction of the boundary
!                         processors. The adjustment is applied to the store
!                         function which determine the sending and receiving
!                         indices.
!                    11/28/01 by David Wong
!                      -- make calculation of loc_shift more general to suit
!                         RC orientation.
!                    08/24/11 by David Wong
!                      -- removed se_ori_ext to eliminate data and geo orientation
!
! Subroutine parameter description:
!
!   In:  dispstr   -- displacement string
!        shift     -- an array to hold the amount of index shifting due to
!                     starting index is 1 in a subroutine
!        num_shift -- number of shifting
!
!   Out: sind    -- store low and high index of each dimension for sending
!                   process
!        rind    -- store low and high index of each dimension for receiving
!                   process
!
! Local variable description:
!
!    ndis      -- North displacement
!    edis      -- East displacement
!    sdis      -- South displacement
!    wdis      -- West displacement
!    i         -- loop index
!    loc_shift -- local adjustment of dummy argument shift
!
! Include file:
! 
!   se_domain_info_ext
!   se_ori_ext
!
! Subroutine/Function call:
!
!   store2 
!
! --------------------------------------------------------------------------

        subroutine se_up_low2 (dispstr, sind, rind, shift, num_shift)

        use se_domain_info_ext
!       use se_ori_ext
        use se_internal_util_module

        implicit none

        integer, intent(inout) :: sind(2,2,8), rind(2,2,8)
        character (len = 12), intent(in) :: dispstr
        integer, intent(in) :: shift(4), num_shift

        integer :: ndis, edis, sdis, wdis, loc_shift(2), i
        integer :: n_adj, e_adj, s_adj, w_adj

        if (num_shift .gt. 0) then
!          if ((se_data_ori .eq. "cr") .or. (se_data_ori .eq. "CR")) then
              if (shift(1) .eq. 1) then
                 loc_shift(1) = 1 - shift(4)
                 loc_shift(2) = 1 - shift(2)
              else
                 loc_shift(1) = 1 - shift(2)
                 loc_shift(2) = 1 - shift(4)
              end if
!          else
!             if (shift(1) .eq. 1) then
!                loc_shift(1) = 1 - shift(2)
!                loc_shift(2) = 1 - shift(4)
!             else
!                loc_shift(1) = 1 - shift(4)
!                loc_shift(2) = 1 - shift(2)
!             end if
!          end if
        else
           loc_shift(1) = 0
           loc_shift(2) = 0
        end if

!       if (se_geo_ori .eq. 0) then
           read (dispstr, 20) sdis, edis, ndis, wdis
!       else
!          read (dispstr, 20) ndis, edis, sdis, wdis
!       end if
 20     format (4i3)

        call se_corner_adjust (ndis, edis, sdis, wdis, 1,
     &                         n_adj, e_adj, s_adj, w_adj)

! -- ( sending ) determine ghost cells indexes configuration 
        call store2 (sind, 1, 1+loc_shift(1), sdis+loc_shift(1),
     &               1+loc_shift(2)-w_adj, se_my_ncols+loc_shift(2)+e_adj)
        call store2 (sind, 2, 1+loc_shift(1), sdis+loc_shift(1),
     &               se_my_ncols-wdis+1+loc_shift(2), se_my_ncols+loc_shift(2))
        call store2 (sind, 3, 1+loc_shift(1)-n_adj, 
     &               se_my_nrows+loc_shift(1)+s_adj,
     &               se_my_ncols-wdis+1+loc_shift(2), se_my_ncols+loc_shift(2))
        call store2 (sind, 4, se_my_nrows-ndis+1+loc_shift(1),
     &               se_my_nrows+loc_shift(1), se_my_ncols-wdis+1+loc_shift(2),
     &               se_my_ncols+loc_shift(2))
        call store2 (sind, 5, se_my_nrows-ndis+1+loc_shift(1),
     &               se_my_nrows+loc_shift(1), 1+loc_shift(2)-w_adj,
     &               se_my_ncols+loc_shift(2)+e_adj)
        call store2 (sind, 6, se_my_nrows-ndis+1+loc_shift(1),
     &               se_my_nrows+loc_shift(1), 1+loc_shift(2), 
     &               edis+loc_shift(2))
        call store2 (sind, 7, 1+loc_shift(1)-n_adj, 
     &               se_my_nrows+loc_shift(1)+s_adj,
     &               1+loc_shift(2), edis+loc_shift(2))
        call store2 (sind, 8, 1+loc_shift(1), sdis+loc_shift(1),
     &               1+loc_shift(2), edis+loc_shift(2))

        call se_corner_adjust (ndis, edis, sdis, wdis, 2,
     &                         n_adj, e_adj, s_adj, w_adj)

! -- ( receiving ) determine ghost cells indexes configuration 
        call store2 (rind, 1, 1-ndis+loc_shift(1), loc_shift(1),
     &               1+loc_shift(2)-w_adj, se_my_ncols+loc_shift(2)+e_adj)
        call store2 (rind, 2, 1-ndis+loc_shift(1), loc_shift(1),
     &               se_my_ncols+1+loc_shift(2), se_my_ncols+edis+loc_shift(2))
        call store2 (rind, 3, 1+loc_shift(1)-n_adj, 
     &               se_my_nrows+loc_shift(1)+s_adj, se_my_ncols+1+loc_shift(2),
     &               se_my_ncols+edis+loc_shift(2))
        call store2 (rind, 4, se_my_nrows+1+loc_shift(1),
     &               se_my_nrows+sdis+loc_shift(1),
     &               se_my_ncols+1+loc_shift(2), se_my_ncols+edis+loc_shift(2))
        call store2 (rind, 5, se_my_nrows+1+loc_shift(1),
     &               se_my_nrows+sdis+loc_shift(1), 1+loc_shift(2)-w_adj,
     &               se_my_ncols+loc_shift(2)+e_adj)
        call store2 (rind, 6, se_my_nrows+1+loc_shift(1),
     &               se_my_nrows+sdis+loc_shift(1), 1-wdis+loc_shift(2),
     &               loc_shift(2))
        call store2 (rind, 7, 1+loc_shift(1)-n_adj, 
     &               se_my_nrows+loc_shift(1)+s_adj,
     &               1-wdis+loc_shift(2), loc_shift(2))
        call store2 (rind, 8, 1-ndis+loc_shift(1), loc_shift(1),
     &               1-wdis+loc_shift(2), loc_shift(2))

!       if ((se_data_ori .eq. "cr") .or. (se_data_ori .eq. "CR")) then
           do i = 1, 8
              call swap (sind(1,1,i), sind(1,2,i))
              call swap (sind(2,1,i), sind(2,2,i))
              call swap (rind(1,1,i), rind(1,2,i))
              call swap (rind(2,1,i), rind(2,2,i))
           end do
!       end if

        return
        end subroutine se_up_low2

! --------------------------------------------------------------------------
! Purpose:
!
!   to store low and high array indexes of a given domain with ghost cells
!
! Revision history:
!
!   Orginal version: 7/28/98 by David Wong
!                    11/05/99 by David Wong
!                      -- recode the code using F90 syntax
!
! Subroutine parameter description:
!
!   In:  direction -- communication direction
!        i1        -- low index of I dimension
!        i2        -- high index of I dimension
!        j1        -- low index of J dimension
!        j2        -- high index of J dimension
!
!   Out: array     -- array with low and high indexes of each dimension
! --------------------------------------------------------------------------

        subroutine store2 (array, direction, i1, i2, j1, j2)

        integer, intent(out) :: array (2,2,8) 
        integer, intent(in) :: direction, i1, i2, j1, j2

          array(1,1,direction) = i1
          array(2,1,direction) = i2
          array(1,2,direction) = j1
          array(2,2,direction) = j2

        return
        end subroutine store2

! --------------------------------------------------------------------------
! Purpose:
!
!   initialize the lower and upper index of each dimension of a local
!   3-D array
!
! Revision history:
!
!   Orginal version: 8/2/98 by David Wong 
!                    11/05/99 by David Wong
!                      -- recode the code using F90 syntax
!                    1/17/01 by David Wong
!                      -- use a new function se_corner_adjust to adjust stencil
!                         in the NE, SE, SW, and NW direction of the boundary
!                         processors. The adjustment is applied to the store
!                         function which determine the sending and receiving
!                         indices.
!                    11/28/01 by David Wong
!                      -- make calculation of loc_shift more general to suit
!                         RC orientation.
!                    08/24/11 by David Wong
!                      -- removed se_ori_ext to eliminate data and geo orientation
!
! Subroutine parameter description:
!
!   In:  dispstr   -- displacement string
!        shift     -- an array to hold the amount of index shifting due to
!                     starting index is 1 in a subroutine
!        num_shift -- number of shifting
!
!   Out: sind    -- store low and high index of each dimension for sending
!                   process
!        rind    -- store low and high index of each dimension for receiving
!                   process
!
! Local variable description:
!
!    ndis      -- North displacement
!    edis      -- East displacement
!    sdis      -- South displacement
!    wdis      -- West displacement
!    i         -- loop index
!    loc_shift -- local adjustment of dummy argument shift
!    ldecomp   -- hold the values of decompstr
!
! Include file:
!
!   se_domain_info_ext
!   se_ori_ext
!
!   se_comm_info_ext
!
!     decompstr -- indicator of which dimenion(s) of data is/are decomposed,
!                  0 (not decomposed), 1 (decomposed)
!
! Subroutine/Function call:
!
!   store3 
!
! --------------------------------------------------------------------------

        subroutine se_up_low3 (dispstr, sind, rind, shift, num_shift, s3)

        use se_domain_info_ext
        use se_comm_info_ext
!       use se_ori_ext
        use se_internal_util_module

        implicit none

        integer, intent(inout) :: sind(2,3,8), rind(2,3,8)
        character (len = 12), intent(in) :: dispstr
        integer, intent(in) :: shift(6), num_shift, s3

        integer :: ndis, edis, sdis, wdis, loc_shift(3), i
        integer :: ldecomp(4)
        integer :: n_adj, e_adj, s_adj, w_adj

        read (se_decompstr, 15) (ldecomp(i), i=1, 4)
 15     format (8i2)

        if (num_shift .gt. 0) then
!          if ((se_data_ori .eq. "cr") .or. (se_data_ori .eq. "CR")) then
              if (shift(1) .eq. 1) then
                 loc_shift(1) = 1 - shift(4)
                 loc_shift(2) = 1 - shift(2)
              else
                 loc_shift(1) = 1 - shift(2)
                 loc_shift(2) = 1 - shift(4)
              end if
!          else
!             if (shift(1) .eq. 1) then
!                loc_shift(1) = 1 - shift(2)
!                loc_shift(2) = 1 - shift(4)
!             else
!                loc_shift(1) = 1 - shift(4)
!                loc_shift(2) = 1 - shift(2)
!             end if
!          end if
           loc_shift(3) = 1 - shift(6)
        else
           loc_shift(1:3) = 0
        end if

!       if (se_geo_ori .eq. 0) then
           read (dispstr, 20) sdis, edis, ndis, wdis
!       else
!          read (dispstr, 20) ndis, edis, sdis, wdis
!       end if
 20     format (4i3)

        call se_corner_adjust (ndis, edis, sdis, wdis, 1,
     &                         n_adj, e_adj, s_adj, w_adj)

! -- ( sending ) determine ghost cells indexes configuration which
! -- depends on spatial decomposition type 1, 2, and 3, respectively
        if ((ldecomp(1) + ldecomp(3) .eq. 1) .and. 
     &      (ldecomp(1) .eq. 1)) then
           call store3 (sind, 1, 1+loc_shift(1), sdis+loc_shift(1), 
     &                  1+loc_shift(2)-w_adj, se_my_ncols+loc_shift(2)+e_adj, 
     &                  1+loc_shift(3), s3+loc_shift(3))
           call store3 (sind, 2, 1+loc_shift(1), sdis+loc_shift(1), 
     &                  se_my_ncols-wdis+1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3))
           call store3 (sind, 3, 1+loc_shift(1)-n_adj, 
     &                  se_my_nrows+loc_shift(1)+s_adj, 
     &                  se_my_ncols-wdis+1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3))
           call store3 (sind, 4, se_my_nrows-ndis+1+loc_shift(1), 
     &                  se_my_nrows+loc_shift(1), 
     &                  se_my_ncols-wdis+1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3))
           call store3 (sind, 5, se_my_nrows-ndis+1+loc_shift(1), 
     &                  se_my_nrows+loc_shift(1), 1+loc_shift(2)-w_adj, 
     &                  se_my_ncols+loc_shift(2)+e_adj, 1+loc_shift(3), 
     &                  s3+loc_shift(3))
           call store3 (sind, 6, se_my_nrows-ndis+1+loc_shift(1), 
     &                  se_my_nrows+loc_shift(1), 1+loc_shift(2), 
     &                  edis+loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3))
           call store3 (sind, 7, 1+loc_shift(1)-n_adj, 
     &                  se_my_nrows+loc_shift(1)+s_adj, 
     &                  1+loc_shift(2), edis+loc_shift(2), 1+loc_shift(3),
     &                  s3+loc_shift(3))
           call store3 (sind, 8, 1+loc_shift(1), sdis+loc_shift(1), 
     &                  1+loc_shift(2), edis+loc_shift(2), 
     &                  1+loc_shift(3), s3+loc_shift(3))
        else if ((ldecomp(1) + ldecomp(2) .eq. 1) .and. 
     &           (ldecomp(2) .eq. 1)) then
           call store3 (sind, 1, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2), sdis+loc_shift(2), 1+loc_shift(3)-w_adj,
     &                  s3+loc_shift(3)+e_adj)
           call store3 (sind, 2, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2), sdis+loc_shift(2), 
     &                  s3-wdis+1+loc_shift(3), 
     &                  s3+loc_shift(3))
           call store3 (sind, 3, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2)-n_adj, se_my_ncols+loc_shift(2)+s_adj,
     &                  s3-wdis+1+loc_shift(3), 
     &                  s3+loc_shift(3))
           call store3 (sind, 4, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  se_my_ncols-ndis+1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 
     &                  s3-wdis+1+loc_shift(3), 
     &                  s3+loc_shift(3))
           call store3 (sind, 5, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  se_my_ncols-ndis+1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 1+loc_shift(3)-w_adj,
     &                  s3+loc_shift(3)+e_adj)
           call store3 (sind, 6, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  se_my_ncols-ndis+1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 1+loc_shift(3), 
     &                  edis+loc_shift(3))
           call store3 (sind, 7, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2)-n_adj, se_my_ncols+loc_shift(2)+s_adj,
     &                  1+loc_shift(3), edis+loc_shift(3))
           call store3 (sind, 8, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2), sdis+loc_shift(2), 1+loc_shift(3), 
     &                  edis+loc_shift(3))
        else
           call store3 (sind, 1, 1+loc_shift(1)-w_adj,
     &                  se_my_nrows+loc_shift(1)+e_adj, 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  1+loc_shift(3), sdis+loc_shift(3))
           call store3 (sind, 2, se_my_nrows-wdis+1+loc_shift(1), 
     &                  se_my_nrows+loc_shift(1), 1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 1+loc_shift(3), 
     &                  sdis+loc_shift(3))
           call store3 (sind, 3, se_my_nrows-wdis+1+loc_shift(1), 
     &                  se_my_nrows+loc_shift(1), 1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 1+loc_shift(3)-n_adj, 
     &                  s3+loc_shift(3)+s_adj)
           call store3 (sind, 4, se_my_nrows-wdis+1+loc_shift(1), 
     &                  se_my_nrows+loc_shift(1), 1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 
     &                  s3-ndis+1+loc_shift(3), 
     &                  s3+loc_shift(3))
           call store3 (sind, 5, 1+loc_shift(1)-w_adj,
     &                  se_my_nrows+loc_shift(1)+e_adj,
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  s3-ndis+1+loc_shift(3), 
     &                  s3+loc_shift(3))
           call store3 (sind, 6, 1+loc_shift(1), edis+loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  s3-ndis+1+loc_shift(3), 
     &                  s3+loc_shift(3))
           call store3 (sind, 7, 1+loc_shift(1), edis+loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  1+loc_shift(3)-n_adj, s3+loc_shift(3)+s_adj)
           call store3 (sind, 8, 1+loc_shift(1), edis+loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  1+loc_shift(3), sdis+loc_shift(3))
        end if

        call se_corner_adjust (ndis, edis, sdis, wdis, 2,
     &                         n_adj, e_adj, s_adj, w_adj)

! -- ( receiving ) determine ghost cells indexes configuration which
! -- depends on spatial decomposition, type 1, 2, and 3, respectively
        if ((ldecomp(1) + ldecomp(3) .eq. 1) .and. 
     &      (ldecomp(1) .eq. 1)) then
           call store3 (rind, 1, 1-ndis+loc_shift(1), loc_shift(1), 
     &                  1+loc_shift(2)-w_adj, se_my_ncols+loc_shift(2)+e_adj, 
     &                  1+loc_shift(3), s3+loc_shift(3))
           call store3 (rind, 2, 1-ndis+loc_shift(1), loc_shift(1), 
     &                  se_my_ncols+1+loc_shift(2), 
     &                  se_my_ncols+edis+loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3))
           call store3 (rind, 3, 1+loc_shift(1)-n_adj, 
     &                  se_my_nrows+loc_shift(1)+s_adj, 
     &                  se_my_ncols+1+loc_shift(2), 
     &                  se_my_ncols+edis+loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3))
           call store3 (rind, 4, se_my_nrows+1+loc_shift(1), 
     &                  se_my_nrows+sdis+loc_shift(1), 
     &                  se_my_ncols+1+loc_shift(2), 
     &                  se_my_ncols+edis+loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3))
           call store3 (rind, 5, se_my_nrows+1+loc_shift(1), 
     &                  se_my_nrows+sdis+loc_shift(1), 1+loc_shift(2)-w_adj, 
     &                  se_my_ncols+loc_shift(2)+e_adj, 1+loc_shift(3), 
     &                  s3+loc_shift(3))
           call store3 (rind, 6, se_my_nrows+1+loc_shift(1), 
     &                  se_my_nrows+sdis+loc_shift(1), 1-wdis+loc_shift(2), 
     &                  loc_shift(2), 1+loc_shift(3), s3+loc_shift(3))
           call store3 (rind, 7, 1+loc_shift(1)-n_adj, 
     &                  se_my_nrows+loc_shift(1)+s_adj, 
     &                  1-wdis+loc_shift(2), loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3))
           call store3 (rind, 8, 1-ndis+loc_shift(1), loc_shift(1), 
     &                  1-wdis+loc_shift(2), loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3))
        else if ((ldecomp(1) + ldecomp(2) .eq. 1) .and. 
     &           (ldecomp(2) .eq. 1)) then
           call store3 (rind, 1, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1-ndis+loc_shift(2), loc_shift(2), 1+loc_shift(3)-w_adj,
     &                  s3+loc_shift(3)+e_adj)
           call store3 (rind, 2, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1-ndis+loc_shift(2), loc_shift(2), 
     &                  s3+1+loc_shift(3), 
     &                  s3+edis+loc_shift(3))
           call store3 (rind, 3, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2)-n_adj, se_my_ncols+loc_shift(2)+s_adj,
     &                  s3+1+loc_shift(3), 
     &                  s3+edis+loc_shift(3))
           call store3 (rind, 4, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  se_my_ncols+1+loc_shift(2), 
     &                  se_my_ncols+sdis+loc_shift(2), 
     &                  s3+1+loc_shift(3), 
     &                  s3+edis+loc_shift(3))
           call store3 (rind, 5, 1+loc_shift(1), se_my_nrows+loc_shift(1),
     &                  se_my_ncols+1+loc_shift(2), 
     &                  se_my_ncols+sdis+loc_shift(2), 1+loc_shift(3)-w_adj, 
     &                  s3+loc_shift(3)+e_adj)
           call store3 (rind, 6, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  se_my_ncols+1+loc_shift(2), 
     &                  se_my_ncols+sdis+loc_shift(2), 1-wdis+loc_shift(3), 
     &                  loc_shift(3))
           call store3 (rind, 7, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2)-n_adj, se_my_ncols+loc_shift(2)+s_adj,
     &                  1-wdis+loc_shift(3), loc_shift(3))
           call store3 (rind, 8, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1-ndis+loc_shift(2), loc_shift(2), 1-wdis+loc_shift(3), 
     &                  loc_shift(3))
        else
           call store3 (rind, 1, 1+loc_shift(1)-w_adj, 
     &                  se_my_nrows+loc_shift(1)+e_adj, 1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 1-ndis+loc_shift(3), 
     &                  loc_shift(3))
           call store3 (rind, 2, se_my_nrows+1+loc_shift(1), 
     &                  se_my_nrows+edis+loc_shift(1), 1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 1-ndis+loc_shift(3), 
     &                  loc_shift(3))
           call store3 (rind, 3, se_my_nrows+1+loc_shift(1), 
     &                  se_my_nrows+edis+loc_shift(1), 1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 1+loc_shift(3)-n_adj,
     &                  s3+loc_shift(3)+s_adj)
           call store3 (rind, 4, se_my_nrows+1+loc_shift(1), 
     &                  se_my_nrows+edis+loc_shift(1), 1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), s3+1+loc_shift(3), 
     &                  s3+sdis+loc_shift(3))
           call store3 (rind, 5, 1+loc_shift(1)-w_adj, 
     &                  se_my_nrows+loc_shift(1)+e_adj, 1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), s3+1+loc_shift(3), 
     &                  s3+sdis+loc_shift(3))
           call store3 (rind, 6, 1-wdis+loc_shift(1), loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  s3+1+loc_shift(3), 
     &                  s3+sdis+loc_shift(3))
           call store3 (rind, 7, 1-wdis+loc_shift(1), loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  1+loc_shift(3)-n_adj, s3+loc_shift(3)+s_adj)
           call store3 (rind, 8, 1-wdis+loc_shift(1), loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  1-ndis+loc_shift(3), loc_shift(3))
        end if

!       if ((se_data_ori .eq. "cr") .or. (se_data_ori .eq. "CR")) then
           do i = 1, 8
              call swap (sind(1,1,i), sind(1,2,i))
              call swap (sind(2,1,i), sind(2,2,i))
              call swap (rind(1,1,i), rind(1,2,i))
              call swap (rind(2,1,i), rind(2,2,i))
           end do
!       end if

        return
        end subroutine se_up_low3

! --------------------------------------------------------------------------
! Purpose:
!
!   to store low and high array indexes of a given domain with ghost cells
!
! Revision history:
!
!   Orginal version: 7/28/98 by David Wong
!                    11/05/99 by David Wong
!                      -- recode the code using F90 syntax
!
! Subroutine parameter description:
!
!   In:  direction -- communication direction
!        i1        -- low index of I dimension
!        i2        -- high index of I dimension
!        j1        -- low index of J dimension
!        j2        -- high index of J dimension
!        k1        -- low index of K dimension
!        k2        -- high index of K dimension
!
!   Out: array     -- array with low and high indexes of each dimension
! --------------------------------------------------------------------------

        subroutine store3 (array, direction, i1, i2, j1, j2, k1, k2)

        integer, intent(out) :: array (2,3,8) 
        integer, intent(in) :: direction, i1, i2, j1, j2, k1, k2

          array(1,1,direction) = i1
          array(2,1,direction) = i2
          array(1,2,direction) = j1
          array(2,2,direction) = j2
          array(1,3,direction) = k1
          array(2,3,direction) = k2

        return
        end subroutine store3

! --------------------------------------------------------------------------
! Purpose:
!
!   initialize the lower and upper index of each dimension of a local
!   4-D array
!
! Revision history:
!
!   Orginal version: 8/3/98 by David Wong
!                    11/05/99 by David Wong
!                      -- recode the code using F90 syntax
!                    1/17/01 by David Wong
!                      -- use a new function se_corner_adjust to adjust stencil
!                         in the NE, SE, SW, and NW direction of the boundary
!                         processors. The adjustment is applied to the store
!                         function which determine the sending and receiving
!                         indices.
!                    11/28/01 by David Wong
!                      -- make calculation of loc_shift more general to suit
!                         RC orientation.
!                    08/24/11 by David Wong
!                      -- removed se_ori_ext to eliminate data and geo orientation
!
! Subroutine parameter description:
!
!   In:  dispstr   -- displacement string
!        shift     -- an array to hold the amount of index shifting due to
!                     starting index is 1 in a subroutine
!        num_shift -- number of shifting
!
!   Out: sind      -- store low and high index of each dimension for sending
!                     process
!        rind      -- store low and high index of each dimension for receiving
!                     process
!
! Local variable description:
!
!    ndis      -- North displacement
!    edis      -- East displacement
!    sdis      -- South displacement
!    wdis      -- West displacement
!    i         -- loop index
!    loc_shift -- local adjustment of dummy argument shift
!    ldecomp   -- hold the values of decompstr 
!
! Include file:
!
!   se_domain_info_ext
!   se_ori_ext
!
!   se_comm_info.ext
!
!     decompstr -- indicator of which dimenion(s) of data is/are decomposed,
!                  0 (not decomposed), 1 (decomposed)
!
! Subroutine/Function call:
!
!   store4 
!
! --------------------------------------------------------------------------

        subroutine se_up_low4 (dispstr, sind, rind, shift, num_shift, s3, s4)

        use se_domain_info_ext
        use se_comm_info_ext
!       use se_ori_ext
        use se_internal_util_module

        implicit none

        integer, intent(inout) :: sind(2,4,8), rind(2,4,8)
        character (len = 12), intent(in) :: dispstr
        integer, intent(in) :: shift(8), num_shift, s3, s4

        integer :: ndis, edis, sdis, wdis, loc_shift(4), i
        integer :: ldecomp(4)
        integer :: n_adj, e_adj, s_adj, w_adj

        if (num_shift .gt. 0) then
!          if ((se_data_ori .eq. "cr") .or. (se_data_ori .eq. "CR")) then
              if (shift(1) .eq. 1) then
                 loc_shift(1) = 1 - shift(4)
                 loc_shift(2) = 1 - shift(2)
              else
                 loc_shift(1) = 1 - shift(2)
                 loc_shift(2) = 1 - shift(4)
              end if
!          else
!             if (shift(1) .eq. 1) then
!                loc_shift(1) = 1 - shift(2)
!                loc_shift(2) = 1 - shift(4)
!             else
!                loc_shift(1) = 1 - shift(4)
!                loc_shift(2) = 1 - shift(2)
!             end if
!          end if
           loc_shift(3) = 1 - shift(6)
           loc_shift(4) = 1 - shift(8)
        else
           loc_shift(1:4) = 0
        end if

        read (se_decompstr, 15) (ldecomp(i), i=1, 4)
 15     format (8i2)

!       if (se_geo_ori .eq. 0) then
           read (dispstr, 20) sdis, edis, ndis, wdis
!       else
!          read (dispstr, 20) ndis, edis, sdis, wdis
!       end if
 20     format (4i3)

        call se_corner_adjust (ndis, edis, sdis, wdis, 1,
     &                         n_adj, e_adj, s_adj, w_adj)

! -- ( sending ) determine ghost cells indexes configuration which
! -- depends on spatial decomposition type 1, 2, 3, 4, 5, and 6, respectively
        if ((ldecomp(1) + ldecomp(3) .eq. 1) .and. 
     &      (ldecomp(1) .eq. 1) .and. (ldecomp(4) .eq. 0)) then
           call store4 (sind, 1, 1+loc_shift(1), sdis+loc_shift(1),
     &                  1+loc_shift(2)-w_adj, se_my_ncols+loc_shift(2)+e_adj,
     &                  1+loc_shift(3), s3+loc_shift(3), 
     &                  1+loc_shift(4), s4+loc_shift(4))
           call store4 (sind, 2, 1+loc_shift(1), sdis+loc_shift(1),
     &                  se_my_ncols-wdis+1+loc_shift(2),
     &                  se_my_ncols+loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3), 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (sind, 3, 1+loc_shift(1)-n_adj,
     &                  se_my_nrows+loc_shift(1)+s_adj,
     &                  se_my_ncols-wdis+1+loc_shift(2),
     &                  se_my_ncols+loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3), 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (sind, 4, se_my_nrows-ndis+1+loc_shift(1), 
     &                  se_my_nrows+loc_shift(1), 
     &                  se_my_ncols-wdis+1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3), 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (sind, 5, se_my_nrows-ndis+1+loc_shift(1),
     &                  se_my_nrows+loc_shift(1), 1+loc_shift(2)-w_adj,
     &                  se_my_ncols+loc_shift(2)+e_adj, 1+loc_shift(3), 
     &                  s3+loc_shift(3), 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (sind, 6, se_my_nrows-ndis+1+loc_shift(1), 
     &                  se_my_nrows+loc_shift(1), 1+loc_shift(2), 
     &                  edis+loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3), 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (sind, 7, 1+loc_shift(1)-n_adj,
     &                  se_my_nrows+loc_shift(1)+s_adj,
     &                  1+loc_shift(2), edis+loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3), 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (sind, 8, 1+loc_shift(1), sdis+loc_shift(1), 
     &                  1+loc_shift(2), edis+loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3), 1+loc_shift(4), 
     &                  s4+loc_shift(4))
        else if ((ldecomp(2) + ldecomp(4) .eq. 1) .and. 
     &           (ldecomp(2) .eq. 1) .and. (ldecomp(1) .eq. 0)) then
           call store4 (sind, 1, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2), sdis+loc_shift(2), 1+loc_shift(3)-w_adj,
     &                  s3+loc_shift(3)+e_adj, 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (sind, 2, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2), sdis+loc_shift(2), 
     &                  s3-wdis+1+loc_shift(3), 
     &                  s3+loc_shift(3), 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (sind, 3, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2)-n_adj, se_my_ncols+loc_shift(2)+s_adj,
     &                  s3-wdis+1+loc_shift(3), 
     &                  s3+loc_shift(3), 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (sind, 4, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  se_my_ncols-ndis+1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 
     &                  s3-wdis+1+loc_shift(3), 
     &                  s3+loc_shift(3), 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (sind, 5, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  se_my_ncols-ndis+1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 1+loc_shift(3)-w_adj,
     &                  s3+loc_shift(3)+e_adj, 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (sind, 6, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  se_my_ncols-ndis+1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 1+loc_shift(3), 
     &                  edis+loc_shift(3), 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (sind, 7, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2)-n_adj, se_my_ncols+loc_shift(2)+s_adj,
     &                  1+loc_shift(3), edis+loc_shift(3), 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (sind, 8, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2), sdis+loc_shift(2), 1+loc_shift(3), 
     &                  edis+loc_shift(3), 1+loc_shift(4), 
     &                  s4+loc_shift(4))
        else if ((ldecomp(3) + ldecomp(4) .eq. 1) .and. 
     &           (ldecomp(3) .eq. 1) .and. (ldecomp(2) .eq. 0)) then
           call store4 (sind, 1, 1+loc_shift(1)-w_adj, 
     &                  se_my_nrows+loc_shift(1)+e_adj, 1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 1+loc_shift(3), 
     &                  sdis+loc_shift(3), 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (sind, 2, se_my_nrows-wdis+1+loc_shift(1), 
     &                  se_my_nrows+loc_shift(1), 1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 1+loc_shift(3), 
     &                  sdis+loc_shift(3), 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (sind, 3, se_my_nrows-wdis+1+loc_shift(1), 
     &                  se_my_nrows+loc_shift(1), 1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 1+loc_shift(3)-n_adj, 
     &                  s3+loc_shift(3)+s_adj, 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (sind, 4, se_my_nrows-wdis+1+loc_shift(1), 
     &                  se_my_nrows+loc_shift(1), 1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 
     &                  s3-ndis+1+loc_shift(3), 
     &                  s3+loc_shift(3), 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (sind, 5, 1+loc_shift(1)-w_adj, 
     &                  se_my_nrows+loc_shift(1)+e_adj, 1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 
     &                  s3-ndis+1+loc_shift(3), 
     &                  s3+loc_shift(3), 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (sind, 6, 1+loc_shift(1), edis+loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  s3-ndis+1+loc_shift(3), 
     &                  s3+loc_shift(3), 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (sind, 7, 1+loc_shift(1), edis+loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  1+loc_shift(3)-n_adj, s3+loc_shift(3)+s_adj,
     &                  1+loc_shift(4), s4+loc_shift(4))
           call store4 (sind, 8, 1+loc_shift(1), edis+loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  1+loc_shift(3), sdis+loc_shift(3), 1+loc_shift(4), 
     &                  s4+loc_shift(4))
        else if ((ldecomp(1) + ldecomp(4) .eq. 1) .and. 
     &           (ldecomp(4) .eq. 1) .and. (ldecomp(2) .eq. 0)) then
           call store4 (sind, 1, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  1+loc_shift(3)-w_adj, s3+loc_shift(3)+e_adj,
     &                  1+loc_shift(4), sdis+loc_shift(4))
           call store4 (sind, 2, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  s3-wdis+1+loc_shift(3), 
     &                  s3+loc_shift(3), 1+loc_shift(4), 
     &                  sdis+loc_shift(4))
           call store4 (sind, 3, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  s3-wdis+1+loc_shift(3), 
     &                  s3+loc_shift(3), 1+loc_shift(4)-n_adj, 
     &                  s4+loc_shift(4)+s_adj)
           call store4 (sind, 4, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  s3-wdis+1+loc_shift(3), 
     &                  s3+loc_shift(3), 
     &                  s4-ndis+1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (sind, 5, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  1+loc_shift(3)-w_adj, s3+loc_shift(3)+e_adj,
     &                  s4-ndis+1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (sind, 6, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  1+loc_shift(3), edis+loc_shift(3), 
     &                  s4-ndis+1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (sind, 7, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  1+loc_shift(3), edis+loc_shift(3), 1+loc_shift(4)-n_adj,
     &                  s4+loc_shift(4)+s_adj)
           call store4 (sind, 8, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  1+loc_shift(3), edis+loc_shift(3), 1+loc_shift(4), 
     &                  sdis+loc_shift(4))
        else if ((ldecomp(2) + ldecomp(4) .eq. 1) .and. 
     &           (ldecomp(4) .eq. 1) .and. (ldecomp(3) .eq. 0)) then
           call store4 (sind, 1, 1+loc_shift(1), sdis+loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  1+loc_shift(3), s3+loc_shift(3), 
     &                  1+loc_shift(4)-w_adj, s4+loc_shift(4)+e_adj)
           call store4 (sind, 2, 1+loc_shift(1), sdis+loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  1+loc_shift(3), s3+loc_shift(3), 
     &                  s4-wdis+1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (sind, 3, 1+loc_shift(1)-n_adj, 
     &                  se_my_nrows+loc_shift(1)+s_adj,
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  1+loc_shift(3), s3+loc_shift(3), 
     &                  s4-wdis+1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (sind, 4, se_my_nrows-ndis+1+loc_shift(1), 
     &                  se_my_nrows+loc_shift(1), 1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3), 
     &                  s4-wdis+1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (sind, 5, se_my_nrows-ndis+1+loc_shift(1), 
     &                  se_my_nrows+loc_shift(1), 1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 1+loc_shift(3)-w_adj,
     &                  s3+loc_shift(3)+e_adj, 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (sind, 6, se_my_nrows-ndis+1+loc_shift(1), 
     &                  se_my_nrows+loc_shift(1), 1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3), 1+loc_shift(4), 
     &                  edis+loc_shift(4))
           call store4 (sind, 7, 1+loc_shift(1)-n_adj, 
     &                  se_my_nrows+loc_shift(1)+s_adj,
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  1+loc_shift(3), s3+loc_shift(3), 
     &                  1+loc_shift(4), edis+loc_shift(4))
           call store4 (sind, 8, 1+loc_shift(1), sdis+loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  1+loc_shift(3), s3+loc_shift(3), 
     &                  1+loc_shift(4), edis+loc_shift(4))
        else
           call store4 (sind, 1, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2), sdis+loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3), 1+loc_shift(4)-w_adj,
     &                  s4+loc_shift(4)+e_adj)
           call store4 (sind, 2, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2), sdis+loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3), 
     &                  s4-wdis+1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (sind, 3, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2)-n_adj, se_my_ncols+loc_shift(2)+s_adj,
     &                  1+loc_shift(3), s3+loc_shift(3), 
     &                  s4-wdis+1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (sind, 4, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  se_my_ncols-ndis+1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3), 
     &                  s4-wdis+1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (sind, 5, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  se_my_ncols-ndis+1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3), 1+loc_shift(4)-w_adj, 
     &                  s4+loc_shift(4)+e_adj)
           call store4 (sind, 6, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  se_my_ncols-ndis+1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3), 1+loc_shift(4), 
     &                  edis+loc_shift(4))
           call store4 (sind, 7, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2)-n_adj, se_my_ncols+loc_shift(2)+s_adj,
     &                  1+loc_shift(3), s3+loc_shift(3), 
     &                  1+loc_shift(4), edis+loc_shift(4))
           call store4 (sind, 8, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2), sdis+loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3), 1+loc_shift(4), 
     &                  edis+loc_shift(4))
        end if

        call se_corner_adjust (ndis, edis, sdis, wdis, 2,
     &                         n_adj, e_adj, s_adj, w_adj)

! -- ( receiving ) determine ghost cells indexes configuration which
! -- depends on spatial decomposition, type 1, 2, and 3, respectively
        if ((ldecomp(1) + ldecomp(3) .eq. 1) .and.
     &      (ldecomp(1) .eq. 1) .and. (ldecomp(4) .eq. 0)) then
           call store4 (rind, 1, 1-ndis+loc_shift(1), loc_shift(1),
     &                  1+loc_shift(2)-w_adj, se_my_ncols+loc_shift(2)+e_adj,
     &                  1+loc_shift(3), s3+loc_shift(3), 
     &                  1+loc_shift(4), s4+loc_shift(4))
           call store4 (rind, 2, 1-ndis+loc_shift(1), loc_shift(1), 
     &                  se_my_ncols+1+loc_shift(2), 
     &                  se_my_ncols+edis+loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3), 1+loc_shift(4),
     &                  s4+loc_shift(4))
           call store4 (rind, 3, 1+loc_shift(1)-n_adj,
     &                  se_my_nrows+loc_shift(1)+s_adj,
     &                  se_my_ncols+1+loc_shift(2),
     &                  se_my_ncols+edis+loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3), 1+loc_shift(4),
     &                  s4+loc_shift(4))
           call store4 (rind, 4, se_my_nrows+1+loc_shift(1), 
     &                  se_my_nrows+sdis+loc_shift(1), 
     &                  se_my_ncols+1+loc_shift(2), 
     &                  se_my_ncols+edis+loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3), 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (rind, 5, se_my_nrows+1+loc_shift(1),
     &                  se_my_nrows+sdis+loc_shift(1), 1+loc_shift(2)-w_adj,
     &                  se_my_ncols+loc_shift(2)+e_adj, 1+loc_shift(3), 
     &                  s3+loc_shift(3), 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (rind, 6, se_my_nrows+1+loc_shift(1), 
     &                  se_my_nrows+sdis+loc_shift(1), 1-wdis+loc_shift(2), 
     &                  loc_shift(2), 1+loc_shift(3), s3+loc_shift(3), 
     &                  1+loc_shift(4), s4+loc_shift(4))
           call store4 (rind, 7, 1+loc_shift(1)-n_adj,
     &                  se_my_nrows+loc_shift(1)+s_adj,
     &                  1-wdis+loc_shift(2), loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3), 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (rind, 8, 1-ndis+loc_shift(1), loc_shift(1), 
     &                  1-wdis+loc_shift(2), loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3), 1+loc_shift(4), 
     &                  s4+loc_shift(4))
        else if ((ldecomp(2) + ldecomp(4) .eq. 1) .and.
     &           (ldecomp(2) .eq. 1) .and. (ldecomp(1) .eq. 0)) then
           call store4 (rind, 1, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1-ndis+loc_shift(2), loc_shift(2), 1+loc_shift(3)-w_adj,
     &                  s3+loc_shift(3)+e_adj, 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (rind, 2, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1-ndis+loc_shift(2), loc_shift(2), 
     &                  s3+1+loc_shift(3), 
     &                  s3+edis+loc_shift(3), 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (rind, 3, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2)-n_adj, se_my_ncols+loc_shift(2)+s_adj,
     &                  s3+1+loc_shift(3), 
     &                  s3+edis+loc_shift(3), 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (rind, 4, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  se_my_ncols+1+loc_shift(2), 
     &                  se_my_ncols+sdis+loc_shift(2), 
     &                  s3+1+loc_shift(3), 
     &                  s3+edis+loc_shift(3), 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (rind, 5, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  se_my_ncols+1+loc_shift(2), 
     &                  se_my_ncols+sdis+loc_shift(2), 1+loc_shift(3)-w_adj, 
     &                  s3+loc_shift(3)+e_adj, 1+loc_shift(4),
     &                  s4+loc_shift(4))
           call store4 (rind, 6, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  se_my_ncols+1+loc_shift(2), 
     &                  se_my_ncols+sdis+loc_shift(2), 1-wdis+loc_shift(3), 
     &                  loc_shift(3), 1+loc_shift(4), s4+loc_shift(4))
           call store4 (rind, 7, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2)-n_adj, se_my_ncols+loc_shift(2)+s_adj,
     &                  1-wdis+loc_shift(3), loc_shift(3), 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (rind, 8, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1-ndis+loc_shift(2), loc_shift(2), 1-wdis+loc_shift(3), 
     &                  loc_shift(3), 1+loc_shift(4), s4+loc_shift(4))
        else if ((ldecomp(3) + ldecomp(4) .eq. 1) .and.
     &           (ldecomp(3) .eq. 1) .and. (ldecomp(2) .eq. 0)) then
           call store4 (rind, 1, 1+loc_shift(1)-w_adj, 
     &                  se_my_nrows+loc_shift(1)+e_adj,
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  1-ndis+loc_shift(3), loc_shift(3), 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (rind, 2, se_my_nrows+1+loc_shift(1), 
     &                  se_my_nrows+edis+loc_shift(1), 1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 1-ndis+loc_shift(3), 
     &                  loc_shift(3), 1+loc_shift(4), s4+loc_shift(4))
           call store4 (rind, 3, se_my_nrows+1+loc_shift(1), 
     &                  se_my_nrows+edis+loc_shift(1), 1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 1+loc_shift(3)-n_adj, 
     &                  s3+loc_shift(3)+s_adj, 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (rind, 4, se_my_nrows+1+loc_shift(1), 
     &                  se_my_nrows+edis+loc_shift(1), 1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), s3+1+loc_shift(3), 
     &                  s3+sdis+loc_shift(3), 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (rind, 5, 1+loc_shift(1)-w_adj, 
     &                  se_my_nrows+loc_shift(1)+e_adj,
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  s3+1+loc_shift(3), 
     &                  s3+sdis+loc_shift(3), 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (rind, 6, 1-wdis+loc_shift(1), loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  s3+1+loc_shift(3), 
     &                  s3+sdis+loc_shift(3), 1+loc_shift(4), 
     &                  s4+loc_shift(4))
           call store4 (rind, 7, 1-wdis+loc_shift(1), loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  1+loc_shift(3)-n_adj, s3+loc_shift(3)+s_adj,
     &                  1+loc_shift(4), s4+loc_shift(4))
           call store4 (rind, 8, 1-wdis+loc_shift(1), loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  1-ndis+loc_shift(3), loc_shift(3), 1+loc_shift(4), 
     &                  s4+loc_shift(4))
        else if ((ldecomp(1) + ldecomp(4) .eq. 1) .and.
     &           (ldecomp(4) .eq. 1) .and. (ldecomp(2) .eq. 0)) then
           call store4 (rind, 1, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  1+loc_shift(3)-w_adj, s3+loc_shift(3)+e_adj,
     &                  1-ndis+loc_shift(4), loc_shift(4))
           call store4 (rind, 2, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  s3+1+loc_shift(3), 
     &                  s3+edis+loc_shift(3), 1-ndis+loc_shift(4), 
     &                  loc_shift(4))
           call store4 (rind, 3, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  s3+1+loc_shift(3), 
     &                  s3+edis+loc_shift(3), 1+loc_shift(4)-n_adj, 
     &                  s4+loc_shift(4)+s_adj)
           call store4 (rind, 4, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  s3+1+loc_shift(3), 
     &                  s3+edis+loc_shift(3), 
     &                  s4+1+loc_shift(4), 
     &                  s4+sdis+loc_shift(4))
           call store4 (rind, 5, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  1+loc_shift(3)-w_adj, s3+loc_shift(3)+e_adj,
     &                  s4+1+loc_shift(4), 
     &                  s4+sdis+loc_shift(4))
           call store4 (rind, 6, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  1-wdis+loc_shift(3), loc_shift(3), 
     &                  s4+1+loc_shift(4), 
     &                  s4+sdis+loc_shift(4))
           call store4 (rind, 7, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  1-wdis+loc_shift(3), loc_shift(3), 1+loc_shift(4)-n_adj,
     &                  s4+loc_shift(4)+s_adj)
           call store4 (rind, 8, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  1-wdis+loc_shift(3), loc_shift(3), 1-ndis+loc_shift(4), 
     &                  loc_shift(4))
        else if ((ldecomp(2) + ldecomp(4) .eq. 1) .and.
     &           (ldecomp(4) .eq. 1) .and. (ldecomp(3) .eq. 0)) then
           call store4 (rind, 1, 1-ndis+loc_shift(1), loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  1+loc_shift(3), s3+loc_shift(3), 
     &                  1+loc_shift(4)-w_adj, s4+loc_shift(4)+e_adj)
           call store4 (rind, 2, 1-ndis+loc_shift(1), loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  1+loc_shift(3), s3+loc_shift(3), 
     &                  s4+1+loc_shift(4), 
     &                  s4+edis+loc_shift(4))
           call store4 (rind, 3, 1+loc_shift(1)-n_adj, 
     &                  se_my_nrows+loc_shift(1)+s_adj,
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  1+loc_shift(3), s3+loc_shift(3), 
     &                  s4+1+loc_shift(4), 
     &                  s4+edis+loc_shift(4))
           call store4 (rind, 4, se_my_nrows+1+loc_shift(1), 
     &                  se_my_nrows+sdis+loc_shift(1), 1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3), s4+1+loc_shift(4), 
     &                  s4+edis+loc_shift(4))
           call store4 (rind, 5, se_my_nrows+1+loc_shift(1), 
     &                  se_my_nrows+sdis+loc_shift(1), 1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3), 1+loc_shift(4)-w_adj,
     &                  s4+loc_shift(4)+e_adj)
           call store4 (rind, 6, se_my_nrows+1+loc_shift(1), 
     &                  se_my_nrows+sdis+loc_shift(1), 1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3), 1-wdis+loc_shift(4), 
     &                  loc_shift(4))
           call store4 (rind, 7, 1+loc_shift(1)-n_adj, 
     &                  se_my_nrows+loc_shift(1)+s_adj,
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  1+loc_shift(3), s3+loc_shift(3), 
     &                  1-wdis+loc_shift(4), loc_shift(4))
           call store4 (rind, 8, 1-ndis+loc_shift(1), loc_shift(1), 
     &                  1+loc_shift(2), se_my_ncols+loc_shift(2), 
     &                  1+loc_shift(3), s3+loc_shift(3), 
     &                  1-wdis+loc_shift(4), loc_shift(4))
        else
           call store4 (rind, 1, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1-ndis+loc_shift(2), loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3), 1+loc_shift(4)-w_adj,
     &                  s4+loc_shift(4)+e_adj)
           call store4 (rind, 2, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1-ndis+loc_shift(2), loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3), s4+1+loc_shift(4), 
     &                  s4+edis+loc_shift(4))
           call store4 (rind, 3, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2)-n_adj, se_my_ncols+loc_shift(2)+s_adj,
     &                  1+loc_shift(3), s3+loc_shift(3), 
     &                  s4+1+loc_shift(4), 
     &                  s4+edis+loc_shift(4))
           call store4 (rind, 4, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  se_my_ncols+1+loc_shift(2), 
     &                  se_my_ncols+sdis+loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3), s4+1+loc_shift(4), 
     &                  s4+edis+loc_shift(4))
           call store4 (rind, 5, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  se_my_ncols+1+loc_shift(2), 
     &                  se_my_ncols+sdis+loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3), 1+loc_shift(4)-w_adj,
     &                  s4+loc_shift(4)+e_adj)
           call store4 (rind, 6, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  se_my_ncols+1+loc_shift(2), 
     &                  se_my_ncols+sdis+loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3), 1-wdis+loc_shift(4), 
     &                  loc_shift(4))
           call store4 (rind, 7, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1+loc_shift(2)-n_adj, se_my_ncols+loc_shift(2)+s_adj,
     &                  1+loc_shift(3), s3+loc_shift(3), 
     &                  1-wdis+loc_shift(4), loc_shift(4))
           call store4 (rind, 8, 1+loc_shift(1), se_my_nrows+loc_shift(1), 
     &                  1-ndis+loc_shift(2), loc_shift(2), 1+loc_shift(3), 
     &                  s3+loc_shift(3), 1-wdis+loc_shift(4), 
     &                  loc_shift(4))
        end if

!       if ((se_data_ori .eq. "cr") .or. (se_data_ori .eq. "CR")) then
           do i = 1, 8
              call swap (sind(1,1,i), sind(1,2,i))
              call swap (sind(2,1,i), sind(2,2,i))
              call swap (rind(1,1,i), rind(1,2,i))
              call swap (rind(2,1,i), rind(2,2,i))
           end do
!       end if

        return
        end subroutine se_up_low4

! --------------------------------------------------------------------------
! Purpose:
!
!   to store low and high array indexes of a given domain with ghost cells
!
! Revision history:
!
!   Orginal version: 7/28/98 by David Wong
!                    11/05/99 by David Wong
!                      -- recode the code using F90 syntax
!
! Subroutine parameter description:
!
!   In:  direction -- communication direction
!        i1        -- low index of I dimension
!        i2        -- high index of I dimension
!        j1        -- low index of J dimension
!        j2        -- high index of J dimension
!        k1        -- low index of K dimension
!        k2        -- high index of K dimension
!        l1        -- low index of L dimension
!        l2        -- high index of L dimension
!
!   Out: array     -- array with low and high indexes of each dimension
! --------------------------------------------------------------------------

        subroutine store4 (array, dir, i1, i2, j1, j2, k1, k2, l1, l2)

        integer, intent(out) :: array (2,4,8) 
        integer, intent(in) :: dir, i1, i2, j1, j2, k1, k2, l1, l2

          array(1,1,dir) = i1
          array(2,1,dir) = i2
          array(1,2,dir) = j1
          array(2,2,dir) = j2
          array(1,3,dir) = k1
          array(2,3,dir) = k2
          array(1,4,dir) = l1
          array(2,4,dir) = l2

        return
        end subroutine store4

! --------------------------------------------------------------------------

        subroutine se_up_low5 (dispstr, sind, rind, shift, num_shift, s1, s2, s3)

        use se_domain_info_ext
        use se_comm_info_ext
!       use se_ori_ext
        use se_internal_util_module

        use se_pe_info_ext

        implicit none

        integer, intent(inout) :: sind(2,5,8), rind(2,5,8)
        character (len = 12), intent(in) :: dispstr
        integer, intent(in) :: shift(10), num_shift, s1, s2, s3

        integer :: ndis, edis, sdis, wdis, loc_shift(5), i
        integer :: ldecomp(4)
        integer :: n_adj, e_adj, s_adj, w_adj

        if (num_shift .gt. 0) then
           if (shift(1) .eq. 1) then
              loc_shift(1) = 1 - shift(4)
              loc_shift(2) = 1 - shift(2)
           else
              loc_shift(1) = 1 - shift(2)
              loc_shift(2) = 1 - shift(4)
           end if
           loc_shift(3) = 1 - shift(6)
           loc_shift(4) = 1 - shift(8)
           loc_shift(5) = 1 - shift(10)
        else
           loc_shift(1:5) = 0
        end if

        read (se_decompstr, 15) (ldecomp(i), i=1, 4)
 15     format (8i2)

        read (dispstr, 20) sdis, edis, ndis, wdis
 20     format (4i3)

        call se_corner_adjust (ndis, edis, sdis, wdis, 1,
     &                         n_adj, e_adj, s_adj, w_adj)

! -- ( sending ) determine ghost cells indexes configuration which
! -- depends on spatial decomposition type 1, 2, 3, 4, 5, and 6, respectively
        if ((ldecomp(1) + ldecomp(3) .eq. 1) .and. 
     &      (ldecomp(1) .eq. 1) .and. (ldecomp(4) .eq. 0)) then
           call store5 (sind, 1, 1+loc_shift(1), sdis+loc_shift(1),
     &                  1+loc_shift(2)-w_adj, se_my_ncols+loc_shift(2)+e_adj,
     &                  1+loc_shift(3), s1+loc_shift(3), 
     &                  1+loc_shift(4), s2+loc_shift(4), 
     &                  1+loc_shift(5), s3+loc_shift(5))
           call store5 (sind, 2, 1+loc_shift(1), sdis+loc_shift(1),
     &                  se_my_ncols-wdis+1+loc_shift(2),
     &                  se_my_ncols+loc_shift(2), 1+loc_shift(3), 
     &                  s1+loc_shift(3), 1+loc_shift(4), 
     &                  s2+loc_shift(4), 1+loc_shift(5), 
     &                  s3+loc_shift(5))
           call store5 (sind, 3, 1+loc_shift(1)-n_adj,
     &                  se_my_nrows+loc_shift(1)+s_adj,
     &                  se_my_ncols-wdis+1+loc_shift(2),
     &                  se_my_ncols+loc_shift(2), 1+loc_shift(3), 
     &                  s1+loc_shift(3), 1+loc_shift(4), 
     &                  s2+loc_shift(4), 1+loc_shift(5), 
     &                  s3+loc_shift(5))
           call store5 (sind, 4, se_my_nrows-ndis+1+loc_shift(1), 
     &                  se_my_nrows+loc_shift(1), 
     &                  se_my_ncols-wdis+1+loc_shift(2), 
     &                  se_my_ncols+loc_shift(2), 1+loc_shift(3), 
     &                  s1+loc_shift(3), 1+loc_shift(4), 
     &                  s2+loc_shift(4), 1+loc_shift(5), 
     &                  s3+loc_shift(5))
           call store5 (sind, 5, se_my_nrows-ndis+1+loc_shift(1),
     &                  se_my_nrows+loc_shift(1), 1+loc_shift(2)-w_adj,
     &                  se_my_ncols+loc_shift(2)+e_adj, 1+loc_shift(3), 
     &                  s1+loc_shift(3), 1+loc_shift(4), 
     &                  s2+loc_shift(4), 1+loc_shift(5), 
     &                  s3+loc_shift(5))
           call store5 (sind, 6, se_my_nrows-ndis+1+loc_shift(1), 
     &                  se_my_nrows+loc_shift(1), 1+loc_shift(2), 
     &                  edis+loc_shift(2), 1+loc_shift(3), 
     &                  s1+loc_shift(3), 1+loc_shift(4), 
     &                  s2+loc_shift(4), 1+loc_shift(5), 
     &                  s3+loc_shift(5))
           call store5 (sind, 7, 1+loc_shift(1)-n_adj,
     &                  se_my_nrows+loc_shift(1)+s_adj,
     &                  1+loc_shift(2), edis+loc_shift(2), 1+loc_shift(3), 
     &                  s1+loc_shift(3), 1+loc_shift(4), 
     &                  s2+loc_shift(4), 1+loc_shift(5), 
     &                  s3+loc_shift(5))
           call store5 (sind, 8, 1+loc_shift(1), sdis+loc_shift(1), 
     &                  1+loc_shift(2), edis+loc_shift(2), 1+loc_shift(3), 
     &                  s1+loc_shift(3), 1+loc_shift(4), 
     &                  s2+loc_shift(4), 1+loc_shift(5), 
     &                  s3+loc_shift(5))
        else
           print *, ' ==d== comm5 unknown pattern '
           stop
        end if

        call se_corner_adjust (ndis, edis, sdis, wdis, 2,
     &                         n_adj, e_adj, s_adj, w_adj)

! -- ( receiving ) determine ghost cells indexes configuration which
! -- depends on spatial decomposition, type 1, 2, and 3, respectively
        if ((ldecomp(1) + ldecomp(3) .eq. 1) .and.
     &      (ldecomp(1) .eq. 1) .and. (ldecomp(4) .eq. 0)) then
           call store5 (rind, 1, 1-ndis+loc_shift(1), loc_shift(1),
     &                  1+loc_shift(2)-w_adj, se_my_ncols+loc_shift(2)+e_adj,
     &                  1+loc_shift(3), s1+loc_shift(3), 
     &                  1+loc_shift(4), s2+loc_shift(4), 
     &                  1+loc_shift(5), s3+loc_shift(5))
           call store5 (rind, 2, 1-ndis+loc_shift(1), loc_shift(1), 
     &                  se_my_ncols+1+loc_shift(2), 
     &                  se_my_ncols+edis+loc_shift(2), 1+loc_shift(3), 
     &                  s1+loc_shift(3), 1+loc_shift(4),
     &                  s2+loc_shift(4), 1+loc_shift(5),
     &                  s3+loc_shift(5))
           call store5 (rind, 3, 1+loc_shift(1)-n_adj,
     &                  se_my_nrows+loc_shift(1)+s_adj,
     &                  se_my_ncols+1+loc_shift(2),
     &                  se_my_ncols+edis+loc_shift(2), 1+loc_shift(3), 
     &                  s1+loc_shift(3), 1+loc_shift(4),
     &                  s2+loc_shift(4), 1+loc_shift(5),
     &                  s3+loc_shift(5))
           call store5 (rind, 4, se_my_nrows+1+loc_shift(1), 
     &                  se_my_nrows+sdis+loc_shift(1), 
     &                  se_my_ncols+1+loc_shift(2), 
     &                  se_my_ncols+edis+loc_shift(2), 1+loc_shift(3), 
     &                  s1+loc_shift(3), 1+loc_shift(4), 
     &                  s2+loc_shift(4), 1+loc_shift(5), 
     &                  s3+loc_shift(5))
           call store5 (rind, 5, se_my_nrows+1+loc_shift(1),
     &                  se_my_nrows+sdis+loc_shift(1), 1+loc_shift(2)-w_adj,
     &                  se_my_ncols+loc_shift(2)+e_adj, 1+loc_shift(3), 
     &                  s1+loc_shift(3), 1+loc_shift(4), 
     &                  s2+loc_shift(4), 1+loc_shift(5), 
     &                  s3+loc_shift(5))
           call store5 (rind, 6, se_my_nrows+1+loc_shift(1), 
     &                  se_my_nrows+sdis+loc_shift(1), 1-wdis+loc_shift(2), 
     &                  loc_shift(2), 1+loc_shift(3), s1+loc_shift(3), 
     &                  1+loc_shift(4), s2+loc_shift(4),
     &                  1+loc_shift(5), s3+loc_shift(5))
           call store5 (rind, 7, 1+loc_shift(1)-n_adj,
     &                  se_my_nrows+loc_shift(1)+s_adj,
     &                  1-wdis+loc_shift(2), loc_shift(2), 1+loc_shift(3), 
     &                  s1+loc_shift(3), 1+loc_shift(4), 
     &                  s2+loc_shift(4), 1+loc_shift(5), 
     &                  s3+loc_shift(5))
           call store5 (rind, 8, 1-ndis+loc_shift(1), loc_shift(1), 
     &                  1-wdis+loc_shift(2), loc_shift(2), 1+loc_shift(3), 
     &                  s1+loc_shift(3), 1+loc_shift(4), 
     &                  s2+loc_shift(4), 1+loc_shift(5), 
     &                  s3+loc_shift(5))
        else
           print *, ' ==d== comm5 unknown pattern '
           stop
        end if

        do i = 1, 8
           call swap (sind(1,1,i), sind(1,2,i))
           call swap (sind(2,1,i), sind(2,2,i))
           call swap (rind(1,1,i), rind(1,2,i))
           call swap (rind(2,1,i), rind(2,2,i))
        end do

        end subroutine se_up_low5

! --------------------------------------------------------------------------
        subroutine store5 (array, dir, i1, i2, j1, j2, k1, k2, l1, l2, m1, m2)

        integer, intent(out) :: array (2,5,8) 
        integer, intent(in) :: dir, i1, i2, j1, j2, k1, k2, l1, l2, m1, m2

          array(1,1,dir) = i1
          array(2,1,dir) = i2
          array(1,2,dir) = j1
          array(2,2,dir) = j2
          array(1,3,dir) = k1
          array(2,3,dir) = k2
          array(1,4,dir) = l1
          array(2,4,dir) = l2
          array(1,5,dir) = m1
          array(2,5,dir) = m2

        end subroutine store5

        end module se_comm_module
