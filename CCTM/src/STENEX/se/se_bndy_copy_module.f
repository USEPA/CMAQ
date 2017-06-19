
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
C $Header: /project/work/rep/STENEX/src/se_snl/se_bndy_copy_module.f,v 1.2 2006/02/15 14:41:56 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C --------------------------------------------------------------------------
C Purpose:
C
C   use F90 interface feature to achieve "faked" polymorphism for pe boundary
C exchange routine and use F90 module feature to modulize pe boundary exchange
C functionality of the stencil exchange library. Shallow Water Model is the 
C target application.
C
C Note: 
C
C   Currently only 2-D case is implemented.
C
C Revision history:
C
C   Orginal version: 11/28/01 by David Wong
C          Modified: 08/24/11 by David Wong
C                      -- eliminated data and geo orientation
C --------------------------------------------------------------------------

        module se_bndy_copy_module

        implicit none

        interface se_bndy_copy
          module procedure se_bndy_copy2
        end interface

        contains

C --------------------------------------------------------------------------
C Purpose:
C
C   determine the set of boundary exchange "neighbouring" processor(s) that 
C   my own processor needs to communicate with.
C
C Revision history:
C
C   Orginal version: 11/28/01 by David Wong
C                    08/24/11 by David Wong
C                      -- removed se_ori_ext to eliminate data and geo orientation
C
C Subroutine parameter description:
C
C   In:  dirstr    -- indicator of communication direction
C                     0 (without communication), 1 (with communication)
C
C   Out: send_to   -- processor number which data needs to be sent to
C        recv_from -- processor number which data is recvd from
C
C Local variable description:
C
C   rdirection -- input communication direction for receiveing
C   sdirection -- communication direction for sending
C   i, j       -- loop indexes
C   k          -- location indicator
C --------------------------------------------------------------------------

        subroutine se_bndy_copy_pat (dirstr, send_to, recv_from)

        use se_bndy_copy_info_ext
!       use se_ori_ext
        use se_internal_util_module

        implicit none

        character (len = 16), intent(in) :: dirstr
        integer, intent(out) :: send_to (8), recv_from (8)

        integer :: i, j, k
        integer :: rdirection (8), sdirection(8)
        
C -- extract inform from input strings

        read (dirstr, 10) (sdirection(i), i=1, 8)
 10     format (8i2)

C -- figuring out send direction pattern
        do i = 1, 8
           rdirection(i) = sdirection(mod(i+3,8)+1)
        end do

C -- determine where data is receiving from
C -- first: N, E, S, and W
        do i = 1, 7, 2
           if ((rdirection (i) .gt. 0) .and.
     &         (se_bngb_pe(i) .ge. 0)) then
              recv_from(i) = se_bngb_pe(i)
           else
              recv_from(i) = -1
           end if
        end do

C -- second: NE, SE, SW, NW, and their immediate neighbours
C --         for instance, NE's immediate neighbours are N, and E
        do i = 2, 8, 2
           if (rdirection(i) .gt. 0) then
              do j = i-1, i+1
                 k = j
                 if (k .eq. 9) then
                    k = 1
                  end if 
                  if (se_bngb_pe(k) .ge. 0) then
                     recv_from(k) = se_bngb_pe(k)
                  else
                     recv_from(k) = -1
                  end if
              end do
           else
              recv_from(i) = -1
           end if
        end do

C -- determine where data is sending to
C -- first: N, E, S, and W
        do i = 1, 7, 2
           if ((sdirection (i) .gt. 0) .and.
     &         (se_bngb_pe(i) .ge. 0)) then
              send_to(i) = se_bngb_pe(i)
           else
              send_to(i) = -1
           end if
        end do

C -- second: NE, SE, SW, NW, and their immediate neighbours
C --         for instance, NE's immediate neighbours are N, and E
        do i = 2, 8, 2
           if (sdirection(i) .gt. 0) then
              do j = i-1, i+1
                 k = j
                 if (k .eq. 9) then
                    k = 1
                  end if 
                  if (se_bngb_pe(k) .ge. 0) then
                     send_to(k) = se_bngb_pe(k)
                  else
                     send_to(k) = -1
                  end if
              end do
           else
              send_to(i) = -1
           end if
        end do

        return
        end subroutine se_bndy_copy_pat

C --------------------------------------------------------------------------
C Purpose:
C
C   perform boundary exchange communication in the global scenario
C
C   Example:
C
C     Give a global data, A (10, 20)
C
C     various exchange examples:
C
C       -- copying data from A(1, 1:19) to A(10, 1:19)
C       -- copying data from (10,20) to A(1,1)
C
C Revision history:
C
C   Orginal version: 11/28/01 by David Wong
C                    03/06/02 David Wong
C                      -- use blocking communication scheme
C                      -- use array copy mechanism when communicates to itself
C                    12/04/02 by David Wong
C                       -- modified the routine to accommodate worker and
C                          I/O processors partition scheme
C
C Subroutine parameter description:
C
C   In:  data    -- original data
C        dispstr -- displacement string
C        dirstr  -- indicator of communication direction
C                   0 (without communication), 1 (with communication)
C        str     -- an optional argument to indicate the starting index of
C                   certain dimension
C
C   Out: data    -- original data after communication
C
C Local variable description:
C
C    send_to       -- processor number which data needs to be sent to
C    send_to_ptr   -- a F90 pointer (alias) of send_to
C    recv_from     -- processor number which data is recvd from
C    recv_from_ptr -- a F90 pointer (alias) of recv_from
C    sdir, rdir    -- loop indexes which indicate send to or recvd from
C    sind          -- store low and high index of each dimension for sending
C                     process
C    sind_ptr      -- a F90 pointer (alias) of sind
C    rind          -- store low and high index of each dimension for receiving
C                     process
C    rind_ptr      -- a F90 pointer (alias) of rind
C    shift         -- an array to hold the amount of index shifting due to
C                     starting index is 1 in a subroutine
C    num_shift     -- number of shifting
C    loc_str       -- a local copy of str
C    request       -- MPI sending request status
C    status        -- MPI waiting status
C    error         -- MPI call return error code
C --------------------------------------------------------------------------

        subroutine se_bndy_copy2 (data, dispstr, dirstr, str)

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

        call se_bndy_copy_pat (dirstr, send_to, recv_from)

        call se_bndy_up_low2 (dispstr, sind, rind, shift, num_shift)

        send_to_ptr => send_to
        recv_from_ptr => recv_from
        sind_ptr => sind
        rind_ptr => rind

        do sdir = 1, 8

           rdir = mod((sdir + 3), 8) + 1

           if (send_to(sdir) .eq. se_myworker_pe) then

              data(rind(1,1,rdir):rind(2,1,rdir),
     $             rind(1,2,rdir):rind(2,2,rdir))
     $        =
     $        data(sind(1,1,sdir):sind(2,1,sdir),
     $             sind(1,2,sdir):sind(2,2,sdir))

           else

              if (send_to(sdir) .ge. 0) then
                 call se_data_send (data, sind_ptr, send_to_ptr, sdir, sdir,
     $                              request)
              end if

              if ((recv_from(rdir) .ge. 0) .and.
     $            (recv_from(rdir) .ne. se_myworker_pe)) then
                 call se_data_recv (data, rind_ptr, recv_from_ptr, rdir, sdir)
              end if

c             if (send_to(sdir) .ge. 0) then
c                call mpi_wait (request, status, error)
c             end if

           end if
        end do

        return
        end subroutine se_bndy_copy2

C --------------------------------------------------------------------------
C Purpose:
C
C   initialize variables sind and rind
C
C Revision history:
C
C   Orginal version: 11/27/01 by David Wong
C                    08/24/11 by David Wong
C                      -- removed se_ori_ext to eliminate data and geo orientation
C
C Subroutine parameter description:
C
C   In:  dispstr   -- displacement string
C        shift     -- an array to hold the amount of index shifting due to
C                     starting index is 1 in a subroutine
C        num_shift -- number of shifting
C
C   Out: sind    -- store low and high index of each dimension for sending
C                   process
C        rind    -- store low and high index of each dimension for receiving
C                   process
C
C Local variable description:
C
C    ndis      -- North displacement
C    edis      -- East displacement
C    sdis      -- South displacement
C    wdis      -- West displacement
C    i         -- loop index
C    loc_shift -- local adjustment of dummy argument shift
C --------------------------------------------------------------------------

        subroutine se_bndy_up_low2 (dispstr, sind, rind, shift, num_shift)

        use se_domain_info_ext
!       use se_ori_ext
        use se_internal_util_module

        implicit none

        integer, intent(inout) :: sind(2,2,8), rind(2,2,8)
        character (len = 12), intent(in) :: dispstr
        integer, intent(in) :: shift(4), num_shift

        integer :: ndis, edis, sdis, wdis, loc_shift(2), i

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
           read (dispstr, *) sdis, edis, ndis, wdis
!       else
!          read (dispstr, *) ndis, edis, sdis, wdis
!       end if

C -- ( sending ) determine ghost cells indexes configuration 
        call store2 (sind, 1, se_my_nrows-sdis+1+loc_shift(1),
     &               se_my_nrows+loc_shift(1), 1+loc_shift(2),
     &               se_my_ncols+loc_shift(2))
        call store2 (sind, 2, se_my_nrows-sdis+1+loc_shift(1),
     &               se_my_nrows+loc_shift(1), 1+loc_shift(2), 
     &               wdis+loc_shift(2))
        call store2 (sind, 3, 1+loc_shift(1), se_my_nrows+loc_shift(1),
     &               1+loc_shift(2), wdis+loc_shift(2))
        call store2 (sind, 4, 1+loc_shift(1), ndis+loc_shift(1),
     &               1+loc_shift(2), wdis+loc_shift(2))
        call store2 (sind, 5, 1+loc_shift(1), ndis+loc_shift(1),
     &               1+loc_shift(2), se_my_ncols+loc_shift(2))
        call store2 (sind, 6, 1+loc_shift(1), ndis+loc_shift(1),
     &               se_my_ncols-edis+1+loc_shift(2), se_my_ncols+loc_shift(2))
        call store2 (sind, 7, 1+loc_shift(1), se_my_nrows+loc_shift(1),
     &               se_my_ncols-edis+1+loc_shift(2), se_my_ncols+loc_shift(2))
        call store2 (sind, 8, se_my_nrows-sdis+1+loc_shift(1),
     &               se_my_nrows+loc_shift(1), se_my_ncols-edis+1+loc_shift(2),
     &               se_my_ncols+loc_shift(2))

C -- ( receiving ) determine ghost cells indexes configuration 
        call store2 (rind, 5, 1+loc_shift(1), sdis+loc_shift(1),
     &               1+loc_shift(2), se_my_ncols+loc_shift(2))

        call store2 (rind, 6, 1+loc_shift(1), sdis+loc_shift(1),
     &               se_my_ncols-wdis+1+loc_shift(2), se_my_ncols+loc_shift(2))

        call store2 (rind, 7, 1+loc_shift(1), se_my_nrows+loc_shift(1),
     &               se_my_ncols-wdis+1+loc_shift(2), se_my_ncols+loc_shift(2))

        call store2 (rind, 8, se_my_nrows-ndis+1+loc_shift(1), 
     &               se_my_nrows+loc_shift(1), 
     &               se_my_ncols-wdis+1+loc_shift(2), se_my_ncols+loc_shift(2))

        call store2 (rind, 1, se_my_nrows-ndis+1+loc_shift(1), 
     &               se_my_nrows+loc_shift(1), 1+loc_shift(2),
     &               se_my_ncols+loc_shift(2))

        call store2 (rind, 2, se_my_nrows-ndis+1+loc_shift(1), 
     &               se_my_nrows+loc_shift(1), 1+loc_shift(2), 
     &               edis+loc_shift(2))

        call store2 (rind, 3, 1+loc_shift(1), se_my_nrows+loc_shift(1),
     &               1+loc_shift(2), edis+loc_shift(2))

        call store2 (rind, 4, 1+loc_shift(1), sdis+loc_shift(1),
     &               1+loc_shift(2), edis+loc_shift(2))

!       if ((se_data_ori .eq. "cr") .or. (se_data_ori .eq. "CR")) then
           do i = 1, 8
              call swap (sind(1,1,i), sind(1,2,i))
              call swap (sind(2,1,i), sind(2,2,i))
              call swap (rind(1,1,i), rind(1,2,i))
              call swap (rind(2,1,i), rind(2,2,i))
           end do
!       end if

        return
        end subroutine se_bndy_up_low2

C --------------------------------------------------------------------------
C Purpose:
C
C   to store low and high array indexes (same as in se_comm_module.f)
C
C Revision history:
C
C   Orginal version: 7/28/98 by David Wong
C
C Subroutine parameter description:
C
C   In:  direction -- communication direction
C        i1        -- low index of I dimension
C        i2        -- high index of I dimension
C        j1        -- low index of J dimension
C        j2        -- high index of J dimension
C
C   Out: array     -- array with low and high indexes of each dimension
C --------------------------------------------------------------------------

        subroutine store2 (array, direction, i1, i2, j1, j2)

        integer, intent(out) :: array (2,2,8) 
        integer, intent(in) :: direction, i1, i2, j1, j2

          array(1,1,direction) = i1
          array(2,1,direction) = i2
          array(1,2,direction) = j1
          array(2,2,direction) = j2

        return
        end subroutine store2

        end module se_bndy_copy_module
