
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
C $Header: /project/work/rep/STENEX/src/se_snl/swap_sandia_routines.f,v 1.3 2007/07/11 14:58:58 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

c Swap 1d,2d,3d,4d routines

C Revision history:
C Aug 18, 2011: David Wong - fixed bug in swap4d (index calculation is incorrect)

c inter-processor swap within a 1d array

      subroutine swap1d( send, recv, n1, dir )
      use swap_sandia
      implicit none
      include 'mpif.h'

      real send(*)           ! 1st value to be sent
      real recv(*)           ! 1st value to be received
      integer n1             ! # of values to send in 1d array
      integer dir	     ! direction to recv from: NSEW

      integer i
      integer recvproc, sendproc
      integer irequest, istatus( MPI_STATUS_SIZE ), ierror
      real, allocatable :: rbuf( : ), sbuf( : )

      recvproc = recvneigh( dir )
      sendproc = sendneigh( dir )

      if ( recvproc >= 0 ) then

        allocate( rbuf( n1 ) )

        call MPI_Irecv( rbuf, n1, MPI_REAL, recvproc, 0, MPI_COMM_WORLD,
     &                  irequest, ierror )

      endif

      if ( sendproc >= 0 ) then
  
        allocate( sbuf( n1 ) )
  
        do i = 1, n1
          sbuf( i ) = send( i )
        end do
  
        call MPI_Send( sbuf, n1, MPI_REAL, sendproc, 0, MPI_COMM_WORLD,
     &                 ierror )
  
        deallocate( sbuf )
  
      end if
  
      if ( recvproc >= 0 ) then
  
        call MPI_Wait( irequest, istatus, ierror )
  
        do i = 1, n1
         recv( i ) = rbuf( i )
        end do
  
        deallocate( rbuf )
  
      end if
  
      return
      end subroutine swap1d

c inter-processor swap within a 2d array

      subroutine swap2d(send,recv,n1,n2,dim1,dir)
      use swap_sandia
      implicit none
      include 'mpif.h'

      real send(*)           ! 1st value to be sent
      real recv(*)           ! 1st value to be received
      integer n1,n2          ! # of values to send in each dim of 2d array
      integer dim1           ! 1st dimension of 2d array
      integer dir	     ! direction to recv from: NSEW

      integer i,j,m,n,ntotal,offset_ij
      integer recvproc,sendproc
      integer irequest,istatus(MPI_STATUS_SIZE),ierror
      real, allocatable :: rbuf(:),sbuf(:)

      ntotal = n1*n2
      offset_ij = dim1 - n1
      recvproc = recvneigh(dir)
      sendproc = sendneigh(dir)

      if (recvproc >= 0) then
        allocate(rbuf(ntotal))
        call MPI_Irecv(rbuf,ntotal,MPI_REAL,recvproc,0,MPI_COMM_WORLD,
     $       irequest,ierror)
      endif

      if (sendproc >= 0) then
  
        allocate(sbuf(ntotal))
  
        m = 1
        n = 1

        do j = 1,n2
          do i = 1,n1
            sbuf(n) = send(m)
            n = n + 1
            m = m + 1
          enddo
          m = m + offset_ij
        enddo
  
        call MPI_Send(sbuf,ntotal,MPI_REAL,sendproc,0,MPI_COMM_WORLD,
     $       ierror)
  
        deallocate(sbuf)
  
      endif
  
      if (recvproc >= 0) then
  
        call MPI_Wait(irequest,istatus,ierror)
  
        m = 1
        n = 1

        do j = 1,n2
          do i = 1,n1
            recv(m) = rbuf(n)
            n = n + 1
            m = m + 1
          enddo
          m = m + offset_ij
        enddo
  
        deallocate(rbuf)
  
      endif
  
      return
      end subroutine swap2d

c inter-processor swap within a 3d array

      subroutine swap3d(send,recv,n1,n2,n3,dim1,dim2,dir)
      use swap_sandia
      implicit none
      include 'mpif.h'

      real send(*)           ! 1st value to be sent
      real recv(*)           ! 1st value to be received
      integer n1,n2,n3       ! # of values to send in each dim of 3d array
      integer dim1,dim2      ! 1st and 2nd dimensions of 3d array
      integer dir	     ! direction to recv from: NSEW

      integer i,j,k,m,n,ntotal,offset_ij,offset_jk
      integer recvproc,sendproc
      integer irequest,istatus(MPI_STATUS_SIZE),ierror
      real, allocatable :: rbuf(:),sbuf(:)

      ntotal = n1*n2*n3
      offset_ij = dim1 - n1
      offset_jk = dim1*dim2 - n2*dim1
      recvproc = recvneigh(dir)
      sendproc = sendneigh(dir)

      if (recvproc >= 0) then
        allocate(rbuf(ntotal))
        call MPI_Irecv(rbuf,ntotal,MPI_REAL,recvproc,0,MPI_COMM_WORLD,
     $       irequest,ierror)
      endif

      if (sendproc >= 0) then
  
        allocate(sbuf(ntotal))
  
        m = 1
        n = 1

        do k = 1,n3
          do j = 1,n2
            do i = 1,n1
              sbuf(n) = send(m)
              n = n + 1
              m = m + 1
            enddo
            m = m + offset_ij
          enddo
          m = m + offset_jk
        enddo
  
        call MPI_Send(sbuf,ntotal,MPI_REAL,sendproc,0,MPI_COMM_WORLD,
     $       ierror)
  
        deallocate(sbuf)
  
      endif
  
      if (recvproc >= 0) then
  
        call MPI_Wait(irequest,istatus,ierror)
  
        m = 1
        n = 1

        do k = 1,n3
          do j = 1,n2
            do i = 1,n1
              recv(m) = rbuf(n)
              n = n + 1
              m = m + 1
            enddo
            m = m + offset_ij
          enddo
          m = m + offset_jk
        enddo
  
        deallocate(rbuf)
  
      endif
  
      return
      end subroutine swap3d

c inter-processor swap within a 4d array

      subroutine swap4d(send,recv,n1,n2,n3,n4,dim1,dim2,dim3,dir)
      use swap_sandia
      implicit none
      include 'mpif.h'

!     real send(*)           ! 1st value to be sent
!     real recv(*)           ! 1st value to be received
      real send(:,:,:,:)           ! 1st value to be sent
      real recv(:,:,:,:)           ! 1st value to be received
      integer n1,n2,n3,n4    ! # of values to send in each dim of 4d array
      integer dim1,dim2,dim3 ! 1st,2nd,3rd dimensions of 4d array
      integer dir	     ! direction to recv from: NSEW

      integer i,j,k,l,m,n,ntotal,offset_ij,offset_jk,offset_kl
      integer recvproc,sendproc
      integer irequest,istatus(MPI_STATUS_SIZE),ierror
!     real, allocatable :: rbuf(:),sbuf(:)

      ntotal = n1*n2*n3*n4
      offset_ij = dim1 - n1
      offset_jk = dim1*dim2 - n2*dim1
      offset_kl = dim1*dim2*dim3 - n3*dim1*dim2
      recvproc = recvneigh(dir)
      sendproc = sendneigh(dir)

      if (recvproc >= 0) then
!       allocate(rbuf(ntotal))
!       call MPI_Irecv(rbuf,ntotal,MPI_REAL,recvproc,0,MPI_COMM_WORLD,
        call MPI_Irecv(recv,ntotal,MPI_REAL,recvproc,0,MPI_COMM_WORLD,
     $       irequest,ierror)
      endif

      if (sendproc >= 0) then
  
!       allocate(sbuf(ntotal))
  
!       m = 1
!       n = 1

!       do l = 1,n4
!         do k = 1,n3
!           do j = 1,n2
!             do i = 1,n1
!               sbuf(n) = send(m)
!               n = n + 1
!               m = m + 1
!             enddo
!             m = m + offset_ij
!           enddo
!           m = m + offset_jk
!         enddo
!         m = m + offset_kl
!       enddo
! 
!       call MPI_Send(sbuf,ntotal,MPI_REAL,sendproc,0,MPI_COMM_WORLD,
        call MPI_Send(send,ntotal,MPI_REAL,sendproc,0,MPI_COMM_WORLD,
     $       ierror)
  
!       deallocate(sbuf)
  
      endif
  
      if (recvproc >= 0) then
  
        call MPI_Wait(irequest,istatus,ierror)
! 
!       m = 1
!       n = 1

!       do l = 1,n4
!         do k = 1,n3
!           do j = 1,n2
!             do i = 1,n1
!               recv(m) = rbuf(n)
!               n = n + 1
!               m = m + 1
!             enddo
!             m = m + offset_ij
!           enddo
!           m = m + offset_jk
!         enddo
!         m = m + offset_kl
!       enddo

!       deallocate(rbuf)
! 
      endif
  
      return
      end subroutine swap4d
