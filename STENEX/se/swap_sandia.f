
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
C $Header: /project/work/rep/STENEX/src/se_snl/swap_sandia.f,v 1.2 2004/03/26 16:25:25 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

c shared variables for Swap 2d,3d,4d routines & init routine

      module swap_sandia

      implicit none

      integer, parameter :: NORTH = 1
      integer, parameter :: SOUTH = 2
      integer, parameter :: EAST  = 3
      integer, parameter :: WEST  = 4

      integer recvneigh(4)  ! 4 neighbor procs I recv from in each dir
      integer sendneigh(4)  ! 4 neighbor procs I send to when recv in each dir
                            ! value of -1 means no neighbor in that dir

c      interface swap_snl
c        module procedure swap2d,swap3d,swap4d
c      end interface

      contains

c set up proc IDs of 4 neighboring processors
c only needs to be called once
c decomposition is defined by:
c   npcol = # of column procs
c   nprow = # of row procs
c   proc 0 = SW corner of domain
c   proc 1 = east of proc 0
c   proc npcol = north of proc 0
c   proc npcol*nprow - 1 = NE corner of domain
c NOTE: sendneigh(NORTH) is not who I send to in NORTH dir,
c   but who I send to when I am receiving from the NORTH,
c   i.e. sendneigh(NORTH) = the proc to the south of me

      subroutine swap_init_snl(npcol,nprow)
      implicit none
      include 'mpif.h'

      integer npcol,nprow

      integer me,nprocs,error

      call MPI_Comm_rank(mpi_comm_world,me,error)
      call MPI_Comm_size(mpi_comm_world,nprocs,error)

      recvneigh(NORTH) = me + npcol
      recvneigh(SOUTH) = me - npcol
      recvneigh(EAST) = me + 1
      recvneigh(WEST) = me - 1

      sendneigh(NORTH) = me - npcol
      sendneigh(SOUTH) = me + npcol
      sendneigh(EAST) = me - 1
      sendneigh(WEST) = me + 1

      if (me < npcol) then                         ! south boundary
        recvneigh(SOUTH) = -1
        sendneigh(NORTH) = -1
      endif
      if (me >= nprocs-npcol) then                 ! north boundary
        recvneigh(NORTH) = -1
        sendneigh(SOUTH) = -1
      endif
      if (mod(me,npcol) == 0) then                 ! west boundary
        recvneigh(WEST) = -1
        sendneigh(EAST) = -1
      endif
      if (mod(me,npcol) == npcol-1) then           ! east boundary
        recvneigh(EAST) = -1
        sendneigh(WEST) = -1
      endif

      return
      end subroutine swap_init_snl

      end module swap_sandia
