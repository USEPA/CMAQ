
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
C $Header: /project/work/rep/STENEX/src/se_snl/se_global_max_module.f,v 1.2 2006/02/15 14:41:56 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C --------------------------------------------------------------------------
C Purpose:
C
C   use F90 interface feature to achieve "faked" polymorphism for global max
C   determining routine
C
C Revision history:
C
C   Orginal version: 11/05/99 by David Wong
C
C                    12/04/02 by David Wong
C                       -- modified the routine to accommodate worker and
C                          I/O processors partition scheme
C
C Parameter List:
C
C   In: var -- distributed variable which needs be determined the max value
C              among all processors
C
C Local Variable:
C
C   max   -- maximum value among all processors
C   error -- error code of mpi call
C
C Include Files:
C
C   mpif.h
C -----------------------------------------------------------------------------

        module se_global_max_module

        use se_pe_info_ext

        implicit none

        interface se_global_max
          module procedure se_global_imax, se_global_rmax
        end interface

        contains

C -----------------------------------------------------------------------------
        function se_global_imax (var) result (se_global_imax_result)

        implicit none

        integer :: se_global_imax_result
        integer, intent(in) :: var

        include "mpif.h"

        integer :: max, error

        call mpi_reduce (var, max, 1, mpi_integer, MPI_MAX, 0, 
     &                   se_worker_comm, error)

        call mpi_bcast (max, 1, mpi_integer, 0, se_worker_comm, error)

        se_global_imax_result = max

        return
        end function se_global_imax

C -----------------------------------------------------------------------------
        function se_global_rmax (var) result (se_global_rmax_result)

        implicit none

        real :: se_global_rmax_result
        real, intent(in) :: var

        include "mpif.h"

        real :: max
        integer :: error

        call mpi_reduce (var, max, 1, mpi_real, MPI_MAX, 0, 
     &                   se_worker_comm, error)

        call mpi_bcast (max, 1, mpi_real, 0, se_worker_comm, error)

        se_global_rmax_result = max

        return
        end function se_global_rmax

        end module se_global_max_module
