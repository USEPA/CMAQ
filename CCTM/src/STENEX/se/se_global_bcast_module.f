
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

! what(1) key, module and SID; SCCS file; date and time of last delta:
! %W% %P% %G% %U%

! --------------------------------------------------------------------------
! Purpose:
!
!   use F90 interface feature to achieve "faked" polymorphism for global broadcast
!   operation from PE 0
!
! Revision history:
!
!   Orginal version: 12/14/21 by David Wong
!
! Parameter List:
!
! Local Variable:
!
! Include Files:
!
!   mpif.h
C -----------------------------------------------------------------------------

        module se_global_bcast_module

          use se_pe_info_ext, only : se_worker_comm, se_myworker_pe, se_numworkers
          use se_domain_info_ext, only : se_gl_ind

          implicit none

          interface se_global_bcast
            module procedure se_global_bcast_2d_r,
     $                       se_global_bcast_2d_i,
     $                       se_global_bcast_2d_l
          end interface

          contains

! -----------------------------------------------------------------------------
          subroutine se_global_bcast_2d_r (indata)

            real, intent(in)  :: indata(:,:)

            include "mpif.h"

            integer :: data_size, error

            data_size = size(indata,1) * size(indata,2)

            call mpi_bcast (indata, data_size, mpi_real, 0, se_worker_comm, error)

          end subroutine se_global_bcast_2d_r

! -----------------------------------------------------------------------------
          subroutine se_global_bcast_2d_i (indata)

            integer, intent(in)  :: indata(:,:)

            include "mpif.h"

            integer :: data_size, error

            data_size = size(indata,1) * size(indata,2)

            call mpi_bcast (indata, data_size, mpi_integer, 0, se_worker_comm, error)

          end subroutine se_global_bcast_2d_i

! -----------------------------------------------------------------------------
          subroutine se_global_bcast_2d_l (indata)

            logical, intent(in)  :: indata(:,:)

            include "mpif.h"

            integer :: data_size, error

            data_size = size(indata,1) * size(indata,2)

            call mpi_bcast (indata, data_size, mpi_logical, 0, se_worker_comm, error)

          end subroutine se_global_bcast_2d_l

        end module se_global_bcast_module
