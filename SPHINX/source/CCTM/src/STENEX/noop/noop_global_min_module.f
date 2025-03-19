
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
C $Header: /project/work/rep/STENEX/src/noop_f90/noop_global_min_module.f,v 1.1.1.1 2000/04/12 17:40:55 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C --------------------------------------------------------------------------
C Purpose:
C
C   use F90 interface feature to achieve "faked" polymorphism for noop global 
C min routine
C
C Revision history:
C
C   Orginal version: 11/05/99 by David Wong
C -----------------------------------------------------------------------------

        module noop_global_min_module

        implicit none

        interface noop_global_min
          module procedure noop_global_imin, noop_global_rmin
        end interface

        contains

C -----------------------------------------------------------------------------
C Purpose: 
C
C   to provide a no-op counter part for serial code se_global_imin.f
C
C Revision history:
C
C   Orginal version: 6/9/99 by David Wong
C                    11/05/99 by David Wong
C                      -- recode using F90 syntax
C
C Parameter List:
C
C   In: var -- input variable
C -----------------------------------------------------------------------------

        function noop_global_imin (var) result (noop_global_imin_result)

        implicit none

        integer, intent(in) :: var
        integer :: noop_global_imin_result

        noop_global_imin_result = var

        end function noop_global_imin 

C -----------------------------------------------------------------------------
C Purpose: 
C
C   to provide a no-op counter part for serial code se_global_rmin.f
C
C Revision history:
C
C   Orginal version: 6/9/99 by David Wong
C                    11/05/99 by David Wong
C                      -- recode using F90 syntax
C
C Parameter List:
C
C   In: var -- input variable
C -----------------------------------------------------------------------------

        function noop_global_rmin (var) result (noop_global_rmin_result)

        real none

        real, intent(in) :: var
        real :: noop_global_rmin_result

        noop_global_rmin_result = var

        end function noop_global_rmin 

        end module noop_global_min_module
