
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
C $Header: /project/work/rep/STENEX/src/noop_f90/noop_gather_module.f,v 1.2 2000/12/22 18:38:22 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C --------------------------------------------------------------------------
C Purpose:
C
C   use F90 module feature to include all routines to perform data gather 
C function in one module and use F90 interface feature to achieve "faked" 
C polymorphism for data gather routine
C
C Revision history:
C
C   Orginal version: 11/05/99 by David Wong
C   Add integer data 12/16/00 by Jeff Young
C --------------------------------------------------------------------------

        module noop_gather_module

        implicit none

        interface noop_gather
          module procedure noop_gather1i, noop_gather1r, 
     &                     noop_gather2i, noop_gather2r,
     &                     noop_gather3i, noop_gather3r,
     &                     noop_gather4i, noop_gather4r
        end interface

        contains

C --------------------------------------------------------------------------
C Purpose:
C
C   to provide a no-op noop_gather1.f counter part for serial code
C
C Revision history:
C
C   Orginal version: 7/14/98 by David Wong
C                    11/05/99 by David Wong
C                      -- recode the code using F90 syntax
C --------------------------------------------------------------------------

        subroutine noop_gather1r (data, pec, ptr, n, sdim)

        implicit none

        integer, intent(in) :: n, sdim, pec(:), ptr(:)
        real, intent(in) :: data(:)

        end subroutine noop_gather1r

C --------------------------------------------------------------------------
C Purpose:
C
C   to provide a no-op noop_gather2.f counter part for serial code
C
C Revision history:
C
C   Orginal version: 7/14/98 by David Wong
C                    11/05/99 by David Wong
C                      -- recode the code using F90 syntax
C --------------------------------------------------------------------------

        subroutine noop_gather2r (data, pec, ptr, n, sdim)

        implicit none

        integer, intent(in) :: n, sdim, pec(:), ptr(:)
        real, intent(in) :: data(:, :)

        end subroutine noop_gather2r

C --------------------------------------------------------------------------
C Purpose:
C
C   to provide a no-op noop_gather3.f counter part for serial code
C
C Revision history:
C
C   Orginal version: 6/23/98 by David Wong
C                    11/05/99 by David Wong
C                      -- recode the code using F90 syntax
C --------------------------------------------------------------------------

        subroutine noop_gather3r (data, pec, ptr, n, sdim)

        implicit none

        integer, intent(in) :: n, sdim, pec(:), ptr(:)
        real, intent(in) :: data(:, :, :)

        end subroutine noop_gather3r 

C --------------------------------------------------------------------------
C Purpose:
C
C   to provide a no-op noop_gather4.f counter part for serial code
C
C Revision history:
C
C   Orginal version: 7/14/98 by David Wong
C                    11/05/99 by David Wong
C                      -- recode the code using F90 syntax
C --------------------------------------------------------------------------

        subroutine noop_gather4r (data, pec, ptr, n, sdim)

        implicit none

        integer, intent(in) :: n, sdim, pec(:), ptr(:)
        real, intent(in) :: data(:, :, :, :)

        end subroutine noop_gather4r

C --------------------------------------------------------------------------

        subroutine noop_gather1i (data, pec, ptr, n, sdim)

        implicit none

        integer, intent(in) :: n, sdim, pec(:), ptr(:)
        integer, intent(in) :: data(:)

        end subroutine noop_gather1i

C --------------------------------------------------------------------------

        subroutine noop_gather2i (data, pec, ptr, n, sdim)

        implicit none

        integer, intent(in) :: n, sdim, pec(:), ptr(:)
        integer, intent(in) :: data(:, :)

        end subroutine noop_gather2i

C --------------------------------------------------------------------------

        subroutine noop_gather3i (data, pec, ptr, n, sdim)

        implicit none

        integer, intent(in) :: n, sdim, pec(:), ptr(:)
        integer, intent(in) :: data(:, :, :)

        end subroutine noop_gather3i 

C --------------------------------------------------------------------------

        subroutine noop_gather4i (data, pec, ptr, n, sdim)

        implicit none

        integer, intent(in) :: n, sdim, pec(:), ptr(:)
        integer, intent(in) :: data(:, :, :, :)

        end subroutine noop_gather4i

        end module noop_gather_module
