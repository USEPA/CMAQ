C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/work/rep/STENEX/src/noop_f90/noop_global_max_module.f,v 1.1.1.1 2000/04/12 17:40:55 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C --------------------------------------------------------------------------
C Purpose:
C
C   use F90 interface feature to achieve "faked" polymorphism for noop global 
C max routine
C
C Revision history:
C
C   Orginal version: 11/05/99 by David Wong
C -----------------------------------------------------------------------------

	module noop_global_max_module

        implicit none

        interface noop_global_max
          module procedure noop_global_imax, noop_global_rmax
        end interface

        contains

C -----------------------------------------------------------------------------
C Purpose: 
C
C   to provide a no-op counter part for serial code se_global_imax.f
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

        function noop_global_imax (var) result (noop_global_imax_result)

        implicit none

        integer, intent(in) :: var
        integer :: noop_global_imax_result

        noop_global_imax_result = var

        end function noop_global_imax 

C -----------------------------------------------------------------------------
C Purpose: 
C
C   to provide a no-op counter part for serial code se_global_rmax.f
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

        function noop_global_rmax (var) result (noop_global_rmax_result)

        implicit none

        real, intent(in) :: var
        real :: noop_global_rmax_result


        noop_global_rmax_result = var

        end function noop_global_rmax 

	end module noop_global_max_module
