
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

C --------------------------------------------------------------------------
C Purpose:
C
C   use F90 interface feature to achieve "faked" polymorphism for noop pe 
C communication routine 
C
C Revision history:
C
C   Orginal version: 11/05/99 by David Wong
C --------------------------------------------------------------------------

       module noop_comm_module

        implicit none

        interface noop_comm
          module procedure noop_pe_comm1, 
     &                     noop_pe_comm2, noop_pe_comm2e, 
     &                     noop_pe_comm3, noop_pe_comm3e, noop_pe_comm3s,
     &                     noop_pe_comm4
        end interface

        contains

C --------------------------------------------------------------------------
C Purpose:
C
C   to provide a no-op noop_comm1.f counter part for serial code
C
C Revision history:
C
C   Orginal version: 10/6/98 by David Wong
C                    11/05/99 by David Wong
C                      -- recode using F90 syntax
C --------------------------------------------------------------------------

        subroutine noop_pe_comm1 (data, dispstr, dirstr, str)

        implicit none

        real, intent(in) :: data(:)
        character (len = 16), intent(in) :: dirstr
        character (len = 12), intent(in) :: dispstr
        character (len = *), optional, intent(in) :: str

        end subroutine noop_pe_comm1 

C --------------------------------------------------------------------------
C Purpose:
C
C   to provide a no-op noop_comm2.f counter part for serial code
C
C Revision history:
C
C   Orginal version: 10/6/98 by David Wong
C                    11/05/99 by David Wong
C                      -- recode using F90 syntax
C --------------------------------------------------------------------------

        subroutine noop_pe_comm2 (data, dispstr, dirstr, str)

        implicit none

        real, intent(in) :: data(:, :)
        character (len = 16), intent(in) :: dirstr
        character (len = 12), intent(in) :: dispstr
        character (len = *), optional, intent(in) :: str

        end subroutine noop_pe_comm2 

C --------------------------------------------------------------------------
C Purpose:
C
C   to provide a no-op noop_comm2e.f counter part for serial code
C
C Revision history:
C
C   Orginal version: 10/6/98 by David Wong
C                    11/05/99 by David Wong
C                      -- recode using F90 syntax
C --------------------------------------------------------------------------

        subroutine noop_pe_comm2e (data, dispstr, dirstr, flag, str)

        implicit none

        real, intent(in) :: data(:, :)
        character (len = 16), intent(in) :: dirstr
        character (len = 12), intent(in) :: dispstr
        integer, intent(in) :: flag
        character (len = *), optional, intent(in) :: str

        end subroutine noop_pe_comm2e 

C --------------------------------------------------------------------------
C Purpose:
C
C   to provide a no-op noop_comm3.f counter part for serial code
C
C Revision history:
C
C   Orginal version: 10/6/98 by David Wong
C                    11/05/99 by David Wong
C                      -- recode using F90 syntax
C --------------------------------------------------------------------------

        subroutine noop_pe_comm3 (data, dispstr, dirstr, str)

        implicit none

        real, intent(in) :: data(:, :, :)
        character (len = 16), intent(in) :: dirstr
        character (len = 12), intent(in) :: dispstr
        character (len = *), optional, intent(in) :: str

        end subroutine noop_pe_comm3 

C --------------------------------------------------------------------------
C Purpose:
C
C   to provide a no-op noop_comm3e.f counter part for serial code
C
C Revision history:
C
C   Orginal version: 10/6/98 by David Wong
C                    11/05/99 by David Wong
C                      -- recode using F90 syntax
C --------------------------------------------------------------------------

        subroutine noop_pe_comm3e (data, dispstr, dirstr, flag, str)

        implicit none

        real, intent(in) :: data(:, :, :)
        character (len = 16), intent(in) :: dirstr
        character (len = 12), intent(in) :: dispstr
        integer, intent(in) :: flag
        character (len = *), optional, intent(in) :: str

        end subroutine noop_pe_comm3e 

        subroutine noop_pe_comm3s (sdata, ddata, dispstr, dirstr, str)
C --------------------------------------------------------------------------
C Purpose:
C
C   to provide a no-op noop_comm3s counter part for serial code
C
C Revision history:
C
C   Original version: Oct 01, 2018
C --------------------------------------------------------------------------
        implicit none

        real, intent(in)  ::  sdata(:,:,:)
        real, intent(in) ::  ddata(:,:,:)
        character (len = 16), intent(in) :: dirstr
        character (len = 12), intent(in) :: dispstr
        character (len = *), optional, intent(in) :: str
 
        end subroutine noop_pe_comm3s
C --------------------------------------------------------------------------
C Purpose:
C
C   to provide a no-op noop_comm4.f counter part for serial code
C
C Revision history:
C
C   Orginal version: 10/6/98 by David Wong
C                    11/05/99 by David Wong
C                      -- recode using F90 syntax
C --------------------------------------------------------------------------

        subroutine noop_pe_comm4 (data, dispstr, dirstr, str)

        implicit none

        real, intent(in) :: data(:, :, :, :)
        character (len = 16), intent(in) :: dirstr
        character (len = 12), intent(in) :: dispstr
        character (len = *), optional, intent(in) :: str

        end subroutine noop_pe_comm4 

        end module noop_comm_module
