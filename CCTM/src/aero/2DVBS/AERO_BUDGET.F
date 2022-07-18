
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

!-----------------------------------------------------------------------
      Module AERO_BUDGET

!  Collects Aerosol Microphysical Process Rates
!
!-----------------------------------------------------------------------

      Implicit None

      SAVE

      ! These Arrays are sorted in the same manner as CGRID so they may
      ! be directly applied in modules like Process Analysis and ISAM
      REAL, ALLOCATABLE :: AERO_COAG  ( :,:,:,: )   ! Change in Species due to Coagulation
      REAL, ALLOCATABLE :: AERO_COND  ( :,:,:,: )   ! Change in Species due to Condensation
      REAL, ALLOCATABLE :: AERO_NPF   ( :,:,:,: )   ! Change in Species due to NPF
      REAL, ALLOCATABLE :: AERO_GROWTH( :,:,:,: )   ! Change in species due to aerosol 
                                                    !   renaming (mode merging)

!      Contains

C-----------------------------------------------------------------------

      End Module AERO_BUDGET
