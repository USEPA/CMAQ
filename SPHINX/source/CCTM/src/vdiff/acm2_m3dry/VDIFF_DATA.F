
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


C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE VDIFF_DATA

C-----------------------------------------------------------------------
C Function: index mapping between CGRID, diffusion, dry dep, and emissions
C           species

C-----------------------------------------------------------------------

C global diffusion species
      INTEGER, SAVE :: N_SPC_DIFF 
     
C global dry dep species
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: DIFF_SPC( : )

      END MODULE VDIFF_DATA
