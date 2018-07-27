
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

C $Header$

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE PHOT_MOD


      IMPLICIT NONE
 
 
      REAL, ALLOCATABLE  :: RJ      ( :, :, :, : ) ! average grid cell J-values (min-1)
      REAL, ALLOCATABLE  :: RJ_RES  ( :, :, :, : ) ! resolved cloud J-values (min-1)
      REAL, ALLOCATABLE  :: RJ_SUB  ( :, :, :, : ) ! subgrid cloud J-values (min-1)
      REAL, ALLOCATABLE  :: ETOT_SFC_WL ( :,:,: )  ! total downward irradiance at sfc [ Watts / m**2  ]
      
!***Indices for specific photolysis rates 

      INTEGER :: LNO2            = 0 
      INTEGER :: LO3O1D          = 0
      INTEGER :: LO3O3P          = 0
      INTEGER :: LACETONE        = 0
      INTEGER :: LKETONE         = 0
      INTEGER :: LMGLY_ADJ       = 0
      INTEGER :: LMGLY_ABS       = 0
      INTEGER :: LHCHOR_06       = 0
      INTEGER :: LH2O2           = 0
      INTEGER :: LHNO3           = 0
      INTEGER :: LACETONE_CO     = 0 
      INTEGER :: LACETONE_CH3CO  = 0

      END MODULE PHOT_MOD
