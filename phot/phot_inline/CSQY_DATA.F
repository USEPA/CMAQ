
C***********************************************************************
C   Portions of Models-3/CMAQ software were developed or based on      *
C   information from various groups: Federal Government employees,     *
C   contractors working on a United States Government contract, and    *
C   non-Federal sources (including research institutions).  These      *
C   research institutions have given the Government permission to      *
C   use, prepare derivative works, and distribute copies of their      *
C   work in Models-3/CMAQ to the public and to permit others to do     *
C   so.  EPA therefore grants similar permissions for use of the       *
C   Models-3/CMAQ software, but users are requested to provide copies  *
C   of derivative works to the Government without restrictions as to   *
C   use by others.  Users are responsible for acquiring their own      *
C   copies of commercial software associated with Models-3/CMAQ and    *
C   for complying with vendor requirements.  Software copyrights by    *
C   the MCNC Environmental Modeling Center are used with their         *
C   permissions subject to the above restrictions.                     *
C***********************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/phot/phot_inline/CSQY_DATA.F,v 1.1 2011/03/29 13:12:19 sjr Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE CSQY_DATA

      IMPLICIT NONE

      CHARACTER( 32 ), SAVE :: JTABLE_REF

      INTEGER, SAVE :: NPHOT_REF  ! # ref phot reactions 
      INTEGER, SAVE :: NTEMP_REF  ! # ref temperatures 
      INTEGER, SAVE :: NWL_REF    ! # ref wavelengths 

C...Names of the mapped photolysis reactions (available to chemical)
C... mechanisms) and their pointers to the reference photolysis rxn

      CHARACTER( 16 ), ALLOCATABLE, SAVE :: PNAME_REF( : )

C...Setup the Mapping from CMAQ chemical reactions to the reference data

      INTEGER, SAVE :: NPHOT_MAP  ! #  phot mapped reactions 

      CHARACTER( 16 ), ALLOCATABLE, SAVE :: PNAME_MAP( : )
      INTEGER, ALLOCATABLE,         SAVE :: PHOT_MAP ( : )
      
      REAL, SAVE, ALLOCATABLE :: STWL_REF ( : ) 
      REAL, SAVE, ALLOCATABLE :: EFFWL_REF( : ) 
      REAL, SAVE, ALLOCATABLE :: ENDWL_REF( : ) 

      REAL, ALLOCATABLE, SAVE :: CLD_BETA_REF    ( : )  ! cloud extinction coef divided by LWC
      REAL, ALLOCATABLE, SAVE :: CLD_COALBEDO_REF( : )  ! cloud coalbedo
      REAL, ALLOCATABLE, SAVE :: CLD_G_REF       ( : )  ! cloud asymmetry factor

      REAL, ALLOCATABLE, SAVE :: FSOLAR_REF( : )        ! initial solar flux [photons*cm-2*s-1]

      REAL, ALLOCATABLE, SAVE :: TEMP_BASE ( : )        ! reference temperatures
      REAL, ALLOCATABLE, SAVE :: TEMP_REF( :,: )        ! reference temperatures

      REAL, ALLOCATABLE, SAVE :: CS_REF ( :,:,: )       ! effective cross sections
      REAL, ALLOCATABLE, SAVE :: QY_REF ( :,:,: )       ! effective quantum yields
      REAL, ALLOCATABLE, SAVE :: ECS_REF( :,:,: )       ! CS*QY averaged UCI Solar Flux

      INTEGER,           SAVE :: NTEMP_STRAT_REF        ! number of stratos temperatures
      REAL, ALLOCATABLE, SAVE :: TEMP_STRAT_REF( : )    ! temperature for stratos O3 xcross, K
      REAL, ALLOCATABLE, SAVE :: O3_CS_STRAT_REF( :,: ) ! ozone xcross at stratos temperatures, cm2

C...    effective quantum yields were computed by performing separate
C...    interval integrations for the cross sections and for the
C...    effective cross sections (cs*qy) (calculated on the finer
C...    wavelength grid.  The effective quantum yield values
C...    were then calculated for the 7 wavelength intervals by 
C...    dividing the effective cross sections by the interval average
C...    cross sections (eQY=eCS/CS).

      REAL, ALLOCATABLE, SAVE :: EQY_REF( :,:,: ) ! eCS/CS averaged 77 bins in UCI Model

      INTEGER  :: IWLR  ! wavelength loop variable
      INTEGER  :: ITTR   ! temperature loop variable
            
      END MODULE CSQY_DATA
