
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

      MODULE SA_IRR_DEFN

C***********************************************************************
C20140428                       
C  
C  (1) Stores initial reaction rates in a C-R-L-nrxns array
C  (2) Contains subroutines SA_IRR_INIT
C                       and ACCUMRR
C
C    Aug 16, 2011: chemical integration time interval is in MINUTES
C
C***********************************************************************

      USE HGRD_DEFN
      USE VGRD_DEFN
      USE HRDATA
      USE SA_DEFN    ! 20130517

      IMPLICIT NONE

!KRT  INCLUDE SUBST_IOPARMS
!KRT  INCLUDE SUBST_IODECL
!KRT  INCLUDE SUBST_CONST
!KRT  INCLUDE SUBST_RXCMMN

      REAL, ALLOCATABLE, SAVE :: RXINIT( :,:,:,: )
      REAL, ALLOCATABLE, SAVE :: RKI_INIT( :,:,:,: )
      REAL, ALLOCATABLE, SAVE :: YC_INIT( :,:,:,: )
      REAL, ALLOCATABLE, SAVE :: PRDRATE( : )
      REAL, ALLOCATABLE, SAVE :: RKMID ( : )

!20140307 Integrated Rates
      REAL, ALLOCATABLE, SAVE :: INTRXN( :,:,:,: )

      CONTAINS
        SUBROUTINE SA_IRR_INIT

C20140428 Initialize arrays to store reaction rates in each grid cell
C
C         Called by hrdriver.F

        IMPLICIT NONE

C=======================================================

        IF ( .NOT. ALLOCATED( RXINIT ) ) 
     &    ALLOCATE( RXINIT( MY_NCOLS, MY_NROWS, NLAYS, N_RXNS ) )
        RXINIT = 0.0

        IF ( .NOT. ALLOCATED( RKI_INIT ) )
     &    ALLOCATE( RKI_INIT( MY_NCOLS, MY_NROWS, NLAYS, N_RXNS ) )
        RKI_INIT = 0.0

        IF ( .NOT. ALLOCATED( YC_INIT ) )
     &    ALLOCATE( YC_INIT( MY_NCOLS, MY_NROWS, NLAYS, N_SPEC ) )
        YC_INIT = 0.0

!20130517        IF ( .NOT. ALLOCATED( PRDRATE ) ) ALLOCATE( PRDRATE( N_SPEC ) )
        IF ( .NOT. ALLOCATED( PRDRATE ) ) ALLOCATE( PRDRATE( NSPC_SA ) )
        PRDRATE = 0.0

        IF ( .NOT. ALLOCATED( RKMID ) ) ALLOCATE( RKMID( N_RXNS ) )
        RKMID = 0.0

!20140307
        IF ( .NOT. ALLOCATED( INTRXN ) ) 
     &    ALLOCATE( INTRXN( MY_NCOLS, MY_NROWS, NLAYS, N_RXNS ) )
        INTRXN = 0.0
        

        END SUBROUTINE SA_IRR_INIT
C----------------------------------------------------------------------

! subroutine SA_PL_CB05 removed.  20130606

! subroutine ACCUMRR added below. 20140307
        SUBROUTINE ACCUMRR( COL, ROW, LAY )

C20140428 Performs integration of reaction rates for ISAM tracers.
C         However, under current production/destruction formulation
C         this subroutine is not used.
C
C         Would be called by hrsolver.F

        IMPLICIT NONE

        !Arguments
        INTEGER  COL
        INTEGER  ROW
        INTEGER  LAY

        !Scratch
        INTEGER  JRXN

        !20140311 record access
        INTEGER, SAVE :: NACCESS = 0

C=====================================================================

        !Accumulate calculated reaction rates
        DO JRXN = 1, N_RXNS
          IF ( JRXN .NE. 12 .AND. JRXN .NE. 13 .AND. JRXN .NE. 30 .AND.
     &         JRXN .NE. 38 .AND. JRXN .NE. 40 .AND. JRXN .NE. 44 )
     &      INTRXN( COL, ROW, LAY, JRXN ) = INTRXN( COL, ROW, LAY, JRXN )
     &                                    + RXRAT( JRXN ) * EBI_TMSTEP
        ENDDO ! jrxn

        !Special treatment on reaction 11
        INTRXN( COL, ROW, LAY, 11 ) = INTRXN( COL, ROW, LAY, 11 )
     &                              + RKI( 11 ) * YC( O1D ) * EBI_TMSTEP

        !Special treatment on reaction 12
        INTRXN( COL, ROW, LAY, 12 ) = INTRXN( COL, ROW, LAY, 12 )
     &                              + RKI( 12 ) * YC( O3 ) * YC( OH ) * EBI_TMSTEP

        !Special treatment on reaction 13
        INTRXN( COL, ROW, LAY, 13 ) = INTRXN( COL, ROW, LAY, 13 )
     &                              + RKI( 12 ) * YC( O3 ) * YC( HO2 ) * EBI_TMSTEP

        !Special treatment on reaction 30
        INTRXN( COL, ROW, LAY, 30 ) = INTRXN( COL, ROW, LAY, 30 )
     &                              + RKI( 30 ) * YC( HO2 ) * YC( NO ) * EBI_TMSTEP

        !Special treatment on reaction 38
        INTRXN( COL, ROW, LAY, 38 ) = INTRXN( COL, ROW, LAY, 38 )
     &                              + RKI( 38 ) * YC( O1D ) * EBI_TMSTEP

        !Special treatment on reaction 40
        INTRXN( COL, ROW, LAY, 40 ) = INTRXN( COL, ROW, LAY, 40 )
     &                              + RKI( 40 ) * YC( OH ) * YC( O ) * EBI_TMSTEP

        !Special treatment on reaction 44
        INTRXN( COL, ROW, LAY, 44 ) = INTRXN( COL, ROW, LAY, 44 )
     &                              + RKI( 44 ) * YC( HO2 ) * YC( O ) * EBI_TMSTEP

        IF ( YES_PRINT ) THEN
          IF ( MYPE .EQ. 5 ) THEN
            IF ( COL .EQ. 8 .AND. ROW .EQ. 13 .AND. LAY .EQ. 1 ) THEN
              print*,'Reaction 13 --  O3+HO2  ,  at col 8, row 13,'
              print*,'intrxn =',INTRXN( COL,ROW,LAY,13 )
              print*,'EBI_TMSTEP =', EBI_TMSTEP
              NACCESS = NACCESS + 1
              print*,'NACCESS =',NACCESS
            ENDIF ! (8,13,1)
          ENDIF ! mype0
        ENDIF ! yes_print


        END SUBROUTINE ACCUMRR
C----------------------------------------------------------------------

      END MODULE SA_IRR_DEFN
