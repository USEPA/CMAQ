
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
      SUBROUTINE ACMCLD ( NSP, NLAYS, F, C, DZH, CBELOW, CLBASE, CLTOP,
     &                    FRAC, TCLIFE, DTCLD )
C-----------------------------------------------------------------------
C
C  FUNCTION:  Subroutine to compute convective mixing in the CBL
C             according to the Asymmetrical Convective Model (ACM).
C             Ref: Pleim and Chang (1992)
C
C  SUMMARY:
C   ACM is based on the Blackadar non-local convective model which is
C   used in HIRPBL where upward mixing similar to Blackadar but
C   downward mixing is to the next lower level representing more
C   realistic gradual subsidence.
C
C  REVISION  HISTORY:
C      Date   Who             What
C    -------- ---             -----------------------------------------
C     06/2005 J.Pleim         Initial version
C     07/2005 J.Young         Clean up for CMAQ-F
C     05/2015 J.Young         Clean up for CMAQv5.1
C     01/2018 J.Pleim         Convert from sigma to Z for MPAS
C-----------------------------------------------------------------------\

      IMPLICIT NONE

C Arguments

      INTEGER, INTENT( IN ) :: NSP            ! no. of species
      INTEGER, INTENT( IN ) :: NLAYS          ! no. of model layers
      REAL, INTENT( IN )    :: F( : )         ! entrainment fraction
      REAL, INTENT( INOUT ) :: C( :, : )      ! species concentration
      REAL, INTENT( IN )    :: DZH( : )
      REAL, INTENT( INOUT ) :: CBELOW( : )    ! spec conc in layer below cld base
      INTEGER, INTENT( IN ) :: CLBASE
      INTEGER, INTENT( IN ) :: CLTOP
      REAL, INTENT( IN )    :: FRAC           ! grid cell fractional cloud cover
      REAL, INTENT( IN )    :: TCLIFE         ! cloud lifetime (s)
      REAL, INTENT( IN )    :: DTCLD          ! cloud integration time step

C Parameters

      REAL, PARAMETER :: HALF = 0.5
      REAL, PARAMETER :: CRANKP = 0.5

C Local variables

      INTEGER NLP, K, NL, S          ! index variables
      INTEGER KB

      REAL DTLIM, F1
      REAL DTS, DELC, M1UP
      REAL( 8 ), ALLOCATABLE, SAVE :: AI( : ), BI( : ), EI( : )
      REAL( 8 ), ALLOCATABLE, SAVE :: DI( : ), UI( : )
      REAL( 8 ) :: ALPHA, BETA, GAMA
      REAL, ALLOCATABLE, SAVE :: VCI( :,: )
      REAL, ALLOCATABLE, SAVE :: MBARKS( : ), MDWN( : )

!--Local Arrays for MPAS implementation
      REAL, ALLOCATABLE, SAVE     :: DZHI  ( : )               ! 1/DZH

      INTEGER :: ALLOCSTAT
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      CHARACTER (LEN = 80) :: XMSG

C-----------------------------------------------------------------------

      IF (FIRSTIME) THEN
         ALLOCATE (AI( NLAYS ), 
     &             BI( NLAYS ), 
     &             EI( NLAYS ),
     &             DI( NLAYS ), 
     &             UI( NLAYS ),
     &             VCI( NLAYS,NSP ),
     &             MBARKS( NLAYS ), 
     &             MDWN( NLAYS ),
     &             DZHI  ( NLAYS ),
     &             STAT=ALLOCSTAT)
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'EXIT: Failure allocating AI, BI, EI, DI, UI, VCI, MBARKS, MDWN or DZHI'
            write (6, *) trim(xmsg); stop
         END IF
         FIRSTIME = .FALSE. 
      END IF

      DTLIM = DTCLD
      MDWN ( CLTOP + 1 ) = 0.0
      M1UP = 0.0
      DZHI = 1.0 / DZH ! array assignment
      KB  = CLBASE - 1
      DZHI( KB ) = 1.0 / DZH( KB )

C Compute ACM mixing rate

      DO K = CLTOP, CLBASE, -1
        MBARKS( K ) = ( 1.0 - F( K ) ) * FRAC / TCLIFE
        MDWN  ( K ) = MBARKS( K ) + MDWN( K + 1 ) * DZH( K + 1 ) * DZHI( K )
        M1UP  = M1UP + MBARKS( K ) * DZH( K )
        DTLIM = MIN( HALF / ( M1UP * DZHI( K ) ), DTLIM )
      END DO
      DTLIM = MIN( HALF / ( M1UP * DZHI( KB ) ), DTLIM )

      DO S = 1, NSP
        VCI( KB, S ) = CBELOW( S )
        VCI( CLTOP+1,S ) = 9999.0
        DO K = CLBASE, CLTOP
          VCI( K,S ) = C( S,K )
          UI( K )  = 0.0           ! init variable for use below
        END DO
      END DO

      NLP = INT( DTCLD / DTLIM + 1.0 )
      DTS = ( DTCLD / NLP )
      DO 2000 NL = 1, NLP      ! loop over sub timestep
        DO 1000 S = 1, NSP     ! loop over species
                                                                              
C Compute tendency of CBL concentrations - Semi-Implicit solution

          DO K = CLBASE, CLTOP
            DELC = DTS
     &           * ( MBARKS( K ) * VCI( KB,S )
     &           -   MDWN( K ) * VCI( K,S )
     &           +   DZH( K+1 ) * DZHI( K ) * MDWN( K+1 ) * VCI( K+1,S ) )
            DI( K ) = VCI( K,S ) + ( 1.0 - CRANKP ) * DELC
            EI( K ) = -CRANKP * MDWN( K ) * DTS * DZH( K ) * DZHI( K-1 )
            BI( K ) = 1.0 + CRANKP * MDWN( K ) * DTS
            AI( K ) = -CRANKP * MBARKS( K ) * DTS
          END DO

          BI( KB ) = 1.0 + CRANKP * M1UP * DTS * DZHI( KB )
          F1 = M1UP * VCI( KB,S )
     &       - MDWN( CLBASE ) * VCI( CLBASE,S ) * DZH( CLBASE )
          DI( KB ) = VCI( KB,S ) - ( 1.0 - CRANKP ) * F1 * DZHI( KB ) * DTS

C Define arrays A,B,E which make up matrix and D which is RHS

          BETA = DI( KB )
          GAMA = BI( KB )
          ALPHA = 1.0
          DO K = CLBASE, CLTOP
            ALPHA = -ALPHA * EI( K ) / BI( K )
            BETA  = ALPHA * DI( K ) + BETA
            GAMA  = ALPHA * AI( K ) + GAMA
          END DO
          UI( KB )   = BETA / GAMA
          UI( CLTOP ) = ( DI( CLTOP ) - AI( CLTOP ) * UI( KB ) ) / BI( CLTOP )

          BETA = DI( KB )
          GAMA = BI( KB )
          ALPHA = 1.0

C Back substitution:
          DO K = CLTOP - 1, CLBASE, -1
            UI( K ) = ( DI( K ) - AI( K ) * UI( KB ) - EI( K+1 ) * UI( K+1 ) )
     &              / BI( K )
          END DO

C Update concentrations
          DO K = KB, CLTOP
            VCI( K,S ) = REAL( UI( K ), 4 )
          END DO

1000    CONTINUE   ! end loop for species
2000  CONTINUE   ! end timestep loop

      DO S = 1, NSP

        CBELOW( S ) = VCI( KB,S )

        DO K = CLBASE, CLTOP
          C( S,K ) = VCI( K,S )
        END DO

      END DO

      RETURN
      END
