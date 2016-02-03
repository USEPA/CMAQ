
      SUBROUTINE SA_WRAP_AE( CGRID, CBFOR, ISAM, ISAMB4,
     &               JDATE, JTIME, TSTEP )

!*************************************************************
!20140428 As a wrapper to apportion the tags with bulk conc after AERO process
!
!     Called by sciproc.F
!*************************************************************
      USE GRID_CONF
      USE CGRID_SPCS
      USE SA_GRD_DEFN  ! for SA_SULF_NEW( :,:,:,: ); inherents SA_DEFN
      USE UTILIO_DEFN  ! 20120906

      IMPLICIT NONE

      INCLUDE SUBST_FILES_ID

! Arguments
      REAL, POINTER             :: CGRID( :,:,:,: )
      REAL, POINTER             :: CBFOR( :,:,:,: )
      REAL, POINTER             :: ISAM( :,:,:,:,: )
      REAL, POINTER             :: ISAMB4( :,:,:,:,: )
      INTEGER, INTENT( IN )     :: JDATE
      INTEGER, INTENT( IN )     :: JTIME
      INTEGER, INTENT( IN )     :: TSTEP( 3 )

! Scratch
      CHARACTER( 16 ), SAVE :: PNAME = 'SA_WRAP_AE'
      INTEGER C,R,L,S,V,K
      INTEGER JSPCTAG
      INTEGER IBGN
      INTEGER VNO, VNO2, VNO3, VN2O5, VHONO, VPNA
      INTEGER VNH3, VHNO3
      INTEGER KNH3, KHNO3
      REAL    TOTAL_OLD, TOTAL_NEW
      CHARACTER( 16 ) :: PRECUR
      REAL, ALLOCATABLE :: TTLB4( :,:,:,: )
      REAL, ALLOCATABLE :: TTL_SULF( :,:,: )

! for interpx
      REAL DX1, DX2
      INTEGER GXOFF, GYOFF
      INTEGER, SAVE :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      REAL PRES ( NCOLS,NROWS,NLAYS )
      REAL TA   ( NCOLS,NROWS,NLAYS )
      INTEGER MDATE, MTIME, MSTEP
      CHARACTER( 96 ) :: XMSG = ' '

! Constants of Gas Law
      REAL, PARAMETER :: SLP0 = 101325.0 ! standard atm (centibars) * 1000
      REAL, PARAMETER :: GAS_CONST = 8.2057E-05 ! gas const (L-atm/mole-k) / 1000
      REAL, PARAMETER :: MWNH3 = 17.03061
      REAL, PARAMETER :: MWNH4 = 18.03858
      REAL, PARAMETER :: MWHNO3 = 63.01287
      REAL, PARAMETER :: MWNO3 = 62.0049
      REAL, PARAMETER :: MWSO4 = 96.0576

! ppm2mole conversion
      REAL ppm2mole

! Identify ISAM species indices 20130529
      LOGICAL, SAVE :: FRST_ISAMSPC = .TRUE.
      INTEGER, SAVE :: JNH3, JNH4I, JNH4J, JHNO3, JNO3I, JNO3J
      INTEGER, SAVE :: JSULF, JSO4I, JSO4J, JSRXN

! Arrays for combined ammonium, combined nitrates, and combined sulfates
      REAL, ALLOCATABLE :: AMMON_TAG( : )
      REAL, ALLOCATABLE :: NTRAT_TAG( : )
      REAL, ALLOCATABLE :: SFATE_TAG( : )

!2013fv29
      REAL  SHARED

!-------------------------------------------------------------------


Ckrt Identify species index in ISAM array
      IF ( FRST_ISAMSPC ) THEN
        JNH3 = INDEX1( 'NH3', NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        JNH4I = INDEX1( 'ANH4I', NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        JNH4J = INDEX1( 'ANH4J', NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        JHNO3 = INDEX1( 'HNO3', NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        JNO3I = INDEX1( 'ANO3I', NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        JNO3J = INDEX1( 'ANO3J', NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        JSULF = INDEX1( 'SULF', NSPC_SA, SPC_NAME( :,OTHRTAG ) )   !Fb23
        JSO4I = INDEX1( 'ASO4I', NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        JSO4J = INDEX1( 'ASO4J', NSPC_SA, SPC_NAME( :,OTHRTAG ) )
        JSRXN = INDEX1( 'SULRXN', NSPC_SA, SPC_NAME( :,OTHRTAG ) )  
      ENDIF ! frst_isamspc
  
Ckrt Sum up pre-process tags for each species at each grid cell
      IF ( .NOT. ALLOCATED( TTLB4 ) ) 
     &   ALLOCATE( TTLB4( MY_NCOLS, MY_NROWS, NLAYS, NSPC_SA ) ) 
      TTLB4 = 0.0
      DO L = 1, NLAYS
        DO R = 1, MY_NROWS
          DO C = 1, MY_NCOLS
            DO K = 1, NTAG_SA
              ! tssa tags
              DO S = 1, NSPC_SA
                TTLB4( C,R,L,S ) = TTLB4( C,R,L,S ) + ISAMB4( C,R,L,S,K )
              ENDDO ! S
            ENDDO ! K
          ENDDO ! C
        ENDDO ! R
      ENDDO ! L

Ckrt Create 1D array of combined ammonium tags and combined nitrate tags
      IF ( .NOT. ALLOCATED( AMMON_TAG ) ) ALLOCATE( AMMON_TAG( NTAG_SA ) )
      IF ( .NOT. ALLOCATED( NTRAT_TAG ) ) ALLOCATE( NTRAT_TAG( NTAG_SA ) )
      IF ( .NOT. ALLOCATED( SFATE_TAG ) ) ALLOCATE( SFATE_TAG( NTAG_SA ) )
      AMMON_TAG = 0.0
      NTRAT_TAG = 0.0
      SFATE_TAG = 0.0

Ckrt Decompose domain for pressure and temperature in MC3 - just do it once...
      IF ( FIRSTIME ) THEN
        FIRSTIME = .FALSE.
        IF ( .NOT. OPEN3( MET_CRO_3D, FSREAD3, PNAME ) ) THEN
          XMSG = 'Could not open  MET_CRO_3D  file '
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF
        CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                   STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )
      ENDIF ! firstime ?

Ckrt Extract pressure and temperature for ppm2mole conversion...
      MDATE = JDATE
      MTIME = JTIME
      MSTEP = TIME2SEC( TSTEP( 2 ) )
      CALL NEXTIME ( MDATE, MTIME, SEC2TIME( MSTEP/2 ) )

      IF ( .NOT. INTERPX( MET_CRO_3D, 'PRES', PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MDATE, MTIME, PRES ) ) THEN
        XMSG = 'Could not interpolate PRES from ' // MET_CRO_3D
        CALL M3EXIT ( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
      END IF

      IF ( .NOT. INTERPX( MET_CRO_3D, 'TA', PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MDATE, MTIME, TA ) ) THEN
        XMSG = 'Could not interpolate TA from ' // MET_CRO_3D
        CALL M3EXIT ( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
      END IF


Ckrt Apportion the conc
      DO L = 1, NLAYS
        DO R = 1, MY_NROWS
          DO C = 1, MY_NCOLS
Ckrt....ppm2mole conversion....
            ppm2mole = PRES( C,R,L )/SLP0/(GAS_CONST * TA( C,R,L ) )

Ckrt....Preprocess total - ammonium and ammonia
            IF ( JNH3 .GT. 0 .AND. JNH4I .GT. 0 .AND. JNH4J .GT. 0 ) THEN
              TTLB4( C,R,L,JNH3 ) = ppm2mole * TTLB4( C,R,L,JNH3 )
              TTLB4( C,R,L,JNH4I ) = TTLB4( C,R,L,JNH4I ) / MWNH4
              TTLB4( C,R,L,JNH4J ) = TTLB4( C,R,L,JNH4J ) / MWNH4
!2013fv29              DO K = 1, NTAG_SA
!2013fv29                AMMON_TAG( K ) =  ppm2mole * ISAMB4( C,R,L,JNH3,K )
!2013fv29     &                           + ISAMB4( C,R,L,JNH4I,K )/ MWNH4
!2013fv29     &                           + ISAMB4( C,R,L,JNH4J,K )/ MWNH4
!2013fv29              ENDDO ! loop upto ntag_sa
            ENDIF ! nh3 being tracked ?

            IF ( JHNO3 .GT. 0 .AND. JNO3I .GT. 0 .AND. JNO3J .GT. 0 ) THEN
              TTLB4( C,R,L,JHNO3 ) = ppm2mole * TTLB4( C,R,L,JHNO3 )
              TTLB4( C,R,L,JNO3I ) = TTLB4( C,R,L,JNO3I ) / MWNO3
              TTLB4( C,R,L,JNO3J ) = TTLB4( C,R,L,JNO3J ) / MWNO3
!2013fv29              DO K = 1, NTAG_SA
!2013fv29                NTRAT_TAG( K ) = ppm2mole * ISAMB4( C,R,L,JHNO3,K )
!2013fv29     &                          + ISAMB4( C,R,L,JNO3I,K )/ MWNO3
!2013fv29     &                          + ISAMB4( C,R,L,JNO3J,K )/ MWNO3
!2013fv29              ENDDO ! loop upto ntag_sa
            ENDIF ! ntrate being tracked ?

!20130822....
            IF ( JSO4I .GT. 0 .AND. JSO4J .GT. 0 ) THEN
              TTLB4( C,R,L,JSULF ) = ppm2mole * TTLB4( C,R,L,JSULF )
              TTLB4( C,R,L,JSO4I ) = TTLB4( C,R,L,JSO4I ) / MWSO4
              TTLB4( C,R,L,JSO4J ) = TTLB4( C,R,L,JSO4J ) / MWSO4
            ENDIF ! sfate being tracked ?

            DO K = 1, NTAG_SA

              IF ( JNH3 .GT. 0 .AND. JNH4I .GT. 0 .AND. JNH4J .GT. 0 ) THEN
                AMMON_TAG( K ) =  ppm2mole * ISAMB4( C,R,L,JNH3,K )
     &                           + ISAMB4( C,R,L,JNH4I,K )/ MWNH4
     &                           + ISAMB4( C,R,L,JNH4J,K )/ MWNH4
                
                SHARED = AMMON_TAG( K ) / MAX( TTLB4( C,R,L,JNH3 )+TTLB4( C,R,L,JNH4I )+TTLB4( C,R,L,JNH4J ) , 1.0E-30 )

                ISAM( C,R,L,JNH4I,K ) = CGRID( C,R,L, SPC_INDEX(JNH4I,2) ) * SHARED
                ISAM( C,R,L,JNH4J,K ) = CGRID( C,R,L, SPC_INDEX(JNH4J,2) ) * SHARED
                ISAM( C,R,L,JNH3 ,K ) = CGRID( C,R,L, SPC_INDEX(JNH3 ,2) ) * SHARED
              ENDIF ! ammonium

              IF ( JHNO3 .GT. 0 .AND. JNO3I .GT. 0 .AND. JNO3J .GT. 0 ) THEN
                NTRAT_TAG( K ) = ppm2mole * ISAMB4( C,R,L,JHNO3,K )
     &                          + ISAMB4( C,R,L,JNO3I,K )/ MWNO3
     &                          + ISAMB4( C,R,L,JNO3J,K )/ MWNO3
                
                SHARED = NTRAT_TAG( K ) / MAX( TTLB4( C,R,L,JHNO3 )+TTLB4( C,R,L,JNO3I )+TTLB4( C,R,L,JNO3J ) , 1.0E-30 )

                ISAM( C,R,L,JNO3I,K ) = CGRID( C,R,L, SPC_INDEX(JNO3I,2) ) * SHARED
                ISAM( C,R,L,JNO3J,K ) = CGRID( C,R,L, SPC_INDEX(JNO3J,2) ) * SHARED
                ISAM( C,R,L,JHNO3 ,K ) = CGRID( C,R,L, SPC_INDEX(JHNO3 ,2) ) * SHARED
              ENDIF ! nitrate

              IF ( JSO4J .GT. 0 .AND. JSRXN .GT. 0 ) THEN
                SFATE_TAG( K ) = ppm2mole * ISAMB4( C,R,L,JSULF,K )
     &                          + ISAMB4( C,R,L,JSO4I,K )/ MWSO4
     &                          + ISAMB4( C,R,L,JSO4J,K )/ MWSO4

                SHARED = SFATE_TAG( K ) / MAX( TTLB4( C,R,L,JSULF )+TTLB4( C,R,L,JSO4I )+TTLB4( C,R,L,JSO4J ) , 1.0E-30 ) 

                ISAM( C,R,L,JSO4I,K ) = CGRID( C,R,L, SPC_INDEX(JSO4I,2) ) * SHARED
                ISAM( C,R,L,JSO4J,K ) = CGRID( C,R,L, SPC_INDEX(JSO4J,2) ) * SHARED

!20130618                ISAM( C,R,L, JSO4J, K ) = ISAMB4( C,R,L, JSO4J, K ) +
!20130618     &   MWSO4 * ppm2mole * ISAMB4( C,R,L,JSRXN,K )
              ENDIF ! sulfate

            ENDDO ! k tags
          ENDDO ! C
        ENDDO ! R
      ENDDO ! L
 
      IF ( ALLOCATED(TTLB4) ) DEALLOCATE( TTLB4 )
      IF ( ALLOCATED(AMMON_TAG) ) DEALLOCATE( AMMON_TAG )
      IF ( ALLOCATED(NTRAT_TAG) ) DEALLOCATE( NTRAT_TAG )
      IF ( ALLOCATED(SFATE_TAG) ) DEALLOCATE( SFATE_TAG )

      END 

