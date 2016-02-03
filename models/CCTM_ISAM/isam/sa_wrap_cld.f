
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

      SUBROUTINE SA_WRAP_CLD( CGRID, CBFOR, ISAM, ISAMB4 )

!*************************************************************
!20140428 As a wrapper to apportion the tags with bulk conc after CONVCLD_ACM
!
!         Called by cldproc.acm.F
!*************************************************************
      USE HGRD_DEFN
      USE VGRD_DEFN
      USE CGRID_SPCS
      USE SA_GRD_DEFN     
      USE UTILIO_DEFN        ! 20120827
      USE OZVOC_DEFN         ! 20120925

      IMPLICIT NONE

! Arguments
      REAL, POINTER             :: CGRID( :,:,:,: )
      REAL, POINTER             :: CBFOR( :,:,:,: )
      REAL, POINTER             :: ISAM( :,:,:,:,: )
      REAL, POINTER             :: ISAMB4( :,:,:,:,: )

! Scratch
      INTEGER C,R,L,S,V,K
      INTEGER VNO, VNO2, VNO3, VN2O5, VHONO, VPNA
      REAL    TOTAL_OLD, TOTAL_NEW
      CHARACTER( 16 ), ALLOCATABLE :: SPC_NAME1( : ) ! tracked spc_name

! NOX related
      INTEGER JNOX, JHNO3, JNO3J
      REAL    CG_NOX, DEL_TOTAL
      INTEGER IND_NOX
      REAL    TTL_HNO3

      REAL    DEL_CG
! sulfate related
      INTEGER JSO2, JSO4J, JSULF
      INTEGER VSULF
      REAL    TTL_SULF_B4, TTL_SULF_AF
      REAL    TTL_SO2_B4, TTL_SO2_AF, BULK_DIFF, TAG_DIFF

! ammon related
      INTEGER JNH3, JNH4J
      REAL    TTL_NH3

! VOC related 20120925
      INTEGER JV
      REAL    TTL_VOC_B4, TTL_VOC_AF

      REAL    R_OZTAGS

!-------------------------------------------------------------------


Ckrt allocate species names for index identification, though mainly for SO2; 
Ckrt deallocate them upon exit of this subroutine
      IF ( .NOT. ALLOCATED( SPC_NAME1 ) ) ALLOCATE( SPC_NAME1( NSPC_SA ) )
      SPC_NAME1( : ) = SPC_NAME( :,OTHRTAG )

Ckrt Identify species index in ISAM array
      JHNO3 = INDEX1( 'HNO3', NSPC_SA, SPC_NAME1 )
      JNO3J = INDEX1( 'ANO3J', NSPC_SA, SPC_NAME1 )

      JSO2 = INDEX1( 'SO2', NSPC_SA, SPC_NAME1 )
      JSO4J = INDEX1( 'ASO4J', NSPC_SA, SPC_NAME1 )
      JSULF = INDEX1( 'SULF', NSPC_SA, SPC_NAME1 )

      JNH3 = INDEX1( 'NH3', NSPC_SA, SPC_NAME1 )
      JNH4J = INDEX1( 'ANH4J', NSPC_SA, SPC_NAME1 )

      R_OZTAGS = REAL( COUNT(L_OZONE) )

Ckrt Apportion the conc
      DO L = 1, NLAYS
        DO R = 1, MY_NROWS
          DO C = 1, MY_NCOLS

Ckrt...other gas species, including NR, 
Ckrt....and ALL PM species ( I'd left out EC and OC! ), 
Clrt....even though PM sulfate, nitrate and ammonium will be overwritten with updates 1110
            DO S = 1, NSPC_SA
              DO K = 1, NTAG_SA
                IF ( SPC_INDEX(S,1) .EQ. 1 .OR. SPC_INDEX(S,1) .EQ. -50 ) THEN
                  !Should cover "O3A" as well 20130627
                  IF ( CBFOR( C,R,L,SPC_INDEX(S,2) ) .GT. 1.0E-30 ) THEN
                    ISAM( C,R,L,S,K ) =
     &                ISAMB4( C,R,L,S,K ) * MIN( 1.0, 
     & CGRID( C,R,L,SPC_INDEX(S,2) ) / CBFOR( C,R,L,SPC_INDEX(S,2) ) ) ! 20130514
                  ELSE ! zero initial bulk conc
                    IF ( SPC_INDEX(S,1) .EQ. -50 ) THEN ! regimes of ozone
                      ISAM( C,R,L,S,K ) = 0.5 *
     &                  CGRID( C,R,L,SPC_INDEX(S,2) ) / real(NTAG_SA)
                    ELSE ! non ozone species
                      ISAM( C,R,L,S,K ) = 
     &                  CGRID( C,R,L,SPC_INDEX(S,2) ) / real(NTAG_SA)
                    ENDIF ! ozone or non-ozone ?
                  ENDIF ! cbfor > 0 ?
                ENDIF ! single species, split species or combined species ?
              ENDDO ! K tags
            ENDDO ! S species

C...Add up the precursors SO2, SULF(Fb22), HNO3 and NH3 over all tags
                TTL_SO2_B4 = 0.0
                TTL_SO2_AF = 0.0
                TTL_SULF_B4 = 0.0
                TTL_SULF_AF = 0.0

                TTL_HNO3 = 0.0
                TTL_NH3 = 0.0
                DO K = 1, NTAG_SA
                  IF ( JSO2 .GT. 0 ) THEN
                    TTL_SO2_B4 = TTL_SO2_B4 + ISAMB4( C, R, L, JSO2, K )
                    TTL_SO2_AF = TTL_SO2_AF + ISAM( C, R, L, JSO2, K )
                  ENDIF ! jso2
                  IF ( JSULF .GT. 0 ) THEN
                    TTL_SULF_B4 = TTL_SULF_B4 + ISAMB4( C, R, L, JSULF, K )
                    TTL_SULF_AF = TTL_SULF_AF + ISAM( C, R, L, JSULF, K )
                  ENDIF ! jsulf
                  IF ( JHNO3 .GT. 0 )
     &             TTL_HNO3 = TTL_HNO3 + ISAMB4( C, R, L, JHNO3, K )
                  IF ( JNH3 .GT. 0 )
     &             TTL_NH3 = TTL_NH3 + ISAMB4( C, R, L, JNH3, K )
                ENDDO ! ktag


Ckrt....pm sulfate, nitrate and ammonium
            DO K = 1, NTAG_SA
              ! sulfate
              IF ( JSO4J .GT. 0 ) THEN
                DEL_CG = CGRID(C,R,L,SPC_INDEX(JSO4J,2))-CBFOR(C,R,L,SPC_INDEX(JSO4J,2))
                BULK_DIFF = TTL_SO2_AF - TTL_SO2_B4
                BULK_DIFF = BULK_DIFF + TTL_SULF_AF - TTL_SULF_B4       !Fb22
                TAG_DIFF = ISAM( C, R, L, JSO2, K ) - ISAMB4( C, R, L, JSO2, K )
                TAG_DIFF = TAG_DIFF 
     &                   + ISAM( C, R, L, JSULF, K ) - ISAMB4( C, R, L, JSULF, K )
                IF ( BULK_DIFF .LT. -1.E-30 .AND.
     &               TAG_DIFF .LT. -1.E-30 .AND.
     &               DEL_CG .GT. 1.0E-30 )
     &            ISAM( C,R,L,JSO4J,K ) =
     &              ISAMB4( C,R,L,JSO4J,K ) 
     &             + DEL_CG * TAG_DIFF / BULK_DIFF
              ENDIF ! jso4j > 0

              ! nitrate
!1228         IF ( JNO3J .GT. 0 .AND. JHNO3 .GT. 0 .AND. JNOX .GT. 0 ) THEN
              IF ( JNO3J .GT. 0 .AND. JHNO3 .GT. 0 ) THEN
                DEL_CG = CGRID(C,R,L,SPC_INDEX(JNO3J,2))-CBFOR(C,R,L,SPC_INDEX(JNO3J,2)) 
                IF ( TTL_HNO3 .GT. 1.0E-30 .AND.
     &               DEL_CG .GT. 1.0E-30 )
     &            ISAM( C,R,L,JNO3J,K ) =
     &              ISAMB4( C,R,L,JNO3J,K )
     &             + DEL_CG * ISAMB4( C,R,L,JHNO3,K ) / TTL_HNO3

              ENDIF ! jno3j > 0 and jhno3 > 0 and jnox > 0

              ! ammonium
              IF ( JNH4J .GT. 0 ) THEN
                DEL_CG = CGRID(C,R,L,SPC_INDEX(JNH4J,2))-CBFOR(C,R,L,SPC_INDEX(JNH4J,2))
                IF ( TTL_NH3 .GT. 1.0E-30 .AND.
     &               DEL_CG .GT. 1.0E-30 )
     &            ISAM( C,R,L,JNH4J,K ) =
     &             ISAMB4( C,R,L,JNH4J,K )
     &            + DEL_CG * ISAMB4( C,R,L,JNH3,K ) / TTL_NH3
              ENDIF ! jnh4 > 0

            ENDDO ! tags , sulfate, nitrate, ammonium

          ENDDO ! C
        ENDDO ! R
      ENDDO ! L

Ckrt Deallocate interim arrays
      IF ( ALLOCATED(SPC_NAME1) ) DEALLOCATE(SPC_NAME1)


      END 

