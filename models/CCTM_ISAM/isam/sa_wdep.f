
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

      SUBROUTINE SA_WDEP( ISAM, ISAMB4,
     &                    N_SPC_WDEPD, WDEP_MAP, NAM_WDEP, BULK_DEP, SADEP )


!*************************************************************
!20140428 As a wrapper to apportion the tags' wet deposition with bulk wetdep
!
!         Called by cldproc_acm.F
!
!*************************************************************
      USE HGRD_DEFN
      USE VGRD_DEFN
      USE CGRID_SPCS
      USE SA_DEFN        
      USE UTILIO_DEFN
      USE OZVOC_DEFN       !20120925

      IMPLICIT NONE


! Arguments
      REAL, POINTER                 :: ISAM( :,:,:,:,: )
      REAL, POINTER                 :: ISAMB4( :,:,:,:,: )
      INTEGER, INTENT( IN )         :: N_SPC_WDEPD
      INTEGER, INTENT( IN )         :: WDEP_MAP( N_SPC_WDEPD )
      CHARACTER( 16 ), INTENT( IN ) :: NAM_WDEP( N_SPC_WDEPD )
      REAL, INTENT( IN )            :: BULK_DEP( :,:,: )
      REAL, INTENT( OUT )           :: SADEP( :,:,:,: )

! Scratch
      INTEGER C,R,L,S,V
      INTEGER JSPCTAG
      INTEGER IBGN, VNOX
      REAL  TTL_NOX_DEP
      REAL, ALLOCATABLE :: TTL_CONC( :,:,: )
      REAL, ALLOCATABLE :: SA_COLSUM( :,:,:,: )
      CHARACTER( 16 ) NAM_NOX
      INTEGER  NSPC, NTAG
      INTEGER  JSPC, ITAG
      INTEGER,     SAVE :: INDX_O3

! VOC 20120925 
      INTEGER  JV
      REAL     TTL_VOCDEP

!-------------------------------------------------------------------


      NSPC = size( ISAMB4, 4 )
      NTAG = size( ISAMB4, 5 )

      IF ( .NOT. ALLOCATED( TTL_CONC ) ) ALLOCATE( TTL_CONC( MY_NCOLS, MY_NROWS, NSPC ) )
      IF ( .NOT. ALLOCATED( SA_COLSUM ) ) 
     &    ALLOCATE( SA_COLSUM( MY_NCOLS, MY_NROWS, NSPC, NTAG ) )
      TTL_CONC = 0.0
      SA_COLSUM = 0.0

Ckrt Do column sum in loops
      DO R = 1, MY_NROWS
        DO C = 1, MY_NCOLS
          DO S = 1, NSPC
            DO V = 1, NTAG
              DO L = 1, NLAYS
                SA_COLSUM( C,R,S,V ) = SA_COLSUM( C,R,S,V ) 
     &                                + ISAMB4( C,R,L,S,V )
              ENDDO ! L          
              TTL_CONC( C,R,S ) = TTL_CONC( C,R,S ) + SA_COLSUM( C,R,S,V )

            ENDDO ! V
          ENDDO ! S
        ENDDO ! C
      ENDDO  ! R

Ckrt...find index of ozone among wet dep species list
      INDX_O3 = INDEX1( 'O3', N_SPC_WDEPD, NAM_WDEP )

Ckrt Apportion the wet dep for each (C,R)
      DO R = 1, MY_NROWS
        DO C = 1, MY_NCOLS
         
          DO JSPCTAG = 1, N_SPCTAG
            IBGN = INDEX( VNAM_SPCTAG(JSPCTAG),'_', .FALSE. )
Ckrt...other gas species
            S = INDEX1( VNAM_SPCTAG(JSPCTAG)(1:IBGN-1),N_SPC_WDEPD, NAM_WDEP )
            IF ( S .GT. 0 .AND. TTL_CONC( C,R, S_SPCTAG(JSPCTAG) ) .GT. 0.0 
     &           .AND. VNAM_SPCTAG(JSPCTAG)(1:IBGN-1) .NE. 'SULF' ) THEN
              SADEP( C,R,S_SPCTAG(JSPCTAG),T_SPCTAG(JSPCTAG) ) =
     &         SA_COLSUM( C,R,S_SPCTAG(JSPCTAG),T_SPCTAG(JSPCTAG) )
     &           * BULK_DEP( C,R,S ) / TTL_CONC( C,R,S_SPCTAG(JSPCTAG) )
            ELSEIF ( VNAM_SPCTAG(JSPCTAG)(1:IBGN-1) .EQ. 'O3A' ) THEN
              IF ( INDX_O3 .GT. 0 )
     &          SADEP( C,R,S_SPCTAG(JSPCTAG),T_SPCTAG(JSPCTAG) ) =
     &           SA_COLSUM( C,R,S_SPCTAG(JSPCTAG),T_SPCTAG(JSPCTAG) )
     &            * BULK_DEP( C,R,INDX_O3 ) / TTL_CONC( C,R,S_SPCTAG(JSPCTAG) )
            ELSEIF ( VNAM_SPCTAG(JSPCTAG)(1:IBGN-1) .EQ. 'O3N' .OR.
     &               VNAM_SPCTAG(JSPCTAG)(1:IBGN-1) .EQ. 'O3V' ) THEN
              IF ( INDX_O3 .GT. 0 ) 
     &          SADEP( C,R,S_SPCTAG(JSPCTAG),T_SPCTAG(JSPCTAG) ) =
     &           SA_COLSUM( C,R,S_SPCTAG(JSPCTAG),T_SPCTAG(JSPCTAG) )
     &            * 0.5 * BULK_DEP( C,R,INDX_O3 ) / TTL_CONC( C,R,S_SPCTAG(JSPCTAG) )
            ENDIF ! ozone or other gas species ?

Ckrt....pm species
            IF ( VNAM_SPCTAG(JSPCTAG)(1:1) .EQ. 'A' .AND.
     &   ( VNAM_SPCTAG(JSPCTAG)(IBGN-1:IBGN-1) .EQ. 'J' .OR.
     &   VNAM_SPCTAG(JSPCTAG)(IBGN-1:IBGN-1) .EQ. 'I' ) ) THEN
              S = INDEX1( VNAM_SPCTAG(JSPCTAG)(1:IBGN-1),N_SPC_WDEPD, NAM_WDEP )
              IF ( S .GT. 0 .AND. TTL_CONC( C,R, S_SPCTAG(JSPCTAG) ) .GT. 0.0 )
     &          SADEP( C,R,S_SPCTAG(JSPCTAG),T_SPCTAG(JSPCTAG) ) =
     &           SA_COLSUM( C,R,S_SPCTAG(JSPCTAG),T_SPCTAG(JSPCTAG) )
     &           * BULK_DEP( C,R,S ) / TTL_CONC( C,R,S_SPCTAG(JSPCTAG) )
            ENDIF


Ckrt....NR species
            S = INDEX1( VNAM_SPCTAG(JSPCTAG)(1:IBGN-1),N_SPC_WDEPD, NAM_WDEP )
            IF ( S .GT. 0 .AND. TTL_CONC( C,R, S_SPCTAG(JSPCTAG) ) .GT. 0.0 )
     &        SADEP( C,R,S_SPCTAG(JSPCTAG),T_SPCTAG(JSPCTAG) ) =
     &        SA_COLSUM( C,R,S_SPCTAG(JSPCTAG),T_SPCTAG(JSPCTAG) )
     &           * BULK_DEP( C,R,S ) / TTL_CONC( C,R,S_SPCTAG(JSPCTAG) )

Ckrt...VOC species  20120925
            IF ( VNAM_SPCTAG(JSPCTAG)(1:IBGN-1) .EQ. 'VOC' ) THEN
              TTL_VOCDEP = 0.0
              DO JV = 1, N_EVOC
                S = INDEX1( NAM_VOC(JV), N_SPC_WDEPD, NAM_WDEP )
                IF ( S .GT. 0 )
     &  TTL_VOCDEP = TTL_VOCDEP + BULK_DEP( C,R,S ) * NCARBON( JV )
              ENDDO ! JV
              IF ( TTL_CONC( C,R, S_SPCTAG(JSPCTAG) ) .GT. 0.0 ) 
     &          SADEP( C,R,S_SPCTAG(JSPCTAG),T_SPCTAG(JSPCTAG) ) =
     &           SA_COLSUM( C,R,S_SPCTAG(JSPCTAG),T_SPCTAG(JSPCTAG) )
     &           * TTL_VOCDEP / TTL_CONC( C,R,S_SPCTAG(JSPCTAG) )
            ENDIF ! if indeed voc

          ENDDO ! jspctag
        ENDDO ! C
      ENDDO ! R

      IF( ALLOCATED( TTL_CONC ) ) DEALLOCATE( TTL_CONC )
      IF( ALLOCATED( SA_COLSUM ) ) DEALLOCATE( SA_COLSUM )

      END 

