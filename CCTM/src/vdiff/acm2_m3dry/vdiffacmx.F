!--------------------------------------------------------------------!
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
      SUBROUTINE VDIFFACMX ( DTSEC, SEDDY, DDEP, ICMP, CNGRD )

C-----------------------------------------------------------------------
C Asymmetric Convective Model v2 (ACM2/ACM1) -- Pleim(2006/2014)
C Function:
C   calculates vertical diffusion

C Subroutines and Functions Called:
C   SEC2TIME, TIME2SEC, WRITE3, NEXTIME,
C   M3EXIT, EDDYX, TRI, MATRIX, PA_UPDATE_EMIS, PA_UPDATE_DDEP

C Revision History:
C   Analogous to VDIFFACM2
C 11 Apr 13 J.Young: fix double adjustment of conc for DDBF in heterogeneous HONO
C           if-then-else clauses; eliminate some white space
C 13 May 13 J.Young: access met data from VDIFF_MET module
C                    change CRANKP to THBAR, CRANKQ to THETA
C 25 May 13 J.Young: re-do the acm/eddy algorithm for computational efficiency
C 30 Apr 14 J.Young: switch THBAR and THETA
C  2 May 14 J.Pleim, J.Young: replace the banded tridiagonal matrix solver for the
C           convective PBL, with the ACM1 matrix solver followed by the tridiagonal
C           matrix solver
C   30 May 14 J.Young: split vdiff calculation out of vdiff proc.
C   07 Nov 14 J.Bash: Updated for the ASX_DATA_MOD shared data module. 
C   02 Nov 2018: L.Zhou, S.Napelenok: isam implementation
C   May 2019  J.Pleim Changed from sigma coords to Z coords for compatability w/ MPAS and WRF
C   12 Dec 19 S.L.Napelenok: ddm-3d implementation for version 5.3.1
C   15 Jun 21 J. Pleim: implemented HONO fix for dry depsotion flux
C-----------------------------------------------------------------------

      USE CGRID_SPCS          ! CGRID mechanism species
      USE GRID_CONF
      USE DESID_VARS, ONLY : VDEMIS_DIFF,DESID_LAYS
      USE DESID_PARAM_MODULE, ONLY : DESID_N_SRM
      USE DEPV_DEFN
      USE ASX_DATA_MOD
      USE VDIFF_MAP
      USE UTILIO_DEFN
!      USE BIDI_MOD
!      USE LSM_MOD, ONLY: N_LUFRAC
      USE VDIFF_DIAG, NLPCR => NLPCR_MEAN
      USE HGRD_DEFN,only : COLSX_PE, ROWSX_PE
      USE BDSNP_MOD, ONLY: GET_N_DEP
#ifdef isam
      USE SA_DEFN, ONLY: N_SPCTAG, ISAM, VNAM_SPCTAG, TRANSPORT_SPC,
     &                   SA_VDEMIS_DIFF, ITAG, NTAG_SA, NSPC_SA,
     &                   S_SPCTAG, T_SPCTAG, SA_DDEP, OTHRTAG, ISAM_SPEC,
     &                   L_NO3, SA_BIDI, BIDITAG, L_NH4
#endif

#ifdef sens
      USE DDM3D_DEFN, ONLY : NP, NPMAX, SNGRD, S_DDEP, S_PLDV, SVDEMIS_DIFF,
     &                       SENS, S_EMIS, S_DD, S_UU, S_DDBF, S_POL,
     &                       S_DELC, S_PLDV_HONO
#endif


      IMPLICIT NONE

      INCLUDE SUBST_FILES_ID  ! file name parameters

      CHARACTER( 120 ) :: XMSG = ' '

C Arguments:
      REAL, INTENT( IN )    :: DTSEC                ! model time step in seconds
C--- SEDDY is strictly an input, but it gets modified here
      REAL, INTENT( INOUT ) :: SEDDY    ( :,:,: )   ! flipped EDDYV
      REAL, INTENT( INOUT ) :: DDEP     ( :,:,: )   ! ddep accumulator
      REAL, INTENT( INOUT ) :: ICMP     ( :,:,: )   ! component flux accumlator 
      REAL, INTENT( INOUT ) :: CNGRD    ( :,:,:,: ) ! cgrid replacement

C Parameters:

C explicit, THETA = 0, implicit, THETA = 1     ! Crank-Nicholson: THETA = 0.5
      REAL, PARAMETER :: THETA = 0.5,
     &                   THBAR = 1.0 - THETA

      REAL, PARAMETER :: EPS = 1.0E-06

C External Functions: None

C Local Variables:

      CHARACTER( 16 ), SAVE :: PNAME = 'VDIFFACMX'

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      LOGICAL, SAVE :: SPECLOG = .TRUE.             ! For BDSNP

      REAL, ALLOCATABLE, SAVE :: DD_FAC     ( : )   ! combined subexpression
      REAL, ALLOCATABLE, SAVE :: DDBF       ( : )   ! secondary DDEP
      REAl, ALLOCATABLE, SAVE :: CMPF       ( : )   ! intermediate CMP
      REAL, ALLOCATABLE, SAVE :: CONC       ( :,: ) ! secondary CGRID expression
      REAL, ALLOCATABLE, SAVE :: EMIS       ( :,: ) ! emissions subexpression
      REAL        DTDENS1                       ! DT * layer 1 air density

C ACM Local Variables
      REAL     :: EDDY  ( NLAYS )               ! local converted eddyv
      REAL        MEDDY                         ! ACM2 intermediate var
      REAL        MBAR                          ! ACM2 mixing rate (S-1)
      REAL     :: MBARKS( NLAYS )               ! by layer
      REAL     :: MDWN  ( NLAYS )               ! ACM down mix rate
      REAL     :: MFAC  ( NLAYS )               ! intermediate loop factor
      REAL     :: AA    ( NLAYS )               ! matrix column one
      REAL     :: BB1   ( NLAYS )               ! diagonal for MATRIX1
      REAL     :: BB2   ( NLAYS )               ! diagonal for TRI
      REAL     :: CC    ( NLAYS )               ! subdiagonal
      REAL     :: EE1   ( NLAYS )               ! superdiagonal for MATRIX1
      REAL     :: EE2   ( NLAYS )               ! superdiagonal for TRI
      REAL, ALLOCATABLE, SAVE :: DD ( :,: )     ! R.H.S
      REAL, ALLOCATABLE, SAVE :: UU ( :,: )     ! returned solution
      REAL        DFACP, DFACQ
      REAL     :: DFSP( NLAYS ), DFSQ( NLAYS )  ! intermediate loop factors
      REAL        DELC, DELP, RP, RQ
      REAL     :: LFAC1( NLAYS )                ! intermediate factor for CONVT
      REAL     :: LFAC2( NLAYS )                ! intermediate factor for CONVT
      REAL     :: LFAC3( NLAYS )                ! intermediate factor for eddy
      REAL     :: LFAC4( NLAYS )                ! intermediate factor for eddy
      REAL, ALLOCATABLE, SAVE :: DEPVCR     ( : )   ! dep vel in one cell
                                                    ! one cell for each landuse category
      REAL, ALLOCATABLE, SAVE :: EFAC1 ( : )
      REAL, ALLOCATABLE, SAVE :: EFAC2 ( : )
      REAL, ALLOCATABLE, SAVE :: POL   ( : )    ! prodn/lossrate = PLDV/DEPV
      REAL        PLDV_HONO                     ! PLDV for HONO
      REAL        DEPV_NO2                      ! dep vel of NO2
      REAL        DEPV_HNO3                     ! dep vel of HNO3
      REAL        FNL                           ! ACM2 Variable
      INTEGER     NLP, NL, LCBL
      INTEGER, SAVE :: NO2_HIT = 0, HONO_HIT = 0, HNO3_HIT = 0, NO2_MAP= 0, HONO_MAP = 0, HNO3_MAP = 0
      INTEGER, SAVE :: O3_HIT = 0, O3_MAP = 0
      INTEGER, SAVE :: NH3_HIT = 0
      REAL        DTLIM, DTS, DTACM, RZ

      INTEGER     ASTAT
      INTEGER     C, R, L, S, V, I, J           ! loop induction variables
      INTEGER     MDATE, MTIME                  ! internal simulation date&time
!--Local Arrays for Z-coord implimentation
      REAL     :: DZH   ( NLAYS )               ! ZF(L) - ZF(L-1)
      REAL     :: DZHI  ( NLAYS )               ! 1/DZH
      REAL     :: DZFI  ( NLAYS )               ! ZH(L+1) - ZH(L)
      integer  gl_c, gl_r
#ifdef isam
      REAL :: TOTAL_SA_NO2
      REAL, ALLOCATABLE, SAVE :: SA_DDBF( : )
      INTEGER IBGN, JSPCTAG

      REAL, ALLOCATABLE,SAVE :: SAEMIS( :,: )
      REAL, ALLOCATABLE,SAVE :: SACONC( :,: )
      REAL, ALLOCATABLE,SAVE :: SA_DD( :,: )
      REAL, ALLOCATABLE,SAVE :: SA_UU( :,: )

      REAL, ALLOCATABLE,SAVE :: SAFRAC( : )
      REAL, ALLOCATABLE,SAVE :: SA_NO2( : )      
      REAL, ALLOCATABLE,SAVE :: SA_SUM( : )

      INTEGER, SAVE              :: ISAM_INDEX_NO2 = 0   ! ...Index locating NO2 in ISAM
      INTEGER, SAVE              :: ISAM_INDEX_NH3 = 0   ! ...Index locating NH3 in ISAM
      INTEGER, SAVE              :: PLDV_INDEX_NH3 = 0   ! ...Index locating NH3 in PLDV

      INTEGER, ALLOCATABLE, SAVE :: ISAM_DEPV( : )
      INTEGER, ALLOCATABLE, SAVE :: INDEX_SA_HONO( : )
      INTEGER, ALLOCATABLE, SAVE :: INDEX_SA_NH3( : )


      CHARACTER( 16 ) :: ISAM_SPECIES

      INTEGER TOP, BOT

      REAL NH3_SUM
#endif      


      INTERFACE
         SUBROUTINE MATRIX1 ( KL, A, B, E, D, X )
            INTEGER,        INTENT( IN )  :: KL
            REAL,           INTENT( IN )  :: A( : ), B( : ), E( : )
            REAL,           INTENT( IN )  :: D( :,: )
            REAL,           INTENT( OUT ) :: X( :,: )
         END SUBROUTINE MATRIX1
         SUBROUTINE TRI ( L, D, U, B, X )
            REAL,           INTENT( IN )  :: L( : ), D( : ), U( : )
            REAL,           INTENT( IN )  :: B( :,: )
            REAL,           INTENT( OUT ) :: X( :,: )
         END SUBROUTINE TRI
#ifdef isam
         SUBROUTINE SA_MATRIX1 ( KL, A, B, E, D, X )
            INTEGER,        INTENT( IN )  :: KL
            REAL,           INTENT( IN )  :: A( : ), B( : ), E(: )
            REAL,           INTENT( IN )  :: D( :,: )
            REAL,           INTENT( OUT ) :: X( :,: )
         END SUBROUTINE SA_MATRIX1

         SUBROUTINE SA_TRI ( L, D, U, B, X )
            REAL,           INTENT( IN )  :: L( : ), D( : ), U( : )
            REAL,           INTENT( IN )  :: B( :,: )
            REAL,           INTENT( OUT ) :: X( :,: )
         END SUBROUTINE SA_TRI
#endif
      END INTERFACE

C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN

         FIRSTIME = .FALSE.

         MDATE = 0; MTIME = 0

C set auxiliary depv arrays

         ALLOCATE ( DD_FAC( N_SPC_DEPV  ),
     &              DDBF  ( N_SPC_DEPV ),
     &              DEPVCR( N_SPC_DEPV ),
     &              EFAC1 ( N_SPC_DEPV ),
     &              EFAC2 ( N_SPC_DEPV ),
     &              POL   ( N_SPC_DEPV ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating DD_FAC, DDBF, DEPVCR, EFAC1, EFAC2, or POL'
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( CMPF( LCMP ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating CMPF'
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( CONC( N_SPC_DIFF,NLAYS ),
     &              EMIS( N_SPC_DIFF,NLAYS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating CONC or EMIS'
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF
         CONC = 0.0; EMIS = 0.0   ! array assignment

         ALLOCATE ( DD( N_SPC_DIFF,NLAYS ),
     &              UU( N_SPC_DIFF,NLAYS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating DD or UU'
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF
         DD = 0.0; UU = 0.0   ! array assignment

         HONO_HIT = 0; HNO3_HIT = 0; NO2_HIT  = 0; NH3_HIT = 0
         HONO_MAP = 0; HNO3_MAP = 0; NO2_MAP  = 0
         DO V = 1, N_SPC_DEPV
            IF ( DV2DF_SPC( V ) .EQ. 'NO2' ) THEN
               NO2_HIT = V
               NO2_MAP = DV2DF( V )
            ELSE IF ( DV2DF_SPC( V ) .EQ. 'HONO' ) THEN
               HONO_HIT = V
               HONO_MAP = DV2DF( V )
            ELSE IF ( DV2DF_SPC( V ) .EQ. 'HNO3' ) THEN
               HNO3_HIT = V
               HNO3_MAP = DV2DF( V )
            ELSE IF ( DV2DF_SPC( V ) .EQ. 'NH3' ) THEN
               NH3_HIT = V
            ELSE IF ( DV2DF_SPC( V ) .EQ. 'O3' ) THEN
               O3_HIT = V
               O3_MAP = DV2DF( V )
            END IF
         END DO

#ifdef isam
         ALLOCATE (  SA_DDBF( N_SPCTAG ), 
     &               SACONC( N_SPCTAG, NLAYS ),
     &               SAEMIS( N_SPCTAG, NLAYS ), 
     &               SA_DD ( N_SPCTAG, NLAYS ),
     &               SA_UU ( N_SPCTAG, NLAYS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure ISAM diffusion variables'
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF
         ALLOCATE (  SAFRAC ( N_SPCTAG ),
     &               SA_SUM ( NSPC_SA ),
     &               ISAM_DEPV( N_SPCTAG ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure ISAM depv variables'
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF
         
         ALLOCATE ( SA_NO2( NTAG_SA ),
     &              INDEX_SA_HONO( NTAG_SA ), 
     &              INDEX_SA_NH3 ( NTAG_SA ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure ISAM HONO/NH3 variables'
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF
         
         SACONC = 0.0
         SAEMIS = 0.0
         SA_DD  = 0.0
         SA_UU  = 0.0

         SAFRAC        = 0.0
         ISAM_DEPV     = 0
         SA_NO2        = 1.0 / REAL( NTAG_SA )
         INDEX_SA_HONO = 0
         INDEX_SA_NH3  = 0

! set default partitioning of surface fluxes
         DO JSPCTAG = 1, N_SPCTAG
            IF ( T_SPCTAG( JSPCTAG ) .EQ. OTHRTAG ) THEN
               SAFRAC( JSPCTAG ) = 1.0
            ELSE
               SAFRAC( JSPCTAG ) = 0.0
            END IF
         END DO
! find NO2 in tracked species
         DO S = 1, NSPC_SA
            IF( ISAM_SPEC( S,1 ) .EQ. 'NO2' )THEN
                ISAM_INDEX_NO2 = S
                EXIT
            END IF            
         END DO

! find NH3 in tracked species
         IF ( SA_BIDI ) THEN
           DO S = 1, NSPC_SA
              IF( ISAM_SPEC( S,1 ) .EQ. 'NH3' )THEN
                  ISAM_INDEX_NH3 = S
                  EXIT
              END IF
           END DO
           S = -1
           S = INDEX1( 'NH3', N_SPC_DEPV, DEPV_SPC )
           IF ( S .GT. 0 ) THEN
             PLDV_INDEX_NH3 = S
           ELSE
             XMSG = 'NH3 not found in DEPV_SPC array'
             CALL M3EXIT( 'PLDV_INDEX_NH3', 0, 0, XMSG, XSTAT1 )
           END IF
         END IF

! set indices determining depv treatment, equals zero if none
         ITAG = 0
         WRITE(LOGDEV,'(/,A7,1X,2(A16,1X))')'JSPCTAG','ISAM_SPECIES','DEPV Value'
         DO JSPCTAG = 1, N_SPCTAG
            ISAM_SPECIES = ISAM_SPEC( S_SPCTAG( JSPCTAG ),1 )
            IF( TRIM( ISAM_SPECIES ) .EQ. 'HONO' )THEN
               ITAG = ITAG + 1
               INDEX_SA_HONO( ITAG ) = JSPCTAG
            END IF
            DO V = 1, N_SPC_DEPV
               IF ( TRIM( ISAM_SPECIES ) .EQ. DV2DF_SPC( V ) ) THEN
                    ISAM_DEPV( JSPCTAG ) = V
               END IF  
            END DO
        END DO       
        WRITE(LOGDEV,'(/,A4,1X,A13,1X,A16)')'ITAG','INDEX_SA_HONO','ISAM_SPECIES'
        DO ITAG = 1, NTAG_SA
           JSPCTAG = INDEX_SA_HONO( ITAG )
           IF ( JSPCTAG .GT. 0 ) THEN
              WRITE(LOGDEV,'(I2,3X,I4,8X,A16)')ITAG,JSPCTAG,VNAM_SPCTAG( JSPCTAG )
           ELSE
              WRITE(LOGDEV,'(I2,3X,I4,8X,A16)')ITAG,JSPCTAG,'MISSING'
           END IF   
        END DO
c       WRITE(LOGDEV,* )'TAG_species, Default Partitioning Coeff.'
c       DO JSPCTAG = 1, N_SPCTAG
c         WRITE(LOGDEV,*)VNAM_SPCTAG( JSPCTAG ),' ,',SAFRAC( JSPCTAG )
c       END DO          
        IF ( ABFLUX .AND. L_NH4 ) THEN
          ITAG = 0
          DO JSPCTAG = 1, N_SPCTAG
            ISAM_SPECIES = ISAM_SPEC( S_SPCTAG( JSPCTAG ),1 )
            IF( TRIM( ISAM_SPECIES ) .EQ. 'NH3' )THEN
               ITAG = ITAG + 1
               INDEX_SA_NH3( ITAG ) = JSPCTAG
            END IF
          END DO

c         DO ITAG = 1, NTAG_SA
c           JSPCTAG = INDEX_SA_NH3( ITAG )
c           SAFRAC( JSPCTAG ) = 0.0 ! to not double count the bi-di emmissions
c         END DO
        END IF
#endif

#ifdef sens
         ALLOCATE ( S_POL   ( NPMAX, N_SPC_DEPV ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating S_POL'
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF
         S_POL = 0.0

         ALLOCATE ( S_DDBF( N_SPC_DEPV, NPMAX ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating S_DBF'
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF
         S_DDBF = 0.0

         ALLOCATE ( SENS( N_SPC_DIFF,NLAYS,NPMAX ),
     &              S_EMIS( N_SPC_DIFF,NLAYS,NPMAX ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating SENS or S_EMIS'
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF
         SENS = 0.0; S_EMIS = 0.0   ! array assignment

         ALLOCATE ( S_DD( N_SPC_DIFF,NLAYS,NPMAX ),
     &              S_UU( N_SPC_DIFF,NLAYS,NPMAX ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating S_DD or S_UU'
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF
         S_DD = 0.0; S_UU = 0.0   ! array assignment

         ALLOCATE ( S_PLDV_HONO( NPMAX ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating S_PLDV_HONO'
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF
         S_PLDV_HONO = 0.0  ! array assignment


#endif
 
      END IF   !  if Firstime

C ------------------------------------------- Row, Col LOOPS -----------

      DO 345 R = 1, NROWS
      DO 344 C = 1, NCOLS
         DZH(1)  =  Met_Data%ZF( C,R,1 )
         DZHI(1) =  1./DZH(1)
         DO L = 2, NLAYS
            DZH(L)  =  Met_Data%ZF( C,R,L ) - Met_Data%ZF( C,R,L-1 ) 
            DZHI(L) =  1./DZH(L)
         ENDDO
         DO L = 1, NLAYS - 1
            DZFI(L) = 1. / ( Met_Data%ZH( C,R,L+1 ) - Met_Data%ZH( C,R,L ) )
         ENDDO
         DZFI(NLAYS) = DZFI(NLAYS-1)

C for ACM time step
         DTLIM = DTSEC

C dt = .75 dzf*dzh / Kz
         DO L = 1, NLAYS - 1
            DTLIM = MIN( DTLIM, 0.75 / ( SEDDY( L,C,R ) * DZHI(L)*DZFI(L) ) )
         END DO
         MBARKS = 0.0   ! array assignment
         MDWN = 0.0     ! array assignment

C conjoin ACM & EDDY ---------------------------------------------------

         MBAR = 0.0
         FNL = 0.0

         IF ( Met_Data%CONVCT( C,R ) ) THEN   ! Do ACM for this column
            LCBL = Met_Data%LPBL( C,R )
            MEDDY = SEDDY( 1,C,R ) * DZFI(1) / (Met_Data%PBL( C,R ) - Met_Data%ZF(C,R,1))
            FNL = 1.0 / ( 1.0 + ( ( KARMAN / ( -Met_Data%HOL( C,R ) ) ) ** 0.3333 )
     &                / ( 0.72 * KARMAN ) )
            MBAR = MEDDY * FNL
            IF ( MEDDY .LT. EPS ) THEN
               gl_c = c + COLSX_PE(1,mype+1) -1
               gl_r = r + ROWSX_PE(1,mype+1) -1
               WRITE( LOGDEV,* ) ' Warning --- MEDDY < 1e-6 s-1'
               WRITE( LOGDEV,* ) ' SEDDY, MEDDY, FNL, HOL = ',
     &                             SEDDY( 1,C,R ), MEDDY, FNL, Met_Data%HOL( C,R )
               XMSG = '*** ACM fails ***'
               WRITE( LOGDEV,*)' c,r=', gl_c,gl_r,' pbl,ust=',Met_Data%PBL( C,R ),Met_Data%USTAR( C,R )
!               CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT2 )
            END IF

            IF ( ( FNL .LE. 0.0 ) .OR.            ! never gonna happen for CONVCT
     &           ( LCBL .GE. NLAYS-1 ) .OR.       ! .GT. never gonna happen
     &           ( Met_Data%HOL( C,R ) .GT. -0.00001 ) )   ! never gonna happen
     &         WRITE( LOGDEV,1015 ) LCBL, MBAR, FNL, SEDDY( 1,C,R ), Met_Data%HOL( C,R )
1015           FORMAT( ' LCBL, MBAR, FNL, SEDDY1, HOL:', I3, 1X, 4(1PE13.5) )

            DO L = 2, LCBL 
               SEDDY( L,C,R ) = ( 1.0 - FNL ) * SEDDY( L,C,R  )
               MBARKS( L ) = MBAR
               MDWN( L )   = MBAR * (Met_Data%PBL( C,R ) - Met_Data%ZF(C,R,L-1)) * DZHI(L)
            END DO
            SEDDY( 1,C,R ) = ( 1.0 - FNL ) * SEDDY( 1,C,R  )
            MBARKS(1) = MBAR
            MBARKS(LCBL) = MDWN(LCBL)
            MDWN(LCBL+1) = 0.0

C Modify Timestep for ACM2
            RZ     = (Met_Data%ZF(C,R,LCBL) - Met_Data%ZF(C,R,1)) * DZHI(1)
            DTACM  = 1.0 / ( MBAR * RZ )
            DTLIM  = MIN( 0.75 * DTACM, DTLIM )
         ELSE
            LCBL = 1
         END IF

C-----------------------------------------------------------------------

         NLP = INT( DTSEC / DTLIM + 0.99 )
         IF ( VDIFFDIAG ) NLPCR( C,R ) = REAL( NLP )
         DTS = DTSEC / REAL( NLP )
         DTDENS1 = DTS * Met_Data%DENS1( C,R )
         DFACP = THETA * DTS
         DFACQ = THBAR * DTS

#ifdef Verbose_Vdiff
        IF ( R .EQ. NROWS / 2 .AND. C .EQ. NCOLS / 2 )
     &      WRITE( LOGDEV,1021 ) Met_Data%CONVCT( C,R ), DTS, EDDYV( C,R,1 ), MBAR, FNL
1021  FORMAT( ' CONVCT, DTS, EDDYV, MBAR, FNL: ', L3, 1X, 4(1PE13.5) )
#endif

         DO L = 1, NLAYS
            DO V = 1, N_SPC_DIFF
               CONC( V,L ) = CNGRD( DIFF_MAP( V ),L,C,R )
            END DO
#ifdef isam
            DO JSPCTAG = 1, N_SPCTAG
               SACONC( JSPCTAG,L ) = ISAM( C,R,L,S_SPCTAG( JSPCTAG ),T_SPCTAG( JSPCTAG ) )
            END DO
#endif
         END DO

#ifdef sens
         DO NP = 1, NPMAX
            DO L = 1, NLAYS
               DO V = 1, N_SPC_DIFF
                  SENS( V,L,NP ) = SNGRD( DIFF_MAP( V ),L,C,R,NP )
               END DO
            END DO
         END DO
#endif

#ifdef isam
         SA_SUM = 0.0
         DO V = 1, NSPC_SA
            DO ITAG = 1, NTAG_SA
               SA_SUM( V ) = SA_SUM( V ) + ISAM( C,R,1,V,ITAG )
            END DO
            SA_SUM( V ) = MAX ( 1.0E-30, SA_SUM( V ) )
         END DO

         SAFRAC = 0.0
         DO JSPCTAG = 1, N_SPCTAG
            SAFRAC( JSPCTAG ) = SACONC( JSPCTAG,1 ) / SA_SUM ( S_SPCTAG( JSPCTAG )   ) 
         END DO

         IF ( ABFLUX .AND. L_NH4 ) THEN
            DO ITAG = 1, NTAG_SA
               JSPCTAG = INDEX_SA_NH3( ITAG )
               SAFRAC( JSPCTAG ) = 0.0 ! to not double count the bi-di emmissions
            END DO
         END IF

         IF( L_NO3 .AND. SFC_HONO ) THEN
! compute the flux partitioning for HONO from NO2 surface reaction
            DO ITAG = 1, NTAG_SA
               SA_NO2( ITAG ) = MAX( ISAM( C,R,1,ISAM_INDEX_NO2,ITAG ), 1.0E-30 )
            END DO
            TOTAL_SA_NO2 = 1.0 / SUM( SA_NO2 )
            DO ITAG = 1, NTAG_SA
               JSPCTAG = INDEX_SA_HONO( ITAG )
               SAFRAC( JSPCTAG ) = SA_NO2( ITAG ) * TOTAL_SA_NO2 
            END DO 
         END IF                
#endif
 
         EMIS = 0.0      ! array assignment
         IF ( DESID_N_SRM .GE. 1 ) 
     &        EMIS( :,1:DESID_LAYS ) = DTS * VDEMIS_DIFF( :,:,C,R )

#ifdef isam
         SAEMIS = 0.0

! modify ground emissions for bidirectional species (for bidi, PLVD > 0.0)
         IF ( SA_BIDI ) THEN
           SA_VDEMIS_DIFF( ISAM_INDEX_NH3,1,C,R,BIDITAG )
     &             = PLDV(PLDV_INDEX_NH3,C,R) * Met_Data%RDEPVHT( C,R )
         END IF

! collapse ISAM emissions array
         DO L = 1, DESID_LAYS
            DO ITAG = 1, NTAG_SA
               BOT = (ITAG-1)*NSPC_SA+1
               TOP = NSPC_SA*ITAG
               SAEMIS( BOT:TOP,L ) = DTS * SA_VDEMIS_DIFF( :,L,C,R,ITAG )
            END DO
         END DO
#endif

#ifdef sens
         S_EMIS = 0.0
         DO NP = 1, NPMAX
            S_EMIS( :,1:DESID_LAYS,NP ) = DTS * SVDEMIS_DIFF( :,:,C,R,NP )
         END DO
#endif

         DO L = 1, NLAYS
            DFSP( L ) = DFACP * DZHI( L )
            DFSQ( L ) = DFACQ * DZHI( L )
            EDDY( L ) = SEDDY( L,C,R ) * DZFI(L)
         END DO

         RP = DFACP * Met_Data%RDEPVHT( C,R )
         RQ = DFACQ * Met_Data%RDEPVHT( C,R )
         DO V = 1, N_SPC_DEPV
            DDBF( V )   = DDEP( V,C,R )
            DEPVCR( V ) = DEPV( V,C,R )
            DD_FAC( V ) = DTDENS1 * DD_CONV( V ) * DEPVCR( V )
            EFAC1 ( V ) = EXP( -DEPVCR( V ) * RP )
            EFAC2 ( V ) = EXP( -DEPVCR( V ) * RQ )
            POL   ( V ) = PLDV( V,C,R ) / DEPVCR( V )
#ifdef sens
            DO NP = 1, NPMAX
               S_POL   ( NP,V ) = S_PLDV( NP,V,C,R ) / DEPVCR( V )
               S_DDBF( V, NP ) = S_DDEP( V, C, R, NP )
            END DO
#endif
         END DO
         PLDV_HONO = PLDV( HONO_HIT,C,R )
#ifdef sens
         DO NP = 1, NPMAX
            S_PLDV_HONO( NP ) = S_PLDV( NP,HONO_HIT,C,R )
         END DO
#endif

#ifdef isam
         DO JSPCTAG = 1, N_SPCTAG
            SA_DDBF( JSPCTAG ) = SA_DDEP( C,R,JSPCTAG )
         END DO
#endif

C These don`t change in the NLP sub-time step loop:---------------------
         DO L = 1, NLAYS
            AA ( L ) = 0.0
            BB1( L ) = 0.0
            EE1( L ) = 0.0
            CC ( L ) = 0.0
            EE2( L ) = 0.0
            BB2( L ) = 0.0
         END DO
         IF ( Met_Data%CONVCT( C,R ) ) THEN
            L = 1
            DELP = Met_Data%PBL( C,R ) - Met_Data%ZF( C,R,L )
            BB1( L ) = 1.0 + DELP * DFSP( L ) * MBARKS( L )
            LFAC1( L ) = DFSQ( L ) * DELP * MBARKS( L )
            LFAC2( L ) = DFSQ( L ) * MDWN( L+1 ) * DZH( L+1 )
            DO L = 2, LCBL
               AA ( L ) = -DFACP * MBARKS( L )
               BB1( L ) = 1.0 + DFACP * MDWN( L )
               EE1( L ) = -DFSP( L-1 ) * DZH( L ) * MDWN( L )
               MFAC( L ) = DZH( L+1 ) * DZHI( L ) * MDWN( L+1 )
            END DO
         END IF

         DO L = 1, NLAYS
            EE2( L ) = - DFSP( L ) * EDDY( L )
            LFAC3( L ) = DFSQ( L ) * EDDY( L )
         END DO

         BB2( 1 ) = 1.0 - EE2( 1 )
         DO L = 2, NLAYS
            CC ( L ) = - DFSP( L ) * EDDY( L-1 )
            BB2( L ) = 1.0 - CC( L ) - EE2( L )
            LFAC4( L ) = DFSQ( L ) * EDDY( L-1 )
         END DO

         DO 301 NL = 1, NLP      ! loop over sub time

            DO V = 1, N_SPC_DEPV

C --------- HET HONO RX -----------------

C Use special treatment for HNO3
C HNO3 produced via the heterogeneous reaction sticks on surfaces and
C is accounted as depositional loss; calculate increased deposition loss
               IF ( V .EQ. HNO3_HIT ) THEN
                  S = HNO3_MAP
                  CONC( S,1 ) = POL( V ) + ( CONC( S,1 ) - POL( V ) ) * EFAC1( V )
                  DEPV_HNO3 = DEPVCR( V ) + PLDV_HONO / CONC( NO2_MAP,1 )
                  DD_FAC( V ) = DTDENS1 * DD_CONV( V ) * DEPV_HNO3
                  DDBF( V ) = DDBF( V ) + THETA * DD_FAC( V ) * CONC( S,1 )
#ifdef sens
                  DO NP = 1, NPMAX
                     SENS( S,1,NP ) = S_POL( NP,V ) + ( SENS( S,1,NP ) - S_POL( NP,V ) ) * EFAC1( V )
                     S_DDBF( V, NP ) = S_DDBF( V, NP ) + THETA * DD_FAC( V ) * SENS ( S,1,NP )
                  END DO
#endif

C Use special treatment for NO2
C Loss of NO2 via the heterogeneous reaction is accounted for as an additional
C depositional loss. Add the loss of NO2 via the heterogeneous reaction
C to the regular deposition velocity (increased dep. vel.).  This will
C reduce the NO2 conc. in the atmosphere without affecting the depositional loss.
               ELSE IF ( V .EQ. NO2_HIT ) THEN
                  S = NO2_MAP
                  DEPV_NO2 = DEPVCR( V ) + 2.0 * PLDV_HONO / CONC( S,1 )
                  EFAC1 ( V ) = EXP( -DEPV_NO2 * RP )
                  EFAC2 ( V ) = EXP( -DEPV_NO2 * RQ )
                  POL   ( V ) = PLDV( V,C,R ) / DEPV_NO2
                  CONC( S,1 ) = POL( V ) + ( CONC( S,1 ) - POL( V ) ) * EFAC1( V )
                  DDBF( V ) = DDBF( V ) + THETA * DD_FAC( V ) * CONC( S,1 )
#ifdef sens
                  DO NP = 1, NPMAX
                     S_POL( NP, V ) = S_PLDV( NP,V,C,R ) / DEPV_NO2
                     SENS( S,1,NP ) = S_POL( NP,V ) + ( SENS( S,1,NP ) - S_POL( NP,V ) ) * EFAC1( V )
                     S_DDBF( V, NP ) = S_DDBF( V, NP ) + THETA * DD_FAC( V ) * SENS ( S,1,NP )
                  END DO
#endif

               ELSE IF ( V .EQ. HONO_HIT ) THEN
                  S = HONO_MAP
                  CONC( S,1 ) = POL( V ) + ( CONC( S,1 ) - POL( V ) ) * EFAC1( V )
                  DDBF( V ) = DDBF( V ) + THETA * DD_FAC( V ) * CONC( S,1 )
C Don't add HONO emissions as negative dep flux
!     &                      - DTDENS1 * DD_CONV( V ) * PLDV( V,C,R ) )
#ifdef sens
                  DO NP = 1, NPMAX
                     SENS( S,1,NP ) = S_POL( NP,V ) + ( SENS( S,1,NP ) - S_POL( NP,V ) ) * EFAC1( V )
                     S_DDBF( V, NP ) = S_DDBF( V, NP ) + THETA * DD_FAC( V ) * SENS ( S,1,NP )
                  END DO
#endif
  
C --------- END of HET HONO RX ----------

               ELSE

C Pass selected N species to the BDSNP Soil NO emissions scheme

                  IF ( MGN_ONLN_DEP ) THEN

                    IF(SPECLOG) then
                      IF( V .eq. N_SPC_DEPV)  THEN
                       SPECLOG = .false. ! no need to do any species more than once
                       WRITE( LOGDEV,*) 'BDSNP Species list complete', speclog
                      END IF
                    END IF

                    IF ( (INDEX(TRIM( DV2DF_SPC( V ) ), 'NH3') .NE. 0) .OR.
     &                 (INDEX(TRIM( DV2DF_SPC( V ) ), 'NH4') .NE. 0) .OR.         
     &                 (INDEX(TRIM( DV2DF_SPC( V ) ), 'HNO3').NE. 0) .OR.
     &                 (INDEX(TRIM( DV2DF_SPC( V ) ), 'NO3') .NE. 0) .OR.
     &                 (INDEX(TRIM( DV2DF_SPC( V ) ), 'NO2') .NE. 0) .OR.
     &                 (INDEX(TRIM( DV2DF_SPC( V ) ), 'PAN') .NE. 0)) THEN


                      IF( SPECLOG ) THEN !write species each time it is used
                        WRITE( LOGDEV,*) 'BDSNP Dry Species Used:', TRIM(DV2DF_SPC( V ) ), V, N_SPC_DEPV
                      END IF

                      IF ( ( DDBF(V)- DDEP( V,C,R) ) .LT. 0.0 ) THEN !negative error checking

                       XMSG = 'Negative Deposition'
!                  WRITE( LOGDEV,*) 'BDSNP Negative Deposition vdiff, variable:', 
!     &            TRIM( DV2DF_SPC( V )), ( DDBF(V)- DDEP( V,C,R) ), C, R                  
!                      CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
                       CALL GET_N_DEP (DV2DF_SPC( V ), 0/
     &                           DTSEC, C, R )
                      else
                       CALL GET_N_DEP (DV2DF_SPC( V ), ( DDBF(V)- DDEP( V,C,R) )/
     &                           DTSEC, C, R )
                      END IF !end negative error checking

                     
                    END IF !end species check
                  
                  END IF !end BDSNP check

                  S = DV2DF( V )
                  CONC( S,1 ) = POL( V ) + ( CONC( S,1 ) - POL( V ) ) * EFAC1( V )
                  DDBF( V ) = DDBF( V ) + THETA * ( DD_FAC( V ) * CONC( S,1 ) 
C Add evasion as negative dep flux                  
     &                      - DTDENS1 * DD_CONV( V ) * PLDV( V,C,R ) )         

#ifdef sens
                  DO NP = 1, NPMAX
                     SENS( S,1,NP ) = S_POL( NP,V ) + ( SENS( S,1,NP ) - S_POL( NP,V ) ) * EFAC1( V )
                     S_DDBF( V, NP ) = S_DDBF( V, NP ) + THETA * ( DD_FAC( V ) * SENS( S, 1,NP )
     &                      - DTDENS1 * DD_CONV( V ) * S_PLDV( NP,V,C,R ) )
                  END DO
#endif

               END IF

            END DO
    
            DO L = 1, NLAYS
               DO V = 1, N_SPC_DIFF
                  DD( V,L ) = 0.0
                  UU( V,L ) = 0.0
#ifdef sens
                  DO NP = 1, NPMAX
                     S_DD( V,L,NP) = 0.0
                     S_UU( V,L,NP) = 0.0
                  END DO
#endif
               END DO
            END DO

#ifdef isam
            DO JSPCTAG = 1, N_SPCTAG
               S = ISAM_DEPV( JSPCTAG )
               IF ( S .GT. 0 ) THEN
                  SACONC( JSPCTAG,1 ) = SACONC( JSPCTAG,1 ) * EFAC1( S )
     &                                + SAFRAC( JSPCTAG ) * POL( S ) * ( 1.0 -  EFAC1( S ) )               
                  SA_DDBF( JSPCTAG  ) = SA_DDBF( JSPCTAG )
     &                                + THETA * DD_FAC( S ) *  SACONC( JSPCTAG,1 )
               END IF
            END DO

c Recalculate bidi NH3 deposition
            IF ( ABFLUX .AND. SA_BIDI ) THEN
              NH3_SUM = 0.0
              DO JSPCTAG = 1, N_SPCTAG
                S = ISAM_DEPV( JSPCTAG )
                IF ( S .EQ. NH3_HIT ) NH3_SUM = NH3_SUM + SACONC( JSPCTAG,1 )
              END DO

              DO JSPCTAG = 1, N_SPCTAG
                S = ISAM_DEPV( JSPCTAG )
                IF ( S .EQ. NH3_HIT ) THEN
                  IF( NH3_SUM .GT. 1.0E-25  ) THEN
                    SA_DDBF( JSPCTAG  ) = DDBF(NH3_HIT) * ( SACONC( JSPCTAG,1 ) / NH3_SUM )
                  ELSE
                    SA_DDBF( JSPCTAG  ) = 0.0
                  END IF
                END IF
              END DO
            END IF

            DO L = 1, NLAYS
               DO V = 1, N_SPCTAG
                 SA_DD( V,L ) = 0.0
                 SA_UU( V,L ) = 0.0
               END DO
            END DO
#endif

C Compute tendency of CBL concentrations - semi-implicit solution
C Set MATRIX1 elements A (col 1), B (diag.), E (superdiag.) and D (RHS)

            IF ( Met_Data%CONVCT( C,R ) ) THEN

               L = 1
               DO V = 1, N_SPC_DIFF
                  DD( V,L ) = CONC( V,L )
     &                      - LFAC1( L ) * CONC( V,L )
     &                      + LFAC2( L ) * CONC( V,L+1 ) 
#ifdef sens
                  DO NP = 1, NPMAX
                     S_DD( V,L,NP ) = SENS( V,L,NP )
     &                         - LFAC1( L ) * SENS( V,L,NP )
     &                         + LFAC2( L ) * SENS( V,L+1,NP )
                  END DO
#endif
               END DO

#ifdef isam
               DO JSPCTAG = 1, N_SPCTAG
                  SA_DD( JSPCTAG, L) = SACONC( JSPCTAG,L )
     &                      - LFAC1( L ) * SACONC( JSPCTAG,L )
     &                      + LFAC2( L ) * SACONC( JSPCTAG,L+1 )
               ENDDO
#endif

               DO L = 2, LCBL
                  DO V = 1, N_SPC_DIFF
                     DELC = MBARKS( L ) * CONC( V,1 )
     &                    -   MDWN( L ) * CONC( V,L )
     &                    +   MFAC( L ) * CONC( V,L+1 )
                     DD( V,L ) = CONC( V,L ) + DFACQ * DELC
#ifdef sens    
                     DO NP = 1, NPMAX
                        S_DELC = MBARKS( L ) * SENS( V,1,NP )
     &                       -   MDWN( L ) * SENS( V,L,NP )
     &                       +   MFAC( L ) * SENS( V,L+1,NP )
                        S_DD( V,L,NP ) = SENS( V,L,NP ) + DFACQ * S_DELC
                     END DO
#endif         
                  END DO

#ifdef isam
                  DO JSPCTAG = 1, N_SPCTAG
                     DELC = MBARKS( L ) * SACONC( JSPCTAG,1 )
     &                     -   MDWN( L ) * SACONC( JSPCTAG,L )
     &                     +   MFAC( L ) * SACONC( JSPCTAG,L+1 )
                     SA_DD( JSPCTAG,L ) = SACONC( JSPCTAG,L ) + DFACQ  * DELC
                  END DO
#endif

               END DO

               CALL MATRIX1 ( LCBL, AA, BB1, EE1, DD, UU )
#ifdef isam 
               CALL SA_MATRIX1( LCBL, AA, BB1,EE1, SA_DD, SA_UU)
#endif

#ifdef sens
               DO NP = 1, NPMAX
                  CALL MATRIX1 ( LCBL, AA, BB1, EE1, S_DD(:,:,NP), S_UU(:,:,NP) )
               END DO
#endif


C update conc
               DO L = 1, LCBL
                  DO V = 1, N_SPC_DIFF
                     CONC( V,L ) = UU( V,L )
#ifdef sens
                     DO NP = 1, NPMAX
                        SENS( V,L,NP ) = S_UU( V,L,NP )
                     END DO
#endif
                  END DO
#ifdef isam
                  DO JSPCTAG = 1, N_SPCTAG
                     SACONC ( JSPCTAG,L ) = SA_UU( JSPCTAG,L )
                  ENDDO
#endif
               END DO

C reinitialize for TRI solver
               DO L = 1, NLAYS
                  DO V = 1, N_SPC_DIFF
                     DD( V,L ) = 0.0
                     UU( V,L ) = 0.0
#ifdef sens
                     DO NP = 1, NPMAX
                        S_DD( V,L,NP ) = 0.0
                        S_UU( V,L,NP ) = 0.0
                     END DO
#endif
                  END DO
#ifdef isam
                  DO JSPCTAG = 1, N_SPCTAG
                     SA_DD( JSPCTAG,L ) = 0.0
                     SA_UU( JSPCTAG,L ) = 0.0
                  ENDDO
#endif
               END DO

            END IF

            L = 1
            DO V = 1, N_SPC_DIFF
               DD( V,L ) = CONC( V,L )
     &                   + LFAC3( L ) * ( CONC( V,L+1 ) - CONC( V,L ) )
     &                   + EMIS( V,L )
#ifdef sens
               DO NP = 1, NPMAX
                  S_DD( V,L,NP ) = SENS( V,L,NP )
     &                           + LFAC3( L ) * ( SENS( V,L+1,NP ) - SENS( V,L,NP ) )
     &                           + S_EMIS( V,L,NP )
               END DO
#endif
            END DO

#ifdef isam
            DO JSPCTAG = 1, N_SPCTAG
               SA_DD( JSPCTAG,L ) = SACONC( JSPCTAG,L )
     &                   + LFAC3( L ) * ( SACONC( JSPCTAG,L+1 ) - SACONC( JSPCTAG,L ) )
     &                   + SAEMIS( JSPCTAG,L )
            END DO
#endif

            DO L = 2, NLAYS-1
               DO V = 1, N_SPC_DIFF
                  DD( V,L ) = CONC( V,L )
     &                      + LFAC3( L ) * ( CONC( V,L+1 ) - CONC( V,L ) )
     &                      - LFAC4( L ) * ( CONC( V,L ) - CONC( V,L-1 ) )
     &                      + EMIS( V,L )
#ifdef sens 
                  DO NP = 1, NPMAX
                     S_DD( V,L,NP ) = SENS( V,L,NP )
     &                              + LFAC3( L ) * ( SENS( V,L+1,NP ) - SENS( V,L,NP ) )
     &                              - LFAC4( L ) * ( SENS( V,L,NP ) - SENS( V,L-1,NP ) )
     &                              + S_EMIS( V,L,NP )
                  END DO
#endif
               END DO
#ifdef isam
               DO JSPCTAG = 1, N_SPCTAG
                  SA_DD( JSPCTAG,L ) = SACONC( JSPCTAG,L )
     &                      + LFAC3( L ) * ( SACONC( JSPCTAG,L+1 ) - SACONC( JSPCTAG,L ) )
     &                      - LFAC4( L ) * ( SACONC( JSPCTAG,L ) - SACONC( JSPCTAG,L-1 ) )
     &                      + SAEMIS( JSPCTAG,L )
               END DO
#endif
            END DO

            L = NLAYS
            DO V = 1, N_SPC_DIFF
               DD( V,L ) = CONC( V,L )
     &                   - LFAC4( L ) * ( CONC( V,L ) - CONC( V,L-1 ) )
#ifdef sens
               DO NP = 1, NPMAX
                  S_DD( V,L,NP ) = SENS( V,L,NP )
     &                           - LFAC4( L ) * ( SENS( V,L,NP ) - SENS( V,L-1,NP ) )
               END DO
#endif
            END DO
#ifdef isam
            DO JSPCTAG = 1, N_SPCTAG
               SA_DD( JSPCTAG,L ) = SACONC( JSPCTAG,L )
     &                   - LFAC4( L ) * ( SACONC( JSPCTAG,L ) - SACONC( JSPCTAG,L-1 ) )
            END DO
#endif

            CALL TRI ( CC, BB2, EE2, DD, UU )
#ifdef isam
            CALL SA_TRI ( CC, BB2, EE2, SA_DD, SA_UU )
#endif

#ifdef sens
            DO NP = 1, NPMAX
               CALL TRI ( CC, BB2, EE2, S_DD(:,:,NP), S_UU(:,:,NP) )
            END DO
#endif


C Load into CGRID
            DO L = 1, NLAYS
               DO V = 1, N_SPC_DIFF
                  CONC( V,L ) = UU( V,L )
#ifdef sens
                  DO NP = 1, NPMAX
                     SENS( V,L,NP ) = S_UU( V,L,NP )
                  END DO
#endif
               END DO
#ifdef isam
               DO JSPCTAG = 1, N_SPCTAG
                  SACONC( JSPCTAG,L ) = SA_UU( JSPCTAG,L )
               END DO
#endif
            END DO
            






            DO V = 1, N_SPC_DEPV

C --------- HET HONO RX -----------------

               IF ( V .EQ. HNO3_HIT ) THEN
                  S = HNO3_MAP
                  CONC( S,1 ) = POL( V ) + ( CONC( S,1 ) - POL( V ) ) * EFAC2( V )
                  DDBF( V ) = DDBF( V ) + THBAR * DD_FAC( V ) * CONC( S,1 )

#ifdef sens
                  DO NP = 1, NPMAX
                     SENS( S,1,NP ) = S_POL( NP,V ) + ( SENS( S,1,NP ) - S_POL( NP,V ) ) * EFAC2( V )
                     S_DDBF( V,NP ) = S_DDBF( V,NP ) + THBAR * DD_FAC( V ) * SENS( S,1,NP )
                  END DO
#endif

               ELSE IF ( V .EQ. NO2_HIT ) THEN
                  S = NO2_MAP
                  CONC( S,1 ) = POL( V ) + ( CONC( S,1 ) - POL( V ) ) * EFAC2( V )
                  DDBF( V ) = DDBF( V ) + THBAR * DD_FAC( V ) * CONC( S,1 )

#ifdef sens
                  DO NP = 1, NPMAX
                     SENS( S,1,NP ) = S_POL( NP,V ) + ( SENS( S,1,NP ) - S_POL( NP,V ) ) * EFAC2( V )
                     S_DDBF( V,NP ) = S_DDBF( V,NP ) + THBAR * DD_FAC( V ) * SENS( S,1,NP )
                  END DO
#endif

               ELSE  IF ( V .EQ. HONO_HIT ) THEN
                  S = HONO_MAP
                  CONC( S,1 ) = POL( V ) + ( CONC( S,1 ) - POL( V ) ) * EFAC2( V )
                  DDBF( V ) = DDBF( V ) + THBAR * DD_FAC( V ) * CONC( S,1 )
C Don't add HONO emissions as negative dep flux
!     &                      - DTDENS1 * DD_CONV( V ) * PLDV( V,C,R ) )
#ifdef sens
                  DO NP = 1, NPMAX
                     SENS( S,1,NP ) = S_POL( NP,V ) + ( SENS( S,1,NP ) - S_POL( NP,V ) ) * EFAC2( V )
                     S_DDBF( V,NP ) = S_DDBF( V,NP ) + THBAR * DD_FAC( V ) * SENS( S,1,NP )
                  END DO
#endif

C --------- END of HET HONO RX ----------

               ELSE
                  S = DV2DF( V )
                  CONC( S,1 ) = POL( V ) + ( CONC( S,1 ) - POL( V ) ) * EFAC2( V )
                  DDBF( V ) = DDBF( V ) + THBAR * ( DD_FAC( V ) * CONC( S,1 )
C Add evasion as negative dep flux                      
     &                      - DTDENS1 * DD_CONV( V ) * PLDV( V,C,R ) )

#ifdef sens
                  DO NP = 1, NPMAX
                     SENS( S,1,NP ) = S_POL( NP,V ) + ( SENS( S,1,NP ) - S_POL( NP,V ) ) * EFAC2( V )
                     S_DDBF( V,NP ) = S_DDBF( V,NP ) + THBAR * ( DD_FAC( V ) * SENS( S,1,NP )
     &                      - DTDENS1 * DD_CONV( V ) * S_PLDV( NP,V,C,R ) )
                  END DO
#endif
                  
               END IF

            END DO
#ifdef isam
C Update ISAM Dry Deposition
            DO JSPCTAG = 1, N_SPCTAG
               S = ISAM_DEPV( JSPCTAG )
               IF ( S .GT. 0 ) THEN
                  SACONC( JSPCTAG,1 ) = SACONC( JSPCTAG,1 ) * EFAC2( S )
     &                                + SAFRAC( JSPCTAG ) * POL( S ) * ( 1.0 -  EFAC2( S ) )               
                  SA_DDBF( JSPCTAG  ) = SA_DDBF( JSPCTAG )
     &                                + THBAR * DD_FAC( S ) *  SACONC( JSPCTAG,1 )
               END IF
            END DO

c Recalculate bidi NH3 deposition
            IF ( ABFLUX .AND. SA_BIDI ) THEN
              NH3_SUM = 0.0
              DO JSPCTAG = 1, N_SPCTAG
                S = ISAM_DEPV( JSPCTAG )
                IF ( S .EQ. NH3_HIT ) NH3_SUM = NH3_SUM + SACONC( JSPCTAG,1 )
              END DO

              DO JSPCTAG = 1, N_SPCTAG
                S = ISAM_DEPV( JSPCTAG )
                IF ( S .EQ. NH3_HIT ) THEN
                  IF( NH3_SUM .GT. 1.0E-25  ) THEN
                    SA_DDBF( JSPCTAG  ) = DDBF(NH3_HIT) * ( SACONC( JSPCTAG,1 ) / NH3_SUM )
                  ELSE
                    SA_DDBF( JSPCTAG  ) = 0.0
                  END IF
                END IF
              END DO
            END IF
#endif
    

301      CONTINUE                 ! end sub time loop


         DO L = 1, NLAYS
            DO V = 1, N_SPC_DIFF
               CNGRD( DIFF_MAP( V ),L,C,R ) = CONC( V,L )
            END DO
#ifdef isam
            DO JSPCTAG = 1, N_SPCTAG
               IF( TRANSPORT_SPC( JSPCTAG ) )THEN
                  ISAM( C,R,L,S_SPCTAG( JSPCTAG ),T_SPCTAG( JSPCTAG ) ) = SACONC( JSPCTAG,L )
               END IF
            END DO
 
#endif
         END DO

         DO V = 1, N_SPC_DEPV
            DDEP( V,C,R ) = DDBF( V )
         END DO

#ifdef isam
         DO JSPCTAG = 1, N_SPCTAG
            SA_DDEP( C,R,JSPCTAG ) = SA_DDBF( JSPCTAG )
         END DO
#endif

#ifdef sens
         DO NP = 1, NPMAX

            DO L = 1, NLAYS
               DO V = 1, N_SPC_DIFF
                  SNGRD( DIFF_MAP( V ),L,C,R,NP ) = SENS( V,L,NP )
               END DO
            END DO

            DO V = 1, N_SPC_DEPV
               S_DDEP( V,C,R,NP ) = S_DDBF( V,NP )
            END DO

         END DO
#endif


 
344   CONTINUE         !  end loop on col C
345   CONTINUE         !  end loop on row R

      RETURN
      END
