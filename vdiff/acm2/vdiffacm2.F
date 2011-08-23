
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
C $Header: /project/yoj/arc/CCTM/src/vdiff/acm2/vdiffacm2.F,v 1.5 2011/08/23 16:52:42 sjr Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)vdiffim.F 1.8 /project/mod3/CMAQ/src/vdiff/eddy/SCCS/s.vdiffim.F 25 Jul 1997 12:57:45

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE VDIFF ( CGRID, JDATE, JTIME, TSTEP )

C-----------------------------------------------------------------------
C Asymmetric Convective Model v2 (ACM2) -- Pleim(2006)
C Function:
C   calculates and writes dry deposition.
C   calculates vertical diffusion

C Subroutines and Functions Called:
C   INIT3, SEC2TIME, TIME2SEC, WRITE3, NEXTIME,
C   M3EXIT, EDDYX, TRI, MATRIX, PA_UPDATE_EMIS, PA_UPDATE_DDEP

C Revision History:
C   Analogous to VDIFFIM (Eddy diffusion PBL scheme)

C 31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module (GRID_CONF)
C  7 Jun 05 P.Bhave: added call to OPSSEMIS if MECHNAME='AE4';
C                      added TSTEP to RDEMIS_AE call vector
C    Aug 05 J. Pleim Update to v4.5
C    Jan 06 J. Pleim ACM2 implementation
C    Jul 06 J.Young: emis module
C    Nov 06 B.Hutzell, J.Young: depv module
C    Mar 08 G.Sarwar: added heterogeneous reaction for HONO. It affects HONO, NO2 and HNO3
C    Apr 08 J.Young: optional inline beis and plume rise emissions
C 21 Jun 10 J.Young: convert for Namelist redesign
C 16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C 15 Aug 11 D. Schwede and G. Sarwar: Modified HONO calculation so that
C              deposition velocity for NO2 that is output in DEPV file 
C              does not include the loss due to the heterogeneous reaction.
C              This additional loss is now accounted for in vdiff.F

C-----------------------------------------------------------------------

      USE CGRID_SPCS          ! CGRID mechanism species
      USE EMIS_DEFN           ! inherits GRID_CONF
      USE DEPV_DEFN
      USE VDIFF_MAP
      USE UTILIO_DEFN

      USE SUBST_MODULES              ! stenex
!     USE SUBST_GLOBAL_SUM_MODULE    ! stenex

      IMPLICIT NONE

      INCLUDE SUBST_PACTL_ID  ! PA control parameters
      INCLUDE SUBST_CONST     ! constants
      INCLUDE SUBST_FILES_ID  ! file name parameters

      CHARACTER( 120 ) :: XMSG = ' '

C Arguments:

      REAL, POINTER :: CGRID( :,:,:,: )              !  concentrations
      INTEGER      JDATE        ! current model date, coded YYYYDDD
      INTEGER      JTIME        ! current model time, coded HHMMSS
      INTEGER      TSTEP( 2 )   ! time step vector (HHMMSS)
                                ! TSTEP(1) = local output step
                                ! TSTEP(2) = sciproc sync. step (chem)

C Parameters:

C explicit, THETA = 0, implicit, THETA = 1
      REAL, PARAMETER :: THETA = 0.5,  ! For dry deposition term
     &                   THBAR = 1.0 - THETA
      REAL THRAT  ! THBAR/THETA

      REAL, PARAMETER :: M2PHA = 1.0E+04       ! 1 hectare = 1.0e4 m**2
      REAL, PARAMETER :: CMLMR = 1.0E+06       ! ppmV/Molar Mixing Ratio
      REAL, PARAMETER :: CNVTD = M2PHA / CMLMR / MWAIR ! combined ddep
                                                       ! conversion factor
C ACM parameters

      REAL, PARAMETER :: CRANKP = 0.5
      REAL, PARAMETER :: CRANKQ = 1.0 - CRANKP
      REAL, PARAMETER :: KARMAN = 0.4
      REAL, PARAMETER :: EPS = 1.0E-06

      INTEGER, PARAMETER :: IFACM = 1     ! 1 = acm, 0 = no acm

C External Functions: None

C File variables:

      REAL         RDEPVHT( NCOLS,NROWS )        ! air dens / dep vel height
      REAL         RJACM  ( NCOLS,NROWS,NLAYS )  ! reciprocal mid-layer Jacobian
      REAL         RVJACMF( NCOLS,NROWS,NLAYS )  ! 1/ mid-full layer vert Jac
      REAL         RRHOJ  ( NCOLS,NROWS,NLAYS )  ! reciprocal density X Jacobian
      REAL         DENS1  ( NCOLS,NROWS )        ! layer 1 air density

C Local Variables:

      CHARACTER( 16 ), SAVE :: PNAME = 'VDIFFACM2'
      CHARACTER( 80 ) :: VARDESC                ! env variable description

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      INTEGER, SAVE :: WSTEP  = 0               ! local write counter
      INTEGER  STATUS                           ! ENV... status

      REAL          DX1, DX2                    ! CX x1- and x2-cell widths
      REAL, ALLOCATABLE, SAVE :: DX3F ( : )
      REAL, ALLOCATABLE, SAVE :: RDX3F( : )     ! reciprocal layer thickness
      REAL, ALLOCATABLE, SAVE :: RDX3M( : )     ! reciprocal layer thickness
      REAL          X3M  ( NLAYS )              ! middle layer heigth
      REAL, SAVE :: CONVPA           ! conversion factor to pressure in Pascals

      REAL, ALLOCATABLE, SAVE :: CNGRD( :,:,:,: )  ! cgrid replacement

      REAL, ALLOCATABLE, SAVE :: DDEP( :,:,: )  ! ddep accumulator
      REAL, ALLOCATABLE, SAVE :: DDEP_PA( :,:,: )! ddep for process analysis
      REAL          WRDD( NCOLS,NROWS )         ! ddep write buffer

      REAL, ALLOCATABLE, SAVE :: EMIS_PA( :,:,:,: ) ! emis for process analysis

      INTEGER, SAVE :: N_SPC_CGRID              ! no. of CGRID species

      REAL, ALLOCATABLE, SAVE :: DD_FAC( : )    ! combined subexpression
      REAL, ALLOCATABLE, SAVE :: DDBF( : )      ! secondary DDEP
      REAL, ALLOCATABLE, SAVE :: CONC( :,: )    ! secondary CGRID expression
      REAL, ALLOCATABLE, SAVE :: EMIS( :,: )    ! emissions subexpression
      REAL         EDDYV ( NCOLS,NROWS,NLAYS )  ! from EDYINTB
      REAL         SEDDY ( NLAYS,NCOLS,NROWS )  ! flipped EDDYV
      INTEGER      NSTEPS( NCOLS,NROWS )        ! diffusion time steps
      REAL         DELT                         ! DT
      REAL         DTDENS1                      ! DT * layer 1 air density
      REAL         DTSEC                        ! model time step in seconds

C ACM Local Variables
      REAL        MBAR                          ! ACM2 mixing rate (S-1)
      REAL        HOL   ( NCOLS,NROWS )         ! PBL over Monin-Obukhov Len
      REAL        XPBL  ( NCOLS,NROWS )         ! PBL HT in gen coords
      INTEGER     LPBL  ( NCOLS,NROWS )         ! layer containing PBL HT
      LOGICAL     CONVCT( NCOLS,NROWS )         ! flag for ACM
      REAL        MEDDY
      REAL        EDDY  ( NLAYS )
      REAL        MBARKS( NLAYS )               ! by layer
      REAL        MDWN  ( NLAYS )               ! ACM down mix rate
      REAL        MFAC                          ! intermediate loop factor
      REAL        AA    ( NLAYS )               ! matrix column one
      REAL        BB    ( NLAYS )               ! diagonal
      REAL        CC    ( NLAYS )               ! subdiagonal
      REAL        EE    ( NLAYS )               ! superdiagonal
      REAL, ALLOCATABLE, SAVE :: DD( :,: )      ! R.H.S
      REAL, ALLOCATABLE, SAVE :: UU( :,: )      ! returned solution
      REAL        XPLUS
      REAL        XMINUS
      REAL, ALLOCATABLE, SAVE :: DEPVCR ( : )   ! dep vel in one cell
      REAL, ALLOCATABLE, SAVE :: EFAC1 ( : )
      REAL, ALLOCATABLE, SAVE :: EFAC2 ( : )
      REAL, ALLOCATABLE, SAVE :: POL   ( : )    ! prodn/lossrate = PLDV/DEPV
      REAL        PLDV_HONO                     ! PLDV for HONO
      REAL        CONC_NO2                      ! CONC of NO2
      REAL        DEPV_NO2                      ! Dep. Vel. of NO2
      REAL        DEPV_HNO3                     ! Dep. Vel. of HNO3
      REAL        FNL
      INTEGER     NLP, NL, LCBL
      INTEGER, SAVE :: NO2_HIT, HONO_HIT, HNO3_HIT, NO2_MAP, HONO_MAP, HNO3_MAP
      REAL        DTLIM, DTS, DTACM, RZ, DELC, LFAC1, LFAC2

      INTEGER, SAVE :: LOGDEV
 
      INTEGER      ALLOCSTAT
      INTEGER      C, R, L, S, V                ! loop induction variables
      INTEGER      MDATE, MTIME, MSTEP          ! internal simulation date&time
                                                ! array in vert. mixing
      INTERFACE
         SUBROUTINE RDMET( MDATE, MTIME, RDEPVHT, RJACM, RVJACMF, RRHOJ,
     &                     DENS1 )
            IMPLICIT NONE
            INTEGER,        INTENT( IN )  :: MDATE, MTIME
            REAL,           INTENT( OUT ) :: RDEPVHT( :,: )
            REAL,           INTENT( OUT ) :: RJACM  ( :,:,: )
            REAL,           INTENT( OUT ) :: RVJACMF( :,:,: )
            REAL,           INTENT( OUT ) :: RRHOJ  ( :,:,: )
            REAL,           INTENT( OUT ) :: DENS1  ( :,: )
         END SUBROUTINE RDMET
         SUBROUTINE PA_UPDATE_EMIS ( PNAME, VDEMIS, JDATE, JTIME, TSTEP )
            IMPLICIT NONE
            CHARACTER( * ), INTENT( IN )  :: PNAME
            REAL,           INTENT( IN )  :: VDEMIS( :,:,:,: )
            INTEGER,        INTENT( IN )  :: JDATE, JTIME
            INTEGER,        INTENT( IN )  :: TSTEP( 2 )
         END SUBROUTINE PA_UPDATE_EMIS
         SUBROUTINE PA_UPDATE_DDEP ( PNAME, DDEP, JDATE, JTIME, TSTEP )
            IMPLICIT NONE
            CHARACTER( * ), INTENT( IN )  :: PNAME
            REAL,           INTENT( IN )  :: DDEP( :,:,: )
            INTEGER,        INTENT( IN )  :: JDATE, JTIME
            INTEGER,        INTENT( IN )  :: TSTEP( 2 )
         END SUBROUTINE PA_UPDATE_DDEP
         SUBROUTINE CONV_CGRID ( CGRID, JDATE, JTIME, CNGRD )
            IMPLICIT NONE
            REAL, POINTER :: CGRID( :,:,:,: )
            INTEGER,        INTENT( IN )  :: JDATE, JTIME
            REAL,           INTENT( INOUT ) :: CNGRD( :,:,:,: )
         END SUBROUTINE CONV_CGRID
         SUBROUTINE REV_CGRID ( CNGRD, JDATE, JTIME, CGRID )
            IMPLICIT NONE
            REAL,           INTENT( INOUT ) :: CNGRD( :,:,:,: )
            INTEGER,        INTENT( IN )  :: JDATE, JTIME
            REAL, POINTER :: CGRID( :,:,:,: )
         END SUBROUTINE REV_CGRID
         SUBROUTINE EDDYX ( JDATE, JTIME, TSTEP,
     &                      EDDYV, HOL, XPBL, LPBL, CONVCT )
            IMPLICIT NONE
            INTEGER,        INTENT( IN )  :: JDATE, JTIME, TSTEP
            REAL,           INTENT( OUT ) :: EDDYV ( :,:,: )
            REAL,           INTENT( OUT ) :: HOL   ( :,: )
            REAL,           INTENT( OUT ) :: XPBL  ( :,: )
            INTEGER,        INTENT( OUT ) :: LPBL  ( :,: )
            LOGICAL,        INTENT( OUT ) :: CONVCT( :,: )
         END SUBROUTINE EDDYX
         SUBROUTINE MATRIX ( A, B, C, D, E, X )
            IMPLICIT NONE
            REAL,           INTENT( IN )  :: A( : ), B( : ), C( : ), E( : )
            REAL,           INTENT( IN )  :: D( :,: )
            REAL,           INTENT( OUT ) :: X( :,: )
         END SUBROUTINE MATRIX
         SUBROUTINE TRI ( L, D, U, B, X )
            IMPLICIT NONE
            REAL,           INTENT( IN )  :: L( : ), D( : ), U( : )
            REAL,           INTENT( IN )  :: B( :,: )
            REAL,           INTENT( OUT ) :: X( :,: )
         END SUBROUTINE TRI
      END INTERFACE

C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN

         FIRSTIME = .FALSE.
         LOGDEV = INIT3()

C set vertical layer definitions from COORD.EXT

         ALLOCATE ( DX3F( NLAYS ),
     &              RDX3F( NLAYS ),
     &              RDX3M( NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating DX3F, RDX3F or RDX3M'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         DO L = 1, NLAYS
            DX3F( L )  = X3FACE_GD( L ) - X3FACE_GD( L-1 )
            RDX3F( L ) = 1.0 / DX3F( L )
            X3M( L ) = 0.5 * ( X3FACE_GD( L ) + X3FACE_GD( L-1 ) )
         END DO

         DO L = 1, NLAYS - 1
            RDX3M( L ) = 1.0 / ( X3M( L+1 ) - X3M( L ) )
         END DO
         RDX3M( NLAYS ) = 0.0

         IF ( .NOT. DEPV_INIT ( JDATE, JTIME, TSTEP( 1 ), CGRID ) ) THEN
            XMSG = 'Failure initializing deposition velocities module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C create global maps

         IF ( .NOT. VDIFF_MAP_INIT( N_SPC_DEPV ) ) THEN
            XMSG = 'Failure initializing index mapping module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         HONO_HIT = 0; HNO3_HIT = 0; NO2_HIT  = 0
         HONO_MAP = 0; HNO3_MAP = 0; NO2_MAP  = 0
         DO V = 1, N_SPC_DEPV
            IF ( TRIM( DV2DF_SPC( V ) ) .EQ. 'NO2' ) THEN
               NO2_HIT = V
               NO2_MAP = DV2DF( V )
            ELSE IF ( TRIM( DV2DF_SPC( V ) ) .EQ. 'HONO' ) THEN
               HONO_HIT = V
               HONO_MAP = DV2DF( V )
            ELSE IF ( TRIM( DV2DF_SPC( V ) ) .EQ. 'HNO3' ) THEN
               HNO3_HIT = V
               HNO3_MAP = DV2DF( V )
            END IF
         END DO

C Open the met files

         CALL OPMET ( JDATE, JTIME, CONVPA )
 
C Set output file characteristics based on COORD.EXT and open the dry dep file

         IF ( MYPE .EQ. 0 ) CALL OPDDEP ( JDATE, JTIME, TSTEP( 1 ), N_SPC_DDEP )

C Allocate and initialize dry deposition array

         ALLOCATE ( DDEP( N_SPC_DEPV,MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating DDEP'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
            
         DDEP = 0.0

C set auxiliary depv arrays

         ALLOCATE ( DD_FAC( N_SPC_DEPV ),
     &              DDBF  ( N_SPC_DEPV ),
     &              DEPVCR( N_SPC_DEPV ),
     &              EFAC1 ( N_SPC_DEPV ),
     &              EFAC2 ( N_SPC_DEPV ),
     &              POL   ( N_SPC_DEPV ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating DD_FAC, DDBF, DEPVCR, EFAC1, EFAC2, or POL'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         IF ( .NOT. EMIS_INIT ( JDATE, JTIME, TSTEP( 1 ) ) ) THEN
            XMSG = 'Failure initializing emissions module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         IF ( LIPR ) THEN
            ALLOCATE ( EMIS_PA( MY_NCOLS,MY_NROWS,EMLAYS,N_SPC_EMIS+1 ),
     &                 STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'EMIS_PA memory allocation failed'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
            ALLOCATE ( DDEP_PA( MY_NCOLS,MY_NROWS,N_SPC_DEPV ),
     &                 STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'DDEP_PA memory allocation failed'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
         END IF

         N_SPC_CGRID = SIZE ( CGRID,4 )

         ALLOCATE ( CNGRD( N_SPC_CGRID,NLAYS,MY_NCOLS,MY_NROWS ),
     &              STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating CNGRD'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         CNGRD = 0.0   ! array assignment

         ALLOCATE ( CONC( N_SPC_DIFF,NLAYS),
     &              EMIS( N_SPC_DIFF,NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating CONC or EMIS'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         CONC = 0.0; EMIS = 0.0   ! array assignment

         ALLOCATE ( DD( N_SPC_DIFF,NLAYS ),
     &              UU( N_SPC_DIFF,NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating DD or UU'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         DD = 0.0; UU = 0.0   ! array assignment

      END IF          !  if Firstime

      MDATE = JDATE
      MTIME = JTIME
      MSTEP = TIME2SEC( TSTEP( 2 ) )
      DTSEC = FLOAT( MSTEP )
      CALL NEXTIME ( MDATE, MTIME, SEC2TIME( MSTEP / 2 ) )

C read & interpolate met data

      CALL RDMET ( MDATE, MTIME, RDEPVHT, RJACM, RVJACMF, RRHOJ, DENS1 )

C read & interpolate deposition velocities

      CALL GET_DEPV ( MDATE, MTIME, TSTEP, CGRID )

C read & interpolate emissions data => VDEMIS from EMIS_DEFN module

      CALL GET_EMIS ( MDATE, MTIME, TSTEP, CONVPA, RDX3F, RRHOJ, RJACM )

      IF ( LIPR ) THEN
         DO S = 1, N_SPC_EMIS+1
            DO L = 1, EMLAYS
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     EMIS_PA( C,R,L,S ) = VDEMIS( S,L,C,R )
                  END DO
               END DO
            END DO
         END DO
         CALL PA_UPDATE_EMIS ( 'VDIF', EMIS_PA, JDATE, JTIME, TSTEP )
      END IF

      CALL EDDYX ( JDATE, JTIME, TSTEP( 2 ),
     &             EDDYV, HOL, XPBL, LPBL, CONVCT )

C EDDYV returned = Kz, where Kz is in m**2/sec

      DO R = 1, MY_NROWS
         DO C = 1, MY_NCOLS
            DO L = 1, NLAYS
               SEDDY( L,C,R ) = EDDYV( C,R,L )
     &                        * RVJACMF( C,R,L ) * RDX3M( L ) ! * DELT
            END DO
         END DO
      END DO

      IF ( IFACM .EQ. 0 ) CONVCT = .FALSE.   ! no ACM

C Convert non-molar mixing ratio species and re-order CGRID

      CALL CONV_CGRID ( CGRID, MDATE, MTIME, CNGRD )

      IF ( WSTEP .EQ. 0 ) DDEP = 0.0

C ------------------------------------------- Row, Col LOOPS -----------

      DO 345 R = 1, MY_NROWS
      DO 344 C = 1, MY_NCOLS

C ACM insert

         DTLIM = DTSEC

C Note: DT has been moved from EDDY to here, dt = .75 dzf dzh / Kz

         DO L = 1, NLAYS - 1
            DTLIM = MIN( DTLIM, 0.75 / ( SEDDY( L,C,R ) * RDX3F( L ) ) )
         END DO
         MBARKS = 0.0   ! array assignment
         MDWN = 0.0     ! array assignment

C New couple ACM & EDDY ------------------------------------------------

         MBAR = 0.0
         FNL = 0.0

         IF ( CONVCT( C,R ) ) THEN   ! Do ACM for this column
            LCBL = LPBL( C,R )
            MEDDY = SEDDY( 1,C,R ) / ( XPBL( C,R ) - X3FACE_GD( 1 ) )
            FNL = 1.0 / ( 1.0 + ( ( KARMAN / ( -HOL( C,R ) ) ) ** 0.3333 )
     &                / ( 0.72 * KARMAN ) )

!           IF ( FNL .GT. 1.0 ) WRITE( LOGDEV,* ) ' FNL= ', FNL

            MBAR = MEDDY * FNL
            DO L = 1, LCBL - 1
               SEDDY( L,C,R ) = SEDDY( L,C,R  ) * ( 1.0 - FNL )
            END DO

            IF ( MBAR .LT. EPS ) THEN
               WRITE( LOGDEV,* ) ' EDDYV, MBAR, FNL, HOL = ',
     &                             EDDYV( C,R,1 ), MBAR, FNL, HOL( C,R )
               CONVCT( C,R ) = .FALSE.
               LCBL = 1
               XMSG = '*** ACM fails ***'
               CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT2 )
            END IF

            IF ( ( FNL .LE. 0.0 ) .OR.            ! never gonna happen for CONVCT
     &           ( LCBL .GE. NLAYS-1 ) .OR.       ! .GT. never gonna happen
     &           ( HOL( C,R ) .GT. -0.00001 ) )   ! never gonna happen
     &         WRITE( LOGDEV,1015 ) LCBL, MBAR, FNL, EDDYV( C,R,1 ),
     &                              SEDDY( 1,C,R ), HOL( C,R )
1015           FORMAT( ' LCBL, MBAR, FNL, SEDDY1, HOL:', I3, 1X, 5(1PE13.5) )

            DO L = 1, LCBL - 1
               MBARKS( L ) = MBAR
               MDWN( L ) = MBAR * ( XPBL( C,R ) - X3FACE_GD( L-1 ) )
     &                   * RDX3F( L )
            END DO

            MBARKS( LCBL ) = MBAR * ( XPBL( C,R ) - X3FACE_GD( LCBL-1 ) )
     &                     * RDX3F( LCBL )
            MDWN( LCBL ) = MBARKS( LCBL )

C Modify Timestep for ACM

            RZ     = ( X3FACE_GD( LCBL ) - X3FACE_GD( 1 ) ) * RDX3F( 1 )
            DTACM  = 1.0 / ( MBAR * RZ )
            DTLIM  = MIN( 0.75 * DTACM, DTLIM )
         ELSE
            LCBL = 1
         END IF

C-----------------------------------------------------------------------

         NLP = INT( DTSEC / DTLIM + 0.99 )
         DTS = DTSEC / NLP
         DTDENS1 = DTS * DENS1( C,R )

#ifdef Verbose
        IF ( R .EQ. MY_NROWS / 2 .AND. C .EQ. MY_NCOLS / 2 )
     &      WRITE( LOGDEV,1021 ) CONVCT( C,R ), DTS, EDDYV( C,R,1 ),
     &                           MBAR, FNL
1021        FORMAT( ' CONVCT, DTS, EDDYV, MBAR, FNL: ', L3, 1X, 4(1PE13.5) )
#endif

C End ACM insert

         DO L = 1, NLAYS
            DO V = 1, N_SPC_DIFF
               CONC( V,L ) = CNGRD( DIFF_MAP( V ),L,C,R )
            END DO
         END DO

         EMIS = 0.0
         DO L = 1, EMLAYS
            DO V = 1, N_SPC_DIFF
               EMIS( V,L ) = VDEMIS( DF2EM( V ),L,C,R ) * DTS
            END DO
         END DO

         DO L = 1, NLAYS
            EDDY( L ) = SEDDY( L,C,R )
         END DO

         DO V = 1, N_SPC_DEPV
            DDBF( V )   = DDEP( V,C,R )
            DEPVCR( V ) = DEPV( V,C,R )
            DD_FAC( V ) = DTDENS1 * DD_CONV( V ) * DEPVCR( V )
            EFAC1 ( V ) = EXP( -DEPVCR( V ) * RDEPVHT( C,R ) * THBAR * DTS )
            EFAC2 ( V ) = EXP( -DEPVCR( V ) * RDEPVHT( C,R ) * THETA * DTS )
            POL   ( V ) = PLDV( V,C,R ) / DEPVCR( V )
         END DO

         PLDV_HONO = PLDV( HONO_HIT,C,R )
         CONC_NO2 = CONC( NO2_MAP,1 )

C-----------------------------------------------------------------------

         DO 301 NL = 1, NLP      ! loop over sub time

C --------- HET HONO RX -----------------

            DO V = 1, N_SPC_DEPV

C For HONO, use special treatment
               IF ( V .EQ. HONO_HIT ) THEN
                  CONC( HONO_MAP,1 ) = POL( V ) + ( CONC( HONO_MAP,1 ) - POL( V ) )
     &                               * EFAC1( V )
                  DDBF( V ) = DDBF( V ) + THBAR * DD_FAC( V ) * CONC( HONO_MAP,1 )

C For HNO3, use special treatment
               ELSE IF ( V .EQ. HNO3_HIT ) THEN
                  CONC( HNO3_MAP,1 ) = POL( V ) + ( CONC( HNO3_MAP,1 ) - POL( V ) )
     &                               * EFAC1( V )

C HNO3 produced via the heterogeneous reaction sticks on the surfaces
C HNO3 is accounted as depositional loss; calculate increased deposition loss
                  DEPV_HNO3 = DEPVCR( V ) + PLDV_HONO / CONC_NO2
                  DD_FAC( V ) = DTDENS1 * DD_CONV( V ) * DEPV_HNO3
                  DDBF( V ) = DDBF( V ) + THBAR * DD_FAC( V ) * CONC( HNO3_MAP,1 )

C For NO2, use special treatment
               ELSE IF ( V .EQ. NO2_HIT ) THEN

C Loss of NO2 via the heterogeneous reaction is accounted for as an additional
C depositional loss. Add the loss of NO2 via the heterogeneous reaction
C to the regular deposition velocity (increased dep. vel.).  This will
C reduce the NO2 conc. in the atmosphere without affecting the depositional loss.

                  DEPV_NO2 = DEPVCR( V ) + 2.0 * PLDV_HONO / CONC_NO2
                  EFAC1 ( V ) = EXP( -DEPV_NO2 * RDEPVHT( C,R ) * THBAR * DTS )
                  EFAC2 ( V ) = EXP( -DEPV_NO2 * RDEPVHT( C,R ) * THETA * DTS )
                  POL   ( V ) = PLDV( V,C,R ) / DEPV_NO2
                  CONC( NO2_MAP,1 ) = POL( V ) + ( CONC( NO2_MAP,1 ) - POL( V ) )
     &                              * EFAC1( V )
                  DDBF( V ) = DDBF( V ) + THBAR * DD_FAC( V ) * CONC( NO2_MAP,1 )

C --------- END of HET HONO RX ----------

               ELSE

C For NH3, use bi-directional flux

                  CONC( DV2DF( V ),1 ) = POL( V ) + ( CONC( DV2DF( V ),1 ) - POL( V ) )
     &                                 * EFAC1( V )
                  DDBF( V ) = DDBF( V )
     &                      + THBAR * DD_FAC( V ) * ( CONC( DV2DF( V ),1 ) - POL( V ) )

               END IF

            END DO

C Init variables for use below

            DO L = 1, NLAYS
               AA( L ) = 0.0
               BB( L ) = 0.0
               CC( L ) = 0.0
               EE( L ) = 0.0
               DO V = 1, N_SPC_DIFF
                  DD( V,L ) = 0.0
                  UU( V,L ) = 0.0
               END DO
            END DO

C Compute tendency of CBL concentrations - semi-implicit solution
C Define arrays A,B,E which make up MATRIX and D which is RHS

            IF ( CONVCT( C,R ) ) THEN
               DO L = 2, LCBL
                  AA( L )   = -CRANKP * MBARKS( L ) * DTS
                  BB( L )   = 1.0 + CRANKP * MDWN( L ) * DTS
                  EE( L-1 ) = -CRANKP * MDWN( L ) * DTS * DX3F( L )
     &                      * RDX3F( L-1 )
                  MFAC = DX3F( L+1 ) * RDX3F( L ) * MDWN( L+1 )
                  DO V = 1, N_SPC_DIFF
                     DELC = DTS * ( MBARKS( L ) * CONC( V,1 )
     &                              - MDWN( L ) * CONC( V,L )
     &                              + MFAC      * CONC( V,L+1 ) )
                     DD( V,L ) = CONC( V,L ) + CRANKQ * DELC
                  END DO
               END DO
            END IF

            AA( 2 ) = AA( 2 ) - EDDY( 1 ) * CRANKP * RDX3F( 2 ) * DTS
            EE( 1 ) = EE( 1 ) - EDDY( 1 ) * CRANKP * RDX3F( 1 ) * DTS

            DO L = 2, NLAYS
               IF ( L .GT. LCBL ) THEN
                  BB( L ) = 1.0
                  DO V = 1, N_SPC_DIFF
                     DD( V,L ) = CONC( V,L )
                  END DO
               END IF
               XPLUS  = EDDY( L )   * RDX3F( L ) * DTS
               XMINUS = EDDY( L-1 ) * RDX3F( L ) * DTS
               BB( L ) = BB( L ) + ( XPLUS + XMINUS ) * CRANKP
               CC( L ) = - XMINUS * CRANKP
               EE( L ) = EE( L ) - XPLUS * CRANKP
               IF ( L .EQ. NLAYS ) THEN
                  DO V = 1, N_SPC_DIFF
                     DD( V,L ) = DD( V,L )
     &                         - CRANKQ * XMINUS
     &                         * ( CONC( V,L ) - CONC( V,L-1 ) )
                  END DO
               ELSE
                  LFAC1 = CRANKQ * XPLUS
                  LFAC2 = CRANKQ * XMINUS
                  DO V = 1, N_SPC_DIFF
                     DD( V,L ) = DD( V,L )
     &                         + LFAC1
     &                         * ( CONC( V,L+1 ) - CONC( V,L ) )
     &                         - LFAC2
     &                         * ( CONC( V,L ) - CONC( V,L-1 ) )
                     IF ( L .LE. EMLAYS ) DD( V,L ) = DD( V,L ) + EMIS( V,L )
                  END DO
               END IF
            END DO

            BB( 1 ) = 1.0
            DO V = 1, N_SPC_DIFF
               DD( V,1 ) = CONC( V,1 )
            END DO

            IF ( CONVCT( C,R ) ) THEN
               LFAC1 = ( XPBL( C,R ) - X3FACE_GD( 1 ) ) * RDX3F( 1 ) * DTS
               LFAC2 = CRANKQ * MDWN( 2 ) * DX3F( 2 )
     &               * RDX3F( 1 ) * DTS
               BB( 1 ) = BB( 1 ) + CRANKP * MBARKS( 1 ) * LFAC1
               LFAC1 = CRANKQ * MBARKS( 1 ) * LFAC1
               DO V = 1, N_SPC_DIFF
                  DD( V,1 ) = DD( V,1 )
     &                      - LFAC1 * CONC( V,1 )
     &                      + LFAC2 * CONC( V,2 ) ! net mixing above
               END DO
            END IF

            BB( 1 ) = BB( 1 ) + CRANKP * EDDY( 1 ) * RDX3F( 1 ) * DTS
            LFAC1 = CRANKQ * EDDY( 1 ) * RDX3F( 1 ) * DTS
            DO V = 1, N_SPC_DIFF
               DD( V,1 ) = DD( V,1 )
     &                   + LFAC1 * ( CONC( V,2 ) - CONC( V,1 ) )
     &                   + EMIS( V,1 )
            END DO

C Subroutine MATRIX then solves for U if ACM2, else TRI solves for U

            IF ( CONVCT( C,R ) ) THEN
               CALL MATRIX ( AA, BB, CC, DD, EE, UU )
            ELSE
               CALL TRI ( CC, BB, EE, DD, UU )
            END IF

C Load into CGRID
            DO L = 1, NLAYS
               DO V = 1, N_SPC_DIFF
                  CONC( V,L ) = UU( V,L )
               END DO
            END DO
            
            DO V = 1, N_SPC_DEPV

C --------- HET HONO RX -----------------

               IF ( V .EQ. HONO_HIT ) THEN

                  CONC( HONO_MAP,1 ) = POL( V ) + ( CONC( HONO_MAP,1 ) - POL( V ) )
     &                               * EFAC2( V )
                  DDBF( V ) = DDBF( V ) + THETA * DD_FAC( V ) * CONC( HONO_MAP,1 )

               ELSE IF ( V .EQ. HNO3_HIT ) THEN

                  CONC( HNO3_MAP,1 ) = POL( V ) + ( CONC( HNO3_MAP,1 ) - POL( V ) )
     &                               * EFAC2( V )
                  DDBF( V ) = DDBF( V ) + THETA * DD_FAC( V ) * CONC( HNO3_MAP,1 )

               ELSE IF ( V .EQ. NO2_HIT ) THEN

                  CONC( NO2_MAP,1 ) = POL( V ) + ( CONC( NO2_MAP,1 ) - POL( V ) )
     &                              * EFAC2( V )
                  DDBF( V ) = DDBF( V ) + THETA * DD_FAC( V ) * CONC( NO2_MAP,1 )

C --------- END of HET HONO RX ----------

               ELSE

C For NH3, use bi-directional flux

                  CONC( DV2DF( V ),1 ) = POL( V ) + ( CONC( DV2DF( V ),1 ) - POL( V ) )
     &                                 * EFAC2( V )
                  DDBF( V ) = DDBF( V )
     &                      + THETA * DD_FAC( V ) * ( CONC( DV2DF( V ),1 ) - POL( V ) )

               END IF

            END DO

301      CONTINUE                 ! end sub time loop

         DO L = 1, NLAYS
            DO V = 1, N_SPC_DIFF
               CNGRD( DIFF_MAP( V ),L,C,R ) = CONC( V,L )
            END DO
         END DO

         DO V = 1, N_SPC_DEPV
            DDEP( V,C,R ) = DDBF( V )
         END DO

344   CONTINUE         !  end loop on col C
345   CONTINUE         !  end loop on row R

C Revert non-molar mixing ratio species and re-order CGRID

      CALL REV_CGRID ( CNGRD, MDATE, MTIME, CGRID )

C If last call this hour:  write accumulated depositions:

      WSTEP = WSTEP + TIME2SEC( TSTEP( 2 ) )
      IF ( WSTEP .GE. TIME2SEC( TSTEP( 1 ) ) ) THEN
         MDATE = JDATE
         MTIME = JTIME
         CALL NEXTIME( MDATE, MTIME, TSTEP( 2 ) )
         WSTEP = 0

         DO V = 1, N_SPC_DDEP
            S = DD2DV( V )
            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  WRDD( C,R ) = DDEP( S,C,R )
               END DO
            END DO

            IF ( .NOT. WRITE3( CTM_DRY_DEP_1, DDEP_SPC( V ),
     &                 MDATE, MTIME, WRDD ) ) THEN
               XMSG = 'Could not write ' // CTM_DRY_DEP_1 // ' file'
               CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF

         END DO

         WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &         'Timestep written to', CTM_DRY_DEP_1,
     &         'for date and time', MDATE, MTIME

         IF ( LIPR ) THEN
            DO V = 1, N_SPC_DEPV
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     DDEP_PA( C,R,V ) = DDEP( V,C,R )
                  END DO
               END DO
            END DO
            CALL PA_UPDATE_DDEP ( 'VDIF', DDEP_PA, JDATE, JTIME, TSTEP )
         END IF

C re-set dry deposition array to zero

         DDEP = 0.0

      END IF

      RETURN
      END
