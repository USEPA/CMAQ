
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
      SUBROUTINE ZADV ( CGRID, JDATE, JTIME, TSTEP )

C-----------------------------------------------------------------------
C Function:
C   Advection in the vertical, x3-direction:
C   The process time step is set equal to TSTEP

C Preconditions:
C   Dates and times represented YYYYDDD:HHMMSS.
C   No "skipped" dates and times. Process time step divides TSTEP exactly
C   CGRID in transport units: SQRT{DET[metric tensor]}*concentration (Mass/Vol)

C Subroutines and functions called:
C   TIME2SEC

C Revision history:
C   02/19/93 by M. Talat Odman  at NCSC
C   05/17/93 by Carlie J. Coats at NCSC:  now uses INTERP3()
C   06/14/94 by Dongming Hwang at NCSC:
C              include statement and subroutine name template
C   10/15/95 by M. Talat Odman at NCSC: generalized coordinates

C   Sep 97 Jeff
C   Aug 98 Jeff better Courant condition tstep limit

C    David Wong, Sep. 1998
C      -- parallelized the code

C    15 Dec 00 J.Young: move CGRID_MAP into f90 module
C                       GLOBAL_RSUM -> Dave Wong's f90 stenex GLOBAL_SUM
C                       GLOBAL_ISUM -> Dave Wong's f90 stenex GLOBAL_SUM

C    28 Jul 01 J.Young: allocatable arrays ...
C                       Since F90 does not preserve dummy argument array
C                       indices, the 3rd dimension of WHAT has been changed
C                       from 0:NLAYS to 1:NLAYS+1 for the sake of vcontvel

C    03 Sep 01 David Wong
C      -- inserted F90 DEALLOCATE statement for NX3
C
C   1/03 - JP modified for Yamo mass conservation
C          Vertical velocity is diagnosed from mass continuity
C          vertical advection is upstream (no call to adv scheme)

C    31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                       domain specifications in one module
C    27 Apr 07 J.Young: Talat's First-order upstream (donor cell) algorithm
C    30 Apr 09 J.Pleim, J.Young: Replace donor cell with ppm, adjust velocity
C                                accordingly
C    21 Aug 09 J.Young: Don't bypass VPPMY if ITER = 0
C    18 Nov 09 J.Young: Combine VPPMY and VPPM functionality
C    21 Jun 10 J.Young: convert for Namelist redesign
C    16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C    11 May 11 D.Wong: incorporated twoway model implementation
C    31 Jul 12 J.Bash: Changed the zadv dt for cases where cc > 1 to be 
C                      more stable for conditions when cc > 10
C    01 Feb 19 D.Wong: Implemented centralized I/O approach, removed all MY_N
C                      clauses
C    09 Sep 19 F. Sidi: Replaced L_CONC_WVEL & L_ACONC_WVEL (depreciated) with W_VEL
C    11 Dec 19 S.L.Napelenok: ddm-3d implementation for version 5.3.1
C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE CGRID_SPCS            ! CGRID species number and offsets
      USE WVEL_DEFN             ! derived vertical velocity component
      USE UTILIO_DEFN

#ifdef isam
      USE SA_DEFN               ! 20120821
#endif

#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_GLOBAL_SUM_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_GLOBAL_SUM_MODULE)
#endif

#ifdef snl_timing
      USE TIMING
#endif
      USE CENTRALIZED_IO_MODULE, only : interpolate_var

#ifdef sens
      USE DDM3D_DEFN, ONLY: SENGRID, NPMAX, NP
#endif 

      IMPLICIT NONE

C Includes:

      INCLUDE SUBST_FILES_ID    ! file name parameters

C Arguments:

      REAL, POINTER :: CGRID( :,:,:,: )
      INTEGER     JDATE         ! current model date, coded YYYYDDD
      INTEGER     JTIME         ! current model time, coded HHMMSS
      INTEGER     TSTEP( 3 )    ! time step vector (HHMMSS)
                                ! TSTEP(1) = local output step
                                ! TSTEP(2) = sciproc sync. step (chem)
                                ! TSTEP(3) = twoway model time step w.r.t. wrf time
                                !            step and wrf/cmaq call frequency

C Parameters:

      INTEGER, PARAMETER :: MAXITER = 30     ! error exit limit

C Advected species dimension

      INTEGER, SAVE :: N_SPC_ADV

C File Variables:

      REAL        RHOJM1( NCOLS,NROWS,NLAYS ) ! RhoJ from Met file at start of tstep
      REAL        RHOJM2( NCOLS,NROWS,NLAYS ) ! RhoJ from Met file at end of tstep

C Local variables:

      CHARACTER( 16 ) :: PNAME = 'ZADVYPPM'
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
!     REAL         UHATJD( NCOLS+1,NROWS+1,NLAYS )  ! x1-component CX-velocity
!     REAL         VHATJD( NCOLS+1,NROWS+1,NLAYS )  ! x2-component CX-velocity

      INTEGER       MTIME, MDATE
!     REAL          CON1( NLAYS,N_SPC_ADV ) ! concentrations subset
      REAL, ALLOCATABLE, SAVE :: CON1( :,: ) ! concentrations subset
      REAL          VEL ( NLAYS+1 )        ! Velocities in a N-S column
      REAL          FLX ( NLAYS+1 )        ! upstream donor cell computed conc. flux
      REAL, ALLOCATABLE, SAVE :: DS ( : )  ! dx3 (dimensionless in sigma coord.)
      REAL          DTSEC                  ! sync time step in seconds
      REAL          DELT                   ! adjusted time step
      REAL          FLUX                   ! intermediate flux

!     INTEGER, SAVE :: ADV_MAP( N_SPC_ADV ) ! global adv map to CGRID
      INTEGER, ALLOCATABLE, SAVE :: ADV_MAP( : ) ! global adv map to CGRID

      INTEGER       COL, ROW, LVL, SPC, VAR ! loop counters
      INTEGER       A2C
      INTEGER       ITER

      CHARACTER( 96 ) :: XMSG = ' '
      REAL          RJ1( NLAYS )           ! local adjusted RHOJ
      REAL          RJ2( NLAYS )           ! local RHOJM at tstep + 1
      REAL          RJT( NLAYS )           ! local adjusted RHOJ
      REAL          DRJ, DUDX, DVDY
      REAL          DIVV( NLAYS )
      REAL          CC                     ! local Courant No.
      REAL          DTNEW                  ! sub timestep
      REAL          DSDT                   ! DS/DT
      REAL, ALLOCATABLE, SAVE :: FBLN( : ) ! blending function for upper layers
      INTEGER       ALLOCSTAT

#ifdef isam
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: NAME_ADV( : )
      REAL    :: SA_CON( NLAYS,N_SPCTAG )
#endif

#ifdef sens
      REAL, ALLOCATABLE, SAVE :: SEN1( :,:,: ) ! ( NLAYS,N_SPC_ADV,NPMAX ) sens subset
#endif

      INTERFACE
#ifdef isam
         SUBROUTINE VPPM ( NI, DT, DS, FLX, VEL, CON, SA_CON )
         USE SA_DEFN
#elif sens
         SUBROUTINE VPPM ( NI, DT, DS, FLX, VEL, CON, SEN1 )
#else
         SUBROUTINE VPPM ( NI, DT, DS, FLX, VEL, CON )
#endif
            INTEGER, INTENT( IN )    :: NI
            REAL,    INTENT( IN )    :: DT, DS( NI )
            REAL,    INTENT( IN )    :: FLX( NI+1 )
!           REAL,    INTENT( IN )    :: VEL( NI+1 )
            REAL,    INTENT( INOUT ) :: VEL( NI+1 )
            REAL,    INTENT( INOUT ) :: CON( :,: )
#ifdef isam
            REAL,    INTENT( INOUT ) :: SA_CON( NI,N_SPCTAG )
#elif sens
            REAL,    INTENT( INOUT ) :: SEN1( :,:,: )
#endif
         END SUBROUTINE VPPM
      END INTERFACE

C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.

         ALLOCATE ( DS( NLAYS ),FBLN( NLAYS ),STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating DS or FBLN'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         N_SPC_ADV = N_GC_TRNS + N_AE_TRNS + N_NR_TRNS + N_TR_ADV + 1
                                                  ! add 1 for advecting RHOJ
         ALLOCATE ( CON1( NLAYS,N_SPC_ADV ),
     &              ADV_MAP( N_SPC_ADV ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating CON1 or ADV_MAP'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

#ifdef isam
         ALLOCATE ( NAME_ADV( N_SPC_ADV ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating NAME_ADV'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
#endif

#ifdef sens
         ALLOCATE ( SEN1( NLAYS,N_SPC_ADV,NPMAX ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating SEN1'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
#endif

C Get default file header attibutes from MET_CRO_3D (assumes file already open)

C Get dx3 from the GRID_CONF(VGRD) F90 module
!         WRITE( LOGDEV,* ) ' '
!         WRITE( LOGDEV,* ) '    layer    S (X3FACE_GD) Delta S'
         DO LVL = 1, NLAYS
            DS ( LVL ) = ABS ( X3FACE_GD( LVL ) - X3FACE_GD( LVL-1 ) )
!           FBLN( LVL ) = 1.0 - 1.0 / ( 1.0 + EXP( 15.0 * ( X3FACE_GD( LVL ) - 0.5 ) ) )
            FBLN( LVL ) = 1.0
!            WRITE( LOGDEV,'(5X, I3, 3F14.7)' ) LVL, X3FACE_GD( LVL ),
!    &                                          DS( LVL ), FBLN(LVL)
         END DO
!         WRITE( LOGDEV,* ) ' '

C Create global map to CGRID
         SPC = 0
         DO VAR = 1, N_GC_TRNS
            SPC = SPC + 1
            ADV_MAP( SPC ) = GC_STRT - 1 + GC_TRNS_MAP( VAR )
#ifdef isam
            NAME_ADV( SPC ) = GC_TRNS( VAR )  ! KRT
#endif
         END DO
         DO VAR = 1, N_AE_TRNS
            SPC = SPC + 1
            ADV_MAP( SPC ) = AE_STRT - 1 + AE_TRNS_MAP( VAR )
#ifdef isam
            NAME_ADV( SPC ) = AE_TRNS( VAR )  ! KRT
#endif
         END DO
         DO VAR = 1, N_NR_TRNS
            SPC = SPC + 1
            ADV_MAP( SPC ) = NR_STRT - 1 + NR_TRNS_MAP( VAR )
#ifdef isam
            NAME_ADV( SPC ) = NR_TRNS( VAR )  ! KRT
#endif
         END DO
         DO VAR = 1, N_TR_ADV
            SPC = SPC + 1
            ADV_MAP( SPC ) = TR_STRT - 1 + TR_ADV_MAP( VAR )
#ifdef isam
            NAME_ADV( SPC ) = TR_ADV( VAR )  ! KRT
#endif
         END DO
 
         ADV_MAP( N_SPC_ADV ) = RHOJ_LOC

      END IF                    ! if firstime

C Time-stepped gridded computation for Z-direction advection.
      DTSEC  = FLOAT( TIME2SEC( TSTEP( 2 ) ) ) ! process time step (seconds)

C vertical velocities are at face centers, positive upward.
C No boundary conditions are needed because VEL(1) = VEL(NLAYS+1) = 0

C Get rho*J at start of sync step
      MDATE = JDATE
      MTIME = JTIME
#ifdef snl_timing
      call start_timing( zadv_int, read_int, 1 )
#endif
      call interpolate_var ('DENSA_J', mdate, mtime, RHOJM1)

#ifdef snl_timing
      call stop_timing( zadv_int, read_int )
#endif

C Get rho*J at end of sync step
      CALL NEXTIME( MDATE, MTIME, TSTEP( 2 ) )
#ifdef snl_timing
      call start_timing( zadv_int, read_int, 1 )
#endif
      call interpolate_var ('DENSA_J', mdate, mtime, RHOJM2)

#ifdef snl_timing
      call stop_timing( zadv_int, read_int )
#endif

      DO 333 ROW = 1, NROWS
         DO 222 COL = 1, NCOLS

            DO SPC = 1, N_SPC_ADV
               A2C = ADV_MAP( SPC )
               DO LVL = 1, NLAYS
                  CON1( LVL,SPC ) = CGRID( COL,ROW,LVL,A2C )
#ifdef sens
                  DO NP = 1, NPMAX
                     SEN1 ( LVL,SPC,NP ) = SENGRID( COL,ROW,LVL,NP,A2C )
                  END DO
#endif
               END DO
            END DO

            DO LVL = 1, NLAYS
               RJ1( LVL ) = RHOJM1( COL,ROW,LVL )
               RJ2( LVL ) = RHOJM2( COL,ROW,LVL )
            END DO

            ITER = 0
            DELT = DTSEC
            VEL( 1 ) = 0.0   ! impermeable boundary condition at the surface
            FLX( 1 ) = 0.0
            DRJ = 0.0
            DO LVL = 1, NLAYS
               DSDT = DS( LVL ) / DELT              ! initial for this col/row
               RJT( LVL ) = CON1( LVL,N_SPC_ADV )   ! initial for this col/row
!              DUDX = ( UHATJD( COL+1,ROW,LVL ) - UHATJD( COL,ROW,LVL ) ) / XCELL_GD
!              DVDY = ( VHATJD( COL,ROW+1,LVL ) - VHATJD( COL,ROW,LVL ) ) / YCELL_GD
!              DIVV( LVL ) = DUDX * DS( LVL ) + DVDY * DS( LVL )
               DIVV( LVL ) = ( RJ1( LVL ) - RJT( LVL ) ) * DSDT
               DRJ = DRJ - DIVV( LVL )
            END DO

#ifdef isam
Ckrt...import isam array into sa_con
            DO SPC = 1, N_SPCTAG
               DO LVL = 1, NLAYS
                  SA_CON( LVL,SPC ) = ISAM( COL,ROW,LVL,S_SPCTAG( SPC ),T_SPCTAG( SPC ) )
               END DO
            END DO
#endif
                        
111         CONTINUE   ! iteration loop if CC > 1

            FLUX = 0.0
            DO LVL = 1, NLAYS
               RJT( LVL ) = CON1( LVL,N_SPC_ADV )
!----Yamo part
               DSDT = DS( LVL ) / DELT
               FLUX = FLUX - DSDT * ( RJ2( LVL ) - RJT( LVL ) )
               FLX( LVL+1 ) = FLX( LVL ) - DS( LVL ) * DRJ - DIVV( LVL )
               FLX( LVL+1 ) = FBLN( LVL ) * FLX( LVL+1 ) + ( 1.0 - FBLN( LVL ) ) * FLUX
               FLUX = FLX( LVL+1 )
            END DO

            DO LVL = 2, NLAYS
               IF ( FLX( LVL ) .GE. 0.0 ) THEN
                  VEL( LVL ) = FLX( LVL ) / RJT( LVL-1 )
               ELSE
                  VEL( LVL ) = FLX( LVL ) / RJT( LVL )
               END IF
            END DO

            VEL( NLAYS+1 ) = FLX( NLAYS+1 ) / RJT( NLAYS )

C Find Maximum Courant Number

            CC = 0.0
            DTNEW = DELT

            DO LVL = 2, NLAYS
               IF ( VEL( LVL ) .GT. 0.0 ) THEN
                  CC = MAX ( CC, ( VEL( LVL ) * DELT / DS( LVL-1 ) ) )
                  DTNEW = MIN( DTNEW, 0.9 * DELT / CC )
               ELSE
                  CC = MAX ( CC, ( -VEL( LVL ) * DELT / DS( LVL ) ) )
   !              DTNEW = MIN( DTNEW, DELT / MAX( CC, 0.9 ) )
                  DTNEW = MIN( DTNEW, 0.9 * DELT / MAX( CC, 1.0 ) ) ! MAX in case vel = 0
               END IF
            END DO

            LVL = NLAYS+1
            IF ( VEL( LVL ) .GT. 0.0 ) THEN
               CC = MAX ( CC, ( VEL( LVL ) * DELT / DS( LVL-1 ) ) )
               DTNEW = MIN( DTNEW, 0.9 * DELT / CC )
            ELSE
               CC = MAX ( CC, ( -VEL( LVL ) * DELT / DS( LVL-1 ) ) )
   !           DTNEW = MIN( DTNEW, DELT / MAX( CC, 0.9 ) )
               DTNEW = MIN( DTNEW, 0.9 * DELT / MAX( CC, 1.0 ) ) ! MAX in case vel = 0
            END IF

            IF ( CC .GT. 1.0 ) THEN ! courant number is larger than unity

C Calculate a sub-time step that satisfies the Courant stability limit.
C Perform vertical advection with the computed velocity and sub-time step.
C Then calculate the difference between the original and sub-time steps.
C The difference is the new sub-time step. Recompute vertical velocities
C that would bring the air density field back to being uniform. Note that
C if Courant number with the new velocity and sub-time step is larger than
C unity again, then the last sub-time step would be split into further
C sub-steps.

               DTNEW = MAX( DTNEW, 1.0 )       

#ifdef isam
               CALL VPPM ( NLAYS, DTNEW, DS, FLX, VEL, CON1, SA_CON )
#elif sens
               CALL VPPM ( NLAYS, DTNEW, DS, FLX, VEL, CON1, SEN1 )
#else
               CALL VPPM ( NLAYS, DTNEW, DS, FLX, VEL, CON1 )
#endif
               DELT = DELT - DTNEW

               ITER = ITER + 1
               IF ( ITER .GT. MAXITER ) THEN
                  WRITE( LOGDEV,2005 ) COL, ROW, CC, DELT, ITER, JTIME
2005              FORMAT( 'zadv col  row     CC', 8X, 'dt    iter   jtime'
     &                    / 'zzzz', 2I4, 1PE12.3, 0PF10.5, 1X, I4, I10.6
     &                    / 10X, 'MetRhoj', 3X, 'TrRhoj', 5X, 'Diff',
     &                       4X, 'adv_rhoj', 3X, 'vel(l)', 6X, 'vel(l+1)' )
                  DO LVL = 1, NLAYS
                     WRITE( LOGDEV,2009 ) LVL, RJ2( LVL ), RJT( LVL ),
     &                                    RJ2( LVL ) - RJT( LVL ),
     &                                    CON1( LVL,N_SPC_ADV ),
     &                                    VEL( LVL ), VEL( LVL+1 )
                  END DO
2009              FORMAT( 'zzz2', I3, 4F10.2, 2(1PE12.3) )
                  WRITE( XMSG,2013 ) JTIME, TSTEP( 2 ), MAXITER
2013              FORMAT( 'vert adv soln failed at', I7.6,  ' with adv step:',
     &                     I7.6, ' HHMMSS', 2X, 'Max Iterations =', I3 )
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
               END IF

               GO TO 111

            END IF

#ifdef isam
            CALL VPPM ( NLAYS, DELT, DS, FLX, VEL, CON1, SA_CON )
#elif sens
            CALL VPPM ( NLAYS, DELT, DS, FLX, VEL, CON1, SEN1 )
#else
            CALL VPPM ( NLAYS, DELT, DS, FLX, VEL, CON1 )
#endif
            DO SPC = 1, N_SPC_ADV
               A2C = ADV_MAP( SPC )
               DO LVL = 1, NLAYS
                  CGRID( COL,ROW,LVL,A2C ) = CON1( LVL,SPC )
#ifdef sens
                  DO NP = 1, NPMAX
                     SENGRID( COL,ROW,LVL,NP,A2C ) = SEN1 ( LVL,SPC,NP )
                  END DO
#endif
               END DO
            END DO

#ifdef isam
Ckrt...update ISAM with SA_CON....20120821
            DO SPC = 1, N_SPCTAG
               IF( TRANSPORT_SPC( SPC ) )THEN
                  DO LVL = 1, NLAYS
                     ISAM( COL,ROW,LVL,S_SPCTAG( SPC ),T_SPCTAG( SPC ) ) = SA_CON( LVL,SPC )
                  END DO
               END IF   
            END DO
#endif
            IF ( W_VEL ) THEN ! If the user wants vertical velocity written to file, fill in global scope array (WY)
              DO LVL = 1, NLAYS
                 WY( LVL,COL,ROW ) = VEL( LVL+1 )
              END DO
            END IF 

222         CONTINUE   ! COL
333      CONTINUE   ! ROW

      IF ( W_VEL ) CALL GET_WVEL( JDATE,JTIME )

      RETURN
      END SUBROUTINE ZADV
