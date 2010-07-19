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
C $Header: /project/yoj/arc/CCTM/src/init/yamo/opconc.F,v 1.1 2010/07/19 18:09:42 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE OPCONC ( CGRID, JDATE, JTIME, TSTEP )

C-----------------------------------------------------------------------
C Function:
C   Create the IO/API netCDF header and open the output CONC file

C Revision history:
C   Jeff - Dec 00 - split out from initscen.F
C                 - move CGRID_MAP into f90 module
C   Jeff - Feb 01 - assumed shape arrays
C   30 Mar 01 J.Young: dyn alloc - Use HGRD_DEFN
C
C    3 Sep 01 David Wong
C     -- let PE 0 open CTM_CONC_1 as new and later on let the rest open
C        it for read and write
C     -- put an explicit barrier before opening a new netCDF file to avoid
C        NCOPEN error
C    7 May 03 J.Young: open and close conc file in processor 0; spin-wait to
C                      deal with nfs network latency for mpich cluster
C   28 Aug 03 J.Young: following Zion Wang at CERT, remove the spin-wait and
C                      simplify opening and closing CONC file (see initscen)
C   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module
C   30 May 05 J.Young: optional save derived vert. vel. to conc file
C   21 Jan 06 J.Young: add subset non-reactives capability with NR_CONC.EXT
C                      and subset layers saved to conc file; no longer save
C                      RHOJ to conc file
C   22 Aug 06 J.Young: enable capturing IC's as 0 timestep
C   11 Jun 08 J.Young: clean up
C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE WVEL_DEFN             ! derived vertical velocity component
      USE STD_CONC              ! standard CONC

      USE SUBST_MODULES         ! stenex
!     USE SUBST_UTIL_MODULE     ! stenex

      IMPLICIT NONE

      INCLUDE SUBST_IOPARMS     ! I/O parameters definitions
      INCLUDE SUBST_IOFDESC     ! file header data structure
#include      SUBST_IODECL      # I/O definitions and declarations
      INCLUDE SUBST_FILES_ID    ! file name parameters

C Arguments:

      REAL, POINTER :: CGRID( :,:,:,: )  ! for initial CONC
      INTEGER      JDATE        ! starting date (YYYYDDD)
      INTEGER      JTIME        ! starting time (HHMMSS)
      INTEGER      TSTEP        ! output timestep (HHMMSS)

C External Functions (not already declared by IODECL3.EXT):

      INTEGER, EXTERNAL :: TRIMLEN      !  string length, excl. trailing blanks
      INTEGER, EXTERNAL :: ENVINT       !  get environment variable as integer
      INTEGER      STATUS               !  ENVINT status

C Local Variables:

      REAL, ALLOCATABLE :: DBUFF( :,:,: )
      INTEGER      ALLOCSTAT

      CHARACTER( 16 ) :: PNAME = 'OPCONC'
      CHARACTER( 96 ) :: XMSG = ' '
      CHARACTER( 28 ) :: SSTR = ' species saved to CONC file:'

C environment variable for no. of layers from bottom to save on CONC file
      CHARACTER( 16 ) :: NLAYS_CONC = 'NLAYS_CONC'

C  environment variable description
      CHARACTER( 80 ) :: VARDESC

      INTEGER      LOGDEV

      INTEGER      K, KD, L, SPC, V   ! loop counters
      INTEGER      STRT, FINI         ! loop counters
      INTEGER      INDX

C-----------------------------------------------------------------------

      LOGDEV = INIT3()

C Set output file characteristics based on COORD.EXT and open it

      FTYPE3D = GRDDED3
      SDATE3D = JDATE
      STIME3D = JTIME
      TSTEP3D = TSTEP
      NVARS3D = N_CSPCS
      IF ( W_VEL ) NVARS3D = NVARS3D + 1   ! for W_YAMO
      NCOLS3D = GL_NCOLS
      NROWS3D = GL_NROWS
      NLAYS3D = C_NLAYS
      NTHIK3D = 1
      GDTYP3D = GDTYP_GD
      P_ALP3D = P_ALP_GD
      P_BET3D = P_BET_GD 
      P_GAM3D = P_GAM_GD
      XORIG3D = XORIG_GD
      YORIG3D = YORIG_GD
      XCENT3D = XCENT_GD
      YCENT3D = YCENT_GD
      XCELL3D = XCELL_GD
      YCELL3D = YCELL_GD
      VGTYP3D = VGTYP_GD
      VGTOP3D = VGTOP_GD
!     VGTPUN3D = VGTPUN_GD ! currently, not defined
      DO L = 1, NLAYS3D + 1
         VGLVS3D( L ) = VGLVS_GD( L )
      END DO
      GDNAM3D = GRID_NAME  ! from HGRD_DEFN

      FDESC3D( 1 ) = 'Concentration file output'
      FDESC3D( 2 ) = 'From CMAQ model dyn alloc version CTM'
      FDESC3D( 3 ) = 'Set of variables (possibly) reduced from CGRID'
      FDESC3D( 4 ) = 'For next scenario continuation runs,'
      FDESC3D( 5 ) = 'use the "one-step" CGRID file'
      KD = 5
      V = CONC_BLEV
      L = 0
      DO K = KD + 1, MIN ( C_NLAYS + KD, MXDESC3 )
         L = L + 1
         WRITE( FDESC3D( K ),'( "Layer", I3, " to", I3, " " )' )
     &   V + L - 1, L
      END DO
      IF ( ( KD + 1 + L ) .LT. MXDESC3 ) THEN
         DO K = KD + 1 + L, MXDESC3
            FDESC3D( K ) = ' '
         END DO
      END IF

      WRITE( LOGDEV,* ) ' '
      WRITE( LOGDEV,* ) '      Conc File Header Description:'
      DO K = 1, KD + L
         WRITE( LOGDEV,* ) '    => ',
     &   FDESC3D( K )( 1:TRIMLEN( FDESC3D( K ) ) )
      END DO

      V = 0
      STRT = 1
      FINI = N_C_GC_SPC
      DO SPC = STRT, FINI
         V = V + 1
         VTYPE3D( SPC ) = M3REAL
         VNAME3D( SPC ) = C_GC_SPC( V )
         UNITS3D( SPC ) = 'ppmV'
         VDESC3D( SPC ) = 'Variable ' // VNAME3D( SPC )
      END DO

      V = 0
      STRT = FINI + 1
      FINI = FINI + N_C_AE_SPC
      DO SPC = STRT, FINI
         V = V + 1
         VTYPE3D( SPC ) = M3REAL
         VNAME3D( SPC ) = C_AE_SPC( V )   ! from STD_CONC module
         IF ( VNAME3D( SPC )(1:3) .EQ. 'NUM' ) THEN
            UNITS3D( SPC ) = 'number/m**3'
         ELSE IF ( VNAME3D( SPC )(1:3) .EQ. 'SRF' ) THEN
            UNITS3D( SPC ) = 'm**2/m**3'
         ELSE
            UNITS3D( SPC ) = 'micrograms/m**3'
         END IF
         VDESC3D( SPC ) = 'Variable ' // VNAME3D( SPC )
      END DO

      V = 0
      STRT = FINI + 1
      FINI = FINI + N_C_NR_SPC
      DO SPC = STRT, FINI
         V = V + 1
         VTYPE3D( SPC ) = M3REAL
         VNAME3D( SPC ) = C_NR_SPC( V )   ! from STD_CONC module
         UNITS3D( SPC ) = 'ppmV'
         VDESC3D( SPC ) = 'Variable ' // VNAME3D( SPC )
      END DO

      V = 0
      STRT = FINI + 1
      FINI = FINI + N_C_TR_SPC ! write all TR species
      DO SPC = STRT, FINI
         V = V + 1
         VTYPE3D( SPC ) = M3REAL
         VNAME3D( SPC ) = C_TR_SPC( V )   ! from STD_CONC module
         UNITS3D( SPC ) = 'ppmV'
         VDESC3D( SPC ) = 'Variable ' // VNAME3D( SPC )
      END DO

      IF ( W_VEL ) THEN   ! for W_YAMO
         SPC = FINI + 1 
         VTYPE3D( SPC ) = M3REAL
         VNAME3D( SPC ) = 'W_VEL'
         UNITS3D( SPC ) = 'm/s'
         VDESC3D( SPC ) = 'Derived vertical velocity component'
      END IF

C create header

      IF ( MYPE .EQ. 0 ) THEN   ! open new
         IF ( .NOT. OPEN3( CTM_CONC_1, FSNEW3, PNAME ) ) THEN
            XMSG = 'Could not open ' // CTM_CONC_1 // ' file'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
      END IF

      CALL SUBST_BARRIER

C write the initial concentrations as step 0 on the conc file
C (inital data assumed to be in correct output units)

      ALLOCATE ( DBUFF( MY_NCOLS,MY_NROWS,C_NLAYS ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating DBUFF'
         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

      WRITE( LOGDEV,* ) ' '

      V = 0

      IF ( N_C_GC_SPC .NE. 0 ) WRITE( LOGDEV,'( /5X, "Gas Chem", A )' ) SSTR

      DO SPC = 1, N_C_GC_SPC
         V = V + 1
         INDX = CONC_MAP( V )

         DBUFF = CGRID( 1:MY_NCOLS,1:MY_NROWS,CONC_BLEV:CONC_ELEV,INDX )

         IF ( .NOT. WRITE3( CTM_CONC_1, C_GC_SPC( SPC ),
     &                      JDATE, JTIME, DBUFF ) ) THEN
            XMSG = 'Could not write ' //
     &             C_GC_SPC( SPC )(1:TRIMLEN( C_GC_SPC( SPC ) ) ) //
     &             ' to ' // CTM_CONC_1
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         WRITE( LOGDEV,'( 5X, I4, " (", I3, ") ", A )' )
     &                SPC, V, C_GC_SPC( SPC )(1:TRIMLEN( C_GC_SPC( SPC ) ) )

      END DO

      IF ( N_C_AE_SPC .NE. 0 ) WRITE( LOGDEV,'( /5X, "Aerosol", A )' ) SSTR

      DO SPC = 1, N_C_AE_SPC
         V = V + 1
         INDX = CONC_MAP( V )

         DBUFF = CGRID( 1:MY_NCOLS,1:MY_NROWS,CONC_BLEV:CONC_ELEV,INDX )

         IF ( .NOT. WRITE3( CTM_CONC_1, C_AE_SPC( SPC ),
     &                      JDATE, JTIME, DBUFF ) ) THEN
            XMSG = 'Could not write ' //
     &             C_AE_SPC( SPC )(1:TRIMLEN( C_AE_SPC( SPC ) ) ) //
     &             ' to ' // CTM_CONC_1
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         WRITE( LOGDEV,'( 5X, I4, " (", I3, ") ", A )' )
     &                SPC, V, C_AE_SPC( SPC )(1:TRIMLEN( C_AE_SPC( SPC ) ) )

      END DO

      IF ( N_C_NR_SPC .NE. 0 ) WRITE( LOGDEV,'( /5X, "Non-reactive", A )' ) SSTR

      DO SPC = 1, N_C_NR_SPC
         V = V + 1
         INDX = CONC_MAP( V )

         DBUFF = CGRID( 1:MY_NCOLS,1:MY_NROWS,CONC_BLEV:CONC_ELEV,INDX )

         IF ( .NOT. WRITE3( CTM_CONC_1, C_NR_SPC( SPC ),
     &                      JDATE, JTIME, DBUFF ) ) THEN
            XMSG = 'Could not write ' //
     &             C_NR_SPC( SPC )(1:TRIMLEN( C_NR_SPC( SPC ) ) ) //
     &             ' to ' // CTM_CONC_1
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         WRITE( LOGDEV,'( 5X, I4, " (", I3, ") ", A )' )
     &                SPC, V, C_NR_SPC( SPC )(1:TRIMLEN( C_NR_SPC( SPC ) ) )
      END DO

      IF ( N_C_TR_SPC .NE. 0 ) WRITE( LOGDEV,'( /5X, "Inert tracer", A )' ) SSTR

      DO SPC = 1, N_C_TR_SPC
         V = V + 1
         INDX = CONC_MAP( V )

         DBUFF = CGRID( 1:MY_NCOLS,1:MY_NROWS,CONC_BLEV:CONC_ELEV,INDX )

         IF ( .NOT. WRITE3( CTM_CONC_1, C_TR_SPC( SPC ),
     &                      JDATE, JTIME, DBUFF ) ) THEN
            XMSG = 'Could not write ' //
     &             C_TR_SPC( SPC )(1:TRIMLEN( C_TR_SPC( SPC ) ) ) //
     &             ' to ' // CTM_CONC_1
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         WRITE( LOGDEV,'( 5X, I4, " (", I3, ") ", A )' )
     &                SPC, V, C_TR_SPC( SPC )(1:TRIMLEN( C_TR_SPC( SPC ) ) )

      END DO

      IF ( W_VEL ) THEN   ! for W_YAMO

         WRITE( LOGDEV,'( /5X, "Derived Vert Vel Comp", A )' ) SSTR // ' W_VEL'

         DBUFF = 0.0

         IF ( .NOT. WRITE3( CTM_CONC_1, 'W_VEL',
     &                      JDATE, JTIME, DBUFF ) ) THEN
            XMSG = 'Could not write W_VEL to ' // CTM_CONC_1
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

      END IF

      DEALLOCATE ( DBUFF )

      WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &  'Timestep written to', CTM_CONC_1,
     &  'for date and time', JDATE, JTIME
      WRITE( LOGDEV, '(  5X,  A, 1X, I8, ":", I6.6 )' )
     &  'from timestep on initial data files for date and time',
     &   JDATE, JTIME

      RETURN
      END
