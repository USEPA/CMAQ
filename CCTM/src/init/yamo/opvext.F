
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

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/init/yamo/opconc.F,v 1.4 2011/10/21 16:11:24 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE OPVEXT ( JDATE, JTIME, TSTEP )

C-----------------------------------------------------------------------
C Function:
C   Create the IO/API netCDF header and open the output VEXT file

C Revision history:
C   24 Nov 17 B.Henderson: coped from OPCONC.
C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE STD_CONC              ! standard CONC
      USE UTILIO_DEFN
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE)
#endif
      USE MODGCTP, only : XY2XY

      IMPLICIT NONE

      INCLUDE SUBST_FILES_ID    ! file name parameters

C Arguments:

      INTEGER      JDATE        ! starting date (YYYYDDD)
      INTEGER      JTIME        ! starting time (HHMMSS)
      INTEGER      TSTEP        ! output timestep (HHMMSS)

C Local Variables:

      CHARACTER( 16 ) :: PNAME = 'OPVEXT'
      CHARACTER( 96 ) :: XMSG = ' '
      CHARACTER( 28 ) :: SSTR = ' species saved to VEXTfile:'

C environment variable for no. of layers from bottom to save on VEXT file
      CHARACTER( 16 ) :: NLAYS_VEXT = 'NLAYS_VEXT'

C  environment variable description
      CHARACTER( 80 ) :: VARDESC

      INTEGER      LOGDEV

      INTEGER      K, KD, L, SPC, V   ! loop counters
      INTEGER      STRT, FINI         ! loop counters
      INTEGER      INDX
      INTEGER      NVERTEXTLON, NVERTEXTLAT
      INTEGER      STATUS
      INTEGER :: NMAX = 1000000
      INTEGER :: GDTYP2 = 1
      REAL*8, ALLOCATABLE :: VERTEXTLATTMP(:), VERTEXTLONTMP(:)
      !REAL*8, ALLOCATABLE :: VERTEXTLAT(:), VERTEXTLON(:)
      !REAL  , ALLOCATABLE :: VERTEXTJ(:), VERTEXTI(:)
      !INTEGER, ALLOCATABLE :: VERTEXTIDX(:)

      LOGICAL, EXTERNAL :: FLUSH3
      CHARACTER( 16 ) :: DOVERTNAME = 'DOVERTEXT'
      CHARACTER( 16 ) :: VERTEXTLATNAME = 'VERTEXTLAT'
      CHARACTER( 16 ) :: VERTEXTLONNAME = 'VERTEXTLON'
      LOGICAL, SAVE :: DOVEXT = .true.
C-----------------------------------------------------------------------

      LOGDEV = INIT3()
      VARDESC = 'Flag to perform vert extract'
      DOVEXT = ENVYN( DOVERTNAME, VARDESC, .false., STATUS )
      IF ( STATUS .EQ. 1 ) THEN
         XMSG = TRIM(VARDESC) //
     & ' Environment variable improperly formatted'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      ELSE IF ( STATUS .NE. 0 ) THEN
         WRITE( LOGDEV, '(5X, A)' ) VARDESC
         XMSG = '... Using default:'
         WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, DOVEXT
      END IF
      IF (.not.DOVEXT) RETURN
      VARDESC = 'Maximum number of vertices'
      NMAX = ENVINT( "MAXVERT         ", VARDESC, NMAX, STATUS )
      IF ( STATUS .EQ. 1 ) THEN
         XMSG = TRIM(VARDESC) //
     & ' Environment variable improperly formatted'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      ELSE IF ( STATUS .NE. 0 ) THEN
         WRITE( LOGDEV, '(5X, A)' ) VARDESC
         XMSG = '... Using default:'
         WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, NMAX
      END IF
      ALLOCATE ( VERTEXTLATTMP(NMAX), VERTEXTLONTMP(NMAX), STAT = STATUS)
      VARDESC = 'List of latitudes'
      IF (.not. DBLLIST( VERTEXTLATNAME, VARDESC,
     & NMAX, NVERTEXTLAT, VERTEXTLATTMP)) THEN
         XMSG = TRIM(VARDESC) //
     & ' Environment variable improperly formatted'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      ENDIF
      IF (NMAX .eq. NVERTEXTLAT) THEN
         XMSG = 'NMAX == NVERTEXTLAT increase NMAX by at least 1'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      ENDIF
      VARDESC = 'List of longitudes'
      IF (.not. DBLLIST( VERTEXTLONNAME, VARDESC,
     & NMAX, NVERTEXTLON, VERTEXTLONTMP)) THEN
         XMSG = TRIM(VARDESC) //
     & ' Environment variable improperly formatted'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      ENDIF
      IF (NMAX .eq. NVERTEXTLON) THEN
         XMSG = 'NMAX == NVERTEXTLON increase NMAX by at least 1'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      ENDIF
      IF (.not. (NVERTEXTLON .eq. NVERTEXTLAT)) THEN
         XMSG = 'Mismatched number of lon/lat for VERTEXT'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      ENDIF
      NVERTEXT = NVERTEXTLON
      ALLOCATE ( VERTEXTLAT(NVERTEXT), VERTEXTLON(NVERTEXT), STAT = STATUS)
      ALLOCATE ( VERTEXTJ(NVERTEXT), VERTEXTI(NVERTEXT), STAT = STATUS)
      ALLOCATE ( VERTEXTJI(NVERTEXT), VERTEXTII(NVERTEXT), STAT = STATUS)
      ALLOCATE ( VERTEXTIDX(NVERTEXT), STAT = STATUS)
      WRITE(LOGDEV, *)'IN', NVERTEXT
      DO V=1,NVERTEXT
          VERTEXTLAT(V) = VERTEXTLATTMP(V)
          VERTEXTLON(V) = VERTEXTLONTMP(V)
      END DO
C Set output file characteristics based on COORD.EXT and open it

      FTYPE3D = GRDDED3
      SDATE3D = JDATE
      STIME3D = JTIME + TSTEP
      TSTEP3D = TSTEP
      NVARS3D = N_CSPCS
      NCOLS3D = 1
      NROWS3D = NVERTEXT
      NLAYS3D = NLAYS
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
      L = 0
      DO K = KD + 1, MIN ( NLAYS + KD, MXDESC3 )
         L = L + 1
         WRITE( FDESC3D( K ),'( "Layer", I3, " to", I3, " " )' )
     &   L - 1, L
      END DO
      IF ( ( KD + 1 + L ) .LT. MXDESC3 ) THEN
         DO K = KD + 1 + L, MXDESC3
            FDESC3D( K ) = ' '
         END DO
      END IF
      CALL XY2XY( GDTYP3D, P_ALP3D, P_BET3D, P_GAM3D, XCENT3D, YCENT3D,
     &            1      ,    0.d0,    0.d0,    0.d0,    0.d0,    0.d0,
     &            NVERTEXT, VERTEXTLON, VERTEXTLAT,VERTEXTI,VERTEXTJ )
      STATUS = 0
      DO V=1,NVERTEXT
        VERTEXTII(V) = INT((VERTEXTI(V) - XORIG3D) / XCELL3D) + 1
        VERTEXTJI(V) = INT((VERTEXTJ(V) - YORIG3D) / YCELL3D) + 1
        IF ((VERTEXTII(V) .gt. GL_NCOLS) .or. (VERTEXTII(V) .lt. 1)
            (VERTEXTJI(V) .gt. GL_NROWS) .or. (VERTEXTJI(V) .lt. 1) THEN
           STATUS = STATUS + 1
           WRITE( LOGDEV, '( /5X, A, F10.4, F10.4, A, I8, I8)' )
     &      'Outside domain', VERTEXTLON(V), VERTEXTLAT(V),
     &      'COL/ROW', VERTEXTII(V), VERTEXTJI(V)
        ENDIF
        WRITE(LOGDEV, *)'IN', VERTEXTLON(V), VERTEXTLAT(V)
        WRITE(LOGDEV, *) 'OUT', VERTEXTIDX(V), VERTEXTII(V),VERTEXTJI(V)
      END DO

      WRITE( LOGDEV,* ) ' '
      WRITE( LOGDEV,* ) '      VEXT  File Header Description:'
      DO K = 1, KD + L
         WRITE( LOGDEV,* ) '    => ', TRIM( FDESC3D( K ) )
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

C create header

      IF ( IO_PE_INCLUSIVE ) THEN   ! open new

         IF ( .NOT. OPEN3( CTM_VEXT_1, FSNEW3, PNAME ) ) THEN
            XMSG = 'Could not open ' // CTM_VEXT_1
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         IF ( .NOT. FLUSH3 ( CTM_VEXT_1 ) ) THEN
            XMSG = 'Could not sync to disk ' // CTM_VEXT_1
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

      END IF

      END SUBROUTINE OPVEXT
