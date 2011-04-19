
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
C $Header: /project/yoj/arc/CCTM/src/cloud/cloud_acm_ae5/rescld.F,v 1.4 2011/04/19 19:12:16 sjr Exp $

C what( 1 ) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE RESCLD ( CGRID, JDATE, JTIME, TSTEP,
     &                    N_SPC_WDEP, WDEP_MAP, DEP )

C-----------------------------------------------------------------------
C  FUNCTION: Resolved-scale CLOUD processor Models-3 science process:
 
C  Revision History:
C      No   Date   Who   What
C      -- -------- ---  -----------------------------------------
C       0 01/15/98 sjr  created program
C       1 03/09/98 sjr  made several revisions: fix to read sub-hourly
C                       rainfall data, reordered some of the code
C       2 12/15/98 David Wong at LM
C           -- changed division of GPKG to multiplication of GPKG reciprocal
C           -- interchanged loops structure in line 317
C       3 03/18/99 David Wong at LM
C           -- replace "* M2PHA * ONE_OVER_GPKG" by "* M2PHA_OVER_GPKG" which
C              is a new constant defined as M2PHA / GPKG
C       4 08/30/99 sjr  revised for new aerosol model (with 2nd moments)
C       5 Dec 00   Jeff move CGRID_MAP into f90 module
C       6 01/04/01 sjr  added QS and QI to total water content calcul.
C       7 Sep 01   Jeff Dyn Alloc - Use HGRD_DEFN
C       8 12/18/03 sjr & jp added QG in the water content calc
C       9 07 Dec 04 J.Young: Vert Dyn Alloc - Use VGRD_DEFN
C      10 31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                            domain specifications in one module
C      11 25 Mar 08 sjr fixed bug in the precipitation flux calculation:
C                       layer thickness now included in column integrated
C                       water content and in precipitation flux 
C                       calculations (bug reported by Raymond D Wright)
C      12 12 Aug 10 J.Young: replace CGRID mechanism include files with
C                    namelists and merge Shawn Roselle's, Sergey Napelenok's
C                    and Steve Howard's aerosol reengineering
C      13 01 Mar 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN;
C                    removed deprecated TRIMLEN
 
C  Called by:  CLDPROC
 
C  Calls the following subroutines:  SCAVWDEP and AQ_MAP
C-----------------------------------------------------------------------

      USE GRID_CONF           ! horizontal & vertical domain specifications
      USE CGRID_SPCS          ! CGRID mechanism species
      USE UTILIO_DEFN

      IMPLICIT NONE

C...........Includes:

      INCLUDE SUBST_CONST                ! constants
      INCLUDE SUBST_FILES_ID             ! file name parameters

      CHARACTER( 120 ) :: XMSG  = ' '    ! Exit status message

C...........Parameters:
      REAL, PARAMETER :: GPKG = 1.0E+03  ! g/kg
      REAL, PARAMETER :: M2PHA = 1.0E+04 ! 1 hectare = 1.0e4 m**2
      REAL, PARAMETER :: M2PHA_OVER_GPKG = M2PHA / GPKG

C Number of species in CGRID
      INTEGER, SAVE :: MXSPCS

C...........Arguments:
!     REAL          CGRID( NCOLS,NROWS,NLAYS,* )  !  concentrations
!     REAL       :: CGRID( :,:,:,: )    ! concentrations
      REAL, POINTER :: CGRID( :,:,:,: )    ! concentrations
      INTEGER       JDATE               ! current model date, coded YYYYDDD
      INTEGER       JTIME               ! current model time, coded HHMMSS
      INTEGER       TSTEP( 2 )          ! model time step, coded H*MMSS
      INTEGER       N_SPC_WDEP          ! # of wet deposition species
!     INTEGER       WDEP_MAP( * )       ! wet deposition map to CGRID
      INTEGER       WDEP_MAP( : )       ! wet deposition map to CGRID
!     REAL          DEP  ( NCOLS,NROWS,* )  ! depositions (etc.)
      REAL       :: DEP  ( :,:,: )      ! depositions (etc.)

C...........Local Variables:

      LOGICAL, SAVE :: FIRSTIME = .TRUE.   ! flag for first pass thru
      LOGICAL, SAVE :: QG_AVAIL = .TRUE.   ! flag for QG available on file
      LOGICAL, SAVE :: QI_AVAIL = .TRUE.   ! flag for QI available on file
      LOGICAL, SAVE :: QS_AVAIL = .TRUE.   ! flag for QS available on file

      CHARACTER( 16 ), SAVE :: PNAME = 'RESCLD'    ! process name
      CHARACTER( 16 ) ::  VARNM               ! variable name for IOAPI to get

      INTEGER       COL                 ! column loop counter
      INTEGER       ROW                 ! row loop counter
      INTEGER       LAY                 ! layer loop counter
      INTEGER       FINI                ! ending position
      INTEGER, SAVE :: LOGDEV           ! output log unit number
      INTEGER       MDATE               ! process date (yyyyddd)
      INTEGER, SAVE :: MSTEP            ! met file time step (hhmmss)
      INTEGER       MTIME               ! process time (hhmmss)
      INTEGER, SAVE :: SDATE            ! met file start date
      INTEGER       SPC                 ! liquid species loop counter
      INTEGER       STRT                ! starting position
      INTEGER, SAVE :: STIME            ! met file start time
      INTEGER       TCLD                ! cloud lifetime (sec)
      INTEGER       VAR                 ! variable loop counter
      INTEGER       ALLOCSTAT           ! memory allocation status

      REAL          AIRM                ! total airmass (mol/m2) in cloudy air
      REAL          ALFA0               ! aitken mode number scavenging coef
      REAL          ALFA2               ! aitken mode sfc area scavenging coef
      REAL          ALFA3               ! aitken mode mass scavenging coef
      REAL          CTHK1               ! cloud thickness (m)
      REAL          METSTEP             ! timestep on the met file (hr)
      SAVE          METSTEP
      REAL          PBARC               ! mean cloud pressure (Pa)
      REAL          PRATE1              ! storm rainfall rate (mm/hr)
      REAL          QCRGCOL             ! vert column integrated liquid water content
      REAL          QCRISGCOL           ! vert column integrated total water content
      REAL          QRSGCOL             ! vert column integrated precip content
      REAL          RAIN                ! non-conv rainfall rate (mm/hr)
      REAL          REMOVAC             ! variable storing H+ deposition
      REAL          TAUCLD              ! cloud lifetime (sec)
      REAL          TBARC               ! mean cloud temp (K)
      REAL          WCBAR               ! liq water content of cloud (kg/m3)
      REAL          WPBAR               ! precipitation water content (kg/m3)
      REAL          WTBAR               ! total water content of cloud (kg/m3)

      REAL, ALLOCATABLE, SAVE :: POLC ( : )   ! incloud conc (mol/mol)
      REAL, ALLOCATABLE, SAVE :: CEND ( : )   ! ending conc (mol/mol)
      REAL, ALLOCATABLE, SAVE :: REMOV( : )   ! moles/m2 or mm*mol/lit scavenged

      REAL          RN   ( NCOLS, NROWS ) ! non-convective rainfall (cm)
      REAL          DENS ( NCOLS, NROWS, NLAYS )  ! air density (kg/m3)
      REAL          DZZ  ( NCOLS, NROWS, NLAYS )  ! layer thickness (m)
      REAL          PRES ( NCOLS, NROWS, NLAYS )  ! air pressure (Pa)
      REAL          QC   ( NCOLS, NROWS, NLAYS )  ! cloud water content (kg/kg)
      REAL          QG   ( NCOLS, NROWS, NLAYS )  ! graupel content (kg/kg)
      REAL          QI   ( NCOLS, NROWS, NLAYS )  ! ice content (kg/kg)
      REAL          QR   ( NCOLS, NROWS, NLAYS )  ! rain water content (kg/kg)
      REAL          QS   ( NCOLS, NROWS, NLAYS )  ! snow content (kg/kg)
!     REAL          QV   ( NCOLS, NROWS, NLAYS )  ! specific humidity (kg/kg)
      REAL          TA   ( NCOLS, NROWS, NLAYS )  ! air temperature (K)
!     REAL          ZH   ( NCOLS, NROWS, NLAYS )  ! mid-layer height (m)
      REAL          ZF   ( NCOLS, NROWS, NLAYS )  ! level/layer-face height (m)

      INTEGER      GXOFF, GYOFF              ! global origin offset from file
C for INTERPX
      INTEGER, SAVE :: STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2
      INTEGER, SAVE :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3

C...........External Functions:

      INTERFACE
        SUBROUTINE SCAVWDEP ( JDATE, JTIME, WTBAR, WCBAR, TBARC, PBARC,
     &                        CTHK1, AIRM, PRATE1, TAUCLD, POLC, CEND,
     &                        REMOV, REMOVAC, ALFA0, ALFA2, ALFA3 )
           IMPLICIT NONE
           INTEGER, INTENT( IN )  :: JDATE, JTIME
           REAL,    INTENT( IN )  :: WTBAR, WCBAR, TBARC, PBARC,
     &                               CTHK1, AIRM, PRATE1, TAUCLD
           REAL,    INTENT( IN )  :: POLC ( : )
           REAL,    INTENT( OUT ) :: REMOVAC
           REAL,    INTENT( OUT ) :: CEND( : ), REMOV( : )
           REAL,    INTENT( OUT ) :: ALFA0, ALFA2, ALFA3
        END SUBROUTINE SCAVWDEP
        SUBROUTINE AQ_MAP( JDATE, JTIME, WTBAR, WCBAR, TBARC, PBARC,
     &                     CTHK1, AIRM, PRATE1, TAUCLD, POLC, CEND,
     &                     REMOV, REMOVAC, ALFA0, ALFA2, ALFA3 )
           IMPLICIT NONE
           INTEGER, INTENT( IN )    :: JDATE, JTIME
           REAL,    INTENT( IN )    :: WTBAR, WCBAR, TBARC, PBARC,
     &                                 CTHK1, AIRM, PRATE1, TAUCLD
           REAL,    INTENT( IN )    :: POLC ( : )
           REAL,    INTENT( INOUT ) :: REMOVAC
           REAL,    INTENT( INOUT ) :: CEND( : ), REMOV( : )
           REAL,    INTENT( IN )    :: ALFA0, ALFA2, ALFA3
        END SUBROUTINE AQ_MAP
      END INTERFACE
C-----------------------------------------------------------------------

C...Initialization

      IF ( FIRSTIME ) THEN

        FIRSTIME = .FALSE.

        LOGDEV = INIT3()

C...open MET_CRO_2D

        IF ( .NOT. OPEN3( MET_CRO_2D, FSREAD3, PNAME ) ) THEN
          XMSG = 'Could not open '// MET_CRO_2D // ' file'
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

C...open MET_CRO_3D

        IF ( .NOT. OPEN3( MET_CRO_3D, FSREAD3, PNAME ) ) THEN
          XMSG = 'Could not open '// MET_CRO_3D // ' file'
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

C...get description from the met file

        IF ( .NOT. DESC3( MET_CRO_3D ) ) THEN
          XMSG = 'Could not get ' // MET_CRO_3D //' file description'
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

C...store met file time, date, and step information and compute
C...  the met timestep in hours

        SDATE = SDATE3D
        STIME = STIME3D
        MSTEP = TSTEP3D

        METSTEP = FLOAT( TIME2SEC( MSTEP ) ) / 3600.0

        VAR = INDEX1( 'QI', NVARS3D, VNAME3D )

        IF ( VAR .EQ. 0 ) THEN
          QI_AVAIL = .FALSE.
          XMSG = 'Parameter QI (cloud ice) was not found on file '
     &           // MET_CRO_3D
          CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
          WRITE( LOGDEV, '(3(/10X,A),(/10X,3(A,1X)),(/10X,A))' )
     &           'YOU SHOULD VERIFY that the cloud microphysics scheme used',
     &           'in the Meteorological Model did not include ice/snow.  If',
     &           'it did, then you need to reprocess the meteorological data',
     &           'through MCIP and pass QI to file ',
     &           TRIM( MET_CRO_3D ), ' to avoid',
     &           'errors in the wet deposition.'
          WRITE( LOGDEV, '((/5X,A),/)' )
     &           'Processing will continue with QI set to ZERO.  <<---<<'
        END IF

        VAR = INDEX1( 'QS', NVARS3D, VNAME3D )

        IF ( VAR .EQ. 0 ) THEN
          QS_AVAIL = .FALSE.
          XMSG = 'Parameter QS (snow) was not found on file ' // MET_CRO_3D
          CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
          WRITE( LOGDEV, '(3(/10X,A),(/10X,3(A,1X)),(/10X,A))' )
     &           'YOU SHOULD VERIFY that the cloud microphysics scheme used',
     &           'in the Meteorological Model did not include ice/snow.  If',
     &           'it did, then you need to reprocess the meteorological data',
     &           'through MCIP and pass QS to file ',
     &           TRIM( MET_CRO_3D ), ' to avoid',
     &           'errors in the wet deposition.'
          WRITE( LOGDEV, '((/5X,A),/)' )
     &           'Processing will continue with QS set to ZERO.  <<--<<'
        END IF

        VAR = INDEX1( 'QG', NVARS3D, VNAME3D )

        IF ( VAR .EQ. 0 ) THEN
          QG_AVAIL = .FALSE.
        END IF

        CALL SUBHFILE ( MET_CRO_2D, GXOFF, GYOFF,
     &                  STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2 )
        CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                  STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

        MXSPCS = N_GC_SPCD + N_AE_SPC + N_NR_SPC + N_TR_SPC

        ALLOCATE ( CEND ( MXSPCS ),
     &             POLC ( MXSPCS ),
     &             REMOV( MXSPCS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating CEND, POLC or REMOV'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

      END IF  ! FIRSTIME

      MDATE = JDATE
      MTIME = JTIME

C...set the cloud lifetime (=adv timestep)

      TCLD = TIME2SEC( TSTEP( 2 ) )
      TAUCLD = REAL( TCLD )

C...set time to the midpoint of this timestep for data interpolation

      CALL NEXTIME ( MDATE, MTIME, SEC2TIME( TCLD / 2 ) )

C...Actual Science Process (loop on internal process time steps):
C...  Interpolate time dependent layered input variables
C...  (reading those variables for which it is necessary)

C...  Get ambient temperature (K)

      VARNM = 'TA'
!     IF ( .NOT. INTERP3( MET_CRO_3D, VARNM, PNAME, MDATE, MTIME,
!    &                    NCOLS * NROWS * NLAYS, TA ) ) THEN
      IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MDATE, MTIME, TA ) ) THEN
        XMSG = 'Could not read TA from ' // MET_CRO_3D
        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

C...Get resolved cloud water mixing ratio (kg H2O / kg air)

      VARNM = 'QC'
!     IF ( .NOT. INTERP3( MET_CRO_3D, VARNM, PNAME, MDATE, MTIME,
!    &                    NCOLS * NROWS * NLAYS, QC ) ) THEN
      IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MDATE, MTIME, QC ) ) THEN
        XMSG = 'Could not read QC from ' // MET_CRO_3D
        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

C...Get resolved rain water mixing ratio (kg H2O / kg air)

      VARNM = 'QR'
!     IF ( .NOT. INTERP3( MET_CRO_3D, VARNM, PNAME, MDATE, MTIME,
!    &                    NCOLS * NROWS * NLAYS, QR ) ) THEN
      IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MDATE, MTIME, QR ) ) THEN
        XMSG = 'Could not read QR from ' // MET_CRO_3D
        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

C...read resolved ice mixing ratio (kg H2O / kg air) from the met
C...  file if it is available

      IF ( QI_AVAIL ) THEN

        VARNM = 'QI'
!       IF ( .NOT. INTERP3( MET_CRO_3D, VARNM, PNAME, MDATE, MTIME,
!    &                      NCOLS * NROWS * NLAYS, QI ) ) THEN
          XMSG = 'Could not read QI from ' // MET_CRO_3D
        IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                      STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                      MDATE, MTIME, QI ) ) THEN
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

      ELSE

        QI = 0.0    ! otherwise fill the array with zeros

      END IF

C...read resolved snow mixing ratio (kg H2O / kg air) from the met
C...  file if it is available

      IF ( QS_AVAIL ) THEN

        VARNM = 'QS'
!       IF ( .NOT. INTERP3( MET_CRO_3D, VARNM, PNAME, MDATE, MTIME,
!    &                      NCOLS * NROWS * NLAYS, QS ) ) THEN
        IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                      STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                      MDATE, MTIME, QS ) ) THEN
          XMSG = 'Could not read QS from ' // MET_CRO_3D
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

      ELSE

        QS = 0.0    ! otherwise fill the array with zeros

      END IF

C...read graupel mixing ratio (kg H2O / kg air) from the met
C...  file if it is available

      IF ( QG_AVAIL ) THEN

        VARNM = 'QG'
!       IF ( .NOT. INTERP3( MET_CRO_3D, VARNM, PNAME, MDATE, MTIME,
!    &                      NCOLS * NROWS * NLAYS, QG ) ) THEN
          XMSG = 'Could not read QG from ' // MET_CRO_3D
        IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                      STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                      MDATE, MTIME, QG ) ) THEN
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

      ELSE

        QG = 0.0    ! otherwise fill the array with zeros

      END IF

!C...Get specific humidity (kg H2O / kg air)
 
!     VARNM = 'QV'
!!    IF ( .NOT. INTERP3( MET_CRO_3D, VARNM, PNAME, MDATE, MTIME,
!!   &                    NCOLS * NROWS * NLAYS, QV ) ) THEN
!     IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
!    &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
!    &                    MDATE, MTIME, QV ) ) THEN
!       XMSG = 'Could not read QV from ' // MET_CRO_3D
!       CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
!     END IF
 
C...Get level heights / layer faces (m)

      VARNM = 'ZF'
!     IF ( .NOT. INTERP3 ( MET_CRO_3D, VARNM, PNAME, MDATE, MTIME,
!    &                     NCOLS * NROWS * NLAYS, ZF ) ) THEN
      IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MDATE, MTIME, ZF ) ) THEN
        XMSG = 'Could not read ZF from ' // MET_CRO_3D
        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

!C...Get mid-layer heights (m)
!
!     VARNM = 'ZH'
!!    IF ( .NOT. INTERP3 ( MET_CRO_3D, VARNM, PNAME, MDATE, MTIME,
!!   &                     NCOLS * NROWS * NLAYS, ZH ) ) THEN
!     IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
!    &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
!    &                    MDATE, MTIME, ZH ) ) THEN
!       XMSG = 'Could not read ZH from ' // MET_CRO_3D
!       CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
!     END IF
 
C...Get pressure (Pa)

      VARNM = 'PRES'
!     IF ( .NOT. INTERP3( MET_CRO_3D, VARNM, PNAME, MDATE, MTIME,
!    &                    NCOLS * NROWS * NLAYS, PRES ) ) THEN
      IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MDATE, MTIME, PRES ) ) THEN
        XMSG = 'Could not read PRES from ' // MET_CRO_3D
        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

C...Get air density (kg/m3)

      VARNM = 'DENS'
!     IF ( .NOT. INTERP3( MET_CRO_3D, VARNM, PNAME, MDATE, MTIME,
!    &                    NCOLS * NROWS * NLAYS, DENS ) ) THEN
      IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MDATE, MTIME, DENS ) ) THEN
        XMSG = 'Could not read DENS from ' // MET_CRO_3D
        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

C...compute layer thicknesses (m)

      DO ROW = 1, MY_NROWS
        DO COL = 1, MY_NCOLS
          DZZ( COL, ROW, 1 ) = ZF( COL, ROW, 1 )
          DO LAY = 2, NLAYS
            DZZ( COL, ROW, LAY ) = ZF( COL, ROW, LAY )
     &                           - ZF( COL, ROW, LAY - 1 )
          END DO
        END DO
      END DO

C...advance the MDATE and MTIME to the next time on the met file
C...  to get ready to read the precipitation amounts.
C...  Precipitation data WILL NOT BE INTERPOLATED!  Precipitation data
C...  on the input file are amounts within the metfiles timestep.

      IF ( .NOT. CURRSTEP( JDATE, JTIME, SDATE, STIME, MSTEP,
     &                     MDATE, MTIME ) ) THEN
        XMSG = 'Cannot get step-starting date and time'
        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
      END IF

      CALL NEXTIME ( MDATE, MTIME, MSTEP )  ! set mdate:mtime to the hour

C...Get nonconvective precipitation amount (cm)

      VARNM = 'RN'
!     IF ( .NOT. READ3( MET_CRO_2D, VARNM, ALLAYS3, MDATE, MTIME,
!    &                  RN ) ) THEN
      IF ( .NOT. INTERPX( MET_CRO_2D, VARNM, PNAME,
     &                    STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                    MDATE, MTIME, RN ) ) THEN
        XMSG = 'Could not read RN from ' // MET_CRO_2D
        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

C...Loop through all grid cells

      DO ROW = 1, MY_NROWS
        DO COL = 1, MY_NCOLS

C...Convert the rainfall into a rainfall rate (mm/hr)

          RAIN = 10.0 * RN( COL, ROW ) / METSTEP

          IF ( RAIN .LT. 0.0 ) THEN
            XMSG = 'NEGATIVE RAIN...PROBABLE BAD MET DATA...'
     &              // MET_CRO_2D
            CALL M3EXIT ( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
          END IF

C...calculate the integrated column cloud+rain water content
C... JP & SJR:  treat graupel as rainwater until we have a more
C...            advanced cloud microphysics scheme
C... include layer thickness in the column integration

          QCRGCOL   = 0.0
          QCRISGCOL = 0.0
          QRSGCOL   = 0.0
          DO LAY = 1, NLAYS
            QC( COL, ROW, LAY ) = MAX( QC( COL, ROW, LAY ), 0.0 )
            QR( COL, ROW, LAY ) = MAX( QR( COL, ROW, LAY ), 0.0 )
            QI( COL, ROW, LAY ) = MAX( QI( COL, ROW, LAY ), 0.0 )
            QS( COL, ROW, LAY ) = MAX( QS( COL, ROW, LAY ), 0.0 )
            QG( COL, ROW, LAY ) = MAX( QG( COL, ROW, LAY ), 0.0 )
            QCRGCOL   = QCRGCOL   + DENS( COL, ROW, LAY )
     &                * DZZ( COL, ROW, LAY )
     &                * ( QC( COL, ROW, LAY ) + QR( COL, ROW, LAY )
     &                +   QG( COL, ROW, LAY ) )
            QCRISGCOL = QCRISGCOL + DENS( COL, ROW, LAY )
     &                * DZZ( COL, ROW, LAY )
     &                * ( QC( COL, ROW, LAY ) + QR( COL, ROW, LAY )
     &                +   QI( COL, ROW, LAY ) + QS( COL, ROW, LAY )
     &                +   QG( COL, ROW, LAY ) )
            QRSGCOL   = QRSGCOL   + DENS( COL, ROW, LAY )
     &                * DZZ( COL, ROW, LAY )
     &                * ( QR( COL, ROW, LAY ) + QS( COL, ROW, LAY )
     &                +   QG( COL, ROW, LAY ) )
          END DO

C...loop through layers

          IF ( QCRGCOL .GT. 0.0 ) THEN
            DO LAY = 1, NLAYS

C...Compute cloud quantities

              IF ( ( QC( COL, ROW, LAY ) + QR( COL, ROW, LAY )
     &               + QG( COL, ROW, LAY ) ) .GT. 0.00005 ) THEN

                TBARC = TA( COL, ROW, LAY )

                PBARC = PRES( COL, ROW, LAY )

                CTHK1 = DZZ( COL, ROW, LAY )

                AIRM = PBARC * CTHK1 * 1.0E3 / ( RDGAS * MWAIR * TBARC )

                WCBAR = ( QC( COL, ROW, LAY ) + QR( COL, ROW, LAY )
     &                +   QG( COL, ROW, LAY ) ) * DENS( COL, ROW, LAY )

                WTBAR = ( QC( COL, ROW, LAY ) + QR( COL, ROW, LAY )
     &                +   QI( COL, ROW, LAY ) + QS( COL, ROW, LAY ) 
     &                +   QG( COL, ROW, LAY ) ) * DENS( COL, ROW, LAY )

C...Compute precipitation flux
C...  include layer thickness in the calculation

                IF ( QRSGCOL .GT. 0.0 ) THEN
                  WPBAR = ( QR( COL, ROW, LAY ) + QS( COL, ROW, LAY )
     &                  +   QG( COL, ROW, LAY ) ) * DENS( COL, ROW, LAY )
                  PRATE1 = RAIN * WPBAR * CTHK1 / QRSGCOL  ! convert to mm/hr
                ELSE
                  PRATE1 = RAIN * WTBAR * CTHK1 / QCRISGCOL  ! convert to mm/hr
                END IF

C...Finally, get in-cloud pollutant concentrations in moles sp
C...  per mole air

                DO SPC = 1, NSPCSD
                  POLC ( SPC ) = CGRID( COL, ROW, LAY, SPC )
                  CEND ( SPC ) = POLC( SPC )
                  REMOV( SPC ) = 0.0
                END DO

C...perform scavenging and aqueous chemistry within the cloud
C...  and re-adjust the ending and removed amounts for those species
C...  that were scavenged or that participated in cloud chemistry

                CALL SCAVWDEP ( JDATE, JTIME, WTBAR, WCBAR, TBARC,
     &                          PBARC, CTHK1, AIRM, PRATE1, TAUCLD,
     &                          POLC, CEND, REMOV, REMOVAC, ALFA0,
     &                          ALFA2, ALFA3 )

C...if the liquid water content is above the specified threshold
C...  then perform the aqueous chemistry within the cloud and
C...  re-adjust the ending and removed amounts for those species
C...  that participated in cloud chemistry

                IF ( WCBAR .GT. 0.00001 ) THEN
                  CALL AQ_MAP ( JDATE, JTIME, WTBAR, WCBAR, TBARC,
     &                          PBARC, CTHK1, AIRM, PRATE1, TAUCLD,
     &                          POLC, CEND, REMOV, REMOVAC, ALFA0,
     &                          ALFA2, ALFA3 )
                END IF

C...convert from moles/m**2 to kg/m**2 and kg/m**2 to kg/hectare

C...  for gases

                SPC = 0
                STRT = GC_STRT
                FINI = GC_STRT - 1 + N_GC_SPC
                DO VAR = STRT, FINI
                  SPC = SPC + 1
                  REMOV( VAR ) = REMOV( VAR ) * GC_MOLWT( SPC )
     &                         * M2PHA_OVER_GPKG
                END DO

C...  for aerosols

                SPC = 0
                STRT = AE_STRT
                FINI = AE_STRT - 1 + N_AE_SPC
                DO VAR = STRT, FINI
                  SPC = SPC + 1
                  IF (( INDEX( AE_SPC( SPC ), 'NUM' ) .EQ. 0 ) .AND.
     &                ( INDEX( AE_SPC( SPC ), 'SRF' ) .EQ. 0 )) THEN
                    REMOV( VAR ) = REMOV( VAR ) * AE_MOLWT( SPC )
     &                           * M2PHA_OVER_GPKG
                  ELSE
                    REMOV( VAR ) = REMOV( VAR ) * M2PHA
                  END IF
                END DO

C...  for non-reactives

                SPC = 0
                STRT = NR_STRT
                FINI = NR_STRT - 1 + N_NR_SPC
                DO VAR = STRT, FINI
                  SPC = SPC + 1
                  REMOV( VAR ) = REMOV( VAR ) * NR_MOLWT( SPC )
     &                         * M2PHA_OVER_GPKG
                END DO

C...  for tracers

                SPC = 0
                STRT = TR_STRT
                FINI = TR_STRT - 1 + N_TR_SPC
                DO VAR = STRT, FINI
                  SPC = SPC + 1
                  REMOV( VAR ) = REMOV( VAR ) * TR_MOLWT( SPC )
     &                         * M2PHA_OVER_GPKG
                END DO

C...load deposition amounts into the DEP array

                DO VAR = 1, N_SPC_WDEP
                  DEP( COL, ROW, VAR ) = DEP( COL, ROW, VAR )
     &                                 + REMOV( WDEP_MAP( VAR ) )
                END DO

C...load H+ concentration into the deposition array as well

                DEP( COL, ROW, N_SPC_WDEP+1 ) =
     &                         DEP( COL, ROW, N_SPC_WDEP+1 ) + REMOVAC

C...set cgrid to the ending concentrations

                DO SPC = 1, NSPCSD
                  IF ( SPC .NE. N_GC_SPCD ) THEN
                    CGRID( COL, ROW, LAY, SPC ) = CEND( SPC )
                  END IF
                END DO

              END IF
            END DO
          END IF

        END DO   ! col
      END DO   ! row

      RETURN

      END
