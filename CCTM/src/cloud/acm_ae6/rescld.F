
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
      SUBROUTINE RESCLD ( CGRD, JDATE, JTIME, TSTEP,
     &                    DEP, RESTRANS )

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
C      14 11 May D.Wong: incorporated twoway model implementation
C      15 01 Jul 11 G. Sarwar: calculate zenith angle to determine daytime and  
C                    nightime needed for sulfur oxidation via metal catalysis
C      16 02Aug12 S.Roselle:  instrumented to calculate and return
C                             transmissivity for resolved clouds
C      07 Nov 14 J.Bash: Updated call to czangle.F for the ASX_DATA_MOD shared data module. 
C      07 May 18 D. Schwede: Removed call to CZANGLE. COSZEN now calculated in ASX_DATA_MOD
C      26 Nov 18 S. Napelenok: ISAM implementation
C       1 Feb 19 D. Wong: Implemented centralized I/O approach, removed all MY_N
C                         clauses
C      01 AUG 19 D. Wong: Modified code to work with two-way model
C      11 Nov 19 F. Sidi: Changed MSTEP to accomdate Centralized I/O changesC      
C      30 Dec 19 S. Napelenok: ddm-3d implementaiton for version 5.3.1


C  Called by:  CLDPROC
 
C  Calls the following subroutines:  SCAVWDEP and AQ_MAP
C-----------------------------------------------------------------------

      USE RUNTIME_VARS, ONLY: STM
      USE GRID_CONF           ! horizontal & vertical domain specifications
      USE CGRID_SPCS          ! CGRID mechanism species
      USE UTILIO_DEFN
      USE AQ_DATA,       ONLY: JH2O2_HYDROMETEORS,  JHNO3_HYDROMETEORS
      USE AERO_DATA, ONLY : ASO4GAS_IDX, MAP_AERO, AEROSPC_MAP
      USE PRECURSOR_DATA, ONLY: SULF_IDX, PRECURSOR_MAP, MAP_PRECURSOR
      USE ASX_DATA_MOD,  ONLY: MET_DATA
      USE PHOT_MOD,      ONLY: RJ, RJ_RES, RJ_SUB, LH2O2, LHNO3
      USE CENTRALIZED_IO_MODULE
#ifdef isam
      USE SA_DEFN, ONLY: ISAM, NSPC_SA, NTAG_SA, MAP_SAtoCGR, OTHRTAG, 
     &                   ISAM_SPEC, DEPSUM_SAVE, DS4_SAVE, REMOV_SAVE,
     &                   ITAG,TOT_SADEP,
     &                   DEPSUM_AORGC_SAVE, DGLY1_SAVE, DMGLY1_SAVE,
     &                   REMOV_AORGC_SAVE
#endif

#ifdef sens
      USE DDM3D_DEFN, ONLY: SENGRID, NP, NPMAX, S_CONDEP, S_POLC, 
     &                      S_CEND, S_REMOV, S_REMOVAC, S_CONDEP, 
     &                      S_TOTDEP
#endif 

#ifdef mpas
      use util_module, only : nextime, sec2time, TIME2SEC, currstep
#endif

      IMPLICIT NONE

C...........Includes:

      INCLUDE SUBST_CONST                ! constants
      INCLUDE SUBST_FILES_ID             ! file name parameters

C...........Arguments:
      REAL, INTENT( INOUT )    :: CGRD( :,:,:,: ) ! concentrations
      INTEGER, INTENT( IN )    :: JDATE            ! current model date, coded YYYYDDD
      INTEGER, INTENT( IN )    :: JTIME            ! current model time, coded HHMMSS
      INTEGER, INTENT( IN )    :: TSTEP( 3 )       ! model time steps, coded HHMMSS
      REAL,    INTENT( INOUT ) :: DEP( :,:,: )     ! wet deposition
      REAL,    INTENT( OUT )   :: RESTRANS( :,: )  ! resolved cloud transmissivity

      CHARACTER( 120 ) :: XMSG  = ' '    ! Exit status message

C...........Parameters:
      REAL, PARAMETER :: PERMIN_TO_PERSEC = 1.0 / 60.0
      REAL, PARAMETER :: GPKG = 1.0E+03  ! g/kg
      REAL, PARAMETER :: M2PHA = 1.0E+04 ! 1 hectare = 1.0e4 m**2
      REAL, PARAMETER :: M2PHA_OVER_GPKG = M2PHA / GPKG

C Number of species in CGRID
      INTEGER, SAVE :: MXSPCS


C...........Local Variables:

      LOGICAL, SAVE :: FIRSTIME = .TRUE.   ! flag for first pass thru

      CHARACTER( 16 ), SAVE :: PNAME = 'RESCLD'    ! process name
      CHARACTER( 16 ), SAVE :: VNAME_RN

      INTEGER       COL                 ! column loop counter
      INTEGER       ROW                 ! row loop counter
      INTEGER       LAY                 ! layer loop counter
      INTEGER       FINI                ! ending position
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
      INTEGER       I

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
      REAL          QCICOL              ! vert column integrated cloud content
      REAL          RAIN                ! non-conv rainfall rate (mm/hr)
      REAL          REMOVAC             ! variable storing H+ deposition
      REAL          TAUCLD              ! cloud lifetime (sec)
      REAL          TBARC               ! mean cloud temp (K)
      REAL          WCBAR               ! liq water content of cloud (kg/m3)
      REAL          WPBAR               ! precipitation water content (kg/m3)
      REAL          WTBAR               ! total water content of cloud (kg/m3)
      REAL          LWP, CLOD

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
      REAL          TA   ( NCOLS, NROWS, NLAYS )  ! air temperature (K)
      REAL          ZF   ( NCOLS, NROWS, NLAYS )  ! level/layer-face height (m)

C Gridded meteorology data:
C Latitude and longitude for zenith angle calculation: Golam Sarwar * July 1, 2011 
      REAL          COSZ                            ! local cosine of zenith angle
      REAL          JH2O2                           ! H2O2 photolysis rate, 1/min 
      REAL          JHNO3                           ! HNO3 photolysis rate, 1/min 

#ifdef isam
      INTEGER, SAVE           :: S_SO2, S_SO4J, S_SULF
      INTEGER, SAVE           :: C_SO2, C_SO4J, C_SULF
      REAL, ALLOCATABLE, SAVE :: SA_POLC   ( :,: )
      REAL, ALLOCATABLE, SAVE :: SA_CEND   ( :,: )
      INTEGER                 :: CSPC
      REAL, ALLOCATABLE, SAVE :: SA_DS4    ( : )
      REAL                    :: SA_SUM
      REAL, ALLOCATABLE, SAVE :: SA_REMOV  ( :,: )
      INTEGER, SAVE           :: S_GLY, S_MGLY, S_AORGCJ
      INTEGER, SAVE           :: C_GLY, C_MGLY, C_AORGCJ
      REAL, ALLOCATABLE, SAVE :: SA_DCSOA_GLY    ( : )
      REAL, ALLOCATABLE, SAVE :: SA_DCSOA_MGLY   ( : )
#endif

C...........External Functions:

      INTERFACE
        SUBROUTINE SCAVWDEP ( JDATE, JTIME, WTBAR, WCBAR, TBARC, PBARC,
     &                        CTHK1, AIRM, PRATE1, TAUCLD, POLC, CEND,
     &                        REMOV, REMOVAC, ALFA0, ALFA2, ALFA3 )
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
     &                     REMOV, REMOVAC, ALFA0, ALFA2, ALFA3, COSZ )
           INTEGER, INTENT( IN )    :: JDATE, JTIME
           REAL,    INTENT( IN )    :: WTBAR, WCBAR, TBARC, PBARC,
     &                                 CTHK1, AIRM, PRATE1, TAUCLD
           REAL,    INTENT( IN )    :: POLC ( : )
           REAL,    INTENT( INOUT ) :: REMOVAC
           REAL,    INTENT( INOUT ) :: CEND( : ), REMOV( : )
           REAL,    INTENT( IN )    :: ALFA0, ALFA2, ALFA3, COSZ	
        END SUBROUTINE AQ_MAP
      END INTERFACE
C-----------------------------------------------------------------------

C...Initialization

      IF ( FIRSTIME ) THEN

        FIRSTIME = .FALSE.

C...Sulfur tracking
        IF ( STM ) THEN
          CALL MAP_AERO()
          CALL MAP_PRECURSOR()
        END IF

        IF (RNA_AVAIL) THEN
           VNAME_RN = 'RNA'
        ELSE
           VNAME_RN = 'RN'
        END IF

C...store met file time, date, and step information and compute
C...  the met timestep in hours

        SDATE = cio_model_sdate
        STIME = cio_model_stime
        MSTEP = file_tstep(f_met)

        METSTEP = FLOAT( TIME2SEC( MSTEP ) ) / 3600.0

        if ( .not. QI_AVAIL) then
          write (logdev, '(a)') 'Parameter QI (cloud ice) was not found on file '
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

        if ( .not. QS_AVAIL) then
          write (logdev, '(a)') 'Parameter QS (snow) was not found on file '
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

        MXSPCS = N_GC_SPCD + N_AE_SPC + N_NR_SPC + N_TR_SPC

        ALLOCATE ( CEND ( MXSPCS ),
     &             POLC ( MXSPCS ),
     &             REMOV( MXSPCS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating CEND, POLC or REMOV'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

#ifdef isam
! move all this somewhere else eventually

        S_SO2  = INDEX1( 'SO2', NSPC_SA, ISAM_SPEC(:,OTHRTAG) )
        S_SO4J = INDEX1( 'ASO4J', NSPC_SA, ISAM_SPEC(:,OTHRTAG) )
        S_SULF = INDEX1( 'SULF', NSPC_SA, ISAM_SPEC(:,OTHRTAG) )

        C_SO2  = INDEX1( 'SO2',   N_GC_SPC, GC_SPC )
        C_SO4J = INDEX1( 'ASO4J', N_AE_SPC, AE_SPC ) + 1 + N_GC_SPC
        C_SULF = INDEX1( 'SULF',  N_GC_SPC, GC_SPC )

        ALLOCATE ( SA_POLC  ( NSPC_SA, NTAG_SA ),
     &             SA_CEND  ( NSPC_SA, NTAG_SA ),
     &             SA_DS4   ( NTAG_SA ),
     &             SA_REMOV ( NSPC_SA, NTAG_SA ),
     &             STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating SA_POLC or SA_CEND'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        S_GLY    = INDEX1( 'GLY', NSPC_SA, ISAM_SPEC(:,OTHRTAG) )
        S_MGLY   = INDEX1( 'MGLY', NSPC_SA, ISAM_SPEC(:,OTHRTAG) )
        S_AORGCJ = INDEX1( 'AORGCJ', NSPC_SA, ISAM_SPEC(:,OTHRTAG) )

        C_GLY    = INDEX1( 'GLY',   N_GC_SPC, GC_SPC )
        C_MGLY   = INDEX1( 'MGLY', N_GC_SPC, GC_SPC )
        C_AORGCJ = INDEX1( 'AORGCJ',  N_AE_SPC, AE_SPC ) + 1 + N_GC_SPC
        ALLOCATE ( SA_DCSOA_GLY ( NTAG_SA ),
     &             SA_DCSOA_MGLY ( NTAG_SA ),
     &             STAT = ALLOCSTAT )
#endif

#ifdef sens
        IF ( .NOT. ALLOCATED( S_CEND ) ) ALLOCATE ( S_CEND ( NPMAX, MXSPCS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating S_CEND'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        IF ( .NOT. ALLOCATED( S_POLC ) ) ALLOCATE ( S_POLC ( NPMAX, MXSPCS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating S_POLC'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        IF ( .NOT. ALLOCATED( S_REMOV ) ) ALLOCATE ( S_REMOV ( NPMAX, MXSPCS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating S_REMOV'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        IF ( .NOT. ALLOCATED( S_REMOVAC ) ) ALLOCATE ( S_REMOVAC ( NPMAX ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating S_REMOVAC'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF 
        S_REMOVAC = 0.0
#endif

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

      call interpolate_var ('TA', mdate, mtime, TA)

C...Get resolved cloud water mixing ratio (kg H2O / kg air)

      call interpolate_var ('QC', MDATE, MTIME, QC )

C...Get resolved rain water mixing ratio (kg H2O / kg air)

      call interpolate_var ('QR', MDATE, MTIME, QR )

C...read resolved ice mixing ratio (kg H2O / kg air) from the met
C...  file if it is available

      IF ( QI_AVAIL ) THEN

        call interpolate_var ('QI', MDATE, MTIME, QI )

      ELSE

        QI = 0.0    ! otherwise fill the array with zeros

      END IF

C...read resolved snow mixing ratio (kg H2O / kg air) from the met
C...  file if it is available

      IF ( QS_AVAIL ) THEN

        call interpolate_var ('QS', MDATE, MTIME, QS )

      ELSE

        QS = 0.0    ! otherwise fill the array with zeros

      END IF

C...read graupel mixing ratio (kg H2O / kg air) from the met
C...  file if it is available

      IF ( QG_AVAIL ) THEN

         call interpolate_var ('QG', MDATE, MTIME, QG )

      ELSE

        QG = 0.0    ! otherwise fill the array with zeros

      END IF

C...Get level heights / layer faces (m)

      call interpolate_var ('ZF', MDATE, MTIME, ZF )

C...Get pressure (Pa)

      call interpolate_var ('PRES', MDATE, MTIME, PRES )

C...Get air density (kg/m3)

      call interpolate_var ('DENS', MDATE, MTIME, DENS )

C...compute layer thicknesses (m)

      DO ROW = 1, NROWS
        DO COL = 1, NCOLS
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

      call interpolate_var (VNAME_RN, MDATE, MTIME, RN )

C...Loop through all grid cells

      DO ROW = 1, NROWS
        DO COL = 1, NCOLS

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
          QCICOL    = 0.0
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
            QCICOL    = QCICOL + DENS( COL, ROW, LAY )
     &                * DZZ( COL, ROW, LAY )
     &                * ( QC( COL, ROW, LAY ) + QI( COL, ROW, LAY ) )
          END DO

C...Calculate the cloud optical depth using a formula derived from
C...  Stephens (1978), JAS(35), pp2111-2132.
C...  only calculate the cloud optical depth when the liquid water
C...  path is >= 10 g/m2

          LWP = QCICOL * 1000.0  ! converts to g/m2
          IF ( LWP .GE. 10.0 ) THEN
             CLOD = 10.0**( 0.2633 + 1.7095 * LOG( LOG10( LWP ) ) )
          ELSE
             CLOD = 0.0
          END IF

C...If no cloud or optical depth < 5, set clear sky values.
C...  (i.e. don't do anything)

          IF ( CLOD .GE. 5.0 ) THEN

             RESTRANS( COL, ROW ) = ( 5.0 - EXP ( -CLOD ) ) / ( 4.0 + 0.42 * CLOD )
              
          END IF

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
                  POLC ( SPC ) = CGRD( COL, ROW, LAY, SPC )
                  CEND ( SPC ) = POLC( SPC )
                  REMOV( SPC ) = 0.0
                END DO

#ifdef isam
                DO SPC = 1, NSPC_SA
                  DO ITAG = 1, NTAG_SA
                    SA_POLC( SPC, ITAG ) = ISAM( COL,ROW,LAY,SPC,ITAG )
                    SA_CEND( SPC, ITAG ) = SA_POLC( SPC, ITAG )
                    SA_REMOV( SPC, ITAG ) = 0.0
                  END DO
                END DO
#endif 

#ifdef sens
                DO NP = 1, NPMAX
                  DO SPC = 1, NSPCSD
                    S_POLC ( NP, SPC ) = SENGRID( COL, ROW, LAY, NP, SPC )
                    S_CEND ( NP, SPC ) = S_POLC( NP, SPC)
                    S_REMOV( NP, SPC ) = 0.0
                  END DO
                END DO
#endif

C...perform scavenging and aqueous chemistry within the cloud
C...  and re-adjust the ending and removed amounts for those species
C...  that were scavenged or that participated in cloud chemistry

                CALL SCAVWDEP ( JDATE, JTIME, WTBAR, WCBAR, TBARC,
     &                          PBARC, CTHK1, AIRM, PRATE1, TAUCLD,
     &                          POLC, CEND, REMOV, REMOVAC, ALFA0,
     &                          ALFA2, ALFA3 )

#ifdef isam
                DO SPC = 1, NSPC_SA
                  CSPC = MAP_SAtoCGR(SPC)
                  SA_SUM = SUM ( SA_POLC( SPC,: ) )
c                 IF ( POLC( CSPC ) .GT. 1.0E-30 .AND. CEND( CSPC ) .GT. 1.0E-09 ) THEN
c                 IF ( POLC( CSPC ) .GT. 1.0E-30 ) THEN
                  IF ( SA_SUM .GT. 1.0E-25 ) THEN
                    DO ITAG = 1, NTAG_SA
                      SA_CEND( SPC, ITAG ) = SA_POLC( SPC, ITAG )
     &                                     * ( CEND( CSPC )
c    &                                       / POLC( CSPC ) )
     &                                       / SA_SUM )
                      SA_REMOV( SPC, ITAG ) = SA_POLC( SPC, ITAG )
     &                                      * ( REMOV( CSPC )
c    &                                        / POLC( CSPC ) )
     &                                        / SA_SUM )
                    END DO
                  ELSE ! no update
                    DO ITAG = 1, NTAG_SA
                      SA_CEND( SPC, ITAG )  = 0.0
                      SA_REMOV( SPC, ITAG ) = 0.0
                    END DO
                  END IF
                END DO
#endif

C...if the liquid water content is above the specified threshold
C...  then perform the aqueous chemistry within the cloud and
C...  re-adjust the ending and removed amounts for those species
C...  that participated in cloud chemistry

                IF ( WCBAR .GT. 1.0E-5 ) THEN

C...  calculate cosine of zenith angle for the cell and determine day or night; Golam Sarwar 

                  COSZ = MET_DATA%COSZEN( COL, ROW )
                  IF ( COSZ .LE. 0.0 ) THEN 
                    JH2O2_HYDROMETEORS = 0.0D0
                    JHNO3_HYDROMETEORS = 0.0D0
                  ELSE
                    JH2O2 = RJ_RES( COL, ROW, LAY, LH2O2 )
                    JHNO3 = RJ_RES( COL, ROW, LAY, LHNO3 )
                    JH2O2_HYDROMETEORS = REAL( JH2O2*PERMIN_TO_PERSEC, 8 )
                    JHNO3_HYDROMETEORS = REAL( JHNO3*PERMIN_TO_PERSEC, 8 )
                  END IF

C...in aqchem, H2SO4 gas is added to ASO4J
C...  mimic this for the ASO4GASJ tracking species
                  IF ( STM ) THEN
                    POLC( AEROSPC_MAP( ASO4GAS_IDX, 2 ) ) = POLC( AEROSPC_MAP( ASO4GAS_IDX, 2 ) )
     &                                                    + POLC( PRECURSOR_MAP( SULF_IDX ) )
                    CEND( AEROSPC_MAP( ASO4GAS_IDX, 2 ) ) = POLC( AEROSPC_MAP( ASO4GAS_IDX, 2 ) )
                  END IF

                  CALL AQ_MAP ( JDATE, JTIME, WTBAR, WCBAR, TBARC,
     &                          PBARC, CTHK1, AIRM, PRATE1, TAUCLD,
     &                          POLC, CEND, REMOV, REMOVAC, ALFA0,
     &                          ALFA2, ALFA3, COSZ )

#ifdef isam
                  DO SPC = 1, NSPC_SA ! general case
                    CSPC = MAP_SAtoCGR(SPC)
                    SA_SUM = SUM ( SA_POLC( SPC,: ) )
                    DO ITAG = 1, NTAG_SA
c                     IF ( POLC( CSPC ) .GT. 1.0E-30 ) THEN
                      IF ( SA_SUM .GT. 1.0E-25 ) THEN
                        SA_CEND( SPC, ITAG ) = SA_POLC( SPC,ITAG )
     &                                       * ( CEND( CSPC )
c    &                                         / MAX( 1.0E-25, POLC( CSPC ) ) )
     &                                         / SA_SUM )
                        SA_REMOV( SPC, ITAG ) = SA_POLC( SPC, ITAG )
     &                                        * ( REMOV( CSPC )
c    &                                          / MAX( 1.0E-25, POLC( CSPC ) ) )
     &                                          / SA_SUM )
                      ELSE !
                        SA_CEND( SPC, ITAG )  = 0.0
                        SA_REMOV( SPC, ITAG ) = 0.0
                      END IF
                    END DO
                  END DO

                  IF (S_SO4J .NE. 0 ) THEN ! sulfate case

                    DO ITAG = 1, NTAG_SA  ! sulfate from H2SO4
                      SA_DS4( ITAG ) = SA_POLC( S_SULF,ITAG )
                      SA_CEND( S_SULF,ITAG )  = 1.0E-30
                    END DO

                    DO ITAG = 1, NTAG_SA
                      SA_DS4( ITAG ) = SA_DS4( ITAG ) + DS4_SAVE ! total sulfate produced
     &                               * SA_POLC( S_SO2,ITAG )
     &                               / SUM ( SA_POLC( S_SO2,: ) )
                      SA_CEND( S_SO4J,ITAG ) = SA_POLC( S_SO4J,ITAG )  ! sulfate before removal
     &                                       + SA_DS4( ITAG )
                    END DO
 
                    SA_SUM = SUM ( SA_CEND( S_SO4J,: ) ) ! total apportioned sulfate before removal

                    DO ITAG = 1, NTAG_SA ! final sulfate removal and concentration
                      SA_REMOV( S_SO4J,ITAG ) = REMOV_SAVE
     &                                        * SA_CEND( S_SO4J,ITAG )
     &                                        / SA_SUM
                      SA_CEND( S_SO4J,ITAG ) = SA_CEND( S_SO4J,ITAG )
     &                                       - DEPSUM_SAVE
     &                                       * SA_CEND( S_SO4J,ITAG )
     &                                       / SA_SUM
                      SA_CEND( S_SO4J,ITAG ) = MAX ( SA_CEND( S_SO4J,ITAG ), 1.0E-30 )
                    END DO
          
                  END IF

                  IF (S_AORGCJ .NE. 0 ) THEN ! AORGCJ case
                    SA_DCSOA_GLY = 0.0
                    SA_DCSOA_MGLY = 0.0
                    DO ITAG = 1, NTAG_SA
                      IF ( S_GLY .NE. 0 ) THEN
                        SA_DCSOA_GLY( ITAG ) = DGLY1_SAVE
     &                               * SA_POLC( S_GLY,ITAG )
     &                               / MAX( 1.0E-25, SUM ( SA_POLC( S_GLY,: ) ) )
                      END IF
                      IF ( S_MGLY .NE. 0 ) THEN
                        SA_DCSOA_MGLY( ITAG ) = DMGLY1_SAVE
     &                               * SA_POLC( S_MGLY,ITAG )
     &                               / MAX( 1.0E-25, SUM ( SA_POLC( S_MGLY,: ) ) )
                      END IF
                      SA_CEND( S_AORGCJ,ITAG ) = SA_POLC( S_AORGCJ,ITAG ) ! AORGCJ before removal
     &                                         + SA_DCSOA_GLY( ITAG )
     &                                         + SA_DCSOA_MGLY( ITAG )
                    END DO
                    SA_SUM = MAX( 1.0E-25, SUM ( SA_CEND( S_AORGCJ,: ) ) ) ! total apportioned AORGCJ before removal
                    DO ITAG = 1, NTAG_SA ! final AORGCJ removal and concentration
                      SA_REMOV( S_AORGCJ,ITAG ) = REMOV_AORGC_SAVE
     &                                        * SA_CEND( S_AORGCJ,ITAG )
     &                                        / SA_SUM
                      SA_CEND( S_AORGCJ,ITAG ) = SA_CEND( S_AORGCJ,ITAG )
     &                                       - DEPSUM_AORGC_SAVE
     &                                       * SA_CEND( S_AORGCJ,ITAG )
     &                                       / SA_SUM
                      SA_CEND( S_AORGCJ,ITAG ) = MAX ( SA_CEND( S_AORGCJ,ITAG ), 1.0E-30 )
                    END DO
                  END IF
#endif
                END IF

C...convert removal change from moles/m**2 to kg/m**2 and kg/m**2 to kg/hectare

                DO I = 1,N_CGRID_SPC
                  IF ( CGRID_MASK_NUM( I ) .OR.
     &                 CGRID_MASK_SRF( I ) ) THEN
                     ! Aerosol Number (N m-2 -> N ha-1)
                     ! Aerosol Surface Area (m2 m-2 -> m2 ha-1)
                     REMOV( I ) = REMOV( I ) * M2PHA
#ifdef sens
                     DO NP = 1, NPMAX
                       S_REMOV( NP,I ) = S_REMOV( NP,I ) * M2PHA
                     END DO
#endif
                     ! ISAM does not track aerosol number or surface
                     ! area

                  ELSE
                     ! Gas and Aerosol Mass (moles m-2 -> kg ha-1)
                     REMOV( I ) = REMOV( I ) * CGRID_MW( I ) * M2PHA_OVER_GPKG
#ifdef sens
                     DO NP = 1, NPMAX
                       S_REMOV( NP,I ) = S_REMOV( NP,I ) * CGRID_MW( I ) * M2PHA_OVER_GPKG
                     END DO
#endif
#ifdef isam
                     CSPC = 0
                     CSPC = INDEXINT1(I,NSPC_SA,MAP_SAtoCGR(:))
                     IF ( CSPC .GT. 0 ) THEN
                       DO ITAG = 1, NTAG_SA
                         SA_REMOV( CSPC,ITAG ) = SA_REMOV( CSPC,ITAG ) * CGRID_MW( I )
     &                                          * M2PHA_OVER_GPKG
                       END DO
                     END IF
#endif
                  END IF


                END DO

C...load deposition amounts into the DEP array

                DO VAR = 1, N_SPC_WDEP
                  DEP( COL, ROW, VAR ) = DEP( COL, ROW, VAR )
     &                                 + REMOV( MAP_WDEPtoCGRID( VAR ) )
                END DO

C...load H+ concentration into the deposition array as well

                DEP( COL, ROW, N_SPC_WDEP+1 ) =
     &                         DEP( COL, ROW, N_SPC_WDEP+1 ) + REMOVAC
 
C...set cgrid to the ending concentrations

                DO SPC = 1, NSPCSD
                  IF ( SPC .NE. N_GC_SPCD ) THEN
                    CGRD( COL, ROW, LAY, SPC ) = CEND( SPC )
                  END IF
                END DO

#ifdef isam
                DO SPC = 1, NSPC_SA
                  DO ITAG = 1, NTAG_SA
                    TOT_SADEP( COL, ROW, SPC, ITAG ) = TOT_SADEP( COL, ROW, SPC, ITAG )
     &                                    + SA_REMOV( SPC, ITAG )
                    ISAM( COL,ROW,LAY,SPC,ITAG ) = SA_CEND( SPC, ITAG )
                  END DO
                END DO
#endif

#ifdef sens
                DO NP = 1, NPMAX

                  DO VAR = 1, N_SPC_WDEP
                    S_TOTDEP( COL, ROW, NP, VAR ) = S_TOTDEP( COL, ROW, NP, VAR )
     &                                            + S_REMOV( NP, MAP_WDEPtoCGRID( VAR ) )
                  END DO

                  S_TOTDEP( COL, ROW, NP, N_SPC_WDEP+1 ) =
     &                     S_TOTDEP( COL, ROW, NP, N_SPC_WDEP+1 ) + S_REMOVAC( NP )

                  DO SPC = 1, NSPCSD
                    IF ( SPC .NE. N_GC_SPCD ) THEN
                      SENGRID( COL, ROW, LAY, NP, SPC ) = S_CEND( NP, SPC )
                    END IF
                  END DO

                END DO
#endif

              END IF        ! Sum( QC,QR,QG ) > 0.00005 ?
            END DO       ! lay
          END IF      ! QCRGCOL > 0 ?

        END DO   ! col
      END DO   ! row

      RETURN

      END
