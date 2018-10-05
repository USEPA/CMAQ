
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
      MODULE MGEMIS
C-----------------------------------------------------------------------
C Description:

C Revision History:
C   03 Nov 15: B. Gantt and G. Sarwar: created the initial version for marine 
C      gas emissions
C   26 Sep 16: D. Wong: removed repeated SEAICE interpx statement
C   20 Feb 18: G. Sarwar: removed Br2, added DMS, and revised estimates of 
C              halocarbon and HOI/I2 emissions

      USE RUNTIME_VARS
      USE HGRD_DEFN
      USE EMIS_VARS

      IMPLICIT NONE

      LOGICAL, SAVE :: MGEM                  ! flag for marine gas emissions

      INTEGER, PARAMETER :: NMGSPC = 12

C Marine gas Emissions Rates
      REAL, ALLOCATABLE, SAVE :: VDEMIS_MG( :,:,: )   ! marine gas emission rates [moles/s]

      PUBLIC NMGSPC, VDEMIS_MG, MGEMIS_INIT, GET_MGEMIS, MGEM, MG_SPC, MGSPC_MAP
      PRIVATE

      INTEGER, SAVE              :: LGC_O3            ! pointer to O3 in CGRID

C Variables for the marine gas diagnostic file
      INTEGER :: NMGDIAG                              ! number of species in marine gas
                                                      ! diagnostic emission file
      REAL,    ALLOCATABLE       :: MGOUTD( : )       ! emission rates
      REAL,    ALLOCATABLE, SAVE :: MGBF( :,:,: )     ! marine gas emiss accumulator
      REAL,    ALLOCATABLE, SAVE :: WRMG( :,: )       ! marine gas emiss write buffer
      INTEGER, SAVE              :: MGSPC_MAP( NMGSPC )

! Species names in the speciated marine gas-emissions
      CHARACTER( 16 ), ALLOCATABLE :: WRMG_SPC( : )   ! species names

C Variables for handling marine gas emissions
      REAL,    ALLOCATABLE, SAVE :: OCEAN( :,: )   ! fractional sea water
      REAL,    ALLOCATABLE, SAVE :: SZONE( :,: )   ! fractional sea
      REAL,    ALLOCATABLE, SAVE :: CHLR( :,: )    ! chlorophyll
      REAL,    ALLOCATABLE, SAVE :: DMSL( :,: )    ! DMS in sea-water (nM)

      CHARACTER( 16 ) :: MG_SPC( NMGSPC)           ! emitted species

      DATA  MG_SPC  /  'MB3             ',
     &                 'MB2             ',
     &                 'MBC             ',
     &                 'MB2C            ',
     &                 'MBC2            ',
     &                 'CH3I            ',
     &                 'MIC             ',
     &                 'MIB             ',
     &                 'MI2             ',
     &                 'I2              ',
     &                 'HOI             ',
     &                 'DMS             ' /

C Variables interpolated from the meteorological input files
      REAL,    ALLOCATABLE, SAVE :: U10( :,: )      ! wind speed at 10m [m/s]
      REAL,    ALLOCATABLE, SAVE :: TSEASFC( :,: )  ! sea surface temp K
      REAL,    ALLOCATABLE, SAVE :: ILON( :,: )     ! longitude
      REAL,    ALLOCATABLE, SAVE :: SEAICE( :,: )   ! seaice fraction

      INTEGER I

C Domain decomposition info from emission and meteorology files
      INTEGER, SAVE :: GXOFF, GYOFF                 ! origin offset
      INTEGER, SAVE :: STRTCOL_O1, ENDCOL_O1, STRTROW_O1, ENDROW_O1
      INTEGER, SAVE :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3
      INTEGER, SAVE :: STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2

      CONTAINS

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      FUNCTION MGEMIS_INIT( JDATE, JTIME, TSTEP ) RESULT ( SUCCESS )

C Revision History:
C   03 Nov 15: B. Gantt and G. Sarwar: created the initial version for marine gas emissions

            USE UTILIO_DEFN
            USE HGRD_DEFN
            USE CGRID_SPCS
            USE RXNS_DATA

            IMPLICIT NONE
            INCLUDE SUBST_CONST                                     ! constants
            INCLUDE SUBST_FILES_ID                                  ! file name parameters

C Arguments:
            INTEGER, INTENT( IN ) :: JDATE, JTIME, TSTEP
            LOGICAL :: SUCCESS


            CHARACTER( 80 )       :: VARDESC                        ! env variable description

C Local Variables:

            CHARACTER( 16 ), SAVE :: PNAME = 'MGEMIS_INIT'
            CHARACTER( 96 ) :: XMSG = ' '
            CHARACTER(  16 ) :: VARNM
            INTEGER STATUS
            INTEGER S, N, L, K, V

C-----------------------------------------------------------------------

            SUCCESS = .TRUE.

C flag for marine gas emissions; this flag is set in the subroutine EM_FILE_INIT 
            IF ( .NOT. MGEM ) RETURN

C Allocate MG arrays using NMGSPC value
            EM_FILE_SURR( IMGSRM )%LEN = NMGSPC
            ALLOCATE( EM_FILE_SURR( IMGSRM )%ARRY ( NMGSPC ) )
            ALLOCATE( EM_FILE_SURR( IMGSRM )%CMAQ ( NMGSPC ) )
            ALLOCATE( EM_FILE_SURR( IMGSRM )%UNITS( NMGSPC ) )
            ALLOCATE( EM_FILE_SURR( IMGSRM )%PHASE( NMGSPC ) )
            ALLOCATE( EM_FILE_SURR( IMGSRM )%MW   ( NMGSPC ) )
            ALLOCATE( EM_FILE_SURR( IMGSRM )%USED ( NMGSPC ) )
            
            EM_FILE_SURR( IMGSRM )%ARRY  = MG_SPC
            EM_FILE_SURR( IMGSRM )%CMAQ  = MG_SPC
            EM_FILE_SURR( IMGSRM )%UNITS = 'MOLES/S'
            EM_FILE_SURR( IMGSRM )%PHASE = 'GAS'
            EM_FILE_SURR( IMGSRM )%MW    = 0.0
            EM_FILE_SURR( IMGSRM )%USED  = .FALSE.

            ALLOCATE ( VDEMIS_MG( NMGSPC,NCOLS,NROWS), STAT = STATUS )
            IF ( STATUS .NE. 0 ) THEN
               XMSG = '*** VDEMIS_MG, memory allocation failed'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            ALLOCATE ( OCEAN( NCOLS,NROWS ),
     &                 SZONE( NCOLS,NROWS ),
     &                 CHLR( NCOLS,NROWS ),
     &                 DMSL( NCOLS,NROWS ),
     &                 TSEASFC( NCOLS,NROWS ),
     &                 U10( NCOLS,NROWS ),
     &                 ILON( NCOLS,NROWS ),
     &                 SEAICE( NCOLS,NROWS ), STAT = STATUS )
            IF ( STATUS .NE. 0 ) THEN
              XMSG = '*** OCEAN,SZONE,TSEASFC,CHLR, or WSPD10 memory'
              CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
              SUCCESS = .FALSE.; RETURN
            END IF

C Open the ocean file, which contains the ocean and surf-zone fractions
            IF ( .NOT. OPEN3( OCEAN_1, FSREAD3, PNAME ) ) THEN
               XMSG = 'Open failure for ' // OCEAN_1
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            CALL SUBHFILE ( OCEAN_1, GXOFF, GYOFF,
     &                      STRTCOL_O1, ENDCOL_O1, STRTROW_O1, ENDROW_O1 )

C Open the 3D cross-point met file
            IF ( .NOT. OPEN3( MET_CRO_3D, FSREAD3, PNAME ) ) THEN
               XMSG = 'Open failure for ' // MET_CRO_3D
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                      STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

C Open the grid file, with contains latitude and longitude
            IF ( .NOT. OPEN3( GRID_CRO_2D, FSREAD3, PNAME ) ) THEN
               XMSG = 'Open failure for ' // GRID_CRO_2D
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            CALL SUBHFILE ( GRID_CRO_2D, GXOFF, GYOFF,
     &                      STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2 )

C Read longitude from the grid file
           IF ( .NOT. INTERPX( GRID_CRO_2D, 'LON', PNAME,
     &                         STRTCOLGC2,ENDCOLGC2,STRTROWGC2,ENDROWGC2,
     &                         1, 1, 0, 0, ILON ) ) THEN
              XMSG = 'Could not interpolate LON from ' // GRID_CRO_2D
              CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
           END IF

C Get index for surface ozone concentration 
            VARNM = 'O3'
            LGC_O3 = INDEX1( VARNM, N_GC_SPC, GC_SPC )
            IF ( LGC_O3 .LE. 0 ) THEN
               XMSG = 'Could not find ' // VARNM // 'in species table'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
            END IF

            IF (MGEM .AND. MGEMDIAG ) THEN                            ! Open the marine gas emission diagnostic file
C Determine number of marine gas species for diagnostic file
                NMGDIAG = NMGSPC

C Allocate diagnostic arrays
                ALLOCATE ( MGOUTD( NMGDIAG ),
     &                     WRMG_SPC( NMGDIAG ), STAT = STATUS )
                IF ( STATUS .NE. 0 ) THEN
                    XMSG = '*** MGOUTD or WRMG_SPC memory alloc failed'
                    CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
                    SUCCESS = .FALSE.; RETURN
                END IF

C Build diagnostic file write buffer (WRMG_SPC) array
                NMGDIAG = 0
                DO S = 1, NMGSPC
                   NMGDIAG = NMGDIAG + 1
                   WRMG_SPC( NMGDIAG ) = MG_SPC( S )
                END DO

C Open the marine gas emission dignostic file
                IF ( IO_PE_INCLUSIVE ) CALL OPMGEMIS ( STDATE, STTIME, TSTEP, NMGDIAG, WRMG_SPC )

                ALLOCATE ( MGBF( NMGDIAG,NCOLS,NROWS ),
     &                     WRMG( NCOLS,NROWS ), STAT = STATUS )
                IF ( STATUS .NE. 0 ) THEN
                   XMSG = '*** MGBF or WRMG memory allocation failed'
                   CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
                   SUCCESS = .FALSE.; RETURN
                END IF
                MGBF = 0.0

            END IF

         END FUNCTION MGEMIS_INIT

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         SUBROUTINE OPMGEMIS ( JDATE, JTIME, TSTEP, NMGDIAG, WRMG_SPC )

C 8 Aug 14 B.Gantt adapted from sea salt diagnostic

         USE GRID_CONF           ! horizontal & vertical domain specifications
         USE UTILIO_DEFN

         IMPLICIT NONE

         INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:
         INTEGER, INTENT( IN )         :: JDATE      ! current model date, coded YYYYDDD
         INTEGER, INTENT( IN )         :: JTIME      ! current model time, coded HHMMSS
         INTEGER, INTENT( IN )         :: TSTEP      ! output time step
         INTEGER, INTENT( IN )         :: NMGDIAG
         CHARACTER( 16 ), INTENT( IN ) :: WRMG_SPC( NMGDIAG )

C Local variables:
         CHARACTER( 16 ) :: PNAME = 'OPMGEMIS'
         CHARACTER( 96 ) :: XMSG = ' '

         INTEGER      V, L       ! loop induction variables

C-----------------------------------------------------------------------

C Try to open existing file for update
         IF ( .NOT. OPEN3( CTM_MGEM_1, FSRDWR3, PNAME ) ) THEN
            XMSG = 'Could not open CTM_MGEM_1 for update - '
     &           // 'try to open new'
            CALL M3MESG( XMSG )

C Set output file characteristics based on COORD.EXT and open diagnostic file
            FTYPE3D = GRDDED3
            SDATE3D = JDATE
            STIME3D = JTIME
            TSTEP3D = TSTEP
            CALL NEXTIME( SDATE3D, STIME3D, TSTEP3D ) !  start the next hour

            NVARS3D = NMGDIAG
            NCOLS3D = GL_NCOLS
            NROWS3D = GL_NROWS
            NLAYS3D = 1
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
!           VGTPUN3D = VGTPUN_GD ! currently, not defined
            DO L = 1, NLAYS3D + 1
               VGLVS3D( L ) = VGLVS_GD( L )
            END DO
!           GDNAM3D = GDNAME_GD
            GDNAM3D = GRID_NAME  ! from HGRD_DEFN

            DO V = 1, NMGDIAG
               VTYPE3D( V ) = M3REAL
               VNAME3D( V ) = WRMG_SPC( V )
               UNITS3D( V ) = 'mol s-1'
               VDESC3D( V ) = 'hourly ' // TRIM( VNAME3D( V ) )
     &                     // ' marine gas emission rate'
            END DO

            FDESC3D( 1 ) = 'hourly layer-1 marine gas emission rates'
            DO L = 2, MXDESC3
               FDESC3D( L ) = ' '
            END DO

C Open marine gas emissions diagnostic file
            IF ( .NOT. OPEN3( CTM_MGEM_1, FSNEW3, PNAME ) ) THEN
               XMSG = 'Could not create the CTM_MGEM_1 file'
               CALL M3EXIT( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
            END IF

         END IF

         RETURN

         END SUBROUTINE OPMGEMIS

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         SUBROUTINE GET_MGEMIS ( JDATE, JTIME, TSTEP, CGRID )

C GET_MGEMIS calculates the marine gas emission rates in a grid cell
C given the fractional grid-cell area covered by open ocean and surf zone

C Key Subroutines/Functions Called:  NONE

C Revision History:
C   03 Nov 15: B. Gantt and G. Sarwar: created the initial version from sea salt diagnostic file
C   20 Feb 18: G. Sarwar: removed Br2, added DMS, and revised estimates of halocarbon and HOI/I2 emissions
C
CReferences:
C MacDonald, S. M., Gómez Martín, J. C., Chance, R., Warriner, S., Saiz-Lopez, A., Carpenter, L. J.,
C and Plane, J. M. C.: A laboratory characterisation of inorganic iodine emissions from the sea
C surface: dependence on oceanic variables and parameterisation for global modelling, Atmos. Chem.
C Phys., 14, 5841-5852, doi:10.5194/acp-14-5841-2014, 2014.
C
C Sherwen, T., Evans, M. J., Carpenter, L. J., Andrews, S. J., Lidster, R. T., Dix, B., Koenig, T. K., 
C Sinreich, R., Ortega, I., Volkamer, R., Saiz-Lopez, A., Prados-Roman, C., Mahajan, A. S., and Ordóñez, C.: 
C Iodine's impact on tropospheric oxidants: a global model study in GEOS-Chem. Atmos. Chem. Phys., 2016, 16, 1161-1186.

C-----------------------------------------------------------------------
            USE UTILIO_DEFN
            USE AEROMET_DATA   ! Includes CONST.EXT
            USE ASX_DATA_MOD, ONLY: MET_DATA                 ! added by Sarwar
            USE CGRID_SPCS     ! CGRID mechanism species
            USE PCGRID_DEFN    ! get cgrid
            USE GRID_CONF

            IMPLICIT NONE

C Arguments
            INTEGER, INTENT( IN ) :: JDATE, JTIME, TSTEP( 3 )
            REAL, POINTER         :: CGRID( :,:,:,: )         !  concentrations

C Includes:
            INCLUDE SUBST_FILES_ID  ! file name parameters

C Parameters:
C.. Define Marine Gas indices

            INTEGER, PARAMETER :: IMB3  = 1
            INTEGER, PARAMETER :: IMB2  = 2
            INTEGER, PARAMETER :: IMBC  = 3
            INTEGER, PARAMETER :: IMB2C = 4
            INTEGER, PARAMETER :: IMBC2 = 5
            INTEGER, PARAMETER :: ICH3I = 6
            INTEGER, PARAMETER :: IMIC  = 7
            INTEGER, PARAMETER :: IMIB  = 8
            INTEGER, PARAMETER :: IMI2  = 9
            INTEGER, PARAMETER :: II2   = 10
            INTEGER, PARAMETER :: IHOI  = 11
            INTEGER, PARAMETER :: IDMS  = 12

            REAL, PARAMETER :: SECS2HR = 1.0 / 3600.0          ! hour per seconds
            REAL, PARAMETER :: C_FACT  = 1.0E-9 /(24.0*3600.0) ! factor to convert nmole/m2/day to mole/m2/sec
            REAL, PARAMETER :: LON2TZ  = 1.0 / 15.0            ! longitude to time zone, 1/degrees
            REAL, PARAMETER :: FHRLY(24) =
     &                         ( / 0.032, 0.032, 0.032, 0.033, 0.034, 0.036,
     &                             0.039, 0.044, 0.051, 0.057, 0.062, 0.064,
     &                             0.062, 0.057, 0.051, 0.044, 0.039, 0.036,
     &                             0.034, 0.033, 0.032, 0.032, 0.032, 0.032 / )

C Local Variables:
!            REAL    :: OFRAC              ! fractional seawater cover
!            REAL    :: SFRAC              ! fractional surf-zone cover
            REAL    :: CHLA
            REAL    :: SSTC
            REAL    :: SST            
            REAL    :: DMS_L              ! DMS in ocean water - nM  
            REAL    :: DMS                ! DMS in atmosphere                        
            REAL    :: SICE               ! SEAICE in a gird-cell

            REAL    :: SCN
            REAL    :: K600
            REAL    :: KW
            REAL    :: KA
            REAL    :: KT
            REAL    :: GAMMA
            REAL    :: ALPHA
            REAL    :: H_DMS

            CHARACTER( 16 ), SAVE :: PNAME = 'GET_MGEMIS'
            CHARACTER( 96 )       :: XMSG = ' '
            CHARACTER( 80 ) :: MSG                      ! Message

            INTEGER         :: C, R, L, N, V, S         ! loop indices

            INTEGER         :: LOGUNIT
            INTEGER         :: NVARS, NSTEPS
            INTEGER         :: LOCHR
            INTEGER, SAVE   :: WSTEP  = 0                      ! local write counter
            INTEGER         :: MDATE, MTIME                    ! MGEM write date&time
 
            REAL            :: IWSTEP                          ! reciprocal of local write counter
            REAL            :: DX1, DX2                        ! CX x1- and x2-cell widths
            REAL            :: O3, I_AQ                        ! SST in K, SSTC in C, O3 in ppb, SWPD10 in m/s
            REAL            :: DUMMY                           ! dummy variable for calculating marine gas emission
!            REAL            :: WDSCALO                         ! WDSCALO: 10 meter wind-speed-dependent scaling factors for emissions flux functions
!            REAL            :: SSTSCALO                        ! SSTSCALO: SST-dependent scaling factor for emissions from Jaegle et al. (2011) (SST in Celsius)
            REAL            :: LON                             ! grid-cell longitude
            REAL            :: WSPD10                          ! wind speed
            REAL            :: WSPD10_M5                       ! wind speed with a minimum speed of 5 m/s (used for HOI and I2 emission calculation)
            REAL            :: MARINE_AREA                     ! marine fraction of surface and later marine surface area
            REAL            :: CURRHR                          ! current GMT hour
            REAL, SAVE      :: A                               ! horizontal area of cells, m**2

            LOGICAL, SAVE   :: FIRST_TIME = .TRUE.

C----------------------------- Begin calc ------------------------------

           IF( FIRST_TIME ) THEN
C calculate grid-cell area
             IF ( GDTYP_GD .EQ. LATGRD3 ) THEN
                DX1 = DG2M * XCELL_GD ! in m.
                DX2 = DG2M * YCELL_GD
     &              * COS( PI180*( YORIG_GD + YCELL_GD*FLOAT( GL_NROWS/2 )))! in m.
             ELSE
                DX1 = XCELL_GD        ! in m.
                DX2 = YCELL_GD        ! in m.
             END IF
             A = DX1 * DX2                                      ! m2

             FIRST_TIME = .FALSE.
           END IF

           CURRHR = 1.0E-4 * REAL( JTIME )

C Assume MET_CRO_2D file is already opened

           IF ( .NOT. INTERPX( MET_CRO_2D, 'WSPD10', PNAME,
     &                         STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3,
     &                         1,1, JDATE, JTIME, U10 ) ) THEN
              XMSG = 'Could not find WSPD10 in ' // MET_CRO_2D //
     &               ' search for WIND10.'
              CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
              IF ( .NOT. INTERPX( MET_CRO_2D, 'WIND10', PNAME,
     &                            STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3,
     &                            1,1, JDATE, JTIME, U10 ) ) THEN
                 XMSG = 'Could not find WIND10 or WSPD10 in ' // MET_CRO_2D
                 CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
              END IF
           END IF

           IF ( .NOT. INTERPX( MET_CRO_2D, 'TSEASFC', PNAME,
     &                         STRTCOLMC3,ENDCOLMC3,STRTROWMC3,ENDROWMC3,
     &                         1,1, JDATE, JTIME, TSEASFC ) ) THEN
              XMSG = 'Could not find TSEASFC in ' // MET_CRO_2D //
     &                ' search for TEMPG.'
              CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
              IF ( .NOT. INTERPX( MET_CRO_2D, 'TEMPG', PNAME,
     &                            STRTCOLMC3,ENDCOLMC3,STRTROWMC3,ENDROWMC3,
     &                            1,1, JDATE, JTIME, TSEASFC ) ) THEN
                 XMSG = 'Could not find TEMPG or TSEASFC in ' //MET_CRO_2D
                 CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
              END IF
           END IF

           IF ( .NOT. INTERPX( MET_CRO_2D, 'SEAICE', PNAME,
     &                         STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3,
     &                         1,1, JDATE, JTIME, SEAICE ) ) THEN
              XMSG = 'Could not find SEAICE in ' // MET_CRO_2D //
     &               ' search for SEAICE '
              CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
           END IF

C Read fractional seawater and surf-zone coverage from the OCEAN file.
C Store results in the OCEAN and SZONE arrays.
           IF ( .NOT. INTERPX( OCEAN_1, 'OPEN', PNAME,
     &                         STRTCOL_O1,ENDCOL_O1, STRTROW_O1,ENDROW_O1,
     &                         1,1, JDATE, JTIME, OCEAN ) ) THEN
              XMSG = 'Could not interpolate OPEN from ' // OCEAN_1
              CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
           END IF

           IF ( .NOT. INTERPX( OCEAN_1, 'SURF', PNAME,
     &                         STRTCOL_O1,ENDCOL_O1, STRTROW_O1,ENDROW_O1,
     &                          1,1, JDATE, JTIME, SZONE ) ) THEN
              XMSG = 'Could not interpolate SURF from ' // OCEAN_1 //
     &               '.  Surf-zone emissions set to zero.'
              CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
           END IF

           IF ( .NOT. INTERPX( OCEAN_1, 'CHLO', PNAME,
     &                         STRTCOL_O1,ENDCOL_O1, STRTROW_O1,ENDROW_O1,
     &                         1,1, JDATE, JTIME, CHLR ) ) THEN
              XMSG = 'Could not interpolate CHLO from ' // OCEAN_1 //
     &               '.  Chlorophyll-a concentrations set to zero.'
              CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
           END IF

            IF ( .NOT. INTERPX( OCEAN_1, 'DMS', PNAME,
     &                          STRTCOL_O1,ENDCOL_O1, STRTROW_O1,ENDROW_O1,
     &                          1,1, JDATE, JTIME, DMSL ) ) THEN
               XMSG = 'Could not interpolate DMSL from ' // OCEAN_1 //
     &                '.  DMSL concentrations set to zero.'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            END IF

          IF ( MGEMDIAG .AND. WSTEP .EQ. 0 ) MGBF = 0.0

C Initialize marine gas output buffer
          DO R = 1, NROWS
             DO C = 1, NCOLS
                MARINE_AREA = ( OCEAN( C,R ) + SZONE( C,R ) )
                IF ( MARINE_AREA .GT. 0.0 .AND. SEAICE( C, R ) .LE. 0.0) THEN
!            convert area fraction to an actual area
                     MARINE_AREA = MARINE_AREA * A

                     CHLA      = CHLR ( C,R )
                     WSPD10    = U10( C,R )
                     WSPD10_M5 = MAX (5.0, WSPD10)
                     SST       = TSEASFC (C,R)
!                    SSTC    = SST - 273.15
                     SSTC      = MIN( (SST - 273.15), 30.0)                          
                     LON       = ILON (C,R)
                     O3       = 1000.0 * CGRID(C, R, 1, LGC_O3)
                     DMS_L     = DMSL( C, R )   

C.. calculate iodide in water
                     I_AQ = 1.46E6 * EXP( -9134.0 / SST)

!..    calculate parameters needed for DMS emissions 

!..    Calculate Schmidt number of DMS (Saltzman et al, JGR, 1993)
       SCN = 2674.0 - 147.12 * SSTC + 3.726 * SSTC * SSTC - 0.038 * SSTC * SSTC * SSTC

!..    Calculate water-side DMS gas-transfer velocity following Liss and Merlivat,  1986 (unit - cm/hr)
       IF ( WSPD10 .LE. 3.6 ) THEN 
          KW = 0.17 * WSPD10 / SQRT(SCN / 600.0)
        ELSE IF ( WSPD10 .GT. 3.6 .AND. WSPD10 .LE. 13.0 ) THEN
          KW = ( 2.85 * WSPD10 - 9.65 ) / SQRT(SCN / 600.0)
        ELSE IF ( WSPD10 .GT. 13.0) THEN
          KW = ( 5.9 * WSPD10 - 49.3 ) / SQRT(SCN / 600.0)
       ENDIF
    
!..    Calculate air-side DMS gas-transfer velocity (McGills et al, JGR, 2000) (unit - cm/hr)
       KA = 659.0 * WSPD10 / SQRT(62.0/18.0)
            
!..    Calculate Henry's Law Coefficient of DMS expressed as C_air/C_water following Dacey et al, GRL, 1984  (unit - atm . L /mole)
       H_DMS = EXP ( 12.64 - 3547.0/SST )                             
       
!..    Calculate Solubility Coeffcient of DMS expressed as C_water/C_air following McGills et al, JGR, 2000 - (unit dimensionless ) 
!..    Dacey et al. calculates Henry's Law Coefficient as C_air/C_water, thus, an inverse is taken for unit consistency 
!..    and then it is converted into dimensionless unit
       ALPHA = 0.082058 * SST / H_DMS                                

!..    Calculate Atmospheric Gradient Fraction following Lan et al,  Global Biogeochemical Cycles, 2011
       GAMMA = 1.0 /( 1.0 + KA / (ALPHA * KW))  

!..    Calculate total gas transfer velocity for DMS following Lan et al,  Global Biogeochemical Cycles, 2011 
       KT = KW * (1.0 - GAMMA)

C.. calculate local hour
                     LOCHR = NINT(CURRHR + LON * LON2TZ )
                     IF (LOCHR .LT.  1) LOCHR = LOCHR + 24
                     IF (LOCHR .GT. 24) LOCHR = LOCHR - 24

                     DUMMY = 2.50E-8 * MARINE_AREA * FHRLY(LOCHR) * CHLA * SECS2HR  

C.. Calculate MB3 emission rate in mole/s
                     VDEMIS_MG( IMB3, C, R ) = DUMMY * 2.00

C.. Calculate MB2 emission rate in mole/s
                     VDEMIS_MG( IMB2, C, R ) = DUMMY * 0.50

C.. Calculate MBC emission rate in mole/s
                     VDEMIS_MG( IMBC, C, R ) = DUMMY * 0.08

C.. Calculate MB2C emission rate in mole/s
                     VDEMIS_MG( IMB2C, C, R ) = DUMMY * 0.12

C.. Calculate MBC2 emission rate in mole/s
                     VDEMIS_MG( IMBC2, C, R ) = DUMMY * 0.10

C.. Calculate CH3I emission rate in mole/s
                     VDEMIS_MG( ICH3I, C, R ) = DUMMY * 1.60

C.. Calculate MIC emission rate in mole/s
                     VDEMIS_MG( IMIC, C, R ) = DUMMY * 1.41

C.. Calculate MIB emission rate in mole/s
                     VDEMIS_MG( IMIB, C, R ) = DUMMY * 0.42

C.. Calculate MI2 emission rate in mole/s
                     VDEMIS_MG( IMI2, C, R ) = DUMMY * 0.46

C.. Calculate I2 emission rate in mole/s (Macdonald et al., ACP, 14, 5841-5852, 2014)
                     DUMMY = O3 * (I_AQ**1.3) * (1.74E9 - 6.54E8*LOG(WSPD10_M5)) ! NMOL/M2/D
                     DUMMY = MARINE_AREA * DUMMY * C_FACT
                     VDEMIS_MG( II2, C, R ) = MAX( 0.0, DUMMY )

C.. Calculate HOI emission rate in mole/s (Macdonald et al., ACP, 14, 5841-5852, 2014)
                     DUMMY = SQRT(I_AQ)
                     DUMMY = O3
     &                     * ((4.15E5*DUMMY - 20.6)/WSPD10_M5 - 23600.0*DUMMY)  ! NMOL/M2/D
                     DUMMY = MARINE_AREA * DUMMY * C_FACT
                     VDEMIS_MG( IHOI, C, R ) = MAX( 0.0, DUMMY )

C .. Calculate DMS emission rate in mole/s
                     DUMMY = MARINE_AREA *  DMS_L * KT * 2.78E-12
                     VDEMIS_MG( IDMS, C, R ) = MAX ( 0.0, DUMMY)

                ELSE
                     DO N = 1, NMGSPC
                        VDEMIS_MG( N, C, R ) = 0.0
                     END DO
                     CYCLE
                END IF

C Update the MGBF array, for writing the diagnostic marine gas emission file
                IF ( MGEMDIAG ) THEN
                   V = 0
                   DO S = 1, NMGSPC
                      V = V + 1
                      MGOUTD( V ) = VDEMIS_MG( S,C,R )
                   END DO

                   DO S = 1, NMGDIAG
                      MGBF( S,C,R ) = MGBF( S,C,R ) + MGOUTD( S )
     &                              * REAL( TIME2SEC ( TSTEP( 2 ) ) )
                  END DO
                END IF  ! MGEMDIAG

             END DO   ! C
         END DO   ! R


C If last call this hour, write out the total MG emissions [moles/s].
C Then reset the MG emissions array and local write counter.
         IF ( MGEMDIAG ) THEN

             WSTEP = WSTEP + TIME2SEC( TSTEP( 2 ) )

             IF ( WSTEP .GE. TIME2SEC( TSTEP( 1 ) ) ) THEN
                IF ( .NOT. CURRSTEP( JDATE, JTIME, STDATE, STTIME, TSTEP( 1 ),
     &                                MDATE, MTIME ) ) THEN
                     XMSG = 'Cannot get step date and time'
                     CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
                END IF
                CALL NEXTIME( MDATE, MTIME, TSTEP( 1 ) )
                IWSTEP = 1.0 / REAL( WSTEP )
#ifdef parallel_io
                IF ( .NOT. IO_PE_INCLUSIVE ) THEN
                  IF ( .NOT. OPEN3( CTM_MGEM_1, FSREAD3, PNAME ) ) THEN
                       XMSG = 'Could not open ' // TRIM(CTM_MGEM_1)
                       CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
                  END IF
                END IF
#endif
                DO V = 1, NMGDIAG
                   DO R = 1, NROWS
                      DO C = 1, NCOLS
                         WRMG( C,R ) = MGBF( V,C,R ) * IWSTEP
                      END DO
                   END DO
                   IF ( .NOT. WRITE3( CTM_MGEM_1, WRMG_SPC( V ),
     &                        MDATE, MTIME, WRMG ) ) THEN
                     XMSG = 'Couldnt write ' // CTM_MGEM_1 // 'file'
                     CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
                   END IF
               END DO
               WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )')
     &                      'Timestep written to', CTM_MGEM_1,
     &                      'for date and time', MDATE, MTIME
               WSTEP = 0
               MGBF = 0.0
             END IF

          END IF  ! MGDIAG

         RETURN

       END SUBROUTINE GET_MGEMIS

      END MODULE MGEMIS
