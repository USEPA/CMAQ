
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
C $Header: /project/work/rep/arc/CCTM/src/emis/emis/EMIS_DEFN.F,v 1.10 2011/10/21 16:10:46 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE EMIS_DEFN

C-----------------------------------------------------------------------
C Function: emissions interface to the chemistry-transport model

C Revision History:
C     28 Jul 2006 J.Young: initial implementation
C     18 Aug 2007 J.Young: move beis part to separate module; add plume rise
C     23 Sep 2009 B.Hutzell: modified algorithm that loads gas emissions from point
C                 sources into VDEMIS array to enable multi-use of an emission species
C     26 Jan 2010 J.Young: fix bug overwriting point source layer 1 NH3; inline rdemis
C     07 Jan 2011 B.Hutzell: updated for namelist definition of model species
C     16 Feb 2011 S.Roselle: replaced I/O API include files with UTILIO_DEFN;
C                            removed deprecated TRIMLEN
C      6 Apr 2011 J.Young, R.Pinder: add lightning NO emissions capability
C     11 May 2011 D.Wong: incorporated twoway model implementation
C      5 Jun 2012 J.Bash: Added support for NH3 bidirectional exchange. Fertilizer
C                         sector emissions are subtracted from the total NH3 emissions
C                         if the CTM_ABFLUX flag is set
C     07 Nov 14 J.Bash: Updated for the ASX_DATA_MOD shared data module. 
C     24 Feb 16 B.Murphy: Generalize scaling of point source species based on
C                         mapping model species, not point source species
C     03 Mar 16 B.Gantt/G. Sarwar: incorporated halogen emissions
C     08 Aug 2016 B.Murphy: Neglect fire emissions for pcVOC
C     12 Jan 2017 B.Murphy: Remove warning when model species are not
C                           read in correctly. Invoke error and model stop when model 
C                           species are not found on any emission file
C-----------------------------------------------------------------------

      USE GRID_CONF           ! horizontal & vertical domain specifications      

      IMPLICIT NONE

      REAL,    ALLOCATABLE, SAVE :: VDEMIS( :,:,:,: ) ! total emissions array
      REAL,    ALLOCATABLE, SAVE :: VDEMIS_TMP( :,:,:,: ) ! total emissions array

      INTEGER,              SAVE :: N_EMIS_SPC ! global emissions species
      INTEGER,              SAVE :: EMLAYS     ! no. of emis integration layers
                                               ! EMLAYS must be .LE. NLAYS
      INTEGER,              SAVE :: ARLYRS     ! no. of area emission layers
      LOGICAL,              SAVE :: EMISCHK ! Let model proceed even if Emis Surrogates 
                                               !are not properly defined
      INTEGER,              SAVE :: EMLYRS     ! no. of layers on base emis input file
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: TRACSPC( : )  ! Variables from Tracer Emission File
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: AREASPC( : )  ! Variables straight from the Area Emission File
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: EM_SPEC  ( : )! Vector of Internal CMAQ Species names for 
                                                          ! each emission operation
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: EM_ARSPEC( : )! Species to be read from Area emissions file 
                                                          ! reordered to match the order of the emitted 
                                                          ! species master list
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: EM_PTSPEC(:,:)! Species to be read from Area emissions file 
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: EM_TRSPEC( : )! Species to be read from Area emissions file 

      REAL(8), ALLOCATABLE, SAVE :: EM_FAC_AR( : )   ! Emissions Factor for Area Species
      REAL(8), ALLOCATABLE, SAVE :: EM_FAC_PT( :,: ) ! Emissions Factor for Point Species
      REAL(8), ALLOCATABLE, SAVE :: EM_FAC_BEIS( : ) ! Emissions Factor for Biogenic Species
      REAL(8), ALLOCATABLE, SAVE :: EM_FAC_MG( : )   ! Emissions Factor for Marine Gas Species
      REAL(8), ALLOCATABLE, SAVE :: EM_FAC_LTNG( : ) ! Emissions Factor for Lightning Species
      REAL(8), ALLOCATABLE, SAVE :: EM_FAC_TR( : )   ! Emissions Factor for Tracer Species

      PUBLIC VDEMIS, N_EMIS_SPC, EMLAYS, EMLYRS, TRACSPC, AREASPC,  ! Variables
     &       EMIS_INIT, GET_EMIS, EMISCHK, EM_ARSPEC, EM_PTSPEC
      
      PRIVATE
                                           ! a 2d file if inline, otherwise a 3d file
      INTEGER, SAVE :: NEMIS_AE   ! no. of aero emis species

      LOGICAL, SAVE :: EM_TRAC = .FALSE. ! do tracer emissions?
      REAL,    SAVE :: CNVTE      ! combined conversion factor
      REAL,    SAVE :: DT         ! TSTEP (output) in sec

      REAL,    ALLOCATABLE, SAVE :: BUFF( :,:,: )        ! read buffer
      REAL,    ALLOCATABLE, SAVE :: BUFF2( :,:,: )        ! read buffer
      REAL,    ALLOCATABLE, SAVE :: FERT( :,:,: )        ! read buffer

      INTEGER, SAVE :: STRT_GC, FINI_GC, STRT_AE, FINI_AE,
     &                 STRT_NR, FINI_NR, STRT_TR, FINI_TR
      INTEGER, SAVE :: SDATE, STIME  ! scenario start date/time (beis)
      INTEGER       :: LDATE, LTIME  ! step start date/time (beis)
      INTEGER, SAVE :: NDATE, NTIME  ! step next date/time (beis)

      CONTAINS

C-----------------------------------------------------------------------
         FUNCTION EMIS_INIT ( JDATE, JTIME, TSTEP ) RESULT ( SUCCESS )

         USE CGRID_SPCS          ! CGRID mechanism species
         USE BEIS_DEFN           ! biogenic emissions
         USE MGEMIS              ! marine gas emissions
         USE LTNG_DEFN           ! NO emissions from lightning strikes
         USE PT3D_DEFN           ! plume rise emissions
         USE UTILIO_DEFN         ! I/O API
         USE AERO_EMIS           ! inherits GRID_CONF
         USE AERO_DATA, ONLY:   ! access subroutine map_pmemis
         USE DEPV_DEFN, ONLY: ABFLUX ! ammonia bidi flag

         IMPLICIT NONE

C Includes:
         INCLUDE SUBST_CONST     ! constants

C Arguments:
         INTEGER, INTENT( IN ) :: JDATE, JTIME, TSTEP   ! TSTEP is output time step (HHMMSS)
         LOGICAL :: SUCCESS

C Parameters:
         REAL, PARAMETER :: CMLMR = 1.0E+06       ! ppmV/Molar Mixing Ratio

C Local Variables:
         REAL    CONVEM          ! conversion for emissions rates to Kg/s
         REAL    DX1, DX2        ! CX x1- and x2-cell widths

         CHARACTER( 16 ), SAVE :: CTM_EMLAYS = 'CTM_EMLAYS'    ! env var for no. of
                                                               ! emiss. layers
         
         CHARACTER( 16 ) :: PNAME = 'EMIS_INIT'
         CHARACTER( 80 ) :: VARDESC   ! env variable description
         CHARACTER( 120 ) :: XMSG = ' '
         INTEGER V, LOGDEV, STATUS

C-----------------------------------------------------------------------

         SUCCESS = .TRUE.

         LOGDEV = INIT3()

         IF ( GDTYP_GD .EQ. LATGRD3 ) THEN
            DX1 = DG2M * XCELL_GD ! in m.
            DX2 = DG2M * YCELL_GD
     &          * COS( PI180*( YORIG_GD + YCELL_GD*FLOAT( GL_NROWS/2 )))! in m.
         ELSE
            DX1 = XCELL_GD        ! in m.
            DX2 = YCELL_GD        ! in m.
         END IF

C Combined Gas Emssions Conversion Factor
         CNVTE = CMLMR * CONVEM * MWAIR / ( DX1 * DX2 )

C Initialize Online Biogenic Emissions
         IF ( .NOT. BEIS_INIT( JDATE, JTIME, TSTEP ) ) THEN
            XMSG = 'Failure initializing biogenics emissions module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C Initialize Online Marine Gas Emissions
         IF ( .NOT. MGEMIS_INIT( JDATE, JTIME, TSTEP ) ) THEN
            XMSG = 'Failure initializing marine gas emissions module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C Open Area Emissions files
         CALL OPEMIS ( JDATE, JTIME, N_GC_EMIS, EM_TRAC, CONVEM, ARLYRS )

C Get number of emissions layers
         IF ( ARLYRS .LE. 1 ) THEN   ! better be inline plume rise run
            EMLAYS = NLAYS   ! env var default
         ELSE 
            EMLAYS = ARLYRS  ! for 3D gridded file
         END IF

         VARDESC = 'Number of emission layers'
         EMLAYS = ENVINT( CTM_EMLAYS, VARDESC, EMLAYS, STATUS )
         IF ( STATUS .NE. 0 ) THEN
            WRITE( LOGDEV, '(5X, A)' ) VARDESC
            IF ( STATUS .EQ. 1 ) THEN
               XMSG = 'Environment variable improperly formatted'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            ELSE
               XMSG = 'Environment variable not set or empty ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
            END IF
         END IF 
         
         EMLAYS = MIN ( EMLAYS, NLAYS )

         WRITE( LOGDEV,1009 ) EMLAYS, NLAYS
1009     FORMAT( / 10X, 'Number of Emissions Layers:         ', I3
     &           / 10X, 'out of total Number of Model Layers:', I3 )

C Initialize Online Lightning NOx Emissions
         IF ( .NOT. LTNG_INIT( JDATE, JTIME, TSTEP, EMLAYS ) ) THEN
            XMSG = 'Failure initializing lightning emissions module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C Initialize 3D Point Source Emissions 
C (N_EMIS_SPC+1 serves as the null map for PTEM_MAP)
         IF ( .NOT. PT3D_INIT( N_EMIS_SPC, EMLAYS, JDATE, JTIME, TSTEP ) ) THEN
            XMSG = 'Failure initializing plume rise emissions module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C Initialize Aerosol Emissions         
         IF ( .NOT. AERO_EMIS_INIT( JDATE, JTIME, TSTEP,N_EMIS_SPC, 
     &              NEMIS_AE, EMLAYS ) ) THEN
            XMSG = 'Failure initializing aerosol emissions module'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
 
                   
C Map the Emissions Species Available on the Input Files To the
C Surrogates Identified by the User via the Namelists and Stop the model
C or Print Warnings if Mistakes Are Made.
         CALL EMIS_SPC_MAP( JDATE, JTIME )

C Allocate Space for Master Emissions Computation
         ALLOCATE ( VDEMIS( N_EMIS_SPC,EMLAYS,NCOLS,NROWS ),STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = 'VDEMIS memory allocation failed'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF
         ALLOCATE ( VDEMIS_TMP( N_EMIS_SPC,EMLAYS,NCOLS,NROWS ),STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = 'VDEMIS_TMP memory allocation failed'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF
 
C Allocate Buffer space for Reading Emissions
         ALLOCATE ( BUFF( NCOLS,NROWS,EMLAYS ), STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = 'BUFF memory allocation failed'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF
         BUFF = 0.0   

         ALLOCATE ( BUFF2( NCOLS,NROWS,EMLAYS ), STAT = STATUS )
          IF ( STATUS .NE. 0 ) THEN
            XMSG = 'BUFF memory allocation failed'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF 
         BUFF2= 0.0   

C Return From Initialization
         SUCCESS = .TRUE.; RETURN

         END FUNCTION EMIS_INIT

C-----------------------------------------------------------------------
         SUBROUTINE GET_EMIS ( JDATE, JTIME, TSTEP, CONVPA, CGRID )

C NEW APPROACH:
C Apply NML factors to all *input* emissions and inline plume rise, aero emis
C biog, SeaSalt, Dust, and Lightning NO
C GET_AERO_EMIS has to apply factors - don`t do it at this level.

         USE CGRID_SPCS          ! CGRID mechanism species
         USE AERO_EMIS           ! inherits GRID_CONF
         USE BEIS_DEFN           ! biogenic emissions
         USE BIOG_EMIS, ONLY: MSPCS
         USE MGEMIS              ! marine gas emissions
         USE PT3D_DEFN           ! plume rise emissions
         USE LTNG_DEFN           ! lightning NO emissions
         USE UTILIO_DEFN
         USE DEPV_DEFN, ONLY: ABFLUX ! ammonia bidi flag
         USE ASX_DATA_MOD, ONLY: MET_DATA, GRID_DATA
         USE RXNS_DATA, ONLY: MECHNAME
         USE PRECURSOR_DATA, ONLY : SULF_IDX, PRECURSOR_MW
         USE AERO_DATA, ONLY : AERONUM_MAP, AEROSRF_MAP

         IMPLICIT NONE

C Includes:
         INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:
         INTEGER, INTENT( IN ) :: JDATE, JTIME  ! date (YYYYDDD), time (HHMMSS)
         INTEGER, INTENT( IN ) :: TSTEP( 3 )    ! time step vector (HHMMSS)
         REAL,    INTENT( IN ) :: CONVPA        ! conversion factor to pressure in Pascals
         REAL, POINTER :: CGRID( :,:,:,: )

C Local Variables:
         REAL             CNVTC         ! combined conversion factor
         REAL             CNVTI         ! intermediate combined conv. factor
         REAL,    SAVE :: CNVTP         ! intermediate combined conv. factor
         REAL             DELT          ! interpolation factor
         INTEGER          C, R, L, N, S, V, ISPC ! loop induction variables
         INTEGER          S_STRT, S_END ! substitute loop induction variables

         CHARACTER( 16 ) :: VNAME
         CHARACTER( 16 ) :: PNAME = 'GET_EMIS'
         CHARACTER( 120 ) :: XMSG = ' '
         LOGICAL, SAVE :: FIRSTIME = .TRUE.
         INTEGER          GXOFF, GYOFF         ! global origin offset from file
         INTEGER, SAVE :: STARTCOL, ENDCOL, STARTROW, ENDROW

C-----------------------------------------------------------------------

         IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
C Get domain decomp info from the emissions file
            CALL SUBHFILE ( EMIS_1, GXOFF, GYOFF, STARTCOL, ENDCOL, STARTROW, ENDROW )
            CNVTP = CONVPA * CNVTE

         END IF    !FirstTime

C Read & Interpolate Emissions 

         VDEMIS     = 0.0   ! array assignment

          DO ISPC = 1, N_EMIS_SPC 
            VNAME = EM_ARSPEC( ISPC )
            IF ( VNAME .EQ. '' ) CYCLE

            BUFF = 0.0   ! array assignment
C                                       vvvvvvv - emis surrogate names
            IF ( .NOT. INTERPX( EMIS_1, VNAME, PNAME,
     &                          STARTCOL,ENDCOL, STARTROW,ENDROW, 1,ARLYRS,
     &                          JDATE, JTIME, BUFF ) ) THEN
               XMSG = 'Could not read '
     &              // TRIM( VNAME ) // ' from ' // EMIS_1
               IF ( .NOT. EMISCHK ) CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            END IF

            !READ Non-Carbon Organic Matter From Gas Emissions
            IF ( VNAME .EQ. 'POC' ) THEN
                 BUFF2 = 0.0
                 IF ( .NOT. INTERPX( EMIS_1, 'PNCOM', PNAME,
     &                          STARTCOL,ENDCOL, STARTROW,ENDROW, 1,ARLYRS,
     &                          JDATE, JTIME, BUFF2 ) ) THEN
                 XMSG = 'Could not read '
     &               // TRIM( VNAME ) // ' from ' // EMIS_1
                 IF ( .NOT. EMISCHK ) CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
              END IF  
              BUFF = BUFF + BUFF2
            ENDIF
 
            ! READ Sulfuric Acid From Gas Emissions and Assign to Aerosols
            IF ( VNAME .EQ. 'PSO4' ) THEN
                 BUFF2 = 0.0
                 IF ( .NOT. INTERPX( EMIS_1, 'SULF', PNAME,
     &                          STARTCOL,ENDCOL, STARTROW,ENDROW, 1,ARLYRS,
     &                          JDATE, JTIME, BUFF2 ) ) THEN
                 XMSG = 'Could not read '
     &               // TRIM( VNAME ) // ' from ' // EMIS_1
                 IF ( .NOT. EMISCHK ) CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
              END IF  
              BUFF = BUFF + BUFF2 * REAL( PRECURSOR_MW( SULF_IDX ), 4 )
            ENDIF
 
            ! Zero Out Sulfuric Acid Emissions Since they have been
            ! added to particulate sulfate
            IF ( VNAME .EQ. 'SULF' ) THEN
              BUFF = 0.0
            END IF

            ! Subtract Fertilizer Emissions from NH3 source if running
            ! BIDIrectional Exchange Module
            IF ( ABFLUX .AND. VNAME .EQ. 'NH3' ) THEN
               BUFF2 = 0.0   ! array assignment
               IF ( .NOT. INTERPX( EMIS_1, 'NH3_FERT', PNAME,
     &                          STARTCOL,ENDCOL, STARTROW,ENDROW, 1,ARLYRS,
     &                          JDATE, JTIME, BUFF2 ) ) THEN
                  XMSG = 'Could not read NH3_FERT from' // EMIS_1
                  IF ( .NOT. EMISCHK ) 
     &                 CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
               END IF
               BUFF( C,R,1 ) = BUFF( C,R,1 ) - BUFF2( C,R,1 )
               IF ( ANY( BUFF .LT. 0.0 ) ) THEN
                    XMSG = 'Negative values in NH3 emissions' 
     &                       //'Error in NH3 Bidi emissions'                          
                    CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF
            END IF ! ABFLUX & Ammonia
 
C Store all emissions in mol/sec or g/sec and convert to ppmv/s later) 
            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  DO L = 1, ARLYRS
                     VDEMIS_TMP( ISPC,L,C,R ) = EM_FAC_AR( ISPC ) * BUFF( C,R,L )
                  END DO
               END DO
            END DO

           END DO   ! ISPC

           !CALL AEROSOL DISTRIBUTION ROUTINE
           VDEMIS = VDEMIS + VDEMIS_TMP

C Retrieve BEIS Emissions

         IF ( BIOGEMIS ) THEN
            CALL GET_BEIS ( JDATE, JTIME, TSTEP )

C tmpbeis312 is based on hourly input - need to interpolate return value
C merge: VDEMIS_BI in mol/sec. Convert to ppm/sec

            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  DO ISPC = 1, N_EMIS_SPC
                     V = BEIS_MAP( ISPC )
                     IF ( V .GT. 0 ) THEN
                        VDEMIS( ISPC,1,C,R ) = VDEMIS( ISPC,1,C,R )
     &                                    + EM_FAC_BEIS( ISPC )
     &                                            * VDEMIS_BI( V,C,R )
                     END IF
                  END DO
               END DO
            END DO

         END IF

C marine gas emissions
         IF ( MGEM ) THEN
            CALL GET_MGEMIS ( JDATE, JTIME, TSTEP, CGRID )

            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  DO ISPC = 1, N_EMIS_SPC
                     V = MGSPC_MAP( ISPC )
                     IF ( V .GT. 0 ) THEN
                        VDEMIS( ISPC,1,C,R ) = VDEMIS( ISPC,1,C,R )
     &                                    + EM_FAC_MG( ISPC ) 
     &                                            * VDEMIS_MG( V,C,R)
                      END IF
                  END DO
               END DO
            END DO

         END IF

C lightning NO

         IF ( LTNG_NO ) THEN
            CALL GET_LTNG ( JDATE, JTIME, TSTEP )

            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  DO L = 1, EMLAYS
                     ISPC = INDEX1( 'NO', N_EMIS_SPC, EM_SPEC )
                     VDEMIS( ISPC,L,C,R ) = VDEMIS( ISPC,L,C,R )
     &                                 +  EM_FAC_LTNG( ISPC )
     &                                         * VDEMIS_LT( C,R,L )
                  END DO
               END DO
            END DO

         END IF

C Point Sources
         IF ( PT3DEMIS ) THEN
            CALL GET_PT3D_EMIS ( EM_PTSPEC, EM_FAC_PT, EM_SPEC, JDATE, JTIME, TSTEP )

C merge: VDEMIS_PT in mol/sec. Convert to ppm/sec
C If aerosols, then the PM emissions obtained from GET_PT3D_EMIS need to be
C accessed by GET_AERO_EMIS to be "speciated" and merged with the AE emissions.
            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  DO L = 1, EMLAYS
                     DO ISPC = 1, N_EMIS_SPC
                        VDEMIS( ISPC,L,C,R ) = VDEMIS( ISPC,L,C,R ) + VDEMIS_PT( C,R,L,ISPC )
                     END DO
                  END DO
               END DO
            END DO
         END IF

C Aerosol Emissions - all units conversions done in GET_AERO_EMIS for aerosols
C GET_AERO_EMIS in f90 module AERO_EMIS

         IF ( N_AE_SPC .GT. 0 ) THEN
            CALL GET_AERO_EMIS ( JDATE, JTIME, TSTEP, N_EMIS_SPC, VDEMIS )
         END IF
             
C Tracer Gases (conversion to ppmv/s)
         DO ISPC = 1, N_EMIS_SPC
            VNAME = EM_TRSPEC( ISPC )
            IF ( VNAME .EQ. '' ) CYCLE
            BUFF = 0.0   ! array assignment
C                                            vvvvvvv - emis surrogate names
            IF ( .NOT. INTERPX( EMIS_TRAC_1, VNAME, PNAME,
     &                          STARTCOL,ENDCOL, STARTROW,ENDROW, 1,EMLAYS,
     &                          JDATE, JTIME, BUFF ) ) THEN
               XMSG = 'Could not read  ' // TRIM( VNAME )
     &              //' from ' // EMIS_TRAC_1
               IF ( .NOT. EMISCHK ) CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            END IF

            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  DO L = 1, EMLAYS
                     VDEMIS( ISPC,L,C,R ) = EM_FAC_TR( ISPC ) * BUFF( C,R,L )
                  END DO
               END DO
            END DO
         END DO

C Convert All Emissions From mol/s to ppmv/s
         DO R = 1, MY_NROWS
            DO C = 1, MY_NCOLS
               DO L = 1, EMLAYS
                  !MAKE SURE TO IGNORE AEROSOL NUMBER AND SURFACE AREA 
                  CNVTC = CNVTP * Grid_Data%RDX3F( L ) * Met_Data%RRHOJ( C,R,L )
                  WHERE( .NOT.(AERONUM_MAP .OR. AEROSRF_MAP ) )
     &                   VDEMIS( :,L,C,R ) = CNVTC * VDEMIS( :,L,C,R ) 
               END DO
            END DO
         END DO
 

         RETURN

         END SUBROUTINE GET_EMIS


C-----------------------------------------------------------------------
         SUBROUTINE EMIS_SPC_MAP( JDATE, JTIME )

C Check the chemical species from the namelists and AERO_DATA against
C the species that are available on the actual emissions input files. If
C they do not agree, print warngings or crash the program depending on
C how severe the error is.
C
C    16 Mar 2017  B.Murphy     Created Subroutine
C-----------------------------------------------------------------------

         USE BEIS_DEFN, only : BIOGEMIS, ! Online Biogenic Emissions Env Variable
     &                         BEIS_MAP  ! Map from BEIS Species to EM_SPEC
         USE BIOG_EMIS, only : EMSPC,    ! Emitted CMAQ-Species from BEIS
     &                         MSPCS     ! Number of Emitted Species
         USE MGEMIS,    only : MGEM,     ! Flag for Marine Gas Emissions
     &                         MG_SPC,   ! Emitted Marine Gas CMAQ-Species
     &                         MGSPC_MAP ! Map from Marine Gas Emissions to EM_SPEC
         USE LTNG_DEFN, only : LTNG_NO,  ! Flag for Lightning Emissions
     &                         LTSPC     ! Lightning Emitted Species (i.e. 'NO')
         USE PT3D_DEFN, only : PT3DEMIS, ! Flag for Point Source Emissions
     &                         NPTGRPS
         USE STK_EMIS,  only : STKSPC    ! Species from Point Source Emissions
         USE UTILIO_DEFN
         USE CGRID_SPCS, only: N_GC_EMIS, N_AE_EMIS, N_NR_EMIS, N_TR_EMIS,
     &                         GC_EMIS,   AE_EMIS,   NR_EMIS,   TR_EMIS
         USE AERO_DATA, only : N_MODE, AEROSPC,  ! Aerosol Properties Table
     &                         N_AEROSPC, AEROMODE
         USE AERO_EMIS, only : 

         IMPLICIT NONE

         INTEGER, INTENT(IN)  :: JDATE, JTIME

         INTEGER    :: NTRSPC, NARSPC, NBIOSPC, 
     &                 NPTSPC, NMGSPC, NLTSPC, NDSTSPC
         INTEGER    :: NEMIS_SPC, NEMIS_SPC_UNIQUE, N_UNUSED

         CHARACTER( 16 ), ALLOCATABLE  :: EMIS_SPCS( : ), EMIS_SPC_UNIQUE( : )
         CHARACTER( 16 ), ALLOCATABLE  :: GC_EMIS_WRITE( : )
         CHARACTER( 16 ), ALLOCATABLE  :: AE_EMIS_WRITE( : )
         CHARACTER( 16 ), ALLOCATABLE  :: NR_EMIS_WRITE( : )
         CHARACTER( 16 ), ALLOCATABLE  :: TR_EMIS_WRITE( : )
         CHARACTER( 16 ), ALLOCATABLE  :: AERO_EMIS_WRITE( : )
         CHARACTER( 16 ), ALLOCATABLE  :: BG_SPC_WRITE( : )
         CHARACTER( 16 ), ALLOCATABLE  :: EMIS_SPC_UNUSED( : )

         INTEGER    :: STRT, PTSTRT, ISPC, IPT, JPT, IDX, LOGDEV, IX

         LOGICAL    :: LERROR
         LOGICAL, ALLOCATABLE :: L_EMIS_SPC( : ), 
     &                           GC_EMIS_MISSING( : ), AE_EMIS_MISSING( : ),
     &                           NR_EMIS_MISSING( : ), TR_EMIS_MISSING( : ),
     &                           AERO_EMIS_MISSING( : ), TMP( : ),
     &                           EMIS_MASK( : ), BIO_IND( : )

         INTEGER, ALLOCATABLE  :: EMIS_IND( : )

         CHARACTER( 16 )  :: CTM_EMISCHK = 'CTM_EMISCHK'    ! Let model proceed even if
                                                                 ! Emissions surrogates are
                                                                 ! not properly defined
         CHARACTER( 16 )  :: PNAME = 'EMIS_SPC_CHECK'
         CHARACTER( 80 )  :: VARDESC
         INTEGER          :: STATUS
         CHARACTER( 200 ) :: XMSG
 
 
C Retrieve Environment Variable Letting User Ignore this Check
C and allowing the model to proceed.
         EMISCHK = .TRUE.  ! Default True for Emissions Check
         VARDESC    = "Let model proceed even if there are emission"
     &              //" surrogates that do not appear on any input file."
         LOGDEV = INIT3()
         EMISCHK = ENVYN( CTM_EMISCHK, VARDESC, EMISCHK, STATUS )
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 1 ) THEN
            XMSG = 'Environment variable improperly formatted'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
         ELSE IF ( STATUS .EQ. -1 ) THEN
            XMSG = 'Environment variable set, but empty...Using default:'
            WRITE( LOGDEV, '(5X,A,I9)' ) XMSG, JTIME
         ELSE IF ( STATUS .EQ. -2 ) THEN
            XMSG = 'Environment variable not set...Using default:'
            WRITE( LOGDEV, '(5X,A,I9)' ) XMSG, JTIME
         END IF


C Find Numbers of Species Available From Emissions Sources
         NBIOSPC = 0
         NMGSPC  = 0
         NLTSPC  = 0
         NDSTSPC = 0
         NTRSPC  = 0
         NARSPC  = 0
         NPTSPC  = 0

         ! Gridded Area and Tracer Sources 
         IF ( N_TR_EMIS .GT. 0 ) THEN
           NTRSPC  = SIZE( TRACSPC, DIM=1 )
         ELSE
           NTRSPC  = 0
         END IF
         NARSPC  = SIZE( AREASPC, DIM=1 )

         ! Point Sources
         IF ( PT3DEMIS ) THEN
             NPTSPC  = SUM( STKSPC( : )%LEN )
         END IF

         ! Biogenic Sources
         IF ( BIOGEMIS ) NBIOSPC = MSPCS

         ! Marine Gas Sources
         IF ( MGEM ) NMGSPC = SIZE( MG_SPC, DIM=1 )

         ! Lightning Sources
         IF ( LTNG_NO ) NLTSPC = 1

         ! Sum Species from Each Source Category
         NEMIS_SPC = NTRSPC  + NARSPC + NPTSPC 
     &             + NBIOSPC + NMGSPC + NLTSPC 
         ALLOCATE( EMIS_SPCS( NEMIS_SPC ) )
         EMIS_SPCS( 1:NEMIS_SPC ) = "                "

         ! Build the master list of all area, point, bio, sea spray
         ! Print a Table of Species from Every Emissions Source
         WRITE( LOGDEV, '(/,A31,A42)' ),'CHECKING COMPATABILITY BETWEEN ',
     &                  'INTERNAL SPECIES LIST AND EMISSIONS INPUTS' 
         WRITE( LOGDEV, '(A55,/,A58)' ),
     &                  'The following tables list the chemical species present ',
     &                  'on each emission file or from each online emission source.' 


         ! Tracer Species
         STRT = 0
         IF ( NTRSPC .GT. 0 ) THEN 
           EMIS_SPCS( STRT+1:STRT+NTRSPC ) = TRACSPC( 1:NTRSPC )
           WRITE( LOGDEV, '(/,A18,I3,A8,/,4(2x,A16))' ),
     &           'Tracer Emissions: ',NTRSPC,' Species',
     &           TRACSPC( 1:NTRSPC )
           STRT = STRT + NTRSPC
         END IF

         ! Gas and Aerosol Gridded Area Emissions
         IF ( NARSPC .GT. 0 ) THEN
           EMIS_SPCS( STRT+1:STRT+NARSPC ) = AREASPC( 1:NARSPC )
           WRITE( LOGDEV, '(/,A40,I3,A8,/,4(2x,A16))' ),
     &           'Gas and Aerosol Gridded Area Emissions: ',
     &           NARSPC,' Species',AREASPC( 1:NARSPC )
           STRT = STRT + NARSPC
         END IF

         ! Point Species
         IF ( NPTSPC .GT. 0 ) THEN 
           PTSTRT = STRT
           DO IPT = 1,NPTGRPS
             JPT = STKSPC( IPT )%LEN
             WRITE( LOGDEV, '(/,A11,I2,A12,I3,A8,/,4(2x,A16))' ),
     &             'Point File ',IPT,' Emissions: ',JPT, 'Species',
     &             STKSPC( IPT )%ARRY
             EMIS_SPCS( PTSTRT+1:PTSTRT+JPT ) = STKSPC( IPT )%ARRY( 1:JPT )
             PTSTRT = PTSTRT + JPT
           END DO
           STRT = STRT + NPTSPC
         END IF

         ! Biogenic Species
         IF ( NBIOSPC .GT. 0 ) THEN
           EMIS_SPCS( STRT+1:STRT+NBIOSPC ) = EMSPC( 1:NBIOSPC )
           ALLOCATE( BIO_IND( NBIOSPC ) )
           BIO_IND = EMSPC .NE. ''
           ALLOCATE( BG_SPC_WRITE( COUNT( BIO_IND ) ) )
           BG_SPC_WRITE = PACK( EMSPC, BIO_IND )
           
           WRITE( LOGDEV, '(/,A20,I3,A8,/,4(2x,A16))' ),
     &           'Biogenic Emissions: ',COUNT( BIO_IND ),' Species',
     &           BG_SPC_WRITE
           STRT = STRT + NBIOSPC
         END IF
         
         ! Marine Gas Species
         IF ( NMGSPC .GT. 0 ) THEN
           EMIS_SPCS( STRT+1:STRT+NMGSPC ) = MG_SPC( 1:NMGSPC )
           WRITE( LOGDEV, '(/,A26,I3,A8,/,4(2x,A16))' ),
     &           'Marine Gas Emissions: ',NMGSPC,' Species',
     &           MG_SPC( 1:NMGSPC )
           STRT = STRT + NMGSPC
         END IF
         
         ! Lightning Species (NO)
         IF ( NLTSPC .GT. 0 ) THEN
           EMIS_SPCS( STRT+1 ) = LTSPC
           WRITE( LOGDEV, '(/,A21,I3,A8,/,4(2x,A16))' ),
     &           'Lightning Emissions: ',NLTSPC,' Species',
     &           LTSPC
           STRT = STRT + NLTSPC
         END IF

C Reduce EMIS_SPCS to just unique values
         ALLOCATE( EMIS_MASK( NEMIS_SPC ) )
         EMIS_MASK = .TRUE.
         ! Determine which surrogates to keep
         DO ISPC = NEMIS_SPC,2,-1
             EMIS_MASK( ISPC ) = .NOT.( ANY( EMIS_SPCS(:ISPC-1) == EMIS_SPCS( ISPC ) ) )
         END DO

         ! Define indices of Surrogates to Keep
         ALLOCATE( EMIS_IND( COUNT( EMIS_MASK ) ) )   
         EMIS_IND = PACK([(ix, ix=1,NEMIS_SPC ) ],EMIS_MASK )
         
         ! Reduce the EMIS_SPCS vector down to just the unique values
         NEMIS_SPC_UNIQUE = SIZE( EMIS_IND,DIM=1 )
         ALLOCATE( EMIS_SPC_UNIQUE( NEMIS_SPC_UNIQUE ) )
         EMIS_SPC_UNIQUE = EMIS_SPCS( EMIS_IND )
         
C Print out the Tables of Gas, Aerosol, Nonreactive and Tracer Species
C for reference
         WRITE( LOGDEV, '(/A57,/,A54)' ),
     &                  'The following tables list the emission surrogate species ',
     &                  'present in each namelist or in the aerosol data table.' 
         
         IF ( N_GC_EMIS .GT. 0 )
     &        WRITE( LOGDEV, '(/,A13,I3,A8,/,4(2x,A16))' ),
     &               'GC Namelist: ',N_GC_EMIS,' Species',GC_EMIS(1:N_GC_EMIS)
         
         IF ( N_AEROSPC .GT. 0 ) THEN
              ALLOCATE( AERO_EMIS_WRITE( COUNT( AEROSPC%EMIS .NE. "" ) ) )
              AERO_EMIS_WRITE = PACK( AEROSPC%EMIS, AEROSPC%EMIS .NE. "" )
              WRITE( LOGDEV, '(/,A16,I3,A8,/,4(2x,A16))' ),
     &               'AeroData Table: ',COUNT( AEROSPC%EMIS .NE. "" ),
     &               ' Species',AERO_EMIS_WRITE
              DEALLOCATE( AERO_EMIS_WRITE )
         END IF
         
         IF ( N_NR_EMIS .GT. 0 )
     &        WRITE( LOGDEV, '(/,A13,I3,A8,/,4(2x,A16))' ),
     &               'NR Namelist: ',N_NR_EMIS,' Species',NR_EMIS(1:N_NR_EMIS)
         
         IF ( N_TR_EMIS .GT. 0 )
     &        WRITE( LOGDEV, '(/,A13,I3,A8,/,4(2x,A16))' ),
     &               'TR Namelist: ',N_TR_EMIS,' Species',TR_EMIS(1:N_TR_EMIS)


C Find the Number and Collect Names of CMAQ Species with Emissions Species
         N_EMIS_SPC = N_GC_EMIS + N_AE_EMIS + N_NR_EMIS + N_TR_EMIS

C Warn the User if there are No Emissions Species On Any File         
         IF ( N_EMIS_SPC .LE. 0 ) THEN
            XMSG = 'There are no emissions: VDEMIS is set to zero' ! below
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
         END IF

         ! Populate EM_SPEC Array with CMAQ Internal Species that will have
         ! Emissions
         ALLOCATE( EM_SPEC( N_EMIS_SPC ) )
         ALLOCATE( EM_MASK_AERO( N_EMIS_SPC ) )
         EM_MASK_AERO = .FALSE.
         ISPC = 0
         DO IGC = 1,N_GC_EMIS
             ISPC = ISPC + 1
             EM_SPEC( ISPC ) = GC_SPC( GC_EMIS_MAP( IGC ) )
         END DO
         DO IAE = 1,N_AE_EMIS
             ISPC = ISPC + 1
             EM_SPEC( ISPC ) = AE_SPC( AE_EMIS_MAP( IAE ) )
             EM_MASK_AERO( ISPC ) = .TRUE.
         END DO
         DO INR = 1,N_NR_EMIS
             ISPC = ISPC + 1
             EM_SPEC( ISPC ) = NR_SPC( NR_EMIS_MAP( INR ) )
         END DO
         DO ITR = 1,N_TR_EMIS
             ISPC = ISPC + 1
             EM_SPEC( ISPC ) = TR_SPC( TR_EMIS_MAP( IGC ) )
         END DO


         ! Populate EM_SPEC_BUFF Array with Master List of All Requested
         ! Emissions Species
         ALLOCATE( EM_SPEC_BUFF( N_EMIS_SPC ) )
         EM_SPEC_BUFF( 1:N_GC_EMIS ) = GC_EMIS( 1:N_GC_EMIS )     !Gases
         EM_FAC_BUFF ( 1:N_GC_EMIS ) = GC_EMIS_FAC( 1:N_GC_EMIS )     !Gases
         N = N_GC_EMIS
         EM_SPEC_BUFF( N+1:N+N_AE_EMIS ) = AE_EMIS( 1:N_AE_EMIS ) !Aerosols
         EM_FAC_BUFF ( N+1:N+N_AE_EMIS ) = AE_EMIS_FAC( 1:N_AE_EMIS ) !Aerosols
         N = N + N_AE_EMIS
         EM_SPEC_BUFF( N+1:N+N_NR_EMIS ) = NR_EMIS( 1:N_NR_EMIS ) !Non-Reactives
         EM_FAC_BUFF ( N+1:N+N_NR_EMIS ) = NR_EMIS_FAC( 1:N_NR_EMIS ) !Non-Reactives
         N = N + N_NR_EMIS
         EM_SPEC_BUFF( N+1:N+N_TR_EMIS ) = TR_EMIS( 1:N_TR_EMIS ) !Tracers
         EM_FAC_BUFF ( N+1:N+N_TR_EMIS ) = TR_EMIS_FAC( 1:N_TR_EMIS ) !Tracers
 
         ! Determine which aerosol species emissions are for number and 
         ! surface area. Create maps from Emission Surrogate Species to 
         ! the number and surface area of each mode. This is referenced to the 
         DO IM = 1,N_MODE
             IDX = INDEX1( AEROMODE( IM )%NUM_NAME, N_EMIS_SPC, EM_SPEC_BUFF )
             IF ( IDX .GT. 0 ) THEN
                 ! Found Aerosol Number for this Mode
                 MAP_NUMtoEM( IM ) = IDX
             ELSE
                 WRITE( XMSG, '(A,I1)'),'Can not find Number Emissions for Mode ',IM 
                 CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
             END IF
             IDX = INDEX1( AEROMODE( IM )%SRF_NAME, N_EMIS_SPC, EM_SPEC_BUFF )
             IF ( IDX .GT. 0 ) THEN
                 ! Found Aerosol Number for this Mode
                 MAP_SRFtoEM( IM ) = IDX
             ELSE
                 WRITE( XMSG, '(A,I1)'),'Can not find Surface Area Emissions for Mode ',IM 
                 CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
             END IF 
         END DO

         ! Determine the Aerosol Chemical and Size Identity of Each CMAQ 
         ! species so that a map can be built to go from CMAQ species to
         ! aerosol type and mode.
         DO IAERO = 1,N_AEROSPC
             DO IM = 1,N_MODE
                 IDX = INDEX1( AEROSPC( IAERO )%NAME( IM ), N_EMIS_SPC, EM_SPEC )
                 IF ( IDX .GT. 0 ) THEN
                     ! Load Maps from EM_SPEC to Aerosol Type and Mode
                     MAP_EMtoAERO( IDX ) = IAERO
                     MAP_EMtoMODE( IDX ) = IM

                     ! Load Map for Reduced Size PM Emission Rate Buffer
                     MAP_EMBUFF( IAERO,IM ) = IDX
                 END IF
                 ! Load Size Distribution Factors
                 EMWT_PROF( IAERO,IM ) = AEROSPC( IAERO )%EMIS_SPLIT( IM )
             END DO
         END DO

         ! Apply Uniform Volatility Distribution Across All Sources
         ! This algorithm distributes the factors across affected
         ! particles and vapors using parameters from AERO_DATA.
         DO ISPC = 1,N_EMIS_SPC
           IF ( EM_SPEC_BUFF( ISPC ) .EQ. 'POC' ) THEN
             DO IPOA = 1,N_VBS_BIN
               IF ( EM_SPEC( ISPC )(2:6) .EQ. POA_NAME( IPOA ) ) THEN
                 IF ( EM_MASK_AERO ) THEN
                   EM_FAC_BUFF( ISPC ) = EM_FAC_BUFF( ISPC )
     &                                 * POA_OP_VF( IPOA ) * POA_AMF( IPOA ) 
                 ELSE
                   EM_FAC_BUFF( ISPC ) = EM_FAC_BUFF( ISPC ) * POA_OP_VF( IPOA )
     &                                 * (1.0 - POA_AMF( IPOA ) ) / GC_MOLWT( V )
                 END IF
               END IF
             END DO
           END IF
         END DO    
 
         ! Populate Arrays Mapping CMAQ Species to Species on Files
         ! For each emission source, check to see which Emissions
         ! species are actually represented. Overwrite the entry with a
         ! blank if the species is not present from the source/file. If
         ! there is a species not found on any file or a species on a
         ! file that is never used, trigger an error (if EMISCHK is
         ! TRUE).
         ALLOCATE( EM_ARSPEC( N_EMIS_SPC ) )
         ALLOCATE( EM_PTSPEC( N_EMIS_SPC, NPTGRPS ) )
         IF ( BIOGEMIS ) ALLOCATE( EM_BGSPEC( N_EMIS_SPC ) )
         IF ( MGE      ) ALLOCATE( EM_MGSPEC( N_EMIS_SPC ) )
         IF ( LTNG_NO  ) ALLOCATE( EM_LTSPEC( N_EMIS_SPC ) )

         ALLOCATE( L_EMIS_SPC( NEMIS_SPC_UNIQUE ) )
         L_EMIS_SPC = .FALSE.
         LERROR = .FALSE.

         ! Populate the Emissions Species on the Area Emission Map
         DO ISPC = 1,N_EMIS_SPC
             IDX = INDEX1( EM_SPEC_BUFF( ISPC ), NARSPC, AREASPC )
             IF ( IDX .GT. 0 ) THEN
                 ! A match for this surrogate is found
                 EM_ARSPEC( ISPC ) = EM_SPEC_BUFF( ISPC )
                 EM_FAC_AR( ISPC ) = EM_FAC_BUFF ( ISPC )

                 ! Mark this species as found on the master list
                 IDX = INDEX1( EM_SPEC( ISPC ), NEMIS_SPC_UNIQUE, EMIS_SPC_UNIQUE )
                 L_EMIS_SPC( IDX ) = .TRUE.
             END IF
         END DO
         IF ( ALL( EM_ARSPEC ) .EQ. '' ) THEN
            WRITE( LOGDEV, '(/,A61)'),
     &         'There are no matching surrogates for the Area Emissions File.'
         END IF

         ! Populate the Emissions Species on the Point Emission Maps
         DO IPT = 1,NPTGRPS
            DO ISPC = 1,N_EMIS_SPC

               IDX = INDEX1( EM_SPEC_BUFF( ISPC ), STKSPC( IPT )%LEN, STKSPC( IPT )%ARRY )
               IF ( IDX .GT. 0 ) THEN
                   ! A match for this surrogate is found
                   EM_PTSPEC( ISPC,IPT ) = EM_SPEC_BUFF( ISPC )
                   EM_FAC_PT( ISPC,IPT ) = EM_FAC_BUFF ( ISPC )

                   ! Mark this species as found on the master list
                   IDX = INDEX1( EM_SPEC( ISPC ), NEMIS_SPC_UNIQUE, EMIS_SPC_UNIQUE )
                   L_EMIS_SPC( IDX ) = .TRUE.
               END IF
            END DO
            IF ( ALL( EM_PTSPEC( :,IPT ) ) .EQ. '' ) THEN
               WRITE( LOGDEV, '(/,A39,/,A22,I2)'),
     &            'There are no matching surrogates for the',
     &            'Point Emissions File: ', IPT 
            END IF
         END DO
 
         ! Populate the Emissions Species from the Online Biogenic
         ! Emission Module (BEIS)
         IF ( NBIOSPC .GT. 0 ) THEN
            DO ISPC = 1,N_EMIS_SPC
               IDX = INDEX1( EM_SPEC_BUFF( ISPC ), NBIOSPC, EMSPC )
               IF ( IDX .GT. 0 ) THEN
                   ! A match for this surrogate is found
                   BEIS_MAP( IDX ) = ISPC
                   EM_FAC_BG( ISPC ) = EM_FAC_BUFF ( ISPC )

                   ! Mark this species as found on the master list
                   IDX = INDEX1( EM_SPEC( ISPC ), NEMIS_SPC_UNIQUE, EMIS_SPC_UNIQUE )
                   L_EMIS_SPC( IDX ) = .TRUE.
               END IF
            END DO
            IF ( ALL( EM_BGSPEC ) .EQ. '' ) THEN
               WRITE( LOGDEV, '(/,A67)'),
     &            'There are no matching surrogates for the Biogenic Emissions Module.'
            END IF
         END IF
 
         ! Populate the Emissions Species from the Online Marine
         ! Gas Emission Module
         IF ( NMGSPC .GT. 0 ) THEN
            DO ISPC = 1,N_EMIS_SPC
               IDX = INDEX1( EM_SPEC_BUFF( ISPC ), NMGSPC, MG_SPC )
               IF ( IDX .GT. 0 ) THEN
                   ! A match for this surrogate is found
                   MGSPC_MAP( IDX ) = ISPC
                   EM_FAC_MG( ISPC ) = EM_FAC_BUFF ( ISPC )

                   ! Mark this species as found on the master list
                   IDX = INDEX1( EM_SPEC( ISPC ), NEMIS_SPC_UNIQUE, EMIS_SPC_UNIQUE )
                   L_EMIS_SPC( IDX ) = .TRUE.
               END IF
            END DO
            IF ( ALL( EM_MGSPEC ) .EQ. '' ) THEN
               WRITE( LOGDEV, '(/,A69)'),
     &            'There are no matching surrogates for the Marine Gas Emissions Module.'
            END IF
         END IF
 
         ! Populate the Emissions Species from the Lightning NO Emission Module
         IF ( NLTSPC .GT. 0 ) THEN
            DO ISPC = 1,N_EMIS_SPC
               IDX = INDEX1( EM_SPEC_BUFF( ISPC ), NLTSPC, LTSPC )
               IF ( IDX .GT. 0 ) THEN
                   ! A match for this surrogate is found
                   EM_LTSPEC( ISPC ) = EM_SPEC_BUFF( ISPC )
                   EM_FAC_LT( ISPC ) = EM_FAC_BUFF ( ISPC )

                   ! Mark this species as found on the master list
                   IDX = INDEX1( EM_SPEC( ISPC ), NEMIS_SPC_UNIQUE, EMIS_SPC_UNIQUE )
                   L_EMIS_SPC( IDX ) = .TRUE.
               END IF
            END DO
            IF ( ALL( EM_LTSPEC ) .EQ. '' ) THEN
               WRITE( LOGDEV, '(/,A67)'),
     &            'There are no matching surrogates for the Lightning NO Emissions Module.'
            END IF
         END IF
 
C Compare with Gas, Aerosol, Nonreactive, and Tracer Species
         ! Initialize variables for tracking surrogate matches

         ! Check Compatability with Gas Emissions Surrogates
         IF ( N_GC_EMIS .GT. 0 ) THEN
             ALLOCATE( GC_EMIS_MISSING( N_GC_EMIS ) )
             GC_EMIS_MISSING = .FALSE.

             DO ISPC = 1,N_GC_EMIS
                IDX = INDEX1( GC_EMIS( ISPC ), NEMIS_SPC_UNIQUE, EMIS_SPC_UNIQUE )
                IF ( IDX .EQ. 0 ) GC_EMIS_MISSING( ISPC ) = .TRUE.
             END DO
             IF ( ANY( GC_EMIS_MISSING ) ) THEN
                ALLOCATE( GC_EMIS_WRITE( COUNT( GC_EMIS_MISSING ) ))
                GC_EMIS_WRITE = PACK( GC_EMIS(1:N_GC_EMIS), GC_EMIS_MISSING )

                WRITE( LOGDEV, '(/,A48,/,2x,A51,/,2x,A42,/,5(5x,A16))'),
     &             'The following GAS emission surrogate species are',
     &             'not present from any of the Area, Point, Biogenic,',
     &             'Marine Gas, or Lightning emissions sources:',
     &             GC_EMIS_WRITE
                LERROR = .TRUE.
             END IF
         END IF
          
         ! Check Compatability with Aerosol Emissions Surrogates
         IF ( N_AEROSPC .GT. 0 ) THEN 
             ALLOCATE( AERO_EMIS_MISSING( N_AE_EMIS ) )
             AERO_EMIS_MISSING = .FALSE.

             DO ISPC = 1,N_AE_EMIS
                IDX = INDEX1( AE_EMIS( ISPC ), NEMIS_SPC_UNIQUE, EMIS_SPC_UNIQUE )
                IF ( IDX .EQ. 0 ) AERO_EMIS_MISSING( ISPC ) = .TRUE.
             END DO 
             IF ( ANY( AERO_EMIS_MISSING ) ) THEN
                ALLOCATE( AERO_EMIS_WRITE( COUNT( AERO_EMIS_MISSING ) ) ) 
                AERO_EMIS_WRITE = PACK( AE_EMIS(1:N_AE_EMIS), AERO_EMIS_MISSING )

                WRITE( LOGDEV, '(/A40,/,2x,A52,/,2x,A53,/,5(5x,A16))'),
     &             'The following AEROSOL emission surrogate',
     &             'species are not present from any of the Area, Point,',
     &             'Biogenic, Marine Gas, or Lightning emissions sources:',
     &             AERO_EMIS_WRITE
                LERROR = .TRUE.
             END IF
         END IF
          
         ! Check Compatability with Nonreactive Emissions Surrogates
         IF ( N_NR_EMIS .GT. 0 ) THEN
             ALLOCATE( NR_EMIS_MISSING( N_NR_EMIS ) )
             NR_EMIS_MISSING = .FALSE.

             DO ISPC = 1,N_NR_EMIS
                IDX = INDEX1( NR_EMIS( ISPC ), NEMIS_SPC_UNIQUE, EMIS_SPC_UNIQUE )
                IF ( IDX .EQ. 0 ) NR_EMIS_MISSING( ISPC ) = .TRUE.
             END DO 
             IF ( ANY( NR_EMIS_MISSING ) ) THEN
                ALLOCATE( NR_EMIS_WRITE( COUNT( NR_EMIS_MISSING ) ) ) 
                NR_EMIS_WRITE = PACK( NR_EMIS(1:N_NR_EMIS), NR_EMIS_MISSING )

                WRITE( LOGDEV, '(/,A45,/,A53,/,A54,/,5(5x,A16))'),
     &             'The following NONREACTIVE emission surrogate ',
     &             'species are not present from any of the Area, Point, ',
     &             'Biogenic, Marine Gas, or Lightning emissions sources: ',
     &             NR_EMIS_WRITE
                LERROR = .TRUE.
             END IF
         END IF
         
         ! Check Compatability with Tracer Emissions Surrogates
         IF ( N_TR_EMIS .GT. 0 ) THEN
             ALLOCATE( TR_EMIS_MISSING( N_TR_EMIS ) )
             TR_EMIS_MISSING = .FALSE.

             DO ISPC = 1,N_TR_EMIS
                IDX = INDEX1( TR_EMIS( ISPC ), NEMIS_SPC_UNIQUE, EMIS_SPC_UNIQUE )
                IF ( IDX .GT. 0 ) TR_EMIS_MISSING( ISPC ) = .TRUE.
             END DO 
             IF ( ANY( TR_EMIS_MISSING ) ) THEN
                ALLOCATE( TR_EMIS_WRITE( COUNT( TR_EMIS_MISSING ) ) ) 
                TR_EMIS_WRITE = PACK( TR_EMIS(1:N_TR_EMIS), TR_EMIS_MISSING )

                WRITE( LOGDEV, '(/,A40,/,A53,/,A54,/,5(5x,A16))'),
     &             'The following TRACER emission surrogate ',
     &             'species are not present from any of the Area, Point, ',
     &             'Biogenic, Marine Gas, or Lightning emissions sources: ',
     &             TR_EMIS_WRITE
                LERROR = .TRUE.
             END IF
         END IF

C Print a warning for each emissions file species that is not used.
         N_UNUSED = COUNT( .NOT. L_EMIS_SPC )
         IF ( N_UNUSED .GT. 0 ) THEN 
           ALLOCATE( EMIS_SPC_UNUSED( N_UNUSED ) )
           EMIS_SPC_UNUSED = PACK( EMIS_SPC_UNIQUE,( .NOT. L_EMIS_SPC ) ) 
         END IF
        
C Stop the Model if Any Emissions Surrogate Species are
C Missing from All Emission Input Sources
         IF ( LERROR ) THEN
           IF ( .NOT. EMISCHK ) THEN
              WRITE( LOGDEV, '(5(/,A))') 
     &               '************************WARNING***********************:',
     &               'One or more emissions surrogates assigned to model species ',
     &               'are not found in emissions sources but the CTM_EMISCHK ',
     &               'environment variable set to False so simulation will proceed.',
     &               '*******************************************************'

              WRITE( LOGDEV, '(/,A55,/,A34,I3,A8,/,4(2x,A16))')
     &              'ATTENTION: The following emission species are available',
     &              'from the inputs but are not used: ',N_UNUSED,' Species',
     &              EMIS_SPC_UNUSED

              WRITE( LOGDEV, * )
              WRITE( LOGDEV, * )
              XMSG = 'For optimal predictions, species with the missing surrogates '
     &             //'should have a surrogate found in at least one source.'
              CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
           ELSE     
              WRITE( LOGDEV, '(3(/,A),/,5(/,A),/,A,4(/,A))') 
     &               '*************************ERROR**********************:',
     &               'One or more emission surrogates assigned to model species ',
     &               'are not found in emission sources.',
     &               'Use one of the below options to continue.', 
     &               '1) Change or remove the missing GC, NR and TR emissions surrogates from the',
     &               'namelists where they are defined.', 
     &               'or',
     &               '2) Change CTM_EMISCHK environment variable to False (F) in the runscript',
     &               'if model predictions are acceptable without using the above emissions.',
     &               '*****************************************************'

              WRITE( LOGDEV, '(/,A55,/,A34,I3,A8,/,4(2x,A16))')
     &             'ATTENTION: The following emission species are available',
     &             'from the inputs but are not used: ',N_UNUSED,' Species',
     &              EMIS_SPC_UNUSED
              WRITE( LOGDEV, * )
              WRITE( LOGDEV, * )
              XMSG = 'Species with the missing surrogates ' 
     &             //'must have a surrogate found in at least one source.'
              CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
           END IF
         END IF



         END SUBROUTINE EMIS_SPC_CHECK

      END MODULE EMIS_DEFN
