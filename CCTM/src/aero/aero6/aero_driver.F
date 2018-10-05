
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

C >>> 08/04/2000 Changes necessary to be able to read and process
C two different types of emissions files.
C the first type is the existing opperational PM2.5 & PM10 unspeciated
C file. The new file format has speciated emissions.
C >>> This version uses the FORTRAN 90 feature for runtime memory
C allocation.

C 1/12/99 David Wong at LM:
C   -- introduce new variable MY_NUMBLKS (eliminate NUMBLKS)
C   -- re-calculate NOXYZ accordingly
C FSB Updated for inclusion of surface area / second moment
C 25 Sep 00 (yoj) various bug fixes, cleanup to coding standards
C   Jeff - Dec 00 - move CGRID_MAP into f90 module
C FSB/Jeff - May 01 - optional emissions processing
C   Jerry Gipson - Jun 01 - added SOA linkages for saprc99
C   Bill Hutzell - Jun 01 - simplified CBLK mapping
C   Jerry Gipson - Jun 03 - modified for new soa treatment
C   Jerry Gipson - Aug 03 - removed SOA prod form alkenes & added
C       emission adjustment factors for ALK & TOL ( RADM2 & SAPRC99 only)
C   Shawn Roselle - Jan 04
C   - removed SOA from transported aerosol surface area
C   - fixed bug in calculation of wet parameters.  Previously, DRY aerosol
C      parameters were being written to the AERDIAG files and mislabeled
C      as WET.
C   Prakash Bhave - May 04
C   - changed AERODIAG species (added RH; removed M0 & M2dry)
C   Jeff Young - Jan 05 - dyn alloc
C   - establish both horizontal & vertical domain specifications in one module
c   Uma Shankar and Prakash Bhave - Jun 05
c   - added code to handle the following species: ANAI, ANAJ, ANAK, ACLI,
c     ACLJ, ACLK, ASO4K, AH2OK, ANO3K, and HCL; removed code for ASEAS
c   - removed obsolete MW variables
C   Prakash Bhave - Jul 05 - added PM25 mass-fraction calculations
C   Jeff Young - Feb 06 - trap fractional humidity above 0.005
C   Prakash Bhave - Apr 06 - added GAMMA_N2O5 to the AEROPROC call vector
C       and the aerosol diagnostic file
C   Prakash Bhave - May 06 - changed units of DG variables from m to um in
C       the aerosol diagnostic file as suggested by Dr. Bill Hutzell
C   Sergey Napelenok - Sep 07 - SOA updates
C   - added code to handle the following species: AALKJ, ATOL1J, ATOL2J,
C     ATOL3J, AXYL1J, AXYL2J, AXYL3J, ABNZ1J, ABNZ2J, ABNZ3J, AISO1J, AISO2J,
C     AISO3J, ATRP1J, ATRP2J, ASQTJ, AORGCJ, TOLNRXN, TOLHRXN, XYLNRXN,
C     XYLHRXN, BNZNRXN, BNZHRXN, ISOPRXN, and SESQRXN
C   - removed following species: AORGAI, AORGAJ, AORGBI, AORGBJ, OLIRXN,
C     CSLRXN, TOLRXN, XYLRXN
C   Prakash Bhave - Oct 07 - SOA updates
C   - added semi-volatile vapors to the CBLK array; moved ppm -> ug/m3 unit
C     conversion from the ORGAER subroutine to this program
C   - updated definition of DRY aerosol to include nonvolatile SOA species
C   - removed adjustment factors for TOLAER (SPTOL, RDTOL) because benzene is
C     now an explicit species so all of the reacted TOL can produce SOA
C   - removed code to handle TERPSP (obsolete); renamed TERPRXN as TRPRXN
C   David Wong - Jan 08 - rearranged calculation of dry 3rd moments to avoid
C      NaN on some compilers (using the M3SUBT variable)
C   Prakash Bhave - Jan 08 - updated MECHNAME check from AE4 to AE5
C   Golam Sarwar -  Mar 08 - added a heterogeneous reaction producing HONO
C   Jim Kelly - Apr 08 - coarse mode updates
C   - added code to account for new species (ANH4K & SRFCOR) and variable
C     coarse std. deviation
C   - removed MW coding now located in AERO_INFO.f
C   - added FIXED_sg flag for call to GETPAR
C   Jeff Young - Aug 10 - convert for Namelist redesign (replace include files)
C   Steve Howard - Mar 11 - Renamed met_data to aeromet_data
C   S.Roselle- Mar 11 - replaced I/O API include files with UTILIO_DEFN
C   David Wong - Aug 11 - put in twoway model implementation
C   David Wong - Oct 11 - extended the twoway implementation to handle finer
C                         time resolution
C
C   Bill Hutzell - Sept 13 - inserted module for AEROSOL_CHEMISTRY to support
C                            diagnostic outputs on reaction gamma and yield 
C                            values 
C   HOT Pye - Jan 13 - Additional information for IEPOX aerosol 
C                      written to AERODIAG file
C   David Wong - Aug 15 - Used IO_PE_INCLUSIVE rather than MYPE to facilitate
C                         parallel I/O implementation
C                       - Used a new logical variable, FIRST_CTM_VIS_1 to
C                         determine when to open CTM_VIS_1 
C  B.Hutzell 22 Feb 16 - Added test to determine to write diagnostics from aerosol
C                        chemistry
C  H Pye and B Murphy April 2016 - Updated dry/wet moment process to use
C                        Extract_aero and Update_aero for getting moment/saving surface area
C  D. Wong 10 May 2016 - added calculation of average PMDIAG species w.r.t.
C                        environment variable CTM_PMDIAG, APMDIAG_BLEV_ELEV, 
C                        and AVG_PMDIAG_SPCS
C                      - added calculation of average visibility species w.r.t.
C                        environment variable CTM_AVISDIAG
C                      - renamed AERODIAM to PMDIAG and CTM_AERDIAG to CTM_PMDIAG
C                      - added flexibility to handle AE6 and AE6i
C                      - renamed DIAM to PMDIAG
C  D. Wong 19 May 2016 - renamed ACONC_END_TIME to AVG_FILE_ENDTIME
C                      - updated the way to define NUM_PMDIAG_SPC
C                      - set CTM_PMDIAG default value to .TRUE.
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE AERO ( CGRID, JDATE, JTIME, TSTEP )

      USE GRID_CONF, ONLY: NCOLS, NROWS, NLAYS, IO_PE_INCLUSIVE
      USE RXNS_DATA, ONLY: MECHNAME
      USE AERO_DATA, ONLY: COAG_BUDGET, COND_BUDGET, GROWTH_BUDGET, NPF_BUDGET,
     &                     FIXED_sg, AEROMODE_DIAM, AEROMODE_LNSG, AEROMODE_DENS,
     &                     MOMENT0_CONC, MOMENT2_CONC, MOMENT3_CONC,
     &                     AEROSPC, AEROSPC_CONC, N_MODE, AH2O_IDX,
     &                     EXTRACT_AERO, UPDATE_AERO, CALCMOMENTS
      USE PRECURSOR_DATA, ONLY: SULPRD_IDX, SO4RATE, PRECURSOR_CONC, 
     &                          PHGRXN_IDX, PHG_RATE, 
     &                          EXTRACT_PRECURSOR, UPDATE_PRECURSOR
      USE SOA_DEFN, ONLY: EXTRACT_SOA, UPDATE_SOA
      USE AEROMET_DATA, ONLY: AIRTEMP, AIRPRES, AIRQV, AIRDENS, AIRRH, 
     &                        H2OSATVP, H2OVP, MWWAT, MWAIR
      USE UTILIO_DEFN, ONLY: OPEN3, INTERPX, WRITE3, FSREAD3, ALLVAR3,
     &                       XSTAT1, TIME2SEC, SEC2TIME
      USE CGRID_SPCS, ONLY: NSPCSD
      USE AEROSOL_CHEMISTRY, ONLY : GAMMA_N2O5IJ, GAMMA_N2O5K, YCLNO2IJ, YCLNO2K, 
     &                              GAMMA_IEPOX, GAMMA_IMAE, KPARTIEPOX, AERO_CHEM_SET
      USE PMDIAG_DATA_MODULE, ONLY: NUM_PMDIAG_SPC, A_PMDIAG_SPC_MAP, PMDIAG_SPC_RECORD
      USE RUNTIME_VARS, ONLY: LOGDEV, PMDIAG, APMDIAG, APMDIAG_ZHI, END_TIME
      USE AERO_BUDGET, ONLY: AERO_COAG, AERO_COND, AERO_GROWTH, AERO_NPF

      IMPLICIT NONE

C *** Includes:

      INCLUDE SUBST_FILES_ID  ! file name parameters (req IOPARMS)

C *** Arguments:

C *** CGRID is conc field (including gas and aerosol variables)
      REAL, POINTER :: CGRID( :,:,:,: )              !  concentrations
      INTEGER      JDATE        ! Current model date , coded YYYYDDD
      INTEGER      JTIME        ! Current model time , coded HHMMSS
      INTEGER      TSTEP( 3 )   ! time step vector (HHMMSS)
                                ! TSTEP(1) = local output step
                                ! TSTEP(2) = sciproc sync. step (chem)
                                ! TSTEP(3) = twoway model time step w.r.t. wrf time
                                !            step and wrf/cmaq call frequency

C *** Local Variables:

      CHARACTER( 16 ), SAVE :: PNAME = 'AERO_DRIVER'
      CHARACTER( 16 ) :: VNAME            ! variable name
      CHARACTER( 96 ) :: XMSG = ' '

      INTEGER   MDATE, MTIME, MSTEP  ! julian date, time and
                                     ! timestep in sec
      INTEGER   C, R, L, V, N        ! loop counters
      INTEGER   SPC                  ! species loop counter
      INTEGER   STRT, FINI           ! loop induction variables
      INTEGER   ALLOCSTAT            ! memory allocation status

      LOGICAL   LERROR               ! Error flag

C *** Grid Description
      REAL DX1                 ! Cell x-dimension
      REAL DX2                 ! Cell y-dimension
      INTEGER GXOFF, GYOFF     ! global origin offset from file
C for INTERPX
      INTEGER, SAVE :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3

C *** Variable to set time step for writing visibility file
      INTEGER, SAVE :: WSTEP  = 0   ! local write counter
      LOGICAL, SAVE :: WRITETIME = .FALSE. ! local write flag

C *** meteorological variables
      REAL PRES   ( NCOLS,NROWS,NLAYS )  ! Atmospheric pressure [ Pa ]
      REAL TA     ( NCOLS,NROWS,NLAYS )  ! Air temperature [ K ]
      REAL DENS   ( NCOLS,NROWS,NLAYS )  ! Air density [ kg/m**-3 ]
      REAL QV     ( NCOLS,NROWS,NLAYS )  ! Water vapor mixing ratio [ kg/kg ]

C *** variables computed and output but not carried in CGRID

C *** aerosol size distribution variables
!     REAL DIAM_SPC( NCOLS,NROWS,NLAYS,39 )
      REAL, ALLOCATABLE, SAVE :: PMDIAG_SPC( :,:,:,: )
      REAL, ALLOCATABLE, SAVE :: A_PMDIAG_SPC( :,:,:,: )
      CHARACTER( 16 ) :: V_LIST( 2 )

      INTEGER, SAVE :: N_APMDIAG_STEP

C *** atmospheric properties
      REAL XLM             ! atmospheric mean free path [ m ]
      REAL AMU             ! atmospheric dynamic viscosity [ kg/m s ]

C *** mass fraction of each mode less than Specified Aerodynamic Diameters
      REAL fPM1 ( N_MODE )  ! PM1 fraction
      REAL fPM25( N_MODE )  ! PM2.5 fraction
      REAL fPM10( N_MODE )  ! PM10 fraction
      REAL fAMS ( N_MODE )  ! AMS Transmission Fraction

C *** visual range information
      REAL BLKDCV1         ! block deciview (Mie)
      REAL BLKEXT1         ! block extinction [ km**-1 ] (Mie)

      REAL BLKDCV2         ! block deciview (Reconstructed)
      REAL BLKEXT2         ! block extinction [ km**-1 ] (Reconstructed)


C *** other internal aerosol variables
      INTEGER IND                         ! index to be used with INDEX1

C *** synchronization time step [ s ]
      REAL DT

C *** variables to set up for "dry transport "
      REAL M3_WET( N_MODE ), M3_DRY( N_MODE )   ! third moment with and without water
      REAL M2_WET( N_MODE ), M2_DRY( N_MODE )   ! second moment with and without water

C *** variables aerosol diagnostic file flag
      INTEGER      STATUS            ! ENV... status
      CHARACTER( 80 ) :: VARDESC     ! environment variable description

C *** first pass flag
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      LOGICAL, SAVE :: FIRST_CTM_PMDIAG_1 = .TRUE.

C *** ratio of molecular weights of water vapor to dry air = 0.622015
      REAL, PARAMETER :: EPSWATER = MWWAT / MWAIR

C *** dry moment factor
      REAL, PARAMETER :: TWOTHIRDS = 2.0 / 3.0

      LOGICAL :: TIME_TO_CALL_FEEDBACK_WRITE

C *** Statement Function **************
      REAL ESATL ! arithmetic statement function for vapor pressure [Pa]
      REAL TT
C *** Coefficients for the equation, ESATL defining saturation vapor pressure
      REAL, PARAMETER :: AL = 610.94
      REAL, PARAMETER :: BL = 17.625
      REAL, PARAMETER :: CL = 243.04

      INTEGER, SAVE :: O3

      CHARACTER( 16 ) :: AVG_FILE_ENDTIME = 'AVG_FILE_ENDTIME'

      REAL, ALLOCATABLE, SAVE :: TEMP_DATA(:)

C *** values of AL, BL, and CL are from:
C     Alduchov and Eskridge, "Improved Magnus Form Approximations of
C                            Saturation Vapor Pressure,"
C                            Jour. of Applied Meteorology, vol. 35,
C                            pp 601-609, April, 1996.

      ESATL( TT ) = AL * EXP( BL * ( TT - 273.15 ) / ( TT - 273.15 + CL ) )

C *** End Statement Function  ********

      logical, save :: now = .true.

#ifdef twoway
      INTERFACE
        SUBROUTINE FEEDBACK_WRITE (C, R, L, CGRID_DATA, O3_VALUE, JDATE, JTIME)
          REAL, INTENT( IN ) :: CGRID_DATA(:), O3_VALUE
          INTEGER, INTENT( IN ) :: C, R, L, JDATE, JTIME
        END SUBROUTINE FEEDBACK_WRITE
      END INTERFACE
#endif

C ------------------ begin body of AERO_DRIVER -------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.

         IF ( ( INDEX(MECHNAME, 'SAPRC07TIC_AE6I_AQ') .GT. 0 ) .OR.
     &        ( INDEX(MECHNAME, 'SAPRC07TIC_AE7I_AQ') .GT. 0 ) ) THEN
            NUM_PMDIAG_SPC = 41
         ELSE
            NUM_PMDIAG_SPC = 39
         END IF

         ALLOCATE (   PMDIAG_SPC( NCOLS,NROWS,NLAYS,NUM_PMDIAG_SPC ),
     &              A_PMDIAG_SPC( NCOLS,NROWS,NLAYS,NUM_PMDIAG_SPC ),
     &              TEMP_DATA( NUM_PMDIAG_SPC ), 
     &              AERO_COND(   NCOLS,NROWS,NLAYS,NSPCSD ),
     &              AERO_COAG(   NCOLS,NROWS,NLAYS,NSPCSD,N_MODE ),
     &              AERO_NPF (    NCOLS,NROWS,NLAYS,NSPCSD ),
     &              AERO_GROWTH ( NCOLS,NROWS,NLAYS,NSPCSD ),
     &              COND_BUDGET( NSPCSD ),
     &              COAG_BUDGET( NSPCSD,N_MODE ),
     &              NPF_BUDGET( NSPCSD ),
     &              GROWTH_BUDGET( NSPCSD ),
     &              STAT=ALLOCSTAT)

         A_PMDIAG_SPC  = 0.0
         N_APMDIAG_STEP = 0

#ifdef twoway
! -- this is for twoway
         VNAME = 'O3'
         N = INDEX1( VNAME, N_GC_CONC, GC_CONC )
         IF ( N .NE. 0 ) THEN
            O3 = GC_STRT - 1 + GC_CONC_MAP( N )
         ELSE
            XMSG = 'Could not find ' // VNAME // 'in gas chem aerosol table'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         END IF
#endif

C *** Open the met files.
         IF ( .NOT. OPEN3( MET_CRO_3D, FSREAD3, PNAME ) ) THEN
            XMSG = 'Could not open  MET_CRO_3D  file '
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         IF ( .NOT. OPEN3( MET_CRO_2D, FSREAD3, PNAME ) ) THEN
            XMSG = 'Could not open  MET_CRO_2D file '
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C *** Open the aerosol parameters file (diameters and standard deviations).

         IF ( PMDIAG .AND. IO_PE_INCLUSIVE ) CALL OPPMDIAG ( JDATE, JTIME, TSTEP( 1 ) )
         IF ( APMDIAG ) CALL OPAPMDIAG ( JDATE, JTIME, TSTEP( 1 ) )

C *** Get domain decomp info from the MET_CRO_3D file
         CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                   STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )


      END IF    ! FIRSTIME

      MDATE  = JDATE
      MTIME  = JTIME
      MSTEP = TIME2SEC( TSTEP( 2 ) )
      CALL NEXTIME ( MDATE, MTIME, SEC2TIME( MSTEP / 2 ) )

      WSTEP = WSTEP + TIME2SEC( TSTEP( 2 ) )
      IF ( WSTEP .GE. TIME2SEC( TSTEP( 1 ) ) ) WRITETIME = .TRUE.

C *** Set floating point synchronization time step:
      DT = FLOAT( MSTEP ) ! set time step in seconds

C *** Get Meteorological Variables

C *** pressure [Pa]
      VNAME = 'PRES'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MDATE, MTIME, PRES ) ) THEN
         XMSG = 'Could not interpolate '// TRIM( VNAME ) // ' from MET_CRO_3D '
         CALL M3EXIT ( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
      END IF

C *** temperature [K]
      VNAME = 'TA'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MDATE, MTIME, TA ) ) THEN
         XMSG = 'Could not interpolate '// TRIM( VNAME ) // ' from MET_CRO_3D '
         CALL M3EXIT ( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
      END IF

C *** specific humidity [g H2O/g air]
      VNAME = 'QV'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MDATE, MTIME, QV ) ) THEN
         XMSG = 'Could not interpolate '// TRIM( VNAME ) // ' from MET_CRO_3D '
         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

C *** air density [kg/m3]
      VNAME = 'DENS'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MDATE, MTIME, DENS ) ) THEN
         XMSG = 'Could not interpolate '// TRIM( VNAME ) // ' from MET_CRO_3D '
         CALL M3EXIT ( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
      END IF

#ifdef twoway
! call FEEDBACK_WRITE when JTIME is mulitple of WRF time step
      IF ( CMAQ_WRF_FEEDBACK ) THEN
         IF ( MOD( TIME2SEC(MOD( JTIME, 10000 )), TIME2SEC(TSTEP( 3 )) ) .EQ. 0 ) THEN
            TIME_TO_CALL_FEEDBACK_WRITE = .TRUE.
         ELSE
            TIME_TO_CALL_FEEDBACK_WRITE = .FALSE.
         END IF
      END IF
#endif

! *** Initialize Shared Arrays for Aerosol Budget
      AERO_COND   = 0.
      AERO_COAG   = 0.
      AERO_NPF    = 0.
      AERO_GROWTH = 0.


C --------------------- Begin loops over grid cells --------------------------

C *** initialize conc arrays

      N_APMDIAG_STEP = N_APMDIAG_STEP + 1

      DO L = 1, NLAYS
         DO R = 1, NROWS
            DO C = 1, NCOLS

C *** Grid cell meteorological data.
               AIRTEMP  = TA   ( C,R,L )
               AIRPRES  = PRES ( C,R,L )   ! Note pascals
               AIRQV    = QV   ( C,R,L )
               AIRDENS  = DENS ( C,R,L )
               H2OSATVP = ESATL( AIRTEMP )
               H2OVP    = AIRPRES * AIRQV / ( EPSWATER  + AIRQV )
               AIRRH    = MAX( 0.005, MIN( 0.99, H2OVP / H2OSATVP ) )

! *** Initialize aerosol process variables
               COND_BUDGET = 0.
               COAG_BUDGET = 0.
               NPF_BUDGET = 0.
               GROWTH_BUDGET = 0.

C *** Extract grid cell concentrations of aero species from CGRID
C     into aerospc_conc in aero_data module (set minimum)
               CALL EXTRACT_AERO( CGRID( C,R,L,: ), .TRUE. )

C *** Extract grid cell concentrations of gas precursors from CGRID (ppm)
C     into precursr_conc in precursor_data
               CALL EXTRACT_PRECURSOR( CGRID( C,R,L,: ) )

C *** Calculate SO4RATE stored in module
               SO4RATE = REAL( PRECURSOR_CONC( SULPRD_IDX ), 4 ) / DT
 
               IF ( PHGRXN_IDX .GT. 0 ) THEN
C *** Calculate PHG_RATE stored in module
                  PHG_RATE = REAL( PRECURSOR_CONC( PHGRXN_IDX ), 4 ) / DT
               ELSE
                  PHG_RATE = 0.0
               END IF

C *** Extract soa concentrations from CGRID and 
C     convert M2 to wet
               CALL EXTRACT_SOA( CGRID( C,R,L,: ) )

C *** Aerosol process routines
               CALL AEROPROC( DT, C, R, L )

C *** Update aerosol variables conc back into CGRID (set minimum) 
C     and convert M2 to dry and save as surface area
               CALL UPDATE_AERO( CGRID( C,R,L,: ), .TRUE. )

C *** Update precursor variables conc back into CGRID
               CALL UPDATE_PRECURSOR( CGRID( C,R,L,: ) )

C *** Update gas soa concentrations back to CGRID
               CALL UPDATE_SOA( CGRID( C,R,L,: ) )

C *** OUTPUT DIAGNOSTIC INFORMATION
C *** Get wet moment info (dry will be converted to wet)
               CALL calcmoments( .true. )
               CALL GETPAR( FIXED_sg ) ! update AEROMODE_DIAM,DENS,SDEV
                   
C *** Calculate volume fraction of each mode for measurement applications
               DO N = 1, N_MODE
                  ! Calculate transmission of each mode in aerosol impactors.
                  ! Report fraction of PM1, PM2.5, and PM10.0 for all modes
                  ! after converting to aerodynamic diameter.
                  CALL AERO_INLET ( AEROMODE_DIAM( N ), AEROMODE_LNSG( N ),
     &                              AEROMODE_DENS( N ), fPM1( N ), fPM25( N ),
     &                              fPM10( N ) )
           
                  ! Calculate AMS Transmission Fraction. Aerosol water will
                  ! be excluded from the calculation since AMS standard
                  ! procedures recommend drying particles before they enter
                  ! the intstrument.
                  CALL AERO_AMS ( MOMENT3_CONC( N ), MOMENT2_CONC( N ),
     &                            MOMENT0_CONC( N ), AEROSPC_CONC( AH2O_IDX,N ), 
     &                            AEROMODE_DENS( N ),AEROSPC( AH2O_IDX)%DENSITY, 
     &                            fAMS( N ) )
               END DO

C *** Write aerosol extinction coefficients and deciviews to visibility
C     diagnostic array (lowest vertical layer only)

               IF ( PMDIAG .OR. APMDIAG ) THEN
                  TEMP_DATA(  7 ) = AEROMODE_DIAM( 1 ) * 1.0E6  ! wet i-mode diam.
                  TEMP_DATA(  8 ) = AEROMODE_DIAM( 2 ) * 1.0E6  ! wet j-mode diam.
                  TEMP_DATA(  9 ) = AEROMODE_DIAM( 3 ) * 1.0E6  ! wet k-mode diam.
                  TEMP_DATA( 10 ) = MOMENT2_CONC( 1 )     ! wet i-mode 2nd moment
                  TEMP_DATA( 11 ) = MOMENT2_CONC( 2 )     ! wet j-mode 2nd moment
                  TEMP_DATA( 12 ) = MOMENT2_CONC( 3 )     ! wet k-mode 2nd moment
                  TEMP_DATA( 16 ) = MOMENT3_CONC( 1 )     ! wet i-mode 3rd moment
                  TEMP_DATA( 17 ) = MOMENT3_CONC( 2 )     ! wet j-mode 3rd moment
                  TEMP_DATA( 18 ) = MOMENT3_CONC( 3 )     ! wet k-mode 3rd moment
                  TEMP_DATA( 20 ) = fPM1( 1 )             ! i-mode PM1 fraction
                  TEMP_DATA( 21 ) = fPM1( 2 )             ! j-mode PM1 fraction
                  TEMP_DATA( 22 ) = fPM1( 3 )             ! k-mode PM1 fraction
                  TEMP_DATA( 23 ) = fPM25( 1 )            ! i-mode fine fraction
                  TEMP_DATA( 24 ) = fPM25( 2 )            ! j-mode fine fraction
                  TEMP_DATA( 25 ) = fPM25( 3 )            ! k-mode fine fraction
                  TEMP_DATA( 26 ) = fPM10( 1 )            ! i-mode PM10 fraction
                  TEMP_DATA( 27 ) = fPM10( 2 )            ! j-mode PM10 fraction
                  TEMP_DATA( 28 ) = fPM10( 3 )            ! k-mode PM10 fraction
                  TEMP_DATA( 29 ) = fAMS( 1 )             ! i-mode fraction transmitted through AMS
                  TEMP_DATA( 30 ) = fAMS( 2 )             ! j-mode fraction transmitted through AMS
                  TEMP_DATA( 31 ) = fAMS( 3 )             ! k-mode fraction transmitted through AMS

                  IF ( AERO_CHEM_SET ) THEN
                     TEMP_DATA( 32 ) = GAMMA_N2O5IJ( C,R,L ) ! fine N2O5 heterorxn probability
                     TEMP_DATA( 33 ) = GAMMA_N2O5K( C,R,L )  ! coarse N2O5 heterorxn probability
                     TEMP_DATA( 34 ) = YCLNO2IJ( C,R,L )     ! fine CLNO2 heterorxn yield
                     TEMP_DATA( 35 ) = YCLNO2K( C,R,L )      ! coarse CLNO2 heterorxn yield
                     TEMP_DATA( 36 ) = GAMMA_IEPOX( C,R,L )  ! IEPOX heterorxn uptake coeff
                  ELSE
                     TEMP_DATA( 32:36 ) = 0.0
                  END IF
                  TEMP_DATA( 37 ) = AEROMODE_DENS( 1 )    ! Aitken Mode Density [kg m-3]
                  TEMP_DATA( 38 ) = AEROMODE_DENS( 2 )    ! Accumulation Mode Density [kg m-3]
                  TEMP_DATA( 39 ) = AEROMODE_DENS( 3 )    ! Coarse Mode Density [kg m3-]
                  IF ( NUM_PMDIAG_SPC .GT. 39 ) THEN ! AERO6i
                     TEMP_DATA( 40 ) = KPARTIEPOX( C,R,L )  ! Particle-Phase Rxn Rate Constant
                     TEMP_DATA( 41 ) = GAMMA_IMAE( C,R,L )  ! IMAE Heterorxn Uptake Coeff
                  END IF
               END IF

! *** Calculate 2nd and 3rd moments of the "dry" aerosol distribution
!     NOTE! "dry" aerosol excludes both H2O and SOA  (Jan 2004 --SJR)
!     EXCEPT!  nonvolatile SOA is part of dry aerosol (Oct 2007 --PVB)
               CALL CALCMOMENTS( .FALSE. )
               CALL GETPAR( FIXED_sg ) ! update AEROMODE_DIAM,DENS,SDEV

               IF ( PMDIAG .OR. APMDIAG ) THEN
                  TEMP_DATA(  1 ) = EXP( AEROMODE_LNSG( 1 ) )
                  TEMP_DATA(  2 ) = EXP( AEROMODE_LNSG( 2 ) )
                  TEMP_DATA(  3 ) = EXP( AEROMODE_LNSG( 3 ) )
                  TEMP_DATA(  4 ) = AEROMODE_DIAM( 1 ) * 1.0E6  ! dry i-mode diam.
                  TEMP_DATA(  5 ) = AEROMODE_DIAM( 2 ) * 1.0E6  ! dry j-mode diam.
                  TEMP_DATA(  6 ) = AEROMODE_DIAM( 3 ) * 1.0e6  ! dry k-mode diam.
                  TEMP_DATA( 13 ) = MOMENT3_CONC( 1 )    ! dry i-mode 3rd moment
                  TEMP_DATA( 14 ) = MOMENT3_CONC( 2 )    ! dry j-mode 3rd moment
                  TEMP_DATA( 15 ) = MOMENT3_CONC( 3 )    ! dry k-mode 3rd moment
                  TEMP_DATA( 19 ) = AIRRH                ! relative humidity
               END IF

C *** Accumulate wet diameters, 2nd, and 3rd moments to average aerosol diagnostic array
               IF ( APMDIAG .AND. L .LE. APMDIAG_ZHI ) THEN
                  DO V = 1, NUM_PMDIAG_SPC
                     IF ( A_PMDIAG_SPC_MAP( V ) .EQ. 1 ) THEN
                        A_PMDIAG_SPC( C,R,L,V ) = A_PMDIAG_SPC( C,R,L,V ) + TEMP_DATA( V )
                     END IF
                  END DO 
               END IF

! *** Write dry aerosol distribution parameters to aerosol diagnostic array
               IF ( WRITETIME .AND. PMDIAG ) PMDIAG_SPC( C,R,L,: ) = TEMP_DATA

! *** Collect Aerosol Sub-Process Rates in shared arrays
               AERO_COND  ( C,R,L,: ) = COND_BUDGET( : )
               AERO_COAG  ( C,R,L,:,: )=COAG_BUDGET( :,: )
               AERO_NPF   ( C,R,L,: ) = NPF_BUDGET( : )
               AERO_GROWTH( C,R,L,: ) = GROWTH_BUDGET( : )

#ifdef twoway
               IF ( CMAQ_WRF_FEEDBACK ) THEN
                  IF ( TIME_TO_CALL_FEEDBACK_WRITE ) THEN
                     CALL FEEDBACK_WRITE ( C, R, L, CGRID(C,R,L,:), CGRID(C,R,L,O3),
!    &                                     aeromode_diam, AEROMODE_LNSG, jdate, jtime)
     &                                     JDATE, JTIME )
                  END IF
               END IF
#endif

            END DO ! loop on MY_COLS
         END DO ! loop on MY_ROWS
      END DO ! loop on NLAYS

C *** From AEROPROC call to ORGAER...
!     If ( pfc .gt. 0 ) Then
!        Write( xmsg,'(a,i8)' ) 'possible false convergences in soa bisection:', pfc
!        Call m3warn( pname, 0, 0, xmsg )
!        pfc = 0
!     End If

C *** If last call this hour, write visibility information.
      IF ( WRITETIME ) THEN
         MDATE = JDATE
         MTIME = JTIME
         CALL NEXTIME ( MDATE, MTIME, TSTEP( 2 ) )
         WSTEP = 0
         WRITETIME = .FALSE.

#ifdef parallel_io
         IF ( FIRST_CTM_PMDIAG_1 ) THEN
            FIRST_CTM_PMDIAG_1 = .FALSE.
            IF ( PMDIAG ) THEN
               IF ( .NOT. IO_PE_INCLUSIVE ) THEN
                  IF ( .NOT. OPEN3( CTM_PMDIAG_1, FSREAD3, PNAME ) ) THEN
                     XMSG = 'Could not open ' // TRIM( CTM_PMDIAG_1 )
                     CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
                  END IF
               END IF
            END IF
            IF ( APMDIAG ) THEN
               IF ( .NOT. IO_PE_INCLUSIVE ) THEN
                  IF ( .NOT. OPEN3( CTM_APMDIAG_1, FSREAD3, PNAME ) ) THEN
                     XMSG = 'Could not open ' // TRIM( CTM_APMDIAG_1 )
                     CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
                  END IF
               END IF
            END IF
         END IF
#endif

C *** Write data to the aerosol diagnostic file.
         IF ( PMDIAG ) THEN

            IF ( .NOT. WRITE3( CTM_PMDIAG_1, ALLVAR3,
     &                         MDATE, MTIME, PMDIAG_SPC ) ) THEN
               XMSG = 'Could not write ' // CTM_PMDIAG_1 // ' file'
               CALL M3EXIT ( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF

            WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &                     'Timestep written to', CTM_PMDIAG_1,
     &                     'for date and time', MDATE, MTIME

         END IF
     
         IF ( APMDIAG ) THEN
            IF ( .NOT. END_TIME ) THEN   ! ending time timestamp
               CALL NEXTIME ( MDATE, MTIME, -TSTEP(1) )
            END IF
         END IF

C *** Write data to the average aerosol diagnostic file.
         IF ( APMDIAG ) THEN

            DO V = 1, NUM_PMDIAG_SPC
 
               IF ( A_PMDIAG_SPC_MAP( V ) .EQ. 1 ) THEN
                  A_PMDIAG_SPC( :,:,:,V ) = A_PMDIAG_SPC( :,:,:,V ) / N_APMDIAG_STEP 

                  IF ( .NOT. WRITE3( CTM_APMDIAG_1, PMDIAG_SPC_RECORD( V )%SPC_NAME,
     &                               MDATE, MTIME, A_PMDIAG_SPC( :,:,:,V ) ) ) THEN
                     XMSG = 'Could not write ' // CTM_APMDIAG_1 // ' file'
                     CALL M3EXIT ( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
                  END IF
                  A_PMDIAG_SPC( :,:,:,V ) = 0.0
               END IF
            END DO

            WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &                     'Timestep written to', CTM_APMDIAG_1,
     &                     'for date and time', MDATE, MTIME

            N_APMDIAG_STEP = 0
         END IF
     
      END IF   ! WRITETIME

      RETURN
      END
