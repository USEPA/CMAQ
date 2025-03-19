
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
C  D. Wong 31 Jan 2019 - adopted the idea to process all twoway related environment
C                        variables in one place
C    1 Feb 19 David Wong: Implemented centralized I/O approach, removed all MY_N
C                         clauses
C    1 Aug 19 David Wong: Added a few more variables in the USE Only blcok for two-way model
C   30 Dec 19 S. Napelenok: ddm-3d implementation for v 5.3.1
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
      USE SOA_DEFN, ONLY: EXTRACT_SOA, UPDATE_ORGVAPOR
      USE AEROMET_DATA, ONLY: AIRTEMP, AIRPRES, AIRQV, AIRDENS, AIRRH, 
     &                        H2OSATVP, H2OVP, MWWAT, MWAIR, SRFTEMP
      USE UTILIO_DEFN, ONLY: XSTAT1, TIME2SEC, SEC2TIME, index1, nextime
#ifdef twoway
     &                       , INDEX1, XSTAT3
#endif
      USE CGRID_SPCS, ONLY: NSPCSD, N_CGRID_SPC
#ifdef twoway
     &                      , N_GC_CONC, GC_CONC, GC_STRT, GC_CONC_MAP
#endif
      USE RUNTIME_VARS, ONLY: LOGDEV, END_TIME
      USE AERO_BUDGET, ONLY: AERO_COAG, AERO_COND, AERO_GROWTH, AERO_NPF

#ifdef twoway
      USE twoway_data_module
#endif
      use CENTRALIZED_IO_MODULE, only : interpolate_var

#ifdef sens
      USE DDM3D_DEFN, ONLY : NP, NPMAX, SENGRID
      Use aero_ddm3d, ONLY : s_so4rate, s_precursor_conc, s_aerospc_conc, s_phg_rate
#endif 


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

C *** Variable to set time step for writing visibility file
      INTEGER, SAVE :: WSTEP  = 0   ! local write counter

C *** meteorological variables
      REAL PRES   ( NCOLS,NROWS,NLAYS )  ! Atmospheric pressure [ Pa ]
      REAL TA     ( NCOLS,NROWS,NLAYS )  ! Air temperature [ K ]
      REAL TEMP2  ( NCOLS,NROWS )        ! 2-meter temperature [ K ]
      REAL DENS   ( NCOLS,NROWS,NLAYS )  ! Air density [ kg/m**-3 ]
      REAL QV     ( NCOLS,NROWS,NLAYS )  ! Water vapor mixing ratio [ kg/kg ]

C *** variables computed and output but not carried in CGRID

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
      INTEGER IM

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

C *** values of AL, BL, and CL are from:
C     Alduchov and Eskridge, "Improved Magnus Form Approximations of
C                            Saturation Vapor Pressure,"
C                            Jour. of Applied Meteorology, vol. 35,
C                            pp 601-609, April, 1996.

      ESATL( TT ) = AL * EXP( BL * ( TT - 273.15 ) / ( TT - 273.15 + CL ) )

C *** End Statement Function  ********

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

         ALLOCATE ( AERO_COND(   NCOLS,NROWS,NLAYS,NSPCSD ),
     &              AERO_COAG(   NCOLS,NROWS,NLAYS,NSPCSD ),
     &              AERO_NPF (    NCOLS,NROWS,NLAYS,NSPCSD ),
     &              AERO_GROWTH ( NCOLS,NROWS,NLAYS,NSPCSD ),
     &              COND_BUDGET( NSPCSD ),
     &              COAG_BUDGET( NSPCSD,N_MODE ),
     &              NPF_BUDGET( NSPCSD ),
     &              GROWTH_BUDGET( NSPCSD ),
     &              STAT=ALLOCSTAT)

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

      END IF    ! FIRSTIME

      MDATE  = JDATE
      MTIME  = JTIME
      MSTEP = TIME2SEC( TSTEP( 2 ) )
      CALL NEXTIME ( MDATE, MTIME, SEC2TIME( MSTEP / 2 ) )

C *** Set floating point synchronization time step:
      DT = FLOAT( MSTEP ) ! set time step in seconds

C *** Get Meteorological Variables

C *** pressure [Pa]
      call interpolate_var ('PRES', mdate, mtime, PRES)

C *** temperature [K]
      call interpolate_var ('TA', mdate, mtime, TA)       ! Grid Cell Temp
      call interpolate_var ('TEMP2',mdate, mtime, TEMP2 ) ! 2-m Temp

C *** specific humidity [g H2O/g air]
      call interpolate_var ('QV', mdate, mtime, QV)

C *** air density [kg/m3]
      call interpolate_var ('DENS', mdate, mtime, DENS)

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
      AERO_COND     = 0.
      AERO_COAG     = 0.
      AERO_NPF      = 0.
      AERO_GROWTH   = 0.

C --------------------- Begin loops over grid cells --------------------------

      DO L = 1, NLAYS
         DO R = 1, NROWS
            DO C = 1, NCOLS

C *** Grid cell meteorological data.
               AIRTEMP  = TA   ( C,R,L )
               SRFTEMP  = TEMP2( C,R )     ! 2-meter temperature (K)
               AIRPRES  = PRES ( C,R,L )   ! Note pascals
               AIRQV    = QV   ( C,R,L )
               AIRDENS  = DENS ( C,R,L )
               H2OSATVP = ESATL( AIRTEMP )
               H2OVP    = AIRPRES * AIRQV / ( EPSWATER  + AIRQV )
               AIRRH    = MAX( 0.005, MIN( 0.99, H2OVP / H2OSATVP ) ) ! 0-1

! *** Initialize aerosol process variables
               COND_BUDGET = 0.
               COAG_BUDGET = 0.
               NPF_BUDGET = 0.
               GROWTH_BUDGET = 0.

C *** Extract grid cell concentrations of aero species from CGRID
C     into aerospc_conc in aero_data module (set minimum)
#ifdef sens
               CALL EXTRACT_AERO( CGRID( C,R,L,: ), .TRUE., SENGRID( C,R,L,:,: ), .TRUE. )
#else
               CALL EXTRACT_AERO( CGRID( C,R,L,: ), .TRUE. )
#endif

C *** Extract grid cell concentrations of gas precursors from CGRID (ppm)
C     into precursr_conc in precursor_data
#ifdef sens
               CALL EXTRACT_PRECURSOR( CGRID( C,R,L,: ), SENGRID( C,R,L,:,: ) )
#else
               CALL EXTRACT_PRECURSOR( CGRID( C,R,L,: ) )
#endif

C *** Calculate SO4RATE stored in module
               SO4RATE = REAL( PRECURSOR_CONC( SULPRD_IDX ), 4 ) / DT
#ifdef sens
               DO NP = 1, NPMAX
                  S_SO4RATE( NP ) =  S_PRECURSOR_CONC( SULPRD_IDX,NP ) / DT
               END DO
#endif
               IF ( PHGRXN_IDX .GT. 0 ) THEN
C *** Calculate PHG_RATE stored in module
                  PHG_RATE = REAL( PRECURSOR_CONC( PHGRXN_IDX ), 4 ) / DT
#ifdef sens
                  DO NP = 1, NPMAX
                     S_PHG_RATE( NP ) = S_PRECURSOR_CONC( PHGRXN_IDX,NP ) / DT
                  END DO
#endif
               ELSE
                  PHG_RATE = 0.0
#ifdef sens
                  S_PHG_RATE = 0.0
#endif
          END IF

C *** Extract soa concentrations from CGRID and 
C     convert M2 to wet
#ifdef sens
               CALL EXTRACT_SOA( CGRID( C,R,L,: ), SENGRID( C,R,L,:,: ), .TRUE. )
#else
               CALL EXTRACT_SOA( CGRID( C,R,L,: ) )
#endif

C *** Aerosol process routines
               CALL AEROPROC( DT, C, R, L )

C *** Update aerosol variables conc back into CGRID (set minimum) 
C     and convert M2 to dry and save as surface area
#ifdef sens
               CALL UPDATE_AERO( CGRID( C,R,L,: ), .TRUE., SENGRID(C,R,L,:,: ) )
#else
               CALL UPDATE_AERO( CGRID( C,R,L,: ), .TRUE. )
#endif

C *** Update precursor variables conc back into CGRID
#ifdef sens
               CALL UPDATE_PRECURSOR( CGRID( C,R,L,: ), SENGRID( C,R,L,:,: ) )
#else
               CALL UPDATE_PRECURSOR( CGRID( C,R,L,: ) )
#endif

C *** Update gas soa concentrations back to CGRID
#ifdef sens
               CALL UPDATE_ORGVAPOR( CGRID( C,R,L,: ), SENGRID( C,R,L,:,: ) )
#else
               CALL UPDATE_ORGVAPOR( CGRID( C,R,L,: ) )
#endif

C *** OUTPUT DIAGNOSTIC INFORMATION
C *** Get wet moment info (dry will be converted to wet)
               CALL calcmoments( .true. )
               CALL GETPAR( FIXED_sg ) ! update AEROMODE_DIAM,DENS,SDEV

! *** Calculate 2nd and 3rd moments of the "dry" aerosol distribution
!     NOTE! "dry" aerosol excludes both H2O and SOA  (Jan 2004 --SJR)
!     EXCEPT nonvolatile SOA is part of dry aerosol (Oct 2007 --PVB)
!               CALL CALCMOMENTS( .FALSE. )
!               CALL GETPAR( FIXED_sg ) ! update AEROMODE_DIAM,DENS,SDEV

! *** Collect Aerosol Sub-Process Rates in shared arrays
               DO V = 1,N_CGRID_SPC
                  AERO_COAG  ( C,R,L,V ) = SUM( COAG_BUDGET( V,: ) )
                  AERO_COND  ( C,R,L,V ) = COND_BUDGET( V )
                  AERO_NPF   ( C,R,L,V ) = NPF_BUDGET( V )
                  AERO_GROWTH( C,R,L,V ) = GROWTH_BUDGET( V )
               END DO
#ifdef twoway
               IF ( CMAQ_WRF_FEEDBACK ) THEN
                  IF ( TIME_TO_CALL_FEEDBACK_WRITE ) THEN
                     CALL FEEDBACK_WRITE ( C, R, L, CGRID(C,R,L,:), CGRID(C,R,L,O3),
     &                                     JDATE, JTIME )
                  END IF
               END IF
#endif

            END DO ! loop on COLS
         END DO ! loop on ROWS
      END DO ! loop on NLAYS

      RETURN
      END
