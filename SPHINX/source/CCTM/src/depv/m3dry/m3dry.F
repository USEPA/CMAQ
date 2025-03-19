
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
C $Header: /project/work/rep/arc/CCTM/src/depv/m3dry/m3dry.F,v 1.12 2012/01/19 14:19:43 yoj Exp $

C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE m3dry ( jdate, jtime, dtsec, cgridl1, depvel_gas, pvd, nh3_ddep)

C-------------------------------------------------------------------------------
C Name:     Models-3 Dry Deposition
C Purpose:  Computes dry deposition velocities using Rst and Ra, and
C           elements of ADOM DD model.
C Revised:  21 Jan 1998  Original version.  (J. Pleim and A. Bourgeois)
C           18 Sep 2001  Made general for USGS 24-category system.
C                        (T. Otte, J. Pleim, and W. Hutzell)
C           14 Jan 2002  Added temperature dependence to Henry's Law
C                        constants.  Added temperature and pressure
C                        dependence to diffusivity.  Added new dry
C                        deposition species, methanol.  (Y. Wu and T. Otte)
C           18 Jan 2002  Changed the reference wet cuticle resistance.
C                        (J. Pleim)
C           09 Jun 2003  Added logic for modeling snow covered surfaces.
C                        Changed the reactivities for SO2, HNO3 and NH3.
C                        Changed pH values to have an east-west variation.
C                        Using the Henry's law constant function from CMAQ in
C                        place of local code.  Also changed the code for
C                        deposition to water to use a pH of 8.1 and the
C                        temperature of water in calculating the Henry's law
C                        constant.  Adjusted values of RSNOW0 = 1000 and
C                        A(NH3) = 20.  Added new dry deposition species: N2O5,
C                        NO3, Generic_aldehyde.  Corrected diffusivities of
C                        chemicals and water and viscosity of air to all be at
C                        the same temperature (273.15K).  Temperature and
C                        pressure adjustments to the values are not needed
C                        because the diffusivities and viscosity are always used
C                        as ratios, so the temperature-pressure dependence was
C                        removed.  Removed dry deposition species, ATRA and
C                        ATRAP, from output.  (D. Schwede, J. Pleim, and
C                        T. Otte)
C           28 Feb 2005  Added optional dry deposition species for chlorine
C                        and mercury.  (G. Sarwar, R. Bullock, and T. Otte)
C           02 Feb 2006  Added mesophyll resistance to dry deposition velocity
C                        calculation, and defined non-zero value for mercury.
C                        (D. Schwede, J. Pleim, and R. Bullock)
C           01 Aug 2007  Added a non-zero mesophyll resistance for NO, NO2, and
C                        CO.  Restored wet cuticle resistance for O3 based on
C                        field study measurements.  Added wet ground resistance.
C                        Changed ground resistance to include partitioning of
C                        wet and dry ground.  Updated pH of rain water for
C                        eastern United States and outside of North America.
C                        Changed reactivity for PAN.  Removed dry deposition
C                        velocity calculations for obsolete chlorine species
C                        ICL1 and ICL2.  Corrected error in the calculation of
C                        surface resistance over water where (Sc/Pr)**(2/3) had
C                        been inadvertently omitted from the numerator.
C                        Surface resistance over water is now a function of
C                        species.  Surface resistance over water now uses wet
C                        bulb temperature rather than ground (water) temperature
C                        in the calculation of the effective Henry's law
C                        constant, and the algorithm has been updated.  Changed
C                        (Sc/Pr)**(2/3) over water to a species-dependent,
C                        meteorologically dependent variable.  Effective Henry's
C                        law constant over land now uses 2-m temperature rather
C                        than layer 1 temperature. Changed ES
C                        into ES_AIR and ES_GRND, and changed QSS into QSS_AIR
C                        and QSS_GRND to clarify usage.  (J. Pleim, E. Cooter,
C                        J. Bash, T. Otte, and G. Sarwar)
C           07 Dec 2007  Add into CMAQ for in-line deposition velocities.
C                        (W. Hutzell, J. Young and T. Otte)
C           07 Jan 2008  Changed the value of d3, the scaling parameter used to
C                        estimate the friction velocity in surface waters from
C                        the atmospheric friction velocity to a value following
C                        Slinn et al. (1978) and Fairall et al. (2007).
C                        (J. Bash)
C           01 Feb 2008  Added bidirectional NH3 flux calculations. (J. Pleim and
C                        J. Young)
C           20 Mar 2008  Added a trap for undefined dry deposition velocities
C                        (e.g., NaN's).  (T. Otte)
C           21 Mar 2008  Added heterogeneous reaction for HONO. It affects HONO, NO2
C                        and HNO3 (G. Sarwar)
C           30 Apr 2008  Added five air toxic species to output.  (W. Hutzell
C                        and T. Otte)
C           August 2008  Applied a minimum value to ustg (0.001 m/s) to prevent
C                        negative rbg values in the bidi calculation (J. Pleim)
C           05 Oct 2009  Added condition that vegetation fraction must be
C                        greater than zero to be considered a land point.  This
C                        works around intermittent inconsistencies in surface
C                        fields in some WRF data sets.  (T. Otte)
C           Dec 2009     Revised bidirectional NH3 flux calculations to use soil
C                        Gamma values read from gridded file.  Bidi flux calcs
C                        based on comparisons with Lillington, NC corn data 2007 (J. Pleim)
C           30 Mar 2010  Modified to output the NH3 bidi stomtal, cuticle and soil component
C                        fluxes and chaged NH3 bidi variables used in estimating the compensation
C                        point double. (J. Bash)
C           16 Feb 2011 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C           20 May 2011  D.Schwede: add MOSAIC processing
C           14 Jul 2011  Replaced dw25 calculation with Hayduk and Laudie method.
C                        LeBas molar volumes are from the Schroeder additive method
C                        with the exception of HGIIGAS (modeled as HgCl2) which was
C                        obtained using the Tyn and Calus method. Also, ICL1 and ICL2
C                        were removed. (D. Schwede)
C           27 Jul 2011 J.Bash: Parmaterized the mesophyll resistance as a function
C                        of solubility following Wesely 1989 Atmos Environ.
C           15 Aug 2011  Modified HONO calculation so that deposition velocity for NO2
C                        that is output in DEPV file does not include the loss due to
C                        the heterogeneous reaction. This additional loss is now
C                        accounted for in vdiff.F (D. Schwede and G. Sarwar)
C           29 Aug 2011 Added NH3 bidirectional flux variables and modules and integrated
C                       NH3 bidi algorithms with MOSAIC algorithms. NH3 bidi routines now
C                       read in foratted EPIC output and maintain a soil NH4 budget and
C                       fluxes are calculated for individual land cover types.
C                       (J. Bash and D. Schwede)
C           22 Sep 2011 -- incorporated twoway model implementation
C                       -- removed non-use dluse array
C                          (David Wong)
C           26 Sep 2011 -- made the number of actual and dummy arguments the same in
C                          calling subroutine Init_ABFlux
C                          (David Wong)
C           7 Jun 2012  Allow read of C-staggered (UWINDC and VWINDC) wind and B-staggered
C                       (UWIND and VWIND) from MET_DOT_3D to maintain compatibility with
C                       current and previous versions of MCIP (J. Bash)
C          12 Sep 2012  Added code for NLCD40 land use classification (D. Schwede)
C          20 May 2013  Added code for increased ozone deposition to oceans due to iodide (D. Schwede)
C          30 Jul 2013  Added new organic N species for explicit deposition due to differences in Henry's Law
C          14 Nov 2013  Added MPAN as an explicit species, separating it from PANX
C          28 Aug 2014  G. Sarwar: added deposition data for CLNO2
C           7 Nov 2014  J. Bash: Modified for the restructuring of vidff. Moved constants and data to 
C                       ASX_DATA_MOD.
C            June 2018  revised for v5.3 - removed mosaic, new epic-bidi - j pleim
C          01 Feb 2019  D. Wong: Implemented centralized I/O approach, removed all MY_N clauses
C          12 Dec 2019  S.L.Napelenok: update for cmaq 5.3.1
C          August 2020  J Pleim: corrected TSTEP to be DTSEC. Corrects NH3 bidi emis, dep outputs
C-------------------------------------------------------------------------------

      USE RUNTIME_VARS
      USE HGRD_DEFN      ! horizontal grid specifications
      USE DEPVVARS
      USE VGRD_DEFN      ! to get VGTYP_GD
      USE UTILIO_DEFN
#ifdef mpas
      Use util_module, only : secsdiff
#else
#ifdef parallel
      USE SE_MODULES     ! stenex (using SE_COMM_MODULE)
#else
      USE NOOP_MODULES   ! stenex (using NOOP_COMM_MODULE)
#endif     
#endif     
      Use ABFlux_Mod     ! bidi NH3 exchange routines
      Use ASX_DATA_MOD   ! contains CONSTANTS include
      Use BIDI_MOD, only:HGBIDI    ! contains CONSTANTS include
      Use HGSIM
#ifdef twoway
      USE twoway_data_module, only : num_land_cat
#endif
      Use CENTRALIZED_IO_MODULE, only : WR_AVAIL

#ifdef sens
      USE DDM3D_DEFN, ONLY : NP, NPMAX, S_PVD, S_CGRIDL1
#endif

      IMPLICIT NONE
C Includes:

      INCLUDE SUBST_FILES_ID  ! file name parameters
      INCLUDE SUBST_PE_COMM   ! PE communication displacement and direction

C Arguments:

      INTEGER,     INTENT( IN )  :: jdate
      INTEGER,     INTENT( IN )  :: jtime
      REAL,        INTENT( IN )  :: dtsec      ! model time step (sec)
      REAL,        INTENT( IN )  :: cgridl1( :,:,: )    ! layer 1 concentrations
      REAL,        INTENT( OUT ) :: depvel_gas( :,:,: )
      REAL,        INTENT( OUT ) :: pvd( :,:,: )
      REAL,        INTENT( OUT ) :: nh3_ddep(:,:)

C Local Variables:

      INTEGER                    :: c,r,l,n              ! loop variables
      INTEGER                    :: elapsedsec,idt
      INTEGER                    :: ifsnow               ! 1=snow
      INTEGER, SAVE, ALLOCATABLE :: lstwetdate( :,: )
      INTEGER, SAVE, ALLOCATABLE :: lstwettime( :,: )

      REAL                       :: cp_air               ! specific heat of moist air
      REAL                       :: ctemp2               ! temp2 [C]
      REAL                       :: dw
      REAL                       :: dw25       ! diffusivity of water at 298.15 k
      REAL                       :: hcan
      REAL                       :: heff                 ! effective Henry's Law constant
      REAL                       :: heff_ap              ! Henry's Law constant for leaf apoplast M/atm
      REAL                       :: hplus
      REAL                       :: kvisw      ! kinematic viscosity of water [cm^2/s]
      REAL                       :: lv                   ! latent heat of vaporization
      REAL                       :: laicr                ! col row lai
      REAL                       :: q2p0cr               ! cell 2.0 m water vapor mix ratio
      REAL                       :: rac
      REAL                       :: rbc
      REAL                       :: rci
      REAL                       :: rcut
      REAL                       :: rgnd
      REAL                       :: rgndc
      REAL                       :: rgw                  ! resist for water-covered sfc
      REAL                       :: rh_grnd              ! rel humidity (ground)
      REAL                       :: rinc
      REAL                       :: rsnow
      REAL                       :: rstom
      REAL                       :: rsurf
      REAL                       :: rwet                 ! wet sfc resist (cuticle or grnd)
      REAL                       :: rwetsfc
      REAL                       :: scw_pr_23            ! (scw/pr)**2/3
      REAL                       :: temp2p0cr            ! cell 2.0-m temp
      REAL                       :: tempgcr              ! cell ground temp
      REAL                       :: tw                   ! wet bulb temp.
      REAL                       :: ustarcr              ! cell friction velocity
      REAL                       :: vegcr                ! cell veg coverage fraction
      REAL                       :: wrmax
      REAL,    SAVE              :: xcent
      REAL,    SAVE              :: ycent
      REAL                       :: xm                   ! liquid water mass frac
      REAL                       :: rh_func              ! Simple RH function for the development of a water film on leaf cuticles 
      Real,            Parameter :: MolNH3   = 17.01     ! g/mol NH3  for NH3 bidi to convert to microg/m3
      Real,            Parameter :: MolAir   = 29.97     ! g/mol dry air
      Real                       :: cnh3,tpvd,lnh3,femis,fdep

      REAL,    SAVE, ALLOCATABLE :: delta     ( :,: )

      REAL,            EXTERNAL  :: hlconst              ! [M / atm]

      LOGICAL                    :: effective  ! true=compute effective Henry's Law const
      LOGICAL, SAVE              :: first_call = .TRUE.

      CHARACTER( 16 ), PARAMETER :: pname      = 'M3DRY'
      CHARACTER( 96 )            :: xmsg = ' '

      REAL, SAVE                 :: scc_pr_23( ltotg )        ! (SCC/PR)**2/3, fn of DIF0

C-------------------------------------------------------------------------------
C gas phase species indexes
      Integer, Save              :: n_HONO
      Integer, Save              :: l_HONO         ! m3dry map
      Integer, Save              :: n_NO2          ! CGRID map
      Integer, Save              :: l_NO2
      Integer, Save              :: n_O3          ! CGRID map
      Integer, Save              :: l_O3
      Integer, Save              :: n_NH3          ! CGRID map
      Integer, Save              :: l_NH3
      Integer, Save              :: n_HG          ! CGRID map
      Integer, Save              :: l_HG
      Integer, Save              :: n_HGII          ! CGRID map
      Integer, Save              :: l_HGII
C-------------------------------------------------------------------------------
C For ozone exchange over the ocean

      REAL                       :: pchang      ! p value used in equation (12) in Chang et al( 2004)
      REAL                       :: kwchang     ! kw value used in Chang et al (2004)
      REAL                       :: ciodide     ! iodide concentration (umol/L)
      REAL                       :: qiodide     ! q in Chang et al (2004)

C-------------------------------------------------------------------------------
C For heterogenous hono on surfaces
      REAL                       :: conc_no2                  ! concentration of NO2
      REAL                       :: kno2                      ! first order rate constant for the heterogenous reaction [1/s]
      REAL                       :: surf_bldg                 ! Surface area of bldgs to volume of air [-]
      REAL                       :: surf_leaf                 ! Surface area of leaves to volume of air [-]
#ifdef sens
      REAL                       :: s_conc_no2( NPMAX )
#endif

C-------------------------------------------------------------------------------
C For Mercury bidirectional flux
      REAL                       :: chg            ! gem layer one concentration
      REAL                       :: awhg           ! GEM Oswald solubility coeff
      REAL                       :: chgIIgas       ! rgm layer one concentration
      REAL                       :: vdHgII         ! divalent mercury deposition velocity
      REAL                       :: vdHg           ! elemental mercury deposition velocity
      REAL                       :: dpvd           ! dummy pvd variable passed to the hgasx subroutine
      REAL                       :: dvel           ! dummy depvel_gas returned from ATX

C-------------------------------------------------------------------------------


      IF ( first_call ) THEN
         first_call = .FALSE.

         DO l = 1, n_spc_m3dry
            IF ( dif0( l ) > 0.0 ) THEN
               scc_pr_23( l ) = ( ( kvis / dif0( l ) ) / pr ) ** twothirds
            ELSE
               scc_pr_23( l ) = 0.0
            END IF
         END DO

#ifdef mpas
         xcent = 0.0
         ycent = 0.0
#else
         IF ( .NOT. desc3( met_cro_2d ) ) THEN
            xmsg = 'Could not get  met_cro_2d  file description'
            CALL m3exit( pname, jdate, jtime, xmsg, xstat2 )
         END IF

         xcent = real( xcent3d, 4 )
         ycent = real( ycent3d, 4 )
#endif

         IF ( abflux ) THEN
            CALL Init_ABFlux( jdate, jtime )
         END IF

         IF ( .NOT. ALLOCATED ( delta ) ) THEN
            ALLOCATE ( delta( ncols,nrows ) )
            delta( :,: ) = 0.0
         END IF

         IF ( .NOT. ALLOCATED ( lstwetdate ) .AND. .NOT. WR_AVAIL ) THEN
            ALLOCATE ( lstwetdate( ncols,nrows ) )
            lstwetdate( :,: ) = 0
         END IF

         IF ( .NOT. ALLOCATED ( lstwettime ) .AND. .NOT. WR_AVAIL ) THEN
            ALLOCATE ( lstwettime( ncols,nrows ) )
            lstwettime( :,: ) = 0
         END IF
C-----------------------------------------------------------------
C        Species maps
C-----------------------------------------------------------------
         l_HONO = 0
         n_HONO = 0
         l_NO2  = 0
         n_NO2  = 0
         l_O3   = 0
         n_O3   = 0
         l_NH3  = 0
         n_NH3  = 0
         l_HG   = 0
         n_HG   = 0
         l_HGII = 0
         n_HGII = 0
         n = 0
         maploop: DO l = 1, n_spc_m3dry
            IF ( .NOT. use_depspc( l ) ) CYCLE maploop
            n = n + 1

            If ( depspc( l ) .EQ. 'NO2' ) THEN
               l_NO2 = l
               n_NO2 = n
            End If
            If ( depspc( l ) .EQ. 'HONO' ) THEN
               l_HONO = l
               n_HONO = n
            End If
            If ( depspc( l ) .EQ. 'O3' ) THEN
               l_O3 = l
               n_O3 = n
            End If
            If ( depspc( l ) .EQ. 'NH3' ) THEN
               l_NH3 = l
               n_NH3 = n
            End If
            If ( depspc( l ) .EQ. 'HG' ) THEN
               l_HG = l
               n_HG = n
            End If
            If ( depspc( l ) .EQ. 'HGIIGAS' ) THEN
               l_HGII = l
               n_HGII = n
            End If
         END DO maploop

      END IF   ! first_call

C-------------------------------------------------------------------------------
C Loop over grid cells and calculate dry deposition.
C-------------------------------------------------------------------------------

      effective = .TRUE.
      idt = nint(dtsec)
      depvel_gas( :,:,: ) = 0.0  ! initialize for this time period
      pvd       ( :,:,: ) = 0.0  !     "       "    "    "    "

#ifdef sens
      s_pvd     ( :,:,:,: ) = 0.0
#endif

      DO r = 1, nrows
      DO c = 1, ncols

         laicr     = MET_DATA%LAI   ( c,r )
         q2p0cr    = MET_DATA%Q2    ( c,r )
         temp2p0cr = MET_DATA%TEMP2 ( c,r )
         tempgcr   = MET_DATA%TEMPG ( c,r )
         ustarcr   = MET_DATA%USTAR ( c,r )
         vegcr     = MET_DATA%VEG   ( c,r )
         ifsnow    = MAX( 0, NINT( MET_DATA%SNOCOV( c,r ) ) )

! Calculate the relative humidity of air and ground.
         
         rh_grnd  = 100.0 * q2p0cr / MET_DATA%QSS_GRND( c,r )
         rh_grnd  = MIN( 100.0, rh_grnd )

         IF ( ( NINT(GRID_DATA%LWMASK( c,r )) .NE. 0 ) .AND. ( vegcr .GT. 0.0 ) ) THEN  ! land

            IF ( .NOT. WR_AVAIL ) THEN  ! approx canopy wetness - dew from Wesely

            ! canopy is wet if > trace precip. or moist with light winds

               IF ( ( MET_DATA%RN( c,r ) + MET_DATA%RC( c,r ) .GT. 0.025 ) .OR.
     &              ( (0.6 + MET_DATA%WSPD10( c,r ))*(100.0-rh_grnd) .LE. 19.0 ) ) THEN

                  delta( c,r )      = 1.0
                  lstwetdate( c,r ) = jdate
                  lstwettime( c,r ) = jtime

               ELSE

                  IF ( MET_DATA%RGRND( c,r ) .GT. 5.0 ) THEN  ! day (if at night, persist delta)

                  ! Determine if canopy was recently wet.

                     IF ( ( lstwetdate( c,r ) .GT. 0 ) .AND.
     &                    ( lstwettime( c,r ) .GT. 0 ) ) THEN  ! canopy recently wet

                        elapsedsec = secsdiff ( lstwetdate( c,r ),
     &                                          lstwettime( c,r ),
     &                                          jdate, jtime )

                        IF ( ( elapsedsec .GT.     0 ) .AND.     ! assume canopy stays
     &                       ( elapsedsec .LE. 7200 ) ) THEN    ! wet for 2 h
                           delta( c,r ) = 1.0
                        ELSE IF ( ( elapsedsec .GT.  7200 ) .AND.     ! ramp down DELTA
     &                            ( elapsedsec .LT. 10800 ) ) THEN    ! between 2 & 3 h
                           delta( c,r ) = ( 10800.0 - FLOAT( elapsedsec ) ) / 3600.0
                        ELSE
                           delta( c,r )      = 0.0
                           lstwetdate( c,r ) = 0
                           lstwettime( c,r ) = 0
                        END IF

                     END IF

                  END IF

               END IF


            ELSE  ! Already have canopy wetness explicitly from met model

               wrmax = 0.2e-3 * vegcr * laicr   ! [m]
               IF ( MET_DATA%WR( c,r ) .LE. 0.0 ) THEN
                  delta( c,r ) = 0.0
               ELSE
                  delta( c,r ) = MET_DATA%WR( c,r ) / wrmax   ! refer to SiB model
                  delta( c,r ) = MIN( delta( c,r ), 1.0 )
               END IF

            END IF   ! canopy wetness

      ! Assign a pH for rain water based on longitude if US simulation.
      ! Otherwise use default pH.  Use pH value in HPLUS calculation.

#ifdef mpas
            hplus = hplus_def
#else
            IF ( ( ycent .GE.   30.0 ) .AND. ( ycent .LE.  45.0 ) .AND.
     &           ( xcent .GE. -120.0 ) .AND. ( xcent .LE. -70.0 ) ) THEN
               IF ( GRID_DATA%LON( c,r ) .GT. -100.0 ) THEN
                  hplus = hplus_east
               ELSE
                  hplus = hplus_west
               END IF
            ELSE
               hplus = hplus_def
            END IF
#endif

         ELSE   ! water
      ! Calculate the water surface film temperature: wet bulb temperature.
      ! Wet bulb temperature based on eqn in Fritschen and Gay (1979).

            ctemp2 = temp2p0cr - stdtemp
            lv     = lv0 - dlvdt * ctemp2
            cp_air = 1004.67 * ( 1.0 + 0.84 * q2p0cr )               ! [J/kg/K]
            tw     = ( ( 4.71e4 * cp_air / lv ) - 0.870 ) + stdtemp  ! [K]

         END IF  ! land or water

      ! Loop over species to calculate dry deposition velocities.

         n = 0
         ddloop: DO l = 1, n_spc_m3dry

            IF ( .NOT. use_depspc( l ) ) CYCLE ddloop

            n = n + 1

!            IF ( ( NINT(GRID_DATA%LWMASK( c,r )) .EQ. 0 ) .OR. ( vegcr .EQ. 0.0 ) ) THEN  ! water
            IF ( NINT(GRID_DATA%LWMASK( c,r )) .EQ. 0 ) THEN  ! water

               IF ( l .EQ. l_HG ) THEN  ! elemental mercury gas
! calculate and save Henry's constant for use in HGSIM

                  IF( HGBIDI ) THEN 
                     awhg = hlconst( subname( l ), tw, effective, hplus_h2o )* 0.08205 * tw
                  ELSE
                     rsurf = 1.0e30
                  END IF
               ELSE   ! any species other than elemental mercury gas

         ! Use CMAQ function for calculating the effective Henry's Law
         ! constant.  Note that original M3DRY wants inverse, non-
         ! dimensional Henry's Law (caq/cg).   Water pH is different
         ! than rain, and we need to use the water temperature.

                  heff  = hlconst( subname( l ), tw, effective, hplus_h2o )

         ! Make Henry's Law constant non-dimensional.

                  heff  = heff * 0.08205 * tw

         ! from Hayduk and Laudie
                  dw25 = 13.26e-5 / ( 0.8904**1.14 * lebas( l )**0.589 )
                  kvisw = 0.017 * EXP( -0.025 * ( tw - stdtemp ) )
                  dw    = dw25 * ( tw * rt25inK ) * ( 0.009025 / kvisw )
                  scw_pr_23 = ( ( kvisw / dw ) / pr ) ** twothirds

                  IF ( l .EQ. l_O3 ) THEN   !implement Chang et al(2004)
c        pChang is a/H or alpha/H which would be 1/H in current model
c        note that in Chang et al (2004) and Garland et al (1980) their H is Cair/Cwater with is
c        the inverse of heff
                     pChang = 1.75
                     kwChang = (d3*ustarcr)/scw_pr_23


c        If a file of chlorophyll concentrations is provided, Iodide concentration are estimated from
c        a fit to the Rebello et al 1990 data. The slope and correlation are given in the paper
c        but not the intercept, so the data in Tables 3 & 4 were fit to get the relationship below.
c        The regression gives the concentration in umol/L and is converted to mol/L for use in Chang et al eq.
c        The slope and correlation are a slightly different than in Table 5.
c        If chlorophyll concs are not available, a constant value for [I-] of 100e-9 mol/l is used
c        Use ocean file variables to determine if the water cell is ocean or lake; method is only for ocean cells

                     IF (((GRID_DATA%OCEAN(c,r) + GRID_DATA%SZONE(c,r)) .GT. 0) .AND. (MET_DATA%SEAICE(c,r) .LE. 0)) THEN
c        Iodide in sea-water based on SST  (mol /dm-3)
                       ciodide = 1.46E6 * EXP( -9134.0 / tempgcr)
                       qiodide = ( ( 2.0e9 * ciodide * dw * 1e-4 ) ** 0.5 ) * heff
                       rgw = 1.0 / ( pChang * kwchang + qiodide )
                     ELSE                  ! O3 over land
                       rgw   = scw_pr_23 / ( heff * d3 * ustarcr )
                     END IF
                  ELSE                     ! other chems
                     rgw   = scw_pr_23 / ( heff * d3 * ustarcr )
                  END IF

                  rsurf = rgw

               END IF ! Hg

            ELSE   ! land

         ! Use CMAQ function for calculating the effective Henry's Law
         ! constant.  Note that original M3DRY wants inverse,
         ! non-dimensional Henry's Law (caq/cg).

               heff  = hlconst( subname( l ), temp2p0cr, effective, hplus )

         ! Make Henry's Law constant non-dimensional.

               heff  = heff * 0.08205 * temp2p0cr

         ! Wet surface resistance.  (Note DELTA = CWC in ADOM lingo.)
         ! This now applies to cuticle and ground.

               IF ( l .Ne. l_O3 ) THEN 
                  rwet = rcw0 / heff      ! wet cuticle
               ELSE
         ! Canopy level wet resistence Rwet to ozone was found to be about 200 s/m on basis of Keysburg exp
         ! Using LAI(1-sided) of about 6.25 measured at Keysburg gives leaf level rwet about 1250 s/m
                  rwet = 1250.0    ! s/m
         ! Leaf level rwet estimated from Altimir et al 2006 gives about 1350 s/m
               END IF

         ! Dry snow resistance.

               rsnow = rsnow0 * a0 / ar( l )

         ! If the surface is cold and wet, use dry snow.

               IF ( tempgcr .LT. stdtemp ) THEN
                  rwetsfc = rsnow
               ELSE
                  rwetsfc = rwet
               END IF

         ! Dry cuticle resistance.

               IF( l .Eq. l_O3 ) THEN
                  rh_func = max( 0.0,( MET_DATA%RH2( c,r ) - 70.0 )/30.0 )
                  rcut = rcut0 * a0 / ar( l ) * ( 1.0 -rh_func) + rwetsfc * rh_func
               ELSE IF ( l .Eq. l_NH3 ) THEN
         !        rcut = 4000.0 * EXP( -0.054 * MET_DATA%RH( c,r ) )
                  rcut = rwetsfc + (100.0-max(MET_DATA%RH2( c,r ),60.0))*laicr ! consistant with bidi
               ELSE 
                  rcut = rcut0 * a0 / ar( l )
               END IF

! Dry ground resistance.  (revised according to Erisman). Canopy height is assumed to be 10 * z0 according to PX-LSM
 
               hcan  = MET_DATA%Z0( c,r ) * 10.0
               rinc  = 14.0 * laicr * hcan / ustarcr
                IF ( depspc( l ) .NE. 'O3' ) THEN
                   rgnd  = rg0 * a0 / ar( l )
                   rgw  = rgwet0 / heff    ! wet ground
               ELSE
! Ozone soil resistence depends on soil moisture (Meszaros et al 2009, Biogeosci; Massman 2004, AE, Ran et al 2016, CMAS)
                   IF ( tempgcr .GE. stdtemp ) THEN
                     rgnd  = 200. + 300. * MET_DATA%SOIM1(c,r)/GRID_DATA%WFC (c,r)
                     rgnd = min (500.0, rgnd)
                     rgw = 500.              ! wet ground for ozone (Massman 2004)
                   ELSE
                     rgnd  = 200. + (rsnow-200.) * MET_DATA%SOIM1(c,r)/GRID_DATA%WFC (c,r)
                     rgnd = min (rsnow, rgnd)
                   endif
               END IF 

               IF ( tempgcr .LT. stdtemp ) THEN   ! frozen ground water = ice
                  rgw = rsnow
               END IF

         ! Determine the snow liquid water mass fraction (0.0 to 0.5).

               xm = 0.02 * ( temp2p0cr - ( stdtemp - 1.0 ) )**2
               xm = MIN (xm, 0.5)
               xm = MAX (xm, 0.0)
               IF ( temp2p0cr .LT. ( stdtemp - 1.0 ) ) xm = 0.0

         ! pathway through canopy to ground, either wet, dry, or snow

               rgndc = rinc + 1.0 / (real( 1-ifsnow )*( ( 1.0 - delta( c,r ) ) / 
     &                 rgnd + delta( c,r ) / rgw ) + real( ifsnow )
     &                 * ( (1.0 - xm) / rsnow + xm / (rsndiff + rgw) ) )

         ! Bulk stomatal resistance; include mesophyll resistance.

               heff_ap = hlconst( subname( l ), temp2p0cr, effective, hplus_ap )

               rstom = MET_DATA%RS( c,r ) * dwat / dif0( l )
     &               + 1.0 / ( heff_ap / 3000.0 + 100.0 * meso( l ) ) / laicr

         ! Bulk surface resistance.

               rci = vegcr
     &             * ( 1.0/rstom + (1.0-delta( c,r ) ) * laicr / rcut
     &             + ( delta( c,r ) * laicr / rwetsfc ) + 1.0 / rgndc )
     &             + (1.0 - vegcr) * ( real( 1-ifsnow ) * ( (1.0-delta( c,r ) ) /
     &                                         rgnd + delta( c,r ) / rgw)  
     &             + real( ifsnow ) * ( (1.0 - xm) / rsnow + xm / (rsndiff + rgw) ) )

               rsurf = 1.0 / rci

            END IF   ! land or water cell

         ! Compute dry deposition velocity.

            rbc = 5.0 / ustarcr * scc_pr_23( l )
            rac = MET_DATA%RA( c,r ) + rbc

            depvel_gas( n,c,r ) = 1.0 / ( rsurf + rac )

C--------------------------------------------------------------------------
            IF ( abflux .And. l .Eq. l_NH3 ) THEN   ! Ammonia Bidirectional Flux
               cnh3  = cgridl1( n,c,r ) * MolNH3/MolAir * 1000.0 * Met_Data%DENS1( C,R ) ! convert to micrograms/m3

               IF ( NINT( GRID_DATA%LWMASK( c,r ) ) .NE. 0 .and. ifsnow .EQ. 0 ) THEN  ! land

                  CALL Get_Flux( cnh3,rwetsfc,rgw,r,c,l,tpvd,lnh3, rbc, rinc, 
     >                           rstom, delta(c,r),rgnd,femis,fdep )
                   pvd( n,c,r ) = tpvd * 0.001 * MolAir / (MolNH3 * Met_Data%DENS1( C,R ))
                   nh3_ddep(c,r) = nh3_ddep(c,r) + fdep * dtsec * 1.0e-5  ! convert from microg/m2  to kg/ha
!                   nh3_emis(c,r) =  nh3_emis(c,r) + depvel_gas( n,c,r ) * cnh3* dtsec * 1.0e-5
                   depvel_gas( n,c,r ) = lnh3
               ELSE   ! water or snow covered
!                   ddep_nh3(c,r) = ddep_nh3(c,r) + depvel_gas( n,c,r ) * cnh3 * dt(2) * 1.0e-5
                   nh3_ddep(c,r) = nh3_ddep(c,r) + depvel_gas( n,c,r ) * cnh3* dtsec * 1.0e-5
               END IF
            END IF   ! 'abflux'

C--------------------------------------------------------------------------
            IF ( sfc_hono ) THEN

C HONO production via heterogeneous reaction on ground surfaces,
C 2NO2 = HONO + HNO3
C Rate constant for the reaction = (3.0E-3/60)* (A/V),
C where A/V is surface area/volume ratio
C HONO is produced and released into the atmosphere
C NO2 is lost via chemical reaction
C HNO3 is sticky and stays on the surfaces

C Calculate A/V for leaves.
C LAI was multiplied by 2 to account for the fact that surface area
C is provided by both sides of the leaves.
C Matthews Jones, Ammonia deposition to semi-natural vegetation,
C PhD dissertation, University of Dundee, Scotland, 2006

               surf_leaf = 2.0 * laicr / MET_DATA%ZF( c,r,1 )

C Calculate A/V for buildings and other structures.
C Buildings and other structures can provide additional surfaces in
C urban areas for the heterogeneous reaction to occur. However, such
C information is not readily available; in the absence of such information,
C it is scaled to purb(c,r). Svensson et al., (1987) suggests a typical value
C of 0.2 for A/V for buildings in urban environments. A maximum value of 0.2
C for A/V for buildings is assigned to the grid cell containing the highest
C purb(c,r) i.e., 100.0. A/V for buildings for other grid-cell is calculated
C as purb(c,r)*(0.2/100.0); Cai et al. (2006) used a value of 1.0 for their
C study at New York (total A/V)

               surf_bldg = GRID_DATA%PURB( c,r ) * 0.002

C Calculate rate constant for the reaction (psudeo-first order reaction,
C unit per second). Calculate pseudo-first order rate constant using Eq 1
C of Vogel et al. (2003).  Unit of KNO2 is in 1/min in the paper; divide it
C by 60 to convert it into 1/sec.

!                 kno2 = MAX( 0.0, 3.0E-3 * (surf_leaf + surf_bldg) / 60.0 )
                  kno2 = MAX( 0.0, 5.0E-5 * (surf_leaf + surf_bldg) )

C Determine NO2 concentration needed for HONO production term.

                  IF ( l .EQ. l_NO2 ) THEN
                     conc_no2 = cgridl1( n,c,r )

#ifdef sens
                     DO NP = 1, NPMAX
                        s_conc_no2( NP ) = s_cgridl1( NP, n,c,r )
                     END DO
#endif

C Loss of NO2 via the heterogeneous reaction is accounted as additional
C depositional loss. Add the loss of NO2 via the heterogeneous reaction
C to the regular deposition velocity (increased dep. vel.).  This will
C reduce the NO2 conc. in the atmosphere. Dep vel is adjusted back to the
C original value in vdiffacm2 after NO2 conc is reduced but before calculating
C depositional loss.

!                    depvel_gas( n,c,r ) = depvel_gas( n,c,r ) + 2.0 * kno2 * zf( c,r )

               END IF

C Calculate production (pvd) for HONO; unit = ppm * m/s

               IF ( l .EQ. l_HONO)
     &            pvd( n,c,r ) = kno2 * conc_no2 * MET_DATA%ZF( c,r,1 )
#ifdef sens
               IF ( l .EQ. l_HONO) THEN
                  DO NP = 1, NPMAX
                     s_pvd( NP,n,c,r ) = kno2 * s_conc_no2( NP ) * MET_DATA%ZF( c,r,1 )
                  END DO
               END IF
#endif
            END IF
C--------------------------------------------------------------------------
C Bidirectional mercury flux section
C--------------------------------------------------------------------------

            IF ( HGBIDI .AND.  l .EQ. l_HG ) THEN

               chg  = cgridl1( n_HG,c,r )

               IF ( NINT( GRID_DATA%LWMASK( c,r ) ) .EQ. 0  .AND. ( vegcr .EQ. 0.0 )) THEN ! water

                  depvel_gas( n_Hg,c,r ) = 1.0 / ( rsurf + rac )
                  vdhg = depvel_gas( n_Hg,c,r )
                  pvd( n_Hg,c,r )  = 0.0 ! in the nether regions of LU/LC

               ELSE ! terrestrial

                  awhg  = hlconst( subname( l ), temp2p0cr, effective, hplus )* 0.08205 * temp2p0cr

C************* Call the Hg air surface exchange subroutine *************************************************
                 CALL ATX(rbc, rcut, rwetsfc, rinc,rsnow, rgw, ifsnow, xm, dvel, chg, awhg,
     &                    dpvd, delta( c,r ),idt, c, r, l_HG, jdate, jtime )

                  pvd( n_HG,c,r ) = dpvd

                  depvel_gas( n_HG,c,r ) = dvel

               END IF ! water

            END IF ! 'HG'

               ! Check for negative values or NaN's

            IF ( depvel_gas( n,c,r ) .LT. 0.0 .OR.
     &           depvel_gas( n,c,r ) .NE. depvel_gas( n,c,r ) ) GO TO 999            

         END DO ddloop  ! (l = 1, n_spc_m3dry)

         IF ( HGBIDI .AND. NINT( GRID_DATA%LWMASK( c,r ) ) .EQ. 0 .AND. ( vegcr .EQ. 0.0 )) THEN ! water

            chgIIgas = cgridl1( n_HGII,c,r )
            vdhgII   = depvel_gas( n_HGII,c,r )

            CALL ASWX( chg, chgIIgas, vdhg, vdhgII, awhg,
     &                 dpvd, c, r, jdate, jtime, idt )

            pvd( n_Hg,c,r ) = dpvd

         END IF ! water

      END DO   ! c
      END DO   ! r

      RETURN

C-------------------------------------------------------------------------------
C Error-handling section.
C-------------------------------------------------------------------------------

999   CONTINUE
      WRITE( logdev,9003 ) c, r, TRIM( subname( l ) ), depvel_gas( n,c,r )
      xmsg = 'ABORT'

1001  CONTINUE

      CALL m3exit( pname, jdate, jtime, xmsg, xstat1 )

      RETURN

C-------------------------------------------------------------------------------
C Format statements.
C-------------------------------------------------------------------------------

6501  FORMAT(/ 1x, 70('='),
     &       / 1x, '--- Subroutine: M3DRY',
     &       / 1x, '---   Found canopy wetness (WR) in MET_CRO_2D ',
     &       / 1x, 70('=') /)

6503  FORMAT(/ 1x, 70('='),
     &       / 1x, '--- Subroutine: M3DRY',
     &       / 1x, '---   Found 2-m water vapor mixing ratio (Q2) in MET_CRO_2D',
     &       / 1x, 70('=') /)

9001  FORMAT( 'Failure reading ', a, 1x, 'from ', a )

9003  FORMAT(/ 1x, 70('*'),
     &       / 1x, '*** Subroutine: M3DRY',
     &       / 1x, '***   NEGATIVE or UNDEFINED Dry Deposition Velocity',
     &       / 1x, '***   Point   = ', 2i5,
     &       / 1x, '***   Species = ', a,
     &       / 1x, '***   Vd      = ', e13.6,
     &       / 1x, 70('*') /)

9005  FORMAT(/ 1x, 70('*'),
     &       / 1x, '*** Subroutine: M3DRY',
     &       / 1x, '***   Bidirectional flux for ammonia assumes ',
     &       / 1x, '***   USGS 24-category land use.',
     &       / 1x, '***   Need to update N_LUFRAC, LUF_FAC, and other places',
     &       / 1x, '***   Land use description is: ', a,
     &       / 1x, 70('*') /)
9033  FORMAT(/ 1x, 70('*'),
     &       / 1x, '*** Subroutine: M3DRY',
     &       / 1x, '***   NEGATIVE or UNDEFINED Land use specific Dry Deposition Velocity',
     &       / 1x, '***   Column  = ', i5,
     &       / 1x, '***   Row     = ', i5,
     &       / 1x, '***   Luc     = ', i5,
     &       / 1x, '***   Species = ', a,
     &       / 1x, '***   Vd      = ', e13.6,
     &       / 1x, '***   lai     = ', g10.3,
     &       / 1x, '***   z0      = ', g10.3,
     &       / 1x, '***   ustar   = ', g10.3,
     &       / 1x, '***   rgndc   = ', g10.3,
     &       / 1x, '***   Ra      = ', g10.3,
     &       / 1x, '***   Rbc     = ', g10.3,
     &       / 1x, '***   Rsurf   = ', g10.3,
     &       / 1x, '***   Rstom   = ', g10.3,
     &       / 1x, '***   Rcut    = ', g10.3,
     &       / 1x, '***   Rwetsfc = ', g10.3,
     &       / 1x, '***   Rgnd    = ', g10.3,
     &       / 1x, '***   Rgw     = ', g10.3,
     &       / 1x, '***   Delta   = ', g10.3,
     &       / 1x, '***   Veg     = ', g10.3,
     &       / 1x, '***   Xm      = ', g10.3,
     &       / 1x, '***   Rsndiff = ', g10.3,
     &       / 1x, '***   ifsnow  = ', i6
     &       / 1x, 70('*') /)

9034  FORMAT(/ 1x, 70('*'),
     &       / 1x, '*** Subroutine: M3DRY',
     &       / 1x, '***   NEGATIVE or UNDEFINED Land use specific Dry Deposition Velocity',
     &       / 1x, '***   Column  = ', i5,
     &       / 1x, '***   Row     = ', i5,
     &       / 1x, '***   Luc     = ', i5,
     &       / 1x, '***   Species = ', a,
     &       / 1x, '***   Vdfstj  = ', e13.6,
     &       / 1x, '***   Rcanj    = ', e13.6,
     &       / 1x, '***   Rstcj   = ', e13.6,
     &       / 1x, '***   Rstomj  = ', e13.6,
     &       / 1x, 70('*') /)

      END SUBROUTINE m3dry
