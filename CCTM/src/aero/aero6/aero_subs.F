
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

C routines for aerosol formation and transformation processes

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE AEROPROC( DT, COL, ROW, LAYER )

C-----------------------------------------------------------------------
C  SUBROUTINE AEROPROC advances the number, second moment, and mass
C   concentrations for each mode over the time interval DT.

C  KEY SUBROUTINES CALLED:
C     GETPAR, ORGAER5, GETCOAGS, INTERCOAG_GH, INTRACOAG_GH,
C     VOLINORG

C  KEY FUNCTIONS CALLED:  ERF, ERFC, GETAF

C  REVISION HISTORY:
C     Coded in December 1999 by Dr. Francis S. Binkowski
C      Modified from older versions used in CMAQ.

C FSB 05/17/00  new version of RPMARES included

C FSB 05/30/00  Fixed minor bug in awater.f

C FSB Correction to extinction coefficient in getbext.

C FSB 07/21/00 corrected units on ORGRATES and ORGBRATE_IN from
C      ppm/min to ppm/sec in AEROPROC and AEROSTEP.

C FSB 11/30/00 following changes from 07/28/00
C     Fixed problem for new emissions file version
C     Combined emissions for M3
C     Used a fixed value of Dpmin for plotting
C     Added variables OMEGA_AT & OMEGA_AC for partitioning
C     Eliminated the restriction on relative humidity for nucleation.
C     Added a branch in EQL for very low relative humidity (<1%).

C FSB Following changes to RPMARES:
C     Number of iterations reduced from 150 to 50.
C     Iterations are used only if 0.5 < = RATIO.
C     In calculating the molality of the bisulfate ion, a
C     MAX(1.0e-10, MSO4 ) is used.

C FSB 08/08/01 Changes to NEWPART to correct mass rate and to AEROSTEP
C     to correct sulfate, include emissions, and trap problem in
C     accumulation mode number calculation.

C FSB 09/19/01 Version AE3, major changes
C     Organics are done with Dr. Benedikt Schell's approach. (ORGAER3)
C     Particle production uses Kulmala approach (NEWPART3)
C     Condensational factors for sulfate and organics are calculated
C      separately.
C     Emissions are assumed to be input in vertical diffusion.
C     Include files replaced by Fortran 90 Modules.
C     The modules also contain subroutines.

C FSB 10/24/01 Added the treatment of gaseous N2O5 -> aerosol HNO3

C FSB 10/25/01 Changed the mass transfer calculation for Aitken to
C     accumulation mode by coagulation as recommended by Dr. Benedikt
C     Schell.

C SJR 04/24/02 Replaced thermodynamic code RPMARES with ISORROPIA
C     --Shawn J. Roselle

C GLG 04/04/03 Modifications to allow for evaporation of semi-volatile
C     organics from aerosol phase.  --Gerald L. Gipson

C FSB 11/18/03 Corrections to sulfate condensation.  Previously,
C     SCONDRATE was undefined when SO4RATE=0 and SCONDRATE was
C     negative when SO4RATE < DMDT_so4.

C PVB 01/08/04 Several changes in preparation for simulation of 2001
C      calendar year --Dr. Prakash V. Bhave
C    -Added interface to new subroutine, GETCOAGS, for calculating
C      coagulation rates.  GETCOAGS is used instead of Gauss-Hermite
C      quadrature for computational efficiency, by setting
C      FASTCOAG_FLAG = .TRUE.  --PVB
C    -Removed SOA from the definition of "DRY" aerosol.  Aerosol surface
C      area is now transported without SOA.  See notes in GETPAR, AERO,
C      and AERO_DEPV subroutines.  --SJR
C    -Moved EQL3 call from AEROPROC to AEROSTEP, immediately following
C      the ORGAER3 call.  This is a side effect of transporting aerosol
C      surface area without SOA.  --SJR
C    -New subroutine, HCOND3, to calculate condensation rates for
C      2nd and 3rd moments.  Results are unchanged.  --FSB
C    -Revised method of calculating SOA.  Partition SOA to the modes
C      in proportion to the amounts of total organic mass (SOA plus
C      primary) in each mode.  Modal geometric standard deviations are
C      now preserved during SOA condensation and evaporation. --FSB
C    -Combined the former subroutines AEROPROC and AEROSTEP into this
C      subroutine; retained the name AEROPROC.  --PVB

C PVB 09/21/04 added in-line documentation with input from FSB.
C     Changed MWH2SO4 from 98.07354 to 98.0 g/mol for consistency with
C     the mechanism files.

C PVB 09/27/04 removed the IF(XM3.GT.0.0) mode-merging precondition
C     because it caused significant erroneous mode crossover.  Fix
C     suggested by Dr. Chris Nolte.

C PVB 01/19/05 Added SO4RATE to the EQL3 call vector.  This is necessary
C     to ensure that the gas and inorganic fine PM (i+j) concentrations
C     are in thermodynamic equilibrium at the end of each time step.

C PVB 05/02/05 Modified ERF statement function for negative arguments such
C     that erf(-x) = -erf(x).  Previous version had erf(-x) = erf(x).

C PVB 04/06/06 Added GAMMA_N2O5 to the AEROPROC and EQL3 call vectors,
C     so it can be written to the aerosol diagnostic file.

C SLN 09/07/07 Several changes in preparation for new SOA module
C    -Removed Aitken-mode SOA species (CBLK(VORGAI),CBLK(VORGBI)) and
C     all of the related variables (OMASS_I, OMASS_J, FRACI, FRACJ,
C     OLD_M2_I, OLD_M3_I, NEW_M2_I, NEW_M3_I)
C    -Replaced the other SOA species (CBLK(VORGAJ),CBLK(VORGBJ)) with
C     precursor-specific SOA species (e.g., CBLK(VTOL1J)).
C    -Replaced SOA_A and SOA_B with an array, SOA_ALL.  Replaced
C     OLDSOA_A and OLDSOA_B with an array, OLDSOA.  Updated ORGAER3
C     call vector accordingly.
C    -Removed dependence of mode-merging criteria on SOA condensation
C     because that logic was flawed.  Deleted all variables needed
C     for that calculation (CHEMRATE_ORG, ORGRATE, ORGBRATE).

C PVB 11/02/07 Moved heterogenous N2O5 chemistry from EQL3 to a new
C     subroutine, HETCHEM.

C PVB 11/29/07 Implementation of new SOA formation mechanisms
C    -Replaced call to ORGAER3 with a call to ORGAER5
C    -Moved CBLK updates of SV species, SOA species, M2, and M3 into
C     ORGAER5 instead of updating those values in AEROPROC

C JOY 04/08/08 White space, alignment, readability

C JTK 04/17/08 Implemented coarse chemistry updates
C     -replaced condensation calculations with call to VOLINORG
C     -added code for coarse surface area and variable coarse std. dev.
C     -modified modal dynamics equations because new particle formation
C      and growth are now calculated in VOLINORG

C JOY 12/07/08 Removed unused EQL3 code, add column and row arguments to
C     VOLINORG for diagnostic logging

C SH  12/17/09 Major restructuring of all aerosol codes
C     -use new Fortran modules (e.g., AERO_DATA, SOA_DEFN); shortened call
C      vector; replaced CBLK with aerospc structure; eliminated AERO_INFO

C SH  03/10/11 Renamed met_data to aeromet_data

C HS  03/10/11 Added call to POAAGE between ORGAER and HETCHEM

C SR  03/25/11 Replaced I/O API include files with UTILIO_DEFN

C BH  09/30/13 Removed Call to HETCHEM and GAMMA_N2O5 argument
C              because of merging of gas and heterogeneous 
C              chemistry

C JB  02/07/14 Added Jai Xing's mass balance fix when there is excessive
C              condensation or evaporation under cold conditions

C BH  07/21/14 Removed call to POAAGE because reaction represented in
C              chemical mechanism

C GS/KF 09/23/14 Updated nucleation scheme with Vehkamaki et al. (2002)

C HP/BM  4/16 Removed m3_wet_flag since wet/dry status is now
C             tracked by AERO_DATA variable. Moments are always wet in AEROPROC
C             Updated treatment of aerosol moments

C SLN 12/30/19 Update for ddm-3d implementation in version 5.3.1

C  REFERENCES:
C   1. Binkowski, F.S. and U. Shankar, The regional particulate matter
C      model 1. Model description and preliminary results, J. Geophys.
C      Res., Vol 100, No D12, 26101-26209, 1995.

C   2. Binkowski, F.S. and S.J. Roselle, Models-3 Community
C      Multiscale Air Quality (CMAQ) model aerosol component 1:
C      Model Description.  J. Geophys. Res., Vol 108, No D6, 4183
C      doi:10.1029/2001JD001409, 2003.

C   3. Bhave, P.V., S.J. Roselle, F.S. Binkowski, C.G. Nolte, S. Yu,
C      G.L. Gipson, and K.L. Schere, CMAQ aerosol module development:
C      recent enhancements and future plans, Paper No. 6.8, CMAS Annual
C      Conference, Chapel Hill, NC, 2004.
C-----------------------------------------------------------------------
      
      USE AERO_DATA
      USE PRECURSOR_DATA      ! gas phase aero precursor data
      USE SOA_DEFN
      USE AEROMET_DATA
      USE UTILIO_DEFN
      USE AERO_BUDGET

#ifdef sens
      USE DDM3D_DEFN, ONLY : NP, NPMAX
      USE AERO_DDM3D, ONLY : s_aerospc_conc, s_precursor_conc,
     &                       s_so4rate
#endif


      IMPLICIT NONE

C *** arguments:
      REAL,    INTENT( IN ) :: DT          ! synchronization time step, sec
      INTEGER, INTENT( IN ) :: COL         ! Column of cell
      INTEGER, INTENT( IN ) :: ROW         ! Row of cell
      INTEGER, INTENT( IN ) :: LAYER       ! Layer of cell

C *** Parameters
      REAL, PARAMETER :: DIFFSULF = 9.36E-06  ! molecular diffusiviity for sulfuric acid
      REAL, PARAMETER :: SQRT2 = 1.4142135623731
      REAL, PARAMETER :: T0 = 288.15   ! [ K ]
      REAL, PARAMETER :: TWOTHIRDS   =  2.0 / 3.0
      REAL, PARAMETER :: ONE_OVER_TICE =  1.0 / 273.16

C *** local variables
      REAL         DIFFCORR   ! Correction to DIFFSULF & DIFFORG for pressure
      REAL         DV_SO4     ! molecular diffusivity of H2SO4 vapor after correction for ambient conditions
      REAL         SQRT_TEMP  ! square root of ambient temperature
      REAL         XLM        ! atmospheric mean free path [m]
      REAL         AMU        ! atmospheric dynamic viscosity [kg/m s]

      REAL( 8 ) :: CGR( N_MODE-1 ) ! Aitken & Accum. modes

      REAL( 8 ) :: LAMDA      ! mean free path [ m ]
      REAL( 8 ) :: KNC        ! KNC = TWO3 * BOLTZMANN *  AIRTEMP / AMU

C *** Free Molecular regime (depends upon modal density)
      REAL( 8 ) :: KFMAT      ! = SQRT( 3.0*BOLTZMANN * AIRTEMP / PDENSAT )
      REAL( 8 ) :: KFMAC      ! = SQRT( 3.0*BOLTZMANN * AIRTEMP / PDENSAC )
      REAL( 8 ) :: KFMATAC    ! = SQRT( 6.0*BOLTZMANN * AIRTEMP / ( PDENSAT + PDENSAC ) )

C *** Intermodal coagulation rates [ m**3/s ] ( 0th & 2nd moments )
      REAL( 8 ) :: BATAC( 2 ) ! Aitken to accumulation
      REAL( 8 ) :: BACAT( 2 ) ! accumulation from Aitken

C *** Intramodal coagulation rates [ m**3/s ] ( 0th & 2nd moments )
      REAL( 8 ) :: BATAT( 2 ) ! Aitken mode
      REAL( 8 ) :: BACAC( 2 ) ! accumulation mode

C *** Intermodal coagulation rate [ m**3/s ] ( 3rd moment )
      REAL( 8 ) :: C3IJ       ! Aitken to accumulation
      REAL( 8 ) :: C30ATAC    ! Aitken to accumulation
      REAL( 8 ) :: DG_D ( N_MODE )
      REAL( 8 ) :: SG_D ( N_MODE )
      REAL( 8 ) :: XXL_D( N_MODE )

! *** Variables for computing the budget
      REAL( 8 ) :: CBUDGET0_NUM ( N_MODE )
      REAL( 8 ) :: CBUDGET0_SRF ( N_MODE )
      REAL( 8 ) :: CBUDGET0_MASS( N_AEROSPC,N_MODE )

C *** variables for advancing concentrations one time step
      REAL( 8 ) :: A, B
      REAL( 8 ) :: Y0, Y
      REAL( 8 ) :: EXPDT
      REAL( 8 ) :: LOSS, PROD, POL
      REAL         TMASS
      REAL         FACTRANS ! special factor to compute mass transfer
      REAL         M30(3), M20(3), M00(3)      ! for initial condidtions in time stepping

C *** Variables for mode merging
      REAL         GETAF
      REAL         AAA, XNUM, XM2, XM3,XXM2, XXM3
      REAL         FNUM, FM2, FM3, PHNUM, PHM2, PHM3
      REAL         ERF, ERFC    ! Error and complementary error function

C *** local variables
      INTEGER      SPC        ! loop counter
      INTEGER      N          ! loop counter

      logical, save :: firstime = .true.
C-----------------------------------------------------------------------

      if ( firstime ) then
         firstime = .false.
#ifdef nomm
         write( logdev,* ) 'aero_subs: nomm'
#endif
      end if

C *** square root of the ambient temperature for later use
      SQRT_TEMP = SQRT( AIRTEMP )
 
C *** Calculate mean free path [ m ]:
C     6.6328E-8 is the sea level value given in Table I.2.8
C     on page 10 of U.S. Standard Atmosphere 1962

      XLM = 6.6328E-8 * STDATMPA * AIRTEMP  / ( T0 * AIRPRES )

C *** Calculate dynamic viscosity [ kg m**-1 s**-1 ]:
C     U.S. Standard Atmosphere 1962 page 14 expression
C     for dynamic viscosity is:
C     dynamic viscosity =  beta * T * sqrt(T) / ( T + S)
C     where beta = 1.458e-6 [ kg sec^-1 K**-0.5 ], s = 110.4 [ K ].
      AMU = 1.458E-6 * AIRTEMP * SQRT_TEMP / ( AIRTEMP + 110.4 )

C *** Set minimums for coarse mode
      MOMENT0_CONC( N_MODE ) = MAX( AEROMODE_MINNUM( N_MODE ),
     &                              MOMENT0_CONC( N_MODE ) )

C *** Secondary Organics
C     Update the secondary organic aerosol (SOA) mass concentrations
C     and the SVOC mass concentrations by equilibrium absorptive
C     partitioning between the particle and vapor phases.  Assume all
C     SOA resides in the accumulation mode.
C
C     Aerosol is wet when it enters orgaer
      CALL ORGAER( DT, LAYER )

C *** Secondary Inorganics
C     The VOLINORG subroutine includes the treatment of new particle
C     production and a fully dynamic treatment of inorganic gas-to-
C     particle mass transfer.

C *** Compute H2SO4 diffusivity, correct for temperature and pressure
      DIFFCORR = ( STDATMPA / AIRPRES ) * ( ONE_OVER_TICE * AIRTEMP ) ** 1.75
      DV_SO4 = DIFFSULF * DIFFCORR

C *** Update size parameters (distribution is wet from ORGAER)
      CALL GETPAR( FIXED_sg )

! *** Mass transfer of inorganic constituents      
! *** Process Budget is calculated inside this routine because NPF and
! *** condensation are calculated simultaneously
      CALL VOLINORG( DT, COL, ROW, LAYER, DV_SO4, CGR )

! *** Coagulation
!     Calculate coagulation coefficients using a method dictated by
!     the value of FASTCOAG_FLAG.  If TRUE, the computationally-
!     efficient GETCOAGS routine is used.  If FALSE, the more intensive
!     Gauss-Hermite numerical quadrature method is used.  See Section
!     2.1 of Bhave et al. (2004) for further discussion.
! *** Initialize Budget Variables
      CBUDGET0_NUM  = MOMENT0_CONC
      CBUDGET0_SRF  = MOMENT2_CONC * PI
      CBUDGET0_MASS = AEROSPC_CONC
 
! *** set atmospheric mean free path in double precision
      LAMDA    = XLM

! *** calculate term used in Equation A6 of Binkowski & Shankar (1995)
      KNC      = TWOTHIRDS * BOLTZMANN *  AIRTEMP / AMU

! *** calculate terms used in Equation A5 of Binkowski & Shankar (1995)
      KFMAT    = SQRT( 3.0 * BOLTZMANN * AIRTEMP / AEROMODE_DENS( 1 ) )
      KFMAC    = SQRT( 3.0 * BOLTZMANN * AIRTEMP / AEROMODE_DENS( 2 ) )
      KFMATAC  = SQRT( 6.0 * BOLTZMANN * AIRTEMP
     &         / ( AEROMODE_DENS( 1 ) + AEROMODE_DENS( 2 ) ) )

! *** transfer of number to accumulation mode from Aitken mode is zero
      BACAT( 1 ) = 0.0

      IF ( FASTCOAG_FLAG ) THEN ! Solve coagulation analytically

C *** set geometric mean diameters, geometric standard deviations, and
C     ln(GSD) in double precision
         DO N = 1, N_MODE
            DG_D( N ) = AEROMODE_DIAM( N )
            SG_D( N ) = EXP( AEROMODE_LNSG( N ) )
            XXL_D( N ) = AEROMODE_LNSG( N )
         END DO

C *** calculate intermodal and intramodal coagulation coefficients
C     for zeroth and second moments, and intermodal coagulation
C     coefficient for third moment
         CALL GETCOAGS( LAMDA, KFMATAC, KFMAT, KFMAC, KNC,
     &                  DG_D(1), DG_D(2), SG_D(1), SG_D(2),
     &                  XXL_D(1),XXL_D(2),
     &                  BATAT( 2 ), BATAT( 1 ), BACAC( 2 ), BACAC( 1 ),
     &                  BATAC( 2 ), BACAT( 2 ), BATAC( 1 ), C3IJ )

      ELSE                 ! Use Gauss-Hermite numerical quadrature

C *** calculate Aitken-mode intramodal coagulation coefficients
C     for zeroth and second moments
         CALL INTRACOAG_GH( LAMDA, KFMAT, KNC, AEROMODE_DIAM( 1 ),
     &                      AEROMODE_LNSG( 1 ), BATAT( 2 ), BATAT( 1 ) )

C *** calculate accumulation-mode intramodal coagulation coefficients
C     for zeroth and second moments
         CALL INTRACOAG_GH( LAMDA, KFMAC, KNC, AEROMODE_DIAM( 2 ),
     &                      AEROMODE_LNSG( 2 ), BACAC( 2 ), BACAC( 1 ) )

C *** calculate intermodal coagulation coefficients for zeroth, second,
C     and third moments
         CALL INTERCOAG_GH( LAMDA, KFMATAC, KNC,
     &                      AEROMODE_DIAM( 1 ), AEROMODE_DIAM( 2 ),
     &                      AEROMODE_LNSG( 1 ), AEROMODE_LNSG( 2 ),
     &                      BATAC( 2 ), BACAT( 2 ), BATAC( 1 ), C3IJ )

      END IF   ! FASTCOAG_FLAG

C *** calculate 3rd moment intermodal transfer rate by coagulation
      C30ATAC = C3IJ * MOMENT0_CONC( 1 ) * MOMENT0_CONC( 2 )

C *** TAKE ONE FORWARD TIME STEP - Solve Modal Dynamics Equations
C     This code implements Section 1.4 of Binkowski and Roselle (2003)
C     with two notable exceptions.  1) emissions are treated in
C     CMAQ`s vertical diffusion routine, so they do not appear in the
C     following equations. 2) new particle formation and condensational
C     growth are now treated in the VOLINORG subroutine, so they do not
C     appear in the following equations.
 
      ! Initial Moment Values
      M00 = MOMENT0_CONC
      M20 = MOMENT2_CONC
      M30 = MOMENT3_CONC

! *** Update Zeroth Moment (i.e. number concentration)
! *** Aitken mode: initial value of M0
      Y0 = MOMENT0_CONC( 1 )

      ! The rate of change for M0_Aitken is described in Equation 8a of
      ! Binkowski & Roselle (2003), with the c_i term equal to 0.
      A = BATAT( 1 )                      ! intramodal coagulation
      B = BATAC( 1 ) * moment0_conc( 2 )  ! intermodal coagulation

      EXPDT = EXP( - B * DT )
      IF ( EXPDT .LT. 1.0D0 ) THEN
         Y = B * Y0 * EXPDT / ( B + A * Y0 * ( 1.0D0 - EXPDT ) )
      ELSE
         Y = Y0                 ! solution in the limit that B approaches zero
      END IF

      ! Transfer new value of M0_Aitken to the moment array
      MOMENT0_CONC( 1 ) = MAX( AEROMODE_MINNUM( 1 ), REAL( Y ) )

      ! Accumulation mode: initial value of M0
      Y0 = MOMENT0_CONC( 2 )

      ! The rate of change for M0_accum is described in Equation 8b of
      ! Binkowski & Roselle (2003), except the coefficient C is zero
      ! because emissions are treated outside the CMAQ aerosol module.
      ! The equation reduces to the form: dY/dt = -A * Y**2 , where
      A = BACAC( 1 )                 ! intramodal coagulation

      ! Solve for M0_accum using Smoluchowski`s solution
      Y = Y0 / ( 1.0D0 + A * Y0 * DT )

      ! Transfer new value of M0_accum to the moment array
      MOMENT0_CONC( 2 ) = MAX( AEROMODE_MINNUM( 2 ), REAL( Y ) )

      ! ** Update Budget Variable for Zeroth Moment
      COAG_BUDGET( AERONUM_MAP( 1 ),1 ) = MOMENT0_CONC( 1 ) - CBUDGET0_NUM( 1 )
      COAG_BUDGET( AERONUM_MAP( 2 ),2 ) = MOMENT0_CONC( 2 ) - CBUDGET0_NUM( 2 )

! *** end of update for zeroth moment - note that the coarse mode number does
! *** not change because coarse-mode coagulation is neglected in the model
 

C *** UPDATE MASS CONCENTRATIONS (for each species)
C     The following procedure is described in Paragraphs 21-23
C     of Binkowski & Roselle (2003), except the Ei,n and Ej,n terms
C     are excluded here because emissions are treated outside the
C     CMAQ aerosol module.

C     Aitken mode mass concentration rates of change are of the form:
C        dc/dt = P - L*c    ! Equation 9a of Binkowski & Roselle (2003)
C     with solution
C        c(t0 + dt) = P/L + ( c(t0) - P/L ) * exp(-L*dt)

C     For all species, loss of Aitken mode mass is due to intermodal
C     coagulation.
C        LOSSn = PI/6 * RHOn * C30ATAC / MASSn
C        RHOn  = MASSn / (M3 * PI/6)
C     When above equations are combined, the PI/6 terms cancel yielding
C        LOSSn = C30ATAC / M3
C     where LOSSn is the loss rate of species n, RHOn is the mass of
C     species n per unit of particle volume, C30ATAC is the 3rd moment
C     loss rate due to intermodal coagulation, MASSn is the mass
C     concentration of species n, and M3 is the 3rd moment
C     concentration.

      LOSS = C30ATAC / MOMENT3_CONC( 1 )

      ! Set up extra variables to solve for Aitken mode mass concentrations
      FACTRANS = REAL( LOSS,4 ) * DT
      EXPDT = EXP( -FACTRANS )

      ! Transfer mass from Aitken to accumulation mode, resulting from 
      ! intermodal coagulation.

      DO SPC = 1, N_AEROSPC
         IF ( AERO_MISSING(SPC,1) ) CYCLE
 
         TMASS = AEROSPC_CONC( SPC,1 ) + AEROSPC_CONC( SPC,2 )
         IF( SPC .EQ. APHGJ_IDX  )THEN 
             ! assumes all production adsorb onto accumulation mode
             TMASS = TMASS + PHG_RATE * DT
         END IF

         AEROSPC_CONC( SPC,1 ) = MAX( AEROSPC( SPC )%MIN_CONC( 1 ),
     &                                REAL( AEROSPC_CONC( SPC,1 ) * EXPDT ) )
         AEROSPC_CONC( SPC,2 ) = MAX( AEROSPC( SPC )%MIN_CONC( 2 ),
     &                                TMASS - AEROSPC_CONC( SPC,1 ) )


         ! Update Budget Variable for Speciated Mass
         IF ( AEROSPC_MAP( SPC,1 ) .NE. 0 ) 
     &      COAG_BUDGET( AEROSPC_MAP( SPC,1 ),1 ) = 
     &           AEROSPC_CONC( SPC,1 ) - CBUDGET0_MASS( SPC,1 )
         IF ( AEROSPC_MAP( SPC,2 ) .NE. 0 ) 
     &      COAG_BUDGET( AEROSPC_MAP( SPC,2 ),1 ) = 
     &           AEROSPC_CONC( SPC,2 ) - CBUDGET0_MASS( SPC,2 )

      END DO

C *** end of update for species mass concentrations
 
! *** UPDATE SECOND MOMENT
      IF ( COAG_MOMENTS .EQ. 3 ) THEN
        ! For each lognormal mode, solve equations of form:
        !    dM2/dt = P2 - L2*M2   ! if L2 > 0
        ! with solution
        !    M2(t) = P2/L2 + ( M2(t0) - P2/L2 ) * exp( -L2*dt )
        ! or
        !    dM2/dt = P2           ! if L2 = 0
        ! with solution
        !    M2(t) = M2(t0) + P2*dt

        ! Loss of 2nd moment from Aitken mode is due to intermodal
        ! coagulation with accumulation mode and intramodal coagulation.
        ! Production term is removed, because new particle formation
        ! and condensational growth are accounted for in VOLINORG.
        LOSS = (
     &          ( BATAT( 2 ) * M00( 1 )
     &          + BATAC( 2 ) * M00( 2 ) ) * M00( 1 )
     &         ) / M20( 1 )

        ! Solve for M2_Aitken based on LOSS during this time step
        ! Note: LOSS is assumed to be non-negative.
        IF ( LOSS .GT. 0.0 ) THEN
           Y = M20( 1 ) * EXP( -LOSS * DT )
        ELSE
           Y = M20( 1 )
        END IF ! test on loss

        ! Transfer new value of M2_Aitken to the array
        MOMENT2_CONC( 1 ) = MAX( REAL( AEROMODE_MINM2( 1 ) ), REAL( Y ) )

        ! Production of 2nd moment in accumulation mode is due to
        ! intermodal coagulation Aitken mode
        PROD = BACAT( 2 ) * M00( 1 ) * M00( 2 )

        ! Loss of 2nd moment from accumulation mode is due only to
        ! intramodal coagulation
        LOSS = ( BACAC( 2 ) * M00( 2 ) * M00( 2 ) ) / M20( 2 )

        ! Solve for M2_accum based on PROD and LOSS during this time step
        ! Note: LOSS is assumed to be non-negative.
        IF ( LOSS .GT. 0.0 ) THEN
           POL = PROD / LOSS
           Y = POL + ( M20(2) - POL ) * EXP( -LOSS * DT )
        ELSE
           Y = M20(2) + PROD * DT
        END IF ! test on loss
     
        ! Transfer new value of M2_accum to moment array
        MOMENT2_CONC( 2 ) = MAX( REAL( AEROMODE_MINM2( 2 ) ), REAL( Y ) )

        ! Coarse mode: no change because coagulation of coarse particles
        ! is neglected in current model version.

      ELSE IF ( COAG_MOMENTS .EQ. 2 ) THEN
        ! Assume constant modal standard deviation and solve for new
        ! second moment as a function of the change in the third moment.
        MOMENT2_CONC( 1 ) = M20( 1 )*( MOMENT3_CONC( 1 ) / M30( 1 ) ) ** ( 2.0 / 3.0 )
        MOMENT2_CONC( 2 ) = M20( 2 )*( MOMENT3_CONC( 2 ) / M30( 2 ) ) ** ( 2.0 / 3.0 )

      END IF

      ! Update Budget Variable for Second Moment
      COAG_BUDGET( AEROSRF_MAP( 1 ),1 ) = MOMENT2_CONC( 1 ) * PI - CBUDGET0_SRF( 1 )
      COAG_BUDGET( AEROSRF_MAP( 2 ),2 ) = MOMENT2_CONC( 2 ) * PI - CBUDGET0_SRF( 2 )

C *** end of update for second moment
 

C *** Mode Merging
C     This code implements Section 1.5 of Binkowski and Roselle (2003).
C     If the Aitken mode mass is growing faster than accumulation mode
C     mass and the Aitken mode number concentration exceeds the
C     accumulation mode number concentration, then modes are merged by
C     renaming.

! *** Initialize Budget Variables
      CBUDGET0_NUM  = MOMENT0_CONC
      CBUDGET0_SRF  = MOMENT2_CONC * PI
      CBUDGET0_MASS = AEROSPC_CONC

#ifdef nomm
      if ( .false. ) then
#else
      IF ( CGR( 1 ) .GT. CGR( 2 ) .AND.
     &     MOMENT0_CONC( 1 ) .GT. MOMENT0_CONC( 2 ) ) THEN
#endif

C *** Before mode merging, update the third moments, geometric mean
C     diameters, geometric standard deviations, modal mass totals, and
C     particle densities, based on the new concentrations of M2, M0, and
C     speciated masses calculated above. This is still the wet
C     distribution.
         CALL GETPAR( FIXED_sg )

C *** Calculate AAA = ln( Dij / DGATK ) / ( SQRT2 * XXLSGAT ), where Dij
C     is the diameter at which the Aitken-mode and accumulation-mode
C     number distributions intersect (i.e., overlap).  AAA is equivalent
C     to the "Xnum" term described below Equation 10a by Binkowski and
C     Roselle (2003).
         AAA = GETAF( MOMENT0_CONC( 1 ), MOMENT0_CONC( 2 ),
     &                AEROMODE_DIAM( 1 ), AEROMODE_DIAM( 2 ), 
     &                AEROMODE_LNSG( 1 ), AEROMODE_LNSG( 2 ),
     &                SQRT2 ) 

C *** Ensure that Xnum is large enough so that no more than half of
C     the Aitken mode mass is merged into the accumulation mode during
C     any given time step.  This criterion is described in Paragraph 26
C     of Binkowski and Roselle (2003).
         XXM3 = 3.0 * AEROMODE_LNSG( 1 ) / SQRT2
         XNUM = MAX( AAA, XXM3 )

C *** Factors used in error function calls for M2 and M3 mode merging
         XXM2 = TWOTHIRDS * XXM3
         XM2  = XNUM - XXM2 ! set up for 2nd moment transfer
         XM3  = XNUM - XXM3 ! set up for 3rd moment and mass transfers

C *** Calculate the fractions of the number, 2nd, and 3rd moment
C     distributions with diameter greater than the intersection diameter
         FNUM  = 0.5 * ERFC( XNUM )            ! Eq 10a of B&R 2003
         FM2   = 0.5 * ERFC( XM2 )             ! Eq 10b of B&R 2003
         FM3   = 0.5 * ERFC( XM3 )             ! Eq 10b of B&R 2003

C *** Calculate the fractions of the number, 2nd, and 3rd moment
C     distributions with diameters less than the intersection diameter.
         PHNUM = 0.5 * ( 1.0 + ERF( XNUM ) )  ! Eq 10c of B&R 2003
         PHM2  = 0.5 * ( 1.0 + ERF( XM2 ) )   ! Eq 10d of B&R 2003
         PHM3  = 0.5 * ( 1.0 + ERF( XM3 ) )   ! Eq 10d of B&R 2003

C *** Update accumulation-mode moment concentrations using
C     Equations 11a - 11c of Binkowski and Roselle (2003).
         MOMENT0_CONC( 2 ) = MOMENT0_CONC( 2 ) + MOMENT0_CONC( 1 ) * FNUM
         MOMENT2_CONC( 2 ) = MOMENT2_CONC( 2 ) + MOMENT2_CONC( 1 ) * FM2
         MOMENT3_CONC( 2 ) = MOMENT3_CONC( 2 ) + MOMENT3_CONC( 1 ) * FM3

C *** Update Aitken-mode moment concentrations using
C     Equations 11d - 11f of Binkowski and Roselle (2003).
         MOMENT0_CONC( 1 ) = MOMENT0_CONC( 1 ) * PHNUM
         MOMENT2_CONC( 1 ) = MOMENT2_CONC( 1 ) * PHM2
         MOMENT3_CONC( 1 ) = MOMENT3_CONC( 1 ) * PHM3
 
C *** Rename masses of each species from Aitken mode to acumulation mode
C     using Equation 11b of Binkowski and Roselle (2003). Do this for
C     all species, even the aerosol water.
         DO SPC = 1, N_AEROSPC
            IF ( AERO_MISSING(SPC,1) ) CYCLE

            AEROSPC_CONC( SPC,2 ) = AEROSPC_CONC( SPC,2 ) + AEROSPC_CONC( SPC,1 ) * FM3
            AEROSPC_CONC( SPC,1 ) = AEROSPC_CONC( SPC,1 ) * PHM3
         END DO

      END IF ! end check on necessity for merging

C *** end of update for mode merging

C *** Update the third moments, geometric mean diameters, geometric
C     standard deviations, modal mass totals, and particle densities,
C     based on the final concentrations of M2, M0, and speciated masses
C     after mode merging is complete. This should be done for the wet
C     distribution.
      CALL GETPAR( FIXED_sg )

C *** Set minimum value for all concentrations in the CBLK array

      DO N = 1, N_MODE
         DO SPC = 1, N_AEROSPC
            AEROSPC_CONC( SPC,N ) = MAX( AEROSPC_CONC( SPC,N ),
     &                                   AEROSPC( SPC )%MIN_CONC( N ) )
#ifdef sens
            IF (AEROSPC_CONC( SPC,N ) .EQ. AEROSPC( SPC )%MIN_CONC( N ) )  THEN
               S_AEROSPC_CONC( SPC,N,: ) = 0.0
            ENDIF
#endif
         END DO
      END DO


! *** Propagate Mode Merging and Minimum value impacts to budget variables
      GROWTH_BUDGET( AERONUM_MAP(:) ) = MOMENT0_CONC(:) - CBUDGET0_NUM(:)
      GROWTH_BUDGET( AEROSRF_MAP(:) ) = MOMENT2_CONC(:) * PI - CBUDGET0_SRF(:)
      DO SPC = 1,N_AEROSPC
         WHERE( AEROSPC_MAP( SPC,: ) .NE. 0 )
     &          GROWTH_BUDGET( AEROSPC_MAP(SPC,:) )=
     &              AEROSPC_CONC(SPC,:) - CBUDGET0_MASS(SPC,:)
      END DO

      RETURN
      END SUBROUTINE AEROPROC


C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE VOLINORG( DT, COL, ROW, LAYER, DV_SO4, CGR )

C *** Calculates the partitioning of inorganic components (CL,NO3,NH4,SO4)
C     between the aerosol and gas phase over the operator synchronization
C     timestep (DT). Partitioning is calculated using the Hybrid approach,
C     where dynamic mass transfer of species to/from the coarse mode is
C     calculated using multiple sub-operator time steps (TSTEP) and the
C     fine modes are equilibrated with the gas phase. The mass transfer
C     calculations are made using the H+ flux-limiting approach of Pilinis
C     et al. (2000). If 'OPTIONFLAG' is not set to 'Hybrid', the mass
C     transfer calculations for the coarse mode are skipped, and the fine
C     modes are equilibrated with the gas phase.

C     Returns updated volatile inorganic species concentrations in the gas
C     and particulate phase, and the aerosol modal parameters

C *** Revision history: 4/07 - Moved HCOND3 and NEWPART3 calls from 
C                              AEROPROC to this subroutine for 
C                              mass transfer calculation  
C     15 Jul 08 J.Young, P.Bhave: increased cutoff to hybrid from .01 to .05 ug/m**3
C               J.Young: change 'OPTIONFLAG' to just a logical variable, 'Hybrid'

C     10 Mar 11 S.Howard Renamed met_data to aeromet_data
C     25 Mar 11 S.Roselle Replaced I/O API include files with UTILIO_DEFN
C     26 Apr 11 G.Sarwar Replaced existing ISORROPIA with ISORROPIA 2.1
C               Updated coarse-mode aerosol speciation and H+ calculation  
C     12 Apr 16 H. Pye and B. Murphy: Update with consistent treatment for wet particles.
C     15 Apr 16 J.Young: Use aerosol factors from AERO_DATA module named constants

C *** References
C 1. Pilinis C, Capaldo KP, Nenes A, Pandis SN (2000) MADM - A new
C    multicomponent aerosol dynamics model. AEROSOL SCIENCE AND TECHNOLOGY.
C    32(5):482-502
C
C 2. Capaldo KP, Pilinis C, Pandis SN (2000) A computationally efficient hybrid
C    approach for dynamic gas/aerosol transfer in air quality models. ATMOSPHERIC
C    ENVIRONMENT. 34(21):3617-3627
C
C 3. Fountoukis C, Nenes, A (2007) ISORROPIA II: a comnputationally efficient 
C    thermodynamic equilibrium model for K+-Ca2+-Mg2+-NH4+-SO42--NO3--Cl-H2O 
C    aerosols. ATMOSPHERIC CHEMISTRY AND PHYSICS. 7, 4639-4659

      USE AERO_DATA
      USE PRECURSOR_DATA
      USE SOA_DEFN
      USE AEROMET_DATA
      USE UTILIO_DEFN

#ifdef sens
      USE DDM3D_DEFN, ONLY : NP, NPMAX
      USE AERO_DDM3D, ONLY : S_SO4RATE, S_PRECURSOR_CONC
#endif

      IMPLICIT NONE

C *** Arguments:
      REAL    DT              ! time step [sec]
      INTEGER COL             ! grid column index
      INTEGER ROW             ! grid row index
      INTEGER LAYER           ! model layer index
      REAL    DV_SO4          ! molecular diffusivity of H2SO4 vapor 
                              ! after correction for ambient conditions
      REAL( 8 ) :: CGR( N_MODE-1 ) ! 3rd moment SO4 growth rate [m^3/m^3-s]

C *** Parameters: 
      INTEGER, PARAMETER :: NINORG = 9      ! number of inorganic species
      INTEGER, PARAMETER :: NVOLINORG = 3   ! number of volatile inorganic species

      ! indices for inorganic species
      INTEGER, PARAMETER :: KNH4 = 1, KNO3 = 2, KCL = 3, KSO4 = 4, KNA = 5, KMG = 6, KK = 7, KCA = 8, KHP = 9

      REAL( 8 ), PARAMETER :: D_TWOTHIRDS = 2.0D0 / 3.0D0

      REAL, PARAMETER :: CUTOFF = 0.05  ! [ug/m**3]
      REAL, PARAMETER :: ALPHSULF = 0.1 ! Accommodation coefficient for sulfuric acid
                                        ! see Capaldo et al. (2000)

C *** Local Variables:

C *** Inputs to subroutine HCOND3

      REAL, SAVE :: COFCBAR_SO4  ! Temperature-independent coefficients
                                 ! for caculating molecular vel [m/s]
                                 ! = sqrt((8*Rgas)/(pi*MW)) 
      REAL         CBAR_SO4      ! molecular velocity of H2SO4                      

      REAL( 8 ) :: AM0( N_MODE ) ! zeroth moments
      REAL( 8 ) :: AM1( N_MODE ) ! first moments
      REAL( 8 ) :: AM2( N_MODE ) ! second moments

      REAL( 8 ) :: M2DRY_INIT( N_MODE )  ! Dry Second Moment Initial Array
      REAL( 8 ) :: M3DRY_INIT( N_MODE )  ! Dry Third Moment Initial Array
      REAL( 8 ) :: M2WET_INIT( N_MODE )  ! Wet Second Moment Initial Array
      REAL( 8 ) :: M3WET_INIT( N_MODE )  ! Wet Third Moment Initial Array
      REAL( 8 ) :: M2WET_FINAL( N_MODE ) ! Wet Second Moment Final Array
      REAL( 8 ) :: M3WET_FINAL( N_MODE ) ! Wet Third Moment Final Array

C *** Outputs from HCOND3: size-dependent term in the condensational-growth 
C     expressions defined in Equations A13-A14 of [Binkowski & Shankar,1995]
      REAL( 8 ) :: FCONC_SO4( N_MODE,2 )  ! All sizes 2nd and 3rd moments
      REAL( 8 ) :: FCONC_OUT( 2 )         ! Output for HCOND3, 2nd and 3rd moments
      REAL( 8 ) :: FCONCM1_SO4       ! reciprocals of total SO4 cond rates

C *** Modal partition factors [ dimensionless ]
C     defined in Equations A17-A18 of [Binkowski & Shankar,1995]
      REAL( 8 ) :: OMEGA_AT_SO4  ! Aitken mode 2nd and 3rd moments
      REAL( 8 ) :: OMEGA_AC_SO4  ! Accumulation mode 2nd and 3rd moments
      REAL( 8 ) :: OMEGA( 2 )    ! partitioning coefficient for equilibrium PM mass
      REAL( 8 ) :: PHI( NINORG,2 ) ! Mass Fraction of each component in each aerosol mode
      REAL( 8 ) :: TOTAER( NINORG )! Total aerosol component across multiple modes 

C *** Variables for new particle formation:
      REAL XH2SO4            ! steady state H2SO4 concentration
      REAL( 8 ) :: DMDT_SO4  ! particle mass production rate [ ug/m**3 s ]
      REAL( 8 ) :: DNDT      ! particle number production rate [ # / m**3 s ]
      REAL( 8 ) :: DM2DT     ! second moment production rate [ m**2 / m**3 s]
      REAL( 8 ) :: SCONDRATE ! SO4 condensation rate [ ug/m**3 s ]

C *** Mode-specific sulfate production rate [ ug/m**3 s ]
      REAL( 8 ) :: CONDSO4( N_MODE )    ! sulfate condensation rate [ ug/m**3 s ]
      REAL( 8 ) :: RATE                 ! CONDSO4 or cond+nucl rate

C *** Size-dependent portion of mass-transfer rate equation
      REAL( 8 ) :: GRFAC1( N_MODE )     ! 2nd moment [ m**2/m**3-s ] 
      REAL( 8 ) :: GRFAC2( N_MODE )     ! 3rd moment [ m**3/m**3-s ] 
      
C *** ISORROPIA input variables
      REAL( 8 ) :: WI( NINORG - 1 )     ! species array [ mol/m**3 ]
      REAL( 8 ) :: RHI                  ! relative humidity [ fraction ]
      REAL( 8 ) :: TEMPI                ! temperature   [ deg K]
      REAL( 8 ) :: CNTRL( 2 )           ! ISOROPIA control parameters 

C *** ISORROPIA output variables
      REAL( 8 ) :: WT( NINORG - 1 )     ! species output array (unused)
      REAL( 8 ) :: GAS( 3 )             ! gas-phase   "     " 
      REAL( 8 ) :: AERLIQ( 15 )         ! liq aerosol "     " 
      REAL( 8 ) :: AERSLD( 19 )         ! solid "     "     "  (unused)
      REAL( 8 ) :: OTHER( 9 )           ! supplmentary output array (unused)
      CHARACTER( 15 ) :: SCASI          ! subcase number output (unused)

C *** Variables to account for mass conservation violations in ISRP3F
!     LOGICAL TRUSTNH4                  ! false if ISOROPIA's partitioning
                                        !  of NH4/NH3 is to be ignored
      LOGICAL TRUSTCL                   ! false if ISOROPIA's partitioning       
                                        !  of Cl/HCl is to be ignored

C *** Initial (double-precision) concentrations [ug/m3]
      REAL( 8 ) :: GNH3R8               ! gas-phase ammonia
      REAL( 8 ) :: GNO3R8               ! gas-phase nitric acid
      REAL( 8 ) :: GCLR8                ! gas-phase hydrochloric acid

C *** Variables for volatile species mass transfer between gas and aerosol and
C     mass partitioning between the modes 
      LOGICAL HYBRID ! mass transfer option flag (mass transfer if .TRUE.)
      REAL( 8 ) :: DELT                 ! time step DT [s]
      REAL( 8 ) :: HPLUS( N_MODE )      ! scratch var for H+ [umol/m**3]

      REAL( 8 ), SAVE :: H2SO4RATM1     ! Mol. wt. ratio of SO4/H2SO4

      REAL( 8 ) :: DVOLINORG( NVOLINORG ) ! vol inorg spcs mass to be xferred [mol/m3]
      REAL( 8 ) :: DVOLMAX                ! max value for DVOLINORG 
      REAL( 8 ) :: CINORG( NINORG,N_MODE ) ! scratch array for inorg spcs [ug/m**3]
      REAL( 8 ) :: SEACAT                 ! coarse sea-salt cations [ug/m**3]
      REAL( 8 ) :: SOILwVOLS              ! windblown dust before removal of
                                          ! SO4,NO3,CL,H2O [ug/m**3]
      REAL( 8 ) :: PMCwVOLS               ! anthrop coarse material before removal
                                          ! of SO4,NO3,CL,H2O [ug/m**3]
      REAL( 8 ) :: INT_TIME               ! internal mass transfer time (s)
      REAL( 8 ) :: TSTEP                  ! mass transfer time step [s]
      REAL( 8 ) :: DRYM20, Y              ! scratch vars for 2nd moment [m**2/m**3]
      REAL( 8 ) :: M3OTHR                 ! vars for 3rd moment calculation [m**3/m**3]
      REAL( 8 ), SAVE :: DF( NVOLINORG )  ! scratch array for mole -> ug conversion
      REAL( 8 ), SAVE :: DFH2OR8          ! mole -> ug conversion for H2O
      REAL( 8 ) :: J( NVOLINORG )         ! condensation/evaporation flux [mol/m**3-s]
      REAL( 8 ) :: CFINAL( NVOLINORG,N_MODE ) ! conc after mass xfer step [ug/m**3]
      REAL( 8 ) :: H2O                    ! Scratch LWC variable for output
      REAL( 8 ) :: H2O_NEW                ! Update of LWC after condensation 
      REAL( 8 ) :: SO4                    ! modal SO4 after condensation or cond + nucl
      REAL( 8 ) :: DDRYM3DT               ! rate of 3rd moment transfer - dry inorg spcs
      REAL( 8 ) :: DDRYM2DT               ! rate of 2nd moment transfer -  "     "    "
      REAL( 8 ) :: DRYM3, WETM3           ! scratch vars for 3rd moment calc [m**3/m**3]
      REAL( 8 ) :: DRYM2, WETM2           ! scratch vars for 2nd moment calc [m**2/m**3]
      REAL( 8 ) :: LOSS                   ! rate of loss of second moment [1/s] 
      REAL( 8 ) :: EQLBHIJ                ! H+ concentration from isoropia for I plus J-modes [ug/m**3]

      REAL( 8 ) :: DELNUM( N_MODE ) ! Change in Number due to some process
      REAL( 8 ) :: DELSRF( N_MODE ) ! Change in Surface Area due to some process
      REAL( 8 ) :: DELSO4( N_MODE ) ! Change in Sulfate due to some process
      REAL( 8 ) :: DELNO3( N_MODE ) ! Change in Nitrate due to some process
      REAL( 8 ) :: DELNH4( N_MODE ) ! Change in Ammonium due to some process
      REAL( 8 ) :: DELCL( N_MODE )  ! Change in Chloride due to some process
      REAL( 8 ) :: DELH2O( N_MODE ) ! Change in Water due to some process
      REAL( 8 ) :: DELH3OP( N_MODE )! Change in Proton due to some process
      REAL( 8 ) :: CBUDGET0_NUM( N_MODE ) ! Initial Number before some process
      REAL( 8 ) :: CBUDGET0_SRF( N_MODE ) ! Initial Surface Area before some process
      REAL( 8 ) :: CBUDGET0_SO4( N_MODE ) ! Initial Sulfate before some process
      REAL( 8 ) :: CBUDGET0_NO3( N_MODE ) ! Initial Nitrate before some process
      REAL( 8 ) :: CBUDGET0_NH4( N_MODE ) ! Initial Ammonium before some process
      REAL( 8 ) :: CBUDGET0_CL( N_MODE )  ! Initial Chloride before some process
      REAL( 8 ) :: CBUDGET0_H2O( N_MODE ) ! Initial Particle Water before some process
      REAL( 8 ) :: CBUDGET0_H3OP( N_MODE )! Initial Proton before some process

      REAL( 8 ) :: TMP
      INTEGER I, N                        ! loop and array indices
      INTEGER IMODE                       ! mode loop index  
      INTEGER ISTEP                       ! loop index, mass transfer time step loop
      INTEGER ISP                         ! loop index, species loop
      LOGICAL TrustIso                    ! For negative vap. press., TrustIso = F

C *** Local Saved Variables 
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      LOGICAL, SAVE :: FIRSTWRITE = .TRUE.
      REAL( 8 ), SAVE :: SO4FAC         !  F6DPIM9 / RHOSO4
      REAL( 8 ), SAVE :: SOILFAC        !  F6DPIM9 / RHOSOIL
      REAL( 8 ), SAVE :: ANTHFAC        !  F6DPIM9 / RHOANTH
      REAL( 8 ), SAVE :: H2SO4RAT       ! Mol. wt ratios H2SO4/SO4
      REAL( 8 ), SAVE :: NH3RAT         ! Mol. wt ratios NH3/NH4
      REAL( 8 ), SAVE :: HNO3RAT        ! Mol. wt ratios HNO3/NO3
      REAL( 8 ), SAVE :: HCLRAT         ! Mol. wt ratios HCL/CL 
      REAL( 8 ), SAVE :: MWH2SO4        ! molecular weight for H2SO4
      REAL( 8 ), SAVE :: FAERH2SO4      ! 1e-6 / mw(h2so4)

      REAL( 8 ), SAVE :: CFAC_ANA
      REAL( 8 ), SAVE :: CFAC_ASO4
      REAL( 8 ), SAVE :: CFAC_ANH4
      REAL( 8 ), SAVE :: CFAC_ANO3
      REAL( 8 ), SAVE :: CFAC_ACL
      REAL( 8 ), SAVE :: CFAC_ACA
      REAL( 8 ), SAVE :: CFAC_AK
      REAL( 8 ), SAVE :: CFAC_AMG
      REAL( 8 ), SAVE :: CFAC_ASEACAT
      REAL( 8 ), SAVE :: CFAC_GNH3
      REAL( 8 ), SAVE :: CFAC_GHNO3
      REAL( 8 ), SAVE :: CFAC_GHCL

      REAL( 8 ), SAVE :: M3FAC_ANA
      REAL( 8 ), SAVE :: M3FAC_ASO4
      REAL( 8 ), SAVE :: M3FAC_ANH4
      REAL( 8 ), SAVE :: M3FAC_ANO3
      REAL( 8 ), SAVE :: M3FAC_ACL
      REAL( 8 ), SAVE :: M3FAC_H2O


      logical, save :: write1 = .true.

#ifdef sens
      INTERFACE
         SUBROUTINE AERO_SENS( SCASI, W )
           CHARACTER( 15 ), INTENT( IN ) :: SCASI    ! (INPUT) subcase number output
           REAL(KIND=8),    INTENT( IN ) :: W( : )   ! (INPUT) WI concentrations 
         END SUBROUTINE AERO_SENS
      END INTERFACE 
#endif

C-----------------------------------------------------------------------
 
      IF ( FIRSTIME ) THEN
      
         FIRSTIME = .FALSE.
#ifdef noiso
         write( logdev,* ) 'aero_subs, volinorg: noiso'
#endif

         CFAC_ANA  = 1.0D-6 / REAL( AEROSPC_MW( ANA_IDX ), 8 )
         CFAC_ASO4 = 1.0D-6 / REAL( AEROSPC_MW( ASO4_IDX ), 8 )
         CFAC_ANH4 = 1.0D-6 / REAL( AEROSPC_MW( ANH4_IDX ), 8 )
         CFAC_ANO3 = 1.0D-6 / REAL( AEROSPC_MW( ANO3_IDX ), 8 )
         CFAC_ACL  = 1.0D-6 / REAL( AEROSPC_MW( ACL_IDX ), 8 )
         CFAC_ACA  = 1.0D-6 / REAL( AEROSPC_MW( ACA_IDX ), 8 )
         CFAC_AK   = 1.0D-6 / REAL( AEROSPC_MW( AK_IDX ), 8 )
         CFAC_AMG  = 1.0D-6 / REAL( AEROSPC_MW( AMG_IDX ), 8 )
         CFAC_ASEACAT = 1.0D-6 / REAL( AEROSPC_MW( ASEACAT_IDX ), 8 )
         CFAC_GNH3 = 1.0D-6 / PRECURSOR_MW( NH3_IDX )
         CFAC_GHNO3= 1.0D-6 / PRECURSOR_MW( HNO3_IDX )
         CFAC_GHCL = 1.0D-6 / PRECURSOR_MW( HCL_IDX )

         M3FAC_ANA  = 1.0D-9 * F6DPI / REAL( AEROSPC( ANA_IDX )%DENSITY, 8 )
         M3FAC_ASO4 = 1.0D-9 * F6DPI / REAL( AEROSPC( ASO4_IDX )%DENSITY, 8 )
         M3FAC_ANH4 = 1.0D-9 * F6DPI / REAL( AEROSPC( ANH4_IDX )%DENSITY, 8 )
         M3FAC_ANO3 = 1.0D-9 * F6DPI / REAL( AEROSPC( ANO3_IDX )%DENSITY, 8 )
         M3FAC_ACL  = 1.0D-9 * F6DPI / REAL( AEROSPC( ACL_IDX )%DENSITY, 8 )
         M3FAC_H2O  = 1.0D-9 * F6DPI / REAL( AEROSPC( AH2O_IDX )%DENSITY, 8 )

         MWH2SO4 = PRECURSOR_MW( SULF_IDX ) ! molecular weight for H2SO4
         FAERH2SO4 = 1.0E-6 / MWH2SO4

         COFCBAR_SO4 = SQRT( 8.0 * RGASUNIV / ( PI * REAL( MWH2SO4,4 ) * 1.0E-3 ) )
         H2SO4RATM1 = AEROSPC_MW( ASO4_IDX ) / MWH2SO4
         SO4FAC  = 1.0D-9 * F6DPI / REAL( AEROSPC( ASO4_IDX )%DENSITY, 8 )
         SOILFAC = 1.0D-9 * F6DPI / REAL( AEROSPC( ASOIL_IDX )%DENSITY, 8 )
         ANTHFAC = 1.0D-9 * F6DPI / REAL( AEROSPC( ACORS_IDX )%DENSITY, 8 )

         DF( KNH4 ) = 1.0D0 / CFAC_ANH4
         DF( KNO3 ) = 1.0D0 / CFAC_ANO3
         DF( KCL )  = 1.0D0 / CFAC_ACL
         DFH2OR8    = 1.0D6 * MWWAT      ! aerospc_mw(AH2O_IDX) (18.0 != 18.0153)

         ! Mol. wt ratios H2SO4/SO4, NH3/NH4, HNO3/NO3, HCL/CL
         H2SO4RAT= PRECURSOR_MW( SULF_IDX ) / REAL( AEROSPC_MW( ASO4_IDX ), 8 )
         NH3RAT  = PRECURSOR_MW( NH3_IDX )  / REAL( AEROSPC_MW( ANH4_IDX ), 8 )
         HNO3RAT = PRECURSOR_MW( HNO3_IDX ) / REAL( AEROSPC_MW( ANO3_IDX ), 8 )
         HCLRAT  = PRECURSOR_MW( HCL_IDX )  / REAL( AEROSPC_MW( ACL_IDX ), 8 )
      END IF

C *** Determine if Hybrid
      TMP = 0.0
#ifdef noiso
      hybrid = .false.
#else
      DO I = 1, N_AEROSPC
         IF(  AEROSPC( I )%tracer )CYCLE
         IF ( AEROSPC( I )%CHARGE .NE. 0 ) TMP = TMP + AEROSPC_CONC( I,N_MODE )
      END DO
      HYBRID = ( TMP .GE. CUTOFF ) .AND. ( AIRRH .GE. 0.18 )
#endif

      DELT  = REAL( DT, 8 )
      TEMPI = AIRTEMP             ! assume const within synch timestep
      RHI   = MIN( 0.95, AIRRH )  ! "        "     "      "     "

C *** Calculate molecular velocities (temperature dependent) and
C     H+ concentration

      CBAR_SO4 = COFCBAR_SO4 * SQRT( AIRTEMP )

      HPLUS = 0.0
      DO I = 1, N_MODE
         DO N = 1, N_AEROSPC
            IF(  AEROSPC( N )%tracer )CYCLE
            HPLUS( I ) = HPLUS( I )
     &                 - AEROSPC( N )%CHARGE * AEROSPC_CONC( N,I ) / AEROSPC_MW( N )
         END DO
      END DO

C *** Condensational Growth (Size-dependent terms)
C     Calculate intermediate variables needed to determine the 2nd and
C     3rd moment condensational-growth rates.  3rd moment terms are 
C     needed for the calculation of new particle production.  See 
C     Section 3.3 of Jiang & Roth (2003) for a detailed discussion.
C    
C *** Set moments using Equation 4 of Binkowski & Shankar
C     (1995) or Equation 3 of Binkowski and Roselle (2003).
C     N.B: these are for a "wet" size distribution

      DO I = 1, N_MODE
         AM0( I ) = MOMENT0_CONC( I ) 
         AM1( I ) = MOMENT0_CONC( I ) * AEROMODE_DIAM( I )
     &            * EXP( 0.5 * AEROMODE_LNSG( I ) * AEROMODE_LNSG( I ) )
         AM2( I ) = MOMENT2_CONC( I )
      END DO
      
C *** Calculate the size-dependent terms in the condensational-
C     growth factor expressions for sulfate using 
C     Equations A13-A14 of Binkowski & Shankar (1995). 
       
      DO I = 1, N_MODE
         CALL HCOND3( AM0( I ), AM1( I ), AM2( I ),
     &                DV_SO4, ALPHSULF, CBAR_SO4, FCONC_OUT )
         FCONC_SO4( I, : ) = FCONC_OUT( : )
      END DO 

      IF ( .NOT. HYBRID ) THEN
         FCONC_SO4( N_MODE,1 ) = 0.0D0
         FCONC_SO4( N_MODE,2 ) = 0.0D0
      END IF

      DO I = 1, N_MODE
         GRFAC1( I ) = FCONC_SO4( I,1 )
         GRFAC2( I ) = FCONC_SO4( I,2 )
      END DO

C *** New Particle Production
C     Calculate the new particle production rate due to binary
C     nucleation of H2O and H2SO4.  These calculations are performed 
C     only when the gas-phase production rate of H2SO4 (i.e., SO4RATE) 
C     is non-zero.  The condensation rate of H2SO4 is calculated as the
C     gas-phase production rate minus the new particle production rate.

C *** Initialize Variables
      DMDT_SO4  = 0.0D0
      DNDT      = 0.0D0
      DM2DT     = 0.0D0
      SCONDRATE = 0.0D0


C *** Produce new particles only during time steps when the gas-phase 
C     production rate of H2SO4 is non-zero

      IF ( SO4RATE .NE. 0.0D0 ) THEN

C *** Adjust sulfuric acid vapor concentration to a value in
C     equilibrium with the production of new particles and the
C     condensation of sulfuric acid vapor on existing particles, based 
C     on Equations A21 and A23 of Binkowski & Shankar (1995).
         TMP = 0.0
         DO I = 1, N_MODE
            TMP = TMP + FCONC_SO4( I,2 )
         END DO

         XH2SO4 = SO4RATE / REAL( TMP,4 )
         XH2SO4 = MAX( XH2SO4, CONMIN )
         !PRECURSOR_CONC( SULF_IDX ) = REAL( XH2SO4, 8 )

#ifdef sens
         DO NP = 1, NPMAX
            IF ( XH2SO4 .EQ. CONMIN ) THEN
               S_PRECURSOR_CONC( SULF_IDX, NP ) = 0.D0
            ELSE
               S_PRECURSOR_CONC( SULF_IDX, NP ) = S_SO4RATE( NP ) / REAL(TMP,8)
            ENDIF
         END DO
#endif

C *** Calculate new particle production rate for 0th, 2nd, & 3rd moments
         CALL NEWPART3 ( AIRRH, AIRTEMP, XH2SO4, SO4RATE,
     &                   DNDT, DMDT_SO4, DM2DT )
         
C *** Calculate sulfate condensation rate as the gas-phase production 
C     rate minus the new particle production rate, following Equation
C     3.23 of Jiang & Roth (2003).
         SCONDRATE = MAX( SO4RATE - DMDT_SO4, 0.0D0 )

! *** Propagate NPF Change to budget variables
         NPF_BUDGET( PRECURSOR_MAP( SULF_IDX ) ) = -DMDT_SO4 
     &             * H2SO4RAT  * DELT ! ug m-3 of Sulfuric Acid
                                      ! (converted to ppmv in update_precursor)
         NPF_BUDGET( AEROSPC_MAP( ASO4_IDX,1 ) ) =  DMDT_SO4 * DELT ! ug m-3
         NPF_BUDGET( AERONUM_MAP( 1 ) ) =  DNDT * DELT  ! N m-3
         NPF_BUDGET( AEROSRF_MAP( 1 ) ) =  DM2DT * DELT ! m2 m-3

      END IF   ! SO4RATE .NE. 0

C *** Sulfate Condensation (Size-resolved)
C     Calculate rate at which condensing sulfate should be added to each
C     mode.  The "omega" factors are defined in Equations 7a and 7b of
C     Binkowski & Shankar (1995). The i-mode and j-mode factors are 
C     calculated using Equation A17 of Binkowski & Shankar (1995). The 
C     condensation rate for accumulation mode (fine-equilibrium scheme) or 
C     coarse mode (hybrid and dynamic schemes) is computed by difference, 
C     to avoid mass conservation violations arising from numerical error.
      TMP = 0.0
      DO I = 1, N_MODE
         TMP = TMP + FCONC_SO4( I,2 )
      END DO

      FCONCM1_SO4  = 1.0D0 / TMP
      OMEGA_AT_SO4 = FCONCM1_SO4 * FCONC_SO4( 1,2 )
      OMEGA_AC_SO4 = FCONCM1_SO4 * FCONC_SO4( 2,2 )

C *** Growth values for mode merge condition
      CGR( 1 ) = SO4FAC * SCONDRATE * OMEGA_AT_SO4
      CGR( 2 ) = SO4FAC * SCONDRATE * OMEGA_AC_SO4

! *** Initialize Budget Counter Variables         
      CBUDGET0_NUM( : ) = REAL( MOMENT0_CONC( : ), 8)
      CBUDGET0_NUM( 1 ) = CBUDGET0_NUM( 1 ) + DNDT * DELT
      CBUDGET0_SRF( : ) = MOMENT2_CONC( : ) * PI
      CBUDGET0_SRF( 1 ) = CBUDGET0_SRF( 1 ) + DM2DT * DELT * PI
      CBUDGET0_SO4( : ) = AEROSPC_CONC( ASO4_IDX, : )
      CBUDGET0_SO4( 1 ) = CBUDGET0_SO4( 1 ) + DMDT_SO4 * DELT
      CBUDGET0_NO3( : ) = REAL( AEROSPC_CONC( ANO3_IDX, : ), 8)
      CBUDGET0_NH4( : ) = REAL( AEROSPC_CONC( ANH4_IDX, : ), 8)
      CBUDGET0_CL ( : ) = REAL( AEROSPC_CONC( ACL_IDX , : ), 8)
      CBUDGET0_H2O( : ) = REAL( AEROSPC_CONC( AH2O_IDX, : ), 8)
      CBUDGET0_H3OP( : )= REAL( AEROSPC_CONC( AH3OP_IDX, : ), 8)

! *** Diagnose Condensaiton Sink to Each Mode
      CONDSO4( 1 ) = OMEGA_AT_SO4 * SCONDRATE      
      
      IF ( HYBRID ) THEN 
         CONDSO4( 2 ) = OMEGA_AC_SO4 * SCONDRATE      
         CONDSO4( 3 ) = SCONDRATE - ( CONDSO4( 1 ) + CONDSO4( 2 ) ) 
      ELSE                                  ! fine equilibrium
         CONDSO4( 2 ) = SCONDRATE - CONDSO4( 1 )
         CONDSO4( 3 ) = 0.0D0               ! no coarse mode chemistry
      END IF

C *** For Hybrid approach, calculate dynamic mass trasfer for
C     semi-volatile components of coarse mode (NO3, CL, NH4)  

      IF ( HYBRID ) THEN 

         CNTRL( 1 ) = 1.0D0 ! reverse problem
         CNTRL( 2 ) = 1.0D0 ! aerosol in metastable state

         INT_TIME = 0.0D0
         TSTEP    = 90.0D0
         ISTEP    = 1
         IMODE    = 3
         TrustIso = .TRUE.
 
         DO WHILE ( INT_TIME .LT. DELT ) 

            IF ( INT_TIME + TSTEP .GT. DELT ) TSTEP = DELT - INT_TIME 
            INT_TIME = INT_TIME + TSTEP 
            ISTEP = ISTEP + 1   

C *** Calculate first moments using Equation 4 of Binkowski & Shankar
C     (1995) or Equation 3 of Binkowski and Roselle (2003).
C     N.B: these are for a "wet" size distribution
            AM0( IMODE ) = MOMENT0_CONC( IMODE )
            AM1( IMODE ) = MOMENT0_CONC( IMODE ) * AEROMODE_DIAM( IMODE )
     &                  * EXP( 0.5 * AEROMODE_LNSG( IMODE ) * AEROMODE_LNSG( IMODE ) )
            AM2( IMODE ) = MOMENT2_CONC( IMODE )

C *** Calculate the size-dependent terms in the condensational-
C     growth factor expressions for sulfate using 
C     Equations A13-A14 of Binkowski & Shankar (1995). 
            CALL HCOND3( AM0( IMODE ), AM1( IMODE ), AM2( IMODE ), DV_SO4, ALPHSULF, 
     &                      CBAR_SO4, FCONC_OUT )  ! adapted from Eq A14
            FCONC_SO4( IMODE, : ) = FCONC_OUT( : )

            GRFAC1( IMODE ) = FCONC_SO4( IMODE,1 ) 
            GRFAC2( IMODE ) = FCONC_SO4( IMODE,2 ) 

C *** Set conc array to aerosol concentrations prior to mass transfer
C     (The _RENORM and cation_FAC constants are set in the AERO_DATA module)

            SEACAT    = REAL( AEROSPC_CONC( ASEACAT_IDX,IMODE ), 8 )              
            SOILwVOLS = REAL( AEROSPC_CONC( ASOIL_IDX,IMODE ), 8 ) / ASOIL_RENORM 
            PMCwVOLS  = REAL( AEROSPC_CONC( ACORS_IDX,IMODE ), 8 ) / ACORSEM_RENORM 

            CINORG( KNH4,IMODE ) = REAL( AEROSPC_CONC( ANH4_IDX,IMODE ), 8 )  ! KNH4 = 1, ANH4_IDX = 4
            CINORG( KNO3,IMODE ) = REAL( AEROSPC_CONC( ANO3_IDX,IMODE ), 8 )  ! KNO3 = 2, ANO3_IDX = 2
            CINORG( KCL, IMODE ) = REAL( AEROSPC_CONC( ACL_IDX,IMODE ), 8 )   ! KCL = 3, ACL_IDX = 3
            CINORG( KSO4,IMODE ) = REAL( AEROSPC_CONC( ASO4_IDX,IMODE ), 8 )  ! KSO4 = 4, ASO4_IDX = 1
            CINORG( KNA, IMODE ) = ASCAT_NA_FAC * SEACAT                      ! KNA = 5
     &                           + ASOIL_NA_FAC * SOILwVOLS
     &                           + ACORS_NA_FAC * PMCwVOLS
            CINORG( KMG, IMODE ) = ASCAT_MG_FAC * SEACAT                      ! KMG = 6
     &                           + ASOIL_MG_FAC * SOILwVOLS
     &                           + ACORS_MG_FAC * PMCwVOLS
            CINORG( KK,  IMODE ) = ASCAT_K_FAC  * SEACAT                      ! KK = 7
     &                           + ASOIL_K_FAC  * SOILwVOLS
     &                           + ACORS_K_FAC  * PMCwVOLS
            CINORG( KCA, IMODE ) = ASCAT_CA_FAC * SEACAT                      ! KCA = 8
     &                           + ASOIL_CA_FAC * SOILwVOLS
     &                           + ACORS_CA_FAC * PMCwVOLS
            CINORG( KHP, IMODE ) = REAL( HPLUS( IMODE ), 8 )                  ! KHP = 9

            M3OTHR = SOILFAC * AEROSPC_CONC( ASOIL_IDX,IMODE )
     &             + ANTHFAC * AEROSPC_CONC( ACORS_IDX,IMODE )
            WETM3  = MOMENT3_CONC( IMODE )
            WETM2  = MOMENT2_CONC( IMODE )
            DRYM3  = WETM3 - REAL( H2OFAC * AEROSPC_CONC( AH2O_IDX, IMODE ), 8 )  ! Assume no SOA in coarse mode
            DRYM20 = WETM2 * ( DRYM3 / WETM3 ) ** D_TWOTHIRDS

C *** Initial vapor-phase concentrations [ug/m3]
            GNO3R8 = PRECURSOR_CONC( HNO3_IDX )
            GNH3R8 = PRECURSOR_CONC( NH3_IDX )
            GCLR8  = PRECURSOR_CONC( HCL_IDX )

C *** Compute sulfate production rate [ug/m3 s] for coarse mode

            RATE = CONDSO4( IMODE )
            SO4  = CINORG( KSO4,IMODE ) + RATE * TSTEP * H2SO4RATM1

            IF ( TrustIso ) THEN

C *** Double Precision vars for ISORROPIA [mole/m3]
C              Compute equilibrium vapor pressures [mol/m3] of NH3, HNO3, and HCL
C              at the gas/particle interface of coarse mode aerosol.
C                 GAS(1) = NH3, GAS(2) = HNO3, GAS(3) = HCl
               WI( 1 ) = CINORG( KNA, IMODE ) * CFAC_ANA
               WI( 2 ) =                  SO4 * CFAC_ASO4
               WI( 3 ) = CINORG( KNH4,IMODE ) * CFAC_ANH4
               WI( 4 ) = CINORG( KNO3,IMODE ) * CFAC_ANO3
               WI( 5 ) = CINORG( KCL, IMODE ) * CFAC_ACL
               WI( 6 ) = CINORG( KCA, IMODE ) * CFAC_ACA
               WI( 7 ) = CINORG( KK,  IMODE ) * CFAC_AK
               WI( 8 ) = CINORG( KMG, IMODE ) * CFAC_AMG
C              Also obtain the aqueous H+ concentration, AERLIQ(1) [mol/m3]

#ifdef verbose_aero
         if ( write1 ) then
         write( logdev,'(a, 8e13.3)' ) "VOLINORG,WI's C:",
     &                                  wi( 1 ), wi( 2 ), wi( 3 ), wi( 4 ),
     &                                  wi( 5 ), wi( 6 ), wi( 7 ), wi( 8 )
         write( logdev,* ) "VOLINORG,RH,T:", rhi, tempi
         end if
#endif

               CALL ISOROPIA( WI, RHI, TEMPI, CNTRL, WT, GAS, AERLIQ,  
     &                        AERSLD, SCASI, OTHER )

               IF ( GAS( 1 ) .LT. 0.0D0 .OR. GAS( 2 ) .LT. 0.0D0 .OR.
     &              GAS( 3 ) .LT. 0.0D0 ) THEN
                  IF ( FIRSTWRITE ) THEN
                     FIRSTWRITE = .FALSE.
                     WRITE( LOGDEV,2023 )
                  END IF
                  WRITE( LOGDEV,2029 ) COL, ROW, LAYER, GAS( 1 ), GAS( 2 ), GAS( 3 )
                  TrustIso = .FALSE.
               END IF

            END IF   ! TrustIso

C *** Change in volatile inorganic PM concentration to achieve
C     equilibrium, calculated as initial-gas-phase concentration minus 
C     equilibrium gas-phase concentration.  DVOLINORG is positive for
C     condensation and negative for evaporation.

#ifdef verbose_aero
         if ( write1 ) then
         write( logdev,'(a, 3e13.3)' ) "GASes:", gas( 1 ), gas( 2 ), gas( 3 )

         dvolinorg( knh4 ) = gnh3r8 * CFAC_GNH3  - gas( 1 )  ! [mol/m**3]
         dvolinorg( kno3 ) = gno3r8 * CFAC_GHNO3 - gas( 2 )  ! [mol/m**3]
         dvolinorg( kcl )  = gclr8  * CFAC_GHCL  - gas( 3 )  ! [mol/m**3]
           
         write( logdev,'(a, 3e13.3)' ) "DVOLINORG_coarse:",
     &      dvolinorg( knh4 ) / ( gnh3r8 * CFAC_GNH3 ),
     &      dvolinorg( kno3 ) / ( gno3r8 * CFAC_GHNO3),
     &      dvolinorg( kcl )  / ( gclr8  * CFAC_GHCL )
         end if
#endif

C *** Calculate condensation/evaporation flux for this time step and update 
C     volatile species concentrations.  Final aerosol conc set to be no less
C     than the minimum aerosol conc.
            IF ( TrustIso ) THEN
               CALL COMPUTE_FLUX( NVOLINORG, GNH3R8, GNO3R8, GCLR8, KNH4,
     &                            KNO3, KCL, GAS( 1:3 ), GRFAC2( IMODE ),
     &                            AERLIQ( 1 ), RATE, J )
            ELSE
               J( : ) = 0.0D0
            END IF 

            IF ( J( KNH4 ) * TSTEP * DF( KNH4 ) * NH3RAT .GT. GNH3R8 ) THEN
               WRITE( LOGDEV,* ) 'Condensed amt. exceeds NH3 conc: aero_subs.f'
               J( KNH4 ) = GNH3R8 / ( TSTEP * DF( KNH4 ) * NH3RAT )
            END IF
            IF ( J( KNO3 ) * TSTEP * DF( KNO3 ) * HNO3RAT .GT. GNO3R8 ) THEN
               WRITE( LOGDEV,* ) 'Condensed amt. exceeds HNO3 conc: aero_subs.f'
               J( KNO3 ) = GNO3R8 / ( TSTEP * DF( KNO3 ) * HNO3RAT )
            END IF
            IF ( J( KCL ) * TSTEP * DF(KCL) * HCLRAT .GT. GCLR8 ) THEN
               WRITE( LOGDEV,* ) 'Condensed amt. exceeds HCl conc: aero_subs.f'
               J( KCL ) = GCLR8 / ( TSTEP * DF( KCL ) * HCLRAT )
            END IF

C *** Integrate mass transfer equation, convert flux from molar to mass

            DO ISP = 1, NVOLINORG
               CFINAL( ISP,IMODE ) = MAX( 0.0D0,
     &                                    CINORG( ISP,IMODE )
     &                                    + J( ISP ) * TSTEP * DF( ISP ) )
            END DO               

C *** Calculate updated H+ concentration 

            HPLUS( IMODE ) = 0.0
     &                     - AEROSPC( ASO4_IDX )%CHARGE * SO4                  / AEROSPC_MW( ASO4_IDX )
     &                     - AEROSPC( ANO3_IDX )%CHARGE * CFINAL( KNO3,IMODE ) / AEROSPC_MW( ANO3_IDX )
     &                     - AEROSPC( ACL_IDX )%CHARGE  * CFINAL( KCL, IMODE ) / AEROSPC_MW( ACL_IDX )
     &                     - AEROSPC( ANH4_IDX )%CHARGE * CFINAL( KNH4,IMODE ) / AEROSPC_MW( ANH4_IDX )
!    &                     - AEROSPC( ANA_IDX )%CHARGE  * CINORG( KNA, IMODE ) / AEROSPC_MW( ANA_IDX )
     &                     - AEROSPC( ASEACAT_IDX )%CHARGE  * CINORG( KNA, IMODE ) / AEROSPC_MW( ASEACAT_IDX )
     &                     - AEROSPC( AMG_IDX )%CHARGE  * CINORG( KMG, IMODE ) / AEROSPC_MW( AMG_IDX )
     &                     - AEROSPC( AK_IDX )%CHARGE   * CINORG( KK,  IMODE ) / AEROSPC_MW( AK_IDX )
     &                     - AEROSPC( ACA_IDX )%CHARGE  * CINORG( KCA, IMODE ) / AEROSPC_MW( ACA_IDX )

C *** Equilibrate aerosol LWC with CFINAL by calling CALC_H2O
            WI( 1 ) = CINORG( KNA, IMODE ) * CFAC_ASEACAT
            WI( 2 ) =                  SO4 * CFAC_ASO4
            WI( 3 ) = CFINAL( KNH4,IMODE ) * CFAC_ANH4
            WI( 4 ) = CFINAL( KNO3,IMODE ) * CFAC_ANO3
            WI( 5 ) = CFINAL( KCL, IMODE ) * CFAC_ACL
            WI( 6 ) = CINORG( KCA, IMODE ) * CFAC_ACA
            WI( 7 ) = CINORG( KK,  IMODE ) * CFAC_AK
            WI( 8 ) = CINORG( KMG, IMODE ) * CFAC_AMG

            CALL CALC_H2O( WI, RHI, TEMPI, H2O_NEW ) 

            H2O = H2O_NEW * DFH2OR8 

C *** Update all Local Aerosol Mass and Vapor Concentrations 
            !Aerosol
            AEROSPC_CONC( ANH4_IDX,IMODE ) = REAL( CFINAL( KNH4,IMODE ),4 )
            AEROSPC_CONC( ANO3_IDX,IMODE ) = REAL( CFINAL( KNO3,IMODE ),4 )
            AEROSPC_CONC( ACL_IDX,IMODE )  = REAL( CFINAL( KCL, IMODE ),4 )
            AEROSPC_CONC( ASO4_IDX,IMODE ) = REAL( SO4,4 )
            AEROSPC_CONC( AH2O_IDX,IMODE ) = REAL( H2O,4 )

            !Gas
            PRECURSOR_CONC( NH3_IDX ) = GNH3R8 + ( CINORG( KNH4,IMODE )
     &                                 -CFINAL( KNH4,IMODE ) ) * NH3RAT 
            PRECURSOR_CONC( HNO3_IDX )= GNO3R8 + ( CINORG( KNO3,IMODE )
     &                                 -CFINAL( KNO3,IMODE) ) * HNO3RAT  
            PRECURSOR_CONC( HCL_IDX ) = GCLR8 + ( CINORG( KCL,IMODE )
     &                                 -CFINAL( KCL,IMODE) ) * HCLRAT 
 
C *** Compute net change in 3rd moment due to dry inorganic mass transfer

            DDRYM3DT = ( ( CFINAL( KNH4,IMODE ) - CINORG( KNH4,IMODE ) ) * M3FAC_ANH4
     &                 + ( CFINAL( KNO3,IMODE ) - CINORG( KNO3,IMODE ) ) * M3FAC_ANO3
     &                 + ( CFINAL( KCL, IMODE ) - CINORG( KCL,IMODE ) )  * M3FAC_ACL
     &                 + ( SO4                  - CINORG( KSO4,IMODE ) ) * M3FAC_ASO4 ) / TSTEP

C *** Compute net change in 2nd moment due to dry inorganic mass transfer
C     (including nucleation) using equation A7 of Binkowski & Shankar (1995)
            DDRYM2DT = D_TWOTHIRDS * GRFAC1( IMODE ) / GRFAC2( IMODE ) * DDRYM3DT   

C *** Update dry 2nd moment for condensation/evaporation based on whether
C     net change in dry 2nd moment is production or loss
            IF ( DDRYM2DT .LT. 0.0D0 ) THEN
               LOSS = DDRYM2DT / DRYM20
               Y = DRYM20 * EXP( LOSS * TSTEP )
            ELSE
               Y = DRYM20 + DDRYM2DT * TSTEP
            END IF

C *** Add water (no SOA) 2nd moment while preserving standard deviation

            !Calculate 3rd Moment
            DRYM3 = ( M3FAC_ASO4 ) * SO4
     &            + M3FAC_ANH4 * CFINAL( KNH4,IMODE )
     &            + M3FAC_ANO3 * CFINAL( KNO3,IMODE )
     &            + M3FAC_ACL  * CFINAL( KCL,IMODE )
     &            + M3FAC_ANA  * SEACAT
     &            + M3OTHR                   
            WETM3 = DRYM3 + H2O * M3FAC_H2O

            !Calculate 2nd moment
            DRYM2 = MAX( REAL( AEROMODE_MINM2( IMODE ), 8 ), Y )
            WETM2 = DRYM2 * ( WETM3 / DRYM3 ) ** D_TWOTHIRDS

            MOMENT2_CONC( IMODE ) = REAL( WETM2,4 )

C *** Update the third moments, geometric mean diameters, geometric 
C     standard deviations, modal mass totals, and modal particle 
C     densities. It is a waste of time updating the aitken and
C     accumulation modes but the coarse mode does need to be updated
C     each sub-time step. This should be for the wet distribution
               
            CALL GETPAR( .TRUE. )

         END DO   ! end mass transfer time step loop
         
      END IF   ! for 'Hybrid' method

      write1 = .false.
C *** End of Coarse Mode dynamic mass transfer calculations

C *** Fine Aerosol Modes: Call ISORROPIA in forward mode to calculate gas-particle equilibrium
      !Get Precursors Vapor concentrations [ug m-3]
      GNH3R8 = PRECURSOR_CONC( NH3_IDX )   
      GNO3R8 = PRECURSOR_CONC( HNO3_IDX )  
      GCLR8  = PRECURSOR_CONC( HCL_IDX )

C *** Diagnose all total gas+particle concentrations to passed to
C     ISORROPIA. Convert everything to [mol m-3].
      WI( 1 ) = SUM( AEROSPC_CONC( ANA_IDX,1:2 ))   * CFAC_ANA
      !Compute sulfate from total sulfate production rate [ug/m3-s] for fine 
      !modes; add in H2SO4 nucleated in model timestep
      WI( 2 ) = ( SUM( AEROSPC_CONC( ASO4_IDX,1:2 ) ) 
     &           +( DMDT_SO4 + SUM( CONDSO4( 1:2 ) ) ) * DELT * H2SO4RATM1 ) * CFAC_ASO4
      WI( 3 ) = PRECURSOR_CONC( NH3_IDX )  * CFAC_GNH3
     &         +SUM( AEROSPC_CONC( ANH4_IDX,1:2 ) ) * CFAC_ANH4 
      WI( 4 ) = PRECURSOR_CONC( HNO3_IDX ) * CFAC_GHNO3
     &         +SUM( AEROSPC_CONC( ANO3_IDX,1:2 ) ) * CFAC_ANO3 
      WI( 5 ) = PRECURSOR_CONC( HCL_IDX )  * CFAC_GHCL 
     &         +SUM( AEROSPC_CONC( ACL_IDX,1:2 ) )  * CFAC_ACL 
      WI( 6 ) = SUM( AEROSPC_CONC( ACA_IDX,1:2 ) )  * CFAC_ACA 
      WI( 7 ) = SUM( AEROSPC_CONC( AK_IDX,1:2 ) )   * CFAC_AK 
      WI( 8 ) = SUM( AEROSPC_CONC( AMG_IDX,1:2 ) )  * CFAC_AMG 

      CNTRL( 1 ) = 0.0D0   ! Forward Problem
      CNTRL( 2 ) = 1.0D0   ! Aerosol in Metastable State

C *** Set flags to account for mass conservation violations in ISRP3F
      TRUSTCL  = .TRUE.
      IF ( (WI( 1 ) + WI( 5 )) .LT. 1.0D-20 .or. WI( 5 ) .LT. 1.0D-10 ) THEN
         TRUSTCL = .FALSE.
      END IF
         
#ifndef noiso
      CALL ISOROPIA( WI, RHI, TEMPI, CNTRL, WT, GAS, AERLIQ,
     &               AERSLD, SCASI, OTHER )

#ifdef sens
      CALL AERO_SENS( SCASI, WI )
#endif

C *** Save H+ concentration information in microgram/m3 for consistency
      EQLBHIJ = AERLIQ(1) * 1.0D6 * AEROSPC_MW( ah3op_idx)

#else
      gas( 1 ) = real( precursor_conc( nh3_idx ),  8 ) * CFAC_GNH3
      gas( 2 ) = real( precursor_conc( hno3_idx ), 8 ) * CFAC_GHNO3
      gas( 3 ) = real( precursor_conc( hcl_idx ),  8 ) * CFAC_GHCL

      EQLBHIJ =  HPLUS( 1 ) + HPLUS( 2 )  ! use charge balance if lacking isoropia info 

#endif

C *** Change in volatile inorganic PM concentration to achieve
C     equilibrium, calculated as initial-gas-phase concentration minus 
C     equilibrium gas-phase concentration.  DVOLINORG is positive for
C     condensation and negative for evaporation.
      DVOLINORG( KNH4 ) = GNH3R8 * CFAC_GNH3  - GAS( 1 )   ! mol m-3
      DVOLINORG( KNO3 ) = GNO3R8 * CFAC_GHNO3 - GAS( 2 )   ! mol m-3
      DVOLINORG( KCL )  = GCLR8  * CFAC_GHCL  - GAS( 3 )   ! mol m-3

      IF ( DVOLINORG( KNH4 ) .LT. 0.0D0 ) THEN
         DVOLMAX = -SUM(REAL( AEROSPC_CONC( ANH4_IDX,1:2 ), 8 ) ) * CFAC_ANH4 + EVAPMIND
         DVOLINORG( KNH4 ) = MAX( DVOLINORG( KNH4 ), DVOLMAX )
      END IF

      IF ( DVOLINORG( KNO3 ) .LT. 0.0D0 ) THEN
         DVOLMAX = -SUM(REAL( AEROSPC_CONC( ANO3_IDX,1:2 ), 8 ) ) * CFAC_ANO3 + EVAPMIND
         DVOLINORG( KNO3 ) = MAX( DVOLINORG( KNO3 ), DVOLMAX)
      END IF

      IF ( .not.TRUSTCL ) THEN  
         DVOLINORG( KCL ) = 0.0D0
      ELSEIF ( DVOLINORG( KCL ) .LT. 0.0D0 ) THEN
         DVOLMAX = -SUM(REAL( AEROSPC_CONC( ACL_IDX,1:2 ), 8 ) ) * CFAC_ACL + EVAPMIND
         DVOLINORG( KCL ) = MAX( DVOLINORG( KCL ), DVOLMAX )
      END IF

C *** Apply modal partitioning of equilibrium aerosol mass
      ! Calculate Distribution of Mass Transfer Among Modes
      OMEGA( 1 ) = GRFAC2( 1 ) / ( GRFAC2( 1 ) + GRFAC2( 2 ) )
      OMEGA( 2 ) = 1.0D0 - OMEGA( 1 )

      ! Save Initial Concentrations
      DO IMODE = 1, 2  
         CINORG( KSO4,IMODE ) = AEROSPC_CONC( ASO4_IDX, IMODE )
         CINORG( KNH4,IMODE ) = AEROSPC_CONC( ANH4_IDX, IMODE )
         CINORG( KNO3,IMODE ) = AEROSPC_CONC( ANO3_IDX, IMODE )
         CINORG( KNA, IMODE ) = AEROSPC_CONC( ANA_IDX,  IMODE )
         CINORG( KCL, IMODE ) = AEROSPC_CONC( ACL_IDX,  IMODE )
         CINORG( KCA, IMODE ) = AEROSPC_CONC( ACA_IDX,  IMODE )
         CINORG( KK,  IMODE ) = AEROSPC_CONC( AK_IDX,   IMODE )
         CINORG( KMG, IMODE ) = AEROSPC_CONC( AMG_IDX,  IMODE )
      END DO

      ! Calculate Initial Distribution of Mass Composition Among Modes
      DO ISP = 1, NVOLINORG
         TOTAER( ISP ) = MAX( SUM( CINORG( ISP,1:2 ) ), CONMIND )
         DO IMODE = 1, 2
            PHI( ISP, IMODE ) = CINORG( ISP,IMODE ) / TOTAER( ISP )
         ENDDO
      ENDDO

      ! Initialize Final Concentrations
      CFINAL = 0.0

      ! Calculate Final Concentrations
      DO ISP = 1, NVOLINORG
         IF ( DVOLINORG( ISP ) .LT. 0.0 ) THEN
            ! Evaporate Mass Using Condensed-Phase Fraction in each Mode
            CFINAL( ISP,1:2 ) = CFINAL( ISP,1:2 ) + CINORG( ISP,1:2 )
     &                        + PHI( ISP,1:2 ) * DVOLINORG( ISP ) * DF( ISP )
         ELSE
            ! Condense Mass Using Condensation Sink Factors
            CFINAL( ISP,1:2 ) = CFINAL( ISP,1:2 ) + CINORG( ISP,1:2 )
     &                        + OMEGA( 1:2 ) * DVOLINORG( ISP ) * DF( ISP )
         END IF
      END DO

C *** Apply Final Concentrations to Moment Variables in Aerosol Scheme
      ! Store Initial Wet Moments
      M3WET_INIT( : ) = MOMENT3_CONC( : )
      M2WET_INIT( : ) = MOMENT2_CONC( : )

      ! Calculate and Store Initial Dry Moments and set wet_moments_flag to false/dry
      ! There has not yet been an update to AEROSPC_CONC so this will
      ! calculate the old 3rd moment.
      call calcmoments( .false. )
      M3DRY_INIT( : ) = MOMENT3_CONC( : )
      M2DRY_INIT( : ) = MOMENT2_CONC( : )

      DO IMODE = 1, 2  ! modal partitioning of equilibrium aerosol mass

         IF ( IMODE .EQ. 1 ) THEN
            ! Update Number Concentration with NPF
            MOMENT0_CONC( IMODE ) = REAL( AM0(IMODE) + DNDT * DELT, 4 )
            ! Add NPF to total condensation rate
            RATE = DMDT_SO4 + CONDSO4( IMODE )
            SO4 = CINORG( KSO4,IMODE ) + RATE * DELT * H2SO4RATM1 
         ELSE
            ! Ignore NPF. The small particles are not in these modes
            SO4 = CINORG( KSO4,IMODE ) + CONDSO4( IMODE ) * DELT * H2SO4RATM1
         END IF

C *** Double precision vars for CALC_H2O
         WI( 1 ) = CINORG( KNA, IMODE ) * CFAC_ANA
         WI( 2 ) =                  SO4 * CFAC_ASO4
         WI( 3 ) = CFINAL( KNH4,IMODE ) * CFAC_ANH4
         WI( 4 ) = CFINAL( KNO3,IMODE ) * CFAC_ANO3
         WI( 5 ) = CFINAL( KCL, IMODE ) * CFAC_ACL
         WI( 6 ) = CINORG( KCA, IMODE ) * CFAC_ACA
         WI( 7 ) = CINORG( KK,  IMODE ) * CFAC_AK
         WI( 8 ) = CINORG( KMG, IMODE ) * CFAC_AMG

         CALL CALC_H2O( WI, RHI, TEMPI, H2O_NEW ) 
         
         ! Update All Aerosol Concentrations
         AEROSPC_CONC( AH2O_IDX, IMODE ) = REAL( H2O_NEW * DFH2OR8, 4 )
         AEROSPC_CONC( ANH4_IDX, IMODE ) = REAL( CFINAL( KNH4,IMODE ),4 )
         AEROSPC_CONC( ANO3_IDX, IMODE ) = REAL( CFINAL( KNO3,IMODE ),4 )
         AEROSPC_CONC( ACL_IDX, IMODE )  = REAL( CFINAL( KCL ,IMODE ),4 )
         AEROSPC_CONC( ASO4_IDX, IMODE ) = REAL( SO4,4 )

C *** Compute net change in 3rd moment due to dry inorganic mass transfer
C     (includes nucleated sulfate mass). This is for projecting the
C     change to the second moment due to dry inorganic condensation
         
         DDRYM3DT = ( ( CFINAL( KNH4,IMODE ) - CINORG( KNH4,IMODE ) ) * M3FAC_ANH4
     &              + ( CFINAL( KNO3,IMODE ) - CINORG( KNO3,IMODE ) ) * M3FAC_ANO3
     &              + ( CFINAL( KCL, IMODE ) - CINORG( KCL,IMODE ) )  * M3FAC_ACL
     &              + ( SO4                  - CINORG( KSO4,IMODE ) ) * M3FAC_ASO4 ) 
     &              / DELT 

C *** Compute net change in 2nd moment due to dry inorganic mass transfer
C     (including nucleation) using equation A7 of Binkowski & Shankar (1995)
         DDRYM2DT = D_TWOTHIRDS * GRFAC1( IMODE ) / GRFAC2( IMODE ) * DDRYM3DT

C *** Update dry 2nd moment for condensation/evaporation based on whether
C     net change in dry 2nd moment is production or loss
         IF ( DDRYM2DT .LT. 0.0D0 ) THEN
            LOSS = DDRYM2DT / M2DRY_INIT( IMODE )
            Y = M2DRY_INIT( IMODE ) * EXP( LOSS * DELT )
         ELSE
            Y = M2DRY_INIT( IMODE ) + DDRYM2DT * DELT
         END IF
         moment2_conc( IMODE ) = REAL( MAX( REAL( AEROMODE_MINM2( IMODE ), 8 ), Y ),4 )

      END DO

C *** Add water and SOA to 2nd moment while preserving standard deviation
      call calcmoments( .true. )
      M3WET_FINAL = moment3_conc
      M2WET_FINAL = moment2_conc

C *** Assign H+ Concentration to each Mode
      HPLUS( 1:2 ) = 0.0
      DO I = 1, N_AEROSPC
         IF(  AEROSPC( I )%tracer )CYCLE
         HPLUS( 1:2 ) = HPLUS( 1:2 )
     &                  - AEROSPC( I )%CHARGE * AEROSPC_CONC( I,1:2 ) / AEROSPC_MW( I )
      END DO

      H2O = AEROSPC_CONC( AH2O_IDX, 1 ) + AEROSPC_CONC( AH2O_IDX, 2 )
      IF(  H2O .GT. CONMIN )THEN
          H2O = 1.0 / H2O
          AEROSPC_CONC( AH3OP_IDX, 1:2 ) = REAL( EQLBHIJ * H2O, 4 ) * AEROSPC_CONC( AH2O_IDX, 1:2 ) 
      ELSE
          AEROSPC_CONC( AH3OP_IDX, 1:2 ) = CONMIN
      END IF
      AEROSPC_CONC( AH3OP_IDX, 3 ) = REAL( HPLUS( 3 ),4 ) * AEROSPC_MW( ah3op_idx ) ! Coarse mode H+ concentration in ug/m3

C *** Update the third moments, geometric mean diameters, geometric 
C     standard deviations, modal mass totals, and modal particle 
C     densities. Note that moment2_conc needs to be up to date when this
C     routine is called. Moment3_conc does not need to be up to date
C     because it will be recalculated inside GETPAR as the sum of 
C     aerospc_conc variables. This should be for the wet distribution.
      CALL GETPAR( .TRUE. )
       
! *** Propagate Concentration Changes from Coarse-Mode Mass
! *** Transfer to Budget Vectors
      DELNUM( : )  = MOMENT0_CONC( : ) - CBUDGET0_NUM( : )
      DELSRF( : )  = MOMENT2_CONC( : ) * PI - CBUDGET0_SRF( : )
      DELSO4( : )  = AEROSPC_CONC( ASO4_IDX, : ) - CBUDGET0_SO4( : )
      DELNH4( : )  = AEROSPC_CONC( ANH4_IDX, : ) - CBUDGET0_NH4( : )
      DELNO3( : )  = AEROSPC_CONC( ANO3_IDX, : ) - CBUDGET0_NO3( : )
      DELCL( : )   = AEROSPC_CONC( ACL_IDX , : ) - CBUDGET0_CL( : )
      DELH2O( : )  = AEROSPC_CONC( AH2O_IDX, : ) - CBUDGET0_H2O( : )
      DELH3OP( : ) = AEROSPC_CONC( AH3OP_IDX,: ) - CBUDGET0_H3OP( : )

      COND_BUDGET( AERONUM_MAP( : ) ) = REAL( DELNUM( : ), 4 )
      COND_BUDGET( AEROSRF_MAP( : ) ) = REAL( DELSRF( : ), 4 )
      COND_BUDGET( AEROSPC_MAP( ASO4_IDX,: ) ) = REAL( DELSO4( : ), 4 )
      COND_BUDGET( AEROSPC_MAP( ANH4_IDX,: ) ) = REAL( DELNH4( : ), 4 )
      COND_BUDGET( AEROSPC_MAP( ANO3_IDX,: ) ) = REAL( DELNO3( : ), 4 )
      COND_BUDGET( AEROSPC_MAP( ACL_IDX ,: ) ) = REAL( DELCL( : ) , 4 )
      COND_BUDGET( AEROSPC_MAP( AH2O_IDX,: ) ) = REAL( DELH2O( : ), 4 )
      COND_BUDGET( AEROSPC_MAP( AH3OP_IDX,:) ) = REAL( DELH3OP( : ),4 )

      COND_BUDGET( PRECURSOR_MAP( SULF_IDX ) ) = SUM( REAL( -DELSO4(:) * H2SO4RAT, 4 ))
      COND_BUDGET( PRECURSOR_MAP( NH3_IDX  ) ) = SUM( REAL( -DELNH4(:) * NH3RAT  , 4 ))
      COND_BUDGET( PRECURSOR_MAP( HNO3_IDX ) ) = SUM( REAL( -DELNO3(:) * HNO3RAT , 4 ))
      COND_BUDGET( PRECURSOR_MAP( HCL_IDX  ) ) = SUM( REAL( -DELCL(:)  * HCLRAT  , 4 ))

 
C *** Update gas-phase concentrations from Aitken and Accumulation Mode Partitioning
      PRECURSOR_CONC( SULF_IDX ) = PRECURSOR_CONC( SULF_IDX )
     &                            +SUM( -DELSO4(:) * H2SO4RAT )
      PRECURSOR_CONC( NH3_IDX )  = GNH3R8 + sum( CINORG( KNH4,1:2 )
     &                            -CFINAL( KNH4,1:2 ) ) * NH3RAT 
      PRECURSOR_CONC( HNO3_IDX ) = GNO3R8 + sum( CINORG( KNO3,1:2 )
     &                            -CFINAL( KNO3,1:2) ) * HNO3RAT  
      PRECURSOR_CONC( HCL_IDX )  = GCLR8 + sum( CINORG( KCL,1:2 )
     &                            -CFINAL( KCL,1:2) ) * HCLRAT 

2023  FORMAT( 1X, 'VOLINORG returning negative gas concentrations from ISOROPIA:'
     &       /10X, 'GAS(1) = NH3, GAS(2) = HNO3, GAS(3) = HCl' )
2029  FORMAT( 1X, '[see VOLINORG msg]'
     &        1X, 'at (C,R,L): ', 3I4, 1X, 'GAS Conc:', 3( 1PE11.3 ) )

      RETURN
      END SUBROUTINE VOLINORG

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE HCOND3( AM0, AM1, AM2, DV, ALPHA, CBAR, F )

C  calculates the size-dependent term in the condensational-growth rate
C  expression for the 2nd and 3rd moments of a lognormal aerosol mode
C  using the harmonic mean method.  This code follows Section A2 of
C  Binkowski & Shankar (1995).
 
C  Key Subroutines/Functions called:  none
 
C  Revision History:
C     coded November 7, 2003 by Dr. Francis S. Binkowski
C     Revised November 20, 2003 by F. Binkowski to have am1 and
C     am2 as inputs
 
C  Reference:
C   1. Binkowski, F.S. and U. Shankar, The regional particulate matter
C      model 1. Model description and preliminary results, J. Geophys.
C      Res., Vol 100, No D12, 26101-26209, 1995.

      IMPLICIT NONE

C *** Includes:

      INCLUDE SUBST_CONST     ! physical and mathematical constants

C *** Arguments:

      REAL( 8 ), INTENT( IN ) :: AM0   ! zeroth moment of mode  [ #/m**3 ]
      REAL( 8 ), INTENT( IN ) :: AM1   ! first moment of mode   [ m/m**3 ]
      REAL( 8 ), INTENT( IN ) :: AM2   ! second moment of mode  [ m**2/m**3 ]
      REAL,      INTENT( IN ) :: Dv    ! molecular diffusivity of the
                                       ! condensing vapor  [ m**2/s ]
      REAL,      INTENT( IN ) :: ALPHA ! accommodation coefficient
      REAL,      INTENT( IN ) :: CBAR  ! kinetic velocity of condensing vapor [ m/s ]

      REAL( 8 ), INTENT( OUT ) :: F( 2 ) ! size-dependent term in condensational-growth
                                         ! rate: F(1) = 2nd moment [ m**2/m**3 s ]
                                         !       F(2) = 3rd moment [ m**3/m**3 s ]

C *** Local Variables:

      REAL( 8 ) :: GNC2 ! integrals used to calculate F(1) [m^2 / m^3 s]
      REAL( 8 ) :: GFM2 !

      REAL( 8 ) :: GNC3 ! integrals used to calculate F(2) [m^3 / m^3 s]
      REAL( 8 ) :: GFM3 !

      REAL( 8 ), PARAMETER :: TWOPI = 2.0D0 * PI
      REAL( 8 ), PARAMETER :: PI4 = 0.25D0 * PI

C-----------------------------------------------------------------------

C *** Implement equation A15 of Binkowski & Shankar (1995) for the
C     2nd and 3rd moments of a lognormal mode of arbitrary size.

      GNC2 = TWOPI * DV * AM0          ! 2nd moment, near-continuum
      GNC3 = TWOPI * DV * AM1          ! 3rd moment, near-continuum
      GFM2 = PI4 * ALPHA * CBAR * AM1  ! 2nd moment, free-molecular
      GFM3 = PI4 * ALPHA * CBAR * AM2  ! 3rd moment, free-molecular

C *** Implement equation A13 of Binkowski & Shankar (1995) for a
C     lognormal mode of arbitrary size.  These are the size-dependent
C     terms in the condensational-growth rate expression, given in
C     equation 7a of B&S (1995).

      F( 1 ) = GNC2 * GFM2 / ( GNC2 + GFM2 )  ! 2nd moment
      F( 2 ) = GNC3 * GFM3 / ( GNC3 + GFM3 )  ! 3rd moment

      RETURN
      END SUBROUTINE HCOND3

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE NEWPART3 ( RH, T, XH2SO4, SO4RATE, DNDT, DMDT_so4, DM2DT )

      USE AERO_DATA
      USE AEROMET_DATA   ! Includes CONST.EXT
      USE PRECURSOR_DATA, ONLY: PRECURSOR_MW, SULF_IDX
      USE UTILIO_DEFN
      
      IMPLICIT NONE

C   REVISION HISTORY:
C     Replacement of Kulmala et al., 1998 nucleation scheme with scheme 
C     of Vehkamaki et al. (2002) by G. Sarwar and K. Fahey - 03/2014

C.. References:
C     Vehkamaki, H., Kulmala, M, Napari, I., Lehtinen, K.E.J., Timmreck, C.,
C     Noppel, M., and A. Laaksonen. (2002) An improved parameterization for 
C     sulfuric acid-water nucleation rates for tropospheric and stratospheric
C     conditions.  JGR, v107(D22).    
   
C..Inputs: 
      REAL, INTENT(IN ) :: RH              ! fractional relative humidity      
      REAL, INTENT(IN)  :: T               ! Ambient temperature [ K ]
      REAL, INTENT(IN)  :: XH2SO4          ! sulfuric acid concentration [ ug/m**3 ]
      REAL, INTENT(IN)  :: SO4RATE         ! gas-phase H2SO4 production rate [ ug/m**3 s ]

C.. Outputs:
      REAL( 8 ), INTENT( OUT ) :: DNDT     ! particle number production rate [ m^-3/s ]
      REAL( 8 ), INTENT( OUT ) :: DMDT_so4 ! SO4 mass production rate [ ug/m**3 s ]
      REAL( 8 ), INTENT( OUT ) :: DM2DT    ! second moment production rate [ m**2/m**3 s ]

C.. Parameters
      CHARACTER( 16 ), PARAMETER :: PNAME = 'NEWPART'                 

C.. Particle size parameters:
      REAL, PARAMETER :: d20 = 2.0E-07                ! diameter of a new particle [cm]
      REAL, PARAMETER :: d20sq = d20 * d20            ! new-particle diameter squared [cm**2]
      REAL, PARAMETER :: m2_20 = 1.0E-4 * d20sq       ! new-particle diameter squared [m**2]
      REAL, PARAMETER :: v20 = PI * d20 * d20sq /6.0  ! volume of a new particle [cm**3]
      
      REAL( 8 )       :: sulfmass                     ! mass of a new particle [ug]
      REAL( 8 )       :: sulfmass1                    ! inverse of sulfmass [ug**-1]

C.. Set constants and local variables for Vehkamaki et al. (2002) scheme
      REAL, PARAMETER :: C1 = 0.740997
      REAL, PARAMETER :: C2 = -0.00266379
      REAL, PARAMETER :: C3 = -0.00349998
      REAL, PARAMETER :: C4 = 0.0000504022
      REAL, PARAMETER :: C5 = 0.00201048
      REAL, PARAMETER :: C6 = -0.000183289
      REAL, PARAMETER :: C7 = 0.00157407
      REAL, PARAMETER :: C8 = -0.0000179059
      REAL, PARAMETER :: C9 = 0.000184403
      REAL, PARAMETER :: C10 = -1.50345E-6

      REAL :: XSTAR           ! mole fraction of sulfuric acid in the critical cluster
      REAL :: NA              ! total gas phase concentration of H2SO4  [ #/cm**3 ]
      REAL :: TEMP            ! ambient temperature
      REAL :: LNRH, LNNA      ! LN(RH), LN(Na)
      REAL :: LNRH2, LNNA2    ! LN(RH)**2, LN(Na)**2
      REAL :: LNRH3, LNNA3    ! LN(RH)**3, LN(Na)**3  
      REAL :: TEMP2, TEMP3    ! TEMP**2, TEMP**3
      REAL( 8 ) :: XFAC       ! exponential term for the nucleation rate  
      REAL( 8 ) :: Jnuc       ! nucleation rate [ #/cm**3 s ]
      
      REAL :: A, B, C, D, E, F, G, H, I, J

      REAL :: MW_H2SO4        ! MW of H2SO4 in [ g / mole ]        
      REAL :: DENSITY_H2SO4   ! DENSITY of H2SO4 in [ kg / m**3 ]

      REAL, PARAMETER :: SCALEFAC = 1.0E-06           ! for [ 1 / m**3 ] ->  [ 1 / cm**3 ]
      REAL, PARAMETER :: MUG2G = 1.0E-6               ! [ ug  ] -> [ g ]                 

C.. Initialize variables
      DNDT     = 0.0D0
      DMDT_so4 = 0.0D0
      DM2DT    = 0.0D0

C.. Calculate molecular weight of H2SO4 [ g / mole ] 
      MW_H2SO4 = REAL( PRECURSOR_MW( SULF_IDX ),4 )
         
C.. Calculate density of sulfuric acid [ kg / m**3 ] 
      DENSITY_H2SO4 = AEROSPC( ASO4_IDX )%DENSITY           

C.. Calculate mass of a new particle [ug]
      sulfmass = 1.0D+3 * DENSITY_H2SO4 * v20

C.. Calculate inverse of sulfmass [ug**-1]      
      sulfmass1 = 1.0D0 / sulfmass   
     
C.. Calculate sulfuric acid concentration in molecules/cm3 
      NA = XH2SO4 * MUG2G * AVO * SCALEFAC / MW_H2SO4     

C.. The parameterization is valid at sulfuric acid concentrations of 1.0E4 - 1.0E11 molecules cm-3
      NA  = MAX (NA, 1.0E4)
      NA  = MIN (1.0E11, NA)

C.. The parameterization is valid at temperatures of 190.00-305.15 K
      TEMP = MAX (T, 190.00)
      TEMP = MIN (305.15, TEMP)

C.. The parameterization is valid at RH of 0.0001-1.0
C   aero_driver.f limits RH to 0.005-0.99; thus no additional constraint is needed

C.. Define convenient constants
      TEMP2 = TEMP * TEMP
      TEMP3 = TEMP * TEMP2
      
      LNRH = LOG( RH )  
      LNNA = LOG( NA )
      LNRH2 = LNRH * LNRH
      LNRH3 = LNRH * LNRH2
      LNNA2 = LNNA * LNNA
      LNNA3 = LNNA * LNNA2

C.. Calculate mole fraction of sulfuric acid in the critical cluster
      XSTAR = C1 + C2 * TEMP + C3 * LNNA + C4 * TEMP * LNNA + 
     &      C5 * LNRH + C6 * TEMP * LNRH +
     &      C7 * LNRH2 + C8 * TEMP * LNRH2 + C9 * LNRH3 + 
     &      C10 * TEMP * LNRH3

C.. Calculate coefficients needed for the nucleation rate [Eq-12 - Vehkamaki et al., 2002]           
      A = 0.14309 + 2.21956 * TEMP - 0.0273911 * TEMP2 + 
     &      0.0000722811 * TEMP3 + 5.91822 / XSTAR
     
      B = 0.117489 + 0.462532 * TEMP - 0.0118059 * TEMP2 + 
     &      0.0000404196 * TEMP3 + 15.7963 / XSTAR
     
      C = -0.215554 - 0.0810269 * TEMP + 0.00143581 * TEMP2 - 
     &      4.7758E-6 * TEMP3 - 2.91297 / XSTAR
     
      D = -3.58856 + 0.049508 * TEMP - 0.00021382 * TEMP2 + 
     &      3.10801E-7 * TEMP3 - 0.0293333 / XSTAR
     
      E = 1.14598 - 0.600796 * TEMP + 0.00864245 * TEMP2 - 
     &      0.0000228947 * TEMP3 - 8.44985 / XSTAR
     
      F = 2.15855 + 0.0808121 * TEMP - 0.000407382 * TEMP2 - 
     &      4.01957E-7 * TEMP3 + 0.721326 / XSTAR

      G = 1.6241 - 0.0160106 * TEMP + 0.0000377124 * TEMP2 + 
     &      3.21794E-8 * TEMP3 - 0.0113255 / XSTAR
      
      H = 9.71682 - 0.115048 * TEMP + 0.000157098 * TEMP2 + 
     &      4.00914E-7 * TEMP3 + 0.71186 / XSTAR
     
      I = -1.05611 + 0.00903378 * TEMP - 0.0000198417 * TEMP2 +
     &      2.46048E-8 * TEMP3 - 0.0579087 / XSTAR

      J = -0.148712 + 0.00283508 * TEMP - 9.24619E-6 * TEMP2 + 
     &      5.00427E-9 * TEMP3 - 0.0127081 / XSTAR

C.. Calculate the exponential term for the nucleation rate [Eq-12 - Vehkamaki et al., 2002] 
      XFAC = A + B * LNRH + C * LNRH2 + D * LNRH3 + 
     &      E * LNNA + F * LNRH * LNNA + G * LNRH2 * LNNA +     
     &      H * LNNA2 + I * LNRH * LNNA2 + J * LNNA3

C.. Calculate particle nucleation rate: unit [ 1 / cm**3 s] [Eq-12 - Vehkamaki et al., 2002] 
      Jnuc = EXP(XFAC)  
      
C.. The parameterization is valid for nucleation rates of 1.0E-7-1.0E10 [ 1 / cm**3 s]
      Jnuc = MAX (Jnuc, 1.0D-7)
      Jnuc = MIN (1.0D10, Jnuc)

C.. Convert the unit of particle nucleation rate into [ 1 / m**3 s] by multiplying it by 1.0E6  
       DNDT = Jnuc * 1.0E06  ! (1/(m**3 s))

C.. Calculate mass production rate [ ug / (m**3 s) ] analogous to
C   Equation 6a of Binkowski & Roselle (2003). Set the upper limit
C   of the mass production rate as the gas-phase production rate of
C   H2SO4, and adjust the number production rate accordingly.
      DMDT_so4 = sulfmass * DNDT 

      IF ( DMDT_so4 .GT. SO4RATE ) THEN
         DMDT_so4 = SO4RATE
         DNDT = DMDT_SO4 * sulfmass1
      END IF

C.. Calculate the production rate of 2nd moment [ m**2 / (m**3 s) ]
C   This is similar to Equation 6c of Binkowski & Roselle (2003),
C   except the factor of PI is removed and the assumed particle
C   diameter is different.
      DM2DT = DNDT * m2_20

      RETURN    
      END SUBROUTINE NEWPART3 

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE Compute_Flux ( nvolinorg, GNH3R8, GNO3R8, GCLR8, KNH4,
     &                          KNO3, KCL, Ceq, CondRate, Hplus, rate, J )
 
C Description
C   Determines the evaporative/condensational flux of volatile
C   inorganic species to aerosol modes. In cases where the resulting H+
C   flux is greater than a specified limit, the Pilinis et al. (2000)
C   AS&T approach is used to modify species vapor pressures such that
C   cond./evap. produces an H+ flux equal to the limit (which is
C   proportional to the current mode concentration of H+).
C   Routine called by VOLINORG.
 
C Arguments
C   Inputs
C     nvolinorg: Number of Volatile inorganic species
C     GNH3R8   : NH3(g) concentration (ug/m3)
C     GNO3R8   : HNO3(g) concentration (ug/m3)
C     GCLR8    : HCl(g) concentration (ug/m3)
C     KNH4     : Index to NH4 species
C     KNO3     : Index to NO3 species
C     KCL      : Index to NO3 species 
C     Ceq      : vapor concentration (mol/m3)
C     CondRate : effective condensation rate (I) of 3rd moment to mode
C              : [treat units as (1/s)]
C     Hplus    : aerosol hydrogen ion concentration (mol/m3) for mode
C     rate     : H2SO4(g) condensation rate (ug/m3/s) for mode
 
C   Output
C     Ceq      : modified vapor concentration (mol/m3)
C     J        : molar cond./evap. flux of volatile inorganics (mol/m3-s)
 
C-----------------------------------------------------------------------

      USE AERO_DATA
      USE PRECURSOR_DATA
      USE AEROMET_DATA

      IMPLICIT NONE

C     Arguments:
      INTEGER      nvolinorg
      REAL( 8 ) :: GNH3R8, GNO3R8, GCLR8 ! gas concentrations [ug/m3]
      INTEGER      KNH4, KNO3, KCL       ! Indices to species
      REAL( 8 ) :: Ceq( nvolinorg )      ! vapor concentrations [mol/m3]
      REAL( 8 ) :: CondRate              ! effective condensation rate (I) for 3rd moment
      REAL( 8 ) :: Hplus                 ! hydrogen ion concentration for mode [mol/m3]
      REAL( 8 ) :: rate
      REAL( 8 ) :: J( nvolinorg )        ! molar cond./evap. flux [mol/m3-s]

C     Local Variables:
      REAL( 8 ),  PARAMETER :: Afact = 1.0D-01  ! factor for H+ limiter
      REAL( 8 ),  PARAMETER :: small = 1.0D-25
      REAL( 8 ) :: Cinf( nvolinorg ) ! gas concentration in mol/m3
      REAL( 8 ) :: Qk              ! factor for modifying vapor press. based on H+ limit
      REAL( 8 ) :: Hflux           ! flux of H+ to mode from cond/evap
      REAL( 8 ) :: Hlim            ! maximum allowable H+ flux to mode
      REAL( 8 ) :: aa, bb, cc      ! terms in quadratic equation
      REAL( 8 ) :: JH2SO4          ! molar flux of H2SO4(g) [mol/m3/s]
      REAL( 8 ) :: CH2SO4          ! effective H2SO4(g) concentration [mol/m3]
      INTEGER      isp             ! inorganic species index

C-----------------------------------------------------------------------

C     Convert gas concentration from ug/m3 to mol/m3
      Cinf( KNH4 ) = GNH3R8 * 1.0D-6 / PRECURSOR_MW( NH3_IDX )
      Cinf( KNO3 ) = GNO3R8 * 1.0D-6 / PRECURSOR_MW( HNO3_IDX )
      Cinf( KCL )  = GCLR8  * 1.0D-6 / PRECURSOR_MW( HCL_IDX )

C     Calculate cond/evap fluxes (no H+ limiting)
      DO isp = 1, nvolinorg
         J( isp ) = CondRate * ( Cinf( isp ) - Ceq( isp ) )
      END DO

C     Convert rate to mol/m3/s and get effective Cinf for H2SO4(g)
      JH2SO4  = rate * 1.0D-6 / PRECURSOR_MW( SULPRD_IDX )
      CH2SO4  = JH2SO4 / CondRate

C     Limit H+ flux (Pilinis et al., 2000, AS&T). Note: J is flux
C     to entire mode, not one particle
      Hlim  = Afact * Hplus
      Hflux = 2.0D0 * JH2SO4 + J( KNO3 ) + J( KCL ) - J( KNH4 )

C     If Hflux is too large, limit the flux by modifying species
C     vapor pressures with Qk factor (Pilinis et al., 2000, AS&T).
      IF ( ABS( Hflux ) .GT. Hlim ) THEN
         Hlim = SIGN( Hlim, Hflux )

C        Solve quadratic for Qk: aa*Qk^2 + bb*Qk + cc = 0
         aa = Ceq( KCL ) + Ceq( KNO3 )

         bb = Hlim / CondRate
     &      + Cinf( KNH4) - Cinf( KNO3 ) - Cinf( KCL ) - 2.0D0 * CH2SO4
         cc = -Ceq( KNH4 )

         Qk = 0.0D0 ! initialize Qk

         IF ( aa .LT. small .AND. 0.0D0 .LT. bb ) THEN ! bb*Qk + cc = 0
            Qk = -cc / bb
         ELSE IF (aa .LT. small .AND. bb .LE. 0.0D0 ) THEN
            Qk = 0.0D0
         ELSE IF (-cc .LT. small .AND. bb .LT. 0.0D0 ) THEN  ! aa*Qk^2 + bb*Qk = 0
            Qk = -bb / aa
         ELSE IF (-cc .LT. small .AND. 0.0D0 .LE. bb ) THEN
            Qk = 0.0D0
         ELSE
            Qk = ( -bb + SQRT ( bb**2 - 4.0D0 * aa * cc ) ) / ( 2.0D0 * aa )
            IF ( bb ** 2 - 4.0D0 * aa * cc .LT. 0.0D0 ) THEN
               PRINT *, 'Compute_Flux, sqrt<0'
               Qk = 0.0D0
            END IF
         END IF

C     Modify vapor pressures and get new fluxes
         IF ( Qk .GT. small ) THEN
            Ceq( KNH4 ) = Ceq( KNH4 ) / Qk
            Ceq( KNO3 ) = Ceq( KNO3 ) * Qk
            Ceq( KCl )  = Ceq( KCl )  * Qk
            DO isp = 1, nvolinorg
               J( isp ) = CondRate * ( Cinf( isp ) - Ceq( isp ) )
            END DO
         END IF

      END IF   ! |Hflux| > Hlim

      END SUBROUTINE Compute_Flux

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE CALC_H2O ( WI, RH, T, H2O_NEW )

C Description
C   Calculate the water content of aerosol at the new time step.  Water
C   calculations use the ZSR mixing rule with salts determined by the
C   ISORROPIA approach.
C   Routine called by VOLINORG.
 
C Arguments
C   Input
C     WI      : Concentration of components [mol/m^3] at new step
C     RH      : Relative humidity [0-1]
C     T       : Temperature [K]
 
C   Output
C     H2O_NEW : Water [mol/m^3] content at new time step
 
C-----------------------------------------------------------------------

      IMPLICIT NONE

C Parameters:
      INTEGER, PARAMETER :: NCMP = 8, NPAIR = 23
      REAL( 8 ),  PARAMETER :: SMALL = 1.0D-20
      REAL( 8 ),  PARAMETER :: Mw = 0.018D0   ! molar mass H2O (kg/mol)

C Arguments:
      REAL( 8 ), INTENT( IN )  :: WI( NCMP )
      REAL( 8 ), INTENT( IN )  :: RH, T
      REAL( 8 ), INTENT( OUT ) :: H2O_NEW

C Local Variables:
      CHARACTER( 3 ) :: SC ! subcase for composition
      REAL( 8 ) :: FSO4, FNH4, FNA, FNO3, FCL ! "free" ion amounts
      REAL( 8 ) :: FCA, FK, FMG            
      REAL( 8 ) :: CASO4         ! amount of calcium sulfate, does not participate in ZSR calc
      REAL( 8 ) :: WATER         ! kg of water for new time step 
      REAL( 8 ) :: X, Y
      REAL( 8 ) :: CONC( NCMP )    ! concentration (mol/m^3)
      REAL( 8 ) :: CONCR( NPAIR )  ! concentration (mol/m^3) ion "pairs" 
      REAL( 8 ) :: M0I( NPAIR )    ! single-solute molalities
      INTEGER :: J

C-----------------------------------------------------------------------

C     Return if small concentration
      IF ( WI( 1 ) + WI( 2 ) + WI( 3 ) + WI( 4 )
     &   + WI( 5 ) + WI( 6 ) + WI( 7 ) + WI( 8 ) .LE. SMALL) THEN
         H2O_NEW = SMALL
         RETURN
      END IF

C     Set component array (mol/m^3) for determining salts
      CONC = WI   ! array assignment

C     Get the sub-case to use in determining salts
      CALL GETSC ( CONC, RH, T, SC )

#ifdef verbose_aero
!     write( logdev,* ) 'CALC_H2O -SC: ', sc
#endif

C     Initialize ion "pairs" (i.e., salts) used in ZSR
      CONCR( : ) = 0.0D0

C     Depending on case, determine moles of salts in solution (i.e., CONCR)
C     for ZSR calculation below

      IF ( SC .EQ. 'S2' ) THEN    ! sulfate poor (NH4-SO4 system), old K2
         CONCR( 4 )= MIN ( CONC( 2 ), 0.5D0 * CONC( 3 ) )  ! (NH4)2SO4

      ELSE IF ( SC .EQ. 'B4' ) THEN  ! sulfate rich (no acid), old L4, O4
         X = 2.0D0 * CONC( 2 ) - CONC( 3 )     ! 2SO4 - NH4
         Y = CONC( 3 ) - CONC( 2 )            ! NH4 - SO4
         IF ( X .LE. Y ) THEN
            CONCR( 13 ) = X      ! (NH4)3H(SO4)2 is MIN (X,Y)
            CONCR(  4 ) = Y - X  ! (NH4)2SO4
         ELSE
            CONCR( 13 ) = Y      ! (NH4)3H(SO4)2 is MIN (X,Y)
            CONCR(  9 ) = X - Y  ! NH4HSO4
         END IF

      ELSE IF ( SC .EQ. 'C2' ) THEN  ! sulfate rich (free acid), old M2, P2
         CONCR( 9 ) = CONC( 3 )                      ! NH4HSO4
         CONCR( 7 ) = MAX( CONC( 2 ) - CONC( 3 ), 0.0D0 )   ! H2SO4

      ELSE IF ( SC .EQ. 'N3' ) THEN    ! sulfate poor (NH4-SO4-NO3 system)
         CONCR( 4 ) = MIN ( CONC( 2 ), 0.5D0 * CONC( 3 ) )           ! (NH4)2SO4
         FNH4       = MAX ( CONC( 3 ) - 2.0D0 * CONCR( 4 ), 0.0D0 )  ! available NH4
         CONCR( 5 ) = MAX ( MIN ( FNH4, CONC( 4 ) ), 0.0D0 )         ! NH4NO3=MIN(NH4,NO3)

      ELSE IF ( SC .EQ. 'Q5' ) THEN    ! sulfate poor, sodium poor (NH4-SO4-NO3-Cl-Na)
         CONCR( 2 ) = 0.5D0 * CONC( 1 )                              ! Na2SO4
         FSO4       = MAX ( CONC( 2 ) - CONCR( 2 ), 0.0D0 )          ! available SO4
         CONCR( 4 ) = MAX ( MIN ( FSO4, 0.5D0 * CONC( 3 ) ), SMALL ) ! NH42S4=MIN(NH4,S4)
         FNH4       = MAX ( CONC( 3 ) - 2.0D0 * CONCR( 4 ), 0.0D0 )  ! available NH4
         CONCR( 5 ) = MIN ( FNH4, CONC( 4 ) )                        ! NH4NO3=MIN(NH4,NO3)
         FNH4       = MAX ( FNH4 - CONCR( 5 ), 0.0D0 )               ! avaialable NH4
         CONCR( 6 ) = MIN ( FNH4, CONC( 5 ) )                        ! NH4Cl=MIN(NH4,Cl)

      ELSE IF ( SC .EQ. 'R6' ) THEN   ! sulfate poor, sodium rich (NH4-SO4-NO3-Cl-Na)
         CONCR( 2 ) = CONC( 2 )                            ! Na2SO4
         FNA        = MAX ( CONC( 1 ) - 2.0D0 * CONCR( 2 ), 0.0D0 )

         CONCR( 3 ) = MIN ( FNA, CONC( 4 ) )               ! NaNO3
         FNO3       = MAX ( CONC( 4 ) - CONCR( 3 ), 0.0D0 )
         FNA        = MAX ( FNA - CONCR( 3 ), 0.0D0 )

         CONCR( 1 ) = MIN ( FNA, CONC( 5 ) )               ! NaCl
         FCL        = MAX ( CONC( 5 ) - CONCR( 1 ), 0.0D0 )
         FNA        = MAX ( FNA - CONCR( 1 ), 0.0D0 )

         CONCR( 5 ) = MIN ( FNO3, CONC( 3 ) )              ! NH4NO3
         FNO3       = MAX ( FNO3 - CONCR( 5 ), 0.0D0 )
         FNH4       = MAX ( CONC( 3 ) - CONCR( 5 ), 0.0D0 )

         CONCR( 6 ) = MIN (FCL, FNH4 )                     ! NH4Cl

      ELSE IF ( SC .EQ. 'I6' ) THEN   ! sulfate rich (no acid) (NH4-SO4-NO3-Cl-Na)
         CONCR(  2 ) = 0.5D0 * CONC( 1 )                          ! Na2SO4
         FSO4        = MAX ( CONC( 2 ) - CONCR( 2 ), 0.0D0 )
         CONCR( 13 ) = MIN ( CONC( 3 ) / 3.0D0, FSO4 / 2.0D0 )    ! (NH4)3H(SO4)2
         FSO4        = MAX ( FSO4 - 2.0D0 * CONCR( 13 ), 0.0D0 )
         FNH4        = MAX ( CONC( 3 ) - 3.0D0 * CONCR( 13 ), 0.0D0 )

         IF ( FSO4 .LE. SMALL ) THEN    ! reduce (NH4)3H(SO4)2, add (NH4)2SO4
            CONCR( 13 ) = MAX ( CONCR( 13 ) - FNH4, 0.0D0 )   ! (NH4)3H(SO4)2
            CONCR(  4 ) = 2.0D0 * FNH4                  ! (NH4)2SO4
         ELSE IF ( FNH4 .LE. SMALL ) THEN ! reduce (NH4)3H(SO4)2, add NH4HSO4
            CONCR(  9 ) = 3.0D0 * MIN ( FSO4, CONCR( 13 ) ) ! NH4HSO4
            CONCR( 13 ) = MAX ( CONCR( 13 ) - FSO4, 0.0D0 )
            IF ( CONCR( 2 ) .GT. SMALL ) THEN ! reduce Na2SO4, add NaHSO4
               FSO4        = MAX ( FSO4 - CONCR( 9 ) / 3.0D0, 0.0D0 )
               CONCR( 12 ) = 2.0D0 * FSO4                ! NaHSO4
               CONCR(  2 ) = MAX ( CONCR( 2 ) - FSO4, 0.0D0 )  ! Na2SO4
             END IF
         END IF

      ELSE IF ( SC .EQ. 'J3' ) THEN   ! sulfate rich (free acid) (NH4-SO4-NO3-Cl-Na)
         CONCR(  9 ) = CONC( 3 )                             ! NH4HSO4
         CONCR( 12 ) = CONC( 1 )                             ! NAHSO4
         CONCR(  7 ) = MAX ( CONC( 2 ) - CONC( 3 ) - CONC( 1 ), 0.0D0 ) ! H2SO4

      ! Crustal cases
      ELSE IF ( SC .EQ. 'V7' ) THEN  ! sulfate poor, sodium+crustal poor
         CASO4     = MIN ( CONC( 6 ), CONC( 2 ) )            ! CCASO4
         FSO4      = MAX ( CONC( 2 ) - CASO4, 0.0D0 )
         FCA       = MAX ( CONC( 6 ) - CASO4, 0.0D0 )

         CONCR( 17 ) = MIN ( 0.5D0 * CONC( 7 ), FSO4 )       ! CK2SO4
         FK          = MAX ( CONC( 7 ) - 2.D0 * CONCR( 17 ), 0.0D0 )
         FSO4        = MAX ( FSO4 - CONCR( 17 ), 0.0D0 )

         CONCR( 2 )  = MIN ( 0.5D0 * CONC( 1 ), FSO4 )       ! CNA2SO4
         FNA         = MAX ( CONC( 1 ) - 2.0D0 * CONCR( 2 ), 0.0D0 )
         FSO4        = MAX ( FSO4 - CONCR( 2 ), 0.0D0 )

         CONCR( 21 ) = MIN ( CONC( 8 ), FSO4 )               ! CMGSO4
         FMG         = MAX ( CONC( 8 ) - CONCR( 21 ), 0.0D0 )
         FSO4        = MAX ( FSO4 - CONCR( 21 ), 0.0D0 )

         CONCR( 4 )  = MAX ( MIN ( FSO4 , 0.5D0 * CONC( 3 ) ) , SMALL ) ! CNH42S4
         FNH4        = MAX ( CONC( 3 ) - 2.0D0 * CONCR( 4 ), 0.0D0 )

         CONCR( 5 )  = MIN ( FNH4, CONC( 4 ) )               ! CNH4NO3
         FNH4        = MAX ( FNH4 - CONCR( 5 ), 0.0D0 )

         CONCR( 6 )  = MIN ( FNH4, CONC( 5 ) )               ! CNH4CL

      ELSE IF ( SC .EQ. 'U8' ) THEN  ! sulfate poor, crustal+sodium rich, crustal poor
         CASO4       = MIN ( CONC( 6 ), CONC( 2 ) )          ! CCASO4
         FSO4        = MAX ( CONC( 2 ) - CASO4, 0.0D0 )
         FCA         = MAX ( CONC( 6 ) - CASO4, 0.0D0 )

         CONCR( 17 ) = MIN ( 0.5D0 * CONC( 7 ), FSO4 )       ! CK2SO4
         FK          = MAX ( CONC( 7 ) - 2.0D0 * CONCR( 17 ), 0.0D0 )
         FSO4        = MAX ( FSO4 - CONCR( 17 ), 0.0D0 )

         CONCR( 21 ) = MIN ( CONC( 8 ), FSO4 )               ! CMGSO4
         FMG         = MAX ( CONC( 8 ) - CONCR( 21 ), 0.0D0 )
         FSO4        = MAX ( FSO4 - CONCR( 21 ), 0.0D0 )

         CONCR( 2 )  = MAX ( FSO4, 0.0D0 )                   ! CNA2SO4
         FNA         = MAX ( CONC( 1 ) - 2.0D0 * CONCR( 2 ), 0.0D0 )

         CONCR( 3 )  = MIN ( FNA, CONC( 4 ) )                ! NaNO3
         FNO3        = MAX ( CONC( 4 ) - CONCR( 3 ), 0.0D0 )
         FNA         = MAX ( FNA - CONCR( 3 ), 0.0D0 )

         CONCR( 1 )  = MIN ( FNA, CONC( 5 ) )                ! NaCl
         FCL         = MAX ( CONC( 5 ) - CONCR( 1 ), 0.0D0 )
         FNA         = MAX ( FNA - CONCR( 1 ), 0.0D0 )

         CONCR( 5 )  = MIN ( FNO3, CONC( 3 ) )               ! NH4NO3
         FNO3        = MAX ( FNO3 - CONCR( 5 ), 0.0D0 )
         FNH4        = MAX ( CONC( 3 ) - CONCR( 5 ), 0.0D0 )

         CONCR( 6 )  = MIN ( FCL, FNH4 )                     ! NH4Cl
         FCL         = MAX ( FCL - CONCR( 6 ), 0.0D0 )
         FNH4        = MAX ( FNH4 - CONCR( 6 ), 0.0D0 )

      ELSE IF ( SC .EQ. 'W13' ) THEN  ! sulfate poor, crustal+sodium rich
         CASO4       = MIN ( CONC( 2 ), CONC( 6 ) )          ! CASO4
         FCA         = MAX ( CONC( 6 ) - CASO4, 0.0D0 )
         FSO4        = MAX ( CONC( 2 ) - CASO4, 0.0D0 )

         CONCR( 17 ) = MIN ( FSO4, 0.5D0 * CONC( 7 ) )       ! K2SO4
         FK          = MAX ( CONC( 7 ) - 2.0D0 * CONCR( 17 ), 0.0D0 )
         FSO4        = MAX ( FSO4 - CONCR( 17 ), 0.0D0 )

         CONCR( 21 ) = FSO4                                  ! MGSO4
         FMG         = MAX ( CONC( 8 ) - CONCR( 21 ), 0.0D0 )

         CONCR( 1 )  = MIN ( CONC( 1 ), CONC( 5 ) )          ! NACL
         FNA         = MAX ( CONC( 1 ) - CONCR( 1 ), 0.0D0 )
         FCL         = MAX ( CONC( 5 ) - CONCR( 1 ), 0.0D0 )

         CONCR( 16 ) = MIN ( FCA, 0.5D0 * FCL )              ! CACL2
         FCA         = MAX ( FCA - CONCR( 16 ), 0.0D0 )
         FCL         = MAX ( CONC( 5 ) - 2.0D0 * CONCR( 16 ), 0.0D0 )

         CONCR( 15 ) = MIN ( FCA, 0.5D0 * CONC( 4 ) )        ! CA(NO3)2
         FCA         = MAX ( FCA - CONCR( 15 ), 0.0D0 )
         FNO3        = MAX ( CONC( 4 ) - 2.0D0 * CONCR( 15 ), 0.0D0 )

         CONCR( 23 ) = MIN ( FMG, 0.5D0 * FCL )              ! MGCL2
         FMG         = MAX ( FMG - CONCR( 23 ), 0.0D0 )
         FCL         = MAX ( FCL - 2.0D0 * CONCR( 23 ), 0.0D0 )

         CONCR( 22 ) = MIN ( FMG, 0.5D0 * FNO3 )             ! MG(NO3)2
         FMG         = MAX ( FMG - CONCR( 22 ), 0.0D0 )
         FNO3        = MAX ( FNO3 - 2.0D0 * CONCR( 22 ), 0.0D0 )

         CONCR( 3 )  = MIN ( FNA, FNO3 )                     ! NANO3
         FNA         = MAX ( FNA - CONCR( 3 ), 0.0D0 )
         FNO3        = MAX ( FNO3 - CONCR( 3 ), 0.0D0 )

         CONCR( 20 ) = MIN ( FK, FCL )                       ! KCL
         FK          = MAX ( FK - CONCR( 20 ), 0.0D0 )
         FCL         = MAX ( FCL - CONCR( 20 ), 0.0D0 )

         CONCR( 19 ) = MIN ( FK, FNO3 )                      ! KNO3
         FK          = MAX ( FK - CONCR( 19 ), 0.0D0 )
         FNO3        = MAX ( FNO3 - CONCR( 19 ), 0.0D0 )

      ELSE IF ( SC .EQ. 'L9' ) THEN  ! sulfate rich, no free acid
         CASO4       = MIN ( CONC( 6 ), CONC( 2 ) )          ! CCASO4
         FSO4        = MAX ( CONC( 2 ) - CASO4, 0.0D0 )
         FCA         = MAX ( CONC( 6 ) - CASO4, 0.0D0 )

         CONCR( 17 ) = MIN ( 0.5D0 * CONC( 7 ), FSO4 )       ! CK2SO4
         FK          = MAX ( CONC( 7 ) - 2.0D0 * CONCR( 17 ), 0.0D0 )
         FSO4        = MAX ( FSO4 - CONCR( 17 ), 0.0D0 )

         CONCR( 2 )  = MIN ( 0.5D0 * CONC( 1 ), FSO4 )       ! CNA2SO4
         FNA         = MAX ( CONC( 1 ) - 2.0D0 * CONCR( 2 ), 0.0D0 )
         FSO4        = MAX ( FSO4 - CONCR( 2 ), 0.0D0 )

         CONCR( 21 ) = MIN ( CONC( 8 ), FSO4 )               ! CMGSO4
         FMG         = MAX ( CONC( 8 ) - CONCR( 21 ), 0.0D0 )
         FSO4        = MAX ( FSO4 - CONCR( 21 ), 0.0D0 )

         CONCR( 13 ) = MIN ( CONC( 3 ) / 3.0D0, FSO4 / 2.0D0 ) ! CLC
         FSO4        = MAX ( FSO4 - 2.0D0 * CONCR( 13 ), 0.0D0 )
         FNH4        = MAX ( CONC( 3 )- 3.0D0 * CONCR( 13 ),  0.0D0 )

         IF ( FSO4 .LE. SMALL ) THEN                           ! convert (NH4)3H(SO4)2 to (NH4)2SO4
            CONCR( 13 ) = MAX( CONCR( 13 ) - FNH4, 0.0D0 )
            CONCR(  4 ) = 2.0D0 * FNH4                         ! CNH42S4 

         ELSE IF ( FNH4 .LE. SMALL ) THEN                      ! convert (NH4)3H(SO4)2 to NH4HSO4
            CONCR(  9 ) = 3.0D0 * MIN( FSO4, CONCR( 13 ) )     ! CNH4HS4
            CONCR( 13 ) = MAX( CONCR( 13 ) - FSO4, 0.0D0 )     ! CLC, (NH4)3H(SO4)2
            IF ( CONCR( 2 ) .GT. SMALL ) THEN                  ! convert Na2SO4 to NaHSO4
               FSO4        = MAX( FSO4 - CONCR( 9 ) / 3.0D0, 0.0D0 )
               CONCR( 12 ) = 2.0D0 * FSO4                      ! CNAHSO4
               CONCR(  2 ) = MAX( CONCR( 2 ) - FSO4, 0.0D0 )   ! CNA2SO4
            END IF
            IF ( CONCR( 17 ) .GT. SMALL ) THEN                 ! convert K2SO4 to KHSO4
               FSO4        = MAX( FSO4 - CONCR( 9 ) / 3.0D0, 0.0D0 )
               CONCR( 18 ) = 2.0D0 * FSO4                      ! CKHSO4
               CONCR( 17 ) = MAX( CONCR( 17 ) - FSO4, 0.0D0 )  ! CK2SO4
            END IF
         END IF
         
      ELSE IF ( SC .EQ. 'K4' ) THEN ! sulfate super rich, free acid
         CONCR(  9 ) = CONC( 3 )                               ! NH4HSO4 = NH3
         CONCR( 12 ) = CONC( 1 )                               ! NaHSO4  = Na
         CONCR( 18 ) = CONC( 7 )                               ! KHSO4   = K
         CONCR( 21 ) = CONC( 8 )                               ! MgSO4   = Mg
         CONCR(  7 ) = MAX( CONC( 2 ) - CONC( 3 ) - CONC( 1 )
     &                    - CONC( 6 ) - CONC( 7 ) - CONC( 8 ), 0.0D0 ) ! H2SO4 = SO4 - NH4 - Na - Ca - K - Mg

      ELSE
         PRINT*, 'aero_subs.f: case not supported ',
     &          '(metastable reverse only)'
      END IF

C     Get single-solute molalities for ZSR calculation
      CALL GETM0I ( RH, M0I )

C     Calculate H2O with ZSR and determine delta water
      WATER = 0.0D0
      DO J = 1, NPAIR
         WATER = WATER + CONCR( J ) / M0I( J )
      END DO

      WATER = MAX ( WATER, SMALL )
      H2O_NEW = WATER / Mw

      END SUBROUTINE CALC_H2O  

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE GETM0I ( RHIN, M0I )

!!!!!!!!!!!!!!! We want to get rid of this dependency !!!!!!!!!!!!!!!!

C Description
C   Determines single-solute molalities for the 13 possible salts at
C   the ambient RH.  These molalities are used in the ZSR calculation
C   in CALC_H2O. Note that the molalities were determined at the beginning
C   of the time step, and so they are available in the IONS common block
C   of isrpia.inc.
C   Routine called by CALC_H2O.

C Revision History
C   ??? ???? Prakash Bhave(?), Jim Kelly(?): initial revision
C   Apr 2011 Havala Pye: Removed use of IONS common block since it requires
C                        ISORROPIA to be called to setup the initial array values.
C                        Now uses single-solute molalities from ZSR common block
C                        in isrpia.inc that is defined in BLKISO in isocom.f
C                        BE VERY CAREFUL ABOUT IMPLICIT VARIABLES HERE!
 
C Arguments
C   Output
C     M0I : Single-solute molalities (mol/kg-H2O) for 13 salts
 
C-----------------------------------------------------------------------

!     implicit none

      INCLUDE 'isrpia.inc'

C Arguments
      REAL( 8 ), INTENT( IN )  :: RHIN
      REAL( 8 ), INTENT( OUT ) :: M0I( NPAIR )

      INTEGER IZ

C Location in pure molality array (function of RH)
      IZ = MIN( INT( RHIN * REAL( NZSR, 8 ) + 0.5D0 ), NZSR )
      IZ = MAX( IZ, 1 )

C Default value
      M0I = 1.0D+5   ! array assignment

C Actual values (10,11 not provided)
      M0I( 01 ) = AWSC( IZ )   ! NACl
      M0I( 02 ) = AWSS( IZ )   ! (NA)2SO4
      M0I( 03 ) = AWSN( IZ )   ! NANO3
      M0I( 04 ) = AWAS( IZ )   ! (NH4)2SO4
      M0I( 05 ) = AWAN( IZ )   ! NH4NO3
      M0I( 06 ) = AWAC( IZ )   ! NH4CL
      M0I( 07 ) = AWSA( IZ )   ! 2H-SO4
      M0I( 08 ) = AWSA( IZ )   ! H-HSO4
      M0I( 09 ) = AWAB( IZ )   ! NH4HSO4


      M0I( 12 ) = AWSB( IZ )   ! NAHSO4
      M0I( 13 ) = AWLC( IZ )   ! (NH4)3H(SO4)2

      M0I( 15 ) = AWCN( IZ )   ! CA(NO3)2
      M0I( 16 ) = AWCC( IZ )   ! CACl2
      M0I( 17 ) = AWPS( IZ )   ! K2SO4
      M0I( 18 ) = AWPB( IZ )   ! KHSO4
      M0I( 19 ) = AWPN( IZ )   ! KNO3
      M0I( 20 ) = AWPC( IZ )   ! KCl
      M0I( 21 ) = AWMS( IZ )   ! MGSO4
      M0I( 22 ) = AWMN( IZ )   ! MG(NO3)2
      M0I( 23 ) = AWMC( IZ )   ! MGCL2
    
      END SUBROUTINE GETM0I               

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE GETSC ( CONC, RH, T, SC )
 
C Description
C   Determines the sub-case to use for water uptake calculations.
C   Follows the procedure of ISORROPIA.
C   Routine called by CALC_H2O.
 
C ArgumentS
C   Inputs
C     CONC : Concentration [mol/m^3] of aerosol components. This routine
C            sets minimum CONC to 1.0D-20
C     RH   : Relative humidity
C     T    : Temperature (K)
     
C   Output
C     SC   : Sub-case for aerosol composition
 
C-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER, PARAMETER :: NCMP = 8    ! was NCMP  = 5    ! number of aerosol components
      REAL( 8 ), PARAMETER :: SMALL = 1.0D-20

C Arguments:
!     REAL( 8 ), INTENT( IN )    :: CONC(  NCMP )
      REAL( 8 ), INTENT( INOUT ) :: CONC(  NCMP )
      REAL( 8 ), INTENT( IN )    :: RH, T
      CHARACTER( 3 ), INTENT( OUT ) :: SC
            
C Local Variables:
      REAL( 8 ) :: T0, TCF                     ! DRH(T) factor
      REAL( 8 ) :: S4RAT, S4RATW, NaRAT, SRI   ! sulfate & sodium ratios
      REAL( 8 ) :: CRAT                        ! crustals ratio
      REAL( 8 ) :: FSO4                        ! "free" sulfate
      REAL( 8 ) :: DNACL, DNH4CL, DNANO3, DNH4NO3, DNH42S4 ! DRH values

      REAL( 8 ) :: GETASR    ! ISORROPIA function for sulfate ratio

      LOGICAL :: SCP1R, SCP2R, SCP3R, SCP4R ! concentration regime

C-----------------------------------------------------------------------

      SCP1R = .FALSE.
      SCP2R = .FALSE.
      SCP3R = .FALSE.
      SCP4R = .FALSE.

C     See if any components are negligible (see isocom.for)
      IF ( CONC( 1 ) + CONC( 4 ) + CONC( 5 ) + 
     &     CONC( 6 ) + CONC( 7 ) + CONC( 8 ) .LE. SMALL ) THEN       ! Ca,K,Mg,Na,Cl,NO3=0
         SCP1R = .TRUE.                                    
      ELSE IF ( CONC( 1 ) +        CONC( 5 ) +
     &          CONC( 6 ) + CONC( 7 ) + CONC( 8 ) .LE. SMALL ) THEN  ! Ca,K,Mg,Na,Cl=0
         SCP2R = .TRUE.                                     
      ELSE IF ( CONC( 6 ) + CONC( 7 ) + CONC( 8 ) .LE. SMALL ) THEN  ! Ca,K,Mg=0
         SCP3R = .TRUE.                                     
      ELSE                                                           ! all species
         SCP4R = .TRUE.
      END IF

      CONC( : ) = MAX ( CONC( : ), SMALL )

C     Deliquescence RH calculations
      DNH42S4 = 0.7997D0
      DNH4NO3 = 0.6183D0
      IF ( INT( T ) .NE. 298 ) THEN
         T0      = 298.15D0
         TCF     = 1.0D0 / T - 1.0D0 / T0
         DNH4NO3 = DNH4NO3 * EXP( 852.0D0 * TCF )
         DNH42S4 = DNH42S4 * EXP(  80.0D0 * TCF )
         DNH4NO3 = MIN ( DNH4NO3, DNH42S4 ) ! adjust for curves crossing T<271K
      END IF

C     Find sub-case "SC"
      IF ( SCP1R ) THEN ! NH4-S04 system

         IF ( RH .GE. DNH42S4 ) THEN
            S4RATW = GETASR( CONC( 2 ), RH ) ! aerosol sulfate ratio
         ELSE
            S4RATW = 2.0D0                ! dry aerosol sulfate ratio
         END IF
         S4RAT  = CONC( 3 ) / CONC( 2 )     ! sulfate ratio (NH4/SO4)

         IF ( S4RATW .LE. S4RAT ) THEN      ! sulfate poor
            SC = 'S2'
         ELSE IF ( 1.0D0 .LE. S4RAT .AND. S4RAT .LT. S4RATW ) THEN ! sulfate rich (no acid)
            SC = 'B4'
         ELSE IF ( S4RAT .LT. 1.0D0 ) THEN   ! sulfate rich (free acid)
            SC = 'C2'
         END IF

      ELSE IF ( SCP2R ) THEN ! NH4-SO4-NO3 system

         IF ( RH .GE. DNH4NO3 ) THEN
            S4RATW = GETASR( CONC( 2 ), RH )
         ELSE
            S4RATW = 2.0D0               ! dry aerosol ratio
         END IF
         S4RAT = CONC( 3 ) / CONC( 2 )

         IF ( S4RATW .LE. S4RAT ) THEN     ! sulfate poor
            SC = 'N3'
         ELSE IF ( 1.0D0 .LE. S4RAT .AND. S4RAT .LT. S4RATW ) THEN  ! sulfate rich (no acid)
            SC = 'B4'
         ELSE IF ( S4RAT .LT. 1.0D0 ) THEN    ! sulfate rich (free acid)
            SC = 'C2'
         END IF

      ELSE IF ( SCP3R )  THEN ! NH4-SO4-NO3-Na-Cl system

C        Adjust DRH of NH4NO3 for low temperature
         DNACL  = 0.7528D0
         DNANO3 = 0.7379D0
         DNH4CL = 0.7710D0
         IF ( INT( T ) .NE. 298 ) THEN
            DNACL   = DNACL  * EXP(  25.0D0 * TCF )
            DNANO3  = DNANO3 * EXP( 304.0D0 * TCF )
            DNH4CL  = DNH4Cl * EXP( 239.0D0 * TCF )
            DNH4NO3 = MIN ( DNH4NO3, DNH4CL, DNANO3, DNACL )
         END IF

         IF ( RH .GE. DNH4NO3 ) THEN
            FSO4   = CONC( 2 ) - CONC( 1 ) / 2.0D0   ! sulfate unbound by Na+
            FSO4   = MAX ( FSO4, SMALL )
            SRI    = GETASR ( FSO4, RH )
            S4RATW = ( CONC( 1 ) + FSO4 * SRI ) / CONC( 2 )
            S4RATW = MIN ( S4RATW, 2.0D0 )
         ELSE
            S4RATW = 2.0D0                       ! ratio for dry aerosol
         END IF
         S4RAT = ( CONC( 1 ) + CONC( 3 ) ) / CONC( 2 )
         NaRAT = CONC( 1 ) / CONC( 2 )

         IF ( S4RATW .LE. S4RAT .AND. NaRAT .LT. 2.0D0 ) THEN ! sulfate poor, sodium poor
            SC = 'Q5'
         ELSE IF ( S4RAT .GE. S4RATW .AND. NaRAT .GE. 2.0D0 ) THEN ! SO4 poor, Na rich
            SC = 'R6'
         ELSE IF ( 1.0D0 .LE. S4RAT .AND. S4RAT .LT. S4RATW ) THEN ! SO4 rich, no acid
            SC = 'I6'
         ELSE IF ( S4RAT .LT. 1.0D0 ) THEN ! sulfate rich, free acid
            SC = 'J3'
         END IF

      ELSE IF ( SCP4R ) THEN ! NH4-SO4-Na-Cl-Ca-K-Mg system

         ! Do I need an RH if check here????
         FSO4   = CONC( 2 ) - CONC( 1 ) / 2.0D0
     &          - CONC( 6 ) - CONC( 7 ) / 2.0D0 - CONC( 8 )  ! sulfate unbound by sodium,calcium,pottasium,magnesium
         FSO4   = MAX ( FSO4, SMALL )
         SRI    = GETASR( FSO4, RH )                         ! sulfate ratio for NH4+
         S4RATW = ( CONC( 1 ) + FSO4 * SRI + CONC( 6 )
     &            + CONC( 7 ) + CONC( 8 ) ) / CONC( 2 )      ! limiting sulfate ratio
         S4RATW = MIN ( S4RATW, 2.0D0 )
         S4RAT = ( CONC( 1 ) + CONC( 3 ) + CONC( 6 ) + CONC( 7 ) + CONC( 8 ) ) / CONC( 2 ) ! sulfate ratio
         NaRAT = ( CONC( 1 ) + CONC( 6 ) + CONC( 7 ) + CONC( 8 ) ) / CONC( 2 ) ! crustals+sodium ratio
         CRAT  = ( CONC( 6 ) + CONC( 7 ) + CONC( 8 ) ) / CONC( 2 )             ! crustals ratio

         IF ( S4RATW .LE. S4RAT .AND. NaRAT .LT. 2.0D0 ) THEN ! sulfate, sodium, crustal poor
            SC = 'V7'
         ELSE IF ( S4RAT .GE. S4RATW .AND. NaRAT .GE. 2.0D0 ) THEN
            IF ( CRAT .LE. 2.0D0 ) THEN       ! sulfate poor, dust+sodium rich, dust poor
               SC = 'U8'
            ELSE                              ! sulfate poor, dust+sodium rich, dust rich
               SC = 'W13'
            END IF
         ELSE IF ( 1.0D0 .LE. S4RAT .AND. S4RAT .LT. S4RATW ) THEN ! sulfate rich, no acid
            SC = 'L9'
         ELSE IF ( S4RAT .LT. 1.0D0 ) THEN     ! sulfate rich, free acid
            SC = 'K4'
         END IF
      END IF

      !print*,'SUBCASE identified in calc_h2o', SC

      END SUBROUTINE GETSC

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      REAL FUNCTION GETAF( NI, NJ, DGNI, DGNJ, XLSGI, XLSGJ, SQRT2 )

C  Returns the value of "Xnum" in Equations 10a and 10c
C  of Binkowski and Roselle (2003), given the number concentrations,
C  median diameters, and natural logs of the geometric standard
C  deviations, in two lognormal modes.  The value returned by GETAF
C  is used subsequently in the mode merging calculations:
C       GETAF = ln( Dij / Dgi ) / ( SQRT2 * ln(Sgi) )
C  where Dij is the diameter of intersection,
C        Dgi is the median diameter of the smaller size mode, and
C        Sgi is the geometric standard deviation of smaller mode.
C  A quadratic equation is solved to obtain GETAF, following the
C  method of Press et al.
C 
C  References:
C   1. Binkowski, F.S. and S.J. Roselle, Models-3 Community
C      Multiscale Air Quality (CMAQ) model aerosol component 1:
C      Model Description.  J. Geophys. Res., Vol 108, No D6, 4183
C      doi:10.1029/2001JD001409, 2003.
C   2. Press, W.H., S.A. Teukolsky, W.T. Vetterling, and B.P.
C      Flannery, Numerical Recipes in Fortran 77 - 2nd Edition.
C      Cambridge University Press, 1992.

      IMPLICIT NONE

      REAL NI, NJ, DGNI, DGNJ, XLSGI, XLSGJ, SQRT2
      REAL AA, BB, CC, DISC, QQ, ALFA, L, YJI

C-----------------------------------------------------------------------

C *** Store intermediate values used for the quadratic solution
C     to reduce computational burden
      ALFA = XLSGI / XLSGJ
      YJI = LOG( DGNJ / DGNI ) / ( SQRT2 * XLSGI )
      L = LOG( ALFA * NJ / NI)

C *** Calculate quadratic equation coefficients & discriminant
      AA = 1.0 - ALFA * ALFA
      BB = 2.0 * YJI * ALFA * ALFA
      CC = L - YJI * YJI * ALFA * ALFA
      DISC = BB * BB - 4.0 * AA * CC

C *** If roots are imaginary, return a negative GETAF value so that no
C     mode merging takes place.
      IF ( DISC .LT. 0.0 ) THEN
         GETAF = - 5.0       ! error in intersection
         RETURN
      END IF

C *** Equation 5.6.4 of Press et al.
      QQ = -0.5 * ( BB + SIGN( 1.0, BB ) * SQRT( DISC ) )

C *** Return solution of the quadratic equation that corresponds to a
C     diameter of intersection lying between the median diameters of
C     the 2 modes.
      GETAF = CC / QQ       ! See Equation 5.6.5 of Press et al.

      RETURN
      END FUNCTION GETAF
 
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE AERO_INLET ( DGN, XXLSG, RHOP, D_AD1, D_AD2, fPM )

C  Calculates the volume fraction of a given aerosol mode that would enter
C  a sharp-cut inlet, using equations from Jiang et al (2006). The
C  parameters D_AD1 and D_AD2 provide the lower and upper cutoff diameters
C  for the inlet size cut and the routine returns the fraction of the
C  size distribution that is transmitted through the hypothetical inlet
C  with the variable fPM. But first, those cutoffs are converted from 
C  Aerodynamic Diameter to Stokes Diameter (used internally within CMAQ).
 
C  CMAQ aerosols are represented with Stokes Diameter. There are many
C  alternate forms of diameter including the following:
C       Volume Equivalent Diameter (D_ve) - the diameter of a sphere with the
C          same volume as the particle.
C       Stokes Diameter (D_st) - the diameter of a sphere with the same
C          terminal velocity as the real particle. If the shape factor
C          is 1, then the Stokes and Volume Equivalent Diameters are
C          equal.
C       Aerodynamic Diameter (D_ad) - the diameter of a sphere with unit
C          density and the same terminal velocity as the real particle.
C          If the density is 1, then the Aerodynamic Diameter and Stokes
C          Diameter are equal.
C       Vacuum Aerodynamic Diameter (D_va) - the diameter of a sphere with very
C          high Knudsen number and thus a simplified relationship with
C          Stokes diameter that is only dependent on density and shape
C          factor, not Slip Correction. The very high Knudsen number
C          could be because the particle is very small or because the
C          system pressure is very low.
C       Electrical Mobility Diameter (D_em) - the diameter of a sphere with the
C          same migration velocity as the real particle. Requires a
C          slip-correction and a shape factor correction when converting
C          from volume equivalent diameter.
 
C  Conversions:
C       Parameters
C         rho  - Density
C         rho0 - Unit Density (1 g cm-3)
C         Cc( ) - Cunningham Slip Correction of a particular diameter
C         X   - Shape factor = (NonSpherical Drag Force)/(Spherical Drag Force)
C       Aerodynamic -> Stokes
C         D_st = D_ad * sqrt(rho0/rho) * sqrt( Cc(D_ad) / Cc(D_st) )
C       Volume Equivalent -> Stokes
C         D_st = D_ve * sqrt(1/X) * sqrt( Cc(D_ve) / Cc(D_st) )
C       Electrical Mobility -> Volume Equivalent
C         D_ve = D_em * [Cc(D_ve) / Cc(D_em)] * 1/X
C             (If X = 1 (i.e. spherical), then D_st = D_ve = D_em )
C       Vacuum Aerodynamic -> Volume Equivalent (DeCarlo et al., 2004)
C         D_ve = D_va * rho0/rho * X
C             (If X = 1 (i.e. spherical), then D_st = D_ve = D_va * rho0/rho )
 
C  Key Subroutines called: none
 
C  Key Functions called:  ERF
 
C  Revision History:
C    Coded Jul 2005 by Prakash Bhave
C          Apr 2008 J.Kelly: corrected equation for Dst25 calculation
C          Feb 2016 B.Murphy: modified to output PM1 and PM10 mode
C                             fractions in addition to PM2.5
 
C  References:
C   1. Jiang, W., Smyth, S., Giroux, E., Roth, H., Yin, D., Differences
C   between CMAQ fine mode particle and PM2.5 concentrations and their
C   impact on model performance evaluation in the Lower Fraser Valley,
C   Atmos. Environ., 40:4973-4985, 2006.
C   2. Meng, Z., Seinfeld, J.H., On the source of the submicrometer
C   droplet mode of urban and regional aerosols, Aerosol Sci. and
C   Technology, 20:253-265, 1994.
 
C-----------------------------------------------------------------------

      IMPLICIT NONE

      INCLUDE SUBST_CONST    ! for PI

C    Input variables
      REAL, INTENT( IN ) :: DGN     ! geometric mean Stokes diameter by number [ m ]
      REAL, INTENT( IN ) :: XXLSG   ! natural log of geometric standard deviation
      REAL, INTENT( IN ) :: RHOP    ! average particle density [ kg/m**3 ]
      REAL, INTENT( IN ) :: D_AD1   ! Lower aerodynamic diameter cut point [ um ]
      REAL, INTENT( IN ) :: D_AD2   ! Upper aerodynamic diameter cut point [ um ]

C    Output variable
      REAL, INTENT( OUT ) :: fPM   ! fraction of particulate volume transmitted through 
                                   ! hypothetical inlet.

C    Internal variables
      REAL, PARAMETER :: SQRT2 = 1.4142136  !  SQRT( 2 )
      REAL, PARAMETER :: B = 0.21470 ! Cunningham slip-correction approx. param [ um ]
                                     ! This factor works well applied to the entire particle size-range
                                     ! The approximation is: Cc(Dp) = 1 + B/Dp
      REAL D_ST1, D_ST2              ! Stokes diameter equivalent of DCA..
      REAL DG                        ! DGN converted to [ um ]
      REAL ERF1, ERF2, ERFARG        ! argument of ERF, from Step#6 of Jiang et al. (2006)

C *** Error function approximation, from Meng & Seinfeld (1994)
      REAL ERF        ! Error function
      REAL XX         ! dummy argument for ERF
      ERF( XX )  = SIGN( 1.0, XX ) * SQRT( 1.0 - EXP( -4.0 * XX * XX / PI ) )

C ----------------------- Begin solution -------------------------------

C *** Calculate Transmission Fractions for Inlets with Aerodynamic
C     Cutoffs
      DG = DGN  ! [um] The units need to be equivalent with the B parameter

      ! from Meng & Seinfeld (1994) and modified form of Fk(X) equation
      ! in Step#6 of Jiang et al. (2006).
      IF ( D_AD1 .EQ. 0.0 ) THEN
          ERF1 = -1.0
      ELSE
          ! Convert size cut to equivalent Stokes diameter using
          ! equation 2 of Jiang et al. (2006). Note: the equation in Step 5
          ! of this paper has a typo (i.e., eq. 2 is correct).
          D_ST1  = 0.5 * ( SQRT( B ** 2 + 4.0 * D_AD1 *
     &                        ( D_AD1 + B ) * 1.0E+03 / RHOP ) - B )
          ! Calculate mass fraction with Dca < SizeCut, using ERF approximation
          ERFARG = ( LOG( D_ST1 ) - LOG( DG ) ) / ( SQRT2 * XXLSG ) - 3.0 * XXLSG / SQRT2
          ERF1 = ERF( ERFARG )
      END IF

      ! Convert size cut to equivalent Stokes diameter using
      ! equation 2 of Jiang et al. (2006). Note: the equation in Step 5
      ! of this paper has a typo (i.e., eq. 2 is correct).
      D_ST2  = 0.5 * ( SQRT( B ** 2 + 4.0 * D_AD2 *
     &                     ( D_AD2 + B ) * 1.0E+03 / RHOP ) - B )

      ! Calculate mass fraction with Dca < SizeCut, using ERF approximation
      ERFARG = ( LOG( D_ST2 ) - LOG( DG ) ) / ( SQRT2 * XXLSG ) - 3.0 * XXLSG / SQRT2
      ERF2 = ERF( ERFARG )

      ! Calculate fraction between Lower and Upper Cut Points
      fPM = 0.5 * ( ERF2 - ERF1 )

      END SUBROUTINE AERO_INLET

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE AERO_AMS ( M3_WET, M2_WET, M0, M_H2O, RHOP, RHO_H2O, fAMS )

C  The subroutine calculates transmission factors applicable for
C  comparison with AMS measurements using the curve suggested by Jimenez
C  and coworkers  (http://cires1.colorado.edu/jimenez-group/wiki/index.php/
C  FAQ_for_AMS_Data_Users#What_is_the_size_cut_of_AMS_measurements.3F).
C  The AMS measures Vacuum Aerodynamic Diameter (D_va) because the pressure is
C  so low in the instrument. This curve (also described in Ensberg et
C  al., 2013)  includes the following (Note: before applying the curve,
C  the bounds must be converted to Stokes Diameter):
C     D_va < 0.04 um: 0% transmission (T)
C     0.04 < D_va < 0.1 um:  T% = 1666.6 * (D_va - 0.04)   (1)
C     0.1  < D_va < 0.55 um: T% = 100%                     (2)
C     0.55 < D_va < 2.0 um:  T% = 100-68.965*(D_va-0.55)   (3)
C     2.0 um < D_va: T = 0%
 
C  CMAQ aerosols are represented with Stokes Diameter. There are many
C  alternate froms of diameter including the following:
C       Volume Equivalent Diameter (D_ve) - the diameter of a sphere with the
C          same volume as the particle.
C       Stokes Diameter (D_st) - the diameter of a sphere with the same
C          terminal velocity as the real particle. If the shape factor
C          is 1, then the Stokes and Volume Equivalent Diameters are
C          equal.
C       Vacuum Aerodynamic Diameter (D_va) - the diameter of a sphere with very
C          high Knudsen number and thus a simplified relationship with
C          Stokes diameter that is only dependent on density and shape
C          factor, not Slip Correction. The very high Knudsen number
C          could be because the particle is very small or because the
C          system pressure is very low.
 
C  Conversions:
C       Parameters
C         ro  - Density
C         ro0 - Unit Density (1 g cm-3)
C         Cc( ) - Cunningham Slip Correction of a particular diameter
C         X   - Shape factor = (NonSpherical Drag Force)/(Spherical Drag Force)
C       Vacuum Aerodynamic -> Volume Equivalent (DeCarlo et al., 2004)
C         D_ve = D_va * ro0/ro * X
C             (If X = 1 (i.e. spherical), then D_st = D_ve = D_va * ro0/ro )
 
C  Key Functions called:  ERF
 
C  Revision History:
C    Coded Feb 2016 B.Murphy: Created

C  References:
C   1. DeCarlo et al., Particle Morphology and Density Characterization
C   by Combined Mobility and Aerodynamic Diameter Measurements. Part 1:
C   Theory, Aerosol Sci. and Technology, 38:1185-1205, 2004
C   2. Ensberg et al., Inorganic and black carbon aerosols in the Los
C   Angeles Basin during CalNex, Journ. Geophys. Res., 2013.

C-----------------------------------------------------------------------

      USE AERO_DATA, ONLY : MIN_SIGMA_G, MAX_SIGMA_G
      USE AEROMET_DATA, ONLY : F6PI, PI6

      IMPLICIT NONE

      INCLUDE SUBST_CONST    ! for PI

C    Input variables
      REAL, INTENT( IN ) :: M3_WET  ! Third Moment of Wet Distribution (m3/m3)
      REAL, INTENT( IN ) :: M2_WET  ! Second Moment of Wet Distribution (m2/m3)
      REAL, INTENT( IN ) :: M0      ! Number of Particles in Distribution (N/m3)
      REAL, INTENT( IN ) :: M_H2O   ! Mass Conc. of Water in Particles (ug/m3)
      REAL, INTENT( IN ) :: RHOP    ! average particle density [ kg/m**3 ]
      REAL, INTENT( IN ) :: RHO_H2O ! Water Density [ kg/m3 ]

C    Output variable
      REAL, INTENT(OUT) :: fAMS   ! fraction of particulate volume transmitted through AMS Inlet

C    Internal variables
      REAL, PARAMETER :: SQRT2 = 1.4142136    !  SQRT( 2 )
      REAL, PARAMETER :: DGMIN = 1.0E-9       !  min(Dp) in [m]
      REAL, PARAMETER :: ONETHIRD = 1.0 / 3.0
      REAL, PARAMETER :: TWOTHIRDS = 2.0 * ONETHIRD

      REAL DG                        ! DGN converted to [ um ]
      REAL DGv                       ! Volume Median Diameter
      REAL XXLSG                     ! ln(StndDev) for current mode
      REAL M3_DRY, M2_DRY, M3SUBT, M_WET, M_DRY, DRY_DENS, DENSFAC
      REAL XFSUM, LXFM2, L2SG, ES36
      REAL DBlo_st, DBhi_st, LOG_HILO, LOG_LOHI, LOG_HI, LOG_LO
      REAL SQRT2LSG, TERM1, TERM2, TERM3, TERM4, TERM5

C *** Error function approximation, from Meng & Seinfeld (1994)
      REAL ERF        ! Error function
      REAL XX         ! dummy argument for ERF
      ERF( XX )  = SIGN( 1.0, XX ) * SQRT( 1.0 - EXP( -4.0 * XX * XX / PI ) )

C ----------------------- Begin solution -------------------------------

C *** First Calculate Parameters of Dry Distribution since this is more
C     applicable in general to AMS measurements.
      M3SUBT = ( 1.0E-9 * F6PI / RHO_H2O ) * M_H2O        ! m3 m-3
      M3_DRY = Max(M3_WET - M3SUBT, 0.0) + TINY(0.0)   ! m3 m-3 
      M2_DRY = M2_WET * ( M3_DRY / M3_WET ) ** TWOTHIRDS  ! m2 m-3

      M_WET = M3_WET * 1.0E+9 * PI6 * RHOP ! ug m-3
      M_DRY = Max(M_WET - M_H2O, 0.0) + TINY(0.0) ! m3 m-3 

      DRY_DENS = M_DRY / M3_DRY * F6PI * 1.0E-9   ! kg m-3

      XFSUM = ONETHIRD * Log( M0 ) + TWOTHIRDS * Log( M3_DRY )
      LXFM2 = Log( M2_DRY )
      L2SG = XFSUM - LXFM2   ! ( ln(sigma) )^2

      L2SG = Min( Max( L2SG, LOG( MIN_SIGMA_G ) ** 2 ), LOG( MAX_SIGMA_G ) ** 2 )
      LXFM2 = XFSUM - L2SG
      ES36 = Exp( 4.5 * L2SG )

      DG = Max( DGMIN, ( M3_DRY / ( M0 * ES36 ) ) ** ONETHIRD ) * 1.0E+06 ![um] Units should correspond to D
      XXLSG = Sqrt( L2SG )  ! ln(sigma)

      DGv = EXP( LOG( DG ) + 3.0 * L2SG )

C *** Calculate Transmission Fractions for AMS with Vacuum Aerodynamic
C     Cutoffs. This approximation is split into a piecewise function (see
C     Appendix B in Ensberg et al., 2013).
      fAMS = 0.0
      DENSFAC = 1.0E+03 / DRY_DENS  ! Density Correction Factor
                                    !   = rho0 / rho
                                    !   = 1000.0 / rho
      SQRT2LSG = SQRT2 * XXLSG

      ! First Piece of Function [T(%) = 1666.6 * (Dva - 0.04) ]
      DBlo_st = 0.040 * DENSFAC  ! Stokes Lower Bound of Piece [um]
      DBhi_st = 0.100 * DENSFAC  ! Stokes Upper Bound of Piece [um]

      LOG_HILO = LOG( 100.0/40.0 )
      LOG_HI   = LOG( DBhi_st/DGv )
      LOG_LO   = LOG( DBlo_st/DGv )

      TERM1 = LOG( DGv/DBlo_st ) / LOG_HILO
      TERM2 =  ERF( LOG_HI / SQRT2LSG )
     &        -ERF( LOG_LO / SQRT2LSG )
      TERM3 = XXLSG / ( LOG_HILO * (pi/2.0) ** 0.5 )
      TERM4 = EXP( -1.0 * ( LOG_LO / SQRT2LSG ) ** 2 )
      TERM5 = EXP( -1.0 * ( LOG_HI / SQRT2LSG ) ** 2 )

      fAMS = ( TERM1*TERM2  +  TERM3*(TERM4-TERM5) )

      ! Second Piece of Function [T(%) = 100]
      DBlo_st = DBhi_st         ! Stokes Lower Bound [um]
      DBhi_st = 0.55 * DENSFAC  ! Stokes Upper Bound [um]

      TERM1 = ERF( LOG( DBhi_st/DGv ) / SQRT2LSG )
      TERM2 = ERF( LOG( DBlo_st/DGv ) / SQRT2LSG )
      fAMS = fAMS +  (TERM1 - TERM2)

      ! Third Piece of Function [T(%) = 1.0 - 0.6805 * (Dva - 0.55) ]
      DBlo_st = DBhi_st        ! Stokes Lower Bound [um]
      DBhi_st = 2.0 * DENSFAC  ! Stokes Upper Bound [um]

      LOG_LOHI = LOG( 550.0/2000.0 )
      LOG_HI   = LOG( DBhi_st/DGv )
      LOG_LO   = LOG( DBlo_st/DGv )

      TERM1 = LOG( DGv/DBhi_st ) / LOG_LOHI
      TERM2 =  ERF( LOG_HI / SQRT2LSG )
     &        -ERF( LOG_LO / SQRT2LSG )
      TERM3 = XXLSG / ( LOG_LOHI * (pi/2.0) ** 0.5 )
      TERM4 = EXP( -1.0 * ( LOG_LO / SQRT2LSG ) ** 2 )
      TERM5 = EXP( -1.0 * ( LOG_HI / SQRT2LSG ) ** 2 )

      fAMS = fAMS + TERM1 * TERM2  +  TERM3 * (TERM4-TERM5)

      ! Apply the Factor of 0.5 Consistent with Appendix B in Ensberg et
      ! al., 2013. Omit the total mass quantity since these are fractions
      ! we want.
      fAMS = 0.5 * fAMS

      END SUBROUTINE AERO_AMS

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      SUBROUTINE HETCHEM_EXTRACT_AERO ( CONCVEC, C, R, L, WET_M2, WET_M3, 
     &                                  DE_WET )

c  Calculates the change in aerosol surface area due to heterogeneous
C  reactions and updates the CGRID array with this information.
  
C  History
C  May 2016: BM Created

C  Key Subroutines Called: none
C
C  Called by: Gas-Phase Chemistry Driver
      USE AERO_DATA, ONLY: EXTRACT_AERO, CALCMOMENTS, N_MODE, MOMENT2_CONC,
     &                     MOMENT3_CONC, AEROMODE_DIAM, AEROMODE_LNSG,
     &                     CHEM_M2DRY_INIT, CHEM_M3DRY_INIT, FIXED_SG
      USE AEROMET_DATA, ONLY: pi
      USE GRID_CONF, ONLY: NLAYS, NCOLS, NROWS
      USE PRECURSOR_DATA, ONLY: EXTRACT_PRECURSOR

#ifdef sens
      USE DDM3D_DEFN, ONLY: SENGRID

#endif
      IMPLICIT NONE
      
      REAL, INTENT( IN ) :: CONCVEC ( : )  ! pointer to model concentrations
      REAL, PARAMETER    :: TWOTHIRDS = 2.0 / 3.0
      REAL               :: M2DRY_FINAL( N_MODE ) ! Dry 2nd Aerosol Moment
      REAL               :: M3DRY_FINAL( N_MODE ) ! Dry 3rd Aerosol Moment
      INTEGER            :: C, R, L, N
      REAL( 8 ), INTENT(OUT) :: WET_M2( : ), WET_M3( : ), DE_WET( : )
      LOGICAL, SAVE      :: FIRSTIME = .TRUE.

      IF ( FIRSTIME ) THEN
          FIRSTIME = .FALSE.
          ALLOCATE( CHEM_M2DRY_INIT( NCOLS,NROWS,NLAYS,N_MODE ),
     &              CHEM_M3DRY_INIT( NCOLS,NROWS,NLAYS,N_MODE ) )

      END IF

C *** extract grid cell concentrations of aero species from CGRID
C     into aerospc_conc in aero_data module
C     also converts dry surface area to wet second moment
#ifdef sens
               CALL EXTRACT_AERO ( CONCVEC(:), .TRUE., SENGRID(NCOLS,NROWS,NLAYS,:,:), .FALSE. )
#else
               CALL EXTRACT_AERO ( CONCVEC(:), .TRUE. )
#endif

C *** extract in inorganic aerosol processors

#ifdef sens
               CALL EXTRACT_PRECURSOR( CONCVEC(:), SENGRID( NCOLS,NROWS,NLAYS,:,: ) )
#else
               CALL EXTRACT_PRECURSOR( CONCVEC(:) ) 
#endif

C *** Update geometric mean diameters, geometric
C     standard deviations, modal mass totals, and modal particle
C     densities, based on the concentrations of M2, M0, and speciated
C     masses.
               CALL getpar( FIXED_sg )
     
C *** set up variables needed for calculating KN2O5 and YIELD_CLNO2

               DO N = 1, N_MODE
C *** estimate the "wet third moments" from moment3_conc
C     Note: this is the H2O concentration from previous time step
                  WET_M3( N ) = REAL( MOMENT3_CONC( N ), 8 )
C *** calculate "wet second moment" assuming that H2O does not
C     affect the geometric standard deviation
                  WET_M2( N ) = REAL( MOMENT2_CONC( N ), 8 ) 
C *** The "wet" geometric mean (same as median) diameter was updated in
C     getpar. It is stored in aeromode_diam
C *** calculate effective diameter (this is actually the mean) using Eq 3 of Pleim et al (1995)
                  DE_WET( N ) = REAL( AEROMODE_DIAM( N ) * EXP( 1.5 * AEROMODE_LNSG( N ) ** 2 ), 8)
               END DO

C *** Retrieve and Save the Dry 3rd and 2nd Moments
               CALL calcmoments( .False. )
               CHEM_M2DRY_INIT( C,R,L,: ) = REAL( MOMENT2_CONC( : ), 8 )
               CHEM_M3DRY_INIT( C,R,L,: ) = REAL( MOMENT3_CONC( : ), 8 )

      END SUBROUTINE HETCHEM_EXTRACT_AERO

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      SUBROUTINE HETCHEM_UPDATE_AERO ( CGRID )

c  Calculates the change in aerosol surface area due to heterogeneous
C  reactions and updates the CGRID array with this information.
C  
C  History
C  May 2016: BM Created

C  Key Subroutines Called: none
C
C  Called by: Gas-Phase Chemistry Driver
      USE AERO_DATA
      USE AEROMET_DATA, ONLY: pi
      USE GRID_CONF, ONLY: NLAYS, NROWS, NCOLS

      IMPLICIT NONE
      
      REAL, POINTER   :: CGRID ( :,:,:,: )  ! pointer to model concentrations
      REAL, PARAMETER :: TWOTHIRDS = 2.0 / 3.0
      REAL            :: M2DRY_FINAL( N_MODE ) ! Dry 2nd Aerosol Moment
      REAL            :: M3DRY_FINAL( N_MODE ) ! Dry 3rd Aerosol Moment
      INTEGER L, R, C, M 

      LOOP_LAY: DO L = 1, NLAYS
         LOOP_ROW: DO R = 1, NROWS
            LOOP_COL: DO C = 1, NCOLS   
      
               ! Extract grid cell concentrations of aero species from CGRID
               ! into aerospc_conc in aero_data module
               ! also converts dry surface area to wet second moment
#ifdef sens
               CALL EXTRACT_AERO ( CGRID( C,R,L,: ), .TRUE., CGRID(C,R,:,: ), .FALSE. )
#else
               CALL extract_aero ( CGRID( C,R,L,: ), .True. )
#endif

               ! Recalculate Dry 3rd Moment After Chemistry Processes
               ! 2nd moment hasnt been update yet.
               CALL calcmoments( .False. )
               M2DRY_FINAL( : ) = MOMENT2_CONC( : )
               M3DRY_FINAL( : ) = MOMENT3_CONC( : )

               ! Calculate new Second Moment Manually
               ! Assume standard deviation is fixed.
               M2DRY_FINAL( : ) = REAL( CHEM_M2DRY_INIT( C,R,L,: ) ) 
     &             * ( M3DRY_FINAL( : ) 
     &             / REAL( CHEM_M3DRY_INIT( C,R,L,: ) ) ) ** TWOTHIRDS

               ! Update the Aerosol Surface Area making sure to multiply
               ! the 2nd moment by Pi to convert properly
               DO M = 1, N_MODE
                 CGRID( C,R,L, aerosrf_map( M ) ) = 
     &                        PI * M2DRY_FINAL( M )
               END DO

            END DO LOOP_COL
         END DO LOOP_ROW
      END DO LOOP_LAY

      END SUBROUTINE HETCHEM_UPDATE_AERO
!
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE SUM_INLET_PM ( INLET, WETNESS, PM )

!     Calculates the total PM mass below a user-defined cut point D_AD,
!     or Aerodynamic Diameter in um.
!-----------------------------------------------------------------------

      USE AERO_DATA, ONLY : AEROSPC_CONC, AEROSPC, N_MODE, 
     &                      AH2O_IDX, AORGH2O_IDX
      USE ELMO_DATA

      IMPLICIT NONE

      ! Input variables
      INTEGER, INTENT( INOUT ) :: INLET ! Integer prescribing the inlet type
                                        !   (see ELMO_INLET table in ELMO_DATA module)
      INTEGER, INTENT( INOUT ) :: WETNESS ! 1 = Wet Aerosol; 2 = Dry Aerosol

      ! Output variable
      REAL, INTENT( OUT ) :: PM   ! PM Mass below cut point

      INTEGER IM
      REAL    VAL

C ----------------------- Begin solution -------------------------------
      PM = 0.0
      
      DO IM = 1,N_MODE
         CALL GET_AERO_INLET( INLET, IM, WETNESS, VAL )
         PM = PM + VAL * ( SUM( AEROSPC_CONC( :,IM ), 
     &                  MASK = .NOT.AEROSPC(:)%TRACER )
     &                  - AEROSPC_CONC( AH2O_IDX,IM ) )
                    IF ( AORGH2O_IDX .GT. 0 ) PM = PM
     &                  - VAL * AEROSPC_CONC( AORGH2O_IDX,IM ) ! ug m-3
      END DO
 
      END SUBROUTINE SUM_INLET_PM

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE GET_AERO_INLET ( INLET, IMODE, WETNESS, FRAC )

!     Calls the AERO_INLET routine for calculating the fraction of each 
!       mode collected by inlet prescribed by INLET. This subroutine is
!       only for PM0.1, PM1, PM2.5, PM10 and PM2.5to10. It is not for
!       AMS inlet calculation. If the user expands the ELMO_INLET
!       table to consider more cutoff ranges, this subroutine can be
!       used to calculate them as well.
!-----------------------------------------------------------------------

      USE ELMO_DATA
      USE AERO_DATA, ONLY : WET_AERO_DIAM, AEROMODE_LNSG, WET_AERO_DENS,
     &                      DRY_AERO_DIAM, DRY_AERO_DENS

      IMPLICIT NONE

      ! Input variables
      INTEGER, INTENT( INOUT ) :: INLET ! Integer prescribing the inlet type
                                        !   (see ELMO_INLET table in ELMO_DATA module)
      INTEGER, INTENT( INOUT ) ::  IMODE   ! Aerosol Mode of interest
      INTEGER, INTENT( INOUT ) ::  WETNESS ! 1 = Wet Aerosol; 2 = Dry Aerosol

      ! Output variable
      REAL, INTENT( OUT ) :: FRAC ! Fraction of aerosol mode IM
                                  !   collected by inlet described by INLET

C ----------------------- Begin solution -------------------------------
      
      IF ( INLET_FRAC( INLET,IMODE,WETNESS ) .LT. 0. ) THEN
         IF ( WETNESS .EQ. IWET ) THEN
           CALL AERO_INLET( WET_AERO_DIAM( IMODE ), AEROMODE_LNSG( IMODE ),
     &                      WET_AERO_DENS( IMODE ), 
     &                      ELMO_INLET( INLET )%DIAM_LO, 
     &                      ELMO_INLET( INLET )%DIAM_HI, 
     &                      FRAC )
         ELSE
           CALL AERO_INLET( DRY_AERO_DIAM( IMODE ), AEROMODE_LNSG( IMODE ),
     &                      DRY_AERO_DENS( IMODE ), 
     &                      ELMO_INLET( INLET )%DIAM_LO, 
     &                      ELMO_INLET( INLET )%DIAM_HI, 
     &                      FRAC )
         END IF
         
         INLET_FRAC( INLET,IMODE,WETNESS ) = FRAC 
      ELSE
         FRAC = INLET_FRAC( INLET,IMODE,WETNESS )
      END IF
 
      END SUBROUTINE GET_AERO_INLET
 
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE SUM_INLET_NUM ( INLET, WETNESS, NUM )

!     Calculates the total particle number within a user-defined range
!     for each aerosol mode and then sums them up.
!-----------------------------------------------------------------------

      USE AERO_DATA, ONLY : DRY_AERO_DIAM, WET_AERO_DIAM,
     &                      AEROMODE_LNSG, MOMENT0_CONC
      USE ELMO_DATA

      IMPLICIT NONE

      ! Input variables
      INTEGER, INTENT( INOUT ) :: INLET ! Integer prescribing the inlet type
                                        !   (see ELMO_INLET table in ELMO_DATA module)
      INTEGER, INTENT( INOUT ) ::  WETNESS ! 1 = Wet Aerosol; 2 = Dry Aerosol

      ! Output variable
      REAL, INTENT( OUT ) :: NUM   ! PM Mass below cut point

      INTEGER IM
      REAL    FRAC

C ----------------------- Begin solution -------------------------------
      NUM = 0.0
      
      DO IM = 1,N_MODE
         IF ( WETNESS .EQ. IWET ) THEN
           CALL NUM_INLET( WET_AERO_DIAM( IM ), AEROMODE_LNSG( IM ),
     &                      ELMO_INLET( INLET )%DIAM_LO, 
     &                      ELMO_INLET( INLET )%DIAM_HI, 
     &                      FRAC )
         ELSE
           CALL NUM_INLET( DRY_AERO_DIAM( IM ), AEROMODE_LNSG( IM ),
     &                      ELMO_INLET( INLET )%DIAM_LO, 
     &                      ELMO_INLET( INLET )%DIAM_HI, 
     &                      FRAC )
         END IF
 
         NUM = NUM + MOMENT0_CONC( IM ) * FRAC
      END DO
 
      END SUBROUTINE SUM_INLET_NUM

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE NUM_INLET ( DGN, XXLSG, D_ST1, D_ST2, fPM )

C  Calculates the number fraction of a given aerosol mode that is between
C  cutoff diamters D_ST1 and D_ST2. The Stokes diameter used by CMAQ is 
C  assumed to be equivalent to the electrical mobility diameter, reported 
C  by typical SMPS datasets. This is true for spherical particles.
C 
C  CMAQ aerosols are represented with Stokes Diameter. There are many
C  alternate forms of diameter including the following:
C       Volume Equivalent Diameter (D_ve) - the diameter of a sphere with the
C          same volume as the particle.
C       Stokes Diameter (D_st) - the diameter of a sphere with the same
C          terminal velocity as the real particle. If the shape factor
C          is 1, then the Stokes and Volume Equivalent Diameters are
C          equal.
C       Aerodynamic Diameter (D_ad) - the diameter of a sphere with unit
C          density and the same terminal velocity as the real particle.
C          If the density is 1, then the Aerodynamic Diameter and Stokes
C          Diameter are equal.
C       Vacuum Aerodynamic Diameter (D_va) - the diameter of a sphere with very
C          high Knudsen number and thus a simplified relationship with
C          Stokes diameter that is only dependent on density and shape
C          factor, not Slip Correction. The very high Knudsen number
C          could be because the particle is very small or because the
C          system pressure is very low.
C       Electrical Mobility Diameter (D_em) - the diameter of a sphere with the
C          same migration velocity as the real particle. Requires a
C          slip-correction and a shape factor correction when converting
C          from volume equivalent diameter.
 
C  Conversions:
C       Parameters
C         rho  - Density
C         rho0 - Unit Density (1 g cm-3)
C         Cc( ) - Cunningham Slip Correction of a particular diameter
C         X   - Shape factor = (NonSpherical Drag Force)/(Spherical Drag Force)
C       Aerodynamic -> Stokes
C         D_st = D_ad * sqrt(rho0/rho) * sqrt( Cc(D_ad) / Cc(D_st) )
C       Volume Equivalent -> Stokes
C         D_st = D_ve * sqrt(1/X) * sqrt( Cc(D_ve) / Cc(D_st) )
C       Electrical Mobility -> Volume Equivalent
C         D_ve = D_em * [Cc(D_ve) / Cc(D_em)] * 1/X
C             (If X = 1 (i.e. spherical), then D_st = D_ve = D_em )
C       Vacuum Aerodynamic -> Volume Equivalent (DeCarlo et al., 2004)
C         D_ve = D_va * rho0/rho * X
C             (If X = 1 (i.e. spherical), then D_st = D_ve = D_va * rho0/rho )
 
C  Key Subroutines called: none
 
C  Key Functions called:  ERF
 
C  Revision History:
C    Coded Jul 2005 by Prakash Bhave
C          Apr 2008 J.Kelly: corrected equation for Dst25 calculation
C          Feb 2016 B.Murphy: modified to output PM1 and PM10 mode
C                             fractions in addition to PM2.5
C          Feb 2020 B.Murphy: Created this number synthesizer
C 
C-----------------------------------------------------------------------

      IMPLICIT NONE

      INCLUDE SUBST_CONST    ! for PI

C    Input variables
      REAL, INTENT( IN ) :: DGN     ! geometric mean Stokes diameter by NUMBER [ m ]
                                    !  Remeber that geometric mean and median are
                                    !  identical for log-normal distributions.
      REAL, INTENT( IN ) :: XXLSG   ! natural log of geometric standard deviation
      REAL, INTENT( IN ) :: D_ST1   ! Lower stokes/electricla mobility diameter cut point [ um ]
      REAL, INTENT( IN ) :: D_ST2   ! Upper stokes/electrical mobility diameter cut point [ um ]

C    Output variable
      REAL, INTENT( OUT ) :: fPM   ! fraction of particulate number transmitted 
                                   ! through size window between D_ST1 and D_ST2

C    Internal variables
      REAL, PARAMETER :: SQRT2 = 1.4142136  !  SQRT( 2 )
      REAL ERF1, ERF2 ! argument of ERF, from Step#6 of Jiang et al. (2006)
      REAL ERFARG
      REAL DG         ! Internal units for median diameter

C *** Error function approximation, from Meng & Seinfeld (1994)
      REAL ERF        ! Error function
      REAL XX         ! dummy argument for ERF
      ERF( XX )  = SIGN( 1.0, XX ) * SQRT( 1.0 - EXP( -4.0 * XX * XX / PI ) )

C ----------------------- Begin solution -------------------------------

C *** Calculate Transmission Fractions for Inlets with Aerodynamic
C     Cutoffs
      DG = DGN  ! [um] The units need to be equivalent with the B parameter

      ! from Meng & Seinfeld (1994) and modified form of Fk(X) equation
      ! in Step#6 of Jiang et al. (2006).
      IF ( D_ST1 .EQ. 0.0 ) THEN
          ERF1 = -1.0
      ELSE
          ! Calculate mass fraction with Dca < SizeCut, using ERF approximation
          ERFARG = ( LOG( D_ST1 ) - LOG( DG ) ) / ( SQRT2 * XXLSG ) 
          ERF1 = ERF( ERFARG )
      END IF

      ! Calculate mass fraction with Dca < SizeCut, using ERF approximation
      ERFARG = ( LOG( D_ST2 ) - LOG( DG ) ) / ( SQRT2 * XXLSG ) 
      ERF2 = ERF( ERFARG )

      ! Calculate fraction between Lower and Upper Cut Points
      fPM = 0.5 * ( ERF2 - ERF1 )

      END SUBROUTINE NUM_INLET
 
