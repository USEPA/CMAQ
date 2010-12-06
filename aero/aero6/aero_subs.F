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
C $Header: /project/yoj/arc/CCTM/src/aero/aero6/aero_subs.F,v 1.1 2010/12/06 15:44:56 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C routines for aerosol formation and transformation processes

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE AEROPROC( DT, COL, ROW, LAYER, GAMMA_N2O5 )
      
      USE AERO_DATA
      USE SOA_DEFN
      USE MET_DATA

      IMPLICIT NONE

C *** arguments:
      REAL,    INTENT( IN ) :: DT          ! synchronization time step, sec
      INTEGER, INTENT( IN ) :: COL         ! Column of cell
      INTEGER, INTENT( IN ) :: ROW         ! Row of cell
      INTEGER, INTENT( IN ) :: LAYER       ! Layer of cell
      REAL,    INTENT( OUT ) :: GAMMA_N2O5 ! N2O5 heterogeneous reaction probability [ ]

C *** Parameters
      REAL, PARAMETER :: DIFFSULF = 9.36E-06  ! molecular diffusiviity for sulfuric acid
      REAL, PARAMETER :: SQRT2 = 1.4142135623731
      REAL, PARAMETER :: T0 = 288.15   ! [ K ]
      REAL, PARAMETER :: TWOTHIRDS = 2.0 / 3.0

C *** local variables
      REAL         DIFFCORR   ! Correction to DIFFSULF & DIFFORG for pressure
      REAL         DV_SO4     ! molecular diffusivity of H2SO4 vapor after correction for ambient conditions
      REAL         SQRT_TEMP  ! square root of ambient temperature
      REAL         XLM        ! atmospheric mean free path [m]
      REAL         AMU        ! atmospheric dynamic viscosity [kg/m s]
      REAL         M2_OLD     ! second moment concentration excluding H2O
      REAL         M3_OLD     ! third moment concentration excluding H2O

      REAL( 8 ) :: CGR( N_MODE-1 ) ! Aitken & Accum. modes

      REAL( 8 ) :: LAMDA      ! mean free path [ m ]
      REAL( 8 ) :: KNC        ! KNC = TWO3 * BOLTZ *  AIRTEMP / AMU

C *** Free Molecular regime (depends upon modal density)
      REAL( 8 ) :: KFMAT      ! = SQRT( 3.0*BOLTZ * AIRTEMP / PDENSAT )
      REAL( 8 ) :: KFMAC      ! = SQRT( 3.0*BOLTZ * AIRTEMP / PDENSAC )
      REAL( 8 ) :: KFMATAC    ! = SQRT( 6.0*BOLTZ * AIRTEMP / ( PDENSAT + PDENSAC ) )

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

C *** variables for advancing concentrations one time step
      REAL( 8 ) :: A, B
      REAL( 8 ) :: Y0, Y
      REAL( 8 ) :: EXPDT
      REAL( 8 ) :: LOSS, PROD, POL, LOSSINV
      REAL         TMASS
      REAL         FACTRANS ! special factor to compute mass transfer
      REAL         M20      ! for initial condidtions in time stepping

C *** Variables for mode merging
      REAL         GETAF
      REAL         AAA, XNUM, XM2, XM3,XXM2, XXM3
      REAL         FNUM, FM2, FM3, PHNUM, PHM2, PHM3
      REAL         ERF, ERFC    ! Error and complementary error function
      REAL         XX           ! dummy argument for ERF and ERFC

C *** local variables
      INTEGER      SPC        ! loop counter
      INTEGER      N          ! loop counter
      LOGICAL      M3_WET_FLAG
      LOGICAL   :: LIMIT_Sg = .FALSE.

      include SUBST_IODECL    ! I/O definitions and declarations
      integer, save :: logdev 
      logical, save :: firstime = .true.
C-----------------------------------------------------------------------

      if ( firstime ) then
         firstime = .false.
         logdev = init3()
         write( logdev,* ) 'aero_subs: nomm'
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
      MOMENT0_CONC( N_MODE ) = MAX( AEROMODE( N_MODE )%MIN_NUM,
     &                              MOMENT0_CONC( N_MODE ) )

C *** Outside AEROPROC, the surface area concentration [ m**2 / m**3 ]
C     of the Aitken and accumulation modes is tracked.  Internally,
C     the 2nd moment concentration is the variable of interest.
C     Surface area = pi * 2nd moment.
      DO N = 1, N_MODE
         MOMENT2_CONC( N ) = MOMENT2_CONC( N ) / PI
      END DO

C *** Update the third moments, geometric mean diameters, geometric
C     standard deviations, modal mass totals, and modal particle
C     densities, based on the concentrations of M2, M0, and speciated
C     masses resulting from transport, cloud processing, and gas-phase
C     chemistry.  Ignore H2O and semi-volatile SOA (i.e., M3_WET_FLAG
C     = .FALSE.) because those species are not transported with the
C     2nd moment and their concentrations will be updated subsequently
C     to establish gas/particle equilibrium.
      M3_WET_FLAG = .FALSE.
      LIMIT_Sg = .FALSE.
      CALL GETPAR( M3_WET_FLAG, LIMIT_Sg )

C *** Secondary Organics
C     Update the secondary organic aerosol (SOA) mass concentrations
C     and the SVOC mass concentrations by equilibrium absorptive
C     partitioning between the particle and vapor phases.  Assume all
C     SOA resides in the accumulation mode.
      CALL ORGAER( DT, LAYER )

C *** Heterogeneous Chemistry
C     Update the N2O5 and HNO3 concentrations to account for
C     heterogenous nitrate formation.
      CALL HETCHEM ( GAMMA_N2O5, DT )

C *** Secondary Inorganics
C     The VOLINORG subroutine includes the treatment of new particle
C     production and a fully dynamic treatment of inorganic gas-to-
C     particle mass transfer.

C *** Compute H2SO4 diffusivity, correct for temperature and pressure
      DIFFCORR = ( STDATMPA / AIRPRES ) * ( AIRTEMP / 273.16 ) ** 1.75
      DV_SO4 = DIFFSULF * DIFFCORR

C *** Augment 2nd and 3rd moment concentrations with H2O and recompute
C     size distribution parameters because wetted distribution is
C     needed in subroutine VOLINORG

      DO N = 1, N_MODE
         M3_OLD = MOMENT3_CONC( N )
         M2_OLD = MOMENT2_CONC( N )

         MOMENT3_CONC( N ) = M3_OLD + H2OFAC * AEROSPC_CONC( AH2O_IDX, N )
         MOMENT2_CONC( N ) = M2_OLD * ( MOMENT3_CONC( N ) / M3_OLD ) ** TWOTHIRDS
      END DO

      M3_WET_FLAG = .TRUE.
      CALL GETPAR( M3_WET_FLAG, LIMIT_Sg )

      CALL VOLINORG( DT, COL, ROW, LAYER,
     &               DV_SO4, CGR, M3_WET_FLAG )

C *** Coagulation
C     Calculate coagulation coefficients using a method dictated by
C     the value of FASTCOAG_FLAG.  If TRUE, the computationally-
C     efficient GETCOAGS routine is used.  If FALSE, the more intensive
C     Gauss-Hermite numerical quadrature method is used.  See Section
C     2.1 of Bhave et al. (2004) for further discussion.

C *** set atmospheric mean free path in double precision
      LAMDA    = XLM

C *** calculate term used in Equation A6 of Binkowski & Shankar (1995)
      KNC      = TWOTHIRDS * BOLTZMANN *  AIRTEMP / AMU

C *** calculate terms used in Equation A5 of Binkowski & Shankar (1995)
      KFMAT    = SQRT( 3.0 * BOLTZMANN * AIRTEMP / AEROMODE_DENS( 1 ) )
      KFMAC    = SQRT( 3.0 * BOLTZMANN * AIRTEMP / AEROMODE_DENS( 2 ) )
      KFMATAC  = SQRT( 6.0 * BOLTZMANN * AIRTEMP
     &         / ( AEROMODE_DENS( 1 ) + AEROMODE_DENS( 2 ) ) )

C *** transfer of number to accumulation mode from Aitken mode is zero
      BACAT( 1 ) = 0.0

      IF ( FASTCOAG_FLAG ) THEN ! Solve coagulation analytically

C *** set geometric mean diameters, geometric standard deviations, and
C     ln(GSD) in double precision
         DO N = 1, N_MODE
            DG_D( N ) = AEROMODE_DIAM( N )
            SG_D( N ) = EXP( AEROMODE_SDEV( N ) )
            XXL_D( N ) = AEROMODE_SDEV( N )
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
     &                      AEROMODE_SDEV( 1 ), BATAT( 2 ), BATAT( 1 ) )

C *** calculate accumulation-mode intramodal coagulation coefficients
C     for zeroth and second moments
         CALL INTRACOAG_GH( LAMDA, KFMAC, KNC, AEROMODE_DIAM( 2 ),
     &                      AEROMODE_SDEV( 2 ), BACAC( 2 ), BACAC( 1 ) )

C *** calculate intermodal coagulation coefficients for zeroth, second,
C     and third moments
         CALL INTERCOAG_GH( LAMDA, KFMATAC, KNC,
     &                      AEROMODE_DIAM( 1 ), AEROMODE_DIAM( 2 ),
     &                      AEROMODE_SDEV( 1 ), AEROMODE_SDEV( 2 ),
     &                      BATAC( 2 ), BACAT( 2 ), BATAC( 1 ), C3IJ )

      END IF   ! FASTCOAG_FLAG

C *** calculate 3rd moment intermodal transfer rate by coagulation
      C30ATAC = C3IJ * MOMENT0_CONC( 1 ) * MOMENT0_CONC( 2 )

C *** TAKE ONE FORWARD TIME STEP - Solve Modal Dynamics Equations
C     This code implements Section 1.4 of Binkowski and Roselle (2003)
C     with two notable exceptions.  1) emissions are treated in
C     CMAQ's vertical diffusion routine, so they do not appear in the
C     following equations. 2) new particle formation and condensational
C     growth are now treated in the VOLINORG subroutine, so they do not
C     appear in the following equations.
 
C     M2 is updated before M0 because the intermodal transfer rate of
C     M2 is a function of the number concentrations.  In contrast,
C     production and loss rates of M0 are independent of M2.  Advancing
C     M2 before M0 avoids operator splitting within the modal-dynamic-
C     equation solution.  A similar rearrangement would be necessary
C     for the M3 update, but the dependence of M3 on number
C     concentrations already is accounted for in the C30ATAC term.

C *** UPDATE SECOND MOMENT
C     For each lognormal mode, solve equations of form:
C        dM2/dt = P2 - L2*M2   ! if L2 > 0
C     with solution
C        M2(t) = P2/L2 + ( M2(t0) - P2/L2 ) * exp( -L2*dt )
C     or
C        dM2/dt = P2           ! if L2 = 0
C     with solution
C        M2(t) = M2(t0) + P2*dt

C *** Aitken mode: initial value of M2
      M20 = MOMENT2_CONC( 1 )

C *** Loss of 2nd moment from Aitken mode is due to intermodal
C     coagulation with accumulation mode and intramodal coagulation.
C     Production term is removed, because new particle formation
C     and condensational growth are accounted for in VOLINORG.
      LOSS = (
     &        ( BATAT( 2 ) * MOMENT0_CONC( 1 )
     &        + BATAC( 2 ) * MOMENT0_CONC( 2 ) ) * MOMENT0_CONC( 1 )
     &       ) / M20

C *** Solve for M2_Aitken based on LOSS during this time step
C     Note: LOSS is assumed to be non-negative.
      IF ( LOSS .GT. 0.0 ) THEN
         Y = M20 * EXP( -LOSS * DT )
      ELSE
         Y = M20
      END IF ! test on loss

C *** Transfer new value of M2_Aitken to the array
      MOMENT2_CONC( 1 ) = MAX( REAL( AEROMODE( 1 )%MIN_M2 ), REAL( Y ) )

C *** Accumulation mode: initial value of M2
      M20 = MOMENT2_CONC( 2 )

C *** Production of 2nd moment in accumulation mode is due to
C     intermodal coagulation Aitken mode
      PROD = BACAT( 2 ) * MOMENT0_CONC( 1 ) * MOMENT0_CONC( 2 )

C *** Loss of 2nd moment from accumulation mode is due only to
C     intramodal coagulation
      LOSS = ( BACAC( 2 ) * MOMENT0_CONC( 2 ) * MOMENT0_CONC( 2 ) ) / M20

C *** Solve for M2_accum based on PROD and LOSS during this time step
C     Note: LOSS is assumed to be non-negative.
      IF ( LOSS .GT. 0.0 ) THEN
         POL = PROD / LOSS
         Y = POL + ( M20 - POL ) * EXP( -LOSS * DT )
      ELSE
         Y = M20 + PROD * DT
      END IF ! test on loss

C *** Transfer new value of M2_accum to moment array
      MOMENT2_CONC( 2 ) = MAX( REAL( AEROMODE( 2 )%MIN_M2 ), REAL( Y ) )

C *** Coarse mode: no change because coagulation of coarse particles
C     is neglected in current model version.

C *** end of update for second moment

C *** Update Zeroth Moment (i.e. number concentration)

C *** Aitken mode: initial value of M0

      Y0 = MOMENT0_CONC( 1 )

C *** The rate of change for M0_Aitken is described in Equation 8a of
C     Binkowski & Roselle (2003), with the c_i term equal to 0.

      A = BATAT( 1 )                      ! intramodal coagulation
      B = BATAC( 1 ) * moment0_conc( 2 )  ! intermodal coagulation

      EXPDT = EXP( - B * DT )
      IF ( EXPDT .LT. 1.0D0 ) THEN
         Y = B * Y0 * EXPDT / ( B + A * Y0 * ( 1.0D0 - EXPDT ) )
      ELSE
         Y = Y0                 ! solution in the limit that B approaches zero
      END IF

C *** Transfer new value of M0_Aitken to the moment array
      MOMENT0_CONC( 1 ) = MAX( AEROMODE( 1 )%MIN_NUM, REAL( Y ) )

C *** Accumulation mode: initial value of M0
      Y0 = MOMENT0_CONC( 2 )

C *** The rate of change for M0_accum is described in Equation 8b of
C     Binkowski & Roselle (2003), except the coefficient C is zero
C     because emissions are treated outside the CMAQ aerosol module.
C     The equation reduces to the form: dY/dt = -A * Y**2 , where
      A = BACAC( 1 )                 ! intramodal coagulation

C *** Solve for M0_accum using Smoluchowski's solution
      Y = Y0 / ( 1.0D0 + A * Y0 * DT )

C *** Transfer new value of M0_accum to the moment array
      MOMENT0_CONC( 2 ) = MAX( AEROMODE( 2 )%MIN_NUM, REAL( Y ) )

C *** end of update for zeroth moment - note that the coarse mode number does
C     not change because coarse-mode coagulation is neglected in the model

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

C *** Set up extra variables to solve for Aitken mode mass concentrations
      FACTRANS = LOSS * DT
      EXPDT = EXP( -FACTRANS )
      LOSSINV = 1.0 / LOSS

C *** Secondary Organics and Inorganics
C     These species are produced by condensation, but their mass
C     concentrations have been updated in subroutine VOLINORG.  Therefore,
C     only the mass transfer due to intermodal coagulation is
C     treated here.

      DO SPC = 1, N_AEROSPC
         IF ( SPC .EQ. ANA_IDX ) CYCLE
         IF ( AEROSPC( SPC )%NAME( 1 ) .EQ. ' ' ) CYCLE
 
         TMASS = AEROSPC_CONC( SPC,1 ) + AEROSPC_CONC( SPC,2 )
         AEROSPC_CONC( SPC,1 ) = MAX( AEROSPC( SPC )%MIN_CONC( 1 ),
     &                                REAL( AEROSPC_CONC( SPC,1 ) * EXPDT ) )
         AEROSPC_CONC( SPC,2 ) = MAX( AEROSPC( SPC )%MIN_CONC( 2 ),
     &                                TMASS - AEROSPC_CONC( SPC,1 ) )
      END DO

C *** end of update for species mass concentrations

C *** Mode Merging
C     This code implements Section 1.5 of Binkowski and Roselle (2003).
C     If the Aitken mode mass is growing faster than accumulation mode
C     mass and the Aitken mode number concentration exceeds the
C     accumulation mode number concentration, then modes are merged by
C     renaming.

#ifdef nomm
      if ( .false. ) then
#else
      IF ( CGR( 1 ) .GT. CGR( 2 ) .AND.
     &     MOMENT0_CONC( 1 ) .GT. MOMENT0_CONC( 2 ) ) THEN
#endif

C *** Before mode merging, update the third moments, geometric mean
C     diameters, geometric standard deviations, modal mass totals, and
C     particle densities, based on the new concentrations of M2, M0, and
C     speciated masses calculated above.
         CALL GETPAR( M3_WET_FLAG, LIMIT_Sg )

C *** Calculate AAA = ln( Dij / DGATK ) / ( SQRT2 * XXLSGAT ), where Dij
C     is the diameter at which the Aitken-mode and accumulation-mode
C     number distributions intersect (i.e., overlap).  AAA is equivalent
C     to the "Xnum" term described below Equation 10a by Binkowski and
C     Roselle (2003).
         AAA = GETAF( MOMENT0_CONC( 1 ), MOMENT0_CONC( 2 ),
     &                AEROMODE_DIAM( 1 ), AEROMODE_DIAM( 2 ), 
     &                AEROMODE_SDEV( 1 ), AEROMODE_SDEV( 2 ),
     &                SQRT2 ) 

C *** Ensure that Xnum is large enough so that no more than half of
C     the Aitken mode mass is merged into the accumulation mode during
C     any given time step.  This criterion is described in Paragraph 26
C     of Binkowski and Roselle (2003).
         XXM3 = 3.0 * AEROMODE_SDEV( 1 ) / SQRT2
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
C     using Equation 11b of Binkowski and Roselle (2003).
         DO SPC = 1, N_AEROSPC
            IF ( SPC .EQ. ANA_IDX ) CYCLE
            IF ( SPC .EQ. ACL_IDX ) CYCLE
            IF ( AEROSPC( SPC )%NAME( 1 ) .EQ. ' ' ) CYCLE

            AEROSPC_CONC( SPC,2 ) = AEROSPC_CONC( SPC,2 ) + AEROSPC_CONC( SPC,1 ) * FM3
            AEROSPC_CONC( SPC,1 ) = AEROSPC_CONC( SPC,1 ) * PHM3
         END DO

      END IF ! end check on necessity for merging

C *** end of update for mode merging

C *** Update the third moments, geometric mean diameters, geometric
C     standard deviations, modal mass totals, and particle densities,
C     based on the final concentrations of M2, M0, and speciated masses
C     after mode merging is complete.
      CALL GETPAR( M3_WET_FLAG, LIMIT_Sg )

C *** Set minimum value for all concentrations in the CBLK array

      DO N = 1, N_MODE
         DO SPC = 1, N_AEROSPC
         AEROSPC_CONC( SPC,N ) = MAX( AEROSPC_CONC( SPC,N ),
     &                                AEROSPC( SPC )%MIN_CONC( N ) )
         END DO
      END DO

      RETURN
      END SUBROUTINE AEROPROC

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE VOLINORG( DT, COL, ROW, LAYER,
     &                     DV_SO4, CGR, M3_WET_FLAG )

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
C     15 Jul 08, J.Young, P.Bhave: increased cutoff to hybrid from .01 to .05 ug/m**3
C                J.Young: change 'OPTIONFLAG' to just a logical variable, 'Hybrid'

C *** References
C 1. Pilinis C, Capaldo KP, Nenes A, Pandis SN (2000) MADM - A new
C    multicomponent aerosol dynamics model. AEROSOL SCIENCE AND TECHNOLOGY.
C    32(5):482-502
C
C 2. Capaldo KP, Pilinis C, Pandis SN (2000) A computationally efficient hybrid
C    approach for dynamic gas/aerosol transfer in air quality models. ATMOSPHERIC
C    ENVIRONMENT. 34(21):3617-3627

      USE AERO_DATA
      USE PRECURSOR_DATA
      USE SOA_DEFN
      USE MET_DATA

      IMPLICIT NONE

C *** Includes:
      INCLUDE SUBST_IODECL    ! I/O definitions and declarations

C *** Arguments:
      REAL    DT              ! time step [sec]
      INTEGER COL             ! grid column index
      INTEGER ROW             ! grid row index
      INTEGER LAYER           ! model layer index
      REAL    DV_SO4          ! molecular diffusivity of H2SO4 vapor 
                              ! after correction for ambient conditions
      REAL( 8 ) :: CGR( N_MODE-1 ) ! 3rd moment SO4 growth rate [m^3/m^3-s]
      LOGICAL M3_WET_FLAG     ! flag to include water in GETPAR update

C *** Parameters: 
      INTEGER, PARAMETER :: NINORG = 6      ! number of inorganic species
      INTEGER, PARAMETER :: NVOLINORG = 3   ! number of volatile inorganic species

      ! indices for inorganic species
      INTEGER, PARAMETER :: KNH4 = 1, KNO3 = 2, KCL = 3, KSO4 = 4, KNA = 5, KHP = 6

      REAL( 8 ), PARAMETER :: MWH2SO4 = 98.0 ! molecular weight for H2SO4
      REAL( 8 ), PARAMETER :: FAERH2SO4 = 1.0E-6 / MWH2SO4
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

C *** Outputs from HCOND3: size-dependent term in the condensational-growth 
C     expressions defined in Equations A13-A14 of [Binkowski & Shankar,1995]
      REAL( 8 ) :: FCONC_SO4( N_MODE,2 )  ! All sizes 2nd and 3rd moments
      REAL( 8 ) :: FCONCM1_SO4       ! reciprocals of total SO4 cond rates

C *** Modal partition factors [ dimensionless ]
C     defined in Equations A17-A18 of [Binkowski & Shankar,1995]
      REAL( 8 ) :: OMEGA_AT_SO4  ! Aitken mode 2nd and 3rd moments
      REAL( 8 ) :: OMEGA_AC_SO4  ! Accumulation mode 2nd and 3rd moments
      REAL( 8 ) :: OMEGA( 2 )    ! partitioning coefficient for equilibrium PM mass

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
      REAL( 8 ) :: WI( NINORG - 1 )     ! species array
      REAL( 8 ) :: RHI                  ! relative humidity
      REAL( 8 ) :: TEMPI                ! temperature
      REAL( 8 ) :: CNTRL( 2 )           ! control parameters 

C *** ISORROPIA output variables
      REAL( 8 ) :: WT( NINORG - 1 )     ! species output array
      REAL( 8 ) :: GAS( 3 )             ! gas-phase   "     " 
      REAL( 8 ) :: AERLIQ( 12 )         ! liq aerosol "     " 
      REAL( 8 ) :: AERSLD( 9 )          ! solid "     "     " 
      REAL( 8 ) :: OTHER( 6 )           ! supplmentary output array
      CHARACTER( 15 ) :: SCASI          ! subcase number output

C *** Variables to account for mass conservation violations in ISRP3F
      LOGICAL TRUSTNH4                  ! false if ISOROPIA's partitioning
                                        !  of NH4/NH3 is to be ignored
      LOGICAL TRUSTCL                   ! false if ISOROPIA's partitioning       
                                        !  of Cl/HCl is to be ignored

C *** Initial (double-precision) concentrations [ug/m3]
      REAL( 8 ) :: GNH3R8               ! gas-phase ammonia
      REAL( 8 ) :: GNO3R8               ! gas-phase nitric acid
      REAL( 8 ) :: GCLR8                ! gas-phase hydrochloric acid
      REAL( 8 ) :: H2OR8                ! aerosol LWC

C *** Variables for volatile species mass transfer between gas and aerosol and
C     mass partitioning between the modes 
      LOGICAL HYBRID ! mass transfer option flag (mass transfer if .TRUE.)
      REAL( 8 ) :: DELT                 ! time step DT [s]
      REAL( 8 ) :: HPLUS( N_MODE )      ! scratch var for H+ [umol/m**3]

      REAL( 8 ), SAVE :: H2SO4RATM1     ! Mol. wt. ratio of SO4/H2SO4

      REAL( 8 ) :: JH2SO4 ! flux of cond./evap. H2SO4 (mol/m3/s)
      REAL( 8 ) :: CH2SO4 ! concentration of H2SO4 before cond/evap (mol/m3)
      REAL( 8 ) :: DVOLINORG( NVOLINORG ) ! vol inorg spcs mass to be xferred [mol/m3]
      REAL( 8 ) :: DVOLMAX                ! max value for DVOLINORG 
      REAL( 8 ) :: CINORG( NINORG,N_MODE ) ! scratch array for inorg spcs [ug/m**3]
      REAL( 8 ) :: INT_TIME               ! internal mass transfer time (s)
      REAL( 8 ) :: TSTEP                  ! mass transfer time step [s]
      REAL( 8 ) :: DRYM20, Y              ! scratch vars for 2nd moment [m**2/m**3]
      REAL( 8 ) :: M3OTHR, M3SOA          ! vars for 3rd moment calculation [m**3/m**3]
      REAL( 8 ), SAVE :: DF( NVOLINORG )  ! scratch array for mole -> ug conversion
      REAL( 8 ), SAVE :: DFH2OR8          ! mole -> ug conversion for H2O
      REAL( 8 ) :: CONVFAC             ! conversion factor for SO4 conc from DH2SO4/DT
      REAL( 8 ) :: J( NVOLINORG )      ! condensation/evaporation flux [mol/m**3-s]
      REAL( 8 ) :: CFINAL( NVOLINORG,N_MODE ) ! conc after mass xfer step [ug/m**3]
      REAL( 8 ) :: H2O                  ! Scratch LWC variable for output
      REAL( 8 ) :: H2O_NEW              ! Update of LWC after condensation 
      REAL( 8 ) :: SO4                  ! modal SO4 after condensation or cond + nucl
      REAL( 8 ) :: DMASS( NVOLINORG )   ! excess mass in fine mode partitioning
      REAL( 8 ) :: DDRYM3DT             ! rate of 3rd moment transfer - dry inorg spcs
      REAL( 8 ) :: DDRYM2DT             ! rate of 2nd moment transfer -  "     "    "
      REAL( 8 ) :: DRYM3, WETM3         ! scratch vars for 3rd moment calc [m**3/m**3]
      REAL( 8 ) :: DRYM2, WETM2         ! scratch vars for 2nd moment calc [m**2/m**3]
      REAL( 8 ) :: LOSS                 ! rate of loss of second moment [1/s] 

      REAL( 8 ) :: TMP
      INTEGER I, N                      ! loop and array indices
      INTEGER IMODE                     ! mode loop index  
      INTEGER ISTEP                     ! loop index, mass transfer time step loop
      INTEGER ISP                       ! loop index, species loop
      INTEGER, SAVE :: LOGDEV 
      LOGICAL, PARAMETER :: LIMIT_Sg = .TRUE.   ! fix Sg at current value?
      LOGICAL TrustIso                  ! For negative vap. press., TrustIso = F 

C *** Local Saved Variables 
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      LOGICAL, SAVE :: FIRSTWRITE = .TRUE.
      REAL( 8 ), SAVE :: SO4FAC         !  F6DPIM9 / RHOSO4
      REAL( 8 ), SAVE :: SOILFAC        !  F6DPIM9 / RHOSOIL
      REAL( 8 ), SAVE :: ANTHFAC        !  F6DPIM9 / RHOANTH
      REAL( 8 ), SAVE :: NH3RAT         ! Mol. wt ratios NH3/NH4
      REAL( 8 ), SAVE :: HNO3RAT        ! Mol. wt ratios HNO3/NO3
      REAL( 8 ), SAVE :: HCLRAT         ! Mol. wt ratios HCL/CL 

C-----------------------------------------------------------------------
 
      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         LOGDEV = INIT3()
         write( logdev,* ) 'aero_subs, volinorg: noiso'

         COFCBAR_SO4 = SQRT( 8.0 * RGASUNIV / ( PI * MWH2SO4 * 1.0E-3 ) )
         H2SO4RATM1 = AEROSPC_MW( ASO4_IDX ) / MWH2SO4
         SO4FAC  = 1.0E-9 * F6DPI / AEROSPC( ASO4_IDX )%DENSITY
         SOILFAC = 1.0E-9 * F6DPI / AEROSPC( ASOIL_IDX )%DENSITY
         ANTHFAC = 1.0E-9 * F6DPI / AEROSPC( ACORS_IDX )%DENSITY

         DF( KNH4 ) = 1.0D6 * AEROSPC_MW( ANH4_IDX )
         DF( KNO3 ) = 1.0D6 * AEROSPC_MW( ANO3_IDX )
         DF( KCL )  = 1.0D6 * AEROSPC_MW( ACL_IDX )
         DFH2OR8    = 1.0D6 * MWWAT      ! aerospc_mw(AH2O_IDX) (18.0 != 18.0153)

         ! Mol. wt ratios NH3/NH4, HNO3/NO3, HCL/CL
         NH3RAT  = PRECURSOR_MW( NH3_IDX )  / AEROSPC_MW( ANH4_IDX )
         HNO3RAT = PRECURSOR_MW( HNO3_IDX ) / AEROSPC_MW( ANO3_IDX )
         HCLRAT  = PRECURSOR_MW( HCL_IDX )  / AEROSPC_MW( ACL_IDX )
      END IF

C *** Determine if Hybrid
      TMP = 0.0
#ifdef noiso
      hybrid = .false.
#else
      DO I = 1, N_AEROSPC
         IF ( AEROSPC( I )%CHARGE .NE. 0 ) TMP = TMP + AEROSPC_CONC( I,N_MODE )
      END DO
      HYBRID = ( TMP .GE. CUTOFF ) .AND. ( AIRRH .GE. 0.18 )
#endif

      DELT  = REAL( DT, 8 )
      CONVFAC = DELT * H2SO4RATM1

      TEMPI = AIRTEMP             ! assume const within synch timestep
      RHI   = MIN( 0.95, AIRRH )  ! "        "     "      "     "

C *** Calculate molecular velocities (temperature dependent) and
C     H+ concentration

      CBAR_SO4 = COFCBAR_SO4 * SQRT( AIRTEMP )

      HPLUS = 0.0
      DO I = 1, N_MODE
         DO N = 1, N_AEROSPC
            HPLUS( I ) = HPLUS( I )
     &                 + AEROSPC( N )%CHARGE * AEROSPC_CONC( N,I ) / AEROSPC_MW( N )
         END DO
      END DO

C *** Condensational Growth (Size-dependent terms)
C     Calculate intermediate variables needed to determine the 2nd and
C     3rd moment condensational-growth rates.  3rd moment terms are 
C     needed for the calculation of new particle production.  See 
C     Section 3.3 of Jiang & Roth (2003) for a detailed discussion.
C    
C *** Calculate first moments using Equation 4 of Binkowski & Shankar
C     (1995) or Equation 3 of Binkowski and Roselle (2003).
C     N.B: these are for a "wet" size distribution

      DO I = 1, N_MODE
         AM0( I ) = MOMENT0_CONC( I ) 
         AM1( I ) = MOMENT0_CONC( I ) * AEROMODE_DIAM( I )
     &            * EXP( 0.5 * AEROMODE_SDEV( I ) * AEROMODE_SDEV( I ) )
         AM2( I ) = MOMENT2_CONC( I )
      END DO
      
C *** Calculate the size-dependent terms in the condensational-
C     growth factor expressions for sulfate using 
C     Equations A13-A14 of Binkowski & Shankar (1995). 
       
      DO I = 1, N_MODE
         CALL HCOND3( AM0( I ), AM1( I ), AM2( I ),
     &                DV_SO4, ALPHSULF, CBAR_SO4, FCONC_SO4( I,: ) )
      END DO 

      IF ( .NOT. HYBRID ) THEN
         FCONC_SO4( N_MODE,1 ) = 0.D0
         FCONC_SO4( N_MODE,2 ) = 0.D0
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

         XH2SO4 = SO4RATE / TMP
         XH2SO4 = MAX( XH2SO4, CONMIN )
         PRECURSOR_CONC( SULF_IDX ) = XH2SO4

C *** Calculate new particle production rate for 0th, 2nd, & 3rd moments
         CALL NEWPART3 ( AIRRH, AIRTEMP, XH2SO4, SO4RATE,
     &                   DNDT, DMDT_SO4, DM2DT )
         
C *** Calculate sulfate condensation rate as the gas-phase production 
C     rate minus the new particle production rate, following Equation
C     3.23 of Jiang & Roth (2003).
         SCONDRATE = MAX( SO4RATE - DMDT_SO4, 0.0D0 )

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

         INT_TIME = 0.0D0
         TSTEP    = 90.0D0
         ISTEP    = 1
         IMODE    = 3
         TrustIso = .TRUE.
 
         DO WHILE ( INT_TIME .LT. DELT ) 

            IF ( INT_TIME + TSTEP .GT. DELT ) TSTEP = DELT - INT_TIME 
            INT_TIME = INT_TIME + TSTEP 
            ISTEP = ISTEP + 1   

            IF ( ISTEP .GT. 1 ) THEN

C *** Calculate first moments using Equation 4 of Binkowski & Shankar
C     (1995) or Equation 3 of Binkowski and Roselle (2003).
C     N.B: these are for a "wet" size distribution
               AM0( 3 ) = MOMENT0_CONC( 3 )
               AM1( 3 ) = MOMENT0_CONC( 3 ) * AEROMODE_DIAM( 3 )
     &                  * EXP( 0.5 * AEROMODE_SDEV( 3 ) * AEROMODE_SDEV( 3 ) )
               AM2( 3 ) = MOMENT2_CONC( 3 )

C *** Calculate the size-dependent terms in the condensational-
C     growth factor expressions for sulfate using 
C     Equations A13-A14 of Binkowski & Shankar (1995). 
               CALL HCOND3( AM0( 3 ), AM1( 3 ), AM2( 3 ), DV_SO4, ALPHSULF, 
     &                      CBAR_SO4, FCONC_SO4( 3,: ) )  ! adapted from Eq A14

               GRFAC1( 3 ) = FCONC_SO4( 3,1 ) 
               GRFAC2( 3 ) = FCONC_SO4( 3,2 ) 

            END IF               ! if ISTEP .GT. 1

C *** Set conc array to aerosol concentrations prior to mass transfer
            CINORG( KNH4,IMODE ) = REAL( AEROSPC_CONC( ANH4_IDX,IMODE ), 8 )
            CINORG( KNO3,IMODE ) = REAL( AEROSPC_CONC( ANO3_IDX,IMODE ), 8 )
            CINORG( KCL, IMODE ) = REAL( AEROSPC_CONC( ACL_IDX,IMODE ), 8 )
            CINORG( KSO4,IMODE ) = REAL( AEROSPC_CONC( ASO4_IDX,IMODE ), 8 )
            CINORG( KNA, IMODE ) = REAL( AEROSPC_CONC( ANA_IDX,IMODE ), 8 )
            CINORG( KHP, IMODE ) = REAL( HPLUS( IMODE ), 8 )

            M3OTHR = SOILFAC * AEROSPC_CONC( ASOIL_IDX,IMODE )
     &             + ANTHFAC * AEROSPC_CONC( ACORS_IDX,IMODE )
            M3SOA  = 0.0D0
            WETM3  = MOMENT3_CONC( IMODE )
            WETM2  = MOMENT2_CONC( IMODE )
            DRYM3  = WETM3 - REAL( H2OFAC * AEROSPC_CONC( AH2O_IDX, IMODE ), 8 )
            DRYM20 = WETM2 * ( DRYM3 / WETM3 ) ** D_TWOTHIRDS

C *** Initial vapor-phase concentrations [ug/m3]
            GNO3R8 = REAL( PRECURSOR_CONC( HNO3_IDX ), 8 )
            GNH3R8 = REAL( PRECURSOR_CONC( NH3_IDX ), 8 )
            GCLR8  = REAL( PRECURSOR_CONC( HCL_IDX ), 8 )

C *** This section of code calculates the distribution of ammonia/
C     ammonium, nitric acid/nitrate, and water between the gas and 
C     aerosol phases as a function of total sulfate, total ammonia, 
C     total nitrate, relative humidity, and temperature.  It is assumed
C     that the aerosol is entirely aqueous (i.e., metastable assumption),
C     irrespective of ambient relative humidity.

C *** Compute sulfate production rate [ug/m3 s] for coarse mode

            RATE = CONDSO4( IMODE )
            SO4  = CINORG( KSO4,IMODE ) + RATE * TSTEP * H2SO4RATM1

            IF ( TrustIso ) THEN

C *** Double Precision vars for ISORROPIA [mole/m3]
               WI( 1 ) = CINORG( KNA, IMODE ) * 1.0D-6 / REAL( AEROSPC_MW( ANA_IDX ), 8 )
               WI( 2 ) =                  SO4 * 1.0D-6 / REAL( AEROSPC_MW( ASO4_IDX ), 8 )
               WI( 3 ) = CINORG( KNH4,IMODE ) * 1.0D-6 / REAL( AEROSPC_MW( ANH4_IDX ), 8 )
               WI( 4 ) = CINORG( KNO3,IMODE ) * 1.0D-6 / REAL( AEROSPC_MW( ANO3_IDX ), 8 )
               WI( 5 ) = CINORG( KCL, IMODE ) * 1.0D-6 / REAL( AEROSPC_MW( ACL_IDX ), 8 )

               CNTRL( 1 ) = 1.0D0 ! reverse problem
               CNTRL( 2 ) = 1.0D0 ! aerosol in metastable state

C *** Set flags to account for mass conservation violations in ISRP3F
               TRUSTCL  = .TRUE.
               TRUSTNH4 = .TRUE.
               IF ( WI( 1 ) + WI( 5 ) .LT. 1.0D-20 ) THEN
                  TRUSTCL = .FALSE.
               ELSE
                  IF ( WI( 3 ) .LT. 1.0D-10 ) TRUSTNH4 = .FALSE.
                  IF ( WI( 5 ) .LT. 1.0D-10 ) TRUSTCL  = .FALSE.
               END IF

C              WI( 1 ) = NA    [mol/m3]
C              WI( 2 ) = SO4      "
C              WI( 3 ) = NH4      "
C              WI( 4 ) = NO3      "
C              WI( 5 ) = CL       "
C              GAS(1) = NH3, GAS(2) = HNO3, GAS(3) = HCl

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
C     equilibrium gas-phase concentration. DVOLINORG is positive for
C     condensation and negative for evaporation.
            DVOLINORG( KNH4 ) = GNH3R8 * 1.0D-6 / REAL( PRECURSOR_MW( NH3_IDX ), 8 ) - GAS( 1 )  
            DVOLINORG( KNO3 ) = GNO3R8 * 1.0D-6 / REAL( PRECURSOR_MW( HNO3_IDX ), 8 ) - GAS( 2 ) 
            DVOLINORG( KCL )  = GCLR8  * 1.0D-6 / REAL( PRECURSOR_MW( HCL_IDX ), 8 ) - GAS( 3 ) 
           
C *** Calculate condensation/evaporation flux for this time step and update 
C     volatile species concentrations.  final aerosol conc set to be  
C     no less than the minimum aerosol conc
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

            HPLUS( IMODE ) = 2.0D0 * SO4          / AEROSPC_MW( ASO4_IDX )
     &                     + CFINAL( KNO3,IMODE ) / AEROSPC_MW( ANO3_IDX )
     &                     + CFINAL( KCL, IMODE ) / AEROSPC_MW( ACL_IDX )
     &                     - CFINAL( KNH4,IMODE ) / AEROSPC_MW( ANH4_IDX )
     &                     - CINORG( KNA, IMODE ) / AEROSPC_MW( ANA_IDX )

C *** Equilibrate aerosol LWC with CFINAL by calling CALC_H2O

            WI( 1 ) = CINORG( KNA, IMODE ) * 1.0D-6 / REAL( AEROSPC_MW( ANA_IDX ), 8 )
            WI( 2 ) =                  SO4 * 1.0D-6 / REAL( AEROSPC_MW( ASO4_IDX ), 8 )
            WI( 3 ) = CFINAL( KNH4,IMODE ) * 1.0D-6 / REAL( AEROSPC_MW( ANH4_IDX ), 8 )
            WI( 4 ) = CFINAL( KNO3,IMODE ) * 1.0D-6 / REAL( AEROSPC_MW( ANO3_IDX ), 8 )
            WI( 5 ) = CFINAL( KCL, IMODE ) * 1.0D-6 / REAL( AEROSPC_MW( ACL_IDX ), 8 )

            CALL CALC_H2O( WI, RHI, TEMPI, H2O_NEW ) 

            H2O = H2O_NEW * DFH2OR8 

C *** Compute net change in 3rd moment due to dry inorganic mass transfer

            DDRYM3DT = (
     &                   ( CFINAL( KNH4,IMODE ) - CINORG( KNH4,IMODE ) )
     &                     * ( 1.0D-9 * F6DPI / REAL( AEROSPC( ANH4_IDX )%DENSITY, 8 ) )
     &                 + ( CFINAL( KNO3,IMODE ) - CINORG( KNO3,IMODE ) )
     &                     * ( 1.0D-9 * F6DPI / REAL( AEROSPC( ANO3_IDX )%DENSITY, 8 ) )
     &                 + ( CFINAL( KCL, IMODE ) - CINORG( KCL,IMODE ) )
     &                     * ( 1.0D-9 * F6DPI / REAL( AEROSPC( ACL_IDX )%DENSITY, 8 ) )
     &                 + ( SO4                  - CINORG( KSO4,IMODE ) )
     &                     * ( 1.0D-9 * F6DPI / REAL( AEROSPC( ASO4_IDX )%DENSITY, 8 ) ) ) / TSTEP

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

C *** Add water and SOA to 2nd moment while preserving standard deviation

            DRYM3 = ( 1.0D-9 * F6DPI / REAL( AEROSPC( ASO4_IDX )%DENSITY, 8 ) ) * SO4
     &            + ( 1.0D-9 * F6DPI / REAL( AEROSPC( ANH4_IDX )%DENSITY, 8 ) )
     &                               * CFINAL( KNH4,IMODE )
     &            + ( 1.0D-9 * F6DPI / REAL( AEROSPC( ANO3_IDX )%DENSITY, 8 ) )
     &                               * CFINAL( KNO3,IMODE )
     &            + ( 1.0D-9 * F6DPI / REAL( AEROSPC( ACL_IDX )%DENSITY, 8 ) )
     &                               * CFINAL( KCL,IMODE )
     &            + ( 1.0D-9 * F6DPI / REAL( AEROSPC( ANA_IDX )%DENSITY, 8 ) )
     &                               * CINORG( KNA,IMODE )
     &            + M3OTHR                   

            WETM3 = DRYM3 + M3SOA
     &            + ( 1.0D-9 * F6DPI / REAL( AEROSPC( AH2O_IDX )%DENSITY, 8 ) ) * H2O  
            DRYM2 = MAX( REAL( AEROMODE( IMODE )%MIN_M2, 8 ), Y )
            WETM2 = DRYM2 * ( WETM3 / DRYM3 ) ** D_TWOTHIRDS


            PRECURSOR_CONC( NH3_IDX ) = GNH3R8 + ( CINORG( KNH4,IMODE )
     &                                - CFINAL( KNH4,IMODE ) ) * NH3RAT 
 
            PRECURSOR_CONC( HNO3_IDX ) = GNO3R8 + ( CINORG( KNO3,IMODE )
     &                                 - CFINAL( KNO3,IMODE) ) * HNO3RAT  
   
            PRECURSOR_CONC( HCL_IDX ) = GCLR8 + ( CINORG( KCL,IMODE )
     &                                - CFINAL( KCL,IMODE) ) * HCLRAT 

            AEROSPC_CONC( ANH4_IDX,IMODE ) = CFINAL( KNH4,IMODE )
            AEROSPC_CONC( ANO3_IDX,IMODE ) = CFINAL( KNO3,IMODE )
            AEROSPC_CONC( ACL_IDX,IMODE )  = CFINAL( KCL, IMODE )
            AEROSPC_CONC( ASO4_IDX,IMODE ) = SO4
            AEROSPC_CONC( AH2O_IDX,IMODE ) = H2O
            MOMENT2_CONC( IMODE ) = WETM2

C *** Update the third moments, geometric mean diameters, geometric 
C     standard deviations, modal mass totals, and modal particle 
C     densities.
               
            CALL GETPAR( M3_WET_FLAG, LIMIT_Sg )

         END DO                 ! end mass transfer time step loop
         
      END IF                     ! endif for 'Hybrid' method

C *** End of dynamic mass transfer calculations for coarse mode

         
C *** Call ISORROPIA in forward mode to calculate gas-particle equilibrium in 
C     the fine aerosol modes 
      GNH3R8  = PRECURSOR_CONC( NH3_IDX )
      GNO3R8  = PRECURSOR_CONC( HNO3_IDX )
      GCLR8   = PRECURSOR_CONC( HCL_IDX )

C *** Compute sulfate from total sulfate production rate [ug/m3-s] for fine 
C     modes; add in H2SO4 nucleated in model timestep

      SO4 = AEROSPC_CONC( ASO4_IDX,1 ) + AEROSPC_CONC( ASO4_IDX,2 )
      SO4 = SO4 + ( DMDT_SO4 + CONDSO4( 1 ) + CONDSO4( 2 ) ) * DELT * H2SO4RATM1 

      WI( 1 ) = ( AEROSPC_CONC( ANA_IDX,1 ) + AEROSPC_CONC( ANA_IDX,2 ) )
     &        * 1.0D-6 / REAL( AEROSPC_MW( ANA_IDX ), 8 )

      WI( 2 ) = SO4 * 1.0D-6 / REAL( AEROSPC_MW( ASO4_IDX ), 8 )

      WI( 3 ) = PRECURSOR_CONC( NH3_IDX ) * 1.0D-6 / REAL( PRECURSOR_MW( NH3_IDX ), 8 )
     &        + ( AEROSPC_CONC( ANH4_IDX,1 ) + AEROSPC_CONC( ANH4_IDX,2 ) )
     &        * 1.0D-6 / REAL( AEROSPC_MW( ANH4_IDX ), 8 )

      WI( 4 ) = PRECURSOR_CONC( HNO3_IDX ) * 1.0D-6 / REAL( PRECURSOR_MW( HNO3_IDX ), 8 )
     &        + ( AEROSPC_CONC( ANO3_IDX,1 ) + AEROSPC_CONC( ANO3_IDX,2 ) )
     &        * 1.0D-6 / REAL( AEROSPC_MW( ANO3_IDX ), 8 )

      WI( 5 ) = PRECURSOR_CONC( HCL_IDX ) * 1.0D-6 / REAL( PRECURSOR_MW( HCL_IDX ), 8 ) 
     &        + ( AEROSPC_CONC( ACL_IDX,1 ) + AEROSPC_CONC( ACL_IDX,2 ) )
     &        * 1.0D-6 / REAL( AEROSPC_MW( ACL_IDX ), 8 )

      CNTRL( 1 ) = 0.0D0   ! Forward Problem
      CNTRL( 2 ) = 1.0D0   ! Aerosol in Metastable State

C *** Set flags to account for mass conservation violations in ISRP3F
      TRUSTCL  = .TRUE.
      TRUSTNH4 = .TRUE.
      IF ( WI( 1 ) + WI( 5 ) .LT. 1.0D-20 ) THEN
         TRUSTCL = .FALSE.
      ELSE
         IF ( WI( 3 ) .LT. 1.0D-10 ) TRUSTNH4 = .FALSE.
         IF ( WI( 5 ) .LT. 1.0D-10 ) TRUSTCL  = .FALSE.
      END IF
         
#ifndef noiso
      CALL ISOROPIA( WI, RHI, TEMPI, CNTRL, WT, GAS, AERLIQ,
     &               AERSLD, SCASI, OTHER )

C *** Update gas-phase concentrations
      PRECURSOR_CONC( NH3_IDX )  = GAS( 1 ) * 1.0D6 * REAL( PRECURSOR_MW( NH3_IDX ), 8 )
      PRECURSOR_CONC( HNO3_IDX ) = GAS( 2 ) * 1.0D6 * REAL( PRECURSOR_MW( HNO3_IDX ), 8 )
      PRECURSOR_CONC( HCL_IDX )  = GAS( 3 ) * 1.0D6 * REAL( PRECURSOR_MW( HCL_IDX ), 8 )
#else
      gas( 1 ) = real( precursor_conc( nh3_idx ),  8 ) * 1.0d-6 / real( precursor_mw( nh3_idx ), 8 )
      gas( 2 ) = real( precursor_conc( hno3_idx ), 8 ) * 1.0d-6 / real( precursor_mw( hno3_idx ), 8 )
      gas( 3 ) = real( precursor_conc( hcl_idx ),  8 ) * 1.0d-6 / real( precursor_mw( hcl_idx ), 8 )
#endif

C *** Change in volatile inorganic PM concentration to achieve
C     equilibrium, calculated as initial-gas-phase concentration minus 
C     equilibrium gas-phase concentration. DVOLINORG is positive for
C     condensation and negative for evaporation.
      DVOLINORG( KNH4 ) = GNH3R8 * 1.0D-6 / REAL( PRECURSOR_MW( NH3_IDX ), 8 ) - GAS( 1 )
      DVOLINORG( KNO3 ) = GNO3R8 * 1.0D-6 / REAL( PRECURSOR_MW( HNO3_IDX ), 8 ) - GAS( 2 )

      IF ( TRUSTCL ) THEN  
         DVOLINORG( KCL )  = GCLR8  * 1.0D-6 / REAL( PRECURSOR_MW( HCL_IDX ), 8 ) - GAS( 3 ) 
      ELSE
         DVOLINORG( KCL ) = 0.0D0
      END IF

      IF ( DVOLINORG( KNH4 ) .LT. 0.0D0 ) THEN
         DVOLMAX = -( REAL( AEROSPC_CONC( ANH4_IDX,1 ), 8 )
     &              + REAL( AEROSPC_CONC( ANH4_IDX,2 ), 8 ) ) / DF( KNH4 )
         DVOLINORG( KNH4 ) = MAX( DVOLINORG( KNH4 ), DVOLMAX )
      END IF

      IF ( DVOLINORG( KNO3 ) .LT. 0.0D0 ) THEN
         DVOLMAX = -( REAL( AEROSPC_CONC( ANO3_IDX,1 ), 8 )
     &              + REAL( AEROSPC_CONC( ANO3_IDX,2 ), 8 ) ) / DF( KNO3 )
         DVOLINORG( KNO3 ) = MAX( DVOLINORG( KNO3 ), DVOLMAX)
      END IF

      IF ( DVOLINORG( KCL ) .LT. 0.0D0 ) THEN
         DVOLMAX = -( REAL( AEROSPC_CONC( ACL_IDX,1 ), 8 )
     &              + REAL( AEROSPC_CONC( ACL_IDX,2 ), 8 ) ) / DF( KCL )
         DVOLINORG( KCL ) = MAX( DVOLINORG( KCL ), DVOLMAX )
      END IF

      OMEGA( 1 ) = GRFAC2( 1 ) / ( GRFAC2( 1 ) + GRFAC2( 2 ) ) ! partitioning coeff
      OMEGA( 2 ) = 1.0D0 - OMEGA( 1 )

C *** Initialize excess evaporated mass array
      DMASS = 0.0D0

      DO IMODE = 1, 2  ! modal partitioning of equilibrium aerosol mass

         CINORG( KSO4,IMODE ) = AEROSPC_CONC( ASO4_IDX, IMODE )
         CINORG( KNH4,IMODE ) = AEROSPC_CONC( ANH4_IDX, IMODE )
         CINORG( KNO3,IMODE ) = AEROSPC_CONC( ANO3_IDX, IMODE )
         CINORG( KNA, IMODE ) = AEROSPC_CONC( ANA_IDX, IMODE )
         CINORG( KCL, IMODE ) = AEROSPC_CONC( ACL_IDX, IMODE )

         WETM3 = MOMENT3_CONC( IMODE )
         WETM2 = MOMENT2_CONC( IMODE )

         IF ( IMODE .EQ. 1 ) THEN
            RATE = DMDT_SO4 + CONDSO4( IMODE )
         ELSE
            RATE = CONDSO4( IMODE )
         END IF

         M3OTHR = 0.0
         M3SOA = 0.0
         DO I = 1, N_AEROSPC

            ! Skip organic species
            IF ( I .EQ. ASO4_IDX .OR. I .EQ. ANH4_IDX .OR. I .EQ. ANO3_IDX .OR.
     &           I .EQ. ANA_IDX  .OR. I .EQ. ACL_IDX  .OR. I .EQ. AH2O_IDX ) CYCLE

            IF ( AEROSPC(I)%isWet ) THEN
               M3SOA = M3SOA 
     &               + ( 1.0D-9 * F6DPI / AEROSPC( I )%DENSITY )
     &               * AEROSPC_CONC( I,IMODE )
            ELSE
               M3OTHR = M3OTHR
     &                + ( 1.0D-9 * F6DPI / AEROSPC( I )%DENSITY )
     &                * AEROSPC_CONC( I,IMODE )
            ENDIF   

         END DO    ! N_AEROSPC loop

         DRYM3 = WETM3 - H2OFAC * REAL( AEROSPC_CONC( AH2O_IDX,IMODE ), 8 ) - M3SOA
         DRYM20 = WETM2 * ( DRYM3 / WETM3 ) ** D_TWOTHIRDS

         DO ISP = 1, NVOLINORG
         
            CFINAL( ISP,IMODE ) = CINORG( ISP,IMODE )
     &                          + OMEGA( IMODE ) * DVOLINORG( ISP )
     &                          * DF( ISP )

            IF ( IMODE .EQ. 1 ) THEN

               IF ( CFINAL( ISP,IMODE ) .LT. 0.0D0 ) THEN
                  DMASS( ISP) = CFINAL( ISP,IMODE )
                  CFINAL( ISP,IMODE ) = 0.0D0
               END IF

            ELSE

               CFINAL( ISP,IMODE ) = CFINAL( ISP,IMODE ) + DMASS( ISP )

               IF ( CFINAL( ISP,IMODE ) .LT. 0.0D0 ) THEN
                  CFINAL( ISP,1 ) = CFINAL( ISP,1 ) + CFINAL( ISP,IMODE ) 
                  CFINAL( ISP,IMODE ) = 0.0D0

                  IF ( CFINAL( ISP, 1 ) .LT. 0.0D0 ) THEN
                     IF ( ABS( CFINAL( ISP,1 ) ) .LT. 1.0D-15 ) THEN
                        CFINAL( ISP,1 ) = 0.0D0
                     ELSE
                        WRITE( LOGDEV,* ) 'VOLINORG: Too much evaporation'
                        CFINAL( ISP,1 ) = 0.0D0
!                       STOP
                     END IF
                  END IF 
               END IF

            END IF   ! IMODE = 1
            
         END DO 
                        
         SO4 = CINORG( KSO4,IMODE ) + RATE * CONVFAC

C *** Double precision vars for ISORROPIA

         WI( 1 ) = CINORG( KNA, IMODE ) * 1.0D-6 / REAL( AEROSPC_MW( ANA_IDX ), 8 )
         WI( 2 ) =                  SO4 * 1.0D-6 / REAL( AEROSPC_MW( ASO4_IDX ), 8 )
         WI( 3 ) = CFINAL( KNH4,IMODE ) * 1.0D-6 / REAL( AEROSPC_MW( ANH4_IDX ), 8 )
         WI( 4 ) = CFINAL( KNO3,IMODE ) * 1.0D-6 / REAL( AEROSPC_MW( ANO3_IDX ), 8 )
         WI( 5 ) = CFINAL( KCL, IMODE ) * 1.0D-6 / REAL( AEROSPC_MW( ACL_IDX ), 8 )

!        write( logdev,* ) wi( 1 ), wi( 2 ), wi( 3 ), wi( 4 ), wi( 5 )
!        write( logdev,* ) '===', rhi, tempi

         CALL CALC_H2O( WI, RHI, TEMPI, H2O_NEW ) 
            
         H2O = H2O_NEW * DFH2OR8 

C *** Compute net change in 3rd moment due to dry inorganic mass transfer
C     (includes nucleated sulfate mass)

         DDRYM3DT = (
     &                ( CFINAL( KNH4,IMODE ) - CINORG( KNH4,IMODE ) )
     &                  * ( 1.0D-9 * F6DPI / REAL( AEROSPC( ANH4_IDX )%DENSITY, 8 ) )
     &              + ( CFINAL( KNO3,IMODE ) - CINORG( KNO3,IMODE ) )
     &                  * ( 1.0D-9 * F6DPI / REAL( AEROSPC( ANO3_IDX )%DENSITY, 8 ) )
     &              + ( CFINAL( KCL, IMODE ) - CINORG( KCL,IMODE ) )
     &                  * ( 1.0D-9 * F6DPI / REAL( AEROSPC( ACL_IDX )%DENSITY, 8 ) )
     &              + ( SO4                  - CINORG( KSO4,IMODE ) )
     &                  * ( 1.0D-9 * F6DPI / REAL( AEROSPC( ASO4_IDX )%DENSITY, 8 ) ) ) / DELT 

C *** Compute net change in 2nd moment due to dry inorganic mass transfer
C     (including nucleation) using equation A7 of Binkowski & Shankar (1995)
         DDRYM2DT = D_TWOTHIRDS * GRFAC1( IMODE ) / GRFAC2( IMODE ) * DDRYM3DT

C *** Update dry 2nd moment for condensation/evaporation based on whether
C     net change in dry 2nd moment is production or loss
         IF ( DDRYM2DT .LT. 0.0D0 ) THEN
            LOSS = DDRYM2DT / DRYM20
            Y = DRYM20 * EXP( LOSS * DELT )
         ELSE
            Y = DRYM20 + DDRYM2DT * DELT
         END IF
 
C *** Add water and SOA to 2nd moment while preserving standard deviation
         DRYM3 = ( 1.0D-9 * F6DPI / REAL( AEROSPC( ASO4_IDX )%DENSITY, 8 ) ) * SO4
     &         + ( 1.0D-9 * F6DPI / REAL( AEROSPC( ANH4_IDX )%DENSITY, 8 ) ) * CFINAL( KNH4,IMODE )
     &         + ( 1.0D-9 * F6DPI / REAL( AEROSPC( ANO3_IDX )%DENSITY, 8 ) ) * CFINAL( KNO3,IMODE )
     &         + ( 1.0D-9 * F6DPI / REAL( AEROSPC( ACL_IDX )%DENSITY, 8 ) )  * CFINAL( KCL, IMODE )
     &         + ( 1.0D-9 * F6DPI / REAL( AEROSPC( ANA_IDX )%DENSITY, 8 ) )  * CINORG( KNA, IMODE )
     &         + M3OTHR

         WETM3 = DRYM3 + M3SOA + REAL( H2OFAC, 8 ) * H2O 
         DRYM2 = MAX( REAL( AEROMODE( IMODE )%MIN_M2, 8 ), Y )
         WETM2 = DRYM2 * ( WETM3 / DRYM3 ) ** D_TWOTHIRDS

         AEROSPC_CONC( ANH4_IDX, IMODE ) = CFINAL( KNH4,IMODE )
         AEROSPC_CONC( ANO3_IDX, IMODE ) = CFINAL( KNO3,IMODE )
         AEROSPC_CONC( ACL_IDX, IMODE )  = CFINAL( KCL ,IMODE )
         AEROSPC_CONC( ASO4_IDX, IMODE ) = SO4
         AEROSPC_CONC( AH2O_IDX, IMODE ) = H2O
         IF ( IMODE .EQ. 1 ) MOMENT0_CONC( IMODE ) = AM0(IMODE) + DNDT * DELT
         MOMENT2_CONC( IMODE ) = WETM2

         HPLUS( IMODE ) = 0.0
         DO I = 1, N_AEROSPC
           HPLUS( IMODE ) = HPLUS( IMODE )
     &                    + AEROSPC( I )%CHARGE * AEROSPC_CONC( I,IMODE ) / AEROSPC_MW( I )
         END DO

      END DO    ! end fine mode mass transfer calculations

C *** Update the third moments, geometric mean diameters, geometric 
C     standard deviations, modal mass totals, and modal particle 
C     densities.
      CALL GETPAR( M3_WET_FLAG, LIMIT_Sg )

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
      SUBROUTINE NEWPART3 ( RH, Temp, XH2SO4, SO4RATE,
     &                      DNDT, DMDT_so4, DM2DT )
C    calculates the new particle production rate due to binary nucleation
C    of H2SO4 and H2O vapor.  The nucleation rate is a parameterized function
C    of temperature, relative humidity, and the vapor-phase H2SO4 concentration,
C    following the work of Kulmala et al (1998).  This rate is then used to
C    determine the production rates of aerosol number, 2nd moment, and aerosol
C    sulfate, following the description in Section 1.2 of Binkowski &
C    Roselle (2003), except the new particles are assumed to be of 2.0
C    nm diameter instead of 3.5 nm.
 
C  Key Subroutines/Functions called: none
 
C  REVISION HISTORY:
C FSB 11/29/99 Extracted code from PARTPROD_VA subroutine of RPM
C SJR ??/??/?? New call vector and limited the mass production rate
C FSB 04/11/02 Decreased the diameter of new particles from 3.5 nm to
C     2 nm.  New particles are monodispersed.
C PVB 09/21/04 Changed MWH2SO4 from 98.07354 to 98.0 g/mol for
C     consistency with the mechanism files.  Added in-line documentation
C     with input from FSB.
 
C  References:
C   1. Kulmala, M., A. Laaksonen, and L. Pirjola, Parameterizations for
C      sulfuric acid/water nucleation rates.  J. Geophys. Res., Vol 103,
C      No D7, 8301-8307, 1998.
C   2. Binkowski, F.S. and S.J. Roselle, Models-3 Community
C      Multiscale Air Quality (CMAQ) model aerosol component 1:
C      Model Description.  J. Geophys. Res., Vol 108, No D6, 4183
C      doi:10.1029/2001JD001409, 2003.

      USE AERO_DATA
      USE MET_DATA

      IMPLICIT NONE

C *** Arguments:

      REAL, INTENT( IN ) :: RH         ! fractional relative humidity
      REAL, INTENT( IN ) :: Temp       ! ambient temperature [ K ]
      REAL, INTENT( IN ) :: XH2SO4     ! sulfuric acid concentration [ ug/m**3 ]
      REAL, INTENT( IN ) :: SO4RATE    ! gas-phase H2SO4 production rate [ ug/m**3 s ]

      REAL( 8 ), INTENT( OUT ) :: DNDT     ! particle number production rate [ m^-3/s ]
      REAL( 8 ), INTENT( OUT ) :: DMDT_so4 ! SO4 mass production rate [ ug/m**3 s ]
      REAL( 8 ), INTENT( OUT ) :: DM2DT    ! second moment production rate [ m**2/m**3 s ]

C *** Parameters
      CHARACTER( 16 ), PARAMETER :: PNAME = 'NEWPART'

C *** Conversion Factors:
      REAL, PARAMETER :: MWH2SO4 = 98.0 ! molecular weight for H2SO4
      REAL, PARAMETER :: ugm3_ncm3 = (AVO / MWh2so4) * 1.0e-12 ! micrograms/m**3 to number/cm**3

C *** Particle size parameters:
      REAL, PARAMETER :: d20 = 2.0E-07            ! diameter of a new particle [cm]
      REAL, PARAMETER :: d20sq = d20 * d20        ! new-particle diameter squared [cm**2]
      REAL, PARAMETER :: m2_20 = 1.0E-4 * d20sq   ! new-particle diameter squared [m**2]
      REAL, PARAMETER :: v20 = PI6 * d20 * d20sq  ! volume of a new particle [cm**3]

      REAL( 8 ) :: sulfmass                       ! mass of a new particle [ug]
      REAL( 8 ) :: sulfmass1                      ! inverse of sulfmass [ug**-1]

C *** Local Variables used to determine nucleation rate

      REAL Nwv     ! water vapor concentration [ 1/cm**3 ]
      REAL Nav0    ! saturation vapor conc of H2SO4 [ 1/cm**3 ]
      REAL Nav     ! H2SO4 vapor concentration [ 1/cm**3 ]
      REAL Nac     ! critical H2SO4 vapor concentration [ 1/cm**3 ]
      REAL RA      ! fractional relative acidity
      REAL delta   ! temperature-correction term
      REAL Xal     ! H2SO4 mole fraction in the critical nucleus
      REAL Nsulf   ! see usage
      REAL( 8 ) :: chi   ! factor to calculate Jnuc
      REAL( 8 ) :: JnucK ! nucleation rate [ cm ** -3  s ** -1 ]
                   ! (Kulmala et al.)
      REAL tt      ! dummy variable for statement functions

C *** Statement Functions
      REAL ph2so4, ph2o ! arithmetic statement functions for saturation
                        ! vapor pressures of h2so4 and h2o [Pa] taken
                        ! from Appendix of Kulmala et al. (1998), p8306

      ph2so4( tt ) = EXP( 27.78492066 - 10156.0 / tt )

      ph2o( tt )   = EXP( 77.34491296 -7235.424651 / tt
     &                   - 8.2 * LOG( tt ) + tt * 5.7113E-03 )

C-----------------------------------------------------------------------

C *** Initialize variables
      DNDT     = 0.0D0
      DMDT_so4 = 0.0D0
      DM2DT    = 0.0D0

      sulfmass = 1.0D+3 * aerospc( ASO4_IDX )%density * v20
      sulfmass1 = 1.0D0 / sulfmass

C *** Nucleation Rate
C     Calculate the sulfuric acid/water nucleation rate.  This code
C     implements Section 3 of Kulmala et al. (1998).  Note that all
C     variables in the Kulmala parameterization are in cgs units.

C *** Calculate water vapor concentration [1/cm**3] using ambient RH
C     and the formula in Appendix of Kulmala et al. (1998), p8306.
      Nwv = RH * ph2o( Temp ) / ( RGASUNIV * Temp ) * AVO * 1.0E-6

C *** Calculate saturation vapor concentration of H2SO4 [1/cm**3]
C       using formula in the Appendix of Kulmala et al. (1998), p8306.
      Nav0 = ph2so4( Temp ) / ( RGASUNIV * Temp ) * AVO * 1.0E-6

C *** Convert ambient H2SO4 vapor concentration into [1/cm**3] units
      Nav = ugm3_ncm3 * XH2SO4

C *** Calculate critical concentration of H2SO4 vapor needed to produce
C     1 particle/(cm**3 s) using Equation 18 of Kulmala et al (1998)
      Nac = EXP( - 14.5125 + 0.1335 * Temp
     &           - 10.5462 * RH + 1958.4 * RH  / Temp )

C *** Calculate relative acidity, defined as the ambient concentration
C     divided by the saturation concentration of H2SO4 vapor
      RA = Nav / Nav0

C *** Calculate temperature correction factor using Equation 22 of
C      Kulmala et al (1998)
      delta = 1.0 + ( Temp - 273.15 ) / 273.15

C *** Calculate mole fraction of H2SO4 in the critical nucleus using
C     Equation 17 of Kulmala et al (1998)
      Xal = 1.2233 - 0.0154 * RA / ( RA + RH ) + 0.0102 * log( Nav )
     &    - 0.0415 * LOG( Nwv ) + 0.0016 * Temp

C *** Calculate Nsulf as defined in Equation 21 of Kulmala et al (1998)
      Nsulf = LOG( Nav / Nac )

C *** Calculate natural log of nucleation rate using Equation 20 of
C     Kulmala et al (1998)
      chi = 25.1289 * Nsulf - 4890.8 * Nsulf / Temp
     &    - 1743.3 / Temp - 2.2479 * delta * Nsulf * RH
     &    + 7643.4 * Xal / Temp
     &    - 1.9712 * Xal * delta / RH

C *** Calculate nucleation rate using Eq 19 of Kulmala et al (1998)
      JnucK = Exp( chi )   ! [ #/cm**3 s ]

C *** Moment Production Rates
C     Calculate the production rate of number, sulfate mass, and 2nd
C     moment, due to the H2SO4/H2O nucleation assuming that each critical
C     nucleus grows instantaneously to 2.0 nm.  This code follows Section
C     1.2 of Binkowski & Roselle (2003), except the assumed particle
C     diameter has been changed from 3.5 to 2.0 nm.

C *** Convert production rate of number to [ #/m**3 s]
      DNDT = 1.0E06 * JnucK

C *** Calculate mass production rate [ ug / (m**3 s) ] analogous to
C     Equation 6a of Binkowski & Roselle (2003).  Set the upper limit
C     of the mass production rate as the gas-phase production rate of
C     H2SO4, and adjust the number production rate accordingly.
      DMDT_so4 = sulfmass * DNDT
      IF ( DMDT_so4 .GT. SO4RATE ) THEN
         DMDT_so4 = SO4RATE
         DNDT = DMDT_SO4 * sulfmass1
      END IF

C *** Calculate the production rate of 2nd moment [ m**2 / (m**3 s) ]
C     This is similar to Equation 6c of Binkowski & Roselle (2003),
C     except the factor of PI is removed and the assumed particle
C     diameter is different.
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
      USE MET_DATA

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
      Cinf( KNH4 ) = GNH3R8 * 1.0D-6 / REAL( PRECURSOR_MW( NH3_IDX ), 8 )
      Cinf( KNO3 ) = GNO3R8 * 1.0D-6 / REAL( PRECURSOR_MW( HNO3_IDX ), 8 )
      Cinf( KCL )  = GCLR8  * 1.0D-6 / REAL( PRECURSOR_MW( HCL_IDX ), 8 )

C     Calculate cond/evap fluxes (no H+ limiting)
      DO isp = 1, nvolinorg
         J( isp ) = CondRate * ( Cinf( isp ) - Ceq( isp ) )
      END DO

C     Convert rate to mol/m3/s and get effective Cinf for H2SO4(g)
      JH2SO4  = rate * 1.0D-6 / REAL( PRECURSOR_MW( SULPRD_IDX ), 8 )
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

      INTEGER, PARAMETER :: NCMP = 5
C     Arguments:
      REAL( 8 ), INTENT( IN )  :: WI( NCMP )
      REAL( 8 ), INTENT( IN )  :: RH, T
      REAL( 8 ), INTENT( OUT ) :: H2O_NEW

C     Parameters:
      INTEGER, PARAMETER :: NPAIR = 13
      REAL( 8 ), PARAMETER :: SMALL = 1.0D-20
      REAL( 8 ), PARAMETER :: Mw = 0.018D0  ! molar mass H2O (kg/mol)

C     Local Variables:
      REAL( 8 ) :: FSO4, FNH4, FNA, FNO3, FCL ! "free" ion amounts
      REAL( 8 ) :: WATER           ! kg of water for new time step
      REAL( 8 ) :: X, Y
      REAL( 8 ) :: CONC ( NCMP )   ! concentration (mol/m^3)
      REAL( 8 ) :: CONCR( NPAIR )  ! concentration (mol/m^3) ion "pairs"
      REAL( 8 ) :: M0I  ( NPAIR )  ! single-solute molalities
      INTEGER :: J
      CHARACTER( 2 ) :: SC         ! sub-case for composition

C-----------------------------------------------------------------------

C     Return if small concentration
      IF ( WI( 1 ) + WI( 2 ) + WI( 3 ) + WI( 4 ) + WI( 5 ) .LE. SMALL ) THEN
         H2O_NEW = SMALL
         RETURN
      END IF

C     Set component array (mol/m^3) for determining salts
      CONC = WI   ! array assignment

C     Get the sub-case to use in determining salts
      CALL GETSC ( CONC, RH, T, SC )

C     Initialize ion "pairs" (i.e., salts) used in ZSR
      CONCR = 0.0D0   ! array assignment

C     Depending on case, determine moles of salts in solution (i.e., CONCR)
C     for ZSR calculation below

      IF ( SC .EQ. 'K2' ) THEN    ! sulfate poor (NH4-SO4 system)
         CONCR( 4 ) = MIN( CONC( 2 ), 0.5D0 * CONC( 3 ) )  ! (NH4)2SO4

      ELSE IF ( SC .EQ. 'L4' .OR. SC .EQ. 'O4' ) THEN  ! sulfate rich (no acid)
         X = 2.0D0 * CONC( 2 ) - CONC( 3 )   ! 2SO4 - NH4
         Y = CONC( 3 ) - CONC( 2 )           ! NH4 - SO4
         IF ( X .LE. Y ) THEN
            CONCR( 13 ) = X                  ! (NH4)3H(SO4)2 is MIN (X,Y)
            CONCR( 4 )  = Y - X              ! (NH4)2SO4
         ELSE
            CONCR( 13 ) = Y                  ! (NH4)3H(SO4)2 is MIN (X,Y)
            CONCR( 9 )  = X - Y              ! NH4HSO4
         END IF

      ELSE IF ( SC .EQ. 'M2' .OR. SC .EQ. 'P2' ) THEN   ! sulfate rich (free acid)
         CONCR( 9 ) = CONC( 3 )                                    ! NH4HSO4
         CONCR( 7 ) = MAX( CONC( 2 )-CONC( 3 ), 0.0D0 )            ! H2SO4

      ELSE IF ( SC .EQ. 'N3' ) THEN            ! sulfate poor (NH4-SO4-NO3 system)
         CONCR( 4 ) = MIN( CONC( 2 ), 0.5D0 * CONC( 3 ) )          ! (NH4)2SO4
         FNH4       = MAX( CONC( 3 ) - 2.0D0 * CONCR( 4 ), 0.0D0 ) ! available NH4
         CONCR( 5 ) = MAX( MIN( FNH4, CONC( 4 ) ), 0.0D0 )         ! NH4NO3=MIN(NH4,NO3)

      ELSE IF ( SC .EQ. 'Q5' ) THEN    ! sulfate poor, sodium poor (NH4-SO4-NO3-Cl-Na)
         CONCR( 2 ) = 0.5D0 * CONC( 1 )                            ! Na2SO4
         FSO4       = MAX( CONC( 2 ) - CONCR( 2 ), 0.0D0 )         ! available SO4
         CONCR( 4 ) = MAX( MIN( FSO4, 0.5D0 * CONC( 3 ) ), SMALL ) ! NH42S4=MIN(NH4,S4)
         FNH4       = MAX( CONC( 3 ) - 2.0D0 * CONCR( 4 ), 0.0D0 ) ! available NH4
         CONCR( 5 ) = MIN( FNH4, CONC( 4 ) )                       ! NH4NO3=MIN(NH4,NO3)
         FNH4       = MAX( FNH4 - CONCR( 5 ), 0.0D0 )              ! avaialable NH4
         CONCR( 6 ) = MIN( FNH4, CONC( 5 ) )                       ! NH4Cl=MIN(NH4,Cl)

      ELSE IF ( SC .EQ. 'R6' ) THEN   ! sulfate poor, sodium rich (NH4-SO4-NO3-Cl-Na)
         CONCR( 2 ) = CONC( 2 )                                    ! Na2SO4
         FNA        = MAX( CONC( 1 ) - 2.0D0 * CONCR( 2 ), 0.0D0 )

         CONCR( 3 ) = MIN( FNA, CONC( 4 ) )                        ! NaNO3
         FNO3       = MAX( CONC( 4 ) - CONCR( 3 ), 0.0D0 )
         FNA        = MAX( FNA-CONCR( 3 ), 0.0D0 )

         CONCR( 1 ) = MIN( FNA, CONC( 5 ) )                        ! NaCl
         FCL        = MAX( CONC( 5 ) - CONCR( 1 ), 0.0D0 )
         FNA        = MAX( FNA-CONCR( 1 ), 0.0D0 )

         CONCR( 5 ) = MIN( FNO3, CONC( 3 ) )                       ! NH4NO3
         FNO3       = MAX( FNO3-CONCR( 5 ), 0.0D0 )
         FNH4       = MAX( CONC( 3 ) - CONCR( 5 ), 0.0D0 )

         CONCR( 6 ) = MIN( FCL, FNH4 )                             ! NH4Cl

      ELSE IF ( SC .EQ. 'S6' ) THEN       ! sulfate rich (no acid) (NH4-SO4-NO3-Cl-Na)
         CONCR( 2 )  = 0.5D0 * CONC( 1 )                           ! Na2SO4
         FSO4        = MAX( CONC( 2 ) - CONCR( 2 ), 0.0D0 )
         CONCR( 13 ) = MIN( CONC( 3 ) / 3.0D0, FSO4 / 2.0D0 )      ! (NH4)3H(SO4)2
         FSO4        = MAX( FSO4 - 2.0D0 * CONCR( 13 ), 0.0D0 )
         FNH4        = MAX( CONC( 3 ) - 3.0D0 * CONCR( 13 ), 0.0D0 )

         IF ( FSO4 .LE. SMALL ) THEN          ! reduce (NH4)3H(SO4)2, add (NH4)2SO4
            CONCR( 13 ) = MAX( CONCR( 13 ) - FNH4, 0.0D0 )         ! (NH4)3H(SO4)2
            CONCR( 4 )  = 2.0D0 * FNH4                             ! (NH4)2SO4
         ELSE IF ( FNH4 .LE. SMALL ) THEN     ! reduce (NH4)3H(SO4)2, add NH4HSO4
            CONCR( 9 )  = 3.0D0 * MIN( FSO4, CONCR( 13 ) )         ! NH4HSO4
            CONCR( 13 ) = MAX( CONCR( 13 ) - FSO4, 0.0D0 )
            IF ( CONCR( 2 ) .GT. SMALL ) THEN ! reduce Na2SO4, add NaHSO4
               FSO4      = MAX( FSO4 - CONCR( 9 ) / 3.0D0, 0.0D0 )
               CONCR( 12 ) = 2.0D0 * FSO4                          ! NaHSO4
               CONCR( 2 )  = MAX( CONCR( 2 ) - FSO4, 0.0D0 )       ! Na2SO4
            END IF
         END IF

      ELSE IF ( SC .EQ. 'T3' ) THEN      ! sulfate rich (free acid) (NH4-SO4-NO3-Cl-Na)
         CONCR( 9 )  = CONC( 3 )                                   ! NH4HSO4
         CONCR( 12 ) = CONC( 1 )                                   ! NAHSO4
         CONCR( 7 )  = MAX( CONC( 2 ) - CONC( 3 ) - CONC( 1 ), 0.0D0 ) ! H2SO4

      ELSE
        PRINT*, 'aero_subs.f: case not supported ',
     &          '(metastable reverse only)'
!       STOP
      END IF

#ifndef noiso
C     Get single-solute molalities for ZSR calculation
      CALL GETM0I ( M0I )
#endif

C     Calculate H2O with ZSR and determine delta water
      WATER = 0.0D0
#ifndef noiso
      DO J = 1, NPAIR
         WATER = WATER + CONCR( J ) / M0I( J )
      END DO
#endif

      WATER = MAX ( WATER, SMALL )
      H2O_NEW = WATER / Mw

      END SUBROUTINE CALC_H2O

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE GETM0I ( M0I )

!!!!!!!!!!!!!!! We want to get rid of this dependency !!!!!!!!!!!!!!!!

C Description
C   Determines single-solute molalities for the 13 possible salts at
C   the ambient RH.  These molalities are used in the ZSR calculation
C   in CALC_H2O. Note that the molalities were determined at the beginning
C   of the time step, and so they are available in the IONS common block
C   of isrpia.inc.
C   Routine called by CALC_H2O.
 
C Arguments
C   Output
C     M0I : Single-solute molalities (mol/kg-H2O) for 13 salts
 
C-----------------------------------------------------------------------

!     implicit none

      INCLUDE 'isrpia.inc'

C     Arguments:
      REAL( 8 ), INTENT( OUT ) :: M0I( NPAIR )

      M0I = M0    ! Array Assignment

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

C     Parameters:
      INTEGER, PARAMETER :: NCMP  = 5    ! number of aerosol components
      REAL( 8 ), PARAMETER :: SMALL = 1.0D-20

C     Arguments:
      REAL( 8 ), INTENT( INOUT )    :: CONC( NCMP )
      REAL( 8 ), INTENT( IN )       :: RH, T
      CHARACTER( 2 ), INTENT( OUT ) :: SC

C     Local Variables:
      REAL( 8 ) :: T0, TCF                     ! DRH(T) factor
      REAL( 8 ) :: S4RAT, S4RATW, NaRAT, SRI   ! sulfate & sodium ratios
      REAL( 8 ) :: FSO4                        ! "free" sulfate
      REAL( 8 ) :: DNACL, DNH4CL, DNANO3, DNH4NO3, DNH42S4 ! DRH values

      REAL( 8 ) :: GETASR    ! ISORROPIA function for sulfate ratio

      LOGICAL :: NH4S4, NH4S4N3, ALLSP  ! concentration regime

C-----------------------------------------------------------------------

      NH4S4   = .FALSE.  ! NH4-SO4
      NH4S4N3 = .FALSE.  ! NH4-SO4-NO3
      ALLSP   = .FALSE.  ! all species

C     See if any components are negligible
      IF ( CONC( 1 ) + CONC( 4 ) + CONC( 5 ) .LE. SMALL ) THEN   ! Na,Cl,NO3=0
         NH4S4 = .TRUE.
      ELSE IF ( CONC( 1 ) + CONC( 5 ) .LE. SMALL ) THEN        ! Na,Cl=0
         NH4S4N3 = .TRUE.
      ELSE
         ALLSP = .TRUE.
      END IF

      CONC( : ) = MAX( CONC( : ), SMALL )

C     Deliquescence RH calculations
      DNH42S4 = 0.7997D0
      DNH4NO3 = 0.6183D0
      IF ( INT( T ) .NE. 298 ) THEN
         T0      = 298.15D0
         TCF     = 1.0D0 / T - 1.0D0 / T0
         DNH4NO3 = DNH4NO3 * EXP( 852.0D0 * TCF )
         DNH42S4 = DNH42S4 * EXP(  80.0D0 * TCF )
         DNH4NO3 = MIN( DNH4NO3, DNH42S4 ) ! adjust for curves crossing T<271K
      END IF

C     Find sub-case "SC"
      IF ( NH4S4 ) THEN ! NH4-S04 system

         IF ( RH .GE. DNH42S4 ) THEN
            S4RATW = GETASR( CONC( 2 ), RH ) ! aerosol sulfate ratio
         ELSE
            S4RATW = 2.0D0                   ! dry aerosol sulfate ratio
         END IF
         S4RAT  = CONC( 3 ) / CONC( 2 )      ! sulfate ratio (NH4/SO4)

         IF ( S4RATW .LE. S4RAT ) THEN
            SC = 'K2'                        ! sulfate poor
         ELSE IF ( 1.0D0 .LE. S4RAT .AND. S4RAT .LT. S4RATW ) THEN
            SC = 'L4'                        ! sulfate rich (no acid)
         ELSE IF ( S4RAT .LT. 1.0D0 ) THEN
            SC = 'M2'                        ! sulfate rich (free acid)
         END IF

      ELSE IF ( NH4S4N3 ) THEN ! NH4-SO4-NO3 system

         IF ( RH .GE. DNH4NO3 ) THEN
            S4RATW = GETASR( CONC( 2 ), RH )
         ELSE
            S4RATW = 2.0D0                   ! dry aerosol ratio
         END IF
         S4RAT = CONC( 3 ) / CONC( 2 )

         IF ( S4RATW .LE. S4RAT ) THEN
            SC = 'N3'                        ! sulfate poor
         ELSE IF ( 1.0D0 .LE. S4RAT .AND. S4RAT .LT. S4RATW ) THEN
            SC = 'O4'                        ! sulfate rich (no acid)
         ELSE IF ( S4RAT .LT. 1.0D0 ) THEN
            SC = 'P2'                        ! sulfate rich (free acid)
         END IF

      ELSE IF ( ALLSP )  THEN ! all species

C     Adjust DRH of NH4NO3 for low temperature
         DNACL   = 0.7528D0
         DNANO3  = 0.7379D0
         DNH4CL  = 0.7710D0
         IF ( INT( T ) .NE. 298 ) THEN
            DNACL   = DNACL  * EXP(  25.0D0 * TCF )
            DNANO3  = DNANO3 * EXP( 304.0D0 * TCF )
            DNH4CL  = DNH4Cl * EXP( 239.0D0 * TCF )
            DNH4NO3 = MIN( DNH4NO3, DNH4CL, DNANO3, DNACL )
         END IF

        IF ( RH .GE. DNH4NO3 ) THEN
           FSO4   = CONC( 2 ) - CONC( 1 ) / 2.0D0   ! sulfate unbound by Na+
           FSO4   = MAX( FSO4, SMALL )
           SRI    = GETASR( FSO4, RH )
           S4RATW = ( CONC( 1 ) + FSO4 * SRI ) / CONC( 2 )
           S4RATW = MIN( S4RATW, 2.0D0 )
        ELSE
           S4RATW = 2.0D0                    ! ratio for dry aerosol
        END IF
        S4RAT = ( CONC( 1 ) + CONC( 3 ) ) / CONC( 2 )
        NaRAT = CONC( 1 ) / CONC( 2 )

        IF ( S4RATW .LE. S4RAT .AND. NaRAT .LT. 2.0D0 ) THEN
           SC = 'Q5'                         ! sulfate poor, sodium poor
        ELSE IF ( S4RAT .GE. S4RATW .AND. NaRAT .GE. 2.0D0 ) THEN
           SC = 'R6'                         ! SO4 poor, Na rich
        ELSE IF ( 1.0D0 .LE. S4RAT .AND. S4RAT .LT. S4RATW ) THEN
           SC = 'S6'                         ! SO4 rich, no acid
        ELSE IF ( S4RAT .LT. 1.0D0 ) THEN
           SC = 'T3'                         ! sulfate rich, free acid
        END IF

      END IF

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
      SUBROUTINE INLET25 ( DGN, XXLSG, RHOP, INFRAC )

C  Calculates the volume fraction of a given aerosol mode that would enter
C  a sharp-cut PM2.5 inlet, using equations from Jiang et al (2005).
 
C  Key Subroutines called: none
 
C  Key Functions called:  ERF
 
C  Revision History:
C    Coded Jul 2005 by Prakash Bhave
C          Apr 2008 J.Kelly: corrected equation for Dst25 calculation
 
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

C    Input variables
      REAL   DGN     ! geometric mean Stokes diameter by number [ m ]
      REAL   XXLSG   ! natural log of geometric standard deviation
      REAL   RHOP    ! average particle density [ kg/m**3 ]

C    Output variable
      REAL   INFRAC  ! fraction of particulate volume transmitted through inlet

C    Internal variables
      REAL, PARAMETER :: SQRT2 = 1.4142135623731    !  SQRT( 2 )
      REAL, PARAMETER :: PI = 3.14159265358979324   ! PI (single precision 3.141593)

      REAL, PARAMETER :: DCA25 = 2.5 ! aerodynamic diameter cut point [ um ]
      REAL, PARAMETER :: B = 0.21470 ! Cunningham slip-correction approx. param [ um ]
      REAL DST25                     ! Stokes diameter equivalent of DCA25
      REAL DG                        ! DGN converted to [ um ]
      REAL ERFARG                    ! argument of ERF, from Step#6 of Jiang et al. (2005)

C *** Error function approximation, from Meng & Seinfeld (1994)
      REAL ERF        ! Error function
      REAL XX         ! dummy argument for ERF
      ERF( XX )  = SIGN( 1.0, XX ) * SQRT( 1.0 - EXP( -4.0 * XX * XX / PI ) )

C ----------------------- Begin solution -------------------------------

C *** Convert 2.5um size cut to its equivalent Stokes diameter using
C     equation 2 of Jiang et al. (2006). Note: the equation in Step 5
C     of this paper has a typo (i.e., eq. 2 is correct).

      DST25 = 0.5 * ( SQRT( B ** 2 + 4.0 * DCA25 *
     &                     ( DCA25 + B ) * 1.0E+03 / RHOP ) - B )

C *** Calculate mass fraction with Dca < 2.5um, using ERF approximation
C     from Meng & Seinfeld (1994) and modified form of Fk(X) equation
C     in Step#6 of Jiang et al. (2005).

      DG = DGN * 1.0E+06
      ERFARG = ( LOG( DST25 ) - LOG( DG ) ) / ( SQRT2 * XXLSG ) - 3.0 * XXLSG / SQRT2
      INFRAC = 0.5 * ( 1.0 + ERF( ERFARG ) )

      END SUBROUTINE INLET25

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE GETVISBY( DCV1, EXT1, DCV2, EXT2 )
 
C *** This routine calculates the Pitchford & Malm visual index,
C     deciview, using the Evans & Fournier extinction with 20 point
C     Gauss_Hermite numerical quadrature to integrate the extincetion
C     over the log normal size distribution.
C     This new method reproduces the results of Willeke and Brockmann
C     very very closely.
C     The Heintzenberg and Baker (h&b) method used previously has been
C     replaced.
C *** This also routine calculates the Pitchford & Malm visual index,
C     deciview, using reconstructed extinction values from the IMPROVE
C     monitoring network
 
C *** References:
 
C     Evans, B.T.N.  and G.R. Fournier, Simple approximations to
C     extinction efficiency valid over all size parameters,
C     Applied Optics, 29, 4666 - 4670.
 
C     Heintzenberg, j. and M. Baker, Applied Optics, vol. 15, no. 5,
C     pp 1178-1181, 1976.
C     correction in Applied Optics August 1976 does not affect this code.
 
C     Sisler, J. Fax dated March 18, 1998 with IMPROVE information.
 
C     Pitchford, M. and W. Malm, Atmos. Environ.,vol 28,no.5,
C     pp 1049-1054, 1994.
 
C     Willeke,K. & J. E. Brockmann, Atmos. Environ., vol.11,
C     pp 995 - 999, 1977.
 
Coding history:
C    who           when     what
C ------------------------------------------------------------------
C   F.S. Binkowski 5/9/95
C             coded this version to use the h&b approximation
C             and alfa and c obtained from fits to adt results
C             b was fit from comparisons to willeke & brockmann.
C   F.S. Binkowski 9/12/95  modified code to push j-loop inside.
C   F.S. Binkowski 5/13/97  made models-3 version
C   F.S. Binkowski 4/20/98  changed to Evans and Fournier approach
C                           for extinction
C   F.S. Binkowski 4/20/98  began code for reconstructed method
C   F.S. Binkowski 4/24/98  merged codes for both methods
C   F.S. Binkowski 5/20/98  corrected CONSTL
C   F.S. Binkowski 3/9/99   Modified for variable XXLSGAT and XXLSGAC
C   P.V. Bhave     1/30/08  Updated EXT2 to account for new SOA species
C   J.T. Kelly     4/17/08  Modified for variable XXLSGCO
C ////////
C NOTE: This subroutine is dependent upon the variables defined in the
C       IF( FIRSTIME ) section of aeroproc.f for implementation of
C                      coarse mode contribution.
C ///////

      USE AERO_DATA
      USE SOA_DEFN
      USE MET_DATA

      IMPLICIT NONE

      REAL    DCV1   ! block deciview (Mie)
      REAL    EXT1   ! block extinction [ km**-1 ] (Mie)

      REAL    DCV2   ! block deciview (Reconstructed)
      REAL    EXT2   ! block extinction [ km**-1 ]
                                  ! (Reconstructed)

C *** Paramters 

      REAL, PARAMETER :: LAM = 0.55E-6  ! wavelenght of visible light [ m ]

      REAL, PARAMETER :: CONSTL = 1.0E3 * PI6 / LAM    ! 1.0e3  to get km**-1

      REAL, PARAMETER :: CONST3 = PI / LAM  ! Changed 3/9/99 FSB

      REAL, PARAMETER :: SCALE = 1.0E-03  ! factor to rescale units from [ 1/Mm ] to [ 1/km ]

      REAL, PARAMETER :: RAY = 0.01         ! standard value for Rayleigh extinction [ 1/km ]
      REAL, PARAMETER :: RAY1 = 1.0 / RAY   ! the reciprocal of Rayleigh

C *** internal variables:

      INTEGER IRH ! percent realtive humidity as an
                  ! integer used for index

      REAL    WFRAC  ! water mass fraction
      REAL    NR, NI ! real and imaginary parts of the refractive index
      REAL    ALFV( n_mode )  ! Mie parameters for modal mass median diameters
      REAL    BBEXT  ! dimensionless extinction coefficient
      REAL    BEXT( n_mode ) ! Modal extinction coefficients [ 1/km ]

      REAL    FRH ! RH correction factor for sulfate and nitrate aerosols

      REAL    humfac(99) ! humidity scaling factors at 1% RH values
         DATA humfac/
     &   1.0000E+00,  1.0000E+00,  1.0000E+00,  1.0000E+00,  1.0000E+00,
     &   1.0000E+00,  1.0000E+00,  1.0000E+00,  1.0000E+00,  1.0000E+00,
     &   1.0000E+00,  1.0000E+00,  1.0000E+00,  1.0001E+00,  1.0001E+00,
     &   1.0004E+00,  1.0006E+00,  1.0024E+00,  1.0056E+00,  1.0089E+00,
     &   1.0097E+00,  1.0105E+00,  1.0111E+00,  1.0115E+00,  1.0118E+00,
     &   1.0122E+00,  1.0126E+00,  1.0130E+00,  1.0135E+00,  1.0139E+00,
     &   1.0173E+00,  1.0206E+00,  1.0254E+00,  1.0315E+00,  1.0377E+00,
     &   1.0486E+00,  1.0596E+00,  1.0751E+00,  1.0951E+00,  1.1151E+00,
     &   1.1247E+00,  1.1343E+00,  1.1436E+00,  1.1525E+00,  1.1615E+00,
     &   1.1724E+00,  1.1833E+00,  1.1955E+00,  1.2090E+00,  1.2224E+00,
     &   1.2368E+00,  1.2512E+00,  1.2671E+00,  1.2844E+00,  1.3018E+00,
     &   1.3234E+00,  1.3450E+00,  1.3695E+00,  1.3969E+00,  1.4246E+00,
     &   1.4628E+00,  1.5014E+00,  1.5468E+00,  1.5992E+00,  1.6516E+00,
     &   1.6991E+00,  1.7466E+00,  1.7985E+00,  1.8549E+00,  1.9113E+00,
     &   1.9596E+00,  2.0080E+00,  2.0596E+00,  2.1146E+00,  2.1695E+00,
     &   2.2630E+00,  2.3565E+00,  2.4692E+00,  2.6011E+00,  2.7330E+00,
     &   2.8461E+00,  2.9592E+00,  3.0853E+00,  3.2245E+00,  3.3637E+00,
     &   3.5743E+00,  3.7849E+00,  4.0466E+00,  4.3594E+00,  4.6721E+00,
     &   5.3067E+00,  5.9412E+00,  6.9627E+00,  8.3710E+00,  9.7793E+00,
     &   1.2429E+01,  1.5078E+01,  1.8059E+01,  2.1371E+01/

         SAVE humfac

      INTEGER N       ! loop counter

C-----------------------------------------------------------------------

C NOTE: In the following calculations, the contribution from the
C        coarse mode is ignored.

C *** calculate the  mass fraction of aerosol water

      WFRAC = MIN( ( AEROSPC_CONC( AH2O_IDX,1 ) + AEROSPC_CONC( AH2O_IDX,2 ) )
     &            / ( AEROMODE_MASS( 1 ) + AEROMODE_MASS( 2 ) ), 1.0 )

C *** interpolate between "dry" state with m = 1.5 - 0.01i
C     and pure water particle with  m = 1.33 - 0.0i as a function of
C     wfrac

      NR = 1.5 - 0.17 * WFRAC     ! real part of refractive index
      NI = 0.01 * ( 1.0 - WFRAC ) ! imaginary part of refractive index

C *** set up Mie parameters for Volume ( mass median diameter)
      
      DO N = 1, N_MODE
        ALFV( N ) = CONST3 * AEROMODE_DIAM( N )
     &            * EXP( 3.0 * AEROMODE_SDEV( N ) * AEROMODE_SDEV( N ) )
      END DO

C *** Call extinction routines

      DO N = 1, N_MODE
         CALL GETBEXT( NR, NI, ALFV( N ), AEROMODE_SDEV( N ), BBEXT )
         BEXT( N ) = CONSTL * MOMENT3_CONC( N ) * BBEXT
      END DO

      BEXT( N_MODE ) = 0.0

      EXT1  = BEXT( 1 ) + BEXT( 2 ) + RAY

      DCV1  = 10.0 * LOG ( EXT1 * RAY1 )

C     note if EXT1 < 0.01 then DCV1 is negative.
C     this implies that visual range is greater than the Rayleigh limit.
C     The definition of deciviews is based upon the Rayleigh limit
C     being the maximum visual range
C     thus, set a floor of 0.0 on DCV1.

      DCV1  = MAX( 0.0, DCV1 )

C *** begin  IMPROVE reconstructed method

      IRH = INT( 100.0 * AIRRH  ) ! truncate relative humidity to
                                  ! nearest integer
      IRH = MIN( 99, IRH )        ! set maximum value on IRH
      IRH = MAX( 1, IRH )         ! set minimum value on IRH


      FRH = humfac( IRH )         ! set humidity correction

C *** NOTE in the following the fine primary mass "other" is
C     treated as though it were fine mass soil.

      EXT2 = 0.0
      
      !  sum from aerospc species
      DO N = 1, N_AEROSPC
         IF ( N .EQ. ASO4_IDX .OR. N .EQ. ANO3_IDX .OR. N .EQ. ANH4_IDX ) THEN
            IF ( AEROSPC( N )%NAME( 1 ) .NE. ' ' ) THEN
               EXT2 = EXT2 + FRH * AEROSPC( N )%VISUAL_IDX * AEROSPC_CONC( N,1 )
            END IF
            IF ( AEROSPC(N )%NAME( 2 ) .NE. ' ' ) THEN
               EXT2 = EXT2 + FRH * AEROSPC( N )%VISUAL_IDX * AEROSPC_CONC( N,2 )
            END IF
            CYCLE
         END IF

         IF ( AEROSPC( N )%NAME( 1 ) .NE. ' ' ) THEN
            EXT2 = EXT2 + AEROSPC( N )%VISUAL_IDX * AEROSPC_CONC( N,1 )
         END IF
         IF ( AEROSPC( N )%NAME( 2 ) .NE. ' ' ) THEN
            EXT2 = EXT2 + AEROSPC( N )%VISUAL_IDX * AEROSPC_CONC( N,2 )
         END IF
      END DO

      !  sum from soa species
      DO N = 1, N_VAPOR  
         EXT2 = EXT2 + 4.0 * AEROSPC_CONC( SOA_AEROMAP( N ),2 )
      END DO

      EXT2 = SCALE * EXT2 + RAY

      DCV2  = 10.0 * LOG ( EXT2  * RAY1 )

C     note if EXT2 < RAY then DCV2 is negative.
C     this implies that visual range is greater than the Rayleigh limit.
C     The definition of deciviews is based upon the Rayleigh limit
C     being the maximum visual range
C     thus, set a floor of 0.0 on BLKDCV.

      DCV2  = MAX( 0.0, DCV2 )

      RETURN
      END SUBROUTINE GETVISBY

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      subroutine getbext(nr, ni, alfv, xlnsig, bext)

C     calculates the extinction coefficient normalized by wavelength
C     and total particle volume concentration for a log normal
C     particle distribution with the logarithm of the
C     geometric standard deviation given by xlnsig.
 
C *** does gauss-hermite quadrature of Qext / alfa
C     over log normal distribution
C     using 20 symmetric points
 
      implicit none

      real nr, ni  ! indices of refraction
      real alfv    ! Mie parameter for dgv
      real xlnsig  ! log of geometric standard deviation
      real bext    ! normalized extinction coefficient
      real aa, aa1 ! see below for definition

      real alfaip, alfaim   ! Mie parameters at abscissas

      real xxqalf  ! function to calculate the extinction ceofficient
      real qalfip, qalfim   ! extinction efficiencies at abscissas

      real, parameter :: pi      = 3.14159265
      real, parameter :: sqrtpi  = 1.77245385
      real, parameter :: sqrtpi1 = 1.0 / sqrtpi
      real, parameter :: sqrt2   = 1.41421356
      real, parameter :: three_pi_two = 3.0 * pi / 2.0
      real, parameter :: const = three_pi_two * sqrtpi1

      integer i
      real sum, xi,wxi,xf

      integer, parameter :: n = 10   ! one-half the number of abscissas
      real ghxi( n ) ! Gauss-Hermite abscissas
      real ghwi( n ) ! Gauss-Hermite weights

C *** the following weights and abscissas are from abramowitz and
C     stegun, page 924
C *** tests show that 20 point is adquate.

      data ghxi / 0.245341, 0.737474, 1.234076, 1.738538, 2.254974,
     &            2.788806, 3.347855, 3.944764, 4.603682, 5.387481 /

      data ghwi/ 4.622437e-1, 2.866755e-1, 1.090172e-1, 2.481052e-2,
     &           3.243773e-3, 2.283386e-4, 7.802556e-6, 1.086069e-7,
     &           4.399341e-10, 2.229394e-13 /

      sum = 0.0

      aa = 1.0 / ( sqrt2 * xlnsig )
      aa1 = sqrt2 * xlnsig ! multiplication cheaper than another division

      do i = 1, n

         xi      = ghxi( i )
         wxi     = ghwi( i)
         xf      = exp( xi * aa1 )
         alfaip  = alfv * xf
         alfaim  = alfv / xf
         qalfip  = xxqalf( alfaip, nr, ni )
         qalfim  = xxqalf( alfaim, nr, ni )

         sum = sum + wxi * ( qalfip + qalfim )

      end do ! i

C fsb      bext = const * aa * sum
      bext = const * sum ! corrected 07/21/2000 FSB; found by Rokjin Park

      return
      end subroutine getbext

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      real function xxqalf( alfa, nr, ni )
 
C *** compute the extinction efficiency divided by the Mie parameter
C     reference:
C     Evans, B.T.N.  and G.R. Fournier, Simple approximations to
C     extinction efficiency valid over all size parameters,
C     Applied Optics, 29, 4666 - 4670.

      implicit none

      real alfa
      real nr, ni  ! real and imaginary parts of index of refraction

      real qextray
      real qextadt
      real qextef
      real tt        !  edge effect factor
      real mu, mum1  !  exponents in formula
      real aa        !  first coefficient in mu
      real gg        !  second coefficient in mu
      real nrm1, sqrtni
      real alfm23   ! functions of alfa (mie parameter)

      real, parameter :: three5 = 3.0 / 5.0,
     &                   three4 = 3.0 / 4.0,
     &                   two3   = 2.0 / 3.0

      nrm1   = nr - 1.0
      sqrtni = sqrt (ni )

      call adtqext( alfa, nr, ni, qextadt )
      call pendrfx( alfa, nr, ni, qextray )

      if ( alfa .gt. 0.5 ) then

         alfm23 = 1.0 / alfa ** two3
         tt = 2.0 - exp( -alfm23 )

         aa = 0.5 + ( nrm1 - two3 * sqrtni - 0.5 * ni )
     &      + ( nrm1 + two3 * ( sqrtni - 5.0  * ni )  ) ** 2

         gg = three5 - three4 * sqrt ( nrm1) + 3.0 * nrm1 ** 4
     &      + 25.0 * ni / ( 6.0 * ni + 5.0 * nrm1 )

         mu     = aa + gg / alfa
         mum1   = - 1.0 / mu

         qextef = qextray
     &          * ( 1.0 + (qextray /( qextadt * tt)) ** mu ) ** mum1

      else

         qextef = qextray   ! Use Rayleigh extinction for really small alfa's

      end if ! check on alfa

      xxqalf = qextef / alfa

      return
      end function xxqalf

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      subroutine adtqext( alfa, nr, ni, QEXT )
 
C *** van de Hulst approximation for QEXT.
C *** This approximation is known as Anomalous Diffraction Theory (ADT)
 
C *** originally coded by Dr Francis  S. Binkowski,
C     AMAB/MD/ESRL RTP,N.C. 27711 28 July 1977.
C     corrected 7/19/90 by fsb.
C     revised 1/8/98 by FSB
 
C *** reference:
C     van de Hulst- Light Scattering by Small Particles,
C     Dover,1981 page 179. Original edition was Wiley, 1957.

      implicit none

      real alfa     ! Mie parameter
      real nr, ni   ! real and imaginary parts of the index of refraction
      real QEXT     ! extinction efficiency for a sphere
      real z, tanb, b, cos2b, v1, v2, x, expmx, expm2x, cs1, cs2
      real twob, cosb
      real nr1

      nr1    = nr - 1.0
      z      = 2.0 * alfa * nr1
      tanb   = ni / nr1
      b      = atan( tanb )
      cosb   = cos( b )
      twob   = 2.0 * b
      cos2b  = cos( twob )
      v1     = 5.0 * nr1
      v2     = 4.08 / ( 1.0 + 3.0 * tanb )
      x      = z * tanb
      expmx  = exp( -x )
      expm2x = exp( -2.0 * x )
      cs1    = cosb / z
      cs2    = cs1 * cs1

      QEXT   = 2.0 - 4.0 * cs1 * expmx * sin( z - b )
     &       + 4.0 * cs2 * ( cos2b - expmx * cos(z - twob ) )
      return
      end subroutine adtqext

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      subroutine pendrfx( alfa, nr, ni, QEXT )
 
C *** calculates the Mie efficiencies for extinction, scattering and
C     absorption using Penndorf's approximations for small
C     values of alfa.
 
C  input:
C       nr        the real part of the refractive index.
C       ni        the imaginary part of the refractive index
C       alfa      mie parameter
 
C  output:
C       QEXT      extinction efficiency
 
C *** Reference:
C       Penndorf, r., Scattering and extinction coefficients for small
C       aerosols, J. Atmos. Sci., 19, p 193, 1962.
C
C *** Coded by Dr Francis  S.  Binkowski,
C       AMAB/MD/ESRL RTP,N.C. 27711 28 July 1977.
C       corrected 7/19/90 by FSB
C       modified 30 september 1992 by FSB
C       modified 1/6/98 by FSB

      implicit none

      real alfa, nr, ni
      real QEXT
      real alf2, alf3, alf4

      real a1, a2, a3
      real xnr, xni, xnr2, xni2, xnri, xnri2, xnrmi
      real xri, xri2, xri36, xnx, xnx2
      real z1, z12, z2, xc1

      xnr   = nr
      xni   = ni
      xnr2  = xnr   * xnr
      xni2  = xni   * xni
      xnri  = xnr2  + xni2
      xnri2 = xnri  * xnri
      xnrmi = xnr2  - xni2
      xri   = xnr   * xni
      xri2  = xri   * xri
      xri36 = 36.0  * xri2
      xnx   = xnri2 + xnrmi - 2.0
      xnx2  = xnx   * xnx

      z1    = xnri2 + 4.0 * xnrmi + 4.0
      z12   = z1    * z1
      z2    = 4.0   * xnri2 + 12.0 * xnrmi + 9.0
      xc1   = 8.0   / ( 3.0 * z12 )

      alf2  = alfa  * alfa
      alf3  = alfa  * alf2
      alf4  = alfa  * alf3

      a1    = 24.0  * xri / z1

      a2    = 4.0   * xri / 15.0 + 20.0 * xri / ( 3.0 * z2 )
     &      + 4.8   * xri * ( 7.0 * xnri2
     &      + 4.0   * ( xnrmi - 5.0 ) ) / z12

      a3    = xc1   * ( xnx2 - xri36 )

      QEXT  = a1    * alfa + a2 * alf3 + a3 * alf4

      return
      end subroutine pendrfx

