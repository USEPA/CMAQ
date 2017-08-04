
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
C $Header$

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

       SUBROUTINE CHEM( CGRID, JDATE, JTIME, TSTEP )

C**********************************************************************
C
C  FUNCTION: Driver subroutine for Euler Backward Iterative solver
C
C  PRECONDITIONS: For the CB6R3 family of mechanisms
C
C  KEY SUBROUTINES/FUNCTIONS CALLED:  HRINIT, PHOT, HRCALCKS, HRSOLVER
C
C  REVISION HISTORY: Created by EBI solver program, Mar 10, 2017
C                       Based on the algorithm in "Test of Two Numerical
C                       Schemes for Use in Atmospheric Transport-Chemistry
C                       Models", O. Hertel, R. Berkowicz, J. Christensen,
C                       and O. Hov, Atm Env., Vol. 27A, No. 16, 1993.
C                       Original MEBI code developed by Ho-Chun Huang,
C                       SUNY, Albany -- "On the performance of numerical
C                       solvers for a chemistry submodel in three-dimensional
C                       air quality models 1. Box model simulations",
C                       H. Huang and J.S. Chang, JGR, Vol 106, No. D17, 2001.
C                       This version replaces Huang and Chang use of numerical
C                       solutions with analytical solutions derived in
C                       Hertel et al.
C   21 Jun 10 J.Young: convert for Namelist redesign
C   11 May 11 D.Wong: incorporated twoway model implementation
C   27 Sep 11 B.Hutzell: revised method for defining CALL_DEG variable
C   18 Jul 14 B.Hutzell: revised: 1) to use the RXNS_DATA and RXNS_FUNCTION
C                        modules instead of include files, 2) to enable
C                        reactions between species types, 3) to calculate
C                        heterogeneous rate constants via AEROSOL_CHEMISTRY
C                        module, 4) to replace call to HRCALCKS with
C                        subroutine in RXNS_FUNCTION module and 5) to change
C                        how degrade routines are used, if present
C   02 Dec 14 B.Hutzell: 1) added terrestrial data to conduct surface
C                        dependent reactions and 2) modified the call CALC_RCONST
C                        routine
C**********************************************************************

      USE HGRD_DEFN             ! horizontal domain specifications
      USE VGRD_DEFN             ! vertical layer specifications
      USE CGRID_SPCS            ! CGRID mechanism species
      USE UTILIO_DEFN           ! IOAPI parameters and functions declarations
      USE RXNS_DATA
      USE AEROSOL_CHEMISTRY
      USE RXNS_FUNCTION
      USE HRDATA
      USE PA_DEFN, Only: LIRR   ! Process Anaylsis control and data variable

      IMPLICIT NONE

C..Includes:
      INCLUDE SUBST_FILES_ID  ! CMAQ files
      INCLUDE SUBST_CONST     ! CMAQ constants

      INCLUDE SUBST_EMISPRM   ! Emissions processing control parameters

C..Arguments:
      REAL, POINTER :: CGRID( :,:,:,: )  ! Species concentrations
      INTEGER JDATE           ! Current date (YYYYDDD)
      INTEGER JTIME           ! Current time (HHMMSS)
      INTEGER TSTEP( 3 )      ! Time step vector (HHMMSS)

C..Parameters:
      REAL( 8 ), PARAMETER :: DCONMIN = 1.0D-30               ! minimum species concentration allowed
      REAL,      PARAMETER :: CONCMIN = 1.0E-30               ! minimum species concentration allowed
      REAL,      PARAMETER :: MAOMV   = 1.0E6 * MWAIR / MWWAT ! Mol Wt of air over Mol Wt of water times

C..External Functions:


C..Saved Local Variables:

      CHARACTER( 16 ), SAVE :: PNAME = 'HRDRIVER'     ! Program name

      INTEGER, SAVE :: ISTFL            ! Unit no. of iteration stat output file
      LOGICAL, SAVE :: LFIRST = .TRUE.  ! Flag for first call to this subroutine

      REAL( 8 ), SAVE :: PA2ATM   ! Pascal to atm conv fac

C..Scratch Local Variables:
      CHARACTER( 132 ) :: MSG           ! Message text
      CHARACTER(  16 ) :: VNAME         ! Name of I/O API data variable

      INTEGER C, E, L, R, S   ! Loop indices
      INTEGER ISP             ! array index

      INTEGER AVGEBI          ! Average no. of EBI iterations
      INTEGER DELT_SEC        ! EBI max time step in seconds
      INTEGER ESP             ! Loop index for emissions species
      INTEGER ITMSTEP         ! Chemistry integration interval (sec)
      INTEGER LEV             ! Layer index
      INTEGER MIDDATE         ! Date at time step midpoint
      INTEGER MIDTIME         ! Time at time step midpoint
      INTEGER NPH             ! Index for number of phot. rxns in PHOT
      INTEGER SPC             ! Species loop index
#ifdef hrstats
      INTEGER MNEBI           ! Min no. of EBI iterations
      INTEGER MXEBI           ! Max no. of EBI iterations
#endif

      LOGICAL LSUNLIGHT       ! Flag for sunlight

      REAL       INV_DENS     ! reciprocal of air mass density, m3/Kg
#ifdef hrstats
      REAL       SUMEBI       ! Sum of EBI iterations
#endif

      REAL( 8 )  CHEMSTEP     ! Chemistry integration interval (min)


      INTEGER       :: GXOFF, GYOFF            ! global origin offset from file

      INTEGER, SAVE :: PECOL_OFFSET        ! Local Column Offset for processor
      INTEGER, SAVE :: PEROW_OFFSET        ! Local Column Offset for processor
C for INTERPX
      INTEGER, SAVE :: STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2
      INTEGER, SAVE :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3

      INTERFACE
         SUBROUTINE PHOT ( MDATE, MTIME, JDATE, JTIME, DTSTEP, RJ )
          INTEGER, INTENT( IN )  :: MDATE       !"centered" Julian date (YYYYDDD)
          INTEGER, INTENT( IN )  :: MTIME       !"centered" time (HHMMSS)
          INTEGER, INTENT( IN )  :: JDATE       ! Current date (YYYYDDD)
          INTEGER, INTENT( IN )  :: JTIME       ! Current time (HHMMSS)
          INTEGER, INTENT( IN )  :: DTSTEP( : ) ! time step vector (HHMMSS)
          REAL,    INTENT( OUT ) :: RJ(:,:,:,:) ! gridded J-values  (1/min units)
        END SUBROUTINE PHOT
        SUBROUTINE HRSOLVER( JDATE, JTIME, C, R, L )
           INTEGER, INTENT( IN ) :: JDATE    ! Current date (YYYYDDD)
           INTEGER, INTENT( IN ) :: JTIME    ! Current time (HHMMSS)
           INTEGER, INTENT( IN ) :: C, R, L  ! Cell col, row, lev
        END SUBROUTINE HRSOLVER
      END INTERFACE

C**********************************************************************

      IF( NUMB_MECH_SPC .EQ. 0 ) RETURN

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  On first call, call routines to set-up for EBI solver
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF( LFIRST ) THEN

         LOGDEV = INIT3( )

         IF( MECHNAME .NE. 'CB6R3_AE6NVPOA_AQ' ) THEN
             MSG = 'This version of the EBI solver can only be used with'
     &            // ' the CB6R3_AE6NVPOA_AQ chemical mechanism'
             CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
         END IF

#ifdef emis_chem
         EMISVD = .FALSE.
#else
         EMISVD = .TRUE.
#endif

         IF( INDEX( MECHNAME, 'AE' ) .NE. 0 ) THEN
           L_AE_VRSN = .TRUE.
         ELSE
           L_AE_VRSN = .FALSE.
         END IF

         IF( INDEX( MECHNAME, 'AQ' ) .NE. 0 ) THEN
           L_AQ_VRSN = .TRUE.
         ELSE
           L_AQ_VRSN = .FALSE.
         END IF

         IF( LIRR ) THEN
            MSG = 'IRR Analysis not allowed with EBI solver'
            CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
         END IF


         CALL HRINIT

         ITMSTEP = TIME2SEC( TSTEP( 2 ) )
         CHEMSTEP = REAL( ITMSTEP, 8 ) / 60.0D0
         WRITE( LOGDEV, 92000 ) CHEMSTEP, DELTAT

         WRITE( LOGDEV, 92020 )
         DO SPC = 1, NUMB_MECH_SPC
            WRITE( LOGDEV, 92040 ) CHEMISTRY_SPC( SPC ), RTOL( SPC )
         END DO

         PA2ATM =  REAL( 1.0 / STDATMPA, 8)

c..If emissions processing requested stop
         IF( .NOT. EMISVD ) THEN  ! assumes emis processing in gas chem

            MSG = 'ERROR: EBI solver not configured to '//
     &            'process emissions in chemistry'
            CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )

         END IF   ! End if doing emissions


#ifdef hrstats
         ISTFL = JUNIT()
         OPEN( UNIT=ISTFL, FILE='iterstat.dat' )
         WRITE( ISTFL, 94020 )
#endif
C Define processor offsets

         PECOL_OFFSET = COLSD_PE( 1, MYPE+1 ) - 1
         PEROW_OFFSET = ROWSD_PE( 1, MYPE+1 ) - 1


C.. Get fractional seawater coverage from the OCEAN file.

         ALLOCATE( OPEN_ZONE( NCOLS, NROWS ), SURF_ZONE( NCOLS, NROWS ) )

         CALL SUBHFILE ( OCEAN_1, GXOFF, GYOFF,
     &                   STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3 )

         VNAME = 'OPEN'
         IF ( .NOT. INTERPX( OCEAN_1, VNAME, PNAME,
     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,1,
     &                       JDATE, JTIME, OPEN_ZONE ) ) THEN
            MSG = 'Could not read OPEN from ' // OCEAN_1
            CALL M3EXIT ( PNAME, JDATE, JTIME, MSG, XSTAT1 )
         END IF

C.. Get fractional surf-zone coverage from the OCEAN file.

         VNAME = 'SURF'
         IF ( .NOT. INTERPX( OCEAN_1, VNAME, PNAME,
     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,1,
     &                       JDATE, JTIME, SURF_ZONE ) ) THEN
            MSG = 'Could not read SURF from ' // OCEAN_1
            CALL M3EXIT ( PNAME, JDATE, JTIME, MSG, XSTAT1 )
         END IF

         ALLOCATE( LAND_ZONE( NCOLS, NROWS ) )
         DO R = 1, NROWS
            DO C = 1, NCOLS
               IF( OPEN_ZONE( C,R ) + SURF_ZONE( C,R ) .GT. 0.0 )THEN
                   LAND_ZONE( C,R ) = .FALSE.
               ELSE
                   LAND_ZONE( C,R ) = .TRUE.
               END IF
             END DO
         END DO

         DEALLOCATE( OPEN_ZONE, SURF_ZONE )


         CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                   STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2 )

         CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                   STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

         ALLOCATE( DENS( NCOLS, NROWS, NLAYS ), PRES( NCOLS, NROWS, NLAYS ),
     &             QV  ( NCOLS, NROWS, NLAYS ), TA  ( NCOLS, NROWS, NLAYS ),
     &             RJ  ( NCOLS, NROWS, NLAYS, NPHOTAB ),
     &             SEAICE( NCOLS, NROWS ) )

         LFIRST = .FALSE.

      END IF      ! First time

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Set date and time to center of time step, get necessary physical
C  data, and get photolysis rates
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      MIDDATE = JDATE
      MIDTIME = JTIME
      ITMSTEP = TIME2SEC( TSTEP( 2 ) )
      CHEMSTEP = REAL( ITMSTEP, 8 ) / 60.0D0
      CALL NEXTIME( MIDDATE, MIDTIME, SEC2TIME( ITMSTEP / 2 ) )

C.. Compute number of time step loops and step size for EBI solver
      DELT_SEC = INT( DELTAT * 60.0D0 + 0.1D0 )
      IF( DELT_SEC .GE. ITMSTEP ) THEN
         N_EBI_STEPS = 2
         EBI_TMSTEP  = 0.5D0 * CHEMSTEP
      ELSE
         IF( MOD( ITMSTEP, DELT_SEC ) .EQ. 0 ) THEN
            N_EBI_STEPS = ITMSTEP / DELT_SEC
         ELSE
            N_EBI_STEPS = ITMSTEP / DELT_SEC + 1
         END IF
         EBI_TMSTEP =  CHEMSTEP / REAL( N_EBI_STEPS, 8 )
      END IF

      N_INR_STEPS = 1


C.. Get fractional seaice coverage from the METCRO2D file.

      VNAME = 'SEAICE'
      IF ( .NOT. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                    STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                    MIDDATE, MIDTIME, SEAICE ) ) THEN
         MSG = 'Could not read SEAICE from MET_CRO_2D'
         CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
      END IF

C.. Get ambient temperature in K

      VNAME = 'TA'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MIDDATE, MIDTIME, TA ) ) THEN
         MSG = 'Could not read TA from MET_CRO_3D'
         CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
      END IF

C.. Get specific humidity in Kg H2O / Kg air
      VNAME = 'QV'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MIDDATE, MIDTIME, QV ) ) THEN
         MSG = 'Could not read QV from MET_CRO_3D'
         CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
      END IF

! Get ambient MASS DENSITY in Kg/m^3
       VNAME = 'DENS'
       IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MIDDATE, MIDTIME, DENS ) ) THEN
          MSG = 'Could not read DENS from MET_CRO_3D'
          CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
       END IF

C.. Get pressure in Pascals
      VNAME = 'PRES'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MIDDATE, MIDTIME, PRES ) ) THEN
         MSG = 'Could not read PRES from MET_CRO_3D'
         CALL M3EXIT ( PNAME, JDATE, JTIME, MSG, XSTAT1 )
      END IF

C.. Get photolysis rates in 1/min
      CALL PHOT ( MIDDATE, MIDTIME, JDATE, JTIME, TSTEP, RJ )
C.. Get Heteorogeneous rates and Update Aerosol Distribution Properties
      CALL HETCHEM_RATES( TA, PRES, QV, CGRID )

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Top of loop over cells
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

#ifdef hrstats
      MNEBI = 1000
      MXEBI = 0
      SUMEBI = 0.0
#endif

      NCELL = 1

      DO L = 1, NLAYS
         DO R = 1, MY_NROWS
            DO C = 1, MY_NCOLS

c..Load ICs
               INV_DENS = 1.0 / DENS( C,R,L )
               DO SPC = 1, NUMB_MECH_SPC
                  S = CGRID_INDEX( SPC )
                  IF( CONVERT_CONC( SPC ) )THEN
                      YC( SPC ) = REAL( MAX( CONCMIN,
     &                            INV_DENS*FORWARD_CONV( SPC )*CGRID( C,R,L,S )), 8 )
                  ELSE
                      YC( SPC ) = REAL( MAX( CGRID( C,R,L,S ), CONCMIN), 8 )
                  END IF
               END DO


c..Set physical quantities
               TEMP( NCELL )     = REAL( TA( C,R,L ), 8)
!                PRESCELL( NCELL ) = REAL( PRES( C,R,L ), 8)
               ATMPRES( NCELL )  = PA2ATM * REAL( PRES( C,R,L ), 8)
               H2O( NCELL )      = REAL( MAX ( MAOMV * QV( C,R,L ), 0.0 ), 8)

               IF( LAND_ZONE( C,R ) .OR. SEAICE ( C,R ) .GT. 0.0 )THEN
                   LAND (NCELL) = .TRUE.
               ELSE
                   LAND (NCELL) = .FALSE.
               END IF

c..Get rate constants
               LSUNLIGHT = .FALSE.
               FORALL( NPH = 1:NPHOTAB ) RJIN( NCELL,NPH ) = REAL( RJ( C,R,L,NPH ), 8)
               IF( MAXVAL(RJIN) .GT. 0.0D0 ) LSUNLIGHT = .TRUE.

               FORALL ( NPH = 1:NHETERO )HET( NCELL,NPH ) = REAL( KHETERO( NPH,C,R,L ), 8)

               CALL CALC_RCONST( TEMP, ATMPRES, H2O, RJIN, HET, LSUNLIGHT, LAND, RKI_SAV, NCELL )
               FORALL( NPH = 1:NRXNS )RKI( NPH ) = RKI_SAV( NCELL, NPH )

c..Call EBI solver
               N_EBI_IT = 0

               CALL HRSOLVER( JDATE, JTIME, (C+PECOL_OFFSET), (R+PEROW_OFFSET), L )

#ifdef hrstats
               MXEBI  = MAX( MXEBI, N_EBI_IT )
               MNEBI  = MIN( MNEBI, N_EBI_IT )
               SUMEBI = SUMEBI + REAL( N_EBI_IT )
#endif


c..Update concentration array
               DO SPC = 1, NUMB_MECH_SPC
                  S = CGRID_INDEX( SPC )
                  IF( CONVERT_CONC( SPC ) )THEN
                      CGRID( C,R,L,S ) = REAL( REVERSE_CONV( SPC ) * DENS( C,R,L )
     &                                 * MAX( YC( SPC ), DCONMIN), 4)
                  ELSE
                      CGRID( C,R,L,S ) = REAL( MAX( YC( SPC ), DCONMIN), 4)
                  END IF
               END DO


            END DO
         END DO
      END DO

!  Update Aerosol Surface Area
      CALL HETCHEM_UPDATE_AERO( CGRID )

#ifdef hrstats
      AVGEBI = SUMEBI / REAL( NCOLS * NROWS * NLAYS )
      WRITE( ISTFL, 94040 ) JDATE, JTIME, MNEBI, AVGEBI, MXEBI
#endif

      RETURN

C*********************** FORMAT STATEMENTS ****************************

92000 FORMAT( / 10X, 'Euler Backward Iterative Parameters -'
     &        / 10X, 'Chemistry Integration Time Interval (min):', F12.4,
     &        / 10X, 'EBI maximum time step (min):              ', F12.4 )

92020 FORMAT( //10X, 'Species convergence tolerances:' )

92040 FORMAT(   10X, A16, 2X, 1PE12.2 )

92060 FORMAT( / 10X, 'Emissions Processing in Chemistry ...'
     &        / 10X, 'Number of Emissions Layers:         ', I3
     &        / 10X, 'out of total Number of Model Layers:', I3 )


94020 FORMAT( 'DATE      TIME ', 'MNEBI AVEBI MXEBI' )

94040 FORMAT( I7, 1X, I6, 1X, 3( I5, 1X ) )
      END
