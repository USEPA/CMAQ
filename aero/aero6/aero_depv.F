
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
C $Header: /project/yoj/arc/CCTM/src/aero/aero5/aero_depv.F,v 1.12 2012/01/19 13:12:14 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)aero_depv.F       1.3 /project/mod3/CMAQ/src/ae_depv/aero_depv/SCCS/s.aero_depv.F 18 Jun 1997 12:55:48

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE AERO_DEPV ( CGRID, JDATE, JTIME, TSTEP, MOSAIC, VDEP_AE )

C-----------------------------------------------------------------------
C aerosol dry deposition routine
C   written 4/9/97 by Dr. Francis S. Binkowski
C   uses code from modpar and vdvg from the aerosol module.
C   This routine uses a single block to hold information
C   for the lowest layer.
C NOTES: This version assumes that RA is available on the met file.
c        Array structure for vector optimization
C 26 Apr 97 Jeff - many mods
C 13 Dec 97 Jeff - expect uncoupled CGRID, concs as micro-g/m**3, #/m**3
C
C 1/11/99 David Wong at LM - change NUMCELLS to CELLNUM in the loop index
C FSB 3/17/99 changed to accommodate surface area/second moment and
C    encapsulated the actual drydep calculation into a subroutine which
C    is attached to this code
C Jeff - Dec 00 - move CGRID_MAP into f90 module
C FSB 12/11/2000. Logic added to allow deposition of particles at their
C     "wet" diameters; that is, accounting for the water on the particles.
C     This is done by adjusting the third and second moments for the
C     presence of water assuming that the geometric standard deviations
C     are not changed by this process. This appears to be a very good
C     assumption.
C 30 Aug 01 J.Young: Dyn alloc; Use HGRD_DEFN
C    Jan 03 J.Young: Change CGRID dimensions, eliminate re-allocations
C  6 Mar 03 J.Young: eliminate a lot of allocate/deallocates
C  7 Aug 03 S.Roselle: updated code for loading the min aero conc array
C 17 Dec 03 S.Roselle: Adjust 2nd and 3rd moments to include SOA,
C     without affecting the geometric standard deviations.
C 31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C     domain specifications in one module
C 07 Jun 05 P.Bhave: Added code to handle new species in the AE4
C     mechanism: ANAI, ANAJ, ANAK, ACLI, ACLJ, ACLK, ASO4K, AH2OK,
C     and ANO3K; look for ASEAS only when using AE3 mechanism
C 30 Jan 08 S.Napelenok & P.Bhave: Added code to handle new SOA species
C     in AE5; defined DRY aerosol to include nonvolatile SOA spcs
C 14 Apr 08 J.Kelly: Added code to handle new species ANH4K and SRFCOR.
C     Also added code to handle variable coarse mode standard deviation
C     in AE5 (no longer fixed at 2.2).
C 08 Sep 08 P.Bhave: Backward compatibility with AE4 mechanisms
C     standardized names of all coarse-mode variables
C 19 Apr 10 S.Howard: aero re-engineering for modularity
C 23 Apr 10 J.Young: replace chem mechanism include files with namelists
C 10 Mar 11 S.Howard: Renamed met_data to aeromet_data
C 25 Mar 11 S.Roselle: Replaced I/O API include files with UTILIO_DEFN
C 20 May 11 D.Schwede: Modified for mosaic
C 31 Aug 11 J.Bash: Moved shared mosaic variables to MOSAIC_MOD
C 27 Sep 11 David Wong: replaced all run time dynamic arrays with allocatable
C                       arrays to avoid run time memory issue  
C 08 Jun 12 J.Young: remove full character blank padding for GNU Fortran (GCC) 4.1.2
C 07 Jul 14 B.Hutzell: replaced mechanism include file(s) with fortran module
C 07 Nov 14 J.Bash: Updated for the ASX_DATA_MOD shared data module. 
C    May 16 B. Murphy, H. Pye: Updated treatment of aerosol moments
C-----------------------------------------------------------------------

      USE GRID_CONF           ! horizontal & vertical domain specifications
      USE RXNS_DATA           ! chemical mechanism data
      USE CGRID_SPCS          ! CGRID mechanism species
      USE UTILIO_DEFN      
      USE AERO_DATA           ! aero variable data
      USE AEROMET_DATA        ! Includes CONST.EXT
      USE Mosaic_Mod, Only: ADEPVJ  ! Shared mosaic variables
      Use LSM_Mod, Only: N_LUFRAC
      USE ASX_DATA_MOD, Only: Met_Data

      IMPLICIT NONE

C Includes:

      INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments
      REAL,    POINTER       :: CGRID( :,:,:,: )
      INTEGER, INTENT( IN )  :: JDATE               ! current model date , coded YYYYDDD
      INTEGER, INTENT( IN )  :: JTIME               ! current model time , coded HHMMSS
      INTEGER, INTENT( IN )  :: TSTEP               ! model time step, coded HHMMSS
      LOGICAL, INTENT( IN )  :: MOSAIC              ! use mosaic option
      REAL,    INTENT( OUT ) :: VDEP_AE( :,:,: )    ! surrogate deposition velocities [ m s**-1 ]

C Parameters
      REAL,    PARAMETER :: T0 = 288.15      ! [ K ] ! starting standard surface temp.
      REAL,    PARAMETER :: TWO3 = 2.0 / 3.0
      INTEGER, PARAMETER :: N_AE_DEP_SPC = 9 ! no. of surrogates for aerosol dry dep velocities

C set up species dimension and indices for deposition velocity internal array VDEP
      INTEGER, PARAMETER :: VDNATK = 1,  ! Aitken mode number
     &                      VDNACC = 2,  ! accumulation mode number
     &                      VDNCOR = 3,  ! coarse mode number
     &                      VDMATK = 4,  ! Aitken mode mass
     &                      VDMACC = 5,  ! accumulation mode mass
     &                      VDMCOR = 6,  ! coarse mode mass
     &                      VDSATK = 7,  ! Aitken mode surface area
     &                      VDSACC = 8,  ! accumulation mode surface area
     &                      VDSCOR = 9   ! coarse mode surface area

C Local variables:

      CHARACTER( 16 ) :: VDAE_NAME( N_AE_DEP_SPC )! dep vel surrogate name table
      DATA         VDAE_NAME( 1 ) / 'VNUMATKN' /
      DATA         VDAE_NAME( 2 ) / 'VNUMACC ' /
      DATA         VDAE_NAME( 3 ) / 'VNUMCOR ' /
      DATA         VDAE_NAME( 4 ) / 'VMASSI  ' /
      DATA         VDAE_NAME( 5 ) / 'VMASSJ  ' /
      DATA         VDAE_NAME( 6 ) / 'VMASSC  ' /
      DATA         VDAE_NAME( 7 ) / 'VSRFATKN' /
      DATA         VDAE_NAME( 8 ) / 'VSRFACC ' /
      DATA         VDAE_NAME( 9 ) / 'VSRFCOR ' /

      INTEGER, ALLOCATABLE, SAVE :: DEPV_SUR( : )   ! pointer to surrogate

C Meteorological variables

      CHARACTER( 16 ), SAVE :: AE_VRSN ! Aerosol version name

      INTEGER, SAVE :: NCELLS              ! number of cells per layer

      REAL, ALLOCATABLE, SAVE  :: XXLSGAT( :,: )    ! log of standard deviation
      REAL, ALLOCATABLE, SAVE  :: XXLSGAC( :,: )
      REAL, ALLOCATABLE, SAVE  :: XXLSGCO( :,: )
 
      REAL, ALLOCATABLE, SAVE  :: DGATK( :,: )      ! geometric mean diameter
      REAL, ALLOCATABLE, SAVE  :: DGACC( :,: )
      REAL, ALLOCATABLE, SAVE  :: DGCOR( :,: )

      REAL, ALLOCATABLE, SAVE  :: PDENSAT( :,: )    ! particle density         
      REAL, ALLOCATABLE, SAVE  :: PDENSAC( :,: )
      REAL, ALLOCATABLE, SAVE  :: PDENSCO( :,: )

      REAL, ALLOCATABLE, SAVE  :: XLM( :,: )        ! mean free path [ m ]
      REAL, ALLOCATABLE, SAVE  :: AMU( :,: )        ! dynamic viscosity [ kg m**-1 s**-1 ]

      REAL, ALLOCATABLE, SAVE :: VDEP( :,:,: )    ! deposition  velocity [ m/s ]
      REAL, ALLOCATABLE, SAVE :: VDEPJ( :,:,:,: ) ! deposition  velocity [ m/s ]

      REAL M3_WET, M3SUBT, M3_DRY
      REAL M2_WET, M2_DRY

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      INTEGER, SAVE :: LOGDEV                   ! unit number for the log file
      CHARACTER( 16 ), SAVE :: PNAME = 'AERO_DEPV'
      CHARACTER( 16 ) :: VNAME            ! varable name
      CHARACTER( 96 ) :: XMSG = ' '

      INTEGER  C, R, V, N, J         ! loop counters
      INTEGER  SPC, S                ! species loop counter
      INTEGER  ALLOCSTAT

      INTERFACE
        SUBROUTINE GETDEP_V ( XLM, AMU, DGATK, DGACC, DGCOR,
     &                        XXLSGAT, XXLSGAC, XXLSGCO,
     &                        PDENSAT, PDENSAC, PDENSCO,
     &                        VDEP, VDEPJ, MOSAIC )
          REAL, INTENT( IN ) :: XLM( :,: )      ! atmospheric mean free path [ m ]
          REAL, INTENT( IN ) :: AMU( :,: )      ! atmospheric dynamic viscosity [ kg/(m s) ]
          REAL, INTENT( IN ) :: DGATK( :,: )    ! nuclei mode geometric mean diameter  [ m ]
          REAL, INTENT( IN ) :: DGACC( :,: )    ! accumulation geometric mean diameter [ m ]
          REAL, INTENT( IN ) :: DGCOR( :,: )    ! coarse mode geometric mean diameter  [ m ]
          REAL, INTENT( IN ) :: XXLSGAT( :,: )  ! Aitken mode
          REAL, INTENT( IN ) :: XXLSGAC( :,: )  ! accumulation mode
          REAL, INTENT( IN ) :: XXLSGCO( :,: )  ! coarse mode
          REAL, INTENT( IN ) :: PDENSAT( :,: )  ! average particle density in nuclei mode
          REAL, INTENT( IN ) :: PDENSAC( :,: )  ! average particle density in accumulation mode
          REAL, INTENT( IN ) :: PDENSCO( :,: )  ! average particle density in coarse mode
          REAL, INTENT( OUT ) :: VDEP( :,:,: ) ! deposition  velocity [ m/s ]
          REAL, INTENT( OUT ) :: VDEPJ( :,:,:,: ) ! deposition  velocity [ m/s ] for each land use category
          LOGICAL, INTENT( IN ) :: MOSAIC
        END SUBROUTINE GETDEP_V
      END INTERFACE

C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         LOGDEV = INIT3()

         NCELLS = NCOLS * NROWS

C  Allocate arrays
         ALLOCATE( XXLSGAT( NCOLS,NROWS ),
     &             XXLSGAC( NCOLS,NROWS ),
     &             XXLSGCO( NCOLS,NROWS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating XXLSGAT, XXLSGAC or XXLSGCO'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE( DGATK( NCOLS,NROWS ),
     &             DGACC( NCOLS,NROWS ),
     &             DGCOR( NCOLS,NROWS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating DGATK, DGACC or DGCOR'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE( PDENSAT( NCOLS,NROWS ),
     &             PDENSAC( NCOLS,NROWS ),
     &             PDENSCO( NCOLS,NROWS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating PDENSAT, PDENSAC or PDENSCO'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE( XLM( NCOLS,NROWS ),
     &             AMU( NCOLS,NROWS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating XLM or AMU'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( VDEP( NCOLS,NROWS,N_AE_DEP_SPC ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating VDEP'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

            ALLOCATE ( VDEPJ( N_LUFRAC,NCOLS,NROWS,N_AE_DEP_SPC ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating VDEPJ'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
         IF ( MOSAIC ) THEN
            ALLOCATE ( ADEPVJ( N_LUFRAC,N_AE_DEPV,NCOLS,NROWS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating ADEV'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
         END IF

         ALLOCATE ( DEPV_SUR( N_AE_DEPV ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating DEPV_SUR'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C Set the dep vel surrogate pointers
         DO V = 1, N_AE_DEPV
            N = INDEX1( AE_DEPV( V ), N_AE_DEP_SPC, VDAE_NAME )
            IF ( N .NE. 0 ) THEN
               DEPV_SUR( V ) = N
            ELSE
               XMSG = 'Could not find ' // AE_DEPV( V ) // ' in aerosol' //
     &                ' surrogate table. >>> Dep vel set to zero <<< '
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
               DEPV_SUR( V ) = 0
            END IF
         END DO

      END IF    ! FIRSTIME      

      IF ( N_AE_SPC .LE. 0 ) RETURN

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Put the grid cell physical data in the block arrays
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      DO R = 1, MY_NROWS
      DO C = 1, MY_NCOLS

C ***    Set meteorological data for the grid cell.
         AIRDENS = Met_Data%DENS1( C,R )
         AIRTEMP = Met_Data%TEMP2( C,R )
         AIRPRES = Met_Data%PRSFC( C,R )

C ***    extract grid cell concentrations of aero species from CGRID
C        into aerospc_conc in aero_data module
C        Also determines second moment from surface area and adds wet
C        species
         CALL EXTRACT_AERO( CGRID( C,R,1,: ), .TRUE. )

C ***    Calculate geometric mean diameters and standard deviations of the
C        "wet" size distribution
         CALL GETPAR( .FALSE. )     

C        Save getpar values to arrays
         XXLSGAT( C,R ) = AEROMODE_LNSG( 1 )
         XXLSGAC( C,R ) = AEROMODE_LNSG( 2 )
         XXLSGCO( C,R ) = AEROMODE_LNSG( 3 )

         DGATK( C,R )   = AEROMODE_DIAM( 1 )
         DGACC( C,R )   = AEROMODE_DIAM( 2 )
         DGCOR( C,R )   = AEROMODE_DIAM( 3 )

         PDENSAT( C,R ) = AEROMODE_DENS( 1 )
         PDENSAC( C,R ) = AEROMODE_DENS( 2 )
         PDENSCO( C,R ) = AEROMODE_DENS( 3 )
 
C        Calculate mean free path [ m ]:
         XLM( C,R ) = 6.6328E-8 * STDATMPA * AIRTEMP / ( T0 * AIRPRES )

C ***    Calcualte dynamic viscosity [ kg m**-1 s**-1 ]:
         AMU( C,R ) = 1.458E-6 * AIRTEMP * SQRT( AIRTEMP )
     &              / ( AIRTEMP + 110.4 )

      END DO ! Column LOOP
      END DO   ! Row LOOP

C *** get dry deposition velocities:

      CALL GETDEP_V ( XLM, AMU, DGATK, DGACC, DGCOR,
     &                XXLSGAT, XXLSGAC, XXLSGCO,
     &                PDENSAT, PDENSAC, PDENSCO,
     &                VDEP, VDEPJ, MOSAIC )

C Return dry deposition velocities for aerosols (first layer only).

      DO R = 1, NROWS
         DO C = 1, NCOLS
            DO V = 1, N_AE_DEPV
               IF ( DEPV_SUR( V ) .GT. 0 ) THEN
                  VDEP_AE( V,C,R ) = VDEP( C,R,DEPV_SUR( V ) )
               ELSE
                  VDEP_AE( V,C,R ) = 0.0
               END IF
            END DO
         END DO
      END DO

      IF ( MOSAIC ) THEN
        DO R = 1, NROWS
           DO C = 1, NCOLS
              DO V = 1, N_AE_DEPV
                DO J = 1, N_LUFRAC
                  IF ( DEPV_SUR( V ) .GT. 0 ) THEN
                     ADEPVJ( J,V,C,R ) = VDEPJ( J,C,R,DEPV_SUR( V ) )
                  ELSE
                     ADEPVJ( J,V,C,R ) = 0.0
                  END IF
                END DO
              END DO
           END DO
        END DO
      END IF

      RETURN
      END SUBROUTINE AERO_DEPV

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE GETDEP_V ( XLM, AMU, DGATK, DGACC, DGCOR,
     &                      XXLSGAT, XXLSGAC, XXLSGCO,
     &                      PDENSAT, PDENSAC, PDENSCO,
     &                      VDEP, VDEPJ, MOSAIC )

C *** Calculate deposition velocity for Aitken, accumulation, and
C     coarse modes.
C     Reference:
C     Binkowski F. S., and U. Shankar, The regional particulate
C     model 1. Model description and preliminary results.
C     J. Geophys. Res., 100, D12, 26191-26209, 1995.
 
C    May 05 D.Schwede: added impaction term to coarse mode dry deposition
C 25 May 05 J.Pleim:  Updated dry dep velocity calculation for aerosols
C                     to Venkatram and Pleim (1999)
C 20 Jul 05 J.Pleim:  Changed impaction term using modal integration of
C                     Stokes**2 / 400 (Giorgi, 1986, JGR)
C 14 Apr 08 J.Kelly:  Added code to calculate deposition velocity of
C                     coarse surface area and to account for variable
C                     standard deviation of the coarse mode.
C 08 Sep 08 P.Bhave:  Backward compatibility with AE4 mechanisms
C                     standardized names of all coarse-mode variables
C-----------------------------------------------------------------------

      USE LSM_MOD, Only: n_lufrac      ! to get n_lufrac
      USE AEROMET_DATA   ! Includes CONST.EXT
      USE ASX_DATA_MOD, Only: Met_Data, Mosaic_Data, Grid_Data

      IMPLICIT NONE

C *** input arguments

C     atmospheric properties
      REAL, INTENT( IN ) :: XLM( :,: )      ! atmospheric mean free path [ m ]
      REAL, INTENT( IN ) :: AMU( :,: )      ! atmospheric dynamic viscosity [ kg/(m s) ]

C     aerosol properties:

C     modal diameters: [ m ]
      REAL, INTENT( IN ) :: DGATK( :,: )    ! nuclei mode geometric mean diameter  [ m ]
      REAL, INTENT( IN ) :: DGACC( :,: )    ! accumulation geometric mean diameter [ m ]
      REAL, INTENT( IN ) :: DGCOR( :,: )    ! coarse mode geometric mean diameter  [ m ]

C     log of modal geometric standard deviations
      REAL, INTENT( IN ) :: XXLSGAT( :,: )  ! Aitken mode
      REAL, INTENT( IN ) :: XXLSGAC( :,: )  ! accumulation mode
      REAL, INTENT( IN ) :: XXLSGCO( :,: )  ! coarse mode

C     average modal particle densities  [ kg/m**3 ]
      REAL, INTENT( IN ) :: PDENSAT( :,: )  ! average particle density in nuclei mode
      REAL, INTENT( IN ) :: PDENSAC( :,: )  ! average particle density in accumulation mode
      REAL, INTENT( IN ) :: PDENSCO( :,: )  ! average particle density in coarse mode

C     Mosaic variables
      LOGICAL, INTENT( IN ) :: MOSAIC

C *** output arguments

      REAL, INTENT( OUT ) :: VDEP( :,:,: ) ! deposition  velocity [ m/s ]
      REAL, INTENT( OUT ) :: VDEPJ( :,:,:,: ) ! deposition  velocity [ m/s ] for each land use category

C *** array indices hardcoded to match SUBROUTINE AERO_DEPV
      INTEGER, PARAMETER :: VDNATK = 1,  ! Aitken mode number
     &                      VDNACC = 2,  ! accumulation mode number
     &                      VDNCOR = 3,  ! coarse mode number
     &                      VDMATK = 4,  ! Aitken mode mass
     &                      VDMACC = 5,  ! accumulation mode mass
     &                      VDMCOR = 6,  ! coarse mode mass
     &                      VDSATK = 7,  ! Aitken mode surface area
     &                      VDSACC = 8,  ! accumulation mode surface area
     &                      VDSCOR = 9   ! coarse mode surface area

C modal Knudsen numbers
      REAL KNATK   ! Aitken mode Knudsen number
      REAL KNACC   ! accumulation "
      REAL KNCOR   ! coarse mode

C modal particle diffusivities for number, 2nd, and 3rd moment, or mass:
      REAL DCHAT0N, DCHAT0A, DCHAT0C
      REAL DCHAT2N, DCHAT2A, DCHAT2C
      REAL DCHAT3N, DCHAT3A, DCHAT3C

C modal sedimentation velocities for number, 2nd, and 3rd moment, or mass:
      REAL VGHAT0N, VGHAT0A, VGHAT0C
      REAL VGHAT2N, VGHAT2A, VGHAT2C
      REAL VGHAT3N, VGHAT3A, VGHAT3C

      INTEGER NCELL, J, C, R

      REAL DCONST1, DCONST1N, DCONST1A, DCONST1C
      REAL DCONST2, DCONST3N, DCONST3A, DCONST3C
      REAL SC0N, SC0A, SC0C     ! Schmidt numbers for number
      REAL SC2N, SC2A, SC2C     ! Schmidt numbers for 2ND MOMENT
      REAL SC3N, SC3A, SC3C     ! Schmidt numbers for 3rd moment
      REAL STOKEN, STOKEA, STOKEC ! Stokes numbers for each mode
      REAL RD0N, RD0A, RD0C     ! canopy resistance for number
      REAL RD2N, RD2A, RD2C     ! canopy resistance for 2nd moment
      REAL RD3N, RD3A, RD3C     ! canopy resisteance for 3rd moment
      REAL UTSCALE              ! scratch function of USTAR and WSTAR
      REAL NU                   ! kinematic viscosity [ m**2 s**-1 ]
      REAL USTFAC               ! scratch function of USTAR, NU, and GRAV
      REAL TWOXLM               ! 2 X atmospheric mean free path

      REAL, PARAMETER :: BHAT    = 1.246 ! Constant from Cunningham slip correction
      REAL, PARAMETER :: THREEPI = 3.0 * PI
      REAL, PARAMETER :: TWO3    = 2.0 / 3.0

C Scalar variables for VARIABLE standard deviations.

      REAL    L2SGAT, L2SGAC   ! see usage
      REAL    L2SGCO

      REAL    EAT1             ! Aitken mode exp( log^2( sigmag )/8 )
      REAL    EAC1             ! accumulation mode exp( log^2( sigmag )/8 )
      REAL    ECO1             ! coarse mode exp( log^2( sigmag )/8 )

      REAL    ESAT04           ! Aitken       " **4
      REAL    ESAC04           ! accumulation "
      REAL    ESCO04           ! coarse       "

      REAL    ESAT08           ! Aitken       " **8
      REAL    ESAC08           ! accumulation "
      REAL    ESCO08           ! coarse       "

      REAL    ESAT12           ! Aitken       " **12
      REAL    ESAC12           ! accumulation "     
      REAL    ESCO12           ! coarse       "     

      REAL    ESAT16           ! Aitken       " **16
      REAL    ESAC16           ! accumulation "
      REAL    ESCO16           ! coarse       "

      REAL    ESAT20           ! Aitken       " **20
      REAL    ESAC20           ! accumulation "
      REAL    ESCO20           ! coarse       "

      REAL    ESAT28           ! Aitken       " **28
      REAL    ESAC28           ! accumulation "
      REAL    ESCO28           ! coarse       "

      REAL    ESAT32           ! Aitken       " **32
      REAL    ESAC32           ! accumulation "
      REAL    ESCO32           ! coarse       "

      REAL    ESAT36           ! Aitken       " **36
      REAL    ESAC36           ! accumulation "
      REAL    ESCO36           ! coarse       "

      REAL    ESAT48           ! Aitken       " **48
      REAL    ESAC48           ! accumulation "     
      REAL    ESCO48           ! coarse       "     

      REAL    ESAT64           ! Aitken       " **64
      REAL    ESAC64           ! accumulation "
      REAL    ESCO64           ! coarse       "

      REAL    ESAT128          ! Aitken       " **128
      REAL    ESAC128          ! accumulation "
      REAL    ESCO128          ! coarse       "

      REAL    ESAT160          ! Aitken       " **160
      REAL    ESAC160          ! accumulation "
      REAL    ESCO160          ! coarse       "

      REAL    ESATM12          ! Aitken       " **(-12)
      REAL    ESACM12          ! accumulation "        
      REAL    ESCOM12          ! coarse       "        

      REAL    ESATM16          ! Aitken       " **(-16)
      REAL    ESACM16          ! accumulation "        
      REAL    ESCOM16          ! coarse       "        

      REAL    ESATM20          ! Aitken       " **(-20)
      REAL    ESACM20          ! accumulation "
      REAL    ESCOM20          ! coarse       "

      REAL    ESATM32          ! Aitken       " **(-32)
      REAL    ESACM32          ! accumulation "
      REAL    ESCOM32          ! coarse       "

      REAL    EIM              ! Impaction efficiency

C-----------------------------------------------------------------------

      VDEP  = 0.0   ! array assignment
      IF ( MOSAIC ) THEN
         VDEPJ = 0.0   ! array assignment
      END IF

      DO R = 1, SIZE( Met_Data%TEMP2, 2 )
         DO C = 1, SIZE( Met_Data%TEMP2, 1 )

C *** Calculate Knudsen numbers

            TWOXLM = XLM( C,R ) + XLM( C,R )
            KNATK = TWOXLM / DGATK( C,R )
            KNACC = TWOXLM / DGACC( C,R )
            KNCOR = TWOXLM / DGCOR( C,R )

C *** Calculate functions of variable standard deviation.

            L2SGAT = XXLSGAT( C,R ) * XXLSGAT( C,R )
            L2SGAC = XXLSGAC( C,R ) * XXLSGAC( C,R )
            L2SGCO = XXLSGCO( C,R ) * XXLSGCO( C,R )

            EAT1   = EXP( 0.125 * L2SGAT )
            EAC1   = EXP( 0.125 * L2SGAC )
            ECO1   = EXP( 0.125 * L2SGCO )

            ESAT04  = EAT1 ** 4
            ESAC04  = EAC1 ** 4
            ESCO04  = ECO1 ** 4

            ESAT08  = ESAT04 * ESAT04
            ESAC08  = ESAC04 * ESAC04
            ESCO08  = ESCO04 * ESCO04

            ESAT12  = ESAT04 * ESAT08
            ESAC12  = ESAC04 * ESAC08
            ESCO12  = ESCO04 * ESCO08

            ESAT16  = ESAT08 * ESAT08
            ESAC16  = ESAC08 * ESAC08
            ESCO16  = ESCO08 * ESCO08

            ESAT20  = ESAT16 * ESAT04
            ESAC20  = ESAC16 * ESAC04
            ESCO20  = ESCO16 * ESCO04

            ESAT28  = ESAT20 * ESAT08
            ESAC28  = ESAC20 * ESAC08
            ESCO28  = ESCO20 * ESCO08

            ESAT32  = ESAT16 * ESAT16
            ESAC32  = ESAC16 * ESAC16
            ESCO32  = ESCO16 * ESCO16

            ESAT36  = ESAT16 * ESAT20
            ESAC36  = ESAC16 * ESAC20
            ESCO36  = ESCO16 * ESCO20

            ESAT48  = ESAT36 * ESAT12
            ESAC48  = ESAC36 * ESAC12
            ESCO48  = ESCO36 * ESCO12

            ESAT64  = ESAT32 * ESAT32
            ESAC64  = ESAC32 * ESAC32
            ESCO64  = ESCO32 * ESCO32

            ESAT128 = ESAT64 * ESAT64
            ESAC128 = ESAC64 * ESAC64
            ESCO128 = ESCO64 * ESCO64

            ESAT160 = ESAT128* ESAT32
            ESAC160 = ESAC128* ESAC32
            ESCO160 = ESCO128* ESCO32

C *** calculate inverses:

            ESATM12 = 1.0 / ESAT12
            ESACM12 = 1.0 / ESAC12
            ESCOM12 = 1.0 / ESCO12

            ESATM16 = 1.0 / ESAT16
            ESACM16 = 1.0 / ESAC16
            ESCOM16 = 1.0 / ESCO16

            ESATM20 = 1.0 / ESAT20
            ESACM20 = 1.0 / ESAC20
            ESCOM20 = 1.0 / ESCO20

            ESATM32 = 1.0 / ESAT32
            ESACM32 = 1.0 / ESAC32
            ESCOM32 = 1.0 / ESCO32

            DCONST1  = BOLTZMANN * Met_Data%TEMP2( C,R ) / ( THREEPI * AMU( C,R ) )
            DCONST1N = DCONST1 / DGATK( C,R )
            DCONST1A = DCONST1 / DGACC( C,R )
            DCONST1C = DCONST1 / DGCOR( C,R )
            DCONST2  = GRAV / ( 18.0 * AMU( C,R ) )
            DCONST3N = DCONST2 * PDENSAT( C,R ) * DGATK( C,R ) * DGATK( C,R )
            DCONST3A = DCONST2 * PDENSAC( C,R ) * DGACC( C,R ) * DGACC( C,R )
            DCONST3C = DCONST2 * PDENSCO( C,R ) * DGCOR( C,R ) * DGCOR( C,R )

C i-mode
            DCHAT0N  = DCONST1N * ( ESAT04  + BHAT * KNATK * ESAT16 )
            DCHAT2N  = DCONST1N * ( ESATM12 + BHAT * KNATK * ESATM16 )
            DCHAT3N  = DCONST1N * ( ESATM20 + BHAT * KNATK * ESATM32 )
            VGHAT0N  = DCONST3N * ( ESAT16  + BHAT * KNATK * ESAT04 )
            VGHAT2N  = DCONST3N * ( ESAT48  + BHAT * KNATK * ESAT20 )
            VGHAT3N  = DCONST3N * ( ESAT64  + BHAT * KNATK * ESAT28 )

C j-mode
            DCHAT0A  = DCONST1A * ( ESAC04  + BHAT * KNACC * ESAC16 )
            DCHAT2A  = DCONST1A * ( ESACM12 + BHAT * KNACC * ESACM16 )
            DCHAT3A  = DCONST1A * ( ESACM20 + BHAT * KNACC * ESACM32 )
            VGHAT0A  = DCONST3A * ( ESAC16  + BHAT * KNACC * ESAC04 )
            VGHAT2A  = DCONST3A * ( ESAC48  + BHAT * KNACC * ESAC20 )
            VGHAT3A  = DCONST3A * ( ESAC64  + BHAT * KNACC * ESAC28 )

C coarse mode
            DCHAT0C  = DCONST1C * ( ESCO04  + BHAT * KNCOR * ESCO16 )
            DCHAT2C  = DCONST1C * ( ESCOM12 + BHAT * KNCOR * ESCOM16 )
            DCHAT3C  = DCONST1C * ( ESCOM20 + BHAT * KNCOR * ESCOM32 )
            VGHAT0C  = DCONST3C * ( ESCO16  + BHAT * KNCOR * ESCO04 )
            VGHAT2C  = DCONST3C * ( ESCO48  + BHAT * KNCOR * ESCO20 )
            VGHAT3C  = DCONST3C * ( ESCO64  + BHAT * KNCOR * ESCO28 )

C now calculate the deposition velocities

            NU = AMU( C,R ) / Met_Data%DENS1( C,R )
            USTFAC = Met_Data%USTAR( C,R ) ** 2 / ( GRAV * NU )
            STOKEN = DCONST3N * USTFAC
            STOKEA = DCONST3A * USTFAC
            STOKEC = DCONST3C * USTFAC
            UTSCALE = Met_Data%USTAR( C,R )
     &              + 0.24 * Met_Data%WSTAR( C,R )**2
     &              /        Met_Data%USTAR( C,R )

C first do 0th moment for the deposition of number

C  Aitken mode
            SC0N = NU / DCHAT0N
            EIM = STOKEN ** 2 / 400.0 * ESAT64
            EIM = MIN( EIM, 1.0 )
            RD0N = 1.0 / ( UTSCALE * ( SC0N ** ( -TWO3 ) + EIM ) )

            VDEP( C,R,VDNATK ) = VGHAT0N
     &             / ( 1.0 - EXP( -VGHAT0N * ( Met_Data%RA( C,R ) + RD0N ) ) )

C accumulation mode
            SC0A = NU / DCHAT0A
            EIM = STOKEA ** 2 / 400.0 * ESAC64
            EIM = MIN( EIM, 1.0 )
            RD0A = 1.0 / ( UTSCALE * ( SC0A ** ( -TWO3 ) + EIM ) )

            VDEP( C,R,VDNACC ) = VGHAT0A
     &             / ( 1.0 - EXP( -VGHAT0A * ( Met_Data%RA( C,R ) + RD0A ) ) )

C coarse mode
            SC0C = NU / DCHAT0C
            EIM = STOKEC ** 2 / 400.0 * ESCO64
            EIM = MIN( EIM, 1.0 )
            RD0C = 1.0 / ( UTSCALE * ( SC0C ** ( -TWO3 ) + EIM ) )

            VDEP( C,R,VDNCOR ) = VGHAT0C
     &             / ( 1.0 - EXP( -VGHAT0C * ( Met_Data%RA( C,R ) + RD0C ) ) )

C now do 2nd moment for the deposition of surface area

C  Aitken mode
            SC2N = NU / DCHAT2N
            EIM = STOKEN ** 2 / 400.0 * ESAT128
            EIM = MIN( EIM, 1.0 )
            RD2N = 1.0 / ( UTSCALE * ( SC2N ** ( -TWO3 ) + EIM ) )

            VDEP( C,R,VDSATK ) = VGHAT2N
     &             / ( 1.0 - EXP( -VGHAT2N * ( Met_Data%RA( C,R ) + RD2N ) ) )

C accumulation mode
            SC2A = NU / DCHAT2A
            EIM = STOKEA ** 2 / 400.0 * ESAC128
            EIM = MIN( EIM, 1.0 )
            RD2A = 1.0 / ( UTSCALE * ( SC2A ** ( -TWO3 ) + EIM ) )

            VDEP( C,R,VDSACC ) = VGHAT2A
     &             / ( 1.0 - EXP( -VGHAT2A * ( Met_Data%RA( C,R ) + RD2A ) ) )

C coarse mode
            SC2C = NU / DCHAT2C
            EIM = STOKEC ** 2 / 400.0 * ESCO128
            EIM = MIN( EIM, 1.0 )
            RD2C = 1.0 / ( UTSCALE * ( SC2C ** ( -TWO3 ) + EIM ) )

            VDEP( C,R,VDSCOR ) = VGHAT2C
     &             / ( 1.0 - EXP( -VGHAT2C * ( Met_Data%RA( C,R ) + RD2C ) ) )

C now do 3rd moment for the deposition of mass

C  Aitken mode
            SC3N = NU / DCHAT3N
            EIM = STOKEN ** 2 / 400.0 * ESAT160
            EIM = MIN( EIM, 1.0 )
            RD3N = 1.0 / ( UTSCALE * ( SC3N ** ( -TWO3 ) + EIM ) )

            VDEP( C,R,VDMATK ) = VGHAT3N
     &             / ( 1.0 - EXP( -VGHAT3N * ( Met_Data%RA( C,R ) + RD3N ) ) )

C accumulation mode
            SC3A = NU / DCHAT3A
            EIM = STOKEA ** 2 / 400.0 * ESAC160
            EIM = MIN( EIM, 1.0 )
            RD3A = 1.0 / ( UTSCALE * ( SC3A ** ( -TWO3 ) + EIM ) )

            VDEP( C,R,VDMACC ) = VGHAT3A
     &             / ( 1.0 - EXP( -VGHAT3A * ( Met_Data%RA( C,R ) + RD3A ) ) )

C coarse mode
            SC3C = NU / DCHAT3C
            EIM = STOKEC ** 2 / 400.0 * ESCO160
            EIM = MIN( EIM, 1.0 )
            RD3C = 1.0 / ( UTSCALE * ( SC3C ** ( -TWO3 ) + EIM ) )

            VDEP( C,R,VDMCOR ) = VGHAT3C
     &             / ( 1.0 - EXP( -VGHAT3C * ( Met_Data%RA( C,R ) + RD3C ) ) )

C Do mosaic calculations - essentially a repeat of the above, using the mosaic vars
            IF ( MOSAIC ) THEN

               DO J = 1, N_LUFRAC
                  IF ( Mosaic_Data%RA( C,R,J ) .GT. 0.0 .And. Grid_Data%LUFRAC( C,R,J ) .GT. 0.0 ) THEN

C now calculate the deposition velocities

                     NU = AMU( C,R ) / Met_Data%DENS1( C,R )
                     USTFAC = Mosaic_Data%USTAR( C,R,J )**2 / ( GRAV * NU )
                     STOKEN = DCONST3N * USTFAC
                     STOKEA = DCONST3A * USTFAC
                     STOKEC = DCONST3C * USTFAC
                     UTSCALE = Mosaic_Data%USTAR( C,R,J )
     &                       + 0.24 * Met_Data%WSTAR( C,R )**2
     &                       /        Mosaic_Data%USTAR( C,R,J )

C first do 0th moment for the deposition of number

C  Aitken mode

                     SC0N = NU / DCHAT0N
                     EIM = STOKEN ** 2 / 400.0 * ESAT64
                     EIM = MIN( EIM, 1.0 )
                     RD0N = 1.0 / ( UTSCALE * ( SC0N ** ( -TWO3 ) + EIM ) )

                     VDEPJ( J,C,R,VDNATK ) = VGHAT0N
     &                       / ( 1.0 - EXP(-VGHAT0N * ( Mosaic_Data%RA( C,R,J ) + RD0N ) ) )

C accumulation mode

                     SC0A = NU / DCHAT0A
                     EIM = STOKEA ** 2 / 400.0 * ESAC64
                     EIM = MIN( EIM, 1.0 )
                     RD0A = 1.0 / ( UTSCALE * ( SC0A ** ( -TWO3 ) + EIM ) )

                     VDEPJ( J,C,R,VDNACC ) = VGHAT0A
     &                       / ( 1.0 - EXP(-VGHAT0A * ( Mosaic_Data%RA( C,R,J ) + RD0A ) ) )

C coarse mode

                     SC0C = NU / DCHAT0C
                     EIM = STOKEC ** 2 / 400.0 * ESCO64
                     EIM = MIN( EIM, 1.0 )
                     RD0C = 1.0 / ( UTSCALE * ( SC0C ** ( -TWO3 ) + EIM ) )

                     VDEPJ( J,C,R,VDNCOR ) = VGHAT0C
     &                       / ( 1.0 - EXP(-VGHAT0C * ( Mosaic_Data%RA( C,R,J ) + RD0C ) ) )

C now do 2nd moment for the deposition of surface area

C  Aitken mode

                     SC2N = NU / DCHAT2N
                     EIM = STOKEN ** 2 / 400.0 * ESAT128
                     EIM = MIN( EIM, 1.0 )
                     RD2N = 1.0 / ( UTSCALE * ( SC2N ** ( -TWO3 ) + EIM ) )

                     VDEPJ( J,C,R,VDSATK ) = VGHAT2N
     &                     / ( 1.0 - EXP(-VGHAT2N * ( Mosaic_Data%RA( C,R,J ) + RD2N ) ) )

C accumulation mode

                     SC2A = NU / DCHAT2A
                     EIM = STOKEA ** 2 / 400.0 * ESAC128
                     EIM = MIN( EIM, 1.0 )
                     RD2A = 1.0 / ( UTSCALE * ( SC2A ** ( -TWO3 ) + EIM ) )

                     VDEPJ( J,C,R,VDSACC ) = VGHAT2A
     &                       / ( 1.0 - EXP(-VGHAT2A * ( Mosaic_Data%RA( C,R,J ) + RD2A ) ) )

C coarse mode

                     SC2C = NU / DCHAT2C
                     EIM = STOKEC ** 2 / 400.0 * ESCO128
                     EIM = MIN( EIM, 1.0 )
                     RD2C = 1.0 / ( UTSCALE * ( SC2C ** ( -TWO3 ) + EIM ) )

                     VDEPJ( J,C,R,VDSCOR ) = VGHAT2C
     &                       / ( 1.0 - EXP(-VGHAT2C * ( Mosaic_Data%RA( C,R,J ) + RD2C ) ) )

C now do 3rd moment for the deposition of mass

C  Aitken mode

                     SC3N = NU / DCHAT3N
                     EIM = STOKEN ** 2 / 400.0 * ESAT160
                     EIM = MIN( EIM, 1.0 )
                     RD3N = 1.0 / ( UTSCALE * ( SC3N ** ( -TWO3 ) + EIM ) )

                     VDEPJ( J,C,R,VDMATK ) = VGHAT3N
     &                       / ( 1.0 - EXP(-VGHAT3N * ( Mosaic_Data%RA( C,R,J ) + RD3N ) ) )

C accumulation mode

                     SC3A = NU / DCHAT3A
                     EIM = STOKEA ** 2 / 400.0 * ESAC160
                     EIM = MIN( EIM, 1.0 )
                     RD3A = 1.0 / ( UTSCALE * ( SC3A ** ( -TWO3 ) + EIM ) )

                     VDEPJ( J,C,R,VDMACC ) = VGHAT3A
     &                       / ( 1.0 - EXP(-VGHAT3A * ( Mosaic_Data%RA( C,R,J ) + RD3A ) ) )

C coarse mode

                     SC3C = NU / DCHAT3C
                     EIM = STOKEC ** 2 / 400.0 * ESCO160
                     EIM = MIN( EIM, 1.0 )
                     RD3C = 1.0 / ( UTSCALE * ( SC3C ** ( -TWO3 ) + EIM ) )

                     VDEPJ( J,C,R,VDMCOR ) = VGHAT3C
     &                     / ( 1.0 - EXP(-VGHAT3C * ( Mosaic_Data%RA( C,R,J ) + RD3C ) ) )

                  END IF ! RA > 0

               END DO ! n_lufrac

            END IF ! mosaic
         END DO ! end loop on C
      END DO ! end loop on R

      RETURN
      END SUBROUTINE GETDEP_V
