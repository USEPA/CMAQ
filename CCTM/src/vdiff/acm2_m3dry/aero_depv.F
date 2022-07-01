
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
!      SUBROUTINE AERO_DEPV ( CGRID, JDATE, JTIME, TSTEP, MOSAIC, VDEP_AE )
      SUBROUTINE AERO_DEPV ( CGRID, JDATE, JTIME, TSTEP, VDEP_AE )

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
!      USE Mosaic_Mod, Only: ADEPVJ  ! Shared mosaic variables
!      Use LSM_Mod, Only: N_LUFRAC
      USE ASX_DATA_MOD, Only: Met_Data

      IMPLICIT NONE

C Includes:

      INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments
      REAL,    POINTER       :: CGRID( :,:,:,: )
      INTEGER, INTENT( IN )  :: JDATE               ! current model date , coded YYYYDDD
      INTEGER, INTENT( IN )  :: JTIME               ! current model time , coded HHMMSS
      INTEGER, INTENT( IN )  :: TSTEP               ! model time step, coded HHMMSS
!      LOGICAL, INTENT( IN )  :: MOSAIC              ! use mosaic option
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

      REAL, ALLOCATABLE, SAVE  :: XXLSG( :,:,: )  ! log of standard deviation
      REAL, ALLOCATABLE, SAVE  :: DG( :,:,: )     ! geometric mean diameter
      REAL, ALLOCATABLE, SAVE  :: PDENS( :,:,: )  ! particle density         
      REAL, ALLOCATABLE, SAVE  :: XLM( :,: )      ! mean free path [ m ]
      REAL, ALLOCATABLE, SAVE  :: AMU( :,: )      ! dynamic viscosity [ kg m**-1 s**-1 ]

      REAL, ALLOCATABLE, SAVE :: VDEP( :,:,: )    ! deposition  velocity [ m/s ]
!      REAL, ALLOCATABLE, SAVE :: VDEPJ( :,:,:,: ) ! deposition  velocity [ m/s ]

      REAL M3_WET, M3SUBT, M3_DRY
      REAL M2_WET, M2_DRY

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      CHARACTER( 16 ), SAVE :: PNAME = 'AERO_DEPV'
      CHARACTER( 16 ) :: VNAME            ! varable name
      CHARACTER( 96 ) :: XMSG = ' '

      INTEGER  C, R, V, N, J, IM     ! loop counters
      INTEGER  SPC, S                ! species loop counter
      INTEGER  ALLOCSTAT
 
      INTERFACE
        SUBROUTINE GETDEP_V ( XLM, AMU, DG, XXLSG, PDENS,
     &                        VDEP, N_AE_DEP_SPC ) !VDEPJ, MOSAIC )
          REAL, INTENT( IN ) :: XLM( :,: )        ! atmospheric mean free path [ m ]
          REAL, INTENT( IN ) :: AMU( :,: )        ! atmospheric dynamic viscosity [ kg/(m s) ]
          REAL, INTENT( IN ) :: DG( :,:,: )       ! geometric mean diameter  [ m ]
          REAL, INTENT( IN ) :: XXLSG( :,:,: )    ! Standard Deviation
          REAL, INTENT( IN ) :: PDENS( :,:,: )    ! average particle density 
          REAL, INTENT( OUT ) :: VDEP( :,:,: )    ! deposition  velocity [ m/s ]
          !REAL, INTENT( OUT ) :: VDEPJ( :,:,:,: ) ! deposition  velocity [ m/s ] for each land use category
          !LOGICAL, INTENT( IN ) :: MOSAIC
          INTEGER, INTENT( IN ) :: N_AE_DEP_SPC
        END SUBROUTINE GETDEP_V
      END INTERFACE
 
C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.

         NCELLS = NCOLS * NROWS

C  Allocate arrays
         ALLOCATE ( XLM( NCOLS,NROWS ), AMU( NCOLS,NROWS ), 
     &              DG( NCOLS,NROWS,N_MODE ), XXLSG( NCOLS,NROWS,N_MODE ),
     &              PDENS( NCOLS,NROWS,N_MODE ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating XLM, AMU, DG, XXLSG, or PDENS.'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         
         ALLOCATE ( VDEP( NCOLS,NROWS,N_AE_DEP_SPC ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating VDEP'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
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

      DO R = 1, NROWS
      DO C = 1, NCOLS

C ***    Set meteorological data for the grid cell.
         AIRDENS = Met_Data%DENS1( C,R )
         AIRTEMP = Met_Data%TEMP2( C,R )
         AIRPRES = Met_Data%PRSFC( C,R )

C ***    extract grid cell concentrations of aero species from CGRID
C        into aerospc_conc in aero_data module
C        Also determines second moment from surface area and adds wet
C        species
#ifdef sens
         CALL EXTRACT_AERO( CGRID( C,R,1,: ), .TRUE., CGRID( C,R,:,: ), .FALSE. ) 
#else
         CALL EXTRACT_AERO( CGRID( C,R,1,: ), .TRUE. )
#endif

C ***    Calculate geometric mean diameters and standard deviations of the
C        "wet" size distribution
         CALL GETPAR( .FALSE. )     
 
C        Calculate mean free path [ m ]:
         XLM( C,R ) = 6.6328E-8 * STDATMPA * AIRTEMP / ( T0 * AIRPRES )

C ***    Calculate dynamic viscosity [ kg m-1 s-1 ]:
         AMU( C,R ) = 1.458E-6 * AIRTEMP * SQRT( AIRTEMP )
     &              / ( AIRTEMP + 110.4 )

         DO IM = 1,N_MODE
C           Save getpar values to arrays
            XXLSG( C,R,IM ) = AEROMODE_LNSG( IM )
            DG( C,R,IM )    = AEROMODE_DIAM( IM )
            PDENS( C,R,IM ) = AEROMODE_DENS( IM )

         END DO

      END DO ! Column LOOP
      END DO   ! Row LOOP

C *** Get dry deposition velocities:
      CALL GETDEP_V ( XLM, AMU, DG, XXLSG, PDENS,
     &                VDEP, N_AE_DEP_SPC ) !VDEPJ, MOSAIC )

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


      RETURN
      END SUBROUTINE AERO_DEPV

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE GETDEP_V ( XLM, AMU, DG, XXLSG, PDENS,
     &                      VDEP, N_AE_DEP_SPC ) !, VDEPJ, MOSAIC )

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
C 29 Aug 18 J.Pleim:  Revised formulation of dry deposition impaction term 
C                     so that it integrates the affect of mode width in the 
C                     Stokes number via the settling velocity rather than 
C                     adjusting the impaction term magnitude directly at the
C                     end. This change is believed to resolve massive 
C                     overprediction of deposition velocity for coarse-mode 
C                     particles. Changed Stokes # to be more representative 
C                     of vegetated surfaces as suggested by Slinn (1982) and 
C                     Giorgi (1986), changed impaction term according to Slinn 
C                     (1982), and added scaling of diffusion layer resistance 
C                     (Rd) by LAI for vegetated fraction of grid cell. Developed 
C                     based on analysis by Qian Shu, University of Florida. 
C  Feb2022 J.Pleim:   Revised aerosol dry deposition model as described in Pleim et al, (2022)
C                     Note that this version only works for NLCD40, MODIS20 or USGS24
C-----------------------------------------------------------------------

      USE GRID_CONF, Only: ncols, nrows
      USE AEROMET_DATA   ! Includes CONST.EXT
      USE ASX_DATA_MOD, Only: Met_Data, Grid_Data !, Mosaic_Data
      USE AERO_DATA, Only: N_MODE
      Use LSM_Mod, Only: N_LUFRAC
      USE UTILIO_DEFN
      USE RUNTIME_VARS, ONLY: LOGDEV
      IMPLICIT NONE
      CHARACTER( 96 ) :: XMSG = ' '

C *** input arguments
      INTEGER, INTENT( IN ) :: N_AE_DEP_SPC

C     atmospheric properties
      REAL, INTENT( IN ) :: XLM( :,: )  ! atmospheric mean free path [ m ]
      REAL, INTENT( IN ) :: AMU( :,: )  ! atmospheric dynamic viscosity [ kg/(m s) ]

C     aerosol properties:

C     modal diameters: [ m ]
      REAL, INTENT( IN ) :: DG( :,:,: ) 

C     log of modal geometric standard deviations
      REAL, INTENT( IN ) :: XXLSG( :,:,: )  

C     average modal particle densities  [ kg m-3 ]
      REAL, INTENT( IN ) :: PDENS( :,:,: )  

C *** output arguments

      ! deposition  velocity [ m/s ]
      REAL, INTENT( OUT ) :: VDEP( :,:,: ) 
      
C *** array indices hardcoded to match SUBROUTINE AERO_DEPV
      INTEGER, PARAMETER, DIMENSION( 3 ) :: 
     &                      VDN = (/ 1,2,3 /) , 
     &                      VDM = (/ 4,5,6 /) , 
     &                      VDS = (/ 7,8,9 /)  

C modal Knudsen number
      REAL KN 

C modal particle diffusivities for number, 2nd, and 3rd moment, or mass:
      REAL DCHAT0
      REAL DCHAT2
      REAL DCHAT3

C modal sedimentation velocities for number, 2nd, and 3rd moment, or mass:
      REAL VGHAT0
      REAL VGHAT2
      REAL VGHAT3

      INTEGER NCELL, J, C, R, IM

      REAL DCONST,  DCONST1
      REAL DCONST2, DCONST3
      REAL SC0     ! Schmidt numbers for number
      REAL SC2     ! Schmidt numbers for 2nd moment
      REAL SC3     ! Schmidt numbers for 3rd moment
      REAL STOKE2  ! Stokes numbers for each mode squared
      REAL RD0     ! canopy resistance for number
      REAL RD2     ! canopy resistance for 2nd moment
      REAL RD3     ! canopy resisteance for 3rd moment
      REAL UTSCALE              ! scratch function of USTAR and WSTAR
      REAL NU                   ! kinematic viscosity [ m2 s-1 ]
      REAL STOKEFAC             ! scratch function of USTAR, NU, and GRAV

      REAL, PARAMETER :: BHAT    = 1.246 ! Constant from Cunningham slip correction
      REAL, PARAMETER :: THREEPI = 3.0 * PI
      REAL, PARAMETER :: TWO3    = 2.0 / 3.0

C Scalar variables for VARIABLE standard deviations.

      REAL    L2SG

      REAL    E1             ! mode exp( log^2( sigmag )/8 )
      REAL    ES04           !        " **4
      REAL    ES08           !        " **8
      REAL    ES12           !        " **12
      REAL    ES16           !        " **16
      REAL    ES20           !        " **20
      REAL    ES28           !        " **28
      REAL    ES32           !        " **32
      REAL    ES36           !        " **36
      REAL    ES48           !        " **48
      REAL    ES64           !        " **64
      REAL    ES128          !        " **128
      REAL    ES160          !        " **160
      REAL    ESM12          !        " **(-12)
      REAL    ESM16          !        " **(-16)
      REAL    ESM20          !        " **(-20)
      REAL    ESM32          !        " **(-32)
      REAL    EIM            ! Impaction efficiency

      REAL    TWOXLM

C Data for new impaction term (Pleim et al 2022)

      REAL, PARAMETER :: Fhair1  = 0.008
      REAL, PARAMETER :: Fhair2  = 0.002   ! for grass
      REAL, PARAMETER :: Ahair1  = 0.5e-6  ! micro obstacle size for needleaf and grass
      REAL, PARAMETER :: Ahair2  = 1.0e-6  ! micro obstacle size for other LU

      REAL Aleaf,Ahair,Fneedle,fveg,fnv,Ustfac,laicr,BAI,Fhair,Fgrass
      REAL U10,SST,awc,bwc,alfbob,Ewc   ! for whitecap effects
      REAL STOKEFAC1,STOKEFAC2,Eb,Eim1,Eim2,Vdv,Vdnv,Stoke

C leaf-scale characteristic length (mm) by LU 
C the 40 categories are for NLCD40 and first 20 are for MODIS20
      REAL, DIMENSION(40) :: Aleaf_lu,BAI_lu
      DATA Aleaf_lu                                    ! mm
     >        / 2.0,    10.0,   2.0,  10.0,    5.0,
     >          3.0,    2.0,    3.0,   3.0,    0.5,
     >          3.0,    4.0,    5.0,   1.0,    1.0,
     >          0.5,    1.0,    2.0,   1.0,    1.0,
     >          1.0,    1.0,    5.0,   5.0,    5.0,
     >          5.0,    0.5,   10.0,   2.0,    5.0,
     >          2.0,    2.0,    0.5,   1.0,    1.0,
     >          1.0,    0.5,    4.0,   5.0,    3.0  /
      DATA BAI_lu                                    
     >        / 1.0,    1.0,    1.0,   1.0,    1.0,
     >          1.0,    1.0,    1.0,   1.0,    1.0,
     >          1.0,    1.0,    2.0,   1.0,    1.0,
     >          1.0,    1.0,    1.0,   1.0,    1.0,
     >          1.0,    1.0,    1.0,   1.8,    2.0,
     >          2.3,    1.0,    1.0,   1.0,    1.0,
     >          1.0,    1.0,    1.0,   1.0,    1.0,
     >          1.0,    1.0,    1.0,   1.0,    1.0  /
C For USGS24
      REAL, DIMENSION(24) :: Aleaf_lu24,BAI_lu24
      DATA Aleaf_lu24                                    ! mm
     >        / 5.0,    4.0,    4.0,   4.0,    2.0,
     >          5.0,    0.5,    2.0,   1.0,    3.0,
     >          10.0,   2.0,    10.0,  2.0,    5.0,
     >          1.0,    3.0,    5.0,   0.5,    2.0,
     >          4.0,    3.0,    1.0,   1.0  /
      DATA BAI_lu24                                    
     >        / 2.0,    1.0,    1.0,   1.0,    1.0,
     >          1.0,    1.0,    1.0,   1.0,    1.0,
     >          1.0,    1.0,    1.0,   1.0,    1.0,
     >          1.0,    1.0,    1.0,   1.0,    1.0,
     >          1.0,    1.0,    1.0,   1.0  /
C-----------------------------------------------------------------------

      VDEP  = 0.0   ! array assignment

      DO IM = 1,N_MODE
      DO R = 1,NROWS
        DO C = 1,NCOLS
C *** Calculate Knudsen numbers
            TWOXLM = XLM( C,R ) + XLM( C,R )
            KN = TWOXLM / DG( C,R,IM )

C *** Calculate functions of variable standard deviation.

            L2SG = XXLSG( C,R,IM ) ** 2

            E1   = EXP( 0.125 * L2SG )
            ES04 = E1 ** 4
            ES08 = ES04 * ES04
            ES12 = ES04 * ES08
            ES16 = ES08 * ES08
            ES20 = ES16 * ES04
            ES28 = ES20 * ES08
            ES32 = ES16 * ES16
            ES36 = ES16 * ES20
            ES48 = ES36 * ES12
            ES64 = ES32 * ES32
            ES128= ES64 * ES64
            ES160= ES128* ES32

C *** calculate inverses:

            ESM12 = 1.0 / ES12
            ESM16 = 1.0 / ES16
            ESM20 = 1.0 / ES20
            ESM32 = 1.0 / ES32

            DCONST  = BOLTZMANN * Met_Data%TEMP2( C,R ) / ( THREEPI * AMU( C,R ) )
            DCONST1 = DCONST / DG( C,R,IM )

            DCONST2 = GRAV / ( 18.0 * AMU( C,R ) )
            DCONST3 = DCONST2 * PDENS( C,R,IM ) * DG( C,R,IM ) * DG( C,R,IM )
C Calculate characteristic parameters
            DCHAT0  = DCONST1 * ( ES04  + BHAT * KN * ES16 )
            DCHAT2  = DCONST1 * ( ESM12 + BHAT * KN * ESM16 )
            DCHAT3  = DCONST1 * ( ESM20 + BHAT * KN * ESM32 )
            VGHAT0  = DCONST3 * ( ES16  + BHAT * KN * ES04 )
            VGHAT2  = DCONST3 * ( ES48  + BHAT * KN * ES20 )
            VGHAT3  = DCONST3 * ( ES64  + BHAT * KN * ES28 )

! Set scale parameters for 2-term impaction by LU - For USGS24
            Aleaf =  0.0
            BAI = 0.0
            if (n_lufrac .eq. 24) Then
              Do j = 1, n_lufrac
                If ( GRID_DATA%LUFRAC( c,r,j ) .Gt. 0.0 ) Then
                  Aleaf =  Aleaf + Aleaf_lu24(j) * GRID_DATA%LUFRAC( c,r,j )
                  BAI =  BAI + BAI_lu24(j) * GRID_DATA%LUFRAC( c,r,j )
                endif
             enddo
            else
! Set scale parameters for 2-term impaction by LU - For NLCD40 or MODIS20
              Do j = 1, n_lufrac
                If ( GRID_DATA%LUFRAC( c,r,j ) .Gt. 0.0 ) Then
                  Aleaf =  Aleaf + Aleaf_lu(j) * GRID_DATA%LUFRAC( c,r,j )
                  BAI =  BAI + BAI_lu(j) * GRID_DATA%LUFRAC( c,r,j )
                endif
             enddo
           endif

           Aleaf = Aleaf * 1.e-3   ! mm to m
           If (n_lufrac .eq. 40) Then
             Fgrass =  GRID_DATA%LUFRAC( c,r,10 )   !Modis grass
     &               + GRID_DATA%LUFRAC( c,r,33 )   !NLCD grass
             Fneedle = GRID_DATA%LUFRAC( c,r,1 )    !Modis Evergreen Needleleaf
     &               + GRID_DATA%LUFRAC( c,r,3 )    !Modis Deciduous Needleleaf
     &               + GRID_DATA%LUFRAC( c,r,29 )   !NLCD Evergreen Forest
     &               + Fgrass
           else If(n_lufrac .eq. 20) Then
             Fgrass =  GRID_DATA%LUFRAC( c,r,10 )   !Modis grass
             Fneedle = GRID_DATA%LUFRAC( c,r,1 )    !Modis Evergreen Needleleaf
     &               + GRID_DATA%LUFRAC( c,r,3 )    !Modis Deciduous Needleleaf
     &               + Fgrass
           else if(n_lufrac .eq. 24) Then
             Fgrass =  GRID_DATA%LUFRAC( c,r,7 )        !USGS grass
     &               + 0.5 * GRID_DATA%LUFRAC( c,r,5 )  !USGS Grassland/Cropland Mosaic
             Fneedle = GRID_DATA%LUFRAC( c,r,14 )       !USGS Evergreen Needleleaf
     &               + GRID_DATA%LUFRAC( c,r,12 )       !USGS Deciduous Needleleaf
     &               + Fgrass
           else
             XMSG = 'LU scheme not supported in new aero_depv'
             CALL M3EXIT( 'aero_depv', 0, 0, XMSG, XSTAT1 )

           endif

           Ahair = Ahair2 * (1-Fneedle) + Ahair1 * Fneedle
           Fhair = Fhair1 * (1-Fgrass) + Fhair2 * Fgrass
! Different formulations for vegetated and non-vegetated parts
           fveg = Met_Data%VEG(C,R)
           IF ( NINT(GRID_DATA%LWMASK( c,r )).eq.0) fveg = 0.0
           LAIcr = max(Met_Data%LAI(C,R),1.0)
           fnv = 1.-fveg
!           if(c.eq.1.and.r.eq.1) Write(Logdev,*) 
!     >         ' Aleaf,Ahair,fveg,fnv,fneedle=',Aleaf,Ahair,fveg,fnv,fneedle

! For water include effects of whitecaps - Hummelshoj et al. (1992)
          IF ( NINT(GRID_DATA%LWMASK( c,r )) .EQ. 0 .AND. MET_DATA%SEAICE(c,r) 
     &           .LE. 0.5 ) Then
            U10 = Met_Data%WSPD10(C,R)
            SST = Met_Data%TSEASFC(C,R) - 273.15   ! C
            awc = 8.46e-5 + 1.63e-6 * SST - 3.35e-8 * SST**2
            bwc = 3.354 - 0.062 * SST
            alfbob = awc*(u10+bwc)**2        ! Albert 2016 with SST deg-C
            Ewc =  alfbob*Met_Data%USTAR( C,R )/u10            
          ENDIF
C now calculate the deposition velocities

            NU = AMU( C,R ) / Met_Data%DENS1( C,R )
            USTFAC = Met_Data%USTAR( C,R ) ** 2 / ( GRAV * NU )

            STOKEFAC1 = Met_Data%USTAR( C,R ) / (GRAV * Aleaf)
            STOKEFAC2 = Met_Data%USTAR( C,R ) / (GRAV * Ahair)
C first do 0th moment for the deposition of number
            SC0 = NU / DCHAT0
            IF ( NINT(GRID_DATA%LWMASK( c,r )) .EQ. 0 .AND. MET_DATA%SEAICE(c,r) 
     &           .LE. 0.5 .AND. SST .GT. -31.) then
              Eb = (1-alfbob) * SC0 ** ( -TWO3 )/3. + Ewc
            ELSE
              Eb = SC0 ** ( -TWO3 )/3.
            ENDIF
! Vegetated land
          Vdv = 0.0
          IF ( ( NINT(GRID_DATA%LWMASK( c,r )) .NE. 0 ) .AND. ( fveg.GT. 0.001 ) ) THEN  
            STOKE2 = (STOKEFAC1 * VGHAT0)**2
            EIM1 = (1-Fhair)*STOKE2 / (1.0 + STOKE2)
            STOKE2 = (STOKEFAC2 * VGHAT0)**2
            EIM2 = Fhair*STOKE2 / (1.0 + STOKE2)
            EIM = EIM1 + EIM2
            RD0 = 1.0 / ( Met_Data%USTAR( C,R )*laicr * ( Eb + EIM ) )

            Vdv = VGHAT0 / ( 1.0 - EXP( -VGHAT0 * ( Met_Data%RA( C,R ) + RD0 ) ) )
          endif
! Non-vege part
          Vdnv=0.0
          if(fnv.gt. 0.001) Then
            Stoke = USTFAC * VGHAT0
            Eim = 10.**(-3./Stoke)
            RD0 = 1.0 / ( Met_Data%USTAR( C,R )*BAI*(Eb + EIM ) )
            Vdnv = VGHAT0 / ( 1.0 - EXP( -VGHAT0 * ( Met_Data%RA( C,R ) + RD0 ) ) )
          endif
            VDEP( C,R,VDN( IM ) ) = fveg*Vdv + fnv*Vdnv

C now do 2nd moment for the deposition of surface area
            SC2 = NU / DCHAT2
            IF ( NINT(GRID_DATA%LWMASK( c,r )) .EQ. 0 .AND. MET_DATA%SEAICE(c,r) 
     &           .LE. 0.5 .AND. SST .GT. -31.) then
              Eb = (1-alfbob) * SC2 ** ( -TWO3 )/3. + Ewc
            ELSE
              Eb = SC2 ** ( -TWO3 )/3.
            ENDIF
! Vegetated land
          Vdv = 0.0
          IF ( ( NINT(GRID_DATA%LWMASK( c,r )) .NE. 0 ) .AND. ( fveg.GT. 0.0 ) ) THEN  
            STOKE2 = (STOKEFAC1 * VGHAT2)**2
            EIM1 = (1-Fhair)*STOKE2 / (1.0 + STOKE2)
            STOKE2 = (STOKEFAC2 * VGHAT2)**2
            EIM2 = Fhair*STOKE2 / (1.0 + STOKE2)
            EIM = EIM1 + EIM2
            RD2 = 1.0 / ( Met_Data%USTAR( C,R )*laicr * ( Eb + EIM ) )
            Vdv = VGHAT2
     &             / ( 1.0 - EXP( -VGHAT2 * ( Met_Data%RA( C,R ) + RD2 ) ) )
          endif
 ! Non-vege part
          Vdnv=0.0
          if(fnv.gt. 0.001) Then
            Stoke = USTFAC * VGHAT2
            Eim = 10.**(-3./Stoke)
            RD2 = 1.0 / (  Met_Data%USTAR( C,R )*BAI*(Eb + EIM ) )
            Vdnv = VGHAT2 / ( 1.0 - EXP( -VGHAT2 * ( Met_Data%RA( C,R ) + RD2 ) ) )
          endif
            VDEP( C,R,VDS( IM ) ) = fveg*Vdv + fnv*Vdnv
C now do 3rd moment for the deposition of mass
            SC3 = NU / DCHAT3
            IF ( NINT(GRID_DATA%LWMASK( c,r )) .EQ. 0 .AND. MET_DATA%SEAICE(c,r) 
     &           .LE. 0.5 .AND. SST .GT. -31.) then
              Eb = (1-alfbob) * SC3 ** ( -TWO3 )/3. + Ewc
            ELSE
              Eb = SC3 ** ( -TWO3 )/3.
            ENDIF
! Vegetated land
          Vdv = 0.0
          IF ( ( NINT(GRID_DATA%LWMASK( c,r )) .NE. 0 ) .AND. ( fveg.GT. 0.0 ) ) THEN  
            STOKE2 = (STOKEFAC1 * VGHAT3)**2
            EIM1 = (1-Fhair)*STOKE2 / (1.0 + STOKE2)
            STOKE2 = (STOKEFAC2 * VGHAT3)**2
            EIM2 = Fhair*STOKE2 / (1.0 + STOKE2)
            EIM = EIM1 + EIM2
            RD3 = 1.0 / ( Met_Data%USTAR( C,R )*laicr * ( Eb + EIM ) )
            Vdv = VGHAT3
     &             / ( 1.0 - EXP( -VGHAT3 * ( Met_Data%RA( C,R ) + RD3 ) ) )
          endif
! Non-vege part
          Vdnv=0.0
          if(fnv.gt. 0.001) Then
            Stoke = USTFAC * VGHAT3
            Eim = 10.**(-3./Stoke)
            RD3 = 1.0 / ( Met_Data%USTAR( C,R )*BAI*(Eb + EIM ) )
            Vdnv = VGHAT3
     &             / ( 1.0 - EXP( -VGHAT3 * ( Met_Data%RA( C,R ) + RD3 ) ) )
          endif
            VDEP( C,R,VDM( IM ) ) = fveg*Vdv + fnv*Vdnv
        END DO ! end loop on C
      END DO ! end loop on R
      END DO ! aerosol mode

      RETURN
      END SUBROUTINE GETDEP_V
