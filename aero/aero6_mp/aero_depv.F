
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
C $Header: /project/yoj/arc/CCTM/src/aero/aero6_mp/aero_depv.F,v 1.7 2011/09/30 12:41:08 sjr Exp $

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
C-----------------------------------------------------------------------

      USE GRID_CONF           ! horizontal & vertical domain specifications
      USE CGRID_SPCS          ! CGRID mechanism species
      USE UTILIO_DEFN      
      USE AERO_DATA           ! aero variable data
      USE SOA_DEFN            ! gas soa data
      USE AEROMET_DATA        ! constants and met data
      USE Mosaic_Mod, Only: ADEPVJ, USTARJ, RAJ  ! Shared mosaic variables
      Use LSM_Mod, Only: N_LUFRAC

      IMPLICIT NONE

C Includes:

      INCLUDE SUBST_RXCMMN    ! to get mech name
      INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments
      REAL,    POINTER       :: CGRID( :,:,:,: )
      INTEGER, INTENT( IN )  :: JDATE               ! current model date , coded YYYYDDD
      INTEGER, INTENT( IN )  :: JTIME               ! current model time , coded HHMMSS
      INTEGER, INTENT( IN )  :: TSTEP               ! model time step, coded HHMMSS
!      REAL,    INTENT( IN )  :: USTARJ( :,:,: )     ! friction velocity [m/s ] for each land use category
!      REAL,    INTENT( IN )  :: RAJ   ( :,:,: )     ! aerodynamic resistance [ s/m ] for each land use category
      LOGICAL, INTENT( IN )  :: MOSAIC              ! use mosaic option
      REAL,    INTENT( OUT ) :: VDEP_AE( :,:,: )    ! surrogate deposition velocities [ m s**-1 ]
!      REAL,    INTENT( OUT ) :: VDEPJ_AE( :,:,:,: ) ! surrogate deposition velocities [ m s**-1 ] for each land use category

C Parameters
      REAL,    PARAMETER :: T0 = 288.15      ! [ K ] ! starting standard surface temp.
      REAL,    PARAMETER :: P0 = 101325.0    ! standard surface pressure [ Pa ]
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
      DATA         VDAE_NAME( 1 ) / 'VNUMATKN        ' /
      DATA         VDAE_NAME( 2 ) / 'VNUMACC         ' /
      DATA         VDAE_NAME( 3 ) / 'VNUMCOR         ' /
      DATA         VDAE_NAME( 4 ) / 'VMASSI          ' /
      DATA         VDAE_NAME( 5 ) / 'VMASSJ          ' /
      DATA         VDAE_NAME( 6 ) / 'VMASSC          ' /
      DATA         VDAE_NAME( 7 ) / 'VSRFATKN        ' /
      DATA         VDAE_NAME( 8 ) / 'VSRFACC         ' /
      DATA         VDAE_NAME( 9 ) / 'VSRFCOR         ' /

      INTEGER, ALLOCATABLE, SAVE :: DEPV_SUR( : )   ! pointer to surrogate

C Meteorological variables
!     REAL PRES    ( NCOLS,NROWS )        ! Atmospheric pressure [ Pa ]
!     REAL TA      ( NCOLS,NROWS )        ! Air temperature [ K ]
!     REAL DENS    ( NCOLS,NROWS )        ! Air density [ kg/m**3 ]
!     REAL RA      ( NCOLS,NROWS )        ! aerodynamic resistance [ s/m ]
!     REAL USTAR   ( NCOLS,NROWS )        ! friction velocity [m/s ]
!     REAL WSTAR   ( NCOLS,NROWS )        ! convective velocity scale [m/s ]
!     REAL HEATFLUX( NCOLS,NROWS )        ! surface heat flux [W/m**2]

      REAL, ALLOCATABLE, SAVE :: PRES    ( :,: )        ! Atmospheric pressure [ Pa ]
      REAL, ALLOCATABLE, SAVE :: TA      ( :,: )        ! Air temperature [ K ]
      REAL, ALLOCATABLE, SAVE :: DENS    ( :,: )        ! Air density [ kg/m**3 ]
      REAL, ALLOCATABLE, SAVE :: RA      ( :,: )        ! aerodynamic resistance [ s/m ]
      REAL, ALLOCATABLE, SAVE :: USTAR   ( :,: )        ! friction velocity [m/s ]
      REAL, ALLOCATABLE, SAVE :: WSTAR   ( :,: )        ! convective velocity scale [m/s ]
      REAL, ALLOCATABLE, SAVE :: HEATFLUX( :,: )        ! surface heat flux [W/m**2]

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

      INTEGER  MDATE, MTIME, MSTEP   ! julian date, time and timestep in sec
      INTEGER  C, R, V, N, J         ! loop counters
      INTEGER  SPC, S                ! species loop counter
      INTEGER  ALLOCSTAT

      INTEGER      GXOFF, GYOFF              ! global origin offset from file
      INTEGER, SAVE :: STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2
      INTEGER, SAVE :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3

      INTERFACE
        SUBROUTINE GETDEP_V ( BLKTA, BLKDENS,
     &                        XLM, AMU,
     &                        BLKWSTAR, BLKUSTAR, BLKRA,
     &                        BLKUSTARJ, BLKRAJ, MOSAIC,
     &                        DGATK, DGACC, DGCOR,
     &                        XXLSGAT, XXLSGAC, XXLSGCO,
     &                        PDENSAT, PDENSAC, PDENSCO,
     &                        VDEP, VDEPJ )
          REAL, INTENT(IN) :: BLKTA  ( :,: )  ! air temperature [ K ]
          REAL, INTENT(IN) :: BLKDENS( :,: )  ! air density  [ kg/m**3 ]
          REAL, INTENT(IN) :: XLM( :,: )      ! atmospheric mean free path [ m ]
          REAL, INTENT(IN) :: AMU( :,: )      ! atmospheric dynamic viscosity [ kg/(m s) ]
          REAL, INTENT(IN) :: BLKWSTAR( :,: ) ! convective velocity scale [ m/s ]
          REAL, INTENT(IN) :: BLKUSTAR( :,: ) ! friction velocity [ m/s ]
          REAL, INTENT(IN) :: BLKRA   ( :,: ) ! aerodynamic resistance [ s/m ]
          REAL, INTENT(IN) :: BLKUSTARJ( :,:,: ) ! friction velocity [ m/s ]
          REAL, INTENT(IN) :: BLKRAJ   ( :,:,: ) ! aerodynamic resistance [ s/m ]
          LOGICAL, INTENT(IN) :: MOSAIC
          REAL, INTENT(IN) :: DGATK( :,: )    ! nuclei mode geometric mean diameter  [ m ]
          REAL, INTENT(IN) :: DGACC( :,: )    ! accumulation geometric mean diameter [ m ]
          REAL, INTENT(IN) :: DGCOR( :,: )    ! coarse mode geometric mean diameter  [ m ]
          REAL, INTENT(IN) :: XXLSGAT( :,: )  ! Aitken mode
          REAL, INTENT(IN) :: XXLSGAC( :,: )  ! accumulation mode
          REAL, INTENT(IN) :: XXLSGCO( :,: )  ! coarse mode
          REAL, INTENT(IN) :: PDENSAT( :,: )  ! average particle density in nuclei mode
          REAL, INTENT(IN) :: PDENSAC( :,: )  ! average particle density in accumulation mode
          REAL, INTENT(IN) :: PDENSCO( :,: )  ! average particle density in coarse mode
          REAL, INTENT(OUT) :: VDEP( :,:,: ) ! deposition  velocity [ m/s ]
          REAL, INTENT(OUT) :: VDEPJ( :,:,:,: ) ! deposition  velocity [ m/s ] for each land use category
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

!        ALLOCATE ( VDEP( NCELLS,N_AE_DEP_SPC ), STAT = ALLOCSTAT )
         ALLOCATE ( VDEP( NCOLS,NROWS,N_AE_DEP_SPC ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating VDEP'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( PRES    ( NCOLS,NROWS ),
     &              TA      ( NCOLS,NROWS ),
     &              DENS    ( NCOLS,NROWS ),
     &              RA      ( NCOLS,NROWS ),
     &              USTAR   ( NCOLS,NROWS ),
     &              WSTAR   ( NCOLS,NROWS ),
     &              HEATFLUX( NCOLS,NROWS ),
     &              STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating arrays'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

!           ALLOCATE ( VDEPJ( N_LUFRAC,NCELLS,N_AE_DEP_SPC ), STAT = ALLOCSTAT )
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

C Open the met files
         IF ( .NOT. OPEN3( MET_CRO_3D, FSREAD3, PNAME ) ) THEN
            XMSG = 'Could not open  MET_CRO_3D  file '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         IF ( .NOT. OPEN3( MET_CRO_2D, FSREAD3, PNAME ) ) THEN
            XMSG = 'Could not open  MET_CRO_2D file '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         IF ( .NOT. DESC3( MET_CRO_2D ) ) THEN
            XMSG = 'Could not get  MET_CRO_2D  file description '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
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

C *** get file horizontal grid offsets
         CALL SUBHFILE ( MET_CRO_2D, GXOFF, GYOFF,
     &                   STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2 )
         CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                   STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

      END IF    ! FIRSTIME      

      IF ( N_AE_SPC .LE. 0 ) RETURN

C begin interpolation of  meteorological variables

      MDATE  = JDATE
      MTIME  = JTIME

C Interpolate time dependent one-layer and layered input variables

ccccccccccccccccccccc enable backward compatiblity ccccccccccccccccccccc
      VNAME = 'RADYNI'
      IF (       INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                    STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                    MDATE, MTIME, RA ) ) THEN
         DO R = 1, MY_NROWS
            DO C = 1, MY_NCOLS
               RA( C,R ) = 1.0 / MAX( RA( C,R ), CONMIN )
            END DO
         END DO
      ELSE
         VNAME = 'RA'
         IF ( .NOT. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                       MDATE, MTIME, RA ) ) THEN
            XMSG = 'Could not interpolate '// VNAME // ' from MET_CRO_2D '
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF
      END IF
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      VNAME = 'USTAR'
      IF ( .NOT. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                    STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                    MDATE, MTIME, USTAR ) ) THEN
         XMSG = 'Could not interpolate '// VNAME // ' from MET_CRO_2D '
         CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
      END IF

      VNAME = 'WSTAR'
      IF ( .NOT. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                    STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                    MDATE, MTIME, WSTAR ) ) THEN
         XMSG = 'Could not interpolate '// VNAME // ' from MET_CRO_2D '
         CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
      END IF

      VNAME = 'HFX'
      IF ( .NOT. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                    STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                    MDATE, MTIME, HEATFLUX ) ) THEN
         XMSG = 'Could not interpolate '// VNAME // ' from MET_CRO_2D '
         CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
      END IF

C Layered variables TA, PRES, DENS:

      VNAME = 'PRES'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,1,
     &                    MDATE, MTIME, PRES ) ) THEN
         XMSG = 'Could not interpolate '// VNAME // ' from MET_CRO_3D '
         CALL M3EXIT (PNAME, MDATE, MTIME, XMSG, XSTAT1)
      END IF

      VNAME = 'TA'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,1,
     &                    MDATE, MTIME, TA ) ) THEN
         XMSG = 'Could not interpolate '// VNAME // ' from MET_CRO_3D '
         CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
      END IF

      VNAME = 'DENS'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,1,
     &                    MDATE, MTIME, DENS ) ) THEN
         XMSG = 'Could not interpolate '// VNAME // ' from MET_CRO_3D '
         CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
      END IF

C ----------End interpolation of meteorological variables -------------

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Put the grid cell physical data in the block arrays
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      DO R = 1, MY_NROWS
      DO C = 1, MY_NCOLS

C ***    Set meteorological data for the grid cell.
         AIRDENS = DENS( C,R )
         AIRTEMP = TA( C,R )
         AIRPRES = PRES( C,R )

C ***    extract grid cell concentrations of aero species from CGRID
C        into aerospc_conc in aero_data module
         CALL EXTRACT_AERO( CGRID( C,R,1,: ), .TRUE. )

C ***    extract soa concentrations from CGRID
         CALL EXTRACT_SOA( CGRID( C,R,1,: ) )

C ***    Calculate aerosol surface area to 2nd moment. 
         DO S = 1, N_MODE
            N = AEROSRF_MAP( S )
            MOMENT2_CONC( S ) = CGRID( C,R,1,N ) / SNGL( PI )
         END DO

C ***    Calculate geometric mean diameters and standard deviations of the
C        "dry" size distribution
         CALL GETPAR( .FALSE., .FALSE. ) 

C add contribution of water and SOA

C        Aitken mode.
         M3_DRY = MOMENT3_CONC( 1 )
         M3SUBT = ( 1.0E-9 * F6DPI / AEROSPC( AH2O_IDX )%DENSITY )
     &          * AEROSPC_CONC( AH2O_IDX,1 )
         M3_WET = M3_DRY + M3SUBT
         M2_DRY = MOMENT2_CONC( 1 )
         M2_WET= M2_DRY * ( M3_WET / M3_DRY ) ** TWO3

         MOMENT3_CONC( 1 ) = MAX( CONMIN, M3_WET )
         MOMENT2_CONC( 1 ) = MAX( CONMIN, M2_WET )

C        accumulation mode.
         M3_DRY = MOMENT3_CONC( 2 )

         M3SUBT = ( 1.0E-9 * F6DPI / AEROSPC( AH2O_IDX )%DENSITY )
     &          * AEROSPC_CONC( AH2O_IDX,2 )
         DO SPC = 1, N_VAPOR
            M3SUBT = M3SUBT + ( 1.0E-9 * F6DPI / AEROSPC( APOC_IDX )%DENSITY )
     &             * AEROSPC_CONC( SOA_AEROMAP( SPC ),2 )
         END DO

         M3_WET = M3_DRY + M3SUBT
         M2_DRY = MOMENT2_CONC(2)
         M2_WET = M2_DRY * ( M3_WET / M3_DRY ) ** TWO3

         MOMENT3_CONC( 2 ) = MAX( CONMIN, M3_WET )
         MOMENT2_CONC( 2 ) = MAX( CONMIN, M2_WET )

C        coarse mode
         M3_DRY = moment3_conc(3)
         M3SUBT = ( 1.0E-9 * F6DPI / AEROSPC( AH2O_IDX )%DENSITY )
     &          * AEROSPC_CONC( AH2O_IDX,3 )
         M3_WET = M3_DRY + M3SUBT
         M2_DRY = MOMENT2_CONC( 3 )
         M2_WET = M2_DRY * ( M3_WET / M3_DRY ) ** TWO3

         MOMENT3_CONC( 3 ) = MAX( CONMIN, M3_WET )
         MOMENT2_CONC( 3 ) = MAX( CONMIN, M2_WET )

C ***    Calculate geometric mean diameters and standard deviations of the
C        "wet" size distribution
         CALL GETPAR( .TRUE., .FALSE. )     

C        Save getpar values to arrays
         XXLSGAT( C,R ) = AEROMODE_SDEV( 1 )
         XXLSGAC( C,R ) = AEROMODE_SDEV( 2 )
         XXLSGCO( C,R ) = AEROMODE_SDEV( 3 )

         DGATK( C,R )   = AEROMODE_DIAM( 1 )
         DGACC( C,R )   = AEROMODE_DIAM( 2 )
         DGCOR( C,R )   = AEROMODE_DIAM( 3 )

         PDENSAT( C,R ) = AEROMODE_DENS( 1 )
         PDENSAC( C,R ) = AEROMODE_DENS( 2 )
         PDENSCO( C,R ) = AEROMODE_DENS( 3 )
 
C        Calculate mean free path [ m ]:
         XLM( C,R ) = 6.6328E-8 * P0 * AIRTEMP / ( T0 * AIRPRES )

C ***    Calcualte dynamic viscosity [ kg m**-1 s**-1 ]:
         AMU( C,R ) = 1.458E-6 * AIRTEMP * SQRT( AIRTEMP )
     &              / ( AIRTEMP + 110.4 )

      END DO ! Column LOOP
      END DO   ! Row LOOP

C *** get dry deposition velocities:

!     CALL GETDEP_V ( NCELLS, N_AE_DEP_SPC,
      CALL GETDEP_V ( TA, DENS,
     &                XLM, AMU,
     &                WSTAR, USTAR, RA,
     &                USTARJ, RAJ, MOSAIC,
     &                DGATK, DGACC, DGCOR,
     &                XXLSGAT, XXLSGAC, XXLSGCO,
     &                PDENSAT, PDENSAC, PDENSCO,
     &                VDEP, VDEPJ )

C Return dry deposition velocities for aerosols (first layer only).

!     N = 0
      DO R = 1, NROWS
         DO C = 1, NCOLS
!           N = N + 1
            DO V = 1, N_AE_DEPV
               IF ( DEPV_SUR( V ) .GT. 0 ) THEN
!                 VDEP_AE( V,C,R ) = VDEP( N,DEPV_SUR( V ) )
                  VDEP_AE( V,C,R ) = VDEP( C,R,DEPV_SUR( V ) )
               ELSE
                  VDEP_AE( V,C,R ) = 0.0
               END IF
            END DO
         END DO
      END DO

!     N = 0
      IF ( MOSAIC ) THEN
        DO R = 1, NROWS
           DO C = 1, NCOLS
!             N = N + 1
              DO V = 1, N_AE_DEPV
                DO J = 1, N_LUFRAC
                  IF ( DEPV_SUR( V ) .GT. 0 ) THEN
!                    ADEPVJ( J,V,C,R ) = VDEPJ( J,N,DEPV_SUR( V ) )
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
!     SUBROUTINE GETDEP_V ( NUMCELLS, N_AE_DEP_SPC,
      SUBROUTINE GETDEP_V ( BLKTA, BLKDENS,
     &                      XLM, AMU,
     &                      BLKWSTAR, BLKUSTAR, BLKRA,
     &                      BLKUSTARJ, BLKRAJ, MOSAIC,
     &                      DGATK, DGACC, DGCOR,
     &                      XXLSGAT, XXLSGAC, XXLSGCO,
     &                      PDENSAT, PDENSAC, PDENSCO,
     &                      VDEP, VDEPJ )

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

      IMPLICIT NONE

C *** input arguments

!     INTEGER NUMCELLS
!     INTEGER N_AE_DEP_SPC

C     meteorological information in blocked arays:
!     REAL BLKTA  ( NUMCELLS )  ! air temperature [ K ]
!     REAL BLKDENS( NUMCELLS )  ! air density  [ kg/m**3 ]
      REAL, INTENT(IN) :: BLKTA  ( :,: )  ! air temperature [ K ]
      REAL, INTENT(IN) :: BLKDENS( :,: )  ! air density  [ kg/m**3 ]

C     atmospheric properties
!     REAL XLM( NUMCELLS )      ! atmospheric mean free path [ m ]
!     REAL AMU( NUMCELLS )      ! atmospheric dynamic viscosity [ kg/(m s) ]
      REAL, INTENT(IN) :: XLM( :,: )      ! atmospheric mean free path [ m ]
      REAL, INTENT(IN) :: AMU( :,: )      ! atmospheric dynamic viscosity [ kg/(m s) ]

C     Planetary boundary laryer (PBL) variables
!     REAL BLKWSTAR( NUMCELLS ) ! convective velocity scale [ m/s ]
!     REAL BLKUSTAR( NUMCELLS ) ! friction velocity [ m/s ]
!     REAL BLKRA   ( NUMCELLS ) ! aerodynamic resistance [ s/m ]
      REAL, INTENT(IN) :: BLKWSTAR( :,: ) ! convective velocity scale [ m/s ]
      REAL, INTENT(IN) :: BLKUSTAR( :,: ) ! friction velocity [ m/s ]
      REAL, INTENT(IN) :: BLKRA   ( :,: ) ! aerodynamic resistance [ s/m ]

C     Mosaic variables
!     REAL BLKUSTARJ( N_LUFRAC,NUMCELLS ) ! friction velocity [ m/s ]
!     REAL BLKRAJ   ( N_LUFRAC,NUMCELLS ) ! aerodynamic resistance [ s/m ]
      REAL, INTENT(IN) :: BLKUSTARJ( :,:,: ) ! friction velocity [ m/s ]
      REAL, INTENT(IN) :: BLKRAJ   ( :,:,: ) ! aerodynamic resistance [ s/m ]
      LOGICAL, INTENT(IN) :: MOSAIC

C     aerosol properties:

C     modal diameters: [ m ]
!     REAL DGATK( NUMCELLS )    ! nuclei mode geometric mean diameter  [ m ]
!     REAL DGACC( NUMCELLS )    ! accumulation geometric mean diameter [ m ]
!     REAL DGCOR( NUMCELLS )    ! coarse mode geometric mean diameter  [ m ]
      REAL, INTENT(IN) :: DGATK( :,: )    ! nuclei mode geometric mean diameter  [ m ]
      REAL, INTENT(IN) :: DGACC( :,: )    ! accumulation geometric mean diameter [ m ]
      REAL, INTENT(IN) :: DGCOR( :,: )    ! coarse mode geometric mean diameter  [ m ]

C     log of modal geometric standard deviations
!     REAL XXLSGAT( NUMCELLS )  ! Aitken mode
!     REAL XXLSGAC( NUMCELLS )  ! accumulation mode
!     REAL XXLSGCO( NUMCELLS )  ! coarse mode
      REAL, INTENT(IN) :: XXLSGAT( :,: )  ! Aitken mode
      REAL, INTENT(IN) :: XXLSGAC( :,: )  ! accumulation mode
      REAL, INTENT(IN) :: XXLSGCO( :,: )  ! coarse mode

C     average modal particle densities  [ kg/m**3 ]
!     REAL PDENSAT( NUMCELLS )  ! average particle density in nuclei mode
!     REAL PDENSAC( NUMCELLS )  ! average particle density in accumulation mode
!     REAL PDENSCO( NUMCELLS )  ! average particle density in coarse mode
      REAL, INTENT(IN) :: PDENSAT( :,: )  ! average particle density in nuclei mode
      REAL, INTENT(IN) :: PDENSAC( :,: )  ! average particle density in accumulation mode
      REAL, INTENT(IN) :: PDENSCO( :,: )  ! average particle density in coarse mode

C *** output arguments

!     REAL VDEP( NUMCELLS,N_AE_DEP_SPC ) ! deposition  velocity [ m/s ]
!     REAL VDEPJ( N_LUFRAC,NUMCELLS,N_AE_DEP_SPC ) ! deposition  velocity [ m/s ] for each land use category
      REAL, INTENT(OUT) :: VDEP( :,:,: ) ! deposition  velocity [ m/s ]
      REAL, INTENT(OUT) :: VDEPJ( :,:,:,: ) ! deposition  velocity [ m/s ] for each land use category

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

      REAL, PARAMETER :: BHAT     = 1.246 ! Constant from Cunningham slip correction
      REAL, PARAMETER :: PI       = 3.14159265 ! single precision
      REAL, PARAMETER :: THREEPI  = 3.0 * PI
      REAL, PARAMETER :: TWO3     = 2.0 / 3.0
      REAL, PARAMETER :: AVO      = 6.0221367 E23  ! Avogadro's Constant [ 1/mol ]
      REAL, PARAMETER :: RGASUNIV = 8.314510       ! universal gas const [ J/mol-K ]
      REAL, PARAMETER :: BOLTZ    = RGASUNIV / AVO ! Boltzmann's Constant [ J/K ]
      REAL, PARAMETER :: GRAV     = 9.80622  ! mean gravitational accel [ m/sec**2 ]
                                             ! FSB NOTE: Value is now mean of polar
                                             ! and equatorial values. Source: CRC
                                             ! Handbook (76th Ed) page 14-6.

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
      IF (MOSAIC) THEN
         VDEPJ = 0.0   ! array assignment
      END IF

!     DO NCELL = 1, NUMCELLS
      DO R = 1, SIZE(BLKTA, 2)
         DO C = 1, SIZE(BLKTA, 1)

C *** Calculate Knudsen numbers

!           KNATK = 2.0 * XLM( NCELL ) / DGATK( NCELL )
!           KNACC = 2.0 * XLM( NCELL ) / DGACC( NCELL )
!           KNCOR = 2.0 * XLM( NCELL ) / DGCOR( NCELL )

            KNATK = 2.0 * XLM( C, R ) / DGATK( C, R )
            KNACC = 2.0 * XLM( C, R ) / DGACC( C, R )
            KNCOR = 2.0 * XLM( C, R ) / DGCOR( C, R )

C *** Calculate functions of variable standard deviation.

!           L2SGAT = XXLSGAT( NCELL ) * XXLSGAT( NCEL L)
!           L2SGAC = XXLSGAC( NCELL ) * XXLSGAC( NCEL L)
!           L2SGCO = XXLSGCO( NCELL ) * XXLSGCO( NCEL L)

            L2SGAT = XXLSGAT( C, R ) * XXLSGAT( C, R )
            L2SGAC = XXLSGAC( C, R ) * XXLSGAC( C, R )
            L2SGCO = XXLSGCO( C, R ) * XXLSGCO( C, R )

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

!           DCONST1  = BOLTZ * BLKTA( NCELL ) / ( THREEPI * AMU( NCELL ) )
!           DCONST1N = DCONST1 / DGATK( NCELL )
!           DCONST1A = DCONST1 / DGACC( NCELL )
!           DCONST1C = DCONST1 / DGCOR( NCELL )
!           DCONST2  = GRAV / ( 18.0 * AMU( NCELL ) )
!           DCONST3N = DCONST2 * PDENSAT( NCELL ) * DGATK( NCELL ) * DGATK( NCELL )
!           DCONST3A = DCONST2 * PDENSAC( NCELL ) * DGACC( NCELL ) * DGACC( NCELL )
!           DCONST3C = DCONST2 * PDENSCO( NCELL ) * DGCOR( NCELL ) * DGCOR( NCELL )

            DCONST1  = BOLTZ * BLKTA( C, R ) / ( THREEPI * AMU( C, R ) )
            DCONST1N = DCONST1 / DGATK( C, R )
            DCONST1A = DCONST1 / DGACC( C, R )
            DCONST1C = DCONST1 / DGCOR( C, R )
            DCONST2  = GRAV / ( 18.0 * AMU( C, R ) )
            DCONST3N = DCONST2 * PDENSAT( C, R ) * DGATK( C, R ) * DGATK( C, R )
            DCONST3A = DCONST2 * PDENSAC( C, R ) * DGACC( C, R ) * DGACC( C, R )
            DCONST3C = DCONST2 * PDENSCO( C, R ) * DGCOR( C, R ) * DGCOR( C, R )

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

!           NU = AMU( NCELL ) / BLKDENS( NCELL )
!           USTFAC = BLKUSTAR( NCELL ) * BLKUSTAR( NCELL ) / ( GRAV * NU )
            NU = AMU( C, R ) / BLKDENS( C, R )
            USTFAC = BLKUSTAR( C, R ) * BLKUSTAR( C, R ) / ( GRAV * NU )
            STOKEN = DCONST3N * USTFAC
            STOKEA = DCONST3A * USTFAC
            STOKEC = DCONST3C * USTFAC
!           UTSCALE = BLKUSTAR( NCELL )
!    &              + 0.24 * BLKWSTAR( NCELL ) * BLKWSTAR( NCELL )
!    &              /        BLKUSTAR( NCELL )
            UTSCALE = BLKUSTAR( C, R )
     &              + 0.24 * BLKWSTAR( C, R ) * BLKWSTAR( C, R )
     &              /        BLKUSTAR( C, R )

C first do 0th moment for the deposition of number

C  Aitken mode
            SC0N = NU / DCHAT0N
            EIM = STOKEN ** 2 / 400.0 * ESAT64
            EIM = MIN( EIM, 1.0 )
            RD0N = 1.0 / ( UTSCALE * ( SC0N ** ( -TWO3 ) + EIM ) )

!           VDEP( NCELL,VDNATK ) = VGHAT0N
!    &             / ( 1.0 - EXP( -VGHAT0N * ( BLKRA( NCELL ) + RD0N ) ) )

            VDEP( C, R,VDNATK ) = VGHAT0N
     &             / ( 1.0 - EXP( -VGHAT0N * ( BLKRA( C, R ) + RD0N ) ) )

C accumulation mode
            SC0A = NU / DCHAT0A
            EIM = STOKEA ** 2 / 400.0 * ESAC64
            EIM = MIN( EIM, 1.0 )
            RD0A = 1.0 / ( UTSCALE * ( SC0A ** ( -TWO3 ) + EIM ) )

!           VDEP( NCELL,VDNACC ) = VGHAT0A
!    &             / ( 1.0 - EXP( -VGHAT0A * ( BLKRA( NCELL ) + RD0A ) ) )

            VDEP( C,R,VDNACC ) = VGHAT0A
     &             / ( 1.0 - EXP( -VGHAT0A * ( BLKRA( C,R ) + RD0A ) ) )

C coarse mode
            SC0C = NU / DCHAT0C
            EIM = STOKEC ** 2 / 400.0 * ESCO64
            EIM = MIN( EIM, 1.0 )
            RD0C = 1.0 / ( UTSCALE * ( SC0C ** ( -TWO3 ) + EIM ) )

!           VDEP( NCELL,VDNCOR ) = VGHAT0C
!    &             / ( 1.0 - EXP( -VGHAT0C * ( BLKRA( NCELL ) + RD0C ) ) )

            VDEP( C,R,VDNCOR ) = VGHAT0C
     &             / ( 1.0 - EXP( -VGHAT0C * ( BLKRA( C,R ) + RD0C ) ) )

C now do 2nd moment for the deposition of surface area

C  Aitken mode
            SC2N = NU / DCHAT2N
            EIM = STOKEN ** 2 / 400.0 * ESAT128
            EIM = MIN( EIM, 1.0 )
            RD2N = 1.0 / ( UTSCALE * ( SC2N ** ( -TWO3 ) + EIM ) )

!           VDEP( NCELL,VDSATK ) = VGHAT2N
!    &             / ( 1.0 - EXP( -VGHAT2N * ( BLKRA( NCELL ) + RD2N ) ) )

            VDEP( C,R,VDSATK ) = VGHAT2N
     &             / ( 1.0 - EXP( -VGHAT2N * ( BLKRA( C,R ) + RD2N ) ) )

C accumulation mode
            SC2A = NU / DCHAT2A
            EIM = STOKEA ** 2 / 400.0 * ESAC128
            EIM = MIN( EIM, 1.0 )
            RD2A = 1.0 / ( UTSCALE * ( SC2A ** ( -TWO3 ) + EIM ) )

!           VDEP( NCELL,VDSACC ) = VGHAT2A
!    &             / ( 1.0 - EXP( -VGHAT2A * ( BLKRA( NCELL ) + RD2A ) ) )

            VDEP( C,R,VDSACC ) = VGHAT2A
     &             / ( 1.0 - EXP( -VGHAT2A * ( BLKRA( C,R ) + RD2A ) ) )

C coarse mode
            SC2C = NU / DCHAT2C
            EIM = STOKEC ** 2 / 400.0 * ESCO128
            EIM = MIN( EIM, 1.0 )
            RD2C = 1.0 / ( UTSCALE * ( SC2C ** ( -TWO3 ) + EIM ) )

!           VDEP( NCELL,VDSCOR ) = VGHAT2C
!    &             / ( 1.0 - EXP( -VGHAT2C * ( BLKRA( NCELL ) + RD2C ) ) )

            VDEP( C,R,VDSCOR ) = VGHAT2C
     &             / ( 1.0 - EXP( -VGHAT2C * ( BLKRA( C,R ) + RD2C ) ) )

C now do 3rd moment for the deposition of mass

C  Aitken mode
            SC3N = NU / DCHAT3N
            EIM = STOKEN ** 2 / 400.0 * ESAT160
            EIM = MIN( EIM, 1.0 )
            RD3N = 1.0 / ( UTSCALE * ( SC3N ** ( -TWO3 ) + EIM ) )

!           VDEP( NCELL,VDMATK ) = VGHAT3N
!    &             / ( 1.0 - EXP( -VGHAT3N * ( BLKRA( NCELL ) + RD3N ) ) )

            VDEP( C,R,VDMATK ) = VGHAT3N
     &             / ( 1.0 - EXP( -VGHAT3N * ( BLKRA( C,R ) + RD3N ) ) )

C accumulation mode
            SC3A = NU / DCHAT3A
            EIM = STOKEA ** 2 / 400.0 * ESAC160
            EIM = MIN( EIM, 1.0 )
            RD3A = 1.0 / ( UTSCALE * ( SC3A ** ( -TWO3 ) + EIM ) )

!           VDEP( NCELL,VDMACC ) = VGHAT3A
!    &             / ( 1.0 - EXP( -VGHAT3A * ( BLKRA( NCELL ) + RD3A ) ) )

            VDEP( C,R,VDMACC ) = VGHAT3A
     &             / ( 1.0 - EXP( -VGHAT3A * ( BLKRA( C,R ) + RD3A ) ) )

C coarse mode
            SC3C = NU / DCHAT3C
            EIM = STOKEC ** 2 / 400.0 * ESCO160
            EIM = MIN( EIM, 1.0 )
            RD3C = 1.0 / ( UTSCALE * ( SC3C ** ( -TWO3 ) + EIM ) )

!           VDEP( NCELL,VDMCOR ) = VGHAT3C
!    &             / ( 1.0 - EXP( -VGHAT3C * ( BLKRA( NCELL ) + RD3C ) ) )

            VDEP( C,R,VDMCOR ) = VGHAT3C
     &             / ( 1.0 - EXP( -VGHAT3C * ( BLKRA( C,R ) + RD3C ) ) )

!     END DO ! end loop on cells

C Do mosaic calculations - essentially a repeat of the above, using the mosaic vars
            IF ( MOSAIC ) THEN

!        DO NCELL = 1, NUMCELLS ! Calculate Knudsen numbers
               DO J = 1, N_LUFRAC
!                 IF ( BLKRAJ( J,NCELL ) .GT. 0.0 ) THEN
                  IF ( BLKRAJ( J,C,R ) .GT. 0.0 ) THEN

C now calculate the deposition velocities

!                    NU = AMU( NCELL ) / BLKDENS( NCELL )
!                    USTFAC = BLKUSTARJ( J,NCELL ) * BLKUSTARJ( J,NCELL ) / ( GRAV * NU )
                     NU = AMU( C,R ) / BLKDENS( C,R )
                     USTFAC = BLKUSTARJ( J,C,R ) * BLKUSTARJ( J,C,R ) / ( GRAV * NU )
                     STOKEN = DCONST3N * USTFAC
                     STOKEA = DCONST3A * USTFAC
                     STOKEC = DCONST3C * USTFAC
!                    UTSCALE = BLKUSTARJ( J,NCELL )
!    &                       + 0.24 * BLKWSTAR( NCELL ) * BLKWSTAR( NCELL )
!    &                       /        BLKUSTARJ( J,NCELL )
                     UTSCALE = BLKUSTARJ( J,C,R )
     &                       + 0.24 * BLKWSTAR( C,R ) * BLKWSTAR( C,R )
     &                       /        BLKUSTARJ( J,C,R )

C first do 0th moment for the deposition of number

C  Aitken mode

                     SC0N = NU / DCHAT0N
                     EIM = STOKEN ** 2 / 400.0 * ESAT64
                     EIM = MIN( EIM, 1.0 )
                     RD0N = 1.0 / ( UTSCALE * ( SC0N ** ( -TWO3 ) + EIM ) )

!                    VDEPJ( J,NCELL,VDNATK ) = VGHAT0N
!    &                       / ( 1.0 - EXP(-VGHAT0N * ( BLKRAJ( J,NCELL ) + RD0N ) ) )

                     VDEPJ( J,C,R,VDNATK ) = VGHAT0N
     &                       / ( 1.0 - EXP(-VGHAT0N * ( BLKRAJ( J,C,R ) + RD0N ) ) )

C accumulation mode

                     SC0A = NU / DCHAT0A
                     EIM = STOKEA ** 2 / 400.0 * ESAC64
                     EIM = MIN( EIM, 1.0 )
                     RD0A = 1.0 / ( UTSCALE * ( SC0A ** ( -TWO3 ) + EIM ) )

!                    VDEPJ( J,NCELL,VDNACC ) = VGHAT0A
!    &                       / ( 1.0 - EXP(-VGHAT0A * ( BLKRAJ( J,NCELL ) + RD0A ) ) )

                     VDEPJ( J,C,R,VDNACC ) = VGHAT0A
     &                       / ( 1.0 - EXP(-VGHAT0A * ( BLKRAJ( J,C,R ) + RD0A ) ) )

C coarse mode

                     SC0C = NU / DCHAT0C
                     EIM = STOKEC ** 2 / 400.0 * ESCO64
                     EIM = MIN( EIM, 1.0 )
                     RD0C = 1.0 / ( UTSCALE * ( SC0C ** ( -TWO3 ) + EIM ) )

!                    VDEPJ( J,NCELL,VDNCOR ) = VGHAT0C
!    &                       / ( 1.0 - EXP(-VGHAT0C * ( BLKRAJ( J,NCELL ) + RD0C ) ) )

                     VDEPJ( J,C,R,VDNCOR ) = VGHAT0C
     &                       / ( 1.0 - EXP(-VGHAT0C * ( BLKRAJ( J,C,R ) + RD0C ) ) )

C now do 2nd moment for the deposition of surface area

C  Aitken mode

                     SC2N = NU / DCHAT2N
                     EIM = STOKEN ** 2 / 400.0 * ESAT128
                     EIM = MIN( EIM, 1.0 )
                     RD2N = 1.0 / ( UTSCALE * ( SC2N ** ( -TWO3 ) + EIM ) )

!                    VDEPJ( J,NCELL,VDSATK ) = VGHAT2N
!    &                     / ( 1.0 - EXP(-VGHAT2N * ( BLKRAJ( J,NCELL ) + RD2N ) ) )

                     VDEPJ( J,C,R,VDSATK ) = VGHAT2N
     &                     / ( 1.0 - EXP(-VGHAT2N * ( BLKRAJ( J,C,R ) + RD2N ) ) )

C accumulation mode

                     SC2A = NU / DCHAT2A
                     EIM = STOKEA ** 2 / 400.0 * ESAC128
                     EIM = MIN( EIM, 1.0 )
                     RD2A = 1.0 / ( UTSCALE * ( SC2A ** ( -TWO3 ) + EIM ) )

!                    VDEPJ( J,NCELL,VDSACC ) = VGHAT2A
!    &                       / ( 1.0 - EXP(-VGHAT2A * ( BLKRAJ( J,NCELL ) + RD2A ) ) )

                     VDEPJ( J,C,R,VDSACC ) = VGHAT2A
     &                       / ( 1.0 - EXP(-VGHAT2A * ( BLKRAJ( J,C,R ) + RD2A ) ) )

C coarse mode

                     SC2C = NU / DCHAT2C
                     EIM = STOKEC ** 2 / 400.0 * ESCO128
                     EIM = MIN( EIM, 1.0 )
                     RD2C = 1.0 / ( UTSCALE * ( SC2C ** ( -TWO3 ) + EIM ) )

!                    VDEPJ( J,NCELL,VDSCOR ) = VGHAT2C
!    &                       / ( 1.0 - EXP(-VGHAT2C * ( BLKRAJ( J,NCELL ) + RD2C ) ) )

                     VDEPJ( J,C,R,VDSCOR ) = VGHAT2C
     &                       / ( 1.0 - EXP(-VGHAT2C * ( BLKRAJ( J,C,R ) + RD2C ) ) )

C now do 3rd moment for the deposition of mass

C  Aitken mode

                     SC3N = NU / DCHAT3N
                     EIM = STOKEN ** 2 / 400.0 * ESAT160
                     EIM = MIN( EIM, 1.0 )
                     RD3N = 1.0 / ( UTSCALE * ( SC3N ** ( -TWO3 ) + EIM ) )

!                    VDEPJ( J,NCELL,VDMATK ) = VGHAT3N
!    &                       / ( 1.0 - EXP(-VGHAT3N * ( BLKRAJ( J,NCELL ) + RD3N ) ) )

                     VDEPJ( J,C,R,VDMATK ) = VGHAT3N
     &                       / ( 1.0 - EXP(-VGHAT3N * ( BLKRAJ( J,C,R ) + RD3N ) ) )

C accumulation mode

                     SC3A = NU / DCHAT3A
                     EIM = STOKEA ** 2 / 400.0 * ESAC160
                     EIM = MIN( EIM, 1.0 )
                     RD3A = 1.0 / ( UTSCALE * ( SC3A ** ( -TWO3 ) + EIM ) )

!                    VDEPJ( J,NCELL,VDMACC ) = VGHAT3A
!    &                       / ( 1.0 - EXP(-VGHAT3A * ( BLKRAJ( J,NCELL ) + RD3A ) ) )

                     VDEPJ( J,C,R,VDMACC ) = VGHAT3A
     &                       / ( 1.0 - EXP(-VGHAT3A * ( BLKRAJ( J,C,R ) + RD3A ) ) )

C coarse mode

                     SC3C = NU / DCHAT3C
                     EIM = STOKEC ** 2 / 400.0 * ESCO160
                     EIM = MIN( EIM, 1.0 )
                     RD3C = 1.0 / ( UTSCALE * ( SC3C ** ( -TWO3 ) + EIM ) )

!                    VDEPJ( J,NCELL,VDMCOR ) = VGHAT3C
!    &                     / ( 1.0 - EXP(-VGHAT3C * ( BLKRAJ( J,NCELL ) + RD3C ) ) )

                     VDEPJ( J,C,R,VDMCOR ) = VGHAT3C
     &                     / ( 1.0 - EXP(-VGHAT3C * ( BLKRAJ( J,C,R ) + RD3C ) ) )

                  END IF ! RAJ > 0

               END DO ! n_lufrac

            END IF ! mosaic
!     END DO ! end loop on cells
         END DO ! end loop on C
      END DO ! end loop on R

      RETURN
      END SUBROUTINE GETDEP_V
