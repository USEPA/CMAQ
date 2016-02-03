      SUBROUTINE UPDATE_MET( JDATE, JTIME, TSTEP, DEPV, WVEL )
!******************************************************************************
! Developed jointly by the ARAP Group of Titan Corporation, Princeton, NJ and 
! Atmospheric and Environmental Research, Inc., San Ramon, CA 
! under EPRI-sponsorship.
!
! FUNCTION:  Update meteorological fields for JDATE and JTIME
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!    Updated April 2005 for CMAQ-APT-PM (PKK, AER)
!    Updated November 2005 for CMAQ v4.5 (PKK, AER)
!    Updated July 2006 to treat wet deposition by convective precipitation
!    correctly and to consider ratio of water in puff to total water in grid
!    column, PK, AER
!    Updated August 2011 for CMAQ 5.0 beta (PK, ENVIRON)
!    Additional updates March 2012 for CMAQ 5.0 final (PK, ENVIRON) 
!    Updated Feb 2015 to allow for missing QI in 3D met fields (PK, ENVIRON)
!******************************************************************************
 
! --- MODULES AND INCLUDES

! November 2005, PK, AER: Updated for dynamic vertical layers in CMAQ 4.5
      USE GRID_CONF             ! horizontal and vertical domain specifications

      USE COMMON_PUF
      USE MULTCOMP_INC
      USE COMMON_MET
      USE HOST_INC
      USE CGRID_SPCS, only: n_gc_depv, n_ae_depv, n_nr_depv, n_tr_depv,
     &                gc_spc, ae_spc, nr_spc, tr_spc,
     &                gc_depv_map, ae_depv_map, nr_depv_map, tr_depv_map,
     &                gc_depv, ae_depv, nr_depv, tr_depv

      IMPLICIT NONE

! --- ARGUMENTS
 
      INTEGER         JDATE     !  current model date, coded YYYYDDD
      INTEGER         JTIME     !  current model time, coded HHMMSS
      INTEGER         TSTEP     !  model time step,  format HHMMSS

      REAL, INTENT(IN) :: DEPV( :,:,: )  ! host model 2D dry deposition velocities
      REAL, INTENT(IN) :: WVEL( :,:,: )  ! host model derived vertical velocities

! --- PARAMETERS
      REAL, PARAMETER :: STDATMPA = 101325.  ! Standard atmospheric pressure (Pa)
      REAL, PARAMETER :: PA2ATM = 1.0 / STDATMPA   ! conv. factor from Pascals to atmospheres
      REAL, PARAMETER :: MINMOLI = 1.0E-04    ! min absolute inverse
                                              ! Monin-Obukhov Length (1/m)
      REAL, PARAMETER :: RADYNI_MIN = 1.0E-30 ! min inverse aerodynamic
                                              ! resistance (m/s)

! --- LOCALS

      INTEGER         MDATE     !  date at mid-time step, coded YYYYDDD
      INTEGER         MTIME     !  time at mid-time step, coded HHMMSS
      INTEGER         MSTEP     !  time step, format seconds

      REAL, SAVE ::   METSTEP   ! timestep on the met file (hr)

      CHARACTER( 16 ) :: VARNM

      INTEGER ROW                   ! Row index
      INTEGER COL                   ! Column index
      INTEGER LAY                   ! Layer index
      INTEGER JJ, IJ, JK, KK, IJK   ! Indices for SCICHEM 1-D arrays
      INTEGER ICLD

      INTEGER       I     ! loop counter

      INTEGER :: IOS

!.......   3-D variables from host model
      REAL, DIMENSION(:,:,:),ALLOCATABLE :: UWIND ! wind u-component (m/s)
      REAL, DIMENSION(:,:,:),ALLOCATABLE :: VWIND ! wind v-component (m/s)
      REAL, DIMENSION(:,:,:),ALLOCATABLE :: WWIND ! wind w-component (m/s)
      REAL, DIMENSION(:,:,:),ALLOCATABLE :: TA    ! temperature (K)
      REAL, DIMENSION(:,:,:),ALLOCATABLE :: QV    ! specific humidity
                                                  ! (kg water/kg air)
      REAL, DIMENSION(:,:,:),ALLOCATABLE :: PRES  ! pressure (Pa)
      REAL, DIMENSION(:,:,:),ALLOCATABLE :: DENS  ! air density (kg/m3)

      REAL, ALLOCATABLE, SAVE :: JACF( :,:,: ) ! full-layer Jacobian
      REAL, ALLOCATABLE, SAVE :: MSFX2( :,: )  ! map scale factor ** 2
      REAL, ALLOCATABLE :: DBUFF ( :,:,: )     ! input for W_VEL

!.......   Cloud variables
      REAL, DIMENSION(:,:,:),ALLOCATABLE :: QC    ! cloud water content
                                                  ! (kg water/kg air)
      REAL, DIMENSION(:,:,:),ALLOCATABLE :: QR    ! rain water content
                                                  ! (kg water/kg air)
      REAL, DIMENSION(:,:,:),ALLOCATABLE :: QG    ! graupel content
                                                  ! (kg water/kg air)
      REAL, DIMENSION(:,:,:),ALLOCATABLE :: QI    ! ice content
                                                  ! (kg water/kg air)
      REAL, DIMENSION(:,:,:),ALLOCATABLE :: QS    ! snow content
                                                  ! (kg water/kg air)

      REAL :: LWC                       ! Cloud liquid water content (g/m3)
      REAL :: TWC                       ! Cloud total water content (g/m3)
      REAL :: PWC                       ! Precipitation water content (g/m3)

      REAL :: GRDVOL                    ! Volume of grid cell (m3)
      REAL :: XMAP, YMAP                ! Map factors

!.......   2-D variables from host model
      REAL, DIMENSION(:,:),ALLOCATABLE :: PBL     ! boundary layer height (m)
      REAL, DIMENSION(:,:),ALLOCATABLE :: USTAR   ! hor. friction vel. (m/s)
      REAL, DIMENSION(:,:),ALLOCATABLE :: MOLI    ! inv. Monin-Obuknov length
                                                  ! (1/m)
      REAL, DIMENSION(:,:),ALLOCATABLE :: WSTAR   ! convective velocity scale
                                                  ! (m/s)
      REAL, DIMENSION(:,:),ALLOCATABLE :: RA      ! aerodynamic resistance
                                                  ! (s/m)
      REAL, DIMENSION(:,:),ALLOCATABLE :: ZZERO   ! roughness lengths (m)

      REAL, DIMENSION(:,:),ALLOCATABLE :: CLDT2D  ! cloud top in meters
      REAL, DIMENSION(:,:),ALLOCATABLE :: CLDB2D  ! cloud bottom in meters
      REAL, DIMENSION(:,:),ALLOCATABLE :: CFRAC2D ! fractional cloud coverage
      REAL, DIMENSION(:,:),ALLOCATABLE :: WBAR2D  ! avg. liquid water content
                                                  ! of clouds in g/m**3
      REAL, DIMENSION(:,:),ALLOCATABLE :: RN2D    ! non-convective precip (cm)
      REAL, DIMENSION(:,:),ALLOCATABLE :: RC2D    ! convective precip (cm)

! Deposition velocity variables
! Mapping of host model deposited species to SCICHEM species
      integer, save :: i_depv_list( MAX_MC )

! Deposition velocity names
! Note: name is currently only for debugging. The mapping is done by
!       i_depv_list
      CHARACTER( 16 ), DIMENSION( MAX_MC ), SAVE :: VDNAME1
      CHARACTER( 16 ) :: SPNAME

      CHARACTER( 16 ) :: BLNK = ' '

      INTEGER       ISP, VAR, SPC             ! species loop counters

      LOGICAL IsMCParticle

      CHARACTER( 16 ) :: PNAME = 'UPDATE_MET'
      CHARACTER( 120 ) :: XMSG = ' '

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      LOGICAL, SAVE :: QG_AVAIL = .TRUE.   ! flag for QG available on file
      LOGICAL, SAVE :: QI_AVAIL = .TRUE.   ! flag for QI available on file
      LOGICAL, SAVE :: QS_AVAIL = .TRUE.   ! flag for QS available on file

      IF ( FIRSTIME ) THEN

         FIRSTIME = .FALSE.

! --- Set up mapping between SCICHEM species and CMAQ deposited species.

! Initializations
         i_depv_list = 0
         VDNAME1 = BLNK

! --- Reactive gases
         ISP = 0
         DO VAR = 1, N_GC_DEPV
            ISP = ISP + 1
            SPNAME = GC_SPC( GC_DEPV_MAP( VAR ) )
            DO SPC = 1, nspecies

! --- Skip if this is particle species
               IF ( IsMCParticle( SPC ) ) CYCLE
               IF ( TRIM(species(SPC)%name) == SPNAME ) THEN
                  i_depv_list( SPC ) = ISP
                  VDNAME1( SPC ) = GC_DEPV( VAR )
                  EXIT
               END IF
            END DO
         END DO

! --- Particles
         DO VAR = 1, N_AE_DEPV
            ISP = ISP + 1
            SPNAME = AE_SPC( AE_DEPV_MAP( VAR ) )
            DO SPC = 1, nspecies

! --- Skip if this is not a particle species
               IF ( .NOT. IsMCParticle( SPC ) ) CYCLE
               IF ( TRIM(species(SPC)%name) == SPNAME ) THEN
                  i_depv_list( SPC ) = ISP
                  VDNAME1( SPC ) = AE_DEPV( VAR )
                  EXIT
               END IF
            END DO
         END DO

! --- Non-reactive gases
         DO VAR = 1, N_NR_DEPV
            ISP = ISP + 1
            SPNAME = NR_SPC( NR_DEPV_MAP( VAR ) )
            DO SPC = 1, nspecies

! --- Skip if this is particle species
               IF ( IsMCParticle( SPC ) ) CYCLE
               IF ( TRIM(species(SPC)%name) == SPNAME ) THEN
                  i_depv_list( SPC ) = ISP
                  VDNAME1( SPC ) = NR_DEPV( VAR )
                  EXIT
               END IF
            END DO
         END DO

! --- Tracer gases (should not really be deposited, but kept for
! --- compatibility with host model)
         DO VAR = 1, N_TR_DEPV
            ISP = ISP + 1
            SPNAME = TR_SPC( TR_DEPV_MAP( VAR ) )
            DO SPC = 1, nspecies

! --- Skip if this is particle species
               IF ( IsMCParticle( SPC ) ) CYCLE
               IF ( TRIM(species(SPC)%name) == SPNAME ) THEN
                  i_depv_list( SPC ) = ISP
                  VDNAME1( SPC ) = TR_DEPV( VAR )
                  EXIT
               END IF
            END DO
         END DO
!debug
         write(*,*)'Dry dep velocity mapping to SCICHEM species'
         do spc = 1, nspecies
            if (i_depv_list(spc) == 0) then
               write(*,*)'Skipping ',species(spc)%name
               cycle
            end if
            write(*,*)'spc,name,index,vdname: ',
     &                 spc,species(spc)%name,i_depv_list(spc),vdname1(spc)
         end do
!debug

!..... Initialize dry deposition velocities
         VDEP2D = 0.

C...open MET_CRO_2D

         IF ( .NOT. OPEN3( MET_CRO_2D, FSREAD3, PNAME ) ) THEN
            XMSG = 'Could not open '// MET_CRO_2D // ' file'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C...get description from the met file

         IF ( .NOT. DESC3( MET_CRO_2D ) ) THEN
            XMSG = 'Could not get ' // MET_CRO_2D //' file description'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C...open MET_CRO_3D

         IF ( .NOT. OPEN3( MET_CRO_3D, FSREAD3, PNAME ) ) THEN
            XMSG = 'Could not open '// MET_CRO_3D // ' file'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C...get description from the met file

         IF ( .NOT. DESC3( MET_CRO_3D ) ) THEN
            XMSG = 'Could not get ' // MET_CRO_3D //' file description'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C...store met file timestep in hours

         METSTEP = FLOAT( TIME2SEC( TSTEP3D ) ) / 3600.0

! ... for vertical velocity calculations
         ALLOCATE( JACF( GL_NCOLS, GL_NROWS, NLAYS ), STAT = IOS )
         IF ( IOS /= 0 ) THEN
            XMSG = 'Failure allocating JACF'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( MSFX2( GL_NCOLS, GL_NROWS ), STAT = IOS )
         IF ( IOS /= 0 ) THEN
            XMSG = 'Failure allocating MSFX2'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         IF ( .NOT. INTERPX( GRID_CRO_2D, 'MSFX2', PNAME,
     &                       1, GL_NCOLS, 1, GL_NROWS, 1, 1,
     &                       JDATE, JTIME, MSFX2 ) ) THEN
            XMSG = 'Could not interpolate MSFX2 from ' // GRID_CRO_2D
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VAR = INDEX1( 'QI', NVARS3D, VNAME3D )
         IF ( VAR == 0 ) THEN
            QI_AVAIL = .FALSE.
            XMSG = 'Parameter QI (cloud ice) was not found on file '
     &             // MET_CRO_3D
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
         END IF

         VAR = INDEX1( 'QS', NVARS3D, VNAME3D )
         IF ( VAR == 0 ) THEN
            QS_AVAIL = .FALSE.
            XMSG = 'Parameter QS (snow) was not found on file '
     &             // MET_CRO_3D
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
         END IF

         VAR = INDEX1( 'QG', NVARS3D, VNAME3D )
         IF ( VAR == 0 ) THEN
            QG_AVAIL = .FALSE.
            XMSG = 'Parameter QG (graupel) was not found on file '
     &             // MET_CRO_3D
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
         END IF

      END IF   ! if firsttime
!debug
!       write(*,*)'depv shape: ',shape(depv) 
!       write(*,*)'wvel shape: ',shape(wvel) 
!debug

!...... Read host model wind fields
      IF ( .NOT. ALLOCATED( UWIND ) ) THEN
         ALLOCATE( UWIND( GL_NCOLS+1, GL_NROWS+1, NLAYS ), STAT = IOS )
         IF ( IOS /= 0 ) THEN

            XMSG = 'Could not allocate UWIND array '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

         END IF
      END IF

      VARNM = 'UWIND'
      IF ( .NOT. INTERPX( MET_DOT_3D, VARNM, PNAME,
     &                    1, GL_NCOLS + 1, 1, GL_NROWS + 1, 1, NLAYS,
     &                    JDATE, JTIME, UWIND ) ) THEN

         XMSG = 'Could not read '// VARNM // ' from ' // MET_DOT_3D
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF                    !  if not interpx for uwind
C
      IF ( .NOT. ALLOCATED( VWIND ) ) THEN
         ALLOCATE( VWIND( GL_NCOLS+1, GL_NROWS+1, NLAYS ), STAT = IOS )
         IF ( IOS /= 0 ) THEN

            XMSG = 'Could not allocate VWIND array '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

         END IF
      END IF

      VARNM = 'VWIND'
      IF ( .NOT. INTERPX( MET_DOT_3D, VARNM, PNAME,
     &                    1, GL_NCOLS + 1, 1, GL_NROWS + 1, 1, NLAYS,
     &                    JDATE, JTIME, VWIND ) ) THEN

         XMSG = 'Could not read '// VARNM // ' from ' // MET_DOT_3D
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF                    !  if not interpx for vwind

C...... Vertical winds
      IF ( .NOT. INTERPX( MET_CRO_3D, 'JACOBF', PNAME,
     &                    1, GL_NCOLS, 1, GL_NROWS, 1, NLAYS,
     &                    JDATE, JTIME, JACF ) ) THEN
         XMSG = 'Could not interpolate JACOBF from MET_CRO_3D - '
     &        // 'Using JACOBM <- KLUDGE!'
         CALL M3WARN( PNAME, JDATE, JTIME, XMSG )

         IF ( .NOT. ALLOCATED( DBUFF ) ) THEN
            ALLOCATE ( DBUFF( GL_NCOLS, GL_NROWS, NLAYS ), STAT = IOS )
            IF ( IOS /= 0 ) THEN
               XMSG = 'Failure allocating DBUFF'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
         END IF

         IF ( .NOT. INTERPX( MET_CRO_3D, 'JACOBM', PNAME,
     &                       1, GL_NCOLS, 1, GL_NROWS, 1, NLAYS,
     &                       JDATE, JTIME, DBUFF ) ) THEN
            XMSG = 'Could not interpolate JACOBM from MET_CRO_3D'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         DO LAY = 1, NLAYS - 1
            DO ROW = 1, GL_NROWS
               DO COL = 1, GL_NCOLS
                  JACF( COL,ROW,LAY ) = 0.5 * ( DBUFF( COL,ROW,LAY ) + 
     &                                          DBUFF( COL,ROW,LAY+1 ) )
               END DO
            END DO
         END DO
         DO ROW = 1, GL_NROWS
            DO COL = 1, GL_NCOLS
               JACF( COL,ROW,NLAYS ) = 0.4 * JACF( COL,ROW,NLAYS-1 )
     &                               +       DBUFF( COL,ROW,NLAYS )
            END DO
         END DO

         DEALLOCATE( DBUFF )

      END IF   ! INTERPX for JACOBF

! convert from contravariant vertical velocity component to true wind
      IF ( .NOT. ALLOCATED( WWIND ) ) THEN
         ALLOCATE( WWIND( GL_NCOLS, GL_NROWS, NLAYS ), STAT = IOS )
         IF ( IOS /= 0 ) THEN

            XMSG = 'Could not allocate WWIND array '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

         END IF
      END IF

      DO LAY = 1, NLAYS
         DO ROW = 1, GL_NROWS
            DO COL = 1, GL_NCOLS
               WWIND( COL,ROW,LAY ) = JACF( COL,ROW,LAY ) * 
     &                       MSFX2( COL,ROW ) * WVEL( COL,ROW,LAY )
            END DO
         END DO
      END DO
!debug
!      write(*,*)'wvel(16,20,2),wwind(16,20,2): ',wvel(16,20,2),wwind(16,20,2)
!      write(*,*)'wvel(48,20,2),wwind(48,20,2): ',wvel(48,20,2),wwind(48,20,2)
!      write(*,*)'wvel(80,20,2),wwind(80,20,2): ',wvel(80,20,2),wwind(80,20,2)
!      write(*,*)'wvel(112,20,2),wwind(112,20,2): ',wvel(112,20,2),wwind(112,20,2)
!      write(*,*)'wvel(16,61,2),wwind(16,61,2): ',wvel(16,61,2),wwind(16,61,2)
!      write(*,*)'wvel(48,61,2),wwind(48,61,2): ',wvel(48,61,2),wwind(48,61,2)
!      write(*,*)'wvel(80,61,2),wwind(80,61,2): ',wvel(80,61,2),wwind(80,61,2)
!      write(*,*)'wvel(112,61,2),wwind(112,61,2): ',wvel(112,61,2),wwind(112,61,2)
!      write(*,*)'wvel(16,102,2),wwind(16,102,2): ',wvel(16,102,2),wwind(16,102,2)
!      write(*,*)'wvel(48,102,2),wwind(48,102,2): ',wvel(48,102,2),wwind(48,102,2)
!      write(*,*)'wvel(80,102,2),wwind(80,102,2): ',wvel(80,102,2),wwind(80,102,2)
!      write(*,*)'wvel(112,102,2),wwind(112,102,2): ',wvel(112,102,2),wwind(112,102,2)
!debug

      DO LAY = 1, NLAYS
!
!  Obtain cell face values of horizontal velocities.
!
         DO ROW = 1, GL_NROWS
            DO COL = 1, GL_NCOLS + 1

               UWIND( COL, ROW, LAY ) = 0.5*( UWIND( COL, ROW  , LAY ) +
     &                                    UWIND( COL, ROW + 1, LAY ) )

            END DO            !  end loop on cols
         END DO               !  end loop on rows

         DO ROW = 1, GL_NROWS + 1
            DO COL = 1, GL_NCOLS

               VWIND( COL, ROW, LAY ) = 0.5*( VWIND( COL  , ROW, LAY ) +
     &                                    VWIND( COL + 1, ROW, LAY ) )

            END DO            !  end loop on cols
         END DO               !  end loop on rows

      END DO                  !  end loop on levels

!.......   Set date and time for center of this time step, convert chemistry
!.......   time step (minutes), compute total elapsed time (hours)

      MDATE = JDATE
      MTIME = JTIME
      MSTEP = TIME2SEC( TSTEP )
      CALL NEXTIME ( MDATE, MTIME, SEC2TIME( MSTEP / 2 ) )

!........  Temperature
      IF ( .NOT. ALLOCATED( TA ) ) THEN
         ALLOCATE( TA( GL_NCOLS, GL_NROWS, NLAYS ), STAT = IOS )
         IF ( IOS /= 0 ) THEN

            XMSG = 'Could not allocate TA array '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

         END IF
      END IF

      VARNM = 'TA'
      IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                    1, GL_NCOLS, 1, GL_NROWS, 1, NLAYS,
     &                    MDATE, MTIME, TA ) ) THEN

         XMSG = 'Could not read '// VARNM // ' from ' // MET_CRO_3D
         CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )

      END IF      !  if INTERPX failed

C........  Water vapor mixing ratio
      IF ( .NOT. ALLOCATED( QV ) ) THEN
         ALLOCATE( QV( GL_NCOLS, GL_NROWS, NLAYS ), STAT = IOS )
         IF ( IOS /= 0 ) THEN

            XMSG = 'Could not allocate QV array '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

         END IF
      END IF

      VARNM = 'QV'
      IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                    1, GL_NCOLS, 1, GL_NROWS, 1, NLAYS,
     &                    MDATE, MTIME, QV ) ) THEN

         XMSG = 'Could not read '// VARNM // ' from ' // MET_CRO_3D
         CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )

      END IF      !  if INTERPX failed

C........  Cloud water mixing ratio
      IF ( .NOT. ALLOCATED( QC ) ) THEN
         ALLOCATE( QC( GL_NCOLS, GL_NROWS, NLAYS ), STAT = IOS )
         IF ( IOS /= 0 ) THEN

            XMSG = 'Could not allocate QC array '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

         END IF
      END IF

      VARNM = 'QC'
      IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                    1, GL_NCOLS, 1, GL_NROWS, 1, NLAYS,
     &                    MDATE, MTIME, QC ) ) THEN

         XMSG = 'Could not read '// VARNM // ' from ' // MET_CRO_3D
         CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )

      END IF      !  if INTERPX failed

C........  Rain water mixing ratio
      IF ( .NOT. ALLOCATED( QR ) ) THEN
         ALLOCATE( QR( GL_NCOLS, GL_NROWS, NLAYS ), STAT = IOS )
         IF ( IOS /= 0 ) THEN

            XMSG = 'Could not allocate QR array '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

         END IF
      END IF

      VARNM = 'QR'
      IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                    1, GL_NCOLS, 1, GL_NROWS, 1, NLAYS,
     &                    MDATE, MTIME, QR ) ) THEN

         XMSG = 'Could not read '// VARNM // ' from ' // MET_CRO_3D
         CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )

      END IF      !  if INTERPX failed

C........  Graupel mixing ratio
!          (may not always be available so just give warning message)
      IF ( .NOT. ALLOCATED( QG ) ) THEN
         ALLOCATE( QG( GL_NCOLS, GL_NROWS, NLAYS ), STAT = IOS )
         IF ( IOS /= 0 ) THEN

            XMSG = 'Could not allocate QG array '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

         END IF
      END IF

      VARNM = 'QG'
      IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                    1, GL_NCOLS, 1, GL_NROWS, 1, NLAYS,
     &                    MDATE, MTIME, QG ) ) THEN

         XMSG = 'Could not read '// VARNM // ' from ' // MET_CRO_3D
         CALL M3WARN ( PNAME, MDATE, MTIME, XMSG )
         QG = 0.

      END IF      !  if INTERPX failed

!........  Ice mixing ratio
      IF ( .NOT. ALLOCATED( QI ) ) THEN
         ALLOCATE( QI( GL_NCOLS, GL_NROWS, NLAYS ), STAT = IOS )
         IF ( IOS /= 0 ) THEN

            XMSG = 'Could not allocate QI array '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

         END IF
      END IF

!...read resolved ice mixing ratio (kg H2O / kg air) from the met
!...  file if it is available

      IF ( QI_AVAIL ) THEN

         VARNM = 'QI'
         IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                       1, GL_NCOLS, 1, GL_NROWS, 1, NLAYS,
     &                       MDATE, MTIME, QI ) ) THEN

            XMSG = 'Could not read '// VARNM // ' from ' // MET_CRO_3D
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )

         END IF      !  if INTERPX failed

      ELSE

        QI = 0.0    ! otherwise fill the array with zeros

      END IF

!........  Snow mixing ratio
      IF ( .NOT. ALLOCATED( QS ) ) THEN
         ALLOCATE( QS( GL_NCOLS, GL_NROWS, NLAYS ), STAT = IOS )
         IF ( IOS /= 0 ) THEN

            XMSG = 'Could not allocate QI array '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

         END IF
      END IF

!...read resolved snow mixing ratio (kg H2O / kg air) from the met
!...  file if it is available

      IF ( QS_AVAIL ) THEN

         VARNM = 'QS'
         IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                       1, GL_NCOLS, 1, GL_NROWS, 1, NLAYS,
     &                       MDATE, MTIME, QS ) ) THEN

            XMSG = 'Could not read '// VARNM // ' from ' // MET_CRO_3D
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )

         END IF      !  if INTERPX failed

      ELSE

        QS = 0.0    ! otherwise fill the array with zeros

      END IF

C........  Pressure
      IF ( .NOT. ALLOCATED( PRES ) ) THEN
         ALLOCATE( PRES( GL_NCOLS, GL_NROWS, NLAYS ), STAT = IOS )
         IF ( IOS /= 0 ) THEN

            XMSG = 'Could not allocate PRES array '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

         END IF
      END IF

      VARNM = 'PRES'
      IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                    1, GL_NCOLS, 1, GL_NROWS, 1, NLAYS,
     &                    MDATE, MTIME, PRES ) ) THEN

         XMSG = 'Could not read '// VARNM // ' from ' // MET_CRO_3D
         CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )

      END IF  !  if INTERPX failed

C........  Density
      IF ( .NOT. ALLOCATED( DENS ) ) THEN
         ALLOCATE( DENS( GL_NCOLS, GL_NROWS, NLAYS ), STAT = IOS )
         IF ( IOS /= 0 ) THEN

            XMSG = 'Could not allocate DENS array '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

         END IF
      END IF

      VARNM = 'DENS'
      IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                    1, GL_NCOLS, 1, GL_NROWS, 1, NLAYS,
     &                    MDATE, MTIME, DENS ) ) THEN

         XMSG = 'Could not read '// VARNM // ' from ' // MET_CRO_3D
         CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )

      END IF  !  if INTERPX failed

!------ SCICHEM 3D fields

! ... Initialize mass of total water and  precip. water in clouds
      CLDMASST = 0.
      CLDMASSP = 0.

      DO LAY = 1, NZB
!pk start at layer 2 and go to layer nzb + 1
!pk         KK = (LAY - 1)*NXYB
         KK = LAY * NXYB
         DO ROW = 1, NYB
            JJ = ( ROW - 1 ) * NXB
            JK = JJ + KK
            DO COL = 1, NXB
               IJ = JJ + COL
               IJK = JK + COL

               XMAP = XMAP2D( IJ )
               YMAP = YMAP2D( IJ )
               GRDVOL = DXB * DYB * ( ZW3D( COL, ROW, LAY + 1 ) - 
     &                                ZW3D( COL, ROW, LAY ) ) / ( XMAP * YMAP )

               U_UA( IJK )  = UWIND( COL + 1, ROW  , LAY )
               V_UA( IJK )  = VWIND( COL  , ROW + 1, LAY )
               W_UA( IJK )  = WWIND( COL  , ROW  ,   LAY )

!..... Convert specific humidity to g water/g dry air
               H_UA( IJK )  = QV( COL, ROW, LAY ) /
     &                        ( 1. - QV( COL, ROW, LAY ) )
               P_UA( IJK )  = PRES ( COL, ROW, LAY ) * PA2ATM
!..... Potential Temperature (K)
               T_UA( IJK )  = TA ( COL, ROW, LAY ) /
     &                        P_UA( IJK ) ** 0.28571

!..... Liquid water content (kg/m3)
               LWC = ( QC( COL, ROW, LAY ) + QR( COL, ROW, LAY )
     &             +   QG( COL, ROW, LAY ) ) * DENS( COL, ROW, LAY )
!..... Total water content (kg/m3)
               TWC = LWC + ( QI( COL, ROW, LAY ) + QS( COL, ROW, LAY ) )
     &             * DENS( COL, ROW, LAY )
!..... Precipitation water content (kg/m3)
               PWC = ( QR( COL, ROW, LAY ) + QS( COL, ROW, LAY )
     &             +   QG( COL, ROW, LAY ) ) * DENS( COL, ROW, LAY )

!..... Convert to g/m3
               LWC = LWC * 1.E3
               TWC = TWC * 1.E3
               PWC = PWC * 1.E3

               CLDMASST( IJ ) = CLDMASST( IJ ) + TWC * GRDVOL
               CLDMASSP( IJ ) = CLDMASSP( IJ ) + PWC * GRDVOL

               DO ICLD = 1, MAX_CLD
                  CLD_UA( IJK, ICLD ) = LWC
                  CLDT_UA( IJK, ICLD ) = TWC
                  CLDP_UA( IJK, ICLD ) = PWC
               END DO
            END DO
         END DO
      END DO

      DO IJ = 1, NXYB               !FIRST LAYER IN THE GROUND
         U_UA( IJ ) = -U_UA( IJ + NXYB )
         V_UA( IJ ) = -V_UA( IJ + NXYB )
         W_UA( IJ ) = 0.
         H_UA( IJ ) = H_UA( IJ + NXYB )
         T_UA( IJ ) = T_UA( IJ + NXYB )
         P_UA( IJ ) = P_UA( IJ + NXYB )
         DO ICLD = 1, MAX_CLD
            CLD_UA( IJ, ICLD ) = CLD_UA( IJ + NXYB, ICLD )
            CLDT_UA( IJ, ICLD ) = CLDT_UA( IJ + NXYB, ICLD )
            CLDP_UA( IJ, ICLD ) = CLDP_UA( IJ + NXYB, ICLD )
         END DO
      END DO

! --- Release memory (3D arrays)
      DEALLOCATE( QV )
      DEALLOCATE( QC )
      DEALLOCATE( QR )
      DEALLOCATE( QG )
      DEALLOCATE( QI )
      DEALLOCATE( QS )
      DEALLOCATE( WWIND )
      DEALLOCATE( VWIND )
      DEALLOCATE( UWIND )
      DEALLOCATE( PRES )
      DEALLOCATE( DENS )
      DEALLOCATE( TA )
C
C ..... Read 2-D fields
      IF ( .NOT. ALLOCATED( PBL ) ) THEN
         ALLOCATE( PBL( GL_NCOLS, GL_NROWS ), STAT = IOS )
         IF ( IOS /= 0 ) THEN

            XMSG = 'Could not allocate PBL array '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

         END IF
      END IF

      VARNM = 'PBL'
      IF ( .NOT. INTERPX( MET_CRO_2D, VARNM, PNAME,
     &                    1, GL_NCOLS, 1, GL_NROWS, 1, 1,
     &                    MDATE, MTIME, PBL ) ) THEN

         XMSG = 'Could not read '// VARNM // ' from ' // MET_CRO_2D
         CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )

      END IF  !  if INTERPX failed
C
      IF ( .NOT. ALLOCATED( USTAR ) ) THEN
         ALLOCATE( USTAR( GL_NCOLS, GL_NROWS ), STAT = IOS)
         IF ( IOS /= 0 ) THEN

            XMSG = 'Could not allocate USTAR array '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

         END IF
      END IF

      VARNM = 'USTAR'
      IF ( .NOT. INTERPX( MET_CRO_2D, VARNM, PNAME,
     &                    1, GL_NCOLS, 1, GL_NROWS, 1, 1,
     &                    MDATE, MTIME, USTAR ) ) THEN

         XMSG = 'Could not read '// VARNM // ' from ' // MET_CRO_2D
         CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )

      END IF  !  if INTERPX failed
C
      IF ( .NOT. ALLOCATED( MOLI ) ) THEN
         ALLOCATE( MOLI( GL_NCOLS, GL_NROWS ), STAT = IOS )
         IF ( IOS /= 0 ) THEN

            XMSG = 'Could not allocate MOLI array '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

         END IF
      END IF

      VARNM = 'MOLI'
      IF ( .NOT. INTERPX( MET_CRO_2D, VARNM, PNAME,
     &                    1, GL_NCOLS, 1, GL_NROWS, 1, 1,
     &                    MDATE, MTIME, MOLI ) ) THEN

         XMSG = 'Could not read '// VARNM // ' from ' // MET_CRO_2D
         CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )

      END IF  !  if INTERPX failed
C
      IF ( .NOT. ALLOCATED( WSTAR ) ) THEN
         ALLOCATE( WSTAR( GL_NCOLS, GL_NROWS ), STAT = IOS )
         IF ( IOS /= 0 ) THEN

            XMSG = 'Could not allocate WSTAR array '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

         END IF
      END IF

      VARNM = 'WSTAR'
      IF ( .NOT. INTERPX( MET_CRO_2D, VARNM, PNAME,
     &                    1, GL_NCOLS, 1, GL_NROWS, 1, 1,
     &                    MDATE, MTIME, WSTAR ) ) THEN

         XMSG = 'Could not read '// VARNM // ' from ' // MET_CRO_2D
         CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )

      END IF  !  if INTERPX failed

      IF ( .NOT. ALLOCATED( ZZERO ) ) THEN
         ALLOCATE( ZZERO( GL_NCOLS, GL_NROWS ), STAT = IOS )
         IF (IOS /= 0) THEN

            XMSG = 'Could not allocate ZZERO array '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

         END IF
      END IF

      VARNM = 'ZRUF'
      IF ( .NOT. INTERPX( MET_CRO_2D, VARNM, PNAME,
     &                    1, GL_NCOLS, 1, GL_NROWS, 1, 1,
     &                    MDATE, MTIME, ZZERO ) ) THEN
 
         XMSG = 'Could not read '// VARNM // ' from ' // MET_CRO_2D
         CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )

      END IF  !  if INTERPX failed

C........  Cloud top
      IF ( .NOT. ALLOCATED( CLDT2D ) ) THEN
         ALLOCATE( CLDT2D( GL_NCOLS, GL_NROWS ), STAT = IOS )
         IF ( IOS /= 0 ) THEN

            XMSG = 'Could not allocate CLDT2D array '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

         END IF
      END IF

      VARNM = 'CLDT'
      IF ( .NOT. INTERPX( MET_CRO_2D, VARNM, PNAME,
     &                    1, GL_NCOLS, 1, GL_NROWS, 1, 1,
     &                    MDATE, MTIME, CLDT2D ) ) THEN
 
         XMSG = 'Could not read '// VARNM // ' from ' // MET_CRO_2D
         CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )

      END IF  !  if INTERPX failed

C........  Cloud base
      IF ( .NOT. ALLOCATED( CLDB2D ) ) THEN
         ALLOCATE( CLDB2D( GL_NCOLS, GL_NROWS ), STAT = IOS )
         IF ( IOS /= 0 ) THEN

            XMSG = 'Could not allocate CLDB2D array '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

         END IF
      END IF

      VARNM = 'CLDB'
      IF ( .NOT. INTERPX( MET_CRO_2D, VARNM, PNAME,
     &                    1, GL_NCOLS, 1, GL_NROWS, 1, 1,
     &                    MDATE, MTIME, CLDB2D ) ) THEN

         XMSG = 'Could not read '// VARNM // ' from ' // MET_CRO_2D
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF  !  if INTERPX failed

C........  Cloud fraction
      IF ( .NOT. ALLOCATED( CFRAC2D ) ) THEN
         ALLOCATE( CFRAC2D( GL_NCOLS, GL_NROWS ), STAT = IOS )
         IF ( IOS /= 0 ) THEN

            XMSG = 'Could not allocate CFRAC2D array '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

         END IF
      END IF

      VARNM = 'CFRAC'
      IF ( .NOT. INTERPX( MET_CRO_2D, VARNM, PNAME,
     &                    1, GL_NCOLS, 1, GL_NROWS, 1, 1,
     &                    MDATE, MTIME, CFRAC2D ) ) THEN

         XMSG = 'Could not read '// VARNM // ' from ' // MET_CRO_2D
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF  !  if INTERPX failed

C........  Average cloud lwc (g/m3)
      IF ( .NOT. ALLOCATED( WBAR2D ) ) THEN
         ALLOCATE( WBAR2D( GL_NCOLS, GL_NROWS ), STAT = IOS )
         IF ( IOS /= 0 ) THEN

            XMSG = 'Could not allocate WBAR2D array '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

         END IF
      END IF

      VARNM = 'WBAR'
      IF ( .NOT. INTERPX( MET_CRO_2D, VARNM, PNAME,
     &                    1, GL_NCOLS, 1, GL_NROWS, 1, 1,
     &                    MDATE, MTIME, WBAR2D ) ) THEN

         XMSG = 'Could not read '// VARNM // ' from ' // MET_CRO_2D
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF  !  if INTERPX failed

C...Get nonconvective precipitation amount (cm)
      IF ( .NOT. ALLOCATED( RN2D ) ) THEN
         ALLOCATE( RN2D( GL_NCOLS, GL_NROWS ), STAT = IOS )
         IF ( IOS /= 0 ) THEN

            XMSG = 'Could not allocate RN2D array '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

         END IF
      END IF

      VARNM = 'RN'
      IF ( .NOT. INTERPX( MET_CRO_2D, VARNM, PNAME,
     &                    1, GL_NCOLS, 1, GL_NROWS, 1, 1,
     &                    MDATE, MTIME, RN2D ) ) THEN

         XMSG = 'Could not read '// VARNM // ' from ' // MET_CRO_2D
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF  !  if INTERPX failed

C...Get convective precipitation amount (cm)
      IF ( .NOT. ALLOCATED( RC2D ) ) THEN
         ALLOCATE( RC2D( GL_NCOLS, GL_NROWS ), STAT = IOS )
         IF ( IOS /= 0 ) THEN

            XMSG = 'Could not allocate RC2D array '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

         END IF
      END IF

      VARNM = 'RC'
      IF ( .NOT. INTERPX( MET_CRO_2D, VARNM, PNAME,
     &                    1, GL_NCOLS, 1, GL_NROWS, 1, 1,
     &                    MDATE, MTIME, RC2D ) ) THEN

         XMSG = 'Could not read '// VARNM // ' from ' // MET_CRO_2D
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF  !  if INTERPX failed

C........  Aerodynamic resistances
C          For backward compatibility allow reading of either inverse
C          resistance or resistance
      IF ( .NOT. ALLOCATED( RA ) ) THEN
         ALLOCATE( RA( GL_NCOLS, GL_NROWS ), STAT = IOS )
         IF ( IOS /= 0 ) THEN

            XMSG = 'Could not allocate RA array '
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

         END IF
      END IF

      VARNM = 'RADYNI'
      IF ( INTERPX( MET_CRO_2D, VARNM, PNAME,
     &              1, GL_NCOLS, 1, GL_NROWS, 1, 1,
     &              MDATE, MTIME, RA ) ) THEN

         DO ROW = 1, GL_NROWS
            DO COL = 1, GL_NCOLS
               RA( COL, ROW ) = 1.0 / MAX( RA( COL, ROW ), RADYNI_MIN )
            END DO
         END DO

      ELSE
         VARNM = 'RA'
         IF ( .NOT. INTERPX( MET_CRO_2D, VARNM, PNAME,
     &                       1, GL_NCOLS, 1, GL_NROWS, 1, 1,
     &                       MDATE, MTIME, RA ) ) THEN
            XMSG = 'Could not read '// VARNM // ' from ' // MET_CRO_2D
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )

         END IF  !  if INTERPX failed

      END IF

      DO SPC = 1, nspecies

! --- Skip if species is not dry deposited
         IF ( i_depv_list ( SPC ) == 0 ) CYCLE

!..... Set 2D deposition velocities
         DO ROW = 1, NYB
            JJ = ( ROW - 1 ) * NXB
            DO COL = 1, NXB
               IJ = JJ + COL
               VDEP2D( IJ, SPC ) = DEPV( i_depv_list( SPC ), COL, ROW  )
            END DO
         END DO

      END DO

!debug
!      DO SPC = 1, nspecies
!         write(*,*)'spc,name,vd: ',spc,species(spc)%name,vdep2d(7683,spc)
!      end do
!debug
!------ Remaining SCICHEM 2D fields
      DO ROW = 1, NYB
         JJ = ( ROW - 1 ) * NXB
         DO COL = 1, NXB
            IJ = JJ + COL
            ZI_BL( IJ ) =  PBL( COL, ROW )        ! Mixing height (m)

c bound MOLI away from (-e-04, e-04)
            MOLI( COL, ROW ) = SIGN ( 
     &                         MAX (
     &                             ABS ( MOLI( COL, ROW ) ),
     &                             MINMOLI
     &                             ),
     &                         MOLI( COL, ROW )
     &                         )
            XMOL2( IJ ) =  1. / MOLI( COL, ROW )    ! Monin-Obhukov length (m)
            WSTR2( IJ ) =  WSTAR( COL, ROW ) ** 2   ! Convective velocity scale
                                                    ! squared (m2/s2)
            USTR2( IJ ) =  USTAR( COL, ROW ) ** 2   ! Friction velocity
                                                    ! squared (m2/s2)
            RADYN2( IJ ) =  RA( COL, ROW )          ! Aerodynamic resistance
                                                    ! (s/m)
            ZRUF2 ( IJ ) = ZZERO( COL, ROW )        ! Roughness length (m)

!------ Set cloud variables that affect photolysis rates
            CLDTOP2( IJ )  = CLDT2D ( COL, ROW )
            CLDBOT2( IJ )  = CLDB2D ( COL, ROW )
            FCC_BL ( IJ )  = CFRAC2D( COL, ROW )
            LWC2   ( IJ )  = WBAR2D ( COL, ROW )

!------ Set precipitation rates (mm/hr)
            IF ( FCC_BL(IJ) > 0. ) THEN
               PRATE_BL( IJ )  = 10.0 * ( RN2D ( COL, ROW ) +
     &                           RC2D ( COL, ROW ) / FCC_BL( IJ ) ) / METSTEP
               IF ( PRATE_BL(IJ) > 0. ) THEN
                  FPRCPC_BL( IJ ) = RC2D ( COL, ROW ) / ( RC2D ( COL, ROW ) +
     &                              FCC_BL( IJ ) * RN2D( COL, ROW ) )
               ELSE
                  FPRCPC_BL( IJ ) = 0.
               END IF
            ELSE
               PRATE_BL( IJ ) = 0.
               FPRCPC_BL( IJ ) = 0.
            END IF

         END DO
      END DO

! --- Release memory (2D arrays)
      DEALLOCATE( CFRAC2D )
      DEALLOCATE( CLDB2D  )
      DEALLOCATE( CLDT2D  )
      DEALLOCATE( WBAR2D  )
      DEALLOCATE( RN2D    )
      DEALLOCATE( RC2D    )
      DEALLOCATE( WSTAR   )
      DEALLOCATE( ZZERO   )
      DEALLOCATE( MOLI    )
      DEALLOCATE( USTAR   )
      DEALLOCATE( PBL     )
      DEALLOCATE( RA      )

      RETURN
      END
