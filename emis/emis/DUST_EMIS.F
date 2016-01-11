
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
C $Header: /project/work/rep/arc/CCTM/src/emis/emis/DUST_EMIS.F,v 1.6 2011/10/21 16:10:45 yoj Exp $

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      module dust_emis
     
C-----------------------------------------------------------------------
C Description:
C * Extracts selected landuse categories from BELD01 and BELD03 and merges
C * the selections into a dust-related landuse array (ULAND).

C   Optionally, reads 3 gridded crop calendar file and calculates an
C   erodible agriculture land fraction. (cropcal)

C * Applies a predetermined removal fraction in and below canopy to
C * ULAND and determines a transport factor (TFB) for this regime.
C * = applies to tfbelow

C Function: 3d point source emissions interface to the chemistry-transport model

C Revision History:
C 16 Dec 10 J.Young: Adapting Daniel Tong`s work on windblown dust
C 21 Apr 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C 11 May 11 D.Wong: incorporated twoway model implementation
C  8 Jul 11 J.Young: unified string lengths in character lists for compiler
C                    compatibility
C 11 Nov 11 J.Young: generalizing land use/cover
C  8 Jun 12 J.Young: remove full character blank padding for GNU Fortran (GCC) 4.1.2
C 13 Jul 12 J.Young: following Daniel Tong: changed clayc, siltc, sandc units from mass
C                    fraction to %; adjusted F/G (vertical to horizontal flux) ratio
C                    to be continuous for clay content > 20%
C 30 Sep 13 J.Young: corrected diag file units description; added snow cover adjustment;
C                    adjusted F/G (vertical to horizontal flux) ratio to be continuous
C                    for clay content > 0.2; convert volumetric soil moisture to
C                    gravimetric water content; corrected soil moisture factor (fmoit);
C                    use lwmask>0 rather than sltyp>0 (non-existent) for over water test
C-----------------------------------------------------------------------
      use lus_defn
      use aero_data, only: n_mode

      implicit none 

C number of chemical species in windblown dust aerosol
      integer, parameter :: ndust_spc = 19

C windblown dust emissions rates
      real, allocatable, save :: dustoutm( :,:,:,: )  ! mass emission rates [g/m**3/s]
      real, allocatable, save :: dustoutn( :,:,: )    ! number emission rates [1/m**3/s]
      real, allocatable, save :: dustouts( :,:,: )    ! surface-area emisrates [m2/m**3/s]

      public ndust_spc, dustoutm, dustoutn, dustouts, dust_spc,
     &       dust_emis_init, get_dust_emis
      private

      real, allocatable, save :: dust_em( :,: )  ! total dust emissions [g/m**3/s]

C number of lognormal modes in windblown dust aerosol = n_mode (from aero_data)
C - only accumulation and coarse modes used
      type dust_type
         character( 16 ) :: name  ( n_mode )
         real            :: spcfac( n_mode )
      end type dust_type

C speciation factors from SPECIATE composite of 4 desert soil profiles (Pouliot):
      type( dust_type ), parameter :: dust_spc( ndust_spc ) = (/
C                        -------- name --------         -------- spcfac --------
C                      Aitken    accum     coarse        Aitken  accum   coarse
     &   dust_type( (/'       ','ASO4J  ','ASO4K  '/), (/ 0.0, 0.02250, 0.02655/) ),   ! Sulfate
     &   dust_type( (/'       ','ANO3J  ','ANO3K  '/), (/ 0.0, 0.00020, 0.00160/) ),   ! Nitrate
     &   dust_type( (/'       ','ACLJ   ','ACLK   '/), (/ 0.0, 0.00945, 0.01190/) ),   ! Chlorine
     &   dust_type( (/'       ','ANH4J  ','       '/), (/ 0.0, 0.00005, 0.0    /) ),   ! Ammonium
     &   dust_type( (/'       ','ANAJ   ','       '/), (/ 0.0, 0.03935, 0.0    /) ),   ! Sodium
     &   dust_type( (/'       ','ACAJ   ','       '/), (/ 0.0, 0.07940, 0.0    /) ),   ! Calcium
     &   dust_type( (/'       ','AMGJ   ','       '/), (/ 0.0, 0.0,     0.0    /) ),   ! Magnesium
     &   dust_type( (/'       ','AKJ    ','       '/), (/ 0.0, 0.03770, 0.0    /) ),   ! Potassium
     &   dust_type( (/'       ','APOCJ  ','       '/), (/ 0.0, 0.01075, 0.0    /) ),   ! Organic Carbon
     &   dust_type( (/'       ','APNCOMJ','       '/), (/ 0.0, 0.00430, 0.0    /) ),   ! Non-Carbon Organic Matter
     &   dust_type( (/'       ','AECJ   ','       '/), (/ 0.0, 0.0,     0.0    /) ),   ! Elemental Carbon
     &   dust_type( (/'       ','AFEJ   ','       '/), (/ 0.0, 0.03355, 0.0    /) ),   ! Iron
     &   dust_type( (/'       ','AALJ   ','       '/), (/ 0.0, 0.05695, 0.0    /) ),   ! Aluminum
     &   dust_type( (/'       ','ASIJ   ','       '/), (/ 0.0, 0.19425, 0.0    /) ),   ! Silicon
     &   dust_type( (/'       ','ATIJ   ','       '/), (/ 0.0, 0.00280, 0.0    /) ),   ! Titanium
     &   dust_type( (/'       ','AMNJ   ','       '/), (/ 0.0, 0.00115, 0.0    /) ),   ! Manganese
     &   dust_type( (/'       ','AH2OJ  ','       '/), (/ 0.0, 0.00541, 0.0    /) ),   ! Water
     &   dust_type( (/'       ','AOTHRJ ','       '/), (/ 0.0, 0.50219, 0.0    /) ),   ! Other
     &   dust_type( (/'       ','       ','ASOIL  '/), (/ 0.0, 0.0,     0.95995/) ) /) ! Non-Anion Dust

      real, parameter :: fracmj = 0.20  ! mass fraction assigned to accum mode
      real, parameter :: fracmk = 0.80  ! mass fraction assigned to coarse mode

C diam`s from FRACMJ,FRACMK-weighted 2 2-bin averages of geom means
      real, parameter :: dgvj = 1.3914  ! geom mean diam of accum mode [um]
      real, parameter :: dgvk = 5.2590  ! geom mean diam of coarse mode [um]
      real, parameter :: sigj = 2.0000  ! geom std deviation of accum mode flux
      real, parameter :: sigk = 2.0000  ! geom std deviation of coarse mode flux

C Local Variables:

C Factors for converting 3rd moment emission rates into number and 2nd moment
C emission rates.  (Diameters in [um] changed to [m] ) See Equations 7b and 7c
C of Binkowski & Roselle (2003)
      real       :: l2sgj    ! [ln( sigj )] ** 2
      real       :: l2sgk    ! [ln( sigk )] ** 2
      real, save :: factnumj ! = exp( 4.5 * l2sgj ) / dgvj ** 3 * 1.0e18
      real, save :: factnumk ! = exp( 4.5 * l2sgk ) / dgvk ** 3 * 1.0e18
      real, save :: factm2j  ! = exp( 0.5 * l2sgj ) / dgvj * 1.0e6
      real, save :: factm2k  ! = exp( 0.5 * l2sgk ) / dgvk * 1.0e6
      real, save :: factsrfj ! = pi * factm2j
      real, save :: factsrfk ! = pi * factm2k

      real, save :: dustmode_dens( n_mode ) ! average modal density [kg/m**3]
      real       :: sumsplit, sumfrac
      integer    :: n, idx

C Number of soil types: For both WRF and MM5-PX met models, there are 16 types;
C the first 12 soil types are used and the rest lumped into Other.
      integer, parameter :: nsltyp = 13

C Variables for the windblown dust diagnostic file:
      logical, save      :: dustem_diag   ! flag for dustemis diagnostic file
      integer, parameter :: fndust_diag = 42 ! number of fixed diagnostic output vars
      integer, save      :: ndust_diag       ! number of diagnostic output vars
      real, allocatable, save :: diagv( : ) ! diagnostic output variables
      real, allocatable, save :: dustbf( :,:,: ) ! diagnostic accumulate buffer

#ifdef verbose_wbdust
      real, allocatable, save :: sdiagv( : )  ! global sum of each diag output var
#endif

      type diag_type
         character( 16 ) :: var
         character( 16 ) :: units
         character( 80 ) :: desc
      end type diag_type

      type( diag_type ), allocatable, save :: diagnm( : )
      type( diag_type ), allocatable, save :: vdiagnm_emis( : )
      type( diag_type ), allocatable, save :: vdiagnm_frac( : )
      type( diag_type ), allocatable, save :: vdiagnm_utstar( : )
      character( 10 ) :: truncnm
      character( 16 ) :: vnm

      type( diag_type ), parameter :: fdiagnm( fndust_diag ) = (/
C                      var              units                 desc
C                 ----------------    --------    -------------------------------------------
     & diag_type( 'Cropland_Emis   ', 'g/m**2/s', 'emissions for cropland landuse type        '),
     & diag_type( 'Desertland_Emis ', 'g/m**3/s', 'total for the desert types and cropland  '),
     & diag_type( 'Cropland_Frac   ', 'percent ', 'cropland erodible landuse fraction (%)     '),
     & diag_type( 'Desertland_Frac ', 'percent ', 'total desert fraction (%)                  '),
     & diag_type( 'Cropland_Utstar ', 'm/s     ', 'threshold u* for cropland                  '),
     & diag_type( 'Ustar           ', 'm/s     ', 'u* (friction velocity)                     '),
     & diag_type( 'Soil_Moist_Fac  ', '        ', 'soil moisture factor for threshold u*      '),
     & diag_type( 'Soil_Erode_Pot  ', '        ', 'soil -> dust erodiblity potential          '),
     & diag_type( 'VertOHoriz_Flux ', '        ', 'ratio of vertical to horizontal flux       '),
     & diag_type( 'Mx_Adsrb_H2O_Frc', '        ', 'max adsorbed water fraction                '),
     & diag_type( 'H2O_Barren_Cover', 'percent ', 'water & barren land coverage               '),
     & diag_type( 'Vegetation_Cover', 'percent ', 'vege land coverage                         '),
     & diag_type( 'Urban_Cover     ', 'percent ', 'urban land coverage                        '),
     & diag_type( 'Forest_Cover    ', 'percent ', 'forest land coverage                       '),
     & diag_type( 'Trfac_Above_Can' , '        ', 'transport factor above canopy              '),
     & diag_type( 'Trfac_Inside_Can', '        ', 'transport factor in and below canopy       '),
     & diag_type( 'ASO4J           ', 'g/s     ', 'accumulation mode Sulfate                  '),
     & diag_type( 'ANO3J           ', 'g/s     ', 'accumulation mode Nitrate                  '),
     & diag_type( 'ACLJ            ', 'g/s     ', 'accumulation mode Chlorine                 '),
     & diag_type( 'ANH4J           ', 'g/s     ', 'accumulation mode Ammonium                 '),
     & diag_type( 'ANAJ            ', 'g/s     ', 'accumulation mode Sodium                   '),
     & diag_type( 'ACAJ            ', 'g/s     ', 'accumulation mode Calcium                  '),
     & diag_type( 'AMGJ            ', 'g/s     ', 'accumulation mode Magnesium                '),
     & diag_type( 'AKJ             ', 'g/s     ', 'accumulation mode Potassium                '),
     & diag_type( 'APOCJ           ', 'g/s     ', 'accumulation mode Organic Carbon           '),
     & diag_type( 'APNCOMJ         ', 'g/s     ', 'accumulation mode Non-Carbon Organic Matter'),
     & diag_type( 'AECJ            ', 'g/s     ', 'accumulation mode Elemental Carbon         '),
     & diag_type( 'AFEJ            ', 'g/s     ', 'accumulation mode Iron                     '),
     & diag_type( 'AALJ            ', 'g/s     ', 'accumulation mode Aluminum                 '),
     & diag_type( 'ASIJ            ', 'g/s     ', 'accumulation mode Silicon                  '),
     & diag_type( 'ATIJ            ', 'g/s     ', 'accumulation mode Titanium                 '),
     & diag_type( 'AMNJ            ', 'g/s     ', 'accumulation mode Manganese                '),
     & diag_type( 'AH2OJ           ', 'g/s     ', 'accumulation mode Water                    '),
     & diag_type( 'AOTHRJ          ', 'g/s     ', 'accumulation mode Other                    '),
     & diag_type( 'ASO4K           ', 'g/s     ', 'coarse mode Sulfate                        '),
     & diag_type( 'ANO3K           ', 'g/s     ', 'coarse mode Nitrate                        '),
     & diag_type( 'ACLK            ', 'g/s     ', 'coarse mode Chlorine                       '),
     & diag_type( 'ASOIL           ', 'g/s     ', 'coarse mode total                          '),
     & diag_type( 'ANUMJ           ', '#/s     ', 'accumulation mode number                   '),
     & diag_type( 'ANUMK           ', '#/s     ', 'coarse mode number                         '),
     & diag_type( 'ASRFJ           ', 'm**2/s  ', 'accumulation mode surface area             '),
     & diag_type( 'ASRFK           ', 'm**2/s  ', 'coarse mode surface area                   ')/)
  
C Module shared variables:
      real, allocatable, save :: agland( :,: )   ! agriculture land fraction
      real, allocatable, save :: wmax  ( :,: )   ! max adsorb water percent
      real, allocatable, save :: kvh   ( :,: )   ! ratio of vertical flux / horizontal (k factor)
      real, allocatable, save :: sd_ep ( :,: )   ! soil->dust erodiblity potential
      real, allocatable, save :: tfb   ( :,: )   ! transport fraction in and below canopy

      integer, save :: sdate, stime  ! scenario start date & time

C Modified values for 12 NAM soil types; Silt values taken from Fig.1 of Gillette et
C al., 1980. Values of Other are set to zero so no clay is available for fine particle
C emissions.
      real :: clayc( nsltyp ) = ! the amount of clay in each soil type [Kg/Kg]
     &        (/ 0.03,   ! Sand
     &           0.06,   ! Loamy Sand
     &           0.10,   ! Sandy Loam
     &           0.13,   ! Silt Loam
     &           0.06,   ! Silt
     &           0.18,   ! Loam
     &           0.27,   ! Sandy Clay Loam
     &           0.34,   ! Silty Clay Loam
     &           0.34,   ! Clay Loam
     &           0.42,   ! Sandy Clay
     &           0.47,   ! Silty Clay
     &           0.58,   ! Clay
     &           0.00 /) ! Other

      real :: siltc( nsltyp ) = ! the amount of silt in each soil type [Kg/Kg]
     &        (/ 0.05,   ! Sand
     &           0.12,   ! Loamy Sand
     &           0.32,   ! Sandy Loam
     &           0.70,   ! Silt Loam
     &           0.84,   ! Silt
     &           0.05,   ! Loam
     &           0.15,   ! Sandy Clay Loam
     &           0.56,   ! Silty Clay Loam
     &           0.34,   ! Clay Loam
     &           0.06,   ! Sandy Clay
     &           0.47,   ! Silty Clay
     &           0.20,   ! Clay
     &           0.00 /) ! Other

      real :: sandc( nsltyp ) = ! the amount of sand in each soil type [Kg/Kg]
     &        (/ 0.92,   ! Sand
     &           0.82,   ! Loamy Sand
     &           0.58,   ! Sandy Loam
     &           0.17,   ! Silt Loam
     &           0.10,   ! Silt
     &           0.43,   ! Loam
     &           0.58,   ! Sandy Clay Loam
     &           0.10,   ! Silty Clay Loam
     &           0.32,   ! Clay Loam
     &           0.52,   ! Sandy Clay
     &           0.06,   ! Silty Clay
     &           0.22,   ! Clay
     &           0.00 /) ! Other

      real :: eropot( 3 ) =     ! erodible potential of soil components
     &        (/ 0.08,   ! clay
     &           1.00,   ! silt
     &           0.12 /) ! sand

      integer, save :: logdev

      CONTAINS

C=======================================================================
         function dust_emis_init( jdate, jtime, tstep ) result( success )

! Revision History.
!     Aug 12, 15 D. Wong: Replaced MYPE with IO_PE_INCLUSIVE for parallel I/O implementation

         use hgrd_defn        ! horizontal domain specifications
         use aero_data        ! aerosol species definitions
         use asx_data_mod     ! meteorology data
         use utilio_defn

C Arguments:
         integer, intent( in ) :: jdate   ! current model date, coded YYYYDDD
         integer, intent( in ) :: jtime   ! current model time, coded HHMMSS
         integer, intent( in ) :: tstep   ! output time step
         logical      success

C Includes:
         include SUBST_FILES_ID  ! file name parameters
 !       include SUBST_CONST     ! physical and mathematical constants

C External Functions:
         integer, external :: setup_logdev

C Local variables:
         character( 16 ) :: ctm_dustem_diag = 'CTM_DUSTEM_DIAG' ! env var for
                                                            ! diagnostic file
         character( 16 ) :: ctm_erode_agland = 'CTM_ERODE_AGLAND' ! env var to
                                                       ! use erodible cropland
         character( 16 ) :: pname = 'DUST_EMIS_INIT'
         character( 16 ) :: vname
         character( 80 ) :: vardesc
         character( 120 ) :: xmsg = ' '

         logical :: erode_agland    ! default .true.
         integer status
         integer c, r, i, j, k, l, n

 !       INTEGER KVHC

         interface
            subroutine cropcal ( jdate, jtime, agland )
               integer, intent( in )  :: jdate, jtime
               real,    intent( out ) :: agland( :,: )
            end subroutine cropcal
            subroutine tfbelow ( jdate, jtime, tfb )
               integer, intent( in )  :: jdate, jtime
               real,    intent( out ) :: tfb( :,: )
            end subroutine tfbelow
         end interface

C-----------------------------------------------------------------------

         logdev = setup_logdev()
         success = .true.

         allocate ( dustoutm( ndust_spc,n_mode,ncols,nrows ),
     &              dustoutn( n_mode,ncols,nrows ),
     &              dustouts( n_mode,ncols,nrows ), stat = status )
         if ( status .ne. 0 ) then
            xmsg = '*** DUSTOUTM, DUSTOUTN or DUSTOUTS memory allocation failed'
            call m3warn ( pname, jdate, jtime, xmsg )
            success = .false.; return
         end if

C Allocate emissions array
         allocate( dust_em( ncols,nrows ), stat = status )
         if ( status .ne. 0 ) then
            xmsg = '*** Failure allocating DUST_EM'
            call m3warn( pname, jdate, jtime, xmsg )
            success = .false.; return
         end if

C Allocate private arrays
         allocate( agland( ncols,nrows ),
     &             wmax  ( ncols,nrows ),
     &             kvh   ( ncols,nrows ),
     &             sd_ep ( ncols,nrows ),
     &             tfb   ( ncols,nrows ), stat = status )
         if ( status .ne. 0 ) then
            xmsg = '*** Failure allocating AGLAND, WMAX, KVH, SD_EP, or TFB'
            call m3warn( pname, jdate, jtime, xmsg )
            success = .false.; return
         end if
         agland = 0.0   ! array assignment
         wmax   = 0.0   ! array assignment
         kvh    = 0.0   ! array assignment
         sd_ep  = 0.0   ! array assignment

C Initialize land use/cover variables
         if ( .not. lus_init( jdate, jtime ) ) then
            xmsg = 'Failure initializing land use module'
            call m3exit( pname, jdate, jtime, xmsg, xstat2 )
         end if

C Get env var for diagnostic output
         dustem_diag = .false.         ! default
         vardesc = 'Flag for writing the windblown dust emission diagnostic file'
         dustem_diag = envyn( ctm_dustem_diag, vardesc, dustem_diag, status )
         if ( status .ne. 0 ) write( logdev,'( 5x, a )' ) vardesc
         if ( status .eq. 1 ) then
            xmsg = 'Environment variable improperly formatted'
            call m3warn( pname, jdate, jtime, xmsg )
            success = .false.; return
         else if ( status .eq. -1 ) then
            xmsg = 'Environment variable set, but empty ... Using default:'
            write( logdev,'( 5x, a, i9 )' ) xmsg, jtime
         else if ( status .eq. -2 ) then
            xmsg = 'Environment variable not set ... Using default:'
            write( logdev,'( 5x, a, i9 )' ) xmsg, jtime
         end if

         if ( dustem_diag ) then    ! Open the emissions diagnostic file

C Set up variable diagnostic names (from LUS_DEFN)
            allocate( vdiagnm_emis( n_dlcat ),
     &                vdiagnm_frac( n_dlcat ),
     &                vdiagnm_utstar( n_dlcat ), stat = status )
            if ( status .ne. 0 ) then
               xmsg = '*** Failure allocating AGLAND, WMAX, KVH, SD_EP,'
     &              // ' SLTYP, LWMASK, or TFB'
               call m3warn( pname, jdate, jtime, xmsg )
               success = .false.; return
            end if
            vdiagnm_emis = diag_type( ' ', ' ', ' ' )   ! array assignment
            vdiagnm_frac = diag_type( ' ', ' ', ' ' )   ! array assignment
            vdiagnm_utstar = diag_type( ' ', ' ', ' ' ) ! array assignment

            ndust_diag = fndust_diag + 3 * n_dlcat

            do i = 1, n_dlcat
               truncnm = vnmld( i )%desc   ! char( 10 )
C...           replace embedded spaces (within 16 chars) with "_"
C...           replace embedded dashes (within 16 chars) with "_"
               l = len_trim( truncnm )
               do k = 1, l
                  if ( truncnm( k:k ) .eq. " " .or.
     &                 truncnm( k:k ) .eq. "-" ) truncnm( k:k ) = "_"
               end do
               vnm = trim( truncnm ) // '_Emis'    ! char( 16 )
               vdiagnm_emis( i ) = diag_type( vnm, 'g/m**2/s', vnmld( i )%desc )
               vnm = trim( truncnm ) // '_Frac'    ! char( 16 )
               vdiagnm_frac( i ) = diag_type( vnm, 'percent', vnmld( i )%desc )
               vnm = trim( truncnm ) // '_Utstr'    ! char( 16 )
               vdiagnm_utstar( i ) = diag_type( vnm, 'm/s', vnmld( i )%desc )
            end do

C Allocate diagnostic emissions arrays
            allocate( diagnm( ndust_diag ),    ! diag_type
     &                diagv ( ndust_diag ),
     &                dustbf( ndust_diag,ncols,nrows ), stat = status )
            if ( status .ne. 0 ) then
               xmsg = '*** Failure allocating DIAGNM, DIAGV or DUSTBF'
               call m3warn( pname, jdate, jtime, xmsg )
               success = .false.; return
            end if

#ifdef verbose_wbdust
            allocate( sdiagv( ndust_diag ), stat = status )
            if ( status .ne. 0 ) then
               xmsg = '*** Failure allocating SDIAGV'
               call m3warn( pname, jdate, jtime, xmsg )
               success = .false.; return
            end if
#endif

C Build the complete diagnostic name array                     n for MODIS NOAH
            do i = 1, n_dlcat                                ! 4
               diagnm( i ) = vdiagnm_emis( i )
            end do
            n = n_dlcat + 1               ! 4 (if BELD3)       5
            diagnm( n ) = fdiagnm( 1 )    ! Cropland_Emis
            n = n + 1                     ! 5                  6
            diagnm( n ) = fdiagnm( 2 )    ! Desertland_Emis

            do i = 1, n_dlcat
               diagnm( i+n ) = vdiagnm_frac( i )
            end do
            n = n + n_dlcat + 1           ! 9                 11
            diagnm( n ) = fdiagnm( 3 )    ! Cropland_Frac
            n = n + 1                     ! 10                12
            diagnm( n ) = fdiagnm( 4 )    ! Desertland_Frac

            do i = 1, n_dlcat
               diagnm( i+n ) = vdiagnm_utstar( i )
            end do
            n = n + n_dlcat + 1           ! 14                17
            diagnm( n ) = fdiagnm( 5 )    ! Cropland_Utstar
            n = n + 1                     ! 15                18
            diagnm( n ) = fdiagnm( 6 )    ! Ustar

            n = n - 6                     ! 9                 12
            do i = 7, fndust_diag
               diagnm( i+n ) = fdiagnm( i )                 ! 19-54
            end do

            sdate = envint( 'CTM_STDATE', 'Scenario Start (YYYYJJJ)', 0, status )
            stime = envint( 'CTM_STTIME', 'Scenario Start (HHMMSS)', 0, status )

            if ( io_pe_inclusive )
     &         call opdust_emis ( sdate, stime, tstep, ndust_diag, diagnm )

         end if   ! dustem_diag

C Get soil type data   <== move to ASX_DATA_MOD ?
!        if ( metmod .eq. 'mm5' ) then
 !          vname = 'SLTYP'
!        else if (metmod == 'NAM') then
!           vname = 'sotyp'
!        end if

C Get env var for erodible agriculture land fraction
         erode_agland = .true.   ! default
         vardesc = 'Flag for calculating erodible agriculture land fraction'
         erode_agland = envyn( ctm_erode_agland, vardesc, erode_agland, status )
         if ( status .ne. 0 ) write( logdev,'( 5x, a )' ) vardesc
         if ( status .eq. 1 ) then
            xmsg = 'Environment variable improperly formatted'
            call m3warn( pname, jdate, jtime, xmsg )
            success = .false.; return
         else if ( status .eq. -1 ) then
            xmsg = 'Environment variable set, but empty ... Using default:'
            write( logdev,'( 5x, a, i9 )' ) xmsg, jtime
         else if ( status .eq. -2 ) then
            xmsg = 'Environment variable not set ... Using default:'
            write( logdev,'( 5x, a, i9 )' ) xmsg, jtime
         end if

         if ( erode_agland ) then
            call cropcal ( sdate, stime, agland )
            do r = 1, my_nrows
            do c = 1, my_ncols
               if ( agland( c,r ) .lt. 0.0 .or. agland( c,r ) .gt. 100.0 ) then
                  xmsg = '*** ERROR in AGLAND'
                  call m3exit( pname, jdate, jtime, xmsg, xstat1 )
               end if
            end do
            end do
         end if

C Get transport factor within canopy and 4 land use type percents
         call tfbelow ( jdate, jtime, tfb )

 !       KVHC = 0

         do r = 1, my_nrows
         do c = 1, my_ncols

C Get soil type index (LWMASK = 0 for water)
            if ( Grid_Data%lwmask( c,r ) .gt. 0.0 ) then
               j = Grid_Data%sltyp( c,r )
CCCCCCCC kludge (fixed in wrf-px after 4 Mar 11)
               if ( j .gt. 4 ) j = j + 1   ! PX combines "silt" with "silt loam"

C Calculate maximum amount of the adsorbed water
C    w` = 0.0014(%clay)**2 + 0.17(%clay) - w` in %
C    Fecan et al. 1999 Annales Geophys.,17,144-157.
               wmax( c,r ) = ( 14.0 * clayc( j ) + 17.0 ) * clayc( j )   ! [%]

C    log(F/G) = 0.134(%clayX100)-6
C    F/G (vertical to horizontal flux) ratio [1/cm] is a function of clay content 0~20%
C    Marticorena and Bergametti, 1995
               if ( clayc( j ) .le. 0.2 ) then
                  kvh( c,r ) = 10.0 ** ( 13.4 * clayc( j ) - 6.0 )
 !                KVHC = KVHC + 1
               else
!                 kvh( c,r ) = 2.0e-4
                  kvh( c,r ) = 4.786e-4 ! = 10.0 ** ( 13.4 * 0.2 - 6.0 )
               end if

C erodibility potential of soil component
               sd_ep( c,r ) = clayc( j ) * eropot( 1 )
     &                      + siltc( j ) * eropot( 2 )
     &                      + sandc( j ) * eropot( 3 )

            end if   ! over land

 !          if ( wmax ( c,r ) .lt. 0.0 .or. wmax ( c,r ) .gt. 100.0  .or.
 !   &           kvh  ( c,r ) .lt. 0.0 .or. kvh  ( c,r ) .gt. 5.0e-4 .or.
 !   &           sd_ep( c,r ) .lt. 0.0 .or. sd_ep( c,r ) .gt. 100.0 ) then
 !             xmsg = '*** ERROR in WMAX, KVH or SD_EP'
 !             write( logdev,'( 3x, a, 2i4 )' ) 'Column/Row: ', c, r
 !             write( logdev,'( 3x, a, f10.5 )' ) 'WMAX: ', wmax( c,r )
 !             write( logdev,'( 3x, a, f10.5 )' ) 'KVH: ', kvh( c,r )
 !             write( logdev,'( 3x, a, f10.5 )' ) 'SD_EP: ', sd_ep( c,r )
 !             call m3exit( pname, jdate, jtime, xmsg, xstat1 )
 !          end if

         end do   ! c
         end do   ! r

         kvh = 100.0 * kvh   ! 1/cm -> 1/m

         write( logdev,'( 2x, a, i8.6, f12.5 )' ) 'max wmax:', jtime, maxval( wmax )

 !       WRITE( LOGDEV,'( 2X, A, I8.6, I6 )' ) 'KVHC:', JTIME, KVHC

         l2sgj = log( sigj ) * log( sigj )
         l2sgk = log( sigk ) * log( sigk )

C Factors for converting 3rd moment emission rates into number and 2nd moment
C emission rates.  (Diameters in [um] changed to [m] ) See Equations 7b and 7c
C of Binkowski & Roselle (2003)
         factnumj = 1.0e18 * exp( 4.5 * l2sgj ) / dgvj ** 3
         factnumk = 1.0e18 * exp( 4.5 * l2sgk ) / dgvk ** 3
         factm2j  = 1.0e06 * exp( 0.5 * l2sgj ) / dgvj
         factm2k  = 1.0e06 * exp( 0.5 * l2sgk ) / dgvk
         factsrfj = pi * factm2j
         factsrfk = pi * factm2k

C Calculate modal average dust particle densities (accum and coarse modes) [ kg/m**3 ]
C The following works because the dust_spc`s are a fixed split of the total emitted
C mass.
         dustmode_dens( 1 ) = 0.0
         do n = 2, n_mode
            sumsplit = 0.0; sumfrac = 0.0
            do i = 1, ndust_spc
               sumsplit = sumsplit + dust_spc( i )%spcfac( n ) ! should = 1.0
               idx = findAero( dust_spc( i )%name( n ), .true. )
               sumfrac = sumfrac + dust_spc( i )%spcfac( n ) / aerospc( idx )%density
            end do
            dustmode_dens( n ) = sumsplit / sumfrac
         end do

#ifdef verbose_wbdust
         write( logdev,* ) ' '
         write( logdev,* ) '    l2sgj,l2sgk:         ', l2sgj, l2sgk
         write( logdev,* ) '    factnumj,factnumk:   ', factnumj, factnumk
         write( logdev,* ) '    factm2j,factm2k:     ', factm2j, factm2k
         write( logdev,* ) '    factsrfj,factsrfk:   ', factsrfj, factsrfk
         write( logdev,* ) '    modal avg dens(j/k): ', dustmode_dens( 2 ), dustmode_dens( 3 )
         write( logdev,* ) ' '
#endif

         end function dust_emis_init

C=======================================================================
         subroutine opdust_emis ( jdate, jtime, tstep, ndust_var, dust_var )

C   27 Dec 10 J.Young: initial

         use grid_conf           ! horizontal & vertical domain specifications
         use utilio_defn

         implicit none

         include SUBST_FILES_ID  ! file name parameters

C Arguments:
         integer      jdate      ! current model date, coded YYYYDDD
         integer      jtime      ! current model time, coded HHMMSS
         integer      tstep      ! output time step
         integer      ndust_var
         type( diag_type ) :: dust_var( ndust_var )

C Local variables:
         character( 16 ) :: pname = 'OPDUST_EMIS'
         character( 96 ) :: xmsg = ' '

         integer      v, l       ! loop induction variables

C-----------------------------------------------------------------------

C Try to open existing file for update
         if ( .not. open3( ctm_dust_emis_1, fsrdwr3, pname ) ) then
            xmsg = 'Could not open CTM_DUST_EMIS_1 for update - '
     &           // 'try to open new'
            call m3mesg( xmsg )

C Set output file characteristics based on COORD.EXT and open diagnostic file
            ftype3d = grdded3
            sdate3d = jdate
            stime3d = jtime
            tstep3d = tstep
            call nextime( sdate3d, stime3d, tstep3d ) !  start the next hour

            nvars3d = ndust_var
            ncols3d = gl_ncols
            nrows3d = gl_nrows
            nlays3d = 1
            nthik3d = 1
            gdtyp3d = gdtyp_gd
            p_alp3d = p_alp_gd
            p_bet3d = p_bet_gd
            p_gam3d = p_gam_gd
            xorig3d = xorig_gd
            yorig3d = yorig_gd
            xcent3d = xcent_gd
            ycent3d = ycent_gd
            xcell3d = xcell_gd
            ycell3d = ycell_gd
            vgtyp3d = vgtyp_gd
            vgtop3d = vgtop_gd
!           vgtpun3d = vgtpun_gd ! currently, not defined
            do l = 1, nlays3d + 1
               vglvs3d( l ) = vglvs_gd( l )
            end do
            gdnam3d = grid_name  ! from HGRD_DEFN

            do v = 1, nvars3d
               vtype3d( v ) = m3real
               vname3d( v ) = dust_var( v )%var
               units3d( v ) = dust_var( v )%units
               vdesc3d( v ) = dust_var( v )%desc
            end do

            fdesc3d( 1 ) = 'windblown dust parameters, variables, and'
            fdesc3d( 2 ) = 'hourly layer-1 windblown dust emission rates'
            do l = 3, mxdesc3
               fdesc3d( l ) = ' '
            end do

C Open windblown dust emissions diagnostic file
            if ( .not. open3( ctm_dust_emis_1, fsnew3, pname ) ) then
               xmsg = 'Could not create the CTM_DUST_EMIS_1 file'
               call m3exit( pname, sdate3d, stime3d, xmsg, xstat1 )
            end if

         end if

         return

         end subroutine opdust_emis

C=======================================================================
         subroutine get_dust_emis( jdate, jtime, tstep, rjacm, cellhgt )

         use grid_conf        ! horizontal & vertical domain specifications
         use asx_data_mod     ! meteorology data
         use aero_data
         use utilio_defn

C       8/18/11 D.Wong: incorporated twoway model implementation and change
C                       RC -> RCA and RN -> RNA and made it backward compatible
C       8/12/15 D.Wong: added code to handle parallel I/O implementation

C Arguments:
         integer, intent( in ) :: jdate      ! current model date, coded YYYYDDD
         integer, intent( in ) :: jtime      ! current model time, coded HHMMSS
         integer, intent( in ) :: tstep( 3 ) ! output time step, sync step, 2way step
         real,    intent( in ) :: rjacm( ncols,nrows ) ! reciprocal Jacobian [1/m]
         real,    intent( in ) :: cellhgt    ! grid-cell height [sigma]

C Includes:

         include SUBST_FILES_ID  ! file name parameters

C External Functions:
               
C Parameters:
         real zi                           ! Height of Internal Boundary Layer [m]
!        real, parameter :: sc  = 32.0     ! SENSIT-based scaling parameter
!        real, parameter :: z0s = 1.0e-05  ! Surface roughness for dust [m],
                                           ! Ref. Marticorena et al. JGR 1997

!        real, parameter :: amen = 2.8     ! values are from 0 to 3.5, mean 2.8
!        real, parameter :: amen = 2.61    ! Marticorena and Bergametti, 1995
         real, parameter :: amen = 1.0     ! Marticorena and Bergametti, 1997
!        real, parameter :: amen = 32.0    ! values adjusted based on Dale's MNORT measurements
         real, parameter :: radg = 122.87  ! rho_air/grav [g s^2/m^4] 122.87->120.8
!        real, parameter :: fdsa = 0.5     ! factor of dust source area 1/.1/.01
         real, parameter :: fdsa = 1.0     ! factor of dust source area 1/.1/.01
         real, parameter :: gmad = 0.78    ! Factor to convert geometric to aerodynamic diameter
         real, parameter :: cfac = amen * radg * fdsa * gmad  ! combined factor for qam
                               ! = 3066.8352,250.13874,95.8386 w/ amen=32.0,2.61,1.0 resp.

         real, parameter :: f6dpi = 6.0 / pi
         real, parameter :: gpkg = 1.0e03     ! g/kg

         character( 16 ) :: pname = 'GET_DUST_EMIS'
         character( 16 ) :: vname
         character( 96 ) :: xmsg
         integer status
         integer c, r, j, m, n, v

         integer, save   :: wstep = 0      ! local write counter
         integer         :: mdate, mtime   ! diagnostic file write date&time

                       ! automatic arrays
         real       :: fruf  ( ncols,nrows )
         real       :: fmoit ( ncols,nrows )      ! factor of soil moisture on u*t
         real       :: ustr  ( ncols,nrows )      ! friction velocity [m/s]
         real       :: soimt ( ncols,nrows )      ! gravimetric soil moisture (Kg/Kg)
         real       :: tfa   ( ncols,nrows )      ! transport fraction above canopy
         real       :: wrbuf ( ncols,nrows )      ! diagnositc write buffer
         real, allocatable, save :: utstar( :,:,: ) ! threshold U* [m/s]
         real, allocatable, save :: utem  ( :,:,: ) ! U term = U*(U**2-Ut**2) [(m/s)**3]
         real, allocatable, save :: qam   ( :,:,: ) ! emis for landuse type [g/m**2/s]
         real, allocatable, save :: elus  ( :,:,: ) ! Erodible landuse percent (0~100)
         real, allocatable, save :: qad   ( : )     ! temp var for qam

         real       :: edust( n_mode )  ! mass emis rate [g/s] per mode (only accum & coarse)
         real       :: sumdfr           ! sum var for deset fraction
         real       :: rlay1hgt         ! reciprocal of layer-1 height [1/m]
         real       :: m3j              ! 3rd moment accumulation (J) mode emis rates [m3/m3/s]
         real       :: m3k              ! 3rd moment coarse mode (K) emis rates [m3/m3/s]

         character( 16 ), save :: rc_name, rn_name    ! new names: RC -> RCA, RN -> RNA
         logical, save :: firstime = .true.

C Soil moisture limit: 13 types and 3 variables, which are:
C    1 - saturation moisture limit, (gravimetric units assumed, Kg/Kg)
             !!!! This is probably volumetric-- NOAH
C    2 - fill capacity, and   <- not used
C    3 - wilting point        <- not used
C Modified values compatiable with both MM5 & NAM.
C Silt values are based on NAM documentation on soil types.
C Other includes all types higher than 12. The values of Other, serving as
C placeholders, are randomly chosen.  Values of Other, however, have no effect
C on dust emissions as the threshold velocity of Other will be high.
C        real :: soilml( nsltyp,3 ) = reshape (
C    &           (/ 0.395,  0.135,  0.068,     ! Sand
C    &              0.410,  0.150,  0.075,     ! Loamy Sand
C    &              0.435,  0.195,  0.114,     ! Sandy Loam
C    &              0.485,  0.255,  0.179,     ! Silt Loam
C    &              0.476,  0.361,  0.084,     ! Silt
C    &              0.451,  0.240,  0.155,     ! Loam
C    &              0.420,  0.255,  0.175,     ! Sandy Clay Loam
C    &              0.477,  0.322,  0.218,     ! Silty Clay Loam
C    &              0.476,  0.325,  0.250,     ! Clay Loam
C    &              0.426,  0.310,  0.219,     ! Sandy Clay
C    &              0.482,  0.370,  0.283,     ! Silty Clay
C    &              0.482,  0.367,  0.286,     ! Clay
C    &              0.482,  0.367,  0.286 /),  ! Other
C    &           (/ nsltyp,3 /), order = (/ 2,1 /) ) ! fill columns first

C Since only soilml( nsltyp,1 ) is used, set the following:
!        real :: soilml1( nsltyp ) =
!    &           (/ 0.395,     ! Sand
!    &              0.410,     ! Loamy Sand
!    &              0.435,     ! Sandy Loam
!    &              0.485,     ! Silt Loam
!    &              0.476,     ! Silt
!    &              0.451,     ! Loam
!    &              0.420,     ! Sandy Clay Loam
!    &              0.477,     ! Silty Clay Loam
!    &              0.476,     ! Clay Loam
!    &              0.426,     ! Sandy Clay
!    &              0.482,     ! Silty Clay
!    &              0.482,     ! Clay
!    &              0.482 /)   ! Other

C convert to gravimetric [kg/kg]
         real :: soilml1( nsltyp ) =
     &           (/ 0.242,     ! Sand
     &              0.257,     ! Loamy Sand
     &              0.286,     ! Sandy Loam
     &              0.350,     ! Silt Loam
     &              0.350,     ! Silt
     &              0.307,     ! Loam
     &              0.277,     ! Sandy Clay Loam
     &              0.350,     ! Silty Clay Loam
     &              0.332,     ! Clay Loam
     &              0.284,     ! Sandy Clay
     &              0.357,     ! Silty Clay
     &              0.344,     ! Clay
     &              0.363 /)   ! Other

!! FROM NCAR LSM Group 17 Apr 2007   "volumetric"
!! ALSO in Spyrou, et al. JGR 2010
!        real :: soilml1( nsltyp ) =
!    &           (/ 0.339,     ! Sand
!    &              0.421,     ! Loamy Sand
!    &              0.434,     ! Sandy Loam
!    &              0.476,     ! Silt Loam
!    &              0.476,     ! Silt
!    &              0.439,     ! Loam
!    &              0.404,     ! Sandy Clay Loam
!    &              0.464,     ! Silty Clay Loam
!    &              0.465,     ! Clay Loam
!    &              0.406,     ! Sandy Clay
!    &              0.468,     ! Silty Clay
!    &              0.468,     ! Clay
!    &              0.482 /)   ! Other

C convert to gravimetric [kg/kg]
!        real :: soilml1( nsltyp ) =
!    &           (/ 0.208,     ! Sand
!    &              0.264,     ! Loamy Sand
!    &              0.286,     ! Sandy Loam
!    &              0.344,     ! Silt Loam
!    &              0.350,     ! Silt
!    &              0.299,     ! Loam
!    &              0.266,     ! Sandy Clay Loam
!    &              0.341,     ! Silty Clay Loam
!    &              0.324,     ! Clay Loam
!    &              0.271,     ! Sandy Clay
!    &              0.347,     ! Silty Clay
!    &              0.334,     ! Clay
!    &              0.363 /)   ! Other

C Threshold Friction Velocity [m/s] for 13 soil types and 3 land use types, which are:
C    1 -shrubland,
C    2 -shrubgrass land, and
C    3 -barren land
C Local (surface) Threshold Friction Velocity from field measurements
C   Ref. Gillette et al., JGR, 1980 for desert soils (J=2).
C   Ref. Gillette, JGR, 1988 for Loose Agr. Soils (J=3).
C The following soil types were not measured for desert land (we chose to use
C agr. data): Sandy Clay Loam, Clay Loam, Sandy Clay, and Silty Clay.
C Modified values compatible with both MM5 & NAM.
C There is no measurement of this value for Silt. The values for Silt are chosen from
C Silty Loam since the soil composition is close.
C Other includes all types higher than 12. The values of Other are too high to allow
C any dust emission.
         real :: uthfv( nsltyp,3 ) = reshape (
     &           (/ 0.80,   0.42,   0.28,     ! Sand
     &              1.00,   0.51,   0.34,     ! Loamy Sand
     &              1.40,   0.66,   0.29,     ! Sandy Loam
     &              1.70,   0.34,   1.08,     ! Silt Loam
     &              1.70,   0.34,   1.08,     ! Silt
     &              1.70,   0.49,   0.78,     ! Loam
     &              1.70,   0.78,   0.78,     ! Sandy Clay Loam
     &              1.70,   0.33,   0.64,     ! Silty Clay Loam
     &              1.70,   0.71,   0.71,     ! Clay Loam
     &              1.70,   0.71,   0.71,     ! Sandy Clay
     &              1.70,   0.56,   0.56,     ! Silty Clay
     &              1.70,   0.78,   0.54,     ! Clay
     &              9.99,   9.99,   9.99 /),  ! Other
     &           (/ nsltyp,3 /), order = (/ 2,1 /) ) ! fill columns first

C D.Tong change:
!        real :: uthfv( nsltyp,3 ) = reshape (
!    &           (/ 0.34,   0.34,   0.23,     ! Sand
!    &              0.47,   0.47,   0.24,     ! Loamy Sand
!    &              0.22,   0.42,   0.16,     ! Sandy Loam
!    &              0.22,   0.22,   0.51,     ! Silt Loam
!    &              0.22,   0.22,   0.71,     ! Silt
!    &              0.28,   0.29,   0.43,     ! Loam
!    &              0.42,   0.42,   0.58,     ! Sandy Clay Loam
!    &              0.32,   0.20,   0.32,     ! Silty Clay Loam
!    &              0.42,   0.42,   0.63,     ! Clay Loam
!    &              0.42,   0.42,   0.45,     ! Sandy Clay
!    &              0.20,   0.20,   0.19,     ! Silty Clay
!    &              0.42,   0.42,   0.29,     ! Clay
!    &              9.99,   9.99,   3.40 /),  ! Other
!    &           (/ nsltyp,3 /), order = (/ 2,1 /) ) ! fill columns first

         real :: agmsk = 0.75 ! agriculture (cropland) fraction

         interface
            subroutine tfabove ( tfa )
               real,    intent( out ) :: tfa( :,: )
            end subroutine tfabove
         end interface

#ifdef verbose_wbdust
         integer dryhit
         integer dusthit
#endif

 !      INTEGER FCT

C-----------------------------------------------------------------------

         if ( firstime ) then
            firstime = .false.
 !          if ( .not. desc3 ( met_cro_2d ) ) then  <== move to ASX_DATA_MOD ?
 !             xmsg = 'Could not get ' // met_cro_2d //' file description'
 !             call m3exit( pname, jdate, jtime, xmsg, xstat1 )
 !          end if
 !          if ( index1( 'RCA', nvars3d, vname3d ) .gt. 0 ) then
 !             rc_name = 'RCA'
 !             rn_name = 'RNA'
 !          else
 !             rc_name = 'RC'
 !             rn_name = 'RN'
 !          end if
            allocate ( utstar( ncols,nrows,n_dlcat+1 ),
     &                   utem( ncols,nrows,n_dlcat+1 ),
     &                    qam( ncols,nrows,n_dlcat+1 ),
     &                   elus( ncols,nrows,n_dlcat+1 ), stat = status )
            if ( status .ne. 0 ) then
               xmsg = '*** UTSTAR, UTEM, QAM, or ELUS memory allocation failed'
               call m3exit( pname, jdate, jtime, xmsg, xstat1 )
            end if
            allocate ( qad( n_dlcat+1 ), stat = status )
            if ( status .ne. 0 ) then
               xmsg = '*** QAD memory allocation failed'
               call m3exit( pname, jdate, jtime, xmsg, xstat1 )
            end if

#ifdef parallel_io
            if ( .not. io_pe_inclusive ) then
               if ( .not. open3( ctm_dust_emis_1, fsnonio, pname ) ) then
                  xmsg = 'Could not open ' // trim(ctm_dust_emis_1)
                  call m3exit( pname, jdate, jtime, xmsg, xstat1 )
               end if
            end if
#endif

         end if

C Convert layer-average U* into surface local U*
C    -> U*t has to be surface values from measurements.

!         do r = 1, my_nrows
!         do c = 1, my_ncols
!!           zi = sc * zruf( c,r )
!!           ustar( c,r ) = ustar( c,r ) * log( zi / zruf( c,r ) ) / log( zi / z0s )
!            ustr( c,r ) = Met_Data%ustar( c,r )
!     &                  * 3.465736 / log( 32.0e+05 * Met_Data%z0( c,r ) )
!         end do
!         end do

C ***
C        recalculating U* using the similarity theory and the aeolian
C        roughness (cf. the aerodynamic roughness which is used in WRF).
C        The constant value of z0 = 1e-5 m is used here following 
C        Prigent et al., JGR 2005 for now.          
C
         ustr = 0.4 * Met_Data%WSPD10 / LOG ( 10.0 / 0.00001 )
                                      ! 13.8155106
C         
C         Later, I should test this (following a discussion with J. Bash):
C          do r = 1, my_nrows
C          do c = 1, my_ncols
C          IF ( Met_Data%LAI( C,R ) .ge.  1.0 ) THEN
C                ustr( c,r ) = Met_Data%ustar( c,r ) * exp( -Met_Data%LAI( c,r ) )
C          ELSE 
C                ustr( c,r ) = 0.4 * Met_Data%WSPD10( c,r ) / LOG( 10.0 / 0.00001 )
C          ENDIF
C          end do
C          end do
C ***

C calculate transport factor above the canopy
         call tfabove ( tfa )

C Convert volumetric soil moisture to gravimetric water content based on Zender,
C et al. (2003)
C The met model soil moisture is assumed to be in volumetric units (m**3/m**3).
C If it is already gravimetric (Kg/Kg), the following conversion should be disabled
C below:
C           soimt( c,r ) = soimt( c,r )
C    &                   * 1000.0 / ( 2600.0 * ( 0.511 + 0.126 * sandc( j ) )
C sandc in %
C numerically has the effect of reducing soimt by from 0.62794 to 0.78277

C-----------------------------------------------------------------------
C  formula of Draxler & Gillette (2001) Atmos. Environ.
C      F   =  K A (r/g) U* ( U*^2 - Ut*^2 )
C  where
C      F   = emission flux                         [g/m**2-s]
C      K   = constant 2.0E-04                      [1/m]
C      A   = 0~3.5  mean = 2.8
C      r/g = ratio of air density to gravity=120.8 [g-s**2/m**4]
C      U*  = friction velocity                     [m/s]
C      Ut* = threshold friction velocity           [m/s]
C-----------------------------------------------------------------------

#ifdef verbose_wbdust
         dryhit = 0
         dusthit = 0
#endif

C Initialize windblown dust diagnostics output buffer
         if ( dustem_diag .and. wstep .eq. 0 ) then
            dustbf = 0.0   ! array assignment
#ifdef verbose_wbdust
            sdiagv = 0.0   ! array assignment
#endif
         end if

 !       FCT = 0

         do r = 1, my_nrows
         do c = 1, my_ncols
            dust_em( c,r ) = 0.0
            soimt( c,r ) = 0.0
!           fmoit( c,r ) = 999.9
            fmoit( c,r ) = 0.0   ! for diagnostic output visualization
            fruf ( c,r ) = 0.0
            do m = 1, n_dlcat+1
!              utstar( c,r,m ) = 999.9
               utstar( c,r,m ) = 0.0   ! for diagnostic output visualization
               qam   ( c,r,m ) = 0.0
               elus  ( c,r,m ) = 0.0
               utem  ( c,r,m ) = 0.0
            end do
            qad = 0.0   ! array assignment
            rlay1hgt = rjacm ( c,r ) / cellhgt

            if ( Met_Data%snocov( c,r ) .gt. 0.0 ) cycle   ! grid cell snow covered

C Dust possiblity if rain < 1/100 in. (1 in. = 2.540 cm) and not over water (SLTYP = 0)
C Dust possiblity if rain < 1/100 in. (1 in. = 2.540 cm) and not over water (SLTYP = 14)
!           if ( ( ( rn( c,r ) + rc( c,r ) ) .le. 0.0254 ) .and. ! rn, rc = [cm]
!??  &           ( sltyp( c,r ) .gt. 0.0 ) ) then
!    &           ( sltyp( c,r ) .lt. 14.0 ) ) then
C Dust possiblity if not over water and rain < 1/100 in. (1 in. = 2.540 cm)
            if ( ( Grid_Data%lwmask( c,r ) .gt. 0.0 ) .and.
     &           ( Met_Data%rn( c,r ) + Met_Data%rc( c,r ) ) .le. 0.0254 ) then
                                                             ! rn, rc = [cm]
               j = Grid_Data%sltyp( c,r )
               if ( j .gt. 13 ) j = 13   ! = ?

C Change soil moisture units from volumetric (m**3/m**3) to gravimetric (Kg/Kg)
               soimt( c,r ) = Met_Data%soim1( c,r ) ! <- [m**3/m**3]
     &                      * 1000.0 / ( 2600.0 * ( 0.511 + 0.126 * sandc( j ) ) )

C soil moisture effect on U*t
               if ( soimt( c,r ) .le. 0.01 * wmax( c,r ) ) then   ! wmax in [%]
                  fmoit( c,r ) = 1.0
               else if ( soimt( c,r ) .le. soilml1( j ) ) then
 !                FCT = FCT + 1
                  fmoit( c,r ) = sqrt( 1.0 + 1.21
     &                         * ( 100.0 * soimt( c,r ) - wmax( c,r ) ) ** 0.68 )
!              else
!                 fmoit( c,r ) = ?  not used
               end if
C
C roughness effect on effect on U*t            
C fruf( c,r ) = 1.0 - ((LOG(0.00001/0.00000333))/(LOG(0.7*(0.1/0.00000333)**0.8)))
               fruf( c,r ) = 1.0   ! roughness factor = 1.0 (for now)             
!              if ( fruf( c,r ) .le. 0.0 .or. fruf( c,r ) .gt. 1.0 ) then
!                 xmsg = 'ERROR: fruf out of bound'
!                 call m3exit( pname, jdate, jtime, xmsg, xstat1 )
!              end if
!              fruf( c,r ) = 1.0 / fruf( c,r ) 
C
C Dust occurs where erodible land exists, soil moisture is less than saturation,
C and U* > U*t

               if ( soimt( c,r ) .le. soilml1( j ) ) then

#ifdef verbose_wbdust
                  dryhit = dryhit + 1
#endif

C Adjust U*t based on soil moisture ---
                  do m = 1, n_dlcat     ! desert type landuse category
                     elus( c,r,m ) = ladut( c,r,m ) * dmsk( m )  ! desert land [%]
                  end do
                  elus( c,r,n_dlcat+1 ) = agland( c,r ) * agmsk  ! crop land [%]

                  do m = 1, n_dlcat+1   ! desert type & crop landuse category

                     if ( elus( c,r,m ) .gt. 100.0 .or. elus( c,r,m ) .lt. 0.0 ) then
                        write( xmsg,2009 ) elus( c,r,m ), c, r, m
                        call m3exit( pname, jdate, jtime, xmsg, xstat1 )
                     end if

                     if ( elus( c,r,m ) .gt. 0.0 ) then
!                       utstar( c,r,m ) = 0.1 * uthfv( j,n ) * fmoit( c,r ) ! testing
!                       utstar( c,r,m ) = 0.5 * uthfv( j,3 ) * fmoit( c,r ) ! testing
!                       n = dmap( m )   ! map to BELD3 for uthfv
!                       utstar( c,r,m ) = uthfv( j,n ) * fmoit( c,r )
                        utstar( c,r,m ) = uthfv( j,3 ) * fmoit( c,r ) * fruf( c,r )
                        if ( ustr( c,r ) .gt. utstar( c,r,m ) ) then
                           ! wind erosion occurs only when U* > Ut*
!                          utem( c,r,m ) = ustr( c,r )
                           utem( c,r,m ) = ( ustr( c,r ) + utstar( c,r,m ) )
     &                                   * ( ustr( c,r ) * ustr( c,r )
     &                                   -   utstar( c,r,m ) * utstar( c,r,m ) )
                           qam( c,r,m ) = cfac * kvh( c,r ) * utem( c,r,m ) * sd_ep( c,r )
                                                                          ! [g/m**2/s]
!                          qam( c,r,m ) = qam( c,r,m ) * rlay1hgt
                           qad( m ) = qad( m ) + qam( c,r,m ) * rlay1hgt
     &                              * ( elus( c,r,m ) * 0.01 )            ! [g/m**3/s]
                        end if
                     end if

                     if ( elus( c,r,m ) .eq. 0.0 .and. qad( m ) .ne. 0.0 ) then
                        xmsg = 'ERROR: erodible land use = 0, but emissions .ne. 0'
                        call m3exit( pname, jdate, jtime, xmsg, xstat1 )
                     end if

!                    dust_em( c,r ) = dust_em( c,r ) + qam( c,r,m )
                     dust_em( c,r ) = dust_em( c,r ) + qad( m )

                  end do   ! m landuse

C Dust removal by surrounding vegetation <-???
C Adjust dust emissions for transport factors

                  dust_em( c,r ) = dust_em( c,r ) * tfa( c,r ) * tfb( c,r )

               end if   ! if soil moisture
            end if   ! if rain & land

         end do   ! c
         end do   ! r

 !       WRITE( LOGDEV,'( 3X, A, I8.6, I6 )' ) 'FCT:', JTIME, FCT

#ifdef verbose_wbdust
         write( logdev,'( /5x, a, 1x, 2i8 )' ) 'dry hit count, out of total cells:',
     &                                         dryhit, (c-1)*(r-1)
#endif

         do r = 1, my_nrows
         do c = 1, my_ncols

C J/K mass emis rate [g/s]  (edust( 1 ) not used)
            edust( 2 ) = fracmj * dust_em( c,r )
            edust( 3 ) = fracmk * dust_em( c,r )

            do v = 1, ndust_spc
               dustoutm( v,1,c,r ) = 0.0
            end do

            do n = 2, n_mode
               do v = 1, ndust_spc
                  dustoutm( v,n,c,r ) = edust( n ) * dust_spc( v )%spcfac( n )
               end do
            end do

C J/K 3rd moment emis rate [m3/s] (needed for number and surface)
            m3j = edust( 2 ) * f6dpi / ( gpkg * dustmode_dens( 2 ) )
            m3k = edust( 3 ) * f6dpi / ( gpkg * dustmode_dens( 3 ) )

C Mode-specific emission rates of particle number [1/s]
            dustoutn( 1,c,r ) = 0.0
            dustoutn( 2,c,r ) = m3j * factnumj
            dustoutn( 3,c,r ) = m3k * factnumk

C Mode-specific dry surface area emission rates [m**2/s].
C 2nd moment multiplied by PI to obtain the surface area emissions rate.
            dustouts( 1,c,r ) = 0.0
            dustouts( 2,c,r ) = m3j * factsrfj
            dustouts( 3,c,r ) = m3k * factsrfk

#ifdef verbose_wbdust
            if ( m3j .ne. 0.0 ) dusthit = dusthit + 1
#endif
   
            if ( dustem_diag ) then
               do m = 1, n_dlcat+1
                  diagv( m ) = qam( c,r,m )             ! g/m**2/s
               end do
               n = n_dlcat + 2       !  5 (if BELD3)
               diagv( n ) = dust_em( c,r )              ! g/m**3/s

               sumdfr = 0.0
               do m = 1, n_dlcat+1
                  diagv( m+n ) = elus( c,r,m )
                  sumdfr = sumdfr + elus( c,r,m )
               end do
               n = n + n_dlcat + 2   ! 10
               diagv( n ) = sumdfr

               do m = 1, n_dlcat+1
                  if ( utstar( c,r,m ) .ne. 999.9 ) then
                     diagv( m+n ) = utstar( c,r,m )
                  else
                     diagv( m+n ) = 0.0
                  end if
               end do

               n = n + n_dlcat + 2   ! 15
               diagv( n    ) = ustr ( c,r )       ! 'Ustar' (sublayer)
               diagv( n+1  ) = fmoit( c,r )       ! 'Soil_Moist_Fac  '
               diagv( n+2  ) = sd_ep( c,r )       ! 'Soil_Erode_Pot  '
               diagv( n+3  ) = kvh  ( c,r )       ! 'VertOHoriz_Flux '
               diagv( n+4  ) = wmax ( c,r )       ! 'Mx_Adsrb_H2O_Frc' 
               diagv( n+5  ) = uland( c,r,1 )     ! 'H2O_Barren_Cover'
               diagv( n+6  ) = uland( c,r,2 )     ! 'Vegetation_Cover'
               diagv( n+7  ) = uland( c,r,3 )     ! 'Urban_Cover     '
               diagv( n+8  ) = uland( c,r,4 )     ! 'Forest_Cover    '
               diagv( n+9  ) = tfa  ( c,r )       ! 'Trfac_Above_Can '
               diagv( n+10 ) = tfb  ( c,r )       ! 'Trfac_Inside_Can'

               n = n + 10            ! 25

               do v = 1, ndust_spc-1
                  diagv( v+n ) = dustoutm( v,2,c,r )  ! accum. mode
               end do   ! diagv(25+18) = diagv(43)

               n = n + ndust_spc - 1 ! 43
               do v = 1, 3
                  diagv( v+n ) = dustoutm( v,3,c,r )  ! coarse mode
               end do   ! diagv(43+3) = diagv(46)
               n = n + 4             ! 47
               diagv( n ) = dustoutm( ndust_spc,3,c,r )  ! 'ASOIL'

               diagv( n+1 ) = dustoutn( 2,c,r )
               diagv( n+2 ) = dustoutn( 3,c,r )
               diagv( n+3 ) = dustouts( 2,c,r )
               diagv( n+4 ) = dustouts( 3,c,r )

C Multiply by sync step because when write to output we divide by the output step
C to get a timestep average.
               do v = 1, ndust_diag
                  dustbf( v,c,r ) = dustbf( v,c,r ) + diagv( v )
     &                            * float( time2sec( tstep( 2 ) ) )
#ifdef verbose_wbdust
                  sdiagv( v ) = sdiagv( v ) + diagv( v )
     &                                      * float( time2sec( tstep( 2 ) ) )
#endif
               end do
            end if   ! dustem_diag
         end do   ! col
         end do   ! row

#ifdef verbose_wbdust
         write( logdev,'( 5x, a, 2i8 / )' ) 'dust hit count, out of total cells:',
     &                                      dusthit, (c-1)*(r-1)
#endif

         if ( dustem_diag ) then

C If last call this hour, write out the windblown dust emissions dignostics.
C Then reset the emissions array and local write counter.

            wstep = wstep + time2sec( tstep( 2 ) )

            if ( wstep .ge. time2sec( tstep( 1 ) ) ) then
               if ( .not. currstep( jdate, jtime, sdate, stime, tstep( 1 ),
     &                              mdate, mtime ) ) then
                  xmsg = 'Cannot get step date and time'
                  call m3exit( pname, jdate, jtime, xmsg, xstat3 )
               end if
               call nextime( mdate, mtime, tstep( 1 ) )

               do v = 1, ndust_diag
                  do r = 1, my_nrows
                     do c = 1, my_ncols
                        wrbuf( c,r ) = dustbf( v,c,r ) / float( wstep )
                     end do
                  end do

                  if ( .not. WRITE3( ctm_dust_emis_1, diagnm( v )%var,
     &                       mdate, mtime, wrbuf ) ) then
                     xmsg = 'Could not write ' // trim( diagnm( v )%var )
     &                    // ' to CTM_DUST_EMIS_1'
                     call m3exit( pname, mdate, mtime, xmsg, xstat1 )
                  end if
               end do
               write( logdev,'( /5x, 2( a, 1x ), i8, ":", i6.6 )' )
     &               'Timestep written to CTM_DUST_EMIS_1',
     &               'for date and time', mdate, mtime
#ifdef verbose_wbdust
               sdiagv = sdiagv / float( wstep )   ! array assignment
               write( logdev,2015 ) jdate, jtime
               do v = 1, ndust_diag
                  if ( diagnm( v )%var(1:4) .ne. 'ANUM' ) then
                     write( logdev,2019 ) v, diagnm( v )%var, sdiagv( v )
                  else
                     write( logdev,2023 ) v, diagnm( v )%var, sdiagv( v )
                  end if
               end do
               sdiagv = 0.0   ! array assignment
#endif
               wstep = 0
               dustbf = 0.0   ! array assignment
            end if   !  time to write
         end if   ! dustem_diag

2009     Format( '*** Erodible landuse incorrect ', 1pe13.5, 1x, 'at: ', 3i4 )
2015     format( /5x, 'Total grid time-avg sum of dust emis variables at:',
     &            1x, i8, ":", I6.6  )
2019     format( i10, 1x, a, f20.5 )
2023     format( i10, 1x, a, e20.3 )

         end subroutine get_dust_emis

c-----------------------------------------------------------------------

      end module dust_emis

