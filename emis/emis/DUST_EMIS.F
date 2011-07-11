
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
C $Header: /project/yoj/arc/CCTM/src/emis/emis/DUST_EMIS.F,v 1.4 2011/07/11 13:40:36 yoj Exp $

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      module dust_emis
     
C-----------------------------------------------------------------------
C Description:
C * Extracts selected landuse types from BELD01 and BELD03 and merges
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
C  8 Jul 11 J.Young: unified string lengths in character lists for compiler
C                    compatibility
C-----------------------------------------------------------------------
      use aero_data, only: n_mode

      implicit none 

C number of chemical species in windblown dust aerosol
      integer, parameter :: ndust_spc = 19

C Sea-Salt Emissions Rates
      real, allocatable, save :: dustoutm( :,:,:,: )  ! mass emission rates [g/m**3/s]
      real, allocatable, save :: dustoutn( :,:,: )    ! number emission rates [1/m**3/s]
      real, allocatable, save :: dustouts( :,:,: )    ! surface-area emisrates [m2/m**3/s]

      public ndust_spc, dustoutm, dustoutn, dustouts, dust_spc,
     &       dust_emis_init, get_dust_emis
      private

      real, allocatable, save :: dust_em( :,: )  ! total dust emissions [g/s]

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

      integer, parameter :: ndltyp = 3   ! number of desert land type beld1
      character( 16 ) :: vnmld( ndltyp ) = (/
     &                   'USGS_shrubland ',
     &                   'USGS_shrubgrass',
     &                   'USGS_sprsbarren' /)

C Number of soil types: For both WRF and MM5-PX met models, there are 16 types;
C the first 12 soil types are used and the rest lumped into Other.
      integer, parameter :: nsltyp = 13

C Variables for the windblown dust diagnostic file:
      logical, save      :: dustem_diag   ! flag for dustemis diagnostic file
      integer, parameter :: ndust_diag = 51 ! number of diagnostic output variables
      real, save         :: diagv( ndust_diag ) ! diagnostic output variables
      real, allocatable, save :: dustbf( :,:,: ) ! diagnostic accumulate buffer

#ifdef verbose
      real, save         :: sdiagv( ndust_diag )  ! global sum of each diag output var
#endif

      character( 16 ) :: diagnm( ndust_diag ) = (/  ! output variable names
     &                   'QSHLD  ', 'QSHGR  ', 'QSPBA  ', 'QCROP  ',
     &                   'ETOTL  ',
     &                   'LSHLD  ', 'LSHGR  ', 'LSPBA  ', 'LCROP  ',
     &                   'TLDST  ',
     &                   'TFABV  ', 'TFINF  ',
     &                   'FMOIT  ', 'SD_EP  ', 'KV2HZ  ', 'WTMAX  ',
     &                   'ULDBR  ', 'ULDVG  ', 'ULDUB  ', 'ULDFR  ',
     &                   'USTAR  ' ,
     &                   'USHLD  ', 'USHGR  ', 'USPBA  ', 'UCROP  ',
     &                   'ASO4J  ', 'ANO3J  ', 'ACLJ   ', 'ANH4J  ',
     &                   'ANAJ   ', 'ACAJ   ', 'AMGJ   ', 'AKJ    ',
     &                   'APOCJ  ', 'APNCOMJ', 'AECJ   ',
     &                   'AFEJ   ', 'AALJ   ', 'ASIJ   ', 'ATIJ   ', 'AMNJ   ',
     &                   'AH2OJ  ', 'AOTHRJ ',
     &                   'ASO4K  ', 'ANO3K  ', 'ACLK   ', 'ASOIL  ',
     &                   'ANUMJ  ', 'ANUMK  ', 'ASRFJ  ', 'ASRFK  ' /)
!    &                   'STDEVJ ', 'STDEVK '  /)

C diagnostic output from duster (in PRMTR( :,:,1:29 ):
C  1 'QSHLD'  QAM( C,R,1 ) * 1.E6  ! g/s->ug/s   emissions for shrub landuse type
C  2 'QSHGR'  QAM( C,R,2 ) * 1.E6  ! g/s->ug/s      "      "  shrubgrass landuse type
C  3 'QSPBA'  QAM( C,R,3 ) * 1.E6  ! g/s->ug/s      "      "  barren landuse type
C  4 'QCROP'  QAM( C,R,4 ) * 1.E6  ! g/s->ug/s      "      "  cropland landuse type
C  5 'ETOTL'  QAT( C,R )           ! g/s         total for the 3 desert types and cropland
C  6 'LSHLD'  ELUS( C,R,1 )        ! shrub       erodible landuse fraction (%)
C  7 'LSHGR'  ELUS( C,R,2 )        ! shrubgrass     "        "       "
C  8 'LSPBA'  ELUS( C,R,3 )        ! barren         "        "       "
C  9 'LCROP'  ELUS( C,R,4 )        ! cropland       "        "       "
C 10 'TLDST'                       ! total desert fraction
C  =  LADUT( C,R,1 ) + LADUT( C,R,2 ) + LADUT( C,R,3 ) + AGLAND( C,R )
C 11 'TFABV'  TFA ( C,R )          ! transport fraction above canopy
C 12 'TFINF'  TFB ( C,R )          ! transport fraction in and below canopy
C 13 'FMOIT'  FMOIT( C,R )         ! soil moisture factor for threshold u*
C 14 'SD_EP'  SD_EP ( C,R )        ! soil->dust erodiblity potential
C 15 'KV2HZ'  KVH  ( C,R )         ! ratio of vertical to horizontal flux
C 16 'WTMAX'  WMAX ( C,R )         ! max adsorbed water fraction
C 17 'ULDBR'  ULAND( C,R,1 )       ! water & barren land coverage
C 18 'ULDVG'  ULAND( C,R,2 )       ! vege land coverage
C 19 'ULDUB'  ULAND( C,R,3 )       ! urban land coverage
C 20 'ULDFR'  ULAND( C,R,4 )       ! forest land coverage
C 21 'USTAR'  USTAR ( C,R )        ! u* (friction velocity)
C 22 'USHLD'  UTSTAR ( C,R,1 ) or 0.0  ! threshold u* for shrub
C 23 'USHGR'  UTSTAR ( C,R,2 ) or 0.0  !     "     "      shrubgrass
C 24 'USPBA'  UTSTAR ( C,R,3 ) or 0.0  !     "     "      barren
C 25 'UCROP'  UTSTAR ( C,R,4 ) or 0.0  !     "     "      cropland
C 26 'ASO4J'                       ! accumulation mode Sulfate
C 27 'ANO3J'                       ! accumulation mode Nitrate
C 28 'ACLJ'                        ! accumulation mode Chlorine
C 29 'ANH4J'                       ! accumulation mode Ammonium
C 30 'ANAJ'                        ! accumulation mode Sodium
C 31 'ACAJ'                        ! accumulation mode Calcium
C 32 'AMGJ'                        ! accumulation mode Magnesium
C 33 'AKJ'                         ! accumulation mode Potassium
C 34 'APOCJ'                       ! accumulation mode Organic Carbon
C 35 'APNCOMJ'                     ! accumulation mode Non-Carbon Organic Matter
C 36 'AECJ'                        ! accumulation mode Elemental Carbon
C 37 'AFEJ'                        ! accumulation mode Iron
C 38 'AALJ'                        ! accumulation mode Aluminum
C 39 'ASIJ'                        ! accumulation mode Silicon
C 40 'ATIJ'                        ! accumulation mode Titanium
C 41 'AMNJ'                        ! accumulation mode Manganese
C 42 'AH2OJ'                       ! accumulation mode Water
C 43 'AOTHRJ'                      ! accumulation mode Other
C 44 'ASO4K'                       ! coarse mode Sulfate
C 45 'ANO3K'                       ! coarse mode Nitrate
C 46 'ACLK'                        ! coarse mode Chlorine
C 47 'ASOIL'                       ! coarse mode total
C 48 'ANUMJ'                       ! accumulation mode number   [#/s]
C 49 'ANUMK'                       ! coarse mode number         [#/s]
C 50 'ASRFJ'                       ! accumulation mode surface area  [m**2/s]
C 51 'ASRFK'                       ! coarse mode surface area        [m**2/s]
CX 52 'STDEVJ'                     ! accumulation mode geometric standard deviation
CX 53 'STDEVK'                     ! coarse mode geometric standard deviation

C Module shared variables:
      real, allocatable, save :: sltyp ( :,: )   ! soil type (13 USDA categories)
      real, allocatable, save :: lwmask( :,: )   ! land-water mask (1=land, 0=water)
      real, allocatable, save :: agland( :,: )   ! agriculture land fraction
      real, allocatable, save :: tfb   ( :,: )   ! transport fraction in and below canopy
      real, allocatable, save :: wmax  ( :,: )   ! max adsorb water fraction
      real, allocatable, save :: kvh   ( :,: )   ! ratio of vertical flux / horizontal (k factor)
      real, allocatable, save :: sd_ep ( :,: )   ! soil->dust erodiblity potential
      real, allocatable, save :: uland ( :,:,: ) ! fraction of 4 landtype for dust removal [%]
      real, allocatable, save :: ladut ( :,:,: ) ! % of desertland (BELD01)

      integer, save :: sdate, stime  ! scenario start date & time

C Domain decomposition info from emission and meteorology files
      integer, save :: gxoff, gyoff             ! origin offset
      integer, save :: strtcol_01, endcol_01, strtrow_01, endrow_01
      integer, save :: strtcolgc2, endcolgc2, strtrowgc2, endrowgc2
      integer, save :: strtcolmc2, endcolmc2, strtrowmc2, endrowmc2

C Modified values for 12 NAM soil types; Silt values taken from Fig.1 of Gillette et
C al., 1980. Values of Other are set to zero so no clay is available for fine particle
C emissions.
      real :: clayc( nsltyp ) = ! the amount of clay in each soil type
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

      real :: siltc( nsltyp ) = ! the amount of silt in each soil type
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

      real :: sandc( nsltyp ) = ! the amount of sand in each soil type
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

C Variables for grid cell area
      real, save    :: dx1, dx2     ! grid-cell width and length [m]
      real, save    :: cellarea     ! grid-cell area [m**2]

      integer, save :: logdev

      CONTAINS

C=======================================================================
         function dust_emis_init( jdate, jtime, tstep ) result( success )

         use hgrd_defn        ! horizontal domain specifications
         use aero_data        ! aerosol species definitions
         use utilio_defn

C Arguments:
         integer, intent( in ) :: jdate   ! current model date, coded YYYYDDD
         integer, intent( in ) :: jtime   ! current model time, coded HHMMSS
         integer, intent( in ) :: tstep   ! output time step
         logical      success

C Includes:
         include SUBST_FILES_ID  ! file name parameters
         include SUBST_CONST     ! physical and mathematical constants

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

         logical :: erode_agland = .true. ! default
         integer status
         integer i, j, c, r

         interface
            subroutine cropcal ( jdate, jtime, agland )
            use hgrd_defn
            use utilio_defn
            implicit none
            integer, intent( in )  :: jdate, jtime
            real,    intent( out ) :: agland( :,: )
            end subroutine cropcal
            subroutine tfbelow ( jdate, jtime, uland, tfb )
            use hgrd_defn
            use utilio_defn
            implicit none
            integer, intent( in )  :: jdate, jtime
            real,    intent( out ) :: uland( :,:,: )
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
     &             sltyp ( ncols,nrows ),
     &             lwmask( ncols,nrows ),
     &             tfb   ( ncols,nrows ),
     &             uland ( ncols,nrows,4 ),
     &             ladut ( ncols,nrows,ndltyp ), stat = status )
         if ( status .ne. 0 ) then
            xmsg = '*** Failure allocating AGLAND, WMAX, KVH, SD_EP,'
     &           // ' SLTYP, LWMASK, TFB, ULAND, or LADUT'
            call m3warn( pname, jdate, jtime, xmsg )
            success = .false.; return
         end if

         agland = 0.0   ! array assignment
         wmax   = 0.0   ! array assignment
         kvh    = 0.0   ! array assignment
         sd_ep  = 0.0   ! array assignment

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

         if ( dustem_diag ) then    ! Open the sea-salt emission dignostic file

C Allocate diagnostic emissions arrays
            allocate( dustbf( ndust_diag,ncols,nrows ), stat = status )
            if ( status .ne. 0 ) then
               xmsg = '*** Failure allocating DUSTBF'
               call m3warn( pname, jdate, jtime, xmsg )
               success = .false.; return
            end if

            sdate = envint( 'CTM_STDATE', 'Scenario Start (YYYYJJJ)', 0, status )
            stime = envint( 'CTM_STTIME', 'Scenario Start (HHMMSS)', 0, status )

            if ( mype .eq. 0 )
     &         call opdust_emis ( sdate, stime, tstep, ndust_diag, diagnm )

         end if

         call subhfile ( dust_lu_1, gxoff, gyoff,
     &                   strtcol_01, endcol_01, strtrow_01, endrow_01 )

C Get desert land (fraction) data
         do i = 1, ndltyp
            if ( .not. xtract3( dust_lu_1, vnmld( i ),
     &                          1,1, strtrow_01,endrow_01, strtcol_01,endcol_01,
     &                          0, 0, ladut( 1,1,i ) ) ) then
               xmsg = 'Could not read ' // trim( vnmld( i ) ) // ' from DUST_LU_1'
               call m3exit( pname, jdate, jtime, xmsg, xstat1 )
            end if
         end do

         call subhfile ( met_cro_2d, gxoff, gyoff,
     &                   strtcolmc2, endcolmc2, strtrowmc2, endrowmc2 )

         call subhfile ( grid_cro_2d, gxoff, gyoff,
     &                   strtcolgc2, endcolgc2, strtrowgc2, endrowgc2 )

C Get soil type data
!        if ( metmod .eq. 'mm5' ) then
            vname = 'SLTYP'
!        else if (metmod == 'NAM') then
!           vname = 'sotyp'
!        end if
         if ( .not. interpx( met_cro_2d, vname, pname,
     &                       strtcolmc2,endcolmc2, strtrowmc2,endrowmc2, 1,1,
     &                       jdate, jtime, sltyp ) ) then
            xmsg = 'Could not read ' // trim( vname ) // ' from MET_CRO_2D'
            call m3exit( pname, jdate, jtime, xmsg, xstat1 )
         end if

         vname = 'LWMASK'
         if ( .not. interpx( grid_cro_2d, vname, pname,
     &                       strtcolgc2,endcolgc2, strtrowgc2,endrowgc2, 1,1,
     &                       jdate, jtime, lwmask ) ) then
            xmsg = 'Could not read ' // trim( vname ) // ' from GRID_CRO_2D'
            call m3exit( pname, jdate, jtime, xmsg, xstat1 )
         end if

C Get grid cell area
C note: crude estimate is made for LAT/LONG coordinate systems
         if ( gdtyp_gd .eq. latgrd3 ) then
            dx1 = dg2m * xcell_gd ! in m
            dx2 = dg2m * ycell_gd
     &          * cos( pi180*( yorig_gd + ycell_gd
     &          * float( gl_nrows/2 ) ) ) ! in m
         else
            dx1 = xcell_gd        ! in m
            dx2 = ycell_gd        ! in m
         end if
         cellarea = dx1 * dx2

C Get env var for erodible agriculture land fraction
         erode_agland = .false.         ! default
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
           
         call tfbelow ( jdate, jtime, uland, tfb )

         do r = 1, my_nrows
         do c = 1, my_ncols
            if ( tfb( c,r ) .lt. 0.0 .or. tfb( c,r ) .gt. 1.0 ) then
               xmsg = '*** ERROR in below-canopy top transport factor'
               call m3exit( pname, jdate, jtime, xmsg, xstat1 )
            end if

!           if ( metmod .eq. 'MM5' ) then
!              if ( sltyp( c,r ) .lt. 0.0 .or. sltyp( c,r ) .gt. 13.0 ) then
!                 write( xmsg,'("*** Soil type ", F6.1, ", Incorrect at: ", 2I4 )')
!    &                               sltyp( c,r ), c, r
!                 call m3warn( pname, jdate, jtime, xmsg )
!                 success = .false.; return
!              end if
!           end if

C Get soil type index (LWMASK = 0 for water)
            if ( lwmask( c,r ) .gt. 0.0 ) then
               j = nint( sltyp( c,r ) )
CCCCCCCC kludge (fixed in wrf-px after 4 Mar 11)
               if ( j .gt. 4 ) j = j + 1   ! PX combines "silt" with "silt loam"

C Calculate maximum amount of the adsorbed water
C    w` = 0.0014(%clay)**2 + 0.17(%clay)
C    Fecan et al. 1999 Annales Geophys.,17,144-157.
               wmax( c,r ) = ( 0.0014 * clayc( j ) + 0.17 ) * clayc( j )

C    log(F/G) = 0.134(%clayX100)-6
C    F/G (vertical to horizontal flux) ratio is a function of clay content 0~20%
C    Marticorena and Bergametti, 1995
               if ( clayc( j ) .le. 0.2 ) then
                  kvh( c,r ) = 10.0 ** ( 13.4 * clayc( j ) - 6.0 )
               else
                  kvh( c,r ) = 2.0e-4
               end if

C erodibility potential of soil component
               sd_ep( c,r ) = clayc( j ) * eropot( 1 )
     &                      + siltc( j ) * eropot( 2 )
     &                      + sandc( j ) * eropot( 3 )

            end if   ! over land

            if ( wmax ( c,r ) .lt. 0.0 .or. wmax ( c,r ) .gt. 1.0    .or.
     &           kvh  ( c,r ) .lt. 0.0 .or. kvh  ( c,r ) .gt. 5.0e-4 .or.
     &           sd_ep( c,r ) .lt. 0.0 .or. sd_ep( c,r ) .gt. 1.0 ) then
               xmsg = '*** ERROR in WMAX, KVH or SD_EP'
               call m3exit( pname, jdate, jtime, xmsg, xstat1 )
            end if

         end do   ! c
         end do   ! r

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

#ifdef verbose
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
         subroutine opdust_emis ( jdate, jtime, tstep, ndust_diag, wrdust_spc )

C   27 Dec 10 J.Young: initial

         use grid_conf           ! horizontal & vertical domain specifications
         use utilio_defn

         implicit none

         include SUBST_FILES_ID  ! file name parameters

C Arguments:
         integer      jdate      ! current model date, coded YYYYDDD
         integer      jtime      ! current model time, coded HHMMSS
         integer      tstep      ! output time step
         integer      ndust_diag
         character( 16 ) :: wrdust_spc( ndust_diag )

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

            nvars3d = ndust_diag
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
               vname3d( v ) = wrdust_spc( v )
               if ( vname3d( v )(1:4) .eq. 'ANUM' ) then
                  units3d( v ) = 'number/s'
               else if ( vname3d( v )(1:4) .eq. 'ASRF' ) then
                  units3d( v ) = 'm2/s'
               else
                  units3d( v ) = 'g/s'
               end if
               vdesc3d( v ) = 'hourly ' // trim( vname3d( v ) )
     &                     // ' windblown dust emission rate'
            end do

            fdesc3d( 1 ) = 'hourly layer-1 windblown dust emission rates'
            do l = 2, mxdesc3
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

         use grid_conf           ! horizontal & vertical domain specifications
         use utilio_defn
         use aero_data

C Arguments:
         integer, intent( in ) :: jdate      ! current model date, coded YYYYDDD
         integer, intent( in ) :: jtime      ! current model time, coded HHMMSS
         integer, intent( in ) :: tstep( 2 ) ! sync, output time step
         real,    intent( in ) :: rjacm( ncols,nrows ) ! reciprocal Jacobian [1/m]
         real,    intent( in ) :: cellhgt    ! grid-cell height [sigma]

C Includes:

         include SUBST_FILES_ID  ! file name parameters
         include SUBST_CONST     ! constants (for PI)

!        include SUBST_AE_EMIS   ! aerosol emission surrogate names and map table

C External Functions:
               
C Parameters:
         real zi                           ! Height of Internal Boundary Layer [m]
         real, parameter :: sc  = 32.0     ! SENSIT-based scaling parameter
         real, parameter :: z0s = 0.00001  ! Surface roughness for dust [m],
                                           ! Ref. Marticorena et al. JGR 1997

!        real, parameter :: amen = 2.8     ! values are from 0 to 3.5, mean 2.8
         real, parameter :: amen = 32.0    ! values adjusted based on Dale's MNORT measurements
         real, parameter :: radg = 122.87  ! g*s**2/m**4 122.87->120.8
!        real, parameter :: fdsa = 0.5     ! factor of dust source area 1/.1/.01
         real, parameter :: fdsa = 1.0     ! factor of dust source area 1/.1/.01
         real, parameter :: gmad = 0.78    ! Factor to convert geometric to aerodynamic diameter
         real, parameter :: cfac = amen * radg * fdsa * gmad  ! combined factor for qam

         real, parameter :: f6dpi = 6.0 / pi
         real, parameter :: gpkg = 1.0e03     ! g/kg

         character( 16 ) :: pname = 'GET_DUST_EMIS'
         character( 16 ) :: vname
         character( 96 ) :: xmsg
         integer c, r, j, m, v

         integer, save   :: wstep = 0      ! local write counter
         integer         :: mdate, mtime   ! diagnostic file write date&time

                       ! automatic arrays
         real       :: fmoit ( ncols,nrows )        ! factor of soil moisture on u*t
         real       :: rn    ( ncols,nrows )        ! preciptation nonconvective
         real       :: rc    ( ncols,nrows )        ! preciptation convective
         real       :: ustar ( ncols,nrows )        ! friction velocity [m/s]
         real       :: utstar( ncols,nrows,ndltyp+1 ) ! threshold U* [m/s]
         real       :: soimt ( ncols,nrows )        ! soil moisture (m**3/m**3)
         real       :: utem  ( ncols,nrows,ndltyp+1 ) ! U term = U*(U**2-Ut**2) [(m/s)**3]
         real       :: qam   ( ncols,nrows,ndltyp+1 ) ! emis for landuse type [g/m**/s]
         real       :: elus  ( ncols,nrows,ndltyp+1 ) ! Erodible landuse fraction (0~100)
         real       :: wind10( ncols,nrows )        ! wind speed at 10m altitude [m/s]
         real       :: zruf  ( ncols,nrows )        ! surface roughness length [m]
         real       :: tfa   ( ncols,nrows )        ! transport fraction above canopy
         real       :: wrbuf ( ncols,nrows )        ! diagnositc write buffer

         real       :: edust( n_mode )  ! mass emis rate [g/s] per mode (only accum & coarse)
         real       :: rlay1hgt         ! reciprocal of layer-1 height [1/m]
         real       :: m3j              ! 3rd moment accumulation (J) mode emis rates [m3/m3/s]
         real       :: m3k              ! 3rd moment coarse mode (K) emis rates [m3/m3/s]

C Soil moisture limit: 13 types and 3 variables, which are:
C    1 - saturation moisture limit,
C    2 - fill capacity, and   <- not used
C    3 - wilting point        <- not used
C Modified values compatiable with both MM5 & NAM.
C Silt values are based on NAM documentation on soil types.
C Other includes all types higher than 12. The values of Other, serving as
C placeholders, are randomly chosen.  Values of Other, however, have no effect
C on dust emissions as the threshold velocity of Other will be high.
         real :: soilml( nsltyp,3 ) = reshape (
     &           (/ 0.395,  0.135,  0.068,     ! Sand
     &              0.410,  0.150,  0.075,     ! Loamy Sand
     &              0.435,  0.195,  0.114,     ! Sandy Loam
     &              0.485,  0.255,  0.179,     ! Silt Loam
     &              0.476,  0.361,  0.084,     ! Silt
     &              0.451,  0.240,  0.155,     ! Loam
     &              0.420,  0.255,  0.175,     ! Sandy Clay Loam
     &              0.477,  0.322,  0.218,     ! Silty Clay Loam
     &              0.476,  0.325,  0.250,     ! Clay Loam
     &              0.426,  0.310,  0.219,     ! Sandy Clay
     &              0.482,  0.370,  0.283,     ! Silty Clay
     &              0.482,  0.367,  0.286,     ! Clay
     &              0.482,  0.367,  0.286 /),  ! Other
     &           (/ nsltyp,3 /), order = (/ 2,1 /) )

C Threshold Friction Velocity [m/s] for 13 soil types and 3 land use types, which are:
C    1 -shrubland,
C    2 -shrubgrass land, and
C    3 -barren land
C Local (surface) Threshold Friction Velocity from field measurements
C   Ref. Gillette et al., JGR, 1980 for desert soils (J=2).
C   Ref. Gillette, JGR, 1988 for Loose Agr. Soils (J=3).
C The following soil types were not measured for desert land (we chose to use
C agr. data): Sandy Clay Loam, Clay Loam, Sandy Clay, and Silty Clay.
C Modified values compatiable with both MM5 & NAM.
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
     &           (/ nsltyp,3 /), order = (/ 2,1 /) )

         real :: dmsk( 4 ) = ! land use type desert fraction
     &           (/ 0.50,    ! shrubland
     &              0.25,    ! shrubgrass
     &              0.75,    ! barrenland
     &              0.75 /)  ! cropland

         interface
            subroutine tfabove ( ustar, swind, uland, tfa )
            use hgrd_defn
            implicit none
            real,    intent( in )  :: ustar( :,: )
            real,    intent( in )  :: swind( :,: )
            real,    intent( in )  :: uland( :,:,: )
            real,    intent( out ) :: tfa( :,: )
            end subroutine tfabove
         end interface

#ifdef verbose
         integer dryhit
         integer dusthit
#endif

C-----------------------------------------------------------------------

         vname = 'USTAR'
         if ( .not. interpx( met_cro_2d, vname, pname,
     &                       strtcolmc2,endcolmc2, strtrowmc2,endrowmc2, 1,1,
     &                       jdate, jtime, ustar ) ) then
            xmsg = 'Could not read '// vname // ' from MET_CRO_2D '
            call m3exit( pname, jdate, jtime, xmsg, xstat1 )
         end if

         vname = 'ZRUF'
         if ( .not. interpx( met_cro_2d, vname, pname,
     &                       strtcolmc2,endcolmc2, strtrowmc2,endrowmc2, 1,1,
     &                       jdate, jtime, zruf ) ) then
            XMSG = 'Could not read '// vname // ' from MET_CRO_2D '
            call m3exit( pname, jdate, jtime, xmsg, xstat1 )
         end if

C Convert layer-average U* into surface local U*
C    -> U*t has to be surface values from measurements.

         do r = 1, my_nrows
         do c = 1, my_ncols
!           ustar( c,r ) = ustar( c,r ) * log( zi ) / log( zi * zruf( c,r ) / z0s )
            zi = sc * zruf( c,r )
            ustar( c,r ) = ustar( c,r ) * log( zi / zruf( c,r ) ) / log( zi / z0s )
         end do
         end do

         vname = 'WSPD10'
         if ( .not. interpx( met_cro_2d, vname, pname,
     &                       strtcolmc2,endcolmc2, strtrowmc2,endrowmc2, 1,1,
     &                       jdate, jtime, wind10 ) ) then
            xmsg = 'Could not read '// trim( vname ) // ' from MET_CRO_2D '
            call m3exit( pname, jdate, jtime, xmsg, xstat1 )
         end if

C calculate transport factor above the canopy
         call tfabove ( ustar, wind10, uland, tfa )

         vname = 'SOIM1'
!        vname = 'SOILW'   ??? 'NAM' ???
         if ( .not. interpx( met_cro_2d, vname, pname,
     &                       strtcolmc2,endcolmc2, strtrowmc2,endrowmc2, 1,1,
     &                       jdate, jtime, soimt ) ) then
            xmsg = 'Could not read '// trim( vname ) // ' from MET_CRO_2D '
            call m3exit( pname, jdate, jtime, xmsg, xstat1 )
         end if

         vname = 'RN'
         if ( .not. interpx( met_cro_2d, vname, pname,
     &                       strtcolmc2,endcolmc2, strtrowmc2,endrowmc2, 1,1,
     &                       jdate, jtime, rn ) ) then
            xmsg = 'Could not read '// trim( vname ) // ' from MET_CRO_2D '
            call m3exit( pname, jdate, jtime, xmsg, xstat1 )
         end if

         vname = 'RC'
         if ( .not. interpx( met_cro_2d, vname, pname,
     &                       strtcolmc2,endcolmc2, strtrowmc2,endrowmc2, 1,1,
     &                       jdate, jtime, rc ) ) then
            xmsg = 'Could not read '// trim( vname ) // ' from MET_CRO_2D '
            call m3exit( pname, jdate, jtime, xmsg, xstat1 )
         end if

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

#ifdef verbose
         dryhit = 0
         dusthit = 0
#endif

C Initialize windblown dust diagnostics output buffer
         if ( dustem_diag .and. wstep .eq. 0 ) then
            dustbf = 0.0   ! array assignment
#ifdef verbose
            sdiagv = 0.0   ! array assignment
#endif
         end if

         do r = 1, my_nrows
         do c = 1, my_ncols
            dust_em( c,r ) = 0.0
            fmoit( c,r ) = 999.9
            do m = 1, 4
               utstar( c,r,m ) = 999.9
               qam   ( c,r,m ) = 0.0
               elus  ( c,r,m ) = 0.0
               utem  ( c,r,m ) = 0.0
            end do
            rlay1hgt = rjacm ( c,r ) / cellhgt

c Dust possiblity if rain < 1/100 in. (1 in. = 2.540 cm) and not over water (SLTYP = 0)
            if ( ( ( rn( c,r ) + rc( c,r ) ) .le. 0.0254 ) .and. ! rn, rc = [cm]
     &           ( sltyp( c,r ) .gt. 0.0 ) ) then

               j = nint( sltyp( c,r ) )
               if ( j .gt. 13 ) j = 13

C soil moisture effect on U*t
               if ( soimt( c,r ) .le. wmax( c,r ) ) then
                  fmoit( c,r ) = 1.0
               else if ( soimt( c,r ) .le. soilml( j,1 ) ) then
                  fmoit( c,r ) = sqrt( 1.0 + 1.21 * ( soimt( c,r ) - wmax( c,r ) )**0.68 )
               end if

C Dust occurs where erodible land exists, soil moisture is less than saturation,
C and U* > U*t

               if ( soimt( c,r ) .le. soilml( j,1 ) ) then

#ifdef verbose
                  dryhit = dryhit + 1
#endif

C Adjust U*t based on soil moisture ---
                  do m = 1, 3   ! landuse type
                     elus( c,r,m ) = ladut( c,r,m ) * dmsk( m )  ! desert land [%]
                  end do
                  elus( c,r,4 ) = agland( c,r ) * dmsk( 4 )      ! crop land [%]

                  do m = 1, 4   ! landuse type

                     if ( elus( c,r,m ) .gt. 100.0 .or. elus( c,r,m ) .lt. 0.0 ) then
                        write( xmsg,2009 ) elus( c,r,m ), c, r, m
                        call m3exit( pname, jdate, jtime, xmsg, xstat1 )
                     end if

                     if ( elus( c,r,m ) .gt. 0.0 ) then
                        utstar( c,r,m ) = uthfv( j,3 ) * fmoit( c,r )
                        if ( ustar( c,r ) .gt. utstar( c,r,m ) ) then
                           ! wind erosion occurs only when U* > U*t
                           utem( c,r,m ) = ustar( c,r )
     &                                   * ( ustar( c,r ) * ustar( c,r )
     &                                   -   utstar( c,r,m ) * utstar( c,r,m ) )
                           qam( c,r,m ) = cfac * kvh( c,r ) * utem( c,r,m ) * sd_ep( c,r )
                                                                          ! [g/m**2/s]
                           qam( c,r,m ) = qam( c,r,m ) * rlay1hgt
     &                                  * ( elus( c,r,m ) * 0.01 )        ! [g/m**3/s]
                        end if
                     end if

                     if ( elus( c,r,m ) .eq. 0.0 .and. qam( c,r,m ) .ne. 0.0 ) then
                        xmsg = 'ERROR: erodible land use = 0, but emissions .ne. 0'
                        call m3exit( pname, jdate, jtime, xmsg, xstat1 )
                     end if

                     dust_em( c,r ) = dust_em( c,r ) + qam( c,r,m )

                  end do   ! m landuse

C Dust removal by surrounding vegetation <-???
C Adjust dust emissions for transport factors

                  dust_em( c,r ) = dust_em( c,r ) * tfa( c,r ) * tfb( c,r )

               end if   ! if soil moisture
            end if   ! if rain & land

         end do   ! c
         end do   ! r

#ifdef verbose
         write( logdev,'( /5x, a, 2i8 / )' ) 'dry hit count, out of total:',
     &                                       dryhit, (c-1)*(r-1)
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

C Mode-specific dry surface area emission rates [m2/s].
C 2nd moment multiplied by PI to obtain the surface area emissions rate.
            dustouts( 1,c,r ) = 0.0
            dustouts( 2,c,r ) = m3j * factsrfj
            dustouts( 3,c,r ) = m3k * factsrfk

#ifdef verbose
            if ( m3j .ne. 0.0 ) dusthit = dusthit + 1
#endif
   
            if ( dustem_diag ) then
               diagv(  1 ) = qam( c,r,1 )            ! g/m**3/s
               diagv(  2 ) = qam( c,r,2 )            ! g/m**3/s
               diagv(  3 ) = qam( c,r,3 )            ! g/m**3/s
               diagv(  4 ) = qam( c,r,4 )            ! g/m**3/s
               diagv(  5 ) = dust_em( c,r )          ! g/m**3/s
               diagv(  6 ) = elus( c,r,1 )           ! shrub
               diagv(  7 ) = elus( c,r,2 )           ! shrubgrass
               diagv(  8 ) = elus( c,r,3 )           ! barren
               diagv(  9 ) = elus( c,r,4 )           ! cropland
               diagv( 10 ) = ladut( c,r,1 ) + ladut( c,r,2 )
     &                     + ladut( c,r,3 ) + agland( c,r )
               diagv( 11 ) = tfa ( c,r )
               diagv( 12 ) = tfb ( c,r )
               diagv( 13 ) = fmoit( c,r )
               diagv( 14 ) = sd_ep( c,r )
               diagv( 15 ) = kvh  ( c,r )
               diagv( 16 ) = wmax ( c,r )
               diagv( 17 ) = uland( c,r,1 )
               diagv( 18 ) = uland( c,r,2 )
               diagv( 19 ) = uland( c,r,3 )
               diagv( 20 ) = uland( c,r,4 )
               diagv( 21 ) = ustar( c,r )

               do v = 1, 4
                  if ( utstar( c,r,v ) .ne. 999.9 ) then
                     diagv( 21+v ) = utstar( c,r,v )
                  else
                     diagv( 21+v ) = 0.0
                  end if
               end do   ! diagv(25)

               do v = 1, ndust_spc-1
                  diagv( 25+v ) = dustoutm( v,2,c,r )
               end do   ! diagv(25+18) = diagv(43)

               do v = 1, 3
                  diagv( 43+v ) = dustoutm( v,3,c,r )
               end do   ! diagv(43+3) = diagv(46)
               diagv( 47 ) = dustoutm( ndust_spc,3,c,r ) ! 'ASOIL'

               diagv( 48 ) = dustoutn( 2,c,r )
               diagv( 49 ) = dustoutn( 3,c,r )
               diagv( 50 ) = dustouts( 2,c,r )
               diagv( 51 ) = dustouts( 3,c,r )

!              if ( stdevj( c,r ) .ne. 0.0 ) then
!                 diagv( 52 ) = stdevj( c,r )
!                 diagv( 53 ) = stdevk( c,r )
!              end if

               do v = 1, ndust_diag
                  dustbf( v,c,r ) = dustbf( v,c,r ) + diagv( v )
     &                                              * float( time2sec ( tstep( 2 ) ) )
#ifdef verbose
                  sdiagv( v ) = sdiagv( v ) + diagv( v )
     &                                      * float( time2sec ( tstep( 2 ) ) )
#endif
               end do
            end if   ! dustem_diag
         end do   ! col
         end do   ! row

#ifdef verbose
         write( logdev,'( /5x, a, 2i8 / )' ) 'dust hit count, out of total:',
     &                                                dusthit, (c-1)*(r-1)
#endif

         if ( dustem_diag ) then
C If last call this hour, write out the windblown dust emissions dignostics.
C Then reset the sea-salt emissions array and local write counter.

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
                  if ( .not. WRITE3( ctm_dust_emis_1, diagnm( v ),
     &                       mdate, mtime, wrbuf ) ) then
                     xmsg = 'Could not write ' // trim( diagnm( v ) )
     &                    // ' to CTM_DUST_EMIS_1'
                     call m3exit( pname, mdate, mtime, xmsg, xstat1 )
                  end if
               end do
               write( logdev, '( /5x, 2( a, 1x ), i8, ":", i6.6 )' )
     &               'Timestep written to CTM_DUST_EMIS_1',
     &               'for date and time', mdate, mtime
#ifdef verbose
               write( logdev,2015 ) jdate, jtime
               do v = 1, ndust_diag
                  if ( diagnm( v )(1:4) .ne. 'ANUM' ) then
                     write( logdev,2019 ) v, diagnm( v ), sdiagv( v ) / float( wstep )
                  else
                     write( logdev,2023 ) v, diagnm( v ), sdiagv( v ) / float( wstep )
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
2019     format( i10, 1x, a6, f20.5 )
2023     format( i10, 1x, a6, e20.3 )

         end subroutine get_dust_emis

c-----------------------------------------------------------------------

      end module dust_emis

