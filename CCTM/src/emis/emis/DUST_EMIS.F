
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
C  8 Jul 11 J.Young: unified string lengths in character lists for compiler compatibility
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
C 15 Sep 15 H.Foroutan: revised threshold friction velocity parameterization
C 20 Oct 15 H.Foroutan: Updated the calculation of the threshold velocity(U*t), which is
C                       now based on dust particle size, following Shao and Lu [JGR,2000].
C                       Implemented a dynamic vegetation fraction based on the MODIS FPAR.
C                       Introduced a new parametrization for surface roughness (z0)
C                       applicable to dust emission schemes, and accordingly calculated
C                       the friction velocity (U*) at the surface using 10m wind speed
C                       and the new (microspcopic) surface roughness.
C                       Surface roughness adjusted for estimated annual vegetation height.
C                       Included drag partitioning coefficient. Updated the calculation of
C                       the vertical-to-horizontal flux based on Lu and Shao [JGR,1999].
C                       Updated the dust diag output file accordingly.
C  8 Jan 16 J.Young: Changes for computational efficiency
C  2 Feb 16 J.Young: move dust aero speciation table to AERO_DATA
C-----------------------------------------------------------------------
      use lus_defn
      use aero_data

      implicit none 

C windblown dust emissions rates
      real, allocatable, save :: dustoutm( :,:,:,: )  ! mass emission rates [g/m**3/s]
      real, allocatable, save :: dustoutn( :,:,: )    ! number emission rates [1/m**3/s]
      real, allocatable, save :: dustouts( :,:,: )    ! surface-area emisrates [m2/m**3/s]

      public ndust_spc, dustoutm, dustoutn, dustouts, dust_spc,
     &       dust_emis_init, get_dust_emis
      private

      real, allocatable, save :: dust_em( :,: )  ! total dust emissions [g/m**3/s]

C updated values of mass fraction for "freshly emitted dust"
C based on Kok [PNAS, 2011] and Nabat et al. [ACP, 2012]
      real, parameter :: fracmj = 0.07  ! mass fraction assigned to accum mode
      real, parameter :: fracmk = 0.93  ! mass fraction assigned to coarse mode

C diam`s from fracmj,fracmk-weighted 2 2-bin averages of geom means
C 2 J-mode bins: 0.1-1.0, 1.0-2.5 um
C 2 K-mode bins: 2.5-5.0, 5.0-10.0 um
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
      logical, save      :: dustem_diag      ! flag for dustemis diagnostic file
      integer, parameter :: fndust_diag = 19 ! number of fixed diagnostic output vars
      integer, save      :: ndust_diag       ! number of diagnostic output vars
      real, allocatable, save :: diagv( : )  ! diagnostic output variables
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
      type( diag_type ), allocatable, save :: vdiagnm_ustar( : )
      type( diag_type ), allocatable, save :: vdiagnm_kvh( : )
      type( diag_type ), allocatable, save :: vdiagnm_rough( : )

      character( 10 ) :: truncnm
      character( 16 ) :: vnm

      type( diag_type ), parameter :: fdiagnm( fndust_diag ) = (/
C                      var              units                 desc
C                 ----------------    --------    -------------------------------------------
     & diag_type( 'Cropland_Emis   ', 'g/m**3/s', 'emissions for cropland landuse type        '),
     & diag_type( 'Desertland_Emis ', 'g/m**3/s', 'total emis for desert types and cropland   '),
     & diag_type( 'Cropland_Frac   ', 'percent ', 'cropland erodible landuse fraction (%)     '),
     & diag_type( 'Desertland_Frac ', 'percent ', 'total desert fraction (%)                  '),
     & diag_type( 'Cropland_Ustar  ', 'm/s     ', 'u* for cropland                            '),
     & diag_type( 'Cropland_kvh    ', '1/m     ', 'cropland vert to horiz flux ratio          '),
     & diag_type( 'Cropland_Rough  ', '        ', 'cropland surface roughness factor          '),
     & diag_type( 'Soil_Moist_Fac  ', '        ', 'soil moisture factor for threshold u*      '),
     & diag_type( 'Soil_Erode_Pot  ', '        ', 'soil -> dust erodiblity potential          '),
     & diag_type( 'Mx_Adsrb_H2O_Frc', '        ', 'max adsorbed water fraction                '),
     & diag_type( 'Vegetation_Frac ', '        ', 'vegetation land coverage                   '),
     & diag_type( 'Urban_Cover     ', 'percent ', 'urban land coverage                        '),
     & diag_type( 'Forest_Cover    ', 'percent ', 'forest land coverage                       '),
     & diag_type( 'Trfac_Above_Can ', '        ', 'transport factor above canopy              '),
     & diag_type( 'Trfac_Inside_Can', '        ', 'transport factor in and below canopy       '),
     & diag_type( 'ANUMJ           ', '#/s     ', 'accumulation mode number                   '),
     & diag_type( 'ANUMK           ', '#/s     ', 'coarse mode number                         '),
     & diag_type( 'ASRFJ           ', 'm**2/s  ', 'accumulation mode surface area             '),
     & diag_type( 'ASRFK           ', 'm**2/s  ', 'coarse mode surface area                   ')/)
  
C Module shared variables:
      real, allocatable, save :: agland( :,: )   ! agriculture land fraction
      real, allocatable, save :: wmax  ( :,: )   ! max adsorb water percent
      real, allocatable, save :: kvh   ( :,:,: ) ! ratio of vertical flux / horizontal (k factor)
      real, allocatable, save :: sd_ep ( :,: )   ! soil->dust erodiblity potential
      real, allocatable, save :: tfb   ( :,: )   ! transport fraction in and below canopy
      real, allocatable, save :: fpar  ( :,: )   ! modis fpar

      integer, save :: sdate, stime  ! scenario start date & time

      real :: eropot( 3 ) =     ! erodible potential of soil components
     &        (/ 0.08,   ! clay
     &           1.00,   ! silt
     &           0.12 /) ! sand

      integer, save :: logdev

      CONTAINS

C=======================================================================
         function dust_emis_init( jdate, jtime, tstep ) result( success )

C Revision History.
C   Aug 12, 15 D. Wong: Replaced MYPE with IO_PE_INCLUSIVE for parallel I/O
C   implementation

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
         character( 16 ) :: modis_fpar_1 = 'MODIS_FPAR'
                            ! Fraction of Absorbed Photosynthetically Active Radiation

         logical :: erode_agland = .true. ! default
         integer status
         integer c, r, i, j, k, l, n
         integer idiag
         integer n_mass_emissions

         integer  gxoff, gyoff                 ! global origin offset from file
         integer, save :: strtcol, endcol, strtrow, endrow
         integer  jdatemod

         type( diag_type ), allocatable :: diagnm_swap( : )

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
            xmsg = '*** Failure allocating DUSTOUTM, DUSTOUTN, or DUSTOUTS'
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
     &             sd_ep ( ncols,nrows ),
     &             fpar  ( ncols,nrows ),
     &             tfb   ( ncols,nrows ), stat = status )
         if ( status .ne. 0 ) then
            xmsg = '*** Failure allocating AGLAND, WMAX, FPAR, SD_EP, or TFB'
            call m3warn( pname, jdate, jtime, xmsg )
            success = .false.; return
         end if
         agland = 0.0   ! array assignment
         wmax   = 0.0   ! array assignment
         sd_ep  = 0.0   ! array assignment
         fpar   = 0.0   ! array assignment

C Open MODIS file to get vegetation fraction
         if ( .not. open3( modis_fpar_1, fsread3, pname ) ) then
            xmsg = 'Could not open ' // modis_fpar_1
            call m3exit( pname, jdate, jtime, xmsg, xstat1 )
         end if

C Get the file description 
         if ( .not. desc3( modis_fpar_1 ) ) then
            xmsg = 'Could not get '
     &      // trim( modis_fpar_1 )
     &      // ' file description'
            call m3exit( pname, jdate, jtime, xmsg, xstat1 )
         end if

C To be able to use either climatological (2001-2010 averaged) or
C current fpar value. The year for the climatological fpar is 2005 in
C the input file.
         if ( sdate3d .eq. 2005001 ) then          ! climatological
            jdatemod = 2005000 + mod( jdate,1000 )
         else                                      ! current
            jdatemod = jdate
         end if

C Get domain decomp info
         call subhfile ( modis_fpar_1, gxoff, gyoff,
     &                   strtcol, endcol, strtrow, endrow )

C Read in FPAR from MODIS file
         xmsg = 'Could not read FPAR from ' // trim( modis_fpar_1 )
         if ( .not. xtract3( modis_fpar_1, 'MODIS_FPAR_T', 1,1,
     &                       strtrow,endrow,strtcol,endcol,
     &                       jdatemod, jtime, fpar( 1,1 ) ) )
     &   call m3exit ( pname, jdate, jtime, xmsg, xstat1 )

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
            allocate( vdiagnm_emis ( n_dlcat ),
     &                vdiagnm_frac ( n_dlcat ),
     &                vdiagnm_kvh  ( n_dlcat ),
     &                vdiagnm_rough( n_dlcat ),
     &                vdiagnm_ustar( n_dlcat ), stat = status )
            if ( status .ne. 0 ) then
               xmsg = '*** Failure allocating VDIAGNM_*'
               call m3warn( pname, jdate, jtime, xmsg )
               success = .false.; return
            end if
            vdiagnm_emis  = diag_type( ' ', ' ', ' ' )  ! array assignment
            vdiagnm_frac  = diag_type( ' ', ' ', ' ' )  ! array assignment
            vdiagnm_ustar = diag_type( ' ', ' ', ' ' )  ! array assignment
            vdiagnm_kvh   = diag_type( ' ', ' ', ' ' )  ! array assignment
            vdiagnm_rough = diag_type( ' ', ' ', ' ' )  ! array assignment

C...Count the number of mass emissions species
            n_mass_emissions = 0
            do i = 1, ndust_spc
               do j = 1, n_mode
                  if( len_trim( dust_spc( i )%name( j ) ) .lt. 1 )cycle
                  n_mass_emissions = n_mass_emissions + 1  
               end do
            end do
            
            ndust_diag = fndust_diag + 5 * n_dlcat + n_mass_emissions 

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
               vnm = trim( truncnm ) // '_Ustr'    ! char( 16 )
               vdiagnm_ustar( i ) = diag_type( vnm, 'm/s', vnmld( i )%desc )
               vnm = trim( truncnm ) // '_Kvh'    ! char( 16 )
               vdiagnm_kvh( i ) = diag_type( vnm, '1/m', vnmld( i )%desc )
               vnm = trim( truncnm ) // '_Rough'    ! char( 16 )
               vdiagnm_rough( i ) = diag_type( vnm, '   ', vnmld( i )%desc )
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
            n = n_dlcat + 1
            diagnm( n ) = fdiagnm( 1 )    ! Cropland_Emis
            n = n + 1
            diagnm( n ) = fdiagnm( 2 )    ! Desertland_Emis

            do i = 1, n_dlcat
               diagnm( i+n ) = vdiagnm_frac( i )
            end do
            n = n + n_dlcat + 1
            diagnm( n ) = fdiagnm( 3 )    ! Cropland_Frac
            n = n + 1
            diagnm( n ) = fdiagnm( 4 )    ! Desertland_Frac

            do i = 1, n_dlcat
               diagnm( i+n ) = vdiagnm_ustar( i )
            end do
            n = n + n_dlcat + 1
            diagnm( n ) = fdiagnm( 5 )    ! Cropland_Ustar

            do i = 1, n_dlcat
               diagnm( i+n ) = vdiagnm_kvh( i )
            end do
            n = n + n_dlcat + 1
            diagnm( n ) = fdiagnm( 6 )    ! Cropland_Kvh

            do i = 1, n_dlcat
               diagnm( i+n ) = vdiagnm_rough( i )
            end do
            n = n + n_dlcat + 1
            diagnm( n ) = fdiagnm( 7 )    ! Cropland_Rough

            n = n - 7                     ! add remaining variables in fdiagnm
            do i = 8, fndust_diag 
               idiag = i+n
               diagnm( idiag ) = fdiagnm( i )
            end do

C...append diagnostic variables with mass emissions species
            do j = 2, n_mode
               do i = 1, ndust_spc
                  n = len_trim( dust_spc( i )%name( j ) )
                  if( n .lt. 1  )cycle ! assumes cmaq species names atleast one character long
                  n = 0
                  do k = 1, idiag   ! determine if dust emissions is already added to diagnostic output
                     if( dust_spc( i )%name( j ) .Eq. diagnm( k )%var )Then
                         n = k
                         exit
                     end if
                  end do
                  if( n .gt. 0  )then ! skip already added 
                      cycle
                  else 
                      idiag = idiag + 1
                      diagnm( idiag )%var = dust_spc( i )%name( j )
                  end if
                  diagnm( idiag )%units = 'g/m**3/s'
                  Select Case( j ) ! assumes only two aerosol modes dust emissions
!                    Case( 1 )
!                       diagnm( idiag )%desc = 'aitken mode' 
                    Case( 2 )
                       diagnm( idiag )%desc = 'accumulation mode' 
                    Case( 3 )
                       diagnm( idiag )%desc = 'coarse mode' 
!                    Case Default 
!                       diagnm( idiag )%des = 'Undefined mode ' 
                    end Select
                    diagnm( idiag )%desc = Trim( diagnm( idiag )%desc )
     &                                   // ' emissions for '  
     &                                   // Trim( dust_spc( i )%description )
               end do
            end do
            
! remove unused space in diagnm by deallocated and reallocating to idiag value
            allocate( diagnm_swap( ndust_diag ), stat = status )
            if ( status .ne. 0 ) then
               xmsg = '*** Failure allocating DIAGNM_SWAP'
               call m3warn( pname, jdate, jtime, xmsg )
               success = .false.; return
            end if
            diagnm_swap = diagnm

            deallocate( diagnm )

            ndust_diag = idiag
            allocate( diagnm( ndust_diag ), stat = status )
            if ( status .ne. 0 ) then
               xmsg = '*** Failure reallocating DIAGNM'
               call m3warn( pname, jdate, jtime, xmsg )
               success = .false.; return
            end if
            diagnm( 1:ndust_diag ) = diagnm_swap( 1:ndust_diag )
            deallocate( diagnm_swap )

            sdate = envint( 'CTM_STDATE', 'Scenario Start (YYYYJJJ)', 0, status )
            stime = envint( 'CTM_STTIME', 'Scenario Start (HHMMSS)', 0, status )

            if ( io_pe_inclusive )
     &         call opdust_emis ( sdate, stime, tstep, ndust_diag, diagnm )

         end if   ! dustem_diag

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
         call tfbelow ( jdate, jtime, tfb )

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
               idx = findAero( dust_spc( i )%name( n ), .true. )
               if( aerospc( idx )%tracer )cycle
               if( dust_spc( i )%spcfac( n ) .lt. 1.0e-30 )cycle
               sumsplit = sumsplit + dust_spc( i )%spcfac( n ) ! should = 1.0
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
         integer, intent( in ) :: jdate      ! current model date, coded YYYYDDD
         integer, intent( in ) :: jtime      ! current model time, coded HHMMSS
         integer, intent( in ) :: tstep      ! output time step
         integer, intent( in ) :: ndust_var
         type( diag_type ), intent( in ) :: dust_var( : )

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
         integer, parameter :: ndp = 4     ! number of soil texture type particle sizes:
                                           !  1  Coarse sand
                                           !  2  Fine-medium sand
                                           !  3  Silt
                                           !  4  Clay

         real, parameter :: f6dpi = 6.0 / pi
         real, parameter :: gpkg = 1.0e03     ! g/kg

         real, parameter :: mv    = 0.16
         real, parameter :: sigv  = 1.45
         real, parameter :: betav = 202.0
         real, parameter :: sigv_mv  = sigv * mv   ! = 0.232
         real, parameter :: betav_mv = betav * mv  ! = 32.32
         real, parameter :: mb    = 0.5
         real, parameter :: sigb  = 1.0
         real, parameter :: betab = 90.0
         real, parameter :: sigb_mb  = sigb * mb   ! = 0.5
         real, parameter :: betab_mb = betab * mb  ! = 45.0

         character( 16 ) :: pname = 'GET_DUST_EMIS'
         character( 16 ) :: vname
         character( 96 ) :: xmsg
         integer status
         integer c, r, j, m, n, v

         integer, save   :: wstep = 0      ! local write counter
         integer         :: mdate, mtime   ! diagnostic file write date&time

                       ! automatic arrays
         real       :: fmoit  ( ncols,nrows )      ! factor of soil moisture on u*t
         real       :: soimt  ( ncols,nrows )      ! gravimetric soil moisture (Kg/Kg)
         real       :: tfa    ( ncols,nrows )      ! transport fraction above canopy
         real       :: wrbuf  ( ncols,nrows )      ! diagnositc write buffer
         real       :: vegfrac( ncols,nrows )      ! vegetation fraction
         real       :: vegfree                     ! 1.0 - vegfrac for this col, row
         real       :: lai    ( ncols,nrows )      ! leaf area index

         real, allocatable, save :: ustr  ( :,:,: ) ! U* [m/s]
         real, allocatable, save :: qam   ( :,:,: ) ! emis for landuse type [g/m**2/s]
         real, allocatable, save :: elus  ( :,:,: ) ! erodible landuse percent (0~100)
         real, allocatable, save :: fruf  ( :,:,: ) ! surface roughness factor

         real       :: edust( n_mode )  ! mass emis rate [g/s] per mode (only accum & coarse)
         real       :: sumdfr           ! sum var for desert fraction
         real       :: rlay1hgt         ! reciprocal of layer-1 height [1/m]
         real       :: m3j              ! 3rd moment accumulation (J) mode emis rates [m3/m3/s]
         real       :: m3k              ! 3rd moment coarse mode (K) emis rates [m3/m3/s]
         real       :: fruf2            ! surface roughness factor squared

         character( 16 ), save :: rc_name, rn_name    ! new names: RC -> RCA, RN -> RNA
         logical, save :: firstime = .true.

         real       :: lambda, vegheight
         real       :: z0
         real       :: lambdav           ! vegetation roughness density - Shao et. al [Aus. J. Soil Res., 1996]
         real       :: flxfac1, flxfac2  ! combined soli type mapping factors
         real       :: hflux, vflux      ! horizontal and vertical dust flux
         real       :: jday
         integer    :: emap( n_dlcat+1 )

C---Height for veg elements
         real :: hv( 4 )

C---Vegetation fraction for 4 land types  ! not used
C from Federal Geographic Data Committee [1997]
C Note: All other landuse types are mapped into these 4 types.
!        real :: vegfra( 4 ) =
!    &           (/ 0.11,     ! shrubland
!    &              0.17,     ! shrubgrass
!    &              0.01,     ! barrenland
!    &              0.30 /)   ! cropland

C---Height for solid elements
!        real :: hb( 4 ) =
!    &           (/ 0.01,     ! shrubland
!    &              0.02,     ! shrubgrass
!    &              0.005,    ! barrenland
!    &              0.02 /)   ! cropland

C---Roughness density for solid elements
C from Darmenova et al. [JGR,2009] and Xi and Sokolik [JGR,2015]
        real :: lambdab( 4 ) =
     &           (/ 0.03,     ! shrubland
     &              0.04,     ! shrubgrass
     &              0.0001,   ! barrenland
     &              0.15 /)   ! cropland

C---Compound for computational efficiency
         real :: hb_lambdab( 4 ) =
     &           (/ 6.0e-04,   ! shrubland
     &              8.0e-04,   ! shrubgrass
     &              2.0e-06,   ! barrenland
     &              3.0e-03 /) ! cropland

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

C converted to gravimetric [kg/kg]
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
!! ALSO in Spyrou, et al. [JGR,2010]
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

C---Soil texture: the amount of
C   1: Coarse sand, 2: Fine-medium sand, 3: Silt, 4: Clay
C   in each soil type [Kg/Kg]. from Menut et al. [JGR,2013]
         real :: soiltxt( nsltyp,ndp ) = reshape (
     &           (/ 0.46,   0.46,   0.05,   0.03,     ! Sand
     &              0.41,   0.41,   0.18,   0.00,     ! Loamy Sand
     &              0.29,   0.29,   0.32,   0.10,     ! Sandy Loam
     &              0.00,   0.17,   0.70,   0.13,     ! Silt Loam
     &              0.00,   0.10,   0.85,   0.05,     ! Silt
     &              0.00,   0.43,   0.39,   0.18,     ! Loam
     &              0.29,   0.29,   0.15,   0.27,     ! Sandy Clay Loam
     &              0.00,   0.10,   0.56,   0.34,     ! Silty Clay Loam
     &              0.00,   0.32,   0.34,   0.34,     ! Clay Loam
     &              0.00,   0.52,   0.06,   0.42,     ! Sandy Clay
     &              0.00,   0.06,   0.47,   0.47,     ! Silty Clay
     &              0.00,   0.22,   0.20,   0.58,     ! Clay
     &              0.00,   0.00,   0.00,   0.00 /),  ! Other
     &           (/ nsltyp,4 /), order = (/ 2,1 /) ) ! fill columns first

C---Mean mass median particle diameter (m) for each soil texture type
C   Chatenet et al. [Sedimentology,1996] and Menut et al. [JGR,2013]
         real :: dp( ndp ) =
     &           (/ 690.0E-6,     ! Coarse sand
     &              210.0E-6,     ! Fine-medium sand
     &              125.0E-6,     ! Silt
     &                2.0E-6 /)   ! Clay

         interface
            subroutine tfabove ( tfa )
               real,    intent( out ) :: tfa( :,: )
            end subroutine tfabove
         end interface

#ifdef verbose_wbdust
         integer dryhit
         integer dusthit
#endif

C-----------------------------------------------------------------------

         if ( firstime ) then
            firstime = .false.
            allocate ( ustr( ncols,nrows,n_dlcat+1 ),
     &                  qam( ncols,nrows,n_dlcat+1 ),
     &                 fruf( ncols,nrows,n_dlcat+1 ),
     &                  kvh( ncols,nrows,n_dlcat+1 ),
     &                 elus( ncols,nrows,n_dlcat+1 ), stat = status )
            if ( status .ne. 0 ) then
               xmsg = '*** Failure allocating USTR, QAM, FRUF, KVH, or ELUS'
               call m3exit( pname, jdate, jtime, xmsg, xstat1 )
            end if
         end if

C---Calculate transport factor above the canopy
         call tfabove ( tfa )

C---Get Julian day number in year
         jday = float( mod( jdate,1000 ) )

C---Vegetation height dynamically changed based on the month of the year
C   Veg. heights in [m] for 1: Shrubland 2: shrubgrass 3: barrenland 4: Cropland
C   following the idea of Xi and Sokolik [JGR,2015]
         if ( jday .gt. 59 .and. jday .le. 90 ) then         ! Mar
            hv = (/ 0.15 , 0.05 , 0.10 , 0.05 /)
         else if ( jday .gt. 90 .and. jday .le. 120 ) then   ! Apr
            hv = (/ 0.15 , 0.10 , 0.10 , 0.05 /)
         else if ( jday .gt. 120 .and. jday .le. 151 ) then  ! May
            hv = (/ 0.12 , 0.20 , 0.10 , 0.10 /)
         else if ( jday .gt. 151 .and. jday .le. 181 ) then  ! Jun
            hv = (/ 0.12 , 0.15 , 0.10 , 0.30 /)
         else if ( jday .gt. 181 .and. jday .le. 212 ) then  ! Jul
            hv = (/ 0.10 , 0.12 , 0.10 , 0.50 /)
         else if ( jday .gt. 212 .and. jday .le. 243 ) then  ! Aug
            hv = (/ 0.10 , 0.12 , 0.10 , 0.50 /)
         else if ( jday .gt. 243 .and. jday .le. 273 ) then  ! Sep
            hv = (/ 0.10 , 0.10 , 0.10 , 0.30 /)
         else if ( jday .gt. 273 .and. jday .le. 304 ) then  ! Oct
            hv = (/ 0.05 , 0.08 , 0.10 , 0.10 /)
         else                                                ! Nov-Feb
            hv = (/ 0.05 , 0.05 , 0.05 , 0.05 /)
         end if

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

C set erodible landuse map
         do m = 1, n_dlcat
            emap( m ) = dmap( m )  ! dmap maps to one of the 3 BELD3 desert types
         end do
         emap( n_dlcat+1 ) = 4

C --------- ###### Start Main Loop ###### ---------
 
         do r = 1, my_nrows
         do c = 1, my_ncols
            dust_em( c,r ) = 0.0
            soimt( c,r )   = 0.0
            fmoit( c,r )   = 0.0   ! for diagnostic output visualization
            vegfrac( c,r ) = 0.0
            do m = 1, n_dlcat+1
               ustr( c,r,m ) = 0.0   ! for diagnostic output visualization
               qam ( c,r,m ) = 0.0
               elus( c,r,m ) = 0.0
               fruf( c,r,m ) = 0.0
               kvh ( c,r,m ) = 0.0
            end do
 
            rlay1hgt = rjacm ( c,r ) / cellhgt

C---Vegetation fraction based on the MODIS FPAR
            vegfrac( c,r ) = max( min( fpar( c,r ), 0.95 ), 0.005 )
            vegfree = 1.0 - vegfrac( c,r )
            lambdav = -0.35 * log( vegfree ) ! Shao et al. [Aus. J. Soil Res.,1996]

C---Dust possiblity only if 1. not over water 
C                           2. rain < 1/100 in. (1 in. = 2.540 cm)
C                           3. not snow-covered
C                           4. if soimt <= limit
C                           5. desert type or ag landuse
C                           6. erodible landuse
C                           7. friction velocity > threshold

            if ( ( Grid_Data%lwmask( c,r ) .gt. 0.0 ) .and.
     &           ( Met_Data%rn( c,r ) + Met_Data%rc( c,r ) .le. 0.0254 ) .and. ! rn, rc = [cm]
     &           ( Met_Data%snocov( c,r ) .lt. 0.001 ) ) then ! less than 0.1% snow coverage
C---Dust possiblity 1,2,3

               j = Grid_Data%sltyp( c,r )

C kludge (fixed in wrf-px after 4 Mar 11)
               if ( j .gt. 4 ) j = j + 1   ! PX combines "silt" with "silt loam"
               if ( j .gt. 13 ) j = 13   ! = ?

C Calculate maximum amount of the adsorbed water
C    w` = 0.0014(%clay)**2 + 0.17(%clay) - w` in %
C    Fecan et al. [1999,Annales Geophys.,17,144-157]
               wmax( c,r ) = ( 14.0 * soiltxt( j,4 ) + 17.0 ) * soiltxt( j,4 )   ! [%]

!              write( logdev,'( 2x, a, i8.6, f12.5 )' ) 'max wmax:', jtime, maxval( wmax )

C Change soil moisture units from volumetric (m**3/m**3) to gravimetric (Kg/Kg)
               soimt( c,r ) = Met_Data%soim1( c,r ) ! <- [m**3/m**3]
     &                      * 1000.0 / ( 2650.0 * ( 0.511 + 0.126
     &                                          * ( soiltxt( j,1 ) + soiltxt( j,2 ) ) ) )

               if ( soimt( c,r ) .le. soilml1( j ) ) then
C---Dust possiblity 4

#ifdef verbose_wbdust
                  dryhit = dryhit + 1
#endif

C---Soil moisture effect on U*t
                  if ( soimt( c,r ) .le. 0.01 * wmax( c,r ) ) then   ! wmax in [%]
                     fmoit( c,r ) = 1.0
                  else
                     fmoit( c,r ) = sqrt( 1.0 + 1.21
     &                            * ( 100.0 * soimt( c,r ) - wmax( c,r ) ) ** 0.68 )
                  end if

C---Erodibility potential of soil component
                  sd_ep( c,r ) = soiltxt( j,4 ) * eropot( 1 )
     &                         + soiltxt( j,3 ) * eropot( 2 )
     &                         + ( soiltxt( j,1 ) + soiltxt( j,2 ) ) * eropot( 3 )

C---Lu and Shao [JGR,1999] and Kang et al. [JGR,2011]
C   First, mapping soil types into 4 main soil types following Kang et al. [JGR,2011]
                  select case ( j )
                     case( 1, 2 )          ! sand
                  !     pp = 5000.0
                  !     calpha = 0.001
                  !     pfrac = 0.06
                  !     flxfac1 = calpha * 9.81 * pfrac * 500.0 / pp
                        flxfac1 = 5.886e-05
                  !     flxfac2 = 2.09 * sqrt( 2650.0 / pp )
                        flxfac2 = 1.5215430
                     case( 3, 4, 6, 8, 9 ) ! loam
                  !     pp = 10000.0
                  !     calpha = 0.0006
                  !     pfrac = 0.18
                  !     flxfac1 = calpha * 9.81 * pfrac * 500.0 / pp
                        flxfac1 = 5.2974e-05
                  !     flxfac2 = 2.09 * sqrt( 2650.0 / pp )
                        flxfac2 = 1.0758933
                     case( 7 )             ! sandy clay loam
                  !     pp = 10000.0
                  !     calpha = 0.0006
                  !     pfrac = 0.32
                  !     flxfac1 = calpha * 9.81 * pfrac * 500.0 / pp
                        flxfac1 = 9.4176e-05
                  !     flxfac2 = 2.09 * sqrt( 2650.0 / pp )
                        flxfac2 = 1.0758933
                     case( 5, 10, 11, 12 ) ! clay
                  !     pp = 30000.0
                  !     calpha = 0.0002
                  !     pfrac = 0.72
                  !     flxfac1 = calpha * 9.81 * pfrac * 500.0 / pp
                        flxfac1 = 2.3544e-05
                  !     flxfac2 = 2.09 * sqrt( 2650.0 / pp )
                        flxfac2 = 0.1964303
                     case default          ! others -- no dust
                  !     pp = 100000.0
                  !     calpha = 1.0
                  !     pfrac = 0.0
                  !     flxfac1 = calpha * 9.81 * pfrac * 500.0 / pp
                        flxfac1 = 0.0
                  !     flxfac2 = 2.09 * sqrt( 2650.0 / pp )
                        flxfac2 = 0.3402273
                  end select

                  do m = 1, n_dlcat     ! desert type landuse category
                     elus( c,r,m ) = ladut( c,r,m ) * vegfree   ! desert land [%]
                  end do
                  elus( c,r,n_dlcat+1 ) = agland( c,r ) * vegfree ! crop land [%]
 
C ------- Start Loop Over Erodible Landuse ----
 
                  do m = 1, n_dlcat+1   ! desert type & crop landuse categories

                     if ( elus( c,r,m ) .gt. 100.0 .or. elus( c,r,m ) .lt. 0.0 ) then
                        write( xmsg,2009 ) elus( c,r,m ), c, r, m
                        call m3exit( pname, jdate, jtime, xmsg, xstat1 )
                     end if

                     if ( elus( c,r,m ) .gt. 0.0 ) then

                        n = emap( m )
                        lambda    = lambdab( n ) + lambdav
                        vegheight = ( hb_lambdab( n ) + hv( n ) * lambdav ) / lambda

C---New parametrization for surface roughness by H. Foroutan - Oct. 2015
                        if ( lambda .le. 0.2 ) then
                           z0 = 0.96  * ( lambda ** 1.07 )  * vegheight 
                        else
                           z0 = 0.083 * ( lambda ** ( -0.46 ) ) * vegheight 
                        end if

C---Calculate friction velocity (U*) at the surafce applicable to dust emission 
                        ustr( c,r,m ) = karman * Met_Data%WSPD10( c,r ) / log ( 10.0 / z0 )

C---Roughness effect on U*t (Drag partitioning)
C   Xi and Sokolik [JGR,2015]
                        fruf2  = ( 1.0 - sigv_mv  * lambdav )
     &                     *     ( 1.0 + betav_mv * lambdav )
     &                     *     ( 1.0 - sigb_mb  * lambdab( n ) / vegfree ) 
     &                     *     ( 1.0 + betab_mb * lambdab( n ) / vegfree )

                        if( fruf2 .gt. 1.0 ) then

                           fruf( c,r,m ) = sqrt( fruf2 )
                        else 
                           fruf( c,r,m ) = 10.0
                        end if

C---Vert-to-Horiz dust flux ratio : Kang et al. [JGR, 2011] : Eq. (12)
!                       kvh( c,r,m ) = ( calpha * 9.81 * pfrac * 1000.0 / 2.0 / pp )
!    &                               * ( 0.24 + 2.09 * ustr( c,r,m ) * sqrt( 2650.0 / pp ) )
                        kvh( c,r,m ) = flxfac1 * ( 0.24 + flxfac2 * ustr( c,r,m ) )
                        hflux = dust_hflux( ndp, dp,
     &                                      soiltxt( j,: ),
     &                                      fmoit( c,r ),
     &                                      fruf( c,r,m ),
     &                                      ustr( c,r,m ),
     &                                      sd_ep( c,r ),
     &                                      Met_Data%dens1( c,r ) )
                        vflux = hflux * kvh( c,r,m )                    ! [g/m**2/s]
                        qam( c,r,m ) = qam( c,r,m ) + vflux * rlay1hgt
     &                               * ( elus( c,r,m ) * 0.01 )         ! [g/m**3/s]
                     end if  ! if erodible land

                     if ( elus( c,r,m ) .eq. 0.0 .and. qam( c,r,m ) .ne. 0.0 ) then
                        xmsg = 'Erodible land use = 0, but emissions .ne. 0'
                        call m3exit( pname, jdate, jtime, xmsg, xstat1 )
                     end if

                     dust_em( c,r ) = dust_em( c,r ) + qam( c,r,m )

                  end do   ! m landuse
 
C ------- End Loop Over Erodible Landuse ----
 
C Dust removal by surrounding vegetation <-???
C Adjust dust emissions for transport factors

                  dust_em( c,r ) = dust_em( c,r ) * tfa( c,r ) * tfb( c,r )

               end if   ! if soil moisture
            end if   ! if rain & land & snow

         end do   ! c
         end do   ! r

C --------- ###### End Main Loop ##### ---------

#ifdef verbose_wbdust
         write( logdev,'( /5x, a, 1x, 2i8 )' ) 'dry hit count, 
     &                                          out of total cells:',
     &                                          dryhit, (c-1)*(r-1)
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
                  diagv( m ) = qam( c,r,m )             ! g/m**3/s
               end do
               n = n_dlcat + 2
               diagv( n ) = dust_em( c,r )              ! g/m**3/s

               sumdfr = 0.0
               do m = 1, n_dlcat+1
                  diagv( m+n ) = elus( c,r,m )
                  sumdfr = sumdfr + elus( c,r,m )
               end do
               n = n + n_dlcat + 2
               diagv( n ) = sumdfr

               do m = 1, n_dlcat+1
                  diagv( m+n ) = ustr( c,r,m )
               end do
               n = n + n_dlcat + 1

               do m = 1, n_dlcat+1
                  diagv( m+n ) = kvh( c,r,m )
               end do
               n = n + n_dlcat + 1

               do m = 1, n_dlcat+1
                  diagv( m+n ) = fruf( c,r,m )
               end do
               n = n + n_dlcat + 1

               diagv( n+1  ) = fmoit( c,r )       ! 'Soil_Moist_Fac  '
               diagv( n+2  ) = sd_ep( c,r )       ! 'Soil_Erode_Pot  '
               diagv( n+3  ) = wmax ( c,r )       ! 'Mx_Adsrb_H2O_Frc'
               diagv( n+4  ) = vegfrac( c,r )     ! 'Vegetation_Frac '
               diagv( n+5  ) = uland( c,r,3 )     ! 'Urban_Cover     '
               diagv( n+6  ) = uland( c,r,4 )     ! 'Forest_Cover    '
               diagv( n+7  ) = tfa  ( c,r )       ! 'Trfac_Above_Can '
               diagv( n+8 )  = tfb  ( c,r )       ! 'Trfac_Inside_Can'

               n = n + 8

! accum and coarse mode number density emissions
               diagv( n+1 ) = dustoutn( 2,c,r )
               diagv( n+2 ) = dustoutn( 3,c,r )
! accum and coarse mode surface area density emissions
               diagv( n+3 ) = dustouts( 2,c,r )
               diagv( n+4 ) = dustouts( 3,c,r )
              
               n = n + 4
               m = 0
               do v = 1, ndust_spc
                  if ( trim( dust_spc( v )%name( 2 ) ) .ne. ' ' ) then  ! accum. mode mass emissions
                     m = m + 1
                     diagv( m+n ) = dustoutm( v,2,c,r )
                  end if
               end do

               do v = 1, ndust_spc
                  if ( trim( dust_spc( v )%name( 3 ) ) .ne. ' ' ) then  ! coarse mode mass emissions
                     m = m + 1
                     diagv( m+n ) = dustoutm( v,3,c,r )
                  end if
               end do

               n = n + m


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
     &                                       dusthit, (c-1)*(r-1)
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

C=======================================================================
         function dust_hflux( ndp, dp, soiltxt, fmoit, fruf, ustr, sd_ep, dens )
     &      result( hflux )

C usage: hflux = dust_flux( ndp, dp,
C                           soiltxt( j,: ),
C                           fmoit( c,r ),
C                           fruf( c,r,m ),
C                           ustr( c,r,m ),
C                           sd_ep( c,r ),
C                           dens( c,r ) )

            implicit none

            include SUBST_CONST   ! for grav

            integer, intent( in ) :: ndp
            real,    intent( in ) :: dp( ndp )
            real,    intent( in ) :: soiltxt( ndp )
            real,    intent( in ) :: fmoit, fruf, ustr, sd_ep, dens
            real hflux

            real, parameter :: amen = 1.0          ! Marticorena and Bergametti [JGR,1997]
            real, parameter :: cfac = 1000.0 * amen / grav
            real, parameter :: A = 260.60061       ! 0.0123 * 2650.0 * 9.81 / 1.227
            real, parameter :: B = 1.6540342e-06   ! 0.0123 * 0.000165 / 1.227
            real utstar                            ! threshold U* [m/s]
            real utem                              ! U term [(m/s)**3]
            real fac
            integer n

! I can't initialize dp this way - it has to be passed in since ndp is variable

C---Mean mass median diameter (m) for each soil texture
C   [Chatenet et al., Sedimentology 1996 and Menut et al., JGR 2013]
!           real :: dp( ndp ) =
!    &              (/ 690.0E-6,     ! Coarse sand
!    &                 210.0E-6,     ! Fine-medium sand
!    &                 125.0E-6,     ! Silt
!    &                   2.0E-6 /)   ! Clay

            fac = cfac * dens * sd_ep
            utem    = 0.0
            utstar  = 0.0
            hflux   = 0.0
            do n = 1, ndp   ! loop over dust particle size
!              utstar = sqrt( 0.0123 * ( 2650.0 * 9.81 * dp( n ) / 1.227 + 0.000165
!              / 1.227 / dp( n ) ) )              ! X roughness & moisture effects
               utstar = sqrt( A * dp( n ) + B / dp( n ) ) * fmoit * fruf !Shao and Lu [JGR,2000]
               if ( ustr .gt. utstar ) then  ! wind erosion occurs only if U* > U*t
C---Horiz. Flux from White (1979)
                  utem = ( ustr + utstar ) * ( ustr * ustr - utstar * utstar )
C---Horiz. Flux from Owen (1964)
!                 utem = ustr * ( ustr * ustr - utstar * utstar )
                  hflux = hflux
     &                  + fac * utem * soiltxt( n )   !  [g/m/s]
               end if
            end do   ! dust particle size

         end function dust_hflux

      end module dust_emis

