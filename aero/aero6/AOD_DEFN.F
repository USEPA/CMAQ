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

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      module aod_defn 
C Calculates the aerosol optical thickness using reconstructed extinction
C values from the IMPROVE monitoring network updated from Malm and Hand,
C (Atmos. Env. 41, 3407-3427, 2007) See comments in AERO_DATA.F

C Revision History:
C 10 Sep 15 J.Young: inital

      implicit none

C *** environment variable for AOD file
      character( 16 ), save :: ctm_aod = 'CTM_AOD'

C *** AOD diagnostic file name (now in FILES_CTM.EXT)
!     character( 16 ) :: ctm_aod_1 = 'CTM_AOD_1'

C *** flag for AOD file [F], default
      logical, save :: aod

      real, allocatable :: aod_data( :,: )

      real, private, allocatable :: conc_cr ( :,: ),
     &                              ta_cr   ( : ),
     &                              pres_cr ( : ),
     &                              qv_cr   ( : ),
     &                              zh_cr   ( : ),
     &                              ext_cr  ( : )

      contains

C-----------------------------------------------------------------------

      function aod_init ( jdate, jtime, tstep ) result( success )
         use grid_conf           ! horizontal & vertical domain specifications
         use cgrid_spcs          ! for nspcsd
         use utilio_defn

         include SUBST_FILES_ID

C Arguments:
         integer, intent( in ) :: jdate   ! current model date, coded YYYYDDD
         integer, intent( in ) :: jtime   ! current model time, coded HHMMSS
         integer, intent( in ) :: tstep   ! output time step
         logical success

         character( 16 ) :: pname = 'Aod_Init'
         character( 96 ) :: xmsg = ' '
         integer ios, l

         success = .true.

         allocate ( conc_cr ( nlays,nspcsd ),
     &              zh_cr   ( nlays ),
     &              ta_cr   ( nlays ),
     &              pres_cr ( nlays ),
     &              qv_cr   ( nlays ),
     &              ext_cr  ( nlays ),
     &              aod_data( ncols,nrows ), stat = ios )

         if ( ios .ne. 0 ) then
            xmsg = '*** ERROR allocating conc_cr, zh_cr, ta_cr, pres_cr, qv_cr,'
     &           // ' or ext_cr, or ext_cr'
            call m3warn ( pname, sdate3d, stime3d, xmsg )
            success = .false.; return
         end if

         ftype3d = grdded3
         sdate3d = jdate
         stime3d = jtime
         tstep3d = tstep
         call nextime ( sdate3d, stime3d, tstep3d ) !  start the next hour

         nvars3d = 1
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
!        vgtpun3d = vgtpun_gd ! currently, not defined

         gdnam3d = grid_name  ! from hgrd_defn

         vtype3d( 1 ) = m3real
         vname3d( 1 ) = 'AOD'
         units3d( 1 ) = '----'
         vdesc3d( 1 ) = 'Aerosol Optical Depth'

         fdesc3d( 1 ) = 'Aerosol Optical Depth'
         do l = 2, mxdesc3
            fdesc3d( l ) = ' '
         end do

c Open the diagnostic file

         if ( mype .eq. 0 ) then
            if ( .not. open3( ctm_aod_1, fsnew3, pname ) ) then
               xmsg = 'Could not create '// ctm_aod_1 // ' file'
               call m3warn ( pname, sdate3d, stime3d, xmsg )
               success = .false.; return
            end if
         end if

         return
      end function aod_init

!-----------------------------------------------------------------------
      subroutine get_aod ( cgrid )

C compute extinction and aod from conc files similar to getvisby.f as a template
         use aero_data          
         use asx_data_mod     ! meteorology data, includes SUBST_CONST
         use utilio_defn

         implicit none

         real, pointer :: cgrid( :,:,:,: )

       ! character( 16 ) :: aod_file  = 'AOD'
!        character( 16 ) :: ext_file  = 'EXTINC'
         character( 16 ) :: pname = 'Get_Aod'
         integer :: c, r, k 
         real    :: tot_thickness
         real    :: lvl_thickness
         real    :: blkesat
         real    :: blkevap
         real    :: airrh

!        logical :: output_ext
         logical :: m3_wet_flag ! flag to determine if M2 is wet

         integer :: nvars
         integer :: logdev

         real, parameter :: epswater = mwwat / mwair

         real, parameter :: al = 610.94
         real, parameter :: bl = 17.625
         real, parameter :: cl = 243.04
         real    :: tt
         real    :: esatl
         esatl( tt ) = al * exp( bl * ( tt - stdtemp ) / ( tt - stdtemp + cl ) )

!        logical, save :: once = .true.

!-----------------------------------------------------------------------

         logdev = init3()

!        output_ext = envyn( 'output_ext', 'flag to output 3D extiction file',
!    &                       .false., ios)

!        if ( output_ext ) then
!           vname3d( 1 ) = 'EXTINCTION'
!           units3d( 1 ) = '1/km'
!           write ( vdesc3d( 1 ), '(a)') 'block extinction (Reconstructed)'
!           if ( .not. open3( ext_file, fsnew3, pname ) ) then
!              write( logdev,* ) ' could not open file:', ext_file
!              stop
!           end if
!        end if

         do r = 1, my_nrows
            do c = 1, my_ncols
               conc_cr = cgrid( c,r,:,: )
               zh_cr   = Met_Data%zh( c,r,: )
               ta_cr   = Met_Data%ta( c,r,: )
               pres_cr = Met_Data%pres( c,r,: )
               qv_cr   = Met_Data%qv( c,r,: )
               aod_data( c,r ) = 0.0
               tot_thickness = 0.0
               do k = 1, nlays
                  lvl_thickness = 2.0 * ( zh_cr( k ) - tot_thickness )
                  tot_thickness = tot_thickness + lvl_thickness
!                 if (once) then
!                    write( logdev,* ) k, lvl_thickness, tot_thickness
!                 end if

! compute fractional relative humidity
                  blkesat = esatl( ta_cr( k ) )
                  blkevap = pres_cr( k ) * qv_cr( k )     ! Note pres in pascals
     &                    / ( epswater  + qv_cr( k ) )
                  airrh   = max( 0.005, min( 0.99, blkevap / blkesat ) )

! compute extinction, 1/km
                  call extract_aero( conc_cr( k,: ), .false. )
                  M3_wet_flag = .false.
                  call getpar( M3_wet_flag, .false. )

                  call get_extinc( airrh, ext_cr( k ) )
! compute aod
                  aod_data( c,r ) = aod_data( c,r )
     &                            + ext_cr( k ) * 0.001 * lvl_thickness
               end do

!              if (once) then
!                 once = .false.
!              end if

            end do
         end do

         return
         end subroutine get_aod

!-----------------------------------------------------------------------
      subroutine get_extinc( airrh, ext2 )
 
C *** adapted from getvisby.F
C-----------------------------------------------------------------------
 
         use aero_data

         implicit none
 
         real, intent( in )  :: airrh  ! rel hum [ fraction ]
         real, intent( out ) :: ext2   ! extinction [ 1/km ] (Reconstructed)

C *** Parameters 
C factor to rescale units from [ 1/Mm ] to [ 1/km ]
         real, parameter :: scale = 1.0e-03

C *** Internal Variables:

         integer irh ! percent realtive humidity as an integer used for index
         real    frh ! RH correction factor: sulfate, nitrate, and ammonium aerosols
         real    frh_ss ! RH correction factor: sea salt aerosols (Malm & Hand, AE,2007)

C air humidity scaling factors at 1% RH intervals
         real, parameter :: humfac( 99 ) = (/
     &      1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,
     &      1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,
     &      1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0001e+00,  1.0001e+00,
     &      1.0004e+00,  1.0006e+00,  1.0024e+00,  1.0056e+00,  1.0089e+00,
     &      1.0097e+00,  1.0105e+00,  1.0111e+00,  1.0115e+00,  1.0118e+00,
     &      1.0122e+00,  1.0126e+00,  1.0130e+00,  1.0135e+00,  1.0139e+00,
     &      1.0173e+00,  1.0206e+00,  1.0254e+00,  1.0315e+00,  1.0377e+00,
     &      1.0486e+00,  1.0596e+00,  1.0751e+00,  1.0951e+00,  1.1151e+00,
     &      1.1247e+00,  1.1343e+00,  1.1436e+00,  1.1525e+00,  1.1615e+00,
     &      1.1724e+00,  1.1833e+00,  1.1955e+00,  1.2090e+00,  1.2224e+00,
     &      1.2368e+00,  1.2512e+00,  1.2671e+00,  1.2844e+00,  1.3018e+00,
     &      1.3234e+00,  1.3450e+00,  1.3695e+00,  1.3969e+00,  1.4246e+00,
     &      1.4628e+00,  1.5014e+00,  1.5468e+00,  1.5992e+00,  1.6516e+00,
     &      1.6991e+00,  1.7466e+00,  1.7985e+00,  1.8549e+00,  1.9113e+00,
     &      1.9596e+00,  2.0080e+00,  2.0596e+00,  2.1146e+00,  2.1695e+00,
     &      2.2630e+00,  2.3565e+00,  2.4692e+00,  2.6011e+00,  2.7330e+00,
     &      2.8461e+00,  2.9592e+00,  3.0853e+00,  3.2245e+00,  3.3637e+00,
     &      3.5743e+00,  3.7849e+00,  4.0466e+00,  4.3594e+00,  4.6721e+00,
     &      5.3067e+00,  5.9412e+00,  6.9627e+00,  8.3710e+00,  9.7793e+00,
     &      1.2429e+01,  1.5078e+01,  1.8059e+01,  2.1371e+01/)

C Update from Christian Hogrefe...
C sea salt humidity scaling factors at 1% RH values
C based on "fss(Rh)" in Table 1 of
C "revised IMPROVE algorithm for estimating light extinction
C from particle speciation data", available at
C http://vista.cira.colostate.edu/improve/Publications/GrayLit/gray_literature.htm
C values for RH96%-99% were linearly extrapolated by using the 94% and 95% values:
C     f(96%)=f(95%)+(f(95%)-f(94%))
C     f(97%)=f(96%)+(f(96%)-f(95%))
C     f(98%)=f(97%)+(f(97%)-f(96%))
C     f(99%)=f(98%)+(f(98%)-f(97%))

         real, parameter :: humfac_ss( 99 ) = (/
     &        1.00, 1.00, 1.00, 1.00, 1.00,
     &        1.00, 1.00, 1.00, 1.00, 1.00,
     &        1.00, 1.00, 1.00, 1.00, 1.00,
     &        1.00, 1.00, 1.00, 1.00, 1.00,
     &        1.00, 1.00, 1.00, 1.00, 1.00,
     &        1.00, 1.00, 1.00, 1.00, 1.00,
     &        1.00, 1.00, 1.00, 1.00, 1.00,
     &        1.00, 1.00, 1.00, 1.00, 1.00,
     &        1.00, 1.00, 1.00, 1.00, 1.00,
     &        1.00, 2.36, 2.38, 2.42, 2.45,
     &        2.48, 2.50, 2.51, 2.53, 2.56,
     &        2.58, 2.59, 2.62, 2.66, 2.69,
     &        2.73, 2.78, 2.83, 2.83, 2.86,
     &        2.89, 2.91, 2.95, 3.01, 3.05,
     &        3.13, 3.17, 3.21, 3.25, 3.27,
     &        3.35, 3.42, 3.52, 3.57, 3.63,
     &        3.69, 3.81, 3.95, 4.04, 4.11,
     &        4.28, 4.49, 4.61, 4.86, 5.12,
     &        5.38, 5.75, 6.17, 6.72, 7.35,
     &        7.98, 8.61, 9.24, 9.87/)

         integer m, n    ! loop counters

C-----------------------------------------------------------------------

C *** IMPROVE reconstructed method

         irh = int( 100.0 * airrh  ) ! truncate relative humidity to nearest integer
         irh = max( 1, min( 99, irh ) ) ! set bounds

         frh = humfac( irh )         ! aso4, ano3, anh4 humidity correction
         frh_ss = humfac_ss( irh )   ! sea salt aerosol humidity correction

C *** NOTE in the following the fine primary mass "other" is
C     treated as though it were fine mass soil.

         ext2 = 0.0
      
C     sum from aerospc species
         do n = 1, n_aerospc

            if (  aerospc( n )%tracer ) cycle

            if ( n .eq. aso4_idx .or. n .eq. ano3_idx .or. n .eq. anh4_idx ) then
               do m = 1, n_mode
                  if ( .not. aero_missing( n,m ) ) then
                     ext2 = ext2 + frh * aerospc( n )%visual_idx * aerospc_conc( n,m )
                  end if
               end do
               cycle
            end if

            if ( n .eq. acl_idx .or. n .eq. ana_idx .or. n .eq. aseacat_idx ) then
               do m = 1, n_mode
                  if ( .not. aero_missing( n,m ) ) then
                     ext2 = ext2 + frh_ss * aerospc( n )%visual_idx * aerospc_conc( n,m )
                  end if
               end do
               cycle
            end if

            do m = 1, n_mode
               if ( .not. aero_missing( n,m ) ) then
                  ext2 = ext2 + aerospc( n )%visual_idx * aerospc_conc( n,m )
               end if
            end do

         end do

         ext2 = scale * ext2

         return
      end subroutine get_extinc

      end module aod_defn
