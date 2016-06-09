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
C    May 16 H. Pye, B. Murphy: Updated treatment of aerosol moments

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
         units3d( 1 ) = 'na'
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
C Revisions:
C 4/2016: H Pye, removed m3_wet_flag since it is now tracked via
C         AERO_DATA.F. Moments are wet after extract_aero

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
! extract_aero will compute the wet second and third moments
                  call extract_aero( conc_cr( k,: ), .false. )
                  call getpar( .false. )

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
C Revisions:
C 05/2016: P Liu, 1) update humfac and add humfac_large, because ammonium sulfate, nitrate and OM
C                 are splitted into small and large modes,humfac will be used for small mode
C                 2) update algorithm for estimating light extinction efficiency based on IMPROVE 
C                 for both updates see Pitchford et al., Journal of the Air & Waste Management Association (57)(2007), pp 1326.
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
         real    frh_large ! RH correction factor for large mode sulfate, nitrate, and ammonium aerosols
         real    frh_ss ! RH correction factor: sea salt aerosols (Malm & Hand, AE,2007)

C air humidity scaling factors at 1% RH intervals for small mode                                                               
C values for RH96%-99% are the same as value for RH95%, based on the
C Regional Haze Rule protocal: "Values are presented to RH = 95%, to
C which higher RH are 'rolled back' under the RHR protocol to mitigate
C the potentially large errors associated with RH measurement
C uncertainty for near saturation conditions"

         real, parameter :: humfac( 99 ) = (/
     &      1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,
     &      1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,
     &      1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,
     &      1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,
     &      1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,
     &      1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,
     &      1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,
     &      1.0000e+00,  1.3800e+00,  1.4000e+00,  1.4200e+00,  1.4400e+00,
     &      1.4600e+00,  1.4800e+00,  1.4900e+00,  1.5100e+00,  1.5300e+00,
     &      1.5500e+00,  1.5700e+00,  1.5900e+00,  1.6200e+00,  1.6400e+00,
     &      1.6600e+00,  1.6800e+00,  1.7100e+00,  1.7300e+00,  1.7600e+00,
     &      1.7800e+00,  1.8100e+00,  1.8300e+00,  1.8600e+00,  1.8900e+00,
     &      1.9200e+00,  1.9500e+00,  1.9900e+00,  2.0200e+00,  2.0600e+00,
     &      2.0900e+00,  2.1300e+00,  2.1700e+00,  2.2200e+00,  2.2600e+00,
     &      2.3100e+00,  2.3600e+00,  2.4100e+00,  2.4700e+00,  2.5400e+00,
     &      2.6000e+00,  2.6700e+00,  2.7500e+00,  2.8400e+00,  2.9300e+00,
     &      3.0300e+00,  3.1500e+00,  3.2700e+00,  3.4200e+00,  3.5800e+00,
     &      3.7600e+00,  3.9800e+00,  4.2300e+00,  4.5300e+00,  4.9000e+00,
     &      5.3500e+00,  5.9300e+00,  6.7100e+00,  7.7800e+00,  9.3400e+00,
     &      9.3400e+00,  9.3400e+00,  9.3400e+00,  9.3400e+00/)

C air humidity scaling factors at 1% RH intervals for large mode
         real, parameter :: humfac_large( 99 ) = (/
     &      1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,
     &      1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,
     &      1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,
     &      1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,
     &      1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,
     &      1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,
     &      1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,  1.0000e+00,
     &      1.0000e+00,  1.3100e+00,  1.3200e+00,  1.3400e+00,  1.3500e+00,
     &      1.3600e+00,  1.3800e+00,  1.3900e+00,  1.4100e+00,  1.4200e+00,
     &      1.4400e+00,  1.4500e+00,  1.4700e+00,  1.4900e+00,  1.5000e+00,
     &      1.5200e+00,  1.5400e+00,  1.5500e+00,  1.5700e+00,  1.5900e+00,
     &      1.6100e+00,  1.6300e+00,  1.6500e+00,  1.6700e+00,  1.6900e+00,
     $      1.7100e+00,  1.7300e+00,  1.7500e+00,  1.7800e+00,  1.8000e+00,
     &      1.8300e+00,  1.8600e+00,  1.8900e+00,  1.9200e+00,  1.9500e+00,
     &      1.9800e+00,  2.0100e+00,  2.0500e+00,  2.0900e+00,  2.1300e+00,
     &      2.1800e+00,  2.2200e+00,  2.2700e+00,  2.3300e+00,  2.3900e+00,
     &      2.4500e+00,  2.5200e+00,  2.6000e+00,  2.6900e+00,  2.7900e+00,
     &      2.9000e+00,  3.0200e+00,  3.1600e+00,  3.3300e+00,  3.5300e+00,
     &      3.7700e+00,  4.0600e+00,  4.4300e+00,  4.9200e+00,  5.5700e+00,
     &      5.5700e+00,  5.5700e+00,  5.5700e+00,  5.5700e+00/)

C sea salt humidity scaling factors at 1% RH values

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
     &        7.35, 7.35, 7.35, 7.35/)

         integer m, n    ! loop counters

         real :: total_mass_so4,total_mass_no3,total_mass_om  ! total aerosol mass of sulfate, nitrate,or OM
         real :: mass_with_nh4
C-----------------------------------------------------------------------

C *** IMPROVE reconstructed method

         irh = int( 100.0 * airrh  ) ! truncate relative humidity to nearest integer
         irh = max( 1, min( 99, irh ) ) ! set bounds

         frh = humfac( irh )             ! aso4, ano3, anh4 humidity correction for small mode
         frh_large = humfac_large( irh ) ! aso4, ano3, anh4 humidity correction for large mode
         frh_ss = humfac_ss( irh )   ! sea salt aerosol humidity correction

C *** NOTE in the following the fine primary mass "other" is
C     treated as though it were fine mass soil.

      
C     split aerosol mass of sulfate nitrate and OM to small and large
C     modes based on their individual total mass

         total_mass_so4 = 0.0
         total_mass_no3 = 0.0
         total_mass_om  = 0.0

         do n = 1, n_aerospc
             if ( n .eq. aso4_idx ) then
                do m = 1, n_mode
                  if ( .not. aero_missing( n,m ) ) then
                     total_mass_so4 = total_mass_so4 +aerospc_conc( n,m )
                  end if
                end do
                cycle
             end if

             if ( n .eq. ano3_idx ) then
                do m = 1, n_mode
                  if ( .not. aero_missing( n,m ) ) then
                     total_mass_no3 = total_mass_no3 +aerospc_conc( n,m )
                  end if
                end do
                cycle
             end if

             if ( aerospc( n )%om ) then
                 do m = 1, n_mode
                  if ( .not. aero_missing( n,m ) ) then
                     total_mass_om = total_mass_om +aerospc_conc( n,m )
                  end if
                 end do
                 cycle
              end if
         end do
C  convert mass of sulfate and nitrate to ammonium sulfate and ammounium nitrate
         total_mass_so4 = total_mass_so4 * (aerospc_mw(anh4_idx)*2+aerospc_mw(aso4_idx))
     &                    /aerospc_mw(aso4_idx) 
         total_mass_no3 = total_mass_no3 * (aerospc_mw(anh4_idx)+aerospc_mw(ano3_idx))
     &                    /aerospc_mw(ano3_idx)       

C     get the extinction efficiency for all species
         ext2 = 0.0
         
         do n = 1, n_aerospc

            if (  aerospc( n )%tracer ) cycle

            if ( n .eq. aso4_idx ) then         ! for ammonium sulfate
               do m = 1, n_mode
                  if ( .not. aero_missing( n,m ) ) then
                     mass_with_nh4 = aerospc_conc( n,m)*
     &                               (aerospc_mw(anh4_idx)*2+aerospc_mw(n))/aerospc_mw(n)
                     if (total_mass_so4 .lt. 20.0)  then  ! if total mass less than 20ug/m3, split to 2 modes
                       ext2 = ext2 + frh*aerospc( n )%visual_idx *(mass_with_nh4-mass_with_nh4/20.0)
     &                        + frh_large * aerospc(n)%visual_idx_large*mass_with_nh4/20.0
     
                     else                               ! else  all mass goes to large mode
                       ext2 = ext2 + frh_large * aerospc( n )%visual_idx_large *mass_with_nh4
                          
                     end if
                  end if
               end do
               cycle
            end if
        
            if (n .eq. ano3_idx ) then        ! for ammonium nitrate
                do m = 1, n_mode
                  if ( .not. aero_missing( n,m ) ) then
                    mass_with_nh4 = aerospc_conc( n,m)*
     &                              (aerospc_mw(anh4_idx)+aerospc_mw(n))/aerospc_mw(n)
                     if (total_mass_no3 .lt. 20.0)  then  ! if total mass less than 20ug/m3, split to 2 modes
                       ext2 = ext2 + frh*aerospc( n )%visual_idx*(mass_with_nh4-mass_with_nh4/20.0)
     &                        + frh_large * aerospc(n)%visual_idx_large*mass_with_nh4/20.0

                     else                               ! else  all mass goes to large mode
                       ext2 = ext2 + frh_large * aerospc( n)%visual_idx_large *mass_with_nh4

                     end if 
                  end if
                end do
                cycle
            end if

            if ( aerospc( n )%om ) then        ! for organic mass
                 do m = 1, n_mode
                  if ( .not. aero_missing( n,m ) ) then
                   if (total_mass_om .lt. 20.0)  then  ! if total mass less than 20ug/m3, split to 2 modes
                       ext2 = ext2 + aerospc( n )%visual_idx*(aerospc_conc( n,m)-aerospc_conc( n,m)/20.0)
     &                        + aerospc(n)%visual_idx_large*aerospc_conc( n,m)/20.0

                     else                               ! else  all mass goes to large mode
                       ext2 = ext2 + aerospc(n)%visual_idx_large *aerospc_conc( n,m)

                     end if
                  end if
                 end do
                 cycle
            end if


            if ( n .eq. acl_idx .or. n .eq. ana_idx .or. n .eq. aseacat_idx ) then ! for sea salt
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
