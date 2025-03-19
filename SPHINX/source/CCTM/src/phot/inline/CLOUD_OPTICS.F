
!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !
!  continuous development by various groups and is based on information  !
!  from these groups: Federal Government employees, contractors working  !
!  within a United States Government contract, and non-Federal sources   !
!  including research institutions.  These groups give the Government    !
!  permission to use, prepare derivative works of, and distribute copies !
!  of their work in the CMAQ system to the public and to permit others   !
!  to do so.  The United States Environmental Protection Agency         !
!  therefore grants similar permission to use the CMAQ system software,  !
!  but users are requested to provide copies of derivative works or     !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software       !
!  that is used with the CMAQ system but distributed under the GNU      !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                             !
!------------------------------------------------------------------------!

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      Module Cloud_Optics

! Purpose:
! Calculate the optical properties of hydrometeors per wavelength and layer.
! Five types are treated liquid cloud droplet, ice particles, snowflakes, graupel
! and rain droplet. The last three are lumped into one catagory called cloud 
! aggregates
! History:
! 09/15/14: B.Hutzell: Initial version created
! 02/01/19: D.Wong:    Cleaned up USE module clauses

         Implicit None

         real, parameter   :: cldmin   = 1.e-20      ! minimum value for cloud quantities
         
         real, allocatable :: cloud_liquid_ext ( :,: )   ! resolved cloud liquid droplet extinction, 1/m
         real, allocatable :: cloud_liquid_scat( :,: )   ! resolved cloud liquid droplet scattering, 1/m
         real, allocatable :: cloud_liquid_ssa ( :,: )   ! resolved cloud liquid droplet co-albedo
         real, allocatable :: cloud_liquid_asy ( :,: )   ! resolved cloud liquid droplet asymmetery factor

         real, allocatable :: cloud_aggreg_ext ( :,: )   ! resolved cloud aggregates extinction, 1/m
         real, allocatable :: cloud_aggreg_scat( :,: )   ! resolved cloud aggregates scattering, 1/m
         real, allocatable :: cloud_aggreg_ssa ( :,: )   ! resolved cloud aggregates co-albedo
         real, allocatable :: cloud_aggreg_asy ( :,: )   ! resolved cloud aggregates asymmetery factor

         real, allocatable :: cloud_ice_ext  ( :,: )    ! resolved cloud ice particle extinction, 1/m
         real, allocatable :: cloud_ice_scat ( :,: )    ! resolved cloud ice particle scattering, 1/m
         real, allocatable :: cloud_ice_ssa  ( :,: )    ! resolved cloud ice particle single scattering albedo
         real, allocatable :: cloud_ice_asy  ( :,: )    ! resolved cloud ice particle asymmetery factor
         real, allocatable :: cloud_ice_del  ( :,: )    ! resolved cloud ice delta forward tranmission function

         real, allocatable :: total_tau_cld( : )         ! total optical depth of resolved cloud 
#ifdef phot_debug         
         real, allocatable :: ave_asymm_cld( : )         ! column average of resolved cloud asymmetry factor
         real, allocatable :: ave_ssa_cld  ( : )         ! column average of resolved cloud single scattering albedo
#endif
 
         real, allocatable :: cloud_ext_coeff( :,: )   ! total cloud extinction coefficient, 1/m
         real, allocatable :: cloud_scat_coef( :,: )   ! total cloud scattering coefficent, 1/m
         real, allocatable :: cloud_asym_fact( :,: )   ! average cloud asymmetery factor


         real, allocatable :: total_tau_urcld( : ) ! total optical depth of unresolved cloud 
         real, allocatable :: ave_asymm_urcld( : ) ! column average of unresolved cloud asymmetry factor
         real, allocatable :: ave_ssa_urcld  ( : ) ! column average of unresolved cloud single scattering albedo

         real, allocatable :: rel( : )             ! liquid droplet effective radius, um
         real, allocatable :: dge( : )             ! generalized ice particle size, um
         real              :: max_dge              ! maximum allowed value for dge, um
         real              :: max_dge_squ          ! maximum allowed value squared for dge , um2
         real              :: min_dge              ! minimum allowed value for dge, um
        
         logical, allocatable :: cloud_layering ( : ) ! flag to use optical depth correction for cloud layering

         
         integer            :: row_cloud 
         integer            :: col_cloud
         
         public             :: init_cloud_optics,  general_effective_size, get_ice_optics,
     &                         get_droplet_optics, relcalc, get_aggregate_optics

         real, parameter, private :: low      = 1.e-06      ! epsilon
         real, parameter, private :: high     = 1.0e0 - low ! 1.0 - epsilon
         real, parameter, private :: cldtiny  = high*cldmin ! minimum value for cloud scattering
         
! Abscissas of Gauss-Laguerre Integration
! for 32 point quadrature
         real( 8 ), parameter, private :: xk(32) = 
     &    (/ 0.0444893658333D+0, 0.23452610952D+0,  
     &       0.576884629302D+0,  1.07244875382D+0, 1.72240877644D+0, 2.52833670643D+0, 
     &       3.49221327285D+0,   4.61645677223D+0, 5.90395848335D+0, 7.3581268086D+0,   
     &       8.98294126732D+0,   10.783012089D+0,  12.763745476D+0,  14.9309117981D+0,    
     &       17.2932661372D+0,   19.8536236493D+0, 22.6357789624D+0, 25.6201482024D+0,  
     &       28.8739336869D+0,   32.3333294017D+0, 36.1132042245D+0, 40.1337377056D+0,  
     &       44.5224085362D+0,   49.2086605665D+0, 54.3501813324D+0, 59.8791192845D+0,  
     &       65.9833617041D+0,   72.6842683222D+0, 80.1883747906D+0, 88.735192639D+0,   
     &       98.8295523184D+0,   111.751398227D+0 /)

! total weights (weight*exp(xk)) of Modified Gauss-Laguerre Integration
         real( 8 ), parameter, private :: totalw(32) = 
     &    (/ 0.114187105768D+0, 0.266065216898D+0, 
     &       0.418793137325D+0, 0.572532846497D+0, 0.727648788453D+0, 0.884536718946D+0, 
     &       1.04361887597D+0,  1.20534920595D+0,  1.37022171969D+0,  1.53877595906D+0,     
     &       1.71164594592D+0,  1.8895649683D+0,   2.07318851235D+0,  2.26590144444D+0,      
     &       2.46997418988D+0,  2.64296709494D+0,  2.76464437462D+0,  3.22890542981D+0,     
     &       2.92019361963D+0,  4.3928479809D+0,   4.27908673189D+0,  5.20480398519D+0,      
     &       5.11436212961D+0,  4.15561492173D+0,  6.19851060567D+0,  5.34795780128D+0,     
     &       6.28339212457D+0,  6.89198340969D+0,  7.92091094244D+0,  9.20440555803D+0,     
     &       11.1637432904D+0,  15.3902417688D+0 /)
     
         real( 8 ), private          :: newtotalw(32)
! Abscissas for 16 point quadrature
         real( 8 ), parameter, private :: gauss_laguerre_node( 16 ) =   
     &     (/ 0.8764941047892792D-1, 0.4626963289150808D+0, 1.141057774831227D+0,    
     &        2.129283645098381D+0,  3.437086633893207D+0, 5.078018614549768D+0,    
     &        7.070338535048235D+0,  9.438314336391938D+0, 12.21422336886616D+0,
     &        15.44152736878162D+0,  19.18015685675314D+0, 23.51590569399191D+0, 
     &        28.57872974288214D+0,  34.58339870228663D+0, 41.94045264768833D+0,
     &        51.70116033954332D+0 /)      
     
! total weights for 16 points     
         real( 8 ), parameter, private :: gauss_laguerre_weight( 16 ) = 
     &     (/ 0.2250363148642442D+0, 0.5258360527623427D+0, 0.8319613916870883D+0,
     &        1.146099240963750D+0,  1.471751316966809D+0,  1.813134687381348D+0,
     &        2.175517519694609D+0,  2.565762750165028D+0,  2.993215086371375D+0,
     &        3.471234483102089D+0,  4.020044086444668D+0,  4.672516607732857D+0,
     &        5.487420657986129D+0,  6.585361233289269D+0,  8.276357984364143D+0,
     &        11.82427755165841D+0 /)
     
         real( 8 ), private            :: gauss_laguerre_total( 16 )
     
         real( 8 ), parameter :: cloud_largest      = 9.0d+307 
         real( 8 ), parameter :: cloud_smallest     = 9.0d-307
         real( 8 ), parameter :: cloud_log_largest  =  709.090848126508d0
         real( 8 ), parameter :: cloud_log_smallest = -709.090848126508d0
         
         contains         
      
!-----------------------------------------------------------------------
         subroutine init_cloud_optics()

         use VGRD_DEFN, ONLY : NLAYS
         USE UTILIO_DEFN
         USE CSQY_DATA

         Implicit None

         integer          :: allocstat           ! memory allocation status
         integer          :: i

         Character( 132 ) :: xmsg
         Character(  32 ) :: pname = 'init_cloud_optics'
         Logical, Save    :: initialized = .false.
                
         If ( initialized ) Return
         
         initialized = .true.
         
         allocate( cloud_layering( nlays ),
     &             rel           ( nlays ),
     &             dge           ( nlays ), stat = allocstat )
         If( allocstat .ne. 0 )Then
           xmsg = 'Failure Allocating cloud_layering, rel, dge'
           call m3exit( pname, 0, 0, xmsg, xstat1 )
         End If

         max_dge     = maxi_diameter_ice-low
         max_dge_squ = max_dge*max_dge
         min_dge     = mini_diameter_ice+low
         
         cloud_layering = .true.
         
         allocate( cloud_liquid_ext ( nlays,nwl_ref ), 
     &             cloud_liquid_scat( nlays,nwl_ref ),
     &             cloud_liquid_ssa ( nlays,nwl_ref ),
     &             cloud_liquid_asy ( nlays,nwl_ref ), stat = allocstat )
     
         If( allocstat .ne. 0 )Then
           xmsg = 'Failure Allocating cloud_liquid_ext, cloud_liquid_scat, cloud_liquid_ssa, cloud_liquid_asy'
           call m3exit( pname, 0, 0, xmsg, xstat1 )
         End If

         cloud_liquid_ext  = 0.0
         cloud_liquid_scat = 0.0
         cloud_liquid_ssa  = 1.0
         cloud_liquid_asy  = 0.0          

         allocate( cloud_ice_ext ( nlays,nwl_ref ), 
     &             cloud_ice_scat( nlays,nwl_ref ),
     &             cloud_ice_ssa ( nlays,nwl_ref ),
     &             cloud_ice_del ( nlays,nwl_ref ),
     &             cloud_ice_asy ( nlays,nwl_ref ), stat = allocstat )
     
         If( allocstat .ne. 0 )Then
           xmsg = 'Failure Allocating cloud_ice_ext, cloud_ice_ext,cloud_ice_ssa, cloud_ice_del, ice_asy_rcld'
           call m3exit( pname, 0, 0, xmsg, xstat1 )
         End If
         
         cloud_ice_ext  = 0.0
         cloud_ice_scat = 0.0
         cloud_ice_ssa  = 1.0
         cloud_ice_asy  = 0.0

         allocate( cloud_aggreg_ext ( nlays,nwl_ref ), 
     &             cloud_aggreg_scat( nlays,nwl_ref ),
     &             cloud_aggreg_ssa ( nlays,nwl_ref ),
     &             cloud_aggreg_asy ( nlays,nwl_ref ), stat = allocstat )
     
         If( allocstat .ne. 0 )Then
           xmsg = 'Failure Allocating cloud_aggreg_ext, cloud_aggreg_ext,cloud_aggreg_ssa,'
     &         // 'cloud_aggreg_del, cloud_aggreg_asy'
           call m3exit( pname, 0, 0, xmsg, xstat1 )
         End If

         cloud_aggreg_ext  = cldmin
         cloud_aggreg_scat = cldtiny
         cloud_aggreg_ssa  = 1.0
         cloud_aggreg_asy  = 0.0

         allocate( cloud_ext_coeff( nlays,nwl_ref ), 
     &             cloud_scat_coef( nlays,nwl_ref ),
     &             cloud_asym_fact( nlays,nwl_ref ), stat = allocstat )
     
         If( allocstat .ne. 0 )Then
           xmsg = 'Failure Allocating cloud_ext_coeff, cloud_scat_coef, cloud_asy_fact'
           call m3exit( pname, 0, 0, xmsg, xstat1 )
         End If
         
         allocate( total_tau_cld( nwl_ref ), stat = allocstat )
     
         If( allocstat .ne. 0 )Then
           xmsg = 'Failure Allocating total_tau'
           call m3exit( pname, 0, 0, xmsg, xstat1 )
         End If

#ifdef phot_debug
         allocate( ave_asymm_cld( nwl_ref ),
     &             ave_ssa_cld( nwl_ref ),   stat = allocstat )
     
         If( allocstat .ne. 0 )Then
           xmsg = 'Failure Allocating ave_asymm, ave_ssa'
           call m3exit( pname, 0, 0, xmsg, xstat1 )
         End If
#endif         
         
         do i = 1, 32
            newtotalw( i ) = xk( i ) * xk( i ) * totalw( i )
         end do

         do i = 1, 16
            gauss_laguerre_total( i ) = gauss_laguerre_node( i )
     &                                * gauss_laguerre_node( i )
     &                                * gauss_laguerre_weight( i )
         end do

         end subroutine init_cloud_optics

!-----------------------------------------------------------------------
         subroutine general_effective_size( levels, t )
! Purpose: calculate the generalized effective size
!         of ice particle based on temperature. The routine
!         was adapted from WRF version 3.5 implementation of 
!         RRTMG
! returns the effect raduis, re,  of cloud ice particles, at
! temperature, t

            Implicit None

! Arguments:
            integer, intent(in)    :: levels  ! layers of process
            real,    intent(in)    :: t(:)    ! air temperaure, K
    
! Local:
            real corr
            integer i
            integer k
            integer index

            real, save :: retab(95)  ! look up table effective size, um, of ice particle versus 
                                    ! temperature. Values of re(T) are tabulated over temperature 
                                    ! interval 180 K -- 274 K; hexagonal columns assumed:
!
! The table comes from CAM version 4.0 and does not matched
! citation reference, equation (4) in 
! Kristjässon, J. E., J. M. Edwards, and D. L. Mitchell (1999),
! A new parameterization scheme for the optical properties of 
! ice crystals for use in general circulation models of the 
! atmosphere, Phys. Chem. Earth, B24, 231–236. 
! or 1030.7*EXP(0.05522*(Temp-279.5))
! Array's first value corresponds to Temp equals 186.1 K
! last temp correspond to Temp equal 253.8 K.

            data retab /
     &          5.92779, 6.26422, 6.61973, 6.99539, 7.39234,           ! 5
     &          7.81177, 8.25496, 8.72323, 9.21800, 9.74075, 10.2930,  ! 11
     &          10.8765, 11.4929, 12.1440, 12.8317, 13.5581, 14.2319,  ! 17
     &          15.0351, 15.8799, 16.7674, 17.6986, 18.6744, 19.6955,  ! 23
     &          20.7623, 21.8757, 23.0364, 24.2452, 25.5034, 26.8125,  ! 29
     &          27.7895, 28.6450, 29.4167, 30.1088, 30.7306, 31.2943,  ! 35
     &          31.8151, 32.3077, 32.7870, 33.2657, 33.7540, 34.2601,  ! 41
     &          34.7892, 35.3442, 35.9255, 36.5316, 37.1602, 37.8078,  ! 47
     &          38.4720, 39.1508, 39.8442, 40.5552, 41.2912, 42.0635,  ! 53
     &          42.8876, 43.7863, 44.7853, 45.9170, 47.2165, 48.7221,  ! 59
     &          50.4710, 52.4980, 54.8315, 57.4898, 60.4785, 63.7898,  ! 65
     &          65.5604, 71.2885, 75.4113, 79.7368, 84.2351, 88.8833,  ! 71
     &          93.6658, 98.5739, 103.603, 108.752, 114.025, 119.424,  ! 77
     &          124.954, 130.630, 136.457, 142.446, 148.608, 154.956,  ! 83
     &          161.503, 168.262, 175.248, 182.473, 189.952, 197.699,  ! 89
     &          205.728, 214.055, 222.694, 231.661, 240.971, 250.639/  ! 95

            do k = 1, levels
               If( t(k) .le. 179.0 )Then
                  index = 1
               Else If( t(k) .ge. 273.0 )Then
                  index = 94
               Else
                  index = max( int(t(k)-179.0), 1 )
               End If
               corr = t(k) - aint( t(k) )      ! temperatures of retab values differ by one degree K
               dge(k) = retab(index) + (retab(index+1)-retab(index))*corr

! Convert from effective radius to generalized effective size (*1.0315; Fu et al. 1996)
! but limit to upper bound in Fu et al. (1996) ice parameterization
               dge(k) = max( min( max_dge, 1.0315*dge(k) ), min_dge)
            end do

         return

         end subroutine general_effective_size

!-----------------------------------------------------------------------
         subroutine get_ice_optics( levels, t, iwc )
! Purpose calculate optical properties for ice particles
! Uses Fu (1996) parameterization for ice particle generalized effective size, dge, from 5 to 140 microns, 
! *** NOTE: Fu parameterization requires particle size in generalized effective size.
! and uses Ebert and Curry (1992) parameteriztion size, dge, >= 140 microns.
! *** NOTE: Transition between two methods has not been smoothed. 
! Algorithm adapted Rapid Radiative Model Global (RRTMG) version 3.8 and Weather Research Forecasting model
! (WRF) version 3.5

            USE UTILIO_DEFN         ! IO functions and parameters
            USE CSQY_DATA           ! number and value of wavelengths

            IMPLICIT NONE
! arguments:
            integer, intent( inout ) :: levels     ! layers to process
            real,    intent( inout ) :: t(:)       ! air temperaure, K
            real,    intent( in    ) :: iwc( : )   ! cloud ice water content, g/m3

! ice water coefficients (Ebert and Curry,1992, JGR, 97, 3831-3836)     
            real, save :: abari(4) = (/ 3.448e-03, 3.448e-03,3.448e-03,3.4480e-03/)
            real, save :: bbari(4) = (/ 2.431e+00, 2.431e+00,2.431e+00,2.4310e+00/)
            real, save :: cbari(4) = (/ 1.000e-05, 1.10e-04 ,1.861e-02,4.6658e-01/)
            real, save :: dbari(4) = (/ 0.000e+00, 1.405e-05,8.328e-04,2.0500e-05/)
            real, save :: ebari(4) = (/ 7.661e-01, 0.773e+00,0.794e+00,0.9595e+00/)
            real, save :: fbari(4) = (/ 5.851e-04, 5.665e-04,7.267e-04,1.0760e-04/)

! Local:
            real    factor
            real    fint
            real    forwice ! forward sccatering parameter
            integer iwl     ! loop counter
            integer layer   ! loop counter
            integer index
           
            character( 132 )      :: XMSG
            character( 16 ), save :: pname = 'GET_ICE_OPTICS'
           
            logical               :: error_flag

                 
            cloud_ice_ext ( 1:levels, 1:nwl_ref ) = cldmin
            cloud_ice_scat( 1:levels, 1:nwl_ref ) = cldtiny
            cloud_ice_ssa ( 1:levels, 1:nwl_ref ) = high
            cloud_ice_asy ( 1:levels, 1:nwl_ref ) = 0.0
            dge           ( 1:levels )            = 0.0

!           forall( layer = 1:levels )
!              dge( layer ) = 0.0
!              forall( iwl = 1:nwl_ref ) 
!                 cloud_liquid_ext ( layer, iwl ) = cldmin
!                 cloud_liquid_scat( layer, iwl ) = cldtiny
!                 cloud_liquid_ssa ( layer, iwl ) = high
!                 cloud_liquid_asy ( layer, iwl ) = 0.0
!              end forall 
!           end forall
              
            if( maxval( iwc ) .le. cldmin )return

            call general_effective_size( levels, t )

! Calculation of optical propeties due to ice particle
! Note that this loop structure may not be the most efficientbecuase the 
! inner loop use the farther right array index. The code does this because cycle 
! condition per layer may be more efficient than iterating over wavelength than 
! layer             

            error_flag = .false.
            
            do layer = 1, levels
               if( iwc( layer ) .le. cldmin )cycle
               do iwl = 1, nwl_ref
#ifdef phot_debug                                     
                  if (dge(layer) .lt. mini_diameter_ice)then
                      write(xmsg,*)Trim(pname)
     &                // ': ICE PARTICLE GENERALIZED EFFECTIVE SIZE OUT OF BOUNDS'
     &                // ' dge(', layer, ') = ', dge(layer),' um '
                      call m3mesg(xmsg)
                      error_flag = .true.
                  end if
#endif                   
                  if (dge(layer) .ge. mini_diameter_ice .and. dge(layer) .le. maxi_diameter_ice) then
                     factor = freq_diameter_ice * (dge(layer) - mini_diameter_ice)
                     index = int(factor)
                     fint = max( factor - float(index),0.0 )
                     index = min(  ndiameter_ice - 1, max( 1, index ) )
                     cloud_ice_ext(layer, iwl) =  ice_extinct(index,iwl) 
     &                                         +  fint * (ice_extinct(index+1,iwl) - ice_extinct(index,iwl))
                     cloud_ice_ssa(layer, iwl) =  1.0 - ice_coalbedo(index,iwl) 
     &                                         +  fint * (ice_coalbedo(index+1,iwl) - ice_coalbedo(index,iwl))
                     cloud_ice_asy(layer, iwl) =  ice_asymfact(index,iwl) 
     &                                         +  fint * (ice_asymfact(index+1,iwl) - ice_asymfact(index,iwl))
                     cloud_ice_del(layer, iwl) = ice_deltrans(index,iwl) 
     &                                         + fint * (ice_deltrans(index+1,iwl) - ice_deltrans(index,iwl))
#ifdef phot_debug                      
                     if (cloud_ice_del(layer, iwl) .gt. 1.0 .or. cloud_ice_del(layer, iwl) .lt. 0.0)then
                         write(xmsg, 99950)'ice particle delta function outside bounds and equals ', 
     &                   cloud_ice_del(layer, iwl),' at layer ', layer
                         call m3mesg(xmsg)
                         error_flag = .true.
                     end if 
#endif
                     if(cloud_ice_ssa(layer, iwl) .gt. low)then
                         forwice = cloud_ice_del(layer, iwl) + 0.5 / cloud_ice_ssa(layer, iwl)
                     else 
                         forwice = high
                     end if 

! See Fu (1996) p. 2067 
                     if (cloud_ice_asy(layer, iwl) .ge. 1.0) cloud_ice_asy(layer, iwl) = cloud_ice_asy(layer, iwl)-low
                     if (cloud_ice_asy(layer, iwl) .le. 0.0) cloud_ice_asy(layer, iwl) = cloud_ice_asy(layer, iwl)+low
                     if (forwice .gt. cloud_ice_asy(layer, iwl)) forwice = cloud_ice_asy(layer, iwl)

                  else if (dge(layer) .gt. maxi_diameter_ice) then
                     if (effwl_ref(iwl) .lt. 0.700) then
                        index = 1
                     else if (effwl_ref(iwl) .lt. 1.300) then
                        index = 2
                     else if (effwl_ref(iwl) .lt. 1.900) then
                        index = 3
                     else if (effwl_ref(iwl) .lt. 2.500) then
                        index = 4
                     else ! if (effwl_ref(iwl) .e. 3.500) then
                        index = 5
                     end if
                     cloud_ice_ext(layer, iwl) = abari(index) + bbari(index)/dge(layer) 
                     cloud_ice_ssa(layer, iwl) = 1.0 - cbari(index) - dbari(index)*dge(layer) 
                     cloud_ice_asy(layer, iwl) = ebari(index) + fbari(index)*dge(layer) 
                     if (cloud_ice_asy(layer, iwl) .ge. 1.0)cloud_ice_asy(layer, iwl) = 1.0-low
                     if (cloud_ice_asy(layer, iwl) .le. 0.0)cloud_ice_asy(layer, iwl) = cloud_ice_asy(layer, iwl)+low
                     forwice = cloud_ice_asy(layer, iwl)*cloud_ice_asy(layer, iwl)
                  end if

! adjust results for fraction of light in forward scattering peak from nonspheric particles
! see Appendix A in Fu (1996), equations A.2(a,b,c)
!!! temporary to check interpolation method
                  cloud_ice_ext(layer, iwl) = (1.0 - forwice*cloud_ice_ssa(layer, iwl)) 
     &                                      *  cloud_ice_ext(layer, iwl)
                  cloud_ice_ssa(layer, iwl) =  cloud_ice_ssa(layer, iwl)*(1.0 - forwice) 
     &                                      / (1.0 - forwice*cloud_ice_ssa(layer, iwl))
                  cloud_ice_asy(layer, iwl) = (cloud_ice_asy(layer, iwl) -  forwice)                 
     &                                      / (1.0 - forwice)
                  cloud_ice_del(layer, iwl) = forwice
! convert extinction coefficient to extinction per layer thickness
                  cloud_ice_ext(layer, iwl) = cloud_ice_ext(layer, iwl) * iwc(layer)
! calculate scattering per per layer thickness
                  cloud_ice_scat(layer, iwl) =  cloud_ice_ssa(layer, iwl) * cloud_ice_ext(layer, iwl)
#ifdef phot_debug                       
! Check to ensure all calculated quantities are within physical limits.
                  if (cloud_ice_ext(layer, iwl) .lt. 0.0)then
                     write(xmsg,99950)'ice particle extinction equals ', 
     &               cloud_ice_ext(layer, iwl), ' at layer = ',layer
                     error_flag = .true.
                  end if
                  if (cloud_ice_ssa(layer, iwl) .gt. high .or. cloud_ice_ssa(layer, iwl) .lt. low)then
                     write(xmsg, 99950)'ice particle SSA outside bounds and equals ', 
     &               cloud_ice_ssa(layer, iwl),' at layer ', layer
                     call m3mesg(xmsg)
!                    error_flag = .true.
                  end if 
                  if (cloud_ice_asy(layer, iwl) .gt. 1.0 .or. cloud_ice_asy(layer, iwl) .lt. -1.0)then
                     write(xmsg, 99950)'ice particle asymmetery factor outside bounds and equals ', 
     &               cloud_ice_asy(layer, iwl),' at layer ', layer
                     call m3mesg(xmsg)
                     error_flag = .true.
                  end if 
                  if (cloud_ice_scat(layer, iwl) .gt. cloud_ice_ext(layer, iwl))then
                     write(xmsg, 99950)'ice particle scattering greater than extinction, SSA ', 
     &               cloud_ice_ssa(layer, iwl),' at layer ', layer
                     call m3mesg(xmsg)
                     error_flag = .true.
                  end if                   
#endif
               end do
            end do

#ifdef phot_debug                       
            if( error_flag )then
               write(xmsg,99951)'Encountered the above Errors at COLUMN = ', col_cloud,
     &         ' and ROW = ', row_cloud
               call m3exit(pname,0,0,'Encountered the above Errors', XSTAT1 )
            end if
#endif
             
99950    format(a,es12.4,a,i3)
99951    format(a,i5,a,i5)
99962    format(a,1x,10(es12.4,1x),a,10(es12.4,1x))

         end subroutine get_ice_optics

!-----------------------------------------------------------------------
         subroutine relcalc( levels, t, owater_frac, icefrac, snowfrac )
!----------------------------------------------------------------------- 
! Purpose: 
! Compute effective radius of cloud water droplets
! Subroutine adapted WRF version 3.5 by Phil Rasch
! Method: 
! analytic formula following the formulation originally developed by J. T. Kiehl
! for CAM version 3.0 and 4.0 
!-----------------------------------------------------------------------
            Implicit None

! Arguments:
            integer, intent( in ) :: levels            ! layers to process
            real,    intent( in ) :: t( : )            ! Air Temperature, K
            real,    intent( in ) :: owater_frac       ! Open water fractional coverage
            real,    intent( in ) :: icefrac           ! Sea Ice fractional coverage
            real,    intent( in ) :: snowfrac          ! Snow fractional coverage

! Parameters:
            real, parameter :: tmelt     = 273.16 ! freezing temperature of fresh water (K)
            real, parameter :: rliqland  =  8.0   ! liquid drop size if over land
            real, parameter :: rliqocean = 14.0   ! liquid drop size if over ocean
            real, parameter :: rliqice   = 14.0   ! liquid drop size if over sea ice
             
            real, parameter :: del_land_ocean   = rliqocean - rliqland
!           real, parameter :: del_land_seaice  = rliqice - rliqland
             
            real, parameter :: sheight_factor = 1.0e-2 ! conversion factor for snow height
                                                       ! equal 0.001 in rrtmg implementation in WRF 3.5
                                                       ! times 10.0 factor in original relcalc subroutine
! variables:
            integer         :: k                    ! loop counter
            real            :: snowh                ! snow height
                                 
!           snowh = sheight_factor * snowfrac 
              
            forall( k=1:levels ) ! effective radius algorithm
! Start with temperature-dependent value appropriate for continental air
               rel(k) = rliqland + del_land_ocean * min(1.0,max(0.0,0.05*(tmelt-t(k))))
 ! Ramp up for snow frac over land; uses fill increase if snow_frac is 100%
               rel(k) = rel(k) + (rliqocean-rel(k)) * min(1.0, snowfrac)  ! min(1.0,max( 0.0, snowh))
 ! Ramp up between polluted value over land to clean value over ocean.
               rel(k) = rel(k) + (rliqocean-rel(k)) * min(1.0,max( 0.0, owater_frac))
 ! Ramp up between the resultant value and a sea ice value in the presence of ice.
               rel(k) = rel(k) + (rliqice-rel(k))   * min(1.0,max( 0.0, icefrac ))
            end forall
              
         end subroutine relcalc            

!-----------------------------------------------------------------------
         subroutine get_droplet_optics( levels, t, owater_frac, icefrac, snowfrac, lwc )

            USE UTILIO_DEFN         ! IO functions and parameters
            USE CSQY_DATA           ! number and value of wavelengths
            
            Implicit None
! Agruments:
            integer, intent( inout ) :: levels            ! layers to process
            real,    intent( inout ) :: t( : )            ! Air Temperature, K
            real,    intent( inout ) :: owater_frac       ! Open water fractional coverage
            real,    intent( inout ) :: icefrac           ! Sea Ice fractional coverage
            real,    intent( inout ) :: snowfrac          ! Snow fractional coverage
            real,    intent( in )    :: lwc( : )          ! cloud liquid water content, g/m3

! local:
            real    :: radliq
            real    :: fint
            integer :: index
            integer :: iwl
            integer :: layer

            character( 132 )      :: XMSG
            character( 32 ), save :: pname = 'GET_DROPLET_OPTICS'

            logical               :: error_flag

            error_flag = .false.
 
!           forall( layer = 1:levels )
!              rel( layer ) = 0.0
!              forall( iwl = 1:nwl_ref ) 
!                 cloud_liquid_ext ( layer, iwl ) = cldmin
!                 cloud_liquid_scat( layer, iwl ) = cldtiny
!                 cloud_liquid_ssa ( layer, iwl ) = high
!                 cloud_liquid_asy ( layer, iwl ) = 0.0
!              end forall 
!           end forall

            cloud_liquid_ext ( 1:levels, 1:nwl_ref ) = cldmin
            cloud_liquid_scat( 1:levels, 1:nwl_ref ) = cldtiny
            cloud_liquid_ssa ( 1:levels, 1:nwl_ref ) = high
            cloud_liquid_asy ( 1:levels, 1:nwl_ref ) = 0.0
             
            if( maxval(lwc) .le. cldmin )return
             
            call relcalc( levels, t, owater_frac, icefrac, snowfrac )
             
! Calculation of optical coefficients due to water clouds droplets
! Note that this loop structure may not be the most efficientbecuase the 
! inner loop use the farther right array index. The code does this because cycle 
! condition per layer may be more efficient than iterating over wavelength than 
! layer
            error_flag = .false.
             
            do layer = 1, levels
               if( lwc( layer ) .le. cldmin )cycle
               do iwl = 1, nwl_ref
                  radliq = rel(layer)
#ifdef phot_debug                    
                  if (radliq .lt. mini_radius_liquid .or. radliq .gt. maxi_radius_liquid)then
                     write(xmsg, 99950)'liquid effective radius outside bounds and equals ', radliq,
     &               ' um at layer ', layer
                     call m3mesg(xmsg)
                     error_flag = .true.
                  end if
#endif                    
                  index = int(radliq - init_radius_liquid)
                  if (index .le. 0) index = 1
                  if (index .ge. nradius_liquid) index = nradius_liquid - 1
                    
                  fint = max( freq_radius_liquid*(radliq - init_radius_liquid - real(index)), 0.0)
     
                  cloud_liquid_ext(layer, iwl) = liquid_extinct(index,iwl) 
     &                                         + fint * (liquid_extinct(index+1,iwl) - liquid_extinct(index,iwl))
     
                  cloud_liquid_ssa(layer, iwl) = 1.0 - liquid_coalbedo(index,iwl) 
     &                                         + fint * (liquid_coalbedo(index+1,iwl) - liquid_coalbedo(index,iwl))
                  if (cloud_liquid_ssa(layer, iwl) .le. 0.0)then
                     cloud_liquid_ssa(layer, iwl) = cloud_liquid_ssa(layer, iwl) + low
                  else if(cloud_liquid_ssa(layer, iwl) .ge. 1.0)then
                     cloud_liquid_ssa(layer, iwl) = cloud_liquid_ssa(layer, iwl) - low
                  end if
                  cloud_liquid_asy(layer, iwl) = liquid_asymfact(index,iwl) 
     &                                         + fint * (liquid_asymfact(index+1,iwl) - liquid_asymfact(index,iwl))

!                 forwliq(iwl) = cloud_liquid_asy(ig)*cloud_liquid_asy(iwl)

! convert extinction coefficient into extinction per layer 
                  cloud_liquid_ext(layer, iwl)  = cloud_liquid_ext(layer, iwl) * lwc(layer)
                    
! calculate scattering per layer
                  cloud_liquid_scat(layer, iwl) = cloud_liquid_ssa(layer, iwl) * cloud_liquid_ext(layer, iwl)
#ifdef phot_debug                    
! Check to ensure all calculated quantities are within physical limits.
                  if (cloud_liquid_ext(layer, iwl) .lt. 0.0)then
                     write(xmsg,99950)'cloud droplet extinction equals ', 
     &               cloud_liquid_ext(layer, iwl), ' at layer = ',layer
                     error_flag = .true.
                  end if
                  if (cloud_liquid_ssa(layer, iwl) .gt. high .or. cloud_liquid_ssa(layer, iwl) .lt. low)then
                     write(xmsg, 99950)'liquid cloud droplet SSA outside bounds and equals ', 
     &               cloud_liquid_ssa(layer, iwl),' at layer ', layer
                     call m3mesg(xmsg)
!                    error_flag = .true.
                  end if 
                  if (cloud_liquid_scat(layer, iwl) .gt. cloud_liquid_ext(layer, iwl))then
                     write(xmsg, 99950)'cloud droplet scattering greater than extinction, SSA = ', 
     &               cloud_liquid_ssa(layer, iwl),' at layer ', layer
                     call m3mesg(xmsg)
                     error_flag = .true.
                  end if 
                  if (cloud_liquid_asy(layer, iwl) .gt. 1.0 .or. cloud_liquid_asy(layer, iwl) .lt. -1.0)then
                     write(xmsg, 99950)'liquid cloud droplet asymmetery factor outside bounds and equals ', 
     &               cloud_liquid_asy(layer, iwl),' at layer ', layer
                     call m3mesg(xmsg)
                     error_flag = .true.
                  end if 
#endif                    
               end do
            end do
#ifdef phot_debug                                            
            if( error_flag )then
               write(xmsg,99951)'Encountered the above Errors at COLUMN = ', col_cloud,
     &         ' and ROW = ', row_cloud
               call m3exit(pname,0,0,'Encountered the above Errors', XSTAT1 )
            end if
#endif
           
99950       format(a,es12.4,a,i3)
99951       format(a,i5,a,i5)
         end subroutine get_droplet_optics

!-----------------------------------------------------------------------
         subroutine aggreg_size_effective(hydro_type, q, reff, nlayers)

!---------------------------------------------------------------------------
! Purpose: compute effective radius of cloud water and ice aggregegates:
!         rain droplets, snowflakes and graupel from water liquid and ice
!         
! METHOD: 
!   assume exponential particle size distribution and spherical particles
!                     use Gauss-Laguerre Quadrature for integration
! HISTORY: 08/15/2014: B.Hutzell adapted from NCAR CAM model version 3 
!---------------------------------------------------------------------------

            implicit none
!..Includes:
!           INCLUDE SUBST_CONST     ! CMAQ constants

!...Arguments:
            integer, intent(in)  :: hydro_type        ! aggregegate to calculate
            real,    intent(in)  :: q   ( : )         ! aggregegate mixing ratio, g/m3
            real,    intent(out) :: reff( : )         ! effective radius, um
            integer, intent(in)  :: nlayers             ! # of layers

! constants  
! values for n0 values taken from default column of Table 1. in Wainwright et. al (2014)
! J. of Appl. Meteo. Climat., vol 53. pp 2072.
            real( 8 ), parameter :: n0_rain  =    0.08D0    ! cm(-4)
            real( 8 ), parameter :: n0_snow  =    0.03D0    ! cm(-4)
            real( 8 ), parameter :: n0_grau  =    0.005D0   ! cm(-4)
            real( 8 ), parameter :: rho_rain = 1000.0D0     ! kg m(-3)
            real( 8 ), parameter :: rho_snow =  100.0D0     ! kg m(-3)
            real( 8 ), parameter :: rho_grau =  400.0D0     ! kg m(-3)

            real( 8 ), parameter :: chi_rain = -4.47806054D+01 ! -2.0*(1.0e+3*pi*rho_rain*n0_rain)**0.25, (cm-4*g/m3)**.25
            real( 8 ), parameter :: chi_snow = -1.97059682D+01 ! -2.0*(1.0e+3*pi*rho_snow*n0_snow)**0.25, (cm-4*g/m3)**.25
            real( 8 ), parameter :: chi_grau = -1.78063523D+01 ! -2.0*(1.0e+3*pi*rho_grau*n0_grau)**0.25, (cm-4*g/m3)**.25
   
!!!!!!!!!!! real, parameter :: limit = 1.0E-10 ! value of q where calculation converges to a lower limit
            real, parameter :: limit = 1.0E-04 ! value of q where calculation converges to a upper limit

            real, parameter :: dmin_snow = 887.873       ! lower convergence results for snow, um 
            real, parameter :: dmin_grau = dmin_snow     ! lower convergence results for graupel, um 
            real, parameter :: dmin_rain = 0.5*dmin_snow ! lower convergence results for rain, um 

!  local variables
            integer   :: lay, nk
            real( 8 ) :: rho_hydro
            real( 8 ) :: sum1, sum2 
            real( 8 ) :: lamda
            real( 8 ) :: n0
            real( 8 ) :: chi
            real( 8 ) :: comp
            real( 8 ) :: psd      ! partical size distribution
            real( 8 ) :: argument
            real      :: factor
            real      :: dmin  ! value if q .le. limit

! initialize
!
! cloud rain/snow/graupel effective radius
!
            if ( maxval( q ) .le. cldmin )then
               reff( 1:nlayers )  = min_dge
               return
            end if
               select case ( hydro_type )
               case( 1 )
                  rho_hydro =  rho_rain
                  n0        =  n0_rain
                  chi       =  chi_rain
                  factor    =  1.0e+4
                  dmin      =  dmin_rain
               case( 2 )
                  rho_hydro =  rho_snow
                  n0        =  n0_snow
                  chi       =  chi_snow
                  factor    =  2.0e+4
                  dmin      =  dmin_snow
               case( 3 )
                  rho_hydro =  rho_grau
                  n0        =  n0_grau
                  chi       =  chi_grau
                  factor    =  2.0e+4
                  dmin      =  dmin_snow
               case default
                  reff( 1:nlayers )  = min_dge
                  return
               end select
               
            do lay = 1, nlayers
!              lamda = (1.0e+3*pi*rho_hydro*n0/q(lay))**0.25
               if( q(lay) .le.  limit )then
                  reff(lay) = dmin
                  cycle
               end if
               lamda = chi*(1.0D0/real( q(lay), 8))**0.25D0
               sum1 = 0.0D0
               sum2 = 0.0D0
!original method used thirty-two nodes
               do nk = 1, 32
                  argument = lamda*xk(nk)
                  if( argument .lt. cloud_log_smallest ) cycle ! assume dexp( argument ) equals zero
                  psd = n0*dexp( argument ) 
                  comp = newtotalw(nk) * psd
                  sum2 = sum2 + comp 
                  sum1 = sum1 + xk(nk)*comp
! reff results sixteen point seem off from thirty two points up to a factor of two
!              do nk = 1, 16
!                 psd  = n0*exp(lamda*gauss_laguerre_node(nk))
!                 comp = gauss_laguerre_total(nk) * psd
!                 sum2 = sum2 + comp 
!                 sum1 = sum1 + gauss_laguerre_node(nk)*comp
               end do
               if( sum2 .lt. cloud_smallest )then
                 reff(lay) = dmin
               else
                 reff(lay) = factor * real( sum1/sum2, 4 )    ! microns
               end if
            end do

         end subroutine aggreg_size_effective

!-----------------------------------------------------------------------
         subroutine get_aggregate_optics( levels, rwc, swc, gwc )
! Purpose calculate optical properties for aggregates: combined rain droplet, snowflakes and graupel
! Uses Fu (1996) parameterization for ice particle generalized effective size, dge, from 5 to 140 microns, 
! Algorithm adapted Rapid Radiative Model Global (RRTMG) version 3.9 and Weather Research Forecasting model
! (WRF) version 3.6

            use VGRD_DEFN, ONLY : NLAYS
            USE UTILIO_DEFN         ! IO functions and parameters
            USE CSQY_DATA           ! number and value of wavelengths

            IMPLICIT NONE
! arguments:
            integer, intent( inout ) :: levels     ! layer to processes
            real,    intent( inout ) :: rwc( : )   ! rain water content, g/m3
            real,    intent( inout ) :: swc( : )   ! snowflake content, g/m3
            real,    intent( inout ) :: gwc( : )   ! graupel content, g/m3

! Local:
            real     :: factor
            real     :: fint
            real     :: forwice                ! forward sccatering parameter
            real     :: reff_rain( nlays )     ! effective radius of rain droplet, um
            real     :: deff_snow( nlays )     ! effective diameter of snowflakes, um
            real     :: deff_graupel( nlays )  ! effective diameter of graupel, um
            
            real     :: deff_hydro( nlays )    ! effective diameter for unmodified hydrometeor, um
            real     :: hydro_content( nlays ) ! unmodified hydrometeor content, g/m3

            real     :: rain_ext               ! rain droplet extinction coefficient, 1/m
            real     :: rain_ssa               ! rain droplet sing scattering albedo
            real     :: rain_scat              ! rain droplet scattering coefficient, 1/m
            real     :: rain_asy               ! rain droplet asymmetry factor

            real     :: snow_ext               ! snowflake extinction coefficient, 1/m
            real     :: snow_ssa               ! snowflake sing scattering albedo
            real     :: snow_scat              ! snowflake scattering coefficient, 1/m
            real     :: snow_asy               ! snowflake asymmetry factor

            real     :: graupel_ext            ! graupel extinction coefficient, 1/m
            real     :: graupel_ssa            ! graupel sing scattering albedo
            real     :: graupel_scat           ! graupel scattering coefficient, 1/m
            real     :: graupel_asy            ! graupel asymmetry factor

            real     :: snow_del                ! snowflake delta forward tranmission function
            real     :: graupel_del             ! graupel delta forward tranmission function

            integer  :: iwl                    ! loop counter
            integer  :: layer                  ! loop counter
            integer  :: index
            
            character( 132 )       :: XMSG
            character(  32 ), save :: pname = 'GET_AGGREGATE_OPTICS'
            
            logical  :: error_flag
            logical  :: normalize

! for rain droplets used simple parameterization in Goddard Radiation Model

            normalize = .false.
            error_flag = .false.
           
! initialize optical properties to minimums
            forall( layer = 1:levels, iwl = 1:nwl_ref )
               cloud_aggreg_ext ( layer,iwl )  = cldmin
               cloud_aggreg_scat( layer,iwl )  = cldtiny
               cloud_aggreg_asy ( layer,iwl )  = 0.0
               cloud_aggreg_ssa ( layer,iwl )  = high
            end forall
!           cloud_aggreg_ext ( 1:levels,1:nwl_ref ) = cldmin
!           cloud_aggreg_scat( 1:levels,1:nwl_ref ) = cldtiny
!           cloud_aggreg_ssa ( 1:levels,1:nwl_ref ) = high
!           cloud_aggreg_asy ( 1:levels,1:nwl_ref ) = 0.0

           
            if( maxval( rwc ) .gt. cldmin )then
!!!!!!!!!      call aggreg_size_effective( 1, rwc, reff_rain, levels)

!Parameterization for rain droplets is taken from Goddard Space Flight Radiation Model
!in WRF version 3.6. Their derivation is discussed in Chou and Suarez (1999),
! A Solar Radiation Parameterization for Atmospheric Studies, NASA/TM-1999-104606,
!Vol. 15, pages 17-20.

               rain_ext  =  3.0e-3
               rain_ssa  =  high
               rain_scat =  rain_ssa*rain_ext
               rain_asy  =  0.883
               do layer = 1, levels
                  if( rwc( layer ) .le. cldmin )cycle
                  forall ( iwl = 1:nwl_ref )
                     cloud_aggreg_ext (layer,iwl) = rain_ext  * rwc(layer)
                     cloud_aggreg_scat(layer,iwl) = rain_scat * rwc(layer)
                     cloud_aggreg_asy (layer,iwl) = rain_asy * cloud_aggreg_scat(layer, iwl)
                     cloud_aggreg_ssa (layer, iwl) = rain_ssa
                  end forall
               end do
               normalize = .true.
            end if
            
            if( maxval( swc ) .gt. cldmin )then

               call aggreg_size_effective( 2, swc, deff_snow, levels)

               do layer = 1, levels
                  if( swc( layer ) .le. cldmin )cycle
                  deff_hydro(layer)    = deff_snow(layer)
! correct the snowflake effective size to be within maxi and min parameters then
! updated concentrations. Latter step is taken from the RRTMG code version 3.9 so Fu (1996) can be
! used for optical properties.
                  if( deff_snow( layer ) .ge. maxi_diameter_ice )then
                      hydro_content( layer ) = swc( layer ) * max_dge_squ
     &                                       / (deff_snow( layer )*deff_snow( layer ))
                  else
                      hydro_content(layer)   = swc(layer)
                  end if
                  deff_snow( layer ) = max( min( deff_snow( layer ), max_dge ), min_dge )
                  do iwl = 1, nwl_ref                 
                     factor = freq_diameter_ice * (deff_snow(layer) - mini_diameter_ice)
                     index = int(factor)
                     fint = max( factor - float(index),0.0 )
                     index = min(  ndiameter_ice - 1, max( 1, index ) )
                     snow_ext  =  ice_extinct(index,iwl) 
     &                         +  fint * (ice_extinct(index+1,iwl) - ice_extinct(index,iwl))
                     snow_ssa  =  1.0 - ice_coalbedo(index,iwl) 
     &                         +  fint * (ice_coalbedo(index+1,iwl) - ice_coalbedo(index,iwl))
                     snow_asy  =  ice_asymfact(index,iwl) 
     &                         +  fint * (ice_asymfact(index+1,iwl) - ice_asymfact(index,iwl))
                     snow_del  = ice_deltrans(index,iwl) 
     &                         + fint * (ice_deltrans(index+1,iwl) - ice_deltrans(index,iwl))
#ifdef phot_debug                     
                     if (snow_del .gt. 1.0 .or. snow_del .lt. 0.0)then
                         write(xmsg, 99960)'snowflake delta function outside bounds and equals ', 
     &                   snow_del,' at layer ', layer
                         call m3mesg(xmsg)
                         error_flag = .true.
                     end if 
!                    if( iwl .eq. 1 .and. row_cloud .eq. 1 .and. col_cloud .eq. 1 )then
!                        write(log_cloud_optics,99962)'hdc,deff_snow,snow_ext,snow_asy,ssa,del = ',
!     &                  hydro_content( layer ),deff_snow(layer),snow_ext,snow_asy,
!     &                  snow_ssa, snow_del
!                    end if
#endif                     

                     if(snow_ssa .gt. low)then
                         forwice = snow_del + 0.5 / snow_ssa
                     else 
                        forwice = high
                     end if 
! See Fu (1996) p. 2067 
                     if (snow_asy .ge. 1.0) snow_asy = snow_asy-low
                     if (snow_asy .le. 0.0) snow_asy = snow_asy+low
                     if (forwice .gt. snow_asy) forwice = snow_asy
! adjust results for fraction of light in forward scattering peak from nonspheric particles
! see Appendix A in Fu (1996), equations A.2(a,b,c)
                     snow_ext = (1.0 - forwice*snow_ssa) *  snow_ext
                     snow_ssa =  snow_ssa*(1.0 - forwice) / (1.0 - forwice*snow_ssa)
                     snow_asy = (snow_asy -  forwice) / (1.0 - forwice)
                     snow_del = forwice
! calculate extinction and scattering coefficients per layer
                     snow_ext  =  snow_ext * hydro_content(layer)
                     snow_scat =  snow_ssa * snow_ext
#ifdef phot_debug                      
! Check to ensure all calculated quantities are within physical limits.
                     if (snow_ext .lt. 0.0 .or. snow_ext .ne. snow_ext )then
                        write(xmsg,99960)'snowflake extinction equals ', 
     &                  snow_ext, ' at layer = ',layer
                        error_flag = .true.
                     end if
                     if (snow_ssa .gt. high .or. snow_ssa .lt. low .or. snow_ssa .ne. snow_ssa )then
                        write(xmsg, 99960)'snowflake SSA outside bounds and equals ', 
     &                  snow_ssa,' at layer ', layer
                        call m3mesg(xmsg)
!                       error_flag = .true.
                     end if 
                     if (snow_asy .ge. 1.0 .or. snow_asy .le. -1.0 .or. snow_asy .ne. snow_asy )then
                         write(xmsg, 99960)'snowflake asymmetery factor outside bounds and equals ', 
     &                   snow_asy,' at layer ', layer
                         call m3mesg(xmsg)
                         error_flag = .true.
                     end if 
                     if (snow_scat .gt. snow_ext)then
                         write(xmsg, 99960)'snowflake scattering greater than extinction, SSA ', 
     &                   snow_ssa,' at layer ', layer
                         call m3mesg(xmsg)
                         error_flag = .true.
                     end if                      
!                    if( iwl .eq. 1 .and. row_cloud .eq. 1 .and. col_cloud .eq. 1 )then
!                        write(log_cloud_optics,99962)'swc,deff_hydro,snow_ext,snow_scat,snow_asyssa,del = ',
!     &                  swc( layer ),deff_hydro(layer),snow_ext/hydro_content(layer),snow_scat/hydro_content(layer),snow_asy,
!     &                  snow_ssa, snow_del
!                    end if
#endif
                     cloud_aggreg_ext(layer, iwl)  = cloud_aggreg_ext(layer, iwl)  + snow_ext
                     cloud_aggreg_scat(layer, iwl) = cloud_aggreg_scat(layer, iwl) + snow_scat
                     cloud_aggreg_asy(layer, iwl)  = cloud_aggreg_asy(layer, iwl)  + (snow_asy*snow_scat)
#ifdef phot_debug                                          
!             if( abs( cloud_aggreg_asy(layer, iwl) ) .ge. cloud_aggreg_scat(layer, iwl) )then
!                 write(log_cloud_optics,99962)
!     &           'swc,deff_hydro,snow_ext,snow_scat,snow_asy * snow_scat, ssa,del,  cloud_aggreg_asy = ',
!     &           swc( layer ),deff_hydro(layer),snow_ext,snow_scat,(snow_asy*snow_scat),
!     &           snow_ssa, snow_del, cloud_aggreg_asy(layer, iwl), cloud_aggreg_scat(layer, iwl)
!             end if
#endif
                  end do
               end do
               normalize = .true.
            end if

            if( maxval( gwc ) .gt. cldmin )then

               call aggreg_size_effective( 3, gwc, deff_graupel, levels)

               do layer = 1, levels
                  if( gwc( layer ) .le. cldmin )cycle                     
                  deff_hydro(layer) = deff_graupel(layer)
! correct effective size to be within maxi and min parameters then
! updated concentrations. Latter step is taken from the RRTMG code 
! version 3.9 so Fu (1996) can be used for optical properties.
                  if( deff_graupel( layer ) .ge. maxi_diameter_ice )then
                     hydro_content( layer ) = gwc( layer ) * max_dge_squ
     &                                      / (deff_graupel( layer )*deff_graupel( layer )) 
                  else
                     hydro_content(layer)   = gwc(layer)                    
                  end if
                  deff_graupel( layer ) = max( min( deff_graupel( layer ), max_dge ), min_dge )
                  do iwl = 1, nwl_ref
                     factor = freq_diameter_ice * (deff_graupel(layer) - mini_diameter_ice)
                     index = int(factor)
                     fint = max( factor - float(index), 0.0 )
                     index = min(  ndiameter_ice - 1, max( 1, index ) )
                     graupel_ext  =  ice_extinct(index,iwl) 
     &                            +  fint * (ice_extinct(index+1,iwl) - ice_extinct(index,iwl))
                     graupel_ssa  =  1.0 - ice_coalbedo(index,iwl) 
     &                            +  fint * (ice_coalbedo(index+1,iwl) - ice_coalbedo(index,iwl))
                     graupel_asy  =  ice_asymfact(index,iwl) 
     &                            +  fint * (ice_asymfact(index+1,iwl) - ice_asymfact(index,iwl))
                     graupel_del  = ice_deltrans(index,iwl) 
     &                            + fint * (ice_deltrans(index+1,iwl) - ice_deltrans(index,iwl))
#ifdef phot_debug                     
                     if (graupel_del .gt. 1.0 .or. graupel_del .lt. 0.0)then
                         write(xmsg, 99960)'graupel delta function outside bounds and equals ', 
     &                   graupel_del,' at layer ', layer
                         call m3mesg(xmsg)
                         error_flag = .true.
                     end if 
!                    if( iwl .eq. 1 .and. row_cloud .eq. 1 .and. col_cloud .eq. 1 )then
!                        write(log_cloud_optics,99962)'hdc,deff_graupel,graupel_ext,graupel_asy,ssa,del = ',
!     &                  hydro_content( layer ),deff_graupel(layer),graupel_ext,graupel_asy,
!     &                  graupel_ssa, graupel_del
!                    end if
#endif

                     if(graupel_ssa .gt. low)then
                         forwice = graupel_del + 0.5 / graupel_ssa
                     else 
                         forwice = high
                     end if 

! See Fu (1996) p. 2067 
                     if (graupel_asy .ge. 1.0) graupel_asy = graupel_asy-low
                     if (graupel_asy .le. 0.0) graupel_asy = graupel_asy+low
                     if (forwice .gt. graupel_asy) forwice = graupel_asy
! adjust results for fraction of light in forward scattering peak from nonspheric particles
! see Appendix A in Fu (1996), equations A.2(a,b,c)
!!! temporary to check interpolation method
                     graupel_ext = (1.0 - forwice*graupel_ssa) *  graupel_ext
                     graupel_ssa =  graupel_ssa*(1.0 - forwice) / (1.0 - forwice*graupel_ssa)
                     graupel_asy = (graupel_asy -  forwice) / (1.0 - forwice)
                     graupel_del = forwice
! calculate extinction and scattering coefficients per layer
                     graupel_ext  =  graupel_ext * hydro_content(layer)
                     graupel_scat =  graupel_ssa * graupel_ext
#ifdef phot_debug                     
! Check to ensure all calculated quantities are within physical limits.
                     if (graupel_ext .lt. 0.0 .or. graupel_ext .ne. graupel_ext )then
                        write(xmsg,99960)'graupel extinction equals ', 
     &                  graupel_ext, ' at layer = ',layer
                        error_flag = .true.
                     end if
                     if (graupel_ssa .gt. high .or. graupel_ssa .lt. low .or. graupel_ssa .ne. graupel_ssa )then
                        write(xmsg, 99960)'graupel SSA outside bounds and equals ', 
     &                  graupel_ssa,' at layer ', layer
                        call m3mesg(xmsg)
!                       error_flag = .true.
                      end if 
                      if (graupel_asy .gt. 1.0 .or. graupel_asy .lt. -1.0 .or. graupel_asy .ne. graupel_asy )then
                         write(xmsg, 99960)'graupel asymmetery factor outside bounds and equals ', 
     &                   graupel_asy,' at layer ', layer
                         call m3mesg(xmsg)
                         error_flag = .true.
                     end if 
!                    if( iwl .eq. 1 .and. row_cloud .eq. 1 .and. col_cloud .eq. 1 )then
!                       write(log_cloud_optics,99962)'gwc,deff_hydro,graupel_ext,graupel_scat,graupel_asy,ssa,del = ',
!     &                 gwc( layer ),deff_hydro(layer),graupel_ext/hydro_content(layer),graupel_scat/hydro_content(layer),
!     &                 graupel_asy,graupel_ssa, graupel_del
!                    end if
#endif
                     cloud_aggreg_ext(layer, iwl)  = cloud_aggreg_ext(layer, iwl)  + graupel_ext
                     cloud_aggreg_scat(layer, iwl) = cloud_aggreg_scat(layer, iwl) + graupel_scat
                     cloud_aggreg_asy(layer, iwl)  = cloud_aggreg_asy(layer, iwl)  + (graupel_asy*graupel_scat)
#ifdef phot_debug                     
                     if( abs( cloud_aggreg_asy(layer, iwl) ) .gt. cloud_aggreg_scat(layer, iwl) )then
                        write(logdev,99962)
     &                  'gwc,deff_hydro,graupel_ext,graupel_scat,graupel_asy*graupel_scat,ssa,del, cloud_aggreg_asy = ',
     &                  gwc( layer ),deff_hydro(layer),graupel_ext,graupel_scat,(graupel_asy*graupel_scat),
     &                  graupel_ssa, graupel_del, cloud_aggreg_asy(layer, iwl), cloud_aggreg_scat(layer, iwl)
                     end if
#endif                     
                  end do
               end do
               normalize = .true.                
            end if   
     
! computed average properties: single scattering albedo and asymmetery factor
            if( .Not. normalize )RETURN
            
            forall( layer = 1:levels, iwl = 1:nwl_ref )
               cloud_aggreg_ssa(layer,iwl) = cloud_aggreg_scat(layer,iwl)/cloud_aggreg_ext(layer,iwl)
               cloud_aggreg_asy(layer,iwl) =  cloud_aggreg_asy(layer,iwl)/cloud_aggreg_scat(layer,iwl)
            end forall
#ifdef phot_debug                                  
!           do layer = 1, nlays
!              do iwl = 1,nwl_ref
!                 if( abs( cloud_aggreg_asy(layer, iwl) ) .ge. 1.0 )then 
!                     write(log_cloud_optics,99964)
!     &               'layer, iwln cloud_aggreg_scat, cloud_aggreg_scat, cloud_aggreg_ssa, cloud_aggreg_asy = ',
!     &               layer, iwl, cloud_aggreg_ext(layer, iwl), cloud_aggreg_scat(layer, iwl), cloud_aggreg_ssa(layer, iwl), 
!     &               cloud_aggreg_asy(layer, iwl)
!                     error_flag = .true.
!                 end if
!              end do
!           end do
            if( error_flag )then
               write(xmsg,99961)'Encountered the above Errors at COLUMN = ', col_cloud,
     &         ' and ROW = ', row_cloud
               call m3exit(pname,0,0,'Encountered the above Errors', XSTAT1 )
            end if
#endif           

99960       format(a,es12.4,a,i3)
99961       format(a,i5,a,i5)
99962       format(a,1x,10(es12.4,1x),a,10(es12.4,1x))
99964       format(a,2(1x,i3),1x,10(es12.4,1x),a,10(es12.4,1x))
         end subroutine get_aggregate_optics

!-----------------------------------------------------------------------
         subroutine clear_hydrometeor_optics()
            implicit none
!Purpose clear values for hydrometeor optical properties
!Arguments: None
            cloud_liquid_ext  = 0.0
            cloud_liquid_scat = 0.0
            cloud_liquid_ssa  = 1.0
            cloud_liquid_asy  = 0.0          
            cloud_ice_ext     = 0.0
            cloud_ice_scat    = 0.0
            cloud_ice_ssa     = 1.0
            cloud_ice_asy     = 0.0
            cloud_aggreg_ext  = cldmin
            cloud_aggreg_scat = cldtiny
            cloud_aggreg_ssa  = 1.0
            cloud_aggreg_asy  = 0.0              
         end subroutine clear_hydrometeor_optics

      end module cloud_optics   
