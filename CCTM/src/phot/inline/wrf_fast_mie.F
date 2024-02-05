
       module wrf_fast_mie
       
       integer, parameter :: lunerr = -1

       REAL( 8 ), PUBLIC :: CPU_TIME_DELTA1 = 0.0D0 ! Timing individual
       REAL( 8 ), PUBLIC :: CPU_TIME_DELTA2 = 0.0D0 ! Timing individual
       REAL( 8 ), PUBLIC :: CPU_TIME_DELTA3 = 0.0D0 ! Timing individual
       
       public fast_mieaer_modal, aero_optics_tabular
       
!      integer, private :: logdev

       real(8),parameter,private :: pii = 3.14159265358979323846264338327950288419716939937510582097494D0
       real,   parameter,private :: pi  = 3.14159265
       real,   parameter,private :: sqrtpi = pi**(0.5) 
       real,   parameter,private :: sqrtpi1 = 1.0 / sqrtpi
       real,   parameter,private :: sqrt2   = 2.0**(0.5)
       real,   parameter,private :: three_pi_two = 3.0 * pi / 2.0 

      Logical, Parameter, Private :: Use_Odd_Quadrature = .True.
       Integer, Parameter, Private :: Quadrature_Points = 3
!      Integer, Parameter, Private :: Quadrature_Points = 1
      
!B.Hutzell One point quadature IGH = 1

       real, parameter, Private :: ghxi_1(1) = 0.00000000000
       real, parameter, Private :: ghwi_1(1) = 1.77245385091
       
!B.Hutzell Three point quadature IGH = 3
       real, parameter, Private :: ghxi_3(3) = (/ -1.22474487139, 
     &                                     0.00000000000,   
     &                                     1.22474487139 /)

       real, parameter, Private :: ghwi_3(3) = (/ 0.295408975151,  
     &                                    1.181635900000,  
     &                                    0.295408975151 /)
                                         
!B.Hutzell Five point quadature IGH = 5
       real(8), parameter, Private :: ghxi_5(5) = (/ -2.02018287046d0,  
     &                                       -0.958572464614d0, 
     &                                        0.00000000000d0,  
     &                                        0.958572464614d0, 
     &                                        2.02018287046d0 /)

       real(8), parameter, Private :: ghwi_5(5) = (/ 0.019953242059d0,   
     &                                       0.393619323152d0,  
     &                                       0.945308720483d0,   
     &                                       0.393619323152d0,   
     &                                       0.019953242059d0 /)

                                         

!B.Hutzell Nine point quadature IGH = 9 points
!No.  Abscissas  Weight  Total Weight  
       real, parameter, Private :: ghxi_9(9) = (/ -3.19099320178,   
     &                                    -2.26658058453,   
     &                                    -1.46855328922,   
     &                                    -0.72355101875,  
     &                                     0.00000000000,   
     &                                     0.72355101875,  
     &                                     1.46855328922,   
     &                                     2.26658058453,    
     &                                     3.19099320178 /)  

       real, parameter, Private :: ghwi_9(9) = (/ 3.96069772633E-5, 
     &                                    0.00494362428,    
     &                                    0.08847452739,    
     &                                    0.43265155900,    
     &                                    0.72023521561,    
     &                                    0.43265155900,    
     &                                    0.08847452739,    
     &                                    0.004943624275,   
     &                                    3.96069772633E-5 /)

         Type :: Optics_Tables
            integer :: nradii
            real :: rmin
            real :: rmax
            real :: bma
            real :: bpa
            real :: xrmin
            real :: xrmax
            real :: xr
            real, allocatable ::  extp( :,:,:,: )       ! specific extinction
            real, allocatable ::  albp( :,:,:,: )       ! single scat alb
            real, allocatable ::  asmp( :,:,:,: )       ! asymmetry factor
            real, allocatable ::  ascat( :,:,:,: )      ! scattering efficiency, JCB 2004/02/09
            real, allocatable ::  pmom2( :,:,:,: )      ! phase function expansion, #2
            real, allocatable ::  pmom3( :,:,:,: )      ! phase function expansion, #3
            real, allocatable ::  pmom4( :,:,:,: )      ! phase function expansion, #4
            real, allocatable ::  pmom5( :,:,:,: )      ! phase function expansion, #5
            real, allocatable ::  pmom6( :,:,:,: )      ! phase function expansion, #6
            real, allocatable ::  pmom7( :,:,:,: )      ! phase function expansion, #7
            real, allocatable ::  sback2p( :,:,:,: )    ! backscatter
         end Type
         
       
         Type :: Optics_Tables_MP
            integer :: nradii
            real :: rmin
            real :: rmax
            real :: bma
            real :: bpa
            real :: xrmin
            real :: xrmax
            real :: xr
            real, allocatable ::  extp( :,:,: )       ! specific extinction
            real, allocatable ::  albp( :,:,: )       ! single scat alb
            real, allocatable ::  asmp( :,:,: )       ! asymmetry factor
            real, allocatable ::  ascat( :,:,: )      ! scattering efficiency, JCB 2004/02/09
!            real, allocatable ::  pmom2( :,:,: )      ! phase function expansion, #2
!            real, allocatable ::  pmom3( :,:,: )      ! phase function expansion, #3
!            real, allocatable ::  pmom4( :,:,: )      ! phase function expansion, #4
!            real, allocatable ::  pmom5( :,:,: )      ! phase function expansion, #5
!            real, allocatable ::  pmom6( :,:,: )      ! phase function expansion, #6
!            real, allocatable ::  pmom7( :,:,: )      ! phase function expansion, #7
            real, allocatable ::  sback2p( :,:,: )    ! backscatter
         end Type

         Type(Optics_Tables_MP), Allocatable, Private :: Modal_Tables( : )
         Type(Optics_Tables_MP), Allocatable, Private :: Modal_Tables_Check( : )

         integer, parameter, private :: prefr=22,prefi=33
         integer, parameter, private :: nsiz=200,nlog=30

         integer, private :: nbin_a_maxd ! Max # of aerosol bins or modes
         integer, private :: nspint      ! Num of spectral intervals across
         integer, private :: ncoef=40    ! number of chebychev polynomials used
         integer, private :: nrefr,nrefi ! number of step between max/min of real and imaginary refractive indices 

!         integer, parameter :: nsizes(n_mode) = (/ 200,200,200 /)
         integer, allocatable, private :: nsizes( : ) 
         
         real, private :: sback_bhmie ! aerosol backscattering coefficient
         real, private :: refrmin     ! minimum of real part of refractive index
         real, private :: refrmax     ! maximum of real part of refractive index
         real, private :: refimin     ! minimum of imag part of refractive index
         real, private :: refimax     ! maximum of imag part of refractive index
         real, private :: drefr       ! increment in real part of refractive index
         real, private :: drefi       ! increment in imag part of refractive index


         real, private :: reciprocal_drefr  ! reciprocal of increment in real part of refractive index
         real, private :: reciprocal_drefi  ! reciprocal of increment in  imag part of refractive index
         real, private :: reciprocal_drefri ! reciprocal of real times imag increments of refractive index

         real, allocatable, private :: wavmid( : ) ! wavelenghts, cm
         real, allocatable, private :: refrtab(:) ! table of real refractive indices for aerosols
         real, allocatable, private :: refitab(:) ! table of imag refractive indices for aerosols

  
  

          logical :: BHMIE_SUCCESS      = .True.
          Logical :: Create_Table       = .false.
          Logical :: eflag_wrf_fast_mie = .false.
         
       contains

! ------------------------------------------------------------------
       subroutine aero_optics_tabular( mode,iwave, crefin, Vol, dgn, 
     &                                  sig, bext, bscat, gfac)

! FSB NOTE: this subroutine calculates for single mode     
     
! *** calculate the extinction and scattering coefficients and
!     assymetry factors for each wavelength as a sum over the 
!     individual lognormal modes. Each mode may have a different 
!     set of refractive indices.

      USE CSQY_DATA,  ONLY : NWL => NWL_REF, WAVELENGTH => EFFWL_REF
         
      IMPLICIT NONE

! *** input variables
      integer, intent(in)    :: mode       ! aerosol mode index
      integer, intent(in)    :: iwave      ! index in wavelength array
      complex, intent(in)    :: crefin     ! Complex refractive index 
      real,    intent(in)    :: Vol        ! modal aerosol volumes [m**3 /m**3]
      real,    intent(in)    :: dgn        ! modal geometric mean diameter [m]
      real,    intent(in)    :: sig        ! geometric standard deviation 
      
! *** output variables 
      real, intent(out)    :: bext         ! extinction coefficient [ 1 / m ]
      real, intent(out)    :: bscat        ! scattering coefficient [ 1 / m ]
      real, intent(out)    :: gfac         ! assymetry factor for Mie and molecular scattering

      
       character( 32 ), parameter :: pname = 'aero_optics_tabular'
       
! FSB define parameters 
       real, parameter :: integral_factor = three_pi_two * sqrtpi1 


! *** internal variables
      real :: beta_Sc       ! aerosol scattering coefficient  
      real :: beta_Ex       ! aerosol extinction coefficients       
      real :: G             ! modal aerosol assymetry factors
      real :: sum_g
      real :: LSIGX
      real :: lambdam1       ! 1/ lambda (m)
      real :: dgv            ! geometric mean of volume distribution (m)
      real :: mie_volume_parameter ! Mie size parameter for volume distribution
      real :: vfac
      real :: bscoef         ! backscatter efficiency

       real    :: dgv_p, dgv_m             ! diameters at quadature points
       real    :: mie_par_top, mie_par_bot ! mie parameter at quadature points
     
! variable for integration over volume distribution     
       real    :: nr
       real    :: aa1
       real    :: xlnsig
       real    :: sum_e,sum_s, xi,wxi,xf
       real    :: sum_sg
!  *** these are Qext/alfa and Qscat/alfv at the abscissas
       real    :: qalfip_e, qalfim_e ! extinction  
       real    :: qalfip_s, qalfim_s ! scattering
       real    :: gsalfp, gsalfm     ! scattering times asymmetry factor

      integer :: i
      real,    allocatable,  save  :: GHXI(:), GHWI(:) ! weight and abscissas
      integer, save  :: IGH                            ! number of weights and abscissa
      integer, save  :: NMAX                           ! optimumized number of weights and abscissa

       Logical, Save :: Initialize = .True.
      
       If( Initialize )Then
          Select Case( Quadrature_Points )
            Case( 1,3,5,9 )
              IGH = Quadrature_Points
            Case Default
              IGH = 3
          End Select

          NMAX = Max( Int( IGH / 2 ), 0)
          
          Allocate( GHXI( NMAX + 1 ), GHWI( NMAX + 1 ) )
 
          Select Case ( IGH ) 
            Case ( 1 )
              GHXI(1)  = ghxi_1(1)
              GHWI(1)  = ghwi_1(1)
            Case ( 3 )
              do i = 1, NMAX + 1
                GHXI(i) = ghxi_3(i)
                GHWI(i) = ghwi_3(i)
              end do 
            Case ( 5 )
              do i = 1, NMAX + 1
                GHXI(i) = ghxi_5(i)
                GHWI(i) = ghwi_5(i)
              end do 
            Case ( 9 )
              do i = 1, NMAX + 1
                GHXI(i) = ghxi_9(i)
                GHWI(i) = ghwi_9(i)
              end do 
          end select 
       
          Initialize = .False.
          
       End If

! Algorithm based on 04/15/2012 codes       
!     by Dr. Francis S. Binkowski
!     Center for Environmental Modeling for Policy Development
!     Institute for the Environment
!     University of North Carolina at Chapel Hill
!     email: frank_binkowski@unc.edu

! *** initialize variables
       lambdam1 = 1.0e9 / wavelength(iwave)  ! lambdam1 in [ m^(-1) ]
       bext  = 0.0
       bscat = 0.0
       sum_g = 0.0

       LSIGX = log(sig)
       
!     calculate Mie size parameter for volume distribution
!     exp(3.0 * xlnsig*xlnsig)  converts dgn to dgv (volume diameter)
       dgv =  dgn * exp(3.0 * LSIGX * LSIGX)      
       mie_volume_parameter =  pi * dgv * lambdam1

       nr = real(crefin)      

      
! Integration code over modal volume distribution
       aa1 = sqrt2 * lsigx
! This 1.0 / Sqrt( A ) in derivation of the integral where A = 1.0 / ( 2.0 * xlnsg**2 ) 

! For wet_diameter in fast_mieaer call
! Need wet_diameter corresponding xi or Gauss-Hermite Quadrature point
! dgv * exp[ u / sqrt(A) ] where u = xi Gauss-Hermite Quadrature
! Therefore, xf = exp( xi / sqrt(A) ) or xf = exp( xi * aa1 ) 

       bext    = 1.0e-30  ! [ 1 / m ]
       bscat   = 1.0e-30    ! [ 1 / m ]
       gfac    = 0.0
!       return

!start integration at zero point
       xi      = 0.0
       wxi     = GHWI(NMAX+1)
       xf      = 1.0

       
! fetch the effficiencies at zero point
        call fast_mieaer_modal( mie_volume_parameter,crefin,qalfip_e,qalfip_s,gsalfp,bscoef )
       
       sum_e  = wxi * qalfip_e
       sum_s  = wxi * qalfip_s
       sum_sg = wxi * gsalfp

! FSB do NMAX calls to the MIE codes      
       do i = 1, NMAX
          xi      = GHXI(i)
          wxi     = GHWI(i)
          xf      = exp( xi * aa1 )
          dgv_p   = dgv * xf
          dgv_m   = dgv / xf ! division cheaper than another exp()
          mie_par_top = mie_volume_parameter * xf
          mie_par_bot = mie_volume_parameter / xf
! *** call subroutine to fetch the effficiencies


          call fast_mieaer_modal( mie_par_top,crefin,qalfip_e,qalfip_s,gsalfp,bscoef )
          call fast_mieaer_modal( mie_par_bot,crefin,qalfim_e,qalfim_s,gsalfm,bscoef )

          sum_e  = sum_e + wxi  * ( qalfip_e + qalfim_e ) 
          sum_s  = sum_s + wxi  * ( qalfip_s + qalfim_s ) 
          sum_sg = sum_sg + wxi * ( gsalfp + gsalfm ) 

       end do
       

       G        = sum_sg / sum_s ! this is <cos>
       beta_Ex  = integral_factor * sum_e  ! 
       beta_Sc  = integral_factor * sum_s  
       

! *** bhmie driver returns values divided by mie_vol_parameter
!     Calculate the actual extinction and scattering coefficients 
!     by multplying by the modal volume and dividing by the wavelength
                  
       vfac  =  Vol * lambdam1         
       bext    = vfac * beta_Ex  ! [ 1 / m ]
       bscat   = vfac * beta_Sc  ! [ 1 / m ]
       gfac    = G
       
99501  Format(I2,' Quadrature Points for Volume Averaged Aerosol Optics')
99504  Format('Even Number Quadrature Points for Volume Averaged Aerosol Optics')
       
       END SUBROUTINE aero_optics_tabular

        subroutine fast_mieaer_modal( mie_parameter,refindx,extaer,scataer,gaer,bscoef )

!!!!!    USE UTILIO_DEFN
         USE AERO_DATA           ! aero variable data
         USE CSQY_DATA,  ONLY : NWL => NWL_REF, NUM_REFRACTIVE, REFRACTIVE_INDEX,
     &                          WAVELENGTH => EFFWL_REF, FIRST_DAY => NEW_START

         use get_env_module
        
        IMPLICIT NONE
!***********************************************************************
! Purpose:  calculate aerosol scattering albedo, asymmetry factor, and 
!   extinction efficencies. If Two_Stream is true, sets Legendre coefficients. 
!   The method parameterizes aerosol coefficients using chebychev polynomials
!   requires double precision on 32-bit machines uses Wiscombe's (1979) mie
!   scattering code or Bohren-Huffman (1983) Mie scattering
!   subroutine, bhmie, to calculates efficiencies by a homogenous
!   isotropic sphere. The latter subroutine is used if RadTran solution is a
!   Two Stream Method.
!
! INPUT
!       mie_parameter -- 2*pi*wet diameter/wavelength
!       refindx      -- volume averaged complex index of refraction
! OUTPUT:
!   real sactaer ! aerosol single scattering efficiency
!        gaer    ! aerosol asymmetery factor
!        extaer  ! aerosol extinction efficiency, 
!        bscoef  ! aerosol backscatter efficiency, to convert steradian divide by 4*PI
!----------------------------------------------------------------------

! arguments:
        real,    intent(in)                   :: mie_parameter
        complex, intent(in)                   :: refindx

        real, intent(out) :: extaer,scataer,gaer
        real, intent(out) :: bscoef

!local variables
         character( 32 ), parameter :: pname = 'fast_mieaer_modal'
         

         real weighte, weights
! various bookeeping variables         
         integer, parameter :: ltype = 1  ! total number of indicies of refraction

         real x
         real thesum ! for normalizing things
         real sizem ! size in microns
         integer kcallmieaer
!
         integer m, j, nc, klevel
         real, save :: pscat !scattering cross section
         real, save :: pext  ! parameterized specific extinction (cm2/g)
         real, save :: pasm  ! parameterized asymmetry factor
         real, save :: pd_pscat !scattering cross section
         real, save :: pd_pext  ! parameterized specific extinction (cm2/g)
         real, save :: pd_pasm  ! parameterized asymmetry factor


         real ppmom2     ! 2 Lengendre expansion coefficient (numbered 0,1,2,...)
         real ppmom3     ! 3     ...
         real ppmom4     ! 4     ...
         real ppmom5     ! 5     ...
         real ppmom6     ! 6     ...
         real ppmom7     ! 7     ...
         real sback2     ! JCB 2007/02/01 sback*conjg(sback)
         
         integer ns            ! Spectral loop index
         integer i             ! Longitude loop index
         integer k       ! Level loop index
         
         integer, save :: isecfrm0 = 0

 


         integer            :: ncoef_check   ! number of chebychev polynomials used
         integer, save      :: numang=0,nmom=7,ipolzn=0,momdim=7
         integer            :: nrefr_check,nrefi_check

         integer            :: nsize
         integer            :: nr,ni
         
!        
         real(8)       :: pmom(0:7,1)
         real(8), save :: xmu(1) = 1.0d0
         real(8), save :: mimcut = 0.0d0
         
         complex(8) :: sforw,sback,tforw(2),tback(2)
         complex(8) :: s1(1),s2(1)

         logical, save :: perfct  = .false.
         logical, save :: anyang  = .false.
         logical, save :: prnt(2) = (/ .false., .false./)
         logical, save :: first   = .true.
         logical, save :: TWO_STREAM = .true.
         logical       :: search
         logical       :: exists
         
         real ::  aero_radius

         integer itab,jtab
         integer itabp,jtabp
         
         real ttab,utab
         
!        nsiz = number of wet particle sizes
!        crefin = complex refractive index
         integer n
         real(8), allocatable :: qext( : )   ! array of extinction efficiencies
         real(8), allocatable :: qsca( : )   ! array of scattering efficiencies
         real(8), allocatable :: gqsc( : )   ! array of asymmetry factor * scattering efficiency
         real, allocatable    :: asymm( : )  ! array of asymmetry factor
         real, allocatable    :: scat( : )   ! JCB 2004/02/09
         real, allocatable    :: sb2( : )    ! JCB 2007/02/01 - 4*abs(sback)^2/(size parameter)^2 backscattering efficiency
         real, allocatable    :: qextr4( : ) !  extinction, real*4
         real, allocatable    :: rs( : )     ! surface mode radius (cm)

         real, allocatable, save :: cext     (:)
         real, allocatable, save :: casm     (:)
         real, allocatable, save :: cpmom2   (:)
         real, allocatable, save :: cscat    (:)                      
         real, allocatable, save :: cpsback2p(:)                      

         real(8) :: thesize        ! 2 pi radpart / waveleng = size parameter

         complex(8)       :: crefin,crefd
         complex(8), save :: crefw
         real, save :: rmin=0.005e-4,rmax=50.e-4   ! min, max aerosol size bin

         
         real bma,bpa
         
         real, save :: xrmin,xrmax,xr
         real xrad      ! normalized aerosol radius
         
         real, save :: rhoh2o = 1.0     ! density of liquid water (g/cm3)
         
         real refr         ! real part of refractive index
         real refi         ! imaginary part of refractive index
         
         real thesize_bhmie    ! 2 pi radpart / waveleng = size parameter
         real qext_bhmie       ! array of extinction efficiencies
         real qsca_bhmie       ! array of scattering efficiencies
         real qback_bhmie      ! array of scattering efficiencies
         real gsca_bhmie       ! array of asymmetry factor * scattering efficiency
         complex refrel_bhmie

         real :: weight_real   ! nomralized real part refractive index difference used in bilinear interpolation 
         real :: weight_imag   ! nomralized imaginary part refractive index difference used in bilinear interpolation
         real :: wrwi          ! weighting factors used on bilinear interpolation points
         real :: wr_wrwi       ! weighting factors used on bilinear interpolation points
         real :: wi_wrwi       ! weighting factors used on bilinear interpolation points
         real :: one_wrwi_wi   ! weighting factors used on bilinear interpolation points

         real :: quadrature_factor
         real :: max_quad_point

         real :: reciprocal_drefr_check  ! reciprocal of increment in real part of refractive index
         real :: reciprocal_drefi_check  ! reciprocal of increment in  imag part of refractive index
         real :: reciprocal_drefri_check ! reciprocal of real times imag increments of refractive index
         real :: refrmin_check ! minimum of real part of refractive index
         real :: refrmax_check ! maximum of real part of refractive index
         real :: refimin_check ! minimum of imag part of refractive index
         real :: refimax_check ! maximum of imag part of refractive index
         real :: drefr_check   ! increment in real part of refractive index
         real :: drefi_check   ! increment in imag part of refractive index

         real, allocatable :: refrtab_check(:) ! table of real refractive indices for aerosols
         real, allocatable :: refitab_check(:) ! table of imag refractive indices for aerosols
         real, allocatable, save :: ch(:)      ! values of chebychev polynomials

         real, parameter :: pie   = pi
         real, parameter :: third = 1.0/3.0
         
         integer :: irams, jrams
! diagnostic declarations
         integer :: kcallmieaer2
         integer :: ibin, imode
         integer :: mie_unit
         integer :: astat
         character(150) :: msg
         CHARACTER(600) :: MIE_TABLE


       if(first)then
          
          first = .false.
          
!       parameterize aerosol radiative properties in terms of
!       relative humidity, surface mode wet radius, aerosol species,
!       and wavelength
!       first find min,max of real and imaginary parts of refractive index
         crefw=cmplx(1.33,0.0)
         refrmin=real(crefw)
         refrmax=real(crefw)

! change Rahul's imaginary part of the refractive index from positive to negative
         refimin=-imag(crefw)
         refimax=-imag(crefw)
         nspint = nwl
         allocate( wavmid( nwl ) )
         wavmid(1:nwl) =  1.0e-7*wavelength(1:nwl)                         

         allocate( nsizes( n_mode ) )
         
         nsizes = (/ 200,200,200 /)
         
         allocate( Modal_Tables( N_MODE ) )
         
         Select Case ( Quadrature_Points ) 
         Case ( 1 )
             max_quad_point = ghxi_1(1)
         Case ( 3 )
             max_quad_point = ghxi_3(3)
         Case ( 5 )
             max_quad_point = ghxi_5(5)
         Case ( 9 )
             max_quad_point = ghxi_9(9)
         end select 

         nbin_a_maxd = n_mode
         
         call get_env( mie_table, 'MIE_TABLE', 'MIE_TABLE',logdev )
         inquire( file=trim(mie_table),exist=exists)
         if( .not. exists )then
             Create_Table = .true.
         else
             Create_Table = .false.
         endif

         if( Create_Table )then

            Create_table = .false.
               
            
            do i=1,NUM_REFRACTIVE
               refrmin = amin1( refrmin,MINVAL( REFRACTIVE_INDEX( i )%REAL_PART( :,: ) ))
               refrmax = amax1( refrmax,MAXVAL( REFRACTIVE_INDEX( i )%REAL_PART( :,: ) ))
               refimin = amin1( refimin,-MINVAL( REFRACTIVE_INDEX( i )%IMAG_PART( :,: ) ))  
               refimax = amax1( refimax,-MAXVAL( REFRACTIVE_INDEX( i )%IMAG_PART( :,: ) ))
            enddo
            
             rmax = amax1(rmax,60.0*MAXVAL(max_dg_wet))
             rmin = amin1(rmin,40.0*MINVAL(min_dg_dry))
             drefr=(refrmax-refrmin)
             if(drefr.gt.1.e-4)then
                nrefr=prefr
                drefr=drefr/(nrefr-1)
             else
                nrefr=1
             endif
           
             drefi=(refimax-refimin)
             if(drefi.gt.1.e-4)then
                nrefi=prefi
                drefi=drefi/(nrefi-1)
             else
                nrefi=1
             endif
           
           
             reciprocal_drefr  = 1.0/drefr
             reciprocal_drefi  = 1.0/drefi
             reciprocal_drefri = reciprocal_drefr * reciprocal_drefi
           
!          
             
             bma=0.5*alog(rmax/rmin) ! JCB
             bpa=0.5*alog(rmax*rmin) ! JCB
             xrmin=alog(rmin)
             xrmax=alog(rmax)
           
             quadrature_factor = exp( sqrt2 * log( max_sigma_g ) * max_quad_point )
             
             do ns = 1,n_mode
                Modal_Tables(ns)%nradii = nsizes(ns)
                Modal_Tables(ns)%rmax = 60.0*max_dg_wet(ns)*quadrature_factor
!                if( ns .eq. n_mode )Modal_Tables(ns)%rmax = 4.0*Modal_Tables(ns)%rmax
! convert modal rmax into Mie Parameter            
                Modal_Tables(ns)%rmax = 2.0*pi*Modal_Tables(ns)%rmax/minval( wavmid )
                Modal_Tables(ns)%rmin = 40.0*min_dg_dry(ns)/quadrature_factor
! convert modal rmin into Mie Parameter            
                Modal_Tables(ns)%rmin = 2.0*pi*Modal_Tables(ns)%rmin/maxval( wavmid )
                Modal_Tables(ns)%bma=0.5*log(Modal_Tables(ns)%rmax/Modal_Tables(ns)%rmin)
                Modal_Tables(ns)%bpa=0.5*log(Modal_Tables(ns)%rmax*Modal_Tables(ns)%rmin)
                Modal_Tables(ns)%xrmin=log(Modal_Tables(ns)%rmin)
                Modal_Tables(ns)%xrmax=log(Modal_Tables(ns)%rmax)
                allocate( Modal_Tables(ns)%extp(ncoef,prefr,prefi),      
     &                    Modal_Tables(ns)%albp(ncoef,prefr,prefi),   
     &                    Modal_Tables(ns)%asmp(ncoef,prefr,prefi),   
     &                    Modal_Tables(ns)%ascat(ncoef,prefr,prefi),  
     &                    Modal_Tables(ns)%sback2p(ncoef,prefr,prefi) )
             end do

! check if coverage of mie parameter is continuous between Modal_Tables 
             do ns = 2,n_mode
             
                if( Modal_Tables(ns-1)%rmax .ne. Modal_Tables(ns)%rmin )then
                    Modal_Tables(ns-1)%rmax  = Modal_Tables(ns)%rmin
                    Modal_Tables(ns-1)%xrmax = log(Modal_Tables(ns-1)%rmax)
                    Modal_Tables(ns-1)%bma=0.5*log(Modal_Tables(ns-1)%rmax/Modal_Tables(ns-1)%rmin)
                    Modal_Tables(ns-1)%bpa=0.5*log(Modal_Tables(ns-1)%rmax*Modal_Tables(ns-1)%rmin)
                end if
            
             end do
            
             
             nsize = MAXVAL( Modal_Tables(1:n_mode)%nradii )
             
             allocate( qext( nsize ),
     &                 qsca( nsize ),
     &                 gqsc( nsize ) )
            
             allocate( asymm( nsize ),
     &                  scat( nsize ),
     &                   sb2( nsize ),
     &                qextr4( nsize ),
     &                    rs( nsize ) )          
            
          
!         calibrate parameterization with range of refractive indices 

            allocate( refrtab(nrefr),
     &                refitab(nrefi),
     &                ch(ncoef))
     
            do 120 ni=1,nrefi
              do 120 nr=1,nrefr                                        

               refrtab(nr)=refrmin+(nr-1)*drefr
               refitab(ni)=refimin+(ni-1)*drefi
               crefd=dcmplx(real(refrtab(nr),8),real(refitab(ni),8))
!              mie calculations of optical efficiencies
               loop_modes: do imode = 1,n_mode
                  nsize = Modal_Tables(imode)%nradii ! nsizes( imode )
                  do n=1,nsize
                  
                     xr=cos(pie*(real(n)-0.5)/real(nsize))
                     rs(n)=real(exp(xr*Modal_Tables(imode)%bma+Modal_Tables(imode)%bpa),8)
!                    size parameter and weighted refractive index
                     thesize=real(rs(n),8)
                  
! backscattering efficiency, Bohren and Huffman, page 122
! as stated by Bohren and Huffman, this is 4*pie times what is should be
! may need to be smoothed - a very rough function - for the time being we won't apply smoothing
! and let the integration over the size distribution be the smoothing
                            
                     thesize_bhmie = real( thesize )
                     refrel_bhmie  = cmplx(real(refrtab(nr)),-real(refitab(ni)))
                     call driver_bhmie_flexy(thesize_bhmie,refrel_bhmie,qext_bhmie,qsca_bhmie,gsca_bhmie) 
                     asymm(n) = gsca_bhmie/qsca_bhmie 
                     qextr4(n)= qext_bhmie*thesize_bhmie
                     scat(n)  = qsca_bhmie*thesize_bhmie 
                     sback    = sback_bhmie
                     sb2(n)   = 4.0*sback*dconjg(sback)
     &                         / (thesize_bhmie*thesize_bhmie) ! JCB 2007/02/01  
                         
                  
                  enddo
  100             continue

                  call fitcurv(rs,qextr4,Modal_Tables(imode)%extp(:,nr,ni),ncoef,nsize)
                  call fitcurv(rs,scat,Modal_Tables(imode)%ascat(:,nr,ni),ncoef,nsize) 
                  call fitcurv(rs,asymm,Modal_Tables(imode)%asmp(:,nr,ni),ncoef,nsize)
                  call fitcurv(rs,sb2,Modal_Tables(imode)%sback2p(:,nr,ni),ncoef,nsize)           
                                       
               enddo loop_modes

120         continue

            If( MYPE .eq. 0 )Then
            
                mie_unit = get_free_iounit()
                open(unit=mie_unit,file=trim(mie_table),form='FORMATTED',status='REPLACE',
     &               iostat=astat)
                if( astat .ne. 0 )then
                  msg = 'Error creating Mie Lookup Table, ' // trim(mie_table)
     &               // 'and new start is true.'  
                  eflag_wrf_fast_mie = .True.
                  return
                endif
                write(mie_unit,'(a)')'real and imaginary part of minimum refractive index '
                write(mie_unit,'(a)')'imaginary part uses negative sign convention so reverse sign common value'
                write(mie_unit,'(2(es12.4,1x))')refrtab(1),refitab(1)
                write(mie_unit,'(a)')'real and imaginary part of maximum refractive index'
                write(mie_unit,'(es12.4,1x,es12.4)')refrtab(nrefr),refitab(nrefi)
                write(mie_unit,'(a)')'number real and imaginary parts'
                write(mie_unit,'(i4,1x,i4)')nrefr,nrefi
                write(mie_unit,'(a)')'minimum mie parameter for each mode '
                write(mie_unit,'(3(es16.7,1x))')Modal_Tables(1:n_mode)%rmin
                write(mie_unit,'(a)')'maximum mie parameters for each mode' 
                write(mie_unit,'(3(es16.7,1x))'),Modal_Tables(1:n_mode)%rmax
                write(mie_unit,'(a)')'number coefficients for chebyshev polynomials for each fit= '
                write(mie_unit,'(i4)')ncoef
                do ni=1,nrefi
                   do nr=1,nrefr
                     write(mie_unit,'(a,es12.4,a2,es12.4,a5)')'! refractive index = ( ',refrtab(nr),', ',refitab(ni),' ) '
                     write(mie_unit,'(a)')'! ext, scat, asym, and backscat coefficients for chebyshev polynomials'
                     do nc = 1,ncoef
                        write(mie_unit,'(21(es16.7,1x))')
     &                  (Modal_Tables(imode)%extp(nc,nr,ni),
     &                   Modal_Tables(imode)%ascat(nc,nr,ni),
     &                   Modal_Tables(imode)%asmp(nc,nr,ni),
     &                   Modal_Tables(imode)%sback2p(nc,nr,ni),imode=1,n_mode)
                     end do
                   end do
                 end do 
                 close(mie_unit)
            End If

            deallocate( qext,
     &                  qsca,
     &                  gqsc  )
          
            deallocate( asymm, 
     &                   scat, 
     &                    sb2, 
     &                 qextr4, 
     &                     rs  ) 
     
           
        else ! read table from ascii file     

           inquire( file=trim(mie_table),exist=exists)
           if( .not. exists )then
              msg = 'Mie Lookup Table, ' // trim(mie_table)
     &            // ' not found and new start is false.' 
              eflag_wrf_fast_mie = .True.
              return
           end if
           mie_unit = get_free_iounit()
           open(unit=mie_unit,file=trim(mie_table),form='FORMATTED',status='OLD',
     &          iostat=astat)
           if( astat .ne. 0 )then
              msg = 'Error reading Mie Lookup Table, ' // trim(mie_table) 
              eflag_wrf_fast_mie = .True.
              return
           endif
           read(mie_unit,'(a)')msg
           read(mie_unit,'(a)')msg
           read(mie_unit,'(2(es12.4,1x))')refrmin,refimin
           read(mie_unit,'(a)')msg
           read(mie_unit,'(es12.4,1x,es12.4)')refrmax,refimax
           read(mie_unit,'(a)')msg
           read(mie_unit,'(i4,1x,i4)')nrefr,nrefi
           read(mie_unit,'(a)')msg
           read(mie_unit,'(3(es16.7,1x))')Modal_Tables(1:n_mode)%rmin
           read(mie_unit,'(a)')msg
           read(mie_unit,'(3(es16.7,1x))'),Modal_Tables(1:n_mode)%rmax
           read(mie_unit,'(a)')msg
           read(mie_unit,'(i4)')ncoef

            do ns = 1,n_mode
               Modal_Tables(ns)%nradii = nsizes(ns)
               Modal_Tables(ns)%bma=0.5*log(Modal_Tables(ns)%rmax/Modal_Tables(ns)%rmin)
               Modal_Tables(ns)%bpa=0.5*log(Modal_Tables(ns)%rmax*Modal_Tables(ns)%rmin)
               Modal_Tables(ns)%xrmin=log(Modal_Tables(ns)%rmin)
               Modal_Tables(ns)%xrmax=log(Modal_Tables(ns)%rmax)
               allocate( Modal_Tables(ns)%extp(ncoef,nrefr,nrefi),      
     &                   Modal_Tables(ns)%albp(ncoef,nrefr,nrefi),   
     &                   Modal_Tables(ns)%asmp(ncoef,nrefr,nrefi),   
     &                   Modal_Tables(ns)%ascat(ncoef,nrefr,nrefi),  
     &                   Modal_Tables(ns)%sback2p(ncoef,nrefr,nrefi) )
            end do


            drefr=(refrmax-refrmin)
            if(drefr.gt.1.e-4)then
               drefr=drefr/(nrefr-1)
            endif
            
            drefi=(refimax-refimin)
            if(drefi.gt.1.e-4)then
               drefi=drefi/(nrefi-1)
            endif
            
            reciprocal_drefr  = 1.0/drefr
            reciprocal_drefi  = 1.0/drefi
            reciprocal_drefri = reciprocal_drefr * reciprocal_drefi

            allocate( refrtab(nrefr),
     &                refitab(nrefi),
     &                ch(ncoef))

            do ni=1,nrefi
              do nr=1,nrefr
                refrtab(nr)=refrmin+(nr-1)*drefr
                refitab(ni)=refimin+(ni-1)*drefi
                read(mie_unit,'(a)')msg
                read(mie_unit,'(a)')msg
                do nc = 1,ncoef
                   read(mie_unit,'(21(es16.7,1x))')
     &             (Modal_Tables(imode)%extp(nc,nr,ni),
     &              Modal_Tables(imode)%ascat(nc,nr,ni),
     &              Modal_Tables(imode)%asmp(nc,nr,ni),
     &              Modal_Tables(imode)%sback2p(nc,nr,ni),imode=1,n_mode)
                end do
               end do
            end do 
            close(mie_unit)
           
        end if

        allocate( cext     (ncoef), 
     &            casm     (ncoef),
     &            cpmom2   (ncoef),
     &            cscat    (ncoef),
     &            cpsback2p(ncoef))
        
        return
                 
      endif ! end first call block
      
         aero_radius = mie_parameter 
         ns         = 1
         do m = 1,n_mode
            if( aero_radius .le. Modal_Tables(m)%rmax)exit
         end do
         m = min(m,n_mode)
         
               gaer=0.0
               extaer=0.0
               bscoef=0.0        ! JCB 2007/02/01 - backscattering coefficient
! loop over the bins
                  sizem=aero_radius ! radius in cm
! check against limits of mie parameter
                  if(aero_radius.le.Modal_Tables(m)%rmin)then
                    aero_radius=1.01*Modal_Tables(m)%rmin
                    write( msg, '(a, 1x, es11.4, a, 1x, es11.4 )' )       
     &              'In ' // Trim( pname ) // ':aerosol mie parameter set to ',
     &              aero_radius, ' from ', mie_parameter
                     write(logdev,'(a)')TRim( msg )
                  endif
!
                  if(aero_radius.ge.Modal_Tables(m)%rmax)then
                    aero_radius=0.99*Modal_Tables(m)%rmax
                    write( msg, '(a, 1x, es11.4, a, 1x, es11.4 )' )       
     &              'In ' // Trim( pname ) // ':aerosol mie parameter set to ',
     &              aero_radius, ' from ', mie_parameter
                     write(logdev,'(a)')Trim( msg )
                  endif


                  crefin=refindx
                  refr=real(crefin)
! change Rahul's imaginary part of the index of refraction from positive to negative
                  refi=-imag(crefin)

                  x=alog(aero_radius) ! radius in cm
                  xrad=x
! normalize size parameter
                  xrad=(2*xrad-Modal_Tables(m)%xrmax-Modal_Tables(m)%xrmin)
     &                /(Modal_Tables(m)%xrmax-Modal_Tables(m)%xrmin)
     
! retain this diagnostic code
                  if(abs(refr).gt.10.0.or.abs(refr).le.0.001)then
                      write( msg, '(a,1x, e14.5)' )  
     &               'FASTJ mie /refr/ outside range 1e-3 to 10 refr= ', refr
                      write(logdev,'(a)')Trim( msg )
                  endif
                  if(abs(refi).gt.10.0)then
                     write( msg, '(a,1x, e14.5)' )  
     &              'FASTJ mie /refi/ >10 refi', refi
                     write(logdev,'(a)')Trim( msg )
                  endif

! interpolate coefficients linear in refractive index
! first call calcs itab,jtab,ttab,utab
                  itab=0
                  search = .true.
                  
                  search = .false.
                  
                  itab = int( (refr-refrmin)*reciprocal_drefr ) + 1
                  itab = min( nrefr,max( itab,1 ) )
                  jtab = int( (refi-refimin)*reciprocal_drefi ) + 1
                  jtab = min( nrefi,max( jtab,1 ) )

                  if( itab .eq. 1 .or. itab .eq. nrefr )then
                      weight_real = 0.0                                      
                      itabp = itab                                      
                  else
                      weight_real = (refr-refrtab(itab))
     &                            *  reciprocal_drefr                 
                      itabp = itab + 1
                  end if
                  if( jtab .eq. 1 .or. jtab .eq. nrefi )then
                      weight_imag = 0.0
                      jtabp = jtab                                      
                  else
                      weight_imag = (refi-refitab(jtab))
     &                            *  reciprocal_drefi                
                      jtabp = jtab + 1
                  end if

                  wrwi    = weight_real*weight_imag
                  wr_wrwi = weight_real - wrwi
                  one_wrwi_wi = 1.0 - wr_wrwi - weight_imag
                  wi_wrwi     = weight_imag - wrwi
                  
 
                  do nc=1,ncoef
                  
                     cext(nc)  = one_wrwi_wi*Modal_Tables(m)%extp(nc,itab,jtab)
     &                         + wr_wrwi*Modal_Tables(m)%extp(nc,itabp,jtab)
     &                         +          wrwi*Modal_Tables(m)%extp(nc,itabp,jtabp)
     &                         + wi_wrwi*Modal_Tables(m)%extp(nc,itab,jtabp)                     
     
                     
                     cscat(nc) = one_wrwi_wi*Modal_Tables(m)%ascat(nc,itab,jtab)
     &                         + wr_wrwi*Modal_Tables(m)%ascat(nc,itabp,jtab)
     &                         +         wrwi*Modal_Tables(m)%ascat(nc,itabp,jtabp)
     &                         + wi_wrwi*Modal_Tables(m)%ascat(nc,itab,jtabp)                     
     
                     casm(nc)  = one_wrwi_wi*Modal_Tables(m)%asmp(nc,itab,jtab)
     &                         + wr_wrwi*Modal_Tables(m)%asmp(nc,itabp,jtab)
     &                         +       wrwi*Modal_Tables(m)%asmp(nc,itabp,jtabp)
     &                         + wi_wrwi*Modal_Tables(m)%asmp(nc,itab,jtabp)                     
                     
                     cpsback2p(nc)  = one_wrwi_wi*Modal_Tables(m)%sback2p(nc,itab,jtab)
     &                              +    wr_wrwi*Modal_Tables(m)%sback2p(nc,itabp,jtab)
     &                              +      wrwi*Modal_Tables(m)%sback2p(nc,itabp,jtabp)
     &                              +    wi_wrwi*Modal_Tables(m)%sback2p(nc,itab,jtabp)                     
                  enddo
                  
!                 chebyshev polynomials
                  ch(1)=1.
                  ch(2)=xrad
                  do nc=3,ncoef
                     ch(nc)=2.*xrad*ch(nc-1)-ch(nc-2)
                  enddo


!                 parameterized optical properties
                  pext=0.5*cext(1)
                  pscat=0.5*cscat(1)
                  pasm=0.5*casm(1)
                  sback2=0.5*cpsback2p(1) ! JCB 2007/02/01 - backscattering efficiency
                  do nc=2,ncoef
                      pext=pext+ch(nc)*cext(nc)
                      pscat=pscat+ch(nc)*cscat(nc)
                      pasm=pasm+ch(nc)*casm(nc)
                      sback2=sback2+ch(nc)*cpsback2p(nc)
                  enddo
                  pext=exp(pext)
                  pscat=exp(pscat)
                  pasm=exp(pasm)
                  sback2=max( exp(sback2),0.0 )


               thesize = mie_parameter
                  
               pext  = pext/thesize 
               pscat = pscat/thesize 
               pasm  = pasm * pscat
               extaer  = pext
               scataer = pscat
               gaer    = pasm
! convert backscattering efficiency to backscattering coefficient, units (cm)^-1
               bscoef= sback2/thesize
               
               
      return
      end subroutine fast_mieaer_modal                    

      function binarysearch(length, array, value)
        ! Given an array and a value, returns the index of the element that
        ! is closest to, but less than, the given value.
        ! Uses a binary search algorithm.
        ! "delta" is the tolerance used to determine if two values are equal
        ! if ( abs(x1 - x2) <= delta) then
        !    assume x1 = x2
        ! endif

        implicit none
        integer, intent(in) :: length
        real,    intent(in) :: array(length)
        real,    intent(in) :: value

        integer :: binarysearch

        integer :: left, middle, right

        real, parameter :: d = 1.0e-9

        if ( value .ge. array(length) ) then
             binarysearch = length
             return
        end if
        if ( value .le. array(1) ) then
             binarysearch = 1
             return
        end if
        
        left = 1
        right = length
        do
            if (left .gt. right) then
                exit
            end if
            middle = nint((left+right) / 2.0)
            if ( abs(array(middle) - value) .le. d) then
                binarySearch = middle
                return
            else if (array(middle) .gt. value) then
                right = middle - 1
            else
                left = middle + 1
            end if
        end do
        binarysearch = right

       end function binarysearch

       real function interpolate(x_len, x_array, y_len, y_array, f, x, y )
        ! This function uses bilinear interpolation to estimate the value
        ! of a function f at point (x,y)
        ! f is assumed to be sampled on a regular grid, with the grid x values specified
        ! by x_array and the grid y values specified by y_array
        ! Reference: http://en.wikipedia.org/wiki/Bilinear_interpolation
        implicit none
        integer, intent(in) :: x_len, y_len           
        real, intent(in) :: x_array(x_len)
        real, intent(in) :: y_array(y_len)
        real, intent(in) :: f(x_len, y_len)
        real, intent(in) :: x,y

        real :: denom, x1, x2, y1, y2
        integer :: i,j

        i = binarysearch(x_len, x_array, x)
        j = binarysearch(y_len, y_array, y)

        x1 = x_array(i)
        x2 = x_array(i+1)

        y1 = y_array(j)
        y2 = y_array(j+1)
        
        denom = (x2 - x1)*(y2 - y1)

        interpolate = (f(i,j)*(x2-x)*(y2-y) + f(i+1,j)*(x-x1)*(y2-y) + 
     &                 f(i,j+1)*(x2-x)*(y-y1) + f(i+1, j+1)*(x-x1)*(y-y1))/denom

        end function interpolate
!****************************************************************
      subroutine fitcurv(rs,yin,coef,ncoef,maxm)

!     fit y(x) using Chebychev polynomials
!     wig 7-Sep-2004: Removed dependency on pre-determined maximum
!                     array size and replaced with f90 array info.

!      USE module_peg_util, only:  peg_message

      IMPLICIT NONE

      integer, intent(in) :: maxm, ncoef
      real, intent(in)    :: rs(:), yin(:)
      real, intent(inout) :: coef(:)
!local: 
       real x(size(rs)),y(size(yin))

      integer m
      real xmin, xmax
      character(80) msg


 
      do 100 m=1,maxm
      x(m)=alog(rs(m))
      y(m)=alog(yin(m))
  100 continue

      xmin=x(1)
      xmax=x(maxm)
      do 110 m=1,maxm
      x(m)=(2*x(m)-xmax-xmin)/(xmax-xmin)
  110 continue

      call chebft(coef,ncoef,maxm,y)

      return
      end subroutine fitcurv                        

!**************************************************************
      subroutine fitcurv_nolog(rs,yin,coef,ncoef,maxm)

!     fit y(x) using Chebychev polynomials
!     wig 7-Sep-2004: Removed dependency on pre-determined maximum
!                     array size and replaced with f90 array info.

      IMPLICIT NONE

      integer, intent(in) :: maxm, ncoef
      real, intent(in)    :: rs(:), yin(:)
      real, intent(inout) :: coef(:) 
!local:      
      real x(size(rs)),y(size(yin))

      integer m
      real xmin, xmax
      character*80 msg
           

      do 100 m=1,maxm
      x(m)=alog(rs(m))
      y(m)=yin(m) ! note, no "alog" here
  100 continue

      xmin=x(1)
      xmax=x(maxm)
      do 110 m=1,maxm
      x(m)=(2*x(m)-xmax-xmin)/(xmax-xmin)
  110 continue

      call chebft(coef,ncoef,maxm,y)

      return
      end subroutine fitcurv_nolog                        
!************************************************************************
      subroutine chebft(c,ncoef,n,f)
!     given a function f with values at zeroes x_k of Chebychef polynomial
!     T_n(x), calculate coefficients c_j such that
!     f(x)=sum(k=1,n) c_k t_(k-1)(y) - 0.5*c_1
!     where y=(x-0.5*(xmax+xmin))/(0.5*(xmax-xmin))
!     See Numerical Recipes, pp. 148-150.

      IMPLICIT NONE
      real pi
      integer, intent( in ) :: ncoef, n
      real, intent( out ) :: c(:)
      real, intent( in  ) :: f(:)

! local variables      
!      parameter (pi=3.14159265)
      real(8) :: fac, thesum
      integer j, k
      
      fac=2.0d0/real( n,8 )
      do j=1,ncoef
         thesum=0.0d0
         do k=1,n
            thesum=thesum
     &            +real(f(k),8)
     &            *dcos((pii*real((j-1),8))*((real(k,8)-0.5)/real(n,8)))
         enddo
         c(j)=real(fac*thesum)
      enddo
      return
      end subroutine chebft             
! ------------------------------------------------------------------
      subroutine driver_bhmie_flexy(xx, crefin, qextalf, qscatalf, gscatalfg)
           implicit none
           
           real, intent(in)     :: XX 
           real, intent(out)    :: qextalf, qscatalf, gscatalfg
           complex, intent(in)  :: CREFIN
!local            
           real( 8 ), parameter  :: one_third = 1.0d0 / 3.0d0
           integer              :: NXX
           integer              :: nstop, modulus
           
           real :: QEXTI, QSCA, QBACK, G_MIE, xx1
           
           real( 8 )    :: x
           complex( 8 ) :: refractive_index
           
           x = real( XX, 8 )
           refractive_index = dcmplx( real( CREFIN ), imag( CREFIN ) )
           
           modulus = int( abs( x * refractive_index ) )      
           nstop = int( x + 4.0d0 * x**one_third + 2.0d0 )
           
           nxx = max( modulus, nstop ) + 15
       
           xx1 = 1.0 / XX
           
           CALL BHMIE_FLEXY(XX, NXX, NSTOP, CREFIN,QEXTI,QSCA,QBACK,G_MIE)
           
           qextalf   = QEXTI * xx1
           qscatalf  = QSCA * xx1
           gscatalfg = qscatalf * G_MIE
           sback_bhmie = QBACK * xx1
        
!           write(6,'(a,12(ES12.4,1X))',advance='NO')'WRF BHMIE_FLEXY: XX, QEXT,QSCA,G_MIE = ',
!    &      xx,qextalf,qscatalf,gscatalfg

       end subroutine driver_bhmie_flexy
! ------------------------------------------------------------------
       SUBROUTINE BHMIE_FLEXY (X,  NMX, NSTOP, REFREL, QQEXT, QQSCA, QBACK, GSCA)

! FSB Changed the call vector to return only QEXT, QSCAT QBACK GSCA
!     and ignore NANG, S1 and S2 and all calculations for them

          implicit none 

! Arguments:
          real,    intent(in) :: X        ! X = pi*particle_diameter / Wavelength
          integer, intent(in) :: NMX      ! maximum number of terms in Mie series 
          integer, intent(in) :: NSTOP    ! minumum number of terms in Mie series 
          complex, intent(in) :: REFREL   ! refractive index

!    REFREL = (complex refr. index of sphere)/(real index of medium)
!    in the current use the index of refraction of the the medium
!    i taken at 1.0 real.
!
!    Output

       real,    intent(out) :: QQEXT, QQSCA, QBACK, GSCA

!     QQEXT   Efficiency factor for extinction
!     QQSCA   Efficiency factor for scattering
!     QQBACK  Efficiency factor for back scatter
!     GSCA    asymmetry factor <cos>
!     SUCCESS flag for successful calculation
! REFERENCE: 
!  Bohren, Craig F. and Donald R. Huffman, Absorption and 
!    Scattering of Light by Small Particles, Wiley-Interscience
!    copyright 1983. Paperback Published 1998.
! FSB
!    This code was originally listed in Appendix A. pp 477-482.
!    As noted below, the original code was subsequently 
!    modified by Prof. Bruce T. Drain of Princetion University.
!    The code was further modified for a specific application
!    in a large three-dimensional code requiring as much 
!    computational efficiency as possible. 
!    Prof. Francis S. Binkowski of The University of North
!    Carolina at Chapel Hill. 

! Declare parameters:
! Note: important that MXNANG be consistent with dimension of S1 and S2
!       in calling routine!

          integer, parameter    :: MXNANG=10, NMXX=150000   ! FSB new limits
          integer, parameter    :: NANG  = 2
!          real*8, parameter     :: PII = 3.1415916536D0
          real*8, parameter     :: ONE = 1.0D0, TWO = 2.0D0
!          real(8),parameter     :: ONE_THIRD = 1.0D0/3.0D0
          complex*16, parameter :: COMPLEX_DZERO = (0.0D0,0.0D0)
          complex,    parameter :: COMPLEX_ZERO  = (0.0,0.0)

! Local variables:
          integer    :: N, NN
          real*8     :: QSCA, QEXT, DX1, DXX1      
          real*8     :: CHI,CHI0,CHI1,DX,EN,P,PSI,PSI0,PSI1,XSTOP,YMOD               
          real*8     :: TWO_N_M_ONE, TWO_N_P_ONE, EN1, FACTOR
          complex*16 :: AN,AN1,BN,BN1,DREFRL,XI,XI1,Y, Y1, DREFRL1
          complex*16 :: D(NMX)
          complex*16 :: FAC1, FAC2
          complex*16 :: XBACK


!***********************************************************************
! Subroutine BHMIE is the Bohren-Huffman Mie scattering subroutine
!    to calculate scattering and absorption by a homogenous isotropic
!    sphere.
! Given:
!    X = 2*pi*a/lambda
!    REFREL = (complex refr. index of sphere)/(real index of medium)
!    real refractive index of medium taken as 1.0 
! Returns:
!    QEXT  = efficiency factor for extinction
!    QSCA  = efficiency factor for scattering
!    QBACK = efficiency factor for backscatter
!            see Bohren & Huffman 1983 p. 122
!    GSCA = <cos> asymmetry for scattering
!
! Original program taken from Bohren and Huffman (1983), Appendix A
! Modified by Prof. Bruce T.Draine, Princeton Univ. Obs., 90/10/26
! in order to compute <cos(theta)>
! 91/05/07 (BTD): Modified to allow NANG=1
! 91/08/15 (BTD): Corrected error (failure to initialize P)
! 91/08/15 (BTD): Modified to enhance vectorizability.
! 91/08/15 (BTD): Modified to make NANG=2 if called with NANG=1
! 91/08/15 (BTD): Changed definition of QBACK.
! 92/01/08 (BTD): Converted to full double precision and double complex
!                 eliminated 2 unneed lines of code
!                 eliminated redundant variables (e.g. APSI,APSI0)
!                 renamed RN -> EN = double precision N
!                 Note that DOUBLE COMPLEX and DCMPLX are not part
!                 of f77 standard, so this version may not be fully
!                 portable.  In event that portable version is
!                 needed, use src/bhmie_f77.f
! 93/06/01 (BTD): Changed AMAX1 to generic function MAX
! FSB April 09,2012 This code was modified by: 
! Prof.  Francis S. Binkowski University of North Carolina at
! Chapel Hill, Institue for the Environment.
!
! The modifications were made to enhance computation speed 
! for use in a three-dimensional code. This was done by
! removing code that calculated angular scattering. The method
! of calculating QEXT, QBACK was also changed. 
 
!***********************************************************************
!*** Safety checks



           BHMIE_SUCCESS = .TRUE.
!           NANG = 2 ! FSB only this value 
! IF(NANG.GT.MXNANG)STOP'***Error: NANG > MXNANG in bhmie'
!         IF (NANG .LT. 2) NANG = 2

          DX = REAL( X, 8 )
! FSB D efine reciprocals so that divisions can be replaced by multiplications.      
           DX1  = ONE / DX
           DXX1 = DX1 * DX1
           DREFRL = DCMPLX( REAL( REFREL ), IMAG( REFREL ) )
           DREFRL1 = ONE / DREFRL
           Y = DX * DREFRL
           Y1 = ONE / Y
!           YMOD = ABS(Y)
 
!*** Series expansion terminated after NSTOP terms
!    Logarithmic derivatives calculated from NMX on down
!          XSTOP = X + 4.0 * X**0.3333 + 2.0
!          NMX  = MAX(XSTOP,YMOD) + 15

! BTD experiment 91/1/15: add one more term to series and compare results
!      NMX=AMAX1(XSTOP,YMOD)+16
! test: compute 7001 wavelengths between .0001 and 1000 micron
! for a=1.0micron SiC grain.  When NMX increased by 1, only a single
! computed number changed (out of 4*7001) and it only changed by 1/8387
! conclusion: we are indeed retaining enough terms in series!

          FACTOR = 1.0D0
 
!          IF (NMX .GT. NMXX) THEN
!             WRITE(6,*)'Error: NMX > NMXX=',NMXX,' for |m|x=',YMOD
!             BHMIE_SUCCESS = .FALSE.
!             RETURN
!          END IF

! FSB all code relating to scattering angles is removed out for
!     reasons of efficiency when running in a three-dimensional 
!     code. We only need QQSCA, QQEXT, GSCA AND QBACK

 
!*** Logarithmic derivative D(J) calculated by downward recurrence
!    beginning with initial value (0.,0.) 
 
          D(NMX) = COMPLEX_DZERO
          NN = NMX - 1
          DO N = 1,NN
             EN  = REAL( NMX - N + 1, 8 )
! FSB In the following division by Y has been replaced by 
!     multiplication by Y1, the reciprocal of Y.          
             D(NMX-N) = ( EN * Y1 ) - (ONE / ( D(NMX-N+1) + EN * Y1)) 
          END DO
 
!*** Riccati-Bessel functions with real argument X
!    calculated by upward recurrence
 
          PSI0 =  COS(DX)
          PSI1 =  SIN(DX)
          CHI0 = -SIN(DX)
          CHI1 =  PSI0
          XI1  =  DCMPLX(PSI1,-CHI1)
          QSCA =  0.0D0
          GSCA =  0.0D0
          QEXT =  0.0D0
          P    = -ONE
          XBACK = COMPLEX_DZERO

! FSB Start main loop       
          DO N = 1,NSTOP
             EN        = REAL( N, 8 )
             EN1       = ONE / EN
             TWO_N_M_ONE = TWO * EN - ONE
! for given N, PSI  = psi_n        CHI  = chi_n
!              PSI1 = psi_{n-1}    CHI1 = chi_{n-1}
!              PSI0 = psi_{n-2}    CHI0 = chi_{n-2}
! Calculate psi_n and chi_n
             PSI = TWO_N_M_ONE * PSI1 * DX1 - PSI0
             CHI = TWO_N_M_ONE * CHI1 * DX1 - CHI0
             XI  = DCMPLX(PSI,-CHI)
 
!*** Compute AN and BN:
! FSB Rearrange to get common terms
             FAC1 = D(N) * DREFRL1 + EN * DX1 
             AN   = (FAC1) * PSI - PSI1
             AN   = AN / ( (FAC1 )* XI - XI1 )
             FAC2 = ( DREFRL * D(N) + EN * DX1)
             BN   = ( FAC2) * PSI -PSI1
             BN   = BN / ((FAC2) * XI - XI1 )

! FSB calculate sum for QEXT as done by Wiscombe
!     get common factor
             TWO_N_P_ONE = (TWO * EN + ONE)
             QEXT = QEXT + (TWO_N_P_ONE) * (REAL(AN) + REAL(BN) ) 
             QSCA = QSCA + (TWO_N_P_ONE) * ( ABS(AN)**2 + ABS(BN)**2 )
          
! FSB calculate XBACK from B & H Page 122          
             FACTOR = -1.0d0 * FACTOR  ! calculate (-1.0 ** N)
             XBACK  = XBACK + (TWO_N_P_ONE) * factor * (AN - BN)
          
! FSB calculate asymmetry factor   
             
             GSCA = GSCA + REAL((TWO_N_P_ONE)/(EN * (EN + ONE)) * 
     &                (REAL(AN)*REAL(BN)+IMAG(AN)*IMAG(BN)))
             
              IF (N .GT. 1)THEN
                GSCA = GSCA + REAL((EN - EN1) 
     &                *  (REAL(AN1)*REAL(AN) + IMAG(AN1)*IMAG(AN) 
     &                +   REAL(BN1)*REAL(BN) + IMAG(BN1)*IMAG(BN)))
             ENDIF

!*** Store previous values of AN and BN for use in computation of g=<cos(theta)>
             AN1 = AN
             BN1 = BN

! FSB set up for next iteration
             PSI0 = PSI1
             PSI1 = PSI
             CHI0 = CHI1
             CHI1 = CHI
             XI1  = DCMPLX(PSI1,-CHI1)
          
          END DO   ! main  loop on n
 
!*** Have summed sufficient terms.

!    Now compute QQSCA,QQEXT,QBACK,and GSCA
          GSCA  = REAL( TWO / QSCA ) * GSCA

! FSB in the following, divisions by DX * DX has been replaced by
!      multiplication by DXX1 the reciprocal of 1.0 / (DX *DX)           
          QQSCA = REAL( TWO * QSCA * DXX1 )
          QQEXT = REAL( TWO * QEXT * DXX1 )
          QBACK = REAL( REAL( 0.5D0 * XBACK * CONJG(XBACK), 8 ) * DXX1 ) ! B&H Page 122

       END subroutine BHMIE_FLEXY

      end module wrf_fast_mie
