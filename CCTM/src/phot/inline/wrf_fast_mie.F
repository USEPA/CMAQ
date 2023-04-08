
       module wrf_fast_mie
       
       integer, parameter :: lunerr = -1

       REAL( 8 ), PUBLIC :: CPU_TIME_DELTA1 = 0.0D0 ! Timing individual
       REAL( 8 ), PUBLIC :: CPU_TIME_DELTA2 = 0.0D0 ! Timing individual
       REAL( 8 ), PUBLIC :: CPU_TIME_DELTA3 = 0.0D0 ! Timing individual
       
       public fast_mieaer, aero_optics_tabular
       
       integer, private :: logdev

       real, allocatable, private :: sizeaer( : ),tauaer( :,: )
       real, private :: l2,l3,l4,
     &                  l5,l6,l7

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
            real, allocatable ::  pmom2( :,:,: )      ! phase function expansion, #2
            real, allocatable ::  pmom3( :,:,: )      ! phase function expansion, #3
            real, allocatable ::  pmom4( :,:,: )      ! phase function expansion, #4
            real, allocatable ::  pmom5( :,:,: )      ! phase function expansion, #5
            real, allocatable ::  pmom6( :,:,: )      ! phase function expansion, #6
            real, allocatable ::  pmom7( :,:,: )      ! phase function expansion, #7
            real, allocatable ::  sback2p( :,:,: )    ! backscatter
         end Type

         Type(Optics_Tables_MP), Allocatable, Private :: Modal_Tables( : )

         Logical, Private :: Create_Table = .True.
         
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
!      print*,'sqrt2, xlnsig = ',sqrt2, xlnsig
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
!      print*,'mie_tabular, alfaip, alfaim = ',alfaip, alfaim
!        call fast_mieaer_modal( mode,iwave,dgv,crefin,qalfip_e,qalfip_s,gsalfp,bscoef )
!      call fast_mieaer( iwave,dgv,crefin,qalfip_e,qalfip_s,gsalfp,bscoef )

!       qalfip_e = 0.0
!       qalfip_s = 0.0
!       gsalfp   = 0.0
!       qalfim_e = 0.0
!       qalfim_s = 0.0
!       gsalfm   = 0.0
!       bscoef   = 0.0
       
        call fast_mieaer_modal( mie_volume_parameter,crefin,qalfip_e,qalfip_s,gsalfp,bscoef )
!       write(6,'(a,12(ES12.4,1X))')'AERO_TABULAR1:  qalfip_e,qalfip_s,gsalfp = ',mie_volume_parameter,
!     & qalfip_e,qalfip_s,gsalfp
!        call fast_mieaer_mp( mie_volume_parameter,crefin,qalfip_e,qalfip_s,gsalfp,bscoef )
!       write(6,'(a,12(ES12.4,1X))')'AERO_TABULAR2:  qalfip_e,qalfip_s,gsalfp = ',mie_volume_parameter,
!     & qalfip_e,qalfip_s,gsalfp
       
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
!       write(6,'(a,12(ES12.4,1X))')
!     & 'AERO_TABULAR1:  qalfip_e,qalfip_s,gsalfp,qalfim_e,qalfim_s,gsalfm = ',mie_par_top,mie_par_bot,
!     & qalfip_e,qalfip_s,gsalfp,qalfim_e,qalfim_s,gsalfm

!           call fast_mieaer_mp( mie_par_top,crefin,qalfip_e,qalfip_s,gsalfp,bscoef )
!           call fast_mieaer_mp( mie_par_bot,crefin,qalfim_e,qalfim_s,gsalfm,bscoef )
!       write(6,'(a,12(ES12.4,1X))')
!     & 'AERO_TABULAR2:  qalfip_e,qalfip_s,gsalfp,qalfim_e,qalfim_s,gsalfm = ',mie_par_top,mie_par_bot,
!     & qalfip_e,qalfip_s,gsalfp,qalfim_e,qalfim_s,gsalfm
     
     

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

!      write(6,'(a,12(ES12.4,1X))')'AERO_OPTICAL2: XX, QEXT,QSCA,G_MIE = ',mie_volume_parameter,beta_Ex,beta_Sc, &
!    & beta_Ex*lamdam1,beta_Ex*lamdam1,gfac
!      write(6,'(a,12(ES12.4,1X))')'AERO_OPTICAL2: RAD,WVL = ',0.5e2*dgn* exp(3.0 * LSIGX * LSIGX),lamda_in*1.e-3
!       write(6,'(a,12(ES12.4,1X))')'AERO_OPTICAL2: pi,dgn*exp(3.0*LSIGX*LSIGX),lamdam1 = ',pi, &
!     & dgn*exp(3.0*LSIGX*LSIGX),lamdam1,real(crefin),aimag(crefin)
       
99501  Format(I2,' Quadrature Points for Volume Averaged Aerosol Optics')
99504  Format('Even Number Quadrature Points for Volume Averaged Aerosol Optics')
       
       END SUBROUTINE aero_optics_tabular


        subroutine fast_mieaer( iwave,diameter_wet,refindx,extaer,scataer,gaer,bscoef )

         USE UTILIO_DEFN
         USE RUNTIME_VARS, ONLY : LOGDEV ! , SET_CPU_TIMING, TIMING_DELTA
         USE AERO_DATA           ! aero variable data
         USE CSQY_DATA,  ONLY : NWL => NWL_REF, NUM_REFRACTIVE, REFRACTIVE_INDEX,
     &                          WAVELENGTH => EFFWL_REF

         USE bhmie_routine
        
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
!       mode         -- array index for aerosol mode
!       iwave        -- array index for light wavelength
!       diameter_wet -- wet diameter, m
!       refindx      -- volume averaged complex index of refraction
! OUTPUT:
!   real sactaer ! aerosol single scattering efficiency
!        gaer    ! aerosol asymmetery factor
!        extaer  ! aerosol extinction efficiency, 
!        bscoef  ! aerosol backscatter efficiency, to convert steradian divide by 4*PI
!----------------------------------------------------------------------

! arguments:
        integer, intent(in)                   :: iwave
        real,    intent(in)                   :: diameter_wet
        complex, intent(in)                   :: refindx

        real, intent(out) :: extaer,scataer,gaer
        real, intent(out) :: bscoef

!local variables
         real, allocatable ,save :: wavmid( : ) ! cm
         real weighte, weights
! various bookeeping variables
         integer, save :: nbin_a_maxd ! Max # of aerosol bins or modes
         integer, save :: nspint      ! Num of spectral intervals across
         
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

 
         integer, save      :: numang=0,nmom=7,ipolzn=0,momdim=7
         integer, save      :: nrefr,nrefi
         integer, parameter :: prefr=22,prefi=33
         integer, parameter :: nsizes = 400
         integer, parameter :: nsiz=200,nlog=30,ncoef=50
         integer            :: nsize

         integer            :: nr,ni
         
!        
         REAL( 8 )     :: CPU_TIME_START
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
!        
         real, allocatable :: p2( : ),p3( : )        
         real, allocatable :: p4( : ),p5( : )
         real, allocatable :: p6( : ),p7( : )
         
         
         real, allocatable, save ::  extp( :,:,:,: )       ! specific extinction
         real, allocatable, save ::  albp( :,:,:,: )       ! single scat alb
         real, allocatable, save ::  asmp( :,:,:,: )       ! asymmetry factor
         real, allocatable, save ::  ascat( :,:,:,: )       ! scattering efficiency, JCB 2004/02/09
         real, allocatable, save ::  pmom2( :,:,:,: )       ! phase function expansion, #2
         real, allocatable, save ::  pmom3( :,:,:,: )       ! phase function expansion, #3
         real, allocatable, save ::  pmom4( :,:,:,: )       ! phase function expansion, #4
         real, allocatable, save ::  pmom5( :,:,:,: )       ! phase function expansion, #5
         real, allocatable, save ::  pmom6( :,:,:,: )       ! phase function expansion, #6
         real, allocatable, save ::  pmom7( :,:,:,: )         ! phase function expansion, #7
         real, allocatable, save ::  sback2p( :,:,:,: )    ! JCB 2007/02/01 - backscatter
         real ::  aero_radius

!--------------
! Coefficients of Chebychev Polynomials
         real cext(ncoef),casm(ncoef),cpmom2(ncoef)
         real cscat(ncoef)                          ! JCB 2004/02/09
         real cpmom3(ncoef),cpmom4(ncoef),cpmom5(ncoef)
         real cpmom6(ncoef),cpmom7(ncoef)
         real cpsback2p(ncoef)                      ! JCB 2007/02/09  - backscatter
         
         integer itab,jtab
         integer itabp,jtabp
         real    ttab,utab
         
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

         real(8) :: thesize        ! 2 pi radpart / waveleng = size parameter

!           specabs = absorption coeff / unit dry mass
!           specscat = scattering coeff / unit dry mass

         complex(8)       :: crefin,crefd
         complex(8), save :: crefw
         real, save :: rmin=0.005e-4,rmax=50.e-4   ! min, max aerosol size bin

         
         real bma,bpa
         
         real, save :: xrmin,xrmax,xr
         real xrad      ! normalized aerosol radius
         real ch(ncoef) ! chebychev polynomial
         real term1(ncoef) ! chebychev polynomial
         real term2(ncoef) ! chebychev polynomial
         real term3(ncoef) ! chebychev polynomial
         real term4(ncoef) ! chebychev polynomial
         
         real, save :: rhoh2o = 1.0     ! density of liquid water (g/cm3)
         
         real refr         ! real part of refractive index
         real refi         ! imaginary part of refractive index
         
         real thesize_bhmie    ! 2 pi radpart / waveleng = size parameter
         real qext_bhmie       ! array of extinction efficiencies
         real qsca_bhmie       ! array of scattering efficiencies
         real qback_bhmie      ! array of scattering efficiencies
         real gsca_bhmie       ! array of asymmetry factor * scattering efficiency
         
         real :: weight_real   ! nomralized real part refractive index difference used in bilinear interpolation 
         real :: weight_imag   ! nomralized imaginary part refractive index difference used in bilinear interpolation
         real :: wrwi          ! weighting factors used on bilinear interpolation points
         real :: wr_wrwi       ! weighting factors used on bilinear interpolation points
         real :: wi_wrwi       ! weighting factors used on bilinear interpolation points
         real :: one_wrwi_wi   ! weighting factors used on bilinear interpolation points
         
         complex refrel_bhmie

         real :: quadrature_factor
         real :: max_quad_point

         real, save :: refrmin ! minimum of real part of refractive index
         real, save :: refrmax ! maximum of real part of refractive index
         real, save :: refimin ! minimum of imag part of refractive index
         real, save :: refimax ! maximum of imag part of refractive index
         real, save :: drefr   ! increment in real part of refractive index
         real, save :: drefi   ! increment in imag part of refractive index
         real, save :: refrtab(prefr) ! table of real refractive indices for aerosols
         real, save :: refitab(prefi) ! table of imag refractive indices for aerosols
         complex specrefndx(ltype)    ! refractivr indices

         real, save :: reciprocal_drefr  ! reciprocal of increment in real part of refractive index
         real, save :: reciprocal_drefi  ! reciprocal of increment in  imag part of refractive index
         real, save :: reciprocal_drefri ! reciprocal of real times imag increments of refractive index

         real, parameter :: pie   = pi
         real, parameter :: third = 1.0/3.0
         
         integer irams, jrams
! diagnostic declarations
         integer kcallmieaer2
         integer ibin, imode
         character(150) :: msg

      if(first)then
        first=.false.

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
        
        do i=1,NUM_REFRACTIVE
           refrmin = amin1( refrmin,MINVAL( REFRACTIVE_INDEX( i )%REAL_PART( :,: ) ))
           refrmax = amax1( refrmax,MAXVAL( REFRACTIVE_INDEX( i )%REAL_PART( :,: ) ))
           refimin = amin1( refimin,-MINVAL( REFRACTIVE_INDEX( i )%IMAG_PART( :,: ) ))  
           refimax = amax1( refimax,-MAXVAL( REFRACTIVE_INDEX( i )%IMAG_PART( :,: ) ))
        enddo
        
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

         nbin_a_maxd = n_mode
!
         
         nspint = nwl
         allocate( wavmid( nwl ) )

         allocate( extp(ncoef,prefr,prefi,nspint),      
     &             albp(ncoef,prefr,prefi,nspint),   
     &             asmp(ncoef,prefr,prefi,nspint),   
     &             ascat(ncoef,prefr,prefi,nspint),  
     &             pmom2(ncoef,prefr,prefi,nspint),  
     &             pmom3(ncoef,prefr,prefi,nspint),  
     &             pmom4(ncoef,prefr,prefi,nspint),  
     &             pmom5(ncoef,prefr,prefi,nspint),  
     &             pmom6(ncoef,prefr,prefi,nspint),  
     &             pmom7(ncoef,prefr,prefi,nspint),  
     &             sback2p(ncoef,prefr,prefi,nspint) )
     
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

          quadrature_factor = exp( sqrt2 * log( max_sigma_g ) * max_quad_point )
         
         rmax = amax1(rmax,60.0*MAXVAL(max_dg_wet))*quadrature_factor
         rmin = amin1(rmin,40.0*MINVAL(min_dg_dry))/quadrature_factor
         bma=0.5*alog(rmax/rmin) ! JCB
         bpa=0.5*alog(rmax*rmin) ! JCB
         xrmin=alog(rmin)
         xrmax=alog(rmax)


         
         nsize = nsizes
         
         allocate( qext( nsize ),
     &             qsca( nsize ),
     &             gqsc( nsize ) )
      
         allocate( asymm( nsize ),
     &              scat( nsize ),
     &               sb2( nsize ),
     &            qextr4( nsize ),
     &                rs( nsize ) )          

          allocate( p2(nsize),p3(nsize),p4(nsize),p5(nsize) )
          allocate( p6(nsize),p7(nsize) )
          
!wavelength loop                                            
         do 200 ns=1,nspint                                          
                                                                     
            wavmid(ns) =  1.0e-7*wavelength(ns)                         
!         calibrate parameterization with range of refractive indices 

!                 print*,"computing table for wavelength ",wavelength(ns)

            do 120 ni=1,nrefi
              do 120 nr=1,nrefr                                        

               refrtab(nr)=refrmin+(nr-1)*drefr
               refitab(ni)=refimin+(ni-1)*drefi
               crefd=dcmplx(real(refrtab(nr),8),real(refitab(ni),8))
!              mie calculations of optical efficiencies
                  nsize = nsizes
                  do n=1,nsize
                  
                     xr=cos(pie*(real(n)-0.5)/real(nsize))
                     rs(n)=real(exp(xr*bma+bpa),8)
!                    size parameter and weighted refractive index
                     thesize=real(2.*pie*rs(n)/wavmid(ns),8)
!                     thesize=min(thesize,10000.d0)
                  
                     if( .Not. TWO_STREAM )then
                         call miev0(thesize,crefd,perfct,mimcut,anyang, 
     &                       numang,xmu,nmom,ipolzn,momdim,prnt,
     &                       qext(n),qsca(n),gqsc(n),pmom,sforw,sback,s1,
     &                       s2,tforw,tback )
                         qextr4(n)=qext(n)
!                        qabs(n)=qext(n)-qsca(n) ! not necessary anymore JCB 2004/02/09
                         scat(n)=qsca(n) ! JCB 2004/02/09
!                         asymm(n)=gqsc(n)*scat(n) ! assume always greater than zero
                         asymm(n)=gqsc(n)/scat(n) ! assume always greater than zero
                       
! coefficients of phase function expansion; note modification by JCB of miev0 coefficients
                         p2(n)=pmom(2,1)/pmom(0,1)*5.0
                         p3(n)=pmom(3,1)/pmom(0,1)*7.0
                         p4(n)=pmom(4,1)/pmom(0,1)*9.0
                         p5(n)=pmom(5,1)/pmom(0,1)*11.0
                         p6(n)=pmom(6,1)/pmom(0,1)*13.0
                         p7(n)=pmom(7,1)/pmom(0,1)*15.0
! backscattering efficiency, Bohren and Huffman, page 122
! as stated by Bohren and Huffman, this is 4*pie times what is should be
! may need to be smoothed - a very rough function - for the time being we won't apply smoothing
! and let the integration over the size distribution be the smoothing
                            
                     else
                     
                         thesize_bhmie = real( thesize )
                         refrel_bhmie  = cmplx(real(refrtab(nr)),-real(refitab(ni)))
                         call driver_bhmie_flexy(thesize_bhmie,refrel_bhmie,qext_bhmie,qsca_bhmie,gsca_bhmie) 
                         asymm(n) = gsca_bhmie/qsca_bhmie 
                         qextr4(n)= qext_bhmie*thesize_bhmie
                         scat(n)  = qsca_bhmie*thesize_bhmie 
                         sback    = sback_bhmie
                         sb2(n)   = 4.0*sback*dconjg(sback)
     &                            / (thesize_bhmie*thesize_bhmie) ! JCB 2007/02/01  
                         
                     endif
                  
                  enddo
  100             continue

                  call fitcurv(rs,qextr4,extp(:,nr,ni,ns),ncoef,nsize)
                  call fitcurv(rs,scat,ascat(:,nr,ni,ns),ncoef,nsize) ! JCB 2004/02/07 - scattering efficiency
                  call fitcurv(rs,asymm,asmp(:,nr,ni,ns),ncoef,nsize)
                  call fitcurv(rs,sb2,sback2p(:,nr,ni,ns),ncoef,nsize) ! JCB 2007/02/01 - backscattering efficiency             
                                       
                  if( .Not. TWO_STREAM )then
                   
                     call fitcurv_nolog(rs,p2,pmom2(:,nr,ni,ns),ncoef,nsize)
                     call fitcurv_nolog(rs,p3,pmom3(:,nr,ni,ns),ncoef,nsize)
                     call fitcurv_nolog(rs,p4,pmom4(:,nr,ni,ns),ncoef,nsize)
                     call fitcurv_nolog(rs,p5,pmom5(:,nr,ni,ns),ncoef,nsize)
                     call fitcurv_nolog(rs,p6,pmom6(:,nr,ni,ns),ncoef,nsize)
                     call fitcurv_nolog(rs,p7,pmom7(:,nr,ni,ns),ncoef,nsize)
                  
                  endif

  120       continue
  200    continue

         deallocate( qext,
     &               qsca,
     &               gqsc  )
      
         deallocate( asymm, 
     &                scat, 
     &                 sb2, 
     &              qextr4, 
     &                  rs  ) 
     
         deallocate( p2,p3,p4,p5 )
         deallocate( p6,p7 )
         
      endif ! end first call block
      
!         CPU_TIME_START = SET_CPU_TIMING()
         
         thesum=0.0
         thesum=thesum+1.0e-6
         aero_radius= 50.0*diameter_wet
         ns         = iwave
         m          = 3

!               tauaer=0.0
!               waer=0.0
               gaer=0.0
               sizeaer=0.0
               extaer=0.0
               l2=0.0
               l3=0.0
               l4=0.0
               l5=0.0
               l6=0.0
               l7=0.0
               bscoef=0.0        ! JCB 2007/02/01 - backscattering coefficient
! loop over the bins
                  sizem=aero_radius ! radius in cm
! check limits of particle size
! rce 2004-dec-07 - use klevel in write statements
                  if(aero_radius.le.rmin)then
                    write( msg, '(a, 1x, es11.4, a, 1x, es11.4 )' )       
     &              'AE Mode, WRF FAST mie: wet aerosol diameter set to ',
     &              0.02*rmin, ' from ', diameter_wet
                    aero_radius=rmin
                     write(logdev,'(a)')TRim( msg )
                  endif
!
                  if(aero_radius.gt.rmax)then
                    write( msg, '(a, 1x, es11.4, a, 1x, es11.4 )' )       
     &              'AE Mode, WRF FAST mie: wet aerosol diameter set to ',
     &              0.02*rmax, ' from ', diameter_wet
                     write(logdev,'(a)')Trim( msg )
                     aero_radius=rmax
                  endif


                  crefin=refindx
                  refr=real(crefin)
! change Rahul's imaginary part of the index of refraction from positive to negative
                  refi=-imag(crefin)

                  x=alog(aero_radius) ! radius in cm
                  xrad=x
                  thesize=2.0*pie*aero_radius/wavmid(ns)
! normalize size parameter
                  xrad=(2.0*xrad-xrmax-xrmin)
     &                /(xrmax-xrmin)
     
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
                  
                  
                  itab = int( (refr-refrmin)*reciprocal_drefr ) + 1
                  itab = min( nrefr,max( itab,1 ) )
                  jtab = int( (refi-refimin)*reciprocal_drefi ) + 1
                  jtab = min( nrefi,max( jtab,1 ) )
!                  print*,'ShortCut itab, jtab = ',itab,jtab
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
                     cscat(nc) = one_wrwi_wi*extp(nc,itab,jtab,ns) + wr_wrwi*extp(nc,itabp,jtab,ns)
     &                        +          wrwi*extp(nc,itabp,jtabp,ns) + wi_wrwi*extp(nc,itab,jtabp,ns)                     
                     cscat(nc) = one_wrwi_wi*ascat(nc,itab,jtab,ns) + wr_wrwi*ascat(nc,itabp,jtab,ns)
     &                         +         wrwi*ascat(nc,itabp,jtabp,ns) + wi_wrwi*ascat(nc,itab,jtabp,ns)                     
                     casm(nc)  = one_wrwi_wi*asmp(nc,itab,jtab,ns) + wr_wrwi*asmp(nc,itabp,jtab,ns)
     &                         +       wrwi*asmp(nc,itabp,jtabp,ns) + wi_wrwi*asmp(nc,itab,jtabp,ns)                     
                     cpsback2p(nc)  = one_wrwi_wi*sback2p(nc,itab,jtab,ns) + wr_wrwi*sback2p(nc,itabp,jtab,ns)
     &                              +  wrwi*sback2p(nc,itabp,jtabp,ns) + wi_wrwi*sback2p(nc,itab,jtabp,ns)                     
                  enddo

! interpolate coefficients linear in refractive index
! first call calcs itab,jtab,ttab,utab
                  itab=0
                  search = .true.
                  
                  
                  
!                  call binterp(extp(:,:,:,ns),ncoef,nrefr,nrefi,   
!     &                          refr,refi,refrtab,refitab,search,itab,jtab,
!     &                          ttab,utab,cext)
!                  print*,'binterp itab, jtab = ',itab,jtab 
!                  do nc=1,ncoef
!                     write(6,'(a,4(es12.4,1x))')'shortcut vs binterp cext = ',cscat(nc),cext(nc),
!     &               cscat(nc)-cext(nc)                     
!                  end do
!! JCB 2004/02/09  -- new code for scattering cross section
!
!                  search = .false.
!                  
!                  call binterp(ascat(:,:,:,ns),ncoef,nrefr,nrefi,   
!     &                          refr,refi,refrtab,refitab,search,itab,jtab,
!     &                          ttab,utab,cscat)
!                  call binterp(asmp(:,:,:,ns),ncoef,nrefr,nrefi,   
!     &                          refr,refi,refrtab,refitab,search,itab,jtab,
!     &                          ttab,utab,casm)
!                  call binterp(sback2p(:,:,:,ns),ncoef,nrefr,nrefi, 
!     &                          refr,refi,refrtab,refitab,search,itab,jtab,
!     &                          ttab,utab,cpsback2p)
!     
     
!                  CALL TIMING_DELTA( CPU_TIME_START, CPU_TIME_DELTA1 )
                  
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

       
! JCB 2004/02/09 -- for scattering efficiency
!                  pscat=0.5*cscat(1)
!                  do nc=2,ncoef
!                      pscat=pscat+ch(nc)*cscat(nc)
!                  enddo
!                  pscat=exp(pscat)
!                 
!                  pasm=0.5*casm(1)
!                  do nc=2,ncoef
!                      pasm=pasm+ch(nc)*casm(nc)
!                  enddo
!                  pasm=exp(pasm)
!                     
!                  sback2=0.5*cpsback2p(1) ! JCB 2007/02/01 - backscattering efficiency
!                  do nc=2,ncoef
!                      sback2=sback2+ch(nc)*cpsback2p(nc)
!                  enddo
!                  sback2=exp(sback2)
!                  if(sback2.le.0.0)sback2=0.0


                  pext  = pext/thesize 
                  pscat = pscat/thesize 
                  pasm  = pasm * pscat
                  extaer  = pext
                  scataer = pscat
                  gaer    = pasm
! convert backscattering efficiency to backscattering coefficient, units (cm)^-1
                  bscoef= sback2/thesize

!               CALL TIMING_DELTA( CPU_TIME_START, CPU_TIME_DELTA2 )
                              
                  if( .Not. TWO_STREAM )then

                      do nc=1,ncoef
                         cpmom2(nc) = one_wrwi_wi*pmom2(nc,itab,jtab,ns) + wr_wrwi*pmom2(nc,itabp,jtab,ns)
     &                              +        wrwi*pmom2(nc,itabp,jtabp,ns) + wi_wrwi*pmom2(nc,itab,jtabp,ns)                     
                         cpmom3(nc) = one_wrwi_wi*pmom3(nc,itab,jtab,ns) + wr_wrwi*pmom3(nc,itabp,jtab,ns)
     &                              +        wrwi*pmom3(nc,itabp,jtabp,ns) + wi_wrwi*pmom3(nc,itab,jtabp,ns)                     
                         cpmom4(nc) = one_wrwi_wi*pmom2(nc,itab,jtab,ns) + wr_wrwi*pmom4(nc,itabp,jtab,ns)
     &                              +        wrwi*pmom4(nc,itabp,jtabp,ns) + wi_wrwi*pmom4(nc,itab,jtabp,ns)                     
                         cpmom5(nc) = one_wrwi_wi*pmom5(nc,itab,jtab,ns) + wr_wrwi*pmom5(nc,itabp,jtab,ns)
     &                              +        wrwi*pmom5(nc,itabp,jtabp,ns) + wi_wrwi*pmom5(nc,itab,jtabp,ns)                     
                         cpmom6(nc) = one_wrwi_wi*pmom6(nc,itab,jtab,ns) + wr_wrwi*pmom6(nc,itabp,jtab,ns)
     &                              +        wrwi*pmom6(nc,itabp,jtabp,ns) + wi_wrwi*pmom6(nc,itab,jtabp,ns)                     
                         cpmom7(nc) = one_wrwi_wi*pmom7(nc,itab,jtab,ns) + wr_wrwi*pmom7(nc,itabp,jtab,ns)
     &                              +        wrwi*pmom7(nc,itabp,jtabp,ns) + wi_wrwi*pmom7(nc,itab,jtabp,ns)                     
                      enddo
                      
!                      call binterp(pmom2(:,:,:,ns),ncoef,nrefr,nrefi,   
!     &                              refr,refi,refrtab,refitab,search,itab,jtab,
!     &                              ttab,utab,cpmom2)
!                      call binterp(pmom3(:,:,:,ns),ncoef,nrefr,nrefi,   
!     &                              refr,refi,refrtab,refitab,search,itab,jtab,
!     &                              ttab,utab,cpmom3)
!                      call binterp(pmom4(:,:,:,ns),ncoef,nrefr,nrefi,   
!     &                              refr,refi,refrtab,refitab,search,itab,jtab,
!     &                              ttab,utab,cpmom4)
!                      call binterp(pmom5(:,:,:,ns),ncoef,nrefr,nrefi,   
!     &                              refr,refi,refrtab,refitab,search,itab,jtab,
!     &                              ttab,utab,cpmom5)
!                      call binterp(pmom6(:,:,:,ns),ncoef,nrefr,nrefi,   
!     &                              refr,refi,refrtab,refitab,search,itab,jtab,
!     &                              ttab,utab,cpmom6)
!                      call binterp(pmom7(:,:,:,ns),ncoef,nrefr,nrefi,   
!     &                              refr,refi,refrtab,refitab,search,itab,jtab,
!     &                              ttab,utab,cpmom7)

                      ppmom2=0.5*cpmom2(1)
                      ppmom3=0.5*cpmom3(1)
                      ppmom4=0.5*cpmom4(1)
                      if( sizem .le. 0.03e-04 )then
                          ppmom5 = 0.0
                          ppmom6 = 0.0
                          ppmom7 = 0.0
                      else    
                          ppmom5=0.5*cpmom5(1)
                          ppmom6=0.5*cpmom6(1)
                          ppmom7=0.5*cpmom7(1)
                      endif
                      
                      do nc=2,ncoef
                          ppmom2=ppmom2+ch(nc)*cpmom2(nc)
                          ppmom3=ppmom3+ch(nc)*cpmom3(nc)
                          ppmom4=ppmom4+ch(nc)*cpmom4(nc)
                          if( sizem .gt. 0.03e-04 )then
                             ppmom5=ppmom5+ch(nc)*cpmom5(nc)
                             ppmom6=ppmom6+ch(nc)*cpmom6(nc)
                             ppmom7=ppmom7+ch(nc)*cpmom7(nc)
                          endif
                      enddo
                      
                      ppmom2 = max( ppmom2,0.0 )
                      ppmom3 = max( ppmom3,0.0 )
                      ppmom4 = max( ppmom4,0.0 )
                      if( sizem .gt. 0.03e-04 )then
                          ppmom5 = max( ppmom5,0.0 )
                          ppmom6 = max( ppmom6,0.0 )
                          ppmom7 = max( ppmom7,0.0 )
                      endif
                      
!                      if(ppmom2.le.0.0)ppmom2=0.0
!                   
!                      ppmom3=0.5*cpmom3(1)
!                      do nc=2,ncoef
!                          ppmom3=ppmom3+ch(nc)*cpmom3(nc)
!                      enddo
!!                      ppmom3=exp(ppmom3)  ! no exponentiation required
!                      if(ppmom3.le.0.0)ppmom3=0.0
!!                     
!                      ppmom4=0.5*cpmom4(1)
!                      do nc=2,ncoef
!                          ppmom4=ppmom4+ch(nc)*cpmom4(nc)
!                      enddo
!                      if(ppmom4.le.0.0.or.sizem.le.0.03e-04)ppmom4=0.0
!!                     
!                      ppmom5=0.5*cpmom5(1)
!                      do nc=2,ncoef
!                          ppmom5=ppmom5+ch(nc)*cpmom5(nc)
!                      enddo
!                      if(ppmom5.le.0.0.or.sizem.le.0.03e-04)ppmom5=0.0
!!                     
!                      ppmom6=0.5*cpmom6(1)
!                      do nc=2,ncoef
!                          ppmom6=ppmom6+ch(nc)*cpmom6(nc)
!                      enddo
!                      if(ppmom6.le.0.0.or.sizem.le.0.03e-04)ppmom6=0.0
!!                     
!                      ppmom7=0.5*cpmom7(1)
!                      do nc=2,ncoef
!                          ppmom7=ppmom7+ch(nc)*cpmom7(nc)
!                      enddo
!                      if(ppmom7.le.0.0.or.sizem.le.0.03e-04)ppmom7=0.0

! need weighting by scattering cross section ?  JCB 2004/02/09
                   l2=ppmom2
                   l3=ppmom3
                   l4=ppmom4
                   l5=ppmom5
                   l6=ppmom6
                   l7=ppmom7       
                endif 


      return
      end subroutine fast_mieaer                    
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
!*************************************************************************
      subroutine binterp(table,km,im,jm,x,y,xtab,ytab,search,ix,jy,t,u,xout)

!     bilinear interpolation of table
!
      implicit none

       real,    intent( in )    :: table(:,:,:)
       integer, intent( in )    :: im,jm,km
       real,    intent( in )    :: x,y
       real,    intent( in )    :: xtab(:),ytab(:)
       logical, intent( in )    :: search
       integer, intent( inout ) :: ix,jy
       real,    intent( inout ) :: t,u
       real,    intent( out )   :: xout(:)

!local:       
       integer :: i,j,k
       real    :: dx,dy
       
       real, save    :: tu,tuc,tcu,tcuc
       integer, save :: ip1,jp1

       if ( search ) then
!      if(ix.gt.0)go to 30
      if(ix.lt.1)then 
         if(im.gt.1)then
           do i=1,im
             if(x.lt.xtab(i))exit ! go to 10
           enddo
   10      ix=max0(i-1,1)
           ip1=min0(ix+1,im)
           dx=(xtab(ip1)-xtab(ix))
           if(abs(dx).gt.1.e-20)then
              t=(x-xtab(ix))/(xtab(ix+1)-xtab(ix))
           else
              t=0
           endif
         else
           ix=1
           ip1=1
           t=0
         endif
         if(jm.gt.1)then
           do j=1,jm
             if(y.lt.ytab(j))exit ! go to 20
           enddo
   20      jy=max0(j-1,1)
           jp1=min0(jy+1,jm)
           dy=(ytab(jp1)-ytab(jy))
           if(abs(dy).gt.1.e-20)then
              u=(y-ytab(jy))/dy
           else
              u=0
           endif
         else
           jy=1
           jp1=1
           u=0
         endif
         tu=t*u
         tuc=t-tu
         tcuc=1-tuc-u
         tcu=u-tu
      endif
   30 continue

      end if ! search 
      
      do k=1,km
         xout(k)=tcuc*table(k,ix,jy)+tuc*table(k,ip1,jy)   
     &          +tu*table(k,ip1,jp1)+tcu*table(k,ix,jp1)
      enddo

      return
      end subroutine binterp                                            
!***************************************************************
      subroutine  miev0 ( xx, crefin, perfct, mimcut, anyang,   
     &                     numang, xmu, nmom, ipolzn, momdim, prnt,   
     &                     qext, qsca, gqsc, pmom, sforw, sback, s1,   
     &                     s2, tforw, tback )
!
!    computes mie scattering and extinction efficiencies; asymmetry
!    factor;  forward- and backscatter amplitude;  scattering
!    amplitudes for incident polarization parallel and perpendicular
!    to the plane of scattering, as functions of scattering angle;
!    coefficients in the legendre polynomial expansions of either the
!    unpolarized phase function or the polarized phase matrix;
!    and some quantities needed in polarized radiative transfer.
!
!      calls :  biga, ckinmi, small1, small2, testmi, miprnt,
!               lpcoef, errmsg
!
!      i n t e r n a l   v a r i a b l e s
!      -----------------------------------
!
!  an,bn           mie coefficients  little-a-sub-n, little-b-sub-n
!                     ( ref. 1, eq. 16 )
!  anm1,bnm1       mie coefficients  little-a-sub-(n-1),
!                     little-b-sub-(n-1);  used in -gqsc- sum
!  anp             coeffs. in s+ expansion ( ref. 2, p. 1507 )
!  bnp             coeffs. in s- expansion ( ref. 2, p. 1507 )
!  anpm            coeffs. in s+ expansion ( ref. 2, p. 1507 )
!                     when  mu  is replaced by  - mu
!  bnpm            coeffs. in s- expansion ( ref. 2, p. 1507 )
!                     when  mu  is replaced by  - mu
!  calcmo(k)       true, calculate moments for k-th phase quantity
!                     (derived from -ipolzn-; used only in 'lpcoef')
!  cbiga(n)        bessel function ratio capital-a-sub-n (ref. 2, eq. 2)
!                     ( complex version )
!  cior            complex index of refraction with negative
!                     imaginary part (van de hulst convention)
!  cioriv          1 / cior
!  coeff           ( 2n + 1 ) / ( n ( n + 1 ) )
!  fn              floating point version of index in loop performing
!                     mie series summation
!  lita,litb(n)    mie coefficients -an-, -bn-, saved in arrays for
!                     use in calculating legendre moments *pmom*
!  maxtrm          max. possible no. of terms in mie series
!  mm              + 1 and  - 1,  alternately.
!  mim             magnitude of imaginary refractive index
!  mre             real part of refractive index
!  maxang          max. possible value of input variable -numang-
!  nangd2          (numang+1)/2 ( no. of angles in 0-90 deg; anyang=f )
!  noabs           true, sphere non-absorbing (determined by -mimcut-)
!  np1dn           ( n + 1 ) / n
!  npquan          highest-numbered phase quantity for which moments are
!                     to be calculated (the largest digit in -ipolzn-
!                     if  ipolzn .ne. 0)
!  ntrm            no. of terms in mie series
!  pass1           true on first entry, false thereafter; for self-test
!  pin(j)          angular function little-pi-sub-n ( ref. 2, eq. 3 )
!                     at j-th angle
!  pinm1(j)        little-pi-sub-(n-1) ( see -pin- ) at j-th angle
!  psinm1          ricatti-bessel function psi-sub-(n-1), argument -xx-
!  psin            ricatti-bessel function psi-sub-n of argument -xx-
!                     ( ref. 1, p. 11 ff. )
!  rbiga(n)        bessel function ratio capital-a-sub-n (ref. 2, eq. 2)
!                     ( real version, for when imag refrac index = 0 )
!  rioriv          1 / mre
!  rn              1 / n
!  rtmp            (real) temporary variable
!  sp(j)           s+  for j-th angle  ( ref. 2, p. 1507 )
!  sm(j)           s-  for j-th angle  ( ref. 2, p. 1507 )
!  sps(j)          s+  for (numang+1-j)-th angle ( anyang=false )
!  sms(j)          s-  for (numang+1-j)-th angle ( anyang=false )
!  taun            angular function little-tau-sub-n ( ref. 2, eq. 4 )
!                     at j-th angle
!  tcoef           n ( n+1 ) ( 2n+1 ) (for summing tforw,tback series)
!  twonp1          2n + 1
!  yesang          true if scattering amplitudes are to be calculated
!  zetnm1          ricatti-bessel function  zeta-sub-(n-1) of argument
!                     -xx-  ( ref. 2, eq. 17 )
!  zetn            ricatti-bessel function  zeta-sub-n of argument -xx-
!
! ----------------------------------------------------------------------
! --------  i / o specifications for subroutine miev0  -----------------
! ----------------------------------------------------------------------
      implicit none
      logical  anyang, perfct, prnt(*)
      integer  ipolzn, momdim, numang, nmom
      real*8     gqsc, mimcut, pmom( 0:momdim, * ), qext, qsca,   
     &          xmu(*), xx
      complex*16  crefin, sforw, sback, s1(*), s2(*), tforw(*),   
     &          tback(*)
      integer maxang,mxang2,maxtrm
      real*8 onethr
! ----------------------------------------------------------------------
!
      parameter ( maxang = 501, mxang2 = maxang/2 + 1 )
!
!                                  ** note --  maxtrm = 10100  is neces-
!                                  ** sary to do some of the test probs,
!                                  ** but 1100 is sufficient for most
!                                  ** conceivable applications
!     parameter ( maxtrm = 1100 )
      parameter ( maxtrm = 10100 )
      parameter ( onethr = 1./3. )
!
      logical   anysav, calcmo(4), noabs, ok, pass1, persav, yesang
      integer   npquan
      integer i,j,n,nmosav,iposav,numsav,ntrm,nangd2
      real*8      mim, mimsav, mre, mm, np1dn
      real*8 rioriv,xmusav,xxsav,sq,fn,rn,twonp1,tcoef, coeff
      real*8 xinv,psinm1,chinm1,psin,chin,rtmp,taun
      real*8      rbiga( maxtrm ), pin( maxang ), pinm1( maxang )
      complex*16   an, bn, anm1, bnm1, anp, bnp, anpm, bnpm, cresav,   
     &           cior, cioriv, ctmp, zet, zetnm1, zetn
      complex*16   cbiga( maxtrm ), lita( maxtrm ), litb( maxtrm ),   
     &           sp( maxang ), sm( maxang ), sps( mxang2 ), sms( mxang2 )
      equivalence  ( cbiga, rbiga )
      save  pass1
      sq( ctmp ) = dble( ctmp )**2 + dimag( ctmp )**2
      data  pass1 / .true. /
!
!
      if ( pass1 )  then
!                                   ** save certain user input values
         xxsav  = xx
         cresav = crefin
         mimsav = mimcut
         persav = perfct
         anysav = anyang
         nmosav = nmom
         iposav = ipolzn
         numsav = numang
         xmusav = xmu( 1 )
!                              ** reset input values for test case
         xx      = 10.0
         crefin  = ( 1.5, - 0.1 )
         perfct  = .false.
         mimcut  = 0.0
         anyang  = .true.
         numang  = 1
         xmu( 1 )= - 0.7660444
         nmom    = 1
         ipolzn  = - 1
!
      end if
!                                        ** check input and calculate
!                                        ** certain variables from input
!
   10 call  ckinmi( numang, maxang, xx, perfct, crefin, momdim,   
     &               nmom, ipolzn, anyang, xmu, calcmo, npquan )
!
      if ( perfct .and. xx .le. 0.1 )  then
!                                            ** use totally-reflecting
!                                            ** small-particle limit
!
         call  small1 ( xx, numang, xmu, qext, qsca, gqsc, sforw,   
     &                   sback, s1, s2, tforw, tback, lita, litb )
         ntrm = 2
         go to 200
      end if
!
      if ( .not.perfct )  then
!
         cior = crefin
         if ( dimag( cior ) .gt. 0.0 )  cior = dconjg( cior )
         mre =     dble( cior )
         mim =  - dimag( cior )
         noabs = mim .le. mimcut
         cioriv = 1.0 / cior
         rioriv = 1.0 / mre
!
         if ( xx * dmax1( 1.d0, cdabs(cior) ) .le. 0.d1 ) then
!
!                                    ** use general-refractive-index
!                                    ** small-particle limit
!                                    ** ( ref. 2, p. 1508 )
!
            call  small2 ( xx, cior, .not.noabs, numang, xmu, qext,   
     &                     qsca, gqsc, sforw, sback, s1, s2, tforw,   
     &                     tback, lita, litb )
            ntrm = 2
            go to 200
         end if
!
      end if
!
      nangd2 = ( numang + 1 ) / 2
      yesang = numang .gt. 0
!                              ** estimate number of terms in mie series
!                              ** ( ref. 2, p. 1508 )
      if ( xx.le.8.0 )  then
         ntrm = xx + 4. * xx**onethr + 1.
      else if ( xx.lt.4200. )  then
         ntrm = xx + 4.05 * xx**onethr + 2.
      else
         ntrm = xx + 4. * xx**onethr + 2.
      end if
      if ( ntrm+1 .gt. maxtrm )then   
           call errmsg( 'miev0--parameter maxtrm too small', .true. )
      endif
!
!                            ** calculate logarithmic derivatives of
!                            ** j-bessel-fcn., big-a-sub-(1 to ntrm)
      if ( .not.perfct )then
           call  biga( cior, xx, ntrm, noabs, yesang, rbiga, cbiga )
      endif           
!
!                            ** initialize ricatti-bessel functions
!                            ** (psi,chi,zeta)-sub-(0,1) for upward
!                            ** recurrence ( ref. 1, eq. 19 )
      xinv = 1.0 / xx
      psinm1   = dsin( xx )
      chinm1   = dcos( xx )
      psin = psinm1 * xinv - chinm1
      chin = chinm1 * xinv + psinm1
      zetnm1 = dcmplx( psinm1, chinm1 )
      zetn   = dcmplx( psin, chin )
!                                     ** initialize previous coeffi-
!                                     ** cients for -gqsc- series
      anm1 = ( 0.0, 0.0 )
      bnm1 = ( 0.0, 0.0 )
!                             ** initialize angular function little-pi
!                             ** and sums for s+, s- ( ref. 2, p. 1507 )
      if ( anyang )  then
         do  60  j = 1, numang
            pinm1( j ) = 0.0
            pin( j )   = 1.0
            sp ( j ) = ( 0.0, 0.0 )
            sm ( j ) = ( 0.0, 0.0 )
   60    continue
      else
         do  70  j = 1, nangd2
            pinm1( j ) = 0.0
            pin( j )   = 1.0
            sp ( j ) = ( 0.0, 0.0 )
            sm ( j ) = ( 0.0, 0.0 )
            sps( j ) = ( 0.0, 0.0 )
            sms( j ) = ( 0.0, 0.0 )
   70    continue
      end if
!                         ** initialize mie sums for efficiencies, etc.
      qsca = 0.0
      gqsc = 0.0
      sforw      = ( 0., 0. )
      sback      = ( 0., 0. )
      tforw( 1 ) = ( 0., 0. )
      tback( 1 ) = ( 0., 0. )
!
!
! ---------  loop to sum mie series  -----------------------------------
!
      mm = + 1.0
      do  100  n = 1, ntrm
!                           ** compute various numerical coefficients
         fn     = n
         rn     = 1.0 / fn
         np1dn  = 1.0 + rn
         twonp1 = 2 * n + 1
         coeff  = twonp1 / ( fn * ( n + 1 ) )
         tcoef  = twonp1 * ( fn * ( n + 1 ) )
!
!                              ** calculate mie series coefficients
         if ( perfct )  then
!                                   ** totally-reflecting case
!
            an = ( ( fn*xinv ) * psin - psinm1 ) /   
     &           ( ( fn*xinv ) * zetn - zetnm1 )
            bn = psin / zetn
!
         else if ( noabs )  then
!                                      ** no-absorption case
!
            an =  ( ( rioriv*rbiga(n) + ( fn*xinv ) ) * psin - psinm1 )   
     &          / ( ( rioriv*rbiga(n) + ( fn*xinv ) ) * zetn - zetnm1 )
            bn =  ( (  mre * rbiga(n) + ( fn*xinv ) ) * psin - psinm1 )   
     &           / ( (  mre * rbiga(n) + ( fn*xinv ) ) * zetn - zetnm1 )
         else
!                                       ** absorptive case
!
            an = ( ( cioriv * cbiga(n) + ( fn*xinv ) ) * psin - psinm1 )
     &          /( ( cioriv * cbiga(n) + ( fn*xinv ) ) * zetn - zetnm1 )
            bn = ( (   cior * cbiga(n) + ( fn*xinv ) ) * psin - psinm1 ) 
     &           /( (   cior * cbiga(n) + ( fn*xinv ) ) * zetn - zetnm1 )
            qsca = qsca + twonp1 * ( sq( an ) + sq( bn ) )
!
         end if
!                       ** save mie coefficients for *pmom* calculation
         lita( n ) = an
         litb( n ) = bn
!                            ** increment mie sums for non-angle-
!                            ** dependent quantities
!
         sforw      = sforw      + twonp1 * ( an + bn )
         tforw( 1 ) = tforw( 1 ) + tcoef  * ( an - bn )
         sback      = sback      + ( mm * twonp1 ) * ( an - bn )
         tback( 1 ) = tback( 1 ) + ( mm * tcoef )  * ( an + bn )
         gqsc = gqsc + ( fn - rn ) * dble( anm1 * dconjg( an )   
     &        + bnm1 * dconjg( bn ) )   
     &        + coeff * dble( an * dconjg( bn ) )
!
         if ( yesang )  then
!                                      ** put mie coefficients in form
!                                      ** needed for computing s+, s-
!                                      ** ( ref. 2, p. 1507 )
            anp = coeff * ( an + bn )
            bnp = coeff * ( an - bn )
!                                      ** increment mie sums for s+, s-
!                                      ** while upward recursing
!                                      ** angular functions little pi
!                                      ** and little tau
            if ( anyang )  then
!                                         ** arbitrary angles
!
!                                              ** vectorizable loop
               do  80  j = 1, numang
                  rtmp = ( xmu( j ) * pin( j ) ) - pinm1( j )
                  taun =  fn * rtmp - pinm1( j )
                  sp( j )  = sp( j ) + anp * ( pin( j ) + taun )
                  sm( j )  = sm( j ) + bnp * ( pin( j ) - taun )
                  pinm1( j ) = pin( j )
                  pin( j ) = ( xmu( j ) * pin( j ) ) + np1dn * rtmp
   80          continue
!
            else
!                                  ** angles symmetric about 90 degrees
               anpm = mm * anp
               bnpm = mm * bnp
!                                          ** vectorizable loop
               do  90  j = 1, nangd2
                  rtmp = ( xmu( j ) * pin( j ) ) - pinm1( j )
                  taun =  fn * rtmp - pinm1( j )
                  sp ( j ) = sp ( j ) +  anp * ( pin( j ) + taun )
                  sms( j ) = sms( j ) + bnpm * ( pin( j ) + taun )
                  sm ( j ) = sm ( j ) +  bnp * ( pin( j ) - taun )
                  sps( j ) = sps( j ) + anpm * ( pin( j ) - taun )
                  pinm1( j ) = pin( j )
                  pin( j ) = ( xmu( j ) * pin( j ) ) + np1dn * rtmp
   90          continue
!
            end if
         end if
!                          ** update relevant quantities for next
!                          ** pass through loop
         mm   =  - mm
         anm1 = an
         bnm1 = bn
!                           ** upward recurrence for ricatti-bessel
!                           ** functions ( ref. 1, eq. 17 )
!
         zet    = ( twonp1 * xinv ) * zetn - zetnm1
         zetnm1 = zetn
         zetn   = zet
         psinm1 = psin
         psin   = dble( zetn )
  100 continue
!
! ---------- end loop to sum mie series --------------------------------
!
!
      qext = 2. / xx**2 * dble( sforw )
      if ( perfct .or. noabs )  then
         qsca = qext
      else
         qsca = 2. / xx**2 * qsca
      end if
!
      gqsc = 4. / xx**2 * gqsc
      sforw = 0.5 * sforw
      sback = 0.5 * sback
      tforw( 2 ) = 0.5 * (   sforw + 0.25 * tforw( 1 ) )
      tforw( 1 ) = 0.5 * (   sforw - 0.25 * tforw( 1 ) )
      tback( 2 ) = 0.5 * (   sback + 0.25 * tback( 1 ) )
      tback( 1 ) = 0.5 * ( - sback + 0.25 * tback( 1 ) )
!
      if ( yesang )  then
!                                ** recover scattering amplitudes
!                                ** from s+, s- ( ref. 1, eq. 11 )
         if ( anyang )  then
!                                         ** vectorizable loop
            do  110  j = 1, numang
               s1( j ) = 0.5 * ( sp( j ) + sm( j ) )
               s2( j ) = 0.5 * ( sp( j ) - sm( j ) )
  110       continue
!
         else
!                                         ** vectorizable loop
            do  120  j = 1, nangd2
               s1( j ) = 0.5 * ( sp( j ) + sm( j ) )
               s2( j ) = 0.5 * ( sp( j ) - sm( j ) )
  120       continue
!                                         ** vectorizable loop
            do  130  j = 1, nangd2
               s1( numang+1 - j ) = 0.5 * ( sps( j ) + sms( j ) )
               s2( numang+1 - j ) = 0.5 * ( sps( j ) - sms( j ) )
  130       continue
         end if
!
      end if
!                                         ** calculate legendre moments
  200 if ( nmom.gt.0 )then
           call lpcoef ( ntrm, nmom, ipolzn, momdim, calcmo, npquan, 
     &                   lita, litb, pmom )
      endif
!
      if ( dimag(crefin) .gt. 0.0 )  then
!                                         ** take complex conjugates
!                                         ** of scattering amplitudes
         sforw = dconjg( sforw )
         sback = dconjg( sback )
         do  210  i = 1, 2
            tforw( i ) = dconjg( tforw(i) )
            tback( i ) = dconjg( tback(i) )
  210    continue
!
         do  220  j = 1, numang
            s1( j ) = dconjg( s1(j) )
            s2( j ) = dconjg( s2(j) )
  220    continue
!
      end if
!
      if ( pass1 )  then
!                             ** compare test case results with
!                             ** correct answers and abort if bad
!
         call  testmi ( qext, qsca, gqsc, sforw, sback, s1, s2, 
     &                   tforw, tback, pmom, momdim, ok )
         if ( .not. ok )  then
            prnt(1) = .false.
            prnt(2) = .false.
            call  miprnt( prnt, xx, perfct, crefin, numang, xmu, qext,   
     &                     qsca, gqsc, nmom, ipolzn, momdim, calcmo,  
     &                     pmom, sforw, sback, tforw, tback, s1, s2 )
            call errmsg( 'miev0 -- self-test failed', .true. )
         end if
!                                       ** restore user input values
         xx     = xxsav
         crefin = cresav
         mimcut = mimsav
         perfct = persav
         anyang = anysav
         nmom   = nmosav
         ipolzn = iposav
         numang = numsav
         xmu(1) = xmusav
         pass1 = .false.
         go to 10
!
      end if
!
      if ( prnt(1) .or. prnt(2) )then
         call  miprnt( prnt, xx, perfct, crefin, numang, xmu, qext, 
     &                 qsca, gqsc, nmom, ipolzn, momdim, calcmo,   
     &                 pmom, sforw, sback, tforw, tback, s1, s2 )
      endif
!
      return
!
      end subroutine  miev0 
!****************************************************************************                                           
      subroutine  ckinmi( numang, maxang, xx, perfct, crefin, momdim,
     &                    nmom, ipolzn, anyang, xmu, calcmo, npquan )
!
!        check for bad input to 'miev0' and calculate -calcmo,npquan-
!
      implicit none
      logical  perfct, anyang, calcmo(*)
      integer  numang, maxang, momdim, nmom, ipolzn, npquan
      real*8    xx, xmu(*)
      integer i,l,j,ip
      complex*16  crefin
!
      character*4  string
      logical  inperr
!
      inperr = .false.
!
      if ( numang.gt.maxang )  then
         call errmsg( 'miev0--parameter maxang too small', .true. )
         inperr = .true.
      end if
      if ( numang.lt.0 )  call  wrtbad( 'numang', inperr )
      if ( xx.lt.0. )     call  wrtbad( 'xx', inperr )
      if ( .not.perfct .and. dble(crefin).le.0. )call wrtbad( 'crefin', inperr )
      if ( momdim.lt.1 )  call wrtbad( 'momdim', inperr )
!
      if ( nmom.ne.0 )  then
         if ( nmom.lt.0 .or. nmom.gt.momdim ) call wrtbad('nmom',inperr)
         if ( iabs(ipolzn).gt.4444 )  call  wrtbad( 'ipolzn', inperr )
         npquan = 0
         do 5  l = 1, 4
            calcmo( l ) = .false.
    5    continue
         if ( ipolzn.ne.0 )  then
!                                 ** parse out -ipolzn- into its digits
!                                 ** to find which phase quantities are
!                                 ** to have their moments calculated
!
            write( string, '(i4)' )  iabs(ipolzn)
            do 10  j = 1, 4
               ip = ichar( string(j:j) ) - ichar( '0' )
               if ( ip.ge.1 .and. ip.le.4 )  calcmo( ip ) = .true.
               if ( ip.eq.0 .or. (ip.ge.5 .and. ip.le.9) )then
                    call  wrtbad( 'ipolzn', inperr )
               endif
               npquan = max0( npquan, ip )
   10       continue
         end if
      end if
!
      if ( anyang )  then
!                                ** allow for slight imperfections in
!                                ** computation of cosine
          do  20  i = 1, numang
             if ( xmu(i).lt.-1.00001 .or. xmu(i).gt.1.00001 )then
                  call wrtbad( 'xmu', inperr )
             endif
   20     continue
      else
          do  22  i = 1, ( numang + 1 ) / 2
             if ( xmu(i).lt.-0.00001 .or. xmu(i).gt.1.00001 )then
                  call wrtbad( 'xmu', inperr )
             endif
   22     continue
      end if
!
      if ( inperr )then
           call errmsg( 'miev0--input error(s).  aborting...', .true. )
      endif
!
      if ( xx.gt.20000.0 .or. dble(crefin).gt.10.0 .or. dabs( dimag(crefin) ).gt.10.0 )then
           call  errmsg('miev0--xx or crefin outside tested range', .false. )
      endif
!
      return
      end subroutine  ckinmi
!***********************************************************************                                                   
      subroutine  lpcoef ( ntrm, nmom, ipolzn, momdim, calcmo, npquan,
     &                     a, b, pmom )
!
!         calculate legendre polynomial expansion coefficients (also
!         called moments) for phase quantities ( ref. 5 formulation )
!
!     input:  ntrm                    number terms in mie series
!             nmom, ipolzn, momdim    'miev0' arguments
!             calcmo                  flags calculated from -ipolzn-
!             npquan                  defined in 'miev0'
!             a, b                    mie series coefficients
!
!     output: pmom                   legendre moments ('miev0' argument)
!
!     *** notes ***
!
!         (1)  eqs. 2-5 are in error in dave, appl. opt. 9,
!         1888 (1970).  eq. 2 refers to m1, not m2;  eq. 3 refers to
!         m2, not m1.  in eqs. 4 and 5, the subscripts on the second
!         term in square brackets should be interchanged.
!
!         (2)  the general-case logic in this subroutine works correctly
!         in the two-term mie series case, but subroutine  'lpco2t'
!         is called instead, for speed.
!
!         (3)  subroutine  'lpco1t', to do the one-term case, is never
!         called within the context of 'miev0', but is included for
!         complete generality.
!
!         (4)  some improvement in speed is obtainable by combining the
!         310- and 410-loops, if moments for both the third and fourth
!         phase quantities are desired, because the third phase quantity
!         is the real part of a complex series, while the fourth phase
!         quantity is the imaginary part of that very same series.  but
!         most users are not interested in the fourth phase quantity,
!         which is related to circular polarization, so the present
!         scheme is usually more efficient.
!
      implicit none
      logical  calcmo(*)
      integer  ipolzn, momdim, nmom, ntrm, npquan
      real*8    pmom( 0:momdim, * )
      complex*16  a(*), b(*)
!
!           ** specification of local variables
!
!      am(m)       numerical coefficients  a-sub-m-super-l
!                     in dave, eqs. 1-15, as simplified in ref. 5.
!
!      bi(i)       numerical coefficients  b-sub-i-super-l
!                     in dave, eqs. 1-15, as simplified in ref. 5.
!
!      bidel(i)    1/2 bi(i) times factor capital-del in dave
!
!      cm,dm()     arrays c and d in dave, eqs. 16-17 (mueller form),
!                     calculated using recurrence derived in ref. 5
!
!      cs,ds()     arrays c and d in ref. 4, eqs. a5-a6 (sekera form),
!                     calculated using recurrence derived in ref. 5
!
!      c,d()       either -cm,dm- or -cs,ds-, depending on -ipolzn-
!
!      evenl       true for even-numbered moments;  false otherwise
!
!      idel        1 + little-del  in dave
!
!      maxtrm      max. no. of terms in mie series
!
!      maxmom      max. no. of non-zero moments
!
!      nummom      number of non-zero moments
!
!      recip(k)    1 / k
!
      integer maxtrm,maxmom,mxmom2,maxrcp
!     parameter  ( maxtrm = 1102, maxmom = 2*maxtrm, mxmom2 = maxmom/2,   
!    &             maxrcp = 4*maxtrm + 2 )
      parameter  ( maxtrm = 10102, maxmom = 2*maxtrm, mxmom2 = maxmom/2,   
     &             maxrcp = 4*maxtrm + 2 )
      real*8      am( 0:maxtrm ), bi( 0:mxmom2 ), bidel( 0:mxmom2 ),   
     &            recip( maxrcp )
      complex*16 cm( maxtrm ), dm( maxtrm ), cs( maxtrm ), ds( maxtrm ),  
     &           c( maxtrm ), d( maxtrm )
      integer k,j,l,nummom,ld2,idel,m,i,mmax,imax
      real*8 thesum
      equivalence  ( c, cm ),  ( d, dm )
      logical    pass1, evenl
      save  pass1, recip
      data  pass1 / .true. /
!
!
      if ( pass1 )  then
!
         do  1  k = 1, maxrcp
            recip( k ) = 1.0 / k
    1    continue
         pass1 = .false.
!
      end if
!
      do  5  j = 1, max0( 1, npquan )
         do  5  l = 0, nmom
            pmom( l, j ) = 0.0
    5 continue
!
      if ( ntrm.eq.1 )  then
         call  lpco1t ( nmom, ipolzn, momdim, calcmo, a, b, pmom )
         return
      else if ( ntrm.eq.2 )  then
         call  lpco2t ( nmom, ipolzn, momdim, calcmo, a, b, pmom )
         return
      end if
!
      if ( ntrm+2 .gt. maxtrm )then
           call errmsg( 'lpcoef--parameter maxtrm too small', .true. )
      endif
!
!                                     ** calculate mueller c, d arrays
      cm( ntrm+2 ) = ( 0., 0. )
      dm( ntrm+2 ) = ( 0., 0. )
      cm( ntrm+1 ) = ( 1. - recip( ntrm+1 ) ) * b( ntrm )
      dm( ntrm+1 ) = ( 1. - recip( ntrm+1 ) ) * a( ntrm )
      cm( ntrm ) = ( recip(ntrm) + recip(ntrm+1) ) * a( ntrm )   
     &           + ( 1. - recip(ntrm) ) * b( ntrm-1 )
      dm( ntrm ) = ( recip(ntrm) + recip(ntrm+1) ) * b( ntrm ) 
     &           + ( 1. - recip(ntrm) ) * a( ntrm-1 )
!
      do  10  k = ntrm-1, 2, -1
         cm( k ) = cm( k+2 ) - ( 1. + recip(k+1) ) * b( k+1 ) 
     &                        + ( recip(k) + recip(k+1) ) * a( k )
     &                        + ( 1. - recip(k) ) * b( k-1 )
         dm( k ) = dm( k+2 ) - ( 1. + recip(k+1) ) * a( k+1 ) 
     &                        + ( recip(k) + recip(k+1) ) * b( k )
     &                        + ( 1. - recip(k) ) * a( k-1 )
   10 continue
      cm( 1 ) = cm( 3 ) + 1.5 * ( a( 1 ) - b( 2 ) )
      dm( 1 ) = dm( 3 ) + 1.5 * ( b( 1 ) - a( 2 ) )
!
      if ( ipolzn.ge.0 )  then
!
         do  20  k = 1, ntrm + 2
            c( k ) = ( 2*k - 1 ) * cm( k )
            d( k ) = ( 2*k - 1 ) * dm( k )
   20    continue
!
      else
!                                    ** compute sekera c and d arrays
         cs( ntrm+2 ) = ( 0., 0. )
         ds( ntrm+2 ) = ( 0., 0. )
         cs( ntrm+1 ) = ( 0., 0. )
         ds( ntrm+1 ) = ( 0., 0. )
!
         do  30  k = ntrm, 1, -1
            cs( k ) = cs( k+2 ) + ( 2*k + 1 ) * ( cm( k+1 ) - b( k ) )
            ds( k ) = ds( k+2 ) + ( 2*k + 1 ) * ( dm( k+1 ) - a( k ) )
   30    continue
!
         do  40  k = 1, ntrm + 2
            c( k ) = ( 2*k - 1 ) * cs( k )
            d( k ) = ( 2*k - 1 ) * ds( k )
   40    continue
!
      end if
!
!
      if( ipolzn.lt.0 )  nummom = min0( nmom, 2*ntrm - 2 )
      if( ipolzn.ge.0 )  nummom = min0( nmom, 2*ntrm )
      if ( nummom .gt. maxmom )then
           call errmsg( 'lpcoef--parameter maxtrm too small', .true. )
      endif
!
!                               ** loop over moments
      do  500  l = 0, nummom
         ld2 = l / 2
         evenl = mod( l,2 ) .eq. 0
!                                    ** calculate numerical coefficients
!                                    ** a-sub-m and b-sub-i in dave
!                                    ** double-sums for moments
         if( l.eq.0 )  then
!
            idel = 1
            do  60  m = 0, ntrm
               am( m ) = 2.0 * recip( 2*m + 1 )
   60       continue
            bi( 0 ) = 1.0
!
         else if( evenl )  then
!
            idel = 1
            do  70  m = ld2, ntrm
               am( m ) = ( 1. + recip( 2*m-l+1 ) ) * am( m )
   70       continue
            do  75  i = 0, ld2-1
               bi( i ) = ( 1. - recip( l-2*i ) ) * bi( i )
   75       continue
            bi( ld2 ) = ( 2. - recip( l ) ) * bi( ld2-1 )
!
         else
!
            idel = 2
            do  80  m = ld2, ntrm
               am( m ) = ( 1. - recip( 2*m+l+2 ) ) * am( m )
   80       continue
            do  85  i = 0, ld2
               bi( i ) = ( 1. - recip( l+2*i+1 ) ) * bi( i )
   85       continue
!
         end if
!                                     ** establish upper limits for sums
!                                     ** and incorporate factor capital-
!                                     ** del into b-sub-i
         mmax = ntrm - idel
         if( ipolzn.ge.0 )  mmax = mmax + 1
         imax = min0( ld2, mmax - ld2 )
         if( imax.lt.0 )  go to 600
         do  90  i = 0, imax
            bidel( i ) = bi( i )
   90    continue
         if( evenl )  bidel( 0 ) = 0.5 * bidel( 0 )
!
!                                    ** perform double sums just for
!                                    ** phase quantities desired by user
         if( ipolzn.eq.0 )  then
!
            do  110  i = 0, imax
!                                           ** vectorizable loop (cray)
               thesum = 0.0
               do  100  m = ld2, mmax - i
                  thesum = thesum + am( m ) * 
     &                       ( dble( c(m-i+1) * dconjg( c(m+i+idel) ) )  
     &                       + dble( d(m-i+1) * dconjg( d(m+i+idel) ) ) )
  100          continue
               pmom( l,1 ) = pmom( l,1 ) + bidel( i ) * thesum
  110       continue
            pmom( l,1 ) = 0.5 * pmom( l,1 )
            go to 500
!
         end if
!
         if ( calcmo(1) )  then
            do  160  i = 0, imax
!                                           ** vectorizable loop (cray)
               thesum = 0.0
               do  150  m = ld2, mmax - i
                  thesum = thesum + am( m ) *
     &                         dble( c(m-i+1) * dconjg( c(m+i+idel) ) )
  150          continue
               pmom( l,1 ) = pmom( l,1 ) + bidel( i ) * thesum
  160       continue
         end if
!
!
         if ( calcmo(2) )  then
            do  210  i = 0, imax
!                                           ** vectorizable loop (cray)
               thesum = 0.0
               do  200  m = ld2, mmax - i
                   thesum = thesum + am( m )
     &                    *  dble( d(m-i+1) * dconjg( d(m+i+idel) ) )
  200          continue
               pmom( l,2 ) = pmom( l,2 ) + bidel( i ) * thesum
  210       continue
         end if
!
!
         if ( calcmo(3) )  then
            do  310  i = 0, imax
!                                           ** vectorizable loop (cray)
               thesum = 0.0
               do  300  m = ld2, mmax - i
                  thesum = thesum + am( m )
     &                   * ( dble( c(m-i+1) * dconjg( d(m+i+idel) ) )  
     &                   + dble( c(m+i+idel) * dconjg( d(m-i+1) ) ) )
  300          continue
               pmom( l,3 ) = pmom( l,3 ) + bidel( i ) * thesum
  310       continue
            pmom( l,3 ) = 0.5 * pmom( l,3 )
         end if
!
!
         if ( calcmo(4) )  then
            do  410  i = 0, imax
!                                           ** vectorizable loop (cray)
               thesum = 0.0
               do  400  m = ld2, mmax - i
                  thesum = thesum + am( m ) * 
     &                       ( dimag( c(m-i+1) * dconjg( d(m+i+idel) ) )  
     &                       + dimag( c(m+i+idel) * dconjg( d(m-i+1) ) ))
  400          continue
               pmom( l,4 ) = pmom( l,4 ) + bidel( i ) * thesum
  410       continue
            pmom( l,4 ) = - 0.5 * pmom( l,4 )
         end if
!
  500 continue
!
!
  600 return
      end subroutine  lpcoef
!*********************************************************************                                                    
      subroutine  lpco1t ( nmom, ipolzn, momdim, calcmo, a, b, pmom )
!
!         calculate legendre polynomial expansion coefficients (also
!         called moments) for phase quantities in special case where
!         no. terms in mie series = 1
!
!        input:  nmom, ipolzn, momdim     'miev0' arguments
!                calcmo                   flags calculated from -ipolzn-
!                a(1), b(1)               mie series coefficients
!
!        output: pmom                     legendre moments
!
      implicit none
      logical  calcmo(*)
      integer  ipolzn, momdim, nmom,nummom,l
      real*8    pmom( 0:momdim, * ),sq,a1sq,b1sq
      complex*16  a(*), b(*), ctmp, a1b1c
      sq( ctmp ) = dble( ctmp )**2 + dimag( ctmp )**2
!
!
      a1sq = sq( a(1) )
      b1sq = sq( b(1) )
      a1b1c = a(1) * dconjg( b(1) )
!
      if( ipolzn.lt.0 )  then
!
         if( calcmo(1) )  pmom( 0,1 ) = 2.25 * b1sq
         if( calcmo(2) )  pmom( 0,2 ) = 2.25 * a1sq
         if( calcmo(3) )  pmom( 0,3 ) = 2.25 * dble( a1b1c )
         if( calcmo(4) )  pmom( 0,4 ) = 2.25 *dimag( a1b1c )
!
      else
!
         nummom = min0( nmom, 2 )
!                                   ** loop over moments
         do  100  l = 0, nummom
!
            if( ipolzn.eq.0 )  then
               if( l.eq.0 )  pmom( l,1 ) = 1.5 * ( a1sq + b1sq )
               if( l.eq.1 )  pmom( l,1 ) = 1.5 * dble( a1b1c )
               if( l.eq.2 )  pmom( l,1 ) = 0.15 * ( a1sq + b1sq )
               go to 100
            end if
!
            if( calcmo(1) )  then
               if( l.eq.0 )  pmom( l,1 ) = 2.25 * ( a1sq + b1sq / 3. )
               if( l.eq.1 )  pmom( l,1 ) = 1.5 * dble( a1b1c )
               if( l.eq.2 )  pmom( l,1 ) = 0.3 * b1sq
            end if
!
            if( calcmo(2) )  then
               if( l.eq.0 )  pmom( l,2 ) = 2.25 * ( b1sq + a1sq / 3. )
               if( l.eq.1 )  pmom( l,2 ) = 1.5 * dble( a1b1c )
               if( l.eq.2 )  pmom( l,2 ) = 0.3 * a1sq
            end if
!
            if( calcmo(3) )  then
               if( l.eq.0 )  pmom( l,3 ) = 3.0 * dble( a1b1c )
               if( l.eq.1 )  pmom( l,3 ) = 0.75 * ( a1sq + b1sq )
               if( l.eq.2 )  pmom( l,3 ) = 0.3 * dble( a1b1c )
            end if
!
            if( calcmo(4) )  then
               if( l.eq.0 )  pmom( l,4 ) = - 1.5 * dimag( a1b1c )
               if( l.eq.1 )  pmom( l,4 ) = 0.0
               if( l.eq.2 )  pmom( l,4 ) = 0.3 * dimag( a1b1c )
            end if
!
  100    continue
!
      end if
!
      return
      end subroutine  lpco1t 
!********************************************************************                                                  
      subroutine  lpco2t ( nmom, ipolzn, momdim, calcmo, a, b, pmom )
!
!         calculate legendre polynomial expansion coefficients (also
!         called moments) for phase quantities in special case where
!         no. terms in mie series = 2
!
!        input:  nmom, ipolzn, momdim     'miev0' arguments
!                calcmo                   flags calculated from -ipolzn-
!                a(1-2), b(1-2)           mie series coefficients
!
!        output: pmom                     legendre moments
!
      implicit none
      logical  calcmo(*)
      integer  ipolzn, momdim, nmom,l,nummom
      real*8    pmom( 0:momdim, * ),sq,pm1,pm2,a2sq,b2sq
      complex*16  a(*), b(*)
      complex*16  a2c, b2c, ctmp, ca, cac, cat, cb, cbc, cbt, cg, ch
      sq( ctmp ) = dble( ctmp )**2 + dimag( ctmp )**2
!
!
      ca = 3. * a(1) - 5. * b(2)
      cat= 3. * b(1) - 5. * a(2)
      cac = dconjg( ca )
      a2sq = sq( a(2) )
      b2sq = sq( b(2) )
      a2c = dconjg( a(2) )
      b2c = dconjg( b(2) )
!
      if( ipolzn.lt.0 )  then
!                                   ** loop over sekera moments
         nummom = min0( nmom, 2 )
         do  50  l = 0, nummom
!
            if( calcmo(1) )  then
               if( l.eq.0 )then
                   pmom( l,1 ) = 0.25
     &                         * ( sq(cat) + (100./3.) * b2sq )
               endif
               if( l.eq.1 ) pmom( l,1 ) = (5./3.) * dble( cat * b2c )
               if( l.eq.2 ) pmom( l,1 ) = (10./3.) * b2sq
            end if
!
            if( calcmo(2) )  then
               if( l.eq.0 )then
                   pmom( l,2 ) = 0.25
     &                         * ( sq(ca) +  (100./3.) * a2sq )
               endif
               if( l.eq.1 ) pmom( l,2 ) = (5./3.) * dble( ca * a2c )
               if( l.eq.2 ) pmom( l,2 ) = (10./3.) * a2sq
            end if
!
            if( calcmo(3) )  then
               if( l.eq.0 )then
                   pmom( l,3 ) = 0.25
     &                         * dble( cat*cac + (100./3.)*b(2)*a2c )
               endif
               if( l.eq.1 ) pmom( l,3 ) = 5./6. * dble( b(2)*cac + cat*a2c )
               if( l.eq.2 ) pmom( l,3 ) = 10./3. * dble( b(2) * a2c )
            end if
!
            if( calcmo(4) )  then
               if( l.eq.0 ) pmom( l,4 ) = -0.25 * dimag( cat*cac + (100./3.)*b(2)*a2c )
               if( l.eq.1 ) pmom( l,4 ) = -5./6. * dimag( b(2)*cac + cat*a2c )
               if( l.eq.2 ) pmom( l,4 ) = -10./3. * dimag( b(2) * a2c )
            end if
!
   50    continue
!
      else
!
         cb = 3. * b(1) + 5. * a(2)
         cbt= 3. * a(1) + 5. * b(2)
         cbc = dconjg( cb )
         cg = ( cbc*cbt + 10.*( cac*a(2) + b2c*cat) ) / 3.
         ch = 2.*( cbc*a(2) + b2c*cbt )
!
!                                   ** loop over mueller moments
         nummom = min0( nmom, 4 )
         do  100  l = 0, nummom
!
            if( ipolzn.eq.0 .or. calcmo(1) )  then
               if( l.eq.0 ) pm1 = 0.25 * sq(ca) + sq(cb) / 12. + (5./3.) * dble(ca*b2c) + 5.*b2sq
               if( l.eq.1 ) pm1 = dble( cb * ( cac/6. + b2c ) )
               if( l.eq.2 ) pm1 = sq(cb)/30. + (20./7.) * b2sq + (2./3.) * dble( ca * b2c )
               if( l.eq.3 ) pm1 = (2./7.) * dble( cb * b2c )
               if( l.eq.4 ) pm1 = (40./63.) * b2sq
               if ( calcmo(1) )  pmom( l,1 ) = pm1
            end if
!
            if( ipolzn.eq.0 .or. calcmo(2) )  then
               if( l.eq.0 ) pm2 = 0.25*sq(cat) + sq(cbt) / 12. + (5./3.) * dble(cat*a2c) + 5.*a2sq
               if( l.eq.1 ) pm2 = dble( cbt * ( dconjg(cat)/6. + a2c) )
               if( l.eq.2 ) pm2 = sq(cbt)/30. + (20./7.) * a2sq + (2./3.) * dble( cat * a2c )
               if( l.eq.3 ) pm2 = (2./7.) * dble( cbt * a2c )
               if( l.eq.4 ) pm2 = (40./63.) * a2sq
               if ( calcmo(2) )  pmom( l,2 ) = pm2
            end if
!
            if( ipolzn.eq.0 )  then
               pmom( l,1 ) = 0.5 * ( pm1 + pm2 )
               go to 100
            end if
!
            if( calcmo(3) )  then
               if( l.eq.0 ) pmom( l,3 ) = 0.25 * dble( cac*cat + cg + 20.*b2c*a(2) )
               if( l.eq.1 ) pmom( l,3 ) = dble( cac*cbt + cbc*cat + 3.*ch ) / 12.
               if( l.eq.2 ) pmom( l,3 ) = 0.1 * dble( cg + (200./7.) * b2c * a(2) )
               if( l.eq.3 ) pmom( l,3 ) = dble( ch ) / 14.
               if( l.eq.4 ) pmom( l,3 ) = 40./63. * dble( b2c * a(2) )
            end if
!
            if( calcmo(4) )  then
               if( l.eq.0 ) pmom( l,4 ) = 0.25 * dimag( cac*cat + cg + 20.*b2c*a(2) )
               if( l.eq.1 ) pmom( l,4 ) = dimag( cac*cbt + cbc*cat + 3.*ch ) / 12.
               if( l.eq.2 ) pmom( l,4 ) = 0.1 * dimag( cg + (200./7.) * b2c * a(2) )
               if( l.eq.3 ) pmom( l,4 ) = dimag( ch ) / 14.
               if( l.eq.4 ) pmom( l,4 ) = 40./63. * dimag( b2c * a(2) )
            end if
!
  100    continue
!
      end if
!
      return
      end subroutine  lpco2t 
!*********************************************************************                                                  
      subroutine  biga( cior, xx, ntrm, noabs, yesang, rbiga, cbiga )
!
!        calculate logarithmic derivatives of j-bessel-function
!
!     input :  cior, xx, ntrm, noabs, yesang  (defined in 'miev0')
!
!    output :  rbiga or cbiga  (defined in 'miev0')
!
!    internal variables :
!
!       confra     value of lentz continued fraction for -cbiga(ntrm)-,
!                     used to initialize downward recurrence.
!       down       = true, use down-recurrence.  false, do not.
!       f1,f2,f3   arithmetic statement functions used in determining
!                     whether to use up-  or down-recurrence
!                     ( ref. 2, eqs. 6-8 )
!       mre        real refractive index
!       mim        imaginary refractive index
!       rezinv     1 / ( mre * xx ); temporary variable for recurrence
!       zinv       1 / ( cior * xx ); temporary variable for recurrence
!
      implicit none
      logical  down, noabs, yesang
      integer  ntrm,n
      real*8    mre, mim, rbiga(*), xx, rezinv, rtmp, f1,f2,f3
!      complex*16  cior, ctmp, confra, cbiga(*), zinv
      complex*16  cior, ctmp,  cbiga(*), zinv
      f1( mre ) =  - 8.0 + mre**2 * ( 26.22 + mre * ( - 0.4474 
     &              + mre**3 * ( 0.00204 - 0.000175 * mre ) ) )
      f2( mre ) = 3.9 + mre * ( - 10.8 + 13.78 * mre )
      f3( mre ) =  - 15.04 + mre * ( 8.42 + 16.35 * mre )
!
!                                  ** decide whether 'biga' can be
!                                  ** calculated by up-recurrence
      mre =  dble( cior )
      mim =  dabs( dimag( cior ) )
      if ( mre.lt.1.0 .or. mre.gt.10.0 .or. mim.gt.10.0 )  then
         down = .true.
      else if ( yesang )  then
         down = .true.
         if ( mim*xx .lt. f2( mre ) )  down = .false.
      else
         down = .true.
         if ( mim*xx .lt. f1( mre ) )  down = .false.
      end if
!
      zinv  = 1.0 / ( cior * xx )
      rezinv = 1.0 / ( mre * xx )
!
      if ( down )  then
!                          ** compute initial high-order 'biga' using
!                          ** lentz method ( ref. 1, pp. 17-20 )
!
         ctmp = confra( ntrm, zinv, xx )
!
!                                   *** downward recurrence for 'biga'
!                                   *** ( ref. 1, eq. 22 )
         if ( noabs )  then
!                                            ** no-absorption case
            rbiga( ntrm ) = dble( ctmp )
            do  25  n = ntrm, 2, - 1
                rbiga( n-1 ) = (n*rezinv) 
     &                       - 1.0 / ( (n*rezinv) + rbiga( n ) )
   25       continue
!
         else
!                                            ** absorptive case
            cbiga( ntrm ) = ctmp
            do  30  n = ntrm, 2, - 1
               cbiga( n-1 ) = (n*zinv) - 1.0 / ( (n*zinv) + cbiga( n ) )
   30       continue
!
         end if
!
      else
!                              *** upward recurrence for 'biga'
!                              *** ( ref. 1, eqs. 20-21 )
         if ( noabs )  then
!                                            ** no-absorption case
            rtmp = dsin( mre*xx )
            rbiga( 1 ) =  - rezinv 
     &                 + rtmp / ( rtmp*rezinv - dcos( mre*xx ) )
            do  40  n = 2, ntrm
               rbiga( n ) = - ( n*rezinv )  
     &                    + 1.0 / ( ( n*rezinv ) - rbiga( n-1 ) )
   40       continue
!
         else
!                                                ** absorptive case
            ctmp = cdexp( - dcmplx(0.d0,2.d0) * cior * xx )
            cbiga( 1 ) = - zinv + (1.-ctmp) / ( zinv * (1.-ctmp) -   
     &                      dcmplx(0.d0,1.d0)*(1.+ctmp) )
            do  50  n = 2, ntrm
               cbiga( n ) = - (n*zinv) + 1.0 / ((n*zinv) - cbiga( n-1 ))
   50       continue
         end if
!
      end if
!
      return
      end subroutine  biga  
!**********************************************************************                                                   
      complex*16 function  confra( n, zinv, xx )
!
!         compute bessel function ratio capital-a-sub-n from its
!         continued fraction using lentz method ( ref. 1, pp. 17-20 )
!
!         zinv = reciprocal of argument of capital-a
!
!    i n t e r n a l    v a r i a b l e s
!    ------------------------------------
!
!    cak      term in continued fraction expansion of capital-a
!                ( ref. 1, eq. 25 )
!    capt     factor used in lentz iteration for capital-a
!                ( ref. 1, eq. 27 )
!    cdenom   denominator in -capt-  ( ref. 1, eq. 28b )
!    cnumer   numerator   in -capt-  ( ref. 1, eq. 28a )
!    cdtd     product of two successive denominators of -capt-
!                factors  ( ref. 1, eq. 34c )
!    cntn     product of two successive numerators of -capt-
!                factors  ( ref. 1, eq. 34b )
!    eps1     ill-conditioning criterion
!    eps2     convergence criterion
!    kk       subscript k of -cak-  ( ref. 1, eq. 25b )
!    kount    iteration counter ( used only to prevent runaway )
!    maxit    max. allowed no. of iterations
!    mm       + 1  and - 1, alternately
!
      implicit none
      integer   n,maxit,mm,kk,kount
      real*8     xx,eps1,eps2
      complex*16   zinv
      complex*16   cak, capt, cdenom, cdtd, cnumer, cntn
!      data  eps1 / 1.e - 2 /, eps2 / 1.e - 8 /
      data  eps1 / 1.d-2 /, eps2 / 1.d-8 /
      data  maxit / 10000 /
!
!                                      *** ref. 1, eqs. 25a, 27
      confra = ( n + 1 ) * zinv
      mm     =  - 1
      kk     = 2 * n + 3
      cak    = ( mm * kk ) * zinv
      cdenom = cak
      cnumer = cdenom + 1.0 / confra
      kount  = 1
!
   20 kount = kount + 1
      if ( kount.gt.maxit )then
           call errmsg( 'confra--iteration failed to converge$', .true.)
      endif
!
!                                         *** ref. 2, eq. 25b
      mm  =  - mm
      kk  = kk + 2
      cak = ( mm * kk ) * zinv
!                                         *** ref. 2, eq. 32
      if (      cdabs( cnumer/cak ).le.eps1 .or. cdabs( cdenom/cak ).le.eps1 )  then
!
!                                  ** ill-conditioned case -- stride
!                                  ** two terms instead of one
!
!                                         *** ref. 2, eqs. 34
         cntn   = cak * cnumer + 1.0
         cdtd   = cak * cdenom + 1.0
         confra = ( cntn / cdtd ) * confra
!                                             *** ref. 2, eq. 25b
         mm  =  - mm
         kk  = kk + 2
         cak = ( mm * kk ) * zinv
!                                         *** ref. 2, eqs. 35
         cnumer = cak + cnumer / cntn
         cdenom = cak + cdenom / cdtd
         kount  = kount + 1
         go to 20
!
      else
!                                ** well-conditioned case
!
!                                        *** ref. 2, eqs. 26, 27
         capt   = cnumer / cdenom
         confra = capt * confra
!                                    ** check for convergence
!                                    ** ( ref. 2, eq. 31 )
!
         if ( dabs( dble(capt) - 1.0 ).ge.eps2 .or. dabs( dimag(capt) ) .ge.eps2 )  then
!
!                                        *** ref. 2, eqs. 30a-b
            cnumer = cak + 1.0 / cnumer
            cdenom = cak + 1.0 / cdenom
            go to 20
         end if
      end if
!
      return
!
      end function confra
!********************************************************************      
      subroutine  miprnt( prnt, xx, perfct, crefin, numang, xmu,  
     &                     qext, qsca, gqsc, nmom, ipolzn, momdim,  
     &                     calcmo, pmom, sforw, sback, tforw, tback, 
     &                     s1, s2 )
!
!         print scattering quantities of a single particle
!
      implicit none
      logical  perfct, prnt(*), calcmo(*)
      integer  ipolzn, momdim, nmom, numang,i,m,j
      real*8    gqsc, pmom( 0:momdim, * ), qext, qsca, xx, xmu(*)
      real*8 fi1,fi2,fnorm
      complex*16  crefin, sforw, sback, tforw(*), tback(*), s1(*), s2(*)
      character*22  fmt
!
!
      if ( perfct )then
          write ( *, '(''1'',10x,a,1p,e11.4)' )
     &                 'perfectly conducting case, size parameter =', xx
      endif
      if ( .not.perfct )then
          write ( *, '(''1'',10x,3(a,1p,e11.4))' ) 
     &                   'refractive index:  real ', dble(crefin), 
     &              '  imag ', dimag(crefin), ',   size parameter =', xx
      endif
!
      if ( prnt(1) .and. numang.gt.0 )  then
!
         write ( *, '(/,a)' ) 
     &     '    cos(angle)  ------- s1 ---------  ------- s2 ---------'// 
     &     '  --- s1*conjg(s2) ---   i1=s1**2   i2=s2**2  (i1+i2)/2'//   
     &     '  deg polzn'
         do  10  i = 1, numang
            fi1 = dble( s1(i) ) **2 + dimag( s1(i) )**2
            fi2 = dble( s2(i) ) **2 + dimag( s2(i) )**2
            write( *, '( i4, f10.6, 1p,10e11.3 )'   )   
     &               i, xmu(i), s1(i), s2(i), s1(i)*dconjg(s2(i)),   
     &               fi1, fi2, 0.5*(fi1+fi2), (fi2-fi1)/(fi2+fi1)
   10    continue
!
      end if
!
!
      if ( prnt(2) )  then
!
         write ( *, '(/,a,9x,a,17x,a,17x,a,/,(0p,f7.2, 1p,6e12.3) )' ) 
     &            '  angle', 's-sub-1', 't-sub-1', 't-sub-2',  
     &                0.0,     sforw,    tforw(1),  tforw(2),  
     &               180.,     sback,    tback(1),  tback(2)
         write ( *, '(/,4(a,1p,e11.4))' )
     &            ' efficiency factors,  extinction:', qext,  
     &                               '   scattering:', qsca, 
     &                               '   absorption:', qext-qsca,  
     &                           '   rad. pressure:', qext-gqsc
!
         if ( nmom.gt.0 )  then
!
            write( *, '(/,a)' )  ' normalized moments of :'
            if ( ipolzn.eq.0 ) write ( *, '(''+'',27x,a)' ) 'phase fcn'
            if ( ipolzn.gt.0 )  write ( *, '(''+'',33x,a)' )   
     &          'm1           m2          s21          d21'
            if ( ipolzn.lt.0 )  write ( *, '(''+'',33x,a)' )  
     &          'r1           r2           r3           r4'
!
            fnorm = 4. / ( xx**2 * qsca )
            do  20  m = 0, nmom
               write ( *, '(a,i4)' )  '      moment no.', m
               do 20  j = 1, 4
                  if( calcmo(j) )  then
                     write( fmt, 98 )  24 + (j-1)*13
                     write ( *,fmt )  fnorm * pmom(m,j)
                  end if
   20       continue
         end if
!
      end if
!
      return
!
   98 format( '( ''+'', t', i2, ', 1p,e13.4 )' )
      end subroutine  miprnt  
!**************************************************************************                                            
      subroutine  small1 ( xx, numang, xmu, qext, qsca, gqsc, sforw, 
     &                     sback, s1, s2, tforw, tback, a, b )
!
!       small-particle limit of mie quantities in totally reflecting
!       limit ( mie series truncated after 2 terms )
!
!        a,b       first two mie coefficients, with numerator and
!                  denominator expanded in powers of -xx- ( a factor
!                  of xx**3 is missing but is restored before return
!                  to calling program )  ( ref. 2, p. 1508 )
!
      implicit none
      integer  numang,j
      real*8    gqsc, qext, qsca, xx, xmu(*)
      real*8 twothr,fivthr,fivnin,sq,rtmp
      complex*16  a( 2 ), b( 2 ), sforw, sback, s1(*), s2(*), 
     &          tforw(*), tback(*)
!
      parameter  ( twothr = 2./3., fivthr = 5./3., fivnin = 5./9. )
      complex*16    ctmp
      sq( ctmp ) = dble( ctmp )**2 + dimag( ctmp )**2
!
!
      a( 1 ) = dcmplx ( 0.d0, twothr * ( 1. - 0.2 * xx**2 ) )  
     &        / dcmplx ( 1.d0 - 0.5 * xx**2, twothr * xx**3 )
!
      b( 1 ) = dcmplx ( 0.d0, - ( 1. - 0.1 * xx**2 ) / 3. )  
     &        / dcmplx ( 1.d0 + 0.5 * xx**2, - xx**3 / 3. )
!
      a( 2 ) = dcmplx ( 0.d0,   xx**2 / 30. )
      b( 2 ) = dcmplx ( 0.d0, - xx**2 / 45. )
!
      qsca = 6. * xx**4 * ( sq( a(1) ) + sq( b(1) )  
     &                       + fivthr * ( sq( a(2) ) + sq( b(2) ) ) )
      qext = qsca
      gqsc = 6. * xx**4 * dble( a(1) * dconjg( a(2) + b(1) )  
     &                     + ( b(1) + fivnin * a(2) ) * dconjg( b(2) ) )
!
      rtmp = 1.5 * xx**3
      sforw      = rtmp * ( a(1) + b(1) + fivthr * ( a(2) + b(2) ) )
      sback      = rtmp * ( a(1) - b(1) - fivthr * ( a(2) - b(2) ) )
      tforw( 1 ) = rtmp * ( b(1) + fivthr * ( 2.*b(2) - a(2) ) )
      tforw( 2 ) = rtmp * ( a(1) + fivthr * ( 2.*a(2) - b(2) ) )
      tback( 1 ) = rtmp * ( b(1) - fivthr * ( 2.*b(2) + a(2) ) )
      tback( 2 ) = rtmp * ( a(1) - fivthr * ( 2.*a(2) + b(2) ) )
!
      do  10  j = 1, numang
         s1( j ) = rtmp * ( a(1) + b(1) * xmu(j) + fivthr * 
     &              ( a(2) * xmu(j) + b(2) * ( 2.*xmu(j)**2 - 1. )) )
         s2( j ) = rtmp * ( b(1) + a(1) * xmu(j) + fivthr *  
     &               ( b(2) * xmu(j) + a(2) * ( 2.*xmu(j)**2 - 1. )) )
   10 continue
!                                     ** recover actual mie coefficients
      a( 1 ) = xx**3 * a( 1 )
      a( 2 ) = xx**3 * a( 2 )
      b( 1 ) = xx**3 * b( 1 )
      b( 2 ) = xx**3 * b( 2 )
!
      return
      end subroutine  small1 
!*************************************************************************                                                 
      subroutine  small2 ( xx, cior, calcqe, numang, xmu, qext, qsca, 
     &                      gqsc, sforw, sback, s1, s2, tforw, tback,  
     &                      a, b )
!
!       small-particle limit of mie quantities for general refractive
!       index ( mie series truncated after 2 terms )
!
!        a,b       first two mie coefficients, with numerator and
!                  denominator expanded in powers of -xx- ( a factor
!                  of xx**3 is missing but is restored before return
!                  to calling program )  ( ref. 2, p. 1508 )
!
!        ciorsq    square of refractive index
!
      implicit none
      logical  calcqe
      integer  numang,j
      real*8    gqsc, qext, qsca, xx, xmu(*)
      real*8 twothr,fivthr,sq,rtmp
      complex*16  a( 2 ), b( 2 ), cior, sforw, sback, s1(*), s2(*),   
     &          tforw(*), tback(*)
!
      parameter  ( twothr = 2./3., fivthr = 5./3. )
      complex*16  ctmp, ciorsq
      sq( ctmp ) = dble( ctmp )**2 + dimag( ctmp )**2
!
!
      ciorsq = cior**2
      ctmp = dcmplx( 0.d0, twothr ) * ( ciorsq - 1.0 )
      a(1) = ctmp * ( 1.0 - 0.1 * xx**2 + (ciorsq/350. + 1./280.)*xx**4)  
     &        / ( ciorsq + 2.0 + ( 1.0 - 0.7 * ciorsq ) * xx**2  
     &            - ( ciorsq**2/175. - 0.275 * ciorsq + 0.25 ) * xx**4  
     &            + xx**3 * ctmp * ( 1.0 - 0.1 * xx**2 ) )
!
      b(1) = (xx**2/30.) * ctmp * ( 1.0 + (ciorsq/35. - 1./14.) *xx**2 )   
     &        / ( 1.0 - ( ciorsq/15. - 1./6. ) * xx**2 )
!
      a(2) = ( 0.1 * xx**2 ) * ctmp * ( 1.0 - xx**2 / 14. )   
     &        / ( 2. * ciorsq + 3. - ( ciorsq/7. - 0.5 ) * xx**2 )
!
      qsca = 6. * xx**4 * ( sq(a(1)) + sq(b(1)) + fivthr * sq(a(2)) )
      gqsc = 6. * xx**4 * dble( a(1) * dconjg( a(2) + b(1) ) )
      qext = qsca
      if ( calcqe ) qext = 6. * xx * dble( a(1) + b(1) + fivthr * a(2) )
!
      rtmp = 1.5 * xx**3
      sforw      = rtmp * ( a(1) + b(1) + fivthr * a(2) )
      sback      = rtmp * ( a(1) - b(1) - fivthr * a(2) )
      tforw( 1 ) = rtmp * ( b(1) - fivthr * a(2) )
      tforw( 2 ) = rtmp * ( a(1) + 2. * fivthr * a(2) )
      tback( 1 ) = tforw( 1 )
      tback( 2 ) = rtmp * ( a(1) - 2. * fivthr * a(2) )
!
      do  10  j = 1, numang
         s1( j ) = rtmp * ( a(1) + ( b(1) + fivthr * a(2) ) * xmu(j) )
         s2( j ) = rtmp * ( b(1) + a(1) * xmu(j) + fivthr * a(2)  
     &                       * ( 2. * xmu(j)**2 - 1. ) )
   10 continue
!                                     ** recover actual mie coefficients
      a( 1 ) = xx**3 * a( 1 )
      a( 2 ) = xx**3 * a( 2 )
      b( 1 ) = xx**3 * b( 1 )
      b( 2 ) = ( 0., 0. )
!
      return
      end subroutine  small2  
!***********************************************************************                                                 
      subroutine  testmi ( qext, qsca, gqsc, sforw, sback, s1, s2,
     &                     tforw, tback, pmom, momdim, ok )
!
!         compare mie code test case results with correct answers
!         and return  ok=false  if even one result is inaccurate.
!
!         the test case is :  mie size parameter = 10
!                             refractive index   = 1.5 - 0.1 i
!                             scattering angle = 140 degrees
!                             1 sekera moment
!
!         results for this case may be found among the test cases
!         at the end of reference (1).
!
!         *** note *** when running on some computers, esp. in single
!         precision, the 'accur' criterion below may have to be relaxed.
!         however, if 'accur' must be set larger than 10**-3 for some
!         size parameters, your computer is probably not accurate
!         enough to do mie computations for those size parameters.
!
      implicit none
      integer momdim,m,n
      real*8    qext, qsca, gqsc, pmom( 0:momdim, * )
      complex*16  sforw, sback, s1(*), s2(*), tforw(*), tback(*)
      logical  ok, wrong
!
      real*8    accur, testqe, testqs, testgq, testpm( 0:1 )
      complex*16 testsf, testsb,tests1,tests2,testtf(2), testtb(2)
      data   testqe / 2.459791 /,  testqs / 1.235144 /,  
     &       testgq / 1.139235 /,  testsf / ( 61.49476, -3.177994 ) /, 
     &       testsb / ( 1.493434, 0.2963657 ) /,   
     &       tests1 / ( -0.1548380, -1.128972) /,  
     &       tests2 / ( 0.05669755, 0.5425681) /,  
     &       testtf / ( 12.95238, -136.6436 ), ( 48.54238, 133.4656 ) /,  
     &       testtb / ( 41.88414, -15.57833 ), ( 43.37758, -15.28196 )/,  
     &       testpm / 227.1975, 183.6898 /
      real*8 calc,exact
!      data   accur / 1.e-5 /
      data   accur / 1.e-4 /
      wrong( calc, exact ) = dabs( (calc - exact) / exact ) .gt. accur
!
!
      ok = .true.
      if ( wrong( qext,testqe ) )call  tstbad( 'qext', abs((qext - testqe) / testqe), ok )
      if ( wrong( qsca,testqs ) )call  tstbad( 'qsca', abs((qsca - testqs) / testqs), ok )
      if ( wrong( gqsc,testgq ) )call  tstbad( 'gqsc', abs((gqsc - testgq) / testgq), ok )
!
      if ( wrong(  dble(sforw),  dble(testsf) ) .or. wrong( dimag(sforw), dimag(testsf) ) )then
           call  tstbad( 'sforw', cdabs((sforw - testsf) / testsf), ok )
      endif
!
      if ( wrong(  dble(sback),  dble(testsb) ) .or. wrong( dimag(sback), dimag(testsb) ) )then
           call  tstbad( 'sback', cdabs((sback - testsb) / testsb), ok )
      endif
!
      if ( wrong(  dble(s1(1)),  dble(tests1) ) .or. wrong( dimag(s1(1)), dimag(tests1) ) )then
           call  tstbad( 's1', cdabs((s1(1) - tests1) / tests1), ok )
      endif
!
      if ( wrong(  dble(s2(1)),  dble(tests2) ) .or. wrong( dimag(s2(1)), dimag(tests2) ) )then
           call  tstbad( 's2', cdabs((s2(1) - tests2) / tests2), ok )
      endif
!
      do  20  n = 1, 2
         if ( wrong(  dble(tforw(n)),  dble(testtf(n)) ) .or. wrong( dimag(tforw(n)), dimag(testtf(n)) ) )then
              call  tstbad( 'tforw', cdabs( (tforw(n) - testtf(n))/testtf(n) ), ok )
         endif
         if ( wrong(  dble(tback(n)),  dble(testtb(n)) ) .or. wrong( dimag(tback(n)), dimag(testtb(n)) ) )then
              call  tstbad( 'tback', cdabs( (tback(n) - testtb(n))/testtb(n) ), ok )
         endif
   20 continue
!
      do  30  m = 0, 1
         if ( wrong( pmom(m,1), testpm(m) ) )then
              call  tstbad( 'pmom', dabs( (pmom(m,1)-testpm(m))/testpm(m) ), ok )
         endif
   30 continue
!
      return
!
      end subroutine  testmi  
!**************************************************************************                                              
      subroutine  errmsg( messag, fatal )
!
!        print out a warning or error message;  abort if error
!
!      USE module_peg_util, only:  peg_message, peg_error_fatal

      implicit none
      logical       fatal, once
      character*80 msg
      character*(*) messag
      integer       maxmsg, nummsg
      save          maxmsg, nummsg, once
      data nummsg / 0 /,  maxmsg / 100 /,  once / .false. /
!
!
      if ( fatal )  then
!         write ( *, '(2a)' )  ' ******* error >>>>>>  ', messag
!         stop
            write( msg, '(a)' )'FASTJ mie fatal error ' // messag                  
            write(logdev,'(a)')Trim( msg )
      end if
!
      nummsg = nummsg + 1
      if ( nummsg.gt.maxmsg )  then
!         if ( .not.once )  write ( *,99 )
        if ( .not.once )then
           write( msg, '(a)' ) 
     &      'FASTJ mie: too many warning messages -- no longer printing '
           write(logdev,'(a)')Trim( msg )
         end if    
         once = .true.
      else
         msg =   'FASTJ mie warning '  // messag
          write(logdev,'(a)')Trim( msg )
!         write ( *, '(2a)' )  ' ******* warning >>>>>>  ', messag
      endif
!
      return
!
!   99 format( ///,' >>>>>>  too many warning messages --  ',   &
!         'they will no longer be printed  <<<<<<<', /// )
      end subroutine  errmsg  
!********************************************************************                     
      subroutine  wrtbad ( varnam, erflag )
!
!          write names of erroneous variables
!
!      input :   varnam = name of erroneous variable to be written
!                         ( character, any length )
!
!      output :  erflag = logical flag, set true by this routine
! ----------------------------------------------------------------------
!      USE module_peg_util, only:  peg_message
      
      implicit none
      character*(*)  varnam
      logical        erflag
      integer        maxmsg, nummsg
      character*80 msg
      save  nummsg, maxmsg
      data  nummsg / 0 /,  maxmsg / 50 /     
!
!
      nummsg = nummsg + 1
      msg = 'FASTJ mie input variable in error ' // varnam                   
      write(6,'(a)')Trim( msg )
      erflag = .true.
      if ( nummsg.eq.maxmsg )then
         call  errmsg ( 'too many input variable errors.  aborting...$', .true. )
      endif
      return
!
      end subroutine  wrtbad 
!******************************************************************                        
      subroutine  tstbad( varnam, relerr, ok )
!
!       write name (-varnam-) of variable failing self-test and its
!       percent error from the correct value.  return  ok = false.
!
      implicit none
      character*(*)  varnam
      logical        ok
      real*8          relerr
!
!
      ok = .false.
      write( 6, '(/,3a,1p,e11.2,a)' )  
     &        ' output variable  ', varnam,'  differed by', 100.*relerr,   
     &        '  per cent from correct value.  self-test failed.'
      return
!
      end subroutine  tstbad                      
!-----------------------------------------------------------------------

        subroutine fast_mieaer_modal( mie_parameter,refindx,extaer,scataer,gaer,bscoef )

         USE UTILIO_DEFN
         USE AERO_DATA           ! aero variable data
         USE CSQY_DATA,  ONLY : NWL => NWL_REF, NUM_REFRACTIVE, REFRACTIVE_INDEX,
     &                          WAVELENGTH => EFFWL_REF

         USE bhmie_routine
        
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
         real, allocatable ,save :: wavmid( : ) ! cm
         real weighte, weights
! various bookeeping variables
         integer, save :: nbin_a_maxd ! Max # of aerosol bins or modes
         integer, save :: nspint      ! Num of spectral intervals across
         
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

 
         integer, save      :: numang=0,nmom=7,ipolzn=0,momdim=7
         integer, save      :: nrefr,nrefi
         integer, parameter :: prefr=22,prefi=33
         integer, parameter :: nsizes(n_mode) = (/ 200,200,200 /)
         integer, parameter :: nsiz=200,nlog=30,ncoef=40
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
!        
         real, allocatable :: p2( : ),p3( : )        
         real, allocatable :: p4( : ),p5( : )
         real, allocatable :: p6( : ),p7( : )
         
         
!         real, allocatable, save ::  extp( :,:,: )       ! specific extinction
!         real, allocatable, save ::  albp( :,:,: )       ! single scat alb
!         real, allocatable, save ::  asmp( :,:,: )       ! asymmetry factor
!         real, allocatable, save ::  ascat( :,:,: )       ! scattering efficiency, JCB 2004/02/09
!         real, allocatable, save ::  pmom2( :,:,: )       ! phase function expansion, #2
!         real, allocatable, save ::  pmom3( :,:,: )       ! phase function expansion, #3
!         real, allocatable, save ::  pmom4( :,:,: )       ! phase function expansion, #4
!         real, allocatable, save ::  pmom5( :,:,: )       ! phase function expansion, #5
!         real, allocatable, save ::  pmom6( :,:,: )       ! phase function expansion, #6
!         real, allocatable, save ::  pmom7( :,:,: )         ! phase function expansion, #7
!         real, allocatable, save ::  sback2p( :,:,: )    ! JCB 2007/02/01 - backscatter
         real ::  aero_radius

!--------------
! Coefficients of Chebychev Polynomials
         real cext(ncoef)
         real casm(ncoef)
         real cpmom2(ncoef)
         real cscat(ncoef)                          ! JCB 2004/02/09
         real cpmom3(ncoef)
         real cpmom4(ncoef)
         real cpmom5(ncoef)
         real cpmom6(ncoef)
         real cpmom7(ncoef)
         real cpsback2p(ncoef)                      ! JCB 2007/02/09  - backscatter
         
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

         real(8) :: thesize        ! 2 pi radpart / waveleng = size parameter

!           specabs = absorption coeff / unit dry mass
!           specscat = scattering coeff / unit dry mass

         complex(8)       :: crefin,crefd
         complex(8), save :: crefw
         real, save :: rmin=0.005e-4,rmax=50.e-4   ! min, max aerosol size bin

         
         real bma,bpa
         
         real, save :: xrmin,xrmax,xr
         real xrad      ! normalized aerosol radius
         real ch(ncoef) ! chebychev polynomial
         
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

         real, save :: refrmin ! minimum of real part of refractive index
         real, save :: refrmax ! maximum of real part of refractive index
         real, save :: refimin ! minimum of imag part of refractive index
         real, save :: refimax ! maximum of imag part of refractive index
         real, save :: drefr   ! increment in real part of refractive index
         real, save :: drefi   ! increment in imag part of refractive index
         real, save :: refrtab(prefr) ! table of real refractive indices for aerosols
         real, save :: refitab(prefi) ! table of imag refractive indices for aerosols
!         complex specrefndx(ltype)    ! refractivr indices

         real, save :: reciprocal_drefr  ! reciprocal of increment in real part of refractive index
         real, save :: reciprocal_drefi  ! reciprocal of increment in  imag part of refractive index
         real, save :: reciprocal_drefri ! reciprocal of real times imag increments of refractive index


         real, parameter :: pie   = pi
         real, parameter :: third = 1.0/3.0
         
         integer irams, jrams
! diagnostic declarations
         integer kcallmieaer2
         integer ibin, imode
         integer mie_unit
         character(150) :: msg

!      if(first)then
       if( Create_Table )then
           Create_Table = .False.
           
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

         nbin_a_maxd = n_mode
!
         
         bma=0.5*alog(rmax/rmin) ! JCB
         bpa=0.5*alog(rmax*rmin) ! JCB
         xrmin=alog(rmin)
         xrmax=alog(rmax)

         nspint = nwl
         allocate( wavmid( nwl ) )
         wavmid(1:nwl) =  1.0e-7*wavelength(1:nwl)                         

!         allocate( extp(ncoef,prefr,prefi),      
!     &             albp(ncoef,prefr,prefi),   
!     &             asmp(ncoef,prefr,prefi),   
!     &             ascat(ncoef,prefr,prefi),  
!     &             pmom2(ncoef,prefr,prefi),  
!     &             pmom3(ncoef,prefr,prefi),  
!     &             pmom4(ncoef,prefr,prefi),  
!     &             pmom5(ncoef,prefr,prefi),  
!     &             pmom6(ncoef,prefr,prefi),  
!     &             pmom7(ncoef,prefr,prefi),  
!     &             sback2p(ncoef,prefr,prefi) )
!     
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

          quadrature_factor = exp( sqrt2 * log( max_sigma_g ) * max_quad_point )
         
         do ns = 1,n_mode
            Modal_Tables(ns)%nradii = nsizes(ns)
            Modal_Tables(ns)%rmax = 60.0*max_dg_wet(ns)*quadrature_factor
            Modal_Tables(ns)%rmax = 2.0*pi*Modal_Tables(ns)%rmax/minval( wavmid )
            Modal_Tables(ns)%rmin = 40.0*min_dg_dry(ns)/quadrature_factor
            Modal_Tables(ns)%rmin = 2.0*pi*Modal_Tables(ns)%rmin/maxval( wavmid )
            Modal_Tables(ns)%bma=0.5*log(Modal_Tables(ns)%rmax/Modal_Tables(ns)%rmin)
            Modal_Tables(ns)%bpa=0.5*log(Modal_Tables(ns)%rmax*Modal_Tables(ns)%rmin)
            Modal_Tables(ns)%xrmin=log(Modal_Tables(ns)%rmin)
            Modal_Tables(ns)%xrmax=log(Modal_Tables(ns)%rmax)
            allocate( Modal_Tables(ns)%extp(ncoef,prefr,prefi),      
     &                Modal_Tables(ns)%albp(ncoef,prefr,prefi),   
     &                Modal_Tables(ns)%asmp(ncoef,prefr,prefi),   
     &                Modal_Tables(ns)%ascat(ncoef,prefr,prefi),  
     &                Modal_Tables(ns)%pmom2(ncoef,prefr,prefi),  
     &                Modal_Tables(ns)%pmom3(ncoef,prefr,prefi),  
     &                Modal_Tables(ns)%pmom4(ncoef,prefr,prefi),  
     &                Modal_Tables(ns)%pmom5(ncoef,prefr,prefi),  
     &                Modal_Tables(ns)%pmom6(ncoef,prefr,prefi),  
     &                Modal_Tables(ns)%pmom7(ncoef,prefr,prefi),  
     &                Modal_Tables(ns)%sback2p(ncoef,prefr,prefi) )
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
     &             qsca( nsize ),
     &             gqsc( nsize ) )
      
         allocate( asymm( nsize ),
     &              scat( nsize ),
     &               sb2( nsize ),
     &            qextr4( nsize ),
     &                rs( nsize ) )          

          allocate( p2(nsize),p3(nsize),p4(nsize),p5(nsize) )
          allocate( p6(nsize),p7(nsize) )
          
!         calibrate parameterization with range of refractive indices 

            do 120 ni=1,nrefi
              do 120 nr=1,nrefr                                        

               refrtab(nr)=refrmin+(nr-1)*drefr
               refitab(ni)=refimin+(ni-1)*drefi
               crefd=dcmplx(real(refrtab(nr),8),real(refitab(ni),8))
!              mie calculations of optical efficiencies
               loop_modes: do imode = 1,n_mode
                  nsize = nsizes( imode )
                  do n=1,nsize
                  
                     xr=cos(pie*(real(n)-0.5)/real(nsize))
                     rs(n)=real(exp(xr*Modal_Tables(imode)%bma+Modal_Tables(imode)%bpa),8)
!                    size parameter and weighted refractive index
                     thesize=real(rs(n),8)
                  
                     if( .Not. TWO_STREAM )then
                         call miev0(thesize,crefd,perfct,mimcut,anyang, 
     &                       numang,xmu,nmom,ipolzn,momdim,prnt,
     &                       qext(n),qsca(n),gqsc(n),pmom,sforw,sback,s1,
     &                       s2,tforw,tback )
                         qextr4(n)=qext(n)
!                        qabs(n)=qext(n)-qsca(n) ! not necessary anymore JCB 2004/02/09
                         scat(n)=qsca(n) ! JCB 2004/02/09
!                         asymm(n)=gqsc(n)*scat(n) ! assume always greater than zero
                         asymm(n)=gqsc(n)/scat(n) ! assume always greater than zero
                       
! coefficients of phase function expansion; note modification by JCB of miev0 coefficients
                         p2(n)=pmom(2,1)/pmom(0,1)*5.0
                         p3(n)=pmom(3,1)/pmom(0,1)*7.0
                         p4(n)=pmom(4,1)/pmom(0,1)*9.0
                         p5(n)=pmom(5,1)/pmom(0,1)*11.0
                         p6(n)=pmom(6,1)/pmom(0,1)*13.0
                         p7(n)=pmom(7,1)/pmom(0,1)*15.0
! backscattering efficiency, Bohren and Huffman, page 122
! as stated by Bohren and Huffman, this is 4*pie times what is should be
! may need to be smoothed - a very rough function - for the time being we won't apply smoothing
! and let the integration over the size distribution be the smoothing
                            
                     else
                     
                         thesize_bhmie = real( thesize )
                         refrel_bhmie  = cmplx(real(refrtab(nr)),-real(refitab(ni)))
                         call driver_bhmie_flexy(thesize_bhmie,refrel_bhmie,qext_bhmie,qsca_bhmie,gsca_bhmie) 
                         asymm(n) = gsca_bhmie/qsca_bhmie 
                         qextr4(n)= qext_bhmie*thesize_bhmie
                         scat(n)  = qsca_bhmie*thesize_bhmie 
                         sback    = sback_bhmie
                         sb2(n)   = 4.0*sback*dconjg(sback)
     &                            / (thesize_bhmie*thesize_bhmie) ! JCB 2007/02/01  
                         
                     endif
                  
                  enddo
  100             continue

                  call fitcurv(rs,qextr4,Modal_Tables(imode)%extp(:,nr,ni),ncoef,nsize)
                  call fitcurv(rs,scat,Modal_Tables(imode)%ascat(:,nr,ni),ncoef,nsize) ! JCB 2004/02/07 - scattering efficiency
                  call fitcurv(rs,asymm,Modal_Tables(imode)%asmp(:,nr,ni),ncoef,nsize)
                  call fitcurv(rs,sb2,Modal_Tables(imode)%sback2p(:,nr,ni),ncoef,nsize) ! JCB 2007/02/01 - backscattering efficiency             
                                       
                  if( .Not. TWO_STREAM )then
                     call fitcurv_nolog(rs,p2,Modal_Tables(imode)%pmom2(:,nr,ni),ncoef,nsize)
                     call fitcurv_nolog(rs,p3,Modal_Tables(imode)%pmom3(:,nr,ni),ncoef,nsize)
                     call fitcurv_nolog(rs,p4,Modal_Tables(imode)%pmom4(:,nr,ni),ncoef,nsize)
                     call fitcurv_nolog(rs,p5,Modal_Tables(imode)%pmom5(:,nr,ni),ncoef,nsize)
                     call fitcurv_nolog(rs,p6,Modal_Tables(imode)%pmom6(:,nr,ni),ncoef,nsize)
                     call fitcurv_nolog(rs,p7,Modal_Tables(imode)%pmom7(:,nr,ni),ncoef,nsize)
                  
                  endif
               enddo loop_modes

  120       continue

!        If( MYPE .eq. 0 )Then
!
!       mie_unit = junit()
!       open(unit=mie_unit,file='mie_table_coeffs_single.txt',form='FORMATTED',status='REPLACE')
!       write(mie_unit,'(a,es12.4,a,es12.4,a)')'minimum refractive index = ( ',refrtab(1),', ',refitab(1),' ) '
!       write(mie_unit,'(a,es12.4,a,es12.4,a)')'maximum refractive index = ( ',refrtab(nrefr),', ',refitab(nrefi),' ) '
!       write(mie_unit,'(a,i4,a,i4,a)')'number real and imaginary parts = ( ',nrefr,', ',nrefi,' ) '
!       write(mie_unit,'(a,3(es16.7,1x))')'minimum mie parameters = ',Modal_Tables(1:n_mode)%rmin
!       write(mie_unit,'(a,3(es16.7,1x))')'maximum mie parameters = ',Modal_Tables(1:n_mode)%rmax
!       write(mie_unit,'(a,i4)')'number coefficients for chebyshev polynomials = ',ncoef
!       do ni=1,nrefi
!          do nr=1,nrefr
!            write(mie_unit,'(a,es12.4,a,es12.4,a)')'! refractive index = ( ',refrtab(nr),', ',refitab(ni),' ) '
!            write(mie_unit,'(a)')'! ext, scat, asym, and backscat coefficients for chebyshev polynomials'
!            do nc = 1,ncoef
!               write(mie_unit,'(21(es16.7,1x))')
!     &         (Modal_Tables(imode)%extp(nc,nr,ni),
!     &          Modal_Tables(imode)%ascat(nc,nr,ni),
!     &          Modal_Tables(imode)%asmp(nc,nr,ni),
!     &          Modal_Tables(imode)%sback2p(nc,nr,ni),imode=1,n_mode)
!            end do
!          end do
!        end do 
!       close(mie_unit)
!        End If


         deallocate( qext,
     &               qsca,
     &               gqsc  )
      
         deallocate( asymm, 
     &                scat, 
     &                 sb2, 
     &              qextr4, 
     &                  rs  ) 
     
         deallocate( p2,p3,p4,p5 )
         deallocate( p6,p7 )

        if( first )then
          
           first=.false.
           Return
           
        end if
         
      endif ! end first call block
      
!         thesum=0.0
!         thesum=thesum+1.0e-6
         aero_radius = mie_parameter 
         ns         = 1
         do m = 1,n_mode
            if( aero_radius .le. Modal_Tables(m)%rmax)exit
         end do
         m = min(m,n_mode)
         
!               extaer  = 1.0E-30 ! pext
!               scataer = 1.0E-30 ! pscat
!               gaer    = 0.0     ! pasm
!               bscoef  = 1.0E-30 !sback2/thesize
!               Return

               gaer=0.0
               sizeaer=0.0
               extaer=0.0
               l2=0.0
               l3=0.0
               l4=0.0
               l5=0.0
               l6=0.0
               l7=0.0
               bscoef=0.0        ! JCB 2007/02/01 - backscattering coefficient
! loop over the bins
                  sizem=aero_radius ! radius in cm
! check against limits of mie parameter
                  if(aero_radius.le.Modal_Tables(m)%rmin)then
                    write( msg, '(a, 1x, es11.4, a, 1x, es11.4 )' )       
     &              'AE Mode, WRF FAST mie: aerosol mie parameter set to ',
     &              0.02*rmin, ' from ', mie_parameter
                    aero_radius=rmin
                     write(logdev,'(a)')TRim( msg )
                  endif
!
                  if(aero_radius.gt.Modal_Tables(m)%rmax)then
                    write( msg, '(a, 1x, es11.4, a, 1x, es11.4 )' )       
     &              'AE Mode, WRF FAST mie: aerosol mie parameter set to ',
     &              0.02*rmax, ' from ', mie_parameter
                     write(logdev,'(a)')Trim( msg )
                     aero_radius=rmax
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
!                  print*,'ShortCut itab, jtab = ',itab,jtab
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
!                  print*,'Mode = ',m
!                  print*,'ShortCut itab, jtab,itabp, jtabp = ',itab,jtab,itabp, jtabp
!                  print*,'weight_real,weight_imag = ',weight_real,weight_imag

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

                  if( .Not. TWO_STREAM )then

                      do nc=1,ncoef
                         cpmom2(nc) = one_wrwi_wi*Modal_Tables(m)%pmom2(nc,itab,jtab)
     &                              +    wr_wrwi*Modal_Tables(m)%pmom2(nc,itabp,jtab)
     &                              +       wrwi*Modal_Tables(m)%pmom2(nc,itabp,jtabp)
     &                              +    wi_wrwi*Modal_Tables(m)%pmom2(nc,itab,jtabp)                     

                         cpmom3(nc) = one_wrwi_wi*Modal_Tables(m)%pmom3(nc,itab,jtab)
     &                              +    wr_wrwi*Modal_Tables(m)%pmom3(nc,itabp,jtab)
     &                              +       wrwi*Modal_Tables(m)%pmom3(nc,itabp,jtabp)
     &                              +    wi_wrwi*Modal_Tables(m)%pmom3(nc,itab,jtabp)                     

                         cpmom4(nc) = one_wrwi_wi*Modal_Tables(m)%pmom4(nc,itab,jtab)
     &                              +    wr_wrwi*Modal_Tables(m)%pmom4(nc,itabp,jtab)
     &                              +       wrwi*Modal_Tables(m)%pmom4(nc,itabp,jtabp)
     &                              +    wi_wrwi*Modal_Tables(m)%pmom4(nc,itab,jtabp)                     

                         cpmom5(nc) = one_wrwi_wi*Modal_Tables(m)%pmom5(nc,itab,jtab)
     &                              +    wr_wrwi*Modal_Tables(m)%pmom5(nc,itabp,jtab)
     &                              +       wrwi*Modal_Tables(m)%pmom5(nc,itabp,jtabp)
     &                              +    wi_wrwi*Modal_Tables(m)%pmom5(nc,itab,jtabp)                     

                         cpmom6(nc) = one_wrwi_wi*Modal_Tables(m)%pmom6(nc,itab,jtab)
     &                              +    wr_wrwi*Modal_Tables(m)%pmom6(nc,itabp,jtab)
     &                              +       wrwi*Modal_Tables(m)%pmom6(nc,itabp,jtabp)
     &                              +    wi_wrwi*Modal_Tables(m)%pmom6(nc,itab,jtabp)                     

                         cpmom7(nc) = one_wrwi_wi*Modal_Tables(m)%pmom7(nc,itab,jtab)
     &                              +    wr_wrwi*Modal_Tables(m)%pmom7(nc,itabp,jtab)
     &                              +       wrwi*Modal_Tables(m)%pmom7(nc,itabp,jtabp)
     &                              +    wi_wrwi*Modal_Tables(m)%pmom7(nc,itab,jtabp)                     


                      enddo
                      
                      ppmom2=0.5*cpmom2(1)
                      ppmom3=0.5*cpmom3(1)
                      ppmom4=0.5*cpmom4(1)
                      if( sizem .le. 0.03e-04 )then
                          ppmom5 = 0.0
                          ppmom6 = 0.0
                          ppmom7 = 0.0
                      else    
                          ppmom5=0.5*cpmom5(1)
                          ppmom6=0.5*cpmom6(1)
                          ppmom7=0.5*cpmom7(1)
                      endif
                      
                      do nc=2,ncoef
                          ppmom2=ppmom2+ch(nc)*cpmom2(nc)
                          ppmom3=ppmom3+ch(nc)*cpmom3(nc)
                          ppmom4=ppmom4+ch(nc)*cpmom4(nc)
                          if( sizem .gt. 0.03e-04 )then
                             ppmom5=ppmom5+ch(nc)*cpmom5(nc)
                             ppmom6=ppmom6+ch(nc)*cpmom6(nc)
                             ppmom7=ppmom7+ch(nc)*cpmom7(nc)
                          endif
                      enddo
                      
                      ppmom2 = max( ppmom2,0.0 )
                      ppmom3 = max( ppmom3,0.0 )
                      ppmom4 = max( ppmom4,0.0 )
                      if( sizem .gt. 0.03e-04 )then
                          ppmom5 = max( ppmom5,0.0 )
                          ppmom6 = max( ppmom6,0.0 )
                          ppmom7 = max( ppmom7,0.0 )
                      endif
                      
! need weighting by scattering cross section ?  JCB 2004/02/09
                   l2=ppmom2
                   l3=ppmom3
                   l4=ppmom4
                   l5=ppmom5
                   l6=ppmom6
                   l7=ppmom7       
                endif

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



        subroutine fast_mieaer_mp( mie_parameter,refindx,extaer,scataer,gaer,bscoef )

         USE UTILIO_DEFN
         USE RUNTIME_VARS, ONLY : LOGDEV ! , SET_CPU_TIMING, TIMING_DELTA
         USE AERO_DATA           ! aero variable data
         USE CSQY_DATA,  ONLY : NWL => NWL_REF, NUM_REFRACTIVE, REFRACTIVE_INDEX,
     &                          WAVELENGTH => EFFWL_REF

         USE bhmie_routine
        
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
!       mode         -- array index for aerosol mode
!       iwave        -- array index for light wavelength
!       diameter_wet -- wet diameter, m
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
         real, allocatable ,save :: wavmid( : ) ! cm
         real weighte, weights
! various bookeeping variables
         integer, save :: nbin_a_maxd ! Max # of aerosol bins or modes
         integer, save :: nspint      ! Num of spectral intervals across
         
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

 
         integer, save      :: numang=0,nmom=7,ipolzn=0,momdim=7
         integer, save      :: nrefr,nrefi
         integer, parameter :: prefr=22,prefi=33
         integer, parameter :: nsizes = 600
         integer, parameter :: nsiz=200,nlog=30,ncoef=50
         integer            :: nsize

         integer            :: nr,ni
         
!        
         REAL( 8 )     :: CPU_TIME_START
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
!        
         real, allocatable :: p2( : ),p3( : )        
         real, allocatable :: p4( : ),p5( : )
         real, allocatable :: p6( : ),p7( : )
         
         
         real, allocatable, save ::  extp( :,:,: )       ! specific extinction
         real, allocatable, save ::  albp( :,:,: )       ! single scat alb
         real, allocatable, save ::  asmp( :,:,: )       ! asymmetry factor
         real, allocatable, save ::  ascat( :,:,: )       ! scattering efficiency, JCB 2004/02/09
         real, allocatable, save ::  pmom2( :,:,: )       ! phase function expansion, #2
         real, allocatable, save ::  pmom3( :,:,: )       ! phase function expansion, #3
         real, allocatable, save ::  pmom4( :,:,: )       ! phase function expansion, #4
         real, allocatable, save ::  pmom5( :,:,: )       ! phase function expansion, #5
         real, allocatable, save ::  pmom6( :,:,: )       ! phase function expansion, #6
         real, allocatable, save ::  pmom7( :,:,: )         ! phase function expansion, #7
         real, allocatable, save ::  sback2p( :,:,: )    ! JCB 2007/02/01 - backscatter
         
         Type(Optics_Tables), Allocatable :: Modal_Tables_MP( : )

         real ::  aero_radius

!--------------
! Coefficients of Chebychev Polynomials
         real cext(ncoef),casm(ncoef),cpmom2(ncoef)
         real cscat(ncoef)                          ! JCB 2004/02/09
         real cpmom3(ncoef),cpmom4(ncoef),cpmom5(ncoef)
         real cpmom6(ncoef),cpmom7(ncoef)
         real cpsback2p(ncoef)                      ! JCB 2007/02/09  - backscatter
         
         integer itab,jtab
         integer itabp,jtabp
         real    ttab,utab
         
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

         real(8) :: thesize        ! 2 pi radpart / waveleng = size parameter

!           specabs = absorption coeff / unit dry mass
!           specscat = scattering coeff / unit dry mass

         complex(8)       :: crefin,crefd
         complex(8), save :: crefw
         real, save :: rmin=0.005e-4,rmax=50.e-4   ! min, max aerosol size bin

         
         real bma,bpa
         
         real, save :: xrmin,xrmax,xr
         real xrad      ! normalized aerosol radius
         real ch(ncoef) ! chebychev polynomial
!         real term1(ncoef) ! chebychev polynomial
!         real term2(ncoef) ! chebychev polynomial
!         real term3(ncoef) ! chebychev polynomial
!         real term4(ncoef) ! chebychev polynomial
         
         real, save :: rhoh2o = 1.0     ! density of liquid water (g/cm3)
         
         real refr         ! real part of refractive index
         real refi         ! imaginary part of refractive index
         
         real thesize_bhmie    ! 2 pi radpart / waveleng = size parameter
         real qext_bhmie       ! array of extinction efficiencies
         real qsca_bhmie       ! array of scattering efficiencies
         real qback_bhmie      ! array of scattering efficiencies
         real gsca_bhmie       ! array of asymmetry factor * scattering efficiency
         
         real :: weight_real   ! nomralized real part refractive index difference used in bilinear interpolation 
         real :: weight_imag   ! nomralized imaginary part refractive index difference used in bilinear interpolation
         real :: wrwi          ! weighting factors used on bilinear interpolation points
         real :: wr_wrwi       ! weighting factors used on bilinear interpolation points
         real :: wi_wrwi       ! weighting factors used on bilinear interpolation points
         real :: one_wrwi_wi   ! weighting factors used on bilinear interpolation points
         
         complex refrel_bhmie

         real :: quadrature_factor
         real :: max_quad_point

         real, save :: refrmin ! minimum of real part of refractive index
         real, save :: refrmax ! maximum of real part of refractive index
         real, save :: refimin ! minimum of imag part of refractive index
         real, save :: refimax ! maximum of imag part of refractive index
         real, save :: drefr   ! increment in real part of refractive index
         real, save :: drefi   ! increment in imag part of refractive index
         real, save :: refrtab(prefr) ! table of real refractive indices for aerosols
         real, save :: refitab(prefi) ! table of imag refractive indices for aerosols
!         complex specrefndx(ltype)    ! refractivr indices

         real, save :: reciprocal_drefr  ! reciprocal of increment in real part of refractive index
         real, save :: reciprocal_drefi  ! reciprocal of increment in  imag part of refractive index
         real, save :: reciprocal_drefri ! reciprocal of real times imag increments of refractive index

         real, parameter :: pie   = pi
         real, parameter :: third = 1.0/3.0
         
         integer irams, jrams
! diagnostic declarations
         integer kcallmieaer2
         integer ibin, imode
         integer        :: mie_unit
         character(150) :: msg
         character( 16), parameter :: mie_table = 'MIE_TABLE'
         character( 16), parameter :: pname     = 'fast_mieaer'


!      if(first)then
       if( Create_Table )then
        first=.false.
        Create_Table = .False.

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
        
        do i=1,NUM_REFRACTIVE
           refrmin = amin1( refrmin,MINVAL( REFRACTIVE_INDEX( i )%REAL_PART( :,: ) ))
           refrmax = amax1( refrmax,MAXVAL( REFRACTIVE_INDEX( i )%REAL_PART( :,: ) ))
           refimin = amin1( refimin,-MINVAL( REFRACTIVE_INDEX( i )%IMAG_PART( :,: ) ))  
           refimax = amax1( refimax,-MAXVAL( REFRACTIVE_INDEX( i )%IMAG_PART( :,: ) ))
        enddo
        
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

         nbin_a_maxd = n_mode
!
         
         nspint = nwl
         allocate( wavmid( nwl ) )
         wavmid(1:nwl) =  1.0e-7*wavelength(1:nwl)                         

         allocate( extp(ncoef,prefr,prefi),      
     &             albp(ncoef,prefr,prefi),   
     &             asmp(ncoef,prefr,prefi),   
     &             ascat(ncoef,prefr,prefi),  
     &             sback2p(ncoef,prefr,prefi) )
     
    
         If( .Not. TWO_STREAM )Then

         allocate( pmom2(ncoef,prefr,prefi),  
     &             pmom3(ncoef,prefr,prefi),  
     &             pmom4(ncoef,prefr,prefi),  
     &             pmom5(ncoef,prefr,prefi),  
     &             pmom6(ncoef,prefr,prefi),  
     &             pmom7(ncoef,prefr,prefi) )

         End If
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

          quadrature_factor = exp( sqrt2 * log( max_sigma_g ) * max_quad_point )
         
         rmax = amax1(rmax,60.0*MAXVAL(max_dg_wet))*quadrature_factor
         rmax = 2.0*pi*rmax/minval( wavmid )
         rmin = amin1(rmin,40.0*MINVAL(min_dg_dry))/quadrature_factor
         rmin = 2.0*pi*rmin/maxval( wavmid )
         
         bma=0.5*alog(rmax/rmin) ! JCB
         bpa=0.5*alog(rmax*rmin) ! JCB
         xrmin=alog(rmin)
         xrmax=alog(rmax)

         nsize = nsizes

!        IF( READ_TABLE )THEN

!          mie_unit = getefile( mie_table, .true., .true., pname )

!          if ( mie_unit .lt. 0 ) then
!            msg = 'Error opening ' // mie_table
!            call m3exit ( pname, 0, 0, msg, xstat1 )
!          end if

!           do ni=1,nrefi
!             do nr=1,nrefr
!                read(mie_unit,'(a)')msg
!                do nc = 1,ncoef
!                   read(mie_unit,'(21(es16.7,1x))')(extp(nc,nr,nr))
!                   end do
!                read(mie_unit,'(a)')msg
!                do nc = 1,ncoef
!                   read(mie_unit,'(21(es16.7,1x))')(ascat(nc,nr,nr))
!                end do
!                read(mie_unit,'(a)')msg
!                do nc = 1,ncoef
!                   read(mie_unit,'(21(es16.7,1x))')(asmp(nc,nr,nr))
!                end do
!                read(mie_unit,'(a)')msg
!                do nc = 1,ncoef
!                   read(mie_unit,'(21(es16.7,1x))')(sback2p(nc,nr,nr))
!                end do
!             end do
!           end do
!           close(mie_unit)

!         END IF
!        
!        If( .Not. READ_TABLE )Then
         
         allocate( qext( nsize ),
     &             qsca( nsize ),
     &             gqsc( nsize ) )
      
         allocate( asymm( nsize ),
     &              scat( nsize ),
     &               sb2( nsize ),
     &            qextr4( nsize ),
     &                rs( nsize ) )          

          If( .Not. TWO_STREAM )Then

             allocate( p2(nsize),
     &                 p3(nsize),
     &                 p4(nsize),
     &                 p5(nsize),
     &                 p6(nsize),
     &                 p7(nsize) )

          End IF
          
!wavelength loop                                            
!         do 200 ns=1,nspint                                
          ns = 1          
                                                                     
!         calibrate parameterization with range of refractive indices 

!                 print*,"computing table for wavelength ",wavelength(ns)

            do 120 ni=1,nrefi
              do 120 nr=1,nrefr                                        

               refrtab(nr)=refrmin+(nr-1)*drefr
               refitab(ni)=refimin+(ni-1)*drefi
               crefd=dcmplx(real(refrtab(nr),8),real(refitab(ni),8))
!              mie calculations of optical efficiencies
                  nsize = nsizes
                  do n=1,nsize
                  
                     xr=cos(pie*(real(n)-0.5)/real(nsize))
                     rs(n)=real(exp(xr*bma+bpa),8)
                     thesize = real(rs(n),8)
!                    size parameter and weighted refractive index
!                     thesize=real(2.*pie*rs(n)/wavmid(ns),8)
!                     thesize=min(thesize,10000.d0)
                  
                     if( .Not. TWO_STREAM )then
                         call miev0(thesize,crefd,perfct,mimcut,anyang, 
     &                       numang,xmu,nmom,ipolzn,momdim,prnt,
     &                       qext(n),qsca(n),gqsc(n),pmom,sforw,sback,s1,
     &                       s2,tforw,tback )
                         qextr4(n)=qext(n)
!                        qabs(n)=qext(n)-qsca(n) ! not necessary anymore JCB 2004/02/09
                         scat(n)=qsca(n) ! JCB 2004/02/09
!                         asymm(n)=gqsc(n)*scat(n) ! assume always greater than zero
                         asymm(n)=gqsc(n)/scat(n) ! assume always greater than zero
                       
! coefficients of phase function expansion; note modification by JCB of miev0 coefficients
                         p2(n)=pmom(2,1)/pmom(0,1)*5.0
                         p3(n)=pmom(3,1)/pmom(0,1)*7.0
                         p4(n)=pmom(4,1)/pmom(0,1)*9.0
                         p5(n)=pmom(5,1)/pmom(0,1)*11.0
                         p6(n)=pmom(6,1)/pmom(0,1)*13.0
                         p7(n)=pmom(7,1)/pmom(0,1)*15.0
! backscattering efficiency, Bohren and Huffman, page 122
! as stated by Bohren and Huffman, this is 4*pie times what is should be
! may need to be smoothed - a very rough function - for the time being we won't apply smoothing
! and let the integration over the size distribution be the smoothing
                            
                     else
                     
                         thesize_bhmie = real( thesize )
                         refrel_bhmie  = cmplx(real(refrtab(nr)),-real(refitab(ni)))
                         call driver_bhmie_flexy(thesize_bhmie,refrel_bhmie,qext_bhmie,qsca_bhmie,gsca_bhmie) 
                         asymm(n) = gsca_bhmie/qsca_bhmie 
                         qextr4(n)= qext_bhmie*thesize_bhmie
                         scat(n)  = qsca_bhmie*thesize_bhmie 
                         sback    = sback_bhmie
                         sb2(n)   = 4.0*sback*dconjg(sback)
     &                            / (thesize_bhmie*thesize_bhmie) ! JCB 2007/02/01  
                         
!                 write(6,'("bhmie result: refr,refi,thesize,ext,scat,asym = ",5(es12.4,1x))')
!    &            refrtab(nr),refitab(ni),thesize,qextr4(n),scat(n),asymm(n)
                         
                     endif
                  
                  enddo
  100             continue

                  call fitcurv(rs,qextr4,extp(:,nr,ni),ncoef,nsize)
                  call fitcurv(rs,scat,ascat(:,nr,ni),ncoef,nsize) ! JCB 2004/02/07 - scattering efficiency
                  call fitcurv(rs,asymm,asmp(:,nr,ni),ncoef,nsize)
                  call fitcurv(rs,sb2,sback2p(:,nr,ni),ncoef,nsize) ! JCB 2007/02/01 - backscattering efficiency             


                  if( .Not. TWO_STREAM )then
                   
                     call fitcurv_nolog(rs,p2,pmom2(:,nr,ni),ncoef,nsize)
                     call fitcurv_nolog(rs,p3,pmom3(:,nr,ni),ncoef,nsize)
                     call fitcurv_nolog(rs,p4,pmom4(:,nr,ni),ncoef,nsize)
                     call fitcurv_nolog(rs,p5,pmom5(:,nr,ni),ncoef,nsize)
                     call fitcurv_nolog(rs,p6,pmom6(:,nr,ni),ncoef,nsize)
                     call fitcurv_nolog(rs,p7,pmom7(:,nr,ni),ncoef,nsize)
                  
                  endif

  120       continue
!        If( MYPE .ne. 0 )Then
!
!        print*,'writing mie coefficients'

!        mie_unit = junit()
!        open(unit=mie_unit,file='mie_table_coeffs_single.txt',form='FORMATTED')
!        do ni=1,nrefi
!           do nr=1,nrefr
!             write(mie_unit,'(a)')'! extp coefficients for chebyshev polynomials'
!             do nc = 1,ncoef
!                write(mie_unit,'(21(es16.7,1x))')(extp(nc,nr,ni))
!             end do
!             write(mie_unit,'(a)')'! ascat coefficients for chebyshev polynomials'
!             do nc = 1,ncoef
!                write(mie_unit,'(21(es16.7,1x))')(ascat(nc,nr,ni))
!             end do
!             write(mie_unit,'(a)')'! asym coefficients for chebyshev polynomials'
!             do nc = 1,ncoef
!                write(mie_unit,'(21(es16.7,1x))')(asmp(nc,nr,ni))
!             end do
!             write(mie_unit,'(a)')'! back scat coefficients for chebyshev polynomials'
!             do nc = 1,ncoef
!                write(mie_unit,'(21(es16.7,1x))')(sback2p(nc,nr,ni))
!              end do
!          end do
!        end do
!        close(mie_unit)

!        End If
                                       
!  200    continue

        
         deallocate( qext,
     &               qsca,
     &               gqsc  )
      
         deallocate( asymm, 
     &                scat, 
     &                 sb2, 
     &              qextr4, 
     &                  rs  ) 
     
           If( .Not. TWO_STREAM )Then

              deallocate( p2,p3,p4,p5,
     &                    p6,p7 )

           End If

!        End If ! Not READ_TABLE

         print*,'Initializing Fast_mie'
         
      endif ! end first call block
      
!         CPU_TIME_START = SET_CPU_TIMING()
         
         thesum=0.0
         thesum=thesum+1.0e-6
         aero_radius = mie_parameter 
         ns         = 1
         m          = 3

!               tauaer=0.0
!               waer=0.0
               gaer    = 0.0
               sizeaer = 1.0e-30
               extaer  = 1.0e-30
               return 
               
               l2=0.0
               l3=0.0
               l4=0.0
               l5=0.0
               l6=0.0
               l7=0.0
               bscoef=0.0        ! JCB 2007/02/01 - backscattering coefficient
! loop over the bins
                  sizem=aero_radius ! radius in cm
! check limits of particle size
! rce 2004-dec-07 - use klevel in write statements
                  if(aero_radius.le.rmin)then
                    write( msg, '(a, 1x, es11.4, a, 1x, es11.4 )' )       
     &              'AE Mode, WRF FAST mie: aerosol mie parameter set to ',
     &              0.02*rmin, ' from ', mie_parameter
                    aero_radius=rmin
                     write(logdev,'(a)')TRim( msg )
                  endif
!
                  if(aero_radius.gt.rmax)then
                    write( msg, '(a, 1x, es11.4, a, 1x, es11.4 )' )       
     &              'AE Mode, WRF FAST mie: aerosol mie parameter set to ',
     &              0.02*rmax, ' from ', mie_parameter
                     write(logdev,'(a)')Trim( msg )
                     aero_radius=rmax
                  endif


                  crefin=refindx
                  refr=real(crefin)
! change Rahul's imaginary part of the index of refraction from positive to negative
                  refi=-imag(crefin)

                  x=alog(aero_radius) ! radius in cm
                  xrad=x
! normalize size parameter
                  xrad=(2.0*xrad-xrmax-xrmin)
     &                /(xrmax-xrmin)
     
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
                  
                  
                  itab = int( (refr-refrmin)*reciprocal_drefr ) + 1
                  itab = min( nrefr,max( itab,1 ) )
                  jtab = int( (refi-refimin)*reciprocal_drefi ) + 1
                  jtab = min( nrefi,max( jtab,1 ) )
!                  print*,'ShortCut itab, jtab = ',itab,jtab
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
                     cext(nc)  = one_wrwi_wi*extp(nc,itab,jtab) + wr_wrwi*extp(nc,itabp,jtab)
     &                        +          wrwi*extp(nc,itabp,jtabp) + wi_wrwi*extp(nc,itab,jtabp)                     
                     cscat(nc) = one_wrwi_wi*ascat(nc,itab,jtab) + wr_wrwi*ascat(nc,itabp,jtab)
     &                         +         wrwi*ascat(nc,itabp,jtabp) + wi_wrwi*ascat(nc,itab,jtabp)                     
                     casm(nc)  = one_wrwi_wi*asmp(nc,itab,jtab) + wr_wrwi*asmp(nc,itabp,jtab)
     &                         +       wrwi*asmp(nc,itabp,jtabp) + wi_wrwi*asmp(nc,itab,jtabp)                     
                     cpsback2p(nc)  = one_wrwi_wi*sback2p(nc,itab,jtab) + wr_wrwi*sback2p(nc,itabp,jtab)
     &                              +  wrwi*sback2p(nc,itabp,jtabp) + wi_wrwi*sback2p(nc,itab,jtabp)                     
                  enddo

! interpolate coefficients linear in refractive index
! first call calcs itab,jtab,ttab,utab
                  itab=0
                  search = .true.
                                   
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

!                 write(6,'("fast mie result: mie_parameter,ext,scat,asym = ",5(es12.4,1x))')
!    &            mie_parameter,extaer,scataer,gaer
                              
                  if( .Not. TWO_STREAM )then

                      do nc=1,ncoef
                         cpmom2(nc) = one_wrwi_wi*pmom2(nc,itab,jtab) + wr_wrwi*pmom2(nc,itabp,jtab)
     &                              +        wrwi*pmom2(nc,itabp,jtabp) + wi_wrwi*pmom2(nc,itab,jtabp)                     
                         cpmom3(nc) = one_wrwi_wi*pmom3(nc,itab,jtab) + wr_wrwi*pmom3(nc,itabp,jtab)
     &                              +        wrwi*pmom3(nc,itabp,jtabp) + wi_wrwi*pmom3(nc,itab,jtabp)                     
                         cpmom4(nc) = one_wrwi_wi*pmom2(nc,itab,jtab) + wr_wrwi*pmom4(nc,itabp,jtab)
     &                              +        wrwi*pmom4(nc,itabp,jtabp) + wi_wrwi*pmom4(nc,itab,jtabp)                     
                         cpmom5(nc) = one_wrwi_wi*pmom5(nc,itab,jtab) + wr_wrwi*pmom5(nc,itabp,jtab)
     &                              +        wrwi*pmom5(nc,itabp,jtabp) + wi_wrwi*pmom5(nc,itab,jtabp)                     
                         cpmom6(nc) = one_wrwi_wi*pmom6(nc,itab,jtab) + wr_wrwi*pmom6(nc,itabp,jtab)
     &                              +        wrwi*pmom6(nc,itabp,jtabp) + wi_wrwi*pmom6(nc,itab,jtabp)                     
                         cpmom7(nc) = one_wrwi_wi*pmom7(nc,itab,jtab) + wr_wrwi*pmom7(nc,itabp,jtab)
     &                              +        wrwi*pmom7(nc,itabp,jtabp) + wi_wrwi*pmom7(nc,itab,jtabp)                     
                      enddo
                      
                      ppmom2=0.5*cpmom2(1)
                      ppmom3=0.5*cpmom3(1)
                      ppmom4=0.5*cpmom4(1)
                      if( sizem .le. 0.03e-04 )then
                          ppmom5 = 0.0
                          ppmom6 = 0.0
                          ppmom7 = 0.0
                      else    
                          ppmom5=0.5*cpmom5(1)
                          ppmom6=0.5*cpmom6(1)
                          ppmom7=0.5*cpmom7(1)
                      endif
                      
                      do nc=2,ncoef
                          ppmom2=ppmom2+ch(nc)*cpmom2(nc)
                          ppmom3=ppmom3+ch(nc)*cpmom3(nc)
                          ppmom4=ppmom4+ch(nc)*cpmom4(nc)
                          if( sizem .gt. 0.03e-04 )then
                             ppmom5=ppmom5+ch(nc)*cpmom5(nc)
                             ppmom6=ppmom6+ch(nc)*cpmom6(nc)
                             ppmom7=ppmom7+ch(nc)*cpmom7(nc)
                          endif
                      enddo
                      
                      ppmom2 = max( ppmom2,0.0 )
                      ppmom3 = max( ppmom3,0.0 )
                      ppmom4 = max( ppmom4,0.0 )
                      if( sizem .gt. 0.03e-04 )then
                          ppmom5 = max( ppmom5,0.0 )
                          ppmom6 = max( ppmom6,0.0 )
                          ppmom7 = max( ppmom7,0.0 )
                      endif
                      
! need weighting by scattering cross section ?  JCB 2004/02/09
                   l2=ppmom2
                   l3=ppmom3
                   l4=ppmom4
                   l5=ppmom5
                   l6=ppmom6
                   l7=ppmom7       
                endif 


      return
      end subroutine fast_mieaer_MP                    

      end module wrf_fast_mie
