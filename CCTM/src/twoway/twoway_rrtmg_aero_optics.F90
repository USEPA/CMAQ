! Revision History:
!  2016/02/23 David Wong extracted the complex number module and put it in a file
!  2016/05/23 David Wong - replaced rrtmg_aero_optical_util_module with
!                          cmaq_rrtmg_aero_optical_util_module to avoid duplication 
!                          of the same module name on WRF side of the two-way model

MODULE cmaq_rrtmg_aero_optical_util_module


     Integer      :: AERO_UTIL_LOG = 0 
private
public :: aero_optical, aero_optical2, aero_optical_CS, AERO_UTIL_LOG

interface ghintBH 
  module procedure ghintBH_1, ghintBH_2, ghintBH_Odd
end interface

interface ghintBH_CS
  module procedure ghintBH_CS_even, ghintBH_CS_odd
end interface

      Logical, Parameter :: Use_Odd_Quadrature = .True.
       Integer, Parameter :: Quadrature_Points = 3
!      Integer, Parameter :: Quadrature_Points = 1
      
!B.Hutzell One point quadature IGH = 1

       real, parameter :: ghxi_1(1) = 0.00000000000
       real, parameter :: ghwi_1(1) = 1.77245385091
       
!B.Hutzell Three point quadature IGH = 3
       real, parameter :: ghxi_3(3) = (/ -1.22474487139,   &
                                          0.00000000000,   &
                                          1.22474487139 /)

       real, parameter :: ghwi_3(3) = (/ 0.295408975151,   &
                                         1.181635900000,   &
                                         0.295408975151 /)
                                         
!B.Hutzell Five point quadature IGH = 5
       real(8), parameter :: ghxi_5(5) = (/ -2.02018287046d0,  &
                                            -0.958572464614d0, & 
                                             0.00000000000d0,  &
                                             0.958572464614d0, &
                                             2.02018287046d0 /)

       real(8), parameter :: ghwi_5(5) = (/ 0.019953242059d0,   &
                                            0.393619323152d0,   &
                                            0.945308720483d0,   &
                                            0.393619323152d0,   &
                                            0.019953242059d0 /)

                                         

!B.Hutzell Nine point quadature IGH = 9 points
!No.  Abscissas  Weight  Total Weight  
       real, parameter :: ghxi_9(9) = (/ -3.19099320178,  &  
                                         -2.26658058453,  &  
                                         -1.46855328922,  &  
                                         -0.72355101875,  & 
                                          0.00000000000,  &  
                                          0.72355101875,  & 
                                          1.46855328922,  &  
                                          2.26658058453,  &   
                                          3.19099320178 /)  

       real, parameter :: ghwi_9(9) = (/ 3.96069772633E-5, &
                                         0.00494362428,    &
                                         0.08847452739,    &
                                         0.43265155900,    &
                                         0.72023521561,    &
                                         0.43265155900,    &
                                         0.08847452739,    &
                                         0.004943624275,   &
                                         3.96069772633E-5 /)



contains

! ------------------------------------------------------------------
       subroutine getqext_BH (xx, crefin, qextalf, qscatalf, gscatalfg,SUCCESS)
       implicit none

       real, intent(in)     :: XX 
       real, intent(out)    :: qextalf, qscatalf, gscatalfg
       complex, intent(in)  :: CREFIN
       logical, intent(out) :: success
!local        
       real( 8 ), parameter  :: one_third = 1.0d0 / 3.0d0
       integer              :: NXX
       integer              :: nstop, modulus

       real :: QEXT, QSCA, QBACK, G_MIE, xx1
       
       real( 8 )    :: x
       complex( 8 ) :: refractive_index
       
       x = real( XX, 8 )
       refractive_index = dcmplx( real( CREFIN ), imag( CREFIN ) )
       
       modulus = int( abs( x * refractive_index ) )      
       nstop = int( x + 4.0d0 * x**one_third + 2.0d0 )
       
       nxx = max( modulus, nstop ) + 15
       
       xx1 = 1.0 / XX
       
!       CALL BHMIE (XX,CREFIN,QEXT,QSCA,QBACK,G_MIE, SUCCESS)
        CALL BHMIE_FLEXI (XX, NXX, NSTOP, CREFIN,QEXT,QSCA,QBACK,G_MIE, SUCCESS)

       qextalf   = QEXT * xx1
       qscatalf  = QSCA * xx1
       gscatalfg = qscatalf * G_MIE

       end subroutine getqext_bh

! ------------------------------------------------------------------
       SUBROUTINE BHMIE (X, REFREL, QQEXT, QQSCA, QBACK, GSCA, SUCCESS)

! FSB Changed the call vector to return only QEXT, QSCAT QBACK GSCA
!     and ignore NANG, S1 and S2 and all calculations for them

       implicit none 

! Arguments:
       real, intent(in)    :: X        ! X = pi*particle_diameter / Wavelength
       complex, intent(in) :: REFREL

!    REFREL = (complex refr. index of sphere)/(real index of medium)
!    in the current use the index of refraction of the the medium
!    i taken at 1.0 real.
!
!    Output

       real,    intent(out) :: QQEXT, QQSCA, QBACK, GSCA
       logical, intent(out) :: SUCCESS

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

       integer, parameter :: MXNANG=10, NMXX=600000   ! FSB new limits
       real*8, parameter  :: PII = 3.1415916536D0
       real*8, parameter  :: ONE = 1.0D0, TWO = 2.0D0

! Local variables:
       integer    :: NANG
       integer    :: N,NSTOP,NMX,NN
       real*8     :: QSCA, QEXT, DX1, DXX1      
       real*8     :: CHI,CHI0,CHI1,DX,EN,P,PSI,PSI0,PSI1,XSTOP,YMOD               
       real*8     :: TWO_N_M_ONE, TWO_N_P_ONE, EN1, FACTOR
       complex*16 :: AN,AN1,BN,BN1,DREFRL,XI,XI1,Y, Y1, DREFRL1
       complex*16 :: D(NMXX), FAC1, FAC2
       complex*16   :: XBACK

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

       SUCCESS = .TRUE.
       NANG = 2 ! FSB only this value 
! IF(NANG.GT.MXNANG)STOP'***Error: NANG > MXNANG in bhmie'
!      IF (NANG .LT. 2) NANG = 2

       DX = REAL( X, 8 )
! FSB Define reciprocals so that divisions can be replaced by multiplications.      
       DX1  = ONE / DX
       DXX1 = DX1 * DX1
       DREFRL = DCMPLX( REFREL ) 
       DREFRL1 = ONE / DREFRL
       Y = DX * DREFRL
       Y1 = ONE / Y
       YMOD = ABS(Y)
 
!*** Series expansion terminated after NSTOP terms
!    Logarithmic derivatives calculated from NMX on down
       XSTOP = REAL( X + 4.0 * X**0.3333 + 2.0, 8)
       NMX  = INT( MAX(XSTOP,YMOD) ) + 15

! BTD experiment 91/1/15: add one more term to series and compare results
!      NMX=AMAX1(XSTOP,YMOD)+16
! test: compute 7001 wavelengths between .0001 and 1000 micron
! for a=1.0micron SiC grain.  When NMX increased by 1, only a single
! computed number changed (out of 4*7001) and it only changed by 1/8387
! conclusion: we are indeed retaining enough terms in series!
       NSTOP = INT( XSTOP )
       FACTOR = 1.0D0
 
       IF (NMX .GT. NMXX) THEN
          WRITE(6,*)'Error: NMX > NMXX=',NMXX,' for |m|x=',YMOD
          SUCCESS = .FALSE.
          RETURN
       END IF

! FSB all code relating to scattering angles is removed out for
!     reasons of efficiency when running in a three-dimensional 
!     code. We only need QQSCA, QQEXT, GSCA AND QBACK

 
!*** Logarithmic derivative D(J) calculated by downward recurrence
!    beginning with initial value (0.,0.) 
 
       D(NMX) = DCMPLX(0.0D0,0.0D0)
       NN = NMX - 1
       DO N = 1,NN
          EN  = REAL(NMX - N + 1, 8 )
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
       XBACK = (0.0d0,0.0d0)

! FSB Start main loop       
       DO N = 1,NSTOP
          EN        = REAL( N, 8)
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
          QSCA = QSCA + (TWO_N_P_ONE) * ( ABS(AN)**2+ ABS(BN)**2 )
          
! FSB calculate XBACK from B & H Page 122          
          FACTOR = -1.0d0 * FACTOR  ! calculate (-1.0 ** N)
          XBACK = XBACK + (TWO_N_P_ONE) * factor * (AN - BN)
          
! FSB calculate asymmetry factor   
           GSCA = GSCA + REAL( ((TWO_N_P_ONE)/(EN * (EN + ONE))) *     &
                 (REAL(AN)*REAL(BN)+IMAG(AN)*IMAG(BN)))

          IF (N .GT. 1)THEN
             GSCA = GSCA + REAL( (EN - EN1) *                         &
                    (REAL(AN1)*REAL(AN) + IMAG(AN1)*IMAG(AN) +  &
                     REAL(BN1)*REAL(BN) + IMAG(BN1)*IMAG(BN)))
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
       GSCA  = REAL( TWO / QSCA )  * GSCA

! FSB in the following, divisions by DX * DX has been replaced by
!      multiplication by DXX1 the reciprocal of 1.0 / (DX *DX)           
       QQSCA = REAL( TWO * QSCA * DXX1 )
       QQEXT = REAL( TWO * QEXT * DXX1 ) 
       QBACK = REAL( REAL ( 0.5d0 * XBACK * CONJG(XBACK), 8 ) * DXX1 )  ! B&H Page 122

       END subroutine BHMIE

! ------------------------------------------------------------------
       subroutine aero_optical ( lamda_in, nmode, nr, ni, Vol,   &
                                 dgn, sig, bext, bscat, g_bar,   &
                                 modulus, success )
     
! *** calculate the extinction and scattering coefficients and
!     assymetry factors for each wavelength as a sum over the 
!     individual lognormal modes. Each mode may have a different 
!     set of refractive indices.

      IMPLICIT NONE
! *** input variables
      real, intent(in)    :: lamda_in               ! wavelengths  [micro-m]
      INTEGER, intent(in) :: nmode                  ! number of lognormal modes
      real, intent(in)    :: nr( nmode), ni(nmode)  ! real and imaginary 
                                                    ! refractive indices
      real, intent(in)    :: Vol(nmode)             ! modal aerosol volumes [m**3 /m**3]
      real, intent(in)    :: dgn(nmode)             ! geometric mean diameters 
                                                    ! for number distribution [ m]
      real, intent(in)    :: sig(nmode)             ! geometric standard deviation 

      real, intent(in), optional :: modulus(nmode)  ! modulus of refracive index                          
      
! *** output variables 
      real, intent(out)    :: bext    ! extinction coefficient [ 1 / m ]
      real, intent(out)    :: bscat   ! scattering coefficient [ 1 / m ]
      real, intent(out)    :: g_bar   ! assymetry factor for Mie and molecular scattering
      logical, intent(out) :: success ! flag for successful calculation
! *** internal variables
      INTEGER  :: j             ! loop index
!     real     :: xlnsig(nmode) ! natural log of geometric standard deviations      
      real     :: beta_Sc, bsc  !aerosol scattering coefficient 
 
      real     :: beta_Ex       ! aerosol extinction coefficients       
      real     :: G             ! modal aerosol assymetry factors
      real     :: sum_g
      real     :: LSIGX
      real     :: lamdam1       ! 1/ lamda
      real     :: alphav        ! Mie size parameter
      real     :: vfac
      real     :: modalph

      real, parameter :: pi = 3.14159265359

       Logical, Save :: Initialize = .True.
       
! *** coded 09/08/2004 by Dr. Francis S. Binkowski
! FSB Modified for RRTMG version December 2009.
! FSB modified 10/06/2004, 10/12/2004, 10/18/2005
! FSB 01/12/2006
!     Formerly Carolina Environmental Program
! FSB now the Institute for the Environment
!     University of North Carolina at Chapel Hill
!     email: frank_binkowski@unc.edu


! *** initialize variables
       lamdam1 = 1.0e6 / lamda_in   ! lamda now in [ m ]
       bext    = 0.0
       bscat   = 0.0
       sum_g   = 0.0
        
!      write(30,*) ' inside aero_optical', ' lamda = ', lamda
      
       DO j = 1, nmode
!    calculate the extinction and scattering coefficients
!    for each mode 
!         write(20,*) ' j = ', j
          LSIGX = log(sig(j))

!         write(30,*) 'j = ', j
!         write(30,*) 'VLX = ', Vol(j)
!         write(30,*) 'DGX = ', dgn(j)
!         write(30,*) 'SIGX = ', sig(j)
!         write(30,*) 'NRX =', nr(j)
!         write(30,*) 'NIX = ', ni(j)
!         write(30,*) 'LSIGX = ', LSIGX
       
!     calculate Mie size parameter for volume distribution
!     exp(3.0 * xlnsig*xlnsig)  converts dgn to dgv (volume diameter)
          alphav =  pi * dgn(j) * exp(3.0 * LSIGX * LSIGX) * lamdam1

          if (present(modulus)) then
             modalph = alphav * modulus(j)   
          end if

!         Write(30,*) ' alphav = ', alphav
       
!         write(20,*) j, alphav, modalph, modulus(j)
!         write(20,*) j, alphav, modalph, nr(j), ni(j)
      
 
          CALL ghintBH (nr(j), ni(j), alphav, LSIGX, beta_EX, beta_Sc, G, success)            

!         write(30,*) 'after Call to ghintBH ',' j = ', j 
!         write(30,*) ' beta_EX =', beta_EX
!         write(30,*) ' beta_SC = ', beta_Sc
!         write(30,*) ' G = ', G

! *** ghintBH returns the normalized values
!     Calculate the actual extinction and scattering coefficients 
!     by multplying by the modal volume and dividing by the wavelength
         
         
          vfac  =  Vol(j) * lamdam1 

!         write(20,*)' vfac = ', vfac
!         write(20,*)' G = ', G
!         write(20,*) ' beta_Ex = ', beta_Ex
!         write(20,*) ' beta_Sc = ', beta_Sc
        
! *** sum to get total extinction and scattering 
!     and contribution to the overal assymetry factor

          bext    = bext  + vfac * beta_Ex  ! [ 1 / m ]
          bsc     = vfac * beta_Sc
          bscat   = bscat + bsc          
          sum_g   = sum_g + bsc * G

       END DO  ! loop on modes  
       
! *** calculate combined assymetry factor for all modes  

       g_bar = sum_g / bscat ! changed to divide by bscat

!      write(30,*) ' inside aero_optical after loop '       
!      write(30,*) ' g_bar = ', g_bar
!      write(20,*) ' bext = ', bext
!      write(20,*) ' bscat = ', bscat
!      write(20,*) ' gbar = ', g_bar 
      
       END SUBROUTINE aero_optical

! ------------------------------------------------------------------
       subroutine ghintBH_1 (nr, ni, alfv, xlnsig, Qext_GH, Qscat_GH, g_gh, success) 

! FSB *********** This is the newest (05_30_2012) version of GhintBH
!      this version does the Mie method and calculates the optimum set of 
!      set of Gauss-Hermite abscissas and weights. 
! FSB Calls Penndorf codes for alfv .le.  0.3 

!      Dr. Francis S. Binkowski, The University of North Carolina
!                                at Chapel Hill
! FSB this code file now contains all of the necessary subroutines that 
!     are called to perform an integral of the Bohren and Huffman
!     Mie codes ( as updated by Prof. Bruce C. Drain of Princeton)
!       calculates the extinction and scattering coefficients 
!       normalized by wavelength and total particle volume
!       concentration for a log normal particle distribution 
!       with the logarithm of the geometric  standard deviation
!       given by xlnsig. The integral of the
!       asymmetry factor g is also calculated.
! FSB Change 12/20/2011 This code now has a choice of IGH based
!     upon alfv and nr. 
!  *** Does Gauss-Hermite quadrature of Qext / alfa & Qscat / alfa
!      and asymmetry factor <cos> over log normal distribution using 
!      symmetric  points.
 
       implicit none

       real, intent(in)     :: nr, ni     ! refractive indices
       real, intent(in)     :: alfv       ! Mie parameter for dgv
       real, intent(in)     :: xlnsig     ! log of geometric  standard deviation
       real, intent(out)    :: Qext_GH    ! normalized extinction efficiency
       real, intent(out)    :: Qscat_GH   ! normalized scattering efficiency
       real, intent(out)    :: g_GH       ! asymmetry factor <cos>
       logical, intent(out) :: success    ! flag for successful calculation
      
       real    :: bext_P, bscat_P, babs_P, g_PCS, xlnsg2  ! see below for definition
      
       real    :: aa1                ! see below for definition
       real    :: alfaip, alfaim     ! Mie parameters at abscissas
     
!  *** these are Qext/alfa and Qscat/alfv at the abscissas
       real    :: qalfip_e, qalfim_e ! extinction  
       real    :: qalfip_s, qalfim_s ! scattering
       real    :: gsalfp, gsalfm     ! scattering times asymmetry factor
       integer :: IGH                ! index for GH quadrature      

! FSB define parameters 
       real, parameter :: pi = 3.14159265
       real, parameter :: sqrtpi = 1.772454 
       real, parameter :: sqrtpi1 = 1.0 / sqrtpi 
       real, parameter :: sqrt2 = 1.414214 
       real, parameter :: three_pi_two = 3.0 * pi / 2.0 
       real, parameter :: const = three_pi_two * sqrtpi1 
      
       integer :: i
       complex :: crefin                  ! complex index of refraction      
       real    :: sum_e,sum_s, xi,wxi,xf
       real    :: sum_sg

! Gauss-Hermite abscissas and weights
! *** the following weights and abscissas are from Abramowitz
!     Stegun, Table 25.10 page 924 
! FSB full precision from Table 25.10 

! FSB ten-point  - IGH = 5
       real, parameter :: ghxi_10(5) = (/ 0.342901327223705,     &
                                          1.036610829789514,     &
                                          1.756683649299882,     &
                                          2.532731674232790,     &
                                          3.436159118837738 /)

       real, parameter :: ghwi_10(5) = (/ 6.108626337353e-01,    &
                                          2.401386110823e-01,    &
                                          3.387439445548e-02,    &
                                          1.343645746781e-03,    &
                                          7.640432855233e-06 /)

! FSB six-point - IGH = 3
       real, parameter :: ghxi_6(3) = (/ 0.436077411927617,      &
                                         1.335849074013597,      &
                                         2.350604973674492 /)

       real, parameter :: ghwi_6(3) = (/ 7.246295952244e-01,     &
                                         1.570673203229e-01,     &
                                         4.530009905509e-03 /)

! FSB two-point - IGH = 1
       real, parameter :: ghxi_2(1) = (/ 0.707106781186548 /)

       real, parameter :: ghwi_2(1) = (/ 8.862269254528e-01 /)

       real    :: GHXI(5), GHWI(5) ! weight and abscissas
       integer :: NMAX             ! number of weights and abscissa

! FSB Check for valid range of Penndorf application.     
       if ( alfv .le. 0.3) then
          xlnsg2 = xlnsig*xlnsig
          call pennfsb (nr,ni,alfv,xlnsg2,bext_P,bscat_P,babs_P,g_PCS)
          Qext_GH  = bext_P
          Qscat_GH = bscat_p
          g_GH     = g_PCS * exp(4.0 * xlnsg2) ! match GH integral            
       else

! FSB We need to do a full Mie calculation now 
!     Choose IGH. These choices are designed to improve
!     the computational efficiency without sacrificing accuracy.

          IGH=3 ! default value; six_point is sufficient generally
! six point
          NMAX = 3

          if (nr .ge. 1.7) then 
! 10 point     
             IGH = 5 ! more points needed here
             NMAX = 5
          end if

          if ( alfv .gt. 20.0 .or. alfv .lt. 0.5 ) then
             IGH  = 1 ! in  this range fewer points are needed
             NMAX = 1
          end if

          if (IGH == 1) then
             GHXI(1)    = ghxi_2(1)
             GHWI(1)    = ghwi_2(1)
          else if (IGH == 3) then
             do i = 1, NMAX
                GHXI(i) = ghxi_6(i)
                GHWI(i) = ghwi_6(i)
             end do 
          else
             do i = 1,NMAX
                GHXI(i) = ghxi_10(i)
                GHWI(i) = ghwi_10(i)
             end do  
          end if ! set up number of abscissas and weights 
 
! FSB set  complex refractive index.      
          crefin= cmplx(nr,ni)      

! FSB now start the integration code
          aa1 = sqrt2 * xlnsig   ! This 1.0 / Sqrt( A ) in derivation of the integral
                                 ! where A = 1.0 / ( 2.0 * xlnsg**2 ) 

! Then alpha = alfv * exp[ u / sqrt(A) ]
! For Gauss-Hermite Quadrature u = xi 
! Therefore, xf = exp( xi / sqrt(A) ),
!  or xf = exp( xi * aa1 ) 
          sum_e  = 0.0
          sum_s  = 0.0
          sum_sg = 0.0
! FSB do NMAX calls to the MIE codes
          do i = 1,NMAX
             xi      = GHXI(i)
             wxi     = GHWI(i)
             xf      = exp( xi * aa1 )
             alfaip  = alfv * xf
             alfaim  = alfv / xf ! division cheaper than another exp()
! *** call subroutine to fetch the effficiencies

             call getqext_BH (alfaip, crefin, qalfip_e, qalfip_s, gsalfp, success)
             call getqext_BH (alfaim, crefin, qalfim_e, qalfim_s, gsalfm, success)

             sum_e  = sum_e + wxi  * ( qalfip_e + qalfim_e ) 
             sum_s  = sum_s + wxi  * ( qalfip_s + qalfim_s ) 
             sum_sg = sum_sg + wxi * ( gsalfp + gsalfm ) 
          end do 

          g_GH     = sum_sg / sum_s ! this is <cos>
          Qext_GH  = const * sum_e  ! 
          Qscat_GH = const * sum_s  
       end if

       end subroutine ghintBH_1

! ------------------------------------------------------------------
       subroutine pennfsb (n, k, xx, lnsg2, bext, bscat, babs, g)

! FSB a new version of Penndorf's equations. This version does 
!     analytical integration for Qext, Qscat, Qabs to generate
!     bext, bscat, babs. Note that the expressions for Qext & Qscat
!     hve been divide through by xx.
!
!     Reference:
!     Caldas, M., V. Semiao, 2001, Radiative properties of small
!                     particles: and extension of the Penndorff Model. Journal 
!                     of the Optical Society of America A, Vol. 18, No. 4, 
!                     pp 831-838.  

!       Penndorf, R., 1962a,Scattering and extinction coefficients for small
!                     absorbing and nonabsorbing aerosols,
!                     J. Optical Society of America, 52, 896-904.

!       Penndorf, P., 1962b,Scattering and extinction coefficients for 
!                     small Spherical aerosols, J. Atmos. Sci., 19, p 193

! FSB Coded by Dr. Francis S. Binkowski on October 25, 2011 by combining
!     two previous versions to get a common code for the Penndorf and
!     and Caldas & Semiao approaches. The Penndorf Qext, Qscat are much
!     better than the versions from Caldas & Semiao despite claims to
!     the contrary. The values of the asymmetry factor from Caldas & Semiao 
!     are better than can be obtained from Penndorf. 

! FSB This version does the analytical integral ove a lognormal 
!     size distribution.

       implicit none 
!     input variables
       real, intent(in)  :: n, k     ! refractive index
       real, intent(in)  :: xx       ! pi * diameter / wavelength
       real, intent(in)  :: lnsg2    ! log(sigma_g)**2       
       real, intent(out) :: bext     ! extinction coefficient
       real, intent(out) :: bscat    ! scattering coefficient
       real, intent(out) :: babs     ! absorption coefficient
       real, intent(out) :: g        ! asmmetry factor

!     internal variables
       complex*16  :: m, m2,m4,m6,m21,m22 
       complex*16  :: P,Q,R,S,T,U,V,W
       complex*16  :: Qprime, Rprime,Sprime,Tprime
       complex*16  :: Uprime, Vprime, Wprime
       real*8      :: Qs, gQs, gpennCS
       real*8      :: P1,P2, Q1, Q2 , S2,V1, V2 ! see usage
       real*8      :: P1SQ, P2SQ  ! see usage
       real*8      :: y, y2, y3, y4, y6, y7,  y8, y9       
       real*8      :: x, x2, x3, x4, x6, x7,  x8, x9 
       real        :: mag, modalf
! FSB define useful numbers and fractions 
       real, parameter :: pi = 3.14159265358979324d0 
       real, parameter :: three_pi_two = 1.5d0 * pi

       real*8, parameter :: one = 1.0d0
       real*8, parameter :: two = 2.0d0
       real*8, parameter :: three = 3.0d0
       real*8, parameter :: four = 4.0d0
       real*8, parameter :: five = 5.0d0
       real*8, parameter :: six = 6.0d0
       real*8, parameter :: eight = 8.0d0
       real*8, parameter :: nine = 9.0d0
       real*8, parameter :: fifteen = 15.0d0
       real*8, parameter :: fortyfive = 45.0d0
!      real*8, parameter :: two5ths = two / five
       real*8, parameter :: twothrds = two / three
       real*8, parameter :: fourthirds = four / three
       real*8, parameter :: onefifteenth = one / fifteen
       real*8, parameter :: twofifteenths = two * onefifteenth
!      real*8, parameter :: fourninths = four / nine
       real*8, parameter :: eightthirds = two * fourthirds
       real*8, parameter :: one_big = one / 31500.0d0
       real*8, parameter :: two_fortyfive = two / fortyfive
       real*8, parameter :: four_225 = four / 225.0d0 
       real*8, parameter :: one_210 = one / 210.0d0
!      real*8, parameter :: one_half = one / two 
!      real*8, parameter :: four_two = two
       real*8, parameter :: nine_two = 4.5d0
!      real*8, parameter :: sixteen_two = eight
!      real*8, parameter :: thirtysix_two = 36.0 / two
!      real*8, parameter :: twentyfive_two = 25.0d0 / two
!      real*8, parameter :: sixtyfour_two = 64.0d0 / two
!      real*8, parameter :: fortynine_two = 49.0d0 / two
!      real*8, parameter :: eightyone_two = 81.0d0 / two
       real*8            :: A,B,C,D,E, AA,BB,CC

! FSB start code
       mag = sqrt( n * n + k * k )
       modalf = mag * xx
       y  = REAL( xx, 8 ) ! convert to real*8
! FSB get powers of y        
       y2 = y * y
       y3 = y2 * y
       y4 = y3 * y
       y6 = y3 * y3
       y7 = y3 * y4
       y8 = y4 * y4
       y9 = y6 * y3 

! FSB Calculate integrals ove the lognormal distribution
!     this is done term by term and the form is
!     xn = yn * exp( (n**2) * lnsig2 /2.0d0)

       x  = y 
       x2 = y2 * exp( two              * lnsg2)
       x3 = y3 * exp( nine_two         * lnsg2)
       x4 = y4 ! * exp( eight            * lnsg2)      
       x6 = y6 ! * exp( thirtysix_two    * lnsg2)
       x7 = y7 ! * exp( fortynine_two    * lnsg2)
       x8 = y8 ! * exp( fortynine_two    * lnsg2)
       x9 = y9 ! * exp( eightyone_two    * lnsg2)

        
! FSB explicitly calculate complex refrative index m        
       m = dcmplx(n,-k)
! FSB get powers and functions of m        
       m2 = m * m
       m4 = m2 * m2
       m6 = m2 * m4
       m21 = m2 - one
       m22 = m2 + two

! FSB calculate Penndorf's definitions from Table II of Penndorf (1962a)        
       P = m21 / m22
       Q = (m2 - two ) / m22
       S = m21 / ( two * m2 + three)
       V = m21
! FSB get real & imaginary parts following Penndorf's mptation        
       P1 = real(P)
       P2 = -aimag(P)
       P1SQ = P1 * P1
       P2SQ = P2 * P2

       Q1 = real(Q)
       Q2 = -aimag(Q)
       S2 = -aimag(S)
       V1 = real(V)
       v2 = -aimag(V)


! FSB Get bext from Penndorf (1962a) Equation (7) up to x4 
!     consistent with equation (8)
!     We have then divided through by x and integrated analytically
       bext = REAL( four * P2 + ( 2.4d0 * (P1 * Q2 + P2 * Q1 ) +  twothrds * S2          &
            + twofifteenths * V2 ) * x2 + ( eightthirds * ( P1SQ - P2SQ ) ) * x3, 4 )

! FSB get bscat from Penndorf Equation (9) up to x4 
!     we have divided through by x and integrated analytically
       bscat = REAL( eightthirds * ( P1SQ + P2SQ ) * x3 )
! FSB calculate babs
!      babs = bext - bscat

! FSB now get asymmetry factor from Caldas & Semiao (2001)      
!    
! *** The following additional variables from Caldas & Semiao (2001)
!     are defined in Equations 10a to 10h.

       R = (m6 + 20.0d0*m4 -200.0d0*m2 + 200.0d0) / m22**2
       T = m21 / ( ( 2.0d0 * M2 + 3.0d0) **2 )
       U = m21 / (3.0d0 * M2 + 4.0d0 )
       W = m21 * ( 2.0d0 * m2 - 5.0d0)

! *** further definitions from Caldas & Semiao (2001)      
       Qprime = Q
       Rprime = 18.0d0 * R
       Sprime = 5.0d0 * S / P
       Tprime = 375.0d0 * T / P
!      Uprime = 28.0d0 * U / P
       Vprime = V / P
       Wprime = 5.0d0 * W / P

! FSB calculate gQs and Qs from Caldas & Semiao (2001)
! *** calculate Qs equation 13
!      Qs = eightthirds * abs(P)**2                                        &
!           * (x4 + onefifteenth * real(Qprime) * x6                       &
!           + fourthirds * aimag(P) * x7                                   &
!           + one_big * ( 35.0d0 * abs(Qprime)**2                          &
!           + 20.0d0 * real(Rprime) + 35.0d0 * abs(Vprime)**2              &
!           + 21.0d0 * abs(Sprime)**2 ) * x8                               &
!           + two_fortyfive * aimag( Qprime * ( P - conjg(P) )) * x9 ) 

! *** calculate gQs equation 15
      
!      gQs = four_225 * abs(P)**2 * (                                      &
!              (5.0d0 * Real(Vprime) + 3.0d0 * real(Sprime) ) * x6         &
!             + one_210 * ( 35.0d0 * real(Vprime*conjg(Qprime) )           &
!             + 21.0d0 * real(Sprime * conjg(Qprime) )                     &
!             + 10.0d0 * real(Wprime)- 6.0d0 * real(Tprime) ) * x8         &
!             - twothrds * ( 5.0d0 * aimag(Vprime * conjg(P) )             &
!             + 3.0d0 * aimag(Sprime * conjg(P) ) ) * x9    )

! FSB recast into specific terms 
       A = 1.0D0 * x4
       B = onefifteenth * real(Qprime) * x6 
       C = fourthirds * aimag(P) * x7
       D = one_big * ( 35.0d0 * abs(Qprime)**2                             &
           + 20.0d0 * real(Rprime) + 35.0d0 * abs(Vprime)**2               &
           + 21.0d0 * abs(Sprime)**2 ) * x8     
       E = two_fortyfive * aimag( Qprime * ( P - conjg(P) )) * x9   
       
       Qs = eightthirds * abs(P)**2 *( A + B + C + D + E )
       
       AA = (5.0d0 * Real(Vprime) + 3.0d0 * real(Sprime) ) * x6 
       BB = one_210 * ( 35.0d0 * real(Vprime*conjg(Qprime) )               &
            + 21.0d0 * real(Sprime * conjg(Qprime) )                       &
            + 10.0d0 * real(Wprime)- 6.0d0 * real(Tprime) ) * x8         
       CC = twothrds * ( 5.0d0 * aimag(Vprime * conjg(P) )                 &
            + 3.0d0 * aimag(Sprime * conjg(P) ) ) * x9    

       gQs = four_225 * abs(P)**2 * ( AA + BB + CC )
      
! FSB calculate asymmetry factor and adjust with empirical term.      
       g = REAL(gQs / Qs)
!  FSB now multiply by three_pi_two  get output  values        
       bext  = three_pi_two * bext  
       bscat = three_pi_two * bscat 
! FSB calculate babs
       babs = bext - bscat
        
       end subroutine pennfsb
        
! ------------------------------------------------------------------
       subroutine aero_optical2( lamda_in, crefin, Vol, dgn, &
                                 sig, bext, bscat, gfac, success )

! FSB NOTE: this subroutine calculates for single mode     
     
! *** calculate the extinction and scattering coefficients and
!     assymetry factors for each wavelength as a sum over the 
!     individual lognormal modes. Each mode may have a different 
!     set of refractive indices.

      IMPLICIT NONE

! *** input variables
      real, intent(in)    :: lamda_in   ! wavelengths  [micro-m]
      complex, intent(in) :: crefin     ! Complex refractive index 
      real, intent(in)    :: Vol        ! modal aerosol volumes [m**3 /m**3]
      real, intent(in)    :: dgn        ! geometric mean diameters 
                                        ! for number distribution [ m]
      real, intent(in)    :: sig        ! geometric standard deviation 
      
! *** output variables 
      real, intent(out)    :: bext         ! extinction coefficient [ 1 / m ]
      real, intent(out)    :: bscat        ! scattering coefficient [ 1 / m ]
      real, intent(out)    :: gfac         ! assymetry factor for Mie and molecular scattering
      logical, intent(out) :: success      ! flag for successful calculation

      
      
! *** internal variables
!     real :: xlnsig(nmode) ! natural log of geometric standard deviations      
      real :: beta_Sc       ! aerosol scattering coefficient 
 
      real :: beta_Ex       ! aerosol extinction coefficients       
      real :: G             ! modal aerosol assymetry factors
      real :: sum_g
      real :: LSIGX
      real :: lamdam1       ! 1/ lamda
      real :: alphav        ! Mie size parameter
      real :: vfac
      real, parameter :: pi = 3.14159265359

       Logical, Save :: Initialize = .True.
      
       
! FSB coded 04/15/2012 by Dr. Francis S. Binkowski
!     modified from an earlier version
!     Center for Environmental Modeling for PolicyDevelopment
!     Institute for the Environment
!     University of North Carolina at Chapel Hill
!     email: frank_binkowski@unc.edu

! *** initialize variables
       lamdam1 = 1.0e6 / lamda_in ! lamda now in [ m ]
       bext  = 0.0
       bscat = 0.0
       sum_g = 0.0
!      write(20,*) ' j = ', j
       LSIGX = log(sig)
       
!     calculate Mie size parameter for volume distribution
!     exp(3.0 * xlnsig*xlnsig)  converts dgn to dgv (volume diameter)
       alphav =  pi * dgn * exp(3.0 * LSIGX * LSIGX) * lamdam1
       
!      write(20,*) j, alphav, modalph, modulus(j)
!      write(20,*) j, alphav, modalph, NRX, NIX

       If(Initialize .And. AERO_UTIL_LOG .GT. 0 )Then
          If( Use_Odd_Quadrature )then
              write(AERO_UTIL_LOG,99501)Quadrature_Points
          else
              write(AERO_UTIL_LOG,99504)
              Initialize = .False.
          End If
       End If

       If( Use_Odd_Quadrature )then
           CALL ghintBH (Initialize, crefin, alphav, LSIGX, beta_EX, beta_Sc, G, success)
       Else
           CALL ghintBH (crefin, alphav, LSIGX, beta_EX, beta_Sc, G, success)
       End If
       

! *** ghintBH returns the normalized values
!     Calculate the actual extinction and scattering coefficients 
!     by multplying by the modal volume and dividing by the wavelength
         
         
       vfac  =  Vol * lamdam1         
       bext    = vfac * beta_Ex  ! [ 1 / m ]
       bscat   = vfac * beta_Sc  ! [ 1 / m ]
       gfac    = G
99501  Format(I2,' Quadrature Points for Volume Averaged Aerosol Optics')
99504  Format('Even Number Quadrature Points for Volume Averaged Aerosol Optics')
       
       END SUBROUTINE aero_optical2

! ------------------------------------------------------------------
       subroutine aero_optical_CS ( lamda_in, refcor,refshell, VOLCOR,   &
                                    VOLSHELL, DGNCOR, DGNSHELL, SIG,     &
                                    bext, bscat, gfac, succesS )
     
! FSB NOTE: values for one mode are returend      
! *** calculate the extinction and scattering coefficients and
!     assymetry factors for each wavelength as a sum over the 
!     individual lognormal modes. Each mode may have a different 
!     set of refractive indices.

       IMPLICIT NONE
! *** input variables
       real,intent(in)    :: lamda_in   ! wavelengths  [micro-m]                      
       complex,intent(in) :: refcor     ! Complex refractive index -core
       complex,intent(in) :: refshell   ! Complex refractive index -shell
       real,intent(in)    ::  VOLCOR    ! volume of core
       real,intent(in)    ::  VOLSHELL  ! volume of shell
       real,intent(in)    ::  DGNCOR    ! geometric mean diameters  
                                        ! for number distribution [m]
       real,intent(in)    ::  DGNSHELL  ! geometric mean diameters  
                                        ! for number distribution [m]
       real,intent(in)    ::  SIG       ! geometric standard deviation 
      
! *** output variables 
       real,intent(out)     ::  bext      ! extinction coefficient [ 1 / m ]
       real,intent(out)     ::  bscat     ! scattering coefficient [ 1 / m ]
       real,intent(out)     ::  gfac      ! assymetry factor 
       logical, intent(OUT) :: success    ! flag for successful calculation
      
! *** internal variables
!      real    :: xlnsig(nmode)    ! natural log of geometric standard deviations      
       real    :: beta_Sc          ! aerosol scattering coefficient 
 
       real    :: beta_Ex          ! aerosol extinction coefficients       
       real    :: G                ! modal aerosol assymetry factors
       real    :: LSIGX
       real    :: XX, YY           ! Mie size parameter
       real    :: expfac
       real    :: lamdam1          ! 1/ lamda
       real    :: vfac
       
       Logical, Save :: Initialize = .True.

       real, parameter :: pi = 3.14159265359
       
! FSB coded 04/15/2012 by Dr. Francis S. Binkowski
!     modified from an earlier version
!     Center for Environmental Modeling for PolicyDevelopment
!     Institute for the Environment
!     University of North Carolina at Chapel Hill
!     email: frank_binkowski@unc.edu


! *** initialize variables
       lamdam1 = 1.0e6 / lamda_in ! lamda now in [ m ]
        
!      write(20,*) ' inside aero_optical', ' lamda = ', lamda
      
!    calculate the extinction and scattering coefficients
       LSIGX  = log(SIG)
       expfac = pi * exp(3.0 * LSIGX * LSIGX) * lamdam1
        
!     calculate Mie size parameter for volume distribution
!     exp(3.0 * xlnsig*xlnsig)  converts dgn to dgv (volume diameter)
       XX =  DGNCOR * expfac                                       
       YY =  DGNSHELL * expfac
       
       If(Initialize .And. AERO_UTIL_LOG .GT. 0 )Then
          If( Use_Odd_Quadrature )then
              write(AERO_UTIL_LOG,99500)Quadrature_Points
          else
              write(AERO_UTIL_LOG,99502)
              Initialize = .False.
          End If
       End If
       
       If( Use_Odd_Quadrature )then
           CALL ghintBH_CS(Initialize,refcor,refshell,XX,YY,LSIGX,beta_EX,beta_Sc,G, success)
       Else
           CALL ghintBH_CS(refcor,refshell,XX,YY,LSIGX,beta_EX,beta_Sc,G, success)
       End If            

! FSB ghintBH_CS returns the normalized values
!     Calculate the actual extinction and scattering coefficients 
!     by multplying by the modal volume and dividing by the wavelength.
!     For the coated-sphere (core-shell) calculation use the combined
!     volume         
         
       vfac   =  (VOLCOR + VOLSHELL) * lamdam1         
       bext   = vfac * beta_Ex  ! [ 1 / m ]
       bscat  = vfac * beta_Sc  ! [ 1 / m ]
       gfac   = G
99500  Format(I2,' Quadrature Points for Core-Shell Aerosol Optics')
99502  Format('Even Number Quadrature Points for Core-Shell Aerosol Optics')
       END SUBROUTINE aero_optical_CS

! ------------------------------------------------------------------
       subroutine ghintBH_2 (crefin,alfv,xlnsig,Qext_GH,Qscat_GH,g_gh, success) 

! *************** REVISED VERSION < NOTE
! FSB *********** This is the newest (04_14_2012) version of GhintBH
!      this version does the Mie method and calculates the optimum set of 
!      set of Gauss-Hermite abscissas and weights. 
!      Dr. Francis S. Binkowski, The University of North Carolina
!                                at Chapel Hill
! FSB this code file now contains all of the necessary subroutines that 
!     are called to perform an integral of the Bohren and Huffman
!     Mie codes ( as updated by Prof. Bruce C. Drain of Princeton)
!       calculates the extinction and scattering coefficients 
!       normalized by wavelength and total particle volume
!       concentration for a log normal particle distribution 
!       with the logarithm of the geometric  standard deviation
!       given by xlnsig. The integral of the
!       asymmetry factor g is also calculated.
! FSB Change 12/20/2011 This code now has a choice of IGH based
!     upon alfv and nr. 
! FBB Changes Simplified code. Eliminated Penndorf code
!  *** Does Gauss-Hermite quadrature of Qext / alfa & Qscat / alfa
!      and asymmetry factor <cos> over log normal distribution using 
!      symmetric  points.
!
       implicit none

       complex, intent(in) :: crefin     ! complex index of refraction
       real, intent(in)    :: alfv       ! Mie parameter for dgv
       real, intent(in)    :: xlnsig     ! log of geometric  standard deviation
       real, intent(out)   :: Qext_GH    ! normalized extinction efficiency
       real, intent(out)   :: Qscat_GH   ! normalized scattering efficiency
       real, intent(out)   :: g_GH       ! asymmetry factor <cos>
       logical, intent(out) :: success   ! flag for successful calculation
       
       real    :: nr                 ! real part of  refractive index      
       real    :: aa1                ! see below for definition
       real    :: alfaip, alfaim     ! Mie parameters at abscissas
     
!  *** these are Qext/alfa and Qscat/alfv at the abscissas
       real    :: qalfip_e, qalfim_e ! extinction  
       real    :: qalfip_s, qalfim_s ! scattering
       real    :: gsalfp, gsalfm     ! scattering times asymmetry factor
       integer :: IGH                ! index for GH quadrature      

! FSB define parameters 
       real, parameter :: pi = 3.14159265
       real, parameter :: sqrtpi = 1.772454 
       real, parameter :: sqrtpi1 = 1.0 / sqrtpi 
       real, parameter :: sqrt2 = 1.414214 
       real, parameter :: three_pi_two = 3.0 * pi / 2.0 
       real, parameter :: const = three_pi_two * sqrtpi1 
       
       integer ::  i
       real    ::  sum_e,sum_s, xi,wxi,xf
       real    ::  sum_sg

! Gauss-Hermite abscissas and weights
! *** the following weights and abscissas are from Abramowitz
!     Stegun, Table 25.10 page 924
! FSB full precision from Table 25.10 

! FSB ten-point  - IGH = 5
       real, parameter :: ghxi_10(5) = (/ 0.342901327223705,     &
                                          1.036610829789514,     &
                                          1.756683649299882,     &
                                          2.532731674232790,     &
                                          3.436159118837738 /)

       real, parameter :: ghwi_10(5) = (/ 6.108626337353e-01,    &
                                          2.401386110823e-01,    &
                                          3.387439445548e-02,    &
                                          1.343645746781e-03,    &
                                          7.640432855233e-06 /)

! FSB six-point - IGH = 3
       real, parameter :: ghxi_6(3) = (/ 0.436077411927617,      &
                                         1.335849074013597,      &
                                         2.350604973674492 /)

       real, parameter :: ghwi_6(3) = (/ 7.246295952244e-01,     &
                                         1.570673203229e-01,     &
                                         4.530009905509e-03 /)

! FSB two-point - IGH = 1
       real, parameter :: ghxi_2(1) = (/ 0.707106781186548 /)

       real, parameter :: ghwi_2(1) = (/ 8.862269254528e-01 /)

       real    :: GHXI(5), GHWI(5) ! weight and abscissas
       integer :: NMAX             ! number of weights and abscissa


! start code
! FSB now choose IGH. These choices are designed to improve
!     the computational efficiency without sacrificing accuracy.


       nr = real(crefin)      

       IGH=3 ! default value; six_point is sufficient generally
! six point
       NMAX = 3

       if (nr .ge. 1.7) then 
! 10 point     
          IGH = 5 ! more points needed here
          NMAX = 5
       end if

       if( alfv .gt. 20.0 .or. alfv .lt. 0.5 ) then
          IGH  = 1 ! in  this range fewer points are needed
          NMAX = 1
       end if

       if (IGH == 1) then
! two point
          GHXI(1)    = ghxi_2(1)
          GHWI(1)    = ghwi_2(1)
       else if (IGH == 3) then
          do i = 1, NMAX
             GHXI(i) = ghxi_6(i)
             GHWI(i) = ghwi_6(i)
          end do 
       else
          do i = 1,NMAX
             GHXI(i) = ghxi_10(i)
             GHWI(i) = ghwi_10(i)
          end do  
       end if ! set up number of abscissas and weights 
      
! FSB now start the integration code
       aa1 = sqrt2 * xlnsig    ! This 1.0 / Sqrt( A ) in derivation of the integral
                               ! where A = 1.0 / ( 2.0 * xlnsg**2 ) 

! Then alpha = alfv * exp[ u / sqrt(A) ]
! For Gauss-Hermite Quadrature u = xi 
! Therefore, xf = exp( xi / sqrt(A) ),
!  or xf = exp( xi * aa1 ) 

       sum_e  = 0.0
       sum_s  = 0.0
       sum_sg = 0.0
! FSB do NMAX calls to the MIE codes      
       do i = 1,NMAX
          xi      = GHXI(i)
          wxi     = GHWI(i)
          xf      = exp( xi * aa1 )
          alfaip  = alfv * xf
          alfaim  = alfv / xf ! division cheaper than another exp()
! *** call subroutine to fetch the effficiencies

          call getqext_BH(alfaip,crefin,qalfip_e,qalfip_s, gsalfp, success)
          call getqext_BH(alfaim,crefin,qalfim_e,qalfim_s, gsalfm, success)

          sum_e  = sum_e + wxi  * ( qalfip_e + qalfim_e ) 
          sum_s  = sum_s + wxi  * ( qalfip_s + qalfim_s ) 
          sum_sg = sum_sg + wxi * ( gsalfp + gsalfm ) 

       end do 

       g_GH     = sum_sg / sum_s ! this is <cos>
       Qext_GH  = const * sum_e  ! 
       Qscat_GH = const * sum_s  

       end subroutine ghintBH_2

! ------------------------------------------------------------------
       subroutine ghintBH_CS_even (RCORE, RSHELL , XX, YY, xlnsig,  &                  
                              Qext_GH,Qscat_GH, g_gh, success)

! FSB code for coated-sphere (core-shell) version

! *************** REVISED VERSION < NOTE
! FSB *********** This is the newest (04_14_2012) version of ghintBH_CS
!      for the coated-sphere (core-shell) method using BHCOAT
!      this version does the Mie method and calculates the optimum set of 
!      set of Gauss-Hermite abscissas and weights. 
!      Dr. Francis S. Binkowski, The University of North Carolina
!                                at Chapel Hill
       
! FSB this code file now contains all of the necessary subroutines that 
!     are called to perform an integral of the Bohren and Huffman
!     Mie codes ( as updated by Prof. Bruce C. Drain of Princeton)
!       calculates the extinction and scattering coefficients 
!       normalized by wavelength and total particle volume
!       concentration for a log normal particle distribution 
!       with the logarithm of the geometric  standard deviation
!       given by xlnsig. The integral of the
!       asymmetry factor g is also calculated.
! FSB Change 12/20/2011 This code now has a choice of IGH based
!     upon alfv and nr. 
! FBB Changes Simplified code. Eliminated Penndorf code
!  *** Does Gauss-Hermite quadrature of Qext / alfa & Qscat / alfa
!      and asymmetry factor <cos> over log normal distribution using 
!      symmetric  points.
!
       implicit none
       complex, intent(in) :: RCORE      ! refractive index of core
       complex, intent(in) :: RSHELL     ! refractive index of shell
       real, intent(in)    :: XX         ! Mie parameter for core
       real, intent(in)    :: YY         ! Mie parameter for shell
       real, intent(in)    :: xlnsig     ! log of geometric  standard deviation
       real, intent(out)   :: Qext_GH    ! normalized extinction efficiency
       real, intent(out)   :: Qscat_GH   ! normalized scattering efficiency
       real, intent(out)   :: g_GH       ! asymmetry factor <cos>
       logical, intent(out) :: success   ! flag for successful calculation

       real    :: nr                     ! real part of  refractive index      
       real    :: aa1                    ! see below for definition
       real    :: XXP, XXM               ! Mie parameters at abscissas - CORE
       real    :: YYP, YYM               ! Mie parameters at abscissas - SHELL
     
! FSB define parameters 
      real, parameter :: pi = 3.14159265
      real, parameter :: sqrtpi = 1.772454 
      real, parameter :: sqrtpi1 = 1.0 / sqrtpi 
      real, parameter :: sqrt2 = 1.414214 
      real, parameter :: three_pi_two = 3.0 * pi / 2.0 
      real, parameter ::  const = three_pi_two * sqrtpi1 
 
!  *** these are Qext/alfa and Qscat/alfv at the abscissas
       real    :: qalfip_e, qalfim_e     ! extinction  
       real    :: qalfip_s, qalfim_s     ! scattering
       real    :: gsalfp, gsalfm         ! scattering times asymmetry factor
       integer :: IGH                    ! index for GH quadrature      
       integer ::  i
       real    ::  sum_e,sum_s, xi,wxi,xf, temp
       real    ::  sum_sg

! Gauss-Hermite abscissas and weights
! *** the following weights and abscissas are from Abramowitz
!     Stegun, Table 25.10 page 924
! FSB full precision from Table 25.10 

! FSB ten-point  - IGH = 5
       real, parameter :: ghxi_10(5) = (/ 0.342901327223705,     &
                                          1.036610829789514,     &
                                          1.756683649299882,     &
                                          2.532731674232790,     &
                                          3.436159118837738 /)

       real, parameter :: ghwi_10(5) = (/ 6.108626337353e-01,    &
                                          2.401386110823e-01,    &
                                          3.387439445548e-02,    &
                                          1.343645746781e-03,    &
                                          7.640432855233e-06 /)

! FSB six-point - IGH = 3
       real, parameter :: ghxi_6(3) = (/ 0.436077411927617,      &
                                         1.335849074013597,      &
                                         2.350604973674492 /)

       real, parameter :: ghwi_6(3) = (/ 7.246295952244e-01,     &
                                         1.570673203229e-01,     &
                                         4.530009905509e-03 /)

! FSB two-point - IGH = 1
       real, parameter :: ghxi_2(1) = (/ 0.707106781186548 /)

       real, parameter :: ghwi_2(1) = (/ 8.862269254528e-01 /)

       real GHXI(5), GHWI(5) ! weight and abscissas
       integer NMAX ! number of weights and abscissa

! start code
! FSB now choose IGH. These choices are designed to improve
!     the computational efficiency without sacrificing accuracy.


       nr = real(RSHELL)      

       IGH=3 ! default value; six_point is sufficient generally
! six point
       NMAX = 3

       if (nr .ge. 1.7) then 
! 10 point     
          IGH = 5 ! more points needed here
          NMAX = 5
       end if

       if ( XX .gt. 20.0 .or. XX .lt. 0.5 ) then
          IGH  = 1 ! in  this range fewer points are needed
          NMAX = 1
       end if

       if (IGH == 1) then
! two point
          GHXI(1)    = ghxi_2(1)
          GHWI(1)    = ghwi_2(1)
       else if (IGH == 3) then
          do i = 1, NMAX
             GHXI(i) = ghxi_6(i)
             GHWI(i) = ghwi_6(i)
          end do 
       else
          do i = 1,NMAX
             GHXI(i) = ghxi_10(i)
             GHWI(i) = ghwi_10(i)
          end do  
       end if ! set up number of abscissas and weights 

! FSB now start the integration code
       aa1 = sqrt2 * xlnsig   ! This 1.0 / Sqrt( A ) in derivation of the integral
                              ! where A = 1.0 / ( 2.0 * xlnsg**2 ) 

! Then alpha = alfv * exp[ u / sqrt(A) ]
! For Gauss-Hermite Quadrature u = xi 
! Therefore, xf = exp( xi / sqrt(A) ),
!  or xf = exp( xi * aa1 ) 
       sum_e  = 0.0
       sum_s  = 0.0
       sum_sg = 0.0
! FSB do NMAX calls to the MIE codes      
       do i = 1,NMAX
          xi      = GHXI(i)
          wxi     = GHWI(i)
          xf      = exp( xi * aa1 )
          temp    = 1.0 / xf
          XXP     = XX * xf
          XXM     = XX * temp ! division cheaper than another exp()
          YYP     = YY * xf
          YYM     = YY * temp ! division cheaper than another exp()
! *** call subroutine to fetch the effficiencies

          call getqsgBHCS(XXP,YYP,RCORE,RSHELL,qalfip_e,qalfip_s,gsalfp, success)
          call getqsgBHCS(XXM,YYM,RCORE,RSHELL,qalfim_e,qalfim_s,gsalfm, success)
          
          sum_e  = sum_e  + wxi  * ( qalfip_e + qalfim_e ) 
          sum_s  = sum_s  + wxi  * ( qalfip_s + qalfim_s ) 
          sum_sg = sum_sg + wxi  * ( gsalfp   + gsalfm   ) 
       end do 

       g_GH     = sum_sg / sum_s ! this is <cos>
       Qext_GH  = const * sum_e  ! 
       Qscat_GH = const * sum_s  

       end subroutine ghintBH_CS_even
      
! ------------------------------------------------------------------
       subroutine getqsgBHCS (XX,YY,RRFRL1,RRFRL2,qxtalf,qscalf,qsgalf, success)
       implicit none

       real, intent(in)    :: XX, YY
       real, intent(out)   :: qxtalf, qscalf, qsgalf
       complex, intent(in) :: RRFRL1,RRFRL2            ! refractive indices Core , Shell 
       logical, intent(out) :: success                 ! flag for successful calculation

       real    :: QEXT, QSCA, QBACK, G_MIE
       real    :: xx1
       character (len = 20) :: mystr1, mystr2, mystr3, mystr4

       xx1    = 1.0 / YY

!      if (     (xx *  real(RRFRL1) >= 30.0)   &
!          .or. (xx * aimag(RRFRL1) >= 30.0)   &
!          .or. (yy * aimag(RRFRL2) >= 30.0)) then
!         print *, ' ==d== bhcoat error'
!      end if

       call BHCOAT (XX,YY,RRFRL1,RRFRL2,QEXT,QSCA,QBACK,G_MIE, SUCCESS)

!      write (mystr1, *) QEXT
!      write (mystr2, *) QSCA
!      write (mystr3, *) QBACK
!      write (mystr4, *) G_MIE

!      if ((trim(mystr1) == ' NaN') .or.  &
!          (trim(mystr2) == ' NaN') .or.  &
!          (trim(mystr3) == ' NaN') .or.  &
!          (trim(mystr4) == ' NaN')) then
!          call BHCOAT (XX,YY,RRFRL1,RRFRL2,QEXT,QSCA,QBACK,G_MIE)
!      end if

       qxtalf = QEXT * xx1
       qscalf = QSCA * xx1
       qsgalf = qscalf * G_MIE 

       END subroutine getqsgBHCS


! ------------------------------------------------------------------
      SUBROUTINE BHCOAT (XX, YY, RRFRL1, RRFRL2, QQEXT, QQSCA, QBACK, GGSCA, SUCCESS)
      
      use complex_number_module

      implicit none ! added by FSB

! Arguments:
       real, intent(in)    :: XX,YY             ! Defined below
       complex, intent(in) :: RRFRL1,RRFRL2     ! Defined below
       real, intent(out)   :: QQEXT,QQSCA,QBACK ! Defined below
       real, intent(out)   :: GGSCA             ! asymmetry factor <cos> added by FSB
       logical,intent(out) :: success

! Local variables:
     
       real*8, parameter     :: DEL = 1.0D-08  
       real*8, parameter     :: ONE = 1.0D0, TWO = 2.0D0 
!      complex*16, save :: II
!      data II/(0.D0,1.D0)/
       type(complex_number) :: II

       integer :: IFLAG,N,NSTOP

       character (len = 400) :: mystr

!         -----------------------------------------------------------
!              del is the inner sphere convergence criterion
!         -----------------------------------------------------------
     
       real*8 :: CHI0Y,CHI1Y,CHIY,PSI0Y,PSI1Y,PSIY,QEXT,RN,QSCA,X,Y,YSTOP,GSCA
       real*8 :: TWO_N_M_ONE, TWO_N_P_ONE
       real*8 :: RY, RYY, RNRY, RN1, factor
       
!      complex*16 :: AMESS1,AMESS2,AMESS3,AMESS4,AN,ANCAP,AN1, BN,BNCAP,BN1, BRACK,   &
       type(complex_number) :: AMESS1,AMESS2,AMESS3,AMESS4,AN,ANCAP,AN1, BN,BNCAP,BN1, BRACK,   &
                     CHI0X2,CHI0Y2,CHI1X2,CHI1Y2,CHIX2,CHIPX2,CHIPY2,CHIY2,CRACK,     &
                     D0X1,D0X2,D0Y2,D1X1,D1X2,D1Y2,DNBAR,GNBAR,                       &
                     REFREL,RFREL1,RFREL2, XBACK,XI0Y,XI1Y,XIY,                       &
                     X1,X2,Y2,RCX1, RCX2,RCY2, FAC1, FAC2

!***********************************************************************
! NOTES from Prof. Bruce T. Draine, Princeton University
! Subroutine BHCOAT calculates Q_ext, Q_sca, Q_back for coated sphere.
! All bessel functions computed by upward recurrence.
! Input:
!        XX = 2*PI*RCORE*REFMED/WAVEL
!        YY = 2*PI*RMANT*REFMED/WAVEL
!        RFREL1 = REFCOR/REFMED
!        RFREL2 = REFMAN/REFMED 
! where  REFCOR = complex refr.index of core)
!        REFMAN = complex refr.index of mantle)
!        REFMED = real refr.index of medium)
!        RCORE = radius of core
!        RMANT = radius of mantle
!        WAVEL = wavelength of light in ambient medium
!
! Routine BHCOAT is taken from Bohren & Huffman (1983)
! Obtained from C.L.Joseph
!
! History:
! 92/11/24 (BTD) Explicit declaration of all variables
! April 30,2012 (FSB) added additional code to optimize
!  run time by finding common terms and replacing multiple
!  divisions by multiplication by a reciprocal. 
! April 09, 2012  code transferred from BTD's BMHMIE to
! calculate the asymmetry factor by  Prof. Francis S. Binkowski of 
! The University of North Carolina at Chapel Hill.
! April 30,2012 (FSB) added additional code to optimize
!  run time by finding common terms and replacing multiple
!  divisions by multiplication by a reciprocal. 
! July 16, 2010  more optimization by Dr. David Wong (DW) at US EPA
 
! REFERENCE: 
!  Bohren, Craig F. and Donald R. Huffman, Absorption and 
!    Scattering of Light by Small Particles, Wiley-Interscience
!    copyright 1983. Paperback Published 1998.
!    This code was originally listed in Appendix B. pp 483-489.
!    As noted above , the original code was subsequently 
!    modified by Prof. Bruce T. Draine of Princeton University.
! 
! FSB The background for this code is discussed in Borhen & Huffman (1983)
! on pages 181-183 ( Equations 8.2 ) and on pages 483-484. 
!***********************************************************************
!
! Start Code 

       SUCCESS = .TRUE.      

       II = c_set(0.0D0, 1.0D0)

! this technique will make the second 4 byte in the 8 byte variable be 0
! rather than arbitrary digits to increase accuracy
       write (mystr, *) xx, yy, real(RRFRL1), aimag(RRFRL1), real(RRFRL2), aimag(RRFRL2)
       read  (mystr, *) x,  y,  RFREL1, RFREL2

!      X      = XX
!      Y      = YY
       RY     = ONE / Y
       RYY    = RY * RY
!      RFREL1%real_part = real(RRFRL1)
!      RFREL1%imag_part = aimag(RRFRL1)
!      RFREL2%real_part = real(RRFRL2)
!      RFREL2%imag_part = aimag(RRFRL2)
       x1     = c_mul(x, rfrel1)
       x2     = c_mul(x, rfrel2)
       y2     = c_mul(y, rfrel2)
       RCX1   = c_div(ONE, X1)
       RCX2   = c_div(ONE, X2)
       RCY2   = c_div(ONE, Y2)
       refrel = c_div(rfrel2, rfrel1)
       ystop  = y + 4.0 * y**0.3333 + 2.0
       nstop  = INT( ystop )

!         -----------------------------------------------------------
!              series terminated after nstop terms
!         -----------------------------------------------------------

!   initialize variables 
       d0x1   = c_div(c_cos(x1), c_sin(x1))
       d0x2   = c_div(c_cos(x2), c_sin(x2))
       d0y2   = c_div(c_cos(y2), c_sin(y2))

       psi0y  = cos(y)
       psi1y  = sin(y)
       chi0y  = -sin(y)
       chi1y  = cos(y)

       xi0y   = c_sub(psi0y, c_mul(chi0y, II))
       xi1y   = c_sub(psi1y, c_mul(chi1y, II))

       chi0y2 = c_mul(-1.0d0, c_SIN(y2))
       chi1y2 = c_COS(y2)
       chi0x2 = c_mul(-1.0d0, c_SIN(x2))
       chi1x2 = c_COS(x2)
       qsca   = 0.0d0
       qext   = 0.0d0
       GSCA   = 0.0d0
       xback  = c_set(0.0d0, 0.0d0)
       iflag  = 0
       factor = 1.0d0

!      print *, ' ==d== N ', nstop

! FSB Start main loop      
       DO n = 1, nstop
          rn = REAL( n, 8 )
          RN1 = ONE / RN
          TWO_N_M_ONE = TWO * RN - ONE
          TWO_N_P_ONE = TWO * RN + ONE
          psiy = (TWO_N_M_ONE)*psi1y*RY - psi0y
          chiy = (TWO_N_M_ONE)*chi1y*RY - chi0y
          xiy  = c_sub(psiy, c_mul(chiy, II))
          d1y2 = c_sub(c_div(ONE, c_sub(c_mul(rn, RCY2), d0y2)), c_mul(rn, RCY2))

          IF (iflag .eq. 0) THEN
! *** Calculate inner sphere ancap, bncap
!      and brack and crack
             d1x1   = c_sub(c_div(ONE, c_sub(c_mul(rn, RCX1), d0x1)), c_mul(rn, RCX1))
             d1x2   = c_sub(c_div(ONE, c_sub(c_mul(rn, RCX2), d0x2)), c_mul(rn, RCX2))

             chix2  = c_sub(c_mul(c_mul(TWO*rn - ONE, chi1x2), RCX2), chi0x2)
             chiy2  = c_sub(c_mul(c_mul(TWO*rn - ONE, chi1y2), RCY2), chi0y2)

             chipx2 = c_sub(chi1x2, c_mul(c_mul(rn, chix2), RCX2))
             chipy2 = c_sub(chi1y2, c_mul(c_mul(rn, chiy2), RCY2))

!            ANCAP  = (REFREL*D1X1 - D1X2) /                              & 
!                     ( (REFREL*D1X1*CHIX2 - CHIPX2) * (CHIX2*D1X2 - CHIPX2) )

             ANCAP = c_sub(c_mul(c_mul(REFREL, D1X1), CHIX2), CHIPX2)
             ANCAP = c_mul(ANCAP, c_sub(c_mul(CHIX2, D1X2), CHIPX2))
             ANCAP = c_div(c_sub(c_mul(REFREL, D1X1), D1X2), ANCAP)

             brack  = c_mul(ancap, c_sub(c_mul(chiy2, d1y2), chipy2))

             bncap  = c_sub(c_mul(refrel, d1x2), d1x1)
             bncap  = c_div(bncap, c_sub(c_mul(refrel, chipx2), c_mul(d1x1, chix2)))
             bncap  = c_div(bncap, c_sub(c_mul(chix2, d1x2), chipx2))

             crack  = c_mul(bncap, c_sub(c_mul(chiy2, d1y2), chipy2))
! *** calculate convergence test expressions
!     for inner sphere.
! *** see pages 483-485 of Bohren & Huffman for
!     definitions. 
             amess1 = c_mul(brack, chipy2)
             amess2 = c_mul(brack, chiy2)
             amess3 = c_mul(crack, chipy2)
             amess4 = c_mul(crack, chiy2)

! Now test for convergence for inner sphere
!  All four criteria must be satisfied. See page 484 of B & H
             IF (c_ABS(amess1) .LE. del*c_ABS(d1y2)  .AND.                          &
                (c_ABS(amess2) .LE. del)             .AND.                          &
                (c_ABS(amess3) .LE. del*c_ABS(d1y2)) .AND.                          &
                (c_ABS(amess4) .LE. del)                ) THEN
!               convergence for inner sphere        
                brack = c_set(0.0D0,0.0D0)
                crack = c_set(0.0D0,0.0D0)
                iflag = 1
!         ELSE
! no convergence yet
!            iflag = 0
             END IF 
          END IF ! test on iflag .eq. 0

! *** note usage of brack and crack See equations on
!     Page 485  and discussion on pages 486 -487 of B & H      
          dnbar = c_sub(d1y2, c_mul(brack, chipy2))
          dnbar = c_div(dnbar, c_sub(ONE, c_mul(brack, chiy2)))
          gnbar = c_sub(d1y2, c_mul(crack, chipy2))
          gnbar = c_div(gnbar, c_sub(ONE, c_mul(crack, chiy2)))
!*** Store previous values of an and bn for use 
!    in computation of g=<cos(theta)>
          IF (N .GT. 1) THEN
             AN1 = an
             BN1 = bn
          END IF    
! *** update an and bn  
          RNRY = rn * RY 
          FAC1 = c_add(c_div(dnbar, rfrel2), RNRY)

          an = c_sub(c_mul(psiy, FAC1), psi1y)
          an = c_div(an, c_sub(c_mul(FAC1, xiy), xi1y))
          FAC2 = c_add(c_mul(rfrel2, gnbar), RNRY)
          bn = c_sub(c_mul(psiy, FAC2), psi1y)
          bn = c_div(bn, c_sub(c_mul(FAC2, xiy), xi1y))
      
! *** Calculate sums for qsca, qext, xback      
          qsca  = qsca + (TWO_N_P_ONE) * (c_ABS(an)**2 + c_ABS(bn)**2)
      
          qext  = qext + TWO_N_P_ONE * (an%real_part + bn%real_part)
      
! DW        XBACK = XBACK +  (TWO_N_P_ONE) * (-1.)**N * (AN-BN)
          FACTOR = FACTOR * (-1.0D0)
          XBACK = c_add(XBACK, c_mul(TWO_N_P_ONE * FACTOR, c_sub(AN, BN)))

! FSB calculate the sum for the asymmetry factor 

          GSCA = GSCA + ((TWO_N_P_ONE)/(RN* (RN + ONE)))*                     &
                 (an%real_part*bn%real_part + an%imag_part*bn%imag_part)
 
          IF (n .GT. 1) THEN
        
! DW         GSCA = GSCA + ((RN - ONE) * (RN + ONE) * RN1) *             &
             GSCA = GSCA + (RN - RN1) *                                  &
                   (AN1%real_part*AN%real_part + AN1%imag_part*AN%imag_part +            &
                    BN1%real_part*BN%real_part + BN1%imag_part*BN%imag_part)
     
          END IF
! continue update for next interation        
          psi0y  = psi1y
          psi1y  = psiy
          chi0y  = chi1y
          chi1y  = chiy
          xi1y   = c_sub(psi1y, c_mul(chi1y, II))
          chi0x2 = chi1x2
          chi1x2 = chix2
          chi0y2 = chi1y2
          chi1y2 = chiy2
          d0x1   = d1x1
          d0x2   = d1x2
          d0y2   = d1y2
       END DO  ! end of main loop 
  
!*** Have summed sufficient terms.
!    Now compute QQSCA,QQEXT,QBACK,and GSCA
       GGSCA = REAL( TWO * GSCA / qsca )
       QQSCA = REAL( TWO * qsca * RYY )
       QQEXT = REAL( TWO * qext * RYY )
!      QBACK = 0.5 * REAL ( ( xback * conjg(xback) ) * RYY )

       QBACK = 0.5 * real((xback%real_part**2 + xback%imag_part**2) * RYY)
!      QBACK = real((xback%real_part**2 + xback%imag_part**2) * RYY)

!       write (6, '(a19, 20e18.10)') ' ==d== bhcoat z ', GGSCA, GSCA, qsca
!       write (6, '(a19, 20e18.10)') ' ==d== bhcoat z ', QQSCA, qsca, RYY
!       write (6, '(a19, 20e18.10)') ' ==d== bhcoat z ', QQEXT, qext, RYY
!       write (6, '(a19, 20e18.10)') ' ==d== bhcoat z ', QBACK, xback, RYY

       end subroutine BHCOAT

       subroutine ghintBH_Odd (INIT, crefin,alfv,xlnsig,Qext_GH,Qscat_GH,g_gh, success ) 

! *************** REVISED VERSION < NOTE
! FSB *********** This is the newest (04_14_2012) version of GhintBH
!      this version does the Mie method and calculates the optimum set of 
!      set of Gauss-Hermite abscissas and weights. 
!      Dr. Francis S. Binkowski, The University of North Carolina
!                                at Chapel Hill
! FSB this code file now contains all of the necessary subroutines that 
!     are called to perform an integral of the Bohren and Huffman
!     Mie codes ( as updated by Prof. Bruce C. Drain of Princeton)
!       calculates the extinction and scattering coefficients 
!       normalized by wavelength and total particle volume
!       concentration for a log normal particle distribution 
!       with the logarithm of the geometric  standard deviation
!       given by xlnsig. The integral of the
!       asymmetry factor g is also calculated.
! FSB Change 12/20/2011 This code now has a choice of IGH based
!     upon alfv and nr. 
! FBB Changes Simplified code. Eliminated Penndorf code
!  *** Does Gauss-Hermite quadrature of Qext / alfa & Qscat / alfa
!      and asymmetry factor <cos> over log normal distribution using 
!      symmetric  points.
!
       implicit none

       logical, intent(INOUT)        :: INIT       ! initialize number of qudraure points
       complex, intent(in)           :: crefin     ! complex index of refraction
       real, intent(in)              :: alfv       ! Mie parameter for dgv
       real, intent(in)              :: xlnsig     ! log of geometric  standard deviation
       real, intent(out)             :: Qext_GH    ! normalized extinction efficiency
       real, intent(out)             :: Qscat_GH   ! normalized scattering efficiency
       real, intent(out)             :: g_GH       ! asymmetry factor <cos>
       logical, intent(out)          :: success    ! flag for successful calculation
       
       real    :: nr                 ! real part of  refractive index      
       real    :: aa1                ! see below for definition
       real    :: alfaip, alfaim     ! Mie parameters at abscissas
     
!  *** these are Qext/alfa and Qscat/alfv at the abscissas
       real    :: qalfip_e, qalfim_e ! extinction  
       real    :: qalfip_s, qalfim_s ! scattering
       real    :: gsalfp, gsalfm     ! scattering times asymmetry factor

! FSB define parameters 
       real, parameter :: pi = 3.14159265
       real, parameter :: sqrtpi = 1.772454 
       real, parameter :: sqrtpi1 = 1.0 / sqrtpi 
       real, parameter :: sqrt2 = 1.414214 
       real, parameter :: three_pi_two = 3.0 * pi / 2.0 
       real, parameter :: const = three_pi_two * sqrtpi1 
       
       integer ::  i
       real    ::  sum_e,sum_s, xi,wxi,xf
       real    ::  sum_sg

       real,    allocatable,  save  :: GHXI(:), GHWI(:) ! weight and abscissas
       integer, save  :: IGH                            ! number of weights and abscissa
       integer, save  :: NMAX                           ! optimumized number of weights and abscissa


! start code
! FSB now choose IGH. These choices are designed to improve
!     the computational efficiency without sacrificing accuracy.

     If( INIT )Then

          Select Case( Quadrature_Points )
            Case( 1,3,9 )
              IGH = Quadrature_Points
            Case Default
              IGH = 3
          End Select

          NMAX = Max( Int( IGH / 2 ), 0)

          If( Allocated( GHXI ) .Or. Allocated( GHWI ) )Then
              Success = .False.
              Return             
          End If
          
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
            Case ( 9 )
              do i = 1, NMAX + 1
                GHXI(i) = ghxi_9(i)
                GHWI(i) = ghwi_9(i)
              end do 
          end select 
          
          If( AERO_UTIL_LOG .GT. 0 )Then
              write(AERO_UTIL_LOG,*)'BHMIE: IGH,(NMAX + 1) = ',IGH,(NMAX + 1)
              do i = 1, NMAX + 1
                write(AERO_UTIL_LOG,*)'BHMIE: i, GHXI(i), GHWI(i) = ',i, GHXI(i), GHWI(i)
              end do
          End If
          
          INIT = .False.
       Else
          If( .Not. Allocated( GHXI ) .Or. .Not. Allocated( GHWI ) )Then
              Success = .False.
              Return             
          End If                
       End If ! set up number of abscissas and weights 
 
       nr = real(crefin)      

      
! FSB now start the integration code
       aa1 = sqrt2 * xlnsig    ! This 1.0 / Sqrt( A ) in derivation of the integral
                               ! where A = 1.0 / ( 2.0 * xlnsg**2 ) 

! Then alpha = alfv * exp[ u / sqrt(A) ]
! For Gauss-Hermite Quadrature u = xi 
! Therefore, xf = exp( xi / sqrt(A) ),
!  or xf = exp( xi * aa1 ) 

!start integration at zero point
       xi      = 0.0
       wxi     = GHWI(NMAX+1)
       xf      = 1.0
       alfaip  = alfv
! fetch the effficiencies at zero point

       call getqext_BH(alfaip,crefin,qalfip_e,qalfip_s, gsalfp, success)

       sum_e  = wxi * qalfip_e
       sum_s  = wxi * qalfip_s
       sum_sg = wxi * gsalfp

! FSB do NMAX calls to the MIE codes      
       do i = 1, NMAX
          xi      = GHXI(i)
          wxi     = GHWI(i)
          xf      = exp( xi * aa1 )
          alfaip  = alfv * xf
          alfaim  = alfv / xf ! division cheaper than another exp()
! *** call subroutine to fetch the effficiencies

          call getqext_BH(alfaip,crefin,qalfip_e,qalfip_s, gsalfp, success)
          call getqext_BH(alfaim,crefin,qalfim_e,qalfim_s, gsalfm, success)

          sum_e  = sum_e + wxi  * ( qalfip_e + qalfim_e ) 
          sum_s  = sum_s + wxi  * ( qalfip_s + qalfim_s ) 
          sum_sg = sum_sg + wxi * ( gsalfp + gsalfm ) 

       end do
       

       g_GH     = sum_sg / sum_s ! this is <cos>
       Qext_GH  = const * sum_e  ! 
       Qscat_GH = const * sum_s  

       end subroutine ghintBH_Odd
! ------------------------------------------------------------------
       subroutine ghintBH_CS_Odd (INIT, RCORE, RSHELL , XX, YY, xlnsig,  &                  
                                  Qext_GH,Qscat_GH, g_gh, success)

! FSB code for coated-sphere (core-shell) version

! *************** REVISED VERSION < NOTE
! FSB *********** This is the newest (04_14_2012) version of ghintBH_CS
!      for the coated-sphere (core-shell) method using BHCOAT
!      this version does the Mie method and calculates the optimum set of 
!      set of Gauss-Hermite abscissas and weights. 
!      Dr. Francis S. Binkowski, The University of North Carolina
!                                at Chapel Hill
       
! FSB this code file now contains all of the necessary subroutines that 
!     are called to perform an integral of the Bohren and Huffman
!     Mie codes ( as updated by Prof. Bruce C. Drain of Princeton)
!       calculates the extinction and scattering coefficients 
!       normalized by wavelength and total particle volume
!       concentration for a log normal particle distribution 
!       with the logarithm of the geometric  standard deviation
!       given by xlnsig. The integral of the
!       asymmetry factor g is also calculated.
! FSB Change 12/20/2011 This code now has a choice of IGH based
!     upon alfv and nr. 
! FBB Changes Simplified code. Eliminated Penndorf code
!  *** Does Gauss-Hermite quadrature of Qext / alfa & Qscat / alfa
!      and asymmetry factor <cos> over log normal distribution using 
!      symmetric  points.
!
       implicit none

       logical, intent(inout) :: INIT       ! initialize number of qudraure points
       complex, intent(in)    :: RCORE      ! refractive index of core
       complex, intent(in)    :: RSHELL     ! refractive index of shell
       real, intent(in)       :: XX         ! Mie parameter for core
       real, intent(in)       :: YY         ! Mie parameter for shell
       real, intent(in)       :: xlnsig     ! log of geometric  standard deviation
       real, intent(out)      :: Qext_GH    ! normalized extinction efficiency
       real, intent(out)      :: Qscat_GH   ! normalized scattering efficiency
       real, intent(out)      :: g_GH       ! asymmetry factor <cos>
       logical, intent(out)   :: success   ! flag for successful calculation

       real    :: nr                     ! real part of  refractive index      
       real    :: aa1                    ! see below for definition
       real    :: XXP, XXM               ! Mie parameters at abscissas - CORE
       real    :: YYP, YYM               ! Mie parameters at abscissas - SHELL
     
! FSB define parameters 
      real, parameter :: pi = 3.14159265
      real, parameter :: sqrtpi = 1.772454 
      real, parameter :: sqrtpi1 = 1.0 / sqrtpi 
      real, parameter :: sqrt2 = 1.414214 
      real, parameter :: three_pi_two = 3.0 * pi / 2.0 
      real, parameter ::  const = three_pi_two * sqrtpi1 
 
!  *** these are Qext/alfa and Qscat/alfv at the abscissas
       real    :: qalfip_e, qalfim_e     ! extinction  
       real    :: qalfip_s, qalfim_s     ! scattering
       real    :: gsalfp, gsalfm         ! scattering times asymmetry factor
       integer ::  i
       real    ::  sum_e,sum_s, xi,wxi,xf, temp
       real    ::  sum_sg


       real,    allocatable,  save  :: GHXI(:), GHWI(:) ! weight and abscissas
       integer,               save  :: IGH              ! number of weights and abscissa
       integer,               save  :: NMAX             ! optimized number of weights and abscissa

! start code
! FSB now choose IGH. These choices are designed to improve
!     the computational efficiency without sacrificing accuracy.

     If( INIT )Then

          Select Case( Quadrature_Points )
            Case( 1,3,9 )
              IGH = Quadrature_Points
            Case Default
              IGH = 3
          End Select
                    
          If( Allocated( GHXI ) .Or. Allocated( GHWI ) )Then
              Success = .False.
              Return             
          End If

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
            Case ( 9 )
              do i = 1, NMAX + 1
                GHXI(i) = ghxi_9(i)
                GHWI(i) = ghwi_9(i)
              end do  
          end select 

          If( AERO_UTIL_LOG .GT. 0 )Then
              write(AERO_UTIL_LOG,*)'BHCoat: IGH,(NMAX + 1) = ',IGH,(NMAX + 1)
              do i = 1, NMAX + 1
                write(AERO_UTIL_LOG,*)'BHCoat: i, GHXI(i), GHWI(i) = ',i, GHXI(i), GHWI(i)
              end do
          End If
          
          INIT = .False.
          
       Else
          If( .Not. Allocated( GHXI ) .Or. .Not. Allocated( GHWI ) )Then
              Success = .False.
              Return             
          End If      
       End If ! set up number of abscissas and weights 

       nr = real(RSHELL)      

! FSB now start the integration code
       aa1 = sqrt2 * xlnsig   ! This 1.0 / Sqrt( A ) in derivation of the integral
                              ! where A = 1.0 / ( 2.0 * xlnsg**2 ) 

! Then alpha = alfv * exp[ u / sqrt(A) ]
! For Gauss-Hermite Quadrature u = xi 
! Therefore, xf = exp( xi / sqrt(A) ),
!  or xf = exp( xi * aa1 ) 

!start integration at zero point

          xi      = 0.0
          wxi     = GHWI(NMAX+1)
          xf      = 1.0
          XXP     = XX
          YYP     = YY

! fetch the effficiencies at zero point

          call getqsgBHCS(XXP,YYP,RCORE,RSHELL,qalfip_e,qalfip_s,gsalfp, success)
          
          sum_e  = wxi  * qalfip_e
          sum_s  = wxi  * qalfip_s
          sum_sg = wxi  * gsalfp   

! FSB do NMAX calls to the MIE codes      
       do i = 1, NMAX
          xi      = GHXI(i)
          wxi     = GHWI(i)
          xf      = exp( xi * aa1 )
          temp    = 1.0 / xf
          XXP     = XX * xf
          XXM     = XX * temp ! division cheaper than another exp()
          YYP     = YY * xf
          YYM     = YY * temp ! division cheaper than another exp()
! *** call subroutine to fetch the effficiencies

          call getqsgBHCS(XXP,YYP,RCORE,RSHELL,qalfip_e,qalfip_s,gsalfp, success)
          call getqsgBHCS(XXM,YYM,RCORE,RSHELL,qalfim_e,qalfim_s,gsalfm, success)
          
          sum_e  = sum_e  + wxi  * ( qalfip_e + qalfim_e ) 
          sum_s  = sum_s  + wxi  * ( qalfip_s + qalfim_s ) 
          sum_sg = sum_sg + wxi  * ( gsalfp   + gsalfm   ) 
       end do 

       g_GH     = sum_sg / sum_s ! this is <cos>
       Qext_GH  = const * sum_e  ! 
       Qscat_GH = const * sum_s  


       end subroutine ghintBH_CS_Odd        
    

! ------------------------------------------------------------------
       SUBROUTINE BHMIE_FLEXI (X, NMX, NSTOP, REFREL, QQEXT, QQSCA, QBACK, GSCA, SUCCESS)

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
       logical, intent(out) :: SUCCESS

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
       real*8, parameter     :: PII = 3.1415916536D0
       real*8, parameter     :: ONE = 1.0D0, TWO = 2.0D0
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

       SUCCESS = .TRUE.
!       NANG = 2 ! FSB only this value 
! IF(NANG.GT.MXNANG)STOP'***Error: NANG > MXNANG in bhmie'
!      IF (NANG .LT. 2) NANG = 2

       DX = REAL( X, 8 )
! FSB Define reciprocals so that divisions can be replaced by multiplications.      
       DX1  = ONE / DX
       DXX1 = DX1 * DX1
       DREFRL = DCMPLX( REAL( REFREL ), IMAG( REFREL ) )
       DREFRL1 = ONE / DREFRL
       Y = DX * DREFRL
       Y1 = ONE / Y
!       YMOD = ABS(Y)
 
!*** Series expansion terminated after NSTOP terms
!    Logarithmic derivatives calculated from NMX on down
!       XSTOP = X + 4.0 * X**0.3333 + 2.0
!       NMX  = MAX(XSTOP,YMOD) + 15

! BTD experiment 91/1/15: add one more term to series and compare results
!      NMX=AMAX1(XSTOP,YMOD)+16
! test: compute 7001 wavelengths between .0001 and 1000 micron
! for a=1.0micron SiC grain.  When NMX increased by 1, only a single
! computed number changed (out of 4*7001) and it only changed by 1/8387
! conclusion: we are indeed retaining enough terms in series!

       FACTOR = 1.0D0
 
!       IF (NMX .GT. NMXX) THEN
!          WRITE(6,*)'Error: NMX > NMXX=',NMXX,' for |m|x=',YMOD
!          SUCCESS = .FALSE.
!          RETURN
!       END IF

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
          XBACK = XBACK + (TWO_N_P_ONE) * factor * (AN - BN)
          
! FSB calculate asymmetry factor   
       
          GSCA = GSCA + REAL((TWO_N_P_ONE)/(EN * (EN + ONE)) *     &
                 (REAL(AN)*REAL(BN)+IMAG(AN)*IMAG(BN)))

          IF (N .GT. 1)THEN
             GSCA = GSCA + REAL((EN - EN1) *                         &
                    (REAL(AN1)*REAL(AN) + IMAG(AN1)*IMAG(AN) +  &
                     REAL(BN1)*REAL(BN) + IMAG(BN1)*IMAG(BN)))
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

       END subroutine BHMIE_FLEXI


END MODULE cmaq_rrtmg_aero_optical_util_module
