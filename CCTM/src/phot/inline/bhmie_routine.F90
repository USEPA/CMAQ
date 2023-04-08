
module bhmie_routine

  implicit none
  save

  logical :: BHMIE_SUCCESS = .True.
  
  
  real :: sback_bhmie

  integer,parameter,private :: sp = 4 ! selected_real_kind(p=6,r=37)
  integer,parameter,private :: dp = 8 ! selected_real_kind(p=15,r=307)

  real(dp),parameter,private :: pii = 3.14159265358979323846264338327950288419716939937510582097494D0


contains

  subroutine driver_bhmie(x,refrel,qexti,qscat,qback,gscat)

     implicit none

    real, intent(in) :: x
    complex, intent(in) :: refrel
    real, intent(out) :: gscat,qback,qexti,qscat

    integer, save :: nang = 2
    real     :: one_over_x
    real(dp) :: x_bhmie
    complex(2*dp) :: refrel_bhmie
    complex(2*dp), dimension(8) :: s1,s2
    real(dp) :: gsca_bhmie,qback_bhmie,qext_bhmie,qsca_bhmie
    real(dp) :: angles(16)

    x_bhmie      = real( x,8 )
    refrel_bhmie = cmplx( real( real(refrel),8 ), real( aimag(refrel),8 ) )

    s1=(0.0d0,0.0d0)
    s2=(0.0d0,0.0d0)

!    write(6,'(a,12(ES12.4,1X))')'refrel_bhmie,refel = ',real(refrel_bhmie),aimag(refrel_bhmie),real(refrel),aimag(refrel)

    call bhmie(x_bhmie,refrel_bhmie,nang,s1,s2,qext_bhmie,qsca_bhmie,qback_bhmie,gsca_bhmie)


     gscat = real( gsca_bhmie )
     qback = real( qback_bhmie )
     qexti = real( qext_bhmie )
     qscat = real( qsca_bhmie )

     one_over_x   = 1.0/x

     QEXTI = QEXTI * one_over_x
     QSCAT = QSCAT * one_over_x
     QBACK = QBACK * one_over_x
     GSCAT = QSCAT * GSCAT
 
  end subroutine driver_bhmie

  subroutine bhmie(x,refrel,nang,s1,s2,qext,qsca,qback,gsca,angles)

    implicit none

    ! Declare parameters:

    integer,parameter :: nmxx=1000000

    ! Arguments:

    integer, intent(in) :: nang
    real(dp), intent(in) :: x
    complex(2*dp), intent(in) :: refrel
    complex(2*dp), dimension(:),intent(out) :: s1,s2
    real(dp), intent(out) :: gsca,qback,qext,qsca
    real(dp),intent(in),optional :: angles(:)

    ! Local variables:

    integer :: j,jj,n,nstop,nmx,nn
    real(dp) :: chi,chi0,chi1,dang,dx,en,fn,p,psi,psi0,psi1,theta,xstop,ymod
    real(dp), dimension(nang) :: amu, pi, pi0, pi1, tau
    complex(2*dp) :: an,an1,bn,bn1,drefrl,xi,xi1,y

    complex(2*dp),allocatable,dimension(:) :: d
    allocate(d(nmxx))

    !***********************************************************************
    !
    ! Subroutine BHMIE is derived from the Bohren-Huffman Mie scattering
    !     subroutine to calculate scattering and absorption by a homogenous
    !     isotropic sphere.
    ! Given:
    !    X = 2*pi*a/lambda
    !    REFREL = (complex refr. index of sphere)/(real index of medium)
    !    NANG = number of angles between 0 and 90 degrees
    !           (will calculate 2*NANG-1 directions from 0 to 180 deg.)
    !           if called with NANG<2, will set NANG=2 and will compute
    !           scattering for theta=0,90,180.
    ! Returns:
    !    S1(1 - 2*NANG-1) = -i*f_22 (incid. E perp. to scatt. plane,
    !                                scatt. E perp. to scatt. plane)
    !    S2(1 - 2*NANG-1) = -i*f_11 (incid. E parr. to scatt. plane,
    !                                scatt. E parr. to scatt. plane)
    !    QEXT = C_ext/pi*a**2 = efficiency factor for extinction
    !    QSCA = C_sca/pi*a**2 = efficiency factor for scattering
    !    QBACK = 4.*pi*(dC_sca/domega)/pi*a**2
    !          = backscattering efficiency
    !    GSCA = <cos(theta)> for scattering
    !
    ! S1 and S2 are the diagonal elements of the "amplitude scattering matrix"
    ! (see eq. 3.12 of Bohren & Huffman 1983) -- the off-diagonal elements
    ! vanish for a spherical target.
    ! For unpolarized incident light, the intensity of scattered light a
    ! distance r from the sphere is just
    !          1
    !  I_s = ------ * I_in * S_11
    !        (kr)^2
    !
    ! where k=2*pi/lambda 
    ! and the "Muller matrix element" S_11 = 0.5*( |S_1|^2 + |S_2|^2 )
    !
    ! for incident light polarized perp to the scattering plane,
    ! the scattered light is polarized perp to the scattering plane
    ! with intensity I_s = I_in * |S_1|^2 / (kr)^2
    !
    ! for incident light polarized parallel to the scattering plane,
    ! the scattered light is polarized parallel to the scattering plane
    ! with intensity I_s = I_in * |S_2|^2 / (kr)^2
    !
    ! History:
    ! Original program taken from Bohren and Huffman (1983), Appendix A
    ! Modified by B.T.Draine, Princeton Univ. Obs., 90.10.26
    ! in order to compute <cos(theta)>
    ! 91.05.07 (BTD): Modified to allow NANG=1
    ! 91.08.15 (BTD): Corrected error (failure to initialize P)
    ! 91.08.15 (BTD): Modified to enhance vectorizability.
    ! 91.08.15 (BTD): Modified to make NANG=2 if called with NANG=1
    ! 91.08.15 (BTD): Changed definition of QBACK.
    ! 92.01.08 (BTD): Converted to full real(dp) and complex(dp)
    !                 eliminated 2 unneed lines of code
    !                 eliminated redundant variables (e.g. APSI,APSI0)
    !                 renamed RN -> EN = real(dp) N
    !                 Note that complex(dp) and DCMPLX are not part
    !                 of f77 standard, so this version may not be fully
    !                 portable.  In event that portable version is
    !                 needed, use src/bhmie_f77.f
    ! 93.06.01 (BTD): Changed AMAX1 to generic function MAX
    ! 98.09.17 (BTD): Added variable "SINGLE" and warning in event that
    !                 code is used with single-precision arithmetic (i.e.,
    !                 compiler does not support complex(dp))
    ! 99.02.17 (BTD): Replaced calls to REAL() and IMAG() by
    !                 REAL() and AIMAG() for compatibility with g77
    !                 Note that when code is used with standard f77 
    !                 compilers, it is now necessary to enable two lines
    !                 defining functions REAL(X) and AIMAG(X)
    ! 99.02.19 (BTD): added lines to be enabled to properly define
    !                 REAL() and AIMAG() if NOT using g77
    !                 ***see below!!***
    ! 01.02.16 (BTD): added IMPLICIT NONE
    ! 01.02.27 (BTD): changed definition of QBACK back to convention of
    !                 Bohren & Huffman and others:
    !                 Q_back = 4.*pi*(dC_sca/dOmega)/(pi*a^2) in backward
    !                          direction
    ! 02.03.09 (BTD): defined statement function REAL_SP to
    !                 avoid warning regarding type conversion when taking
    !                 real part of S1(1) to evaluate QEXT
    !                 some cleanup regarding type conversion
    ! 02.05.30 (BTD): introduced internal complex(dp) arrays S1,S2
    !                 to possibly increase accuracy during summations.
    !                 After summations, output scattering amplitudes
    !                 via single complex arrays S1,S2 as before.
    !                 Usage of this routine is unaffected by change.
    !                 Note: no longer need statement function REAL_SP
    ! 02.09.18 (BTD): Error in evaluation of QBACK reported by Okada Yasuhiko
    !                 Was calculating QBACK using S1 rather than S1
    !                 Corrected.
    ! 02.10.16 (BTD): Added comments explaining definition of S_1 and S_2 .
    ! 10.07.23 (TPR): Converted to Fortran 95, and inputs and outputs have
    !                 64-bit precision. Added option to specify directly
    !                 which angles to compute the scattering matrix for.
    ! end history
    !
    !***********************************************************************

    !***********************************************************************
    !*** Safety checks

    if(nang.lt.2) stop "nang should be > 1"

    !*** Obtain pi:

    dx=x
    drefrl=refrel
    y=x*drefrl
    ymod=abs(y)

    !*** Series expansion terminated after NSTOP terms
    !    Logarithmic derivatives calculated from NMX on down

    xstop=x+4.0D0*x**0.3333D0+2.0D0
    nmx=nint(max(xstop,ymod))+15
    nstop=nint(xstop)

    ! BTD experiment 91.1.15: add one more term to series and compare results
    !      NMX=MAX(XSTOP,YMOD)+16
    ! test: compute 7001 wavelengths between .0001 and 1000 micron
    ! for a=1.0micron SiC grain.  When NMX increased by 1, only a single
    ! computed number changed (out of 4*7001) and it only changed by 1/8387
    ! conclusion: we are indeed retaining enough terms in series!

    if(nmx.gt.nmxx)then
       write(0,*)'error: nmx > nmxx=',nmxx,' for |m|x=',ymod
       stop
    endif

    !*** Require NANG.GE.1 in order to calculate scattering intensities

    if(present(angles)) then

       if(size(angles).ne.nang) stop "The number of angles specified should be equal to nang"
       if(angles(1).ne.0.) stop "angles(1) should be 0."
       if(angles(nang).ne.pii/2.) stop "angles(nang) should be pi/2."

       amu = cos(angles)

    else

       dang=0.

       if(nang.gt.1) dang=.50D0*pii/real(nang-1,dp)

       do j=1,nang
          theta=real(j-1,dp)*dang
          amu(j)=cos(theta)
       end do

    end if

    pi0=0.0D0
    pi1=1.0D0

    s1=(0.0D0,0.0D0)
    s2=(0.0D0,0.0D0)

    !*** Logarithmic derivative D(J) calculated by downward recurrence
    !    beginning with initial value (0.,0.) at J=NMX

    d(nmx)=(0.0D0,0.0D0)
    nn=nmx-1

    do n=1,nn
       en=nmx-n+1
       d(nmx-n)=(en/y)-(1.0D0/(d(nmx-n+1)+en/y))
    end do

    !*** Riccati-Bessel functions with real argument X
    !    calculated by upward recurrence

    psi0=cos(dx)
    psi1=sin(dx)
    chi0=-sin(dx)
    chi1=cos(dx)
    xi1=cmplx(psi1,-chi1)
    qsca=0.0D0
    gsca=0.0D0
    p=-1.0D0
    do n=1,nstop
       en=n
       fn=(2.0D0*en+1.0D0)/(en*(en+1.0D0))

       ! for given N, PSI  = psi_n        CHI  = chi_n
       !              PSI1 = psi_{n-1}    CHI1 = chi_{n-1}
       !              PSI0 = psi_{n-2}    CHI0 = chi_{n-2}
       ! Calculate psi_n and chi_n

       psi=(2.0D0*en-1.0D0)*psi1/dx-psi0
       chi=(2.0D0*en-1.0D0)*chi1/dx-chi0
       xi=cmplx(psi,-chi)

       !*** Store previous values of AN and BN for use
       !    in computation of g=<cos(theta)>

       if(n.gt.1)then
          an1=an
          bn1=bn
       endif

       !*** Compute AN and BN:

       an=(d(n)/drefrl+en/dx)*psi-psi1
       an=an/((d(n)/drefrl+en/dx)*xi-xi1)
       bn=(drefrl*d(n)+en/dx)*psi-psi1
       bn=bn/((drefrl*d(n)+en/dx)*xi-xi1)

       !*** Augment sums for Qsca and g=<cos(theta)>

       qsca=qsca+(2.0D0*en+1.0D0)*(abs(an)**2+abs(bn)**2)
       gsca=gsca+(2.0D0*en+1.0D0)/(en*(en+1.0D0))*(real(an)*real(bn)+aimag(an)*aimag(bn))
       if(n.gt.1)then
          gsca=gsca+(en-1.0D0)*(en+1.0D0)/en* &
               &      (real(an1)*real(an)+aimag(an1)*aimag(an)+ &
               &      real(bn1)*real(bn)+aimag(bn1)*aimag(bn))
       endif

       !*** Now calculate scattering intensity pattern
       !    First do angles from 0 to 90

       do j=1,nang
          jj=2*nang-j
          pi(j)=pi1(j)
          tau(j)=en*amu(j)*pi(j)-(en+1.)*pi0(j)
          s1(j)=s1(j)+fn*(an*pi(j)+bn*tau(j))
          s2(j)=s2(j)+fn*(an*tau(j)+bn*pi(j))
       end do

       !*** Now do angles greater than 90 using PI and TAU from
       !    angles less than 90.
       !    P=1 for N=1,3,...; P=-1 for N=2,4,...

       p=-p
       do j=1,nang-1
          jj=2*nang-j
          s1(jj)=s1(jj)+fn*p*(an*pi(j)-bn*tau(j))
          s2(jj)=s2(jj)+fn*p*(bn*pi(j)-an*tau(j))
       end do
       psi0=psi1
       psi1=psi
       chi0=chi1
       chi1=chi
       xi1=cmplx(psi1,-chi1)

       !*** Compute pi_n for next value of n
       !    For each angle J, compute pi_n+1
       !    from PI = pi_n , PI0 = pi_n-1

       do j=1,nang
          pi1(j)=((2.0D0*en+1.0D0)*amu(j)*pi(j)-(en+1.0D0)*pi0(j))/en
          pi0(j)=pi(j)
       end do
    end do

    !*** Have summed sufficient terms.
    !    Now compute QSCA,QEXT,QBACK,and GSCA

    gsca  =  2.0D0*gsca/qsca
    qsca  = (2.0D0/(dx*dx))*qsca
    qext  = (4.0D0/(dx*dx))*real(s1(1))
    qback =  4.0D0*(abs(s1(2*nang-1))/dx)**2.0D0

  end subroutine bhmie
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
        
!        write(6,'(a,12(ES12.4,1X))',advance='NO')'WRF BHMIE_FLEXY: XX, QEXT,QSCA,G_MIE = ', &
!      & xx,qextalf,qscatalf,gscatalfg

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
       real*8, parameter     :: PII = 3.1415916536D0
       real*8, parameter     :: ONE = 1.0D0, TWO = 2.0D0
!       real(8),parameter     :: ONE_THIRD = 1.0D0/3.0D0
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
!          BHMIE_SUCCESS = .FALSE.
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

       END subroutine BHMIE_FLEXY
    subroutine driver_bhmie_f77(xx, crefin, qextalf, qscatalf, qbackalf, gscatalfg)
       implicit none

       real, intent(in)     :: XX 
       real, intent(out)    :: qextalf, qscatalf, gscatalfg, qbackalf
       complex, intent(in)  :: CREFIN
!local        
       real( 8 ), parameter  :: one_third = 1.0d0 / 3.0d0
       integer              :: NXX
       integer              :: nstop, modulus

       real :: QEXTI, QSCA, QBACK, G_MIE, xx1
              
       xx1 = 1.0 / XX
       
        CALL BHMIE_F77(XX, CREFIN,QEXTI,QSCA,QBACK,G_MIE)

        qextalf   = QEXTI * xx1
        qscatalf  = QSCA  * xx1
        qbackalf  = QBACK * xx1
        gscatalfg = qscatalf * G_MIE
        
!        write(6,'(a,12(ES12.4,1X))',advance='NO')'WRF BHMIE_FLEXY: XX, QEXT,QSCA,G_MIE = ', &
!      & xx,qextalf,qscatalf,gscatalfg

    end subroutine driver_bhmie_f77
      SUBROUTINE BHMIE_F77(X,REFREL,QEXT,QSCA,QBACK,GSCA)
!C Declare parameters:
!C Note: important that MXNANG be consistent with dimension of S1 and S2
!C       in calling routine!
      INTEGER MXNANG,NMXX
      PARAMETER(MXNANG=1000,NMXX=15000)
      

!C Arguments:
      REAL,    INTENT( IN  ) ::  X
      COMPLEX, INTENT( IN  ) ::  REFREL
      REAL,    INTENT( OUT ) ::  GSCA,QBACK,QEXT,QSCA
      
!C Local variables:
      INTEGER J,JJ,N,NSTOP,NMX,NN
      REAL APSI,APSI1,CHI,CHI0,CHI1,DANG,FN,P, &
     &     RN,THETA,XSTOP,YMOD
      REAL AMU(MXNANG),PI(MXNANG),PI0(MXNANG),PI1(MXNANG),TAU(MXNANG)
      DOUBLE PRECISION PSI0,PSI1,PSI,DN,DX
      COMPLEX S1(2*MXNANG-1),S2(2*MXNANG-1)
      COMPLEX AN,AN1,BN,BN1,XI,XI1,Y
      COMPLEX D(NMXX)
      INTEGER, SAVE :: NANG = 2
!C***********************************************************************
!C Subroutine BHMIE is the Bohren-Huffman Mie scattering subroutine
!C    to calculate scattering and absorption by a homogenous isotropic
!C    sphere.
!C Given:
!C    X = 2*pi*a/lambda
!C    REFREL = (complex refr. index of sphere)/(real index of medium)
!C    NANG = number of angles between 0 and 90 degrees
!C           (will calculate 2*NANG-1 directions from 0 to 180 deg.)
!C           if called with NANG<2, will set NANG=2 and will compute
!C           scattering for theta=0,90,180.
!C Returns:
!C    S1(1 - 2*NANG-1) = -i*f_22 (incid. E perp. to scatt. plane,
!C                                scatt. E perp. to scatt. plane)
!C    S2(1 - 2*NANG-1) = -i*f_11 (incid. E parr. to scatt. plane,
!C                                scatt. E parr. to scatt. plane)
!C    QEXT = C_ext/pi*a**2 = efficiency factor for extinction
!C    QSCA = C_sca/pi*a**2 = efficiency factor for scattering
!C    QBACK = (dC_sca/domega)/pi*a**2
!C          = backscattering efficiency
!C    GSCA = <cos(theta)> for scattering
!C
!C Original program taken from Bohren and Huffman (1983), Appendix A
!C Modified by B.T.Draine, Princeton Univ. Obs., 90/10/26
!C in order to compute <cos(theta)>
!C 91/05/07 (BTD): Modified to allow NANG=1
!C 91/08/15 (BTD): Corrected error (failure to initialize P)
!C 91/08/15 (BTD): Modified to enhance vectorizability.
!C 91/08/15 (BTD): Modified to make NANG=2 if called with NANG=1
!C 91/08/15 (BTD): Changed definition of QBACK.
!C 92/01/08 (BTD): Note that this version has been superceded by
!C                 fully double precision version = bhmie.f which,
!C                 unfortunately, is not standard f77.
!C                 However, retain this in case standard f77 version
!C                 is required for porting to some other system.
!C***********************************************************************
!C*** Safety checks
      IF(NANG.GT.MXNANG)STOP'***Error: NANG > MXNANG in bhmie'
      IF(NANG.LT.2)NANG=2
!C*** Obtain pi:
!      PII=4.E0*ATAN(1.E0)
      DX=X
      Y=X*REFREL
      YMOD=ABS(Y)
!C
!C*** Series expansion terminated after NSTOP terms
!C    Logarithmic derivatives calculated from NMX on down
      XSTOP=X+4.E0*X**0.3333+2.0
!C*** Original code:
!C      NMX=AMAX1(XSTOP,YMOD)+15
!C      NSTOP=XSTOP
!C*** Experimental code:
      NMX=1.0*AMAX1(XSTOP,YMOD)+15
      NSTOP=1.0*XSTOP
!C
      IF(NMX.GT.NMXX)THEN
          WRITE(0,*)'Error: NMX > NMXX=',NMXX,' for |m|x=',YMOD
          STOP
      ENDIF
!C*** Require NANG.GE.1 in order to calculate scattering intensities
      DANG=0.
      IF(NANG.GT.1)DANG=.5E0*PII/FLOAT(NANG-1)
      DO 1000 J=1,NANG
          THETA=FLOAT(J-1)*DANG
          AMU(J)=COS(THETA)
 1000 CONTINUE
      DO 1100 J=1,NANG
          PI0(J)=0.E0
          PI1(J)=1.E0
 1100 CONTINUE
      NN=2*NANG-1
      DO 1200 J=1,NN
          S1(J)=(0.E0,0.E0)
          S2(J)=(0.E0,0.E0)
 1200 CONTINUE
!C
!C*** Logarithmic derivative D(J) calculated by downward recurrence
!C    beginning with initial value (0.,0.) at J=NMX
!C
      D(NMX)=(0.E0,0.E0)
      NN=NMX-1
      DO 2000 N=1,NN
          RN=NMX-N+1
          D(NMX-N)=(RN/Y)-(1.E0/(D(NMX-N+1)+RN/Y))
 2000 CONTINUE
!
!C*** Riccati-Bessel functions with real argument X
!C    calculated by upward recurrence
!C
      PSI0=DCOS(DX)
      PSI1=DSIN(DX)
      CHI0=-SIN(X)
      CHI1=COS(X)
!C APSI0 never used, so this line removed from program:
!C      APSI0=PSI0
      APSI1=PSI1
!C XI0 never used, so this line removed from program:
!C      XI0=CMPLX(APSI0,-CHI0)
      XI1=CMPLX(APSI1,-CHI1)
      QSCA=0.E0
      GSCA=0.E0
      P=-1.
      DO 3000 N=1,NSTOP
          DN=N
          RN=N
          FN=(2.E0*RN+1.E0)/(RN*(RN+1.E0))
          PSI=(2.E0*DN-1.E0)*PSI1/DX-PSI0
          APSI=PSI
          CHI=(2.E0*RN-1.E0)*CHI1/X-CHI0
          XI=CMPLX(APSI,-CHI)
!C
!C*** Store previous values of AN and BN for use
!C    in computation of g=<cos(theta)>
          IF(N.GT.1)THEN
              AN1=AN
              BN1=BN
          ENDIF
!C
!C*** Compute AN and BN:
          AN=(D(N)/REFREL+RN/X)*APSI-APSI1
          AN=AN/((D(N)/REFREL+RN/X)*XI-XI1)
          BN=(REFREL*D(N)+RN/X)*APSI-APSI1
          BN=BN/((REFREL*D(N)+RN/X)*XI-XI1)
!C
!C*** Augment sums for Qsca and g=<cos(theta)>
          QSCA=QSCA+(2.*RN+1.)*(CABS(AN)**2+CABS(BN)**2)
          GSCA=GSCA+((2.*RN+1.)/(RN*(RN+1.)))*            &
     &         (REAL(AN)*REAL(BN)+AIMAG(AN)*AIMAG(BN))
          IF(N.GT.1)THEN
              GSCA=GSCA+((RN-1.)*(RN+1.)/RN)*             &
     &        (REAL(AN1)*REAL(AN)+AIMAG(AN1)*AIMAG(AN)+   &
     &         REAL(BN1)*REAL(BN)+AIMAG(BN1)*AIMAG(BN))   
          ENDIF
!C
!C*** Now calculate scattering intensity pattern
!C    First do angles from 0 to 90
          DO 2500 J=1,NANG
              JJ=2*NANG-J
              PI(J)=PI1(J)
              TAU(J)=RN*AMU(J)*PI(J)-(RN+1.E0)*PI0(J)
              S1(J)=S1(J)+FN*(AN*PI(J)+BN*TAU(J))
              S2(J)=S2(J)+FN*(AN*TAU(J)+BN*PI(J))
 2500     CONTINUE
!C
!C*** Now do angles greater than 90 using PI and TAU from
!C    angles less than 90.
!C    P=1 for N=1,3,...; P=-1 for N=2,4,...
          P=-P
          DO 2600 J=1,NANG-1
              JJ=2*NANG-J
              S1(JJ)=S1(JJ)+FN*P*(AN*PI(J)-BN*TAU(J))
              S2(JJ)=S2(JJ)+FN*P*(BN*PI(J)-AN*TAU(J))
 2600     CONTINUE
          PSI0=PSI1
          PSI1=PSI
          APSI1=PSI1
          CHI0=CHI1
          CHI1=CHI
          XI1=CMPLX(APSI1,-CHI1)
!C
!C*** Compute pi_n for next value of n
!C    For each angle J, compute pi_n+1
!C    from PI = pi_n , PI0 = pi_n-1
          DO 2800 J=1,NANG
              PI1(J)=((2.*RN+1.)*AMU(J)*PI(J)-(RN+1.)*PI0(J))/RN
              PI0(J)=PI(J)
 2800     CONTINUE
 3000 CONTINUE
!C
!C*** Have summed sufficient terms.
!C    Now compute QSCA,QEXT,QBACK,and GSCA
      GSCA=2.*GSCA/QSCA
      QSCA=(2.E0/(X*X))*QSCA
      QEXT=(4.E0/(X*X))*REAL(S1(1))
      QBACK=CABS(S1(2*NANG-1))*CABS(S1(2*NANG-1))/(PII*X*X)
      RETURN
      END SUBROUTINE BHMIE_F77
end module bhmie_routine
