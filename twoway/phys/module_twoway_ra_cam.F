MODULE module_twoway_ra_cam

! use module_ra_cam_support

private
public :: get_aerosol_Optics

contains

! ---------------------------------------------------------------------------------

      Subroutine get_aerosol_Optics( ns,nmode,vpath,    &
            INMASS_ws, INMASS_in, INMASS_ec, INMASS_ss, &
            INMASS_h2o,  INDGN, SIG, LAMDA,dens_air,    &
                  tauaer, waer, gaer, faer )

! FSB This routine calculates the aerosol information ( tauaer, waer, 
!     gaer, & faer needed to calculate the solar radiation)  The calling 
!     program specifies the location ( row, column altitude and wave
!     length for the calculation.   

! FSB Input variables:
      
      integer ns ! index for wavelength should be between 1 and 19.
      integer nmode ! should be 3
      real*8 vpath ! contribution to vertical path [ cm**3 / g ]
! FSB mode types 
!     nmode = 1 Aitken
!     nmode = 2 accumulation
!     nmode = 3 coarse 
! FSB modal mass concentration by species  [ ug / m**3]  NOTE:  MKS
      real*8 INMASS_ws(nmode)   ! water soluble
      real*8 INMASS_in(nmode)   ! insolugle
      real*8 INMASS_ec(nmode)   ! elemental carbon or soot like
      real*8 INMASS_ss(nmode)    ! sea salt
      real*8 INMASS_h2o(nmode)  ! water
      
      real*8 INDGN( nmode)          ! geometric mean diameter [ m ] NOTE: MKS      
      real*8 SIG( nmode)          ! geometric standard deviation
      real*8 LAMDA                ! wavelenght  [ micro-m ]
      real*8 dens_air              ! air density [ g / cm **3 ]

!... output aerosol radiative property  [dimensionless]
!
      real*8 tauaer   ! aerosol extinction optical depth
      real*8 waer     ! aerosol single scattering albedo
      real*8 gaer     ! aerosol assymetry parameter
      real*8 faer     ! aerosol forward scattered fractio


! FSB total modal mass concentration [ g/ cm **3 ]     
      real*8 MMASS(nmode)

!
! FSB Internal variables
      integer nspint ! number of spectral intervals
       parameter( nspint = 19)
      real*8 NR(nmode), NI(nmode) ! refractive indices       
      real*8 Mdens1(nmode) ! reciprocal density concentration [ cm**3 / g ]
      real*8 delta_z       ! layer thickness [ cm ]
      real*8 DGN(nmode)    ! modal geometric mean diameter [cm]
! FSB modal mass concentration by species  [ g / cm**3]  NOTE:  cgs
      real*8 MASS_ws(nmode)   ! water soluble
      real*8 MASS_in(nmode)   ! insolugle
      real*8 MASS_ec(nmode)   ! elemental carbon or soot like
      real*8 MASS_ss(nmode)    ! sea salt
      real*8 MASS_h2o(nmode)  ! water

! FSB Modal volumes [ cm**3 / cm **3 ]
      real*8 MVOL_ws(nmode)   ! water soluble
      real*8 MVOL_in(nmode)   ! insolugle
      real*8 MVOL_ec(nmode)   ! soot like
      real*8 MVOL_ss(nmode)   !sea salt
      real*8 MVOL_h2o(nmode)  ! water

      real*8 VOL(nmode)  ! total modal volume[ cm** 3 / cm**3]
      real*8 frac_aec(nmode)    ! volume fraction of soot like aerosol
! FSB additions for external mixtures    
      real*8 sig_scat, sig_abs  ! specific scattering and absorption cross sections
                              ! for diesel soot
                              ! reference schnaiter et al. jour. of
                              ! aerosol science vol. 34, pp1421-1444, 2003
!!       parameter(sig_scat = 1.6d-6, sig_abs = 6.6d-6) ! [ m**2 / ug ] values at 550 [nm]
       parameter(sig_scat = 1.6d-2, sig_abs = 6.6d-2) ! [ cm**2 / ug ] values at 550 [nm]
      
      
      real*8 Tot_mass_ec ! total mass of elemental carbon [ g / cm **3 ] 
      real*8 Tot_mass  !totla aerosol mass [ g / cm **3 ]
      real*8 angstrm_scat, angstrm_abs
       parameter( angstrm_scat = 1.1d0, angstrm_abs = 1.9d0) ! angstrom exponents
      real*8 wavescale_scat, wavescale_abs ! scaling for scattering 
                                                         ! and absorption
      real*8 l550                                          ! 550 [micro-m]
       parameter( l550 = 0.550d0 )
      real*8 rat550                                        ! l550 / lamda

      real*8 dsootdens                     ! diesel soot density 
       parameter( dsootdens = 1.7d0 )      ! [ g /cm ** 3] schnaiter et al. (2003)
                                         ! page 1430 table 2
      real*8 qqscat, qqabs ! specific cross sections at  various
                                         ! spectral intervals [ m**2 / g]
      real*8 ndqscat                       ! non dimensionsal qqscat 
      real*8 asmfac_soot           ! asymmetry factro for soot
!     real*8 gcalc                         ! function to return asymmetry factor
      real*8 asmfac_tot
   
      real*8    bscat_s ! scattering coefficient for soot [1/m]
      real*8    babs_s  ! absorption ceofficient for soot [1/m]
      real*8    bext_s  ! extinction coefficient for soot [1/m
                      ! page 1430 table 2
      integer m, n ! loop indices

      real*8    aermmr   ! mass mixing ratio of aerosols
      real*8    aerMMr_ec ! mass mixing ratio for ec

      real*8    bext                ! extinction coefficient [cm**2 / g]
      real*8    bscat               ! scattering coefficient [cm**2 / g]
      real*8    g_bar              ! asymmetry factor    
      logical do_external

!...component densities [ g/ cm**3 ]

       real*8        rhows   !  bulk density of water soluble aerosol
       parameter (   rhows = 1.8d0 )

       real*8        rhoin  ! bulk density forinsoluble aerosol 
       parameter (   rhoin = 2.2d0 )

       real*8        rhoec  ! bulk density for soot aerosol
       parameter (   rhoec = 1.7d0 )

       real*8        rhoh2o   !  bulk density of aerosol water
       parameter (   rhoh2o = 1.0d0 )
       
       real*8        rhoss    ! bulk density of seasalt
       parameter(    rhoss = 2.2D0 )
! FSB reciprocal component densities[ cm ** 3 / g ] 

       real*8        rhows1    !   water soluble aerosol
       parameter (   rhows1 = 1.0d0 / rhows )

       real*8        rhoin1    ! insoluble aerosol 
       parameter (   rhoin1 = 1.0d0 /  rhoin )

       real*8        rhoec1    !  soot aerosol
       parameter (   rhoec1 = 1.0d0 / rhoec )

       real*8        rhoh2o1   !  aerosol water
       parameter (   rhoh2o1 = 1.0d0 / rhoh2o  )
       
       real*8        rhoss1    !  seasalt
       parameter(    rhoss1 = 1.0d0 / rhoss )      
     
! *** refractive indices

! *** Except as otherwise noted reference values of refractive 
!     indices  for aerosol particles are from the OPAC Data base. 
!     Hess, M. P. Koepke, and I. Schult, Optical properties of
!     aerosols and clouds: The software package OPAC, Bulletan of
!     the American Meteorological Society, Vol 79, No 5, 
!     pp 831 - 844, May 1998. 
!     OPAC is a downloadable data set of optical properties of 
!     10 aerosol components, 6 water clouds and 3 cirrus clouds 
!     at UV, visible and IR wavelengths
!     www.lrz-muenchen.de/~uh234an/www/radaer/opac.htm

! FSB water soluble
      real*8  xnreal_ws(nspint), xnimag_ws(nspint) ! water soluble
       data xnreal_ws / 9 *1.5d0, 7 * 1.46d0, 1.42d0, 2 * 1.46d0 /
       data xnimag_ws / 0.0d0, 8 * 0.0d0, 10 * 0.01d0 /
      
! FSB insoluble      
      real*8 xnreal_in(nspint), xnimag_in(nspint) ! dust like       
       data xnreal_in / 9 * 1.53d0, 7 * 1.51d0, 1.52d0, 2 * 1.5d0 /
       data xnimag_in / 9 * 0.008d0, 7 * 0.015d0, 0.011d0, 2 * 0.008d0/
       
! FSB sea salt      
      real*8  xnreal_ss(nspint), xnimag_ss(nspint) ! sea salt
       data xnreal_ss / 9 * 1.50d0, 7 * 1.46d0, 1.5d0, 2 * 1.48d0/
       data xnimag_ss / 9 * 0.0d0, 7 * 0.003d0, 0.008d0, 2 * 0.001d0/
! FSB water
      real*8  xnreal_h2(nspint), xnimag_h2(nspint) ! water
       data xnreal_h2 / 9 * 1.34d0, 7 * 1.35d0, 3 * 1.33d0/ 
       data xnimag_h2 / 9 * 0.0d0, 6 * 0.001d0,   &
                        2 * 0.01d0, 2 * 0.004d0 /      
! FSB elemental carbon - soot like      
      real*8  xnreal_ec(nspint), xnimag_ec(nspint) ! soot like
       data xnreal_ec / 19 * 1.75d0 / 
       data xnimag_ec / 19 * 0.63d0 /
      
      
! FSB values for soot like changed to value given in 
!     Bond, T. C. and  R.W. Bergstrom, Light absorption by carbonaceousc 
!       particles: An investigativ review,
!     Aerosol Science and Technology, Vol 40, pp 27-67, 2006.
!     Their valuse are at 550 [nm], we are holding these values
!     constant over all wavelenghts. When carbon soot
!     becomes a major component an external approach is used wiht
!     mass cross sections SIG_SCAT and SIG_ABS	[ m**2 / g ] are
!     used. See below. 

    

!
! FSB Begin code

! FSB calculate layer thickness [cm]
      delta_z = vpath / dens_air

! FSB sum to get total modal volumes and total mass
      Tot_mass = 0.0d0
      do m = 1, nmode
! FSB convert to cgs units
!     1 g/cm**3 = 1.0E-12 ug / M**3
        MASS_ws(m)  = 1.0D-12 * INMASS_ws(m)
        MASS_in(m)  = 1.0D-12 * INMASS_in(m)
        MASS_ec(m)  = 1.0D-12 * INMASS_ec(m)
        MASS_ss(m)  = 1.0D-12 * INMASS_ss(m)
        MASS_h2o(m) = 1.0D-12 * INMASS_h2o(m)
!     convert diameters from meters to micro-meters
        DGN(M) = 1.0E6 * INDGN(M)
      end do
      
      
      
      do m = 1, nmode
        MVOL_ws(m)  = rhows1  * MASS_ws(m)
        MVOL_in(m)  = rhoin1  * MASS_in(m)
        MVOL_ec(m)  = rhoec1  * MASS_ec(m)
        MVOL_ss(m)  = rhoss1  * MASS_ss(m)
        MVOL_h2o(m) = rhoh2o1 * MASS_h2o(m)
      
       VOL(m) = MVOL_ws(m) + MVOL_in(m) + MVOL_ss(m) +   &
                MVOL_ec(m) + MVOL_h2o(m)
       
       MMASS(m) = MASS_ws(m) + MASS_in(m) + MASS_ss(m)  &
                  + MASS_ec(m)+ MASS_h2o(m)     
        
       Tot_mass = Tot_mass + MMASS(m)
       end do
      
! FSB determine fraction of elemental carbon      
      do m = 1, nmode
      do_external = .false.      
       frac_aec(m) = MVOL_ec(m) / VOL(m)
       if( frac_aec(m) .ge. 0.60 ) do_external = .true.
      end do 
! FSB now calculate the refractive indices adjusting for
!     the possibility of an external mixture. 
      Tot_mass_ec = 0.0d0
     
      do m = 1, nmode
     
       if( do_external) then
        nr(m) = (MVOL_ws(m) * xnreal_ws(ns) +                       &
           MVOL_in(m) * xnreal_in(ns) + MVOL_ss(m) * xnreal_ss(ns)  &
          + MVOL_h2o(m) * xnreal_h2(ns)  ) / VOL(m)

        ni(m) = (MVOL_ws(m) * xnimag_ws(ns) +                         &
         MVOL_in(m) * xnimag_in(ns)   + MVOL_ss(m) * xnimag_ss(ns)    &
           + MVOL_h2o(m) * xnimag_h2(ns) ) / VOL(m)

! FSB adjust volume and mass for external mixture   
        VOL(m) = VOL(m) - MVOL_ec(m) 
        MMASS(m) = MMASS(m) - mass_ec(m)
        Tot_mass = Tot_mass - mass_ec(m)
        Tot_mass_ec = Tot_mass_ec + mass_ec(m)
        
       else  ! all internal mixture
        nr(m) = (MVOL_ws(m) * xnreal_ws(ns) +                         &
           MVOL_in(m) * xnreal_in(ns) + MVOL_ss(m) * xnreal_ss(ns)    &
          + MVOL_h2o(m) * xnreal_h2(ns) + MVOL_ec(m) * xnreal_ec(ns)  &
          ) / VOL(m)

        ni(m) = (MVOL_ws(m) * xnimag_ws(ns) +                         &
         MVOL_in(m) * xnimag_in(ns)   + MVOL_ss(m) * xnimag_ss(ns)    &
          + MVOL_h2o(m) * xnimag_h2(ns) +  MVOL_ec(m) * xnimag_ec(ns) &
         ) / VOL(m)

       end if ! test on external moxture

!      write(20,*) ' inside get_aerosol_optics '
!      write(20,*) ' dens_air =', dens_air
!      write(20,*) ' vpath = ', vpath
!      write(20,*) ' delta_z = ', delta_z
!      write(20,*) ' tot_mass = ', tot_mass
!       write(20,*)  'm, VOL(m), MMASS(m), nr(m), ni(m)'       
!       write(20,*)  m, VOL(m), MMASS(m), nr(m), ni(m)
!       write(20,*) 'DGN(m), SIG(m)'!
!       write(20,*) DGN(m), SIG(m)
!       write(30,*) ' m = ', m
!       write(30,*) ' vol(m) =', VOL(m)
!       write(30,*) ' Mdens1(m) = ', Mdens1(m)
!       write(30,*) ' MMass(m) = ', MMass(m)
!       write(30,*) ' nr(m), ni(m) = ', nr(m), ni(m)
!       write(30,*) ' DGN(m), SIG(m) = ', DGN(m), SIG(m)
      end do ! loop over modes 
!        write(30,*) ' tot_mass = ', tot_mass
!        write(30,*) ' tot_mass_ec = ', tot_mass_ec
!        write(30,*) ' aermmr = ', aermmr
!        write(30,*) ' vpath = ', vpath
! *** calculate the extinction and scattering coefficients
!     in [ cm ** 2 / g ] and the assymetry factor for the  aerosol
!     particles.
! NOTE: both wavelength and geometrical mean diameter are in [micro-m]
!       only their ratio is important in aero_optical.
! FSB aero_optical returns teh extinction, scattering and asymmetry factors
!     summed over all modes. 
      
           CALL aero_optical( LAMDA,  &
              NMODE,                  &
              NR, NI,                 &
              VOL, DGN, SIG,          &
              bext, bscat, g_bar)
     
        tauaer = bext  * delta_z  ! contributionto aerosol optical depth

        waer   = bscat / bext 
        gaer   = g_bar
        faer   = gaer * gaer

      if( do_external ) then       
! FSB add ec contribution for  external mixture 

       rat550         = l550 / lamda
       wavescale_scat = (rat550) ** angstrm_scat
       wavescale_abs  = (rat550) ** angstrm_abs 
       qqscat         = wavescale_scat * sig_scat
       qqabs          = wavescale_abs * sig_abs

! In the following calculation, the conversion facto r= 
! 1.0d-4 ( cm/micro-m ) * (micro_m ) * ( cm**2 / g ) * ( g/ cm**3
        ndqscat = 1.0d-3 *  lamda * qqscat * dsootdens                            
        asmfac_soot    = gcalc(ndqscat) 

! *** diesel soot carbon absorption and scattering values.
        aermmr_ec = tot_mass_ec / dens_air
        babs_s    =  wavescale_abs  * sig_abs 
        bscat_s   = wavescale_scat * sig_scat  
        bext_s    = babs_s + bscat_s 
              
! FSB add optical depth for ec
        tauaer  = tauaer + bext_s * aermmr_ec * vpath
       
        asmfac_tot  = ( bscat * G_bar +          &
                      bscat_s * asmfac_soot ) /  &
                      ( bscat + bscat_s )        
         gaer       = asmfac_tot
         faer       = asmfac_tot * asmfac_tot
         waer       = (bscat + bscat_s) /        &
                             ( bext + bext_s) 

      end if ! test on external mixture  additions
        
!         write(30,*) ' aermmr_ec ', aermmr_ec
!         write(30,*) ' bext = ', bext
!         write(30,*) ' tauaer = ', tauaer
      
      return
      end subroutine get_aerosol_Optics

! ---------------------------------------------------------------------------------

      real*8 function gcalc( qscat_avg )
! *** calculate asymmetry factor.      
! *** implement Hanna-Mathur method as a callable function 
! *** coded 06/06/2005 by Dr. Francis S. Binkowski
!     Carolina Environmental Program, 
!     The University of North Carolina at Chapel Hill, NC
!     email: frank_binkowski@unc.edu
!     
      implicit none

      real*8 qscat_avg
      real*8 aa0, aa1,aa2,aa3,aa4
       parameter( aa0 =  88.466d0,  &
                  aa1 = -61.628d0,  &
                  aa2 =  34.483d0,  &
                  aa3 = -8.543d0,   &
                  aa4 =  0.77435d0 )
      real*8 qangle 
      real*8 qq, qf1,qf2,qf3
      real*8 pi
       parameter( pi = 3.14159265359d0 )
      real*8 degtorad 
       parameter( degtorad = pi / 180.0d0 )

      qangle = aa0
      if ( qscat_avg .gt. 1.0d-4 ) then
      
        qq  = qscat_avg * qscat_avg
        qf1 = aa0 + aa1 * qscat_avg
        qf2 = aa2 + aa3 * qscat_avg
        qf3 = aa4 * qq
        qangle = qf1 + qf2 * qq + qf3 * qq

      end if 

      qangle = degtorad * qangle 
      gcalc  = cos( qangle )   ! assymetry factor 

      return
      end function


! ---------------------------------------------------------------------------------

       
!     /////////////////////////////////////////////////////////////////
      
       subroutine aero_optical(  lamda,  &
                  nmode,                 &
                  nr, ni,                &
                  Vol,  dgn, sig,        &
                  bext, bscat, g_bar )
     
! *** calculate the extinction and scattering coefficients and
!     assymetry factors for each wavelength as a sum over the 
!     individual lognormal modes. Each mode may have a different 
!     set of refractive indices.

      IMPLICIT NONE
! *** input variables
      real*8 lamda      ! wavelengths  [micro-m]
                      
      INTEGER nmode  ! number of lognormal modes
      real*8 nr( nmode), ni(nmode) ! real(r8) and imaginary 
                                ! refractive indices
      real*8 Vol(nmode) ! modal aerosol volumes [cm**3 /cm**3]
      real*8 dgn(nmode) ! geometric mean diameters 
                      ! for number distribution [ micro-m]
      real*8 sig(nmode) ! geometric standard deviation 
      
      
! *** output variables 
      real*8 bext  ! extinction coefficient [ cm**2 / g ]
      real*8 bscat ! scattering coefficient [ cm**2 / g ]
      real*8 g_bar  ! assymetry factor for Mie and molecular scattering
      
      
! *** internal variables 
      INTEGER  j  ! loop index
      real*8 xlnsig(nmode) ! natural log of geometric standard deviations      
      real*8 beta_Sc, bsc  !aerosol scattering coefficient 
 
      real*8 beta_Ex       ! aerosol extinction coefficients       
      real*8 G             ! modal aerosol assymetry factors
      real*8 sum_g
      real*8 VLX, DGX, SIGX, NRX, NIX, LSIGX
      real*8 lamdam1 ! 1/ lamda
      real*8 alphav ! Mie size parameter
      real*8 vfac
      real*8 pi
      PARAMETER( pi = 3.14159265359D0)
       
       
! *** coded 09/08/2004 by Dr. Francis S. Binkowski
! FSB modified 10/06/2004, 10/12/2004, 10/18/2005
! FSB 01/12/2006
!     Carolina Environmental Program
!     University of North Carolina at Chapel Hill
!     email: frank_binkowski@unc.edu


! *** initialize variables
       bext  = 0.0d0
       bscat = 0.0d0
       sum_g = 0.0d0
       lamdam1 = 1.0d0 / lamda
        
!     write(20,*) ' inside aero_optical', 'lamda = ', lamda
      
       DO j = 1, nmode
!    calculate the extinction and scattering coefficients
!    for each mode 
!        write(20,*) ' j = ', j
        VLX   = Vol(j)
        DGX   = dgn(j)
        SIGX  = sig(j) 
        NRX   = nr(j)
        NIX   = ni(j)
        LSIGX = log(SIGX)
       
!     calculate Mie size parameter for volume distribution
!      scale converts 1 / lamda in [ 1/ nm] to be in [1 / m]
!     exp(3.0 * xlnsig*xlnsig)  converts dgn to dgv (volume diameter)
          alphav =  pi * DGX *  &
                           exp(3.0d0 * LSIGX * LSIGX) * lamdam1 
!      write(20,*) ' j VLX DGX NRX NIX LSIGX alphav'
!      write(20,*)  j, VLX, DGX, NRX, NIX, LSIGX, alphav 

        CALL fast_optics(NRX, NIX, alphav, LSIGX, beta_EX, beta_Sc, G)            

! *** fast_optics  returns the normalized values
!     Calculate the actual extinction and scattering coefficients 
!     by multplying by the modal volume and dividing by the wavelength
!     then scaling lamdam1 to  get [ 1 /cm ].  
         
         
         vfac  = 1.0d4  * VLX * lamdam1

!        write(20,*)' vfac = ', vfac
!        write(20,*)' G = ', G
!        write(20,*) ' beta_Ex = ', beta_Ex
!        write(20,*) ' beta_Sc = ', beta_Sc
! *** sum to get total extinction and scattering 
!     and contribution to the overal assymetry factor

          bext    = bext  + vfac * beta_Ex
          bsc     = vfac * beta_Sc
          bscat   = bscat + bsc          
          sum_g   = sum_g + bsc * G


       END DO  ! loop on modes  
       
! *** calculate combined assymetry factor for all modes  

       g_bar = sum_g / bscat ! changed to divide by bscat
!        write(30,*) ' inside aero_optical '       
!        write(30,*) ' g_bar = ', g_bar
!       write(20,*) ' bext = ', bext
!       write(20,*) ' bscat = ', bscat
!       write(20,*) ' gbar = ', g_bar 
      RETURN
      
      END SUBROUTINE ! aero_optical

! ---------------------------------------------------------------------------------

!     /////////////////////////////////////////////////////////////////


!     /////////////////////////////////////////////////////////////////

      SUBROUTINE FAST_OPTICS ( NR, NI, ALPHV, XLNSIG,  &
                               BETA_EXT, BETA_SCAT, G )
! *********************************************************************
! FSB Note this is a special version coded in REAL*8 to be consistent 
!     with the NCAR radiation package<<<<<<<<<
! *********************************************************************
!-----------------------------------------------------------------------
!  A parameterization of the aerosol extinction and scattering code
!     Coded by Dr. Francis S. Binkowski
!     Carolina Environmental Program,
!     The University of North Carolina at Chapel Hill
!     email: frank_binkowski@unc.edu
!     Code dates. Began February 25, 2005, current March 18, 2005
!     modified by FSB May 10, 2005 to calculate assymetry factor by the
!     semi-empirical method of Hanna and Mathur. Note that
!     the normalized scattering coefficient (non-dimensional) is
!     interpreted as an average of  scattering efficiency over the
!     size distribution
!     Modified 05/25/2005 to ensure that if ni = 0.0, that
!     beta_scat = beta_ext.
!     Modified 03/17/2008 to ensure match with reference calculation for
!     all cases of sigma_g. 
!     The  form of the mathematical expression is based upon that of
!     Heintzenberg & Baker (1976), and further inspired by the work
!     of Willeke and Brockmann(1977) who showed the basic shape of
!     the extinction resulting from an integration of the Mie extinction
!     efficiency over a log normal particle size distribution
!     The parameterization is based upon a fit to the standard CMAQ
!     radiative transfer code for visibility. to which a module for
!     absorption was added so that absorption and scattering would also be
!     available.
!
!     The  shape has been modified for absorption by small particles
!     by using the formulae of Penndorf (1962). For large particles,
!     the shape has been altered by using the large sphere values from 
!     Table I of  Irvine (1965)
!     
!
!     Results are consistent with those of Willeke and Brockmann (1977).
!     
!     The asymmetry factor is calculated by a new algorithm FSB - 04/04/2008
!  References:
!
!     Heintzenberg, J. and M. Baker, Spherical particle populations:
!     approximate analytic relationship between size distribution
!     parameters and integral optical properties, Applied Optics,
!     Volume 15, Number 5 pp 1178-1181, May 1976.
!
!     Irvine, W. M. Light Scattering by spherical particles: Radiation
!     pressure, asymmetry factor, and extinction cross section,
!     Journal of eh Optical Society of Amerioca, Vol. 55, NO. 1, 
!     January ,1965

!     Penndorf, R. Scattering and Extinction Coefficeints for small
!     spherical particles, J. Atmospheric Sciences, Volume 19, p 193,
!     March 1962.
!
!     Willeke, K. and  J. E. Brockmann, Extinction coefficients for
!     multmodal atmospheric particle size distributions, Atmospheric
!     Environment, vol. 11, pp 95-999, 1977.
!-----------------------------------------------------------------------

      IMPLICIT NONE

!***arguments

      REAL*8, INTENT(IN)  :: NR, NI     ! indices of refraction
      REAL*8, INTENT(IN)  :: ALPHV       ! Mie parameter for dgv
      REAL*8, INTENT(IN)  :: XLNSIG     ! log of geometric standard deviation

      REAL*8, INTENT(OUT) :: BETA_EXT   ! normalized extinction coefficient
      REAL*8, INTENT(OUT) :: BETA_SCAT  ! normalized scattering coefficient
      REAL*8, INTENT(OUT) :: G          ! assymetry factor

!***internal variables

! *** These variables are set in CMAQ photolysis routines through an 
!      INCLUDE file that is not
!     used here.
! **********************************************************************
      REAL*8, PARAMETER :: PI = 3.14159265358979324D0                  !*
      REAL*8, PARAMETER :: PI180  = PI / 180.0                         !*
! ********************************************************************** 
!   
      REAL*8 NR1, NR2, TAU
      REAL*8 C, CEXT, CSCAT
      REAL*8 B, BEXT, BSCAT
      REAL*8 BBFAC
      REAL*8 ALPHA_I
      REAL*8 A, LOGX2, XLNSIG2, MM1

      REAL*8, PARAMETER :: SIGMA_G_TWO = 2.0
      REAL*8, PARAMETER :: LOGSIG_TWO = 0.693147
      REAL*8, PARAMETER :: LOGSIG_TWO_SQD = LOGSIG_TWO * LOGSIG_TWO
      REAL*8, PARAMETER :: LOGSIG_105 = 1.102500
      REAL*8, PARAMETER :: DIFFSIG = SIGMA_G_TWO - 1.05 
      REAL*8, PARAMETER :: A_TWO = 0.5 / LOGSIG_TWO_SQD
      REAL*8 LARGESCAT  ! large sphere limit for scattering
      REAL*8 LARGEEXT   ! large sphere limit for extinction 
      REAL*8 SMALL_G, LARGE_G

      REAL*8 ALPHV2, ALPHV3
      REAL*8 X_ALPHA, X_ALPHA2, X_ALPHA3
      REAL*8 SIGMA_G, FCORR 
      REAL*8 EXPFAC2, EXPFAC3
      REAL*8 EFAC, EFAC_EXT, EFAC_SCAT
      REAL*8 DEN1, FAC1, FAC2
      REAL*8 F1, F2, F3
      REAL*8 G1, G2, G3, G4, G5
      REAL*8 NN, TT

      REAL*8 T1F1, T2F1, T1F2, T2F2, T1F3, T2F3
      REAL*8 T1G1, T2G1, T1G2, T2G2, T1G3, T2G3, T1G4, T2G4
      REAL*8 T1G5, T2G5, T1P1, T2P1

!***the following are for calculating the Penndorff Coefficients

      REAL*8 PENN1, A1, A2, A3, PENN2
      REAL*8 XNR, XNI, XNR2, XNI2, XNRI, XNRI2, XNRMI
      REAL*8 XRI, XRI2, XRI36, XNX, XNX2
      REAL*8 Z1, Z12, Z2, XC1

!***coefficients for polynomials

      REAL*8, PARAMETER :: F1A0 = -1.309193D-1
      REAL*8, PARAMETER :: F1A1 =  2.565668D+0

      REAL*8, PARAMETER :: F2A0 =  1.489233D+1
      REAL*8, PARAMETER :: F2A1 = -5.311351D+1
      REAL*8, PARAMETER :: F2A2 =  8.180334D+1
      REAL*8, PARAMETER :: F2A3 = -4.549854D+1

      REAL*8, PARAMETER :: F3A0 =  5.549359D-1
      REAL*8, PARAMETER :: F3A1 =  2.571002D-1
      REAL*8, PARAMETER :: F3A2 =  9.212703D-1
      REAL*8, PARAMETER :: F3A3 =  5.155047D-2

      REAL*8, PARAMETER :: G1A0 =  1.000000D+0
      REAL*8, PARAMETER :: G1A1 = -7.636121D-1
      REAL*8, PARAMETER :: G1A2 =  1.285532D+0
      REAL*8, PARAMETER :: G1A3 =  1.629161D-2

      REAL*8, PARAMETER :: G2A0 =  1.000000D+0
      REAL*8, PARAMETER :: G2A1 = -2.507954D+0
      REAL*8, PARAMETER :: G2A2 =  5.170246D+0
      REAL*8, PARAMETER :: G2A3 = -4.945515D+0

      REAL*8, PARAMETER :: G3A0 =  1.000000D+0
      REAL*8, PARAMETER :: G3A1 = -2.650134D+0
      REAL*8, PARAMETER :: G3A2 =  4.515327D+0
      REAL*8, PARAMETER :: G3A3 = -4.027442D+0

      REAL*8, PARAMETER :: G4A0 =  1.000000D+0
      REAL*8, PARAMETER :: G4A1 = -8.532647D-1
      REAL*8, PARAMETER :: G4A2 =  6.766073D-1

      REAL*8, PARAMETER :: G5A0 =  1.0
      REAL*8, PARAMETER :: G5A1 = -6.674946D-1
      REAL*8, PARAMETER :: G5A2 = -5.426378D-1

! *** Coefficients for quadratic fit for <cos> large particles
!     from Irvine (1965) Table I
      REAL*8, PARAMETER :: IRVA0 = 1.076232D+0          
      REAL*8, PARAMETER :: IRVA1 = -4.891972D-2
      REAL*8, PARAMETER :: IRVA2 = -2.243449D-2


! FSB Coefficients for SMALL_G  calculation
      REAL*8, PARAMETER :: XXA0 =  3.392224D-2
      REAL*8, PARAMETER :: XXA1 =  8.276532D-1
      REAL*8, PARAMETER :: XXA2 = -3.784926D-1
      REAL*8, PARAMETER :: XXA3 =   5.853108D-2
      REAL*8, PARAMETER :: G_ALPHA_I = XXA0 + XXA1 + XXA2 + XXA3

! FSB Coefficients for LARGE_G calculation
      REAL*8, PARAMETER :: YYA0 =  6.776173D-1
      REAL*8, PARAMETER :: YYA1 =  2.385339D-2
      REAL*8, PARAMETER :: YYA2 = -7.952263D-4
      REAL*8, PARAMETER :: YYA3 =  9.410370D-6

! FSB Coefficientf for adjusting asymmetry factor calculation 
      REAL*8, PARAMETER :: CXG0 =  -5.845196D-2
      REAL*8, PARAMETER :: CXG1 = 3.229183D-1
      REAL*8, PARAMETER :: CXG2 =  2.764716D-1
      REAL*8, PARAMETER :: CXG3 = -8.790411D-2


!***FSB added for assymetry factor calculation

      REAL*8 QSCAT_AVG

      REAL*8, PARAMETER :: AA0 =  88.466D0
      REAL*8, PARAMETER :: AA1 = -61.628D0
      REAL*8, PARAMETER :: AA2 =  34.483D0
      REAL*8, PARAMETER :: AA3 =  -8.543D0
      REAL*8, PARAMETER :: AA4 =   0.77435D0

      
      REAL*8 GMAX
      REAL*8 QANGLE
      REAL*8 QQ, QF1, QF2, QF3
      REAL*8 QQSUM, QQF1,QQF2, QQF3, QQCORR

      REAL*8, PARAMETER :: DEGTORAD = PI180
      REAL*8, PARAMETER :: THREE_PI_TWO = 3.0d0 * PI / 2.0d0

!***FSB start calculation
      SIGMA_G = EXP( XLNSIG )
! FSB check range of SIGMA_G      
! *** Maximamum value of SIGMA_G allowed is 2.0 
      SIGMA_G = MIN( 2.0D0, SIGMA_G )
! *** Minimum allowed value fo SIGMA_F is 1.05
      SIGMA_G = MAX( 1.05D0, SIGMA_G ) 
      
      XLNSIG2 = XLNSIG * XLNSIG
      A = 0.5d0  /  XLNSIG2 

      NR1 = NR - 1.0
      NR2 = NR * NR
!***evaluate polynomials
!***  optimize for piplined microprocessor
!***  appproach recommended by Dr. Carlie Coats

      NN = NR1 * NR1

      T1F1 = F1A0 + F1A1 * NR1
      T1F2 = F2A0 + F2A1 * NR1
      T2F2 = F2A2 + F2A3 * NR1
      T1F3 = F3A0 + F3A1 * NR1
      T2F3 = F3A2 + F3A3 * NR1

      F1 = T1F1                 ! linear
      F2 = T1F2 + T2F2 * NN     ! cubic
      F3 = T1F3 + T2F3 * NN     ! cubic

      C       = F1
! FSB correct for values of SIGMA_ less than 2.0.
!     The smallest value of SIGMA_G allowed is 1.05 
!     2.0 - 1.05 = 0.95
!      FCORR   = (1.0 - 0.13 * (SIGMA_G - 1.05)  / 0.95 )
!     1.0 / 0.95 = 1.052632.  1.0 / 0.95 * 0.13 = 0.136842
      FCORR   = 1.0d0 - 0.136842d0 * (SIGMA_G - 1.05d0)  
      
      CEXT    = C * FCORR
      CSCAT   = CEXT
      B       = F3 * A_TWO 
      ALPHA_I = F2
      BEXT    = B
      BSCAT   = B
      PENN1   = 0.0
      PENN2   = 0.0
      IF ( NI .GE. 0.0 ) THEN

         TAU = NI / NR1

!***evaluate more polynomials
!***  optimize for piplined microprocessor
!***  appproach recommended by Dr. Carlie Coats

         TT = TAU * TAU
         T1G1 = G1A0 + G1A1 * TAU
         T2G1 = G1A2 + G1A3 * TAU
         T1G2 = G2A0 + G2A1 * TAU
         T2G2 = G2A2 + G2A3 * TAU
         T1G3 = G3A0 + G3A1 * TAU
         T2G3 = G3A2 + G3A3 * TAU
         T1G4 = G4A0 + G4A1 * TAU
         T2G4 = G4A2            ! quadratic
         T1G5 = G5A0 + G5A1 * TAU
         T2G5 = G5A2            ! quadratic
         G1   = T1G1 + T2G1 * TT
         G2   = T1G2 + T2G2 * TT
         G3   = T1G3 + T2G3 * TT
         G4   = T1G4 + T2G4 * TT
         G5   = T1G5 + T2G5 * TT

! *** adjust the variables 

         CEXT    = CEXT    * G1
         CSCAT   = CSCAT   * G2
         BEXT    = BEXT    * G3
         BSCAT   = BSCAT   * G4
         ALPHA_I = ALPHA_I * G5

!*** Calculate the Penndorf Coefficients for teh small particle limit

         XNR   = NR
         XNI   = NI
         XNR2  = XNR   * XNR
         XNI2  = XNI   * XNI
         XNRI  = XNR2  + XNI2
         XNRI2 = XNRI  * XNRI
         XNRMI = XNR2  - XNI2
         XRI   = XNR   * XNI
         XRI2  = XRI   * XRI
         XRI36 = 36.0  * XRI2
         XNX   = XNRI2 + XNRMI - 2.0d0
         XNX2  = XNX   * XNX

         Z1    = XNRI2 + 4.0d0 * XNRMI + 4.0d0
         Z12   = Z1    * Z1
         Z2    = 4.0d0   * XNRI2 + 12.0d0 * XNRMI + 9.0d0
         XC1   = 8.0d0   / ( 3.0d0 * Z12 )
         A1    = 24.0d0  * XRI / Z1

         A2    = 4.0d0   * XRI / 15.0d0 + 20.0d0 * XRI / ( 3.0d0 * Z2 ) + &
                 4.8d0   * XRI * (  7.0d0 * XNRI2 +                       &
                 4.0d0   * ( XNRMI - 5.0d0 ) ) / Z12

         A3    = XC1   * ( XNX2 - XRI36 )

         ALPHV2 = ALPHV * ALPHV
         ALPHV3 = ALPHV * ALPHV * ALPHV
         EXPFAC2 = EXP( 2.0d0 * XLNSIG2 )
         EXPFAC3 = EXP( 4.5d0 * XLNSIG2 )

         T1P1 = A1 + A2 * ALPHV2 * EXPFAC2
         T2P1 = A3 * ALPHV3 * EXPFAC3

!***PENN1 is the analytic integral of the Pendorff formulae over
!***   a log normal particle size distribution.

         PENN1 = THREE_PI_TWO * ( T1P1 + T2P1 )
         PENN2 = THREE_PI_TWO * T2P1        
      else
         print *, ' ==d== fast optics neg ', ni, alphv
      END IF                    ! test of ni > 0.0
     
      X_ALPHA  = ALPHV / ALPHA_I
      
      LOGX2    = LOG( X_ALPHA ) ** 2

      BBFAC    = BEXT * A  / ( BEXT + A )

      EFAC_EXT = EXP( -BBFAC * LOGX2 )
      
!***FSB calculate normalized extinction and scattering coefficients

      BETA_EXT = THREE_PI_TWO * CEXT *  &
                          SQRT( A / ( BEXT + A ) ) * EFAC_EXT

      BETA_SCAT = BETA_EXT      ! NI = 0.0 case

! *** Check for 0.0 < NI

      IF ( NI .GE. 0.0 ) THEN 
        
!        BBFAC = 1.5 * BBFAC ! reset BBFAC for scattering
        BBFAC = 1.1d0 * BBFAC ! reset BBFAC for scattering

        EFAC_SCAT = EXP( -BBFAC * LOGX2 )

! *** recalculate the normalized scattering coefficient
      
        BETA_SCAT = THREE_PI_TWO * CSCAT *  &
                             SQRT( A / ( BSCAT + A ) ) * EFAC_SCAT

! *** Adjust beta_ext for small particle absorption:

         IF ( X_ALPHA .LT. 0.13d0  ) THEN
           
           FAC1 = ALPHV + 0.6d0 * ALPHV2
           
           BETA_EXT = MAX( BETA_EXT, &
                      ( BETA_EXT * FAC1 + ( 1.0d0 - FAC1 ) * PENN1 ) )
     
           BETA_SCAT = MAX(BETA_SCAT, &
                      ( BETA_SCAT * FAC1 + (1.0d0 - FAC1) * PENN2 ))
      
        END IF ! test for   XALPHA < 0.13 
      
      END IF  ! test for 0.0  < NI
! *** Calculate large sphere limits(Irvine, 1965)

       MM1 = EXP(0.5d0 * XLNSIG2) / ALPHV
       
! FSB large sphere limit - scattering

      LARGESCAT = THREE_PI_TWO * (8.652439D-1 + 1.501772D-1 * NR) * MM1
      
! FSB large spnere limit for extinction ( no edge effectss )

      LARGEEXT = THREE_PI_TWO * (2.0d0 * MM1)      

! FSB Adjust for large sphere limits

      IF ( ALPHV .GE. 10.0d0 ) THEN
      
        BETA_EXT  = MAX( BETA_EXT, LARGEEXT)
        
        BETA_SCAT = MAX(BETA_SCAT, LARGESCAT)
     
      END IF ! test for 10.0 < ALPHV 

!***FSB now calculate the asymetry factor
!***  using the Hanna-Mathur quasi-empirical method
!      GMAX = 0.0

!      QSCAT_AVG = BETA_SCAT
!      QANGLE = AA0

!      IF ( QSCAT_AVG .GT. 1.0E-4 ) THEN
!         QQ  = QSCAT_AVG * QSCAT_AVG
!         QF1 = AA0 + AA1 * QSCAT_AVG
!         QF2 = AA2 + AA3 * QSCAT_AVG
!         QF3 = AA4 * QSCAT_AVG * QSCAT_AVG
!         QANGLE = QF1 + QF2 * QQ + QF3 * QQ
!      END IF

!      QANGLE = DEGTORAD * QANGLE
!      G      = COS( QANGLE)     ! assymetry factor


!***FSB now calculate the asymetry factor
!***  using the modified Hanna-Mathur quasi-empirical method

      X_ALPHA2 = X_ALPHA * X_ALPHA
      X_ALPHA3 = X_ALPHA * X_ALPHA * X_ALPHA

      SMALL_G = 0.0
      LARGE_G = 0.0

      IF ( ALPHV .LT. 2.6d0 ) THEN

! FSB Calculate SMALL_G for sigma_g = 2.0

         QF1 = XXA0 + XXA1 * ALPHV
         QF2 = XXA2 * ALPHV2
         QF3 = XXA3 * ALPHV3 
         SMALL_G = QF1 + QF2 + QF3 ! valid at sigma_g = 2.0
         
! FSB adjust SMALL_G for values less than 2.0 
!     The smallest sigma_g allowed is 1.05 
         QQF1 = CXG0 + CXG1 * ALPHV  
         QQF2 = CXG2 * ALPHV2
         QQF3 = CXG3 * ALPHV3
         QQSUM = QQF1 + QQF2 + QQF3
         FAC1 = 1.052632 * ( 2.0d0 - SIGMA_G) 
         QQSUM = MAX( 0.0D0, QQSUM) 
         QQCORR =   (1.0d0 - FAC1)  + FAC1 * QQSUM 
         SMALL_G = SMALL_G *  QQCORR   
         G = SMALL_G

      ELSE IF ( ALPHV .LT. 100.0 ) THEN

         QF1 =  YYA0 + YYA1 * X_ALPHA  
         QF2 = YYA2 * X_ALPHA2
         QF3 = YYA3 * X_ALPHA3 
         LARGE_G = QF1 + QF2 + QF3
         G = LARGE_G

      ELSE

         ! Large sphere limit from fit to Irvine (1965).

         G = 1.124484D0 - 1.153869D-1 * NR

      END IF

! *** write diagnostic output

!!      write(30,*)  SIGMA_G, ALPHV, X_ALPHA, SMALL_G, LARGE_G
      

      RETURN
      END SUBROUTINE FAST_OPTICS

end MODULE module_twoway_ra_cam
