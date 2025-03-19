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
module megan_fx
    
      USE ASX_DATA_MOD, ONLY : Grid_Data
      USE HGRD_DEFN, ONLY: NCOLS,NROWS
! Aggregated code from MEGAN3.0 by Francis S.Binkowski 
! with minor modifications where noted below.
! Implemented for CMAQ 5.4 and proofed by Jeff Willison.

      IMPLICIT NONE
      
      REAL , PARAMETER :: CONVERTWM2TOUMOLM2S = 4.5 ,                   &
                          SOLARCONSTANT = 1367. ,                        &
                          WATERAIRRATIO = 18.016/28.97
                                              ! Ratio between water and air molecules
 
! Canopy characteristics for MEGCAN canopy types
      INTEGER , PARAMETER :: NRTYP = 6          ! Number of canopy types
      INTEGER , PARAMETER :: NRCHA = 17         ! Number of canopy characteristics
! 16 variables are assigned for each canopy type
! 1  = canopy depth
! 2  = leaf width
! 3  = leaf length
! 4  = canopy height
! 5  = scattering coefficient for PPFD
! 6  = scattering coefficient for near IR
! 7  = reflection coefficient for diffuse PPFD
! 8  = reflection coefficient for diffuse near IR
! 9  = clustering coefficient (accounts for leaf clumping influence on mean
!    projected leaf area in the direction of the suns beam)
! 10 = leaf IR emissivity
! 11 = leaf stomata and cuticle factor: 1=hypostomatous, 2=amphistomatous,
!     1.25=hypostomatous but with some transpiration through cuticle
! 12 = daytime temperature lapse rate (K m-1)
! 13 = nighttime temperature lapse rate (K m-1)
! 14 = warm (>283K) canopy total humidity change (Pa)
! 15 = cool (>= 283K) canopy total humidity change (Pa)
! 16 = normalized canopy depth where wind is negligible
! 17 = canopy transparency
!
! Six canopy types currently used in MEGCAN:
! 1  = Needleleaf trees
! 2  = Tropical forest trees,
! 3  = Temperate broadleaf trees
! 4  = shrubs
! 5  = herbaceous
! 6  = crops
 
      REAL , DIMENSION(NRCHA,NRTYP) :: Canopychar = reshape((/16.,16., &
                                      16.,1.,0.5,1.,0.005,0.05,0.05,   &
                                      0.015,0.01,0.02,0.1,0.1,0.1,0.1, &
                                      0.15,0.15,24.,24.,24.,2.,0.5,1.0,&
                                      0.2,0.2,0.2,0.2,0.2,0.2,0.8,0.8, &
                                      0.8,0.8,0.8,0.8,0.057,0.057,     &
                                      0.057,0.057,0.057,0.057,0.389,   &
                                      0.389,0.389,0.389,0.389,0.389,   &
                                      0.85,1.1,0.9,0.85,0.7,0.65,0.95, &
                                      0.95,0.95,0.95,0.95,0.95,1.25,   &
                                      1.25,1.25,1.,1.25,1.25,0.06,0.06,&
                                      0.06,0.06,0.06,0.06,-0.06,-0.06, &
                                      -0.06,-0.06,-0.06,-0.06,700.,    &
                                      700.,700.,700.,700.,700.,150.,   &
                                      150.,150.,150.,150.,150.,0.7,0.7,&
                                      0.7,0.7,0.7,0.7,0.2,0.2,0.2,0.2, &
                                      0.2,0.2/),SHAPE=(/NRCHA,NRTYP/), &
                                      ORDER=(/2,1/))
 
!   for Soil NOx 
! =======================================================================
!  MEGSEA.EXT
!  This include file contains wilting point information
!  for calculating soil moisture activity factor
!
! Created by Alex Guenther and Ling Huang in March 2017
!=======================================================================

!      INTEGER, PARAMETER :: NrTyp = 6          ! Number of canopy types
      REAL,   PARAMETER  :: d1 = 0.04

!-- WWLT is wilting point (M^3/M^3) (JN90)

!      REAL, PARAMETER  ::    WWLT(16) = (/               &        
!                            0.068, 0.075, 0.114, 0.179,  &
!                            0.155, 0.175, 0.218, 0.250,  &
!                            0.219, 0.283, 0.286, 0.286,  &
!                            0.286, 0.286, 0.286, 0.286   /)           
!     REAL, PARAMETER  ::    WWLT(16) =   (/  
!                            0.068, 0.075, 0.114, 0.179,  & 
!                            0.150, 0.155, 0.175, 0.218,  &
!                            0.250, 0.219, 0.283, 0.286,  &
!                            0.155, 0.286, 0.286, 0.175 /)
!

    !   Based upon  MEGVEA.EXT includes suggestions from CJC

    !=======================================================================
    !  MEGVEA.EXT
    !  This include file contains information required
    !  for running MEGEAV module for calculating emission activity responses

    !  Created by Alex Guenther and Ling Huang in Feb 2017
    !
    ! 
    !=======================================================================



    !Number of emission classes
    INTEGER, PARAMETER :: NCLASS = 19
    INTEGER, PARAMETER :: NEMIS  = NCLASS
    ! number of emission classes

    ! CO2 related emission activity factor parameters
    REAL,PARAMETER :: CO2   = 400.0
    REAL,PARAMETER :: ISmax =   1.344
    REAL,PARAMETER :: CO2h  =   1.4614
    REAL,PARAMETER :: Cstar = 585.0

    ! PSTD
    REAL,PARAMETER :: PSTD = 200

    ! canopy depth emission response
    REAL,PARAMETER :: CCD1 = -0.2
    REAL,PARAMETER :: CCD2 =  1.3

    !Light and temperature emission activity response coefficients for each emission class
    !LDF: light dependent fraction
    REAL           LDF(NCLASS)
    !CT1: temperature coefficient (emission type 1: light dependent)
    REAL           CT1(NCLASS)
    !Cleo: temperature coefficient (emission type 1: light dependent)
    REAL           Cleo(NCLASS)
    !beta: temperature coefficient (emission type 2: light independent)
    REAL           beta(NCLASS)

      DATA    beta(1),LDF(1),CT1(1),Cleo(1)        /  0.13,1.0,95,2  /
      DATA    beta(2),LDF(2),CT1(2),Cleo(2)        /  0.13,1.0,95,2  /
      DATA    beta(3),LDF(3),CT1(3),Cleo(3)        /  0.10,0.6,80,1.83  /
      DATA    beta(4),LDF(4),CT1(4),Cleo(4)        /  0.10,0.9,80,1.83  /
      DATA    beta(5),LDF(5),CT1(5),Cleo(5)        /  0.10,0.2,80,1.83  /
      DATA    beta(6),LDF(6),CT1(6),Cleo(6)        /  0.10,0.4,80,1.83  /
      DATA    beta(7),LDF(7),CT1(7),Cleo(7)        /  0.10,0.1,80,1.83  /
      DATA    beta(8),LDF(8),CT1(8),Cleo(8)        /  0.10,0.0,80,1.83  /
      DATA    beta(9),LDF(9),CT1(9),Cleo(9)        /  0.17,0.5,130,2.37  /
      DATA    beta(10),LDF(10),CT1(10),Cleo(10)    /  0.17,0.4,130,2.37  /
      DATA    beta(11),LDF(11),CT1(11),Cleo(11)    /  0.08,0.8,60,1.6  /
      DATA    beta(12),LDF(12),CT1(12),Cleo(12)    /  0.10,0.2,80,1.83  /
      DATA    beta(13),LDF(13),CT1(13),Cleo(13)    /  0.13,0.8,95,2  /
      DATA    beta(14),LDF(14),CT1(14),Cleo(14)    /  0.13,0.8,95,2  /
      DATA    beta(15),LDF(15),CT1(15),Cleo(15)    /  0.10,0.2,80,1.83  /
      DATA    beta(16),LDF(16),CT1(16),Cleo(16)    /  0.10,0.2,80,1.83  /
      DATA    beta(17),LDF(17),CT1(17),Cleo(17)    /  0.10,0.8,80,1.83  /
      DATA    beta(18),LDF(18),CT1(18),Cleo(18)    /  0.10,0.1,80,1.83  /
      DATA    beta(19),LDF(19),CT1(19),Cleo(19)    /  0.08,1.0,60,1.6  /



    ! Parameters for leaf age algorithm for each emission activity classes
    REAL           Anew(NCLASS)
    REAL           Agro(NCLASS)
    REAL           Amat(NCLASS)
    REAL           Aold(NCLASS)

      DATA    Anew(  1),  Agro(  1),  Amat(  1),  Aold(  1) &
          /  0.05     ,  0.6      ,  1.0    ,  0.9       /

      DATA    Anew(  2),  Agro(  2),  Amat(  2),  Aold(  2) &
          /  0.05     ,  0.6      ,  1.0     , 0.9       /

      DATA    Anew(  3),  Agro(  3),  Amat(  3),  Aold(  3) &
          /  2.0      ,  1.8      ,  1.0     ,  1.05       /

      DATA    Anew(  4),  Agro(  4),  Amat(  4),  Aold(  4) &
          /  2.0      ,  1.8      ,  1.0     ,  1.05       /

      DATA    Anew(  5),  Agro(  5),  Amat(  5),  Aold(  5) &
          /  2.0      ,  1.8      ,  1.0     ,  1.05       /

      DATA    Anew(  6),  Agro(  6),  Amat(  6),  Aold(  6) &
          /  2.0      ,  1.8      ,  1.0     ,  1.05       /

      DATA    Anew(  7),  Agro(  7),  Amat(  7),  Aold(  7) &
          /  2.0      ,  1.8      ,  1.0     ,  1.05       /

      DATA    Anew(  8),  Agro(  8),  Amat(  8),  Aold(  8) &
          /  1.0      ,  1.0      ,  1.0      ,  1.0       /

      DATA    Anew(  9),  Agro(  9),  Amat(  9),  Aold(  9) &
          /  0.4      ,  0.6      ,  1.0     ,  0.95       /

      DATA    Anew( 10),  Agro( 10),  Amat( 10),  Aold( 10) &
          /  0.4      ,  0.6      ,  1.0    ,  0.95       /

      DATA    Anew( 11),  Agro( 11),  Amat( 11),  Aold( 11) &
          /  3.5      ,  3.0      ,  1.0    ,  1.2        /

      DATA    Anew( 12),  Agro( 12),  Amat( 12),  Aold( 12) &
          /  1.0      ,  1.0      ,  1.0    ,  1.0       /

      DATA    Anew( 13),  Agro( 13),  Amat( 13),  Aold( 13) &
          /  1.0      ,  1.0      ,  1.0    ,  1.0       /

      DATA    Anew( 14),  Agro( 14),  Amat( 14),  Aold( 14) &
          /  1.0      ,  1.0      ,  1.0    ,  1.0       /

      DATA    Anew( 15),  Agro( 15),  Amat( 15),  Aold( 15) &
          /  1.0      ,  1.0      ,  1.0      ,  1.0       /

      DATA    Anew( 16),  Agro( 16),  Amat( 16),  Aold( 16) &
          /  1.0      ,  1.0      ,  1.0      ,  1.0       /

      DATA    Anew( 17),  Agro( 17),  Amat( 17),  Aold( 17) &
          /  1.0      ,  1.0      ,  1.0      ,  1.0       /

      DATA    Anew( 18),  Agro( 18),  Amat( 18),  Aold( 18) &
          /  1.0      ,  1.0      ,  1.0      ,  1.0       /

      DATA    Anew( 19),  Agro( 19),  Amat( 19),  Aold( 19) &
          /  1.0      ,  1.0      ,  1.0      ,  1.0       /


    !stress emission activity response coefficients for each emission class
    !CAQ: coefficient for poor Air Quality stress
    REAL           CAQ(NCLASS)
    !CHW: coefficient for high wind speed stress
    REAL           CHW(NCLASS)
    !CHT: coefficient for high temperature stress
    REAL           CHT(NCLASS)
    !CLT: coefficient for high temperature stress
    REAL           CLT(NCLASS)



      DATA    CAQ(1),CHW(1),CHT(1),CLT(1)           /  1,1,1,1  /
      DATA    CAQ(2),CHW(2),CHT(2),CLT(2)           /  1,1,1,1  /
      DATA    CAQ(3),CHW(3),CHT(3),CLT(3)           /  1,5,1,1  /
      DATA    CAQ(4),CHW(4),CHT(4),CLT(4)           /  5,5,5,5  /
      DATA    CAQ(5),CHW(5),CHT(5),CLT(5)           /  1,5,1,1  /
      DATA    CAQ(6),CHW(6),CHT(6),CLT(6)           /  1,5,1,1  /
      DATA    CAQ(7),CHW(7),CHT(7),CLT(7)           /  1,5,1,1  /
      DATA    CAQ(8),CHW(8),CHT(8),CLT(8)           /  1,1,1,1  /
      DATA    CAQ(9),CHW(9),CHT(9),CLT(9)           /  5,5,5,5  /
      DATA    CAQ(10),CHW(10),CHT(10),CLT(10)       /  5,5,5,5  /
      DATA    CAQ(11),CHW(11),CHT(11),CLT(11)       /  1,1,1,1  /
      DATA    CAQ(12),CHW(12),CHT(12),CLT(12)       /  1,1,1,1  /
      DATA    CAQ(13),CHW(13),CHT(13),CLT(13)       /  1,1,1,1  /
      DATA    CAQ(14),CHW(14),CHT(14),CLT(14)       /  1,1,1,1  /
      DATA    CAQ(15),CHW(15),CHT(15),CLT(15)       /  1,1,1,1  /
      DATA    CAQ(16),CHW(16),CHT(16),CLT(16)       /  1,1,1,1  /
      DATA    CAQ(17),CHW(17),CHT(17),CLT(17)       /  5,5,5,5  /
      DATA    CAQ(18),CHW(18),CHT(18),CLT(18)       /  1,1,1,1  /
      DATA    CAQ(19),CHW(19),CHT(19),CLT(19)       /  1,1,1,1  /

    !TAQ: threshold for poor Air Quality stress (ppm-hours)
    REAL           TAQ(NCLASS)
    !THW: threshold for high wind speed stress (m/s)
    REAL           THW(NCLASS)
    !THT: threshold for high temperature stress (Celsius degree)
    REAL           THT(NCLASS)
    !TLT: threshold for high temperature stress (Celsius degree)
    REAL           TLT(NCLASS)

      DATA    TAQ(1),THW(1),THT(1),TLT(1)           /  20,12,40,10  /
      DATA    TAQ(2),THW(2),THT(2),TLT(2)           /  20,12,40,10  /
      DATA    TAQ(3),THW(3),THT(3),TLT(3)           /  20,12,40,10  /
      DATA    TAQ(4),THW(4),THT(4),TLT(4)           /  20,12,40,10  /
      DATA    TAQ(5),THW(5),THT(5),TLT(5)           /  20,12,40,10  /
      DATA    TAQ(6),THW(6),THT(6),TLT(6)           /  20,12,40,10  /
      DATA    TAQ(7),THW(7),THT(7),TLT(7)           /  20,12,40,10  /
      DATA    TAQ(8),THW(8),THT(8),TLT(8)           /  20,12,40,10  /
      DATA    TAQ(9),THW(9),THT(9),TLT(9)           /  20,12,40,10  /
      DATA    TAQ(10),THW(10),THT(10),TLT(10)       /  20,12,40,10  /
      DATA    TAQ(11),THW(11),THT(11),TLT(11)       /  20,12,40,10  /
      DATA    TAQ(12),THW(12),THT(12),TLT(12)       /  20,12,40,10  /
      DATA    TAQ(13),THW(13),THT(13),TLT(13)       /  20,12,40,10  /
      DATA    TAQ(14),THW(14),THT(14),TLT(14)       /  20,12,40,10  /
      DATA    TAQ(15),THW(15),THT(15),TLT(15)       /  20,12,40,10  /
      DATA    TAQ(16),THW(16),THT(16),TLT(16)       /  20,12,40,10  /
      DATA    TAQ(17),THW(17),THT(17),TLT(17)       /  20,12,40,10  /
      DATA    TAQ(18),THW(18),THT(18),TLT(18)       /  20,12,40,10  /
      DATA    TAQ(19),THW(19),THT(19),TLT(19)       /  20,12,40,10  /


    !stress emission activity delta thresholds for each emission class
    !DTAQ: delta threshold for poor Air Quality stress (ppm-hours)
    REAL           DTAQ(NCLASS)
    !DTHW: delta threshold for high wind speed stress (m/s)
    REAL           DTHW(NCLASS)
    !DTHT: delta threshold for high temperature stress (Celsius degree)
    REAL           DTHT(NCLASS)
    !DTLT: delta threshold for low temperature stress (Celsius degree)
    REAL           DTLT(NCLASS)

      DATA    DTAQ(1),DTHW(1),DTHT(1),DTLT(1)               /  30,8,8,8  /
      DATA    DTAQ(2),DTHW(2),DTHT(2),DTLT(2)               /  30,8,8,8  /
      DATA    DTAQ(3),DTHW(3),DTHT(3),DTLT(3)               /  30,8,8,8  /
      DATA    DTAQ(4),DTHW(4),DTHT(4),DTLT(4)               /  30,8,8,8  /
      DATA    DTAQ(5),DTHW(5),DTHT(5),DTLT(5)               /  30,8,8,8  /
      DATA    DTAQ(6),DTHW(6),DTHT(6),DTLT(6)               /  30,8,8,8  /
      DATA    DTAQ(7),DTHW(7),DTHT(7),DTLT(7)               /  30,8,8,8  /
      DATA    DTAQ(8),DTHW(8),DTHT(8),DTLT(8)               /  30,8,8,8  /
      DATA    DTAQ(9),DTHW(9),DTHT(9),DTLT(9)               /  30,8,8,8  /
      DATA    DTAQ(10),DTHW(10),DTHT(10),DTLT(10)           /  30,8,8,8  /
      DATA    DTAQ(11),DTHW(11),DTHT(11),DTLT(11)           /  30,8,8,8  /
      DATA    DTAQ(12),DTHW(12),DTHT(12),DTLT(12)           /  30,8,8,8  /
      DATA    DTAQ(13),DTHW(13),DTHT(13),DTLT(13)           /  30,8,8,8  /
      DATA    DTAQ(14),DTHW(14),DTHT(14),DTLT(14)           /  30,8,8,8  /
      DATA    DTAQ(15),DTHW(15),DTHT(15),DTLT(15)           /  30,8,8,8  /
      DATA    DTAQ(16),DTHW(16),DTHT(16),DTLT(16)           /  30,8,8,8  /
      DATA    DTAQ(17),DTHW(17),DTHT(17),DTLT(17)           /  30,8,8,8  /
      DATA    DTAQ(18),DTHW(18),DTHT(18),DTLT(18)           /  30,8,8,8  /
      DATA    DTAQ(19),DTHW(19),DTHT(19),DTLT(19)           /  30,8,8,8  /

    ! MEGAN species
    ! Based on Alex Guenther's "Description Class.xlsx" for MEGANv3

!      DATA     MGN_SPC(  1)  / 'ISOP            '/      ! isoprene
!      DATA     MGN_SPC(  2)  / 'MBO             '/      ! MBO
!      DATA     MGN_SPC(  3)  / 'MT_PINE         '/      ! monoterpenes: pines (alpha and beta)
!      DATA     MGN_SPC(  4)  / 'MT_ACYC         '/      ! monoterpenes: acyclic, 3 = (e.g., myrcene, ocimenes)
!      DATA     MGN_SPC(  5)  / 'MT_CAMP         '/      ! monoterpenes: carene, camphene, others
!      DATA     MGN_SPC(  6)  / 'MT_SABI         '/      ! monoterpenes: sabinene, limonene, terpinenes, others
!      DATA     MGN_SPC(  7)  / 'MT_AROM         '/      ! C10 aromatic: cymenes, cymenenes & C8-C13 oxygenated (e.g., camphor)
!      DATA     MGN_SPC(  8)  / 'NO              '/      ! Nitric oxide
!      DATA     MGN_SPC(  9)  / 'SQT_HR          '/      ! Highly reactive SQT (e.g., caryophyllene)
!      DATA     MGN_SPC( 10)  / 'SQT_LR          '/      ! less reactive SQT (e.g., longifolene, copaene) and salates
!      DATA     MGN_SPC( 11)  / 'MEOH            '/      ! methanol
!      DATA     MGN_SPC( 12)  / 'ACTO            '/      ! acetone
!      DATA     MGN_SPC( 13)  / 'ETOH            '/      ! acetaldehyde and ethanol
!      DATA     MGN_SPC( 14)  / 'ACID            '/      ! organic acids: formicacid, acetic acid, pyruvic acid
!      DATA     MGN_SPC( 15)  / 'LVOC            '/      ! C2 to C4 HC (e.g.,ethene, ethane)
!      DATA     MGN_SPC( 16)  / 'OXPROD          '/      ! oxidation products:aldehydes
!      DATA     MGN_SPC( 17)  / 'STRESS          '/      ! Stress compounds(e.g., linalool)
!      DATA     MGN_SPC( 18)  / 'OTHER           '/      ! other VOC (e.g.,indole, pentane, methyl bromide)
!      DATA     MGN_SPC( 19)  / 'CO              '/      ! carbon monoxide




CONTAINS    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        
! ______________ SUBROUTINE get_CBETA_____________
! This subroutine calculates the solar zenith angle , its sine and the eccentricity
!    INPUT variables:
!    JD       current Julian Day.
!    LAT      current latitude [deg]
!    HOUR     current hour [ hr ] 

!    OUTPUT variables:
!    BETA            curent solar zenith angle
!    SINBETA         Sine of zenith angle
!    ECCEBTRICITY    orbital eccentricity (solar distance) [ AU ] 
!  coded by Dr. Francis S.Binkowski on April 2, 2019, modified Abril 4, 2019
!  based upon earlier code and updated to latest algorithms from USNO

!   Reference:
!   The algorithm for the solar position  are from 
!   https://aa.usno.navy.mil/faq/docs/SunApprox.php
!   Us NAVAL OBSERVATORY The acuracy is good for two centuries 
!   before and after 2000 CE. 

      SUBROUTINE get_BETA( JD, LAT, HOUR, BETA, SINBETA, ECCENTRICITY )
      IMPLICIT NONE
!*--CALCBETA575
! INPUTS: 
      REAL, INTENT(IN) :: JD   ! True Julian day of interest
      REAL, INTENT(IN) :: LAT  ! Latitude [ degrees ]
      REAL, INTENT(IN) :: HOUR ! local standard time
      
! OUTPUT:
      REAL, INTENT(OUT) :: BETA ! the zenith angle of the sun at lat, hour in degrees
      REAL, INTENT(OUT) :: SINBETA ! sine of BETA
      REAL, INTENT(OUT) :: ECCENTRICITY ! orbital eccentricity 
                                        ! earth distance from sun {AU}
! LOCAL VARIBLES            
      REAL :: sindelta , cosdelta , a , b ,  D, R, num, den, decl
      REAL :: sinepsilon, cosepsilon, sinlamda, coslamda, RA, EQT, hangle
      REAL :: g, q, L, lamda, epsilon, e, sing, singsq, sin2g, cosg, cos2g, latrad      
      REAL, PARAMETER :: PI = 4.0 * ATAN(1.0), twopi = 2.0 * PI
      REAL, PARAMETER :: twopi_24 = twopi / 24.0 ! eliminates runtime division
      REAL, PARAMETER :: RAD2DEG = 180.0 / PI,  DEG2RAD = 1.0 / RAD2DEG
      REAL, PARAMETER :: JD0 = 2451545.5 ! True Julian day for January 1, 2000
      REAL, PARAMETER :: ONE15 = 1.0  / 15.0        
      REAL, PARAMETER :: RADCONV = RAD2DEG * ONE15       

       
!--------------------------------------------------------------------

      D = JD  - JD0 
      
      g = 357.529 + 0.98560028 * D  ! Mean anomaly of the Sun

      g = MODULO(g,360.0)
      IF ( g.LT.0.0 ) g = g + 360.0
 
      g = deg2RAD * g   ! g in radians now

!     calculate trig functions of g using identities
!     this speeds up the calculations
 
      sing = SIN(g)
      singsq = sing * sing
      cosg  = sqrt( 1 - singsq)
      sin2g = 2.0 * sing * cosg
      cos2g = cosg*cosg - sing*sing

      q = 280.459 + 0.98564736 *  D     ! Mean longitude of the Sun:

! *** now force L to be betweeon 0.0 and 360. degrees
      q = MODULO( q,360.0)
      IF ( q.LT.0.0 ) q = q + 360.0

      lamda = q + 1.915 * sing + 0.020 * sin2g ! apparent longitude of the sun
      lamda = MODULO(lamda,360.0)
      IF ( lamda.LT.0.0 ) lamda = lamda + 360.0     
      
!     epsilon = 23.429 - 4.0E-7 * D  ! obliquity of ecliptic
      epsilon = 23.439 - 3.6e-7 * D  ! obliquity of ecliptic

!   convert to radian
       epsilon   = DEG2RAD * epsilon
       lamda     = DEG2RAD * lamda
      
      sinepsilon = SIN(epsilon)  
      sinlamda   = SIN(lamda)
      coslamda   = SQRT( 1.0 - sinlamda *sinlamda )
            
      sindelta = sinepsilon * sinlamda           ! sine of solar declination
      cosdelta = SQRT(1.0 - sindelta * sindelta) ! cosine of solar declination

      cosepsilon = SQRT( 1.0 - sinepsilon * sinepsilon )      
      
!   calculate Right Ascension of the sun

       num = cosepsilon * sinlamda
       den = coslamda
       RA  = atan2(num,den)

!   Ignore EQT.  It adds at most plus or minus 16 minutes over the year
!      hangle = (twopi * (HOUR-12.0) / 24.0 ) ! hour angle in radians
!   eliminate runtime division to save time
      hangle = (twopi_24 * (HOUR-12.0) ) ! hour angle in radians

      latrad = DEG2RAD * LAT

 
      a = SIN(latrad) * sindelta
      b = COS(Latrad) * cosdelta
      SINBETA = a + b * COS( hangle) 
      
      BETA = ASIN(SINBETA) * RAD2DEG ! [ degrees]
     
!   calculate solar distance [ Astronomical Units ] This does
!     change over the seasons and more importantly over
!     annual and longer time scales because it is a function of
!     the Mean anomaly of the Sun.

      R            = 1.00014 - 0.01671*cosg - 0.00014*cos2g 
      ECCENTRICITY = R    
      RETURN
      END SUBROUTINE  get_BETA
 


     REAL FUNCTION getJD (YEAR,MONTH,DAY)
!
!---COMPUTES THE JULIAN DATE (JD) GIVEN A GREGORIAN CALENDAR
!   DATE (YEAR,MONTH,DAY).

!   REFERENCE:
!   Reda,Ibrahim, and Andreas Afshin, 2008, Solar position algorithm for solar
!    radiation applications, NREL/TP-550-34302, Revised January 2008l, National 
!    Renewable Energy Laboratory, Golden CO. 
!    Coded April 10, 2019 by Dr. Francis S. Binkowski 
 
    REAL YEAR,MONTH,DAY,Y, M, D, A, B,  JD
!
    Y  = YEAR
    M  = MONTH
    D  = DAY

!   The following is from Equation (4) on Page 3 of the reference.
  
    A  = AINT( Y / 100.0)
    B = 2.0 -A +AINT(A/4) 
    
  getJD = AINT( 365.25 *( Y + 4716.0 ) ) + AINT( 30.6001*( M + 1.0) ) + D    &
            + B - 1524.5
!
    RETURN
    END FUNCTION getJD  





!///////////////_______----------------------- 
!              This SUBROUTINE converts a date on the Gregorian
!                 calendar to a day of the year.
! REFERENCE:

! Original  Programmer:   David G. Simpson
!                NASA Goddard Space Flight Center
!                Greenbelt, Maryland  2077  Date:         November 20, 2001


!  Modified April 13, 2019 by Dr Francis S. Binkowski to do only Gregorian years


      SUBROUTINE get_DOY( YEAR, MONTH, DAY, DOY)

      IMPLICIT NONE
!   INPUT:      
      REAL, INTENT(IN)  :: YEAR, MONTH, DAY   ! GREGORIAN DATE                                                    

!   OUTPUT:
      INTEGER, INTENT(OUT)  :: DOY                ! DAY OF the YEAR
      
!   LOCAL:
      
      INTEGER :: Y                               ! year
      INTEGER :: M                               ! month (1-12)
      INTEGER :: D                               ! day of month 
      INTEGER :: K
   
      
      LOGICAL :: LEAP

!   BEGIN CODE:
     
       Y = YEAR
       M = MONTH
       D = DAY
       
      LEAP = .FALSE.
      
!   TEST FOR LEAP YEARS
      
      IF ( MOD(Y,4)   .EQ. 0) LEAP = .TRUE.
      IF ( MOD(Y,100) .EQ. 0) LEAP = .FALSE.
      IF  (MOD(Y,400) .EQ. 0) LEAP = .TRUE.

      IF (LEAP) THEN
         K = 1
      ELSE
         K = 2
      END IF

!   CALCULATE DAY OF THE YEAR using INTEGER arithmetic

      DOY = ( ( 275 * M) / 9 ) - K * ( ( M + 9) / 12 ) + D - 30

      RETURN
      
      END SUBROUTINE get_DOY
      
     SUBROUTINE get_date(YEAR,DOY,MM,DD)
     
!=============WHEN GIVEN A VALID YEAR, YYYY, AND DAY OF THE
!             YEAR, DDD, RETURNS THE MONTH, MM, AND DAY OF THE
!             MONTH, DD.

! REference:
!             SEE ACM ALGORITHM 398, TABLELESS DATE CONVERSION, BY
!            {*filter*} STONE, CACM 13(10):621.ACM 1970; DOI:10.1145/355598.362779. 7. 
!            Summary We have introduced a formalism which allows us to 
!            explicate certain rather gross properties of language ...

!   Modified to f90 by Dr. Francis S. Binkowski on April 13, 2019

!      INPUT:
      REAL, INTENT(in)  ::  YEAR, DOY    ! year and day of year 
      
!      OUTPUT:      
      INTEGER, INTENT(out) :: MM , DD

!     LOCAL:       
      INTEGER              :: T
      INTEGER              ::  YYYY , DDD

!   Start code 

      YYYY = YEAR
      DDD  = DOY 
      T    = 0
      
      IF( MOD(YYYY,4) .EQ. 0)  T = 1 ! test for leap year
      
!-----------THE FOLLOWING STATEMENT IS NECESSARY IF YYYY IS LESS TNAN
!           1900 OR GREATER THAN 2100.

      IF( MOD(YYYY,400) .NE.0 .AND. MOD(YYYY,100) .EQ. 0 ) T = 0
      
      DD = DDD
      
      IF( DDD.GT. 59 + T ) DD = DD + 2 - T
      
      MM =  (  (DD + 91 ) * 100 ) / 3055
      
      DD = ( DD + 91 ) - ( MM * 3055 ) / 100
      
      MM = MM - 2
!----------MM WILL BE CORRECT IFF DDD IS CORRECT FOR YYYY.
      
      RETURN
      
     
      END  SUBROUTINE get_date
      
     
      
      
!   These subroutines are from MEGCAN
 
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   SUBROUTINE GaussianDist
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
 
      SUBROUTINE GAUSSIANDIST(Distgauss,Layers)
 
      IMPLICIT NONE
!*--GAUSSIANDIST91
 
      INTEGER , INTENT(IN) :: Layers
 
      REAL , DIMENSION(Layers) , INTENT(OUT) :: Distgauss
 
! local variables
      INTEGER :: i
!----------------------------------------------------------------
 
      IF ( Layers.EQ.1 ) THEN
         Distgauss(1) = 0.5
      ELSEIF ( Layers.EQ.3 ) THEN
         Distgauss(1) = 0.112702
         Distgauss(2) = 0.5
         Distgauss(3) = 0.887298
      ELSEIF ( Layers.EQ.5 ) THEN
         Distgauss(1) = 0.0469101
         Distgauss(2) = 0.2307534
         Distgauss(3) = 0.5
         Distgauss(4) = 0.7692465
         Distgauss(5) = 0.9530899
      ELSE
         DO i = 1 , Layers
            Distgauss(i) = (i-0.5)/Layers
         ENDDO
      ENDIF
      
      RETURN
      
      END  SUBROUTINE GAUSSIANDIST
 
 
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   SUBROUTINE SolarFractions
!   Based on actual and potential max solar radiation:
!   Determine the fraction of solar radiation that is
!   diffuse PPFD, direct PPFD, diffuse near IR, direct near IR
!
!   Originally developed by Alex Guenther in 1990s
!   Modified in 2010
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
 
      SUBROUTINE SOLARFRACTIONS(Solar,Maxsolar,Qdiffv,Qbeamv,Qdiffn,    &
                              & Qbeamn)
 
      IMPLICIT NONE
!*--SOLARFRACTIONS137
 
      REAL , INTENT(IN) :: Solar , Maxsolar
      REAL , INTENT(OUT) :: Qdiffv , Qbeamv , Qdiffn , Qbeamn
      REAL :: fracdiff , ppfdfrac , ppfddiffrac , qv , qn
! internal variables
      REAL :: transmis
!-----------------------------------------------------
      IF ( Maxsolar<=0 ) THEN
         transmis = 0.5
      ELSEIF ( Maxsolar<Solar ) THEN
         transmis = 1.0
      ELSE
         transmis = Solar/Maxsolar
      ENDIF
 
!FracDiff is based on Lizaso 2005
      fracdiff = 0.156 + 0.86/(1+EXP(11.1*(transmis-0.53)))
 
!PPFDfrac is based on Goudrian and Laar 1994
      ppfdfrac = 0.55 - transmis*0.12
 
!PPFDdifFrac is based on data in Jacovides 2007
      ppfddiffrac = fracdiff*(1.06+transmis*0.4)
 
! Calculate  Qdiffv,Qbeamv, Qdiffn, Qbeamn in the subroutine
 
      IF ( ppfddiffrac>1.0 ) ppfddiffrac = 1.0
 
      qv = ppfdfrac*Solar
      Qdiffv = qv*ppfddiffrac
      Qbeamv = qv - Qdiffv
      qn = Solar - qv
      Qdiffn = qn*fracdiff
      Qbeamn = qn - Qdiffn
 
      RETURN
 
      END  SUBROUTINE SOLARFRACTIONS
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo!
!   Subroutine CanopyRad
!
!   Canopy light environment model
!   Code originally developed by Alex Guenther in 1990s
!   Coded into FORTRAN by Xuemei Wang
!   based on Spitters et al. (1986), 
!   Goudrian and van Laar (1994), Leuning (1997)
!   Initial code 8-99, 
!   modified 7-2000, 12-2001, 1-2017
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

      SUBROUTINE CanopyRad(Distgauss, Layers, LAI, Sinbeta,   &
                 Qbeamv, Qdiffv, Qbeamn, Qdiffn, Cantype,     &
                 Canopychar, Sunfrac, QbAbsV, QdAbsV, QsAbsV, &
                 QbAbsn, QdAbsn, QsAbsn, SunQv,               &
                 ShadeQv, SunQn, ShadeQn, SunPPFD, ShadePPFD, &
                 NrCha, NrTyp)

      IMPLICIT NONE

! input
      INTEGER,INTENT(IN) :: Layers, NrCha, NrTyp, Cantype
      REAL,INTENT(IN) :: Qbeamv,Qdiffv,Sinbeta,LAI,Qbeamn,Qdiffn
      REAL,DIMENSION(Layers),INTENT(IN) :: Distgauss

! output
      REAL,INTENT(OUT) :: QbAbsV, QbAbsn

      REAL,DIMENSION(Layers),INTENT(OUT) :: ShadePPFD, SunPPFD, & 
                     QdAbsv, QsAbsv, QsAbsn, ShadeQv,  SunQn,  &  
                     QdAbsn, SunQv, ShadeQn, Sunfrac

      REAL,DIMENSION(NrCha,NrTyp), INTENT(IN) :: Canopychar

! internal variables
      INTEGER :: i
      REAL :: ScatV, ScatN, RefldV, RefldN, ReflbV, ReflbN,     & 
              Kb, Kd, KbpV, KbpN, KdpV, KdpN, LAIdepth, Cluster,& 
              QdAbsVL, QsAbsVL, QdAbsNL, QsAbsNL, CANTRAN, LAIadj

! Stefan-boltzman constant  W m-2 K-4
      REAL,PARAMETER :: Sb = 0.0000000567    
      REAL,PARAMETER :: ConvertShadePPFD = 4.6
      REAL,PARAMETER :: ConvertSunPPFD = 4.0
!---------------------------------------------------------------------
        
! adjust LAI for canopy transparency
      CANTRAN = Canopychar(17,Cantype)
      LAIadj = LAI / ( 1 - CANTRAN )

      IF (((Qbeamv  + Qdiffv ) > 0.001) .AND.   & 
          (Sinbeta  > 0.002) .AND.             & 
          (LAIadj  > 0.001)) THEN       ! Daytime

! Scattering coefficients (scatV,scatN), diffuse and beam reflection 
! coefficients (ref..) for visible or near IR
        ScatV   = Canopychar(5,Cantype)
        ScatN   = Canopychar(6,Cantype)
        RefldV  = Canopychar(7,Cantype)
        RefldN  = Canopychar(8,Cantype)
        Cluster = Canopychar(9,Cantype)
!        print*,'cluster',  Cluster
! Extinction coefficients for black leaves for beam (kb) or diffuse (kd)
        Kb = Cluster * 0.5 / Sinbeta 
! (0.5 assumes a spherical leaf angle distribution (0.5 = cos (60 deg))
        Kd = 0.8 * Cluster              
! (0.8 assumes a spherical leaf angle distribution)

        Call CalcExtCoeff(Qbeamv,ScatV,Kb,Kd,ReflbV,KbpV,KdpV,QbAbsV)
        Call CalcExtCoeff(Qbeamn,ScatN,Kb,Kd,ReflbN,KbpN,KdpN,QbAbsn)

        DO i = 1,layers
! LAI depth at this layer
          LAIdepth   = LAIadj  * Distgauss(i)  
!fraction of leaves that are sunlit
          Sunfrac(i) = EXP(-Kb * LAIdepth)  

          Call CalcRadComponents(Qdiffv , Qbeamv , kdpV,  &
                               kbpV, kb, scatV, refldV,  &
                               reflbV, LAIdepth, QdAbsVL, QsAbsVL)

          Call CalcRadComponents(Qdiffn , Qbeamn , kdpN,  &
                               kbpN, kb, scatN, refldN,  &
                               reflbN, LAIdepth, QdAbsNL, QsAbsNL)

       ShadePPFD(i) = (QdAbsVL + QsAbsVL) * ConvertShadePPFD/(1 - scatV)
       SunPPFD(i) = ShadePPFD(i) + (QbAbsV* ConvertSunPPFD/(1 - scatV))
          QdAbsV(i) = QdAbsVL
          QsAbsV(i) = QsAbsVL
          QdAbsn(i) = QdAbsNL
          QsAbsn(i) = QsAbsNL
          ShadeQv(i) = QdAbsVL + QsAbsVL
          SunQv(i)   = ShadeQv(i) + QbAbsV 
          ShadeQn(i) = QdAbsNL + QsAbsNL
          SunQn(i)   = ShadeQn(i) + QbAbsn 
        ENDDO

      ELSE                           ! Night time

        QbAbsV  = 0
        QbAbsn   = 0

        Sunfrac(:)   = 0.2
        SunQn(:)     = 0
        ShadeQn(:)   = 0
        SunQv(:)     = 0
        ShadeQv(:)   = 0
        SunPPFD(:)   = 0
        ShadePPFD(:) = 0
        QdAbsV(:)    = 0
        QsAbsV(:)    = 0
        QdAbsn(:)    = 0
        QsAbsn(:)    = 0

      ENDIF
         
      RETURN
      END SUBROUTINE CanopyRad
 
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   Subroutine CalcExtCoeff
!   Calculate light extinction coefficients
!   Code originally developed by Alex Guenther in 1990s
!   Coded into FORTRAN by Xuemei Wang
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
 
      SUBROUTINE CALCEXTCOEFF(Qbeam,Scat,Kb,Kd,Reflb,Kbp,Kdp,           &
                            & Qbeamabsorb)
 
      IMPLICIT NONE
!*--CALCEXTCOEFF308
 
      REAL , INTENT(IN) :: Qbeam , Scat , Kb , Kd
      REAL , INTENT(OUT) :: Reflb , Kbp , Kdp , Qbeamabsorb
 
! local variables
      REAL :: p
!-------------------------------------------------------------------
 
      p = (1-Scat)**0.5
      Reflb = 1 - EXP((-2*((1-p)/(1+p))*Kb)/(1+Kb))
 
! Extinction coefficients
      Kbp = Kb*p
      Kdp = Kd*p
! Absorbed beam radiation
      Qbeamabsorb = Kb*Qbeam*(1-Scat)

      RETURN
 
      END  SUBROUTINE CALCEXTCOEFF
 
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   Subroutine CalcRadComponents
!   Code originally developed by Alex Guenther in 1990s
!   Coded into FORTRAN by Xuemei Wang
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
 
      SUBROUTINE CALCRADCOMPONENTS(Qdiff,Qbeam,Kdp,Kbp,Kb,Scat,Refld,   &
                                  Reflb,Laidepth,Qdabs,Qsabs)
 
      IMPLICIT NONE
!*--CALCRADCOMPONENTS340
 
      REAL , INTENT(IN) :: Qdiff , Qbeam , Kdp , Kbp , Kb , Scat ,      &
                           Refld , Reflb , Laidepth
      REAL , INTENT(OUT) :: Qdabs , Qsabs
!-------------------------------------------------------------------
 
      Qdabs = Qdiff*Kdp*(1-Refld)*EXP(-Kdp*Laidepth)
      Qsabs = Qbeam*((Kbp*(1-Reflb)*EXP(-Kbp*Laidepth))                 &
              -(Kb*(1-Scat)*EXP(-Kb*Laidepth)))
      
      RETURN
      
      END  SUBROUTINE CALCRADCOMPONENTS
 
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   Subroutine CanopyEB
!
!   Canopy energy balance model for estimating leaf temperature
!   Coded into FORTRAN by Xuemei Wang
!   Code developed by Alex Guenther in 1990s
!   based on Goudrian and Laar (1994) and Leuning (1997)
!   Initial code 8-99, modified 7-2000 and 12-2001
!   Modified in 1-2017 by Alex Guenther and Ling Huang
!   to correct IR balance and atmos. emissivity
!   Note: i denotes an array containing a vertical profile
!         through the canopy with 0 (above canopy conditions)
!         plus 1 to number of canopy layers
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
 
      SUBROUTINE CANOPYEB(Trate,Layers,Distgauss,Canopychar,Cantype,   &
                         Tairk,Humidairpa,Ws,Sunppfd,Shadeppfd,Sunqv,  &
                         Shadeqv,Sunqn,Shadeqn,Sunleaftk,Sunleafsh,    &
                         Sunleaflh,Sunleafir,Shadeleaftk,Shadeleafsh,  &
                         Shadeleaflh,Shadeleafir,Nrcha,Nrtyp,Ws0,      &
                         Tairk0,Humidairpa0)
 
      IMPLICIT NONE
!*--CANOPYEB377
 
! inputs
      INTEGER , INTENT(IN) :: Nrcha , Nrtyp , Layers , Cantype
      REAL , INTENT(IN) :: Trate , Tairk0 , Humidairpa0 , Ws0
      REAL , DIMENSION(Layers) , INTENT(IN) :: Distgauss , Sunqv ,      &
                                       Shadeqv , Sunqn , Shadeqn ,     &
                                       Sunppfd , Shadeppfd
      REAL , DIMENSION(Nrcha,Nrtyp) , INTENT(IN) :: Canopychar
 
! outputs
      REAL , DIMENSION(Layers) , INTENT(OUT) :: Humidairpa , Ws ,      &
                                       Sunleaftk , Sunleafsh ,         &
                                       Sunleaflh , Sunleafir , Tairk , &
                                       Shadeleaftk , Shadeleafsh ,     &
                                       Shadeleaflh , Shadeleafir
 
! local variables
      INTEGER :: i
!     &         Deltah, UnexposedLeafIRin, ExposedLeafIRin, IRin,IRout
      REAL :: cdepth , lwidth , llength , cheight , eps ,               &
               transpiretype , deltah , emissatm , irin , irout
      REAL , DIMENSION(Layers) :: ldepth , wsh, wsh1
!-----------------------------------------------------------------------
 
      cdepth = Canopychar(1,Cantype)
      lwidth = Canopychar(2,Cantype)
      llength = Canopychar(3,Cantype)
      cheight = Canopychar(4,Cantype)
      eps = Canopychar(10,Cantype)
      transpiretype = Canopychar(11,Cantype)
 
      IF ( Tairk0>288 ) THEN
! Pa m-1  (humidity profile for T < 288)
         deltah = Canopychar(14,Cantype)/cheight
      ELSEIF ( Tairk0>278 ) THEN
         deltah = (Canopychar(14,Cantype)-((288-Tairk0)/10)             &
                  *(Canopychar(14,Cantype)-Canopychar(15,Cantype)))     &
                  /cheight
      ELSE
! Pa m-1  (humidity profile for T <278)
         deltah = Canopychar(15,Cantype)/cheight
      ENDIF
 
      ldepth(:) = cdepth*Distgauss(:)
      Tairk(:) = Tairk0 + (Trate*ldepth(:))               ! check this
      Humidairpa(:) = Humidairpa0 + (deltah*ldepth(:))
 
      wsh(:)  = (cheight-ldepth(:)) - (Canopychar(16,Cantype)*cheight)

      wsh1(:) = wsh(:)
      WHERE(wsh(:)<0.001)Wsh1(:) = 0.0009 ! to avoid undef for LOG(negative)

      Ws(:) = (Ws0*LOG(wsh1(:))/LOG(cheight-Canopychar(16,Cantype)*      &
            & cheight))
      WHERE(wsh(:)<0.001)Ws(:) = 0.05
 
      DO i = 1 , Layers
 
! REVISE - Replace UnexposedLeafIR with LeafIR
 
!        IRin     = UnexposedLeafIRin(TairK(i), Eps)
!        ShadeleafIR(i) = 2 * IRin
!        SunleafIR(i) = 0.5*ExposedLeafIRin(HumidairPa0,TairK0)+1.5*IRin
 
! Apparent atmospheric emissivity for clear skies:
! function of water vapor pressure (Pa)
! and ambient Temperature (K) based on Brutsaert(1975)
! referenced in Leuning (1997)
         emissatm = 0.642*(Humidairpa(i)/Tairk(i))**(1./7.)
         irin = LEAFIR(Tairk(i),emissatm)
         Shadeleafir(i) = irin
         Sunleafir(i) = irin
 
      ! Sun
         CALL LEAFEB(Sunppfd(i),Sunqv(i)+Sunqn(i),Sunleafir(i),eps,     &
                     transpiretype,lwidth,llength,Tairk(i),Humidairpa(i)&
                     ,Ws(i),Sunleaftk(i),Sunleafsh(i),Sunleaflh(i),     &
                     irout)
 
         Sunleafir(i) = Sunleafir(i) - irout
 
      ! Shade
         CALL LEAFEB(Shadeppfd(i),Shadeqv(i)+Shadeqn(i),Shadeleafir(i), &
                     eps,transpiretype,lwidth,llength,Tairk(i),         &
                     Humidairpa(i),Ws(i),Shadeleaftk(i),Shadeleafsh(i), &
                     Shadeleaflh(i),irout)
 
         Shadeleafir(i) = Shadeleafir(i) - irout
      ENDDO
      
      RETURN
      
      END  SUBROUTINE CANOPYEB
 
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   Subroutine LeafEB
!
!   Leaf energy balance
!   Code originally developed by Alex Guenther in 1990s
!   Coded into FORTRAN by Xuemei Wang
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
 
      SUBROUTINE LEAFEB(Ppfd,Q,Irin,Eps,Transpiretype,Lwidth,Llength,   &
                        Tairk,Humidairpa,Ws,Tleaf,Sh,Lh,Irout)
 
      IMPLICIT NONE
!*--LEAFEB480
 
      REAL , INTENT(IN) :: Eps , Transpiretype , Lwidth , Llength ,     &
                           Ppfd , Q , Irin , Tairk , Humidairpa , Ws
      REAL , INTENT(OUT) :: Irout , Tleaf , Sh , Lh
 
! local variables
 
      INTEGER :: i
!     &        LHairT,Tdelt,Balance,LeafBLC,LeafH,LeafLE,LeafIRout,
      REAL :: humidairkgm3 , ghforced , stomres , iroutairt ,     &
              lathv , ws1 , lhairt , tdelt , balance , gh1 , sh1 , lh1 , e1 ,   &
              irout1 , gh
!----------------------------------------------------
 
      IF ( Ws<=0 ) THEN
         ws1 = 0.001
      ELSE
         ws1 = Ws
      ENDIF
 
      ! Air vapor density kg m-3
      humidairkgm3 = CONVERTHUMIDITYPA2KGM3(Humidairpa,Tairk)
 
      ! Heat convection coefficient (W m-2 K-1) for forced convection.
      ! Nobel page 366
      ghforced = 0.0259/(0.004*((Llength/Ws1)**0.5))
 
      ! Stomatal resistence s m-1
      stomres = RESSC(Ppfd)
 
! REVISE - Replace LeafIRout with LeafIR
!      IRoutairT = LeafIROut(tairK, eps)
!      iroutairt = LEAFIR(Tairk+tdelt,Eps)
       IRoutairT = LeafIR(TairK, Eps)

 
      ! Latent heat of vaporization (J Kg-1)
      lathv = LHV(Tairk)
 
      ! Latent heat flux
      lhairt = LEAFLE(Tairk,humidairkgm3,lathv,ghforced,stomres,        &
               Transpiretype)
 
      e1 = (Q+Irin-iroutairt-lhairt)
      IF ( e1.EQ.0. ) e1 = -1.
 
      tdelt = 1.
      balance = 10.
      DO i = 1 , 10
         IF ( ABS(balance)>2 ) THEN
          ! Boundary layer conductance
            gh1 = LEAFBLC(ghforced,tdelt,Llength)
          ! Convective heat flux
            sh1 = LEAFH(tdelt,gh1)
          ! Latent heat of vaporization (J Kg-1)
            lathv = LHV(Tairk+tdelt)
            Lh = LEAFLE(Tairk+tdelt,humidairkgm3,lathv,gh1,stomres,     &
                 Transpiretype)
            lh1 = Lh - lhairt
 
! REVISE - Replace LeafIROut with LeafIR
!          IRout  = LeafIROut(TairK + Tdelt, Eps)
            Irout = LEAFIR(Tairk+tdelt,Eps)
            irout1 = Irout - iroutairt
            tdelt = e1/((sh1+lh1+irout1)/tdelt)
            balance = Q + Irin - Irout - sh1 - Lh
         ENDIF
      ENDDO
 
      IF ( tdelt>10 ) tdelt = 10
      IF ( tdelt<-10 ) tdelt = -10
 
      Tleaf = Tairk + tdelt
      gh = LEAFBLC(ghforced,Tleaf-Tairk,Llength)
      Sh = LEAFH(Tleaf-Tairk,gh)
      Lh = LEAFLE(Tleaf,humidairkgm3,lathv,gh,stomres,Transpiretype)
 
! REVISE - Replace LeafIROut with LeafIR
!      IRout = LeafIROut(Tleaf, Eps)
      Irout = LEAFIR(Tleaf,Eps)
      
      RETURN
      
      END  SUBROUTINE LEAFEB
 
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   FUNCTION Calcbeta
!   Calculates the solar zenith angle
!   Code originally developed by Alex Guenther in 1990s
!   Coded into FORTRAN by Xuemei Wang
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
 
      FUNCTION CALCBETA(Day,Lat,Hour)
 
      IMPLICIT NONE
!*--CALCBETA575
 
      REAL :: Day
 
      REAL :: Hour , Lat , sindelta , cosdelta , a , b , sinbeta ,      &
              CALCBETA
      REAL , PARAMETER :: PI = 3.14159 , RPI180 = 57.29578
!--------------------------------------------------------------------
      !sindelta = -SIN(0.40907*COS(6.28*(Day+10)/(365)))
      sindelta = -SIN(0.40907)*COS(6.28*(Day+10)/(365))
      cosdelta = (1-sindelta**2.)**0.5
 
      a = SIN(Lat/RPI180)*sindelta
      b = COS(Lat/RPI180)*cosdelta
      sinbeta = a + b*COS(2*PI*(Hour-12)/24)
      CALCBETA = ASIN(sinbeta)*57.29578
 
      END FUNCTION CALCBETA
 
! The following is the original code for eccentricity.
 
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   FUNCTION CalcEccentricity
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
 
      FUNCTION CALCECCENTRICITY(Day)
 
      IMPLICIT NONE
!*--CALCECCENTRICITY605
      REAL :: Day
      REAL :: CALCECCENTRICITY
!----------------------------------------------------------------
 
      CALCECCENTRICITY = 1 + 0.033*COS(2*3.14*(Day-10)/365)
 
      END  FUNCTION CALCECCENTRICITY
 
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   FUNCTION WaterVapPres
!
!   Convert water mixing ratio (kg/kg) to water vapor pressure
!   (Pa or Kpa depending on units of input )
!   Mixing ratio (kg/kg), temp (C), pressure (KPa)
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
 
      FUNCTION WATERVAPPRES(Dens,Pres,Waterairratio)
 
      IMPLICIT NONE
!*--WATERVAPPRES627
      REAL :: Dens , Pres , WATERVAPPRES , Waterairratio
!----------------------------------------------------------------
 
      WATERVAPPRES = (Dens/(Dens+Waterairratio))*Pres
 
      END FUNCTION WATERVAPPRES
 
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   FUNCTION Stability
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
 
      FUNCTION STABILITY(Canopychar,Cantype,Solar,Nrcha,Nrtyp)
 
      IMPLICIT NONE
!*--STABILITY644
      INTEGER :: Cantype , Nrcha , Nrtyp
      REAL :: Solar , trateboundary , STABILITY
      REAL , DIMENSION(Nrcha,Nrtyp) :: Canopychar
!----------------------------------------------------------------
 
      trateboundary = 500
 
      IF ( Solar>trateboundary ) THEN
            ! Daytime temperature lapse rate
         STABILITY = Canopychar(12,Cantype)
      ELSEIF ( Solar>0 ) THEN
         STABILITY = Canopychar(12,Cantype)                             &
                     - ((trateboundary-Solar)/trateboundary)            &
                     *(Canopychar(12,Cantype)-Canopychar(13,Cantype))
      ELSE
            ! Nightime temperature lapse rate
         STABILITY = Canopychar(13,Cantype)
      ENDIF
 
      END  FUNCTION STABILITY
 
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   FUNCTION ConvertHumidityPa2kgm3
!
!   Saturation vapor density  (kg/m3)
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
 
      FUNCTION CONVERTHUMIDITYPA2KGM3(Pa,Tk)
 
      IMPLICIT NONE
!*--CONVERTHUMIDITYPA2KGM3677
      REAL :: CONVERTHUMIDITYPA2KGM3 , Pa , Tk
!--------------------------------------------------------------------
 
      CONVERTHUMIDITYPA2KGM3 = 0.002165*Pa/Tk
 
      END FUNCTION CONVERTHUMIDITYPA2KGM3
 
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   FUNCTION ResSC
!
!   Leaf stomatal cond. resistance s m-1
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
 
      FUNCTION RESSC(Par)
 
      IMPLICIT NONE
!*--RESSC696
      REAL :: Par , scadj , RESSC
!----------------------------------------------------------------
 
      scadj = ((0.0027*1.066*Par)/((1+0.0027*0.0027*Par**2.)**0.5))
 
      IF ( scadj<0.1 ) THEN
         RESSC = 2000.0
      ELSE
         RESSC = 200.0/scadj
      ENDIF
 
      END  FUNCTION RESSC
 
 
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   FUNCTION LeafIR
!
!   Calculate IR transfer between leaf and air
!   Added by Alex Guenther and Ling Huang to replace previous
!   MEGAN2.1 IR balance functions
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
 
      FUNCTION LEAFIR(Tk,Eps)
 
      IMPLICIT NONE
!*--LEAFIR723
      REAL :: Eps , Tk , LEAFIR
! Stefan-boltzman constant  W m-2 K-4
      REAL , PARAMETER :: SB = 0.0000000567
!----------------------------------------------------------------
 
      LEAFIR = Eps*SB*(2*(Tk**4.))
 
      END FUNCTION LEAFIR
 
 
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   FUNCTION LHV
!
!   Latent Heat of vaporization(J Kg-1) from Stull p641
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
 
      FUNCTION LHV(Tk)
 
      IMPLICIT NONE
!*--LHV745
      REAL :: Tk , LHV
!----------------------------------------------------------------
 
! REVISE - Replace 273 with 273.15
!      LHV = 2501000 - (2370 * (Tk - 273))
      LHV = 2501000 - (2370*(Tk-273.15))
 
      END  FUNCTION LHV
 
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   FUNCTION LeafLE
!
!   Latent energy term in Energy balance
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
 
      FUNCTION LEAFLE(Tleaf,Ambvap,Lathv,Gh,Stomres,Transpiretype)
 
      IMPLICIT NONE
!*--LEAFLE766
      REAL :: Tleaf , Ambvap , Lathv , Gh , Stomres , Transpiretype ,   &
              leafres , vapdeficit , LEAFLE , le
!----------------------------------------------------------------
 
      leafres = (1/(1.075*(Gh/1231))) + Stomres
      vapdeficit = (SVDTK(Tleaf)-Ambvap)
 
! Latent heat of vap (J Kg-1) * vap deficit(Kg m-3) /
!                 leaf resistence (s m-1)
      le = Transpiretype*(1/leafres)*Lathv*vapdeficit
      IF ( le<0 ) THEN
         LEAFLE = 0
      ELSE
         LEAFLE = le
      ENDIF
 
      END FUNCTION LEAFLE
 
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   FUNCTION LeafBLC
!
!   Boundary layer conductance
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
 
      FUNCTION LEAFBLC(Ghforced,Tdelta,Llength)
 
      IMPLICIT NONE
!*--LEAFBLC796
      REAL :: Ghforced , Tdelta , Llength , ghfree , LEAFBLC
!----------------------------------------------------------------
 
! This is based on Leuning 1995 p.1198 except using molecular
! conductivity (.00253 W m-1 K-1 Stull p 640) instead of molecular
! diffusivity so that you end up with a heat convection coefficient
! (W m-2 K-1) instead of a conductance for free convection
 
      IF ( Tdelta>=0 ) THEN
         ghfree = 0.5*0.00253*((160000000*Tdelta/(Llength**3.))**0.25)  &
                  /Llength
      ELSE
         ghfree = 0
      ENDIF
      LEAFBLC = Ghforced + ghfree
 
      END  FUNCTION LEAFBLC
 
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   FUNCTION LeafH
!
!   Convective energy term in Energy balance (W m-2 heat flux
!      from both sides of leaf)
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
 
      FUNCTION LEAFH(Tdelta,Gh)
 
      IMPLICIT NONE
!*--LEAFH827
      REAL :: Tdelta , Gh , LEAFH
!----------------------------------------------------------------
 
! 2 sides X conductance X Temperature gradient
      LEAFH = 2.0 * Gh * Tdelta
 
      END FUNCTION LEAFH
 
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   FUNCTION SvdTk
!
!   Saturation vapor density  (kg/m3)
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
 
      FUNCTION SVDTK(Tk)
 
      IMPLICIT NONE
!*--SVDTK847
      REAL :: Tk , svp , SVDTK
!----------------------------------------------------------------
 
! Saturation vapor pressure (millibars)
      svp = 10**((-2937.4/Tk)-(4.9283*LOG10(Tk))+23.5518)
      SVDTK = 0.2165*svp/Tk
 
      END  FUNCTION SVDTK


    !   These subroutines were in file megvea.f

    !----------------------------------------------------------------
    !
    !   SUBROUTINE GAMMA_CD
    !       Emission response to canopy depath
    !----------------------------------------------------------------
    SUBROUTINE GAMMA_CD(NCOLS,NROWS,Layers,LAI,CDEA)

        IMPLICIT NONE
        ! input
        INTEGER,INTENT(IN)                             :: NCOLS,NROWS,Layers
        REAL,DIMENSION(NCOLS,NROWS),INTENT(IN)         :: LAI
        ! output
        REAL,DIMENSION(NCOLS,NROWS,Layers),INTENT(OUT) :: CDEA

        ! local
        REAL,DIMENSION(Layers) :: Cdepth
        REAL                   :: LAIdepth
        INTEGER                 :: I,J,K

        IF ( Layers .EQ. 5 ) THEN
            Cdepth (1)   = 0.0469101
            Cdepth (2)   = 0.2307534
            Cdepth (3)   = 0.5
            Cdepth (4)   = 0.7692465
            Cdepth (5)   = 0.9530899
        ELSE
            DO K = 1,Layers
                Cdepth(K) =(K - 0.5) /Layers
            END DO
        ENDIF

        DO K = 1, Layers
        DO J = 1, NROWS
        DO I = 1, NCOLS
            LAIdepth = MIN( LAI(I,J) * Cdepth(K), 3.0 )
            CDEA(I,J,K) = CCD1 * LAIdepth + CCD2
        END DO
        END DO
        END DO

        RETURN

    END SUBROUTINE GAMMA_CD
    !----------------------------------------------------------------

    !----------------------------------------------------------------
    !
    !   FUNCTION GAMTLD
    !       EA Temperature response (light dependent emission)
    !----------------------------------------------------------------
    FUNCTION GAMTLD(T1,T24,S)

        IMPLICIT NONE
        REAL,PARAMETER :: Ct2 = 230
        INTEGER        :: S
        REAL           :: T1,T24,T240,Topt, X, Eopt, GAMTLD

        T240 = T24

        IF (T1 < 260.0) THEN
            GAMTLD = 0.0
        ELSE
            ! Temperature at which maximum emission occurs
            Topt = 312.5 + 0.6 * (T240 - 297.0)
            X    = ((1.0 / Topt) - (1.0 / T1)) / 0.00831
            ! Maximum emission (relative to emission at 30 C)
            Eopt = Cleo(S) * EXP(0.05 * (T24 - 297.0)) *          &
                  Exp(0.05*(T240-297.0))

            GAMTLD= Eopt * Ct2 * Exp(Ct1(S) * X) /                &
                  (Ct2 - Ct1(S) * (1.0 - EXP(Ct2 * X)))
        ENDIF

    END FUNCTION GAMTLD
    !----------------------------------------------------------------

    !----------------------------------------------------------------
    !
    !   FUNCTION GAMTLI
    !       EA Temperature response (light independent emission)
    !----------------------------------------------------------------


    FUNCTION GAMTLI(temp,S)

        IMPLICIT NONE

        REAL           :: temp, GAMTLI
        REAL,PARAMETER :: Ts = 303.15
        INTEGER        :: S

        GAMTLI = exp( beta(S)*(temp-Ts) )

    END FUNCTION GAMTLI
    !----------------------------------------------------------------


    !----------------------------------------------------------------
    !
    !   FUNCTION GAMP
    !       EA Light response
    !----------------------------------------------------------------

    FUNCTION GAMP(PPFD1,PPFD24)

        IMPLICIT NONE
        REAL            :: PPFD1, PPFD24, Alpha, C1, GAMP

        IF (PPFD24 < 0.01) THEN
            GAMP= 0.0
        ELSE
            Alpha  = 0.004
            !        C1     = 0.0468 * EXP(0.0005 * (PPFD24 - PSTD))
            !     &          * (PPFD24 ** 0.6)
            C1 = 1.03
            !        GAMP= (Alpha * C1 * PPFD1) / ((1 + Alpha**2. * PPFD1**2.)**0.5)
            
!   use SQRT her for clarity and efficiency
            
            GAMP= (Alpha * C1 * PPFD1) / SQRT(1.0 + Alpha**2 * PPFD1**2)
        ENDIF

    END FUNCTION GAMP

    !----------------------------------------------------------------
    !
    !   SUBROUTINE GAMMA_HT
    !   EA response to high temperature
    !
    !----------------------------------------------------------------

    SUBROUTINE GAMMA_HT(NCOLS, NROWS, S, MaxT, GAMHT)

        IMPLICIT NONE
        ! input
        INTEGER,INTENT(IN)                            :: NCOLS, NROWS, S
        REAL,DIMENSION(NCOLS,NROWS),INTENT(IN)        :: MaxT
        ! output
        REAL,DIMENSION(NCOLS,NROWS),INTENT(OUT)       :: GAMHT
        ! local
        INTEGER     :: I,J
        REAL        :: THTK, t1

        DO J = 1,NROWS
        DO I = 1,NCOLS
            THTK = 273.15 + THT(S)
            t1 = THTK + DTHT(S)
            IF (MaxT(I,J) <= THTK) THEN
                GAMHT(I,J) = 1.0
            ELSE IF ( MaxT(I,J) < t1) THEN
                GAMHT(I,J) = 1.0 + (CHT(S) - 1.0)* (MaxT(I,J) -  THTK)/DTHT(S)
            ELSE
                GAMHT(I,J) = CHT(S)
            ENDIF
        END DO
        END DO

        RETURN
    END SUBROUTINE GAMMA_HT
    !----------------------------------------------------------------

    !----------------------------------------------------------------
    !
    !   SUBROUTINE GAMMA_LT
    !   EA response to low temperature
    !
    !----------------------------------------------------------------

    SUBROUTINE GAMMA_LT(NCOLS, NROWS, S, MinT, GAMLT)

        IMPLICIT NONE
        ! input
        INTEGER,INTENT(IN)                       :: NCOLS, NROWS, S
        REAL,DIMENSION(NCOLS,NROWS),INTENT(IN)   :: MinT
        ! output
        REAL,DIMENSION(NCOLS,NROWS),INTENT(OUT)  :: GAMLT
        ! local
        INTEGER      :: I,J
        REAL         :: TLTK, t1

        DO J = 1,NROWS
        DO I = 1,NCOLS
            TLTK = 273.15 + TLT(S)
            t1 = TLTK - DTLT(S)
            IF (MinT(I,J) >= TLTK) THEN
                GAMLT(I,J) = 1.0
            ELSE IF ( MinT(I,J) > t1) THEN
                GAMLT(I,J) = 1.0 + (CLT(S) - 1.0)* (TLTK - MinT(I,J))/DTLT(S)
            ELSE
                GAMLT(I,J) = CLT(S)
            ENDIF
        END DO
        END DO

        RETURN
    END SUBROUTINE GAMMA_LT
    !----------------------------------------------------------------


    !----------------------------------------------------------------
    !
    !   SUBROUTINE GAMMA_HW
    !   EA response to high wind speed
    !
    !----------------------------------------------------------------

    SUBROUTINE GAMMA_HW(NCOLS, NROWS, S, MaxWS, GAMHW)

        IMPLICIT NONE
        ! input
        INTEGER,INTENT(IN)                        :: NCOLS, NROWS, S
        REAL,DIMENSION(NCOLS,NROWS),INTENT(IN)    :: MaxWS
        ! output
        REAL,DIMENSION(NCOLS,NROWS),INTENT(OUT)   :: GAMHW
        ! local
        INTEGER     :: I,J
        REAL        :: t1

        DO J = 1,NROWS
        DO I = 1,NCOLS
            t1 = THW(S) + DTHW(S)
            IF (MaxWS(I,J) <= THW(S)) THEN
                GAMHW(I,J) = 1.0
            ELSE IF ( MaxWS(I,J) < t1) THEN
                GAMHW(I,J) = 1.0 + (CHW(S) - 1.0)* (MaxWs(I,J) - THW(S))/ DTHW(S)
            ELSE
                GAMHW(I,J) = CHW(S)
            ENDIF
        END DO
        END DO

        RETURN
    END SUBROUTINE GAMMA_HW
    !----------------------------------------------------------------



    !----------------------------------------------------------------
    !
    !   SUBROUTINE GAMMA_AQ
    !   EA response to air quality
    !
    !----------------------------------------------------------------

    SUBROUTINE GAMMA_AQ(NCOLS, NROWS, S, AQI, GAMAQ)

        IMPLICIT NONE
        ! input
        INTEGER, INTENT(IN)                       :: NCOLS, NROWS, S
        REAL, DIMENSION(NCOLS,NROWS),INTENT(IN)   :: AQI
        ! output
        REAL, DIMENSION(NCOLS,NROWS),INTENT(OUT)   :: GAMAQ
        ! local
        INTEGER    :: I,J
        REAL       :: t1


        DO J = 1, NROWS
        DO I = 1, NCOLS
            t1 = TAQ(S) + DTAQ(S)
            IF (AQI(I,J) <= TAQ(S)) THEN
                GAMAQ(I,J) = 1.0
            ELSE IF ( AQI(I,J) < t1) THEN
                GAMAQ(I,J) = 1.0 + (CAQ(S) - 1.0)* (AQI(I,J) - TAQ(S))/DTAQ(S)
            ELSE
                GAMAQ(I,J) = CAQ(S)
            ENDIF
        END DO
        END DO

        RETURN
    END SUBROUTINE GAMMA_AQ
    !-----------------------------------------------------------------------

    !-----------------------------------------------------------------------
    !
    ! Subroutine GAMMA_CO2
    !-----------------------------------------------------------------------
    !From Alex Guenther 2017-03-11
    SUBROUTINE GAMMA_CO2( NCOLS, NROWS, GAMCO2 )

        IMPLICIT NONE

        INTEGER, INTENT(IN)                         :: NCOLS, NROWS
        REAL,DIMENSION(NCOLS,NROWS),INTENT(OUT)     :: GAMCO2

        ! local
        REAL    :: Ci, CO2temp, cxxx, cyyy
        INTEGER :: C, R

        CO2temp = CO2
        Ci      = 0.7 * CO2

        IF ( CO2 .EQ. 400.0 ) THEN
            GAMCO2 = 1.0
        ELSE
            DO R = 1, NROWS
            DO C = 1, NCOLS
!   set common factors for pipeline                 
            
                cxxx =  Ci**CO2h
                cyyy =  Cstar**CO2h
                
     !      GAMCO2 = ISmax- ((ISmax * Ci**CO2h ) / (Cstar**CO2h + Ci **CO2h))
                GAMCO2(C,R) = ISmax- ((ISmax * cxxx) / (cyyy + cxxx))
                
            END DO
            END DO
        END IF

        RETURN

    END SUBROUTINE GAMMA_CO2

    !-----------------------------------------------------------------------


    !-----------------------------------------------------------------------
    !
    ! Subroutine GAMMA_LAIbidir(gam_LAIbidir,LAI)
    !-----------------------------------------------------------------------
    !From Alex Guenther 2010-01-26
    !If lai < 2 Then
    !gammaLAIbidir= 0.5 * lai
    !ElseIf lai <= 6 Then
    !gammaLAIbidir= 1 - 0.0625 * (lai - 2)
    !Else
    !gammaLAIbidir= 0.75
    !End If
    !
    !     SUBROUTINE GAMMA_LAIbidir returns the gam_LAIbidir values
    !    Xuemei Wang-2010-01-28
    !
    !-----------------------------------------------------------------------
    SUBROUTINE GAMMA_LAIbidir(NCOLS, NROWS,LAI,GAMBD)

        IMPLICIT NONE
        ! input
        INTEGER,INTENT(IN)                          :: NCOLS, NROWS
        REAL,DIMENSION(NCOLS, NROWS),INTENT(IN)     ::  LAI

        ! output
        REAL,DIMENSION(NCOLS, NROWS),INTENT(OUT)    :: GAMBD

        ! local
        INTEGER                                     :: I,J

        DO J = 1, NROWS
        DO I = 1, NCOLS

            IF(LAI(I,J) < 2) THEN
                GAMBD(I,J) =  0.5 * LAI(I,J)
            ELSEIF (LAI(I,J) .LE. 6 ) THEN
                GAMBD(I,J) = 1 - 0.0625 * (LAI(I,J) - 2)
            ELSE
                GAMBD(I,J) = 0.75
            ENDIF

        END DO
        END DO

        RETURN
    END SUBROUTINE GAMMA_LAIbidir
    !----------------------------------------------------------------


    !----------------------------------------------------------------
    !
    !   SUBROUTINE GAMLA
    !
    !     EA leaf age response
    !----------------------------------------------------------------
    !
    !       GAMLA = Fnew*Anew + Fgro*Agro + Fmat*Amat + Fold*Aold
    !       where Fnew = new foliage fraction
    !             Fgro = growing foliage fraction
    !             Fmat = mature foliage fraction
    !             Fold = old foliage fraction
    !             Anew = emission activity for new foliage
    !             Agro = emission activity for growing foliage
    !             Amat = emission activity for mature foliage
    !             Aold = emission activity for old foliage
    !           Age class fractions are determined from LAI changes
    !             LAIc = current LAI
    !             LAIp = past LAI
    !             t  = length of the time step (days)
    !             ti = days between budbreak and emission induction
    !             tm = days between budbreak and peak emission
    !             Tt = average above canopy temperature (K)
    !
    !----------------------------------------------------------------

    SUBROUTINE GAMMA_A( NCOLS, NROWS, S,                      &
          LAIp, LAIc, D_TEMP, GAMLA )

        USE RUNTIME_VARS, ONLY: USE_MEGAN_LAI

        IMPLICIT NONE
 ! input
        INTEGER,INTENT(IN)                       :: NCOLS,NROWS, S
        REAL,DIMENSION(NCOLS,NROWS),INTENT(IN)   :: D_TEMP, LAIp, LAIc
 ! output
        REAL,DIMENSION(NCOLS,NROWS),INTENT(OUT)  :: GAMLA

        INTEGER :: C, R

        REAL :: Fnew, Fgro, Fmat, Fold
        REAL :: ti,tm
        REAL :: Tt

        REAL       :: TSTLEN  

        !Time step of LAI data
        if (USE_MEGAN_LAI) THEN
          TSTLEN = 8.0 ! 8 daily from MEGAN file
        else
          TSTLEN = 1.0 ! 1 Daily from soilout/metcro
        end if

        !---------------------------------------------------
        ! local parameter arrays
        
        DO R = 1, NROWS
        DO C = 1, NCOLS

            Tt = D_TEMP(C,R)

            !... Calculate foliage fraction

            IF (LAIp(C,R) .LT. LAIc(C,R)) THEN

                !        Calculate ti and tm
                IF (Tt .LE. 303.0) THEN
                    ti = 5.0 + 0.7*(300-Tt)
                ELSE
                    ti = 2.9
                END IF
                tm = 2.3*ti

                !       Calculate Fnew and Fmat, then Fgro and Fold
                !       Fnew
                IF (ti .GE. TSTLEN) THEN
                    Fnew = 1.0 - (LAIp(C,R)/LAIc(C,R))
                ELSE
                    Fnew = (ti/TSTLEN) * ( 1-(LAIp(C,R)/LAIc(C,R)) )
                END IF

                !       Fmat
                IF (tm .GE. TSTLEN) THEN
                    Fmat = LAIp(C,R)/LAIc(C,R)
                ELSE
                    Fmat = (LAIp(C,R)/LAIc(C,R)) +                             &
                          ( (TSTLEN-tm)/TSTLEN ) * ( 1-(LAIp(C,R)/LAIc(C,R)) )
                END IF

                Fgro = 1.0 - Fnew - Fmat
                Fold = 0.0

            ELSE IF (LAIp(C,R) .EQ. LAIc(C,R)) THEN

                Fnew = 0.0
                Fgro = 0.1
                Fmat = 0.8
                Fold = 0.1

            ELSE IF (LAIp(C,R) .GT. LAIc(C,R)) THEN

                Fnew = 0.0
                Fgro = 0.0
                Fold = ( LAIp(C,R)-LAIc(C,R) ) / LAIp(C,R)
                Fmat = 1-Fold

            END IF

            !...  Calculate GAMLA
            GAMLA(C,R) = Fnew*Anew(S) + Fgro*Agro(S) + Fmat*Amat(S) + Fold*Aold(S)
        
        END DO
        END DO

        RETURN
    END SUBROUTINE GAMMA_A

!   subroutines used by MEGSEA

!=======================================================================
!=======================================================================
      REAL FUNCTION FERTLZ_ADJ( DATE, LAT )

!***********************************************************************
!  DESCRIPTION:
!    This internal function computes a fertilizer adjustment factor
!    for the given date in yyyyddd format. If it is not growing 
!    season, the adjustment factor is 0; otherwise, it ranges from
!    0.0 to 1.0.
!
!  CALL:
!    GROWSEASON
!
!  HISTORY:
!    07/21/11 : Imported from SMOKE-BEIS v3.14 and modified  (Tan)
!***********************************************************************

      IMPLICIT NONE
            
!.... Function arguments
      INTEGER, INTENT(IN) :: DATE
      REAL,    INTENT(IN) :: LAT

!.... Local variables
      INTEGER  GDAY, GLEN

      CHARACTER(256)  MESG         ! message buffer
!-----------------------------------------------------------------------------

      CALL GROWSEASON( DATE, LAT, GDAY, GLEN )
          FERTLZ_ADJ = 0. !INITIALIZE
      IF( GDAY == 0 ) THEN
          FERTLZ_ADJ = 0.
      ELSE IF( GDAY >= 1 .AND. GDAY < 30 ) THEN
          ! first month of growing season
          FERTLZ_ADJ = 1.
      ELSE IF( GDAY >= 30 .AND. GDAY <= 366) THEN
          ! later month of growing season
          FERTLZ_ADJ = 1. + 30. / FLOAT(GLEN) - FLOAT(GDAY) / FLOAT(GLEN)
      ENDIF

!******************  FORMAT  STATEMENTS   ******************************
94010 FORMAT( A, F10.2, 1X, A, I3, ',', I3 )


      RETURN

      END FUNCTION FERTLZ_ADJ
!=======================================================================
!=======================================================================


!=======================================================================
!=======================================================================
      REAL FUNCTION VEG_ADJ( LAI )

!***********************************************************************
!  DESCRIPTION
!    This internal function computes a vegetation adjustment factor
!    based on LAIv.  See Yienger and Levy 1995
!    VEG_ADJ = (EXP(-0.24*LAIv)+EXP(-0.0525*LAIv))*0.5 
!
!  CALL
!    NONE
!
!  HISTORY:
!***********************************************************************

      IMPLICIT NONE
      
!...  Function arguments
      REAL,    INTENT(IN) :: LAI

!-----------------------------------------------------------------------------
      VEG_ADJ = 0.0
      VEG_ADJ = (EXP(-0.24*LAI)+EXP(-0.0525*LAI))*0.5 

!******************  FORMAT  STATEMENTS   ******************************

      RETURN
      END FUNCTION VEG_ADJ
!=======================================================================
!=======================================================================
            


!=======================================================================
!=======================================================================

!=======================================================================
!=======================================================================
      SUBROUTINE GROWSEASON ( DATE, LAT, GDAY, GLEN )

!***********************************************************************
!  DESCRIPTION
!    This internal function computes the day of the growing season
!    corresponding to the given date in yyyyddd format.
!
!  CALL
!    G2J
!
!  HISTORY:
!    07/21/11 : Imported from SMOKE-BEIS v3.14 and modified  (Tan)
!               Variation of growing season depends on latitude
!               (Guenther)
!    04/22/2019 Converted to 90 format and redid error repotring
!               modified G2j to be completely internal. 
!               DR. FRANCIS S. BINKOWSKI, IE, UNC-CHAPEL HILL
!***********************************************************************

      IMPLICIT NONE

!.......  Function arguments
      INTEGER, INTENT(IN)  :: DATE
      REAL,    INTENT(IN)  :: LAT
      INTEGER, INTENT(OUT) :: GDAY, GLEN 
!.......  External functions

!.......  Local parameters
      INTEGER            :: GSEASON_START
      INTEGER            :: GSEASON_END

!.......  Local variables
      INTEGER  YEAR, MONTH, DAY
      INTEGER  JDAY
      INTEGER  GSJULIAN_START
      INTEGER  GSJULIAN_END
!-----------------------------------------------------------------------------
!   NOTE: The use of "julian Day" to describe the day of tHE year is
!     technically incorrect. 

 ! The Julian Day Number (JDN) is the integer assigned to a whole solar 
 ! day in the Julian day count starting from noon Universal time, with 
 ! Julian day number 0 assigned to the day starting at noon on Monday, 
 ! January 1, 4713 BCE, proleptic Julian calendar (November 24, 4714 BCE, 
 ! in the proleptic Gregorian calendar), a date at which three 
 ! multi-year cycles started (which are: Indiction, Solar, and Lunar cycles)
 !  and which preceded any dates in recorded history. 
 !
 !  For example for January 1st, 2000 CE  at 00:00:00.0 UT1 the Julian Day 
 !  is 2451544.500000 according to  the U.S. Naval Observatory.



      YEAR = INT( FLOAT( DATE ) / 1000. )
      JDAY = DATE - YEAR * 1000

      IF ( LAT .LE. 23.0 .AND. LAT .GE. -23.0 ) THEN
      ! tropical regions, year round
         GSEASON_START = 0101
         GSEASON_END   = 1231

         GSJULIAN_START = G2J(YEAR, GSEASON_START)
         GSJULIAN_END   = G2J(YEAR, GSEASON_END)
         GDAY = JDAY - GSJULIAN_START + 1
         GLEN = GSJULIAN_END - GSJULIAN_START + 1
      ELSE IF ( LAT .LT. -23.0 ) THEN
      ! southern hemisphere
         IF ( LAT .LT. -60.0 ) THEN
         ! antarctic start = 0 end = 0, no growing
            GDAY = 0
            GLEN = 0
         ELSE
         ! southern hemisphere temperate, NOV, DEC, JAN-MAY
            IF (JDAY .GE. 1101 .AND. JDAY .LE. 1231 ) THEN
              GSEASON_START = 1101
              GSEASON_END   = 1231

              GSJULIAN_START = G2J(YEAR, GSEASON_START)
              GSJULIAN_END   = G2J(YEAR, GSEASON_END)
              GDAY = JDAY - GSJULIAN_START + 1
            ELSE IF (JDAY .GE. 0101 .AND. JDAY .LE. 0531) THEN
              GSEASON_START = 0101
              GSEASON_END   = 0531

              GSJULIAN_START = G2J(YEAR, GSEASON_START)
              GSJULIAN_END   = G2J(YEAR, GSEASON_END)
              GDAY = JDAY - GSJULIAN_START + 1 + 61
            ELSE
              GDAY = 0
            ENDIF
            GLEN = 30 + 31 + G2J(YEAR,0531) - G2J(YEAR,0101) + 1

         ENDIF
      ELSE IF ( LAT .GT. 23.0 ) THEN
      ! northern hemisphere
         IF ( LAT .GT. 65.0 ) THEN
         ! arctic start = 0 end = 0, no growing season
            GDAY = 0
            GLEN = 0
         ELSE
         ! northern hemisphere temperate
         ! start= (lat-23)*4.5            189
         ! end = 365 -((lat-23)*3.3)      226
            GSEASON_START = 0
            GSEASON_END   = 1231

            GSJULIAN_START = 0
            GSJULIAN_END   = G2J(YEAR, GSEASON_END)

            GSJULIAN_START = INT( (LAT-23.0) * 4.5 )
            GSJULIAN_END   = GSJULIAN_END - INT( (LAT-23.0) * 3.3 )
            
            ! UNC added to avoid GDAY excede 366
            IF ( JDAY == 366 .AND. GSJULIAN_START==0 ) GSJULIAN_END = GSJULIAN_END - 1
                        
            IF (JDAY .GE. GSJULIAN_START .AND. JDAY .LE. GSJULIAN_END) THEN
               GDAY = JDAY - GSJULIAN_START + 1
            ELSE
               GDAY = 0
            ENDIF
            GLEN = GSJULIAN_END - GSJULIAN_START + 1
         ENDIF
      ENDIF
  
     RETURN

      END SUBROUTINE GROWSEASON

!=======================================================================
!=======================================================================
      
!   This is a modified version of function G2J.

      
      INTEGER FUNCTION G2J( YYYY, MMDD )
      IMPLICIT NONE

!.......  Function arguments
      INTEGER, INTENT(IN) :: YYYY
      INTEGER, INTENT(IN) :: MMDD


!.......  Local parameters
      INTEGER :: MM
      INTEGER :: DD
      INTEGER :: K
      INTEGER :: DOY
      LOGICAL :: LEAP


      MM = INT( FLOAT( MMDD ) / 100.0 )
      DD = MMDD - MM * 100

!   use internal code to get G2j

 !     G2J = JULIAN( YYYY, MM , DD )

!   The following code is taken from NASA subroutine get_DOY 
! Original  Programmer:   David G. Simpson
!            NASA Goddard Space Flight Center
!            Greenbelt, Maryland  2077  Date:         November 20, 2001
!  Modified April 13, 2019 by Dr Francis S. Binkowski to do only Gregorian years


          LEAP = .FALSE.
      
!   TEST FOR LEAP YEARS
      
      IF ( MOD(YYYY,4)   .EQ. 0) LEAP = .TRUE.
      IF ( MOD(YYYY,100) .EQ. 0) LEAP = .FALSE.
      IF  (MOD(YYYY,400) .EQ. 0) LEAP = .TRUE.

      IF (LEAP) THEN
         K = 1
      ELSE
         K = 2
      END IF

!   CALCULATE DAY OF THE YEAR

      DOY = ( ( 275 * MM) / 9 ) - K * ( ( MM + 9) / 12 ) + DD - 30
      
      G2J = DOY  

      END FUNCTION G2J

!=======================================================================



      SUBROUTINE SOILNOX( JDATE, JTIME, NX, NY,            &
                      TA, LSOIL, ISLTYP, SOILM, SOILT,     &
                      LAIc, LAT,                           &
                      PRECADJ,                             &
                      CFNO, CFNOG )

!***********************************************************************
!  DESCRIPTION:
!  
!     Uses new NO algorithm NO = Normalized*Tadj*Padj*Fadj*Cadj
!     to estimate NO emissions 
!     Information needed to estimate NO emissions
!     Julian Day          (integer)    JDATE
!     Surface Temperature (MCIP field) TA    (K)
!     Soil Moisture       (MCIP field) SOILM (M**3/M**3) (LSOIL)
!          (ratio of volume of water per volume of soil)
!     Soil Temperature    (MCIP field) SOILT (K)         (LSOIL)
!     Soil Type           (MCIP field) ISLTYP            (LSOIL)
!
!     saturation values for soil types (constants)       (LSOIL)
!     FOR PX Version, the Temperature adjustment factor accounts for wet and dry soils
!                and  the precipitation adjustment factor accounts for saturated soils
!     FOR the non-PX version, the basic algorithm remains with a temperature adjustment factor (dry soil)
!                     and no adjustment for saturated soils
!
!
!     The following arrays are updated after each call to SOILNOX
!     PULTYPE   type of NO emission pulse 
!     PULSEDATE julian date for the beginning of an NO pulse 
!     PULSETIME        time for the beginning of an NO pulse
!  
!     The calculation are based on the following paper by J.J. Yienger and H. Levy II
!     J.J. Yienger and H. Levy II, Journal of Geophysical Research, vol 100,11447-11464,1995
!
!     The Temperature Adjustment Factor is based on section 4.2 for wet and dry soils with
!       the following modification (PX version):
!       Instead of classifying soils as either 'wet' or 'dry', the wet and dry adjustment is 
!       calculated at each grid cell.  A linear interpolation between the wet and dry adjustment
!       factor is made using the relative amount of soil moisture in the top layer (1cm)
!       as the interpolating factor.  The relative amount of soil moisture is determined by
!       taking the MCIP soil moisture field and dividing by the saturation value defined for each
!       soil type in the PX version of MCIP
!       the soil temperature is used in PX version
!
!     The Precipation Adjustment factor is based on section 4.1 with the following modifications.
!       The rainrate is computed from the MCIP directly using a 24 hr daily total. 
!       THe types of Pulses as described in YL95 were used to estimate the NO emission
!       rate.  
!
!    Also see the following paper for more information:
!    Proceedings of the Air and Waste Management Association/U.S. Environmental Protection
!    Agency EMission Inventory Conference, Raleigh October 26-28, 1999 Raleigh NC
!    by Tom Pierce and Lucille Bender       
!
!    REFERENCES
!
!    JACQUEMIN B. AND NOILHAN J. (1990), BOUND.-LAYER METEOROL., 52, 93-134.
!    J.J. Yienger and H. Levy II, Journal of Geophysical Research, vol 100,11447-11464,1995
!    T. Pierce and L. Bender, Examining the Temporal Variability of Ammonia and Nitric Oxide Emissions from Agricultural Processes
!       Proceedings of the Air and Waste Management Association/U.S. Environmental Protection
!        Agency EMission Inventory Conference, Raleigh October 26-28, 1999 Raleigh NC
!
!  PRECONDITIONS REQUIRED:
!     Normalized NO emissions, Surface Temperature, Soil Moisture, Soil type,
!     NO emission pulse type, soil moisture from previous time step, julian date
!     of NO emission pulse start, time of NO emission pulse start,
!     soil type, SOIL TYPES, Land use data
!
!  SUBROUTINES AND FUNCTIONS CALLED (directly or indirectly):
!     FERTILIZER_ADJ computes fertlizer adjustment factor
!     VEG_ADJ        computes vegatation adjustment factor
!     GROWSEASON     computes day of growing season
!     
!  REVISION  HISTORY:
!    10/01 : Prototype by GAP
!    10/03 : modified transition to non growing season for jul-oct of the year
!    08/04 : Converted to SMOKE code style by C. Seppanen
!    07/21/11 : Imported form SMOKE-BEIS v3.14 for MEGAN v2.10
!    MAY 13, 2019 made inot f90 format and  improved efficiency - 
! 
!***********************************************************************

!        USE SOILNOX_FX

        IMPLICIT NONE
        

!.........  ARGUMENTS and their descriptions
        INTEGER, INTENT (IN)  :: JDATE   !  current simulation date (YYYYDDD)
        INTEGER, INTENT (IN)  :: JTIME   !  current simulation time (HHMMSS)
        INTEGER, INTENT (IN)  :: NX      !  no. columns
        INTEGER, INTENT (IN)  :: NY      !  no. rows

        REAL, INTENT (IN)  ::  TA      ( NX, NY )    !  air temperature (K)
        REAL, INTENT (IN)  ::  SOILM   ( NX, NY )    !  soil moisture (m3/m3)
        REAL, INTENT (IN)  ::  SOILT   ( NX, NY )    !  soil temperature (K)
        REAL, INTENT (IN)  ::  PRECADJ ( NX, NY )    !  precip adjustment
        REAL, INTENT (IN)  ::  LAIc    ( NX, NY )    !  soil temperature (K)
        REAL, INTENT (IN)  ::  LAT     ( NX, NY )    !  Latitude
        REAL, INTENT (IN OUT)  ::  CFNO    ( NX, NY )    !  NO correction factor
        REAL, INTENT (IN OUT)  ::  CFNOG   ( NX, NY )    !  NO correction factor for grass
        
        INTEGER, INTENT (IN)  ::  ISLTYP  ( NX, NY )    !  soil type

        LOGICAL, INTENT (IN) :: LSOIL              ! true: using PX version of MCIP
        
!.........  Local ARRAYS
! Saturation values for 11 soil types from pxpbl.F  (MCIP PX version)
!       PLEIM-XIU LAND-SURFACE AND PBL MODEL (PX-LSM)
! See JACQUEMIN B. AND NOILHAN J. (1990), BOUND.-LAYER METEOROL., 52, 93-134.
        INTEGER, PARAMETER :: MAXSTYPES = 16
!        REAL, PARAMETER    :: SATURATION( MAXSTYPES )     =  (/   &       
!                              0.395, 0.410, 0.435, 0.485,         &
!                              0.451, 0.420, 0.477, 0.476,         &
!                              0.426, 0.482, 0.482            /)       

!.........  SCRATCH LOCAL VARIABLES and their descriptions:
        INTEGER       ::   R, C, L      ! counters
        INTEGER       ::   SOILCAT      ! soil category
        
        REAL          ::   CF           ! NO correction factor
        REAL          ::   CFG          ! NO correction factor for grasslands
        REAL          ::  TAIR         ! surface temperature
        REAL          ::   TSOI         ! soil temperature
        REAL          ::   CFNOWET, CFNODRY, RATIO, FAC1, FAC2 
        REAL, PARAMETER ::  const1 = (1. / 3.0 )  * (1.0 / 30.0)
        REAL, PARAMETER ::  const2 =EXP(-0.103 * 30.0)
        CHARACTER(256)  MESG         ! message buffer
        
        CHARACTER(16) :: PROGNAME = 'SOILNOX'   !  program name

!***********************************************************************

 
!.....  Loop through cells
        DO R = 1, NY
        DO C = 1, NX

          TAIR = TA( C, R )         ! unit in degree K

!.......  Check max bounds for temperature

          IF (TAIR > 315.0 ) THEN
              TAIR = 315.0
          END IF

!.......  CFNOG
          IF( TAIR > 303.00 ) TAIR = 303.00

          IF ( TAIR > 268.8690 ) THEN  
              CFG = EXP( 0.04686 * TAIR - 14.30579 ) ! grass (from BEIS2)
          ELSE
              CFG = 0.0
          END IF

          CFNOG(C,R) = CFG
          
!   pre calculate common factors


              FAC2 = const2

!.......  CFNO
          IF( .NOT. LSOIL ) THEN
          ! no soil

             TSOI = 0.72 * TAIR + 82.28
             IF (TSOI <= 273.16) TSOI = 273.16
             IF (TSOI >= 303.16) TSOI = 303.16
             
              FAC1 = (TSOI- 273.16)

             
              
!             CFNODRY = (1./3.) * (1./30.) * (TSOI-273.16)  ! see YL 1995 Equa 9a p. 11452             
              CFNODRY = const1 * FAC1  ! see YL 1995 Equa 9a p. 11452
            
             IF (TSOI <= 283.16) THEN         ! linear cold case
!                 CFNOWET = (TSOI-273.16)*EXP(-0.103*30.0)*0.28 ! see YL 1995 Equ 7b
                 CFNOWET =  FAC1 * FAC2 * 0.28 ! see YL 1995 Equ 7b
                 
             ELSE                             ! exponential case
!                 CFNOWET = EXP(0.103 * (TSOI-273.16)) *  EXP(-0.103 * 30.0)
                 CFNOWET = EXP(0.103 * FAC1) *  FAC2
             END IF
             CF = 0.5 * CFNOWET + 0.5 * CFNODRY

          ELSE
          ! soil

             TSOI = SOILT( C,R )
             IF (TSOI <= 273.16) TSOI = 273.16
             IF (TSOI >= 303.16) TSOI = 303.16

              FAC1 = (TSOI- 273.16)

!             CFNODRY = (1./3.)*(1./30.)*(TSOI-273.16)  ! see YL 1995 Equa 9a p. 11452
             CFNODRY = const1 * FAC1  ! see YL 1995 Equa 9a p. 11452
                          
             IF (TSOI <= 283.16) THEN         ! linear cold case
!                CFNOWET = (TSOI-273.16)*EXP(-0.103*30.0)*0.28 ! see YL 1995 Equ 7b
                CFNOWET = FAC1 * FAC2 * 0.28 ! see YL 1995 Equ 7b
                
             ELSE                             ! exponential case
!                CFNOWET = EXP(0.103 * (TSOI-273.16)) * EXP(-0.103 * 30.0)
                 CFNOWET = EXP(0.103 * FAC1 ) * FAC2
               
             END IF

             SOILCAT = INT( ISLTYP( C,R ) )
             IF( SOILCAT > 0 .AND. SOILCAT <= MAXSTYPES ) THEN
                 IF(Grid_Data%WSAT(C,R) .eq. 0) THEN
                  ! first ldesid diag call. Do nothing.
                  CF = 0. 
                 ELSE 
                  RATIO = SOILM( C,R ) / Grid_Data%WSAT( C,R )
                  CF = RATIO*CFNOWET + (1.0 - RATIO ) * CFNODRY
                 END IF
             ELSE
             
                 CF = 0.0
                 
             END IF

          END IF  ! Endif LSOIL


!          CFNO(C,R) = CF *                                      &
!                     FERTLZ_ADJ( JDATE, LAT(C,R) ) *           &
!                     VEG_ADJ( LAIc(C,R) ) * PRECADJ(C,R)

          CFNO(C,R) = CF *                                     &
                     FERTLZ_ADJ( JDATE, LAT(C,R) ) *           &
                     VEG_ADJ( LAIc(C,R) ) * PRECADJ(C,R)

          if(cfno(c,r) .lt. 0) then
             cfno(c,r) = 0
          end if

        END DO  ! loop over columns
        END DO  ! loop over rows

        RETURN

        END SUBROUTINE SOILNOX
        
! ///////////////////////////////////////////////////////////////////////////        
        
      SUBROUTINE MEGCANOPY(  YEAR, LAYERS, DOY, ZTIME,     &
                 LAT, LONG, LAIc, TEMP, PAR, WIND, PRES, QV, CTF,        &
                 ShadeleafTK, SunleafTK, SunFrac, SunPPFD, ShadePPFD  )        !

! ----------------------------------------------------------------------------
!   START subroutine MEGCANOPY
!     Coded March 19, 2019 by Dr. Francis S. Binkowski
!     modifies 05/07/2019 by   to correct loop order
!     Institue for the Environment UNC, Chapel Hill

!   Based upon 	PROGRAM MEGCAN

!   Based on code initiated by Alex Guenther in 1990s
!   Coded in FORTRAN by Xuemei Wang--Nov. 2007
!   Revised by Alex Guenther and Ling Huang in Feb 2017
!   to correct, modify, and update the code and make it
!   a stand-alone program
!
!*****************************************************************
!
!*****************************************************************
!   Input varibles
!   
!   NROWs, NCOLS         grid location
!   LAYERS               number of vertical layers in canopy
!   DOY                  day of the year
!   Lat                  Latitude
!   Long                 Longitude
!   ZTIME                Hour of the day (UTC/GMT)
!   TEMP                 Temperature [K]
!   PAR                  Photosynthetically active radiation [ W/m**2]
!   Wind                 Wind speed [m s-1]
!   Humidity             Relative humidity [%]
!   Cantype              Defines set of canopy characteristics
!   LAI                  Leaf area index [m2 per m2 ground area]
!   Pres                 Pressure [Pa]
!
!*****************************************************************
! Variables used
!   PPFD           Incoming photosynthetic active radiation [umol/m2/s1]
!   PPFDfrac             Fraction of solar radiation that is PPFD
!   Solar                Solar radiation [W/m2]
!   Maxsolar             Maximum of solar radiation
!   Sinbeta              Sine of solar angle above horizon
!   Beta                 Solar angle above horizon
!   TairK0               Above canopy air temperature [K]
!   TairK                Array of canopy air temperature [K]
!   Ws0                  Above canopy wind speed [m/s]
!   Ws                   Array of canopy wind speed [m/s]
!   HumidairPA0          Above canopy ambient humidity [Pa]
!   HumidairPa           Array of canopy ambient humidity in [Pa]
!   Transmis             Transmission of PPFD that is diffuse
!   Difffrac             Fraction of PPFD that is diffuse
!   PPFDfrac             Fraction of solar rad that is PPFD
!   Trate			   temperature vertical profile
!   QbAbsV, QbAbsN       Absorbed direct beam visible/near IR
!   QdAbsV, QdAbsN       Absorbed diffuse visible/near IR
!   QsAbsV, QsAbsN       Absorbed scattered visible//near IR
!   QBeamV, QBeamN       Above canopy direct beam visible/near IR
!   QDiffV, QDiffN       Above canopy diffuse visible/near IR
!
! Arrays with values for each canopy layer (vertical profile)
!   SunleafSH            sensible heat flux for sun leaves [W/m2]
!   SunleafLH            latent heat flux for sun leaves [W/m2]
!   SunleafIR            infrared flux for sun leaves [W/m2]
!   ShadeleafSH          sensible heat for shade leaves [W/m2]
!   ShadeleafLH          latent heat flux for shade leaves [W/m2]
!   ShadeleafIR          infrared flux for shade leaves [W/m2]
!   VPgausDis            gaussian weighting factors for distance
!   SunQv                visible radiation on sun leaves
!   ShadeQv              visible radiation on shade leaves
!   SunQn                near IR radiation on sun leaves
!   ShadeQn              near IR radiation on shade leaves
!   sun_ppfd             Array of incoming (NOT absorbed) PPFD on a sun leaf [umol/m2/s]
!   shade_ppfd           Array of incoming (NOT absorbed) PPFD on a shade leaf [umol/m2/s]
!   sun_tk               Array of leaf temperature for sun leaves [K]
!   shade_tk             Array of leaf temperature for shade leaves [K]
!   sun_frac             Array of the fraction of sun leaves. i = 1 is the top canopy layer, 2 is the next layer, etc.

!*****************************************************************
! OUTPUT
! For each time step and location
! Each variable is an array with a value for each canopy layer
!			       (vertical profile)
! i = 1 is the top canopy layer, 2 is the next layer down , etc.
!   ShadeleafTK          leaf temperature for shade leaves [K] (weighted by canopy type)
!   SunleafTK            leaf temperature for sun leaves [K] (weighted by canopy type)
!   SunFrac              fraction of sun leaves (weighted by canopy type)
!   SunPPFD              PPFD on a sun leaf [umol/m2/s] (weighted by canopy type)
!   ShadePPFD            PPFD on a shade leaf [umol/m2/s] (weighted by canopy type)
!
!*****************************************************************
! FUNCTION S
!   Calcbeta             Calculation of solar zenith angle
!   WaterVapPres         Convert water mixing ratio (kg/kg) to water vapor
!   pressure
!   Stability            Temperature lapse rate in canopy
!   get_BETA             solar position 

 !         
      IMPLICIT NONE
     
     
!   INPUT VARIABLES

       INTEGER, INTENT(IN)   :: LAYERS
       REAL, INTENT(IN)      :: YEAR
       REAl, INTENT(IN)      :: DOY
       REAL, INTENT(IN)      :: ZTIME
       REAL, INTENT(IN)      :: LAT ( NCOLS, NROWS )     
       REAL, INTENT(IN)      :: LONG( NCOLS, NROWS )   
       REAL, INTENT(IN)      :: LAIc( NCOLS, NROWS )
       REAL, INTENT(IN)      :: TEMP( NCOLS, NROWS )    
       REAL, INTENT(IN)      :: PAR ( NCOLS, NROWS ) ! comes in as rgrnd from ASX
       REAL, INTENT(IN)      :: WIND( NCOLS, NROWS )
       REAL, INTENT(IN)      :: PRES( NCOLS, NROWS )
       REAL, INTENT(IN)      :: QV  ( NCOLS, NROWS )
       REAL, INTENT(IN)      :: CTF(NrTyp, NCOLS, NROWS ) ! Canopy type factor array

!   OUTPUT VARIABLES 

       REAL, INTENT(OUT)     ::  ShadeleafTK ( NCOLS, NROWS, LAYERS ) 
       REAL, INTENT(OUT)     ::  SunleafTK   ( NCOLS, NROWS, LAYERS ) 
       REAL, INTENT(OUT)     ::  SunFrac     ( NCOLS, NROWS, LAYERS ) 
       REAL, INTENT(OUT)     ::  SunPPFD     ( NCOLS, NROWS, LAYERS ) 
       REAL, INTENT(OUT)     ::  ShadePPFD   ( NCOLS, NROWS, LAYERS ) 

!   LOCAL VARIABLES
      INTEGER                :: I, I_CT, J, MM, DD
      INTEGER                :: IDAY         ! For using original solar method

      REAL                   :: PPFD(NCOLS, NROWS) 
      REAL                   :: TotalCT
      REAL                   :: month, Date, JDAY
      REAL                   :: Sinbeta, Beta, HOUR, DAY
      REAL,DIMENSION(LAYERS) ::  VPgausWt, VPgausDis2,             &
       VPgausDis, VPslwWT,  QdAbsV, QsAbsV, QdAbsn,                &
       QsAbsn, SunQv, ShadeQv, SunQn, ShadeQn,                     &
       TairK, HumidairPa, Ws, SunleafSH, sun_ppfd,shade_ppfd,      &           
       SunleafLH,SunleafIR, ShadeleafSH, sun_tk,shade_tk,sun_frac, &
       ShadeleafLH,ShadeleafIR, sun_ppfd_total, shade_ppfd_total,  &
      sun_tk_total, shade_tk_total, sun_frac_total
  
      REAL :: Solar, Maxsolar, Eccentricity,                       &
              Difffrac, PPFDfrac, QbAbsn,                          &                 
               Trate, Qbeamv,Qdiffv, Qbeamn, Qdiffn,               &
               QbAbsV,Ea1tCanopy, Ea1pCanopy,                      &                 
               TairK0, HumidairPa0, Ws0, SH                         
                       
!      REAL ::  CalcEccentricity,WaterVapPres,                      &      
!               Stability, Calcbeta



!   Start code        

!   calculate the date from Year and day of the year 

      call get_date(YEAR, DOY, MM, DD)
      MONTH = MM ! conver to REAL
      DATE  = DD ! conver to REAL
!   Get authentic  Julian Day Number

      JDAY =  getJD (YEAR,MONTH,Date)
    
       DAY  = DOY 
 
       ppfd=par*4.5*0.45 ! ppfd = par * 4.5
                         ! par = rgrnd * 0.45 

          DO I=1, NCOLS
           DO J=1, NROWS
            SunleafTK(I,J,:)   = TEMP(I,J)
            ShadeleafTK(I,J,:) = TEMP(I,J)
            SunPPFD(I,J,:)     = PPFD(I,J)
            ShadePPFD(I,J,:)   = PPFD(I,J)
            SunFrac(I,J,:)     = 1.0
            TotalCT           = 0.0
            DO I_CT = 1,NRTYP   !canopy types
              TotalCT = TotalCT + CTF(I_CT,I,J) * 0.01
            ENDDO   ! ENDDO I_CT

            IF (TotalCT .GT. 0.0 .AND. LAIc(I,J) .GT. 0.0) THEN
!           only invoke canopy model when both CT and LAI are valid

              sun_ppfd_total     = 0.0
              shade_ppfd_total   = 0.0
              sun_tk_total       = 0.0
              shade_tk_total     = 0.0
              sun_frac_total     = 0.0

              DO I_CT = 1,NRTYP   !canopy types
                IF (CTF(I_CT,I,J) .NE. 0.0) THEN
                sun_ppfd           = 0.0
                shade_ppfd         = 0.0
                sun_tk             = 0.0
                shade_tk           = 0.0
                sun_frac           = 0.0
!            Convert from XXXXXX format to XX.XX (solar hour)
!            HOUR = 0 -> 23.xx
!            Solar hour
               Hour  = ZTIME + LONG(I,J) / 15.0

                IF ( Hour  .LT. 0.0 ) THEN
                  Hour  = Hour  + 24.0
                  Day  = Doy  - 1
                ELSEIF ( Hour  .GT. 24.0 ) THEN
                  Hour  = Hour  - 24.0
                  Day  = Doy  + 1
                ENDIF
!            Solar angle
                Beta   = Calcbeta(Day , Lat(I,J) , Hour )
                Sinbeta    = SIN(Beta  / 57.29578)
                TairK0     = TEMP(I,J)
                Ws0        = WIND(I,J)
!               Solar      = PPFD(I,J)/ConvertWm2toUmolm2s*2
                Solar      = PPFD(I,J)/2.25
                Maxsolar   = Sinbeta  * SolarConstant * CalcEccentricity(Day )
                Call GaussianDist(VPgausDis, Layers)
                Call SolarFractions(Solar, Maxsolar, Qdiffv,Qbeamv,Qdiffn,Qbeamn) 
                Call CanopyRad(VPgausDis, Layers, LAIc(I,J), Sinbeta, Qbeamv, &
                    Qdiffv, Qbeamn, Qdiffn,I_CT ,Canopychar, sun_frac,&
                    QbAbsV, QdAbsV, QsAbsV, QbAbsn, QdAbsn, QsAbsn, SunQv,&
                    ShadeQv, SunQn, ShadeQn, sun_ppfd, shade_ppfd,&
                    NrCha,NrTyp)

                 HumidairPa0  =  WaterVapPres(QV(I,J), PRES(I,J), WaterAirRatio)
                 Trate    =  Stability(Canopychar, I_CT, Solar , NrCha, NrTyp)
                 Call CanopyEB(Trate, Layers, VPgausDis, Canopychar, I_CT,&
                              TairK, HumidairPa, Ws, sun_ppfd,&
                              shade_ppfd, SunQv, ShadeQv, SunQn, ShadeQn,&
                              sun_tk, SunleafSH, SunleafLH, SunleafIR,&
                              shade_tk,ShadeleafSH,ShadeleafLH,ShadeleafIR,&
                              NrCha, NrTyp, Ws0, TairK0, HumidairPa0)
                     sun_ppfd_total(:)   = sun_ppfd_total(:) + &
                                     0.01*CTF(I_CT,I,J)*sun_ppfd(:)
                     shade_ppfd_total(:) = shade_ppfd_total(:) +&
                                     0.01*CTF(I_CT,I,J)*shade_ppfd(:)
                     sun_tk_total(:)     = sun_tk_total(:) +&
                                     0.01*CTF(I_CT,I,J)*sun_tk(:)
                     shade_tk_total(:)   = shade_tk_total(:) +& 
                                     0.01*CTF(I_CT,I,J)*shade_tk(:)
                     sun_frac_total(:)   = sun_frac_total(:) + &
                                     0.01*CTF(I_CT,I,J)*sun_frac(:)
                ENDIF
              ENDDO  ! ENDDO I_CT
              SunleafTK(I,J,:)   = sun_tk_total(:)/TotalCT
              ShadeleafTK(I,J,:) = shade_tk_total(:)/TotalCT
              SunPPFD(I,J,:)     = sun_ppfd_total(:)/TotalCT
              ShadePPFD(I,J,:)   = shade_ppfd_total(:)/TotalCT
              SunFrac(I,J,:)     = sun_frac_total(:)/TotalCT

            ELSEIF( TotalCT .LT. 0.0) THEN

            ELSE
            ! total CT is zero
            SunleafTK(I,J,:)   = TEMP(I,J)
            ShadeleafTK(I,J,:) = TEMP(I,J)
            SunPPFD(I,J,:)     = PPFD(I,J)
            ShadePPFD(I,J,:)   = PPFD(I,J)
            SunFrac(I,J,:)     = 1

            ENDIF
             
           ENDDO   ! ENDDO J
          ENDDO   ! ENDDO I
  RETURN 
      END SUBROUTINE MEGCANOPY 
      
!//////////////////////////////////////////////////////////////////  
    
SUBROUTINE MEGVEA(  LAYERS, JDATE, ZTIME,                &
                    LAIp, LAIc,LDF_in,                              &
                                        GAMSM_in, MaxT, MinT,       &
                    MaxWS,                                          &
!AQI, 
                    D_TEMP, D_PPFD, SUNT, SHAT,         &
                    SUNF, SUNP, SHAP,ER, NON_DIMGARMA  )
! PURPOSE: Calculate Vegetation emission activity (EA) for each emission
!		class as the product of EA for individual drivers
!		calculated by the following functions
!
! Vegetation Emission Activity (EA) algorithm FUNCTIONS
!
!   GAMTLD: EA Temperature response (light dependent emission) 
!   GAMTLI: EA Temperature response (light independent emission)
!   GAMP: EA Light response
!   GAMTP: combines GAMLD, GAMLI, GAMP to get canopy average
!
!   GAMLA: EA leaf age response 
!   GAMBD: EA bidirectional exchange LAI response
!
!   CDEA: Canopy depth emission response
!
!   GAMHW: EA response to high wind storms
!   GAMHT: EA resposne to high temperature
!   GAMLT: EA response to low temperature
!
!   GAMAQ: EA response to air pollution
!
!   GAMCO2: EA CO2 response (only applied to isoprene)
!   GAMSM: EA response to soil moisture (multiplied with LDF)
!
! INCLUDE FILES
!     'PARMS3.EXT'   ! I/O API parameters
!     'IODECL3.EXT'  ! I/O API function declarations
!     'FDESC3.EXT'   ! I/O API file desc. data structures
!     'MEGVEA.EXT'    ! coefficients
!
!  INPUT Files
!	Single value for each location
!		LDF: Light dependent fraction (for categories other than
!		monoterpene, use constant values from MEGVEA.EXT)
!		AQ:  Air Quality indicator
!	Time series for each location
!		LAI: Leaf Area Index 
!		GAMSM: emission activity response to soil moisture
!		MaxT: Daily maximum temperature (K)
!		MinT: Daily minimum temperature (K)
!		MaxWS: Daily mximum wind speed (m/s)
!               D_TEMP: Daily average temperature (K)
!               D_PPFD: Daily averaged PPFD (umol/m2.s)
!	Hourly time series for each canopy layer in each location
!		sunfrac: fraction of leaves that are sunlit
!		SUNT: leaf Temperature of sunlit leaves (K)
!		SUNP: sun leaf PPFD visible light (micromol/m2/s)
!		SHAT: leaf Temperature of shade leaves (K)
!		SHAP: shade leaf PPFD visible light (micromol/m2/s)
!	
!  OUTPUT Files
!	Hourly time series for each location 
!		Emission activity for each of the 20 emission types
!
!
! HISTORY:
!   Based on code initiated by Alex Guenther in 1990s
!   Coded in FORTRAN as
!   MEGAN: Jack Chen 11/04
!   MEGANv2.04: Tan 11/21/06
!   MEGANv2.1: X. Wang 11/04/2007
!       Modified by Julia Lee-Taylor 03/18/2008
!       Modified by Xuemei Wang 09/30/2008
!       Modified by Tan 07/28/2011
!   MEGAN3.0:
!   Alex Guenther and Ling Huang Feb 2017
!     converted program to subroutine

    IMPLICIT NONE
! INPUT VARIABLES
    INTEGER, INTENT(IN)  ::  LAYERS
    INTEGER, INTENT(IN)  ::  JDATE
    REAL, INTENT(IN)     ::  ZTIME
    REAL, INTENT(IN)     ::  LAIp       ( NCOLS, NROWS )
    REAL, INTENT(IN)     ::  LAIc       ( NCOLS, NROWS )
    REAL, INTENT(IN)     ::  LDF_in      ( NCOLS, NROWS , 4 ) !only 4 use maps
    REAL, INTENT(IN)     ::  GAMSM_in       ( NCOLS, NROWS )
    REAL, INTENT(IN)     ::  MaxT        ( NCOLS, NROWS )
    REAL, INTENT(IN)     ::  MinT        ( NCOLS, NROWS )
    REAL, INTENT(IN)     ::  MaxWS       ( NCOLS, NROWS )
!    REAL, INTENT(IN)     ::  AQI         ( NCOLS, NROWS )
    REAL, INTENT(IN)     ::  D_TEMP      ( NCOLS, NROWS )
    REAL, INTENT(IN)     ::  D_PPFD      ( NCOLS, NROWS ) ! comes in as rgrnd
    REAL, INTENT(IN)     ::  SUNT        ( NCOLS, NROWS, LAYERS )
    REAL, INTENT(IN)     ::  SHAT        ( NCOLS, NROWS, LAYERS )
    REAL, INTENT(IN)     ::  SUNF        ( NCOLS, NROWS, LAYERS )
    REAL, INTENT(IN)     ::  SUNP        ( NCOLS, NROWS, LAYERS )
    REAL, INTENT(IN)     ::  SHAP       ( NCOLS, NROWS, LAYERS )

! OUTPUT VARIABLES
    REAL, INTENT(OUT)     :: ER(NCOLS, NROWS )
    REAL, INTENT(OUT)     :: NON_DIMGARMA (NCLASS,NCOLS, NROWS)

 !LOCAL VARIABLES

    LOGICAL, PARAMETER    :: GAMBD_YN  = .false.

    LOGICAL, PARAMETER    :: GAMAQ_YN  = .false.
! For the CMAQ implementation of MEGAN  we refer to soil moisture 
! at layer 2, which is 1 meter for PX and 0.5 m for NOAH.
! Keep this in mind when enabling the GAMSM stress.
    LOGICAL, PARAMETER    :: GAMSM_YN  = .false. 
    LOGICAL, PARAMETER    :: GAMHT_YN  = .false.
    LOGICAL, PARAMETER    :: GAMLT_YN  = .false.
    LOGICAL, PARAMETER    :: GAMHW_YN  = .false.
    LOGICAL, PARAMETER    :: GAMCO2_YN = .false.

    REAL                  :: VPGWT(LAYERS), Ea1L, Ea2L

    REAL  :: CDEA   ( NCOLS, NROWS, LAYERS ) ! Emission response to canopy depth


    REAL  :: YEAR
    REAl  :: DOY
    REAL  :: GAMLA  ( NCOLS, NROWS )     ! EA leaf age response
    REAL  :: GAMAQ  ( NCOLS, NROWS )     ! EA response to air pollution
    REAL  :: GAMBD  ( NCOLS, NROWS )     ! EA bidirectional exchange LAI response
    REAL  :: GAMHT  ( NCOLS, NROWS )     ! EA response to high temperature
    REAL  :: GAMLT  ( NCOLS, NROWS )     ! EA response to low temperature

    REAL  :: GAMHW  ( NCOLS, NROWS )     ! EA response to high wind speed
    REAL  :: GAMSM  ( NCOLS, NROWS )     ! EA response to soil moisture
    REAL  :: GAMCO2 ( NCOLS, NROWS )     ! EA response to CO2
    REAL  :: GAMTP                       ! combines GAMLD, GAMLI, GAMP to get canopy average

    REAL  :: LDFMAP ( NCOLS, NROWS )     ! light depenedent fraction map


    REAL ::  SUM1, SUM2




    ! loop indices
    !INTEGER :: IDATE, ITIME
    INTEGER :: S, T, I, J, K 
!         YEAR   = FLOAT( MOD( JDATE, 1000 ) )
!         DOY    = FLOAT( JDATE / 1000  )       
! these are backwards I think. don't need them anyway

            

    ! EA response to canopy temperature/light

    IF ( Layers .EQ. 5 ) THEN
        VPGWT(1) = 0.1184635
        VPGWT(2) = 0.2393144
        VPGWT(3) = 0.284444444
        VPGWT(4) = 0.2393144
        VPGWT(5) = 0.1184635
    ELSE
        DO K = 1,Layers
            VPGWT(K) = 1.0 / FLOAT( Layers )
        END DO
    ENDIF

! First process Factors independent of species emission classes S :
    
    ! Emission response to canopy depth
    CALL GAMMA_CD( NCOLS, NROWS, Layers, LAIc, CDEA )

    ! EA bidirectional exchange LAI response
    IF ( GAMBD_YN ) THEN
        CALL GAMMA_LAIbidir(NCOLS, NROWS, LAIc, GAMBD)
    ELSE
        GAMBD = 1.0
    ENDIF

    IF ( GAMCO2_YN ) THEN
        CALL GAMMA_CO2(NCOLS, NROWS, GAMCO2)
    ELSE
        GAMCO2 = 1.0
    ENDIF

!  Now process all factors dependent on S:

    DO S = 1,NEMIS  ! Loop over all the emission classes

        IF ( S .EQ. 3 .OR. S .EQ. 4 .OR. S .EQ. 5 .OR. S .EQ. 6 ) THEN
!    otherwise use the input values.           
            LDFMAP = LDF_IN(:,:,S-2) ! only LDF 3, 4, 5, and 6 in file
        ELSE
!  For these species,  Read LDF from previous MEGVEA.EXT 
            LDFMAP = LDF(S)

        ENDIF


        ! leaf age activity factor:  dependent upon S
        CALL GAMMA_A( NCOLS, NROWS, S, LAIp, LAIc, D_TEMP, GAMLA )

        ! emission activity response to air quality

        IF ( GAMAQ_YN ) THEN
!            CALL GAMMA_AQ(NCOLS, NROWS, S, AQI, GAMAQ)
        ELSE
            GAMAQ = 1.0
        ENDIF

        IF ( GAMSM_YN ) THEN
            GAMSM = GAMSM_in
        ELSE
            GAMSM = 1.0
        ENDIF

        ! EA response to high temperature
        IF ( GAMHT_YN ) THEN
            CALL GAMMA_HT(NCOLS, NROWS, S, MaxT, GAMHT)
        ELSE
            GAMHT = 1.0
        ENDIF

        ! EA response to low temperature
        IF ( GAMLT_YN ) THEN
            CALL GAMMA_LT(NCOLS, NROWS, S, MinT, GAMLT)
        ELSE
            GAMLT = 1.0
        ENDIF

        ! EA response to high wind speed
        IF ( GAMHW_YN ) THEN
            CALL GAMMA_HW(NCOLS, NROWS, S, MaxWS, GAMHW)
        ELSE
            GAMHW = 1.0
        ENDIF


!

! THe following code has bee revised for efficiency and clarity.
!          DO I = 1, NCOLS
!            DO J = 1, NROWS
!
!              DO K = 1, Layers
!
!
!                Ea1L(K)  = CDEA(I,J,K) *                              &
!                      GAMTLD(SunT(I,J,K),D_TEMP(I,J),S) *             &
!                      GAMP(SunP(I,J,K),D_PPFD(I,J)) *  SunF(I,J,K) +  &
!                      GAMTLD(ShaT(I,J,K),D_TEMP(I,J),S) *             &
!                      GAMP(ShaP(I,J,K),D_PPFD(I,J))                   &
!                      * (1-SunF(I,J,K))
!
!                Ea2L(K) = GAMTLI(SunT(I,J,K),S)* SunF(I,J,K)+         &
!                   GAMTLI(ShaT(I,J,K),S)*(1-SunF(I,J,K))
!
!              ENDDO   ! ENDDO canopy layers
!              
!            GAMTP(I,J)=SUM((Ea1L(:)*LDFMAP(I,J) +                   &
!                            Ea2L(:)*(1-LDFMAP(I,J)))* VPGWT( : )) )
!            ENDDO   ! NROWS
!          ENDDO ! NCOLS



  
        DO J = 1, NROWS
        DO I = 1, NCOLS! Preserve stride 1 for output arrays

            SUM1 = 0.0
            SUM2 = 0.0

            DO K = 1, Layers
! 2.025 is the conversion to PPFD. 
! SWDNB*.45 = PAR (Wm-2)
! PAR*4.5 = PPFD (umol/m2/s)
                Ea1L = CDEA(I,J,K) *                                  &
                      GAMTLD(SunT(I,J,K),D_TEMP(I,J),S) *             &
              GAMP(SunP(I,J,K),D_PPFD(I,J)*2.025) *  SunF(I,J,K) +    &
                      GAMTLD(ShaT(I,J,K),D_TEMP(I,J),S) *             &
                  GAMP(ShaP(I,J,K),D_PPFD(I,J)*2.025)                 &
                      * (1.0-SunF(I,J,K))
                SUM1 = SUM1 + Ea1L*VPGWT(K)

                Ea2L = GAMTLI(SunT(I,J,K),S)* SunF(I,J,K) +           &
                      GAMTLI(ShaT(I,J,K),S)*(1.0-SunF(I,J,K))
                SUM2 = SUM2 + Ea2L*VPGWT(K)

            END DO   ! END DO canopy layers

            GAMTP = SUM1*LDFMAP(I,J) + SUM2*( 1.0-LDFMAP(I,J) )

 ! ... Calculate emission activity factors
 
            IF ( S .EQ. 1 ) THEN
!    GAMCO2 only applied to isoprene
                ER(:,:) = LAIc(I,J) * GAMTP * GAMCO2(I,J) * GAMLA(I,J) *       &
                          GAMHW(I,J) * GAMAQ(I,J) * GAMHT(I,J) * GAMLT(I,J) *  &
                          GAMSM(I,J) 

            ELSE IF ( S .EQ. 13 ) THEN
            
 !   GAMBD only applied to ethanol and acetaldehyde
            
                ER(I,J) = LAIc(I,J) * GAMTP * GAMBD(I,J) * GAMLA(I,J) *        &
                      GAMHW(I,J) * GAMAQ(I,J) * GAMHT(I,J) * GAMLT(I,J) *      &
                      GAMSM(I,J) 

            ELSE
!  Process remaining species            
            
                ER(I,J) = LAIc(I,J) * GAMTP * GAMLA(I,J) *                            &
                      GAMHW(I,J) * GAMAQ(I,J) * GAMHT(I,J) * GAMLT(I,J) *             &
                       GAMSM(I,J) 

            END IF
            IF ( ER(I,J).GT.0.0 ) THEN
                NON_DIMGARMA (S,I,J) = ER(I,J)
            ELSE
                NON_DIMGARMA (S,I,J) = 0.0
            END IF

        END DO   ! NCOLS
        END DO ! NROWS

    END DO  ! End loop of species (S)
 
    RETURN
    
   END SUBROUTINE MEGVEA

! ///////////////////////////////////////////////////////////////////////////
      SUBROUTINE MEGVSA (IDATE,ITIME,TSTEP,JYEAR,JDAY,                       &
                    L_DESID_DIAG,SLTYP, CTF,LAIc, LAT,           &
                    TEMP, SOILM1,SOILM2, SOILT, PRECADJ,           &
                    CFNO, CFNOG, GAMSM, GAMNO, BDSNP_NO )


!***********************************************************************
!   This subroutine computes soil NO emission activity factor and isoprene
!   soil moisture activity using MCIP output variables.
!
!  DESCRIPTION:
!
!     Uses new NO algorithm NO = Normalized*Tadj*Padj*Fadj*Cadj
!     to estimate NO emissions
!     Information needed to estimate NO emissions
!     Julian Day          (integer)    JDATE
!     Surface Temperature (MCIP field) TA    (K)
!     Soil Moisture       (MCIP field) SOILM (M**3/M**3) (LSOIL)
!          (ratio of volume of water per volume of soil)
!     Soil Temperature    (MCIP field) SOILT (K)         (LSOIL)
!     Soil Type           (MCIP field) ISLTYP            (LSOIL)
!
!     saturation values for soil types (constants)       (LSOIL)
!     FOR PX Version, the Temperature adjustment factor accounts for wet
!     and dry soils
!                and  the precipitation adjustment factor accounts for
!                saturated soils
!     FOR the non-PX version, the basic algorithm remains with a
!     temperature adjustment factor (dry soil)
!                     and no adjustment for saturated soils
!
!     The following arrays are updated after each call to SOILNOX
!     PULTYPE   type of NO emission pulse
!     PULSEDATE julian date for the beginning of an NO pulse
!     PULSETIME        time for the beginning of an NO pulse
!
!     The calculation are based on the following paper by J.J. Yienger
!     and H. Levy II
!     J.J. Yienger and H. Levy II, Journal of Geophysical Research, vol
!     100,11447-11464,1995
!
!     The Temperature Adjustment Factor is based on section 4.2 for wet
!     and dry soils with the following modification (PX version):
!       Instead of classifying soils as either 'wet' or 'dry', the wet
!       and dry adjustment is calculated at each grid cell.  A linear 
!       interpolation between the wet and dry adjustment factor is made 
!       using the relative amount of soil moisture in the top layer (1cm)
!       as the interpolating factor.  The relative amount of soil moisture 
!       is determined by taking the MCIP soil moisture field and dividing by the
!       saturation value defined for each soil type in the PX version of MCIP
!       the soil temperature is used in PX version
!
!     The Precipation Adjustment factor is based on section 4.1 with the
!     following modifications.
!       The rainrate is computed from the MCIP directly using a 24 hr daily total.
!       THe types of Pulses as described in YL95 were used to estimate
!       the NO emission rate.
!
!    Also see the following paper for more information:
!    Proceedings of the Air and Waste Management Association/U.S. Environmental Protection
!    Agency EMission Inventory Conference, Raleigh October 26-28, 1999 Raleigh NC
!    by Tom Pierce and Lucille Bender
!
!    REFERENCES
!
!    JACQUEMIN B. AND NOILHAN J. (1990), BOUND.-LAYER METEOROL., 52, 93-134.
!    J.J. Yienger and H. Levy II, Journal of Geophysical Research, vol 100,11447-11464,1995
!    T. Pierce and L. Bender, Examining the Temporal Variability of Ammonia and 
!      Nitric Oxide Emissions from Agricultural Proc Proceedings of the Air and Waste 
!      Management Association/U.S. Environmental Protection Agency EMission Inventory 
!      Conference, Raleigh October 26-28, 1999 Raleigh NC
!  PRECONDITIONS REQUIRED:
!     Normalized NO emissions, Surface Temperature, Soil Moisture, Soil type,
!     NO emission pulse type, soil moisture from previous time step, julian date
!     of NO emission pulse start, time of NO emission pulse start, soil type, 
!     SOIL TYPES, Land use data
!
!  SUBROUTINES AND FUNCTIONS CALLED (directly or indirectly):
!     FERTILIZER_ADJ computes fertlizer adjustment factor
!     VEG_ADJ        computes vegatation adjustment factor
!     GROWSEASON     computes day of growing season
!
! HISTORY:
!   07/21/11: Imported from SMOKE-BEIS v3.14 for MEGEAN v2.10 (Tan)
!   03/19/17: Make as an indpendent program (MEGSEA) (Ling Huang)
!   03/31/17: Add calculation for soil moisture activity (Ling Huang)
!   06/10/19: Add an option to use BDSNP model to calculate soil NO
!             emissions (Ling Huang) 
!*********************************************************************

      USE BDSNP_MOD
      USE RUNTIME_VARS, ONLY: BDSNP_MEGAN

      IMPLICIT NONE
 


!    input variables
     
     INTEGER, INTENT(IN) :: IDATE, ITIME, TSTEP(3)  
     LOGICAL, INTENT( IN ) :: L_DESID_DIAG
     
      INTEGER, INTENT(IN) :: SLTYP  (NCOLS, NROWS)  ! soil type
      REAL, INTENT(IN)    :: JYEAR, JDAY
      REAL, INTENT(IN)    :: CTF( NrTyp, NCOLS, NROWS ) ! Canopy type factor arra
      REAL, INTENT(IN)    :: LAIc( NCOLS, NROWS )    ! Current time step LAI
      REAL, INTENT(IN)    :: LAT (NCOLS, NROWS )    ! Latitude
      REAL, INTENT(IN)    :: TEMP (NCOLS, NROWS)   ! Temperautre (K)

      REAL, INTENT(IN)    :: SOILM1  (NCOLS, NROWS)  ! soil moisture
      REAL, INTENT(IN)    :: SOILM2  (NCOLS, NROWS)  ! soil moisture
      REAL, INTENT(IN)    :: SOILT  (NCOLS, NROWS)  ! soil temperature
      REAL, INTENT(IN)    :: PRECADJ (NCOLS, NROWS)   

!     output variable
      REAL, INTENT(OUT)   :: CFNO  (NCOLS, NROWS)       ! Emission activity for crop
      REAL, INTENT(OUT)   :: CFNOG  (NCOLS, NROWS)      ! Emission activity for grass
      REAL, INTENT(OUT)   :: GAMSM  (NCOLS, NROWS)      ! Soil moisture activity for isoprene
      REAL, INTENT(OUT)   :: GAMNO  (NCOLS, NROWS)      ! Final NO emission activity
      REAL, INTENT(OUT)   :: BDSNP_NO (NCOLS, NROWS)    ! BDSNP NO emissions(nmol/s/m2)



! Local variables and their descriptions:
      CHARACTER*16  :: GDNAM
      CHARACTER*16  :: CNAME        ! Coord name





      INTEGER :: GDAY, GLEN
      INTEGER :: MXLAI,MXCT
      REAL :: t1,wilt,TMO1,TMO2

      LOGICAL :: LSOIL = .TRUE.

      INTEGER :: T,I,J,MM,DD,I_CT
        
                 CFNO = 0.0 ! INITIALIZE
                 CFNOG = 0.0 ! INITIALIZE

         if (BDSNP_MEGAN) then

          call get_date(JYEAR, JDAY, MM, DD)

          CALL HRNOBDSNP( IDATE,ITIME,TSTEP,MM,  &
                    L_DESID_DIAG,SOILM1,SOILT,SLTYP,LAIc,    &
                                            bdsnp_no)

        else


          CALL SOILNOX(IDATE,ITIME,NCOLS,NROWS,            &
                     TEMP,LSOIL,SLTYP, SOILM1, SOILT,     &
                     LAIc, LAT, PRECADJ,                 &
                     CFNO, CFNOG )

        DO I = 1,NCOLS
          DO J = 1,NROWS
            CALL GROWSEASON(IDATE,LAT(I,J),GDAY,GLEN)
            IF (GDAY .EQ. 0) THEN
             ! non growing season
             ! CFNOG for everywhere
               GAMNO(I,J) = CFNOG(I,J)

             ELSE IF (GDAY .GT. 0 .AND. GDAY .LE. 366) THEN
             ! growing season
             ! CFNOG for everywhere except crops
             TMO1 = 0.
             TMO2 = 0.
             DO I_CT = 1,5
               TMO1 = TMO1 + CTF(I_CT,I,J)
               TMO2 = TMO2 + CTF(I_CT,I,J) * CFNOG(I,J)
             ENDDO
             ! CFNO for crops
             TMO1 = TMO1 + CTF(6,I,J)
             TMO2 = TMO2 + CTF(6,I,J) * CFNO(I,J)
             IF (TMO1 .EQ. 0.0) THEN
                GAMNO(I,J) = 0.0
             ELSE
                GAMNO(I,J) = TMO2 / TMO1
             ENDIF
             ENDIF
 
           ENDDO  !NCOLS
        ENDDO  !NROWS

        END IF ! YL or BDSNP


           DO I = 1, NCOLS
             DO J = 1, NROWS

               !wilt = WWLT(SLTYP(I,J))
               wilt = Grid_Data%WWLT(I,J)
               t1 = wilt + d1
               IF ( SOILM2(I,J) < wilt ) THEN
                   GAMSM(I,J) = 0
               ELSE IF ( SOILM2(I,J) >= wilt .AND. SOILM2(I,J) < t1 ) THEN
                   GAMSM(I,J) = (SOILM2(I,J) - wilt)/d1
               ELSE
                   GAMSM(I,J) = 1
               END IF
             END DO ! NCOLS
           END DO ! NROWS
         
         
  END SUBROUTINE MEGVSA


  subroutine convert2mech(no_in,inper,outer)
         USE centralized_io_module
         use hgrd_defn, only : ncols, nrows
         USE MEGAN_GSPRO
!***********************************************************************
!   This program does chemical speciation and MECHANISM conversion.
!   The output from megan.f is converted from 19 to 201 species which
!   are then lumped according to the MECHANISM assigned in the run script.  
!   The program loops through all timesteps of the input file.
!
!   Procedure
!   1) File set up and assign I/O parameters
!   2) Conversion from MGN 19 to speciated 201
!   3) Conversion from speciated species to MECHANISM species
!   4) Convert to tonne/hour if needed
!
!   The input file gives variables in units of g-species/sec.
!   All outputs are in mole/sec or tonne/hr depending on assignment.
!
!
!   INPUT:
!           1) MEGVEA output (netCDF-ioapi)
!           2) MEGSEA output (netCDF-ioapi)
!
!   OUTPUT:
!           1) MEGAN speciation or MECHANISM species (netCDF-ioapi)
!
!   Requirement:
!      Requires libnetcdf.a and libioapi.a to compile
!
!      setenv MGERFILE    <DEFANGED_input MEGAN output for emission activity factors>  
!      setenv OUTPFILE    <output speciated emission>
!
!   CALLS:  CHECKMEM
!
!   Originally created by Jack Chen 11/04 for MEGAN v.0
!   For MEGAN v2.0 created by Tan 12/01/06
!   For MEGAN v2.1 created by Xuemei Wang 11/04/07
!   For MEGAN v2.1 to use 150 species created by Xuemei Wang 09/30/09
!   For MEGAN v3 to use 201 species created by Alex Guenther 03/19/17
!
!  History:
!  08/14/07 Tan    - Move to MEGANv2.02 with no update
!  08/29/07 modified by A. Guenther to correct error in assigning   
!           emission factor. This version is called MEGANv2.03
!  10/29/07 modified by A. Guenther to correct omission of diurnal variation    
!           factor. This version is called MEGANv2.04
!  11/04/07 modified by Xuemei Wang to give two options for MAP or lookup table for
!           the emission factors. Also gives options for different chemical MECHANISMs
!           in the code: user modifies the external script to assign MECHANISM.
!           This version is called MEGANv2.1.0
!  06/04/08 modified by J. Lee-Taylor to accept vegetation-dependent speciation factors
!           in table format (RESHAPE tables) rather than from DATA statements.
!  09/30/08  modified by Xuemei Wang to give options for input file and test different mechanisms
!  09/27/11  Tan&Xuemei MEGANv2.10 includes soil NOx adjustment and a lot of updates
!  03/19/17  Ling Huang MEGANv3 (a) updates vegetation emission factors and
!            add additional compounds; (b) emission factor unit from
!            ug/m2/hr to nanomoles/m2/s; (c) fix NO emission bug
!  06/08/17  Ling Huang updated/added new mechanism for MEGAN3
!  08/10/19  Ling Huang updated for BDSNP option for MEGAN3.1
!***********************************************************************

      USE RUNTIME_VARS, ONLY: BDSNP_MEGAN
      IMPLICIT NONE



!...  Program I/O files
! Program name
      CHARACTER*16 :: PROGNAME = 'MGN2MECH'


!...  Internal parameters
! internal paramters (status and buffer)


! local variables and their descriptions:
      INTEGER :: t, s, I, N                   ! Counters
      INTEGER :: nmpmg, nmpsp, nmpmc          ! Counters

      REAL, ALLOCATABLE :: tmper(:,:,:)       ! Temp emission buffer


      REAL :: GAMNO(ncols,nrows)         ! NO emission factor
      REAL :: BDSNP_NO(ncols,nrows)      ! NO emissions for BDSNP algorithm (nanomol/m^2/s)

      REAL, INTENT(IN)     ::  inper       (19, NCOLS, NROWS )
      REAL, INTENT(IN)     ::  NO_IN       (NCOLS, NROWS )
      REAL, INTENT(OUT)    ::  outer       (NMGNSPC, NCOLS, NROWS ) ! CB6

      INTEGER :: INO,ios

      REAL, PARAMETER :: nmol2mol   = 1E-9    ! convert nanomoles to moles

      

!***********************************************************************

!=======================================================================
!...  Begin program
!=======================================================================



!.....2) Conversion from MGN 20 to speciated 201
!-----------------------------------------------------------------------
!...  Allocate memory
      ALLOCATE ( tmper( n_spca_spc, ncols, nrows ), STAT = ios )

       INO = 8 ! this was 20 for megan 3.1 

        tmper = 0.
        outer = 0.

        IF ( .NOT. BDSNP_MEGAN ) THEN
          GAMNO = NO_IN
        ELSE
          BDSNP_NO = NO_IN
        ENDIF

        DO s = 1, N_SMAP_SPC
          nmpmg = mg20_map(s)
          nmpsp = spca_map(s)
         IF ( nmpmg .NE. INO ) then
           !...  Not NO
           tmper(nmpsp,:,:) = inper(nmpmg,:,:) * efmaps(:,:,nmpmg)  &
                               * effs_all(s)
         ELSEIF ( nmpmg .EQ. INO ) then

!!-----------------NO Stuff-----------------------
           IF ( .NOT. BDSNP_MEGAN ) THEN
!     GAMNO is emission activity factor
              tmper(nmpsp,:,:) = GAMNO(:,:) * efmaps(:,:,INO)   &
                                * effs_all(s)
           ELSE

! directly use BDSNP soil NO
             tmper(nmpsp,:,:) = BDSNP_NO(:,:)
           ENDIF 
!-----------------end of NO----------------------
         ENDIF     !IF ( nmpmg .NE. INO ) then

        ENDDO ! End species loop

!-----------------------------------------------------------------------
!.....3) Conversion from speciated species to MECHANISM species
!-----------------------------------------------------------------------
        DO s = 1, n_spca_spc
           tmper(s,:,:) = tmper(s,:,:) * nmol2mol 
        ENDDO

          ! lumping to MECHANISM species

          DO s = 1, n_scon_spc
            nmpsp = spmh_map(s)         ! Mapping value for SPCA
            nmpmc = mech_map(s)         ! Mapping value for MECHANISM

           IF ( nmpmc .NE. 999 ) THEN

            outer(nmpmc,:,:) = outer(nmpmc,:,:) +     &
                    (tmper(nmpsp,:,:) * conv_fac(s))
           ENDIF
          ENDDO ! End species loop


      end subroutine convert2mech


!-----------------------------------------------------------------------
!   Created by Tan 07/28/11
!   Updated by Ling Huang 02/18/17 for MEGAN3: LAI data is saved as
!   LAI1, LAI2, ... LAIS92, instead of one variable with multiple time
!   step.
!-----------------------------------------------------------------------
      SUBROUTINE FINDLAI( IDATE, MXLAI, NLAI, LAIp_I, LAIc_I)

      IMPLICIT NONE

! input
      INTEGER,INTENT(IN) :: IDATE  ! YYYYJJJ
      INTEGER,INTENT(IN) :: MXLAI, NLAI
! output
      INTEGER,INTENT(OUT) :: LAIp_I, LAIc_I
! Local
      INTEGER :: JJJ
      REAL    :: XXX

! Calculation


      JJJ = MOD(IDATE,1000)
      IF ( NLAI .EQ. 46 ) THEN
        XXX = JJJ/8.0
        LAIc_I = CEILING(XXX)
      ELSE IF ( NLAI .EQ. 37 ) THEN
        XXX = JJJ/10.0
        LAIc_I = CEILING(XXX)
      !ELSE IF ( NLAI .EQ. 12 ) THEN
      !  CALL DAYMON(IDATE, MM, DAY)
      !  LAIc_I = MM
      ENDIF

      IF (LAIc_I .EQ. 1) THEN
        LAIp_I = MXLAI
      ELSE
        LAIp_I = LAIc_I - 1
      ENDIF


      RETURN
      END SUBROUTINE FINDLAI
!-----------------------------------------------------------------------`


end module megan_fx
