
!***********************************************************************
!   Portions of Models-3/CMAQ software were developed or based on      *
!   information from various groups: Federal Government employees,     *
!   contractors working on a United States Government contract, and    *
!   non-Federal sources (including research institutions).  These      *
!   research institutions have given the Government permission to      *
!   use, prepare derivative works, and distribute copies of their      *
!   work in Models-3/CMAQ to the public and to permit others to do     *
!   so.  EPA therefore grants similar permissions for use of the       *
!   Models-3/CMAQ software, but users are requested to provide copies  *
!   of derivative works to the Government without restrictions as to   *
!   use by others.  Users are responsible for acquiring their own      *
!   copies of commercial software associated with Models-3/CMAQ and    *
!   for complying with vendor requirements.  Software copyrights by    *
!   the MCNC Environmental Modeling Center are used with their         *
!   permissions subject to the above restrictions.                     *
!***********************************************************************

REAL FUNCTION hlconst (name, temp, effective, hplus)

!-------------------------------------------------------------------------------
! Name:     Henry's Law Constant
! Purpose:  Return the Henry's Law constant for the specified substance
!           at the given temperature.
! Notes:    Liberally adapted from EPA's CMAQ hlconst.F.
! Revised:  15 Aug 1997  Original version.  (S. Roselle)
!           18 Jun 2001  Added Henry's Law constants 50-55 for SAPRC99.
!                        (J. Gipson)
!           03 Jul 2001  Added Henry's Law constants 56-57 for Atrazine and
!                        the daughter products from Atrazine and OH reactions.
!                        (W. Hutzell)
!           06 Sep 2002  Added Henry's Law constants 59-73 for toxics.
!                        (J. Gipson)
!           07 Nov 2002  Added capability for calculating the effective Henry's
!                        Law constant and updated coefficients in Henry's Law
!                        constant table.  (S. Roselle)
!           29 May 2003  Converted to free-form f90 and adapted for MCIP2.
!                        (T. Otte and D. Schwede)
!           01 Jul 2004  Updated to reflect current code in CCTM in module
!                        cloud/cloud_radm/hlconst.F.  (T. Otte)
!           25 Feb 2005  Added new species: CL2, HOCL, HCL, FMCL, ICL1, ICL2,
!                        HG and HGIIGAS.  Added a value for the concentration
!                        of [Cl-] for estimating effective Henry's Law
!                        coefficient for Cl2.  (G. Sarwar, R. Bullock, and
!                        T. Otte)
!           24 Apr 2008  Added five new toxic species.  (W. Hutzell and T. Otte)
!           21 May 2008  Updated Henry's Law constants for O3, NO3, hexane,
!                        octane, nonane, and methanol, and updated isoprene
!                        reference.  Updated to be consistent with analogous
!                        routine in CMAQ.  (A. Carlton, S. Roselle, and T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER,       PARAMETER     :: mxspcs     = 116     ! num substances
  INTEGER,       PARAMETER     :: mxdspcs    = 17      ! num dissociating species

  REAL                         :: a          ( mxspcs )  ! HL consts at 298.15 K [M/atm]
  REAL                         :: akeq1                ! temp var for dissoc const
  REAL                         :: akeq2                ! temp var for dissoc const
  REAL                         :: b          ( mxdspcs ) ! dissoc const at 298.15 K [M/atm]
  REAL                         :: clminus                ! chlorine ion conc
  REAL                         :: clminusi               ! 1 / clminus
  REAL                         :: d          ( mxdspcs ) ! -dH/R [K]
  REAL                         :: e          ( mxspcs )  ! enthalpy [K] (like activ en)
  LOGICAL,       INTENT(IN)    :: effective            ! T=calc eff HL const
  REAL,          INTENT(IN)    :: hplus                ! hydrogen ion conc
  REAL                         :: hplus2i              ! 1 / hplus**2
  REAL                         :: hplusi               ! 1 / hplus
  INTEGER,       EXTERNAL      :: index1
  REAL                         :: kh                   ! temp var for HL const
  INTEGER,       PARAMETER     :: latra      = 13      ! Atrazine pointer
  INTEGER,       PARAMETER     :: lcl2       = 14      ! CL2 pointer
  INTEGER,       PARAMETER     :: lco2       =  5      ! CO2 pointer
  INTEGER,       PARAMETER     :: lh2o       = 12      ! H2O pointer
  INTEGER,       PARAMETER     :: lh2o2      =  7      ! H2O2 pointer
  INTEGER,       PARAMETER     :: lhcho      =  8      ! HCHO pointer
  INTEGER,       PARAMETER     :: lhcl       = 16      ! HCL pointer
  INTEGER,       PARAMETER     :: lhco3      =  6      ! HCO3 pointer
  INTEGER,       PARAMETER     :: lhcooh     =  9      ! HCOOH pointer
  INTEGER,       PARAMETER     :: lhno2      =  3      ! HNO2 pointer
  INTEGER,       PARAMETER     :: lhno3      =  4      ! HNO3 pointer
  INTEGER,       PARAMETER     :: lho2       = 10      ! HO2 pointer
  INTEGER,       PARAMETER     :: lhocl      = 15      ! HOCL pointer
  INTEGER,       PARAMETER     :: lhso3      =  2      ! HSO3 pointer
  INTEGER,       PARAMETER     :: lhydrazine = 17      ! Hydrazine pointer
  INTEGER,       PARAMETER     :: lnh4oh     = 11      ! NH4OH pointer
  INTEGER,       PARAMETER     :: lso2       =  1      ! SO2 pointer
  CHARACTER*(*), INTENT(IN)    :: name                 ! name of substance
  REAL                         :: ohion                ! OH ion concentration
  CHARACTER*16,  PARAMETER     :: pname      = 'HLCONST'
  INTEGER                      :: spc                  ! species index
  CHARACTER*16                 :: subname    ( mxspcs )
  REAL,          INTENT(IN)    :: temp                 ! temperature [K]
  REAL                         :: tfac                 ! (298-T)/(298*T)

!-------------------------------------------------------------------------------
! Define Henry's Law constants (A, in M/atm) and enthalpy (E, in K) for various
! substances.  Taken from Rolf Sanders' Compilation of Henry's Law Constants
! for Inorganic and Organic Species of Potential Importance in Environment
! Chemistry (1999).
!-------------------------------------------------------------------------------

  DATA subname(  1), a(  1), e(  1) / 'O3              ', 1.14E-02, 2.3E+03 / ! Kosak 1983
  DATA subname(  2), a(  2), e(  2) / 'HO2             ', 4.0E+03, 5.9E+03 /  ! Hanson et al. 1992
  DATA subname(  3), a(  3), e(  3) / 'H2O2            ', 8.3E+04, 7.4E+03 /  ! O'Sullivan et al. 1996
  DATA subname(  4), a(  4), e(  4) / 'NH3             ', 6.1E+01, 4.2E+03 /  ! Clegg and Brimblecombe 1989
  DATA subname(  5), a(  5), e(  5) / 'NO              ', 1.9E-03, 1.4E+03 /  ! Lide and Frederikse 1995
  DATA subname(  6), a(  6), e(  6) / 'NO2             ', 1.2E-02, 2.5E+03 /  ! Chameides 1984
  DATA subname(  7), a(  7), e(  7) / 'NO3             ', 0.6E+00, 0.0E+00 /  ! Rudich, Talukdar et al. 1996
  DATA subname(  8), a(  8), e(  8) / 'N2O5            ', 1.0E+30, 0.0E+00 /  ! "inf" Sander and Crutzen 1996
  DATA subname(  9), a(  9), e(  9) / 'HNO2            ', 5.0E+01, 4.9E+03 /  ! Becker et al. 1996
  DATA subname( 10), a( 10), e( 10) / 'HNO3            ', 2.1E+05, 8.7E+03 /  ! Leieveld and Crutzen 1991
  DATA subname( 11), a( 11), e( 11) / 'HNO4            ', 1.2E+04, 6.9E+03 /  ! Regimbal and Mozurkewich 1997
  DATA subname( 12), a( 12), e( 12) / 'SO2             ', 1.4E+00, 2.9E+03 /  ! Linde and Frederikse 1995
  DATA subname( 13), a( 13), e( 13) / 'H2SO4           ', 1.0E+30, 0.0E+00 /  ! infinity
  DATA subname( 14), a( 14), e( 14) / 'METHANE         ', 1.4E-03, 1.6E+03 /  ! Linde and Frederikse 1995
  DATA subname( 15), a( 15), e( 15) / 'ETHANE          ', 1.9E-03, 2.3E+03 /  ! Linde and Frederikse 1995
  DATA subname( 16), a( 16), e( 16) / 'PROPANE         ', 1.5E-03, 2.7E+03 /  ! Linde and Frederikse 1995
  DATA subname( 17), a( 17), e( 17) / 'BUTANE          ', 1.1E-03, 0.0E+00 /  ! Mackay and Shiu 1981
  DATA subname( 18), a( 18), e( 18) / 'PENTANE         ', 8.1E-04, 0.0E+00 /  ! Mackay and Shiu 1981
  DATA subname( 19), a( 19), e( 19) / 'HEXANE          ', 0.1E-03, 7.5E+03 /  ! Ashworth, Howe et al. 1988
  DATA subname( 20), a( 20), e( 20) / 'OCTANE          ', 2.9E-03, 7.8E+03 /  ! Hansen et al. 1993
  DATA subname( 21), a( 21), e( 21) / 'NONANE          ', 2.4E-03, 2.1E+02 /  ! Ashworth, Howe et al. 1988
  DATA subname( 22), a( 22), e( 22) / 'DECANE          ', 1.4E-04, 0.0E+00 /  ! Mackay and Shiu 1981
  DATA subname( 23), a( 23), e( 23) / 'ETHENE          ', 4.7E-03, 0.0E+00 /  ! Mackay and Shiu 1981
  DATA subname( 24), a( 24), e( 24) / 'PROPENE         ', 4.8E-03, 0.0E+00 /  ! Mackay and Shiu 1981
  DATA subname( 25), a( 25), e( 25) / 'ISOPRENE        ', 2.8E-02, 0.0E+00 /  ! Karl, Lindinger et al. 2003
  DATA subname( 26), a( 26), e( 26) / 'ACETYLENE       ', 4.1E-02, 1.8E+03 /  ! Wilhelm et al. 1977
  DATA subname( 27), a( 27), e( 27) / 'BENZENE         ', 1.6E-01, 4.1E+03 /  ! Staudinger and Roberts 1996
  DATA subname( 28), a( 28), e( 28) / 'TOLUENE         ', 1.5E-01, 4.0E+03 /  ! Staudinger and Roberts 1996
  DATA subname( 29), a( 29), e( 29) / 'O-XYLENE        ', 1.9E-01, 4.0E+03 /  ! Staudinger and Roberts 1996
  DATA subname( 30), a( 30), e( 30) / 'METHANOL        ', 2.2E+02, 5.2E+03 /  ! Snider and Dawson 1985
  DATA subname( 31), a( 31), e( 31) / 'ETHANOL         ', 1.9E+02, 6.6E+03 /  ! Snider and Dawson 1985
  DATA subname( 32), a( 32), e( 32) / '2-CRESOL        ', 8.2E+02, 0.0E+00 /  ! Betterton 1992
  DATA subname( 33), a( 33), e( 33) / '4-CRESOL        ', 1.3E+02, 0.0E+00 /  ! Betterton 1992
  DATA subname( 34), a( 34), e( 34) / 'METHYLHYDROPEROX', 3.1E+02, 5.2E+03 /  ! O'Sullivan et al. 1996
  DATA subname( 35), a( 35), e( 35) / 'FORMALDEHYDE    ', 3.2E+03, 6.8E+03 /  ! Staudinger and Roberts 1996
  DATA subname( 36), a( 36), e( 36) / 'ACETALDEHYDE    ', 1.4E+01, 5.6E+03 /  ! Staudinger and Roberts 1996
  DATA subname( 37), a( 37), e( 37) / 'GENERIC_ALDEHYDE', 4.2E+03, 0.0E+00 /  ! Graedel and Goldberg 1983
  DATA subname( 38), a( 38), e( 38) / 'GLYOXAL         ', 3.6E+05, 0.0E+00 /  ! Zhou and Mopper 1990
  DATA subname( 39), a( 39), e( 39) / 'ACETONE         ', 3.0E+01, 4.6E+03 /  ! Staudinger and Roberts 1996
  DATA subname( 40), a( 40), e( 40) / 'FORMIC_ACID     ', 8.9E+03, 6.1E+03 /  ! Johnson et al. 1996
  DATA subname( 41), a( 41), e( 41) / 'ACETIC_ACID     ', 4.1E+03, 6.3E+03 /  ! Johnson et al. 1996
  DATA subname( 42), a( 42), e( 42) / 'METHYL_GLYOXAL  ', 3.2E+04, 0.0E+00 /  ! Zhou and Mopper 1990
  DATA subname( 43), a( 43), e( 43) / 'CO              ', 9.9E-04, 1.3E+03 /  ! Linde and Frederikse 1995
  DATA subname( 44), a( 44), e( 44) / 'CO2             ', 3.6E-02, 2.2E+03 /  ! Zheng et al. 1997
  DATA subname( 45), a( 45), e( 45) / 'PAN             ', 2.8E+00, 6.5E+03 /  ! Kames et al. 1991
  DATA subname( 46), a( 46), e( 46) / 'MPAN            ', 1.7E+00, 0.0E+00 /  ! Kames and Schurath 1995
  DATA subname( 47), a( 47), e( 47) / 'OH              ', 3.0E+01, 4.5E+03 /  ! Hanson et al. 1992
  DATA subname( 48), a( 48), e( 48) / 'METHYLPEROXY_RAD', 2.0E+03, 6.6E+03 /  ! Lelieveld and Crutzen 1991
  DATA subname( 49), a( 49), e( 49) / 'PEROXYACETIC_ACI', 8.4E+02, 5.3E+03 /  ! O'Sullivan et al. 1996
  DATA subname( 50), a( 50), e( 50) / 'PROPANOIC_ACID  ', 5.7E+03, 0.0E+00 /  ! Kahn et al. 1995
  DATA subname( 51), a( 51), e( 51) / '2-NITROPHENOL   ', 7.0E+01, 4.6E+03 /  ! USEPA 1982
  DATA subname( 52), a( 52), e( 52) / 'PHENOL          ', 1.9E+03, 7.3E+03 /  ! USEPA 1982
  DATA subname( 53), a( 53), e( 53) / 'BIACETYL        ', 7.4E+01, 5.7E+03 /  ! Betteron 1991
  DATA subname( 54), a( 54), e( 54) / 'BENZALDEHYDE    ', 3.9E+01, 4.8E+03 /  ! Staudinger and Roberts 1996
  DATA subname( 55), a( 55), e( 55) / 'PINENE          ', 4.9E-02, 0.0E+00 /  ! Karl and Lindinger 1997
  DATA subname( 56), a( 56), e( 56) / 'ATRA            ', 4.1E+05, 6.0E+03 /  ! CIBA Corp (1989) and Scholtz (1999)
  DATA subname( 57), a( 57), e( 57) / 'DATRA           ', 4.1E+05, 6.0E+03 /  ! assumed same as Atrazine
  DATA subname( 58), a( 58), e( 58) / 'ADIPIC_ACID     ', 2.0E+08, 0.0E+00 /  ! Saxena and Hildemann (1996)
  DATA subname( 59), a( 59), e( 59) / 'ACROLEIN        ', 8.2E+00, 0.0E+00 /  ! Meylan and Howard (1991)
  DATA subname( 60), a( 60), e( 60) / '1,3-BUTADIENE   ', 1.4E-02, 0.0E+00 /  ! Mackay and Shiu (1981)
  DATA subname( 61), a( 61), e( 61) / 'ACRYLONITRILE   ', 7.3E+00, 0.0E+00 /  ! Meylan and Howard (1991)
  DATA subname( 62), a( 62), e( 62) / 'CARBONTETRACHLOR', 3.4E-02, 4.2E+03 /  ! Staudinger and Roberts (1996)
  DATA subname( 63), a( 63), e( 63) / 'PROPYLENE_DICHLO', 3.4E-01, 4.3E+03 /  ! Staudinger and Roberts (1996)
  DATA subname( 64), a( 64), e( 64) / '1,3DICHLORPROPEN', 6.5E-01, 4.2E+03 /  ! Wright et al (1992b)
  DATA subname( 65), a( 65), e( 65) / '1,1,2,2-CL4ETHAN', 2.4E+00, 3.2E+03 /  ! Staudinger and Roberts (1996)
  DATA subname( 66), a( 66), e( 66) / 'CHLOROFORM      ', 2.5E-01, 4.5E+03 /  ! Staudinger and Roberts (1996)
  DATA subname( 67), a( 67), e( 67) / '1,2DIBROMOETHANE', 1.5E+00, 3.9E+03 /  ! Ashworth et al (1988)
  DATA subname( 68), a( 68), e( 68) / '1,2DICHLOROETHAN', 7.3E-01, 4.2E+03 /  ! Staudinger and Roberts (1996)
  DATA subname( 69), a( 69), e( 69) / 'METHYLENE_CHLORI', 3.6E-01, 4.1E+03 /  ! Staudinger and Roberts (1996)
  DATA subname( 70), a( 70), e( 70) / 'PERCHLOROETHYLEN', 5.9E-02, 4.8E+03 /  ! Staudinger and Roberts (1996)
  DATA subname( 71), a( 71), e( 71) / 'TRICHLOROETHENE ', 1.0E-01, 4.6E+03 /  ! Staudinger and Roberts (1996)
  DATA subname( 72), a( 72), e( 72) / 'VINYL_CHLORIDE  ', 3.9E-02, 3.1E+03 /  ! Staudinger and Roberts (1996)
  DATA subname( 73), a( 73), e( 73) / 'ETHYLENE_OXIDE  ', 8.4E+00, 0.0E+00 /  ! CRC
  DATA subname( 74), a( 74), e( 74) / 'PPN             ', 2.9E+00, 0.0E+00 /  ! Kames and Schurath (1995)
  DATA subname( 75), a( 75), e( 75) / 'NAPHTHALENE     ', 2.0E+00, 3.6E+03 /  ! USEPA 1982
  DATA subname( 76), a( 76), e( 76) / 'QUINOLINE       ', 3.7E+03, 5.4E+03 /  ! USEPA 1982
  DATA subname( 77), a( 77), e( 77) / 'MEK             ', 2.0E+01, 5.0E+03 /  ! Zhou and Mopper 1990
  DATA subname( 78), a( 78), e( 78) / 'MVK             ', 4.1E+01, 0.0E+00 /  ! Iraci et al. 1998
  DATA subname( 79), a( 79), e( 79) / 'METHACROLEIN    ', 6.5E+00, 0.0E+00 /  ! Iraci et al. 1998
  DATA subname( 80), a( 80), e( 80) / 'CL2             ', 8.6E-02, 2.0E+03 /  ! Rolf Sanders Compilation (1999)/Kavanaugh and Trussell (1980) 
  DATA subname( 81), a( 81), e( 81) / 'HOCL            ', 6.6E+02, 5.9E+03 /  ! Rolf Sanders Compilation (1999)/Huthwelker et al (1995) 
  DATA subname( 82), a( 82), e( 82) / 'HCL             ', 1.9E+01, 6.0E+02 /  ! Rolf Sanders Compilation (1999)/Dean (1992) 
  DATA subname( 83), a( 83), e( 83) / 'FMCL            ', 1.1E+00, 0.0E+00 /  ! EPA Suite Program/Unit converted to match the definition by Rolf Sanders.
  DATA subname( 84), a( 84), e( 84) / 'ICL1            ', 6.9E+01, 0.0E+00 /  ! EPA Suite Program/Unit converted to match the definition by Rolf Sanders.
  DATA subname( 85), a( 85), e( 85) / 'ICL2            ', 6.9E+01, 0.0E+00 /  ! EPA Suite Program/Assumed equal to that of ICL1      
  DATA subname( 86), a( 86), e( 86) / 'HG              ', 1.1E-01, 5.0E+03 /  ! Clever et al. (1985)
  DATA subname( 87), a( 87), e( 87) / 'HGIIGAS         ', 1.4E+06, 5.3E+03 /  ! Lindqvist and Rodhe (1985)
  DATA subname( 88), a( 88), e( 88) / 'TECDD_2378      ', 5.1E+00, 3.6E+03 /  ! Paasivirta et al. (1999)
  DATA subname( 89), a( 89), e( 89) / 'PECDD_12378     ', 4.6E+00, 3.2E+03 /  ! Paasivirta et al. (1999)
  DATA subname( 90), a( 90), e( 90) / 'HXCDD_123478    ', 8.1E+00, 2.9E+03 /  ! Paasivirta et al. (1999)
  DATA subname( 91), a( 91), e( 91) / 'HXCDD_123678    ', 2.9E+00, 2.8E+03 /  ! Paasivirta et al. (1999)
  DATA subname( 92), a( 92), e( 92) / 'HXCDD_123789    ', 6.5E+00, 2.7E+03 /  ! Paasivirta et al. (1999)
  DATA subname( 93), a( 93), e( 93) / 'HPCDD_1234678   ', 1.2E+01, 2.4E+03 /  ! Paasivirta et al. (1999)
  DATA subname( 94), a( 94), e( 94) / 'OTCDD           ', 9.8E+00, 2.3E+03 /  ! Paasivirta et al. (1999)
  DATA subname( 95), a( 95), e( 95) / 'TECDF_2378      ', 8.5E+01, 3.7E+03 /  ! Paasivirta et al. (1999)
  DATA subname( 96), a( 96), e( 96) / 'PECDF_12378     ', 5.2E+01, 2.9E+03 /  ! Paasivirta et al. (1999)
  DATA subname( 97), a( 97), e( 97) / 'PECDF_23478     ', 1.8E+02, 3.0E+03 /  ! Paasivirta et al. (1999)
  DATA subname( 98), a( 98), e( 98) / 'HXCDF_123478    ', 3.8E+01, 2.4E+03 /  ! Paasivirta et al. (1999)
  DATA subname( 99), a( 99), e( 99) / 'HXCDF_123678    ', 9.0E+01, 2.9E+03 /  ! Paasivirta et al. (1999)
  DATA subname(100), a(100), e(100) / 'HXCDF_234678    ', 1.0E+02, 2.6E+03 /  ! Paasivirta et al. (1999)
  DATA subname(101), a(101), e(101) / 'HXCDF_123789    ', 5.6E+01, 2.6E+03 /  ! Paasivirta et al. (1999)
  DATA subname(102), a(102), e(102) / 'HPCDF_1234678   ', 2.8E+01, 1.6E+03 /  ! Paasivirta et al. (1999)
  DATA subname(103), a(103), e(103) / 'HPCDF_1234789   ', 8.0E+01, 2.1E+03 /  ! Paasivirta et al. (1999)
  DATA subname(104), a(104), e(104) / 'OTCDF           ', 7.6E+01, 2.4E+03 /  ! Paasivirta et al. (1999)
  DATA subname(105), a(105), e(105) / 'NAPHTHOL        ', 3.60E+03, 0.0E+00 / ! Eabraham et al. (1994)
  DATA subname(106), a(106), e(106) / '1NITRONAPHTHALEN', 5.68E+02, 0.0E+00 / ! Altschuh et al. (1999)
  DATA subname(107), a(107), e(107) / '2NITRONAPHTHALEN', 6.42E+02, 0.0E+00 / ! HENRYWIN v3.10 (Meylan and Howard, 1991)
  DATA subname(108), a(108), e(108) / '14NAPHTHOQUINONE', 5.08E+05, 0.0E+00 / ! HENRYWIN v3.10 (Meylan and Howard, 1991)
  DATA subname(109), a(109), e(109) / '2,4-TOLUENE_DIIS', 7.25E+00, 0.0E+00 / ! HENRYWIN v3.10 (Meylan and Howard, 1991)
  DATA subname(110), a(110), e(110) / 'HEXAMETHYLE_DIIS', 2.08E+01, 0.0E+00 / ! HENRYWIN v3.10 (Meylan and Howard, 1991)
  DATA subname(111), a(111), e(111) / 'HYDRAZINE       ', 1.14E+03, 0.0E+00 / ! Daubert and Danner (1989), and Amoore and Hautala (1983)
  DATA subname(112), a(112), e(112) / 'MALEIC_ANHYDRIDE', 2.54E+02, 0.0E+00 / ! HENRYWIN v3.10 (Meylan and Howard, 1991)
  DATA subname(113), a(113), e(113) / 'TRIETHYLAMINE   ', 6.71E+00, 0.0E+00 / ! Yalkowsky and Dannenfelser (1992), and Riddick et al. (1986)
  DATA subname(114), a(114), e(114) / 'P_DICHLOROBENZEN', 2.38E+00, 0.0E+00 / ! MacKay and Shiu (1981), measured
  DATA subname(115), a(115), e(115) / 'M-XYLENE        ', 1.43E-01, 3.9E+03 / ! Staudinger and Roberts (2001)
  DATA subname(116), a(116), e(116) / 'P-XYLENE        ', 1.35E-01, 3.7E+03 / ! Staudinger and Roberts (2001)

!-------------------------------------------------------------------------------
! Define dissociation constant at 298.15 K (B, in M/atm) and -dH/R (D, in K)
! for various species.  Taken from Table 6.A.1, Seinfeld and Pandis, Atmospheric
! Chemistry and Physics (1997).
!-------------------------------------------------------------------------------

  DATA b(lso2   ), d(lso2   ) / 1.30E-02,  1.96E+03 /  ! SO2*H2O<=>HSO3+H     : Smith and Martell (1976)
  DATA b(lhso3  ), d(lhso3  ) / 6.60E-08,  1.50E+03 /  ! HSO3<=>SO3+H         : Smith and Martell (1976)
  DATA b(lhno2  ), d(lhno2  ) / 5.10E-04, -1.26E+03 /  ! HNO2(aq)<=>NO2+H     : Schwartz and White (1981)
  DATA b(lhno3  ), d(lhno3  ) / 1.54E+01,  8.70E+03 /  ! HNO3(aq)<=>NO3+H     : Schwartz (1984)
  DATA b(lco2   ), d(lco2   ) / 4.30E-07, -1.00E+03 /  ! CO2*H2O<=>HCO3+H     : Smith and Martell (1976)
  DATA b(lhco3  ), d(lhco3  ) / 4.68E-11, -1.76E+03 /  ! HCO3<=>CO3+H         : Smith and Martell (1976)
  DATA b(lh2o2  ), d(lh2o2  ) / 2.20E-12, -3.73E+03 /  ! H2O2(aq)<=>HO2+H     : Smith and Martell (1976)
  DATA b(lhcho  ), d(lhcho  ) / 2.53E+03,  4.02E+03 /  ! HCHO(aq)<=>H2C(OH)2  : Le Hanaf (1968)
  DATA b(lhcooh ), d(lhcooh ) / 1.80E-04, -2.00E+01 /  ! HCOOH(aq)<=>HCOO+H   : Martell and Smith (1977)
  DATA b(lho2   ), d(lho2   ) / 3.50E-05,  0.00E+00 /  ! HO2(aq)<=>H+O2       : Perrin (1982)
  DATA b(lnh4oh ), d(lnh4oh ) / 1.70E-05, -4.50E+02 /  ! NH4*OH<=>NH4+OH      : Smith and Martell (1976)
  DATA b(lh2o   ), d(lh2o   ) / 1.00E-14, -6.71E+03 /  ! H2O<=>H+OH           : Smith and Martell (1976)
  DATA b(latra  ), d(latra  ) / 2.09E-02,  0.00E+00 /  ! C8H14ClN5<=>C8H13ClN5+H  : Weber (1970)
  DATA b(lcl2   ), d(lcl2   ) / 5.01E-04,  0.00E+00 /  ! CL2*H2O <=> HOCL + H + CL : Lin and Pehkonen, JGR, 103, D21, 28093-28102, November 20, 1998. Also see note below
  DATA b(lhocl  ), d(lhocl  ) / 3.16E-08,  0.00E+00 /  ! HOCL <=>H + OCL      : Lin and Pehkonen, JGR, 103, D21, 28093-28102, November 20, 1998
  DATA b(lhcl   ), d(lhcl   ) / 1.74E+06,  6.90E+03 /  ! HCL <=> H + CL       : Marsh and McElroy (1985) 
  DATA b(lhydrazine), d(lhydrazine) / 1.11E-08,  0.00E+00 /  ! HYDRAZINE <=> HYDRAZINE+ + OH-  : Moliner and Street (1989)

!-------------------------------------------------------------------------------
! Note for dissociation constant for equation 14: CL2*H2O <=> HOCL + H + CL 
! Need aqueous [CL-] conc to calculate effective Henry's law coefficient
! Used a value of 2.0 mM following Lin and Pehkonen, JGR, 103, D21, 28093-28102,
! November 20, 1998.
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Determine species.  If species is not in the table, error exit.
!-------------------------------------------------------------------------------

  spc = INDEX1 (name, mxspcs, subname)

  IF ( spc <= 0 ) THEN
    WRITE (6,9000) TRIM(name)
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! Compute the Henry's Law constant.
!-------------------------------------------------------------------------------

  tfac    = (298.0 - temp) / (298.0 * temp)
  kh      = a(spc) * EXP( e(spc) * tfac )
  hlconst = kh

!-------------------------------------------------------------------------------
! Compute the effective Henry's Law constants.
!-------------------------------------------------------------------------------

  IF ( effective ) THEN

    IF ( hplus <= 0.0 ) THEN
       WRITE (6,9100)
       GOTO 1001
    ENDIF

    hplusi  = 1.0 / hplus
    hplus2i = hplusi * hplusi

    check_name: SELECT CASE ( TRIM(name) )

      CASE ( 'SO2' )          !   SO2H2O <=> HSO3- + H+
                              ! & HSO3- <=> SO3= + H+

        akeq1   = b(lso2)  * EXP( d(lso2)  * tfac )
        akeq2   = b(lhso3) * EXP( d(lhso3) * tfac )
        hlconst = kh * ( 1.0 + akeq1 * hplusi + akeq1 * akeq2 * hplus2i )

      CASE ( 'HNO2' )         ! HNO2(aq) <=> NO2- + H+

        akeq1   = b(lhno2) * EXP( d(lhno2) * tfac )
        hlconst = kh * ( 1.0 + akeq1 * hplusi )

      CASE ( 'HNO3' )         ! HNO3(aq) <=> NO3- + H+

        akeq1   = b(lhno3) * EXP( d(lhno3) * tfac )
        hlconst = kh * ( 1.0 + akeq1 * hplusi )

      CASE ( 'CO2' )          !   CO2H2O <=> HCO3- + H+
                              ! & HCO3- <=> CO3= + H+

        akeq1   = b(lco2)  * EXP( d(lco2)  * tfac )
        akeq2   = b(lhco3) * EXP( d(lhco3) * tfac )
        hlconst = kh * ( 1.0 + akeq1 * hplusi + akeq1 * akeq2 * hplus2i )

      CASE ( 'H2O2' )         ! H2O2(aq) <=> HO2- + H+

        akeq1   = b(lh2o2) * EXP( d(lh2o2) * tfac )
        hlconst = kh * ( 1.0 + akeq1 * hplusi )

      CASE ( 'FORMALDEHYDE' ) ! HCHO(aq) <=> H2C(OH)2(aq)

        akeq1   = b(lhcho) * EXP( d(lhcho) * tfac )
        hlconst = kh * ( 1.0 + akeq1 )

      CASE ( 'FORMIC_ACID' )  ! HCOOH(aq) <=> HCOO- + H+

        akeq1   = b(lhcooh) * EXP( d(lhcooh) * tfac )
        hlconst = kh * ( 1.0 + akeq1 * hplusi )

      CASE ( 'HO2' )          ! HO2(aq) <=> H+ + O2-

        akeq1   = b(lho2) * EXP( d(lho2) * tfac )
        hlconst = kh * ( 1.0 + akeq1 * hplusi )

      CASE ( 'NH3' )          ! NH4OH <=> NH4+ + OH-

        akeq1   = b(lnh4oh) * EXP( d(lnh4oh) * tfac )
        akeq2   = b(lh2o)   * EXP( d(lh2o)   * tfac )
        ohion   = akeq2 * hplusi
        hlconst = kh * ( 1.0 + akeq1 / ohion )

      CASE ( 'HYDRAZINE' )    ! HYDRAZINE <=> HYDRAZINE+ + OH-

        akeq1   = b(lhydrazine) * EXP( d(lhydrazine ) * tfac )
        akeq2   = b(lh2o)       * EXP( d(lh2o )       * tfac )
        ohion   = akeq2 * hplusi
        hlconst = kh * ( 1.0 + akeq1 / ohion )

      CASE( 'ATRA', 'DATRA' )  !     ATRA(aq)  <=>  ATRA- + H
                               !  or DATRA(aq) <=> DATRA- + H  

        akeq1   = b(latra) * EXP( d(latra) * tfac )
        hlconst = kh * ( 1.0 + akeq1 * hplusi )

      CASE ( 'CL2' )          ! CL2*H2O <=> HOCL + H + CL 
                              ! HOCL <=>H + OCL 

        ! Assign a value for clminus.
        ! Use 2.0 mM based on Lin and Pehkonen, 1998, JGR
        clminus  = 2.0E-03                ! chlorine ion conc [CL-]
        clminusi = 1.0 / clminus          ! 1 / clminus 

        akeq1    = b(lcl2)  * EXP( d(lcl2) * tfac )
        akeq2    = b(lhocl) * EXP( d(lhocl) * tfac )
        hlconst  = kh * ( 1.0 + akeq1 * hplusi * clminusi +  &
                          akeq1 * akeq2 * hplus2i * clminusi )

      CASE ( 'HCL' )          ! HCL <=> H+ + CL-   

        akeq1   = b(lhcl) * EXP( d(lhcl) * tfac )
        hlconst = kh * ( 1.0 + akeq1 * hplusi ) 

      CASE ( 'HOCL' )         ! HOCL <=> H+ + OCL-   

        akeq1   = b(lhocl) * EXP( d(lhocl) * tfac )
        hlconst = kh * ( 1.0 + akeq1 * hplusi ) 

    END SELECT check_name

  ENDIF

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** FUNCTION: HLCONST',                              &
              /, 1x, '***   SPECIES ', a, ' NOT FOUND IN HENRYS LAW',      &
              /, 1x, '***   CONSTANT TABLE',                               &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** FUNCTION: HLCONST',                              &
              /, 1x, '***   NEGATIVE OR ZERO [H+] CONCENTRATION FOUND',    &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END FUNCTION hlconst
