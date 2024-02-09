
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

      REAL FUNCTION HLCONST ( CNAME, TEMP, EFFECTIVE, HPLUS )

C-----------------------------------------------------------------------
C
C  FUNCTION: return the Henry's law constant for the specified substance
C            at the given temperature
C
C  References:
C
C  1. Nguyen, T. B., Coggon, M. M., Bates, K. H., Zhang, X., Schwantes,
C     R. H., Schilling, K. A., Loza, C. L., Flagan, R. C., Wennberg, P. O.,
C     and Seinfeld, J. H.: Organic aerosol formation from the reactive
C     uptake of isoprene epoxydiols (IEPOX) onto non-acidified inorganic
C     seeds, Atmos. Chem. Phys., 14, 3497-3510,
C     doi:10.5194/acp-14-3497-2014, 2014.
C 
C  revision history:
C    who        when           what
C  ---------  --------  -------------------------------------
C  S.Roselle  08/15/97  code written for Models-3
C  J.Gipson   06/18/01  added Henry's Law constants 50-55 for saprc99
C  W.Hutzell  07/03/01  added Henry's Law constants 56-57 for Atrazine
C                       and the daughter products from Atrazine and OH
C                       reactions.
C  J.Gipson.  09/06/02  added Henry's Law constants 59-73   for toxics
C  S.Roselle  11/07/02  added capability for calculating the effective
C                       Henry's law constant and updated coefficients
C                       in Henry's law constant table
C  J.Gipson   08/06/03  added Henry's Law constants 77-79
C  G.Sarwar   11/21/04  added constants for chlorine chemistry (Henry's
C                       law constants 80-85 and dissociation constants
C                       14-16
C  R.Bullock  07/05/05  added Henry's Law constants 86-87 for mercury 
C                       with enthalpy calculated from cited laboratory 
C                       data fit to an Arrhenius equation
C  W.Hutzell  02/14/06  added HLC 88 to 116, dissociation constant for
C                       17 (hydrazine)
C  A.Carlton  09/20/06  updated Henry's Law constants for 1,7,19,20,21,30 
C                       O3, NO3, hexane, octane, nonane, methanol and
C                       isoprene reference
C  S.Roselle  10/10/07  changed pointers to parameters; reformatted
C                       variable declarations
C  W.Hutzell  03/07/08  added Henry's Law constants for 117 to 120
C  W.Hutzell  01/01/09  added Henry's Law constants for 121 to 125 for
C                       for SAPRC07T mechanism                 
C  Y.Xie      6/11/10  added Henry's Law constants for 126 to 130 for 
C                       isoprene chemistry in SAPRC07               
C  S.Roselle  03/01/11  replaced I/O API include files with UTILIO_DEFN;
C                       removed deprecated TRIMLEN
C  W.Hutzell  08/16/12  updated IOAPI and Henry Constant names to V5.01
C                       standards
C  W.Hutzell  01/30/13  updated PHENOL, and add values for CATECHOL and
C                       XYLENOL
C  H.Pye      02/05/13  added Henry's Law 133-134 for IEPOX and IMAE
C  G.Sarwar   12/18/13  added Henry's Law constants for 135 to 140
C  W.Hutzell  07/11/14  correct error in DATA from last revision
C  D.Luecken  07/11/14  added 141 through 144 for nitrates
C  H.Pye      05/05/15  added monoterpene and isoprene nitrates
C  H.Pye      01/26/2016 added SOA SVOCs
C  G.Sarwar   03/24/15  added iodine and bromine species
C  H.Pye      05/31/16  updated IEPOX H-law from 2.7e6 to 3.0e7 M/atm
C                       to reflect Nguyen et al. 2014.
C  W.Hutzell  09/16/16  1) removed HCHO dissociation because its henry's
C                        constant already accounts for the effect.
C                       2) added henry's constant for compounds 194 thru
C                        199 representing several toxic air pollutants
C  
C  D. Luecken 09/21/2016 
C       1. added HAPs in 194-200; 
C       2. modified existing HEXANE (19),N-HEXANE (117), and STYRENE (119); 
C       3. removed dissociation  calculation for formaldedhyde because the listed value 
C          is already an effective rate (Sander ACP, 2015, note 187)
C       4. added dissociation constant for acrylic acid (LACRYACID) (via B. Hutzell)
C  H.Pye       06/2017  added semivolatile monoterpene products
C  G.Sarwar    08/2018  added Henry's Law constant for ClNO3, FMBR, IBR, and updated iodine and bromine species
C  G. Sarwar   11/2018  added Henry's Law constant for CLO
C  H.Pye       09/2021 added CRACMM emission weighted surrogates
C-----------------------------------------------------------------------

      USE UTILIO_DEFN

      IMPLICIT NONE

C...........ARGUMENTS and their descriptions

      CHARACTER*(*), INTENT ( IN ) :: CNAME      ! name of substance
      REAL,          INTENT ( IN ) :: TEMP       ! temperature (K)
      LOGICAL,       INTENT ( IN ) :: EFFECTIVE  ! true=compute the effective henry's law constant
      REAL,          INTENT ( IN ) :: HPLUS      ! hydrogen ion concentration (mol/l)

C...........INCLUDES and their descriptions


C...........PARAMETERS and their descriptions:

      INTEGER, PARAMETER :: MXSPCS = 277     ! Number of substances
      INTEGER, PARAMETER :: MXDSPCS = 19     ! Number of dissociating species

C...pointers for the dissociation constants (array B and D)

      INTEGER, PARAMETER :: LSO2       =  1  ! SO2
      INTEGER, PARAMETER :: LHSO3      =  2  ! HSO3
      INTEGER, PARAMETER :: LHNO2      =  3  ! HNO3
      INTEGER, PARAMETER :: LHNO3      =  4  ! HNO3
      INTEGER, PARAMETER :: LCO2       =  5  ! CO2
      INTEGER, PARAMETER :: LHCO3      =  6  ! HCO3
      INTEGER, PARAMETER :: LH2O2      =  7  ! H2O2
      INTEGER, PARAMETER :: LHCOOH     =  8  ! HCOOH
      INTEGER, PARAMETER :: LHO2       =  9  ! HO2
      INTEGER, PARAMETER :: LNH4OH     = 10  ! NH4OH
      INTEGER, PARAMETER :: LH2O       = 11  ! H2O
      INTEGER, PARAMETER :: LATRA      = 12  ! Atrazine
      INTEGER, PARAMETER :: LCL2       = 13  ! CL2
      INTEGER, PARAMETER :: LHOCL      = 14  ! HOCL
      INTEGER, PARAMETER :: LHCL       = 15  ! HCL
      INTEGER, PARAMETER :: LHYDRAZINE = 16  ! Hydrazine
      INTEGER, PARAMETER :: LHBR       = 17  ! HBR
      INTEGER, PARAMETER :: LHI        = 18  ! HI
      INTEGER, PARAMETER :: LACRYACID  = 19  ! CCH3COOH


C...........SCRATCH LOCAL VARIABLES and their descriptions:

      CHARACTER( 16 ), SAVE :: PNAME = 'HLCONST'  ! program name
      CHARACTER( 16 ), SAVE :: SUBNAME( MXSPCS )  ! list of substance names

      CHARACTER( 120 ) :: XMSG = ' '    ! exit status message string

      INTEGER       S                   ! loop index
      INTEGER       SPC                 ! species index

      REAL          HPLUSI              ! 1 / HPLUS
      REAL          HPLUS2I             ! 1 / HPLUS**2
      REAL          CLMINUS             ! chlorine ion conc [CL-]
      REAL          CLMINUSI            ! 1 / CLMINUS
      REAL          TFAC                ! (298-T)/(T*298)
      REAL          AKEQ1               ! temp var for dissociation constant
      REAL          AKEQ2               ! temp var for dissociation constant
      REAL          OHION               ! OH ion concentration
      REAL          KH                  ! temp var for henry's law constant

C...Henry's law constant data taken mostly from Rolf Sanders' Compilation of
C...  Henry's Law Constants for Inorganic and Organic Species of Potential
C...  Importance in Environment Chemistry 1999

      REAL, SAVE :: A( MXSPCS )         ! Henry's law constants at 298.15K (M/atm)
      REAL, SAVE :: E( MXSPCS )         ! enthalpy (like activation energy) (K)

C...dissociation constant data taken mostly from 6.A.1 of Seinfeld and Pandis
C...  Atmospheric Chemistry and Physics, 1997

      REAL, SAVE :: B( MXDSPCS )        ! dissociation constant at 298.15K (M or M2)
      REAL, SAVE :: D( MXDSPCS )        ! -dH/R (K)

      DATA SUBNAME(  1), A(  1), E(  1) / 'O3              ', 1.14E-02, 2.3E+03 / ! Kosak 1983
      DATA SUBNAME(  2), A(  2), E(  2) / 'HO2             ', 4.0E+03, 5.9E+03 /  ! Hanson et al. 1992
      DATA SUBNAME(  3), A(  3), E(  3) / 'H2O2            ', 1.1E+05, 7.4E+03 /  ! E: O'Sullivan et al. 1996, A: Sander 1999 as used in Nguyen et al. 2015 PNAS
      DATA SUBNAME(  4), A(  4), E(  4) / 'NH3             ', 6.1E+01, 4.2E+03 /  ! Clegg and Brimblecombe 1989
      DATA SUBNAME(  5), A(  5), E(  5) / 'NO              ', 1.9E-03, 1.4E+03 /  ! Lide and Frederikse 1995
      DATA SUBNAME(  6), A(  6), E(  6) / 'NO2             ', 1.2E-02, 2.5E+03 /  ! Chameides 1984
      DATA SUBNAME(  7), A(  7), E(  7) / 'NO3             ', 0.6E+00, 0.0E+00 /  ! Rudich, Talukdar et al.1996
      DATA SUBNAME(  8), A(  8), E(  8) / 'N2O5            ', 1.0E+30, 0.0E+00 /  ! "inf" Sander and Crutzen 1996
      DATA SUBNAME(  9), A(  9), E(  9) / 'HNO2            ', 5.0E+01, 4.9E+03 /  ! Becker et al. 1996
      DATA SUBNAME( 10), A( 10), E( 10) / 'HNO3            ', 2.1E+05, 8.7E+03 /  ! Leieveld and Crutzen 1991
      DATA SUBNAME( 11), A( 11), E( 11) / 'HNO4            ', 1.2E+04, 6.9E+03 /  ! Regimbal and Mozurkewich 1997
      DATA SUBNAME( 12), A( 12), E( 12) / 'SO2             ', 1.4E+00, 2.9E+03 /  ! Linde and Frederikse 1995
      DATA SUBNAME( 13), A( 13), E( 13) / 'H2SO4           ', 1.0E+30, 0.0E+00 /  ! infinity
      DATA SUBNAME( 14), A( 14), E( 14) / 'METHANE         ', 1.4E-03, 1.6E+03 /  ! Linde and Frederikse 1995
      DATA SUBNAME( 15), A( 15), E( 15) / 'ETHANE          ', 1.9E-03, 2.3E+03 /  ! Linde and Frederikse 1995
      DATA SUBNAME( 16), A( 16), E( 16) / 'PROPANE         ', 1.5E-03, 2.7E+03 /  ! Linde and Frederikse 1995
      DATA SUBNAME( 17), A( 17), E( 17) / 'BUTANE          ', 1.1E-03, 0.0E+00 /  ! Mackay and Shiu 1981
      DATA SUBNAME( 18), A( 18), E( 18) / 'PENTANE         ', 8.1E-04, 0.0E+00 /  ! Mackay and Shiu 1981
      DATA SUBNAME( 19), A( 19), E( 19) / 'HEXANE          ', 6.18E-04,3.8E+03 /  ! Sander, ACP, 2015; Abraham Matteoli (1988)
      DATA SUBNAME( 20), A( 20), E( 20) / 'OCTANE          ', 2.9E-03, 7.8E+03 /  ! Hansen et al. 1993
      DATA SUBNAME( 21), A( 21), E( 21) / 'NONANE          ', 2.4E-03, 2.1E+02 /  ! Ashworth, Howe et al 1988
      DATA SUBNAME( 22), A( 22), E( 22) / 'DECANE          ', 1.4E-04, 0.0E+00 /  ! Mackay and Shiu 1981
      DATA SUBNAME( 23), A( 23), E( 23) / 'ETHENE          ', 4.7E-03, 0.0E+00 /  ! Mackay and Shiu 1981
      DATA SUBNAME( 24), A( 24), E( 24) / 'PROPENE         ', 4.8E-03, 0.0E+00 /  ! Mackay and Shiu 1981
      DATA SUBNAME( 25), A( 25), E( 25) / 'ISOPRENE        ', 2.8E-02, 0.0E+00 /  ! Karl, Lindinger et al 2003
      DATA SUBNAME( 26), A( 26), E( 26) / 'ACETYLENE       ', 4.1E-02, 1.8E+03 /  ! Wilhelm et al. 1977
      DATA SUBNAME( 27), A( 27), E( 27) / 'BENZENE         ', 1.6E-01, 4.1E+03 /  ! Staudinger and Roberts 1996
      DATA SUBNAME( 28), A( 28), E( 28) / 'TOLUENE         ', 1.5E-01, 4.0E+03 /  ! Staudinger and Roberts 1996
      DATA SUBNAME( 29), A( 29), E( 29) / 'O-XYLENE        ', 1.9E-01, 4.0E+03 /  ! Staudinger and Roberts 1996
      DATA SUBNAME( 30), A( 30), E( 30) / 'METHANOL        ', 2.2E+02, 5.2E+03 /  ! Snider and Dawson 1985
      DATA SUBNAME( 31), A( 31), E( 31) / 'ETHANOL         ', 1.9E+02, 6.6E+03 /  ! Snider and Dawson 1985
      DATA SUBNAME( 32), A( 32), E( 32) / '2-CRESOL        ', 8.2E+02, 0.0E+00 /  ! Betterton 1992
      DATA SUBNAME( 33), A( 33), E( 33) / '4-CRESOL        ', 1.3E+02, 0.0E+00 /  ! Betterton 1992
      DATA SUBNAME( 34), A( 34), E( 34) / 'METHYLHYDROPEROX', 3.1E+02, 5.2E+03 /  ! O'Sullivan et al. 1996
      DATA SUBNAME( 35), A( 35), E( 35) / 'FORMALDEHYDE    ', 3.2E+03, 6.8E+03 /  ! Staudinger and Roberts 1996
      DATA SUBNAME( 36), A( 36), E( 36) / 'ACETALDEHYDE    ', 1.4E+01, 5.6E+03 /  ! Staudinger and Roberts 1996
      DATA SUBNAME( 37), A( 37), E( 37) / 'GENERIC_ALDEHYDE', 4.2E+03, 0.0E+00 /  ! Graedel and Goldberg 1983
      DATA SUBNAME( 38), A( 38), E( 38) / 'GLYOXAL         ', 3.6E+05, 0.0E+00 /  ! Zhou and Mopper 1990
      DATA SUBNAME( 39), A( 39), E( 39) / 'ACETONE         ', 3.0E+01, 4.6E+03 /  ! Staudinger and Roberts 1996
      DATA SUBNAME( 40), A( 40), E( 40) / 'FORMIC_ACID     ', 8.9E+03, 6.1E+03 /  ! Johnson et al. 1996
      DATA SUBNAME( 41), A( 41), E( 41) / 'ACETIC_ACID     ', 4.1E+03, 6.3E+03 /  ! Johnson et al. 1996
      DATA SUBNAME( 42), A( 42), E( 42) / 'METHYL_GLYOXAL  ', 3.2E+04, 0.0E+00 /  ! Zhou and Mopper 1990
      DATA SUBNAME( 43), A( 43), E( 43) / 'CO              ', 9.9E-04, 1.3E+03 /  ! Linde and Frederikse 1995
      DATA SUBNAME( 44), A( 44), E( 44) / 'CO2             ', 3.6E-02, 2.2E+03 /  ! Zheng et al. 1997
      DATA SUBNAME( 45), A( 45), E( 45) / 'PAN             ', 2.8E+00, 6.5E+03 /  ! Kames et al. 1991
      DATA SUBNAME( 46), A( 46), E( 46) / 'MPAN            ', 1.7E+00, 0.0E+00 /  ! Kames and Schurath 1995
      DATA SUBNAME( 47), A( 47), E( 47) / 'OH              ', 3.0E+01, 4.5E+03 /  ! Hanson et al. 1992
      DATA SUBNAME( 48), A( 48), E( 48) / 'METHYLPEROXY_RAD', 2.0E+03, 6.6E+03 /  ! Lelieveld and Crutzen 1991
      DATA SUBNAME( 49), A( 49), E( 49) / 'PEROXYACETIC_ACI', 8.4E+02, 5.3E+03 /  ! O'Sullivan et al. 1996
      DATA SUBNAME( 50), A( 50), E( 50) / 'PROPANOIC_ACID  ', 5.7E+03, 0.0E+00 /  ! Kahn et al. 1995
      DATA SUBNAME( 51), A( 51), E( 51) / '2-NITROPHENOL   ', 7.0E+01, 4.6E+03 /  ! USEPA 1982
      DATA SUBNAME( 52), A( 52), E( 52) / 'PHENOL          ', 3.3E+03, 3.6E+03 /  ! Meylan and Howard (1991)
      DATA SUBNAME( 53), A( 53), E( 53) / 'BIACETYL        ', 7.4E+01, 5.7E+03 /  ! Betteron 1991
      DATA SUBNAME( 54), A( 54), E( 54) / 'BENZALDEHYDE    ', 3.9E+01, 4.8E+03 /  ! Staudinger and Roberts 1996
      DATA SUBNAME( 55), A( 55), E( 55) / 'PINENE          ', 4.9E-02, 0.0E+00 /  ! Karl and Lindinger 1997
      DATA SUBNAME( 56), A( 56), E( 56) / 'ATRA            ', 4.1E+05, 6.0E+03 /  ! CIBA Corp (1989) and Scholtz (1999)
      DATA SUBNAME( 57), A( 57), E( 57) / 'DATRA           ', 4.1E+05, 6.0E+03 /  ! assumed same as Atrazine
      DATA SUBNAME( 58), A( 58), E( 58) / 'ADIPIC_ACID     ', 2.0E+08, 0.0E+00 /  ! Saxena and Hildemann (1996)
      DATA SUBNAME( 59), A( 59), E( 59) / 'ACROLEIN        ', 8.2E+00, 0.0E+00 /  ! Meylan and Howard (1991)
      DATA SUBNAME( 60), A( 60), E( 60) / '13-BUTADIENE    ', 1.4E-02, 0.0E+00 /  ! Mackay and Shiu (1981)
      DATA SUBNAME( 61), A( 61), E( 61) / 'ACRYLONITRILE   ', 7.3E+00, 0.0E+00 /  ! Meylan and Howard (1991)
      DATA SUBNAME( 62), A( 62), E( 62) / 'CARBONTETRACHLOR', 3.4E-02, 4.2E+03 /  ! Staudinger and Roberts (1996)
      DATA SUBNAME( 63), A( 63), E( 63) / 'PROPYLENE_DICHLO', 3.4E-01, 4.3E+03 /  ! Staudinger and Roberts (1996)
      DATA SUBNAME( 64), A( 64), E( 64) / '13DICHLOROPROPEN', 6.5E-01, 4.2E+03 /  ! Wright et al (1992b)
      DATA SUBNAME( 65), A( 65), E( 65) / '1122-CL4ETHANE  ', 2.4E+00, 3.2E+03 /  ! Staudinger and Roberts (1996)
      DATA SUBNAME( 66), A( 66), E( 66) / 'CHLOROFORM      ', 2.5E-01, 4.5E+03 /  ! Staudinger and Roberts (1996)
      DATA SUBNAME( 67), A( 67), E( 67) / '12DIBROMOETHANE ', 1.5E+00, 3.9E+03 /  ! Ashworth et al (1988)
      DATA SUBNAME( 68), A( 68), E( 68) / '12DICHLOROETHANE', 7.3E-01, 4.2E+03 /  ! Staudinger and Roberts (1996)
      DATA SUBNAME( 69), A( 69), E( 69) / 'METHYLENE_CHLORI', 3.6E-01, 4.1E+03 /  ! Staudinger and Roberts (1996)
      DATA SUBNAME( 70), A( 70), E( 70) / 'PERCHLOROETHYLEN', 5.9E-02, 4.8E+03 /  ! Staudinger and Roberts (1996)
      DATA SUBNAME( 71), A( 71), E( 71) / 'TRICHLOROETHENE ', 1.0E-01, 4.6E+03 /  ! Staudinger and Roberts (1996)
      DATA SUBNAME( 72), A( 72), E( 72) / 'VINYL_CHLORIDE  ', 3.9E-02, 3.1E+03 /  ! Staudinger and Roberts (1996)
      DATA SUBNAME( 73), A( 73), E( 73) / 'ETHYLENE_OXIDE  ', 8.4E+00, 0.0E+00 /  ! CRC
      DATA SUBNAME( 74), A( 74), E( 74) / 'PPN             ', 2.9E+00, 0.0E+00 /  ! Kames and Schurath (1995)
      DATA SUBNAME( 75), A( 75), E( 75) / 'NAPHTHALENE     ', 2.0E+00, 3.6E+03 /  ! USEPA 1982
      DATA SUBNAME( 76), A( 76), E( 76) / 'QUINOLINE       ', 3.7E+03, 5.4E+03 /  ! USEPA 1982
      DATA SUBNAME( 77), A( 77), E( 77) / 'MEK             ', 2.0E+01, 5.0E+03 /  ! Zhou and Mopper 1990
      DATA SUBNAME( 78), A( 78), E( 78) / 'MVK             ', 4.1E+01, 0.0E+00 /  ! Iraci et al. 1998
      DATA SUBNAME( 79), A( 79), E( 79) / 'METHACROLEIN    ', 6.5E+00, 0.0E+00 /  ! Iraci et al. 1998
      DATA SUBNAME( 80), A( 80), E( 80) / 'CL2             ', 8.6E-02, 2.0E+03 /  ! ROLF SANDERS COMPILATION (1999)/KAVANAUGH AND TRUSSELL (1980)
      DATA SUBNAME( 81), A( 81), E( 81) / 'HOCL            ', 6.6E+02, 5.9E+03 /  ! ROLF SANDERS COMPILATION (1999)/HUTHWELKER ET AL (1995)
      DATA SUBNAME( 82), A( 82), E( 82) / 'HCL             ', 1.9E+01, 6.0E+02 /  ! ROLF SANDERS COMPILATION (1999)/DEAN (1992)
      DATA SUBNAME( 83), A( 83), E( 83) / 'FMCL            ', 1.1E+00, 0.0E+00 /  ! EPA SUITE PROGRAM/UNIT CONVERTED TO MATCH THE DEFINITION BY ROLF SANDERS.
      DATA SUBNAME( 84), A( 84), E( 84) / 'ICL1            ', 6.9E+01, 0.0E+00 /  ! EPA SUITE PROGRAM/UNIT CONVERTED TO MATCH THE DEFINITION BY ROLF SANDERS.
      DATA SUBNAME( 85), A( 85), E( 85) / 'ICL2            ', 6.9E+01, 0.0E+00 /  ! EPA SUITE PROGRAM/ASSUMED EQUAL TO THAT OF ICL1
      DATA SUBNAME( 86), A( 86), E( 86) / 'HG              ', 1.11E-01, 4.97E+03 /! Elemental Mercury from Clever et al. (1985)
      DATA SUBNAME( 87), A( 87), E( 87) / 'HGIIGAS         ', 1.41E+06, 5.26E+03 /! Hg(II) gas as mercuric chloride from Lindqvist and Rodhe (1985)
      DATA SUBNAME( 88), A( 88), E( 88) / 'TECDD_2378      ', 5.1E+00, 3.6E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME( 89), A( 89), E( 89) / 'PECDD_12378     ', 4.6E+00, 3.2E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME( 90), A( 90), E( 90) / 'HXCDD_123478    ', 8.1E+00, 2.9E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME( 91), A( 91), E( 91) / 'HXCDD_123678    ', 2.9E+00, 2.8E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME( 92), A( 92), E( 92) / 'HXCDD_123789    ', 6.5E+00, 2.7E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME( 93), A( 93), E( 93) / 'HPCDD_1234678   ', 1.2E+01, 2.4E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME( 94), A( 94), E( 94) / 'OTCDD           ', 9.8E+00, 2.3E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME( 95), A( 95), E( 95) / 'TECDF_2378      ', 8.5E+01, 3.7E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME( 96), A( 96), E( 96) / 'PECDF_12378     ', 5.2E+01, 2.9E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME( 97), A( 97), E( 97) / 'PECDF_23478     ', 1.8E+02, 3.0E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME( 98), A( 98), E( 98) / 'HXCDF_123478    ', 3.8E+01, 2.4E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME( 99), A( 99), E( 99) / 'HXCDF_123678    ', 9.0E+01, 2.9E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME(100), A(100), E(100) / 'HXCDF_234678    ', 1.0E+02, 2.6E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME(101), A(101), E(101) / 'HXCDF_123789    ', 5.6E+01, 2.6E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME(102), A(102), E(102) / 'HPCDF_1234678   ', 2.8E+01, 1.6E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME(103), A(103), E(103) / 'HPCDF_1234789   ', 8.0E+01, 2.1E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME(104), A(104), E(104) / 'OTCDF           ', 7.6E+01, 2.4E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME(105), A(105), E(105) / 'NAPHTHOL        ', 3.60E+03, 0.0E+00 / ! Eabraham et al. (1994)
      DATA SUBNAME(106), A(106), E(106) / '1NITRONAPHTHALEN', 5.68E+02, 0.0E+00 / ! Altschuh et al. (1999)
      DATA SUBNAME(107), A(107), E(107) / '2NITRONAPHTHALEN', 6.42E+02, 0.0E+00 / ! HENRYWIN v3.10 (Meylan and Howard, 1991)
      DATA SUBNAME(108), A(108), E(108) / '14NAPHTHOQUINONE', 5.08E+05, 0.0E+00 / ! HENRYWIN v3.10 (Meylan and Howard, 1991)
      DATA SUBNAME(109), A(109), E(109) / '24-TOLUENE_DIIS ', 7.25E+00, 0.0E+00 / ! HENRYWIN v3.10 (Meylan and Howard, 1991)
      DATA SUBNAME(110), A(110), E(110) / 'HEXAMETHYLE_DIIS', 2.08E+01, 0.0E+00 / ! HENRYWIN v3.10 (Meylan and Howard, 1991)
      DATA SUBNAME(111), A(111), E(111) / 'HYDRAZINE       ', 1.14E+03, 0.0E+00 / ! Daubert and Danner (1989), and Amoore and Hautala (1983)
      DATA SUBNAME(112), A(112), E(112) / 'MALEIC_ANHYDRIDE', 2.54E+02, 0.0E+00 / ! HENRYWIN v3.10 (Meylan and Howard, 1991)
      DATA SUBNAME(113), A(113), E(113) / 'TRIETHYLAMINE   ', 6.71E+00, 0.0E+00 / ! Yalkowsky and Dannenfelser (1992), and Riddick et al. (1986)
      DATA SUBNAME(114), A(114), E(114) / 'P_DICHLOROBENZEN', 2.38E+00, 0.0E+00 / ! MacKay and Shiu (1981), measured
      DATA SUBNAME(115), A(115), E(115) / 'M-XYLENE        ', 1.43E-01, 3.9E+03 / ! Staudinger and Roberts (2001)
      DATA SUBNAME(116), A(116), E(116) / 'P-XYLENE        ', 1.35E-01, 3.7E+03 / ! Staudinger and Roberts (2001)

      DATA SUBNAME(117), A(117), E(117) / 'N-HEXANE        ', 6.18E-04, 3.8E+03 /  ! Sander, ACP, 2015, Abraham Matteoli (1988)
      DATA SUBNAME(118), A(118), E(118) / 'PROPANAL        ', 1.64E+00, 5.4E+03 /  ! Staudinger and Roberts (2001)
      DATA SUBNAME(119), A(119), E(119) / 'STYRENE         ', 2.74E-01, 0.0E+00 /  ! Sander, ACP, 2015, Kim and Kim (2014)
      DATA SUBNAME(120), A(120), E(120) / '124TRIMETH_BENZE', 1.50E-01, 4.3E+03 /  ! Hanson et al. (1995)
      DATA SUBNAME(121), A(121), E(121) / 'CHLOROACETALDEHY', 4.18E+01, 0.0E+00 /  ! Bond Estimate by EPA SUITE PROGRAM  (2007)
      DATA SUBNAME(122), A(122), E(122) / 'CHLOROACETONE   ', 3.82E-01, 0.0E+00 /  ! Staudinger and Roberts (2001)

      DATA SUBNAME(123), A(123), E(123) / 'NITROSYL_CHLORID', 5.10E-01, 0.0E+00 /  ! Bond Estimate by EPA SUITE PROGRAM  (2007)
      DATA SUBNAME(124), A(124), E(124) / 'NITRYL_CHLORIDE ', 2.43E-02, 0.0E+00 /  ! Behnke et al. (1997), JGR, D102, 3795-3804
      DATA SUBNAME(125), A(125), E(125) / 'HEXYL_HYDROPEROX', 1.23E+01, 0.0E+00 /  ! Bond Estimated by EPA SUITE PROGRAM  (2007)

      DATA SUBNAME(126), A(126), E(126) / 'HYDROXY_NITRATES', 1.70E+04, 9.20E+03 /  ! Shepson et al. (1996), Treves et al. (2000)
      DATA SUBNAME(127), A(127), E(127) / 'HYDROXY_PEROXIDE', 8.30E+04, 7.40E+03 /  ! using value for H2O2
      DATA SUBNAME(128), A(128), E(128) / 'PROPNN          ', 1.00E+04, 0.00E+00 /  ! Nguyen et al. (2015) PNAS using Shepson (1996)
      DATA SUBNAME(129), A(129), E(129) / 'HACET           ', 2.93E+03, 0.00E+00 /  ! Spaulding et al. (2002)
      DATA SUBNAME(130), A(130), E(130) / 'PYRUVIC_ACID    ', 3.00E+05, 5.20E+03 /  ! Staudinger and Roberts (1996) 
      DATA SUBNAME(131), A(131), E(131) / 'XYLENOL         ', 1.05E+03, 0.00E+03 /  ! experimental value from EPA SUITE PROGRAM  (2008)
      DATA SUBNAME(132), A(132), E(132) / 'CATECHOL        ', 3.16E+05, 0.00E+03 /  ! experimental value from EPA SUITE PROGRAM  (2008)
      DATA SUBNAME(133), A(133), E(133) / 'IEPOX           ', 3.0E+07,  0.00E+00 /  ! Nguyen et al. ACP (2014)
      DATA SUBNAME(134), A(134), E(134) / 'IMAE            ', 1.2E+05,  0.00E+00 /  ! Pye et al. ES&T (2013)

      DATA SUBNAME(135), A(135), E(135) / 'DIETHYLKETONE   ', 2.00E+01, 9.2E+03 /   ! ROLF SANDERS COMPILATION (1999)/JANINI AND QUADDORA (1986)
      DATA SUBNAME(136), A(136), E(136) / 'N-PROPANOL      ', 1.10E+02, 0.0E+00 /   ! ROLF SANDERS COMPILATION (1999)/YAWS AND YANG (1992)
      DATA SUBNAME(137), A(137), E(137) / 'ETHYLENEGLYCOL  ', 4.00E+06, 0.0E+00 /   ! ROLF SANDERS COMPILATION (1999)/BONE ET AL. (1983)
      DATA SUBNAME(138), A(138), E(138) / 'HYDROXY-ACETONE ', 2.00E+04, 0.0E+00 /   ! MATSUNAGA ET AL., 41, 790-796, AE, (2007)            
      DATA SUBNAME(139), A(139), E(139) / '2NITRO_1BUTNL   ', 6.30E+03, 9.6E+03 /   ! Shepson et al., 1992 
      DATA SUBNAME(140), A(140), E(140) / 'PYROCATECHOL    ', 4.60E+03, 0.0E+00 /   ! Mackay et al., 1995            
      DATA SUBNAME(141), A(141), E(141) / 'NTR_ALK         ', 6.50E-01, 5.4E+03 /   ! 2-butylnitrate, from Sander
      DATA SUBNAME(142), A(142), E(142) / 'NTR_OH          ', 6.50E+03, 9.5E+03 /   ! 3-nitrooxy-butanol, from Sander
      DATA SUBNAME(143), A(143), E(143) / 'DINTR           ', 1.80E+01, 0.0E+00 /   ! 2,3-pentanedinitrate
      DATA SUBNAME(144), A(144), E(144) / 'NTR_PX          ', 3.40E+02, 6.0E+03 /   ! ethylhydroperoxide 
      DATA SUBNAME(145), A(145), E(145) / 'MTNO3           ', 1.5E+06,  0.0E+00 /   ! monoterpene nitrates HenryWin v3.2 
      DATA SUBNAME(146), A(146), E(146) / 'ISOPNN          ', 4.5E+08,  0.0E+00 /   ! isoprene dinitrate HenryWin v3.2
      DATA SUBNAME(147), A(147), E(147) / 'SVALK1          ', 6.2E+08,  6.0E+03 /   ! Pye 2016 est based on Hodzic 2014 GRL
      DATA SUBNAME(148), A(148), E(148) / 'SVALK2          ', 4.5E+06,  6.0E+03 /   ! Pye 2016 est based on Hodzic 2014 GRL
      DATA SUBNAME(149), A(149), E(149) / 'SVBNZ1          ', 2.1E+08,  6.0E+03 /   ! Pye 2016 est based on Hodzic 2014 GRL
      DATA SUBNAME(150), A(150), E(150) / 'SVBNZ2          ', 2.0E+06,  6.0E+03 /   ! Pye 2016 est based on Hodzic 2014 GRL
      DATA SUBNAME(151), A(151), E(151) / 'SVISO1          ', 4.3E+07,  6.0E+03 /   ! Pye 2016 est based on Hodzic 2014 GRL
      DATA SUBNAME(152), A(152), E(152) / 'SVISO2          ', 3.7E+09,  6.0E+03 /   ! Pye 2016 est based on Hodzic 2014 GRL
      DATA SUBNAME(153), A(153), E(153) / 'SVPAH1          ', 5.1E+07,  6.0E+03 /   ! Pye 2016 est based on Hodzic 2014 GRL
      DATA SUBNAME(154), A(154), E(154) / 'SVPAH2          ', 7.2E+05,  6.0E+03 /   ! Pye 2016 est based on Hodzic 2014 GRL
      DATA SUBNAME(155), A(155), E(155) / 'SVSQT           ', 6.2E+08,  6.0E+03 /   ! Pye 2016 est based on Hodzic 2014 GRL
      DATA SUBNAME(156), A(156), E(156) / 'SVTOL1          ', 4.2E+07,  6.0E+03 /   ! Pye 2016 est based on Hodzic 2014 GRL
      DATA SUBNAME(157), A(157), E(157) / 'SVTOL2          ', 7.3E+06,  6.0E+03 /   ! Pye 2016 est based on Hodzic 2014 GRL
      DATA SUBNAME(158), A(158), E(158) / 'SVTRP1          ', 9.9E+08,  6.0E+03 /   ! Pye 2016 est based on Hodzic 2014 GRL
      DATA SUBNAME(159), A(159), E(159) / 'SVTRP2          ', 1.4E+08,  6.0E+03 /   ! Pye 2016 est based on Hodzic 2014 GRL
      DATA SUBNAME(160), A(160), E(160) / 'SVXYL1          ', 6.2E+07,  6.0E+03 /   ! Pye 2016 est based on Hodzic 2014 GRL
      DATA SUBNAME(161), A(161), E(161) / 'SVXYL2          ', 4.0E+06,  6.0E+03 /   ! Pye 2016 est based on Hodzic 2014 GRL

      DATA SUBNAME(162), A(162), E(162) / 'I2              ', 3.10E+00, 4.6E+03 /   ! ROLF SANDERS COMPILATION (1999)/Berdnikov and Bazhin (1970)
      DATA SUBNAME(163), A(163), E(163) / 'I               ', 6.30E-03, 2.3E+03 /   ! ROLF SANDERS COMPILATION (1999)/Berdnikov and Bazhin (1970)
      DATA SUBNAME(164), A(164), E(164) / 'IO              ', 4.50E+02, 0.0E+00 /   ! Saiz-Lopez et al., ACPD, 2014
      DATA SUBNAME(165), A(165), E(165) / 'OIO             ', 1.00E+04, 0.0E+00 /   ! Saiz-Lopez et al., ACPD, 2014
      DATA SUBNAME(166), A(166), E(166) / 'I2O2            ', 1.00E+30, 0.0E+00 /   ! Sherwen et al., 2016
      DATA SUBNAME(167), A(167), E(167) / 'I2O3            ', 1.00E+30, 0.0E+00 /   ! Sherwen et al., 2016
      DATA SUBNAME(168), A(168), E(168) / 'I2O4            ', 1.00E+30, 0.0E+00 /   ! Sherwen et al., 2016
      DATA SUBNAME(169), A(169), E(169) / 'HI              ', 2.50E+00, 5.3E+03 /   ! Schweitzer et al. (2000)
      DATA SUBNAME(170), A(170), E(170) / 'HOI             ', 1.53E+04, 8.3E+03 /   ! Sherwen et al., 2016      

      DATA SUBNAME(171), A(171), E(171) / 'INO             ', 5.00E-02, 0.0E+00 /   ! Same as ClNO which is from ROLF SANDERS COMPILATION (1999)/Scheer et al. (1997)
      DATA SUBNAME(172), A(172), E(172) / 'INO2            ', 3.00E-01, 0.0E+00 /   ! Same as BRNO2 which is from ROLF SANDERS COMPILATION (1999)/Frenzel et al. (1998)
      DATA SUBNAME(173), A(173), E(173) / 'INO3            ', 1.00E+30, 0.0E+00 /   ! Same as BRNO3; BRNO3 from Sander (2015)      
      DATA SUBNAME(174), A(174), E(174) / 'CH3I            ', 1.40E-01, 4.3E+03 /   ! ROLF SANDERS COMPILATION (1999)/Moore et al. (1995)
      DATA SUBNAME(175), A(175), E(175) / 'MIC             ', 8.90E-01, 4.3E+03 /   ! ROLF SANDERS COMPILATION (1999)/Moore et al. (1995)
      DATA SUBNAME(176), A(176), E(176) / 'MIB             ', 8.90E-01, 4.3E+03 /   ! Same as MIC, Final Report to TCEQ, Environ International, 2012
      DATA SUBNAME(177), A(177), E(177) / 'MI2             ', 2.30E+00, 5.0E+03 /   ! ROLF SANDERS COMPILATION (1999)/Moore et al. (1995)

      DATA SUBNAME(178), A(178), E(178) / 'BRO             ', 7.10E-01, 0.0E+00 /   ! Parrella et al. (2012)
      DATA SUBNAME(179), A(179), E(179) / 'HOBR            ', 6.10E+03, 6.0E+03 /   ! Sherwen et al., 2016
      DATA SUBNAME(180), A(180), E(180) / 'HBR             ', 7.50E-01, 6.1E+03 /   ! A from Schweitzer et al. (2000) and E from Chmeides and Stelson (1992) - ROLF SANDERS COMPILATION (1999)
      DATA SUBNAME(181), A(181), E(181) / 'BR2             ', 7.60E-01, 4.1E+03 /   ! Dean et al. (1992)
      DATA SUBNAME(182), A(182), E(182) / 'BRNO3           ', 1.00E+30, 0.0E+00 /   ! Sander (2015)

      DATA SUBNAME(183), A(183), E(183) / 'MB3             ', 1.40E+00, 5.0E+03 /   ! ROLF SANDERS COMPILATION (1999)/Moore et al. (1995)
      DATA SUBNAME(184), A(184), E(184) / 'CH3BR           ', 1.50E-01, 0.0E+00 /   ! ROLF SANDERS COMPILATION (1999)/Yaws and Yang et al. (1992)
      DATA SUBNAME(185), A(185), E(185) / 'MB2             ', 9.30E-01, 4.4E+03 /   ! ROLF SANDERS COMPILATION (1999)/Moore et al. (1995)
      DATA SUBNAME(186), A(186), E(186) / 'MB2C            ', 7.30E-01, 4.9E+03 /   ! ROLF SANDERS COMPILATION (1999)/Moore et al. (1995)
      DATA SUBNAME(187), A(187), E(187) / 'MBC2            ', 4.00E-01, 4.7E+03 /   ! ROLF SANDERS COMPILATION (1999)/Moore et al. (1995)
      DATA SUBNAME(188), A(188), E(188) / 'MBC             ', 8.90E-01, 4.3E+03 /   ! Same as MIC
      DATA SUBNAME(189), A(189), E(189) / 'BRNO2           ', 3.00E-01, 0.0E+00 /   ! Frenzel et al. (1998)
      DATA SUBNAME(190), A(190), E(190) / 'BRCL            ', 9.70E-01, 0.0E+00 /   ! Sherwen et al., 2016
      DATA SUBNAME(191), A(191), E(191) / 'ICL             ', 1.10E+02, 2.1E+03 /   ! Sherwen et al., 2016

      DATA SUBNAME(192), A(192), E(192) / 'DMS             ', 4.80E-01, 3.1E+03 /   ! ROLF SANDERS COMPILATION (1999)/De Bryuyn et al. (1995)
      DATA SUBNAME(193), A(193), E(193) / 'MSA             ', 7.70E+04, 0.0E+00 /   ! Meylan and Howard (1991), inverse of the value is used due to unit conversion
      DATA SUBNAME(194), A(194), E(194) / 'XYLENE          ', 1.90E-01, 4.2E+03  / ! .35/.3/.35 m/o/p, Sander ACP 2015, Staudinger and Roberts (2001) for m; Fogg and Sangster (2003) for o and p
      DATA SUBNAME(195), A(195), E(195) / 'ETHYLBENZENE    ', 1.42E-01, 4.8E+03 / ! Sander ACP 2015, Fogg Sangster (2003)
      DATA SUBNAME(196), A(196), E(196) / 'ACRYACID        ', 3.141E+03, 0.0E+00 / ! Sander ACP 2015, Lide Frederikse (1995)
      DATA SUBNAME(197), A(197), E(197) / 'ACETONITRILE    ', 5.269E+01, 4.0E+03 / ! Sander ACP 2015, Sander et al (2011)
      DATA SUBNAME(198), A(198), E(198) / 'CHLOROPRENE     ', 4.760E+00, 0.0E+00 / ! Sander ACP 2015, Mackay et al. (1993)
      DATA SUBNAME(199), A(199), E(199) / 'METHCHLORIDE    ', 1.320E-01, 3.3E+03 / ! Sander ACP 2015, Sander et al. (2011)
      DATA SUBNAME(200), A(200), E(200) / 'CARBSULFIDE     ', 2.130E-02, 3.3E+03 / ! Sander ACP 2015, Warneck Williams (2012)
      DATA SUBNAME(201), A(201), E(201) / '6_NITRO_O_CRESOL', 2.938E+01, 0.0E+00 / ! 6-methyl-2-nitrophenol from Sander ACP 2015, Tremp et. al (1992)
      DATA SUBNAME(202), A(202), E(202) / 'SVMT1           ', 7.1E+11,  6.0E+03 /   ! est based on Hodzic 2014 GRL
      DATA SUBNAME(203), A(203), E(203) / 'SVMT2           ', 8.9E+10,  6.0E+03 /   ! est based on Hodzic 2014 GRL
      DATA SUBNAME(204), A(204), E(204) / 'SVMT3           ', 1.1E+10,  6.0E+03 /   ! est based on Hodzic 2014 GRL
      DATA SUBNAME(205), A(205), E(205) / 'SVMT4           ', 1.4E+09,  6.0E+03 /   ! est based on Hodzic 2014 GRL
      DATA SUBNAME(206), A(206), E(206) / 'SVMT5           ', 1.8E+08,  6.0E+03 /   ! est based on Hodzic 2014 GRL
      DATA SUBNAME(207), A(207), E(207) / 'SVMT6           ', 2.2E+07,  6.0E+03 /   ! est based on Hodzic 2014 GRL
      DATA SUBNAME(208), A(208), E(208) / 'SVMT7           ', 2.8E+06,  6.0E+03 /   ! est based on Hodzic 2014 GRL
      DATA SUBNAME(209), A(209), E(209) / 'SVAVB1          ', 5.5E+09,  6.0E+03 /   ! est based on Hodzic 2014 GRL
      DATA SUBNAME(210), A(210), E(210) / 'SVAVB2          ', 7.9E+07,  6.0E+03 /   ! est based on Hodzic 2014 GRL
      DATA SUBNAME(211), A(211), E(211) / 'SVAVB3          ', 1.2E+07,  6.0E+03 /   ! est based on Hodzic 2014 GRL
      DATA SUBNAME(212), A(212), E(212) / 'SVAVB4          ', 1.8E+06,  6.0E+03 /   ! est based on Hodzic 2014 GRL
      DATA SUBNAME(213), A(213), E(213) / 'CLNO3           ', 1.00E+30, 0.0E+00 /   ! Sander (2015)
      DATA SUBNAME(214), A(214), E(214) / 'IBR             ', 2.43E+01, 4.9E+03 /   ! Sherwen et al., 2016
      DATA SUBNAME(215), A(215), E(215) / 'FMBR            ', 7.40E+01, 0.0E+00 /   ! Sander, R. ACP (2015), krysztofiak et al. (2012)
      DATA SUBNAME(216), A(216), E(216) / 'CLO             ', 7.00E-03, 0.0E+00 /   ! Sander et al., ACP, 2015
      DATA SUBNAME(217), A(217), E(217) / 'BENAPY          ', 2.00E+03, 4.7E+03 /   ! benzo[a]pyrene Ma et al. (2010) and ten Hulscher et al. (1992) in  Sander et al., ACP, 2015
      DATA SUBNAME(218), A(218), E(218) / 'XYEOPERA        ', 1.27E-01, 4.0E+03 /   ! O-xylene T dep from Staudinger and Roberts 1996, CRACMM RepCmp (OPERA SAR)
      DATA SUBNAME(219), A(219), E(219) / 'ROCIOXYOPERA    ', 8.12E+00, 0.0E+00 /   ! emission wt. CRACMM value (OPERA SAR)
      DATA SUBNAME(220), A(220), E(220) / 'UALDOPERA       ', 44.0E+00, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(221), A(221), E(221) / 'OLTOPERA        ', 5.09E-03, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(222), A(222), E(222) / 'OLIOPERA        ', 4.85E-03, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(223), A(223), E(223) / 'BENOPERA        ', 0.18E+00, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(224), A(224), E(224) / 'PHENOPERA       ', 1.83E+04, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(225), A(225), E(225) / 'TOLOPERA        ', 0.15E+00, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(226), A(226), E(226) / 'CSLOPERA        ', 1.42E+03, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(227), A(227), E(227) / 'XYMOPERA        ', 0.14E+00, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(228), A(228), E(228) / 'ISOOPERA        ', 1.86E-02, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(229), A(229), E(229) / 'APIOPERA        ', 7.49E-03, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(230), A(230), E(230) / 'LIMOPERA        ', 4.44E-02, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(231), A(231), E(231) / 'ORA2OPERA       ', 9.95E+03, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(232), A(232), E(232) / 'ACROOPERA       ', 8.28E+00, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(233), A(233), E(233) / 'BDE13OPERA      ', 2.42E-02, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(234), A(234), E(234) / 'PROGOPERA       ', 1.65E+04, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(235), A(235), E(235) / 'FURANOPERA      ', 5.11E+03, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(236), A(236), E(236) / 'SLOWROCOPERA    ', 1.95E+00, 0.0E+00 /   ! emission wt. CRACMM value (OPERA SAR)
      DATA SUBNAME(237), A(237), E(237) / 'SESQOPERA       ', 1.07E-01, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(238), A(238), E(238) / 'NAPHOPERA       ', 2.28E+00, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(239), A(239), E(239) / 'HC3OPERA        ', 2.55E-03, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(240), A(240), E(240) / 'HC5OPERA        ', 8.04E-04, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(241), A(241), E(241) / 'HC10OPERA       ', 6.52E-03, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(242), A(242), E(242) / 'ORA1OPERA       ', 5.95E+03, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(243), A(243), E(243) / 'MCTOPERA        ', 5.80E+03, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(244), A(244), E(244) / 'FURANONEOPERA   ', 7.87E+04, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(245), A(245), E(245) / 'IPCOPERA        ', 4.72E+05, 0.0E+00 /   ! CRACMM RepCMP value (OPERA SAR)
      DATA SUBNAME(246), A(246), E(246) / 'IPNOPERA        ', 4.69E+04, 0.0E+00 /   ! CRACMM RepCMP value (OPERA SAR)
      DATA SUBNAME(247), A(247), E(247) / 'VROCN2OXY2OPERA ', 1.15E+05, 0.0E+00 /   ! CRACMM RepCMP value (OPERA SAR)
      DATA SUBNAME(248), A(248), E(248) / 'VROCN2OXY4OPERA ', 9.71E+05, 0.0E+00 /   ! CRACMM RepCMP value (OPERA SAR)
      DATA SUBNAME(249), A(249), E(249) / 'VROCN2OXY8OPERA ', 3.80E+06, 0.0E+00 /   ! CRACMM RepCMP value (OPERA SAR)
      DATA SUBNAME(250), A(250), E(250) / 'VROCN1OXY1OPERA ', 2.14E+04, 0.0E+00 /   ! CRACMM RepCMP value (OPERA SAR)
      DATA SUBNAME(251), A(251), E(251) / 'VROCN1OXY3OPERA ', 2.87E+06, 0.0E+00 /   ! CRACMM RepCMP value (OPERA SAR)
      DATA SUBNAME(252), A(252), E(252) / 'VROCN1OXY6OPERA ', 1.01E+06, 0.0E+00 /   ! CRACMM RepCMP value (OPERA SAR)
      DATA SUBNAME(253), A(253), E(253) / 'VROCP0OXY2OPERA ', 4.03E+05, 0.0E+00 /   ! CRACMM RepCMP value (OPERA SAR)
      DATA SUBNAME(254), A(254), E(254) / 'VROCP0OXY4OPERA ', 8.55E+05, 0.0E+00 /   ! CRACMM RepCMP value (OPERA SAR)
      DATA SUBNAME(255), A(255), E(255) / 'VROCP1OXY1OPERA ', 4.52E+04, 0.0E+00 /   ! CRACMM RepCMP value (OPERA SAR)
      DATA SUBNAME(256), A(256), E(256) / 'VROCP1OXY3OPERA ', 1.02E+04, 0.0E+00 /   ! CRACMM RepCMP value (OPERA SAR)
      DATA SUBNAME(257), A(257), E(257) / 'VROCP2OXY2OPERA ', 4.93E+03, 0.0E+00 /   ! CRACMM RepCMP value (OPERA SAR)
      DATA SUBNAME(258), A(258), E(258) / 'VROCP3OXY2OPERA ', 1.10E+02, 0.0E+00 /   ! CRACMM RepCMP value (OPERA SAR)
      DATA SUBNAME(259), A(259), E(259) / 'VROCP4OXY2OPERA ', 6.45E+02, 0.0E+00 /   ! CRACMM RepCMP value (OPERA SAR)
      DATA SUBNAME(260), A(260), E(260) / 'VROCP5OXY1OPERA ', 1.57E+00, 0.0E+00 /   ! CRACMM RepCMP value (OPERA SAR)
      DATA SUBNAME(261), A(261), E(261) / 'VROCP6OXY1OPERA ', 1.37E+00, 0.0E+00 /   ! CRACMM RepCMP value (OPERA SAR)
      DATA SUBNAME(262), A(262), E(262) / 'VROCP5AROOPERA  ', 4.15E-01, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(263), A(263), E(263) / 'VROCP6AROOPERA  ', 1.87E-01, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(264), A(264), E(264) / 'VROCN2ALKOPERA  ', 1.34E+03, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(265), A(265), E(265) / 'VROCN1ALKOPERA  ', 1.34E+03, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(266), A(266), E(266) / 'VROCP0ALKOPERA  ', 1.33E+03, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(267), A(267), E(267) / 'VROCP1ALKOPERA  ', 1.33E+03, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(268), A(268), E(268) / 'VROCP2ALKOPERA  ', 1.17E+03, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(269), A(269), E(269) / 'VROCP3ALKOPERA  ', 6.10E+02, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(270), A(270), E(270) / 'VROCP4ALKOPERA  ', 2.94E+01, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(271), A(271), E(271) / 'VROCP5ALKOPERA  ', 5.41E-02, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(272), A(272), E(272) / 'VROCP6ALKOPERA  ', 6.90E-03, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(273), A(273), E(273) / 'STYOPERA        ', 3.89E-01, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(274), A(274), E(274) / 'EBZOPERA        ', 1.27E-01, 4.0E+03 /   ! O-xylene T dep from Staudinger and Roberts 1996, CRACMM RepCmp (OPERA SAR)
      DATA SUBNAME(275), A(275), E(275) / 'XYLOPERA        ', 0.14E+00, 0.0E+00 /   ! CRACMM RepCmp value (OPERA SAR)
      DATA SUBNAME(276), A(276), E(276) / 'INALDOPERA      ', 1.00E+05, 0.0E+00 /   ! CRACMM RepCMP value (OPERA SAR)
      DATA SUBNAME(277), A(277), E(277) / 'IPXOPERA        ', 1.55E+07, 0.0E+00 /   ! CRACMM RepCMP value (OPERA SAR)

      DATA B( LSO2   ), D( LSO2   ) / 1.30E-02,  1.96E+03 /  ! SO2*H2O<=>HSO3+H     : Smith and Martell (1976)
      DATA B( LHSO3  ), D( LHSO3  ) / 6.60E-08,  1.50E+03 /  ! HSO3<=>SO3+H         : Smith and Martell (1976)
      DATA B( LHNO2  ), D( LHNO2  ) / 5.10E-04, -1.26E+03 /  ! HNO2(aq)<=>NO2+H     : Schwartz and White (1981)
      DATA B( LHNO3  ), D( LHNO3  ) / 1.54E+01,  8.70E+03 /  ! HNO3(aq)<=>NO3+H     : Schwartz (1984)
      DATA B( LCO2   ), D( LCO2   ) / 4.30E-07, -1.00E+03 /  ! CO2*H2O<=>HCO3+H     : Smith and Martell (1976)
      DATA B( LHCO3  ), D( LHCO3  ) / 4.68E-11, -1.76E+03 /  ! HCO3<=>CO3+H         : Smith and Martell (1976)
      DATA B( LH2O2  ), D( LH2O2  ) / 2.20E-12, -3.73E+03 /  ! H2O2(aq)<=>HO2+H     : Smith and Martell (1976)
      DATA B( LHCOOH ), D( LHCOOH ) / 1.80E-04, -2.00E+01 /  ! HCOOH(aq)<=>HCOO+H   : Martell and Smith (1977)
      DATA B( LHO2   ), D( LHO2   ) / 3.50E-05,  0.00E+00 /  ! HO2(aq)<=>H+O2       : Perrin (1982)
      DATA B( LNH4OH ), D( LNH4OH ) / 1.70E-05, -4.50E+02 /  ! NH4*OH<=>NH4+OH      : Smith and Martell (1976)
      DATA B( LH2O   ), D( LH2O   ) / 1.00E-14, -6.71E+03 /  ! H2O<=>H+OH           : Smith and Martell (1976)
      DATA B( LATRA  ), D( LATRA  ) / 2.09E-02,  0.00E+00 /  ! C8H14ClN5<=>C8H13ClN5+H  : Weber (1970)
      DATA B( LCL2   ), D( LCL2   ) / 5.01E-04,  0.00E+00 /  ! CL2*H2O <=> HOCL + H + CL : LIN AND PEHKONEN, JGR, 103, D21, 28093-28102, NOVEMBER 20, 1998. ALSO SEE NOTE BELOW
      DATA B( LHOCL  ), D( LHOCL  ) / 3.16E-08,  0.00E+00 /  ! HOCL <=>H + OCL      : LIN AND PEHKONEN, JGR, 103, D21, 28093-28102, NOVEMBER 20, 1998
      DATA B( LHCL   ), D( LHCL   ) / 1.74E+06,  6.90E+03 /  ! HCL <=> H + CL       : Marsh and McElroy (1985)
      DATA B( LHYDRAZINE), D( LHYDRAZINE) / 1.11E-08,  0.00E+00 /  ! HYDRAZINE <=> HYDRAZINE+ + OH-  : Moliner and Street (1989)
      DATA B( LHBR   ), D( LHBR   ) / 1.00E+09,  0.00E+00 /  ! HBR <=> H + BR       : Schweitzer et al. (2000)
      DATA B( LHI    ), D( LHI    ) / 3.20E+09,  0.00E+00 /  ! HI <=> H + I         : Schweitzer et al. (2000)
      DATA B( LACRYACID ), D( LACRYACID ) / 5.62E-05,  0.00E+00 /  ! CH3C(O)OH <=> H + CH3C(O)O  : Riddick et al. (1985)

!-------------------------------------------------------------------------------
! Note for dissociation constant for equation 14: CL2*H2O <=> HOCL + H + CL
! Need aqueous [CL-] concentration to calculate effective henry's law coefficient
! Used a value of 2.0 mM following Lin and Pehkonen, JGR, 103, D21, 28093-28102, November 20, 1998
!-------------------------------------------------------------------------------

C...........EXTERNAL FUNCTIONS:

C-----------------------------------------------------------------------

      SPC = 0
      DO S = 1, MXSPCS
        IF( CNAME .EQ. SUBNAME( S ) )THEN
            SPC = S
            EXIT
        END IF
      END DO

C...error if species not found in table

      IF ( SPC .LE. 0 ) THEN
        XMSG = TRIM( CNAME ) // ' not found in Henry''s '//
     &         ' Law Constant table in routine HLCONST.'
        CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

C...compute the Henry's Law Constant

      TFAC = ( 298.0 - TEMP) / ( 298.0 * TEMP )
      KH = A( SPC ) * EXP( E( SPC ) * TFAC )
      HLCONST = KH

C...compute the effective Henry's law constants

      IF ( EFFECTIVE ) THEN

        IF ( HPLUS .LE. 0.0 ) THEN
          XMSG = 'Negative or Zero [H+] concentration specified ' //
     &           'in HLCONST '
          CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
        END IF

        HPLUSI = 1.0 / HPLUS
        HPLUS2I = HPLUSI * HPLUSI

C...assign a value for clminus.  use 2.0 mM based on Lin and Pehkonene, 1998, JGR

        CLMINUS   = 2.0E-03                ! chlorine ion conc [CL-]
        CLMINUSI  = 1.0 / CLMINUS          ! 1 / CLMINUS

        CHECK_NAME: SELECT CASE ( CNAME )

        CASE ('SO2')            !   SO2H2O <=> HSO3- + H+
                                ! & HSO3- <=> SO3= + H+

          AKEQ1 = B( LSO2 )  * EXP( D( LSO2 )  * TFAC )
          AKEQ2 = B( LHSO3 ) * EXP( D( LHSO3 ) * TFAC )
          HLCONST = KH * ( 1.0 + AKEQ1 * HPLUSI + AKEQ1 * AKEQ2 * HPLUS2I )

        CASE ('HNO2')           ! HNO2(aq) <=> NO2- + H+

          AKEQ1 = B( LHNO2 ) * EXP( D( LHNO2 ) * TFAC )
          HLCONST = KH * ( 1.0 + AKEQ1 * HPLUSI )

        CASE ('HNO3')           ! HNO3(aq) <=> NO3- + H+

          AKEQ1 = B( LHNO3 ) * EXP( D( LHNO3 ) * TFAC )
          HLCONST = KH * ( 1.0 + AKEQ1 * HPLUSI )

        CASE ('CO2')            !   CO2H2O <=> HCO3- + H+
                                ! & HCO3- <=> CO3= + H+

          AKEQ1 = B( LCO2 )  * EXP( D( LCO2 )  * TFAC )
          AKEQ2 = B( LHCO3 ) * EXP( D( LHCO3 ) * TFAC )
          HLCONST = KH
     &            * ( 1.0 + AKEQ1 * HPLUSI + AKEQ1 * AKEQ2 * HPLUS2I )

        CASE ('H2O2')           ! H2O2(aq) <=> HO2- + H+

          AKEQ1 = B( LH2O2 ) * EXP( D( LH2O2 ) * TFAC )
          HLCONST = KH * ( 1.0 + AKEQ1 * HPLUSI )

        CASE ('FORMIC_ACID')    ! HCOOH(aq) <=> HCOO- + H+

          AKEQ1 = B( LHCOOH ) * EXP( D( LHCOOH ) * TFAC )
          HLCONST = KH * ( 1.0 + AKEQ1 * HPLUSI )

        CASE ('HO2')            ! HO2(aq) <=> H+ + O2-

          AKEQ1 = B( LHO2 ) * EXP( D( LHO2 ) * TFAC )
          HLCONST = KH * ( 1.0 + AKEQ1 * HPLUSI )

        CASE ('NH3')            ! NH4OH <=> NH4+ + OH-

          AKEQ1 = B( LNH4OH ) * EXP( D( LNH4OH ) * TFAC )
          AKEQ2 = B( LH2O ) * EXP( D( LH2O ) * TFAC )
          OHION = AKEQ2 * HPLUSI
          HLCONST = KH * ( 1.0 + AKEQ1 / OHION )

        CASE ('HYDRAZINE')      ! HYDRAZINE <=> HYDRAZINE+ + OH-

          AKEQ1 = B( LHYDRAZINE ) * EXP( D( LHYDRAZINE ) * TFAC )
          AKEQ2 = B( LH2O ) * EXP( D( LH2O ) * TFAC )
          OHION = AKEQ2 * HPLUSI
          HLCONST = KH * ( 1.0 + AKEQ1 / OHION )

        CASE ('ATRA', 'DATRA')  !     ATRA(aq)  <=>  ATRA- + H
                                !  or DATRA(aq) <=> DATRA- + H

          AKEQ1   = B( LATRA ) * EXP( D( LATRA ) * TFAC )
          HLCONST = KH * ( 1.0 + AKEQ1 * HPLUSI )

        CASE ( 'CL2' )          ! CL2*H2O <=> HOCL + H + CL
                                ! HOCL <=>H + OCL

          AKEQ1   = B( LCL2 )  * EXP( D( LCL2 ) * TFAC )
          AKEQ2   = B( LHOCL ) * EXP( D( LHOCL ) * TFAC )
          HLCONST = KH * ( 1.0 + AKEQ1 * HPLUSI * CLMINUSI
     &            + AKEQ1 * AKEQ2 * HPLUS2I * CLMINUSI )

        CASE ( 'HCL' )          ! HCL <=> H+ + CL-

          AKEQ1   = B( LHCL ) * EXP( D( LHCL ) * TFAC )
          HLCONST = KH * ( 1.0 + AKEQ1 * HPLUSI )

        CASE ( 'HOCL' )         ! HOCL <=> H+ + OCL-

          AKEQ1   = B( LHOCL ) * EXP( D( LHOCL ) * TFAC )
          HLCONST = KH * ( 1.0 + AKEQ1 * HPLUSI )

        CASE ( 'HBR' )          ! HBR <=> H+ + BR-

          AKEQ1   = B( LHBR ) * EXP( D( LHBR ) * TFAC )
          HLCONST = KH * ( 1.0 + AKEQ1 * HPLUSI )

        CASE ( 'HI' )          ! HI <=> H+ + I-

          AKEQ1   = B( LHI ) * EXP( D( LHI ) * TFAC )
          HLCONST = KH * ( 1.0 + AKEQ1 * HPLUSI )

        CASE ( 'ACRYACID' )    ! CH3C(O)OH <=> H + CH3C(O)O 

          AKEQ1   = B( LACRYACID ) * EXP( D( LACRYACID ) * TFAC )
          HLCONST = KH * ( 1.0 + AKEQ1 * HPLUSI )

        END SELECT CHECK_NAME

      END IF

      RETURN
      END
