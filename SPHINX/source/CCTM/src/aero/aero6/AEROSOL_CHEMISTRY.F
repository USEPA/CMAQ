!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !!
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

       MODULE AEROSOL_CHEMISTRY
!-----------------------------------------------------------------------
! Revision history
!
! 26 Sep 14 H. Pye: Heterogeneous uptake of IEPOX added 
! 27 Feb 15 J. Bash:
! 17 March 15 D. Luecken:  Added NTR heterogeneous hydrolysis as per
!    the CB6r3 chemical mechanism used by the Environ CAMX model consult 
!    references 8 and 9., 
! 22 Feb 16 B.Hutzell: Added Flag stating whether module is initialized
! 24 March 16 D. Luecken: removed the references to F_NTR2_ON so that all 
!  nitrates have the potential to partition to aerosol.  
! 24 Mar 16 G. Sarwar: Added heterogeneous uptake of BRONO2 on aerosols
! 10 May 16 H. Pye: Merged with AEROSOL_CHEMISTRY.F from aero6i
!                   Size of NAMES_AERO_RATES reduced to match RXN_DATA_MODULE
!    May 16 H. Pye, B. Murphy: Updated treatment of aerosol moments
! 23 May 16 D. Wong: Allow for calculation for APMDIAG even if PMDIAG is turned off.
! 10 Jun 16 B.Hutzell: reduce occurrences of numerical underflow
! 17 Jul 18 G. Sarwar: Added heterogeneous uptakes of CLNO3, bromine and iodine species
! 01 Feb 19 David Wong: removed MY_N clauses
! 03 Apr 19 H. Pye: Updated IEPOX to consume inorganic sulfate when OS are formed
! 11 Dec 20 G. Sarwar: Removed unused uptake cofficients and added few uptake cofficients for halogen species
! 23 Jun 21 G. Sarwar: Replaced CB6R3M with CB6R5M
!-----------------------------------------------------------------------

       IMPLICIT NONE
       
       REAL( 8 ), ALLOCATABLE :: KHETERO( :,:,:,: ) ! heterogeneous rx rates, 
                                                    ! [ppm**(RXN Order-1)*min]**-1
     
       PUBLIC  KHETERO, HETCHEM_RATES
       
       PRIVATE 

       INTEGER, PARAMETER :: NUMB_AERO_RATES = 55
       
       CHARACTER( 16 ), PARAMETER :: NAMES_AERO_RATES( NUMB_AERO_RATES ) = 
     &                    (/ 'HETERO_N2O5IJ   ', 'HETERO_N2O5K    ', 
     &                       'HETERO_N2O5IJY  ', 'HETERO_N2O5KY   ', 
     &                       'HETERO_NO2      ', 'HETERO_H2NO3PAIJ',
     &                       'HETERO_H2NO3PBIJ', 'HETERO_H2NO3PAK ',
     &                       'HETERO_H2NO3PBK ', 'HETERO_H2NO3PAI ',
     &                       'HETERO_H2NO3PBI ', 'HETERO_H2NO3PAJ ',
     &                       'HETERO_H2NO3PBJ ', 'HETERO_N2O5I    ',
     &                       'HETERO_N2O5J    ', 'HETERO_PNCOMLI  ',
     &                       'HETERO_PNCOMLJ  ', 'HETERO_IEPOX    ',
     &                       'HETERO_TETROL   ', 'HETERO_IEPOXOS  ',
     &                       'HETERO_TETROLDIM', 'HETERO_IEPOXOSDI',
     &                       'HETERO_IMAE     ', 'HETERO_2MG      ',
     &                       'HETERO_IMAEOS   ', 'HETERO_NO3      ',
     &                       'HETERO_GLY      ', 'HETERO_MGLY     ',
     &                       'HETERO_NTR2     ', 'HETERO_CLNO3_WAI',
     &                       'HETERO_CLNO3_WAJ', 'HETERO_HBR_BRJ  ',
     &                       'HETERO_BRNO3_WAI', 'HETERO_BRNO3_WAJ',
     &                       'HETERO_HOBR_CLJ ', 'HETERO_HOBR_BRJ ',
     &                       'HETERO_BRNO3_CLJ', 'HETERO_BRNO3_BRJ',
     &                       'HETERO_BRNO2_CLJ', 'HETERO_BRNO2_BRJ',
     &                       'HETERO_I2O2_AI  ', 'HETERO_I2O2_AJ  ',
     &                       'HETERO_I2O3_AI  ', 'HETERO_I2O3_AJ  ',
     &                       'HETERO_I2O4_AI  ', 'HETERO_I2O4_AJ  ',
     &                       'HETERO_INO3_CLJ ', 'HETERO_INO3_BRJ ',
     &                       'HETERO_INO2_CLJ ', 'HETERO_INO2_BRJ ',
     &                       'HETERO_HOI_CLJ  ', 'HETERO_HOI_BRJ  ',
     &                       'HETERO_ISO3NOSJ ', 'HETERO_ISO3OSJ  ',
     &                       'HETERO_HO2      '/)

       INTEGER, PARAMETER  :: IA_N2O5IJ         =   1
       INTEGER, PARAMETER  :: IA_N2O5K          =   2
       INTEGER, PARAMETER  :: IA_N2O5IJY        =   3
       INTEGER, PARAMETER  :: IA_N2O5KY         =   4
       INTEGER, PARAMETER  :: IA_NO2            =   5
       INTEGER, PARAMETER  :: IA_H2NO3PAIJ      =   6
       INTEGER, PARAMETER  :: IA_H2NO3PBIJ      =   7
       INTEGER, PARAMETER  :: IA_H2NO3PAK       =   8
       INTEGER, PARAMETER  :: IA_H2NO3PBK       =   9
       INTEGER, PARAMETER  :: IA_H2NO3PAI       =   10
       INTEGER, PARAMETER  :: IA_H2NO3PBI       =   11
       INTEGER, PARAMETER  :: IA_H2NO3PAJ       =   12
       INTEGER, PARAMETER  :: IA_H2NO3PBJ       =   13
       INTEGER, PARAMETER  :: IA_N2O5I          =   14
       INTEGER, PARAMETER  :: IA_N2O5J          =   15
       INTEGER, PARAMETER  :: IA_PNCOMLI        =   16
       INTEGER, PARAMETER  :: IA_PNCOMLJ        =   17
       INTEGER, PARAMETER  :: IA_IEPOX          =   18
       INTEGER, PARAMETER  :: IA_TETROL         =   19
       INTEGER, PARAMETER  :: IA_IEPOXOS        =   20
       INTEGER, PARAMETER  :: IA_TETROLDIM      =   21
       INTEGER, PARAMETER  :: IA_IEPOXOSDI      =   22
       INTEGER, PARAMETER  :: IA_IMAE           =   23
       INTEGER, PARAMETER  :: IA_2MG            =   24
       INTEGER, PARAMETER  :: IA_IMAEOS         =   25
       INTEGER, PARAMETER  :: IA_NO3            =   26
       INTEGER, PARAMETER  :: IA_GLY            =   27
       INTEGER, PARAMETER  :: IA_MGLY           =   28
       INTEGER, PARAMETER  :: IA_NTR2           =   29
       INTEGER, PARAMETER  :: IA_CLNO3_WAI      =   30
       INTEGER, PARAMETER  :: IA_CLNO3_WAJ      =   31
       INTEGER, PARAMETER  :: IA_HBR_BRJ        =   32
       INTEGER, PARAMETER  :: IA_BRNO3_WAI      =   33
       INTEGER, PARAMETER  :: IA_BRNO3_WAJ      =   34
       INTEGER, PARAMETER  :: IA_HOBR_CLJ       =   35
       INTEGER, PARAMETER  :: IA_HOBR_BRJ       =   36
       INTEGER, PARAMETER  :: IA_BRNO3_CLJ      =   37
       INTEGER, PARAMETER  :: IA_BRNO3_BRJ      =   38
       INTEGER, PARAMETER  :: IA_BRNO2_CLJ      =   39
       INTEGER, PARAMETER  :: IA_BRNO2_BRJ      =   40
       INTEGER, PARAMETER  :: IA_I2O2_AI        =   41
       INTEGER, PARAMETER  :: IA_I2O2_AJ        =   42
       INTEGER, PARAMETER  :: IA_I2O3_AI        =   43
       INTEGER, PARAMETER  :: IA_I2O3_AJ        =   44
       INTEGER, PARAMETER  :: IA_I2O4_AI        =   45
       INTEGER, PARAMETER  :: IA_I2O4_AJ        =   46
       INTEGER, PARAMETER  :: IA_INO3_CLJ       =   47
       INTEGER, PARAMETER  :: IA_INO3_BRJ       =   48
       INTEGER, PARAMETER  :: IA_INO2_CLJ       =   49
       INTEGER, PARAMETER  :: IA_INO2_BRJ       =   50
       INTEGER, PARAMETER  :: IA_HOI_CLJ        =   51
       INTEGER, PARAMETER  :: IA_HOI_BRJ        =   52
       INTEGER, PARAMETER  :: IA_ISO3NOSJ       =   53
       INTEGER, PARAMETER  :: IA_ISO3OSJ        =   54
       INTEGER, PARAMETER  :: IA_HO2            =   55


       INTEGER, PARAMETER :: INDEX_AERO_RATES( NUMB_AERO_RATES ) = 
     &                    (/ IA_N2O5IJ   , IA_N2O5K    , 
     &                       IA_N2O5IJY  , IA_N2O5KY   , 
     &                       IA_NO2      , IA_H2NO3PAIJ,
     &                       IA_H2NO3PBIJ, IA_H2NO3PAK ,
     &                       IA_H2NO3PBK , IA_H2NO3PAI ,
     &                       IA_H2NO3PBI , IA_H2NO3PAJ ,
     &                       IA_H2NO3PBJ , IA_N2O5I    , 
     &                       IA_N2O5J    , IA_PNCOMLI  ,
     &                       IA_PNCOMLJ  , IA_IEPOX    ,
     &                       IA_TETROL   , IA_IEPOXOS  ,
     &                       IA_TETROLDIM, IA_IEPOXOSDI,
     &                       IA_IMAE     , IA_2MG      ,
     &                       IA_IMAEOS   , IA_NO3      ,
     &                       IA_GLY      , IA_MGLY     ,
     &                       IA_NTR2     , IA_CLNO3_WAI,
     &                       IA_CLNO3_WAJ, IA_HBR_BRJ  ,
     &                       IA_BRNO3_WAI, IA_BRNO3_WAJ,
     &                       IA_HOBR_CLJ , IA_HOBR_BRJ ,
     &                       IA_BRNO3_CLJ, IA_BRNO3_BRJ,
     &                       IA_BRNO2_CLJ, IA_BRNO2_BRJ,
     &                       IA_I2O2_AI  , IA_I2O2_AJ  ,
     &                       IA_I2O3_AI  , IA_I2O3_AJ  ,
     &                       IA_I2O4_AI  , IA_I2O4_AJ  ,
     &                       IA_INO3_CLJ , IA_INO3_BRJ ,
     &                       IA_INO2_CLJ , IA_INO2_BRJ ,
     &                       IA_HOI_CLJ  , IA_HOI_BRJ  ,
     &                       IA_ISO3NOSJ , IA_ISO3OSJ  ,
     &                       IA_HO2      /)

       INTEGER               :: SELECTED_AERO_RATES
       INTEGER, ALLOCATABLE  :: WHICH_AERO_RATE( : )
       
C *** Molecular weight
      REAL( 8 ), SAVE        :: MWTIEPOX      ! molecular weight of IEPOX [g/mol]
      REAL( 8 ), SAVE        :: CFACTOR_IEPOX ! factor used for the mean molecular speed of IEPOX [m/(s^1*(deg K)^0.5)]
      REAL( 8 ), SAVE        :: MWTIMAE       ! molecular weight of IMAE [g/mol]
      REAL( 8 ), SAVE        :: CFACTOR_IMAE  ! factor used for the mean molecular speed of IMAE [m/(s^1*(deg K)^0.5)]
      REAL( 8 ), SAVE        :: MWTNO3        ! molecular weight of NO3 [g/mol]
      REAL( 8 ), SAVE        :: CFACTOR_NO3   ! factor used for the mean molecular speed of NO3 [m/(s^1*(deg K)^0.5)]
      REAL( 8 ), SAVE        :: MWTGLY        ! molecular weight of GLY [g/mol]
      REAL( 8 ), SAVE        :: CFACTOR_GLY   ! factor used for the mean molecular speed of GLY [m/(s^1*(deg K)^0.5)]
      REAL( 8 ), SAVE        :: MWTMGLY       ! molecular weight of MGLY [g/mol]
      REAL( 8 ), SAVE        :: CFACTOR_MGLY  ! factor used for the mean molecular speed of MGLY [m/(s^1*(deg K)^0.5)]
      REAL( 8 ), SAVE        :: MWTHO2        ! molecular weight of HO2 [g/mol]
      REAL( 8 ), SAVE        :: CFACTOR_HO2   ! factor used for the mean molecular speed of HO2 [m/(s^1*(deg K)^0.5)]

C *** 2nd and 3rd moments with wet species
      REAL( 8 ), ALLOCATABLE :: WET_M3( : ) ! WET_M3_I,  WET_M3_J, WET_M3_K   ! M3 w.H2O, svOA
      REAL( 8 ), ALLOCATABLE :: WET_M2( : ) ! WET_M2_I,  WET_M2_J, WET_M2_K   ! M2 w.H2O, svOA
      REAL( 8 ), ALLOCATABLE :: DE_WET( : ) ! DE_AT_WET, DE_AC_WET, DE_CO_WET ! Initial effective diameter w.H2O

C Isoprene product particle phase reaction rates (Eddingsaas et al. 2010) [1/(M^2 s)]
      REAL ( 8 ), PARAMETER :: K_H_WATER    = 9.0D-4
      REAL ( 8 ), PARAMETER :: K_H_NUC      = 2.0D-4
      REAL ( 8 ), PARAMETER :: K_H_SO4      = 8.83D-3 ! Piletic et al. 2013, Budisulistiorini et al. in prep
      REAL ( 8 ), PARAMETER :: K_HSO4_WATER = 1.31D-5
      REAL ( 8 ), PARAMETER :: K_HSO4_NUC   = 2.92D-6

C Acid catalyzed particle phase reactions
      TYPE ACID_CAT
         CHARACTER( 16 ) :: PARENT     ! gas-phase parent species
         CHARACTER( 16 ) :: NUC        ! aerosol-phase nucleophile that adds to epoxide ring
         CHARACTER( 16 ) :: ACID       ! acid that catalyzes epoxide ring opening
         INTEGER         :: IDX_ACID   ! array index for acid concentration
         INTEGER         :: IDX_REMOVE ! array index denoting whether to correct nucleophile for acid
         REAL( 8 )       :: KCHEM      ! particle phase rate constant [1/(M^2 s)]
         CHARACTER( 16 ) :: PROD       ! product of nucleophile+parent
      END TYPE ACID_CAT

C IEPOX uptake parameters 
      INTEGER, SAVE                       :: N_NUCPAIRS          ! NUMBER OF ACID/NUCLEOPHILE PAIRS 
      TYPE( ACID_CAT ), ALLOCATABLE, SAVE :: ACID_NUC_PAIRS( : )
      INTEGER, SAVE                       :: NUMVOC              ! number of VOCs treated (IEPOX, IMAE)

C IEPOX uptake parameters for standard AERO6 or AERO7 (condensed IEPOX SOA)
      INTEGER, PARAMETER       :: N_NUCPAIRS_AE = 6      ! NUMBER OF ACID/NUCLEOPHILE PAIRS 

C IEPOX uptake based on Eddingsaas et al. 2010 parameters with Piletic
C 2013 update for the organosulfate. Same as Pye et al. 2013
C implementation except dimers are not considered. 
      TYPE( ACID_CAT ), PARAMETER :: ACID_NUC_PAIRS_AE( N_NUCPAIRS_AE ) = (/
C                  Parent   Nucleophile     Acid     Acid    Remove   Rate Constant  Product
C                 (parent)    (nuc)        (acid)    Index   Index       (kchem)      (prod)
C                 --------  -----------  ----------  -----   ------   -------------  --------
     &   ACID_CAT('IEPOX',  'AH2OJ    ', 'HPLUS   ',   1,     0,       K_H_WATER,    'AISO3J' ),
     &   ACID_CAT('IEPOX',  'ASO4J    ', 'HPLUS   ',   1,     2,       K_H_SO4,      'AISO3J'  ),
     &   ACID_CAT('IEPOX',  'ANO3J    ', 'HPLUS   ',   1,     0,       K_H_NUC,      'AISO3J'  ),
     &   ACID_CAT('IEPOX',  'AH2OJ    ', 'HSO4    ',   2,     0,       K_HSO4_WATER, 'AISO3J' ),
     &   ACID_CAT('IEPOX',  'ASO4J    ', 'HSO4    ',   2,     2,       K_HSO4_NUC,   'AISO3J'  ),
     &   ACID_CAT('IEPOX',  'ANO3J    ', 'HSO4    ',   2,     0,       K_HSO4_NUC,   'AISO3J'  )/)

C IEPOX + MAE uptake parameters for AERO6i or AERO7i (explicit IEPOX SOA)
      INTEGER, PARAMETER :: N_NUCPAIRS_AEI = 12      ! NUMBER OF ACID/NUCLEOPHILE PAIRS

C IEPOX uptake following the implementation
C in Pye et al. 2013 ES&T base simulation except epoxide-derived organonitrates are no longer
C considered in saprc07tic_ae7i due to their predicted small contribution to ambient PM. In addition,
C oligomerization of MAE/HMML-derived aerosol is not considered also due to its predicted
C small contribution and desire to have the model "dimer" species be only IEPOX derived.
      TYPE( ACID_CAT ), PARAMETER :: ACID_NUC_PAIRS_AEI( N_NUCPAIRS_AEI ) = (/
C                  Parent   Nucleophile     Acid     Acid    Remove   Rate Constant  Product
C                 (parent)    (nuc)        (acid)    Index   Index       (kchem)      (prod)
C                 --------  -----------  ----------  -----   ------   -------------  --------
     &   acid_cat('IEPOX',  'AH2OJ    ', 'HPLUS   ',   1,     0,       k_H_water,    'AIETETJ' ),
     &   acid_cat('IEPOX',  'ASO4J    ', 'HPLUS   ',   1,     2,       k_H_SO4,      'AIEOSJ'  ),
     &   acid_cat('IEPOX',  'AIEOSJ   ', 'HPLUS   ',   1,     0,       k_H_nuc,      'ADIMJ'   ),
     &   acid_cat('IEPOX',  'AIETETJ  ', 'HPLUS   ',   1,     0,       k_H_nuc,      'ADIMJ'   ),
     &   acid_cat('IEPOX',  'AH2OJ    ', 'HSO4    ',   2,     0,       k_HSO4_water, 'AIETETJ' ),
     &   acid_cat('IEPOX',  'ASO4J    ', 'HSO4    ',   2,     2,       k_HSO4_nuc,   'AIEOSJ'  ),
     &   acid_cat('IEPOX',  'AIEOSJ   ', 'HSO4    ',   2,     0,       k_HSO4_nuc,   'ADIMJ'   ),
     &   acid_cat('IEPOX',  'AIETETJ  ', 'HSO4    ',   2,     0,       k_HSO4_nuc,   'ADIMJ'   ),
     &   acid_cat('IMAE',   'AH2OJ    ', 'HPLUS   ',   1,     0,       k_H_water,    'AIMGAJ'  ),
     &   acid_cat('IMAE',   'ASO4J    ', 'HPLUS   ',   1,     2,       k_H_nuc,      'AIMOSJ'  ),
     &   acid_cat('IMAE',   'AH2OJ    ', 'HSO4    ',   2,     0,       k_HSO4_water, 'AIMGAJ'  ),
     &   acid_cat('IMAE',   'ASO4J    ', 'HSO4    ',   2,     2,       k_HSO4_nuc,   'AIMOSJ'  )/)

C IEPOX uptake for CRACMM mechanism

        INTEGER, PARAMETER       :: N_NUCPAIRS_CRCM = 6      ! NUMBER OF ACID/NUCLEOPHILE PAIRS

        TYPE( ACID_CAT ), PARAMETER :: ACID_NUC_PAIRS_CRCM( N_NUCPAIRS_CRCM ) = (/

C                  Parent   Nucleophile     Acid     Acid    Remove  Rate Constant  Product
C                 (parent)    (nuc)        (acid)    Index   Index     (kchem)      (prod)
C                 --------  -----------  ----------  -----   ------  -------------  --------
     &   acid_cat('IEPOX',  'AH2OJ    ', 'HPLUS   ',   1,     0,       K_H_WATER,   'AISO3NOSJ' ),
     &   acid_cat('IEPOX',  'ASO4J    ', 'HPLUS   ',   1,     2,       K_H_SO4,     'AISO3OSJ'  ),
     &   acid_cat('IEPOX',  'ANO3J    ', 'HPLUS   ',   1,     0,       K_H_NUC,     'AISO3NOSJ' ),
     &   acid_cat('IEPOX',  'AH2OJ    ', 'HSO4    ',   2,     0,       K_HSO4_WATER,   'AISO3NOSJ' ),
     &   acid_cat('IEPOX',  'ASO4J    ', 'HSO4    ',   2,     2,       K_HSO4_NUC,     'AISO3OSJ'  ),
     &   acid_cat('IEPOX',  'ANO3J    ', 'HSO4    ',   2,     0,       K_HSO4_NUC,     'AISO3NOSJ' )/)


C Mapping array for location of acid enhanced products and nucleophiles in aerospc_conc 
      INTEGER, ALLOCATABLE, SAVE :: ACID_PRODMAP_IDX( : )
      INTEGER, ALLOCATABLE, SAVE ::  ACID_NUCMAP_IDX( : )

      CONTAINS

      SUBROUTINE HETCHEM_RATES( TEMP, PRESS, WVAPOR, CGRID, DENS )

c Calculates the heterogeneous reactions for N2O5, NO2, CLNO2, and IEPOX. 
c
c Key Subroutines Called: EXTRACT_AERO, EXTRACT_SOA, PATPAR
c                         N2O5_GAMMA
c
c 09/18/13 - B.Hutzell - initial version adapted from the hetchem.F file
c            in aero6 module of CMAQ version 5.01. The adaptation includes
c            heterogeneous nitryl chloride production used for Sarwar et al.
c            (2012).
c 09/12/14 - G. Sarwar - revised the heterogeneous nitryl chloride production
c 09/26/14 - H. Pye - Heterogeneous uptake of IEPOX on acidic aerosol added
c            following Pye et al. (2013).
c 10/01/14 - B.Hutzell - change STP value for N205 diffusivity based on
c            review paper: 1)   M. J. Tang, R. A. Cox, and M. Kalberer.
c            Compilation and evaluation of gas-phase diffusion coefficients 
c            of inorganic reactive trace gases in the atmosphere. Atmos. Chem.
c            Phys. Discuss., 14, 15645–15682, 2014. 
c            www.atmos-chem-phys-discuss.net/14/15645/2014/doi:10.5194/acpd-14-15645-2014.pdf
c 09/12/14 - G. Sarwar - revised the heterogeneous nitryl chloride production
C 10/01/14 - B.Hutzell - change STP value for N205 diffusivity based on
C            review paper: 1)   M. J. Tang, R. A. Cox, and M. Kalberer.
C            Compilation and evaluation of gas-phase diffusion coefficients 
C            of inorganic reactive trace gases in the atmosphere. Atmos. Chem.
C            Phys. Discuss., 14, 15645–15682, 2014. 
C            www.atmos-chem-phys-discuss.net/14/15645/2014/doi:10.5194/acpd-14-15645-2014.pdf
c 01/02/15 - H. Pye - Heterogeneous uptake of MAE added to saprc07tic_ae6i version
C            heterogeneous nitryl chloride production used for Sarwar et al.
c            (2012).
C 02/09/15 - B.Hutzell - corrected NO2 rate by a factor of two based consulting the stoiciometery for 
c            reaction published in Sarwar et al. (2008) and its analytical solution
C 05/06/2015 H Pye - Added NO3 heterogeneous reaction using low end of
C            range from Mao et al 2013 
C 06/2015    H Pye - Added SOA from GLY and MGLY uptake onto particles
C 09/2015    B.Hutzell - Added data and varaibles to calculate pseudo-first order rate constant 
C            for heterogeneous hydrolysis that converts organic nitrate to nitric acid. Both are
C            assumed gas phase species. The reaction comes the CB6r3 chemical mechanism used by 
C            the Environ CAMX model consult references 9 and 10.
c 03/24/16 - G. Sarwar - Heterogeneous uptake of BRONO2 on aerosols
C 5/2016     H Pye - merged with AERO6i version (added NO3, GLY, MGLY, IMAE het rxn)
c 07/17/18 - G. Sarwar - Heterogeneous uptakes of chlorine, bromine and iodine species on aerosols
C 06/10/21 - G. Sarwar: Added CB6R5M_AE7_AQ
C 11/08/23 - N. Skipper: Added heterogeneous uptake of HO2
C
c  REFERENCES:
c   1. Pleim, J.E., F.S. Binkowski, J.K.S. Ching, R.L. Dennis, and N.V.
c      Gallani, An improved representation of the reaction of N2O5 on
c      aerosols for mesoscale air quality models.  In "Regional
c      Photochemical Measurement and Modeling Studies, Vol 2 - Results
c      and Status of Modeling," Eds A.J. Ranzieri and P.A. Solomon, pp
c      904-913, 1995.
c
c   2. Davis, J.M., P.V. Bhave, and K.M. Foley, Parameterization of N2O5
c      reaction probabilities on the surface of particles containing
c      ammonium, sulfate, and nitrate.  Atmos. Chem. Phys., 2008, in
c      press.
c
c   3. Vogel, B., H. Vogel, J. Kleffman, and R. Kurtenbach, Measured and
c      simulated vertical profiles of nitrous acid - Part II. Model
c      simulations and indications for a photolytic source, Atmospheric
c      Environment, 37, 2957-2966, 2003.
c
c   4. Sarwar, G., S.J. Roselle, R. Mathur, W. Appel, R.L. Dennis, and
c      B. Vogel, A comparison of CMAQ HONO predictions with observations
c      from the Northeast Oxidant and Particle Study, Atmospheric
c      Environment, 2008, in press.
C
C   5. Bertram, T. H. and J.A. Thornton, Toward a general parameterization
C      of N2O5 reactivity on aqueous particles: the competing effects of 
C      particle liquid water, nitrate, and chloride, ACP, 9, 8351-8363, 2009 
C
C   6. Sarwar, G., H. Simon2, P. Bhave1, and G. Yarwood. Examining the impact of 
C      heterogeneous nitryl chloride production on air quality across the United
C      States. Atmos. Chem. Phys., 12, 6455-6473, 2012.
C
C   7. Pye et al., Epoxide pathways improve model predictions of isoprene
C      markers and reveal key role of acidity in aerosol formation,
C      Environ. Sci. Technol., doi: 10.1021/es402106h, 2013.
C
C   8. Rollins, A.W., S. Pusede, P.Wooldridge, K.-E.Min, D.R. Gentner, A.H. 
C      Goldstein, S. Liu, D.A. Day, L.M. Russell, C.L. Rubitschun, J.D. Surratt,
C      and R.C. Cohen, Gas/particle partitioning of total alkyl nitrates 
C      observed with TD-LIF in Bakersfield.J.Geophys.Res., 118, 6651-6662, 2013.
C
C   9. Liu, S., J.E. Shilling, C. Song, N. Hiranuma, R.A. Zaveri, L.M. Russell,
C      Hydrolysis of Organonitrate Functional Groups in Aerosol Particles.
C      Aerosol Sci. Technol., 46, 1359-1369, 2012.
C
C  10. Yang, X., R. A. Cox, N. J. Warwick, J. A. Pyle, G. D. Carver, F. M.
C      O'Connor, and N. H. Savage, Tropospheric bromine chemistry
C      and its impacts on ozone: A model study, J. Geophys. Res., 110, D23311,
C      doi:10.1029/2005JD006244, 2005.
C
C  11. Mao et al. Ozone and organic nitrates over the eastern United
C      States: Sensitivity to isoprene chemistry, J. Geophys. Res. doi:
C      10.1002/jgrd.50817, 2013.

C  12. Fernandez et al., Bromine partitioning in the tropical tropopause layer: 
C      implications for stratospheric injection, ACP, 14, 13391-13410, 2014.
C
C  13. Schmidt et al., Modeling the observed tropospheric BrO background: Importance 
C      of multiphase chemistry and implications for ozone, OH, and mercury, 
C      J. Geophys. Res. doi: 10.1002/2015JD024229
C
C  14. Sherwen et al., Global impacts of tropospheric halogens on oxidants and 
C      composition in GEOS-CHEM: ACP, 16, 12239-12271, 2016
C
C  15. Deiber et al., Uptake of ClONO2 and BrONO2 by halide containing droplets
C      ACP, 4, 1291-1299, 2004
C
C  16. Ivatt et al., Suppression of surface ozone by an aerosol-inhibited
C      photochemical ozone regime, Nat. Geosci.,
C      doi: 10.1038/s41561-022-00972-9, 2022
C  17. Jacob, Heterogeneous chemistry and tropospheric ozone, Atm Env.,
C      doi: 10.1016/S1352-2310(99)00462-8, 2000
C
C-----------------------------------------------------------------------
      USE RUNTIME_VARS
      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE RXNS_DATA
      USE AERO_DATA
      USE AEROMET_DATA, ONLY: f6dpi, airdens
      USE PRECURSOR_DATA
      USE SOA_DEFN
      USE ELMO_DATA
      !USE ELMO_PROC
      USE CENTRALIZED_IO_MODULE

      IMPLICIT NONE

      INCLUDE SUBST_CONST       ! CMAQ constants

C *** Arguments
      REAL,           POINTER       :: CGRID ( :,:,:,: )  ! pointer of model concentrations
      REAL,           INTENT( IN )  :: TEMP  ( :,:,: )    ! temperature [K]
      REAL,           INTENT( IN )  :: PRESS ( :,:,: )    ! pressure [Pa]
      REAL,           INTENT( IN )  :: WVAPOR( :,:,: )    ! water vapor mass mixing ratio(Kg/Kg air)
      REAL, OPTIONAL, INTENT( IN )  :: DENS ( :,:,: )     ! air density(Kg/m3 air)

      CHARACTER(16), SAVE ::  PNAME = 'HETCHEM_RATES'
      
C *** Parameters
      REAL( 8 ), PARAMETER :: AQUEOUS_FREQUENCY = 1.0D+9
      REAL( 8 ), PARAMETER :: INV_SQRT_MWNO2  = 1.47425932467825D-1
      REAL( 8 ), PARAMETER :: INV_SQRT_MWN2O5 = 9.62161363758323D-2
      REAL( 8 ), PARAMETER :: INV_SQRT_MWCLNO3  = 1.012739360000D-1        
      REAL( 8 ), PARAMETER :: INV_SQRT_MWBRNO3 = 8.394770000000D-2
      REAL( 8 ), PARAMETER :: INV_SQRT_MWBRNO2  = 8.91220000000D-2
      REAL( 8 ), PARAMETER :: INV_SQRT_MWHOBR   = 1.015869000D-1
      REAL( 8 ), PARAMETER :: INV_SQRT_MWHBR    = 1.111797000D-1                   
      REAL( 8 ), PARAMETER :: INV_SQRT_MWI2O2 = 5.9151900D-2
      REAL( 8 ), PARAMETER :: INV_SQRT_MWI2O3 = 5.7562500D-2
      REAL( 8 ), PARAMETER :: INV_SQRT_MWI2O4 = 5.6090000D-2
      REAL( 8 ), PARAMETER :: INV_SQRT_MWINO3 = 7.275850000D-2
      REAL( 8 ), PARAMETER :: INV_SQRT_MWINO2 = 7.605050000D-2
      REAL( 8 ), PARAMETER :: INV_SQRT_MWHOI  = 8.336220000D-2

      REAL( 8 ), PARAMETER :: UGM3_CONV_FAC   = 8.31251724585556D00 ! =1.0E-3*AIRDENS_STD*(PRESS_STD/TEMP_STD)
      REAL( 8 ), PARAMETER :: COEF1           = 7.24382926227485D10 ! Molec/cc to ppm conv factor 
      REAL( 8 ), PARAMETER :: COEF2           = 2.14805198392421D13 ! convert air density [kg/m3] to number density [ppm]

      REAL( 8 ), PARAMETER :: GAMMA_NO3       = 1.0D-3 ! Jacob 2000
      REAL( 8 ), PARAMETER :: GAMMA_GLY       = 2.9D-3 ! Liggio et al. 2005
      REAL( 8 ), PARAMETER :: GAMMA_HO2       = 0.2D+0 ! Ivatt et al. 2022

      REAL( 8 ), PARAMETER :: STD_DIFF_N2O5 = 0.0855D-4  ! N2O5 molecular diffusivity at 101325 Pa and 273.15 K [m2/sec]
      REAL( 8 ), PARAMETER :: STD_DIFF_CLNO3  = 0.1014D-4    ! CLNO3 molecular diffusivity at 101325 Pa and 273.15 K [m2/sec]   
      REAL( 8 ), PARAMETER :: STD_DIFF_BRNO3  = 0.0855D-4    ! BRONO2 molecular diffusivity at 101325 Pa and 273.15 K [m2/sec]
      REAL( 8 ), PARAMETER :: STD_DIFF_BRNO2  = 0.0999D-4    ! BRNO2 molecular diffusivity at 101325 Pa and 273.15 K [m2/sec]
      REAL( 8 ), PARAMETER :: STD_DIFF_HOBR   = 0.1101D-4    ! HOBR molecular diffusivity at 101325 Pa and 273.15 K [m2/sec]
      REAL( 8 ), PARAMETER :: STD_DIFF_HBR    = 0.1216D-4    ! HBR molecular diffusivity at 101325 Pa and 273.15 K [m2/sec]    
      REAL( 8 ), PARAMETER :: STD_DIFF_I2O2   = 0.0732D-4    ! I2O2 molecular diffusivity at 101325 Pa and 273.15 K [m2/sec] 
      REAL( 8 ), PARAMETER :: STD_DIFF_I2O3   = 0.0707D-4    ! I2O3 molecular diffusivity at 101325 Pa and 273.15 K [m2/sec] 
      REAL( 8 ), PARAMETER :: STD_DIFF_I2O4   = 0.0684D-4    ! I2O4 molecular diffusivity at 101325 Pa and 273.15 K [m2/sec] 
      REAL( 8 ), PARAMETER :: STD_DIFF_INO3   = 0.0792D-4    ! INO3 molecular diffusivity at 101325 Pa and 273.15 K [m2/sec] 
      REAL( 8 ), PARAMETER :: STD_DIFF_INO2   = 0.0833D-4    ! INO2 molecular diffusivity at 101325 Pa and 273.15 K [m2/sec] 
      REAL( 8 ), PARAMETER :: STD_DIFF_HOI    = 0.0972D-4    ! HOI molecular diffusivity at 101325 Pa and 273.15 K [m2/sec] 

!     REAL,      PARAMETER :: GPKG        = 1.0E+03                      ! g/kg unit conversion
      REAL,      PARAMETER :: CFACTOR     = 8.0 * GPKG  * RGASUNIV / PI  ! factor in cbar_coeff
      REAL,      PARAMETER :: PA2ATM      = 1.0 / STDATMPA               ! Pascal to atm conv fac
      REAL,      PARAMETER :: WMASS2PPM   = 1.0E6 * MWAIR / MWWAT        ! H2O mixing ratio to ppm
      REAL,      PARAMETER :: MAOMV       = MWAIR / MWWAT
      REAL,      PARAMETER :: CONCOFM     = 1.0E+06        ! conc. of M = 1E+06 ppm
      REAL,      PARAMETER :: INV_STDTEMP = 1.0 / STDTEMP
      REAL,      PARAMETER :: EPSWATER    = MWWAT / MWAIR
C *** parameters for heterogeneous hydrolysis of NTR2
      REAL,      PARAMETER :: ON_ALPHA1   = 0.34,     ! Organic nitrate (ON) partitioning parameters
     &                        ON_ALPHA2   = 0.66,     ! based on Rollins et al., 2013
     &                        ON_CSTAR1   = 0.73,
     &                        ON_CSTAR2   = 1000.0

      REAL,      PARAMETER :: MIN_TOA     = 0.0001       ! Min. conc. of total OA for partitioning to occur [ug/m3]
      REAL,      PARAMETER :: KHON        = 2.7778E-03   ! heterogeneous ON hydrolysis rate [1/min] ~ 4/day

C *** Local Variables
      REAL( 8 )    :: AIRTEMP          ! air temperature [K]
      REAL( 8 )    :: AIRPRES          ! air pressure [Pa] 
      REAL( 8 )    :: GAMMA            ! fine mode N2O5->NO3 rxn probability
      REAL( 8 )    :: GAMMAIEPOX       ! IEPOX uptake coefficient
      REAL( 8 )    :: GAMMAIMAE        ! IMAE uptake coefficient
      REAL( 8 )    :: KPIEPOX          ! IEPOX particle-phase reaction rate const (pseudo 1st order, sec-1)
      REAL( 8 )    :: KN2O5( N_MODE  ) ! pseudo-first order rate constant, sec-1
      REAL( 8 )    :: KNO3(  N_MODE  ) ! pseudo-first order rate constant, sec-1
      REAL( 8 )    :: KGLY(  N_MODE  ) ! pseudo-first order rate constant, sec-1
      REAL( 8 )    :: KMGLY( N_MODE  ) ! pseudo-first order rate constant, sec-1
      REAL( 8 )    :: KNO2             ! pseudo-first order rate constant, sec-1
      REAL( 8 )    :: KIEPOX           ! pseudo-first order rate constant for IEPOX uptake, sec-1
      REAL( 8 )    :: KIMAE            ! pseudo-first order rate constant for MAE uptake, sec-1
      REAL( 8 )    :: KHO2(  N_MODE  ) ! pseudo-first order rate constant, sec-1

      REAL( 8 )    :: K1               ! pseudo-first order rate constant, sec-1 
      REAL( 8 )    :: K2               ! pseudo-first order rate constant, sec-1 
      REAL( 8 ), PARAMETER :: MIN_VALUE = 1.0D-09      ! minimum concentration for activating chemistry

      REAL( 8 )    :: CBAR_COEFF       ! cell coefficient for mean molecular speed
      REAL( 8 )    :: ADJUST_DIFF      ! Cell temp and press correction to diffusivity
      REAL( 8 )    :: INV_DIFFUSIVITY  ! reciprocal molecular diffusivity [m2/sec] 

      REAL( 8 )    :: XXF( N_MODE )         ! XXF_AT,    XXF_AC,    XXF_CO    ! modal factors to calculate KN2O5
      REAL( 8 )    :: CL_PPM( N_MODE )      ! aerosol chlorine in ppm
      REAL( 8 )    :: BR_PPM( N_MODE )      ! aerosol bromine in ppm
      REAL( 8 )    :: YIELD_CLNO2( N_MODE ) ! model reactions yields of CLNO2      
      
      REAL( 8 )    :: H2OVP    ! ambient water vapor pressure [Atm]
      REAL( 8 )    :: AIRRH    ! Relative Humidity            [Fractional]
      REAL( 8 )    :: YIELDIJ  ! fine mode reaction yield, dimensionaless
      REAL( 8 )    :: TOTSURFA ! aerosol surface area (m**2/m**3)
      REAL( 8 )    :: FACTOR   ! scratch multiplicative factor
      REAL( 8 )    :: RFACTOR  ! factor converting rate constant from cm3/molec/sec to 1/ppm/min
      REAL( 8 )    :: RADIUS_I ! effective particle radius [m] - i-mode
      REAL( 8 )    :: RADIUS   ! effective particle radius [m] - j-mode

      REAL( 8 )    :: SSALT_SURFA       ! sea-salt aerosol surface area (m**2/m**3)
      REAL( 8 )    :: SSALT_VOL         ! sea-salt aerosol volume (m**3/m**3)
      
      REAL( 8 )       :: CBAR             ! molecular velocity of N2O5 (m/s)
      REAL( 8 )       :: CBARIEPOX        ! molecular velocity of IEPOX (m/s)
      REAL( 8 )       :: CBARIMAE         ! molecular velocity of IMAE (m/s)
      REAL( 8 )       :: CBARNO3          ! molecular velocity of NO3 (m/s)
      REAL( 8 )       :: CBARGLY          ! molecular velocity of GLY (m/s)
      REAL( 8 )       :: CBARMGLY         ! molecular velocity of MGLY (m/s)
      REAL( 8 )       :: CBARHO2          ! molecular velocity of HO2 (m/s)
      REAL( 8 )       :: TETROL_PPM       ! accumulation mode aerosol tetrol in ppm
      REAL( 8 )       :: IEPOXOS_PPM      ! accumulation mode iepox organosulfate in ppm
      REAL( 8 )       :: ASO4J_PPM        ! accumulation mode inorganic sulfate in ppm
      REAL( 8 )       :: FH2O(2)          ! fraction of epoxide aerosol from  hydrolysis, index: IEPOX(1) and MAE(2)
      REAL( 8 )       :: FOS(2)           ! fraction of epoxide aerosol as organosulfate, index: IEPOX(1) and MAE(2)
      REAL( 8 )       :: FDIM1(2)         ! fraction of epoxide aerosol as dimer, index: IEPOX(1) and MAE(2)
      REAL( 8 )       :: FDIM2(2)         ! fraction of epoxide aerosol as organosulfate dimer, index: IEPOX(1) and MAE(2)
      REAL( 8 ), SAVE :: INV_MWIETET, INV_MWIEOS  ! reciprocal of molecular weight for tetrol, iepoxos [mol/g]
      REAL( 8 ), SAVE :: INV_MWASO4       ! reciprocal of molecular weight of particulate sulfate [mol/g]

      REAL( 8 )       :: CBAR_CLNO3        ! molecular velocity of CLNO3 (m/s)      
      REAL( 8 )       :: CBAR_HBR          ! molecular velocity of HOBR (m/s)        
      REAL( 8 )       :: CBAR_BRNO2        ! molecular velocity of BRNO2 (m/s)      
      REAL( 8 )       :: CBAR_BRNO3        ! molecular velocity of BRNO3 (m/s)
      REAL( 8 )       :: CBAR_HOBR         ! molecular velocity of HOBR (m/s)                                          
      REAL( 8 )       :: CBAR_I2O2         ! molecular velocity of I2O2 (m/s)  
      REAL( 8 )       :: CBAR_I2O3         ! molecular velocity of I2O3 (m/s) 
      REAL( 8 )       :: CBAR_I2O4         ! molecular velocity of I2O4 (m/s)   
      REAL( 8 )       :: CBAR_INO2         ! molecular velocity of INO2 (m/s) 
      REAL( 8 )       :: CBAR_INO3         ! molecular velocity of INO3 (m/s)  
      REAL( 8 )       :: CBAR_HOI          ! molecular velocity of HOI (m/s)
      
      REAL( 8 )       :: CLNO3_H2O_RXN_TIME   ! CLNO3 reaction time per aerosol surface area density with H2O (s/m)  
      REAL( 8 )       :: HBR_RXN_TIME         ! HBR reaction time per aerosol surface area density with seasalt (s/m)
      REAL( 8 )       :: BRNO3_H2O_RXN_TIME   ! BRNO3 reaction time per aerosol surface area density with H2O (s/m)
      REAL( 8 )       :: HOBR_ASS_RXN_TIME    ! HOBR reaction time per aerosol surface area density with fine-mode ACL (s/m)
      REAL( 8 )       :: BRNO2_RXN_TIME       ! BRNO2 reaction time per aerosol surface area density (s/m)
      REAL( 8 )       :: BRNO3_RXN_TIME       ! BRNO3 reaction time per aerosol surface area density (s/m)
      REAL( 8 )       :: I2O2_RXN_TIME        ! I2O2 reaction time per aerosol surface area density (s/m)
      REAL( 8 )       :: I2O3_RXN_TIME        ! I2O3 reaction time per aerosol surface area density (s/m)
      REAL( 8 )       :: I2O4_RXN_TIME        ! I2O4 reaction time per aerosol surface area density (s/m)
      REAL( 8 )       :: INO2_RXN_TIME        ! INO2 reaction time per aerosol surface area density (s/m)
      REAL( 8 )       :: INO3_RXN_TIME        ! INO3 reaction time per aerosol surface area density (s/m)
      REAL( 8 )       :: HOI_RXN_TIME         ! HOI reaction time per aerosol surface area density (s/m)

      REAL( 8 )       :: INV_DIFF_CLNO3       ! reciprocal molecular diffusivity [m2/sec]
      REAL( 8 )       :: INV_DIFF_HBR         ! reciprocal molecular diffusivity [m2/sec]
      REAL( 8 )       :: INV_DIFF_BRNO3       ! reciprocal molecular diffusivity [m2/sec]
      REAL( 8 )       :: INV_DIFF_BRNO2       ! reciprocal molecular diffusivity [m2/sec]
      REAL( 8 )       :: INV_DIFF_HOBR        ! reciprocal molecular diffusivity [m2/sec]                           
      REAL( 8 )       :: INV_DIFF_I2O2        ! reciprocal molecular diffusivity [m2/sec]
      REAL( 8 )       :: INV_DIFF_I2O3        ! reciprocal molecular diffusivity [m2/sec]
      REAL( 8 )       :: INV_DIFF_I2O4        ! reciprocal molecular diffusivity [m2/sec]
      REAL( 8 )       :: INV_DIFF_INO3        ! reciprocal molecular diffusivity [m2/sec]
      REAL( 8 )       :: INV_DIFF_INO2        ! reciprocal molecular diffusivity [m2/sec]
      REAL( 8 )       :: INV_DIFF_HOI         ! reciprocal molecular diffusivity [m2/sec]

      REAL            :: MARINE_AREA                     ! variable to identify grid-cells over ocean

      INTEGER, SAVE   :: IMAE_IDX
      !INTEGER, SAVE   :: IHMML_IDX
      INTEGER, SAVE   :: NO3_IDX
      INTEGER, SAVE   :: GLY_IDX
      INTEGER, SAVE   :: MGLY_IDX
      INTEGER, SAVE   :: BR_IDX   
      INTEGER, SAVE   :: HO2_IDX

C *** variables for parameterization by Bertram and Thornton 
      REAL         :: ACL           ! i+j or k mode chloride, ug/m3 
      REAL         :: ABR           ! i+j or k mode bromide, ug/m3   
      REAL         :: AH2O          ! i+j or k mode water, ug/m3          
      REAL         :: POC           ! i or j mode primary organic carbon, ug/m3 
      REAL         :: PNCOM         ! i or j mode primary noncarbon organic matter, ug/m3          
      REAL,      SAVE :: MWCLH2O       ! ratio of MWCL/MWH2O 
      REAL,      SAVE :: MWH2OCL       ! ratio of MWH2O/MWCL 
      REAL,      SAVE :: MWCLCLNO2     ! ratio of MWCL/MWCLNO2
      REAL( 8 ), SAVE :: INV_MWCL      ! ratio of 1.0D0/MWCL 
      REAL( 8 ), SAVE :: INV_MWBR      ! ratio of 1.0D0/MWBR   
      REAL( 8 )       :: PPM_FACTOR    ! conversion factor to umoles/m3 to ppm
C *** variables for heterogeneous hydrolysis of NTR2
      REAL            :: CON_TOA       ! Conc. of total OA [ug/m3]
      REAL            :: F_PART_NTR2   ! NTR2 fraction in the particle phase
      
      
      LOGICAL, SAVE     :: FIRSTIME = .TRUE.

      INTEGER           :: IRATE        ! loop counter
      INTEGER           :: INDX         ! found array index
      INTEGER, SAVE     :: CLNO2_IDX
      INTEGER, SAVE     :: IEPOX_IDX

      INTEGER           :: C
      INTEGER           :: R
      INTEGER           :: L
      INTEGER           :: N
      INTEGER           :: I

      INTEGER      SPC        ! loop counter  
      
C *** variables for getting aerosol diagnostic file flag
      INTEGER           :: STATUS                      ! ENV... status
      CHARACTER( 80 )   :: VARDESC                     ! environment variable description
      
      CHARACTER( 132 )  :: XMSG
      
C *** Statement Function **************
      REAL( 8 )            :: INV_ESATL ! arithmetic statement function for reciprocal vapor pressure [Pa]
      REAL( 8 )            :: TT

C *** parameters for calculating GAMMA of HBR ---> ABR, Sherwen et al., ACP, 2016 and IUPAC
      REAL( 8 ) :: GAMMA_HBR                             ! uptake coefficient of HBR  
      REAL( 8 ) :: INV_GAMMA_HBR       
      
! reciprocal of rxn probability for CLNO3 on aerosol, Dieber et al., ACP, 2004
      REAL( 8 ), PARAMETER :: INV_GAMMA_CLNO3_H2O  = 1.0D0 / 0.024D0               ! Dieber et al., ACP, 2004 (with H2O)

! reciprocal of rxn probability for bromine species on aerosol
      REAL( 8 ), PARAMETER :: INV_GAMMA_BRNO3_H2O  = 1.0D0 / 0.03D0                ! Dieber et al., ACP, 2004
      REAL( 8 ), PARAMETER :: INV_GAMMA_HOBR_ASS   = 1.0D0 / 0.1D0                 ! Fernandez et al, 2014
      REAL( 8 ), PARAMETER :: INV_GAMMA_BRNO3  = 1.0D0 / 0.08D0                    ! Fernandez et al, 2014
      REAL( 8 ), PARAMETER :: INV_GAMMA_BRNO2  = 1.0D0 / 0.04D0                    ! Fernandez et al, 2014

! reciprocal of rxn probability for iodine species on aerosol, selective 
      REAL( 8 ), PARAMETER :: INV_GAMMA_I2O2  = 1.0D0 / 0.02D0                     ! Sherwen et al., 2016
      REAL( 8 ), PARAMETER :: INV_GAMMA_I2O3  = 1.0D0 / 0.02D0                     ! Sherwen et al., 2016
      REAL( 8 ), PARAMETER :: INV_GAMMA_I2O4  = 1.0D0 / 0.02D0                     ! Sherwen et al., 2016
      REAL( 8 ), PARAMETER :: INV_GAMMA_HOI   = 1.0D0 / 0.01D0                     ! Sherwen et al., 2016     
      REAL( 8 ), PARAMETER :: INV_GAMMA_INO3  = 1.0D0 / 0.01D0                     ! Saiz-Lopez et al. 2014
      REAL( 8 ), PARAMETER :: INV_GAMMA_INO2  = 1.0D0 / 0.02D0                     ! Saiz-Lopez et al. 2014

C *** Coefficients for the equation, ESATL defining saturation vapor pressure
C *** values of AL, BL, and CL are from:
C     Alduchov and Eskridge, "Improved Magnus Form Approximations of
C                            Saturation Vapor Pressure,"
C                            Jour. of Applied Meteorology, vol. 35,
C                            pp 601-609, April, 1996.
      REAL( 8 ), PARAMETER :: AL = 610.94D0 
      REAL( 8 ), PARAMETER :: BL = 17.625D0
      REAL( 8 ), PARAMETER :: CL = 243.04D0
      REAL( 8 ), PARAMETER :: DL = 1.0D0 / AL


#ifdef verbose_aerosol_chemistry
       LOGICAL              :: DUMP_CELL
#endif


      INTERFACE
         SUBROUTINE GETPAR( FIXED_sg  )
           LOGICAL, INTENT( IN ) :: FIXED_sg    ! fix coarse and accum Sg's to the input value?
         END SUBROUTINE GETPAR
         Subroutine Hetchem_Extract_Aero( CONCVEC, C, R, L, WET_M2,
     &                                  WET_M3, DE_WET )
           Real, Intent( In )    :: CONCVEC( : )
           Integer, Intent(In)   :: C, R, L
           Real(8), Intent( Out ):: Wet_M2( : ), Wet_M3( : ), De_Wet( : )
         End Subroutine Hetchem_Extract_Aero
      END INTERFACE 
  
      INV_ESATL( TT ) = DL * DEXP( BL * ( 273.15D0 - TT ) / ( TT - 273.15D0 + CL ) )
      
C-----------------------------------------------------------------------

      IF ( NHETERO .LT. 1 )RETURN

C *** compute only on first pass

      IF ( FIRSTIME ) THEN
      
        FIRSTIME = .FALSE.

        CALL MAP_AERO()
        CALL MAP_PRECURSOR()

        ALLOCATE( WHICH_AERO_RATE( NHETERO ) )
        
C *** 2nd and 3rd moments (w. H2O and svOA)
        ALLOCATE(  WET_M3( N_MODE ), 
     &             WET_M2( N_MODE ), 
     &             DE_WET( N_MODE ) )
C *** Allocate Gridded Dry Initial Moments so they can be saved for the
C     end of the gas-phase chemical driver
        WHICH_AERO_RATE = -1
        
        SELECTED_AERO_RATES = 0
        DO IRATE = 1, NHETERO
           INDX = INDEX1( HETERO( IRATE ), NUMB_AERO_RATES, NAMES_AERO_RATES )
           IF ( INDX .LT. 1 ) THEN
               XMSG = 'Heterogeneous Reaction Label '// TRIM ( HETERO( IRATE ) )
     &             // ' is not in list of available Reaction Rates.'
               CALL M3EXIT( 'HETCHEM_RATES', 0, 0, XMSG, XSTAT3 )
           END IF 
           SELECTED_AERO_RATES = SELECTED_AERO_RATES + 1
           WHICH_AERO_RATE( SELECTED_AERO_RATES ) = INDEX_AERO_RATES( INDX )
       END DO

       ALLOCATE( KHETERO( SELECTED_AERO_RATES, NCOLS,NROWS,NLAYS ) )
       
       IF ( L_ELMO ) THEN
          ALLOCATE( GAMMA_N2O5IJ( NCOLS,NROWS,NLAYS ) )
          ALLOCATE(  GAMMA_N2O5K( NCOLS,NROWS,NLAYS ) )
          ALLOCATE(  GAMMA_IEPOX( NCOLS,NROWS,NLAYS ) )
          ALLOCATE(   GAMMA_IMAE( NCOLS,NROWS,NLAYS ) )
          ALLOCATE(   KPARTIEPOX( NCOLS,NROWS,NLAYS ) )
          ALLOCATE(     YCLNO2IJ( NCOLS,NROWS,NLAYS ) )
          ALLOCATE(      YCLNO2K( NCOLS,NROWS,NLAYS ) )
       END IF

C ***  Determine molecular weights
       INV_MWCL  = REAL( 1.0 / aerospc_mw( ACL_IDX ), 8 )
       MWCLH2O   = aerospc_mw( ACL_IDX ) / aerospc_mw( AH2O_IDX )
       MWH2OCL   = 1.0 / MWCLH2O
       CLNO2_IDX = INDEX1( 'CLNO2', NUMB_MECH_SPC, CHEMISTRY_SPC )
       
       IF ( CLNO2_IDX .GT. 0 ) THEN
          MWCLCLNO2 = aerospc_mw( ACL_IDX) / SPECIES_MOLWT( CLNO2_IDX )
       ELSE
          MWCLCLNO2 = 0.0
       END IF

       IEPOX_IDX = INDEX1( 'IEPOX', NUMB_MECH_SPC, CHEMISTRY_SPC )
       IF ( IEPOX_IDX .GT. 0 ) THEN
          MWTIEPOX  = REAL( SPECIES_MOLWT( IEPOX_IDX ), 8 ) 
       ELSE
          MWTIEPOX  = 118.1D0
       END IF
       CFACTOR_IEPOX = DSQRT( 8.0D3 * REAL( RGASUNIV, 8 ) / ( MWTIEPOX * DPI ) )

       IMAE_IDX = INDEX1( 'IMAE', NUMB_MECH_SPC, CHEMISTRY_SPC )
       IF( IMAE_IDX .GT. 0 )THEN
           MWTIMAE  = REAL( SPECIES_MOLWT( IMAE_IDX ), 8 )
       ELSE
           MWTIMAE  = 102.0D0
       END IF
       CFACTOR_IMAE = DSQRT( 8.0D3 * REAL( RGASUNIV, 8 ) / ( MWTIMAE * DPI ) )

       IF( AIETET_IDX .GT. 0 ) THEN
          INV_MWIETET = REAL( 1.0/ aerospc_mw( AIETET_IDX ), 8 )
       ELSE
          INV_MWIETET = 0.0D0
       END IF

       IF( AIEOS_IDX .GT. 0 ) THEN
          INV_MWIEOS  = REAL( 1.0/ aerospc_mw( AIEOS_IDX  ), 8 )
       ELSE 
          INV_MWIEOS = 0.0D0
       END IF

       IF( ASO4_IDX .GT. 0 )THEN
          INV_MWASO4 = REAL( 1.0/ aerospc_mw( ASO4_IDX ), 8 )
       ELSE
          INV_MWASO4 = 0D0
       END IF

       NO3_IDX = INDEX1( 'NO3', NUMB_MECH_SPC, CHEMISTRY_SPC )
       IF( NO3_IDX .GT. 0 )THEN
           MWTNO3  = REAL( SPECIES_MOLWT( NO3_IDX ), 8 )
       ELSE
           MWTNO3  = 62.01D0
       END IF
       CFACTOR_NO3 = DSQRT( 8.0D3 * REAL( RGASUNIV, 8 ) / ( MWTNO3 * DPI ) )

       GLY_IDX = INDEX1( 'GLY', NUMB_MECH_SPC, CHEMISTRY_SPC )
       IF( GLY_IDX .GT. 0 )THEN
           MWTGLY  = REAL( SPECIES_MOLWT( GLY_IDX ), 8 )
       ELSE
           MWTGLY  = 58.04D0
       END IF
       CFACTOR_GLY = DSQRT( 8.0D3 * REAL( RGASUNIV, 8 ) / ( MWTGLY * DPI ) )

       MGLY_IDX = INDEX1( 'MGLY', NUMB_MECH_SPC, CHEMISTRY_SPC )
       IF( MGLY_IDX .GT. 0 )THEN
           MWTMGLY  = REAL( SPECIES_MOLWT( MGLY_IDX ), 8 )
       ELSE
           MWTMGLY  = 72.07D0
       END IF
       CFACTOR_MGLY = DSQRT( 8.0D3 * REAL( RGASUNIV, 8 ) / ( MWTMGLY * DPI ) )

       HO2_IDX = INDEX1( 'HO2', NUMB_MECH_SPC, CHEMISTRY_SPC )
       IF( HO2_IDX .GT. 0 )THEN
           MWTHO2  = REAL( SPECIES_MOLWT( HO2_IDX ), 8 )
       ELSE
           MWTHO2  = 33.0D0
       END IF
       CFACTOR_HO2 = DSQRT( 8.0D3 * REAL( RGASUNIV, 8 ) / ( MWTHO2 * DPI ) )

C ***  Map to acids/nucleophiles for epoxide uptake
       INDX =  INDEX1( NAMES_AERO_RATES( IA_IEPOX ), NHETERO, HETERO )   
       IF ( INDX .GT. 0 ) THEN ! Find indices for acid catalyzed species in IEPOX reaction
          If ( ( INDEX( MECHNAME, 'AE6I' ) .GT. 0 ) .OR.
     &         ( INDEX( MECHNAME, 'AE7I' ) .GT. 0 )  ) then
             NUMVOC         = 2 ! IEPOX + MAE/HMML
             N_NUCPAIRS     = N_NUCPAIRS_AEI
             ALLOCATE ( ACID_NUC_PAIRS( N_NUCPAIRS ) )
             ACID_NUC_PAIRS = ACID_NUC_PAIRS_AEI
          Else If ( (INDEX( MECHNAME, 'CRACMM1') .GT. 0)
     &      .OR.    (INDEX( MECHNAME, 'CRACMM2') .GT. 0) ) then
             NUMVOC         = 1
             N_NUCPAIRS     = N_NUCPAIRS_CRCM
             ALLOCATE ( ACID_NUC_PAIRS( N_NUCPAIRS ) )
             ACID_NUC_PAIRS = ACID_NUC_PAIRS_CRCM
          Else ! AERO6/7 (condensed IEPOX SOA)
             NUMVOC         = 1
             N_NUCPAIRS     = N_NUCPAIRS_AE
             ALLOCATE ( ACID_NUC_PAIRS( N_NUCPAIRS ) )
             ACID_NUC_PAIRS = ACID_NUC_PAIRS_AE
          END IF
          ALLOCATE ( ACID_PRODMAP_IDX( N_NUCPAIRS ) )
          ALLOCATE (  ACID_NUCMAP_IDX( N_NUCPAIRS ) )
          ACID_PRODMAP_IDX = 0
          ACID_NUCMAP_IDX  = 0
          DO N = 1, N_NUCPAIRS
            ACID_PRODMAP_IDX(N) = FINDAERO( ACID_NUC_PAIRS(N)%PROD, .TRUE. )
            ACID_NUCMAP_IDX(N)  = FINDAERO( ACID_NUC_PAIRS(N)%NUC,  .TRUE. )
#ifdef verbose_aerosol_chemistry        
            Write( logdev,'( 5x, a, i4 )' ) acid_nuc_pairs(n)%prod, acid_prodmap_idx(n)
            Write( logdev,'( 5x, a, i4 )' ) acid_nuc_pairs(n)%nuc,  acid_nucmap_idx(n)
#endif
          END DO
       END IF

       AERO_CHEM_SET = .TRUE.

      END IF   ! first time condition

      KHETERO = 0.0D0
      
      IF ( L_ELMO ) THEN
         GAMMA_N2O5IJ = 0.0
         GAMMA_N2O5K  = 0.0
         YCLNO2IJ     = 0.0
         YCLNO2K      = 0.0
         GAMMA_IEPOX  = 0.0
         GAMMA_IMAE   = 0.0
         KPARTIEPOX   = 0.0
      END IF
     
C *** Calculate rate constants at each grid cell location
      LOOP_LAY: DO L = 1, NLAYS
         LOOP_ROW: DO R = 1, NROWS
            LOOP_COL: DO C = 1, NCOLS
     
#ifdef verbose_aerosol_chemistry        
               IF ( L .EQ. 1 .AND. R .EQ. INT(NROWS/2)+1 .AND.
     &              C .EQ. INT(NCOLS/2)+1 ) THEN
                  DUMP_CELL = .TRUE.
               ELSE
                  DUMP_CELL = .FALSE.
               END IF
#endif         
C *** Extract aerosol concentrations and update aerosol volume, surface
C     area and diameter values.
               AIRDENS = DENS( C,R,L )

               CALL HETCHEM_EXTRACT_AERO( CGRID( C,R,L,: ), C, R, L,
     &               WET_M2, WET_M3, DE_WET )

C *** Load Meteorological Variables
               AIRTEMP = REAL( TEMP( C,R,L ), 8 )
               AIRPRES = REAL(  PRESS( C,R,L ), 8 )   

C *** to compute RH & molecular kinetic factors
               H2OVP = REAL( PRESS( C,R,L ) * WVAPOR( C,R,L )
     &               / ( EPSWATER  + WVAPOR( C,R,L ) ), 8 )    
     
               AIRRH = MAX( 0.005D0, MIN( 0.99D0, H2OVP * INV_ESATL( AIRTEMP ) ) )

               CBAR_COEFF  = REAL( SQRT( CFACTOR * TEMP( C,R,L ) ), 8) 
               
               ADJUST_DIFF = REAL( ( TEMP( C,R,L ) * INV_STDTEMP ) ** 1.75 * ( STDATMPA / PRESS( C,R,L ) ), 8 )

               IF ( PRESENT( DENS ) ) THEN
                  PPM_FACTOR  = 1.0D-3 * REAL( MWAIR / DENS( C,R,L ), 8 )
               ELSE
                  PPM_FACTOR  = UGM3_CONV_FAC * REAL( TEMP( C,R,L ) / PRESS( C,R,L ), 8 )
               END IF
              
               RFACTOR = 60.0D0 * COEF1 * REAL( PRESS( C,R,L ) / TEMP( C,R,L ), 8 )

C *** calculate molecular speeds (m/s) using Eq 4 of Pleim et al (1995)
               CBAR = CBAR_COEFF * INV_SQRT_MWN2O5
               CBARNO3 = CFACTOR_NO3 * DSQRT( AIRTEMP )
               CBARGLY = CFACTOR_GLY  * DSQRT( AIRTEMP )
               CBARMGLY = CFACTOR_MGLY * DSQRT( AIRTEMP )
               CBARHO2 = CFACTOR_HO2 * DSQRT( AIRTEMP )

               CBAR_CLNO3 = CBAR_COEFF * INV_SQRT_MWCLNO3
               CBAR_HBR   = CBAR_COEFF * INV_SQRT_MWHBR
               CBAR_BRNO3 = CBAR_COEFF * INV_SQRT_MWBRNO3       
               CBAR_BRNO2 = CBAR_COEFF * INV_SQRT_MWBRNO2
               CBAR_HOBR  = CBAR_COEFF * INV_SQRT_MWHOBR                 
               CBAR_I2O2  = CBAR_COEFF * INV_SQRT_MWI2O2
               CBAR_I2O3  = CBAR_COEFF * INV_SQRT_MWI2O3
               CBAR_I2O4  = CBAR_COEFF * INV_SQRT_MWI2O4
               CBAR_INO3  = CBAR_COEFF * INV_SQRT_MWINO3
               CBAR_INO2  = CBAR_COEFF * INV_SQRT_MWINO2
               CBAR_HOI   = CBAR_COEFF * INV_SQRT_MWHOI

C *** correct N2O5 molecular diffusivity for ambient conditions

               INV_DIFFUSIVITY = 1.0D0 / ( STD_DIFF_N2O5 * ADJUST_DIFF )

C *** correct molecular diffusivity for ambient conditions - assumed similar to that of N2O5

               INV_DIFF_CLNO3  = 1.0D0 / ( STD_DIFF_CLNO3 * ADJUST_DIFF )
               INV_DIFF_HBR    = 1.0D0 / ( STD_DIFF_HBR * ADJUST_DIFF )
               INV_DIFF_BRNO3  = 1.0D0 / ( STD_DIFF_BRNO3 * ADJUST_DIFF )
               INV_DIFF_BRNO2  = 1.0D0 / ( STD_DIFF_BRNO2 * ADJUST_DIFF )
               INV_DIFF_HOBR   = 1.0D0 / ( STD_DIFF_HOBR * ADJUST_DIFF )
               INV_DIFF_I2O2   = 1.0D0 / ( STD_DIFF_I2O2 * ADJUST_DIFF )
               INV_DIFF_I2O3   = 1.0D0 / ( STD_DIFF_I2O3 * ADJUST_DIFF )
               INV_DIFF_I2O4   = 1.0D0 / ( STD_DIFF_I2O4 * ADJUST_DIFF )
               INV_DIFF_INO3   = 1.0D0 / ( STD_DIFF_INO3 * ADJUST_DIFF )
               INV_DIFF_INO2   = 1.0D0 / ( STD_DIFF_INO2  * ADJUST_DIFF )
               INV_DIFF_HOI    = 1.0D0 / ( STD_DIFF_HOI * ADJUST_DIFF )

C *** get KN2O5 rate constants YIELD_CLNO2 for each mode
               YIELD_CLNO2 = 0.0D0
               YIELDIJ     = 0.0D0

               DO N = 1, N_MODE          

                  IF ( N .LE. 2 ) THEN
                     GAMMA = N2O5_GAMMA( AIRTEMP, AIRRH, 0 )
                     IF ( L_ELMO ) GAMMA_N2O5IJ( C,R,L ) = REAL( GAMMA, 4 )
                  ELSE IF ( N .EQ. 3 ) THEN
                     GAMMA = N2O5_GAMMA( AIRTEMP, AIRRH, 5 )
                     IF ( L_ELMO ) GAMMA_N2O5K( C,R,L ) = REAL( GAMMA, 4 )
                  ELSE
                     GAMMA = 0.0D0
                  END IF
                                     
                  XXF( N ) = WET_M2( N )
     &                    / ( 4.0D0 + 0.5D0 * DE_WET( N ) * GAMMA * CBAR * INV_DIFFUSIVITY )
     
                  KN2O5( N ) = GAMMA * XXF( N )

                  IF ( GAMMA_NO3 .GT. 0.0D0 ) THEN
                     KNO3( N ) = GAMMA_NO3 * WET_M2( N ) 
     &                         / ( 4.0D0 + 0.5D0 * DE_WET( N ) * GAMMA_NO3 
     &                         * CBARNO3 * INV_DIFFUSIVITY ) * CBARNO3 * DPI
                  ELSE
                     KNO3( N ) = 0.0d0
                  END IF

                  IF( GAMMA_GLY .GT. 0.0D0) THEN
                     KGLY( N ) = GAMMA_GLY * WET_M2( N ) 
     &                         / ( 4.0D0 + 0.5D0 * DE_WET( N ) * GAMMA_GLY 
     &                         * CBARGLY * INV_DIFFUSIVITY ) * CBARGLY * DPI
                     ! scale MGLY uptake by relative H-law (Marais et
                     ! al. ACPD approach) implemented 1/2016 H. Pye
                     KMGLY( N ) = 0.09 * GAMMA_GLY * WET_M2( N ) 
     &                          / ( 4.0D0 + 0.5D0 * DE_WET( N ) * 0.09 * GAMMA_GLY 
     &                          * CBARMGLY * INV_DIFFUSIVITY ) * CBARMGLY * DPI
                  ELSE
                     KGLY( N ) = 0.0D0
                     KMGLY( N ) = 0.0D0
                  END IF

                  IF( GAMMA_HO2 .GT. 0.0D0) THEN
                     KHO2( N ) = GAMMA_HO2 * WET_M2( N )
     &                         / ( 4.0D0 + 0.5D0 * DE_WET( N ) * GAMMA_HO2
     &                         * CBARHO2 * INV_DIFFUSIVITY ) * CBARHO2 * DPI
                  ELSE
                     KHO2( N ) = 0.0D0
                  END IF


c *** get fine aerosol H2O and chlorine concentrations in
                  AH2O = aerospc_conc( AH2O_IDX,N )
                  ACL  = aerospc_conc( ACL_IDX, N )
                  CL_PPM( N ) = PPM_FACTOR * ( ACL * INV_MWCL )

      If ( Index( mechname, 'CB6R5M_AE7_AQ' ) .Gt. 0 ) Then 
         ABR  = aerospc_conc( ABR_IDX, N ) 
         INV_MWBR  = REAL( 1.0 / aerospc_mw( ABR_IDX ), 8 )  
         BR_PPM( N ) = PPM_FACTOR * ( ABR * INV_MWBR ) 
      Else
         BR_PPM( N ) = 0.0D0  
      End If

C *** If only a small amount of water is present on aerosol, keep YIELD_CLNO2 zero
                 IF ( AH2O .GT. 5.0E-01 .AND. ACL .GT. 1.0E-04 ) THEN
                     IF ( MWH2OCL * ACL .GT. 1.0E-4 * AH2O ) THEN 
                        YIELD_CLNO2( N ) = 1.0D0
     &                                   / ( 1.0D0 + ( 1.0D0 / 483.0D0 ) * ( AH2O / ACL ) * MWCLH2O )
                     END IF
                  END IF
 
               END DO ! End each mode 

c *** compute yield from lumped Aitken and Accumulations concentrations
               AH2O = aerospc_conc( AH2O_IDX,1 ) + aerospc_conc( AH2O_IDX,2 )
               ACL  = aerospc_conc( ACL_IDX,1 )  + aerospc_conc( ACL_IDX,2 )
               IF ( AH2O .GT. 5.0E-01 .AND. ACL .GT. 1.0E-04 ) THEN
                  IF ( MWH2OCL * ACL .GT. 1.0E-4 * AH2O ) THEN 
                     YIELDIJ = 1.0D0 / ( 1.0D0 + ( 1.0D0 / 483.0D0 )
     &                       * REAL( ( AH2O / ACL ) * MWCLH2O, 8 ) )
                  END IF
               END IF  
              
               KN2O5 = CBAR * DPI * KN2O5
C *** calculate aerosol surface area
               TOTSURFA = ( WET_M2( 1 ) + WET_M2( 2 ) ) * DPI

C *** Calculate IEPOX and MAE uptake information for accumulation mode
               RADIUS_I  = 0.5D0 * DE_WET( 1 ) ! particle size - i-mode
               RADIUS    = 0.5D0 * DE_WET( 2 ) ! particle size - j-mode

C *** Calculate approximate seasalt surface area, m2/m3
C *** Main components of Seasalt are: NA, CL, MG, K, CA, SO4
C *** However, SO4 over seawater can generate from other sources
C *** Fo SO4, use SO4/NA emissions ratio in seasalt

               MARINE_AREA = ( OCEAN( C,R ) + SZONE( C,R ) )

                IF ( MARINE_AREA .GT. 0.0 ) THEN
                 SSALT_VOL  =   1.0D-9 * ( Real ( aerospc_conc( ANA_IDX,2 ) / aerospc(ANA_IDX)%density, 8 ) +          ! NAJ - m3/m3
     @                                    Real ( aerospc_conc( ACL_IDX,2 ) / aerospc(ACL_IDX)%density, 8 ) +          ! CLJ - m3/m3
     @                                    Real ( aerospc_conc( AMG_IDX,2 ) / aerospc(AMG_IDX)%density, 8 ) +          ! MGJ - m3/m3
     @                                    Real ( aerospc_conc( AK_IDX,2 )  / aerospc(AK_IDX)%density, 8  ) +          ! KJ - m3/m3
     @                                    Real ( aerospc_conc( ACA_IDX,2 ) / aerospc(ACA_IDX)%density, 8 ) +          ! CAJ - m3/m3
     @                                    Real ( 0.2514 * aerospc_conc( ANA_IDX,2 ) / aerospc(ASO4_IDX)%density, 8) ) ! SO4 - m3/m3 - use SO4/NA ratio in seasalt

                SSALT_SURFA = ( 3.0D0 * SSALT_VOL / RADIUS ) * DEXP ( 0.5D0 * AEROMODE_LNSG( 2 ) * AEROMODE_LNSG( 2 ) )
               ELSE
                SSALT_SURFA = 0.0
                ENDIF

C *** calculate GAMMA_HBR  
      GAMMA_HBR = 1.3D-8 * DEXP ( 4290/ AIRTEMP) 
      INV_GAMMA_HBR = 1.0D0 / GAMMA_HBR
                                                                                                               
C *** set up variables needed for calculating K_BRONO2              
               CLNO3_H2O_RXN_TIME   = 4.0D0 * INV_GAMMA_CLNO3_H2O / CBAR_CLNO3
               HBR_RXN_TIME         = 4.0D0 * INV_GAMMA_HBR / CBAR_HBR 
               BRNO3_H2O_RXN_TIME   = 4.0D0 * INV_GAMMA_BRNO3_H2O / CBAR_BRNO3
               HOBR_ASS_RXN_TIME    = 4.0D0 * INV_GAMMA_HOBR_ASS  / CBAR_HOBR
               BRNO3_RXN_TIME       = 4.0D0 * INV_GAMMA_BRNO3 / CBAR_BRNO3
               BRNO2_RXN_TIME       = 4.0D0 * INV_GAMMA_BRNO2 / CBAR_BRNO2
               I2O2_RXN_TIME        = 4.0D0 * INV_GAMMA_I2O2 / CBAR_I2O2
               I2O3_RXN_TIME        = 4.0D0 * INV_GAMMA_I2O3 / CBAR_I2O3
               I2O4_RXN_TIME        = 4.0D0 * INV_GAMMA_I2O4 / CBAR_I2O4
               INO3_RXN_TIME        = 4.0D0 * INV_GAMMA_INO3 / CBAR_INO3
               INO2_RXN_TIME        = 4.0D0 * INV_GAMMA_INO2 / CBAR_INO2
               HOI_RXN_TIME         = 4.0D0 * INV_GAMMA_HOI  / CBAR_HOI
               
               CBARIEPOX = CFACTOR_IEPOX * DSQRT( AIRTEMP )
               CBARIMAE  = CFACTOR_IMAE  * DSQRT( AIRTEMP )

               ! Rate constants and concentrations
               CALL CALCISOPGAMMAS( RADIUS, AIRTEMP, GAMMAIEPOX,
     &                              GAMMAIMAE, KPIEPOX, FH2O, FOS, FDIM1, FDIM2)

               IF( GAMMAIEPOX .GT. 1.0D-72 ) THEN
                  KIEPOX = WET_M2( 2 ) * DPI
     &                   / ( RADIUS * INV_DIFFUSIVITY +  4.0D0 / ( CBARIEPOX * GAMMAIEPOX) )
                  If ( AIETET_IDX .GT. 0 ) Then
                     TETROL_PPM  = PPM_FACTOR
     &                       * REAL( aerospc_conc( AIETET_IDX,2 ), 8 ) * INV_MWIETET
                  Else
                     TETROL_PPM = 0.0D0
                  End If 
                  If ( AIEOS_IDX .GT. 0 ) Then
                     IEPOXOS_PPM = PPM_FACTOR
     &                     * REAL( aerospc_conc( AIEOS_IDX,2 ), 8 ) * INV_MWIEOS
                  Else
                     IEPOXOS_PPM = 0.0D0
                  End If
                  If ( ASO4_IDX .GT. 0 ) Then
                     ASO4J_PPM = PPM_FACTOR
     &                     * REAL( aerospc_conc( ASO4_IDX,2 ), 8 ) * INV_MWASO4
                  Else
                     ASO4J_PPM = 0.0D0
                  End If
               ELSE
                  KIEPOX = 0.0D0
                  TETROL_PPM = 0.0D0
                  IEPOXOS_PPM = 0.0D0
                  ASO4J_PPM = 0.0D0
               END IF

               IF( GAMMAIMAE .GT. 1.0D-72 ) THEN
                  KIMAE = WET_M2( 2 ) * DPI
     &                  / ( RADIUS * INV_DIFFUSIVITY + 4.0D0 / ( CBARIMAE * GAMMAIMAE ) )
               ELSE
                  KIMAE = 0.0d0
               END IF

               ! Diagnostic information
               IF ( L_ELMO ) GAMMA_IEPOX( C,R,L ) = REAL( GAMMAIEPOX,4 )
               IF ( L_ELMO ) GAMMA_IMAE(  C,R,L ) = REAL( GAMMAIMAE, 4 )
               IF ( L_ELMO ) KPARTIEPOX(  C,R,L ) = REAL( KPIEPOX,   4 )

               LOOP_RATES: DO IRATE = 1, SELECTED_AERO_RATES
 
               SELECT CASE( WHICH_AERO_RATE( IRATE ) )
                  CASE( IA_N2O5IJ )  
                     KHETERO( IRATE, C,R,L ) = 60.0D0 * ( KN2O5( 1 ) + KN2O5( 2 ) )! convert to min-1
                          
                  CASE( IA_N2O5K )  
                     KHETERO( IRATE, C,R,L ) = 60.0D0 * KN2O5 (3) ! convert to min-1
                     
                  CASE( IA_N2O5J )  
                     KHETERO( IRATE, C,R,L ) = 60.0D0 * KN2O5 (2) ! convert to min-1
                     
                  CASE( IA_N2O5I )  
                     KHETERO( IRATE, C,R,L ) = 60.0D0 * KN2O5 (1) ! convert to min-1
                     
                  CASE( IA_N2O5IJY )  
                     KHETERO( IRATE, C,R,L ) = 60.0D0 * YIELDIJ * ( KN2O5( 1 ) + KN2O5( 2 ) ) ! convert to min-1
                     IF ( L_ELMO ) YCLNO2IJ( C,R,L ) = REAL( YIELDIJ, 4 )

                  CASE( IA_N2O5KY )  
                     KHETERO( IRATE, C,R,L ) = 60.0D0 * YIELD_CLNO2( 3 ) * KN2O5( 3 ) 
                     IF ( L_ELMO ) YCLNO2K( C,R,L ) = REAL( YIELD_CLNO2( 3 ), 4 )

                  CASE( IA_H2NO3PAIJ )  
                     KHETERO( IRATE, C,R,L ) = 60.0D0 * AQUEOUS_FREQUENCY
     &                                       *  MAX( ( 1.0D0 - YIELDIJ ), 0.0D0 ) 
                     IF ( L_ELMO ) YCLNO2IJ( C,R,L ) = REAL( YIELDIJ, 4 )
                     
                  CASE( IA_H2NO3PBIJ )  
                     IF ( YIELDIJ .GT. 0.0D0 ) THEN
                        KHETERO( IRATE, C,R,L ) = 60.0D0 * AQUEOUS_FREQUENCY * YIELDIJ 
     &                                          / ( CL_PPM( 1 ) + CL_PPM( 2 ) )   ! 1/(min*ppm)
                     ELSE
                        KHETERO( IRATE, C,R,L ) = 0.0D0
                     END IF
                     
                  CASE( IA_H2NO3PAI )
                     KHETERO( IRATE, C,R,L ) = 60.0D0 * AQUEOUS_FREQUENCY
     &                                       * MAX( ( 1.0D0 - YIELD_CLNO2( 1 ) ), 0.0D0 ) 

                  CASE( IA_H2NO3PBI )
                     IF ( YIELD_CLNO2( 1 ) .GT. 0.0D0 ) THEN
                        KHETERO( IRATE, C,R,L ) = 60.0D0 * AQUEOUS_FREQUENCY
     &                                          * YIELD_CLNO2( 1 ) / CL_PPM( 1 ) ! (min*ppm)-1
                        IF ( L_ELMO ) THEN
                           FACTOR = CL_PPM( 1 ) / ( CL_PPM( 1 )+ CL_PPM( 2 ) )
                           YCLNO2IJ( C,R,L ) = REAL( FACTOR * YIELD_CLNO2( 1 ), 4 ) 
     &                                       + YCLNO2IJ( C,R,L )
                        END IF
                     ELSE
                        KHETERO( IRATE, C,R,L ) = 0.0D0
                     END IF 
                              
                  CASE( IA_H2NO3PAJ )
                     KHETERO( IRATE, C,R,L ) = 60.0D0 * AQUEOUS_FREQUENCY
     &                                       * MAX( ( 1.0D0 - YIELD_CLNO2( 2 ) ), 0.0D0 ) 
                               
                  CASE( IA_H2NO3PBJ )
                     IF (  YIELD_CLNO2( 2 ) .GT. 0.0D0 ) THEN
                        KHETERO( IRATE, C,R,L ) =  60.0D0 * AQUEOUS_FREQUENCY
     &                                          *  YIELD_CLNO2( 2 ) / CL_PPM( 2 ) ! (min*ppm)-1
                        IF ( L_ELMO ) THEN
                           FACTOR = CL_PPM( 2 ) / ( CL_PPM( 1 )+ CL_PPM( 2 ) )
                           YCLNO2IJ( C,R,L ) = YCLNO2IJ( C,R,L ) + REAL( FACTOR * YIELD_CLNO2( 2 ), 4 ) 
                        END IF                               
                     ELSE
                          KHETERO( IRATE, C,R,L ) = 0.0D0
                     END IF 
                                
                  CASE( IA_PNCOMLI )
                     POC   = aerospc_conc( APOC_IDX,1 )
                     PNCOM = aerospc_conc( APNCOM_IDX,1 )
                     KHETERO( IRATE, C,R,L ) = RFACTOR * PNCOM_LOSS( POC, PNCOM )
                      
                  CASE( IA_PNCOMLJ )
                     POC   = aerospc_conc( APOC_IDX,2 )
                     PNCOM = aerospc_conc( APNCOM_IDX,2 )
                     KHETERO( IRATE, C,R,L ) = RFACTOR * PNCOM_LOSS( POC, PNCOM )

                  CASE( IA_H2NO3PAK )
                     KHETERO( IRATE, C,R,L ) = 60.0D0 * AQUEOUS_FREQUENCY
     &                                       *  MAX( ( 1.0D0 - YIELD_CLNO2( 3 ) ), 0.0D0 ) ! * KN2O5( 3 )
                               
                  CASE( IA_H2NO3PBK )
                     IF ( YIELD_CLNO2( 3 ) .GT. 0.0D0 ) THEN
                        KHETERO( IRATE, C,R,L ) = 60.0D0 * AQUEOUS_FREQUENCY
     &                                          * YIELD_CLNO2( 3 ) / CL_PPM( 3 ) ! (min*ppm)-1
                        IF ( L_ELMO ) YCLNO2K( C,R,L ) = REAL( YIELD_CLNO2( 3 ), 4 ) 
                     ELSE
                        KHETERO( IRATE, C,R,L ) = 0.0D0
                     END IF 
                               
                  CASE( IA_NO2 )
C *** calculate pseudo-first order rate constant for NO2 loss using Eq 1 of Vogel
C     et al. (2003). Units of KNO2 is in 1/min in the paper; divide it
C     by 60 to convert it into 1/sec
                     KNO2 = MAX ( 0.0D0, 1.0D-04 * TOTSURFA )
                     KHETERO( IRATE, C,R,L )  = 60.0D0 * KNO2 ! convert to min-1

C ***            Isoprene epoxide uptake
                 CASE( IA_IEPOX )
                     KHETERO( IRATE, C,R,L ) = 60.0D0 * KIEPOX ! min-1

                 CASE( IA_TETROL )
                     KHETERO( IRATE, C,R,L ) = 60.0D0 * AQUEOUS_FREQUENCY * FH2O( 1 )

                 CASE( IA_IEPOXOS )
                     IF ( ASO4J_PPM .GT. 0.0D0 ) THEN
                        KHETERO( IRATE, C,R,L ) = 60.0D0 * AQUEOUS_FREQUENCY 
     &                                          * FOS( 1 ) / ASO4J_PPM
                     ELSE
                        KHETERO( IRATE, C,R,L ) = 0.0D0
                     END IF

                 CASE( IA_TETROLDIM )
                     IF ( TETROL_PPM .GT. 0.0D0 ) THEN
                        KHETERO( IRATE, C,R,L ) = 60.0D0 * AQUEOUS_FREQUENCY
     &                                          * FDIM1( 1 ) / TETROL_PPM ! min*ppm-1  
                     ELSE
                        KHETERO( IRATE, C,R,L ) = 0.0D0
                     END IF

                 CASE( IA_IEPOXOSDI )
                     IF ( IEPOXOS_PPM .GT. 0.0D0 ) THEN
                        KHETERO( IRATE, C,R,L ) = 60.0D0 * AQUEOUS_FREQUENCY
     &                                          * FDIM2( 1 ) / IEPOXOS_PPM ! min*ppm-1
                     ELSE
                        KHETERO( IRATE, C,R,L ) = 0.0D0
                     END IF

                 CASE( IA_IMAE )
                     KHETERO( IRATE, C,R,L ) = 60.0D0 * KIMAE ! min-1

                 CASE( IA_2MG )
                     KHETERO( IRATE, C,R,L ) = 60.0D0 * AQUEOUS_FREQUENCY * FH2O(2)

                 CASE( IA_IMAEOS )
                     IF ( ASO4J_PPM .GT. 0.0D0 ) THEN
                        KHETERO( IRATE, C,R,L ) = 60.0D0 * AQUEOUS_FREQUENCY 
     &                                         * FOS(2) / ASO4J_PPM
                     ELSE
                        KHETERO( IRATE, C,R,L ) = 0.0D0
                     END IF


C *** CRACMM1 Implementation of sulfated and non-sulfated IEPOX-SOA

                 CASE( IA_ISO3NOSJ )
                     KHETERO( IRATE, C,R,L ) = 60.0D0 * AQUEOUS_FREQUENCY * FH2O( 1 )

                 CASE( IA_ISO3OSJ )
                     IF ( ASO4J_PPM .GT. 0.0D0 ) THEN
                        KHETERO( IRATE, C,R,L ) = 60.0D0 * AQUEOUS_FREQUENCY
     &                                          * FOS( 1 ) / ASO4J_PPM
                     ELSE
                        KHETERO( IRATE, C,R,L ) = 0.0D0
                     END IF


                 CASE( IA_NO3 )
                     KHETERO( IRATE, C,R,L ) = 60.0D0 * ( KNO3( 1 ) + KNO3( 2 ) ) ! min-1

                 CASE( IA_GLY )
                     KHETERO( IRATE, C,R,L ) = 60.0D0 * (  KGLY( 2 ) ) ! min-1

                 CASE( IA_MGLY )
                     KHETERO( IRATE, C,R,L ) = 60.0D0 * (  KMGLY( 2 ) ) ! min-1
                 
                 CASE( IA_HO2 )
                     KHETERO( IRATE, C,R,L ) = 60.0D0 * (  KHO2( 2 ) ) ! min-1

                 CASE( IA_NTR2 )
C *** calculate gas-particle partitioning of NTR2
                    CON_TOA = SUM( SUM( AEROSPC_CONC( :,1:2 ),2 ), 
     &                        MASK = ( AEROSPC( : )%OM .AND. .NOT. AEROSPC( : )%TRACER ) )
                    
                    F_PART_NTR2 = 0.0
                    IF ( CON_TOA .GE. MIN_TOA ) THEN
                       F_PART_NTR2 = ON_ALPHA1 / ( 1.0 + ON_CSTAR1 / CON_TOA )
     &                             + ON_ALPHA2 / ( 1.0 + ON_CSTAR2 / CON_TOA )
                    END IF

C *** calculate pseudo-first order rate constant for heterogeneous NTR2
C hydrolysis
                     IF ( AIRRH .LE. 0.2D0 .OR. F_PART_NTR2 .LE. 0.0 ) THEN
                        KHETERO( IRATE, C, R, L ) = 0.0D0
                     ELSE IF ( AIRRH .GE. 0.4D0 ) THEN
                        KHETERO( IRATE, C, R, L ) = REAL( F_PART_NTR2 * KHON, 8 ) ! [1/min]
                     ELSE
                        KHETERO( IRATE, C, R, L ) = REAL( F_PART_NTR2 * KHON, 8 ) ! [1/min]
     &                                            * ( 5.0D0 * AIRRH - 1.0D0 )
                     END IF

                CASE( IA_CLNO3_WAI )
                  KHETERO( IRATE, C,R,L ) = 60.0D0 * WET_M2( 1 ) * DPI / ( RADIUS_I * INV_DIFF_CLNO3 + CLNO3_H2O_RXN_TIME )

                CASE( IA_CLNO3_WAJ )   
                  KHETERO( IRATE, C,R,L ) = 60.0D0 * WET_M2( 2 ) * DPI / ( RADIUS * INV_DIFF_CLNO3 + CLNO3_H2O_RXN_TIME )  

                 CASE( IA_HBR_BRJ )
                  KHETERO( IRATE, C,R,L ) = 60.0D0 * SSALT_SURFA / ( RADIUS * INV_DIFF_HBR + HBR_RXN_TIME )  ! use sea-salt surface area

                 CASE( IA_BRNO3_WAI )
                  KHETERO( IRATE, C,R,L ) = 60.0D0 * WET_M2( 1 ) * DPI / ( RADIUS_I * INV_DIFF_BRNO3 + BRNO3_H2O_RXN_TIME )

                 CASE( IA_BRNO3_WAJ )
                  KHETERO( IRATE, C,R,L ) = 60.0D0 * WET_M2( 2 ) * DPI / ( RADIUS * INV_DIFF_BRNO3 + BRNO3_H2O_RXN_TIME )

                 CASE( IA_HOBR_CLJ )
                  K1 = 0.35 * SSALT_SURFA / ( RADIUS * INV_DIFF_HOBR + HOBR_ASS_RXN_TIME ) ! use sea-salt surface area
                  IF ( CL_PPM (2) .GT. MIN_VALUE ) THEN
                   KHETERO( IRATE, C,R,L ) = 60.0D0 * K1 / CL_PPM (2) 
                  ELSE
                   KHETERO( IRATE, C,R,L ) = 0.0D0    
                  ENDIF

                 CASE( IA_HOBR_BRJ )
                  K1 = 0.65 * SSALT_SURFA / ( RADIUS * INV_DIFF_HOBR + HOBR_ASS_RXN_TIME )       ! use sea-salt surface area
                  IF ( BR_PPM (2) .GT. MIN_VALUE ) THEN
                   KHETERO( IRATE, C,R,L ) = 60.0D0 * K1 / BR_PPM (2)
                  ELSE
                   KHETERO( IRATE, C,R,L ) = 0.0D0    
                  ENDIF

                 CASE( IA_BRNO3_CLJ )
                  K1 = 0.35 * SSALT_SURFA / ( RADIUS * INV_DIFF_BRNO3 + BRNO3_RXN_TIME )         ! use sea-salt surface area
                  IF ( CL_PPM (2) .GT. MIN_VALUE ) THEN
                   KHETERO( IRATE, C,R,L ) = 60.0D0 * K1 / CL_PPM (2)                                         ! 1/(min*ppm)
                  ELSE
                   KHETERO( IRATE, C,R,L ) = 0.0D0
                  ENDIF

                 CASE( IA_BRNO3_BRJ )
                  K1 = 0.65 * SSALT_SURFA / ( RADIUS * INV_DIFF_BRNO3 + BRNO3_RXN_TIME )         ! use sea-salt surface area
                  IF ( BR_PPM (2) .GT. MIN_VALUE ) THEN
                   KHETERO( IRATE, C,R,L ) = 60.0D0 * K1 / BR_PPM (2)                                         ! 1/(min*ppm)
                  ELSE
                   KHETERO( IRATE, C,R,L ) = 0.0D0
                  ENDIF

                 CASE( IA_BRNO2_CLJ )
                  K1 =  0.35 * SSALT_SURFA / ( RADIUS * INV_DIFF_BRNO2 + BRNO2_RXN_TIME )        ! use sea-salt surface area
                  IF ( CL_PPM (2) .GT. MIN_VALUE ) THEN
                   KHETERO( IRATE, C,R,L ) = 60.0D0 * K1 / CL_PPM (2)                                         ! 1/(min*ppm)
                  ELSE
                    KHETERO( IRATE, C,R,L ) = 0.0D0
                  ENDIF

                 CASE( IA_BRNO2_BRJ )
                  K1 =  0.65 * SSALT_SURFA / ( RADIUS * INV_DIFF_BRNO2 + BRNO2_RXN_TIME )       ! use sea-salt surface area
                  IF ( BR_PPM (2) .GT. MIN_VALUE ) THEN
                   KHETERO( IRATE, C,R,L ) = 60.0D0 * K1 / BR_PPM (2)                                         ! 1/(min*ppm)
                  ELSE
                    KHETERO( IRATE, C,R,L ) = 0.0D0
                  ENDIF

                 CASE( IA_I2O2_AI )
                  KHETERO( IRATE, C,R,L ) = 60.0D0 * WET_M2( 1 ) * DPI / ( RADIUS_I * INV_DIFF_I2O2 + I2O2_RXN_TIME )

                 CASE( IA_I2O2_AJ )
                  KHETERO( IRATE, C,R,L ) = 60.0D0 * WET_M2( 2 ) * DPI / ( RADIUS * INV_DIFF_I2O2 + I2O2_RXN_TIME ) 

                 CASE( IA_I2O3_AI )
                  KHETERO( IRATE, C,R,L ) = 60.0D0 * WET_M2( 1 ) * DPI / ( RADIUS_I * INV_DIFF_I2O3 + I2O3_RXN_TIME )

                 CASE( IA_I2O3_AJ ) 
                  KHETERO( IRATE, C,R,L ) = 60.0D0 * WET_M2( 2 ) * DPI / ( RADIUS * INV_DIFF_I2O3 + I2O3_RXN_TIME )

                 CASE( IA_I2O4_AI )
                  KHETERO( IRATE, C,R,L ) = 60.0D0 * WET_M2( 1 ) * DPI / ( RADIUS_I * INV_DIFF_I2O4 + I2O4_RXN_TIME )

                 CASE( IA_I2O4_AJ )
                  KHETERO( IRATE, C,R,L ) = 60.0D0 * WET_M2( 2 ) * DPI / ( RADIUS * INV_DIFF_I2O4 + I2O4_RXN_TIME ) 

                 CASE( IA_INO3_CLJ )          
                  K1 = 0.5 * SSALT_SURFA / ( RADIUS * INV_DIFF_INO3 + INO3_RXN_TIME )        ! use sea-salt surface area
                  IF ( CL_PPM (2) .GT. MIN_VALUE ) THEN
                   KHETERO( IRATE, C,R,L ) = 60.0D0 * K1 / CL_PPM (2)                                         ! 1/(min*ppm)
                  ELSE
                   KHETERO( IRATE, C,R,L ) = 0.0D0    
                  ENDIF

                 CASE( IA_INO3_BRJ )
                  K1 = 0.5 * SSALT_SURFA / ( RADIUS * INV_DIFF_INO3 + INO3_RXN_TIME )        ! use sea-salt surface area
                  IF ( BR_PPM (2) .GT. MIN_VALUE ) THEN
                   KHETERO( IRATE, C,R,L ) = 60.0D0 * K1 / BR_PPM (2)                                         ! 1/(min*ppm)
                  ELSE
                   KHETERO( IRATE, C,R,L ) = 0.0D0    
                  ENDIF

                 CASE( IA_INO2_CLJ )
                  K1 =  0.5 * SSALT_SURFA / ( RADIUS * INV_DIFF_INO2 + INO2_RXN_TIME )       ! use sea-salt surface area
                  IF ( CL_PPM (2) .GT. MIN_VALUE ) THEN
                   KHETERO( IRATE, C,R,L ) = 60.0D0 * K1 / CL_PPM (2)                                         ! 1/(min*ppm)
                  ELSE
                   KHETERO( IRATE, C,R,L ) = 0.0D0    
                  ENDIF

                 CASE( IA_INO2_BRJ )
                  K1 =  0.5 * SSALT_SURFA / ( RADIUS * INV_DIFF_INO2 + INO2_RXN_TIME )        ! use sea-salt surface area
                  IF ( BR_PPM (2) .GT. MIN_VALUE ) THEN
                   KHETERO( IRATE, C,R,L ) = 60.0D0 * K1 / BR_PPM (2)                                         ! 1/(min*ppm)
                  ELSE
                   KHETERO( IRATE, C,R,L ) = 0.0D0    
                  ENDIF

                 CASE( IA_HOI_CLJ )                            
                  K1 = 0.5 * SSALT_SURFA / ( RADIUS * INV_DIFF_HOI + HOI_RXN_TIME )           ! use sea-salt surface area
                  IF ( CL_PPM (2) .GT. MIN_VALUE ) THEN
                  KHETERO( IRATE, C,R,L ) = 60.0D0 * K1 / CL_PPM (2)                                         ! 1/(min*ppm)
                  ELSE
                   KHETERO( IRATE, C,R,L ) = 0.0D0    
                  ENDIF 

                 CASE( IA_HOI_BRJ )                            
                  K1 = 0.5 * SSALT_SURFA / ( RADIUS * INV_DIFF_HOI + HOI_RXN_TIME )           ! use sea-salt surface area
                  IF ( BR_PPM (2) .GT. MIN_VALUE ) THEN
                   KHETERO( IRATE, C,R,L ) = 60.0D0 * K1 / BR_PPM (2)                                         ! 1/(min*ppm)
                  ELSE
                   KHETERO( IRATE, C,R,L ) = 0.0D0    
                  ENDIF

               END SELECT

            END DO LOOP_RATES

#ifdef verbose_aerosol_chemistry
            IF ( DUMP_CELL ) THEN

               WRITE( LOGDEV,* ) 'In GAS HETCHEM for MYPE = ', MYPE
               WRITE( LOGDEV,9499 ) C,R,L
               WRITE( LOGDEV,9500 ) 'TEMP        = ', AIRTEMP
               WRITE( LOGDEV,9500 ) 'AIRRH       = ', AIRRH
               WRITE( LOGDEV,9500 ) 'DIFFUSIVITY = ', 1.0 / INV_DIFFUSIVITY
               WRITE( LOGDEV,9500 ) 'INV_SQRT_MWN2O5 = ', INV_SQRT_MWN2O5
               WRITE( LOGDEV,9500 ) 'CBAR_COEFF      = ', CBAR_COEFF
               WRITE( LOGDEV,9500 ) 'CBAR        = ', CBAR
               WRITE( LOGDEV,9500 ) 'PPM_FACTORA = ',
     &                UGM3_CONV_FAC * REAL( TEMP( C,R,L ) / PRESS( C,R,L ), 8 )
               WRITE( LOGDEV,9500 ) 'PPM_FACTOR = ', PPM_FACTOR
               WRITE( LOGDEV,9500 ) 'PPM_FACTOR*INV_MWCL = ', PPM_FACTOR * INV_MWCL
               WRITE( LOGDEV,9500 ) 'PPM_FACTORA*INV_MWCL = ',
     &                UGM3_CONV_FAC * REAL( TEMP( C,R,L ) / PRESS( C,R,L ), 8 ) * INV_MWCL
               WRITE( LOGDEV,9500 ) 'ACLI_PPM  = ', CL_PPM( 1 )
               WRITE( LOGDEV,9500 ) 'ACLJ_PPM  = ', CL_PPM( 2 )
               WRITE( LOGDEV,9500 ) 'ACLK_PPM  = ', CL_PPM( 3 )
               WRITE( LOGDEV,9500 ) 'ANH4I = ', aerospc_conc( ANH4_IDX,1 ) 
               WRITE( LOGDEV,9500 ) 'ANO3I = ', aerospc_conc( ANO3_IDX,1 ) 
               WRITE( LOGDEV,9500 ) 'ASO4I = ', aerospc_conc( ASO4_IDX,1 ) 
               WRITE( LOGDEV,9500 ) 'ACLI  = ', aerospc_conc( ACL_IDX,1 ) 
               WRITE( LOGDEV,9500 ) 'AH2OI = ', aerospc_conc( AH2O_IDX,1 ) 
               WRITE( LOGDEV,9500 ) 'ANH4J = ', aerospc_conc( ANH4_IDX,2 ) 
               WRITE( LOGDEV,9500 ) 'ANO3J = ', aerospc_conc( ANO3_IDX,2 ) 
               WRITE( LOGDEV,9500 ) 'ASO4J = ', aerospc_conc( ASO4_IDX,2 ) 
               WRITE( LOGDEV,9500 ) 'ACLJ  = ', aerospc_conc( ACL_IDX,2) 
               WRITE( LOGDEV,9500 ) 'AH2OJ = ', aerospc_conc( AH2O_IDX,2 ) 
               WRITE( LOGDEV,9500 ) 'ANH4IJ = ',
     &                aerospc_conc( ANH4_IDX,1 ) + aerospc_conc( ANH4_IDX,2 ) 
               WRITE( LOGDEV,9500 ) 'ANO3IJ = ',
     &                aerospc_conc( ANO3_IDX,1 ) + aerospc_conc( ANO3_IDX,2 ) 
               WRITE( LOGDEV,9500 ) 'ASO4IJ = ',
     &                aerospc_conc( ASO4_IDX,1 ) + aerospc_conc( ASO4_IDX,2 ) 
               WRITE( LOGDEV,9500 ) 'ACLIJ  = ',
     &                aerospc_conc( ACL_IDX,1 ) + aerospc_conc( ACL_IDX,2 ) 
               WRITE( LOGDEV,9500 ) 'AH2OIJ = ',
     &                aerospc_conc( AH2O_IDX,1 ) + aerospc_conc( AH2O_IDX,2 ) 
               WRITE( LOGDEV,9500 ) 'GAMMA = ', N2O5_GAMMA( AIRTEMP, AIRRH, 0 )
               WRITE( LOGDEV,9500 ) 'ANH4K = ', aerospc_conc( ANH4_IDX,3 ) 
               WRITE( LOGDEV,9500 ) 'ANO3K = ', aerospc_conc( ANO3_IDX,3 ) 
               WRITE( LOGDEV,9500 ) 'ASO4K = ', aerospc_conc( ASO4_IDX,3 ) 
               WRITE( LOGDEV,9500 ) 'ACLK  = ', aerospc_conc( ACL_IDX, 3 ) 
               WRITE( LOGDEV,9500 ) 'AH2OK = ', aerospc_conc( AH2O_IDX,3 ) 
               WRITE( LOGDEV,9500 ) 'GAMMAK     = ', N2O5_GAMMA( AIRTEMP, AIRRH, 5 )
               WRITE( LOGDEV,9500 ) 'XXF_AT     = ', XXF( 1 )
               WRITE( LOGDEV,9500 ) 'XXF_AC     = ', XXF( 2 )
               WRITE( LOGDEV,9500 ) 'XXF_COR    = ', XXF( 3 )
               WRITE( LOGDEV,'( A )' ) 'Product/Nucleophile mapping for epox uptake:'
               DO N = 1, N_NUCPAIRS
                  WRITE( LOGDEV,'( 5X,A,I4 )' ) ACID_NUC_PAIRS( N )%PROD, ACID_PRODMAP_IDX( N )
                  WRITE( LOGDEV,'( 5X,A,I4 )' ) ACID_NUC_PAIRS( N )%NUC, ACID_NUCMAP_IDX( N )
               END DO
               WRITE( LOGDEV,9500 ) 'GAMMAIEPOX  = ', GAMMAIEPOX
               WRITE( LOGDEV,9500 ) 'GAMMAIMAE   = ', GAMMAIMAE
               WRITE( LOGDEV,9500 ) 'k_particle_iepox =       ', KPIEPOX
               WRITE( LOGDEV,9500 ) 'Fraction tetrol =        ', FH2O( 1 )
               WRITE( LOGDEV,9500 ) 'Fraction iepoxos =       ', FOS(  1 )
               WRITE( LOGDEV,9500 ) 'Fraction tetrol dimer =  ', FDIM1( 1 )
               WRITE( LOGDEV,9500 ) 'Fraction iepoxos dimer = ', FDIM2( 1 )
               WRITE( LOGDEV,9500 ) 'Fraction 2-MG =          ', FH2O( 2 )
               WRITE( LOGDEV,9500 ) 'Fraction mae-os =        ', FOS( 2 )
               WRITE( LOGDEV,9500 ) 'Fraction mae dimers = 0  ', FDIM1( 2 ) + FDIM2( 2 )
               WRITE( LOGDEV,9500 ) 'TETROL_PPM =             ', TETROL_PPM
               WRITE( LOGDEV,9500 ) 'IEPOXOS_PPM =            ', IEPOXOS_PPM
               WRITE( LOGDEV,9501 ) 'KN2O5IJ = ', 60.0D0 * ( KN2O5( 1 ) + KN2O5( 2 ) ),' 1/min'
               WRITE( LOGDEV,9501 ) 'KN2O5K =  ', 60.0D0 * KN2O5(3), ' 1/min'
               WRITE( LOGDEV,9501 ) 'KNO2  =   ', 60.0D0 * KNO2, ' 1/min'
               WRITE( LOGDEV,9501 ) 'YIELDI =  ', YIELD_CLNO2( 1 ), ' '
               WRITE( LOGDEV,9501 ) 'YIELDJ =  ', YIELD_CLNO2( 2 ), ' '
               WRITE( LOGDEV,9501 ) 'YIELDIJ = ', YIELDIJ, ' '
               WRITE( LOGDEV,9501 ) 'YIELDK =  ', YIELD_CLNO2( 3 ), ' '
               WRITE( LOGDEV,9501 ) 'F_PART_NTR2 =  ', F_PART_NTR2, ' '
               WRITE( LOGDEV,9501 ) 'GAMMA_NO3= ', GAMMA_NO3, ' '
               WRITE( LOGDEV,9501 ) 'GAMMA_GLY= ', GAMMA_GLY, ' '
               WRITE( LOGDEV,9501 ) 'GAMMA_HO2= ', GAMMA_HO2, ' '
               WRITE( LOGDEV,9501 ) 'KNO3(1)= ', KNO3( 1 ), ' '
               WRITE( LOGDEV,9501 ) 'KNO3(2)= ', KNO3( 2 ), ' '
               WRITE( LOGDEV,9501 ) 'KNO3(3)= ', KNO3( 3 ), ' '
               WRITE( LOGDEV,9501 ) 'KGLY(2)= ', KGLY( 2 ), ' '
               WRITE( LOGDEV,9501 ) 'KMGLY(2)= ', KMGLY( 2 ), ' '
               WRITE( LOGDEV,9501 ) 'KHO2(2)= ', KHO2( 2 ), ' '
               DO N = 1, NHETERO
                  WRITE( LOGDEV,9501 ) HETERO( N ) // ' = ', KHETERO( N, C,R,L ),' '
               END DO
            END IF          
9499        FORMAT( 'At COL = ',I3,' ROW = ',I3,' LAY = ',I3 )
9500        FORMAT( A, ES12.4 )
9501        FORMAT( A, ES12.4, A )          
#endif
        
            END DO LOOP_COL
         END DO  LOOP_ROW
      END DO LOOP_LAY

      RETURN

      END SUBROUTINE HETCHEM_RATES

C ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      REAL( 8 ) FUNCTION PNCOM_LOSS( POC, PNCOM )
! Calculates rate constant in cm3 molec-1 sec-1 for PNCOM loss by reacting 
! with OH, derived from the reactions in the below POA aging scheme 
! in CMAQ v5.0-v5.02:
! start with the definition of POCRm 
!      - POCRm = reduced primary organic carbon (molar concentration)
!  changes POCRm correspond to changes in PNCOM via a rate constant
!  koheff*[OH] that comes from Weitkamp et al (2008) and George et al (2007)
!
! POCRm = POC/12 - Omoles
!  where Omoles represent fraction that is already PNCOM oxidized
! Omoles = NCOM/(16 + PHOrat) if 14/12 < (POC + NCOM)/POC <  44/12 
!      - see Simon et al (2011) for derivation
! Omoles = NCOM/16            if (POC + NCOM)/POC >= 44/12 
!      - interpretted as POC is fully oxidized and all 
!        NCOM is oxygen 
! Omoles = 0                  if (POC + NCOM)/POC <= 14/12 
!      - if OM/OC < 1.167, then POC is fully reduced and all 
!        NCOM is hydrogen
!      - see Simon et al. (2011) for derivation
! note:we divide POC by 12 b/c we want moles of carbon atoms not moles 
!      of POC (since each carbon atom w\in the molecule is allowed 
!      to react)
! note: we calculate Omoles based on the equations above derived from 
!      Heald et al (2010) and documented in Simon et al (2011).
!
! The following rate equation comes from the reaction above:
!      dPOCRm/dt = POCRm*koheff*[OH] 
!        -assume that [OH] does not change as a result of this reaction
!      solve for POCRm at time, t
!        moles: POCRm(t) = POCRm(0)*EXP(-koheff*[OH]*t) = POCRm(0)*expdt
!
!      One mole of "NCOMm" is formed for every mole of POCRm that reacts:
!             -dPOCRm/dt = dNCOMm/det
!        moles of NCOMm: NCOMm = NCOMm(0) + POCRm(0)*[1-EXP(-koheff*[OH]*t)] = 
!                        NCOMm(0) + POCRm(0)*(1-expdt)
!        NCOMg = NCOMm*15.0 
!          - every mole of newly formed NCOM results in an average gain of 1 oxygen atom 
!            and an average loss of 1 hydrogen atoms (on average, two oxidation steps 
!            convert a CH3 functional group into a COOH functional group) 
!               -(based on Heald et al. (2010)) 
!      Rewrite NCOM formation equation using grams: 
!        NCOMg/15.0 = NCOMg(0)/15.0 + POCRm*(1-expdt) 
!                         V 
!        NCOMg == NCOM(0) + 15.0*POCRm*(1-expdt)
!                          or
!        dNCOMg/dt = (15.0/12.0)*POC*koheff*[OH]-15.0*Omoles*PNCOM*koheff*[OH]
!            ----> 15.0*Omoles*koheff is rate constant of PNCOM loss
! 
! Key Subroutines Called: none
!
! Key Functions Called: none
!
! Revision History:
!    June 2014:  Bill Hutzell-initial version based on the poaaging subroutine
!                in CMAQ v5.0 thru v5.0.2.
! 
!  REFERENCES:
!   1. George, I.J., Vlasenko, A., Slowik, J.G., Broekhuizen, K., 
!      Abbatt, J.P.D. (2007), Heterogeneous oxidation of saturated
!      organic aerosols by hydroxyl radicals: uptake kinetics, 
!      condesned-phase products, and particle size change, 
!      Atmospheric Chemistry and Physics, 7, 4187-4201 
!     
!   2. Heald, C.L., Kroll, J.H., Jimenez, J.L., Docherty, K.S., 
!      DeCarlo, P.F., Aiken, A.C., Chen, Q., Martin, S.T., Farmer, S.T.,
!      Artaxo, P. (2010), A simplified description of the evolution of 
!      organic aerosol composition in the atmosphere, GRL, 37, L08803. 
!
!   3. Simon, H. and Bhave, P.V. (2011), Simulating the degree of oxidation 
!      in atmospheric organic particles  
!      In Review at ES&T.
!
!   4. Weitkamp, E.A., Lambe, A.T., Donahue, N.M., Robinson, A.L. (2008),
!      Laboratory measurements of the heterogeneous oxidation of condensed-
!      phase organic molecular markers for motor vehicl exhaust, 42, 
!      7950-7956.
!-----------------------------------------------------------------------
      
      IMPLICIT NONE
      
! Arguments: Note specific units do not material as long as consistent
!           between arguments, i.e., the same
       REAL, INTENT( IN ) :: POC    ! concentration of primary organic carbon
       REAL, INTENT( IN ) :: PNCOM  ! concentration of primart noncarbon organic matter
! Parameter:
       REAL( 8 ), PARAMETER :: KOHEFF      =  2.5D-12  ! POC + OH rate constant, cm3 molec-1 sec-1
       REAL,      PARAMETER :: LOWER_LIMIT =  7.0/6.0
       REAL,      PARAMETER :: UPPER_LIMIT = 11.0/3.0
       REAL( 8 ), PARAMETER :: MAX_RATE    = 15.0D0/16.0D0 * KOHEFF ! max rate constant for PNCOM loss
! Local    
       REAL( 8 )            :: RATIO   ! the hydrogen to oxygen ratio in organic matter
       
        PNCOM_LOSS = 0.0D0

        IF (  POC .LE. 1.0D-30 ) RETURN
              
        RATIO = REAL( ( POC + PNCOM ) / POC, 8 )

        IF ( RATIO .GT. LOWER_LIMIT .AND. RATIO .LT. UPPER_LIMIT ) THEN
           PNCOM_LOSS = 15.0D0 * KOHEFF
     &                / ( 16.0D0 + (UPPER_LIMIT - RATIO) / (RATIO - LOWER_LIMIT) )
        ELSE IF ( RATIO .GE. UPPER_LIMIT ) THEN 
           PNCOM_LOSS = MAX_RATE
        END IF
        
        RETURN   
      END FUNCTION PNCOM_LOSS

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      REAL( 8 ) FUNCTION N2O5_GAMMA( TEMP, RH, GPARAM )

C  Calculates the N2O5 heterogeneous reaction probability, which is the
C  fraction of collisions between a gaseous N2O5 molecule and a particle
C  surface that leads to nitrate production.  In the literature, this
C  probability is commonly referred to with the Greek letter, GAMMA.  To
C  avoid conflicts with the intrinsic GAMMA function on some compilers,
C  we refer to the reaction probability as N2O5_GAMMA in this function.

C  A variety of parameterizations of N2O5_GAMMA are available in this
C  function.  Users may select among the different parameterizations
C  by changing the input argument, GPARAM.  This argument may take on
C  the following values (see code for further details):
C     1. Constant value of 0.1 based on Dentener & Crutzen (1993)
C     2. Function of particle SO4 and NO3, based on Riemer et al. (2003)
C     3. Function of RH, Temp, and particle composition, based on a
C        combination of parameterizations by Evans & Jacob (2005) and
C        Riemer et al. (2003)
C  If GPARAM matches none of the above values, the default calculation
C  of N2O5_GAMMA is a function of RH, T, particle composition, and phase
C  state, based on the parameterization by Davis et al. (2008).

C  Key Subroutines Called: none

C  Key Functions Called: CRYSTALIZED, FREEZES

C  Revision History:
C    First version was coded in November 2007 by Dr. Prakash Bhave
C    using excerpts of the HETCHEM subroutine, which contained only
C    one option for computing N2O5_GAMMA (i.e., GPARAM = 3).
C
C  PVB 11/03/07 Removed code that sets N2O5_GAMMA to zero when RH < 1%.
C
C  PVB 11/05/07 Corrected GPARAM = 3 option to fix the typographical
C               error in the paper by Evans & Jacob (2005), which was
C               found by Dr. Jerry Davis.
C
C  PVB 04/11/08 Updated formulas for LAM1 & LAM2 based on revised paper
C               by Davis et al. (2008).  Added APNDX flag so users may
C               switch between base parameterization and the alternative
C               discussed in Appendix A by Davis et al.  Set default
C               parameterization to match equations in Appendix A.
C               Reduced all regression coefficients by one decimal place
C               for consistency with revised paper.
C
C  JTK 04/17/08 Moved molar mass to AERO_INFO.f
C
C  SH  12/08/09 Use new Fortran modules (aero_data, met_data) in lieu of
C               CBLK array and AERO_INFO module
C
C  SH  03/10/11 Renamed met_data to aeromet_data

C  GS  09/12/14 Added an opiton for N2O5 heterogeneous reaction probability
C               based on Bertram and Thornton (2009) (fine and coarse mode)
 
C References:
C   1. Dentener, F.J. and P.J. Crutzen, Reaction of N2O5 on tropospheric
C      aerosols: Impact of global distributions of NOx, O3, and OH.
C      J. Geophys. Res., Vol 98, 7149-7163, 1993.
C
C   2. Riemer, N., H. Vogel, B. Vogel, B. Schell, I. Ackermann, C.
C      Kessler, and H. Hass, Impact of the heterogeneous hydrolysis
C      of N2O5 on chemistry of nitrate aerosol formation in the lower
C      troposphere under photosmog conditions.  J. Geophys. Res., Vol
C      108, No D4, 4144, doi:10.1029/2002JD002436, 2003.
C
C   3. Evans, M.J. and D.J. Jacob, Impact of new laboratory studies of
C      N2O5 hydrolysis on global model budgets of tropospheric nitrogen
C      oxides, ozone, and OH.  Geophys. Res. Lett., 32, L09813,
C      doi:10.1029/2005GL022469, 2005.
C
C   4. Davis, J.M., P.V. Bhave, and K.M. Foley, Parameterization of N2O5
C      reaction probabilities on the surface of particles containing
C      ammonium, sulfate, and nitrate.  Atmos. Chem. Phys., 2008, in
C      press.
C
C   5. Mentel, T.F., M. Sohn, and A. Wahner, Nitrate effect in the
C      heterogeneous hydrolysis of dinitrogen pentoxide on aqueous
C      aerosols.  Phys. Chem. Chem. Phys., 1, 5451-5457, 1999.
C
C   6. Bertram, T. H. and J.A. Thornton, Toward a general parameterization
C      of N2O5 reactivity on aqueous particles: the competing effects of 
C      particle liquid water, nitrate, and chloride, ACP, 9, 8351-8363, 2009. 

C-----------------------------------------------------------------------

      USE AERO_DATA
      USE AEROMET_DATA   ! Includes CONST.EXT

      IMPLICIT NONE

C *** Arguments
      REAL( 8 ),    INTENT( IN ) :: TEMP     ! Air temperature [ K ]
      REAL( 8 ),    INTENT( IN ) :: RH       ! Fractional relative humidity
      INTEGER,      INTENT( IN ) :: GPARAM   ! switch to select among
                                             !  parameterizations

C *** Parameters

C *** switch for alternative parameterization of LAM1 & LAM2
C     when APNDX = .TRUE. (default), Eqs A1-A2 are used for reaction
C     probability on aqueous sulfate particles.  Alternatively, set
C     APNDX = .FALSE. to use Eqs 4-5.
      LOGICAL, PARAMETER :: APNDX = .TRUE.

C *** Local Variables

C *** chemical species concentrations [ug/m3]
      REAL      ANH4      ! i+j mode ammonium
      REAL      ANO3      ! i+j mode nitrate
      REAL      ASO4      ! i+j mode sulfate

C *** variables for computing N2O5_GAMMA when GPARAM = 2 or 3
      REAL      FRACSO4   ! aerosol mass ratio of SO4/(SO4+NO3)
      REAL      GAMMA1    ! upper limit of rxn prob
      REAL      GAMMA2    ! lower limit of rxn prob
      REAL      ALPHA     ! RH-dependent parameter to compute GAMMA1
      REAL      BETA      ! TEMP-dependent parameter to compute GAMMA1

C *** variables for default parameterization of N2O5_GAMMA
!     LOGICAL, EXTERNAL ::   CRYSTALIZED      ! function to determine if RH is below CRH
      LOGICAL   CRYSTAL   ! true if ambient RH < CRH, false otherwise
!     LOGICAL, EXTERNAL ::   FREEZES   ! function to determine whether RH exceeds IRH
      LOGICAL   FROZEN    ! true if ambient RH > IRH, false otherwise
      REAL      NNO3      ! particle-phase nitrate [micromoles/m3]
      REAL      NSO4      ! particle-phase sulfate [micromoles/m3]
      REAL      NNH4      ! particle-phase ammonium [micromoles/m3]
      REAL      NANI      ! particle-phase anions [micromoles/m3]
      REAL      X1        ! mole fraction of ammonium bisulfate
      REAL      X2        ! mole fraction of ammonium sulfate
      REAL      X3        ! mole fraction of ammonium nitrate
      REAL      LAM1      ! logit transformation of N2O5_GAMMA on
      REAL      LAM2      !   aqueous NH4HSO4 [LAM1], aqueous (NH4)2SO4
      REAL      LAM3      !   [LAM2], aqueous NH4NO3 [LAM3], and dry
      REAL      LAMD      !   sulfate-containing particles [LAMD]
      REAL      GAM1      ! reaction probability on aqueous NH4HSO4
      REAL      GAM2      !    "          "      "     "    (NH4)2SO4
      REAL      GAM3      !    "          "      "     "    NH4NO3
      REAL      GAMD      !    "          "      " dry sulfate particles
      REAL      T293,T291 ! temperature threshold variables
      REAL      RH46      ! RH threshold variable

C *** variables for parameterization based on Bertram and Thornton (2009)

      REAL      ACLF      ! I & J mode chloride, ug/m3 
      REAL      AH2OF     ! I & J mode water, ug/m3    
      REAL      ANO3F     ! I & J mode nitrate, ug/m3  

      REAL      ACLK      ! k mode chloride, ug/m3 
      REAL      AH2OK     ! k mode water, ug/m3    
      REAL      ANO3K     ! k mode nitrate, ug/m3  

      REAL      ACLM      ! chloride, [moles / liter of particles] 
      REAL      AH2OM     ! water,    [moles / liter of particles] 
      REAL      ANO3M     ! nitrate,  [moles / liter of particles] 
   
      REAL DEN1           ! local varible
      REAL DEN2           ! local varible
      REAL DEN            ! local varible
      REAL INV_VOL        ! local variable 
      
      REAL, PARAMETER :: A = 3.2E-08       ! Unit [s] - Table 2       
      REAL, PARAMETER :: BETA_BT = 1.15E06 ! Unit [1/s] - Table 2  
      REAL, PARAMETER :: DELTA = 1.3E-01   ! Unit [1/M] - Table 2  
      REAL, PARAMETER :: K3OK2B = 6.0E-02  ! Unit [ ] - Table 2  
      REAL, PARAMETER :: K4OK2B = 29.0     ! Unit [ ] - Table 2  
      REAL K2FP                            ! varible in eqn 10
                 
C *** statement function for inverting the logit transformation given
C     in Eq 7 by Davis et al (2008)
      REAL      LOGITINV  ! statement function
      REAL      XX        ! dummy argument for LOGITINV
      LOGITINV( XX ) = 1.0 / ( 1.0 + EXP( -XX ) )

C-----------------------------------------------------------------------

C *** retrieve fine-mode particle-phase ammonium, nitrate, and sulfate [ug/m3]
      ANH4 = aerospc_conc( ANH4_IDX,1 ) + aerospc_conc( ANH4_IDX,2 )
      ANO3 = aerospc_conc( ANO3_IDX,1 ) + aerospc_conc( ANO3_IDX,2 )
      ASO4 = aerospc_conc( ASO4_IDX,1 ) + aerospc_conc( ASO4_IDX,2 )

c *** Retrieve fine-mode particle-phase liquid water, chloride, nitrate [ug/m3]
      AH2OF  = aerospc_conc( AH2O_IDX,1 ) + aerospc_conc( AH2O_IDX,2 ) 
      ACLF   = aerospc_conc( ACL_IDX,1 )  + aerospc_conc( ACL_IDX,2 ) 
      ANO3F  = aerospc_conc( ANO3_IDX,1 ) + aerospc_conc( ANO3_IDX,2 )

c *** Retrieve coarse-mode particle-phase liquid water, chloride, nitrate [ug/m3]
      AH2OK  = aerospc_conc( AH2O_IDX,3 )
      ACLK   = aerospc_conc(  ACL_IDX,3 )
      ANO3K  = aerospc_conc( ANO3_IDX,3 )      

      WHAT_OPTION: SELECT CASE( GPARAM )

         CASE ( 1 )
C *** User Option: GPARAM = 1
C     Dentener and Crutzen (1993) recommended a constant value of
C     N2O5_GAMMA = 0.1, which was used in CMAQ prior to ver4.3.  In more
C     recent literature, this value has been recognized as an upper
C     estimate of N2O5_GAMMA so it should not be used for routine
C     simulations.  It is included here only to facilitate sensitivity
C     studies by CMAQ model users.

            N2O5_GAMMA = 0.1D0
            RETURN
            
         CASE( 2, 3 )
C *** User Options: GPARAM = 2 and 3
C     These options both employ Eqs 2 and 3 by Riemer et al (2003), in
C     which N2O5_GAMMA varies according to the particle-phase sulfate and
C     nitrate concentrations.  In both options, the NO3 effect (i.e.,
C     GAMMA1/GAMMA2) is assumed to be a factor of 10 based on Mentel et
C     al (1999) and Riemer et al (2003).
C      - When GPARAM = 2, upper limit of N2O5_GAMMA is fixed at 0.02.
C        This was the default setting in CMAQ ver4.3 through ver4.5.1.
C      - When GPARAM = 3, upper limit of N2O5_GAMMA is a function of
C        ambient TEMP & RH based on the "Sulfate" equation in Table 1
C        by Evans & Jacob (2005).  This was the default setting in CMAQ
C        ver4.6.  After that release, a typographical error was found
C        in the published equation of Evans & Jacob (2005) so this code
C        has been corrected accordingly.

            IF ( GPARAM .EQ. 2 ) THEN
            
                GAMMA1 = 0.02

            ELSE 
C        In this function, RH is in fractional units whereas the
C        published equation by Evans&Jacob refers to RH as a percentage.

               ALPHA = 2.79E-04
     &               + REAL( RH * ( 1.3D-02 + RH * ( -3.43D-02 + 7.52D-02 * RH ) ), 4 )

C        To fix the typographical error by Evans & Jacob (2005), the
C        sign of BETA has been switched in this code.

               IF ( TEMP .LT. 282.0D0 ) THEN
                  GAMMA1 = 3.0199517 * ALPHA   ! (10.0 ** 0.48) * ALPHA
               ELSE
                  BETA  = 0.04 * ( 294.0 - REAL( TEMP,4 ) )
                  GAMMA1 = ALPHA * ( 10.0 ** BETA )
               END IF
            END IF

            IF ( ANO3 .GT. 0.0 ) THEN
               FRACSO4 = ASO4 / ( ASO4 + ANO3 )
            ELSE
               FRACSO4 = 1.0
            END IF

            GAMMA2 = 0.1 * GAMMA1
            N2O5_GAMMA = REAL( ( GAMMA2 + FRACSO4 * ( GAMMA1 - GAMMA2 ) ), 8 )
   
            RETURN

         CASE ( 4 )

C *** User Option: GPARAM = 4  (fine-mode particles)
C *** It calculates N2O5_GAMMA based on Bertran and Thornton, 2009
  
            INV_VOL = 1.0E-9 * f6pi / REAL( ( WET_M3( 1 ) + WET_M3( 2 ) ), 4 )
        
            AH2OM = AH2OF * INV_VOL / aerospc_mw( AH2O_IDX )
            ACLM  = ACLF  * INV_VOL / aerospc_mw( ACL_IDX )
            ANO3M = ANO3F * INV_VOL / aerospc_mw( ANO3_IDX )
            
            K2FP =  BETA_BT - BETA_BT * EXP( -DELTA * AH2OM )
          
            DEN1 = K3OK2B * ( AH2OM / ANO3M )

            DEN2 = K4OK2B * ( ACLM  / ANO3M  )

            DEN  = DEN1 + 1.0 + DEN2
         
            N2O5_GAMMA = REAL( A * K2FP * (1.0 - 1.0 / DEN), 8 )
                   
            RETURN

         CASE ( 5 )
   
C *** User Option: GPARAM = 5 (coarse-mode particles)
C     It calculates N2O5_GAMMA based on Bertran and Thornton, 2009

            INV_VOL = 1.0E-9 * f6pi / REAL( WET_M3( 3 ), 4 )
        
            AH2OM = AH2OK * INV_VOL / aerospc_mw( AH2O_IDX )
            ACLM  = ACLK  * INV_VOL / aerospc_mw( ACL_IDX )
            ANO3M = ANO3K * INV_VOL / aerospc_mw( ANO3_IDX )
            
            K2FP =  BETA_BT - BETA_BT * EXP( -DELTA * AH2OM )
          
            DEN1 = K3OK2B * ( AH2OM / ANO3M )

            DEN2 = K4OK2B * ( ACLM  / ANO3M )

            DEN  = DEN1 + 1.0 + DEN2      

            N2O5_GAMMA = REAL( A * K2FP * (1.0 - 1.0 / DEN), 8)
                                                         
            RETURN
    
!           IF ( DUMP_CELL ) THEN
!              WRITE(*,*)'AH2OM = ',AH2OM
!              WRITE(*,*)'ACLM  = ',ACLM
!              WRITE(*,*)'ANO3M = ',ANO3M
!              WRITE(*,*)'DEN   = ',DEN
!              WRITE(*,*)'DEN1  = ',DEN1
!              WRITE(*,*)'DEN2  = ',DEN2
!              WRITE(*,*)'K2FP  = ',K2FP
!              WRITE(*,*)'GAMMAK = ',A * K2FP * (1.0 - 1.0 / DEN)
!           END IF
                                
!           RETURN
             
         CASE DEFAULT
      
C *** Default setting in current version of CMAQ:
C     This code implements the paramaterization given in Eq 15 by Davis
C     et al (2008), in which N2O5_GAMMA is a function of RH, TEMP,
C     particle composition, and phase state.  Note: In this function, RH
C     is in fractional units whereas the published equations refer to RH
C     as a percentage.

C *** Check whether the ambient RH is below the crystallization RH for
C     the given inorganic particle composition.
            CRYSTAL = CRYSTALIZED( RH, .TRUE. )

C *** Check whether the ambient RH exceeds the RH of ice formation.
            FROZEN = FREEZES( TEMP, RH )
      
            IF ( FROZEN ) THEN
C *** Set N2O5_GAMMA to constant value if particles contain ice, based on
C     Eq 14 by Davis et al (2008).

               N2O5_GAMMA = 0.02D0                               ! Eq 14

            ELSE
C *** Compute mole-fractional-composition of particles based on Eq 11 by
C     Davis et al (2008).
               NNO3 = ANO3 / aerospc_mw( ANO3_IDX )
               NSO4 = ASO4 / aerospc_mw( ASO4_IDX )
               NNH4 = ANH4 / aerospc_mw( ANH4_IDX )
               NANI = NNO3 + NSO4
         
               X3 = NNO3 / NANI
               X2 = MAX( 0.0, MIN( 1.0 - X3, NNH4/NANI - 1.0 ) )
               X1 = 1.0 - ( X2 + X3 )

C *** Compute N2O5_GAMMA on pure NH4NO3 particles using Eqs 6 and 8 by
C     Davis et al (2008).
               LAM3 = -8.10774 + 4.902 * REAL( RH, 4 )            ! Eq 6
               GAM3 = MIN( LOGITINV( LAM3 ), 0.0154 )             ! Eq 8

C *** Compute N2O5_GAMMA on dry particles using Eqs 9, 10, and 13 by
C     Davis et al (2008).
               IF ( CRYSTAL ) THEN
                  T293     = MAX( 0.0, REAL( TEMP ) - 293.0 )
                  LAMD     = -6.13376 + 3.592 * REAL( RH, 4 )     ! Eq 9
     &                     - 0.19688 * T293
                  GAMD     = MIN( LOGITINV( LAMD ), 0.0124 )      ! Eq 10
                  N2O5_GAMMA = REAL( ( X1 + X2 ) * GAMD           ! Eq 13
     &                       +         X3 * MIN( GAMD, GAM3 ), 8)

C *** Compute N2O5_GAMMA on aqeuous particles using Eqs A1, A2, 8, and 12
C     by Davis et al (2008).  When APNDX = .TRUE. (default), Eqs A1-A2
C     are used for reaction probability on aqueous sulfate particles.
C     Switch to .FALSE. if Eqs 4-5 are desired.  See Appendix A by
C     Davis et al. (2008) for a discussion of these options.
               ELSE
                  T291 = MAX( 0.0, REAL( TEMP ) - 291.0 )
                  IF ( APNDX ) THEN
                     RH46  = MIN( 0.0, REAL( RH )- 0.46 )
                     LAM2  = -3.64849 + 9.553 * RH46          ! Eq A2
                     LAM1  = LAM2 + 0.97579                   ! Eqs A1 & A2
     &                     - 0.20427 * T291
                  ELSE
                     LAM1  = -4.10612 + 2.386 * REAL( RH )    ! Eq 4
     &                     - 0.23771 * T291
                     LAM2  = LAM1 - 0.80570                   ! Eqs 4 & 5
     &                     + 0.10225 * T291
                  END IF

                  GAM1     = MIN( LOGITINV( LAM1 ), 0.08585 ) ! Eq 8
                  GAM2     = MIN( LOGITINV( LAM2 ), 0.053 )   ! Eq 9
                  N2O5_GAMMA = REAL( ( X1 * GAM1 )            ! Eq 12
     &                       +       ( X2 * GAM2 )
     &                       +       ( X3 * GAM3 ), 8 )

               END IF
            END IF

      END SELECT WHAT_OPTION


      RETURN

      END FUNCTION N2O5_GAMMA

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      LOGICAL FUNCTION CRYSTALIZED( RH, COMPLETE )

C  Determines whether the ambient RH is below the crystallization relative
C  humidity (CRH).  The output of this logical function is .TRUE. when the
C  ambient RH is below the CRH and .FALSE. otherwise.  The empirical
C  equations developed by Martin et al (2003) are applied to determine the
C  CRH for a given mixture of sulfate, nitrate, and ammonium.  Though those
C  equations are validated only at 293K, they are applied at all ambient
C  temperatures because insufficient data exist to estimate the temperature
C  dependence of the CRH of mixed sulfate-nitrate-ammonium particles.
C  Users can opt to compute either the RH of initial crystal formation
C (i.e., COMPLETE .EQ. .FALSE.) or the RH of compete crystallization (i.e.,
C  COMPLETE .EQ. .TRUE.).

C  References:
C   1. Martin, S.T., J.C. Schlenker, A. Malinowski, H.-M. Hung, and
C      Y. Rudich, Crystallization of atmospheric sulfate-nitrate-
C      ammonium particles.  Geophys. Res. Lett., 30(21), 2102,
C      doi:10.1029/2003GL017930, 2003.

C  Revision History:
C    PVB 11/05/07 Coded the first version.
C    JTK 04/17/08 Moved molar mass to AERO_INFO.f
C    SH  12/08/09 Use aero_data module in lieu of CBLK array

C-----------------------------------------------------------------------

      USE AERO_DATA

      IMPLICIT NONE

C *** Arguments
      REAL( 8 ), INTENT( IN )    :: RH        ! fractional relative humidity
      LOGICAL,   INTENT( IN )    :: COMPLETE  ! flag deciding which CRH
                                              !  equation to use

C *** Local Variables

C *** chemical species concentrations [micromoles/m3]
      REAL      NSO4     ! i+j mode sulfate
      REAL      NNO3     ! i+j mode nitrate
      REAL      NNH4     ! i+j mode ammonium
      REAL      NCAT     ! i+j mode cations
      REAL      NANI     ! i+j mode anions

C *** cation and anion mole fractions used in CRH equations
      REAL X         ! ammonium/cation mole fraction: NH4/(NH4+H)
      REAL Y         ! sulfate/anion mole fraction:  SO4/(SO4+NO3)

C *** intermediate variables used in CRH equations
      REAL X2, XY, Y2, X2Y, XY2, RDEN

      REAL CRH       ! crystallization RH (fractional units)

C-----------------------------------------------------------------------

C *** Experimental measurements of CRH are lacking below 1% relative
C     humidity.  Under those very dry conditions, we assume that
C     particles will crystallize.  Equations by Martin et al (2003) for
C     internally-mixed sulfate-nitrate-ammonium particles yield maximum
C     CRH values of 35.03% and 34.50% for initial crystal formation and
C     complete crystallization, respectively.  Therefore, the full CRH
C     calculation can be avoided when RH > 35.1%.
      IF ( RH .LE. 0.01D0 ) THEN
         CRYSTALIZED = .TRUE.    ! ambient particles are dry
         RETURN
      ELSE IF ( RH .GT. 0.351D0 ) THEN
         CRYSTALIZED = .FALSE.   ! ambient RH exceeds CRH
         RETURN
      END IF

C *** calculate total particle-phase composition [micromoles/m3]
      NNO3 = ( aerospc_conc( ANO3_IDX,1 ) + aerospc_conc( ANO3_IDX,2 ) )
     &     / aerospc_mw( ANO3_IDX )
      NSO4 = ( aerospc_conc( ASO4_IDX,1 ) + aerospc_conc( ASO4_IDX,2 ) )
     &     / aerospc_mw( ASO4_IDX )
      NNH4 = ( aerospc_conc( ANH4_IDX,1 ) + aerospc_conc( ANH4_IDX,2 ) )
     &     / aerospc_mw( ANH4_IDX )

C *** calculate total anion and cation concentrations
      NCAT = MAX( NNH4, 2.0 * NSO4 + NNO3 )
      NANI = NNO3 + NSO4

C *** calculate ammonium and sulfate mole fractions
      X = NNH4 / NCAT
      Y = NSO4 / NANI

C *** Experimental data of Martin et al. (2003) show no crystal
C     formation when X < 0.50 or Y < 0.22.  For these particle
C     compositions, the full CRH calculation can be avoided.
C
C     Note: Martin`s equation for initial crystal formation returns
C     very large CRH values when X and Y approach zero.  However,
C     those values were verified to be erroneous by personal
C     communication with Dr. Scot Martin on Aug. 30, 2007.
      IF ( ( X .LT. 0.50 ) .OR. ( Y .LT. 0.22 ) ) THEN
         CRYSTALIZED = .FALSE.   ! ambient RH exceeds CRH
         RETURN
      END IF

C *** store some terms needed to evaluate the CRH equations
      X2   = X * X
      XY   = X * Y
      X2Y  = X2 * Y
      Y2   = Y * Y
      XY2  = X * Y2
      RDEN = 1.0 / ( 25.0 + ( X - 0.7 ) * ( Y - 0.5 ) )

C *** calculate CRH using empirical equations of Martin et al (2003)
      IF ( COMPLETE ) THEN

         CRH = 3143.44 + (63.07 * X) + (0.114 * X2) + (87.97 * Y)
     &       - (125.73 * XY) + (0.586 * X2Y) + (0.95 * Y2)
     &       - (1.384 * XY2) - (79692.5 * RDEN)

      ELSE

         CRH = -697.908 - (15.351 * X) + (0.43 * X2) - (22.11 * Y)
     &       + (33.882 * XY) - (1.818 * X2Y) + (0.772 * Y2)
     &       - (1.636 * XY2) + (17707.6 * RDEN)

      END IF

C *** set value of the output variable, CRYSTALIZED
      IF ( RH .LE. REAL( CRH, 8 ) ) THEN
         CRYSTALIZED = .TRUE.    ! ambient particles are dry
      ELSE
         CRYSTALIZED = .FALSE.   ! ambient RH exceeds CRH
      END IF

      RETURN

      END FUNCTION CRYSTALIZED

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      LOGICAL FUNCTION FREEZES( TEMP, RH )

C  Determines whether the ambient RH has exceeded the RH of ice formation,
C  based on the Goff-Gratch equations as given by List (1984).

C  References:
C   1. Goff, J.A. and S. Gratch, Low-pressure properties of water from
C      -160 to 212 F, in Transactions of the American Society of Heating
C      and Ventilating Engineers, pp 95-122, New York, 1946.
C   2. List, R.J. (editor), Smithsonian Meteorological Tables, 5th ed.
C      pp. 350, 1984.

C  Revision History:
C   PVB 11/06/07 Coded the first version.
C-----------------------------------------------------------------------

      IMPLICIT NONE

C *** Arguments
      REAL( 8 ), INTENT( IN ) :: TEMP        ! Air temperature [ K ]
      REAL( 8 ), INTENT( IN ) :: RH          ! Fractional relative humidity

C *** Parameters

C *** The following values are taken from List (1984).  Note that
C     these differ slightly from the original equations published by
C     Goff & Gratch (1946).  We also note that T0 and PST differ
C     slightly from STDTEMP and STDATMPA in the AERO_INFO module.
C     Here, we use 273.16 K and 1013.246 hPa to be consistent with
C     the Goff-Gratch equations as given by List (1984).
      REAL( 8 ), PARAMETER :: TST = 373.16D0    ! steam-point temperature, K
      REAL( 8 ), PARAMETER :: T0  = 273.16D0    ! ice-point temperature, K
      REAL( 8 ), PARAMETER :: PST = 1013.246D0  ! sat vapor pres at TST, hPa
      REAL( 8 ), PARAMETER :: P0  = 6.1071D0    ! sat vapor pres at T0, hPa

      REAL( 8 ), PARAMETER :: LOGPST = 3.0057149D0     ! LOG10(PST)
      REAL( 8 ), PARAMETER :: LOGP0  = 0.78583503D0    ! LOG10(P0)

C *** Local Variables

C *** estimates of IRH using a polynomial approximation
      REAL( 8 )      EIRH   ! IRH approximated using 2nd order polynomial
      REAL( 8 )      LIRH   ! lower-bound of IRH
      REAL( 8 )      UIRH   ! upper-bound of IRH

C *** variables used to compute RH of ice formation
      REAL( 8 )      TSDT, TDTS, T0DT, TDT0  ! intermediate variables
      REAL( 8 )      LOGPW  ! log10 of saturation vapor pressure over H2O
      REAL( 8 )      LOGPI  ! log10 of saturation vapor pressure over ice

      REAL( 8 )      IRH    ! fractional RH at which ice forms

C-----------------------------------------------------------------------

      IF ( TEMP .LT. T0 ) THEN

C *** To mitigate the computational expense associated with Goff-Gratch
C     equations, use a 2nd order polynomial function to approximate IRH.
C     This approximation, EIRH, matches IRH from the full Goff-Gratch
C     equations within 0.004 over the entire low-temperature range of
C     interest (200 to 275K) and is used for screening purposes.
         EIRH = 1.61299D0 + TEMP * ( 4.4117437D-5 * TEMP - 1.4293888D-2 )
         LIRH = EIRH - 0.005D0
         UIRH = EIRH + 0.005D0

         IF ( RH .GT. UIRH ) THEN
            FREEZES = .TRUE.
         ELSE IF ( RH .LT. LIRH ) THEN
            FREEZES = .FALSE.
         ELSE

C *** Compute IRH using Goff-Gratch equations as given by List (1984)
            TSDT  = TST / TEMP
            TDTS  = TEMP / TST
            T0DT  = T0 / TEMP
            TDT0  = TEMP / T0
            LOGPW = -7.90298D0 * ( TSDT - 1.0D0 )
     &            + 5.02808D0 * LOG10( TSDT )
     &            - 1.3816D-7 * ( 10.0D0 ** ( 11.344D0 *  (1.0D0 - TDTS) ) - 1.0D0 )
     &            + 8.1328D-3 * ( 10.0D0 ** ( -3.49149D0 * (TSDT - 1.0D0) ) - 1.0D0 )
     &            + LOGPST
            LOGPI = -9.09718D0 * (T0DT - 1.0D0)
     &            - 3.56654D0 * LOG10( T0DT )
     &            + .876793D0 * (1.0D0 - TDT0)
     &            + LOGP0
            IRH   = 10.0D0 ** ( LOGPI - LOGPW )

            IF ( RH .GT. IRH ) THEN
               FREEZES = .TRUE.
            ELSE
               FREEZES = .FALSE.
            END IF

         END IF
      ELSE
         FREEZES = .FALSE.
      END IF

      RETURN

      END FUNCTION FREEZES

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE CALCISOPGAMMAS( RADIUS, TEMPERATURE,
     &                           GAMMAIEPOX, GAMMAIMAE,
     &                           KPIEPOX, fTET, fOS, fdim1, fdim2 )

C  Calculates the uptake coefficients (gammas) for IEPOX, IMAE, and IHMML
C  based on particle phase H+ (EQLBH) and other particle constituents.
C  Also returns calculated particle phase reaction rate, fraction of uptake
C  producing tetrols, organosulfates, and dimers.

C  Note that indices 1 and 2 are used for both acids (1=H+, 2=bisulfate) and
C  the organics (1=IEPOX, 2=IMAE).

C  Key Subroutines Called: none

C  Key Functions Called: none

C  Revision History: 
C  HOTP 1/18/13 First coding
C  HOTP 9/26/14 Reduced form considering only IEPOX uptake with no
C               speciation developed for CB05
C  HOTP 5/10/16 Merged with AERO6I code. Now handles SAPRC07tic and MAE uptake.

C  References
C  1. Chan, M. N., et al. "Characterization and quantification of isoprene-
C     derived expoxydiols in ambient aerosol in the Southeastern United States,"
C     Environ. Sci. Technol., 2010, 44, 4590-4596.
C  2. Eddingsaas, N. C., VanderVelde, D. G., Wennberg, P. O. "Kinetics 
C     and products of the acid-catalyzed ring-opening of atmospherically
C     relevant butyl epoxy alcohols," J. Phys. Chem A., 2010, 114, 8106-8113.
C  3. Gharagheizi, F., et al. "Representation and prediction of molecular diffusivity
C     of nonelectrolyte organic compounds in water at infinite dilution using the
C     artificial neural network-group contribution method," J. Chemical & Engineering
C     Data, 2011, 46, 1741-1750.
C  4. Hanson, D. R., Ravishankara, A. R., Solomon, S. "Heterogeneous reactions in
C     sulfuric acid aerosols: A framework for model calculations," J. Geophys. Res.,
C     1994, 99, 3615-3629.
C  5. McNeill, V. F., et al. "Aqueous-phase secondary organic aerosol and organosulfate 
C     formation in atmospheric aerosols: A modeling study," Environ Sci Technol., 2012,
C     46, 8075-8081.
C  6. Pye et al., Epoxide pathways improve model predictions of isoprene
C     markers and reveal key role of acidity in aerosol formation,
C     Envion. Sci. Technol, 2013, doi: 10.1021/es402106h. 
C-----------------------------------------------------------------------

      Use aero_data
      Use precursor_data
      Use aeromet_data   ! Includes CONST.EXT (pi) and airtemp
      Use utilio_defn    ! error message and abort

      Implicit None

C *** Arguments
      Real( 8 ), Intent( IN )  :: RADIUS       ! Particle radius [m]
      Real( 8 ), Intent( IN )  :: TEMPERATURE  ! air temperature [deg K]
      Real( 8 ), Intent( OUT ) :: GAMMAIEPOX ! Uptake coeff for IEPOX []
      Real( 8 ), Intent( OUT ) :: GAMMAIMAE  ! Uptake coeff for MAE []
      Real( 8 ), Intent( OUT ) :: KPIEPOX ! Particle phase reaction rate for IEPOX [1/s]
      Real( 8 ), Intent( OUT ) :: fTET(2), fOS(2), fdim1(2), fdim2(2)
                                  ! fraction of tetrol, organosulfate, tetroldimer, osdimer []
                                  ! first value for IEPOX-derived, second for IMAE-derived
C *** Parameters
C     For gamma calculation
      Real( 8 ), Parameter :: inv_alpha    = 1.0d0/0.02d0 ! reciproaccomodation coefficient from McNeill et al. 2012 []
      Real( 8 ), Parameter :: diffusivity  = 1.0d-9    ! liquid phase diffusivity, based on C4-C5 epoxides 
                                                       ! and diols at infinite dilution in water (Gharagheizi et al. 2011) [m2/s]
      Real( 8 ), Parameter :: RgasLatmmolK = 0.08206d0 ! Universal gas constant [L atm/mol K]
      Real( 8 ), Parameter :: small        = 1.0d-8   ! small number to prevent calculations that may have precision issues
      Real( 8 ), Parameter :: smaller      = 1.0d-20  ! smaller number to prevent division by zero

C     Acid related
      Integer, Parameter   :: n_acids      = 2 ! number of acids: H+ and HSO4-
      Integer, Parameter   :: hacid_idx    = 1 ! H+ acid index
      Integer, Parameter   :: hso4acid_idx = 2 ! HSO4 acid index

C     Henry's law coeff for IEPOX (1) and IMAE (2) (HenryWin v 3.2)
      Real( 8 ) :: Heff(2)

C     Diagnostic
      Character (16 ), Parameter :: pname = 'CALCISOPGAMMAS'

C *** Local Variables
      Integer   :: i                   ! counter and temporary indices
      Real( 8 ) :: eqlbh               ! Particle Phase H+ from isoropia [mol/m**3]
      Real( 8 ) :: q, denom            ! quantities in Hanson et al. 1994 equation for gamma []
      Real( 8 ) :: nu                  ! molecular speed [m/s], note this is the same as CBAR but we want to avoid excessive passing
      Real( 8 ) :: acidmolconc( n_acids ) ! concentration of acid in mol/m3
      Real( 8 ) :: acid                ! temporary concentration of acid in mol/m3
      Real( 8 ) :: nuc                 ! nucleophile concentration in mol/m3
      Real( 8 ) :: charge              ! temporary for charge balance calculation [umol/m3]
      Real( 8 ) :: inv_volume          ! reciprocal of particle volume [m3 air/Liters particles]
      Real( 8 ) :: kchem, kparticle(2) 
      Real( 8 ) :: kchemtet(2),kchemos(2), kchemdim1(2), kchemdim2(2) ! particle phase rxn rates [1/s] 
      Real( 8 ) :: gammaisop(2)        ! temporary value for IEPOX (1) and IMAE (2) gammas
      Real( 8 ) :: cfactor_isop(2)     ! cfactor for IEPOX(1) and MAE(2)
      Real( 4 ) :: temp                ! air temperature [deg K]
      Character ( 80 ) :: xmsg         ! error message

C *** External functions
      Interface
         Real Function hlconst ( cname, temp, effective, hplus )
           Use utilio_defn
           Implicit None
           Character*(*), Intent ( In ) :: cname
           Real,          Intent ( In ) :: temp
           Logical,       Intent ( In ) :: effective
           Real,          Intent ( In ) :: hplus
        End Function hlconst
      End Interface

C-----------------------------------------------------------------------

C *** Determine H+ acid concentration of j mode [mol/m3]
      eqlbh = aerospc_conc( ah3op_idx, 2 ) / aerospc_mw( ah3op_idx ) * 1.0d-6
      acidmolconc( hacid_idx ) = Max( eqlbh, 0.0d0 ) 

C *** Calculate HSO4 by charge balance
C     Note: if isorropia has returned a negative H+ concentration
C     we effectively treat all H+ like HSO4 (slightly less effective
C     in catalyzing the ring-opening)
      charge = 0.0d0  ! umol/m3
      Do i = 1, n_aerospc
         If ( aerospc( i )%tracer ) Cycle
         If ( aero_missing( i,2 ) ) Cycle
         charge = charge - aerospc( i )%charge * aerospc_conc( i,2 )
     &          / aerospc_mw( i )
      End Do
      acidmolconc( hso4acid_idx ) = Max( ( charge*1.0d-6 - acidmolconc( hacid_idx ) ), 0.0d0 ) 

C *** J-mode particle volume [L/m**3 air]
      inv_volume = 0.0d0
      Do i = 1, n_aerospc 
         If ( aerospc( i )%tracer ) Cycle
         If ( aero_missing( i,2 ) ) Cycle
!        If ( aerospc( i )%no_M2Wet ) Cycle
         inv_volume = inv_volume + Real( aerospc_conc( i, 2 ) / aerospc(i)%density, 8 ) !  * 1.0d-6
      End Do
      inv_volume = 1.0d6 / inv_volume

C *** Loop over nucleophile/acid pairs defined in acid_nuc_pairs in AERO_DATA
C     and calculate rate of particle phase reaction
      kchem     = 0.0d0   ! intermediate value
      kparticle = 0.0d0   ! accumulating IEPOX and IMAE value
      kchemos   = 0.0d0   ! accumulating organosulfate for IEPOX and IMAE
      kchemtet  = 0.0d0   ! accumulating tetrols or equivalent for IEPOX and IMAE
      kchemdim1 = 0.0d0   ! accumulating tetrol-dimer or equivalent for IEPOX and IMAE
      kchemdim2 = 0.0d0   ! accumulating os-dimer or equivalent for IEPOX and IMAE

      Do i = 1, n_nucpairs

C        Find J-mode nucleophile concentration in ug/m3 and convert to mol/m3
         nuc = aerospc_conc( acid_nucmap_idx( i ), 2 ) 
     &       /  aerospc_mw( acid_nucmap_idx( i ) ) * 1.0d-6

         acid = acidmolconc( acid_nuc_pairs( i )%idx_acid ) ! mol/m3

C        Correct sulfate for presence of bisulfate
         If ( acid_nuc_pairs( i )%idx_remove .Ge. 1 ) Then
            nuc = Max( nuc - acidmolconc( acid_nuc_pairs( i )%idx_remove ), 0.0d0 )
         End If

C        Determine rate of particle phase rxn for a given pair (Eddingsaas et al.)
         kchem = acid_nuc_pairs( i )%kchem* nuc * acid * ( inv_volume*inv_volume )
         
C        Update sum
C        Store information with speciation for IEPOX
         if ( acid_nuc_pairs( i )%parent .eq. 'IEPOX' ) then
            kparticle( 1 )  = kparticle(1) + kchem
            if ( acid_nuc_pairs( i )%prod .eq. 'AIEOSJ' ) then !ae6i
              kchemos( 1 )  = kchemos(1)   + kchem
            else if ( (acid_nuc_pairs( i )%prod .eq. 'ADIMJ')
     &             .and. (acid_nuc_pairs( i )%nuc .eq. 'AIETETJ') ) then !ae6i
              kchemdim1( 1 ) = kchemdim1(1)  + kchem
            else if ( (acid_nuc_pairs( i )%prod .eq. 'ADIMJ')
     &             .and. (acid_nuc_pairs( i )%nuc .eq. 'AIEOSJ') ) then !ae6i
              kchemdim2( 1 ) = kchemdim2(1)  + kchem
            else if ( acid_nuc_pairs( i )%prod .eq. 'AIETETJ' ) then !ae6i
              kchemtet( 1 ) = kchemtet(1)  + kchem
            else if ( acid_nuc_pairs( i )%prod .eq. 'AISO3OSJ' ) then !craccm1
              kchemos( 1 )  = kchemos(1)   + kchem
            else if ( acid_nuc_pairs( i )%prod .eq. 'AISO3NOSJ' ) then !craccm1
              kchemtet( 1 )  = kchemtet(1)   + kchem
            else if ( acid_nuc_pairs( i )%prod .eq. 'AISO3J' ) then !lumped treatments
               if ( acid_nuc_pairs( i )%nuc .eq. 'ASO4J' ) then !organosulfate
                 kchemos( 1 )  = kchemos(1)   + kchem
               else ! non sulfated species
                 kchemtet( 1 ) = kchemtet(1)  + kchem
               end if
            else
               xmsg = 'For IEPOX parent, reaction product not recognized '
               CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
            end if
         end if

C        Store information with speciation for IMAE
         if ( acid_nuc_pairs( i )%parent .eq. 'IMAE' ) then
            kparticle( 2 )  = kparticle( 2 ) + kchem
            if( acid_nuc_pairs( i )%prod .eq. 'AIMOSJ' ) then
              kchemos( 2 )  = kchemos( 2 )   + kchem
            else if( acid_nuc_pairs( i )%prod .eq. 'AIMGAJ' ) then
              kchemtet( 2 ) = kchemtet( 2 )  + kchem
            else
               xmsg = 'For IMAE parent, reaction product not recognized '
               CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
            end if
         end if
      End Do      

C *** Calculate gamma following Pye et al. 2013 ES&T
C     (see also Hanson et al. 1994 JGR Eqn (2) for details)
C *** Calculate gammas for IEPOX and IMAE

C *** Henry's Law coefficients
      temp = real( TEMPERATURE, 4)
      Heff( 1 ) = REAL(HLCONST( 'IEPOX', temp, .False., 0.0 ), 8)
      Heff( 2 ) = REAL(HLCONST( 'IMAE',  temp, .False., 0.0 ), 8)

C *** Cfactor
      cfactor_isop( 1 ) = cfactor_iepox
      cfactor_isop( 2 ) = cfactor_imae

C     Loop over precursor species and calculate gamma
C     (see Hanson et al. 1994 JGR Eqn (2) for details)
      Do  i = 1, numvoc

C     Note that if the rate of particle phase reaction is very slow, there may not
C     be enough digits of precision to properly calculate q which can result in
C     erroneously high gamma values. Only calculate gamma if particle phase reaction
C     is faster than 1e-8 1/s

      ! Diffuso reactive parameter [ m * sqrt ( 1/s s/m2 ) = dim'less ]
      q = radius * dsqrt ( kparticle(i)/ diffusivity )

      If ( q / Tanh( q ) .Gt. 1.0d0 ) Then

         ! Molecular speed [ sqrt ( J/mol K * K / g * mol * g/ kg ) = sqrt (J/kg) = m/s ]
         nu = cfactor_isop( i ) * dsqrt( temperature )

         ! Gamma [ denom:  m/s / ( mol/L/atm * L atm /mol/ K * K * sqrt ( m2/s * 1/s )) = dim'less ]

          denom = ( inv_alpha + nu * radius
     &          / ( 4.0d0 * Heff( i )* RgasLatmmolK * temperature * diffusivity
     &            * ( q / Tanh( q ) -1.0d0 ) ) ) 

          gammaisop( i ) = 1.0d0 / denom

          ftet( i ) = kchemtet( i ) / kparticle( i )
          fos( i )  = kchemos( i ) / kparticle( i )
          fdim1( i ) = kchemdim1( i ) / kparticle( i )
          fdim2( i ) = kchemdim2( i ) / kparticle( i )

C     No/slow reaction
      Else

         gammaisop( i ) = 0.0d0
         ftet( i )  = 0d0
         fos( i )   = 0d0
         fdim1( i ) = 0d0
         fdim2( i ) = 0d0

      End If

      End Do

C *** Calculate final values to return
      gammaiepox = gammaisop( 1 )
      gammaimae  = gammaisop( 2 )
      kpiepox    = kparticle( 1 )

      RETURN
 
      END SUBROUTINE CALCISOPGAMMAS

      END MODULE AEROSOL_CHEMISTRY
