
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

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header$

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

      SUBROUTINE HRINIT
C***********************************************************************
C
C  FUNCTION: To initialize species tolerances, arrays, and indices
C
C  PRECONDITIONS: For the CB05MP51 family of mechanisms
C
C  RETURN VALUES: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Dec. 2, 2014
C   21 Jun 10 J.Young:   convert for Namelist redesign
C   18 Jul 14 B.Hutzell: revised to use the RXNS_DATA and RXNS_FUNCTION
C                        modules instead of include files
C***********************************************************************

      USE UTILIO_DEFN
      USE HRDATA
      USE RXNS_DATA
      USE RXNS_FUNCTION

      IMPLICIT NONE

C.....INCLUDES:
      Include SUBST_CONST          ! common constants

C.....ARGUMENTS: NONE

C.....PARAMETERS: NONE


C.....EXTERNAL FUNCTIONS:
      INTEGER  FINDEX         ! Finds location of a number in a list

C.....SAVED VARIABLES:
      CHARACTER( 16 ), SAVE  ::  PNAME = 'HRINIT'   ! Program name


C.....LOCAL VARIABLES:
      CHARACTER( 132 ) ::  MSG       ! Log message

      INTEGER IND             ! Species index
      INTEGER N               ! Loop index
      INTEGER IOS             ! Allocate error status

C***********************************************************************

      N_SPEC = NUMB_MECH_SPC
      N_RXNS = NRXNS
      N_SPCL_RKS =   0
      ALLOCATE( RKI( NRXNS ),
     &          RKI_SAV( MAX_CELLS_EBI, NRXNS ),
     &          FORWARD_CONV( N_SPEC),
     &          REVERSE_CONV( N_SPEC),
     &          RXRAT( NRXNS ),
     &          RTOL( N_SPEC),
     &          YC(   N_SPEC),
     &          YC0(  N_SPEC),
     &          YCP(  N_SPEC),
     &          PROD( N_SPEC),
     &          LOSS( N_SPEC),
     &          PNEG( N_SPEC),
     &          STAT = IOS )

       IF ( IOS .NE. 0 ) THEN
          MSG = 'Error allocating RKI, RXRAT, RTOL, YC, YC0, YCP, FORWARD_CONV, '
     &       // 'REVERSE_CONV, PROD, LOSS, or PNEG'
          CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
       END IF

      IF( NSPECIAL .GT. 0 )THEN
          ALLOCATE( SYC( MAX_CELLS_EBI, N_SPEC ), STAT = IOS )
          IF ( IOS .NE. 0 ) THEN
               MSG = 'Error allocating SYC'
               CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
          END IF
          ALLOCATE( IOLD2NEW( N_SPEC, 1 ), STAT = IOS )
          IF ( IOS .NE. 0 ) THEN
               MSG = 'Error allocating IOLD2NEW'
               CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
          END IF
          DO N = 1, NUMB_MECH_SPC
             IOLD2NEW( N, 1 ) = N
          END DO
       END IF

      IF( .NOT. MAP_CHEMISTRY_SPECIES() )THEN
         MSG = 'Detected above error(s) when mapping Chemistry species from CGRID species'
         CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT2 )
      END IF

      DO N = 1, NUMB_MECH_SPC
         FORWARD_CONV( N ) = 1.0E-3 * MWAIR / SPECIES_MOLWT( N )
         REVERSE_CONV( N ) = 1.0E+3 / MWAIR * SPECIES_MOLWT( N )
      END DO


      NING1 = 4
      NING2 = 4

      ALLOCATE( YCCELL( MODEL_SPECIES), STAT = IOS )  ! array for degradation routines
      IF ( IOS .NE. 0 ) THEN
         MSG = 'Error allocating YCCELL '
         CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
      END IF


c..Allocate and define ebi species

      N_EBISP  = 133
      ALLOCATE( EBISP( N_EBISP ), STAT = IOS ) 
      IF ( IOS .NE. 0 ) THEN
         MSG = 'Error allocating EBISP' 
         CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      EBISP(   1 ) = HNO3
      EBISP(   2 ) = H2O2
      EBISP(   3 ) = XO2
      EBISP(   4 ) = XO2N
      EBISP(   5 ) = NTROH
      EBISP(   6 ) = NTRALK
      EBISP(   7 ) = ROOH
      EBISP(   8 ) = ALD2
      EBISP(   9 ) = ALDX
      EBISP(  10 ) = ISOPX
      EBISP(  11 ) = IEPOX
      EBISP(  12 ) = ISOPO2
      EBISP(  13 ) = IOLE
      EBISP(  14 ) = IEPXO2
      EBISP(  15 ) = MGLY
      EBISP(  16 ) = FORM
      EBISP(  17 ) = FACD
      EBISP(  18 ) = CO
      EBISP(  19 ) = PAR
      EBISP(  20 ) = MEO2
      EBISP(  21 ) = AACD
      EBISP(  22 ) = MEPX
      EBISP(  23 ) = MEOH
      EBISP(  24 ) = HCO3
      EBISP(  25 ) = PACD
      EBISP(  26 ) = CXO3
      EBISP(  27 ) = PANX
      EBISP(  28 ) = ROR
      EBISP(  29 ) = OLE
      EBISP(  30 ) = ETH
      EBISP(  31 ) = TOL
      EBISP(  32 ) = CRES
      EBISP(  33 ) = TO2
      EBISP(  34 ) = TOLRO2
      EBISP(  35 ) = OPEN
      EBISP(  36 ) = CRO
      EBISP(  37 ) = CAT1
      EBISP(  38 ) = CRON
      EBISP(  39 ) = CRNO
      EBISP(  40 ) = CRN2
      EBISP(  41 ) = CRPX
      EBISP(  42 ) = OPO3
      EBISP(  43 ) = CAO2
      EBISP(  44 ) = OPAN
      EBISP(  45 ) = XYLMN
      EBISP(  46 ) = XYLRO2
      EBISP(  47 ) = NAPH
      EBISP(  48 ) = PAHRO2
      EBISP(  49 ) = ISOP
      EBISP(  50 ) = ISPD
      EBISP(  51 ) = ISOPRXN
      EBISP(  52 ) = NTRM
      EBISP(  53 ) = MACO3
      EBISP(  54 ) = NTRI
      EBISP(  55 ) = TERP
      EBISP(  56 ) = TRPRXN
      EBISP(  57 ) = SO2
      EBISP(  58 ) = SULF
      EBISP(  59 ) = SULRXN
      EBISP(  60 ) = ETOH
      EBISP(  61 ) = ETHA
      EBISP(  62 ) = CL2
      EBISP(  63 ) = CL
      EBISP(  64 ) = HOCL
      EBISP(  65 ) = CLO
      EBISP(  66 ) = FMCL
      EBISP(  67 ) = HCL
      EBISP(  68 ) = CLNO2
      EBISP(  69 ) = TOLNRXN
      EBISP(  70 ) = TOLHRXN
      EBISP(  71 ) = XYLNRXN
      EBISP(  72 ) = XYLHRXN
      EBISP(  73 ) = BENZENE
      EBISP(  74 ) = BENZRO2
      EBISP(  75 ) = BNZNRXN
      EBISP(  76 ) = BNZHRXN
      EBISP(  77 ) = SESQ
      EBISP(  78 ) = SESQRXN
      EBISP(  79 ) = PAHNRXN
      EBISP(  80 ) = PAHHRXN
      EBISP(  81 ) = SOAALK
      EBISP(  82 ) = ALKRXN
      EBISP(  83 ) = MAPAN
      EBISP(  84 ) = NALKO2
      EBISP(  85 ) = NTRCN
      EBISP(  86 ) = NTRCNOH
      EBISP(  87 ) = NTRPX
      EBISP(  88 ) = NOHO2
      EBISP(  89 ) = NCNO2
      EBISP(  90 ) = NCNOHO2
      EBISP(  91 ) = NTRMO2
      EBISP(  92 ) = NTRIO2
      EBISP(  93 ) = H2NO3PIJ
      EBISP(  94 ) = H2NO3PK
      EBISP(  95 ) = ACLI
      EBISP(  96 ) = ACLJ
      EBISP(  97 ) = ACLK
      EBISP(  98 ) = AISO3J
      EBISP(  99 ) = AXYL1J
      EBISP( 100 ) = AOLGAJ
      EBISP( 101 ) = AXYL2J
      EBISP( 102 ) = ATOL1J
      EBISP( 103 ) = ATOL2J
      EBISP( 104 ) = ABNZ1J
      EBISP( 105 ) = ABNZ2J
      EBISP( 106 ) = ATRP1J
      EBISP( 107 ) = AOLGBJ
      EBISP( 108 ) = ATRP2J
      EBISP( 109 ) = AISO1J
      EBISP( 110 ) = AISO2J
      EBISP( 111 ) = ASQTJ
      EBISP( 112 ) = APAH1J
      EBISP( 113 ) = APAH2J
      EBISP( 114 ) = AALK1J
      EBISP( 115 ) = AALK2J
      EBISP( 116 ) = APOCI
      EBISP( 117 ) = APNCOMI
      EBISP( 118 ) = APOCJ
      EBISP( 119 ) = APNCOMJ
      EBISP( 120 ) = FORM_PRIMARY
      EBISP( 121 ) = ALD2_PRIMARY
      EBISP( 122 ) = BUTADIENE13
      EBISP( 123 ) = ACROLEIN
      EBISP( 124 ) = ACROLEIN_PRIMARY
      EBISP( 125 ) = TOLU
      EBISP( 126 ) = MXYL
      EBISP( 127 ) = OXYL
      EBISP( 128 ) = PXYL
      EBISP( 129 ) = APIN
      EBISP( 130 ) = BPIN
      EBISP( 131 ) = HG
      EBISP( 132 ) = HGIIAER
      EBISP( 133 ) = HGIIGAS


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set species tolerances
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      RTOL( NO2              ) = 1.0D-03
      RTOL( NO               ) = 1.0D-03
      RTOL( O                ) = 1.0D+00
      RTOL( O3               ) = 1.0D-03
      RTOL( NO3              ) = 1.0D-03
      RTOL( O1D              ) = 1.0D+00
      RTOL( OH               ) = 1.0D-03
      RTOL( HO2              ) = 1.0D-03
      RTOL( N2O5             ) = 1.0D-03
      RTOL( HNO3             ) = 1.0D-03
      RTOL( HONO             ) = 1.0D-03
      RTOL( PNA              ) = 1.0D-03
      RTOL( H2O2             ) = 1.0D-03
      RTOL( XO2              ) = 1.0D-03
      RTOL( XO2N             ) = 1.0D-03
      RTOL( NTROH            ) = 1.0D-03
      RTOL( NTRALK           ) = 1.0D-03
      RTOL( ROOH             ) = 1.0D-03
      RTOL( ALD2             ) = 1.0D-03
      RTOL( ALDX             ) = 1.0D-03
      RTOL( ISOPX            ) = 1.0D-03
      RTOL( IEPOX            ) = 1.0D-03
      RTOL( ISOPO2           ) = 1.0D-03
      RTOL( IOLE             ) = 1.0D-03
      RTOL( IEPXO2           ) = 1.0D-03
      RTOL( MGLY             ) = 1.0D-03
      RTOL( FORM             ) = 1.0D-03
      RTOL( FACD             ) = 1.0D-03
      RTOL( CO               ) = 1.0D-03
      RTOL( PAR              ) = 1.0D-03
      RTOL( C2O3             ) = 1.0D-03
      RTOL( MEO2             ) = 1.0D-03
      RTOL( AACD             ) = 1.0D-03
      RTOL( MEPX             ) = 1.0D-03
      RTOL( MEOH             ) = 1.0D-03
      RTOL( HCO3             ) = 1.0D+00
      RTOL( PAN              ) = 1.0D-03
      RTOL( PACD             ) = 1.0D-03
      RTOL( CXO3             ) = 1.0D-03
      RTOL( PANX             ) = 1.0D-03
      RTOL( ROR              ) = 1.0D-03
      RTOL( OLE              ) = 1.0D-03
      RTOL( ETH              ) = 1.0D-03
      RTOL( TOL              ) = 1.0D-03
      RTOL( CRES             ) = 1.0D-03
      RTOL( TO2              ) = 1.0D-03
      RTOL( TOLRO2           ) = 1.0D-03
      RTOL( OPEN             ) = 1.0D-03
      RTOL( CRO              ) = 1.0D-03
      RTOL( CAT1             ) = 1.0D-03
      RTOL( CRON             ) = 1.0D-03
      RTOL( CRNO             ) = 1.0D-03
      RTOL( CRN2             ) = 1.0D-03
      RTOL( CRPX             ) = 1.0D-03
      RTOL( OPO3             ) = 1.0D-03
      RTOL( CAO2             ) = 1.0D-03
      RTOL( OPAN             ) = 1.0D-03
      RTOL( XYLMN            ) = 1.0D-03
      RTOL( XYLRO2           ) = 1.0D-03
      RTOL( NAPH             ) = 1.0D-03
      RTOL( PAHRO2           ) = 1.0D-03
      RTOL( ISOP             ) = 1.0D-03
      RTOL( ISPD             ) = 1.0D-03
      RTOL( ISOPRXN          ) = 1.0D+00
      RTOL( NTRM             ) = 1.0D-03
      RTOL( MACO3            ) = 1.0D-03
      RTOL( NTRI             ) = 1.0D-03
      RTOL( TERP             ) = 1.0D-03
      RTOL( TRPRXN           ) = 1.0D+00
      RTOL( SO2              ) = 1.0D-03
      RTOL( SULF             ) = 1.0D+00
      RTOL( SULRXN           ) = 1.0D+00
      RTOL( ETOH             ) = 1.0D-03
      RTOL( ETHA             ) = 1.0D-03
      RTOL( CL2              ) = 1.0D-03
      RTOL( CL               ) = 1.0D-03
      RTOL( HOCL             ) = 1.0D-03
      RTOL( CLO              ) = 1.0D-03
      RTOL( FMCL             ) = 1.0D-03
      RTOL( HCL              ) = 1.0D-03
      RTOL( CLNO2            ) = 1.0D-03
      RTOL( TOLNRXN          ) = 1.0D+00
      RTOL( TOLHRXN          ) = 1.0D+00
      RTOL( XYLNRXN          ) = 1.0D+00
      RTOL( XYLHRXN          ) = 1.0D+00
      RTOL( BENZENE          ) = 1.0D-03
      RTOL( BENZRO2          ) = 1.0D-03
      RTOL( BNZNRXN          ) = 1.0D+00
      RTOL( BNZHRXN          ) = 1.0D+00
      RTOL( SESQ             ) = 1.0D-03
      RTOL( SESQRXN          ) = 1.0D+00
      RTOL( PAHNRXN          ) = 1.0D-03
      RTOL( PAHHRXN          ) = 1.0D-03
      RTOL( SOAALK           ) = 1.0D-03
      RTOL( ALKRXN           ) = 1.0D-03
      RTOL( MAPAN            ) = 1.0D-03
      RTOL( NALKO2           ) = 1.0D-03
      RTOL( NTRCN            ) = 1.0D-03
      RTOL( NTRCNOH          ) = 1.0D-03
      RTOL( NTRPX            ) = 1.0D-03
      RTOL( NOHO2            ) = 1.0D-03
      RTOL( NCNO2            ) = 1.0D-03
      RTOL( NCNOHO2          ) = 1.0D-03
      RTOL( NTRMO2           ) = 1.0D-03
      RTOL( NTRIO2           ) = 1.0D-03
      RTOL( H2NO3PIJ         ) = 1.0D-03
      RTOL( H2NO3PK          ) = 1.0D-03
      RTOL( ACLI             ) = 1.0D-03
      RTOL( ACLJ             ) = 1.0D-03
      RTOL( ACLK             ) = 1.0D-03
      RTOL( AISO3J           ) = 1.0D-03
      RTOL( AXYL1J           ) = 1.0D-03
      RTOL( AOLGAJ           ) = 1.0D-03
      RTOL( AXYL2J           ) = 1.0D-03
      RTOL( ATOL1J           ) = 1.0D-03
      RTOL( ATOL2J           ) = 1.0D-03
      RTOL( ABNZ1J           ) = 1.0D-03
      RTOL( ABNZ2J           ) = 1.0D-03
      RTOL( ATRP1J           ) = 1.0D-03
      RTOL( AOLGBJ           ) = 1.0D-03
      RTOL( ATRP2J           ) = 1.0D-03
      RTOL( AISO1J           ) = 1.0D-03
      RTOL( AISO2J           ) = 1.0D-03
      RTOL( ASQTJ            ) = 1.0D-03
      RTOL( APAH1J           ) = 1.0D-03
      RTOL( APAH2J           ) = 1.0D-03
      RTOL( AALK1J           ) = 1.0D-03
      RTOL( AALK2J           ) = 1.0D-03
      RTOL( APOCI            ) = 1.0D-03
      RTOL( APNCOMI          ) = 1.0D-03
      RTOL( APOCJ            ) = 1.0D-03
      RTOL( APNCOMJ          ) = 1.0D-03
      RTOL( FORM_PRIMARY     ) = 1.0D-03
      RTOL( ALD2_PRIMARY     ) = 1.0D-03
      RTOL( BUTADIENE13      ) = 1.0D-03
      RTOL( ACROLEIN         ) = 1.0D-03
      RTOL( ACROLEIN_PRIMARY ) = 1.0D-03
      RTOL( TOLU             ) = 1.0D-03
      RTOL( MXYL             ) = 1.0D-03
      RTOL( OXYL             ) = 1.0D-03
      RTOL( PXYL             ) = 1.0D-03
      RTOL( APIN             ) = 1.0D-03
      RTOL( BPIN             ) = 1.0D-03
      RTOL( HG               ) = 1.0D-03
      RTOL( HGIIAER          ) = 1.0D-03
      RTOL( HGIIGAS          ) = 1.0D-03


      RETURN

      END
