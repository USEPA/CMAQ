
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


      SUBROUTINE HRINIT
C***********************************************************************
C
C  FUNCTION: To initialize species tolerances, arrays, and indices
C
C  PRECONDITIONS: For the cb6r3 family of mechanisms
C
C  RETURN VALUES: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Aug 29, 2018
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


C.....SAVED VARIABLES:
      CHARACTER( 16 ), SAVE  ::  PNAME = 'HRINIT'   ! Program name


C.....LOCAL VARIABLES:
      CHARACTER( 132 ) ::  MSG       ! Log message

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

       ALLOCATE( ATMPRES( MAX_CELLS_EBI ),
     &           H2O    ( MAX_CELLS_EBI ),
     &           TEMP   ( MAX_CELLS_EBI ),
     &           HET    ( MAX_CELLS_EBI, NHETERO ),
     &           RJIN   ( MAX_CELLS_EBI, NPHOTAB ),
     &           LAND   ( MAX_CELLS_EBI ) ,
     &           STAT = IOS )

       IF ( IOS .NE. 0 ) THEN
          MSG = 'Error allocating ATMPRES, H2O, TEMP, HET, RJIN, LAND '
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

! initialize all reaction rates to zero so unused array elements are defined

       RXRAT = 0.0D0

! initialize all production and loss rates to zero so unused array elements are defined

       PROD  = 0.0D0
       LOSS  = 0.0D0

       NING1 = 4
       NING2 = 4

       ALLOCATE( YCCELL( MODEL_SPECIES), STAT = IOS )  ! array for degradation routines
       IF ( IOS .NE. 0 ) THEN
          MSG = 'Error allocating YCCELL '
          CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
       END IF


c..Allocate and define ebi species

      N_EBISP  = 137
      ALLOCATE( EBISP( N_EBISP ), STAT = IOS ) 
      IF ( IOS .NE. 0 ) THEN
         MSG = 'Error allocating EBISP' 
         CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      EBISP(   1 ) = H2O2
      EBISP(   2 ) = HNO3
      EBISP(   3 ) = SO2
      EBISP(   4 ) = SULF
      EBISP(   5 ) = SULRXN
      EBISP(   6 ) = MEO2
      EBISP(   7 ) = RO2
      EBISP(   8 ) = PACD
      EBISP(   9 ) = AACD
      EBISP(  10 ) = CXO3
      EBISP(  11 ) = ALD2
      EBISP(  12 ) = XO2H
      EBISP(  13 ) = PANX
      EBISP(  14 ) = FORM
      EBISP(  15 ) = MEPX
      EBISP(  16 ) = MEOH
      EBISP(  17 ) = ROOH
      EBISP(  18 ) = XO2
      EBISP(  19 ) = XO2N
      EBISP(  20 ) = NTR1
      EBISP(  21 ) = NTR2
      EBISP(  22 ) = FACD
      EBISP(  23 ) = CO
      EBISP(  24 ) = HCO3
      EBISP(  25 ) = ALDX
      EBISP(  26 ) = GLYD
      EBISP(  27 ) = GLY
      EBISP(  28 ) = MGLY
      EBISP(  29 ) = ETHA
      EBISP(  30 ) = ETOH
      EBISP(  31 ) = KET
      EBISP(  32 ) = PAR
      EBISP(  33 ) = ACET
      EBISP(  34 ) = PRPA
      EBISP(  35 ) = XPRP
      EBISP(  36 ) = XPAR
      EBISP(  37 ) = ROR
      EBISP(  38 ) = ETHY
      EBISP(  39 ) = ETH
      EBISP(  40 ) = OLE
      EBISP(  41 ) = IOLE
      EBISP(  42 ) = ISOP
      EBISP(  43 ) = ISO2
      EBISP(  44 ) = ISOPRXN
      EBISP(  45 ) = ISPD
      EBISP(  46 ) = INTR
      EBISP(  47 ) = ISPX
      EBISP(  48 ) = HPLD
      EBISP(  49 ) = OPO3
      EBISP(  50 ) = EPOX
      EBISP(  51 ) = EPX2
      EBISP(  52 ) = TERP
      EBISP(  53 ) = TRPRXN
      EBISP(  54 ) = BENZENE
      EBISP(  55 ) = CRES
      EBISP(  56 ) = BZO2
      EBISP(  57 ) = OPEN
      EBISP(  58 ) = BENZRO2
      EBISP(  59 ) = TOL
      EBISP(  60 ) = TO2
      EBISP(  61 ) = TOLRO2
      EBISP(  62 ) = XOPN
      EBISP(  63 ) = XYLMN
      EBISP(  64 ) = XLO2
      EBISP(  65 ) = XYLRO2
      EBISP(  66 ) = NAPH
      EBISP(  67 ) = PAHRO2
      EBISP(  68 ) = CRO
      EBISP(  69 ) = CAT1
      EBISP(  70 ) = CRON
      EBISP(  71 ) = OPAN
      EBISP(  72 ) = ECH4
      EBISP(  73 ) = CL2
      EBISP(  74 ) = CL
      EBISP(  75 ) = HOCL
      EBISP(  76 ) = CLO
      EBISP(  77 ) = FMCL
      EBISP(  78 ) = HCL
      EBISP(  79 ) = CLNO2
      EBISP(  80 ) = CLNO3
      EBISP(  81 ) = TOLNRXN
      EBISP(  82 ) = TOLHRXN
      EBISP(  83 ) = XYLNRXN
      EBISP(  84 ) = XYLHRXN
      EBISP(  85 ) = BNZNRXN
      EBISP(  86 ) = BNZHRXN
      EBISP(  87 ) = SESQ
      EBISP(  88 ) = SESQRXN
      EBISP(  89 ) = PAHNRXN
      EBISP(  90 ) = PAHHRXN
      EBISP(  91 ) = SOAALK
      EBISP(  92 ) = ALKRXN
      EBISP(  93 ) = H2NO3PIJ
      EBISP(  94 ) = H2NO3PK
      EBISP(  95 ) = ACLI
      EBISP(  96 ) = ACLJ
      EBISP(  97 ) = ACLK
      EBISP(  98 ) = AISO3J
      EBISP(  99 ) = AGLYJ
      EBISP( 100 ) = AXYL1J
      EBISP( 101 ) = AOLGAJ
      EBISP( 102 ) = AXYL2J
      EBISP( 103 ) = ATOL1J
      EBISP( 104 ) = ATOL2J
      EBISP( 105 ) = ABNZ1J
      EBISP( 106 ) = ABNZ2J
      EBISP( 107 ) = ATRP1J
      EBISP( 108 ) = AOLGBJ
      EBISP( 109 ) = ATRP2J
      EBISP( 110 ) = AISO1J
      EBISP( 111 ) = AISO2J
      EBISP( 112 ) = ASQTJ
      EBISP( 113 ) = APAH1J
      EBISP( 114 ) = APAH2J
      EBISP( 115 ) = AALK1J
      EBISP( 116 ) = AALK2J
      EBISP( 117 ) = PCVOC
      EBISP( 118 ) = PCSOARXN
      EBISP( 119 ) = VLVPO1
      EBISP( 120 ) = VSVPO1
      EBISP( 121 ) = VSVPO2
      EBISP( 122 ) = VSVPO3
      EBISP( 123 ) = VIVPO1
      EBISP( 124 ) = VLVOO1
      EBISP( 125 ) = VLVOO2
      EBISP( 126 ) = VSVOO2
      EBISP( 127 ) = VSVOO3
      EBISP( 128 ) = VSVOO1
      EBISP( 129 ) = FORM_PRIMARY
      EBISP( 130 ) = ALD2_PRIMARY
      EBISP( 131 ) = BUTADIENE13
      EBISP( 132 ) = ACROLEIN
      EBISP( 133 ) = ACRO_PRIMARY
      EBISP( 134 ) = TOLU
      EBISP( 135 ) = HG
      EBISP( 136 ) = HGIIAER
      EBISP( 137 ) = HGIIGAS


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set species tolerances
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      RTOL( NO2          ) = 5.0D-04
      RTOL( NO           ) = 5.0D-04
      RTOL( O            ) = 1.0D+00
      RTOL( O3           ) = 5.0D-04
      RTOL( NO3          ) = 5.0D-04
      RTOL( O1D          ) = 1.0D+00
      RTOL( OH           ) = 5.0D-04
      RTOL( HO2          ) = 5.0D-04
      RTOL( H2O2         ) = 5.0D-04
      RTOL( N2O5         ) = 5.0D-04
      RTOL( HNO3         ) = 5.0D-04
      RTOL( HONO         ) = 5.0D-04
      RTOL( PNA          ) = 5.0D-04
      RTOL( SO2          ) = 5.0D-04
      RTOL( SULF         ) = 1.0D+00
      RTOL( SULRXN       ) = 1.0D+00
      RTOL( C2O3         ) = 5.0D-04
      RTOL( MEO2         ) = 5.0D-04
      RTOL( RO2          ) = 5.0D-04
      RTOL( PAN          ) = 5.0D-04
      RTOL( PACD         ) = 5.0D-04
      RTOL( AACD         ) = 5.0D-04
      RTOL( CXO3         ) = 5.0D-04
      RTOL( ALD2         ) = 5.0D-04
      RTOL( XO2H         ) = 5.0D-04
      RTOL( PANX         ) = 5.0D-04
      RTOL( FORM         ) = 5.0D-04
      RTOL( MEPX         ) = 5.0D-04
      RTOL( MEOH         ) = 5.0D-04
      RTOL( ROOH         ) = 5.0D-04
      RTOL( XO2          ) = 5.0D-04
      RTOL( XO2N         ) = 5.0D-04
      RTOL( NTR1         ) = 5.0D-04
      RTOL( NTR2         ) = 5.0D-04
      RTOL( FACD         ) = 5.0D-04
      RTOL( CO           ) = 5.0D-04
      RTOL( HCO3         ) = 1.0D+00
      RTOL( ALDX         ) = 5.0D-04
      RTOL( GLYD         ) = 5.0D-04
      RTOL( GLY          ) = 5.0D-04
      RTOL( MGLY         ) = 5.0D-04
      RTOL( ETHA         ) = 5.0D-04
      RTOL( ETOH         ) = 5.0D-04
      RTOL( KET          ) = 5.0D-04
      RTOL( PAR          ) = 5.0D-04
      RTOL( ACET         ) = 5.0D-04
      RTOL( PRPA         ) = 5.0D-04
      RTOL( XPRP         ) = 5.0D-04
      RTOL( XPAR         ) = 5.0D-04
      RTOL( ROR          ) = 5.0D-04
      RTOL( ETHY         ) = 5.0D-04
      RTOL( ETH          ) = 5.0D-04
      RTOL( OLE          ) = 5.0D-04
      RTOL( IOLE         ) = 5.0D-04
      RTOL( ISOP         ) = 5.0D-04
      RTOL( ISO2         ) = 5.0D-04
      RTOL( ISOPRXN      ) = 1.0D+00
      RTOL( ISPD         ) = 5.0D-04
      RTOL( INTR         ) = 5.0D-04
      RTOL( ISPX         ) = 5.0D-04
      RTOL( HPLD         ) = 5.0D-04
      RTOL( OPO3         ) = 5.0D-04
      RTOL( EPOX         ) = 5.0D-04
      RTOL( EPX2         ) = 5.0D-04
      RTOL( TERP         ) = 5.0D-04
      RTOL( TRPRXN       ) = 1.0D+00
      RTOL( BENZENE      ) = 5.0D-04
      RTOL( CRES         ) = 5.0D-04
      RTOL( BZO2         ) = 5.0D-04
      RTOL( OPEN         ) = 5.0D-04
      RTOL( BENZRO2      ) = 5.0D-04
      RTOL( TOL          ) = 5.0D-04
      RTOL( TO2          ) = 5.0D-04
      RTOL( TOLRO2       ) = 5.0D-04
      RTOL( XOPN         ) = 5.0D-04
      RTOL( XYLMN        ) = 5.0D-04
      RTOL( XLO2         ) = 5.0D-04
      RTOL( XYLRO2       ) = 5.0D-04
      RTOL( NAPH         ) = 5.0D-04
      RTOL( PAHRO2       ) = 1.0D+00
      RTOL( CRO          ) = 5.0D-04
      RTOL( CAT1         ) = 5.0D-04
      RTOL( CRON         ) = 5.0D-04
      RTOL( OPAN         ) = 5.0D-04
      RTOL( ECH4         ) = 5.0D-04
      RTOL( CL2          ) = 5.0D-04
      RTOL( CL           ) = 5.0D-04
      RTOL( HOCL         ) = 5.0D-04
      RTOL( CLO          ) = 5.0D-04
      RTOL( FMCL         ) = 5.0D-04
      RTOL( HCL          ) = 5.0D-04
      RTOL( CLNO2        ) = 5.0D-04
      RTOL( CLNO3        ) = 5.0D-04
      RTOL( TOLNRXN      ) = 1.0D+00
      RTOL( TOLHRXN      ) = 1.0D+00
      RTOL( XYLNRXN      ) = 1.0D+00
      RTOL( XYLHRXN      ) = 1.0D+00
      RTOL( BNZNRXN      ) = 1.0D+00
      RTOL( BNZHRXN      ) = 1.0D+00
      RTOL( SESQ         ) = 5.0D-04
      RTOL( SESQRXN      ) = 1.0D+00
      RTOL( PAHNRXN      ) = 1.0D+00
      RTOL( PAHHRXN      ) = 1.0D+00
      RTOL( SOAALK       ) = 5.0D-04
      RTOL( ALKRXN       ) = 1.0D+00
      RTOL( H2NO3PIJ     ) = 5.0D-04
      RTOL( H2NO3PK      ) = 5.0D-04
      RTOL( ACLI         ) = 5.0D-04
      RTOL( ACLJ         ) = 5.0D-04
      RTOL( ACLK         ) = 5.0D-04
      RTOL( AISO3J       ) = 5.0D-04
      RTOL( AGLYJ        ) = 5.0D-04
      RTOL( AXYL1J       ) = 5.0D-04
      RTOL( AOLGAJ       ) = 5.0D-04
      RTOL( AXYL2J       ) = 5.0D-04
      RTOL( ATOL1J       ) = 5.0D-04
      RTOL( ATOL2J       ) = 5.0D-04
      RTOL( ABNZ1J       ) = 5.0D-04
      RTOL( ABNZ2J       ) = 5.0D-04
      RTOL( ATRP1J       ) = 5.0D-04
      RTOL( AOLGBJ       ) = 5.0D-04
      RTOL( ATRP2J       ) = 5.0D-04
      RTOL( AISO1J       ) = 5.0D-04
      RTOL( AISO2J       ) = 5.0D-04
      RTOL( ASQTJ        ) = 5.0D-04
      RTOL( APAH1J       ) = 5.0D-04
      RTOL( APAH2J       ) = 5.0D-04
      RTOL( AALK1J       ) = 5.0D-04
      RTOL( AALK2J       ) = 5.0D-04
      RTOL( PCVOC        ) = 5.0D-04
      RTOL( PCSOARXN     ) = 5.0D-04
      RTOL( VLVPO1       ) = 5.0D-04
      RTOL( VSVPO1       ) = 5.0D-04
      RTOL( VSVPO2       ) = 5.0D-04
      RTOL( VSVPO3       ) = 5.0D-04
      RTOL( VIVPO1       ) = 5.0D-04
      RTOL( VLVOO1       ) = 5.0D-04
      RTOL( VLVOO2       ) = 5.0D-04
      RTOL( VSVOO2       ) = 5.0D-04
      RTOL( VSVOO3       ) = 5.0D-04
      RTOL( VSVOO1       ) = 5.0D-04
      RTOL( FORM_PRIMARY ) = 5.0D-04
      RTOL( ALD2_PRIMARY ) = 5.0D-04
      RTOL( BUTADIENE13  ) = 5.0D-04
      RTOL( ACROLEIN     ) = 5.0D-04
      RTOL( ACRO_PRIMARY ) = 5.0D-04
      RTOL( TOLU         ) = 5.0D-04
      RTOL( HG           ) = 5.0D-04
      RTOL( HGIIAER      ) = 5.0D-04
      RTOL( HGIIGAS      ) = 5.0D-04


      RETURN

      END
