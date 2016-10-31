
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
C  PRECONDITIONS: For the CB6R3 family of mechanisms
C
C  RETURN VALUES: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Oct 31, 2016
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

      N_EBISP  = 136
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
      EBISP(  80 ) = TOLNRXN
      EBISP(  81 ) = TOLHRXN
      EBISP(  82 ) = XYLNRXN
      EBISP(  83 ) = XYLHRXN
      EBISP(  84 ) = BNZNRXN
      EBISP(  85 ) = BNZHRXN
      EBISP(  86 ) = SESQ
      EBISP(  87 ) = SESQRXN
      EBISP(  88 ) = PAHNRXN
      EBISP(  89 ) = PAHHRXN
      EBISP(  90 ) = SOAALK
      EBISP(  91 ) = ALKRXN
      EBISP(  92 ) = H2NO3PIJ
      EBISP(  93 ) = H2NO3PK
      EBISP(  94 ) = ACLI
      EBISP(  95 ) = ACLJ
      EBISP(  96 ) = ACLK
      EBISP(  97 ) = AISO3J
      EBISP(  98 ) = AGLYJ
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
      EBISP( 116 ) = PCVOC
      EBISP( 117 ) = PCSOARXN
      EBISP( 118 ) = VLVPO1
      EBISP( 119 ) = VSVPO1
      EBISP( 120 ) = VSVPO2
      EBISP( 121 ) = VSVPO3
      EBISP( 122 ) = VIVPO1
      EBISP( 123 ) = VLVOO1
      EBISP( 124 ) = VLVOO2
      EBISP( 125 ) = VSVOO2
      EBISP( 126 ) = VSVOO3
      EBISP( 127 ) = VSVOO1
      EBISP( 128 ) = FORM_PRIMARY
      EBISP( 129 ) = ALD2_PRIMARY
      EBISP( 130 ) = BUTADIENE13
      EBISP( 131 ) = ACROLEIN
      EBISP( 132 ) = ACRO_PRIMARY
      EBISP( 133 ) = TOLU
      EBISP( 134 ) = HG
      EBISP( 135 ) = HGIIAER
      EBISP( 136 ) = HGIIGAS


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set species tolerances
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      RTOL( NO2          ) = 1.0D-03
      RTOL( NO           ) = 1.0D-03
      RTOL( O            ) = 1.0D+00
      RTOL( O3           ) = 1.0D-03
      RTOL( NO3          ) = 1.0D-03
      RTOL( O1D          ) = 1.0D+00
      RTOL( OH           ) = 1.0D-03
      RTOL( HO2          ) = 1.0D-03
      RTOL( H2O2         ) = 1.0D-03
      RTOL( N2O5         ) = 1.0D-03
      RTOL( HNO3         ) = 1.0D-03
      RTOL( HONO         ) = 1.0D-03
      RTOL( PNA          ) = 1.0D-03
      RTOL( SO2          ) = 1.0D-03
      RTOL( SULF         ) = 1.0D+00
      RTOL( SULRXN       ) = 1.0D+00
      RTOL( C2O3         ) = 1.0D-03
      RTOL( MEO2         ) = 1.0D-03
      RTOL( RO2          ) = 1.0D-03
      RTOL( PAN          ) = 1.0D-03
      RTOL( PACD         ) = 1.0D-03
      RTOL( AACD         ) = 1.0D-03
      RTOL( CXO3         ) = 1.0D-03
      RTOL( ALD2         ) = 1.0D-03
      RTOL( XO2H         ) = 1.0D-03
      RTOL( PANX         ) = 1.0D-03
      RTOL( FORM         ) = 1.0D-03
      RTOL( MEPX         ) = 1.0D-03
      RTOL( MEOH         ) = 1.0D-03
      RTOL( ROOH         ) = 1.0D-03
      RTOL( XO2          ) = 1.0D-03
      RTOL( XO2N         ) = 1.0D-03
      RTOL( NTR1         ) = 1.0D-03
      RTOL( NTR2         ) = 1.0D-03
      RTOL( FACD         ) = 1.0D-03
      RTOL( CO           ) = 1.0D-03
      RTOL( HCO3         ) = 1.0D+00
      RTOL( ALDX         ) = 1.0D-03
      RTOL( GLYD         ) = 1.0D-03
      RTOL( GLY          ) = 1.0D-03
      RTOL( MGLY         ) = 1.0D-03
      RTOL( ETHA         ) = 1.0D-03
      RTOL( ETOH         ) = 1.0D-03
      RTOL( KET          ) = 1.0D-03
      RTOL( PAR          ) = 1.0D-03
      RTOL( ACET         ) = 1.0D-03
      RTOL( PRPA         ) = 1.0D-03
      RTOL( XPRP         ) = 1.0D-03
      RTOL( XPAR         ) = 1.0D-03
      RTOL( ROR          ) = 1.0D-03
      RTOL( ETHY         ) = 1.0D-03
      RTOL( ETH          ) = 1.0D-03
      RTOL( OLE          ) = 1.0D-03
      RTOL( IOLE         ) = 1.0D-03
      RTOL( ISOP         ) = 1.0D-03
      RTOL( ISO2         ) = 1.0D-03
      RTOL( ISOPRXN      ) = 1.0D+00
      RTOL( ISPD         ) = 1.0D-03
      RTOL( INTR         ) = 1.0D-03
      RTOL( ISPX         ) = 1.0D-03
      RTOL( HPLD         ) = 1.0D-03
      RTOL( OPO3         ) = 1.0D-03
      RTOL( EPOX         ) = 1.0D-03
      RTOL( EPX2         ) = 1.0D-03
      RTOL( TERP         ) = 1.0D-03
      RTOL( TRPRXN       ) = 1.0D+00
      RTOL( BENZENE      ) = 1.0D-03
      RTOL( CRES         ) = 1.0D-03
      RTOL( BZO2         ) = 1.0D-03
      RTOL( OPEN         ) = 1.0D-03
      RTOL( BENZRO2      ) = 1.0D-03
      RTOL( TOL          ) = 1.0D-03
      RTOL( TO2          ) = 1.0D-03
      RTOL( TOLRO2       ) = 1.0D-03
      RTOL( XOPN         ) = 1.0D-03
      RTOL( XYLMN        ) = 1.0D-03
      RTOL( XLO2         ) = 1.0D-03
      RTOL( XYLRO2       ) = 1.0D-03
      RTOL( NAPH         ) = 1.0D-03
      RTOL( PAHRO2       ) = 1.0D+00
      RTOL( CRO          ) = 1.0D-03
      RTOL( CAT1         ) = 1.0D-03
      RTOL( CRON         ) = 1.0D-03
      RTOL( OPAN         ) = 1.0D-03
      RTOL( ECH4         ) = 1.0D-03
      RTOL( CL2          ) = 1.0D-03
      RTOL( CL           ) = 1.0D-03
      RTOL( HOCL         ) = 1.0D-03
      RTOL( CLO          ) = 1.0D-03
      RTOL( FMCL         ) = 1.0D-03
      RTOL( HCL          ) = 1.0D-03
      RTOL( CLNO2        ) = 1.0D-03
      RTOL( TOLNRXN      ) = 1.0D+00
      RTOL( TOLHRXN      ) = 1.0D+00
      RTOL( XYLNRXN      ) = 1.0D+00
      RTOL( XYLHRXN      ) = 1.0D+00
      RTOL( BNZNRXN      ) = 1.0D+00
      RTOL( BNZHRXN      ) = 1.0D+00
      RTOL( SESQ         ) = 1.0D-03
      RTOL( SESQRXN      ) = 1.0D+00
      RTOL( PAHNRXN      ) = 1.0D+00
      RTOL( PAHHRXN      ) = 1.0D+00
      RTOL( SOAALK       ) = 1.0D-03
      RTOL( ALKRXN       ) = 1.0D+00
      RTOL( H2NO3PIJ     ) = 1.0D-03
      RTOL( H2NO3PK      ) = 1.0D-03
      RTOL( ACLI         ) = 1.0D-03
      RTOL( ACLJ         ) = 1.0D-03
      RTOL( ACLK         ) = 1.0D-03
      RTOL( AISO3J       ) = 1.0D-03
      RTOL( AGLYJ        ) = 1.0D-03
      RTOL( AXYL1J       ) = 1.0D-03
      RTOL( AOLGAJ       ) = 1.0D-03
      RTOL( AXYL2J       ) = 1.0D-03
      RTOL( ATOL1J       ) = 1.0D-03
      RTOL( ATOL2J       ) = 1.0D-03
      RTOL( ABNZ1J       ) = 1.0D-03
      RTOL( ABNZ2J       ) = 1.0D-03
      RTOL( ATRP1J       ) = 1.0D-03
      RTOL( AOLGBJ       ) = 1.0D-03
      RTOL( ATRP2J       ) = 1.0D-03
      RTOL( AISO1J       ) = 1.0D-03
      RTOL( AISO2J       ) = 1.0D-03
      RTOL( ASQTJ        ) = 1.0D-03
      RTOL( APAH1J       ) = 1.0D-03
      RTOL( APAH2J       ) = 1.0D-03
      RTOL( AALK1J       ) = 1.0D-03
      RTOL( AALK2J       ) = 1.0D-03
      RTOL( PCVOC        ) = 1.0D-03
      RTOL( PCSOARXN     ) = 1.0D-03
      RTOL( VLVPO1       ) = 1.0D-03
      RTOL( VSVPO1       ) = 1.0D-03
      RTOL( VSVPO2       ) = 1.0D-03
      RTOL( VSVPO3       ) = 1.0D-03
      RTOL( VIVPO1       ) = 1.0D-03
      RTOL( VLVOO1       ) = 1.0D-03
      RTOL( VLVOO2       ) = 1.0D-03
      RTOL( VSVOO2       ) = 1.0D-03
      RTOL( VSVOO3       ) = 1.0D-03
      RTOL( VSVOO1       ) = 1.0D-03
      RTOL( FORM_PRIMARY ) = 1.0D-03
      RTOL( ALD2_PRIMARY ) = 1.0D-03
      RTOL( BUTADIENE13  ) = 1.0D-03
      RTOL( ACROLEIN     ) = 1.0D-03
      RTOL( ACRO_PRIMARY ) = 1.0D-03
      RTOL( TOLU         ) = 1.0D-03
      RTOL( HG           ) = 1.0D-03
      RTOL( HGIIAER      ) = 1.0D-03
      RTOL( HGIIGAS      ) = 1.0D-03


      RETURN

      END
