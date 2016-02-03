
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
C  PRECONDITIONS: For the RACM2 family of mechanisms
C
C  RETURN VALUES: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, March 4, 2013
C   21 Jun 10 J.Young: convert for Namelist redesign
C***********************************************************************

      USE CGRID_SPCS, ONLY: N_GC_SPC   ! CGRID mechanism species
      USE UTILIO_DEFN
      USE HRDATA

      IMPLICIT NONE

C.....INCLUDES:
      INCLUDE SUBST_RXCMMN    ! Mechanism reaction common block

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

      N_SPEC = N_GC_SPC
      N_RXNS = NRXNS
      N_SPCL_RKS =   0
      ALLOCATE( RKI( NRXNS ),
     &          RXRAT( NRXNS ),
     &          RTOL( N_SPEC),
     &          YC(   N_SPEC),
     &          YC0(  N_SPEC),
     &          YCP(  N_SPEC),
     &          PROD( N_SPEC),
     &          LOSS( N_SPEC),
     &          STAT = IOS )

       IF ( IOS .NE. 0 ) THEN
          MSG = 'Error allocating RKI, RXRAT, RTOL, YC, YC0, YCP, PROD, LOSS, or PNEG'
          CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
       END IF

      NING1 = 4
      NING2 = 4


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set species indices and pointers
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      O3       =   1
      O3P      =   2
      O1D      =   3
      H2O2     =   4
      HO       =   5
      NO2      =   6
      NO       =   7
      NO3      =   8
      HONO     =   9
      HNO3     =  10
      HNO4     =  11
      HO2      =  12
      HCHO     =  13
      CO       =  14
      ACD      =  15
      MO2      =  16
      ALD      =  17
      ETHP     =  18
      ACT      =  19
      ACO3     =  20
      UALD     =  21
      KET      =  22
      MEK      =  23
      HKET     =  24
      MACR     =  25
      MACP     =  26
      XO2      =  27
      MVK      =  28
      GLY      =  29
      MGLY     =  30
      DCB1     =  31
      DCB2     =  32
      BALD     =  33
      CHO      =  34
      OP1      =  35
      OP2      =  36
      PAA      =  37
      ONIT     =  38
      PAN      =  39
      N2O5     =  40
      SO2      =  41
      SULF     =  42
      SULRXN   =  43
      ETH      =  44
      HC3      =  45
      HC3P     =  46
      HC5      =  47
      HC5P     =  48
      HC8      =  49
      HC8P     =  50
      ALK5RXN  =  51
      ETE      =  52
      ETEP     =  53
      OLT      =  54
      OLTP     =  55
      OLI      =  56
      OLIP     =  57
      DIEN     =  58
      ACE      =  59
      ORA1     =  60
      BENZENE  =  61
      BENP     =  62
      EPX      =  63
      PHEN     =  64
      BENZRO2  =  65
      TOL      =  66
      TR2      =  67
      TLP1     =  68
      CSL      =  69
      TOLRO2   =  70
      XYM      =  71
      XY2      =  72
      XYL1     =  73
      XYLRO2   =  74
      XYP      =  75
      XYO      =  76
      XYO2     =  77
      ISO      =  78
      ISOP     =  79
      ISOPRXN  =  80
      API      =  81
      APIP     =  82
      TRPRXN   =  83
      LIM      =  84
      LIMP     =  85
      RCO3     =  86
      ACTP     =  87
      MEKP     =  88
      KETP     =  89
      MCP      =  90
      MVKP     =  91
      UALP     =  92
      DCB3     =  93
      BALP     =  94
      ADDC     =  95
      MCT      =  96
      MCTO     =  97
      MOH      =  98
      EOH      =  99
      ROH      = 100
      ETEG     = 101
      ISHP     = 102
      MAHP     = 103
      ORA2     = 104
      ORAP     = 105
      PPN      = 106
      MPAN     = 107
      NALD     = 108
      ISON     = 109
      MCTP     = 110
      OLNN     = 111
      OLND     = 112
      ADCN     = 113
      TOLP     = 114
      PER1     = 115
      XYLP     = 116
      PER2     = 117
      XYOP     = 118
      BAL1     = 119
      BAL2     = 120
      TOLNRXN  = 121
      TOLHRXN  = 122
      XYLNRXN  = 123
      XYLHRXN  = 124
      BNZNRXN  = 125
      BNZHRXN  = 126
      SESQ     = 127
      SESQRXN  = 128

c..Allocate and define ebi species

      N_EBISP  = 115
      ALLOCATE( EBISP( N_EBISP ), STAT = IOS ) 
      IF ( IOS .NE. 0 ) THEN
         MSG = 'Error allocating EBISP' 
         CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      EBISP(   1 ) = H2O2
      EBISP(   2 ) = HNO3
      EBISP(   3 ) = HCHO
      EBISP(   4 ) = CO
      EBISP(   5 ) = ACD
      EBISP(   6 ) = MO2
      EBISP(   7 ) = ALD
      EBISP(   8 ) = ETHP
      EBISP(   9 ) = ACT
      EBISP(  10 ) = UALD
      EBISP(  11 ) = KET
      EBISP(  12 ) = MEK
      EBISP(  13 ) = HKET
      EBISP(  14 ) = MACR
      EBISP(  15 ) = MACP
      EBISP(  16 ) = XO2
      EBISP(  17 ) = MVK
      EBISP(  18 ) = GLY
      EBISP(  19 ) = MGLY
      EBISP(  20 ) = DCB1
      EBISP(  21 ) = DCB2
      EBISP(  22 ) = BALD
      EBISP(  23 ) = CHO
      EBISP(  24 ) = OP1
      EBISP(  25 ) = OP2
      EBISP(  26 ) = PAA
      EBISP(  27 ) = ONIT
      EBISP(  28 ) = SO2
      EBISP(  29 ) = SULF
      EBISP(  30 ) = SULRXN
      EBISP(  31 ) = ETH
      EBISP(  32 ) = HC3
      EBISP(  33 ) = HC3P
      EBISP(  34 ) = HC5
      EBISP(  35 ) = HC5P
      EBISP(  36 ) = HC8
      EBISP(  37 ) = HC8P
      EBISP(  38 ) = ALK5RXN
      EBISP(  39 ) = ETE
      EBISP(  40 ) = ETEP
      EBISP(  41 ) = OLT
      EBISP(  42 ) = OLTP
      EBISP(  43 ) = OLI
      EBISP(  44 ) = OLIP
      EBISP(  45 ) = DIEN
      EBISP(  46 ) = ACE
      EBISP(  47 ) = ORA1
      EBISP(  48 ) = BENZENE
      EBISP(  49 ) = BENP
      EBISP(  50 ) = EPX
      EBISP(  51 ) = PHEN
      EBISP(  52 ) = BENZRO2
      EBISP(  53 ) = TOL
      EBISP(  54 ) = TR2
      EBISP(  55 ) = TLP1
      EBISP(  56 ) = CSL
      EBISP(  57 ) = TOLRO2
      EBISP(  58 ) = XYM
      EBISP(  59 ) = XY2
      EBISP(  60 ) = XYL1
      EBISP(  61 ) = XYLRO2
      EBISP(  62 ) = XYP
      EBISP(  63 ) = XYO
      EBISP(  64 ) = XYO2
      EBISP(  65 ) = ISO
      EBISP(  66 ) = ISOP
      EBISP(  67 ) = ISOPRXN
      EBISP(  68 ) = API
      EBISP(  69 ) = APIP
      EBISP(  70 ) = TRPRXN
      EBISP(  71 ) = LIM
      EBISP(  72 ) = LIMP
      EBISP(  73 ) = RCO3
      EBISP(  74 ) = ACTP
      EBISP(  75 ) = MEKP
      EBISP(  76 ) = KETP
      EBISP(  77 ) = MCP
      EBISP(  78 ) = MVKP
      EBISP(  79 ) = UALP
      EBISP(  80 ) = DCB3
      EBISP(  81 ) = BALP
      EBISP(  82 ) = ADDC
      EBISP(  83 ) = MCT
      EBISP(  84 ) = MCTO
      EBISP(  85 ) = MOH
      EBISP(  86 ) = EOH
      EBISP(  87 ) = ROH
      EBISP(  88 ) = ETEG
      EBISP(  89 ) = ISHP
      EBISP(  90 ) = MAHP
      EBISP(  91 ) = ORA2
      EBISP(  92 ) = ORAP
      EBISP(  93 ) = PPN
      EBISP(  94 ) = MPAN
      EBISP(  95 ) = NALD
      EBISP(  96 ) = ISON
      EBISP(  97 ) = MCTP
      EBISP(  98 ) = OLNN
      EBISP(  99 ) = OLND
      EBISP( 100 ) = ADCN
      EBISP( 101 ) = TOLP
      EBISP( 102 ) = PER1
      EBISP( 103 ) = XYLP
      EBISP( 104 ) = PER2
      EBISP( 105 ) = XYOP
      EBISP( 106 ) = BAL1
      EBISP( 107 ) = BAL2
      EBISP( 108 ) = TOLNRXN
      EBISP( 109 ) = TOLHRXN
      EBISP( 110 ) = XYLNRXN
      EBISP( 111 ) = XYLHRXN
      EBISP( 112 ) = BNZNRXN
      EBISP( 113 ) = BNZHRXN
      EBISP( 114 ) = SESQ
      EBISP( 115 ) = SESQRXN


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set species tolerances
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      RTOL( O3      ) = 1.0E-03
      RTOL( O3P     ) = 1.0E+00
      RTOL( O1D     ) = 1.0E+00
      RTOL( H2O2    ) = 1.0E-03
      RTOL( HO      ) = 1.0E-03
      RTOL( NO2     ) = 1.0E-03
      RTOL( NO      ) = 1.0E-03
      RTOL( NO3     ) = 1.0E-03
      RTOL( HONO    ) = 1.0E-03
      RTOL( HNO3    ) = 1.0E-03
      RTOL( HNO4    ) = 1.0E-03
      RTOL( HO2     ) = 1.0E-03
      RTOL( HCHO    ) = 1.0E-03
      RTOL( CO      ) = 1.0E-03
      RTOL( ACD     ) = 1.0E-03
      RTOL( MO2     ) = 1.0E-03
      RTOL( ALD     ) = 1.0E-03
      RTOL( ETHP    ) = 1.0E-03
      RTOL( ACT     ) = 1.0E-03
      RTOL( ACO3    ) = 1.0E-03
      RTOL( UALD    ) = 1.0E-03
      RTOL( KET     ) = 1.0E-03
      RTOL( MEK     ) = 1.0E-03
      RTOL( HKET    ) = 1.0E-03
      RTOL( MACR    ) = 1.0E-03
      RTOL( MACP    ) = 1.0E-03
      RTOL( XO2     ) = 1.0E-03
      RTOL( MVK     ) = 1.0E-03
      RTOL( GLY     ) = 1.0E-03
      RTOL( MGLY    ) = 1.0E-03
      RTOL( DCB1    ) = 1.0E-03
      RTOL( DCB2    ) = 1.0E-03
      RTOL( BALD    ) = 1.0E-03
      RTOL( CHO     ) = 1.0E-03
      RTOL( OP1     ) = 1.0E-03
      RTOL( OP2     ) = 1.0E-03
      RTOL( PAA     ) = 1.0E-03
      RTOL( ONIT    ) = 1.0E-03
      RTOL( PAN     ) = 1.0E-03
      RTOL( N2O5    ) = 1.0E-03
      RTOL( SO2     ) = 1.0E-03
      RTOL( SULF    ) = 1.0E+00
      RTOL( SULRXN  ) = 1.0E+00
      RTOL( ETH     ) = 1.0E-03
      RTOL( HC3     ) = 1.0E-03
      RTOL( HC3P    ) = 1.0E-03
      RTOL( HC5     ) = 1.0E-03
      RTOL( HC5P    ) = 1.0E-03
      RTOL( HC8     ) = 1.0E-03
      RTOL( HC8P    ) = 1.0E-03
      RTOL( ALK5RXN ) = 1.0E+00
      RTOL( ETE     ) = 1.0E-03
      RTOL( ETEP    ) = 1.0E-03
      RTOL( OLT     ) = 1.0E-03
      RTOL( OLTP    ) = 1.0E-03
      RTOL( OLI     ) = 1.0E-03
      RTOL( OLIP    ) = 1.0E-03
      RTOL( DIEN    ) = 1.0E-03
      RTOL( ACE     ) = 1.0E-03
      RTOL( ORA1    ) = 1.0E-03
      RTOL( BENZENE ) = 1.0E-03
      RTOL( BENP    ) = 1.0E-03
      RTOL( EPX     ) = 1.0E-03
      RTOL( PHEN    ) = 1.0E-03
      RTOL( BENZRO2 ) = 1.0E-03
      RTOL( TOL     ) = 1.0E-03
      RTOL( TR2     ) = 1.0E-03
      RTOL( TLP1    ) = 1.0E-03
      RTOL( CSL     ) = 1.0E-03
      RTOL( TOLRO2  ) = 1.0E-03
      RTOL( XYM     ) = 1.0E-03
      RTOL( XY2     ) = 1.0E-03
      RTOL( XYL1    ) = 1.0E-03
      RTOL( XYLRO2  ) = 1.0E-03
      RTOL( XYP     ) = 1.0E-03
      RTOL( XYO     ) = 1.0E-03
      RTOL( XYO2    ) = 1.0E-03
      RTOL( ISO     ) = 1.0E-03
      RTOL( ISOP    ) = 1.0E-03
      RTOL( ISOPRXN ) = 1.0E+00
      RTOL( API     ) = 1.0E-03
      RTOL( APIP    ) = 1.0E-03
      RTOL( TRPRXN  ) = 1.0E+00
      RTOL( LIM     ) = 1.0E-03
      RTOL( LIMP    ) = 1.0E-03
      RTOL( RCO3    ) = 1.0E-03
      RTOL( ACTP    ) = 1.0E-03
      RTOL( MEKP    ) = 1.0E-03
      RTOL( KETP    ) = 1.0E-03
      RTOL( MCP     ) = 1.0E-03
      RTOL( MVKP    ) = 1.0E-03
      RTOL( UALP    ) = 1.0E-03
      RTOL( DCB3    ) = 1.0E-03
      RTOL( BALP    ) = 1.0E-03
      RTOL( ADDC    ) = 1.0E-03
      RTOL( MCT     ) = 1.0E-03
      RTOL( MCTO    ) = 1.0E-03
      RTOL( MOH     ) = 1.0E-03
      RTOL( EOH     ) = 1.0E-03
      RTOL( ROH     ) = 1.0E-03
      RTOL( ETEG    ) = 1.0E-03
      RTOL( ISHP    ) = 1.0E-03
      RTOL( MAHP    ) = 1.0E-03
      RTOL( ORA2    ) = 1.0E-03
      RTOL( ORAP    ) = 1.0E-03
      RTOL( PPN     ) = 1.0E-03
      RTOL( MPAN    ) = 1.0E-03
      RTOL( NALD    ) = 1.0E-03
      RTOL( ISON    ) = 1.0E-03
      RTOL( MCTP    ) = 1.0E-03
      RTOL( OLNN    ) = 1.0E-03
      RTOL( OLND    ) = 1.0E-03
      RTOL( ADCN    ) = 1.0E-03
      RTOL( TOLP    ) = 1.0E-03
      RTOL( PER1    ) = 1.0E-03
      RTOL( XYLP    ) = 1.0E-03
      RTOL( PER2    ) = 1.0E-03
      RTOL( XYOP    ) = 1.0E-03
      RTOL( BAL1    ) = 1.0E-03
      RTOL( BAL2    ) = 1.0E-03
      RTOL( TOLNRXN ) = 1.0E+00
      RTOL( TOLHRXN ) = 1.0E+00
      RTOL( XYLNRXN ) = 1.0E+00
      RTOL( XYLHRXN ) = 1.0E+00
      RTOL( BNZNRXN ) = 1.0E+00
      RTOL( BNZHRXN ) = 1.0E+00
      RTOL( SESQ    ) = 1.0E-03
      RTOL( SESQRXN ) = 1.0E+00


      RETURN

      END
