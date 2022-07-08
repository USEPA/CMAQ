
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
C  PRECONDITIONS: For the CRACMM1_AQ mechanism
C
C  RETURN VALUES: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Jun 14, 2022
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
     &          FORWARD_CONV( NUMB_MECH_SPC),
     &          REVERSE_CONV( NUMB_MECH_SPC),
     &          RXRAT( NRXNS ),
     &          RTOL( NUMB_MECH_SPC),
     &          YC(   NUMB_MECH_SPC),
     &          YC0(  NUMB_MECH_SPC),
     &          YCP(  NUMB_MECH_SPC),
     &          PROD( NUMB_MECH_SPC),
     &          LOSS( NUMB_MECH_SPC),
     &          STAT = IOS )

       IF ( IOS .NE. 0 ) THEN
          MSG = 'Error allocating RKI, RXRAT, RTOL, YC, YC0, YCP, FORWARD_CONV, '
     &       // 'REVERSE_CONV, PROD, LOSS, or PNEG'
          CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
       END IF

       ALLOCATE( ATMPRES( MAX_CELLS_EBI ),
     &           H2O    ( MAX_CELLS_EBI ),
     &           TEMP   ( MAX_CELLS_EBI ),
     &           DENSITY( MAX_CELLS_EBI ),
     &           HET    ( MAX_CELLS_EBI, NHETERO ),
     &           RJIN   ( MAX_CELLS_EBI, NPHOTAB ),
     &           SEAWATER  ( MAX_CELLS_EBI ) ,
     &           STAT = IOS )

       IF ( IOS .NE. 0 ) THEN
          MSG = 'Error allocating ATMPRES, H2O, TEMP, HET, RJIN, SEAWATER '
          CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
       END IF

       IF( NSPECIAL .GT. 0 )THEN
          ALLOCATE( SYC( MAX_CELLS_EBI, NUMB_MECH_SPC ), STAT = IOS )
          IF ( IOS .NE. 0 ) THEN
               MSG = 'Error allocating SYC'
               CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
          END IF
          ALLOCATE( IOLD2NEW( NUMB_MECH_SPC, 1 ), STAT = IOS )
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

      N_EBISP  = 165
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
      EBISP(  12 ) = PINAL
      EBISP(  13 ) = HC10P
      EBISP(  14 ) = LIMAL
      EBISP(  15 ) = MEK
      EBISP(  16 ) = HKET
      EBISP(  17 ) = MACR
      EBISP(  18 ) = MACP
      EBISP(  19 ) = XO2
      EBISP(  20 ) = MVK
      EBISP(  21 ) = GLY
      EBISP(  22 ) = MGLY
      EBISP(  23 ) = DCB1
      EBISP(  24 ) = DCB2
      EBISP(  25 ) = BALD
      EBISP(  26 ) = CHO
      EBISP(  27 ) = OP1
      EBISP(  28 ) = OP2
      EBISP(  29 ) = OPB
      EBISP(  30 ) = OP3
      EBISP(  31 ) = PAA
      EBISP(  32 ) = ONIT
      EBISP(  33 ) = SO2
      EBISP(  34 ) = SULF
      EBISP(  35 ) = SULRXN
      EBISP(  36 ) = ETH
      EBISP(  37 ) = HC3
      EBISP(  38 ) = HC3P
      EBISP(  39 ) = ASOATJ
      EBISP(  40 ) = HC5
      EBISP(  41 ) = HC5P
      EBISP(  42 ) = ETE
      EBISP(  43 ) = ETEP
      EBISP(  44 ) = OLT
      EBISP(  45 ) = OLTP
      EBISP(  46 ) = OLI
      EBISP(  47 ) = OLIP
      EBISP(  48 ) = ACE
      EBISP(  49 ) = ORA1
      EBISP(  50 ) = BEN
      EBISP(  51 ) = BENP
      EBISP(  52 ) = PHEN
      EBISP(  53 ) = TOL
      EBISP(  54 ) = TOLP
      EBISP(  55 ) = CSL
      EBISP(  56 ) = XYM
      EBISP(  57 ) = XYMP
      EBISP(  58 ) = XYE
      EBISP(  59 ) = XYEP
      EBISP(  60 ) = ISO
      EBISP(  61 ) = ISOP
      EBISP(  62 ) = API
      EBISP(  63 ) = APIP1
      EBISP(  64 ) = APIP2
      EBISP(  65 ) = LIM
      EBISP(  66 ) = LIMP1
      EBISP(  67 ) = LIMP2
      EBISP(  68 ) = PINALP
      EBISP(  69 ) = RCO3
      EBISP(  70 ) = LIMALP
      EBISP(  71 ) = ACTP
      EBISP(  72 ) = MEKP
      EBISP(  73 ) = KETP
      EBISP(  74 ) = MCP
      EBISP(  75 ) = MVKP
      EBISP(  76 ) = UALP
      EBISP(  77 ) = DCB3
      EBISP(  78 ) = BALP
      EBISP(  79 ) = ADDC
      EBISP(  80 ) = MCT
      EBISP(  81 ) = MCTO
      EBISP(  82 ) = MOH
      EBISP(  83 ) = EOH
      EBISP(  84 ) = ROH
      EBISP(  85 ) = ETEG
      EBISP(  86 ) = ISHP
      EBISP(  87 ) = IEPOX
      EBISP(  88 ) = MAHP
      EBISP(  89 ) = ORA2
      EBISP(  90 ) = ORAP
      EBISP(  91 ) = PPN
      EBISP(  92 ) = MPAN
      EBISP(  93 ) = TRPN
      EBISP(  94 ) = HOM
      EBISP(  95 ) = NALD
      EBISP(  96 ) = ISON
      EBISP(  97 ) = MCTP
      EBISP(  98 ) = OLNN
      EBISP(  99 ) = OLND
      EBISP( 100 ) = APINP1
      EBISP( 101 ) = APINP2
      EBISP( 102 ) = LIMNP1
      EBISP( 103 ) = LIMNP2
      EBISP( 104 ) = ADCN
      EBISP( 105 ) = VROCP4OXY2
      EBISP( 106 ) = VROCN1OXY6
      EBISP( 107 ) = FURANONE
      EBISP( 108 ) = VROCP3OXY2
      EBISP( 109 ) = VROCP0OXY4
      EBISP( 110 ) = BAL1
      EBISP( 111 ) = BAL2
      EBISP( 112 ) = ELHOM
      EBISP( 113 ) = VROCIOXY
      EBISP( 114 ) = SLOWROC
      EBISP( 115 ) = ACRO
      EBISP( 116 ) = BDE13
      EBISP( 117 ) = BDE13P
      EBISP( 118 ) = FURAN
      EBISP( 119 ) = FURANO2
      EBISP( 120 ) = PROG
      EBISP( 121 ) = SESQ
      EBISP( 122 ) = SESQNRO2
      EBISP( 123 ) = VROCN2OXY2
      EBISP( 124 ) = SESQRO2
      EBISP( 125 ) = VROCP0OXY2
      EBISP( 126 ) = VROCP1OXY3
      EBISP( 127 ) = AGLYJ
      EBISP( 128 ) = IEPOXP
      EBISP( 129 ) = AISO3NOSJ
      EBISP( 130 ) = ASO4J
      EBISP( 131 ) = AISO3OSJ
      EBISP( 132 ) = VROCP6ALK
      EBISP( 133 ) = VROCP6ALKP
      EBISP( 134 ) = VROCP5ALK
      EBISP( 135 ) = VROCP5ALKP
      EBISP( 136 ) = VROCP4ALK
      EBISP( 137 ) = VROCP4ALKP
      EBISP( 138 ) = VROCP3ALK
      EBISP( 139 ) = VROCP3ALKP
      EBISP( 140 ) = VROCP2ALK
      EBISP( 141 ) = VROCP2ALKP
      EBISP( 142 ) = VROCP1ALK
      EBISP( 143 ) = VROCP1ALKP
      EBISP( 144 ) = HC10
      EBISP( 145 ) = VROCP6ALKP2
      EBISP( 146 ) = VROCP5ALKP2
      EBISP( 147 ) = VROCP4ALKP2
      EBISP( 148 ) = VROCP2OXY2
      EBISP( 149 ) = VROCP3ALKP2
      EBISP( 150 ) = VROCP1OXY1
      EBISP( 151 ) = VROCP2ALKP2
      EBISP( 152 ) = VROCP1ALKP2
      EBISP( 153 ) = VROCN1OXY1
      EBISP( 154 ) = HC10P2
      EBISP( 155 ) = VROCP6ARO
      EBISP( 156 ) = VROCP6AROP
      EBISP( 157 ) = VROCN2OXY4
      EBISP( 158 ) = VROCN1OXY3
      EBISP( 159 ) = VROCP5ARO
      EBISP( 160 ) = VROCP5AROP
      EBISP( 161 ) = NAPH
      EBISP( 162 ) = NAPHP
      EBISP( 163 ) = VROCN2OXY8
      EBISP( 164 ) = VROCP5OXY1
      EBISP( 165 ) = VROCP6OXY1


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set species tolerances
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      RTOL( O3          ) = 5.0D-04
      RTOL( O3P         ) = 1.0D+00
      RTOL( O1D         ) = 1.0D+00
      RTOL( H2O2        ) = 5.0D-04
      RTOL( HO          ) = 5.0D-04
      RTOL( NO2         ) = 5.0D-04
      RTOL( NO          ) = 5.0D-04
      RTOL( NO3         ) = 5.0D-04
      RTOL( HONO        ) = 5.0D-04
      RTOL( HNO3        ) = 5.0D-04
      RTOL( HNO4        ) = 5.0D-04
      RTOL( HO2         ) = 5.0D-04
      RTOL( HCHO        ) = 5.0D-04
      RTOL( CO          ) = 5.0D-04
      RTOL( ACD         ) = 5.0D-04
      RTOL( MO2         ) = 5.0D-04
      RTOL( ALD         ) = 5.0D-04
      RTOL( ETHP        ) = 5.0D-04
      RTOL( ACT         ) = 5.0D-04
      RTOL( ACO3        ) = 5.0D-04
      RTOL( UALD        ) = 5.0D-04
      RTOL( KET         ) = 5.0D-04
      RTOL( PINAL       ) = 5.0D-04
      RTOL( HC10P       ) = 5.0D-04
      RTOL( LIMAL       ) = 5.0D-04
      RTOL( MEK         ) = 5.0D-04
      RTOL( HKET        ) = 5.0D-04
      RTOL( MACR        ) = 5.0D-04
      RTOL( MACP        ) = 5.0D-04
      RTOL( XO2         ) = 5.0D-04
      RTOL( MVK         ) = 5.0D-04
      RTOL( GLY         ) = 5.0D-04
      RTOL( MGLY        ) = 5.0D-04
      RTOL( DCB1        ) = 5.0D-04
      RTOL( DCB2        ) = 5.0D-04
      RTOL( BALD        ) = 5.0D-04
      RTOL( CHO         ) = 5.0D-04
      RTOL( OP1         ) = 5.0D-04
      RTOL( OP2         ) = 5.0D-04
      RTOL( OPB         ) = 5.0D-04
      RTOL( OP3         ) = 5.0D-04
      RTOL( PAA         ) = 5.0D-04
      RTOL( ONIT        ) = 5.0D-04
      RTOL( PAN         ) = 5.0D-04
      RTOL( N2O5        ) = 5.0D-04
      RTOL( SO2         ) = 5.0D-04
      RTOL( SULF        ) = 1.0D+00
      RTOL( SULRXN      ) = 1.0D+00
      RTOL( ETH         ) = 5.0D-04
      RTOL( HC3         ) = 5.0D-04
      RTOL( HC3P        ) = 5.0D-04
      RTOL( ASOATJ      ) = 5.0D-04
      RTOL( HC5         ) = 5.0D-04
      RTOL( HC5P        ) = 5.0D-04
      RTOL( ETE         ) = 5.0D-04
      RTOL( ETEP        ) = 5.0D-04
      RTOL( OLT         ) = 5.0D-04
      RTOL( OLTP        ) = 5.0D-04
      RTOL( OLI         ) = 5.0D-04
      RTOL( OLIP        ) = 5.0D-04
      RTOL( ACE         ) = 5.0D-04
      RTOL( ORA1        ) = 5.0D-04
      RTOL( BEN         ) = 5.0D-04
      RTOL( BENP        ) = 5.0D-04
      RTOL( PHEN        ) = 5.0D-04
      RTOL( TOL         ) = 5.0D-04
      RTOL( TOLP        ) = 5.0D-04
      RTOL( CSL         ) = 5.0D-04
      RTOL( XYM         ) = 5.0D-04
      RTOL( XYMP        ) = 5.0D-04
      RTOL( XYE         ) = 5.0D-04
      RTOL( XYEP        ) = 5.0D-04
      RTOL( ISO         ) = 5.0D-04
      RTOL( ISOP        ) = 5.0D-04
      RTOL( API         ) = 5.0D-04
      RTOL( APIP1       ) = 5.0D-04
      RTOL( APIP2       ) = 5.0D-04
      RTOL( LIM         ) = 5.0D-04
      RTOL( LIMP1       ) = 5.0D-04
      RTOL( LIMP2       ) = 5.0D-04
      RTOL( PINALP      ) = 5.0D-04
      RTOL( RCO3        ) = 5.0D-04
      RTOL( LIMALP      ) = 5.0D-04
      RTOL( ACTP        ) = 5.0D-04
      RTOL( MEKP        ) = 5.0D-04
      RTOL( KETP        ) = 5.0D-04
      RTOL( MCP         ) = 5.0D-04
      RTOL( MVKP        ) = 5.0D-04
      RTOL( UALP        ) = 5.0D-04
      RTOL( DCB3        ) = 5.0D-04
      RTOL( BALP        ) = 5.0D-04
      RTOL( ADDC        ) = 5.0D-04
      RTOL( MCT         ) = 5.0D-04
      RTOL( MCTO        ) = 5.0D-04
      RTOL( MOH         ) = 5.0D-04
      RTOL( EOH         ) = 5.0D-04
      RTOL( ROH         ) = 5.0D-04
      RTOL( ETEG        ) = 5.0D-04
      RTOL( ISHP        ) = 5.0D-04
      RTOL( IEPOX       ) = 5.0D-04
      RTOL( MAHP        ) = 5.0D-04
      RTOL( ORA2        ) = 5.0D-04
      RTOL( ORAP        ) = 5.0D-04
      RTOL( PPN         ) = 5.0D-04
      RTOL( MPAN        ) = 5.0D-04
      RTOL( TRPN        ) = 5.0D-04
      RTOL( HOM         ) = 5.0D-04
      RTOL( NALD        ) = 5.0D-04
      RTOL( ISON        ) = 5.0D-04
      RTOL( MCTP        ) = 5.0D-04
      RTOL( OLNN        ) = 5.0D-04
      RTOL( OLND        ) = 5.0D-04
      RTOL( APINP1      ) = 5.0D-04
      RTOL( APINP2      ) = 5.0D-04
      RTOL( LIMNP1      ) = 5.0D-04
      RTOL( LIMNP2      ) = 5.0D-04
      RTOL( ADCN        ) = 5.0D-04
      RTOL( VROCP4OXY2  ) = 5.0D-04
      RTOL( VROCN1OXY6  ) = 5.0D-04
      RTOL( FURANONE    ) = 5.0D-04
      RTOL( VROCP3OXY2  ) = 5.0D-04
      RTOL( VROCP0OXY4  ) = 5.0D-04
      RTOL( BAL1        ) = 5.0D-04
      RTOL( BAL2        ) = 5.0D-04
      RTOL( ELHOM       ) = 5.0D-04
      RTOL( VROCIOXY    ) = 5.0D-04
      RTOL( SLOWROC     ) = 5.0D-04
      RTOL( ACRO        ) = 5.0D-04
      RTOL( BDE13       ) = 5.0D-04
      RTOL( BDE13P      ) = 5.0D-04
      RTOL( FURAN       ) = 5.0D-04
      RTOL( FURANO2     ) = 5.0D-04
      RTOL( PROG        ) = 5.0D-04
      RTOL( SESQ        ) = 5.0D-04
      RTOL( SESQNRO2    ) = 5.0D-04
      RTOL( VROCN2OXY2  ) = 5.0D-04
      RTOL( SESQRO2     ) = 5.0D-04
      RTOL( VROCP0OXY2  ) = 5.0D-04
      RTOL( VROCP1OXY3  ) = 5.0D-04
      RTOL( AGLYJ       ) = 5.0D-04
      RTOL( IEPOXP      ) = 5.0D-04
      RTOL( AISO3NOSJ   ) = 5.0D-04
      RTOL( ASO4J       ) = 5.0D-04
      RTOL( AISO3OSJ    ) = 5.0D-04
      RTOL( VROCP6ALK   ) = 5.0D-04
      RTOL( VROCP6ALKP  ) = 5.0D-04
      RTOL( VROCP5ALK   ) = 5.0D-04
      RTOL( VROCP5ALKP  ) = 5.0D-04
      RTOL( VROCP4ALK   ) = 5.0D-04
      RTOL( VROCP4ALKP  ) = 5.0D-04
      RTOL( VROCP3ALK   ) = 5.0D-04
      RTOL( VROCP3ALKP  ) = 5.0D-04
      RTOL( VROCP2ALK   ) = 5.0D-04
      RTOL( VROCP2ALKP  ) = 5.0D-04
      RTOL( VROCP1ALK   ) = 5.0D-04
      RTOL( VROCP1ALKP  ) = 5.0D-04
      RTOL( HC10        ) = 5.0D-04
      RTOL( VROCP6ALKP2 ) = 5.0D-04
      RTOL( VROCP5ALKP2 ) = 5.0D-04
      RTOL( VROCP4ALKP2 ) = 5.0D-04
      RTOL( VROCP2OXY2  ) = 5.0D-04
      RTOL( VROCP3ALKP2 ) = 5.0D-04
      RTOL( VROCP1OXY1  ) = 5.0D-04
      RTOL( VROCP2ALKP2 ) = 5.0D-04
      RTOL( VROCP1ALKP2 ) = 5.0D-04
      RTOL( VROCN1OXY1  ) = 5.0D-04
      RTOL( HC10P2      ) = 5.0D-04
      RTOL( VROCP6ARO   ) = 5.0D-04
      RTOL( VROCP6AROP  ) = 5.0D-04
      RTOL( VROCN2OXY4  ) = 5.0D-04
      RTOL( VROCN1OXY3  ) = 5.0D-04
      RTOL( VROCP5ARO   ) = 5.0D-04
      RTOL( VROCP5AROP  ) = 5.0D-04
      RTOL( NAPH        ) = 5.0D-04
      RTOL( NAPHP       ) = 5.0D-04
      RTOL( VROCN2OXY8  ) = 5.0D-04
      RTOL( VROCP5OXY1  ) = 5.0D-04
      RTOL( VROCP6OXY1  ) = 5.0D-04


      RETURN

      END
