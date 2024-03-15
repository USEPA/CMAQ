
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
C  PRECONDITIONS: For the CRACMM2 mechanism
C
C  RETURN VALUES: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Mar 15, 2024
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

      N_EBISP  = 176
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
      EBISP(  33 ) = TRPN
      EBISP(  34 ) = HONIT
      EBISP(  35 ) = SO2
      EBISP(  36 ) = SULF
      EBISP(  37 ) = SULRXN
      EBISP(  38 ) = ETH
      EBISP(  39 ) = HC3
      EBISP(  40 ) = HC3P
      EBISP(  41 ) = ASOATJ
      EBISP(  42 ) = HC5
      EBISP(  43 ) = HC5P
      EBISP(  44 ) = ETE
      EBISP(  45 ) = ETEP
      EBISP(  46 ) = OLT
      EBISP(  47 ) = OLTP
      EBISP(  48 ) = OLI
      EBISP(  49 ) = OLIP
      EBISP(  50 ) = ACE
      EBISP(  51 ) = ORA1
      EBISP(  52 ) = BEN
      EBISP(  53 ) = BENP
      EBISP(  54 ) = PHEN
      EBISP(  55 ) = TOL
      EBISP(  56 ) = TOLP
      EBISP(  57 ) = CSL
      EBISP(  58 ) = XYL
      EBISP(  59 ) = XYLP
      EBISP(  60 ) = EBZ
      EBISP(  61 ) = EBZP
      EBISP(  62 ) = ISO
      EBISP(  63 ) = ISON
      EBISP(  64 ) = INO2
      EBISP(  65 ) = ISOP
      EBISP(  66 ) = ISHP
      EBISP(  67 ) = IEPOX
      EBISP(  68 ) = IPX
      EBISP(  69 ) = INALD
      EBISP(  70 ) = ROH
      EBISP(  71 ) = API
      EBISP(  72 ) = APIP1
      EBISP(  73 ) = APIP2
      EBISP(  74 ) = LIM
      EBISP(  75 ) = LIMP1
      EBISP(  76 ) = LIMP2
      EBISP(  77 ) = PINALP
      EBISP(  78 ) = RCO3
      EBISP(  79 ) = LIMALP
      EBISP(  80 ) = ACTP
      EBISP(  81 ) = MEKP
      EBISP(  82 ) = KETP
      EBISP(  83 ) = MCP
      EBISP(  84 ) = MVKP
      EBISP(  85 ) = UALP
      EBISP(  86 ) = DCB3
      EBISP(  87 ) = BALP
      EBISP(  88 ) = ADDC
      EBISP(  89 ) = MCT
      EBISP(  90 ) = MCTO
      EBISP(  91 ) = MOH
      EBISP(  92 ) = EOH
      EBISP(  93 ) = ETEG
      EBISP(  94 ) = MAHP
      EBISP(  95 ) = ORA2
      EBISP(  96 ) = ORAP
      EBISP(  97 ) = PPN
      EBISP(  98 ) = MPAN
      EBISP(  99 ) = MCTP
      EBISP( 100 ) = OLNN
      EBISP( 101 ) = OLND
      EBISP( 102 ) = APINP1
      EBISP( 103 ) = APINP2
      EBISP( 104 ) = LIMNP1
      EBISP( 105 ) = LIMNP2
      EBISP( 106 ) = ADCN
      EBISP( 107 ) = HOM
      EBISP( 108 ) = VROCP4OXY2
      EBISP( 109 ) = VROCN1OXY6
      EBISP( 110 ) = FURANONE
      EBISP( 111 ) = VROCP3OXY2
      EBISP( 112 ) = VROCP0OXY4
      EBISP( 113 ) = BAL1
      EBISP( 114 ) = BAL2
      EBISP( 115 ) = ELHOM
      EBISP( 116 ) = VROCIOXY
      EBISP( 117 ) = SLOWROC
      EBISP( 118 ) = ACRO
      EBISP( 119 ) = BDE13
      EBISP( 120 ) = BDE13P
      EBISP( 121 ) = FURAN
      EBISP( 122 ) = FURANO2
      EBISP( 123 ) = PROG
      EBISP( 124 ) = SESQ
      EBISP( 125 ) = SESQNRO2
      EBISP( 126 ) = VROCN2OXY2
      EBISP( 127 ) = SESQRO2
      EBISP( 128 ) = VROCP0OXY2
      EBISP( 129 ) = VROCP1OXY3
      EBISP( 130 ) = AGLYJ
      EBISP( 131 ) = IEPOXP
      EBISP( 132 ) = AISO3NOSJ
      EBISP( 133 ) = ASO4J
      EBISP( 134 ) = AISO3OSJ
      EBISP( 135 ) = AISO4J
      EBISP( 136 ) = AISO5J
      EBISP( 137 ) = VROCP6ALK
      EBISP( 138 ) = VROCP6ALKP
      EBISP( 139 ) = VROCP5ALK
      EBISP( 140 ) = VROCP5ALKP
      EBISP( 141 ) = VROCP4ALK
      EBISP( 142 ) = VROCP4ALKP
      EBISP( 143 ) = VROCP3ALK
      EBISP( 144 ) = VROCP3ALKP
      EBISP( 145 ) = VROCP2ALK
      EBISP( 146 ) = VROCP2ALKP
      EBISP( 147 ) = VROCP1ALK
      EBISP( 148 ) = VROCP1ALKP
      EBISP( 149 ) = HC10
      EBISP( 150 ) = VROCP6ALKP2
      EBISP( 151 ) = VROCP5ALKP2
      EBISP( 152 ) = VROCP4ALKP2
      EBISP( 153 ) = VROCP2OXY2
      EBISP( 154 ) = VROCP3ALKP2
      EBISP( 155 ) = VROCP1OXY1
      EBISP( 156 ) = VROCP2ALKP2
      EBISP( 157 ) = VROCP1ALKP2
      EBISP( 158 ) = VROCN1OXY1
      EBISP( 159 ) = HC10P2
      EBISP( 160 ) = VROCP6ARO
      EBISP( 161 ) = VROCP6AROP
      EBISP( 162 ) = VROCN2OXY4
      EBISP( 163 ) = VROCN1OXY3
      EBISP( 164 ) = VROCP5ARO
      EBISP( 165 ) = VROCP5AROP
      EBISP( 166 ) = NAPH
      EBISP( 167 ) = NAPHP
      EBISP( 168 ) = VROCN2OXY8
      EBISP( 169 ) = VROCP5OXY1
      EBISP( 170 ) = VROCP6OXY1
      EBISP( 171 ) = ECH4
      EBISP( 172 ) = ATRPNJ
      EBISP( 173 ) = AHOMJ
      EBISP( 174 ) = AHONITJ
      EBISP( 175 ) = STY
      EBISP( 176 ) = STYP


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
      RTOL( TRPN        ) = 5.0D-04
      RTOL( HONIT       ) = 5.0D-04
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
      RTOL( XYL         ) = 5.0D-04
      RTOL( XYLP        ) = 5.0D-04
      RTOL( EBZ         ) = 5.0D-04
      RTOL( EBZP        ) = 5.0D-04
      RTOL( ISO         ) = 5.0D-04
      RTOL( ISON        ) = 5.0D-04
      RTOL( INO2        ) = 5.0D-04
      RTOL( ISOP        ) = 5.0D-04
      RTOL( ISHP        ) = 5.0D-04
      RTOL( IEPOX       ) = 5.0D-04
      RTOL( IPX         ) = 5.0D-04
      RTOL( INALD       ) = 5.0D-04
      RTOL( ROH         ) = 5.0D-04
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
      RTOL( ETEG        ) = 5.0D-04
      RTOL( MAHP        ) = 5.0D-04
      RTOL( ORA2        ) = 5.0D-04
      RTOL( ORAP        ) = 5.0D-04
      RTOL( PPN         ) = 5.0D-04
      RTOL( MPAN        ) = 5.0D-04
      RTOL( MCTP        ) = 5.0D-04
      RTOL( OLNN        ) = 5.0D-04
      RTOL( OLND        ) = 5.0D-04
      RTOL( APINP1      ) = 5.0D-04
      RTOL( APINP2      ) = 5.0D-04
      RTOL( LIMNP1      ) = 5.0D-04
      RTOL( LIMNP2      ) = 5.0D-04
      RTOL( ADCN        ) = 5.0D-04
      RTOL( HOM         ) = 5.0D-04
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
      RTOL( AISO4J      ) = 5.0D-04
      RTOL( AISO5J      ) = 5.0D-04
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
      RTOL( ECH4        ) = 5.0D-04
      RTOL( ATRPNJ      ) = 5.0D-04
      RTOL( AHOMJ       ) = 5.0D-04
      RTOL( AHONITJ     ) = 5.0D-04
      RTOL( STY         ) = 5.0D-04
      RTOL( STYP        ) = 5.0D-04


      RETURN

      END
