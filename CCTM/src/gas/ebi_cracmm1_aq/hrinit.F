
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
C  REVISION HISTORY: Created by EBI solver program, Dec  3, 2021
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
     &           LAND   ( MAX_CELLS_EBI ) ,
     &           STAT = IOS )

       IF ( IOS .NE. 0 ) THEN
          MSG = 'Error allocating ATMPRES, H2O, TEMP, HET, RJIN, LAND '
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

      N_EBISP  = 181
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
      EBISP(  30 ) = PAA
      EBISP(  31 ) = ONIT
      EBISP(  32 ) = SO2
      EBISP(  33 ) = SULF
      EBISP(  34 ) = SULRXN
      EBISP(  35 ) = ETH
      EBISP(  36 ) = HC3
      EBISP(  37 ) = HC3P
      EBISP(  38 ) = ASOATJ
      EBISP(  39 ) = HC5
      EBISP(  40 ) = HC5P
      EBISP(  41 ) = HC8
      EBISP(  42 ) = HC8P
      EBISP(  43 ) = ETE
      EBISP(  44 ) = ETEP
      EBISP(  45 ) = OLT
      EBISP(  46 ) = OLTP
      EBISP(  47 ) = OLI
      EBISP(  48 ) = OLIP
      EBISP(  49 ) = ACE
      EBISP(  50 ) = ORA1
      EBISP(  51 ) = BEN
      EBISP(  52 ) = BENP
      EBISP(  53 ) = EPX
      EBISP(  54 ) = PHEN
      EBISP(  55 ) = BENZRO2
      EBISP(  56 ) = TOL
      EBISP(  57 ) = TR2
      EBISP(  58 ) = TLP1
      EBISP(  59 ) = CSL
      EBISP(  60 ) = TOLRO2
      EBISP(  61 ) = XYM
      EBISP(  62 ) = XY2
      EBISP(  63 ) = XYL1
      EBISP(  64 ) = XYLRO2
      EBISP(  65 ) = XYE
      EBISP(  66 ) = ISO
      EBISP(  67 ) = ISOP
      EBISP(  68 ) = API
      EBISP(  69 ) = APIP1
      EBISP(  70 ) = APIP2
      EBISP(  71 ) = LIM
      EBISP(  72 ) = LIMP1
      EBISP(  73 ) = LIMP2
      EBISP(  74 ) = PINALP
      EBISP(  75 ) = LIMALP
      EBISP(  76 ) = RCO3
      EBISP(  77 ) = ACTP
      EBISP(  78 ) = MEKP
      EBISP(  79 ) = KETP
      EBISP(  80 ) = MCP
      EBISP(  81 ) = MVKP
      EBISP(  82 ) = UALP
      EBISP(  83 ) = DCB3
      EBISP(  84 ) = BALP
      EBISP(  85 ) = ADDC
      EBISP(  86 ) = MCT
      EBISP(  87 ) = MCTO
      EBISP(  88 ) = MOH
      EBISP(  89 ) = EOH
      EBISP(  90 ) = ROH
      EBISP(  91 ) = ETEG
      EBISP(  92 ) = ISHP
      EBISP(  93 ) = IEPOX
      EBISP(  94 ) = MAHP
      EBISP(  95 ) = ORA2
      EBISP(  96 ) = ORAP
      EBISP(  97 ) = PPN
      EBISP(  98 ) = MPAN
      EBISP(  99 ) = TRPN
      EBISP( 100 ) = HOM
      EBISP( 101 ) = NALD
      EBISP( 102 ) = ISON
      EBISP( 103 ) = MCTP
      EBISP( 104 ) = OLNN
      EBISP( 105 ) = OLND
      EBISP( 106 ) = APINP1
      EBISP( 107 ) = APINP2
      EBISP( 108 ) = LIMNP1
      EBISP( 109 ) = LIMNP2
      EBISP( 110 ) = ADCN
      EBISP( 111 ) = TOLP
      EBISP( 112 ) = PER1
      EBISP( 113 ) = XYLP
      EBISP( 114 ) = PER2
      EBISP( 115 ) = XYO2
      EBISP( 116 ) = XYOP
      EBISP( 117 ) = BAL1
      EBISP( 118 ) = BAL2
      EBISP( 119 ) = ELHOM
      EBISP( 120 ) = TOLNRXN
      EBISP( 121 ) = TOLHRXN
      EBISP( 122 ) = XYLNRXN
      EBISP( 123 ) = XYLHRXN
      EBISP( 124 ) = BNZNRXN
      EBISP( 125 ) = BNZHRXN
      EBISP( 126 ) = SOAALK
      EBISP( 127 ) = ALKRXN
      EBISP( 128 ) = ROCIOXY
      EBISP( 129 ) = SLOWROC
      EBISP( 130 ) = NAPH
      EBISP( 131 ) = PAHRO2
      EBISP( 132 ) = PAHNRXN
      EBISP( 133 ) = PAHHRXN
      EBISP( 134 ) = ACRO
      EBISP( 135 ) = BDE13
      EBISP( 136 ) = FURAN
      EBISP( 137 ) = FURANO2
      EBISP( 138 ) = FURANONE
      EBISP( 139 ) = PROG
      EBISP( 140 ) = SESQ
      EBISP( 141 ) = SESQNRO2
      EBISP( 142 ) = VROCP0OXY
      EBISP( 143 ) = VROCP3OXY
      EBISP( 144 ) = VROCN2OXY
      EBISP( 145 ) = SESQRO2
      EBISP( 146 ) = AGLYJ
      EBISP( 147 ) = AISONJ
      EBISP( 148 ) = ATRPNJ
      EBISP( 149 ) = AISO3J
      EBISP( 150 ) = AXYL1J
      EBISP( 151 ) = AOLGAJ
      EBISP( 152 ) = AXYL2J
      EBISP( 153 ) = ATOL1J
      EBISP( 154 ) = ATOL2J
      EBISP( 155 ) = ABNZ1J
      EBISP( 156 ) = ABNZ2J
      EBISP( 157 ) = ATRP1J
      EBISP( 158 ) = AOLGBJ
      EBISP( 159 ) = ATRP2J
      EBISP( 160 ) = AISO1J
      EBISP( 161 ) = AISO2J
      EBISP( 162 ) = APAH1J
      EBISP( 163 ) = APAH2J
      EBISP( 164 ) = AALK1J
      EBISP( 165 ) = AALK2J
      EBISP( 166 ) = APOCI
      EBISP( 167 ) = APNCOMI
      EBISP( 168 ) = APOCJ
      EBISP( 169 ) = APNCOMJ
      EBISP( 170 ) = PCVOC
      EBISP( 171 ) = PCSOARXN
      EBISP( 172 ) = VLVPO1
      EBISP( 173 ) = VSVPO1
      EBISP( 174 ) = VSVPO2
      EBISP( 175 ) = VSVPO3
      EBISP( 176 ) = VIVPO1
      EBISP( 177 ) = VLVOO1
      EBISP( 178 ) = VLVOO2
      EBISP( 179 ) = VSVOO2
      EBISP( 180 ) = VSVOO3
      EBISP( 181 ) = VSVOO1


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set species tolerances
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      RTOL( O3        ) = 5.0D-04
      RTOL( O3P       ) = 1.0D+00
      RTOL( O1D       ) = 1.0D+00
      RTOL( H2O2      ) = 5.0D-04
      RTOL( HO        ) = 5.0D-04
      RTOL( NO2       ) = 5.0D-04
      RTOL( NO        ) = 5.0D-04
      RTOL( NO3       ) = 5.0D-04
      RTOL( HONO      ) = 5.0D-04
      RTOL( HNO3      ) = 5.0D-04
      RTOL( HNO4      ) = 5.0D-04
      RTOL( HO2       ) = 5.0D-04
      RTOL( HCHO      ) = 5.0D-04
      RTOL( CO        ) = 5.0D-04
      RTOL( ACD       ) = 5.0D-04
      RTOL( MO2       ) = 5.0D-04
      RTOL( ALD       ) = 5.0D-04
      RTOL( ETHP      ) = 5.0D-04
      RTOL( ACT       ) = 5.0D-04
      RTOL( ACO3      ) = 5.0D-04
      RTOL( UALD      ) = 5.0D-04
      RTOL( KET       ) = 5.0D-04
      RTOL( PINAL     ) = 5.0D-04
      RTOL( HC10P     ) = 5.0D-04
      RTOL( LIMAL     ) = 5.0D-04
      RTOL( MEK       ) = 5.0D-04
      RTOL( HKET      ) = 5.0D-04
      RTOL( MACR      ) = 5.0D-04
      RTOL( MACP      ) = 5.0D-04
      RTOL( XO2       ) = 5.0D-04
      RTOL( MVK       ) = 5.0D-04
      RTOL( GLY       ) = 5.0D-04
      RTOL( MGLY      ) = 5.0D-04
      RTOL( DCB1      ) = 5.0D-04
      RTOL( DCB2      ) = 5.0D-04
      RTOL( BALD      ) = 5.0D-04
      RTOL( CHO       ) = 5.0D-04
      RTOL( OP1       ) = 5.0D-04
      RTOL( OP2       ) = 5.0D-04
      RTOL( OPB       ) = 5.0D-04
      RTOL( PAA       ) = 5.0D-04
      RTOL( ONIT      ) = 5.0D-04
      RTOL( PAN       ) = 5.0D-04
      RTOL( N2O5      ) = 5.0D-04
      RTOL( SO2       ) = 5.0D-04
      RTOL( SULF      ) = 1.0D+00
      RTOL( SULRXN    ) = 1.0D+00
      RTOL( ETH       ) = 5.0D-04
      RTOL( HC3       ) = 5.0D-04
      RTOL( HC3P      ) = 5.0D-04
      RTOL( ASOATJ    ) = 5.0D-04
      RTOL( HC5       ) = 5.0D-04
      RTOL( HC5P      ) = 5.0D-04
      RTOL( HC8       ) = 5.0D-04
      RTOL( HC8P      ) = 5.0D-04
      RTOL( ETE       ) = 5.0D-04
      RTOL( ETEP      ) = 5.0D-04
      RTOL( OLT       ) = 5.0D-04
      RTOL( OLTP      ) = 5.0D-04
      RTOL( OLI       ) = 5.0D-04
      RTOL( OLIP      ) = 5.0D-04
      RTOL( ACE       ) = 5.0D-04
      RTOL( ORA1      ) = 5.0D-04
      RTOL( BEN       ) = 5.0D-04
      RTOL( BENP      ) = 5.0D-04
      RTOL( EPX       ) = 5.0D-04
      RTOL( PHEN      ) = 5.0D-04
      RTOL( BENZRO2   ) = 5.0D-04
      RTOL( TOL       ) = 5.0D-04
      RTOL( TR2       ) = 5.0D-04
      RTOL( TLP1      ) = 5.0D-04
      RTOL( CSL       ) = 5.0D-04
      RTOL( TOLRO2    ) = 5.0D-04
      RTOL( XYM       ) = 5.0D-04
      RTOL( XY2       ) = 5.0D-04
      RTOL( XYL1      ) = 5.0D-04
      RTOL( XYLRO2    ) = 5.0D-04
      RTOL( XYE       ) = 5.0D-04
      RTOL( ISO       ) = 5.0D-04
      RTOL( ISOP      ) = 5.0D-04
      RTOL( API       ) = 5.0D-04
      RTOL( APIP1     ) = 5.0D-04
      RTOL( APIP2     ) = 5.0D-04
      RTOL( LIM       ) = 5.0D-04
      RTOL( LIMP1     ) = 5.0D-04
      RTOL( LIMP2     ) = 5.0D-04
      RTOL( PINALP    ) = 5.0D-04
      RTOL( LIMALP    ) = 5.0D-04
      RTOL( RCO3      ) = 5.0D-04
      RTOL( ACTP      ) = 5.0D-04
      RTOL( MEKP      ) = 5.0D-04
      RTOL( KETP      ) = 5.0D-04
      RTOL( MCP       ) = 5.0D-04
      RTOL( MVKP      ) = 5.0D-04
      RTOL( UALP      ) = 5.0D-04
      RTOL( DCB3      ) = 5.0D-04
      RTOL( BALP      ) = 5.0D-04
      RTOL( ADDC      ) = 5.0D-04
      RTOL( MCT       ) = 5.0D-04
      RTOL( MCTO      ) = 5.0D-04
      RTOL( MOH       ) = 5.0D-04
      RTOL( EOH       ) = 5.0D-04
      RTOL( ROH       ) = 5.0D-04
      RTOL( ETEG      ) = 5.0D-04
      RTOL( ISHP      ) = 5.0D-04
      RTOL( IEPOX     ) = 5.0D-04
      RTOL( MAHP      ) = 5.0D-04
      RTOL( ORA2      ) = 5.0D-04
      RTOL( ORAP      ) = 5.0D-04
      RTOL( PPN       ) = 5.0D-04
      RTOL( MPAN      ) = 5.0D-04
      RTOL( TRPN      ) = 5.0D-04
      RTOL( HOM       ) = 5.0D-04
      RTOL( NALD      ) = 5.0D-04
      RTOL( ISON      ) = 5.0D-04
      RTOL( MCTP      ) = 5.0D-04
      RTOL( OLNN      ) = 5.0D-04
      RTOL( OLND      ) = 5.0D-04
      RTOL( APINP1    ) = 5.0D-04
      RTOL( APINP2    ) = 5.0D-04
      RTOL( LIMNP1    ) = 5.0D-04
      RTOL( LIMNP2    ) = 5.0D-04
      RTOL( ADCN      ) = 5.0D-04
      RTOL( TOLP      ) = 5.0D-04
      RTOL( PER1      ) = 5.0D-04
      RTOL( XYLP      ) = 5.0D-04
      RTOL( PER2      ) = 5.0D-04
      RTOL( XYO2      ) = 5.0D-04
      RTOL( XYOP      ) = 5.0D-04
      RTOL( BAL1      ) = 5.0D-04
      RTOL( BAL2      ) = 5.0D-04
      RTOL( ELHOM     ) = 5.0D-04
      RTOL( TOLNRXN   ) = 1.0D+00
      RTOL( TOLHRXN   ) = 1.0D+00
      RTOL( XYLNRXN   ) = 1.0D+00
      RTOL( XYLHRXN   ) = 1.0D+00
      RTOL( BNZNRXN   ) = 1.0D+00
      RTOL( BNZHRXN   ) = 1.0D+00
      RTOL( SOAALK    ) = 5.0D-04
      RTOL( ALKRXN    ) = 1.0D+00
      RTOL( ROCIOXY   ) = 5.0D-04
      RTOL( SLOWROC   ) = 5.0D-04
      RTOL( NAPH      ) = 5.0D-04
      RTOL( PAHRO2    ) = 1.0D+00
      RTOL( PAHNRXN   ) = 1.0D+00
      RTOL( PAHHRXN   ) = 1.0D+00
      RTOL( ACRO      ) = 5.0D-04
      RTOL( BDE13     ) = 5.0D-04
      RTOL( FURAN     ) = 5.0D-04
      RTOL( FURANO2   ) = 5.0D-04
      RTOL( FURANONE  ) = 5.0D-04
      RTOL( PROG      ) = 5.0D-04
      RTOL( SESQ      ) = 5.0D-04
      RTOL( SESQNRO2  ) = 5.0D-04
      RTOL( VROCP0OXY ) = 5.0D-04
      RTOL( VROCP3OXY ) = 5.0D-04
      RTOL( VROCN2OXY ) = 5.0D-04
      RTOL( SESQRO2   ) = 5.0D-04
      RTOL( AGLYJ     ) = 5.0D-04
      RTOL( AISONJ    ) = 5.0D-04
      RTOL( ATRPNJ    ) = 5.0D-04
      RTOL( AISO3J    ) = 5.0D-04
      RTOL( AXYL1J    ) = 5.0D-04
      RTOL( AOLGAJ    ) = 5.0D-04
      RTOL( AXYL2J    ) = 5.0D-04
      RTOL( ATOL1J    ) = 5.0D-04
      RTOL( ATOL2J    ) = 5.0D-04
      RTOL( ABNZ1J    ) = 5.0D-04
      RTOL( ABNZ2J    ) = 5.0D-04
      RTOL( ATRP1J    ) = 5.0D-04
      RTOL( AOLGBJ    ) = 5.0D-04
      RTOL( ATRP2J    ) = 5.0D-04
      RTOL( AISO1J    ) = 5.0D-04
      RTOL( AISO2J    ) = 5.0D-04
      RTOL( APAH1J    ) = 5.0D-04
      RTOL( APAH2J    ) = 5.0D-04
      RTOL( AALK1J    ) = 5.0D-04
      RTOL( AALK2J    ) = 5.0D-04
      RTOL( APOCI     ) = 5.0D-04
      RTOL( APNCOMI   ) = 5.0D-04
      RTOL( APOCJ     ) = 5.0D-04
      RTOL( APNCOMJ   ) = 5.0D-04
      RTOL( PCVOC     ) = 5.0D-04
      RTOL( PCSOARXN  ) = 5.0D-04
      RTOL( VLVPO1    ) = 5.0D-04
      RTOL( VSVPO1    ) = 5.0D-04
      RTOL( VSVPO2    ) = 5.0D-04
      RTOL( VSVPO3    ) = 5.0D-04
      RTOL( VIVPO1    ) = 5.0D-04
      RTOL( VLVOO1    ) = 5.0D-04
      RTOL( VLVOO2    ) = 5.0D-04
      RTOL( VSVOO2    ) = 5.0D-04
      RTOL( VSVOO3    ) = 5.0D-04
      RTOL( VSVOO1    ) = 5.0D-04


      RETURN

      END
