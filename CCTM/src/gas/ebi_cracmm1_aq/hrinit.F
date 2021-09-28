
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
C  REVISION HISTORY: Created by EBI solver program, Sep 28, 2021
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

      N_EBISP  = 160
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
      EBISP(  38 ) = ETE
      EBISP(  39 ) = ETEP
      EBISP(  40 ) = OLT
      EBISP(  41 ) = OLTP
      EBISP(  42 ) = OLI
      EBISP(  43 ) = OLIP
      EBISP(  44 ) = ACE
      EBISP(  45 ) = ORA1
      EBISP(  46 ) = BEN
      EBISP(  47 ) = BENP
      EBISP(  48 ) = EPX
      EBISP(  49 ) = PHEN
      EBISP(  50 ) = BENZRO2
      EBISP(  51 ) = TOL
      EBISP(  52 ) = TR2
      EBISP(  53 ) = TLP1
      EBISP(  54 ) = CSL
      EBISP(  55 ) = TOLRO2
      EBISP(  56 ) = XYM
      EBISP(  57 ) = XY2
      EBISP(  58 ) = XYL1
      EBISP(  59 ) = XYLRO2
      EBISP(  60 ) = XYE
      EBISP(  61 ) = ISO
      EBISP(  62 ) = ISOP
      EBISP(  63 ) = ISOPRXN
      EBISP(  64 ) = API
      EBISP(  65 ) = APIP
      EBISP(  66 ) = TRPRXN
      EBISP(  67 ) = LIM
      EBISP(  68 ) = LIMP
      EBISP(  69 ) = RCO3
      EBISP(  70 ) = ACTP
      EBISP(  71 ) = MEKP
      EBISP(  72 ) = KETP
      EBISP(  73 ) = MCP
      EBISP(  74 ) = MVKP
      EBISP(  75 ) = UALP
      EBISP(  76 ) = DCB3
      EBISP(  77 ) = BALP
      EBISP(  78 ) = ADDC
      EBISP(  79 ) = MCT
      EBISP(  80 ) = MCTO
      EBISP(  81 ) = MOH
      EBISP(  82 ) = EOH
      EBISP(  83 ) = ROH
      EBISP(  84 ) = ETEG
      EBISP(  85 ) = ISHP
      EBISP(  86 ) = IEPOX
      EBISP(  87 ) = MAHP
      EBISP(  88 ) = ORA2
      EBISP(  89 ) = ORAP
      EBISP(  90 ) = PPN
      EBISP(  91 ) = MPAN
      EBISP(  92 ) = NALD
      EBISP(  93 ) = ISON
      EBISP(  94 ) = MCTP
      EBISP(  95 ) = OLNN
      EBISP(  96 ) = OLND
      EBISP(  97 ) = ADCN
      EBISP(  98 ) = TOLP
      EBISP(  99 ) = PER1
      EBISP( 100 ) = XYLP
      EBISP( 101 ) = PER2
      EBISP( 102 ) = XYO2
      EBISP( 103 ) = XYOP
      EBISP( 104 ) = BAL1
      EBISP( 105 ) = BAL2
      EBISP( 106 ) = TOLNRXN
      EBISP( 107 ) = TOLHRXN
      EBISP( 108 ) = XYLNRXN
      EBISP( 109 ) = XYLHRXN
      EBISP( 110 ) = BNZNRXN
      EBISP( 111 ) = BNZHRXN
      EBISP( 112 ) = SESQ
      EBISP( 113 ) = SESQRXN
      EBISP( 114 ) = SOAALK
      EBISP( 115 ) = ALKRXN
      EBISP( 116 ) = ROCIOXY
      EBISP( 117 ) = ASOATJ
      EBISP( 118 ) = SLOWROC
      EBISP( 119 ) = NAPH
      EBISP( 120 ) = PAHRO2
      EBISP( 121 ) = PAHNRXN
      EBISP( 122 ) = PAHHRXN
      EBISP( 123 ) = ACRO
      EBISP( 124 ) = BDE13
      EBISP( 125 ) = FURAN
      EBISP( 126 ) = PROG
      EBISP( 127 ) = AISO3J
      EBISP( 128 ) = AXYL1J
      EBISP( 129 ) = AOLGAJ
      EBISP( 130 ) = AXYL2J
      EBISP( 131 ) = ATOL1J
      EBISP( 132 ) = ATOL2J
      EBISP( 133 ) = ABNZ1J
      EBISP( 134 ) = ABNZ2J
      EBISP( 135 ) = ATRP1J
      EBISP( 136 ) = AOLGBJ
      EBISP( 137 ) = ATRP2J
      EBISP( 138 ) = AISO1J
      EBISP( 139 ) = AISO2J
      EBISP( 140 ) = ASQTJ
      EBISP( 141 ) = APAH1J
      EBISP( 142 ) = APAH2J
      EBISP( 143 ) = AALK1J
      EBISP( 144 ) = AALK2J
      EBISP( 145 ) = APOCI
      EBISP( 146 ) = APNCOMI
      EBISP( 147 ) = APOCJ
      EBISP( 148 ) = APNCOMJ
      EBISP( 149 ) = PCVOC
      EBISP( 150 ) = PCSOARXN
      EBISP( 151 ) = VLVPO1
      EBISP( 152 ) = VSVPO1
      EBISP( 153 ) = VSVPO2
      EBISP( 154 ) = VSVPO3
      EBISP( 155 ) = VIVPO1
      EBISP( 156 ) = VLVOO1
      EBISP( 157 ) = VLVOO2
      EBISP( 158 ) = VSVOO2
      EBISP( 159 ) = VSVOO3
      EBISP( 160 ) = VSVOO1


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set species tolerances
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      RTOL( O3       ) = 5.0D-04
      RTOL( O3P      ) = 1.0D+00
      RTOL( O1D      ) = 1.0D+00
      RTOL( H2O2     ) = 5.0D-04
      RTOL( HO       ) = 5.0D-04
      RTOL( NO2      ) = 5.0D-04
      RTOL( NO       ) = 5.0D-04
      RTOL( NO3      ) = 5.0D-04
      RTOL( HONO     ) = 5.0D-04
      RTOL( HNO3     ) = 5.0D-04
      RTOL( HNO4     ) = 5.0D-04
      RTOL( HO2      ) = 5.0D-04
      RTOL( HCHO     ) = 5.0D-04
      RTOL( CO       ) = 5.0D-04
      RTOL( ACD      ) = 5.0D-04
      RTOL( MO2      ) = 5.0D-04
      RTOL( ALD      ) = 5.0D-04
      RTOL( ETHP     ) = 5.0D-04
      RTOL( ACT      ) = 5.0D-04
      RTOL( ACO3     ) = 5.0D-04
      RTOL( UALD     ) = 5.0D-04
      RTOL( KET      ) = 5.0D-04
      RTOL( MEK      ) = 5.0D-04
      RTOL( HKET     ) = 5.0D-04
      RTOL( MACR     ) = 5.0D-04
      RTOL( MACP     ) = 5.0D-04
      RTOL( XO2      ) = 5.0D-04
      RTOL( MVK      ) = 5.0D-04
      RTOL( GLY      ) = 5.0D-04
      RTOL( MGLY     ) = 5.0D-04
      RTOL( DCB1     ) = 5.0D-04
      RTOL( DCB2     ) = 5.0D-04
      RTOL( BALD     ) = 5.0D-04
      RTOL( CHO      ) = 5.0D-04
      RTOL( OP1      ) = 5.0D-04
      RTOL( OP2      ) = 5.0D-04
      RTOL( PAA      ) = 5.0D-04
      RTOL( ONIT     ) = 5.0D-04
      RTOL( PAN      ) = 5.0D-04
      RTOL( N2O5     ) = 5.0D-04
      RTOL( SO2      ) = 5.0D-04
      RTOL( SULF     ) = 1.0D+00
      RTOL( SULRXN   ) = 1.0D+00
      RTOL( ETH      ) = 5.0D-04
      RTOL( HC3      ) = 5.0D-04
      RTOL( HC3P     ) = 5.0D-04
      RTOL( HC5      ) = 5.0D-04
      RTOL( HC5P     ) = 5.0D-04
      RTOL( HC8      ) = 5.0D-04
      RTOL( HC8P     ) = 5.0D-04
      RTOL( ETE      ) = 5.0D-04
      RTOL( ETEP     ) = 5.0D-04
      RTOL( OLT      ) = 5.0D-04
      RTOL( OLTP     ) = 5.0D-04
      RTOL( OLI      ) = 5.0D-04
      RTOL( OLIP     ) = 5.0D-04
      RTOL( ACE      ) = 5.0D-04
      RTOL( ORA1     ) = 5.0D-04
      RTOL( BEN      ) = 5.0D-04
      RTOL( BENP     ) = 5.0D-04
      RTOL( EPX      ) = 5.0D-04
      RTOL( PHEN     ) = 5.0D-04
      RTOL( BENZRO2  ) = 5.0D-04
      RTOL( TOL      ) = 5.0D-04
      RTOL( TR2      ) = 5.0D-04
      RTOL( TLP1     ) = 5.0D-04
      RTOL( CSL      ) = 5.0D-04
      RTOL( TOLRO2   ) = 5.0D-04
      RTOL( XYM      ) = 5.0D-04
      RTOL( XY2      ) = 5.0D-04
      RTOL( XYL1     ) = 5.0D-04
      RTOL( XYLRO2   ) = 5.0D-04
      RTOL( XYE      ) = 5.0D-04
      RTOL( ISO      ) = 5.0D-04
      RTOL( ISOP     ) = 5.0D-04
      RTOL( ISOPRXN  ) = 1.0D+00
      RTOL( API      ) = 5.0D-04
      RTOL( APIP     ) = 5.0D-04
      RTOL( TRPRXN   ) = 1.0D+00
      RTOL( LIM      ) = 5.0D-04
      RTOL( LIMP     ) = 5.0D-04
      RTOL( RCO3     ) = 5.0D-04
      RTOL( ACTP     ) = 5.0D-04
      RTOL( MEKP     ) = 5.0D-04
      RTOL( KETP     ) = 5.0D-04
      RTOL( MCP      ) = 5.0D-04
      RTOL( MVKP     ) = 5.0D-04
      RTOL( UALP     ) = 5.0D-04
      RTOL( DCB3     ) = 5.0D-04
      RTOL( BALP     ) = 5.0D-04
      RTOL( ADDC     ) = 5.0D-04
      RTOL( MCT      ) = 5.0D-04
      RTOL( MCTO     ) = 5.0D-04
      RTOL( MOH      ) = 5.0D-04
      RTOL( EOH      ) = 5.0D-04
      RTOL( ROH      ) = 5.0D-04
      RTOL( ETEG     ) = 5.0D-04
      RTOL( ISHP     ) = 5.0D-04
      RTOL( IEPOX    ) = 5.0D-04
      RTOL( MAHP     ) = 5.0D-04
      RTOL( ORA2     ) = 5.0D-04
      RTOL( ORAP     ) = 5.0D-04
      RTOL( PPN      ) = 5.0D-04
      RTOL( MPAN     ) = 5.0D-04
      RTOL( NALD     ) = 5.0D-04
      RTOL( ISON     ) = 5.0D-04
      RTOL( MCTP     ) = 5.0D-04
      RTOL( OLNN     ) = 5.0D-04
      RTOL( OLND     ) = 5.0D-04
      RTOL( ADCN     ) = 5.0D-04
      RTOL( TOLP     ) = 5.0D-04
      RTOL( PER1     ) = 5.0D-04
      RTOL( XYLP     ) = 5.0D-04
      RTOL( PER2     ) = 5.0D-04
      RTOL( XYO2     ) = 5.0D-04
      RTOL( XYOP     ) = 5.0D-04
      RTOL( BAL1     ) = 5.0D-04
      RTOL( BAL2     ) = 5.0D-04
      RTOL( TOLNRXN  ) = 1.0D+00
      RTOL( TOLHRXN  ) = 1.0D+00
      RTOL( XYLNRXN  ) = 1.0D+00
      RTOL( XYLHRXN  ) = 1.0D+00
      RTOL( BNZNRXN  ) = 1.0D+00
      RTOL( BNZHRXN  ) = 1.0D+00
      RTOL( SESQ     ) = 5.0D-04
      RTOL( SESQRXN  ) = 1.0D+00
      RTOL( SOAALK   ) = 5.0D-04
      RTOL( ALKRXN   ) = 1.0D+00
      RTOL( ROCIOXY  ) = 5.0D-04
      RTOL( ASOATJ   ) = 5.0D-04
      RTOL( SLOWROC  ) = 5.0D-04
      RTOL( NAPH     ) = 5.0D-04
      RTOL( PAHRO2   ) = 1.0D+00
      RTOL( PAHNRXN  ) = 1.0D+00
      RTOL( PAHHRXN  ) = 1.0D+00
      RTOL( ACRO     ) = 5.0D-04
      RTOL( BDE13    ) = 5.0D-04
      RTOL( FURAN    ) = 5.0D-04
      RTOL( PROG     ) = 5.0D-04
      RTOL( AISO3J   ) = 5.0D-04
      RTOL( AXYL1J   ) = 5.0D-04
      RTOL( AOLGAJ   ) = 5.0D-04
      RTOL( AXYL2J   ) = 5.0D-04
      RTOL( ATOL1J   ) = 5.0D-04
      RTOL( ATOL2J   ) = 5.0D-04
      RTOL( ABNZ1J   ) = 5.0D-04
      RTOL( ABNZ2J   ) = 5.0D-04
      RTOL( ATRP1J   ) = 5.0D-04
      RTOL( AOLGBJ   ) = 5.0D-04
      RTOL( ATRP2J   ) = 5.0D-04
      RTOL( AISO1J   ) = 5.0D-04
      RTOL( AISO2J   ) = 5.0D-04
      RTOL( ASQTJ    ) = 5.0D-04
      RTOL( APAH1J   ) = 5.0D-04
      RTOL( APAH2J   ) = 5.0D-04
      RTOL( AALK1J   ) = 5.0D-04
      RTOL( AALK2J   ) = 5.0D-04
      RTOL( APOCI    ) = 5.0D-04
      RTOL( APNCOMI  ) = 5.0D-04
      RTOL( APOCJ    ) = 5.0D-04
      RTOL( APNCOMJ  ) = 5.0D-04
      RTOL( PCVOC    ) = 5.0D-04
      RTOL( PCSOARXN ) = 5.0D-04
      RTOL( VLVPO1   ) = 5.0D-04
      RTOL( VSVPO1   ) = 5.0D-04
      RTOL( VSVPO2   ) = 5.0D-04
      RTOL( VSVPO3   ) = 5.0D-04
      RTOL( VIVPO1   ) = 5.0D-04
      RTOL( VLVOO1   ) = 5.0D-04
      RTOL( VLVOO2   ) = 5.0D-04
      RTOL( VSVOO2   ) = 5.0D-04
      RTOL( VSVOO3   ) = 5.0D-04
      RTOL( VSVOO1   ) = 5.0D-04


      RETURN

      END
