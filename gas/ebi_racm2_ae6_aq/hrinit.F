
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
C  REVISION HISTORY: Created by EBI solver program, March 12, 2015
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



c..Allocate and define ebi species

      N_EBISP  = 136
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
      EBISP( 116 ) = AXYL1J
      EBISP( 117 ) = AOLGAJ
      EBISP( 118 ) = AXYL2J
      EBISP( 119 ) = ATOL1J
      EBISP( 120 ) = ATOL2J
      EBISP( 121 ) = ABNZ1J
      EBISP( 122 ) = ABNZ2J
      EBISP( 123 ) = ATRP1J
      EBISP( 124 ) = AOLGBJ
      EBISP( 125 ) = ATRP2J
      EBISP( 126 ) = AISO1J
      EBISP( 127 ) = AISO2J
      EBISP( 128 ) = ASQTJ
      EBISP( 129 ) = APAH1J
      EBISP( 130 ) = APAH2J
      EBISP( 131 ) = AALK1J
      EBISP( 132 ) = AALK2J
      EBISP( 133 ) = APOCI
      EBISP( 134 ) = APNCOMI
      EBISP( 135 ) = APOCJ
      EBISP( 136 ) = APNCOMJ


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set species tolerances
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      RTOL( O3      ) = 1.0D-03
      RTOL( O3P     ) = 1.0D+00
      RTOL( O1D     ) = 1.0D+00
      RTOL( H2O2    ) = 1.0D-03
      RTOL( HO      ) = 1.0D-03
      RTOL( NO2     ) = 1.0D-03
      RTOL( NO      ) = 1.0D-03
      RTOL( NO3     ) = 1.0D-03
      RTOL( HONO    ) = 1.0D-03
      RTOL( HNO3    ) = 1.0D-03
      RTOL( HNO4    ) = 1.0D-03
      RTOL( HO2     ) = 1.0D-03
      RTOL( HCHO    ) = 1.0D-03
      RTOL( CO      ) = 1.0D-03
      RTOL( ACD     ) = 1.0D-03
      RTOL( MO2     ) = 1.0D-03
      RTOL( ALD     ) = 1.0D-03
      RTOL( ETHP    ) = 1.0D-03
      RTOL( ACT     ) = 1.0D-03
      RTOL( ACO3    ) = 1.0D-03
      RTOL( UALD    ) = 1.0D-03
      RTOL( KET     ) = 1.0D-03
      RTOL( MEK     ) = 1.0D-03
      RTOL( HKET    ) = 1.0D-03
      RTOL( MACR    ) = 1.0D-03
      RTOL( MACP    ) = 1.0D-03
      RTOL( XO2     ) = 1.0D-03
      RTOL( MVK     ) = 1.0D-03
      RTOL( GLY     ) = 1.0D-03
      RTOL( MGLY    ) = 1.0D-03
      RTOL( DCB1    ) = 1.0D-03
      RTOL( DCB2    ) = 1.0D-03
      RTOL( BALD    ) = 1.0D-03
      RTOL( CHO     ) = 1.0D-03
      RTOL( OP1     ) = 1.0D-03
      RTOL( OP2     ) = 1.0D-03
      RTOL( PAA     ) = 1.0D-03
      RTOL( ONIT    ) = 1.0D-03
      RTOL( PAN     ) = 1.0D-03
      RTOL( N2O5    ) = 1.0D-03
      RTOL( SO2     ) = 1.0D-03
      RTOL( SULF    ) = 1.0D+00
      RTOL( SULRXN  ) = 1.0D+00
      RTOL( ETH     ) = 1.0D-03
      RTOL( HC3     ) = 1.0D-03
      RTOL( HC3P    ) = 1.0D-03
      RTOL( HC5     ) = 1.0D-03
      RTOL( HC5P    ) = 1.0D-03
      RTOL( HC8     ) = 1.0D-03
      RTOL( HC8P    ) = 1.0D-03
      RTOL( ALK5RXN ) = 1.0D+00
      RTOL( ETE     ) = 1.0D-03
      RTOL( ETEP    ) = 1.0D-03
      RTOL( OLT     ) = 1.0D-03
      RTOL( OLTP    ) = 1.0D-03
      RTOL( OLI     ) = 1.0D-03
      RTOL( OLIP    ) = 1.0D-03
      RTOL( DIEN    ) = 1.0D-03
      RTOL( ACE     ) = 1.0D-03
      RTOL( ORA1    ) = 1.0D-03
      RTOL( BENZENE ) = 1.0D-03
      RTOL( BENP    ) = 1.0D-03
      RTOL( EPX     ) = 1.0D-03
      RTOL( PHEN    ) = 1.0D-03
      RTOL( BENZRO2 ) = 1.0D-03
      RTOL( TOL     ) = 1.0D-03
      RTOL( TR2     ) = 1.0D-03
      RTOL( TLP1    ) = 1.0D-03
      RTOL( CSL     ) = 1.0D-03
      RTOL( TOLRO2  ) = 1.0D-03
      RTOL( XYM     ) = 1.0D-03
      RTOL( XY2     ) = 1.0D-03
      RTOL( XYL1    ) = 1.0D-03
      RTOL( XYLRO2  ) = 1.0D-03
      RTOL( XYP     ) = 1.0D-03
      RTOL( XYO     ) = 1.0D-03
      RTOL( XYO2    ) = 1.0D-03
      RTOL( ISO     ) = 1.0D-03
      RTOL( ISOP    ) = 1.0D-03
      RTOL( ISOPRXN ) = 1.0D+00
      RTOL( API     ) = 1.0D-03
      RTOL( APIP    ) = 1.0D-03
      RTOL( TRPRXN  ) = 1.0D+00
      RTOL( LIM     ) = 1.0D-03
      RTOL( LIMP    ) = 1.0D-03
      RTOL( RCO3    ) = 1.0D-03
      RTOL( ACTP    ) = 1.0D-03
      RTOL( MEKP    ) = 1.0D-03
      RTOL( KETP    ) = 1.0D-03
      RTOL( MCP     ) = 1.0D-03
      RTOL( MVKP    ) = 1.0D-03
      RTOL( UALP    ) = 1.0D-03
      RTOL( DCB3    ) = 1.0D-03
      RTOL( BALP    ) = 1.0D-03
      RTOL( ADDC    ) = 1.0D-03
      RTOL( MCT     ) = 1.0D-03
      RTOL( MCTO    ) = 1.0D-03
      RTOL( MOH     ) = 1.0D-03
      RTOL( EOH     ) = 1.0D-03
      RTOL( ROH     ) = 1.0D-03
      RTOL( ETEG    ) = 1.0D-03
      RTOL( ISHP    ) = 1.0D-03
      RTOL( MAHP    ) = 1.0D-03
      RTOL( ORA2    ) = 1.0D-03
      RTOL( ORAP    ) = 1.0D-03
      RTOL( PPN     ) = 1.0D-03
      RTOL( MPAN    ) = 1.0D-03
      RTOL( NALD    ) = 1.0D-03
      RTOL( ISON    ) = 1.0D-03
      RTOL( MCTP    ) = 1.0D-03
      RTOL( OLNN    ) = 1.0D-03
      RTOL( OLND    ) = 1.0D-03
      RTOL( ADCN    ) = 1.0D-03
      RTOL( TOLP    ) = 1.0D-03
      RTOL( PER1    ) = 1.0D-03
      RTOL( XYLP    ) = 1.0D-03
      RTOL( PER2    ) = 1.0D-03
      RTOL( XYOP    ) = 1.0D-03
      RTOL( BAL1    ) = 1.0D-03
      RTOL( BAL2    ) = 1.0D-03
      RTOL( TOLNRXN ) = 1.0D+00
      RTOL( TOLHRXN ) = 1.0D+00
      RTOL( XYLNRXN ) = 1.0D+00
      RTOL( XYLHRXN ) = 1.0D+00
      RTOL( BNZNRXN ) = 1.0D+00
      RTOL( BNZHRXN ) = 1.0D+00
      RTOL( SESQ    ) = 1.0D-03
      RTOL( SESQRXN ) = 1.0D+00
      RTOL( AXYL1J  ) = 1.0D-03
      RTOL( AOLGAJ  ) = 1.0D-03
      RTOL( AXYL2J  ) = 1.0D-03
      RTOL( ATOL1J  ) = 1.0D-03
      RTOL( ATOL2J  ) = 1.0D-03
      RTOL( ABNZ1J  ) = 1.0D-03
      RTOL( ABNZ2J  ) = 1.0D-03
      RTOL( ATRP1J  ) = 1.0D-03
      RTOL( AOLGBJ  ) = 1.0D-03
      RTOL( ATRP2J  ) = 1.0D-03
      RTOL( AISO1J  ) = 1.0D-03
      RTOL( AISO2J  ) = 1.0D-03
      RTOL( ASQTJ   ) = 1.0D-03
      RTOL( APAH1J  ) = 1.0D-03
      RTOL( APAH2J  ) = 1.0D-03
      RTOL( AALK1J  ) = 1.0D-03
      RTOL( AALK2J  ) = 1.0D-03
      RTOL( APOCI   ) = 1.0D-03
      RTOL( APNCOMI ) = 1.0D-03
      RTOL( APOCJ   ) = 1.0D-03
      RTOL( APNCOMJ ) = 1.0D-03


      RETURN

      END
