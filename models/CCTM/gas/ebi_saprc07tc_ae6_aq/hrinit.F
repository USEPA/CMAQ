
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
C  PRECONDITIONS: For the SAPRC07TC family of mechanisms
C
C  RETURN VALUES: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Sep 22, 2015
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

      N_EBISP  = 173
      ALLOCATE( EBISP( N_EBISP ), STAT = IOS ) 
      IF ( IOS .NE. 0 ) THEN
         MSG = 'Error allocating EBISP' 
         CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      EBISP(   1 ) = HNO3
      EBISP(   2 ) = CO
      EBISP(   3 ) = CO2
      EBISP(   4 ) = HO2H
      EBISP(   5 ) = SO2
      EBISP(   6 ) = SULF
      EBISP(   7 ) = SULRXN
      EBISP(   8 ) = MEO2
      EBISP(   9 ) = HCHO
      EBISP(  10 ) = COOH
      EBISP(  11 ) = MEOH
      EBISP(  12 ) = RO2C
      EBISP(  13 ) = RO2XC
      EBISP(  14 ) = CCOOOH
      EBISP(  15 ) = CCOOH
      EBISP(  16 ) = RCO3
      EBISP(  17 ) = PAN2
      EBISP(  18 ) = xHO2
      EBISP(  19 ) = yROOH
      EBISP(  20 ) = xCCHO
      EBISP(  21 ) = RCOOOH
      EBISP(  22 ) = RCOOH
      EBISP(  23 ) = BZCO3
      EBISP(  24 ) = PBZN
      EBISP(  25 ) = BZO
      EBISP(  26 ) = MACO3
      EBISP(  27 ) = MAPAN
      EBISP(  28 ) = TBUO
      EBISP(  29 ) = RNO3
      EBISP(  30 ) = ACETONE
      EBISP(  31 ) = NPHE
      EBISP(  32 ) = CRES
      EBISP(  33 ) = xOH
      EBISP(  34 ) = xNO2
      EBISP(  35 ) = xMEO2
      EBISP(  36 ) = xMECO3
      EBISP(  37 ) = xRCO3
      EBISP(  38 ) = xMACO3
      EBISP(  39 ) = xTBUO
      EBISP(  40 ) = xCO
      EBISP(  41 ) = CCHO
      EBISP(  42 ) = RCHO
      EBISP(  43 ) = xHCHO
      EBISP(  44 ) = MEK
      EBISP(  45 ) = zRNO3
      EBISP(  46 ) = xRCHO
      EBISP(  47 ) = HCOOH
      EBISP(  48 ) = xMGLY
      EBISP(  49 ) = xBACL
      EBISP(  50 ) = ROOH
      EBISP(  51 ) = xPROD2
      EBISP(  52 ) = R6OOH
      EBISP(  53 ) = PRD2
      EBISP(  54 ) = yR6OOH
      EBISP(  55 ) = RAOOH
      EBISP(  56 ) = MGLY
      EBISP(  57 ) = IPRD
      EBISP(  58 ) = xGLY
      EBISP(  59 ) = xMEK
      EBISP(  60 ) = xAFG1
      EBISP(  61 ) = xAFG2
      EBISP(  62 ) = GLY
      EBISP(  63 ) = AFG1
      EBISP(  64 ) = AFG2
      EBISP(  65 ) = HCOCO3
      EBISP(  66 ) = BACL
      EBISP(  67 ) = BALD
      EBISP(  68 ) = AFG3
      EBISP(  69 ) = xIPRD
      EBISP(  70 ) = MACR
      EBISP(  71 ) = MVK
      EBISP(  72 ) = xHOCCHO
      EBISP(  73 ) = xRNO3
      EBISP(  74 ) = HOCCHO
      EBISP(  75 ) = xACETONE
      EBISP(  76 ) = ACROLEIN
      EBISP(  77 ) = xBALD
      EBISP(  78 ) = xAFG3
      EBISP(  79 ) = xMACR
      EBISP(  80 ) = xMVK
      EBISP(  81 ) = yISOPOOH
      EBISP(  82 ) = ISOPOOH
      EBISP(  83 ) = yRAOOH
      EBISP(  84 ) = xACROLEIN
      EBISP(  85 ) = ETHENE
      EBISP(  86 ) = PROPENE
      EBISP(  87 ) = BUTADIENE13
      EBISP(  88 ) = ISOPRENE
      EBISP(  89 ) = ISOPRXN
      EBISP(  90 ) = IEPOX
      EBISP(  91 ) = ARO2MN
      EBISP(  92 ) = IEPOXOO
      EBISP(  93 ) = APIN
      EBISP(  94 ) = TRPRXN
      EBISP(  95 ) = ACETYLENE
      EBISP(  96 ) = BENZENE
      EBISP(  97 ) = BENZRO2
      EBISP(  98 ) = TOLUENE
      EBISP(  99 ) = TOLRO2
      EBISP( 100 ) = MXYL
      EBISP( 101 ) = XYLRO2
      EBISP( 102 ) = OXYL
      EBISP( 103 ) = PXYL
      EBISP( 104 ) = TMBENZ124
      EBISP( 105 ) = ETOH
      EBISP( 106 ) = ALK1
      EBISP( 107 ) = ALK2
      EBISP( 108 ) = ALK3
      EBISP( 109 ) = ALK4
      EBISP( 110 ) = ALK5
      EBISP( 111 ) = SOAALK
      EBISP( 112 ) = ALKRXN
      EBISP( 113 ) = OLE1
      EBISP( 114 ) = OLE2
      EBISP( 115 ) = ARO1
      EBISP( 116 ) = NAPHTHAL
      EBISP( 117 ) = PAHRO2
      EBISP( 118 ) = TERP
      EBISP( 119 ) = SESQ
      EBISP( 120 ) = SESQRXN
      EBISP( 121 ) = CL2
      EBISP( 122 ) = CL
      EBISP( 123 ) = CLNO
      EBISP( 124 ) = CLONO
      EBISP( 125 ) = CLNO2
      EBISP( 126 ) = HCL
      EBISP( 127 ) = CLO
      EBISP( 128 ) = CLONO2
      EBISP( 129 ) = HOCL
      EBISP( 130 ) = xCL
      EBISP( 131 ) = xCLCCHO
      EBISP( 132 ) = xCLACET
      EBISP( 133 ) = CLCCHO
      EBISP( 134 ) = CLACET
      EBISP( 135 ) = CLCHO
      EBISP( 136 ) = BNZNRXN
      EBISP( 137 ) = BNZHRXN
      EBISP( 138 ) = XYLNRXN
      EBISP( 139 ) = XYLHRXN
      EBISP( 140 ) = TOLNRXN
      EBISP( 141 ) = TOLHRXN
      EBISP( 142 ) = PAHNRXN
      EBISP( 143 ) = PAHHRXN
      EBISP( 144 ) = HCHO_PRIMARY
      EBISP( 145 ) = CCHO_PRIMARY
      EBISP( 146 ) = ACRO_PRIMARY
      EBISP( 147 ) = H2NO3PIJ
      EBISP( 148 ) = H2NO3PK
      EBISP( 149 ) = ACLI
      EBISP( 150 ) = ACLJ
      EBISP( 151 ) = ACLK
      EBISP( 152 ) = AXYL1J
      EBISP( 153 ) = AOLGAJ
      EBISP( 154 ) = AXYL2J
      EBISP( 155 ) = ATOL1J
      EBISP( 156 ) = ATOL2J
      EBISP( 157 ) = ABNZ1J
      EBISP( 158 ) = ABNZ2J
      EBISP( 159 ) = ATRP1J
      EBISP( 160 ) = AOLGBJ
      EBISP( 161 ) = ATRP2J
      EBISP( 162 ) = AISO1J
      EBISP( 163 ) = AISO2J
      EBISP( 164 ) = ASQTJ
      EBISP( 165 ) = APAH1J
      EBISP( 166 ) = APAH2J
      EBISP( 167 ) = AALK1J
      EBISP( 168 ) = AALK2J
      EBISP( 169 ) = APOCI
      EBISP( 170 ) = APNCOMI
      EBISP( 171 ) = APOCJ
      EBISP( 172 ) = APNCOMJ
      EBISP( 173 ) = AISO3J


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set species tolerances
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      RTOL( NO2          ) = 1.0D-03
      RTOL( NO           ) = 1.0D-03
      RTOL( O3P          ) = 1.0D+00
      RTOL( O3           ) = 1.0D-03
      RTOL( NO3          ) = 1.0D-03
      RTOL( N2O5         ) = 1.0D-03
      RTOL( HNO3         ) = 1.0D-03
      RTOL( O1D          ) = 1.0D+00
      RTOL( OH           ) = 1.0D-03
      RTOL( HONO         ) = 1.0D-03
      RTOL( HO2          ) = 1.0D-03
      RTOL( CO           ) = 1.0D-03
      RTOL( CO2          ) = 1.0D-03
      RTOL( HNO4         ) = 1.0D-03
      RTOL( HO2H         ) = 1.0D-03
      RTOL( SO2          ) = 1.0D-03
      RTOL( SULF         ) = 1.0D+00
      RTOL( SULRXN       ) = 1.0D+00
      RTOL( MEO2         ) = 1.0D-03
      RTOL( HCHO         ) = 1.0D-03
      RTOL( COOH         ) = 1.0D-03
      RTOL( MEOH         ) = 1.0D-03
      RTOL( RO2C         ) = 1.0D-03
      RTOL( RO2XC        ) = 1.0D-03
      RTOL( MECO3        ) = 1.0D-03
      RTOL( PAN          ) = 1.0D-03
      RTOL( CCOOOH       ) = 1.0D-03
      RTOL( CCOOH        ) = 1.0D-03
      RTOL( RCO3         ) = 1.0D-03
      RTOL( PAN2         ) = 1.0D-03
      RTOL( xHO2         ) = 1.0D-03
      RTOL( yROOH        ) = 1.0D-03
      RTOL( xCCHO        ) = 1.0D-03
      RTOL( RCOOOH       ) = 1.0D-03
      RTOL( RCOOH        ) = 1.0D-03
      RTOL( BZCO3        ) = 1.0D-03
      RTOL( PBZN         ) = 1.0D-03
      RTOL( BZO          ) = 1.0D-03
      RTOL( MACO3        ) = 1.0D-03
      RTOL( MAPAN        ) = 1.0D-03
      RTOL( TBUO         ) = 1.0D-03
      RTOL( RNO3         ) = 1.0D-03
      RTOL( ACETONE      ) = 1.0D-03
      RTOL( NPHE         ) = 1.0D-03
      RTOL( CRES         ) = 1.0D-03
      RTOL( xOH          ) = 1.0D-03
      RTOL( xNO2         ) = 1.0D-03
      RTOL( xMEO2        ) = 1.0D-03
      RTOL( xMECO3       ) = 1.0D-03
      RTOL( xRCO3        ) = 1.0D-03
      RTOL( xMACO3       ) = 1.0D-03
      RTOL( xTBUO        ) = 1.0D-03
      RTOL( xCO          ) = 1.0D-03
      RTOL( CCHO         ) = 1.0D-03
      RTOL( RCHO         ) = 1.0D-03
      RTOL( xHCHO        ) = 1.0D-03
      RTOL( MEK          ) = 1.0D-03
      RTOL( zRNO3        ) = 1.0D-03
      RTOL( xRCHO        ) = 1.0D-03
      RTOL( HCOOH        ) = 1.0D-03
      RTOL( xMGLY        ) = 1.0D-03
      RTOL( xBACL        ) = 1.0D-03
      RTOL( ROOH         ) = 1.0D-03
      RTOL( xPROD2       ) = 1.0D-03
      RTOL( R6OOH        ) = 1.0D-03
      RTOL( PRD2         ) = 1.0D-03
      RTOL( yR6OOH       ) = 1.0D-03
      RTOL( RAOOH        ) = 1.0D-03
      RTOL( MGLY         ) = 1.0D-03
      RTOL( IPRD         ) = 1.0D-03
      RTOL( xGLY         ) = 1.0D-03
      RTOL( xMEK         ) = 1.0D-03
      RTOL( xAFG1        ) = 1.0D-03
      RTOL( xAFG2        ) = 1.0D-03
      RTOL( GLY          ) = 1.0D-03
      RTOL( AFG1         ) = 1.0D-03
      RTOL( AFG2         ) = 1.0D-03
      RTOL( HCOCO3       ) = 1.0D-03
      RTOL( BACL         ) = 1.0D-03
      RTOL( BALD         ) = 1.0D-03
      RTOL( AFG3         ) = 1.0D-03
      RTOL( xIPRD        ) = 1.0D-03
      RTOL( MACR         ) = 1.0D-03
      RTOL( MVK          ) = 1.0D-03
      RTOL( xHOCCHO      ) = 1.0D-03
      RTOL( xRNO3        ) = 1.0D-03
      RTOL( HOCCHO       ) = 1.0D-03
      RTOL( xACETONE     ) = 1.0D-03
      RTOL( ACROLEIN     ) = 1.0D-03
      RTOL( xBALD        ) = 1.0D-03
      RTOL( xAFG3        ) = 1.0D-03
      RTOL( xMACR        ) = 1.0D-03
      RTOL( xMVK         ) = 1.0D-03
      RTOL( yISOPOOH     ) = 1.0D-03
      RTOL( ISOPOOH      ) = 1.0D-03
      RTOL( yRAOOH       ) = 1.0D-03
      RTOL( xACROLEIN    ) = 1.0D-03
      RTOL( ETHENE       ) = 1.0D-03
      RTOL( PROPENE      ) = 1.0D-03
      RTOL( BUTADIENE13  ) = 1.0D-03
      RTOL( ISOPRENE     ) = 1.0D-03
      RTOL( ISOPRXN      ) = 1.0D+00
      RTOL( IEPOX        ) = 1.0D-03
      RTOL( ARO2MN       ) = 1.0D-03
      RTOL( IEPOXOO      ) = 1.0D-03
      RTOL( APIN         ) = 1.0D-03
      RTOL( TRPRXN       ) = 1.0D+00
      RTOL( ACETYLENE    ) = 1.0D-03
      RTOL( BENZENE      ) = 1.0D-03
      RTOL( BENZRO2      ) = 1.0D-03
      RTOL( TOLUENE      ) = 1.0D-03
      RTOL( TOLRO2       ) = 1.0D-03
      RTOL( MXYL         ) = 1.0D-03
      RTOL( XYLRO2       ) = 1.0D-03
      RTOL( OXYL         ) = 1.0D-03
      RTOL( PXYL         ) = 1.0D-03
      RTOL( TMBENZ124    ) = 1.0D-03
      RTOL( ETOH         ) = 1.0D-03
      RTOL( ALK1         ) = 1.0D-03
      RTOL( ALK2         ) = 1.0D-03
      RTOL( ALK3         ) = 1.0D-03
      RTOL( ALK4         ) = 1.0D-03
      RTOL( ALK5         ) = 1.0D-03
      RTOL( SOAALK       ) = 1.0D-03
      RTOL( ALKRXN       ) = 1.0D+00
      RTOL( OLE1         ) = 1.0D-03
      RTOL( OLE2         ) = 1.0D-03
      RTOL( ARO1         ) = 1.0D-03
      RTOL( NAPHTHAL     ) = 1.0D-03
      RTOL( PAHRO2       ) = 1.0D+00
      RTOL( TERP         ) = 1.0D-03
      RTOL( SESQ         ) = 1.0D-03
      RTOL( SESQRXN      ) = 1.0D+00
      RTOL( CL2          ) = 1.0D-03
      RTOL( CL           ) = 1.0D-03
      RTOL( CLNO         ) = 1.0D-03
      RTOL( CLONO        ) = 1.0D-03
      RTOL( CLNO2        ) = 1.0D-03
      RTOL( HCL          ) = 1.0D-03
      RTOL( CLO          ) = 1.0D-03
      RTOL( CLONO2       ) = 1.0D-03
      RTOL( HOCL         ) = 1.0D-03
      RTOL( xCL          ) = 1.0D-03
      RTOL( xCLCCHO      ) = 1.0D-03
      RTOL( xCLACET      ) = 1.0D-03
      RTOL( CLCCHO       ) = 1.0D-03
      RTOL( CLACET       ) = 1.0D-03
      RTOL( CLCHO        ) = 1.0D-03
      RTOL( BNZNRXN      ) = 1.0D+00
      RTOL( BNZHRXN      ) = 1.0D+00
      RTOL( XYLNRXN      ) = 1.0D+00
      RTOL( XYLHRXN      ) = 1.0D+00
      RTOL( TOLNRXN      ) = 1.0D+00
      RTOL( TOLHRXN      ) = 1.0D+00
      RTOL( PAHNRXN      ) = 1.0D+00
      RTOL( PAHHRXN      ) = 1.0D+00
      RTOL( HCHO_PRIMARY ) = 1.0D-03
      RTOL( CCHO_PRIMARY ) = 1.0D-03
      RTOL( ACRO_PRIMARY ) = 1.0D-03
      RTOL( H2NO3PIJ     ) = 1.0D-03
      RTOL( H2NO3PK      ) = 1.0D-03
      RTOL( ACLI         ) = 1.0D-03
      RTOL( ACLJ         ) = 1.0D-03
      RTOL( ACLK         ) = 1.0D-03
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
      RTOL( APOCI        ) = 1.0D-03
      RTOL( APNCOMI      ) = 1.0D-03
      RTOL( APOCJ        ) = 1.0D-03
      RTOL( APNCOMJ      ) = 1.0D-03
      RTOL( AISO3J       ) = 1.0D-03


      RETURN

      END
