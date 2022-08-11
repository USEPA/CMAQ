
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
C  PRECONDITIONS: For the SAPRC07TIC_AE7I_AQ mechanism
C
C  RETURN VALUES: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Jun  8, 2022
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

      N_EBISP  = 217
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
      EBISP(  81 ) = xMTNO3
      EBISP(  82 ) = MTNO3
      EBISP(  83 ) = IMACO3
      EBISP(  84 ) = yRAOOH
      EBISP(  85 ) = zMTNO3
      EBISP(  86 ) = xACROLEIN
      EBISP(  87 ) = ETHENE
      EBISP(  88 ) = PROPENE
      EBISP(  89 ) = BUTADIENE13
      EBISP(  90 ) = ISOPRENE
      EBISP(  91 ) = APIN
      EBISP(  92 ) = TRPRXN
      EBISP(  93 ) = ACETYLENE
      EBISP(  94 ) = BENZENE
      EBISP(  95 ) = BENZRO2
      EBISP(  96 ) = TOLUENE
      EBISP(  97 ) = TOLRO2
      EBISP(  98 ) = MXYL
      EBISP(  99 ) = XYLRO2
      EBISP( 100 ) = OXYL
      EBISP( 101 ) = PXYL
      EBISP( 102 ) = TMBENZ124
      EBISP( 103 ) = ETOH
      EBISP( 104 ) = ALK1
      EBISP( 105 ) = ALK2
      EBISP( 106 ) = ALK3
      EBISP( 107 ) = ALK4
      EBISP( 108 ) = ALK5
      EBISP( 109 ) = SOAALK
      EBISP( 110 ) = SVAVB2
      EBISP( 111 ) = SVAVB3
      EBISP( 112 ) = SVAVB4
      EBISP( 113 ) = OLE1
      EBISP( 114 ) = OLE2
      EBISP( 115 ) = ARO1
      EBISP( 116 ) = ARO2MN
      EBISP( 117 ) = NAPHTHAL
      EBISP( 118 ) = PAHRO2
      EBISP( 119 ) = TERP
      EBISP( 120 ) = TERPNRO2
      EBISP( 121 ) = SESQ
      EBISP( 122 ) = SESQRXN
      EBISP( 123 ) = CL2
      EBISP( 124 ) = CL
      EBISP( 125 ) = CLNO
      EBISP( 126 ) = CLONO
      EBISP( 127 ) = CLNO2
      EBISP( 128 ) = HCL
      EBISP( 129 ) = CLO
      EBISP( 130 ) = CLONO2
      EBISP( 131 ) = HOCL
      EBISP( 132 ) = xCL
      EBISP( 133 ) = xCLCCHO
      EBISP( 134 ) = xCLACET
      EBISP( 135 ) = CLCCHO
      EBISP( 136 ) = CLACET
      EBISP( 137 ) = CLCHO
      EBISP( 138 ) = SVAVB1
      EBISP( 139 ) = HCHO_PRIMARY
      EBISP( 140 ) = CCHO_PRIMARY
      EBISP( 141 ) = ACRO_PRIMARY
      EBISP( 142 ) = ISOPO2
      EBISP( 143 ) = ISOPRXN
      EBISP( 144 ) = ISOPND
      EBISP( 145 ) = ISOPNB
      EBISP( 146 ) = HC5
      EBISP( 147 ) = DIBOO
      EBISP( 148 ) = ISOPOOH
      EBISP( 149 ) = HPALD
      EBISP( 150 ) = HACET
      EBISP( 151 ) = NISOPO2
      EBISP( 152 ) = NIT1
      EBISP( 153 ) = NISOPOOH
      EBISP( 154 ) = HC5OO
      EBISP( 155 ) = DHMOB
      EBISP( 156 ) = ISOPNOOD
      EBISP( 157 ) = PROPNN
      EBISP( 158 ) = MVKN
      EBISP( 159 ) = ETHLN
      EBISP( 160 ) = RNO3I
      EBISP( 161 ) = ISOPNOOB
      EBISP( 162 ) = MACRN
      EBISP( 163 ) = NIT1NO3OOA
      EBISP( 164 ) = NIT1NO3OOB
      EBISP( 165 ) = ISOPNN
      EBISP( 166 ) = NIT1OHOO
      EBISP( 167 ) = MVKOO
      EBISP( 168 ) = MACROO
      EBISP( 169 ) = PYRUACD
      EBISP( 170 ) = IEPOX
      EBISP( 171 ) = IEPOXOO
      EBISP( 172 ) = IMPAA
      EBISP( 173 ) = IMAPAN
      EBISP( 174 ) = IMAE
      EBISP( 175 ) = IHMML
      EBISP( 176 ) = H2NO3PIJ
      EBISP( 177 ) = H2NO3PK
      EBISP( 178 ) = ACLI
      EBISP( 179 ) = ACLJ
      EBISP( 180 ) = ACLK
      EBISP( 181 ) = IEPOXP
      EBISP( 182 ) = IMAEP
      EBISP( 183 ) = IHMMLP
      EBISP( 184 ) = AIETETJ
      EBISP( 185 ) = ASO4J
      EBISP( 186 ) = AIEOSJ
      EBISP( 187 ) = ADIMJ
      EBISP( 188 ) = AIMGAJ
      EBISP( 189 ) = AIMOSJ
      EBISP( 190 ) = AISO1J
      EBISP( 191 ) = AOLGBJ
      EBISP( 192 ) = AISO2J
      EBISP( 193 ) = ASQTJ
      EBISP( 194 ) = AAVB2J
      EBISP( 195 ) = AOLGAJ
      EBISP( 196 ) = AAVB3J
      EBISP( 197 ) = AAVB4J
      EBISP( 198 ) = APOCI
      EBISP( 199 ) = APNCOMI
      EBISP( 200 ) = APOCJ
      EBISP( 201 ) = APNCOMJ
      EBISP( 202 ) = PCVOC
      EBISP( 203 ) = PCSOARXN
      EBISP( 204 ) = VLVPO1
      EBISP( 205 ) = VSVPO1
      EBISP( 206 ) = VSVPO2
      EBISP( 207 ) = VSVPO3
      EBISP( 208 ) = VIVPO1
      EBISP( 209 ) = VLVOO1
      EBISP( 210 ) = VLVOO2
      EBISP( 211 ) = VSVOO2
      EBISP( 212 ) = VSVOO3
      EBISP( 213 ) = VSVOO1
      EBISP( 214 ) = AMTNO3J
      EBISP( 215 ) = AMTHYDJ
      EBISP( 216 ) = AISOPNNJ
      EBISP( 217 ) = AGLYJ


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set species tolerances
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      RTOL( NO2          ) = 5.0D-04
      RTOL( NO           ) = 5.0D-04
      RTOL( O3P          ) = 1.0D+00
      RTOL( O3           ) = 5.0D-04
      RTOL( NO3          ) = 5.0D-04
      RTOL( N2O5         ) = 5.0D-04
      RTOL( HNO3         ) = 5.0D-04
      RTOL( O1D          ) = 1.0D+00
      RTOL( OH           ) = 5.0D-04
      RTOL( HONO         ) = 5.0D-04
      RTOL( HO2          ) = 5.0D-04
      RTOL( CO           ) = 5.0D-04
      RTOL( CO2          ) = 5.0D-04
      RTOL( HNO4         ) = 5.0D-04
      RTOL( HO2H         ) = 5.0D-04
      RTOL( SO2          ) = 5.0D-04
      RTOL( SULF         ) = 1.0D+00
      RTOL( SULRXN       ) = 1.0D+00
      RTOL( MEO2         ) = 5.0D-04
      RTOL( HCHO         ) = 5.0D-04
      RTOL( COOH         ) = 5.0D-04
      RTOL( MEOH         ) = 5.0D-04
      RTOL( RO2C         ) = 5.0D-04
      RTOL( RO2XC        ) = 5.0D-04
      RTOL( MECO3        ) = 5.0D-04
      RTOL( PAN          ) = 5.0D-04
      RTOL( CCOOOH       ) = 5.0D-04
      RTOL( CCOOH        ) = 5.0D-04
      RTOL( RCO3         ) = 5.0D-04
      RTOL( PAN2         ) = 5.0D-04
      RTOL( xHO2         ) = 5.0D-04
      RTOL( yROOH        ) = 5.0D-04
      RTOL( xCCHO        ) = 5.0D-04
      RTOL( RCOOOH       ) = 5.0D-04
      RTOL( RCOOH        ) = 5.0D-04
      RTOL( BZCO3        ) = 5.0D-04
      RTOL( PBZN         ) = 5.0D-04
      RTOL( BZO          ) = 5.0D-04
      RTOL( MACO3        ) = 5.0D-04
      RTOL( MAPAN        ) = 5.0D-04
      RTOL( TBUO         ) = 5.0D-04
      RTOL( RNO3         ) = 5.0D-04
      RTOL( ACETONE      ) = 5.0D-04
      RTOL( NPHE         ) = 5.0D-04
      RTOL( CRES         ) = 5.0D-04
      RTOL( xOH          ) = 5.0D-04
      RTOL( xNO2         ) = 5.0D-04
      RTOL( xMEO2        ) = 5.0D-04
      RTOL( xMECO3       ) = 5.0D-04
      RTOL( xRCO3        ) = 5.0D-04
      RTOL( xMACO3       ) = 5.0D-04
      RTOL( xTBUO        ) = 5.0D-04
      RTOL( xCO          ) = 5.0D-04
      RTOL( CCHO         ) = 5.0D-04
      RTOL( RCHO         ) = 5.0D-04
      RTOL( xHCHO        ) = 5.0D-04
      RTOL( MEK          ) = 5.0D-04
      RTOL( zRNO3        ) = 5.0D-04
      RTOL( xRCHO        ) = 5.0D-04
      RTOL( HCOOH        ) = 5.0D-04
      RTOL( xMGLY        ) = 5.0D-04
      RTOL( xBACL        ) = 5.0D-04
      RTOL( ROOH         ) = 5.0D-04
      RTOL( xPROD2       ) = 5.0D-04
      RTOL( R6OOH        ) = 5.0D-04
      RTOL( PRD2         ) = 5.0D-04
      RTOL( yR6OOH       ) = 5.0D-04
      RTOL( RAOOH        ) = 5.0D-04
      RTOL( MGLY         ) = 5.0D-04
      RTOL( IPRD         ) = 5.0D-04
      RTOL( xGLY         ) = 5.0D-04
      RTOL( xMEK         ) = 5.0D-04
      RTOL( xAFG1        ) = 5.0D-04
      RTOL( xAFG2        ) = 5.0D-04
      RTOL( GLY          ) = 5.0D-04
      RTOL( AFG1         ) = 5.0D-04
      RTOL( AFG2         ) = 5.0D-04
      RTOL( HCOCO3       ) = 5.0D-04
      RTOL( BACL         ) = 5.0D-04
      RTOL( BALD         ) = 5.0D-04
      RTOL( AFG3         ) = 5.0D-04
      RTOL( xIPRD        ) = 5.0D-04
      RTOL( MACR         ) = 5.0D-04
      RTOL( MVK          ) = 5.0D-04
      RTOL( xHOCCHO      ) = 5.0D-04
      RTOL( xRNO3        ) = 5.0D-04
      RTOL( HOCCHO       ) = 5.0D-04
      RTOL( xACETONE     ) = 5.0D-04
      RTOL( ACROLEIN     ) = 5.0D-04
      RTOL( xBALD        ) = 5.0D-04
      RTOL( xAFG3        ) = 5.0D-04
      RTOL( xMACR        ) = 5.0D-04
      RTOL( xMVK         ) = 5.0D-04
      RTOL( xMTNO3       ) = 5.0D-04
      RTOL( MTNO3        ) = 5.0D-04
      RTOL( IMACO3       ) = 5.0D-04
      RTOL( yRAOOH       ) = 5.0D-04
      RTOL( zMTNO3       ) = 5.0D-04
      RTOL( xACROLEIN    ) = 5.0D-04
      RTOL( ETHENE       ) = 5.0D-04
      RTOL( PROPENE      ) = 5.0D-04
      RTOL( BUTADIENE13  ) = 5.0D-04
      RTOL( ISOPRENE     ) = 5.0D-04
      RTOL( APIN         ) = 5.0D-04
      RTOL( TRPRXN       ) = 1.0D+00
      RTOL( ACETYLENE    ) = 5.0D-04
      RTOL( BENZENE      ) = 5.0D-04
      RTOL( BENZRO2      ) = 5.0D-04
      RTOL( TOLUENE      ) = 5.0D-04
      RTOL( TOLRO2       ) = 5.0D-04
      RTOL( MXYL         ) = 5.0D-04
      RTOL( XYLRO2       ) = 5.0D-04
      RTOL( OXYL         ) = 5.0D-04
      RTOL( PXYL         ) = 5.0D-04
      RTOL( TMBENZ124    ) = 5.0D-04
      RTOL( ETOH         ) = 5.0D-04
      RTOL( ALK1         ) = 5.0D-04
      RTOL( ALK2         ) = 5.0D-04
      RTOL( ALK3         ) = 5.0D-04
      RTOL( ALK4         ) = 5.0D-04
      RTOL( ALK5         ) = 5.0D-04
      RTOL( SOAALK       ) = 5.0D-04
      RTOL( SVAVB2       ) = 5.0D-04
      RTOL( SVAVB3       ) = 5.0D-04
      RTOL( SVAVB4       ) = 5.0D-04
      RTOL( OLE1         ) = 5.0D-04
      RTOL( OLE2         ) = 5.0D-04
      RTOL( ARO1         ) = 5.0D-04
      RTOL( ARO2MN       ) = 5.0D-04
      RTOL( NAPHTHAL     ) = 5.0D-04
      RTOL( PAHRO2       ) = 1.0D+00
      RTOL( TERP         ) = 5.0D-04
      RTOL( TERPNRO2     ) = 5.0D-04
      RTOL( SESQ         ) = 5.0D-04
      RTOL( SESQRXN      ) = 1.0D+00
      RTOL( CL2          ) = 5.0D-04
      RTOL( CL           ) = 5.0D-04
      RTOL( CLNO         ) = 5.0D-04
      RTOL( CLONO        ) = 5.0D-04
      RTOL( CLNO2        ) = 5.0D-04
      RTOL( HCL          ) = 5.0D-04
      RTOL( CLO          ) = 5.0D-04
      RTOL( CLONO2       ) = 5.0D-04
      RTOL( HOCL         ) = 5.0D-04
      RTOL( xCL          ) = 5.0D-04
      RTOL( xCLCCHO      ) = 5.0D-04
      RTOL( xCLACET      ) = 5.0D-04
      RTOL( CLCCHO       ) = 5.0D-04
      RTOL( CLACET       ) = 5.0D-04
      RTOL( CLCHO        ) = 5.0D-04
      RTOL( SVAVB1       ) = 5.0D-04
      RTOL( HCHO_PRIMARY ) = 5.0D-04
      RTOL( CCHO_PRIMARY ) = 5.0D-04
      RTOL( ACRO_PRIMARY ) = 5.0D-04
      RTOL( ISOPO2       ) = 5.0D-04
      RTOL( ISOPRXN      ) = 1.0D+00
      RTOL( ISOPND       ) = 5.0D-04
      RTOL( ISOPNB       ) = 5.0D-04
      RTOL( HC5          ) = 5.0D-04
      RTOL( DIBOO        ) = 5.0D-04
      RTOL( ISOPOOH      ) = 5.0D-04
      RTOL( HPALD        ) = 5.0D-04
      RTOL( HACET        ) = 5.0D-04
      RTOL( NISOPO2      ) = 5.0D-04
      RTOL( NIT1         ) = 5.0D-04
      RTOL( NISOPOOH     ) = 5.0D-04
      RTOL( HC5OO        ) = 5.0D-04
      RTOL( DHMOB        ) = 5.0D-04
      RTOL( ISOPNOOD     ) = 5.0D-04
      RTOL( PROPNN       ) = 5.0D-04
      RTOL( MVKN         ) = 5.0D-04
      RTOL( ETHLN        ) = 5.0D-04
      RTOL( RNO3I        ) = 5.0D-04
      RTOL( ISOPNOOB     ) = 5.0D-04
      RTOL( MACRN        ) = 5.0D-04
      RTOL( NIT1NO3OOA   ) = 5.0D-04
      RTOL( NIT1NO3OOB   ) = 5.0D-04
      RTOL( ISOPNN       ) = 5.0D-04
      RTOL( NIT1OHOO     ) = 5.0D-04
      RTOL( MVKOO        ) = 5.0D-04
      RTOL( MACROO       ) = 5.0D-04
      RTOL( PYRUACD      ) = 5.0D-04
      RTOL( IEPOX        ) = 5.0D-04
      RTOL( IEPOXOO      ) = 5.0D-04
      RTOL( IMPAA        ) = 5.0D-04
      RTOL( IMAPAN       ) = 5.0D-04
      RTOL( IMAE         ) = 5.0D-04
      RTOL( IHMML        ) = 5.0D-04
      RTOL( H2NO3PIJ     ) = 5.0D-04
      RTOL( H2NO3PK      ) = 5.0D-04
      RTOL( ACLI         ) = 5.0D-04
      RTOL( ACLJ         ) = 5.0D-04
      RTOL( ACLK         ) = 5.0D-04
      RTOL( IEPOXP       ) = 5.0D-04
      RTOL( IMAEP        ) = 5.0D-04
      RTOL( IHMMLP       ) = 5.0D-04
      RTOL( AIETETJ      ) = 5.0D-04
      RTOL( ASO4J        ) = 5.0D-04
      RTOL( AIEOSJ       ) = 5.0D-04
      RTOL( ADIMJ        ) = 5.0D-04
      RTOL( AIMGAJ       ) = 5.0D-04
      RTOL( AIMOSJ       ) = 5.0D-04
      RTOL( AISO1J       ) = 5.0D-04
      RTOL( AOLGBJ       ) = 5.0D-04
      RTOL( AISO2J       ) = 5.0D-04
      RTOL( ASQTJ        ) = 5.0D-04
      RTOL( AAVB2J       ) = 5.0D-04
      RTOL( AOLGAJ       ) = 5.0D-04
      RTOL( AAVB3J       ) = 5.0D-04
      RTOL( AAVB4J       ) = 5.0D-04
      RTOL( APOCI        ) = 5.0D-04
      RTOL( APNCOMI      ) = 5.0D-04
      RTOL( APOCJ        ) = 5.0D-04
      RTOL( APNCOMJ      ) = 5.0D-04
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
      RTOL( AMTNO3J      ) = 5.0D-04
      RTOL( AMTHYDJ      ) = 5.0D-04
      RTOL( AISOPNNJ     ) = 5.0D-04
      RTOL( AGLYJ        ) = 5.0D-04


      RETURN

      END
