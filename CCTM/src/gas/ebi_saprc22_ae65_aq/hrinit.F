
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
C  PRECONDITIONS: For the SAPRC22_AE65_AQ mechanism
C
C  RETURN VALUES: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, May 20, 2025
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

      N_EBISP  = 287
      ALLOCATE( EBISP( N_EBISP ), STAT = IOS ) 
      IF ( IOS .NE. 0 ) THEN
         MSG = 'Error allocating EBISP' 
         CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      EBISP(   1 ) = HNO3
      EBISP(   2 ) = HO2H
      EBISP(   3 ) = CO
      EBISP(   4 ) = CO2
      EBISP(   5 ) = SumRO2
      EBISP(   6 ) = SumRCO3
      EBISP(   7 ) = RO2C
      EBISP(   8 ) = RO2XC
      EBISP(   9 ) = MEO2
      EBISP(  10 ) = HCHO
      EBISP(  11 ) = MEOOH
      EBISP(  12 ) = MEOH
      EBISP(  13 ) = ETO2
      EBISP(  14 ) = MECHO
      EBISP(  15 ) = ROOH
      EBISP(  16 ) = ETOH
      EBISP(  17 ) = BZO2
      EBISP(  18 ) = BZO
      EBISP(  19 ) = OACID
      EBISP(  20 ) = PACID
      EBISP(  21 ) = BZCO3
      EBISP(  22 ) = PBZN
      EBISP(  23 ) = ALK5
      EBISP(  24 ) = TBUO
      EBISP(  25 ) = R1NO3
      EBISP(  26 ) = ACET
      EBISP(  27 ) = NPHE
      EBISP(  28 ) = CRES
      EBISP(  29 ) = NPRAD
      EBISP(  30 ) = NAPPRD
      EBISP(  31 ) = PNAMIN
      EBISP(  32 ) = NAMIN
      EBISP(  33 ) = AMINS
      EBISP(  34 ) = HCHO2
      EBISP(  35 ) = HCOOH
      EBISP(  36 ) = SO2
      EBISP(  37 ) = SULF
      EBISP(  38 ) = SULRXN
      EBISP(  39 ) = MECHO2
      EBISP(  40 ) = RCHO2
      EBISP(  41 ) = RCHO
      EBISP(  42 ) = GLY
      EBISP(  43 ) = BALD
      EBISP(  44 ) = PHEN
      EBISP(  45 ) = NAPS
      EBISP(  46 ) = CATL
      EBISP(  47 ) = AFG2A
      EBISP(  48 ) = AFG2B
      EBISP(  49 ) = MACO3
      EBISP(  50 ) = PAHRO2
      EBISP(  51 ) = CATL3
      EBISP(  52 ) = OTHN
      EBISP(  53 ) = PHOT
      EBISP(  54 ) = ALK3
      EBISP(  55 ) = IMINE
      EBISP(  56 ) = CLETHE
      EBISP(  57 ) = xHO2
      EBISP(  58 ) = xHCHO
      EBISP(  59 ) = yROOH
      EBISP(  60 ) = ACRLNT
      EBISP(  61 ) = PCE
      EBISP(  62 ) = PCLBEN
      EBISP(  63 ) = MECL2
      EBISP(  64 ) = ETBR2
      EBISP(  65 ) = ETCL2
      EBISP(  66 ) = ETOX
      EBISP(  67 ) = CHCL3
      EBISP(  68 ) = xOH
      EBISP(  69 ) = xNO2
      EBISP(  70 ) = xNO3
      EBISP(  71 ) = xGLY
      EBISP(  72 ) = xHCOOH
      EBISP(  73 ) = xMECHO
      EBISP(  74 ) = xETCHO
      EBISP(  75 ) = ETCHO
      EBISP(  76 ) = xGLCHO
      EBISP(  77 ) = GLCHO
      EBISP(  78 ) = xMEK
      EBISP(  79 ) = MEK
      EBISP(  80 ) = xACRO
      EBISP(  81 ) = ACRO
      EBISP(  82 ) = xACET
      EBISP(  83 ) = xMACR
      EBISP(  84 ) = MACR
      EBISP(  85 ) = xMVK
      EBISP(  86 ) = MVK
      EBISP(  87 ) = xBACL
      EBISP(  88 ) = BACL
      EBISP(  89 ) = xMGLY
      EBISP(  90 ) = MGLY
      EBISP(  91 ) = xBUDAL
      EBISP(  92 ) = BUDAL
      EBISP(  93 ) = xFURNS
      EBISP(  94 ) = FURNS
      EBISP(  95 ) = xBALD
      EBISP(  96 ) = xBENX
      EBISP(  97 ) = BENX
      EBISP(  98 ) = xRCHO
      EBISP(  99 ) = xKET2
      EBISP( 100 ) = KET2
      EBISP( 101 ) = xLVKS
      EBISP( 102 ) = LVKS
      EBISP( 103 ) = xOLEA1
      EBISP( 104 ) = OLEA1
      EBISP( 105 ) = xOLEA2
      EBISP( 106 ) = OLEA2
      EBISP( 107 ) = xOLEP
      EBISP( 108 ) = OLEP
      EBISP( 109 ) = xOACID
      EBISP( 110 ) = xPACID
      EBISP( 111 ) = xAMINS
      EBISP( 112 ) = xRPNO3
      EBISP( 113 ) = RPNO3
      EBISP( 114 ) = xRCNO3
      EBISP( 115 ) = RCNO3
      EBISP( 116 ) = xRHNO3
      EBISP( 117 ) = RHNO3
      EBISP( 118 ) = xRDNO3
      EBISP( 119 ) = RDNO3
      EBISP( 120 ) = xHPCRB
      EBISP( 121 ) = HPCRB
      EBISP( 122 ) = xAFG1
      EBISP( 123 ) = AFG1
      EBISP( 124 ) = xAFG2A
      EBISP( 125 ) = xAFG2B
      EBISP( 126 ) = xAFG3
      EBISP( 127 ) = AFG3
      EBISP( 128 ) = xPAN2
      EBISP( 129 ) = PAN2
      EBISP( 130 ) = xMEO2
      EBISP( 131 ) = xETO2
      EBISP( 132 ) = xMECO3
      EBISP( 133 ) = xR2CO3
      EBISP( 134 ) = R2CO3
      EBISP( 135 ) = xMACO3
      EBISP( 136 ) = xTBUO
      EBISP( 137 ) = xBZO
      EBISP( 138 ) = yRUOOH
      EBISP( 139 ) = RUOOH
      EBISP( 140 ) = yRAOOH
      EBISP( 141 ) = RAOOH
      EBISP( 142 ) = yHPCRB
      EBISP( 143 ) = yRPNO3
      EBISP( 144 ) = zR1NO3
      EBISP( 145 ) = zR2NO3
      EBISP( 146 ) = R2NO3
      EBISP( 147 ) = zRHNO3
      EBISP( 148 ) = zRCNO3
      EBISP( 149 ) = zRANO3
      EBISP( 150 ) = RANO3
      EBISP( 151 ) = zRPNO3
      EBISP( 152 ) = zRDNO3
      EBISP( 153 ) = zRNNO3
      EBISP( 154 ) = RNNO3
      EBISP( 155 ) = zPAN2
      EBISP( 156 ) = APANS
      EBISP( 157 ) = ETHAN
      EBISP( 158 ) = PROP
      EBISP( 159 ) = NC4
      EBISP( 160 ) = ETHEN
      EBISP( 161 ) = ETHEN_OP
      EBISP( 162 ) = NROG
      EBISP( 163 ) = PROPE
      EBISP( 164 ) = PROPE_O3
      EBISP( 165 ) = ALK2
      EBISP( 166 ) = ISOP
      EBISP( 167 ) = ISOP_OH
      EBISP( 168 ) = ISOP_O3
      EBISP( 169 ) = ISOPRXN
      EBISP( 170 ) = ISOP_N3
      EBISP( 171 ) = BUT13
      EBISP( 172 ) = BUT13_OH
      EBISP( 173 ) = BUT13_O3
      EBISP( 174 ) = APINE
      EBISP( 175 ) = APINE_OH
      EBISP( 176 ) = TRPRXN
      EBISP( 177 ) = ALK4
      EBISP( 178 ) = BPINE
      EBISP( 179 ) = BPINE_OH
      EBISP( 180 ) = ACETL
      EBISP( 181 ) = BENZ
      EBISP( 182 ) = BENZRO2
      EBISP( 183 ) = TOLU
      EBISP( 184 ) = TOLRO2
      EBISP( 185 ) = OXYL
      EBISP( 186 ) = XYNL
      EBISP( 187 ) = XYLRO2
      EBISP( 188 ) = MXYL
      EBISP( 189 ) = PXYL
      EBISP( 190 ) = BZ123
      EBISP( 191 ) = BZ124
      EBISP( 192 ) = BZ135
      EBISP( 193 ) = C2BEN
      EBISP( 194 ) = MTBE
      EBISP( 195 ) = ALK1
      EBISP( 196 ) = MECHO_OH
      EBISP( 197 ) = GLCHO_HV
      EBISP( 198 ) = ACRO_OH
      EBISP( 199 ) = ACRO_HV
      EBISP( 200 ) = MACR_OH
      EBISP( 201 ) = MACR_N3
      EBISP( 202 ) = BUDAL_OH
      EBISP( 203 ) = MALAH
      EBISP( 204 ) = ALK5_OH
      EBISP( 205 ) = ALK6
      EBISP( 206 ) = ALK6_OH
      EBISP( 207 ) = OLE1
      EBISP( 208 ) = OLE2
      EBISP( 209 ) = OLE2_O3
      EBISP( 210 ) = OLE3
      EBISP( 211 ) = OLE4
      EBISP( 212 ) = OLE4_O3
      EBISP( 213 ) = TERP
      EBISP( 214 ) = TERP_OH
      EBISP( 215 ) = TERP_O3
      EBISP( 216 ) = TERP_N3
      EBISP( 217 ) = SESQ
      EBISP( 218 ) = SESQ_OH
      EBISP( 219 ) = SESQRXN
      EBISP( 220 ) = SESQ_O3
      EBISP( 221 ) = SESQ_N3
      EBISP( 222 ) = ARO1
      EBISP( 223 ) = ARO2
      EBISP( 224 ) = ARO2_OH
      EBISP( 225 ) = STYRS
      EBISP( 226 ) = TAMNS
      EBISP( 227 ) = OLEA1_OH
      EBISP( 228 ) = OLEA1_N3
      EBISP( 229 ) = OLEA2_OH
      EBISP( 230 ) = OLEA2_O3
      EBISP( 231 ) = OLEA2_N3
      EBISP( 232 ) = OLEA2_HV
      EBISP( 233 ) = LVKS_OH
      EBISP( 234 ) = LVKS_O3
      EBISP( 235 ) = OLEP_OH
      EBISP( 236 ) = OLEP_O3
      EBISP( 237 ) = RCNO3_OH
      EBISP( 238 ) = RCNO3_HV
      EBISP( 239 ) = RPNO3_OH
      EBISP( 240 ) = RPNO3_HV
      EBISP( 241 ) = RDNO3_HV
      EBISP( 242 ) = R2NO3_HV
      EBISP( 243 ) = RUOOH_OH
      EBISP( 244 ) = HPCRB_OH
      EBISP( 245 ) = ROOH_OH
      EBISP( 246 ) = AFG1_OH
      EBISP( 247 ) = AFG1_HV
      EBISP( 248 ) = AFG2A_OH
      EBISP( 249 ) = AFG2B_OH
      EBISP( 250 ) = AFG2B_HV
      EBISP( 251 ) = SOAALK
      EBISP( 252 ) = SVAVB2
      EBISP( 253 ) = SVAVB3
      EBISP( 254 ) = SVAVB4
      EBISP( 255 ) = SVAVB1
      EBISP( 256 ) = H2NO3PIJ
      EBISP( 257 ) = H2NO3PK
      EBISP( 258 ) = AISO1J
      EBISP( 259 ) = AOLGBJ
      EBISP( 260 ) = AISO2J
      EBISP( 261 ) = ASQTJ
      EBISP( 262 ) = ATRP1J
      EBISP( 263 ) = ATRP2J
      EBISP( 264 ) = AAVB2J
      EBISP( 265 ) = AOLGAJ
      EBISP( 266 ) = AAVB3J
      EBISP( 267 ) = AAVB4J
      EBISP( 268 ) = APOCI
      EBISP( 269 ) = APNCOMI
      EBISP( 270 ) = APOCJ
      EBISP( 271 ) = APNCOMJ
      EBISP( 272 ) = PCVOC
      EBISP( 273 ) = PCSOARXN
      EBISP( 274 ) = VLVPO1
      EBISP( 275 ) = VSVPO1
      EBISP( 276 ) = VSVPO2
      EBISP( 277 ) = VSVPO3
      EBISP( 278 ) = VIVPO1
      EBISP( 279 ) = VLVOO1
      EBISP( 280 ) = VLVOO2
      EBISP( 281 ) = VSVOO2
      EBISP( 282 ) = VSVOO3
      EBISP( 283 ) = VSVOO1
      EBISP( 284 ) = AGLYJ
      EBISP( 285 ) = HCHO_PRIMARY
      EBISP( 286 ) = CCHO_PRIMARY
      EBISP( 287 ) = ACRO_PRIMARY


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
      RTOL( HNO4         ) = 5.0D-04
      RTOL( HO2H         ) = 5.0D-04
      RTOL( CO           ) = 5.0D-04
      RTOL( CO2          ) = 5.0D-04
      RTOL( SumRO2       ) = 5.0D-04
      RTOL( SumRCO3      ) = 5.0D-04
      RTOL( RO2C         ) = 5.0D-04
      RTOL( RO2XC        ) = 5.0D-04
      RTOL( MEO2         ) = 5.0D-04
      RTOL( HCHO         ) = 5.0D-04
      RTOL( MEOOH        ) = 5.0D-04
      RTOL( MEOH         ) = 5.0D-04
      RTOL( ETO2         ) = 5.0D-04
      RTOL( MECHO        ) = 5.0D-04
      RTOL( ROOH         ) = 5.0D-04
      RTOL( ETOH         ) = 5.0D-04
      RTOL( BZO2         ) = 5.0D-04
      RTOL( BZO          ) = 5.0D-04
      RTOL( MECO3        ) = 5.0D-04
      RTOL( PAN          ) = 5.0D-04
      RTOL( OACID        ) = 5.0D-04
      RTOL( PACID        ) = 5.0D-04
      RTOL( BZCO3        ) = 5.0D-04
      RTOL( PBZN         ) = 5.0D-04
      RTOL( ALK5         ) = 5.0D-04
      RTOL( TBUO         ) = 5.0D-04
      RTOL( R1NO3        ) = 5.0D-04
      RTOL( ACET         ) = 5.0D-04
      RTOL( NPHE         ) = 5.0D-04
      RTOL( CRES         ) = 5.0D-04
      RTOL( NPRAD        ) = 5.0D-04
      RTOL( NAPPRD       ) = 5.0D-04
      RTOL( PNAMIN       ) = 5.0D-04
      RTOL( NAMIN        ) = 5.0D-04
      RTOL( AMINS        ) = 5.0D-04
      RTOL( HCHO2        ) = 5.0D-04
      RTOL( HCOOH        ) = 5.0D-04
      RTOL( SO2          ) = 5.0D-04
      RTOL( SULF         ) = 1.0D+00
      RTOL( SULRXN       ) = 1.0D+00
      RTOL( MECHO2       ) = 5.0D-04
      RTOL( RCHO2        ) = 5.0D-04
      RTOL( RCHO         ) = 5.0D-04
      RTOL( GLY          ) = 5.0D-04
      RTOL( BALD         ) = 5.0D-04
      RTOL( PHEN         ) = 5.0D-04
      RTOL( NAPS         ) = 5.0D-04
      RTOL( CATL         ) = 5.0D-04
      RTOL( AFG2A        ) = 5.0D-04
      RTOL( AFG2B        ) = 5.0D-04
      RTOL( MACO3        ) = 5.0D-04
      RTOL( PAHRO2       ) = 1.0D+00
      RTOL( CATL3        ) = 5.0D-04
      RTOL( OTHN         ) = 5.0D-04
      RTOL( PHOT         ) = 5.0D-04
      RTOL( ALK3         ) = 5.0D-04
      RTOL( IMINE        ) = 5.0D-04
      RTOL( CLETHE       ) = 5.0D-04
      RTOL( xHO2         ) = 5.0D-04
      RTOL( xHCHO        ) = 5.0D-04
      RTOL( yROOH        ) = 5.0D-04
      RTOL( ACRLNT       ) = 5.0D-04
      RTOL( PCE          ) = 5.0D-04
      RTOL( PCLBEN       ) = 5.0D-04
      RTOL( MECL2        ) = 5.0D-04
      RTOL( ETBR2        ) = 5.0D-04
      RTOL( ETCL2        ) = 5.0D-04
      RTOL( ETOX         ) = 5.0D-04
      RTOL( CHCL3        ) = 5.0D-04
      RTOL( xOH          ) = 5.0D-04
      RTOL( xNO2         ) = 5.0D-04
      RTOL( xNO3         ) = 5.0D-04
      RTOL( xGLY         ) = 5.0D-04
      RTOL( xHCOOH       ) = 5.0D-04
      RTOL( xMECHO       ) = 5.0D-04
      RTOL( xETCHO       ) = 5.0D-04
      RTOL( ETCHO        ) = 5.0D-04
      RTOL( xGLCHO       ) = 5.0D-04
      RTOL( GLCHO        ) = 5.0D-04
      RTOL( xMEK         ) = 5.0D-04
      RTOL( MEK          ) = 5.0D-04
      RTOL( xACRO        ) = 5.0D-04
      RTOL( ACRO         ) = 5.0D-04
      RTOL( xACET        ) = 5.0D-04
      RTOL( xMACR        ) = 5.0D-04
      RTOL( MACR         ) = 5.0D-04
      RTOL( xMVK         ) = 5.0D-04
      RTOL( MVK          ) = 5.0D-04
      RTOL( xBACL        ) = 5.0D-04
      RTOL( BACL         ) = 5.0D-04
      RTOL( xMGLY        ) = 5.0D-04
      RTOL( MGLY         ) = 5.0D-04
      RTOL( xBUDAL       ) = 5.0D-04
      RTOL( BUDAL        ) = 5.0D-04
      RTOL( xFURNS       ) = 5.0D-04
      RTOL( FURNS        ) = 5.0D-04
      RTOL( xBALD        ) = 5.0D-04
      RTOL( xBENX        ) = 5.0D-04
      RTOL( BENX         ) = 5.0D-04
      RTOL( xRCHO        ) = 5.0D-04
      RTOL( xKET2        ) = 5.0D-04
      RTOL( KET2         ) = 5.0D-04
      RTOL( xLVKS        ) = 5.0D-04
      RTOL( LVKS         ) = 5.0D-04
      RTOL( xOLEA1       ) = 5.0D-04
      RTOL( OLEA1        ) = 5.0D-04
      RTOL( xOLEA2       ) = 5.0D-04
      RTOL( OLEA2        ) = 5.0D-04
      RTOL( xOLEP        ) = 5.0D-04
      RTOL( OLEP         ) = 5.0D-04
      RTOL( xOACID       ) = 5.0D-04
      RTOL( xPACID       ) = 5.0D-04
      RTOL( xAMINS       ) = 5.0D-04
      RTOL( xRPNO3       ) = 5.0D-04
      RTOL( RPNO3        ) = 5.0D-04
      RTOL( xRCNO3       ) = 5.0D-04
      RTOL( RCNO3        ) = 5.0D-04
      RTOL( xRHNO3       ) = 5.0D-04
      RTOL( RHNO3        ) = 5.0D-04
      RTOL( xRDNO3       ) = 5.0D-04
      RTOL( RDNO3        ) = 5.0D-04
      RTOL( xHPCRB       ) = 5.0D-04
      RTOL( HPCRB        ) = 5.0D-04
      RTOL( xAFG1        ) = 5.0D-04
      RTOL( AFG1         ) = 5.0D-04
      RTOL( xAFG2A       ) = 5.0D-04
      RTOL( xAFG2B       ) = 5.0D-04
      RTOL( xAFG3        ) = 5.0D-04
      RTOL( AFG3         ) = 5.0D-04
      RTOL( xPAN2        ) = 5.0D-04
      RTOL( PAN2         ) = 5.0D-04
      RTOL( xMEO2        ) = 5.0D-04
      RTOL( xETO2        ) = 5.0D-04
      RTOL( xMECO3       ) = 5.0D-04
      RTOL( xR2CO3       ) = 5.0D-04
      RTOL( R2CO3        ) = 5.0D-04
      RTOL( xMACO3       ) = 5.0D-04
      RTOL( xTBUO        ) = 5.0D-04
      RTOL( xBZO         ) = 5.0D-04
      RTOL( yRUOOH       ) = 5.0D-04
      RTOL( RUOOH        ) = 5.0D-04
      RTOL( yRAOOH       ) = 5.0D-04
      RTOL( RAOOH        ) = 5.0D-04
      RTOL( yHPCRB       ) = 5.0D-04
      RTOL( yRPNO3       ) = 5.0D-04
      RTOL( zR1NO3       ) = 5.0D-04
      RTOL( zR2NO3       ) = 5.0D-04
      RTOL( R2NO3        ) = 5.0D-04
      RTOL( zRHNO3       ) = 5.0D-04
      RTOL( zRCNO3       ) = 5.0D-04
      RTOL( zRANO3       ) = 5.0D-04
      RTOL( RANO3        ) = 5.0D-04
      RTOL( zRPNO3       ) = 5.0D-04
      RTOL( zRDNO3       ) = 5.0D-04
      RTOL( zRNNO3       ) = 5.0D-04
      RTOL( RNNO3        ) = 5.0D-04
      RTOL( zPAN2        ) = 5.0D-04
      RTOL( APANS        ) = 5.0D-04
      RTOL( ETHAN        ) = 5.0D-04
      RTOL( PROP         ) = 5.0D-04
      RTOL( NC4          ) = 5.0D-04
      RTOL( ETHEN        ) = 5.0D-04
      RTOL( ETHEN_OP     ) = 5.0D-04
      RTOL( NROG         ) = 5.0D-04
      RTOL( PROPE        ) = 5.0D-04
      RTOL( PROPE_O3     ) = 5.0D-04
      RTOL( ALK2         ) = 5.0D-04
      RTOL( ISOP         ) = 5.0D-04
      RTOL( ISOP_OH      ) = 5.0D-04
      RTOL( ISOP_O3      ) = 5.0D-04
      RTOL( ISOPRXN      ) = 1.0D+00
      RTOL( ISOP_N3      ) = 5.0D-04
      RTOL( BUT13        ) = 5.0D-04
      RTOL( BUT13_OH     ) = 5.0D-04
      RTOL( BUT13_O3     ) = 5.0D-04
      RTOL( APINE        ) = 5.0D-04
      RTOL( APINE_OH     ) = 5.0D-04
      RTOL( TRPRXN       ) = 1.0D+00
      RTOL( ALK4         ) = 5.0D-04
      RTOL( BPINE        ) = 5.0D-04
      RTOL( BPINE_OH     ) = 5.0D-04
      RTOL( ACETL        ) = 5.0D-04
      RTOL( BENZ         ) = 5.0D-04
      RTOL( BENZRO2      ) = 5.0D-04
      RTOL( TOLU         ) = 5.0D-04
      RTOL( TOLRO2       ) = 5.0D-04
      RTOL( OXYL         ) = 5.0D-04
      RTOL( XYNL         ) = 5.0D-04
      RTOL( XYLRO2       ) = 5.0D-04
      RTOL( MXYL         ) = 5.0D-04
      RTOL( PXYL         ) = 5.0D-04
      RTOL( BZ123        ) = 5.0D-04
      RTOL( BZ124        ) = 5.0D-04
      RTOL( BZ135        ) = 5.0D-04
      RTOL( C2BEN        ) = 5.0D-04
      RTOL( MTBE         ) = 5.0D-04
      RTOL( ALK1         ) = 5.0D-04
      RTOL( MECHO_OH     ) = 5.0D-04
      RTOL( GLCHO_HV     ) = 5.0D-04
      RTOL( ACRO_OH      ) = 5.0D-04
      RTOL( ACRO_HV      ) = 5.0D-04
      RTOL( MACR_OH      ) = 5.0D-04
      RTOL( MACR_N3      ) = 5.0D-04
      RTOL( BUDAL_OH     ) = 5.0D-04
      RTOL( MALAH        ) = 5.0D-04
      RTOL( ALK5_OH      ) = 5.0D-04
      RTOL( ALK6         ) = 5.0D-04
      RTOL( ALK6_OH      ) = 5.0D-04
      RTOL( OLE1         ) = 5.0D-04
      RTOL( OLE2         ) = 5.0D-04
      RTOL( OLE2_O3      ) = 5.0D-04
      RTOL( OLE3         ) = 5.0D-04
      RTOL( OLE4         ) = 5.0D-04
      RTOL( OLE4_O3      ) = 5.0D-04
      RTOL( TERP         ) = 5.0D-04
      RTOL( TERP_OH      ) = 5.0D-04
      RTOL( TERP_O3      ) = 5.0D-04
      RTOL( TERP_N3      ) = 5.0D-04
      RTOL( SESQ         ) = 5.0D-04
      RTOL( SESQ_OH      ) = 5.0D-04
      RTOL( SESQRXN      ) = 1.0D+00
      RTOL( SESQ_O3      ) = 5.0D-04
      RTOL( SESQ_N3      ) = 5.0D-04
      RTOL( ARO1         ) = 5.0D-04
      RTOL( ARO2         ) = 5.0D-04
      RTOL( ARO2_OH      ) = 5.0D-04
      RTOL( STYRS        ) = 5.0D-04
      RTOL( TAMNS        ) = 5.0D-04
      RTOL( OLEA1_OH     ) = 5.0D-04
      RTOL( OLEA1_N3     ) = 5.0D-04
      RTOL( OLEA2_OH     ) = 5.0D-04
      RTOL( OLEA2_O3     ) = 5.0D-04
      RTOL( OLEA2_N3     ) = 5.0D-04
      RTOL( OLEA2_HV     ) = 5.0D-04
      RTOL( LVKS_OH      ) = 5.0D-04
      RTOL( LVKS_O3      ) = 5.0D-04
      RTOL( OLEP_OH      ) = 5.0D-04
      RTOL( OLEP_O3      ) = 5.0D-04
      RTOL( RCNO3_OH     ) = 5.0D-04
      RTOL( RCNO3_HV     ) = 5.0D-04
      RTOL( RPNO3_OH     ) = 5.0D-04
      RTOL( RPNO3_HV     ) = 5.0D-04
      RTOL( RDNO3_HV     ) = 5.0D-04
      RTOL( R2NO3_HV     ) = 5.0D-04
      RTOL( RUOOH_OH     ) = 5.0D-04
      RTOL( HPCRB_OH     ) = 5.0D-04
      RTOL( ROOH_OH      ) = 5.0D-04
      RTOL( AFG1_OH      ) = 5.0D-04
      RTOL( AFG1_HV      ) = 5.0D-04
      RTOL( AFG2A_OH     ) = 5.0D-04
      RTOL( AFG2B_OH     ) = 5.0D-04
      RTOL( AFG2B_HV     ) = 5.0D-04
      RTOL( SOAALK       ) = 5.0D-04
      RTOL( SVAVB2       ) = 5.0D-04
      RTOL( SVAVB3       ) = 5.0D-04
      RTOL( SVAVB4       ) = 5.0D-04
      RTOL( SVAVB1       ) = 5.0D-04
      RTOL( H2NO3PIJ     ) = 5.0D-04
      RTOL( H2NO3PK      ) = 5.0D-04
      RTOL( AISO1J       ) = 5.0D-04
      RTOL( AOLGBJ       ) = 5.0D-04
      RTOL( AISO2J       ) = 5.0D-04
      RTOL( ASQTJ        ) = 5.0D-04
      RTOL( ATRP1J       ) = 5.0D-04
      RTOL( ATRP2J       ) = 5.0D-04
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
      RTOL( AGLYJ        ) = 5.0D-04
      RTOL( HCHO_PRIMARY ) = 5.0D-04
      RTOL( CCHO_PRIMARY ) = 5.0D-04
      RTOL( ACRO_PRIMARY ) = 5.0D-04


      RETURN

      END
