
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
C  PRECONDITIONS: For the SAPRC07TIC_2DVBS_AQ mechanism
C
C  RETURN VALUES: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Jul 24, 2022
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

      N_EBISP  = 405
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
      EBISP(  92 ) = TRPRO2
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
      EBISP( 110 ) = SOAALKRO2
      EBISP( 111 ) = OLE1
      EBISP( 112 ) = OLE2
      EBISP( 113 ) = ARO1
      EBISP( 114 ) = ARO2MN
      EBISP( 115 ) = NAPHTHAL
      EBISP( 116 ) = PAHRO2
      EBISP( 117 ) = TERP
      EBISP( 118 ) = TERPNRO2
      EBISP( 119 ) = SESQ
      EBISP( 120 ) = SESQRO2
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
      EBISP( 136 ) = HCHO_PRIMARY
      EBISP( 137 ) = CCHO_PRIMARY
      EBISP( 138 ) = ACRO_PRIMARY
      EBISP( 139 ) = ISOPO2
      EBISP( 140 ) = ISOPRXN
      EBISP( 141 ) = ISOPND
      EBISP( 142 ) = ISOPNB
      EBISP( 143 ) = HC5
      EBISP( 144 ) = DIBOO
      EBISP( 145 ) = ISOPOOH
      EBISP( 146 ) = HPALD
      EBISP( 147 ) = HACET
      EBISP( 148 ) = NISOPO2
      EBISP( 149 ) = NIT1
      EBISP( 150 ) = NISOPOOH
      EBISP( 151 ) = HC5OO
      EBISP( 152 ) = DHMOB
      EBISP( 153 ) = ISOPNOOD
      EBISP( 154 ) = PROPNN
      EBISP( 155 ) = MVKN
      EBISP( 156 ) = ETHLN
      EBISP( 157 ) = RNO3I
      EBISP( 158 ) = ISOPNOOB
      EBISP( 159 ) = MACRN
      EBISP( 160 ) = NIT1NO3OOA
      EBISP( 161 ) = NIT1NO3OOB
      EBISP( 162 ) = ISOPNN
      EBISP( 163 ) = NIT1OHOO
      EBISP( 164 ) = MVKOO
      EBISP( 165 ) = MACROO
      EBISP( 166 ) = PYRUACD
      EBISP( 167 ) = IEPOX
      EBISP( 168 ) = IEPOXOO
      EBISP( 169 ) = IMPAA
      EBISP( 170 ) = IMAPAN
      EBISP( 171 ) = IMAE
      EBISP( 172 ) = IHMML
      EBISP( 173 ) = H2NO3PIJ
      EBISP( 174 ) = H2NO3PK
      EBISP( 175 ) = ACLI
      EBISP( 176 ) = ACLJ
      EBISP( 177 ) = ACLK
      EBISP( 178 ) = IEPOXP
      EBISP( 179 ) = IMAEP
      EBISP( 180 ) = IHMMLP
      EBISP( 181 ) = AIETETJ
      EBISP( 182 ) = ASO4J
      EBISP( 183 ) = AIEOSJ
      EBISP( 184 ) = ADIMJ
      EBISP( 185 ) = AIMGAJ
      EBISP( 186 ) = AIMOSJ
      EBISP( 187 ) = AISO1J
      EBISP( 188 ) = AOLGBJ
      EBISP( 189 ) = AISO2J
      EBISP( 190 ) = AMTNO3J
      EBISP( 191 ) = AMTHYDJ
      EBISP( 192 ) = AISOPNNJ
      EBISP( 193 ) = AGLYJ
      EBISP( 194 ) = CS00O2C07A
      EBISP( 195 ) = CS00O2C04A
      EBISP( 196 ) = CS01O2C04A
      EBISP( 197 ) = CS02O2C04A
      EBISP( 198 ) = CS02O2C02A
      EBISP( 199 ) = CS03O2C04A
      EBISP( 200 ) = CS03O2C02A
      EBISP( 201 ) = CS07O2C10A
      EBISP( 202 ) = CS03O2C07A
      EBISP( 203 ) = CS04O2C07A
      EBISP( 204 ) = CS05O2C07A
      EBISP( 205 ) = CS06O2C07A
      EBISP( 206 ) = CS07O2C07A
      EBISP( 207 ) = CS04O2C04A
      EBISP( 208 ) = CS05O2C04A
      EBISP( 209 ) = CS06O2C04A
      EBISP( 210 ) = CS07O2C04A
      EBISP( 211 ) = CS05O2C02A
      EBISP( 212 ) = CS06O2C02A
      EBISP( 213 ) = CS07O2C02A
      EBISP( 214 ) = CS05O2C01A
      EBISP( 215 ) = CS06O2C01A
      EBISP( 216 ) = CS07O2C01A
      EBISP( 217 ) = CS05O2C00A
      EBISP( 218 ) = CS06O2C00A
      EBISP( 219 ) = CS07O2C00A
      EBISP( 220 ) = CS03O2C10A
      EBISP( 221 ) = CS04O2C10A
      EBISP( 222 ) = CS04O2C02A
      EBISP( 223 ) = CSM1O2C07B
      EBISP( 224 ) = CS00O2C07B
      EBISP( 225 ) = CS01O2C07B
      EBISP( 226 ) = CS02O2C07B
      EBISP( 227 ) = CSM1O2C04B
      EBISP( 228 ) = CS00O2C04B
      EBISP( 229 ) = CS01O2C04B
      EBISP( 230 ) = CS02O2C04B
      EBISP( 231 ) = CS03O2C04B
      EBISP( 232 ) = CS04O2C04B
      EBISP( 233 ) = CS05O2C04B
      EBISP( 234 ) = CS06O2C04B
      EBISP( 235 ) = CS00O2C02B
      EBISP( 236 ) = CS01O2C02B
      EBISP( 237 ) = CS02O2C02B
      EBISP( 238 ) = CS03O2C02B
      EBISP( 239 ) = CS04O2C02B
      EBISP( 240 ) = CS05O2C02B
      EBISP( 241 ) = CS06O2C02B
      EBISP( 242 ) = CS02O2C01B
      EBISP( 243 ) = CS03O2C01B
      EBISP( 244 ) = CS04O2C01B
      EBISP( 245 ) = CS05O2C01B
      EBISP( 246 ) = CS06O2C01B
      EBISP( 247 ) = CS01O2C04
      EBISP( 248 ) = CS02O2C02
      EBISP( 249 ) = CS03O2C02
      EBISP( 250 ) = CS03O2C04
      EBISP( 251 ) = CS04O2C01
      EBISP( 252 ) = CS04O2C02
      EBISP( 253 ) = CS05O2C02
      EBISP( 254 ) = CS05O2C04
      EBISP( 255 ) = CS06O2C01
      EBISP( 256 ) = CSM1O2C10
      EBISP( 257 ) = CS00O2C10
      EBISP( 258 ) = CS01O2C10
      EBISP( 259 ) = CS02O2C10
      EBISP( 260 ) = CS03O2C10
      EBISP( 261 ) = CS04O2C10
      EBISP( 262 ) = CS05O2C10
      EBISP( 263 ) = CS06O2C10
      EBISP( 264 ) = CS07O2C10
      EBISP( 265 ) = CSM1O2C07
      EBISP( 266 ) = CS00O2C07
      EBISP( 267 ) = CS01O2C07
      EBISP( 268 ) = CS02O2C07
      EBISP( 269 ) = CS03O2C07
      EBISP( 270 ) = CS04O2C07
      EBISP( 271 ) = CS05O2C07
      EBISP( 272 ) = CS06O2C07
      EBISP( 273 ) = CS07O2C07
      EBISP( 274 ) = CSM1O2C04
      EBISP( 275 ) = CS00O2C04
      EBISP( 276 ) = CS02O2C04
      EBISP( 277 ) = CS04O2C04
      EBISP( 278 ) = CS06O2C04
      EBISP( 279 ) = CS07O2C04
      EBISP( 280 ) = CSM1O2C02
      EBISP( 281 ) = CS00O2C02
      EBISP( 282 ) = CS01O2C02
      EBISP( 283 ) = CS06O2C02
      EBISP( 284 ) = CS07O2C02
      EBISP( 285 ) = CSM1O2C01
      EBISP( 286 ) = CS00O2C01
      EBISP( 287 ) = CS01O2C01
      EBISP( 288 ) = CS02O2C01
      EBISP( 289 ) = CS03O2C01
      EBISP( 290 ) = CS05O2C01
      EBISP( 291 ) = CS07O2C01
      EBISP( 292 ) = CSM1O2C00
      EBISP( 293 ) = CS00O2C00
      EBISP( 294 ) = CS01O2C00
      EBISP( 295 ) = CS02O2C00
      EBISP( 296 ) = CS03O2C00
      EBISP( 297 ) = CS04O2C00
      EBISP( 298 ) = CS05O2C00
      EBISP( 299 ) = CS06O2C00
      EBISP( 300 ) = CS07O2C00
      EBISP( 301 ) = CSM1O2C04P
      EBISP( 302 ) = CS00O2C04P
      EBISP( 303 ) = CS01O2C04P
      EBISP( 304 ) = CS02O2C04P
      EBISP( 305 ) = CS03O2C04P
      EBISP( 306 ) = CS04O2C04P
      EBISP( 307 ) = CS05O2C04P
      EBISP( 308 ) = CS06O2C04P
      EBISP( 309 ) = CSM1O2C02P
      EBISP( 310 ) = CS00O2C02P
      EBISP( 311 ) = CS01O2C02P
      EBISP( 312 ) = CS02O2C02P
      EBISP( 313 ) = CS03O2C02P
      EBISP( 314 ) = CS04O2C02P
      EBISP( 315 ) = CS05O2C02P
      EBISP( 316 ) = CS06O2C02P
      EBISP( 317 ) = CSM1O2C01P
      EBISP( 318 ) = CS00O2C01P
      EBISP( 319 ) = CS01O2C01P
      EBISP( 320 ) = CS02O2C01P
      EBISP( 321 ) = CS03O2C01P
      EBISP( 322 ) = CS04O2C01P
      EBISP( 323 ) = CS05O2C01P
      EBISP( 324 ) = CS06O2C01P
      EBISP( 325 ) = CSM1O2C00P
      EBISP( 326 ) = CS00O2C00P
      EBISP( 327 ) = CS01O2C00P
      EBISP( 328 ) = CS02O2C00P
      EBISP( 329 ) = CS03O2C00P
      EBISP( 330 ) = CS04O2C00P
      EBISP( 331 ) = CS05O2C00P
      EBISP( 332 ) = CS06O2C00P
      EBISP( 333 ) = CSM1O2C20A
      EBISP( 334 ) = CS00O2C20A
      EBISP( 335 ) = CS01O2C20A
      EBISP( 336 ) = CS02O2C20A
      EBISP( 337 ) = CS03O2C20A
      EBISP( 338 ) = CS04O2C20A
      EBISP( 339 ) = CS05O2C20A
      EBISP( 340 ) = CS06O2C20A
      EBISP( 341 ) = CS07O2C20A
      EBISP( 342 ) = CSM1O2C15A
      EBISP( 343 ) = CS00O2C15A
      EBISP( 344 ) = CS01O2C15A
      EBISP( 345 ) = CS02O2C15A
      EBISP( 346 ) = CS03O2C15A
      EBISP( 347 ) = CS04O2C15A
      EBISP( 348 ) = CS05O2C15A
      EBISP( 349 ) = CS06O2C15A
      EBISP( 350 ) = CS07O2C15A
      EBISP( 351 ) = CSM1O2C10A
      EBISP( 352 ) = CS00O2C10A
      EBISP( 353 ) = CS01O2C10A
      EBISP( 354 ) = CS02O2C10A
      EBISP( 355 ) = CS05O2C10A
      EBISP( 356 ) = CS06O2C10A
      EBISP( 357 ) = CSM1O2C07A
      EBISP( 358 ) = CS01O2C07A
      EBISP( 359 ) = CS02O2C07A
      EBISP( 360 ) = CSM1O2C04A
      EBISP( 361 ) = CSM1O2C02A
      EBISP( 362 ) = CS00O2C02A
      EBISP( 363 ) = CS01O2C02A
      EBISP( 364 ) = CSM1O2C01A
      EBISP( 365 ) = CS00O2C01A
      EBISP( 366 ) = CS01O2C01A
      EBISP( 367 ) = CS02O2C01A
      EBISP( 368 ) = CS03O2C01A
      EBISP( 369 ) = CS04O2C01A
      EBISP( 370 ) = CSM1O2C00A
      EBISP( 371 ) = CS00O2C00A
      EBISP( 372 ) = CS01O2C00A
      EBISP( 373 ) = CS02O2C00A
      EBISP( 374 ) = CS03O2C00A
      EBISP( 375 ) = CS04O2C00A
      EBISP( 376 ) = CSM1O2C10B
      EBISP( 377 ) = CS00O2C10B
      EBISP( 378 ) = CS01O2C10B
      EBISP( 379 ) = CS02O2C10B
      EBISP( 380 ) = CS03O2C10B
      EBISP( 381 ) = CS04O2C10B
      EBISP( 382 ) = CS05O2C10B
      EBISP( 383 ) = CS06O2C10B
      EBISP( 384 ) = CS07O2C10B
      EBISP( 385 ) = CS03O2C07B
      EBISP( 386 ) = CS04O2C07B
      EBISP( 387 ) = CS05O2C07B
      EBISP( 388 ) = CS06O2C07B
      EBISP( 389 ) = CS07O2C07B
      EBISP( 390 ) = CS07O2C04B
      EBISP( 391 ) = CSM1O2C02B
      EBISP( 392 ) = CS07O2C02B
      EBISP( 393 ) = CSM1O2C01B
      EBISP( 394 ) = CS00O2C01B
      EBISP( 395 ) = CS01O2C01B
      EBISP( 396 ) = CS07O2C01B
      EBISP( 397 ) = CSM1O2C00B
      EBISP( 398 ) = CS00O2C00B
      EBISP( 399 ) = CS01O2C00B
      EBISP( 400 ) = CS02O2C00B
      EBISP( 401 ) = CS03O2C00B
      EBISP( 402 ) = CS04O2C00B
      EBISP( 403 ) = CS05O2C00B
      EBISP( 404 ) = CS06O2C00B
      EBISP( 405 ) = CS07O2C00B


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
      RTOL( TRPRO2       ) = 5.0D-04
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
      RTOL( SOAALKRO2    ) = 5.0D-04
      RTOL( OLE1         ) = 5.0D-04
      RTOL( OLE2         ) = 5.0D-04
      RTOL( ARO1         ) = 5.0D-04
      RTOL( ARO2MN       ) = 5.0D-04
      RTOL( NAPHTHAL     ) = 5.0D-04
      RTOL( PAHRO2       ) = 1.0D+00
      RTOL( TERP         ) = 5.0D-04
      RTOL( TERPNRO2     ) = 5.0D-04
      RTOL( SESQ         ) = 5.0D-04
      RTOL( SESQRO2      ) = 5.0D-04
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
      RTOL( AMTNO3J      ) = 5.0D-04
      RTOL( AMTHYDJ      ) = 5.0D-04
      RTOL( AISOPNNJ     ) = 5.0D-04
      RTOL( AGLYJ        ) = 5.0D-04
      RTOL( CS00O2C07A   ) = 5.0D-04
      RTOL( CS00O2C04A   ) = 5.0D-04
      RTOL( CS01O2C04A   ) = 5.0D-04
      RTOL( CS02O2C04A   ) = 5.0D-04
      RTOL( CS02O2C02A   ) = 5.0D-04
      RTOL( CS03O2C04A   ) = 5.0D-04
      RTOL( CS03O2C02A   ) = 5.0D-04
      RTOL( CS07O2C10A   ) = 5.0D-04
      RTOL( CS03O2C07A   ) = 5.0D-04
      RTOL( CS04O2C07A   ) = 5.0D-04
      RTOL( CS05O2C07A   ) = 5.0D-04
      RTOL( CS06O2C07A   ) = 5.0D-04
      RTOL( CS07O2C07A   ) = 5.0D-04
      RTOL( CS04O2C04A   ) = 5.0D-04
      RTOL( CS05O2C04A   ) = 5.0D-04
      RTOL( CS06O2C04A   ) = 5.0D-04
      RTOL( CS07O2C04A   ) = 5.0D-04
      RTOL( CS05O2C02A   ) = 5.0D-04
      RTOL( CS06O2C02A   ) = 5.0D-04
      RTOL( CS07O2C02A   ) = 5.0D-04
      RTOL( CS05O2C01A   ) = 5.0D-04
      RTOL( CS06O2C01A   ) = 5.0D-04
      RTOL( CS07O2C01A   ) = 5.0D-04
      RTOL( CS05O2C00A   ) = 5.0D-04
      RTOL( CS06O2C00A   ) = 5.0D-04
      RTOL( CS07O2C00A   ) = 5.0D-04
      RTOL( CS03O2C10A   ) = 5.0D-04
      RTOL( CS04O2C10A   ) = 5.0D-04
      RTOL( CS04O2C02A   ) = 5.0D-04
      RTOL( CSM1O2C07B   ) = 5.0D-04
      RTOL( CS00O2C07B   ) = 5.0D-04
      RTOL( CS01O2C07B   ) = 5.0D-04
      RTOL( CS02O2C07B   ) = 5.0D-04
      RTOL( CSM1O2C04B   ) = 5.0D-04
      RTOL( CS00O2C04B   ) = 5.0D-04
      RTOL( CS01O2C04B   ) = 5.0D-04
      RTOL( CS02O2C04B   ) = 5.0D-04
      RTOL( CS03O2C04B   ) = 5.0D-04
      RTOL( CS04O2C04B   ) = 5.0D-04
      RTOL( CS05O2C04B   ) = 5.0D-04
      RTOL( CS06O2C04B   ) = 5.0D-04
      RTOL( CS00O2C02B   ) = 5.0D-04
      RTOL( CS01O2C02B   ) = 5.0D-04
      RTOL( CS02O2C02B   ) = 5.0D-04
      RTOL( CS03O2C02B   ) = 5.0D-04
      RTOL( CS04O2C02B   ) = 5.0D-04
      RTOL( CS05O2C02B   ) = 5.0D-04
      RTOL( CS06O2C02B   ) = 5.0D-04
      RTOL( CS02O2C01B   ) = 5.0D-04
      RTOL( CS03O2C01B   ) = 5.0D-04
      RTOL( CS04O2C01B   ) = 5.0D-04
      RTOL( CS05O2C01B   ) = 5.0D-04
      RTOL( CS06O2C01B   ) = 5.0D-04
      RTOL( CS01O2C04    ) = 5.0D-04
      RTOL( CS02O2C02    ) = 5.0D-04
      RTOL( CS03O2C02    ) = 5.0D-04
      RTOL( CS03O2C04    ) = 5.0D-04
      RTOL( CS04O2C01    ) = 5.0D-04
      RTOL( CS04O2C02    ) = 5.0D-04
      RTOL( CS05O2C02    ) = 5.0D-04
      RTOL( CS05O2C04    ) = 5.0D-04
      RTOL( CS06O2C01    ) = 5.0D-04
      RTOL( CSM1O2C10    ) = 5.0D-04
      RTOL( CS00O2C10    ) = 5.0D-04
      RTOL( CS01O2C10    ) = 5.0D-04
      RTOL( CS02O2C10    ) = 5.0D-04
      RTOL( CS03O2C10    ) = 5.0D-04
      RTOL( CS04O2C10    ) = 5.0D-04
      RTOL( CS05O2C10    ) = 5.0D-04
      RTOL( CS06O2C10    ) = 5.0D-04
      RTOL( CS07O2C10    ) = 5.0D-04
      RTOL( CSM1O2C07    ) = 5.0D-04
      RTOL( CS00O2C07    ) = 5.0D-04
      RTOL( CS01O2C07    ) = 5.0D-04
      RTOL( CS02O2C07    ) = 5.0D-04
      RTOL( CS03O2C07    ) = 5.0D-04
      RTOL( CS04O2C07    ) = 5.0D-04
      RTOL( CS05O2C07    ) = 5.0D-04
      RTOL( CS06O2C07    ) = 5.0D-04
      RTOL( CS07O2C07    ) = 5.0D-04
      RTOL( CSM1O2C04    ) = 5.0D-04
      RTOL( CS00O2C04    ) = 5.0D-04
      RTOL( CS02O2C04    ) = 5.0D-04
      RTOL( CS04O2C04    ) = 5.0D-04
      RTOL( CS06O2C04    ) = 5.0D-04
      RTOL( CS07O2C04    ) = 5.0D-04
      RTOL( CSM1O2C02    ) = 5.0D-04
      RTOL( CS00O2C02    ) = 5.0D-04
      RTOL( CS01O2C02    ) = 5.0D-04
      RTOL( CS06O2C02    ) = 5.0D-04
      RTOL( CS07O2C02    ) = 5.0D-04
      RTOL( CSM1O2C01    ) = 5.0D-04
      RTOL( CS00O2C01    ) = 5.0D-04
      RTOL( CS01O2C01    ) = 5.0D-04
      RTOL( CS02O2C01    ) = 5.0D-04
      RTOL( CS03O2C01    ) = 5.0D-04
      RTOL( CS05O2C01    ) = 5.0D-04
      RTOL( CS07O2C01    ) = 5.0D-04
      RTOL( CSM1O2C00    ) = 5.0D-04
      RTOL( CS00O2C00    ) = 5.0D-04
      RTOL( CS01O2C00    ) = 5.0D-04
      RTOL( CS02O2C00    ) = 5.0D-04
      RTOL( CS03O2C00    ) = 5.0D-04
      RTOL( CS04O2C00    ) = 5.0D-04
      RTOL( CS05O2C00    ) = 5.0D-04
      RTOL( CS06O2C00    ) = 5.0D-04
      RTOL( CS07O2C00    ) = 5.0D-04
      RTOL( CSM1O2C04P   ) = 5.0D-04
      RTOL( CS00O2C04P   ) = 5.0D-04
      RTOL( CS01O2C04P   ) = 5.0D-04
      RTOL( CS02O2C04P   ) = 5.0D-04
      RTOL( CS03O2C04P   ) = 5.0D-04
      RTOL( CS04O2C04P   ) = 5.0D-04
      RTOL( CS05O2C04P   ) = 5.0D-04
      RTOL( CS06O2C04P   ) = 5.0D-04
      RTOL( CSM1O2C02P   ) = 5.0D-04
      RTOL( CS00O2C02P   ) = 5.0D-04
      RTOL( CS01O2C02P   ) = 5.0D-04
      RTOL( CS02O2C02P   ) = 5.0D-04
      RTOL( CS03O2C02P   ) = 5.0D-04
      RTOL( CS04O2C02P   ) = 5.0D-04
      RTOL( CS05O2C02P   ) = 5.0D-04
      RTOL( CS06O2C02P   ) = 5.0D-04
      RTOL( CSM1O2C01P   ) = 5.0D-04
      RTOL( CS00O2C01P   ) = 5.0D-04
      RTOL( CS01O2C01P   ) = 5.0D-04
      RTOL( CS02O2C01P   ) = 5.0D-04
      RTOL( CS03O2C01P   ) = 5.0D-04
      RTOL( CS04O2C01P   ) = 5.0D-04
      RTOL( CS05O2C01P   ) = 5.0D-04
      RTOL( CS06O2C01P   ) = 5.0D-04
      RTOL( CSM1O2C00P   ) = 5.0D-04
      RTOL( CS00O2C00P   ) = 5.0D-04
      RTOL( CS01O2C00P   ) = 5.0D-04
      RTOL( CS02O2C00P   ) = 5.0D-04
      RTOL( CS03O2C00P   ) = 5.0D-04
      RTOL( CS04O2C00P   ) = 5.0D-04
      RTOL( CS05O2C00P   ) = 5.0D-04
      RTOL( CS06O2C00P   ) = 5.0D-04
      RTOL( CSM1O2C20A   ) = 5.0D-04
      RTOL( CS00O2C20A   ) = 5.0D-04
      RTOL( CS01O2C20A   ) = 5.0D-04
      RTOL( CS02O2C20A   ) = 5.0D-04
      RTOL( CS03O2C20A   ) = 5.0D-04
      RTOL( CS04O2C20A   ) = 5.0D-04
      RTOL( CS05O2C20A   ) = 5.0D-04
      RTOL( CS06O2C20A   ) = 5.0D-04
      RTOL( CS07O2C20A   ) = 5.0D-04
      RTOL( CSM1O2C15A   ) = 5.0D-04
      RTOL( CS00O2C15A   ) = 5.0D-04
      RTOL( CS01O2C15A   ) = 5.0D-04
      RTOL( CS02O2C15A   ) = 5.0D-04
      RTOL( CS03O2C15A   ) = 5.0D-04
      RTOL( CS04O2C15A   ) = 5.0D-04
      RTOL( CS05O2C15A   ) = 5.0D-04
      RTOL( CS06O2C15A   ) = 5.0D-04
      RTOL( CS07O2C15A   ) = 5.0D-04
      RTOL( CSM1O2C10A   ) = 5.0D-04
      RTOL( CS00O2C10A   ) = 5.0D-04
      RTOL( CS01O2C10A   ) = 5.0D-04
      RTOL( CS02O2C10A   ) = 5.0D-04
      RTOL( CS05O2C10A   ) = 5.0D-04
      RTOL( CS06O2C10A   ) = 5.0D-04
      RTOL( CSM1O2C07A   ) = 5.0D-04
      RTOL( CS01O2C07A   ) = 5.0D-04
      RTOL( CS02O2C07A   ) = 5.0D-04
      RTOL( CSM1O2C04A   ) = 5.0D-04
      RTOL( CSM1O2C02A   ) = 5.0D-04
      RTOL( CS00O2C02A   ) = 5.0D-04
      RTOL( CS01O2C02A   ) = 5.0D-04
      RTOL( CSM1O2C01A   ) = 5.0D-04
      RTOL( CS00O2C01A   ) = 5.0D-04
      RTOL( CS01O2C01A   ) = 5.0D-04
      RTOL( CS02O2C01A   ) = 5.0D-04
      RTOL( CS03O2C01A   ) = 5.0D-04
      RTOL( CS04O2C01A   ) = 5.0D-04
      RTOL( CSM1O2C00A   ) = 5.0D-04
      RTOL( CS00O2C00A   ) = 5.0D-04
      RTOL( CS01O2C00A   ) = 5.0D-04
      RTOL( CS02O2C00A   ) = 5.0D-04
      RTOL( CS03O2C00A   ) = 5.0D-04
      RTOL( CS04O2C00A   ) = 5.0D-04
      RTOL( CSM1O2C10B   ) = 5.0D-04
      RTOL( CS00O2C10B   ) = 5.0D-04
      RTOL( CS01O2C10B   ) = 5.0D-04
      RTOL( CS02O2C10B   ) = 5.0D-04
      RTOL( CS03O2C10B   ) = 5.0D-04
      RTOL( CS04O2C10B   ) = 5.0D-04
      RTOL( CS05O2C10B   ) = 5.0D-04
      RTOL( CS06O2C10B   ) = 5.0D-04
      RTOL( CS07O2C10B   ) = 5.0D-04
      RTOL( CS03O2C07B   ) = 5.0D-04
      RTOL( CS04O2C07B   ) = 5.0D-04
      RTOL( CS05O2C07B   ) = 5.0D-04
      RTOL( CS06O2C07B   ) = 5.0D-04
      RTOL( CS07O2C07B   ) = 5.0D-04
      RTOL( CS07O2C04B   ) = 5.0D-04
      RTOL( CSM1O2C02B   ) = 5.0D-04
      RTOL( CS07O2C02B   ) = 5.0D-04
      RTOL( CSM1O2C01B   ) = 5.0D-04
      RTOL( CS00O2C01B   ) = 5.0D-04
      RTOL( CS01O2C01B   ) = 5.0D-04
      RTOL( CS07O2C01B   ) = 5.0D-04
      RTOL( CSM1O2C00B   ) = 5.0D-04
      RTOL( CS00O2C00B   ) = 5.0D-04
      RTOL( CS01O2C00B   ) = 5.0D-04
      RTOL( CS02O2C00B   ) = 5.0D-04
      RTOL( CS03O2C00B   ) = 5.0D-04
      RTOL( CS04O2C00B   ) = 5.0D-04
      RTOL( CS05O2C00B   ) = 5.0D-04
      RTOL( CS06O2C00B   ) = 5.0D-04
      RTOL( CS07O2C00B   ) = 5.0D-04


      RETURN

      END
