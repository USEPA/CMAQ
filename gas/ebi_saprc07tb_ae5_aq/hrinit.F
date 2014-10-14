
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
C  PRECONDITIONS: For the SAPRC07TB family of mechanisms
C
C  RETURN VALUES: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Oct. 7, 2014
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

      N_SPEC = NUMB_MECH_SPC
      N_RXNS = NRXNS
      N_SPCL_RKS =   9
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


      NING1 = 4
      NING2 = 4



c..Allocate and define ebi species

      N_EBISP  = 151
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
      EBISP(   8 ) = NO2EX
      EBISP(   9 ) = MEO2
      EBISP(  10 ) = HCHO
      EBISP(  11 ) = COOH
      EBISP(  12 ) = MEOH
      EBISP(  13 ) = RO2C
      EBISP(  14 ) = RO2XC
      EBISP(  15 ) = CCOOOH
      EBISP(  16 ) = CCOOH
      EBISP(  17 ) = RCO3
      EBISP(  18 ) = PAN2
      EBISP(  19 ) = xHO2
      EBISP(  20 ) = yROOH
      EBISP(  21 ) = xCCHO
      EBISP(  22 ) = RCOOOH
      EBISP(  23 ) = RCOOH
      EBISP(  24 ) = BZCO3
      EBISP(  25 ) = PBZN
      EBISP(  26 ) = BZO
      EBISP(  27 ) = MACO3
      EBISP(  28 ) = MAPAN
      EBISP(  29 ) = TBUO
      EBISP(  30 ) = RNO3
      EBISP(  31 ) = ACETONE
      EBISP(  32 ) = NPHE
      EBISP(  33 ) = CRES
      EBISP(  34 ) = xOH
      EBISP(  35 ) = xNO2
      EBISP(  36 ) = xMEO2
      EBISP(  37 ) = xMECO3
      EBISP(  38 ) = xRCO3
      EBISP(  39 ) = xMACO3
      EBISP(  40 ) = xTBUO
      EBISP(  41 ) = xCO
      EBISP(  42 ) = CCHO
      EBISP(  43 ) = RCHO
      EBISP(  44 ) = xHCHO
      EBISP(  45 ) = MEK
      EBISP(  46 ) = zRNO3
      EBISP(  47 ) = xRCHO
      EBISP(  48 ) = HCOOH
      EBISP(  49 ) = xMGLY
      EBISP(  50 ) = xBACL
      EBISP(  51 ) = ROOH
      EBISP(  52 ) = xPROD2
      EBISP(  53 ) = R6OOH
      EBISP(  54 ) = PRD2
      EBISP(  55 ) = yR6OOH
      EBISP(  56 ) = RAOOH
      EBISP(  57 ) = MGLY
      EBISP(  58 ) = IPRD
      EBISP(  59 ) = xGLY
      EBISP(  60 ) = xMEK
      EBISP(  61 ) = xAFG1
      EBISP(  62 ) = xAFG2
      EBISP(  63 ) = GLY
      EBISP(  64 ) = AFG1
      EBISP(  65 ) = AFG2
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
      EBISP(  81 ) = yRAOOH
      EBISP(  82 ) = xACROLEIN
      EBISP(  83 ) = ETHENE
      EBISP(  84 ) = PROPENE
      EBISP(  85 ) = BUTADIENE13
      EBISP(  86 ) = ISOPRENE
      EBISP(  87 ) = ISOPRXN
      EBISP(  88 ) = APIN
      EBISP(  89 ) = TRPRXN
      EBISP(  90 ) = ACETYLENE
      EBISP(  91 ) = BENZENE
      EBISP(  92 ) = BENZRO2
      EBISP(  93 ) = TOLUENE
      EBISP(  94 ) = TOLRO2
      EBISP(  95 ) = MXYL
      EBISP(  96 ) = XYLRO2
      EBISP(  97 ) = OXYL
      EBISP(  98 ) = PXYL
      EBISP(  99 ) = TRIMETH_BENZ124
      EBISP( 100 ) = ETOH
      EBISP( 101 ) = ALK1
      EBISP( 102 ) = ALK2
      EBISP( 103 ) = ALK3
      EBISP( 104 ) = ALK4
      EBISP( 105 ) = ALK5
      EBISP( 106 ) = ALK5RXN
      EBISP( 107 ) = OLE1
      EBISP( 108 ) = OLE2
      EBISP( 109 ) = ARO1
      EBISP( 110 ) = ARO2
      EBISP( 111 ) = TERP
      EBISP( 112 ) = SESQ
      EBISP( 113 ) = SESQRXN
      EBISP( 114 ) = CL2
      EBISP( 115 ) = CL
      EBISP( 116 ) = CLNO
      EBISP( 117 ) = CLONO
      EBISP( 118 ) = CLNO2
      EBISP( 119 ) = HCL
      EBISP( 120 ) = CLO
      EBISP( 121 ) = CLONO2
      EBISP( 122 ) = HOCL
      EBISP( 123 ) = xCL
      EBISP( 124 ) = xCLCCHO
      EBISP( 125 ) = xCLACET
      EBISP( 126 ) = CLCCHO
      EBISP( 127 ) = CLACET
      EBISP( 128 ) = CLCHO
      EBISP( 129 ) = BNZNRXN
      EBISP( 130 ) = BNZHRXN
      EBISP( 131 ) = XYLNRXN
      EBISP( 132 ) = XYLHRXN
      EBISP( 133 ) = TOLNRXN
      EBISP( 134 ) = TOLHRXN
      EBISP( 135 ) = HCHO_PRIMARY
      EBISP( 136 ) = CCHO_PRIMARY
      EBISP( 137 ) = ACROLEIN_PRIMARY
      EBISP( 138 ) = AALKJ
      EBISP( 139 ) = AOLGAJ
      EBISP( 140 ) = AXYL1J
      EBISP( 141 ) = AXYL2J
      EBISP( 142 ) = ATOL1J
      EBISP( 143 ) = ATOL2J
      EBISP( 144 ) = ABNZ1J
      EBISP( 145 ) = ABNZ2J
      EBISP( 146 ) = ATRP1J
      EBISP( 147 ) = AOLGBJ
      EBISP( 148 ) = ATRP2J
      EBISP( 149 ) = AISO1J
      EBISP( 150 ) = AISO2J
      EBISP( 151 ) = ASQTJ


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set species tolerances
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      RTOL( NO2              ) = 1.0D-03
      RTOL( NO               ) = 1.0D-03
      RTOL( O3P              ) = 1.0D+00
      RTOL( O3               ) = 1.0D-03
      RTOL( NO3              ) = 1.0D-03
      RTOL( N2O5             ) = 1.0D-03
      RTOL( HNO3             ) = 1.0D-03
      RTOL( O1D              ) = 1.0D+00
      RTOL( OH               ) = 1.0D-03
      RTOL( HONO             ) = 1.0D-03
      RTOL( HO2              ) = 1.0D-03
      RTOL( CO               ) = 1.0D-03
      RTOL( CO2              ) = 1.0D-03
      RTOL( HNO4             ) = 1.0D-03
      RTOL( HO2H             ) = 1.0D-03
      RTOL( SO2              ) = 1.0D-03
      RTOL( SULF             ) = 1.0D+00
      RTOL( SULRXN           ) = 1.0D+00
      RTOL( NO2EX            ) = 1.0D-03
      RTOL( MEO2             ) = 1.0D-03
      RTOL( HCHO             ) = 1.0D-03
      RTOL( COOH             ) = 1.0D-03
      RTOL( MEOH             ) = 1.0D-03
      RTOL( RO2C             ) = 1.0D-03
      RTOL( RO2XC            ) = 1.0D-03
      RTOL( MECO3            ) = 1.0D-03
      RTOL( PAN              ) = 1.0D-03
      RTOL( CCOOOH           ) = 1.0D-03
      RTOL( CCOOH            ) = 1.0D-03
      RTOL( RCO3             ) = 1.0D-03
      RTOL( PAN2             ) = 1.0D-03
      RTOL( xHO2             ) = 1.0D-03
      RTOL( yROOH            ) = 1.0D-03
      RTOL( xCCHO            ) = 1.0D-03
      RTOL( RCOOOH           ) = 1.0D-03
      RTOL( RCOOH            ) = 1.0D-03
      RTOL( BZCO3            ) = 1.0D-03
      RTOL( PBZN             ) = 1.0D-03
      RTOL( BZO              ) = 1.0D-03
      RTOL( MACO3            ) = 1.0D-03
      RTOL( MAPAN            ) = 1.0D-03
      RTOL( TBUO             ) = 1.0D-03
      RTOL( RNO3             ) = 1.0D-03
      RTOL( ACETONE          ) = 1.0D-03
      RTOL( NPHE             ) = 1.0D-03
      RTOL( CRES             ) = 1.0D-03
      RTOL( xOH              ) = 1.0D-03
      RTOL( xNO2             ) = 1.0D-03
      RTOL( xMEO2            ) = 1.0D-03
      RTOL( xMECO3           ) = 1.0D-03
      RTOL( xRCO3            ) = 1.0D-03
      RTOL( xMACO3           ) = 1.0D-03
      RTOL( xTBUO            ) = 1.0D-03
      RTOL( xCO              ) = 1.0D-03
      RTOL( CCHO             ) = 1.0D-03
      RTOL( RCHO             ) = 1.0D-03
      RTOL( xHCHO            ) = 1.0D-03
      RTOL( MEK              ) = 1.0D-03
      RTOL( zRNO3            ) = 1.0D-03
      RTOL( xRCHO            ) = 1.0D-03
      RTOL( HCOOH            ) = 1.0D-03
      RTOL( xMGLY            ) = 1.0D-03
      RTOL( xBACL            ) = 1.0D-03
      RTOL( ROOH             ) = 1.0D-03
      RTOL( xPROD2           ) = 1.0D-03
      RTOL( R6OOH            ) = 1.0D-03
      RTOL( PRD2             ) = 1.0D-03
      RTOL( yR6OOH           ) = 1.0D-03
      RTOL( RAOOH            ) = 1.0D-03
      RTOL( MGLY             ) = 1.0D-03
      RTOL( IPRD             ) = 1.0D-03
      RTOL( xGLY             ) = 1.0D-03
      RTOL( xMEK             ) = 1.0D-03
      RTOL( xAFG1            ) = 1.0D-03
      RTOL( xAFG2            ) = 1.0D-03
      RTOL( GLY              ) = 1.0D-03
      RTOL( AFG1             ) = 1.0D-03
      RTOL( AFG2             ) = 1.0D-03
      RTOL( BACL             ) = 1.0D-03
      RTOL( BALD             ) = 1.0D-03
      RTOL( AFG3             ) = 1.0D-03
      RTOL( xIPRD            ) = 1.0D-03
      RTOL( MACR             ) = 1.0D-03
      RTOL( MVK              ) = 1.0D-03
      RTOL( xHOCCHO          ) = 1.0D-03
      RTOL( xRNO3            ) = 1.0D-03
      RTOL( HOCCHO           ) = 1.0D-03
      RTOL( xACETONE         ) = 1.0D-03
      RTOL( ACROLEIN         ) = 1.0D-03
      RTOL( xBALD            ) = 1.0D-03
      RTOL( xAFG3            ) = 1.0D-03
      RTOL( xMACR            ) = 1.0D-03
      RTOL( xMVK             ) = 1.0D-03
      RTOL( yRAOOH           ) = 1.0D-03
      RTOL( xACROLEIN        ) = 1.0D-03
      RTOL( ETHENE           ) = 1.0D-03
      RTOL( PROPENE          ) = 1.0D-03
      RTOL( BUTADIENE13      ) = 1.0D-03
      RTOL( ISOPRENE         ) = 1.0D-03
      RTOL( ISOPRXN          ) = 1.0D+00
      RTOL( APIN             ) = 1.0D-03
      RTOL( TRPRXN           ) = 1.0D+00
      RTOL( ACETYLENE        ) = 1.0D-03
      RTOL( BENZENE          ) = 1.0D-03
      RTOL( BENZRO2          ) = 1.0D-03
      RTOL( TOLUENE          ) = 1.0D-03
      RTOL( TOLRO2           ) = 1.0D-03
      RTOL( MXYL             ) = 1.0D-03
      RTOL( XYLRO2           ) = 1.0D-03
      RTOL( OXYL             ) = 1.0D-03
      RTOL( PXYL             ) = 1.0D-03
      RTOL( TRIMETH_BENZ124  ) = 1.0D-03
      RTOL( ETOH             ) = 1.0D-03
      RTOL( ALK1             ) = 1.0D-03
      RTOL( ALK2             ) = 1.0D-03
      RTOL( ALK3             ) = 1.0D-03
      RTOL( ALK4             ) = 1.0D-03
      RTOL( ALK5             ) = 1.0D-03
      RTOL( ALK5RXN          ) = 1.0D+00
      RTOL( OLE1             ) = 1.0D-03
      RTOL( OLE2             ) = 1.0D-03
      RTOL( ARO1             ) = 1.0D-03
      RTOL( ARO2             ) = 1.0D-03
      RTOL( TERP             ) = 1.0D-03
      RTOL( SESQ             ) = 1.0D-03
      RTOL( SESQRXN          ) = 1.0D+00
      RTOL( CL2              ) = 1.0D-03
      RTOL( CL               ) = 1.0D-03
      RTOL( CLNO             ) = 1.0D-03
      RTOL( CLONO            ) = 1.0D-03
      RTOL( CLNO2            ) = 1.0D-03
      RTOL( HCL              ) = 1.0D-03
      RTOL( CLO              ) = 1.0D-03
      RTOL( CLONO2           ) = 1.0D-03
      RTOL( HOCL             ) = 1.0D-03
      RTOL( xCL              ) = 1.0D-03
      RTOL( xCLCCHO          ) = 1.0D-03
      RTOL( xCLACET          ) = 1.0D-03
      RTOL( CLCCHO           ) = 1.0D-03
      RTOL( CLACET           ) = 1.0D-03
      RTOL( CLCHO            ) = 1.0D-03
      RTOL( BNZNRXN          ) = 1.0D+00
      RTOL( BNZHRXN          ) = 1.0D+00
      RTOL( XYLNRXN          ) = 1.0D+00
      RTOL( XYLHRXN          ) = 1.0D+00
      RTOL( TOLNRXN          ) = 1.0D+00
      RTOL( TOLHRXN          ) = 1.0D+00
      RTOL( HCHO_PRIMARY     ) = 1.0D-03
      RTOL( CCHO_PRIMARY     ) = 1.0D-03
      RTOL( ACROLEIN_PRIMARY ) = 1.0D-03
      RTOL( AALKJ            ) = 1.0D-03
      RTOL( AOLGAJ           ) = 1.0D-03
      RTOL( AXYL1J           ) = 1.0D-03
      RTOL( AXYL2J           ) = 1.0D-03
      RTOL( ATOL1J           ) = 1.0D-03
      RTOL( ATOL2J           ) = 1.0D-03
      RTOL( ABNZ1J           ) = 1.0D-03
      RTOL( ABNZ2J           ) = 1.0D-03
      RTOL( ATRP1J           ) = 1.0D-03
      RTOL( AOLGBJ           ) = 1.0D-03
      RTOL( ATRP2J           ) = 1.0D-03
      RTOL( AISO1J           ) = 1.0D-03
      RTOL( AISO2J           ) = 1.0D-03
      RTOL( ASQTJ            ) = 1.0D-03


      RETURN

      END
