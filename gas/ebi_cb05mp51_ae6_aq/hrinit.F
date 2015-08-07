
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
C  PRECONDITIONS: For the CB05MP51 family of mechanisms
C
C  RETURN VALUES: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Aug  6, 2015
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
     &          PNEG( N_SPEC),
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

       ALLOCATE( YCCELL( MODEL_SPECIES), STAT = IOS )  ! array for degradation routines
       IF ( IOS .NE. 0 ) THEN
          MSG = 'Error allocating YCCELL '
          CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
       END IF


c..Allocate and define ebi species

      N_EBISP  = 129
      ALLOCATE( EBISP( N_EBISP ), STAT = IOS ) 
      IF ( IOS .NE. 0 ) THEN
         MSG = 'Error allocating EBISP' 
         CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      EBISP(   1 ) = HNO3
      EBISP(   2 ) = H2O2
      EBISP(   3 ) = XO2
      EBISP(   4 ) = XO2N
      EBISP(   5 ) = NTROH
      EBISP(   6 ) = NTRALK
      EBISP(   7 ) = ROOH
      EBISP(   8 ) = ALD2
      EBISP(   9 ) = ALDX
      EBISP(  10 ) = CO
      EBISP(  11 ) = MEO2
      EBISP(  12 ) = FORM
      EBISP(  13 ) = MEPX
      EBISP(  14 ) = MEOH
      EBISP(  15 ) = HCO3
      EBISP(  16 ) = FACD
      EBISP(  17 ) = PACD
      EBISP(  18 ) = AACD
      EBISP(  19 ) = CXO3
      EBISP(  20 ) = PANX
      EBISP(  21 ) = PAR
      EBISP(  22 ) = ROR
      EBISP(  23 ) = OLE
      EBISP(  24 ) = ETH
      EBISP(  25 ) = IOLE
      EBISP(  26 ) = TOL
      EBISP(  27 ) = CRES
      EBISP(  28 ) = TO2
      EBISP(  29 ) = TOLRO2
      EBISP(  30 ) = OPEN
      EBISP(  31 ) = MGLY
      EBISP(  32 ) = CRO
      EBISP(  33 ) = CAT1
      EBISP(  34 ) = CRON
      EBISP(  35 ) = CRNO
      EBISP(  36 ) = CRN2
      EBISP(  37 ) = CRPX
      EBISP(  38 ) = OPO3
      EBISP(  39 ) = CAO2
      EBISP(  40 ) = OPAN
      EBISP(  41 ) = XYLMN
      EBISP(  42 ) = XYLRO2
      EBISP(  43 ) = NAPH
      EBISP(  44 ) = PAHRO2
      EBISP(  45 ) = ISOP
      EBISP(  46 ) = ISPD
      EBISP(  47 ) = IXO2
      EBISP(  48 ) = IXO2N
      EBISP(  49 ) = ISOPRXN
      EBISP(  50 ) = NTRI1
      EBISP(  51 ) = ISOPX
      EBISP(  52 ) = IEPOX
      EBISP(  53 ) = yOH
      EBISP(  54 ) = NTRI2
      EBISP(  55 ) = TERP
      EBISP(  56 ) = TRPRXN
      EBISP(  57 ) = XO2T
      EBISP(  58 ) = SO2
      EBISP(  59 ) = SULF
      EBISP(  60 ) = SULRXN
      EBISP(  61 ) = ETOH
      EBISP(  62 ) = ETHA
      EBISP(  63 ) = CL2
      EBISP(  64 ) = CL
      EBISP(  65 ) = HOCL
      EBISP(  66 ) = CLO
      EBISP(  67 ) = FMCL
      EBISP(  68 ) = HCL
      EBISP(  69 ) = CLNO2
      EBISP(  70 ) = TOLNRXN
      EBISP(  71 ) = TOLHRXN
      EBISP(  72 ) = XYLNRXN
      EBISP(  73 ) = XYLHRXN
      EBISP(  74 ) = BENZENE
      EBISP(  75 ) = BENZRO2
      EBISP(  76 ) = BNZNRXN
      EBISP(  77 ) = BNZHRXN
      EBISP(  78 ) = SESQ
      EBISP(  79 ) = SESQRXN
      EBISP(  80 ) = PAHNRXN
      EBISP(  81 ) = PAHHRXN
      EBISP(  82 ) = SOAALK
      EBISP(  83 ) = ALKRXN
      EBISP(  84 ) = NTRC1
      EBISP(  85 ) = NTRC2
      EBISP(  86 ) = NXO2N
      EBISP(  87 ) = NTRI1O2
      EBISP(  88 ) = NTRI2O2
      EBISP(  89 ) = H2NO3PIJ
      EBISP(  90 ) = H2NO3PK
      EBISP(  91 ) = ACLI
      EBISP(  92 ) = ACLJ
      EBISP(  93 ) = ACLK
      EBISP(  94 ) = AISO3J
      EBISP(  95 ) = AXYL1J
      EBISP(  96 ) = AOLGAJ
      EBISP(  97 ) = AXYL2J
      EBISP(  98 ) = ATOL1J
      EBISP(  99 ) = ATOL2J
      EBISP( 100 ) = ABNZ1J
      EBISP( 101 ) = ABNZ2J
      EBISP( 102 ) = ATRP1J
      EBISP( 103 ) = AOLGBJ
      EBISP( 104 ) = ATRP2J
      EBISP( 105 ) = AISO1J
      EBISP( 106 ) = AISO2J
      EBISP( 107 ) = ASQTJ
      EBISP( 108 ) = APAH1J
      EBISP( 109 ) = APAH2J
      EBISP( 110 ) = AALK1J
      EBISP( 111 ) = AALK2J
      EBISP( 112 ) = APOCI
      EBISP( 113 ) = APNCOMI
      EBISP( 114 ) = APOCJ
      EBISP( 115 ) = APNCOMJ
      EBISP( 116 ) = FORM_PRIMARY
      EBISP( 117 ) = ALD2_PRIMARY
      EBISP( 118 ) = BUTADIENE13
      EBISP( 119 ) = ACROLEIN
      EBISP( 120 ) = ACROLEIN_PRIMARY
      EBISP( 121 ) = TOLU
      EBISP( 122 ) = MXYL
      EBISP( 123 ) = OXYL
      EBISP( 124 ) = PXYL
      EBISP( 125 ) = APIN
      EBISP( 126 ) = BPIN
      EBISP( 127 ) = HG
      EBISP( 128 ) = HGIIAER
      EBISP( 129 ) = HGIIGAS


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set species tolerances
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      RTOL( NO2              ) = 1.0D-03
      RTOL( NO               ) = 1.0D-03
      RTOL( O                ) = 1.0D+00
      RTOL( O3               ) = 1.0D-03
      RTOL( NO3              ) = 1.0D-03
      RTOL( O1D              ) = 1.0D+00
      RTOL( OH               ) = 1.0D-03
      RTOL( HO2              ) = 1.0D-03
      RTOL( N2O5             ) = 1.0D-03
      RTOL( HNO3             ) = 1.0D-03
      RTOL( HONO             ) = 1.0D-03
      RTOL( PNA              ) = 1.0D-03
      RTOL( H2O2             ) = 1.0D-03
      RTOL( XO2              ) = 1.0D-03
      RTOL( XO2N             ) = 1.0D-03
      RTOL( NTROH            ) = 1.0D-03
      RTOL( NTRALK           ) = 1.0D-03
      RTOL( ROOH             ) = 1.0D-03
      RTOL( ALD2             ) = 1.0D-03
      RTOL( ALDX             ) = 1.0D-03
      RTOL( CO               ) = 1.0D-03
      RTOL( MEO2             ) = 1.0D-03
      RTOL( FORM             ) = 1.0D-03
      RTOL( MEPX             ) = 1.0D-03
      RTOL( MEOH             ) = 1.0D-03
      RTOL( HCO3             ) = 1.0D+00
      RTOL( FACD             ) = 1.0D-03
      RTOL( C2O3             ) = 1.0D-03
      RTOL( PAN              ) = 1.0D-03
      RTOL( PACD             ) = 1.0D-03
      RTOL( AACD             ) = 1.0D-03
      RTOL( CXO3             ) = 1.0D-03
      RTOL( PANX             ) = 1.0D-03
      RTOL( PAR              ) = 1.0D-03
      RTOL( ROR              ) = 1.0D-03
      RTOL( OLE              ) = 1.0D-03
      RTOL( ETH              ) = 1.0D-03
      RTOL( IOLE             ) = 1.0D-03
      RTOL( TOL              ) = 1.0D-03
      RTOL( CRES             ) = 1.0D-03
      RTOL( TO2              ) = 1.0D-03
      RTOL( TOLRO2           ) = 1.0D-03
      RTOL( OPEN             ) = 1.0D-03
      RTOL( MGLY             ) = 1.0D-03
      RTOL( CRO              ) = 1.0D-03
      RTOL( CAT1             ) = 1.0D-03
      RTOL( CRON             ) = 1.0D-03
      RTOL( CRNO             ) = 1.0D-03
      RTOL( CRN2             ) = 1.0D-03
      RTOL( CRPX             ) = 1.0D-03
      RTOL( OPO3             ) = 1.0D-03
      RTOL( CAO2             ) = 1.0D-03
      RTOL( OPAN             ) = 1.0D-03
      RTOL( XYLMN            ) = 1.0D-03
      RTOL( XYLRO2           ) = 1.0D-03
      RTOL( NAPH             ) = 1.0D-03
      RTOL( PAHRO2           ) = 1.0D+00
      RTOL( ISOP             ) = 1.0D-03
      RTOL( ISPD             ) = 1.0D-03
      RTOL( IXO2             ) = 1.0D-03
      RTOL( IXO2N            ) = 1.0D-03
      RTOL( ISOPRXN          ) = 1.0D+00
      RTOL( NTRI1            ) = 1.0D-03
      RTOL( ISOPX            ) = 1.0D-03
      RTOL( IEPOX            ) = 1.0D-03
      RTOL( yOH              ) = 1.0D-03
      RTOL( NTRI2            ) = 1.0D-03
      RTOL( TERP             ) = 1.0D-03
      RTOL( TRPRXN           ) = 1.0D+00
      RTOL( XO2T             ) = 1.0D-03
      RTOL( SO2              ) = 1.0D-03
      RTOL( SULF             ) = 1.0D+00
      RTOL( SULRXN           ) = 1.0D+00
      RTOL( ETOH             ) = 1.0D-03
      RTOL( ETHA             ) = 1.0D-03
      RTOL( CL2              ) = 1.0D-03
      RTOL( CL               ) = 1.0D-03
      RTOL( HOCL             ) = 1.0D-03
      RTOL( CLO              ) = 1.0D-03
      RTOL( FMCL             ) = 1.0D-03
      RTOL( HCL              ) = 1.0D-03
      RTOL( CLNO2            ) = 1.0D-03
      RTOL( TOLNRXN          ) = 1.0D+00
      RTOL( TOLHRXN          ) = 1.0D+00
      RTOL( XYLNRXN          ) = 1.0D+00
      RTOL( XYLHRXN          ) = 1.0D+00
      RTOL( BENZENE          ) = 1.0D-03
      RTOL( BENZRO2          ) = 1.0D-03
      RTOL( BNZNRXN          ) = 1.0D+00
      RTOL( BNZHRXN          ) = 1.0D+00
      RTOL( SESQ             ) = 1.0D-03
      RTOL( SESQRXN          ) = 1.0D+00
      RTOL( PAHNRXN          ) = 1.0D+00
      RTOL( PAHHRXN          ) = 1.0D+00
      RTOL( SOAALK           ) = 1.0D-03
      RTOL( ALKRXN           ) = 1.0D+00
      RTOL( NTRC1            ) = 1.0D-03
      RTOL( NTRC2            ) = 1.0D-03
      RTOL( NXO2N            ) = 1.0D-03
      RTOL( NTRI1O2          ) = 1.0D-03
      RTOL( NTRI2O2          ) = 1.0D-03
      RTOL( H2NO3PIJ         ) = 1.0D-03
      RTOL( H2NO3PK          ) = 1.0D-03
      RTOL( ACLI             ) = 1.0D-03
      RTOL( ACLJ             ) = 1.0D-03
      RTOL( ACLK             ) = 1.0D-03
      RTOL( AISO3J           ) = 1.0D-03
      RTOL( AXYL1J           ) = 1.0D-03
      RTOL( AOLGAJ           ) = 1.0D-03
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
      RTOL( APAH1J           ) = 1.0D-03
      RTOL( APAH2J           ) = 1.0D-03
      RTOL( AALK1J           ) = 1.0D-03
      RTOL( AALK2J           ) = 1.0D-03
      RTOL( APOCI            ) = 1.0D-03
      RTOL( APNCOMI          ) = 1.0D-03
      RTOL( APOCJ            ) = 1.0D-03
      RTOL( APNCOMJ          ) = 1.0D-03
      RTOL( FORM_PRIMARY     ) = 1.0D-03
      RTOL( ALD2_PRIMARY     ) = 1.0D-03
      RTOL( BUTADIENE13      ) = 1.0D-03
      RTOL( ACROLEIN         ) = 1.0D-03
      RTOL( ACROLEIN_PRIMARY ) = 1.0D-03
      RTOL( TOLU             ) = 1.0D-03
      RTOL( MXYL             ) = 1.0D-03
      RTOL( OXYL             ) = 1.0D-03
      RTOL( PXYL             ) = 1.0D-03
      RTOL( APIN             ) = 1.0D-03
      RTOL( BPIN             ) = 1.0D-03
      RTOL( HG               ) = 1.0D-03
      RTOL( HGIIAER          ) = 1.0D-03
      RTOL( HGIIGAS          ) = 1.0D-03


      RETURN

      END
