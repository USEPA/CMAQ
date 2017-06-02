
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
C  PRECONDITIONS: For the CB05TUCL family of mechanisms
C
C  RETURN VALUES: None
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, May 26, 2017
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



c..Allocate and define ebi species

      N_EBISP  = 108
      ALLOCATE( EBISP( N_EBISP ), STAT = IOS ) 
      IF ( IOS .NE. 0 ) THEN
         MSG = 'Error allocating EBISP' 
         CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      EBISP(   1 ) = HNO3
      EBISP(   2 ) = H2O2
      EBISP(   3 ) = XO2
      EBISP(   4 ) = XO2N
      EBISP(   5 ) = NTR
      EBISP(   6 ) = ROOH
      EBISP(   7 ) = FORM
      EBISP(   8 ) = ALD2
      EBISP(   9 ) = ALDX
      EBISP(  10 ) = PAR
      EBISP(  11 ) = CO
      EBISP(  12 ) = MEO2
      EBISP(  13 ) = MEPX
      EBISP(  14 ) = MEOH
      EBISP(  15 ) = HCO3
      EBISP(  16 ) = FACD
      EBISP(  17 ) = PACD
      EBISP(  18 ) = AACD
      EBISP(  19 ) = CXO3
      EBISP(  20 ) = PANX
      EBISP(  21 ) = ROR
      EBISP(  22 ) = OLE
      EBISP(  23 ) = ETH
      EBISP(  24 ) = IOLE
      EBISP(  25 ) = TOL
      EBISP(  26 ) = CRES
      EBISP(  27 ) = TO2
      EBISP(  28 ) = TOLRO2
      EBISP(  29 ) = OPEN
      EBISP(  30 ) = MGLY
      EBISP(  31 ) = CRO
      EBISP(  32 ) = CAT1
      EBISP(  33 ) = CRON
      EBISP(  34 ) = CRNO
      EBISP(  35 ) = CRN2
      EBISP(  36 ) = CRPX
      EBISP(  37 ) = OPO3
      EBISP(  38 ) = CAO2
      EBISP(  39 ) = OPAN
      EBISP(  40 ) = XYLMN
      EBISP(  41 ) = XYLRO2
      EBISP(  42 ) = NAPH
      EBISP(  43 ) = PAHRO2
      EBISP(  44 ) = ISOP
      EBISP(  45 ) = ISPD
      EBISP(  46 ) = ISOPRXN
      EBISP(  47 ) = TERP
      EBISP(  48 ) = TRPRXN
      EBISP(  49 ) = SO2
      EBISP(  50 ) = SULF
      EBISP(  51 ) = SULRXN
      EBISP(  52 ) = ETOH
      EBISP(  53 ) = ETHA
      EBISP(  54 ) = CL2
      EBISP(  55 ) = CL
      EBISP(  56 ) = HOCL
      EBISP(  57 ) = CLO
      EBISP(  58 ) = FMCL
      EBISP(  59 ) = HCL
      EBISP(  60 ) = CLNO2
      EBISP(  61 ) = TOLNRXN
      EBISP(  62 ) = TOLHRXN
      EBISP(  63 ) = XYLNRXN
      EBISP(  64 ) = XYLHRXN
      EBISP(  65 ) = BENZENE
      EBISP(  66 ) = BENZRO2
      EBISP(  67 ) = BNZNRXN
      EBISP(  68 ) = BNZHRXN
      EBISP(  69 ) = SESQ
      EBISP(  70 ) = SESQRXN
      EBISP(  71 ) = PAHNRXN
      EBISP(  72 ) = PAHHRXN
      EBISP(  73 ) = SOAALK
      EBISP(  74 ) = ALKRXN
      EBISP(  75 ) = H2NO3PIJ
      EBISP(  76 ) = H2NO3PK
      EBISP(  77 ) = ACLI
      EBISP(  78 ) = ACLJ
      EBISP(  79 ) = ACLK
      EBISP(  80 ) = AXYL1J
      EBISP(  81 ) = AOLGAJ
      EBISP(  82 ) = AXYL2J
      EBISP(  83 ) = ATOL1J
      EBISP(  84 ) = ATOL2J
      EBISP(  85 ) = ABNZ1J
      EBISP(  86 ) = ABNZ2J
      EBISP(  87 ) = ATRP1J
      EBISP(  88 ) = AOLGBJ
      EBISP(  89 ) = ATRP2J
      EBISP(  90 ) = AISO1J
      EBISP(  91 ) = AISO2J
      EBISP(  92 ) = ASQTJ
      EBISP(  93 ) = APAH1J
      EBISP(  94 ) = APAH2J
      EBISP(  95 ) = AALK1J
      EBISP(  96 ) = AALK2J
      EBISP(  97 ) = PCVOC
      EBISP(  98 ) = PCSOARXN
      EBISP(  99 ) = VLVPO1
      EBISP( 100 ) = VSVPO1
      EBISP( 101 ) = VSVPO2
      EBISP( 102 ) = VSVPO3
      EBISP( 103 ) = VIVPO1
      EBISP( 104 ) = VLVOO1
      EBISP( 105 ) = VLVOO2
      EBISP( 106 ) = VSVOO2
      EBISP( 107 ) = VSVOO3
      EBISP( 108 ) = VSVOO1


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set species tolerances
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      RTOL( NO2      ) = 5.0D-04
      RTOL( NO       ) = 5.0D-04
      RTOL( O        ) = 1.0D+00
      RTOL( O3       ) = 5.0D-04
      RTOL( NO3      ) = 5.0D-04
      RTOL( O1D      ) = 1.0D+00
      RTOL( OH       ) = 5.0D-04
      RTOL( HO2      ) = 5.0D-04
      RTOL( N2O5     ) = 5.0D-04
      RTOL( HNO3     ) = 5.0D-04
      RTOL( HONO     ) = 5.0D-04
      RTOL( PNA      ) = 5.0D-04
      RTOL( H2O2     ) = 5.0D-04
      RTOL( XO2      ) = 5.0D-04
      RTOL( XO2N     ) = 5.0D-04
      RTOL( NTR      ) = 1.0D+00
      RTOL( ROOH     ) = 5.0D-04
      RTOL( FORM     ) = 5.0D-04
      RTOL( ALD2     ) = 5.0D-04
      RTOL( ALDX     ) = 5.0D-04
      RTOL( PAR      ) = 5.0D-04
      RTOL( CO       ) = 5.0D-04
      RTOL( MEO2     ) = 5.0D-04
      RTOL( MEPX     ) = 5.0D-04
      RTOL( MEOH     ) = 5.0D-04
      RTOL( HCO3     ) = 1.0D+00
      RTOL( FACD     ) = 5.0D-04
      RTOL( C2O3     ) = 5.0D-04
      RTOL( PAN      ) = 5.0D-04
      RTOL( PACD     ) = 5.0D-04
      RTOL( AACD     ) = 5.0D-04
      RTOL( CXO3     ) = 5.0D-04
      RTOL( PANX     ) = 5.0D-04
      RTOL( ROR      ) = 5.0D-04
      RTOL( OLE      ) = 5.0D-04
      RTOL( ETH      ) = 5.0D-04
      RTOL( IOLE     ) = 5.0D-04
      RTOL( TOL      ) = 5.0D-04
      RTOL( CRES     ) = 5.0D-04
      RTOL( TO2      ) = 5.0D-04
      RTOL( TOLRO2   ) = 5.0D-04
      RTOL( OPEN     ) = 5.0D-04
      RTOL( MGLY     ) = 5.0D-04
      RTOL( CRO      ) = 5.0D-04
      RTOL( CAT1     ) = 5.0D-04
      RTOL( CRON     ) = 5.0D-04
      RTOL( CRNO     ) = 5.0D-04
      RTOL( CRN2     ) = 5.0D-04
      RTOL( CRPX     ) = 5.0D-04
      RTOL( OPO3     ) = 5.0D-04
      RTOL( CAO2     ) = 5.0D-04
      RTOL( OPAN     ) = 5.0D-04
      RTOL( XYLMN    ) = 5.0D-04
      RTOL( XYLRO2   ) = 5.0D-04
      RTOL( NAPH     ) = 5.0D-04
      RTOL( PAHRO2   ) = 1.0D+00
      RTOL( ISOP     ) = 5.0D-04
      RTOL( ISPD     ) = 5.0D-04
      RTOL( ISOPRXN  ) = 1.0D+00
      RTOL( TERP     ) = 5.0D-04
      RTOL( TRPRXN   ) = 1.0D+00
      RTOL( SO2      ) = 5.0D-04
      RTOL( SULF     ) = 1.0D+00
      RTOL( SULRXN   ) = 1.0D+00
      RTOL( ETOH     ) = 5.0D-04
      RTOL( ETHA     ) = 5.0D-04
      RTOL( CL2      ) = 5.0D-04
      RTOL( CL       ) = 5.0D-04
      RTOL( HOCL     ) = 5.0D-04
      RTOL( CLO      ) = 5.0D-04
      RTOL( FMCL     ) = 5.0D-04
      RTOL( HCL      ) = 5.0D-04
      RTOL( CLNO2    ) = 5.0D-04
      RTOL( TOLNRXN  ) = 1.0D+00
      RTOL( TOLHRXN  ) = 1.0D+00
      RTOL( XYLNRXN  ) = 1.0D+00
      RTOL( XYLHRXN  ) = 1.0D+00
      RTOL( BENZENE  ) = 5.0D-04
      RTOL( BENZRO2  ) = 5.0D-04
      RTOL( BNZNRXN  ) = 1.0D+00
      RTOL( BNZHRXN  ) = 1.0D+00
      RTOL( SESQ     ) = 5.0D-04
      RTOL( SESQRXN  ) = 1.0D+00
      RTOL( PAHNRXN  ) = 1.0D+00
      RTOL( PAHHRXN  ) = 1.0D+00
      RTOL( SOAALK   ) = 5.0D-04
      RTOL( ALKRXN   ) = 1.0D+00
      RTOL( H2NO3PIJ ) = 5.0D-04
      RTOL( H2NO3PK  ) = 5.0D-04
      RTOL( ACLI     ) = 5.0D-04
      RTOL( ACLJ     ) = 5.0D-04
      RTOL( ACLK     ) = 5.0D-04
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
