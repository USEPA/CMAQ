
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
C  REVISION HISTORY: Created by EBI solver program, Dec. 2, 2014
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

      N_EBISP  =  94
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
      EBISP(  40 ) = XYL
      EBISP(  41 ) = XYLRO2
      EBISP(  42 ) = ISOP
      EBISP(  43 ) = ISPD
      EBISP(  44 ) = ISOPRXN
      EBISP(  45 ) = TERP
      EBISP(  46 ) = TRPRXN
      EBISP(  47 ) = SO2
      EBISP(  48 ) = SULF
      EBISP(  49 ) = SULRXN
      EBISP(  50 ) = ETOH
      EBISP(  51 ) = ETHA
      EBISP(  52 ) = CL2
      EBISP(  53 ) = CL
      EBISP(  54 ) = HOCL
      EBISP(  55 ) = CLO
      EBISP(  56 ) = FMCL
      EBISP(  57 ) = HCL
      EBISP(  58 ) = CLNO2
      EBISP(  59 ) = TOLNRXN
      EBISP(  60 ) = TOLHRXN
      EBISP(  61 ) = XYLNRXN
      EBISP(  62 ) = XYLHRXN
      EBISP(  63 ) = BENZENE
      EBISP(  64 ) = BENZRO2
      EBISP(  65 ) = BNZNRXN
      EBISP(  66 ) = BNZHRXN
      EBISP(  67 ) = SESQ
      EBISP(  68 ) = SESQRXN
      EBISP(  69 ) = H2NO3PIJ
      EBISP(  70 ) = H2NO3PK
      EBISP(  71 ) = ACLI
      EBISP(  72 ) = ACLJ
      EBISP(  73 ) = ACLK
      EBISP(  74 ) = AXYL1J
      EBISP(  75 ) = AOLGAJ
      EBISP(  76 ) = AXYL2J
      EBISP(  77 ) = ATOL1J
      EBISP(  78 ) = ATOL2J
      EBISP(  79 ) = ABNZ1J
      EBISP(  80 ) = ABNZ2J
      EBISP(  81 ) = ATRP1J
      EBISP(  82 ) = AOLGBJ
      EBISP(  83 ) = ATRP2J
      EBISP(  84 ) = AISO1J
      EBISP(  85 ) = AISO2J
      EBISP(  86 ) = ASQTJ
      EBISP(  87 ) = APAH1J
      EBISP(  88 ) = APAH2J
      EBISP(  89 ) = AALK1J
      EBISP(  90 ) = AALK2J
      EBISP(  91 ) = APOCI
      EBISP(  92 ) = APNCOMI
      EBISP(  93 ) = APOCJ
      EBISP(  94 ) = APNCOMJ


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set species tolerances
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      RTOL( NO2      ) = 1.0D-03
      RTOL( NO       ) = 1.0D-03
      RTOL( O        ) = 1.0D+00
      RTOL( O3       ) = 1.0D-03
      RTOL( NO3      ) = 1.0D-03
      RTOL( O1D      ) = 1.0D+00
      RTOL( OH       ) = 1.0D-03
      RTOL( HO2      ) = 1.0D-03
      RTOL( N2O5     ) = 1.0D-03
      RTOL( HNO3     ) = 1.0D-03
      RTOL( HONO     ) = 1.0D-03
      RTOL( PNA      ) = 1.0D-03
      RTOL( H2O2     ) = 1.0D-03
      RTOL( XO2      ) = 1.0D-03
      RTOL( XO2N     ) = 1.0D-03
      RTOL( NTR      ) = 1.0D+00
      RTOL( ROOH     ) = 1.0D-03
      RTOL( FORM     ) = 1.0D-03
      RTOL( ALD2     ) = 1.0D-03
      RTOL( ALDX     ) = 1.0D-03
      RTOL( PAR      ) = 1.0D-03
      RTOL( CO       ) = 1.0D-03
      RTOL( MEO2     ) = 1.0D-03
      RTOL( MEPX     ) = 1.0D-03
      RTOL( MEOH     ) = 1.0D-03
      RTOL( HCO3     ) = 1.0D+00
      RTOL( FACD     ) = 1.0D-03
      RTOL( C2O3     ) = 1.0D-03
      RTOL( PAN      ) = 1.0D-03
      RTOL( PACD     ) = 1.0D-03
      RTOL( AACD     ) = 1.0D-03
      RTOL( CXO3     ) = 1.0D-03
      RTOL( PANX     ) = 1.0D-03
      RTOL( ROR      ) = 1.0D-03
      RTOL( OLE      ) = 1.0D-03
      RTOL( ETH      ) = 1.0D-03
      RTOL( IOLE     ) = 1.0D-03
      RTOL( TOL      ) = 1.0D-03
      RTOL( CRES     ) = 1.0D-03
      RTOL( TO2      ) = 1.0D-03
      RTOL( TOLRO2   ) = 1.0D-03
      RTOL( OPEN     ) = 1.0D-03
      RTOL( MGLY     ) = 1.0D-03
      RTOL( CRO      ) = 1.0D-03
      RTOL( CAT1     ) = 1.0D-03
      RTOL( CRON     ) = 1.0D-03
      RTOL( CRNO     ) = 1.0D-03
      RTOL( CRN2     ) = 1.0D-03
      RTOL( CRPX     ) = 1.0D-03
      RTOL( OPO3     ) = 1.0D-03
      RTOL( CAO2     ) = 1.0D-03
      RTOL( OPAN     ) = 1.0D-03
      RTOL( XYL      ) = 1.0D-03
      RTOL( XYLRO2   ) = 1.0D-03
      RTOL( ISOP     ) = 1.0D-03
      RTOL( ISPD     ) = 1.0D-03
      RTOL( ISOPRXN  ) = 1.0D+00
      RTOL( TERP     ) = 1.0D-03
      RTOL( TRPRXN   ) = 1.0D+00
      RTOL( SO2      ) = 1.0D-03
      RTOL( SULF     ) = 1.0D+00
      RTOL( SULRXN   ) = 1.0D+00
      RTOL( ETOH     ) = 1.0D-03
      RTOL( ETHA     ) = 1.0D-03
      RTOL( CL2      ) = 1.0D-03
      RTOL( CL       ) = 1.0D-03
      RTOL( HOCL     ) = 1.0D-03
      RTOL( CLO      ) = 1.0D-03
      RTOL( FMCL     ) = 1.0D-03
      RTOL( HCL      ) = 1.0D-03
      RTOL( CLNO2    ) = 1.0D-03
      RTOL( TOLNRXN  ) = 1.0D+00
      RTOL( TOLHRXN  ) = 1.0D+00
      RTOL( XYLNRXN  ) = 1.0D+00
      RTOL( XYLHRXN  ) = 1.0D+00
      RTOL( BENZENE  ) = 1.0D-03
      RTOL( BENZRO2  ) = 1.0D-03
      RTOL( BNZNRXN  ) = 1.0D+00
      RTOL( BNZHRXN  ) = 1.0D+00
      RTOL( SESQ     ) = 1.0D-03
      RTOL( SESQRXN  ) = 1.0D+00
      RTOL( H2NO3PIJ ) = 1.0D-03
      RTOL( H2NO3PK  ) = 1.0D-03
      RTOL( ACLI     ) = 1.0D-03
      RTOL( ACLJ     ) = 1.0D-03
      RTOL( ACLK     ) = 1.0D-03
      RTOL( AXYL1J   ) = 1.0D-03
      RTOL( AOLGAJ   ) = 1.0D-03
      RTOL( AXYL2J   ) = 1.0D-03
      RTOL( ATOL1J   ) = 1.0D-03
      RTOL( ATOL2J   ) = 1.0D-03
      RTOL( ABNZ1J   ) = 1.0D-03
      RTOL( ABNZ2J   ) = 1.0D-03
      RTOL( ATRP1J   ) = 1.0D-03
      RTOL( AOLGBJ   ) = 1.0D-03
      RTOL( ATRP2J   ) = 1.0D-03
      RTOL( AISO1J   ) = 1.0D-03
      RTOL( AISO2J   ) = 1.0D-03
      RTOL( ASQTJ    ) = 1.0D-03
      RTOL( APAH1J   ) = 1.0D-03
      RTOL( APAH2J   ) = 1.0D-03
      RTOL( AALK1J   ) = 1.0D-03
      RTOL( AALK2J   ) = 1.0D-03
      RTOL( APOCI    ) = 1.0D-03
      RTOL( APNCOMI  ) = 1.0D-03
      RTOL( APOCJ    ) = 1.0D-03
      RTOL( APNCOMJ  ) = 1.0D-03


      RETURN

      END
