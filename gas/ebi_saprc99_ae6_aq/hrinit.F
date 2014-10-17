
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
C  PRECONDITIONS: For the SAPRC99 family of mechanisms
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

      N_EBISP  =  93
      ALLOCATE( EBISP( N_EBISP ), STAT = IOS ) 
      IF ( IOS .NE. 0 ) THEN
         MSG = 'Error allocating EBISP' 
         CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      EBISP(   1 ) = HNO3
      EBISP(   2 ) = CO
      EBISP(   3 ) = HO2H
      EBISP(   4 ) = SO2
      EBISP(   5 ) = SULF
      EBISP(   6 ) = SULRXN
      EBISP(   7 ) = C_O2
      EBISP(   8 ) = HCHO
      EBISP(   9 ) = COOH
      EBISP(  10 ) = MEOH
      EBISP(  11 ) = RO2_R
      EBISP(  12 ) = ROOH
      EBISP(  13 ) = R2O2
      EBISP(  14 ) = RO2_N
      EBISP(  15 ) = RNO3
      EBISP(  16 ) = MEK
      EBISP(  17 ) = PROD2
      EBISP(  18 ) = CCO_OOH
      EBISP(  19 ) = CCO_OH
      EBISP(  20 ) = RCO_O2
      EBISP(  21 ) = PAN2
      EBISP(  22 ) = CCHO
      EBISP(  23 ) = RCO_OOH
      EBISP(  24 ) = RCO_OH
      EBISP(  25 ) = BZCO_O2
      EBISP(  26 ) = PBZN
      EBISP(  27 ) = BZ_O
      EBISP(  28 ) = MA_RCO3
      EBISP(  29 ) = MA_PAN
      EBISP(  30 ) = TBU_O
      EBISP(  31 ) = ACET
      EBISP(  32 ) = NPHE
      EBISP(  33 ) = PHEN
      EBISP(  34 ) = BZNO2_O
      EBISP(  35 ) = HOCOO
      EBISP(  36 ) = HCOOH
      EBISP(  37 ) = RCHO
      EBISP(  38 ) = GLY
      EBISP(  39 ) = MGLY
      EBISP(  40 ) = BACL
      EBISP(  41 ) = CRES
      EBISP(  42 ) = BALD
      EBISP(  43 ) = METHACRO
      EBISP(  44 ) = MVK
      EBISP(  45 ) = ISOPROD
      EBISP(  46 ) = DCB1
      EBISP(  47 ) = DCB2
      EBISP(  48 ) = DCB3
      EBISP(  49 ) = ETHENE
      EBISP(  50 ) = ISOPRENE
      EBISP(  51 ) = ISOPRXN
      EBISP(  52 ) = TRP1
      EBISP(  53 ) = TRPRXN
      EBISP(  54 ) = ALK1
      EBISP(  55 ) = ALK2
      EBISP(  56 ) = ALK3
      EBISP(  57 ) = ALK4
      EBISP(  58 ) = ALK5
      EBISP(  59 ) = ALK5RXN
      EBISP(  60 ) = ARO1
      EBISP(  61 ) = ARO1RO2
      EBISP(  62 ) = TOLNRXN
      EBISP(  63 ) = TOLHRXN
      EBISP(  64 ) = ARO2
      EBISP(  65 ) = ARO2RO2
      EBISP(  66 ) = XYLNRXN
      EBISP(  67 ) = XYLHRXN
      EBISP(  68 ) = BENZENE
      EBISP(  69 ) = BENZRO2
      EBISP(  70 ) = BNZNRXN
      EBISP(  71 ) = BNZHRXN
      EBISP(  72 ) = OLE1
      EBISP(  73 ) = OLE2
      EBISP(  74 ) = SESQ
      EBISP(  75 ) = SESQRXN
      EBISP(  76 ) = AALKJ
      EBISP(  77 ) = AOLGAJ
      EBISP(  78 ) = AXYL1J
      EBISP(  79 ) = AXYL2J
      EBISP(  80 ) = ATOL1J
      EBISP(  81 ) = ATOL2J
      EBISP(  82 ) = ABNZ1J
      EBISP(  83 ) = ABNZ2J
      EBISP(  84 ) = ATRP1J
      EBISP(  85 ) = AOLGBJ
      EBISP(  86 ) = ATRP2J
      EBISP(  87 ) = AISO1J
      EBISP(  88 ) = AISO2J
      EBISP(  89 ) = ASQTJ
      EBISP(  90 ) = APOCI
      EBISP(  91 ) = APNCOMI
      EBISP(  92 ) = APOCJ
      EBISP(  93 ) = APNCOMJ


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set species tolerances
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      RTOL( NO2      ) = 1.0D-03
      RTOL( NO       ) = 1.0D-03
      RTOL( O3P      ) = 1.0D+00
      RTOL( O3       ) = 1.0D-03
      RTOL( NO3      ) = 1.0D-03
      RTOL( N2O5     ) = 1.0D-03
      RTOL( HNO3     ) = 1.0D-03
      RTOL( O1D2     ) = 1.0D+00
      RTOL( HO       ) = 1.0D-03
      RTOL( HONO     ) = 1.0D-03
      RTOL( HO2      ) = 1.0D-03
      RTOL( CO       ) = 1.0D-03
      RTOL( HNO4     ) = 1.0D-03
      RTOL( HO2H     ) = 1.0D-03
      RTOL( SO2      ) = 1.0D-03
      RTOL( SULF     ) = 1.0D+00
      RTOL( SULRXN   ) = 1.0D+00
      RTOL( C_O2     ) = 1.0D-03
      RTOL( HCHO     ) = 1.0D-03
      RTOL( COOH     ) = 1.0D-03
      RTOL( MEOH     ) = 1.0D-03
      RTOL( RO2_R    ) = 1.0D-03
      RTOL( ROOH     ) = 1.0D-03
      RTOL( R2O2     ) = 1.0D-03
      RTOL( RO2_N    ) = 1.0D-03
      RTOL( RNO3     ) = 1.0D-03
      RTOL( MEK      ) = 1.0D-03
      RTOL( PROD2    ) = 1.0D-03
      RTOL( CCO_O2   ) = 1.0D-03
      RTOL( PAN      ) = 1.0D-03
      RTOL( CCO_OOH  ) = 1.0D-03
      RTOL( CCO_OH   ) = 1.0D-03
      RTOL( RCO_O2   ) = 1.0D-03
      RTOL( PAN2     ) = 1.0D-03
      RTOL( CCHO     ) = 1.0D-03
      RTOL( RCO_OOH  ) = 1.0D-03
      RTOL( RCO_OH   ) = 1.0D-03
      RTOL( BZCO_O2  ) = 1.0D-03
      RTOL( PBZN     ) = 1.0D-03
      RTOL( BZ_O     ) = 1.0D+00
      RTOL( MA_RCO3  ) = 1.0D-03
      RTOL( MA_PAN   ) = 1.0D-03
      RTOL( TBU_O    ) = 1.0D+00
      RTOL( ACET     ) = 1.0D-03
      RTOL( NPHE     ) = 1.0D-03
      RTOL( PHEN     ) = 1.0D-03
      RTOL( BZNO2_O  ) = 1.0D+00
      RTOL( HOCOO    ) = 1.0D+00
      RTOL( HCOOH    ) = 1.0D-03
      RTOL( RCHO     ) = 1.0D-03
      RTOL( GLY      ) = 1.0D-03
      RTOL( MGLY     ) = 1.0D-03
      RTOL( BACL     ) = 1.0D-03
      RTOL( CRES     ) = 1.0D-03
      RTOL( BALD     ) = 1.0D-03
      RTOL( METHACRO ) = 1.0D-03
      RTOL( MVK      ) = 1.0D-03
      RTOL( ISOPROD  ) = 1.0D-03
      RTOL( DCB1     ) = 1.0D-03
      RTOL( DCB2     ) = 1.0D-03
      RTOL( DCB3     ) = 1.0D-03
      RTOL( ETHENE   ) = 1.0D-03
      RTOL( ISOPRENE ) = 1.0D-03
      RTOL( ISOPRXN  ) = 1.0D+00
      RTOL( TRP1     ) = 1.0D-03
      RTOL( TRPRXN   ) = 1.0D+00
      RTOL( ALK1     ) = 1.0D-03
      RTOL( ALK2     ) = 1.0D-03
      RTOL( ALK3     ) = 1.0D-03
      RTOL( ALK4     ) = 1.0D-03
      RTOL( ALK5     ) = 1.0D-03
      RTOL( ALK5RXN  ) = 1.0D+00
      RTOL( ARO1     ) = 1.0D-03
      RTOL( ARO1RO2  ) = 1.0D-03
      RTOL( TOLNRXN  ) = 1.0D+00
      RTOL( TOLHRXN  ) = 1.0D+00
      RTOL( ARO2     ) = 1.0D-03
      RTOL( ARO2RO2  ) = 1.0D-03
      RTOL( XYLNRXN  ) = 1.0D+00
      RTOL( XYLHRXN  ) = 1.0D+00
      RTOL( BENZENE  ) = 1.0D-03
      RTOL( BENZRO2  ) = 1.0D-03
      RTOL( BNZNRXN  ) = 1.0D+00
      RTOL( BNZHRXN  ) = 1.0D+00
      RTOL( OLE1     ) = 1.0D-03
      RTOL( OLE2     ) = 1.0D-03
      RTOL( SESQ     ) = 1.0D-03
      RTOL( SESQRXN  ) = 1.0D+00
      RTOL( AALKJ    ) = 1.0D-03
      RTOL( AOLGAJ   ) = 1.0D-03
      RTOL( AXYL1J   ) = 1.0D-03
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
      RTOL( APOCI    ) = 1.0D-03
      RTOL( APNCOMI  ) = 1.0D-03
      RTOL( APOCJ    ) = 1.0D-03
      RTOL( APNCOMJ  ) = 1.0D-03


      RETURN

      END
