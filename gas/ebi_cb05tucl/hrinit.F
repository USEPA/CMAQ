
C***********************************************************************
C   Portions of Models-3/CMAQ software were developed or based on      *
C   information from various groups: Federal Government employees,     *
C   contractors working on a United States Government contract, and    *
C   non-Federal sources (including research institutions).  These      *
C   research institutions have given the Government permission to      *
C   use, prepare derivative works, and distribute copies of their      *
C   work in Models-3/CMAQ to the public and to permit others to do     *
C   so.  EPA therefore grants similar permissions for use of the       *
C   Models-3/CMAQ software, but users are requested to provide copies  *
C   of derivative works to the Government without restrictions as to   *
C   use by others.  Users are responsible for acquiring their own      *
C   copies of commercial software associated with Models-3/CMAQ and    *
C   for complying with vendor requirements.  Software copyrights by    *
C   the MCNC Environmental Modeling Center are used with their         *
C   permissions subject to the above restrictions.                     *
C***********************************************************************
C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/gas/ebi_cb05tucl/hrinit.F,v 1.1 2011/03/17 19:14:26 sjr Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE HRINIT

C-----------------------------------------------------------------------
C  Function: To initialize species tolerances, arrays, and indices
C
C  Preconditions: For the CB05TUCL family of mechanisms
C
C  Return Values: None
C
C  Key Subroutines/Functions Called: None
C
C  Revision History: Created by EBI solver program, Jan. 21, 2011
C   21 Jun 10 J.Young: convert for Namelist redesign
C-----------------------------------------------------------------------

      USE CGRID_SPCS, ONLY: N_GC_SPC   ! CGRID mechanism species
      USE HRDATA

      IMPLICIT NONE

C.....Includes:
      INCLUDE SUBST_RXCMMN    ! Mechanism reaction common block

C.....Arguments: NONE

C.....Parameters: NONE

C.....External Functions:
      INTEGER  FINDEX         ! Finds location of a number in a list

C.....Saved Variables:
      CHARACTER( 16 ), SAVE  ::  PNAME = 'HRINIT'   ! Program name

C.....Local Variables:
      CHARACTER( 132 ) :: MSG = ' '       ! Log message

      INTEGER IND             ! Species index
      INTEGER N               ! Loop index
      INTEGER IOS             ! Allocate error status

C-----------------------------------------------------------------------

      N_SPEC = N_GC_SPC
      N_RXNS = NRXNS
      N_SPCL_RKS =   0

      ALLOCATE( RKI( NRXNS ),
     &          RXRAT( NRXNS ),
     &          RTOL( N_SPEC),
     &          YC(   N_SPEC),
     &          YC0(  N_SPEC),
     &          YCP(  N_SPEC),
     &          PROD( N_SPEC),
     &          LOSS( N_SPEC),
     &          PNEG( N_SPEC), STAT = IOS )
      IF ( IOS .NE. 0 ) THEN
         MSG = 'Error allocating RKI, RXRAT, RTOL, YC, YC0, YCP, PROD, LOSS, or PNEG'
         CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      NING1 = 4
      NING2 = 4

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set species indices and pointers
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      NO2      =   1
      NO       =   2
      O        =   3
      O3       =   4
      NO3      =   5
      O1D      =   6
      OH       =   7
      HO2      =   8
      N2O5     =   9
      HNO3     =  10
      HONO     =  11
      PNA      =  12
      H2O2     =  13
      XO2      =  14
      XO2N     =  15
      NTR      =  16
      ROOH     =  17
      FORM     =  18
      ALD2     =  19
      ALDX     =  20
      PAR      =  21
      CO       =  22
      MEO2     =  23
      MEPX     =  24
      MEOH     =  25
      HCO3     =  26
      FACD     =  27
      C2O3     =  28
      PAN      =  29
      PACD     =  30
      AACD     =  31
      CXO3     =  32
      PANX     =  33
      ROR      =  34
      OLE      =  35
      ETH      =  36
      IOLE     =  37
      TOL      =  38
      CRES     =  39
      TO2      =  40
      TOLRO2   =  41
      OPEN     =  42
      MGLY     =  43
      CRO      =  44
      CAT1     =  45
      CRON     =  46
      CRNO     =  47
      CRN2     =  48
      CRPX     =  49
      OPO3     =  50
      CAO2     =  51
      OPAN     =  52
      XYL      =  53
      XYLRO2   =  54
      ISOP     =  55
      ISPD     =  56
      ISOPRXN  =  57
      TERP     =  58
      TRPRXN   =  59
      SO2      =  60
      SULF     =  61
      SULRXN   =  62
      ETOH     =  63
      ETHA     =  64
      CL2      =  65
      CL       =  66
      HOCL     =  67
      CLO      =  68
      FMCL     =  69
      HCL      =  70
      TOLNRXN  =  71
      TOLHRXN  =  72
      XYLNRXN  =  73
      XYLHRXN  =  74
      BENZENE  =  75
      BENZRO2  =  76
      BNZNRXN  =  77
      BNZHRXN  =  78
      SESQ     =  79
      SESQRXN  =  80

c..Allocate and define ebi species
      N_EBISP  =  67
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
      EBISP(  58 ) = TOLNRXN
      EBISP(  59 ) = TOLHRXN
      EBISP(  60 ) = XYLNRXN
      EBISP(  61 ) = XYLHRXN
      EBISP(  62 ) = BENZENE
      EBISP(  63 ) = BENZRO2
      EBISP(  64 ) = BNZNRXN
      EBISP(  65 ) = BNZHRXN
      EBISP(  66 ) = SESQ
      EBISP(  67 ) = SESQRXN


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set species tolerances
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      RTOL( NO2     ) = 1.0E-03
      RTOL( NO      ) = 1.0E-03
      RTOL( O       ) = 1.0E+00
      RTOL( O3      ) = 1.0E-03
      RTOL( NO3     ) = 1.0E-03
      RTOL( O1D     ) = 1.0E+00
      RTOL( OH      ) = 1.0E-03
      RTOL( HO2     ) = 1.0E-03
      RTOL( N2O5    ) = 1.0E-03
      RTOL( HNO3    ) = 1.0E-03
      RTOL( HONO    ) = 1.0E-03
      RTOL( PNA     ) = 1.0E-03
      RTOL( H2O2    ) = 1.0E-03
      RTOL( XO2     ) = 1.0E-03
      RTOL( XO2N    ) = 1.0E-03
      RTOL( NTR     ) = 1.0E+00
      RTOL( ROOH    ) = 1.0E-03
      RTOL( FORM    ) = 1.0E-03
      RTOL( ALD2    ) = 1.0E-03
      RTOL( ALDX    ) = 1.0E-03
      RTOL( PAR     ) = 1.0E-03
      RTOL( CO      ) = 1.0E-03
      RTOL( MEO2    ) = 1.0E-03
      RTOL( MEPX    ) = 1.0E-03
      RTOL( MEOH    ) = 1.0E-03
      RTOL( HCO3    ) = 1.0E+00
      RTOL( FACD    ) = 1.0E-03
      RTOL( C2O3    ) = 1.0E-03
      RTOL( PAN     ) = 1.0E-03
      RTOL( PACD    ) = 1.0E-03
      RTOL( AACD    ) = 1.0E-03
      RTOL( CXO3    ) = 1.0E-03
      RTOL( PANX    ) = 1.0E-03
      RTOL( ROR     ) = 1.0E-03
      RTOL( OLE     ) = 1.0E-03
      RTOL( ETH     ) = 1.0E-03
      RTOL( IOLE    ) = 1.0E-03
      RTOL( TOL     ) = 1.0E-03
      RTOL( CRES    ) = 1.0E-03
      RTOL( TO2     ) = 1.0E-03
      RTOL( TOLRO2  ) = 1.0E-03
      RTOL( OPEN    ) = 1.0E-03
      RTOL( MGLY    ) = 1.0E-03
      RTOL( CRO     ) = 1.0E-03
      RTOL( CAT1    ) = 1.0E-03
      RTOL( CRON    ) = 1.0E-03
      RTOL( CRNO    ) = 1.0E-03
      RTOL( CRN2    ) = 1.0E-03
      RTOL( CRPX    ) = 1.0E-03
      RTOL( OPO3    ) = 1.0E-03
      RTOL( CAO2    ) = 1.0E-03
      RTOL( OPAN    ) = 1.0E-03
      RTOL( XYL     ) = 1.0E-03
      RTOL( XYLRO2  ) = 1.0E-03
      RTOL( ISOP    ) = 1.0E-03
      RTOL( ISPD    ) = 1.0E-03
      RTOL( ISOPRXN ) = 1.0E+00
      RTOL( TERP    ) = 1.0E-03
      RTOL( TRPRXN  ) = 1.0E+00
      RTOL( SO2     ) = 1.0E-03
      RTOL( SULF    ) = 1.0E+00
      RTOL( SULRXN  ) = 1.0E+00
      RTOL( ETOH    ) = 1.0E-03
      RTOL( ETHA    ) = 1.0E-03
      RTOL( CL2     ) = 1.0E-03
      RTOL( CL      ) = 1.0E-03
      RTOL( HOCL    ) = 1.0E-03
      RTOL( CLO     ) = 1.0E-03
      RTOL( FMCL    ) = 1.0E-03
      RTOL( HCL     ) = 1.0E-03
      RTOL( TOLNRXN ) = 1.0E+00
      RTOL( TOLHRXN ) = 1.0E+00
      RTOL( XYLNRXN ) = 1.0E+00
      RTOL( XYLHRXN ) = 1.0E+00
      RTOL( BENZENE ) = 1.0E-03
      RTOL( BENZRO2 ) = 1.0E-03
      RTOL( BNZNRXN ) = 1.0E+00
      RTOL( BNZHRXN ) = 1.0E+00
      RTOL( SESQ    ) = 1.0E-03
      RTOL( SESQRXN ) = 1.0E+00

      RETURN
      END
