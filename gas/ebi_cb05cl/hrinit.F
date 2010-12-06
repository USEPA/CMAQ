
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
C $Header: /project/yoj/arc/CCTM/src/gas/ebi_cb05cl/hrinit.F,v 1.2 2010/12/06 14:44:39 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE HRINIT

C-----------------------------------------------------------------------
C  Function: To initialize species tolerances, arrays, and indices
 
C  Preconditions: For the CB05CL family of mechanisms
 
C  Return Values: None
 
C  Key Subroutines/Functions Called: None
 
C  Revision History: Created by EBI solver program, Jan. 31, 2008
C-----------------------------------------------------------------------

      USE CGRID_SPCS, ONLY: N_GC_SPC   ! CGRID mechanism species
      USE HRDATA

      IMPLICIT NONE

C.....Includes:
      INCLUDE SUBST_IOPARMS   ! I/O parameters definitions
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
      CRO      =  43
      MGLY     =  44
      XYL      =  45
      XYLRO2   =  46
      ISOP     =  47
      ISPD     =  48
      ISOPRXN  =  49
      TERP     =  50
      TRPRXN   =  51
      SO2      =  52
      SULF     =  53
      SULRXN   =  54
      ETOH     =  55
      ETHA     =  56
      CL2      =  57
      CL       =  58
      HOCL     =  59
      CLO      =  60
      FMCL     =  61
      HCL      =  62
      TOLNRXN  =  63
      TOLHRXN  =  64
      XYLNRXN  =  65
      XYLHRXN  =  66
      BENZENE  =  67
      BENZRO2  =  68
      BNZNRXN  =  69
      BNZHRXN  =  70
      SESQ     =  71
      SESQRXN  =  72

      N_EBISP  =  59
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
      EBISP(  30 ) = CRO
      EBISP(  31 ) = MGLY
      EBISP(  32 ) = XYL
      EBISP(  33 ) = XYLRO2
      EBISP(  34 ) = ISOP
      EBISP(  35 ) = ISPD
      EBISP(  36 ) = ISOPRXN
      EBISP(  37 ) = TERP
      EBISP(  38 ) = TRPRXN
      EBISP(  39 ) = SO2
      EBISP(  40 ) = SULF
      EBISP(  41 ) = SULRXN
      EBISP(  42 ) = ETOH
      EBISP(  43 ) = ETHA
      EBISP(  44 ) = CL2
      EBISP(  45 ) = CL
      EBISP(  46 ) = HOCL
      EBISP(  47 ) = CLO
      EBISP(  48 ) = FMCL
      EBISP(  49 ) = HCL
      EBISP(  50 ) = TOLNRXN
      EBISP(  51 ) = TOLHRXN
      EBISP(  52 ) = XYLNRXN
      EBISP(  53 ) = XYLHRXN
      EBISP(  54 ) = BENZENE
      EBISP(  55 ) = BENZRO2
      EBISP(  56 ) = BNZNRXN
      EBISP(  57 ) = BNZHRXN
      EBISP(  58 ) = SESQ
      EBISP(  59 ) = SESQRXN

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
      RTOL( CRO     ) = 1.0E-03
      RTOL( MGLY    ) = 1.0E-03
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
