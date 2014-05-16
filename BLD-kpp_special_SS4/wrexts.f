
C***************************************************************************
C  Significant portions of Models-3/CMAQ software were developed by        *
C  Government employees and under a United States Government contract.     *
C  Portions of the software were also based on information from non-       *
C  Federal sources, including software developed by research institutions  *
C  through jointly funded cooperative agreements. These research institu-  *
C  tions have given the Government permission to use, prepare derivative   *
C  works, and distribute copies of their work to the public within the     *
C  Models-3/CMAQ software release and to permit others to do so. EPA       *
C  therefore grants similar permissions for use of Models-3/CMAQ software, *
C  but users are requested to provide copies of derivative works to the    *
C  Government without re-strictions as to use by others.  Users are        *
C  responsible for acquiring their own copies of commercial software       *
C  associated with the Models-3/CMAQ release and are also responsible      *
C  to those vendors for complying with any of the vendors' copyright and   *
C  license restrictions. In particular users must obtain a Runtime license *
C  for Orbix from IONA Technologies for each CPU used in Models-3/CMAQ     *
C  applications.                                                           *
C                                                                          *
C  Portions of I/O API, PAVE, and the model builder are Copyrighted        *
C  1993-1997 by MCNC--North Carolina Supercomputing Center and are         *
C  used with their permissions subject to the above restrictions.          *
C***************************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/work/rep/MECH/src/driver/mech/WREXTS.f,v 1.6 2001/03/05 19:50:14 yoj Exp $ 

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)WREXTS.F	1.1 /project/mod3/MECH/src/driver/mech/SCCS/s.WREXTS.F 02 Jan 1997 15:26:56

      SUBROUTINE WREXTS (EQNAME_MECH,
     &                   DESCRP_MECH,
     &                   NS, SPCLIS, SPC1RX,
     &                   NR,
     &                   MXPRD,
     &                   IRR,
     &                   RTDAT,
     &                   SC,
     &                   NPRDCT,
     &                   NREACT,
     &                   KUNITS, 
     &                   KTYPE,
     &                   IRXBITS,
     &                   IORDER,
     &                   KTN1, KRX1, 
     &                   KTN2, KRX2, 
     &                   KTN3, KRX3,
     &                   KTN4, KRX4,
     &                   KTN5, KRX5,
     &                   KTN6, KRX6,
     &                   KTN7, KRX7,
!    &                   KCNV, KRXCNV,
     &                   NFALLOFF, 
     &                   IRRFALL,
     &                   RFDAT,
     &                   NWM,  NRXWM,
     &                   NWW,  NRXWW,
     &                   NWO2, NRXWO2,
     &                   NWN2, NRXWN2,
     &                   NWCH4, NRXWCH4,
     &                   NWH2, NRXWH2,
     &                   RXLABEL,
     &                   IP, 
     &                   IPH, 
     &                   NPHOTAB,
     &                   PHOTAB,
     &                   ISPECIAL, 
     &                   NSPECIAL_RXN, 
     &                   NSPECIAL,
     &                   SPECIAL,
     &                   NAMCONSTS,
     &                   CVAL,
     &                   N_SS_SPC,
     &                   SS_SPC,
     &                   SS1RX )
      
      IMPLICIT NONE

      INCLUDE 'PARMS.e'
      INCLUDE 'CHMECH.e'

C Argument variables

      CHARACTER( 120 ) :: EQNAME_MECH
      CHARACTER(  32 ) :: DESCRP_MECH

      INTEGER             NS          ! no. of species found in mechanism table
      CHARACTER(  16 ) :: SPCLIS( MAXSPEC ) ! species list from mechanism table

      INTEGER NR
      INTEGER MXPRD                   ! max no. products
!     INTEGER IRR( NR,MXPRD+3 ) caller uses different leading dimension
      INTEGER IRR( MAXRXNUM,MAXPRODS+3 )
!     REAL    SC( NR,MXPRD ) caller uses different leading dimension
      REAL    SC( MAXRXNUM,MAXPRODS )
!     INTEGER NPRDCT( NR )            ! no. of products for rx j
      INTEGER NPRDCT( MAXRXNUM )      ! no. of products for rx j
!     INTEGER NREACT( NR )            ! no. of reactants for rx j
      INTEGER NREACT( MAXRXNUM )      ! no. of reactants for rx j
      INTEGER SPC1RX( MAXSPEC )       ! rx index of 1st occurence of species
                                      ! in mechanism table
!     INTEGER KUNITS, 
!    &        KTYPE( NR ),
!    &        IRXBITS( NR ),
!    &        IORDER( NR ),
!    &        KTN1, KRX1( NR ), 
!    &        KTN2, KRX2( NR ), 
!    &        KTN3, KRX3( NR ),
!    &        KTN4, KRX4( NR ),
!    &        KTN5, KRX5( NR ),
!    &        KTN6, KRX6( NR ),
!    &        KTN7, KRX7( NR ),
!    &        KCNV, KRXCNV( NR )

!     INTEGER NFALLOFF, 
!    &        IRRFALL( NFALLOFF )
!     REAL( 8 ) ::  RFDAT( 5,NFALLOFF )

!     INTEGER NWM,
!    &        NRXWM( NWM )
!     INTEGER NWW,
!    &        NRXWW( NWW )
!     INTEGER NWO2,
!    &        NRXWO2( NWO2 )
!     INTEGER NWN2,
!    &        NRXWN2( NWN2 )

      CHARACTER( 16 ) :: RXLABEL( MAXRXNUM )     ! label for rx 

      INTEGER IP
!     INTEGER IPH( IP,3 ) caller uses different leading dimension
      INTEGER IPH( MAXPHOTRXNS,3 )

      INTEGER NPHOTAB                            ! no. of photolysis tables
      CHARACTER( 16 ) :: PHOTAB( MAXPHOTRXNS )   ! photolysis table label

      INTEGER NSPECIAL_RXN
      INTEGER ISPECIAL( MAXSPECRXNS,2 )
      INTEGER NSPECIAL
      CHARACTER( 16 ) :: SPECIAL( MAXSPECRXNS )

      CHARACTER( 16 ) :: NAMCONSTS( MAXCONSTS )
      REAL( 8 )       :: CVAL( MAXCONSTS )

      INTEGER         :: N_SS_SPC
      CHARACTER( 16 ) :: SS_SPC( MAXNLIST )
      INTEGER         :: SS1RX( MAXNLIST )


      INTEGER, EXTERNAL :: INDEX1

C Local Variables

      INTEGER ISPC, IRX, IFLD0, IFLD1, IFLD2

      INTEGER, EXTERNAL :: JUNIT
      EXTERNAL NAMEVAL
 
      CHARACTER( 120 ) :: EQNAME_SPCS
      CHARACTER( 120 ) :: EQNAME_RXDT
      CHARACTER( 120 ) :: EQNAME_RXCM

      CHARACTER(  47 ) :: EXHEAD_SPCS
      CHARACTER(  50 ) :: EXHEAD_RXDT
      CHARACTER(  52 ) :: EXHEAD_RXCM

      CHARACTER(  12 ) :: EXFLNM_SPCS = 'SPCSDATX'
      CHARACTER(  12 ) :: EXFLNM_RXDT = 'RXNSDATX'
      CHARACTER(  12 ) :: EXFLNM_RXCM = 'RXNSCOMX'

      INTEGER EXUNIT_SPCS
      INTEGER EXUNIT_RXDT
      INTEGER EXUNIT_RXCM

      LOGICAL         :: HAS_CONSTS = .FALSE.

      CHARACTER(  4 ) :: VARA4, VARB4
!     CHARACTER( 20 ) :: BUFF20( NS )
!     CHARACTER( 20 ) :: BUFF20( MAXSPEC )
      CHARACTER( 20 ) :: BUFF20( MAXRXNUM )
!     REAL( 8 ) ::  RBUFF( NR )
      REAL( 8 ) ::  DBUFF( MAXRXNUM )
      REAL          SBUFF( MAXRXNUM )

C----------------------------------------------------------------------

      EXUNIT_SPCS = JUNIT()
      EXUNIT_RXDT = JUNIT()
      EXUNIT_RXCM = JUNIT()
c symbolic link locates "EXFLNM_..."; setenv requires INQUIRE (NAMEVAL):
      CALL NAMEVAL ( EXFLNM_SPCS, EQNAME_SPCS )
      CALL NAMEVAL ( EXFLNM_RXDT, EQNAME_RXDT )
      CALL NAMEVAL ( EXFLNM_RXCM, EQNAME_RXCM )

      OPEN ( UNIT = EXUNIT_SPCS, FILE = EQNAME_SPCS, STATUS = 'UNKNOWN' )
      OPEN ( UNIT = EXUNIT_RXDT, FILE = EQNAME_RXDT, STATUS = 'UNKNOWN' )
      OPEN ( UNIT = EXUNIT_RXCM, FILE = EQNAME_RXCM, STATUS = 'UNKNOWN' )

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     Mechanism CTM Species intermediate INCLUDE File prologue
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
 
c                    12345678901234567890123456789012345678901234567
      EXHEAD_SPCS = 'Mechanism CTM Species intermediate INCLUDE File'
      WRITE( EXUNIT_SPCS, 1031 ) EXHEAD_SPCS
1031  FORMAT( 'C', 1X, 9('-'), 1X, A47, 1X, 10('-') )
      CALL WRHDR1 ( EXUNIT_SPCS, EQNAME_SPCS, 72 )
      WRITE( EXUNIT_SPCS, 1027 )
1027  FORMAT( 'C', 1X, 'Generated from ...' )
      CALL WRHDR1 ( EXUNIT_SPCS, EQNAME_MECH, 72 )
      WRITE( EXUNIT_SPCS, 1033 ) DESCRP_MECH
1033  FORMAT( 'C', 1X, 'for Mechanism Name:', 1X, A32 )
      WRITE( EXUNIT_SPCS, 1035 )
1035  FORMAT( /'C', 1X, 'The following are reserved symbols declared in this',
     &              1X, 'INCLUDE file:'
     &        /'C', 4X, 'NSPCS    = Number of mechanism species'
     &        /'C', 4X, 'SPCNAMES = Table of mechanism species names'
     &        /'C', 4X, 'SPC1RX   = rx index of 1st occurence of species',
     &              1X,            'in mechanism table' )

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     Mechanism Reactions DATA INCLUDE File prologue
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
 
C                    12345678901234567890123456789012345678901234567890
      EXHEAD_RXDT = 'Mechanism Reactions, Rates, etc. DATA INCLUDE File'
      WRITE( EXUNIT_RXDT, 1037 ) EXHEAD_RXDT
1037  FORMAT( 'C', 1X, 9('-'), 1X, A50, 1X,  9('-') )
      CALL WRHDR1 ( EXUNIT_RXDT, EQNAME_RXDT, 72 )
      WRITE( EXUNIT_RXDT, 1027 )
      CALL WRHDR1 ( EXUNIT_RXDT, EQNAME_MECH, 72 )
      WRITE( EXUNIT_RXDT, 1033 ) DESCRP_MECH
      WRITE( EXUNIT_RXDT, 1039 )
1039  FORMAT( /'C', 1X, 'This DATA INCLUDE file must be preceded by the',
     &              1X, 'corresponding'
     &        /'C', 1X, 'COMMON INCLUDE file, which contains all the',
     &              1X, 'appropriate type'
     &        /'C', 1X, 'declarations for this file.  The DATA INCLUDE file',
     &              1X, 'is meant'
     &        /'C', 1X, 'to load the COMMON INCLUDE file.' /
     &        /'C', 1X, 'The following are reserved symbols declared in this',
     &              1X, 'INCLUDE file:'
     &        /'C', 4X, 'IRXXN = Reactions index')

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     Mechanism Reactions COMMON BLOCK INCLUDE File prologue
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
 
      IF ( MAXVAL( CVAL ) .GT. 0.0D0 ) HAS_CONSTS = .TRUE.

C                    1234567890123456789012345678901234567890123456789012
      EXHEAD_RXCM = 'Mechanism Reactions, Rates, etc. COMMON INCLUDE File'
      WRITE( EXUNIT_RXCM, 1041 ) EXHEAD_RXCM
1041  FORMAT( 'C', 1X, 8('-'), 1X, A52, 1X,  8('-') )
      CALL WRHDR1 ( EXUNIT_RXCM, EQNAME_RXCM, 72 )
      WRITE( EXUNIT_RXCM, 1033 ) DESCRP_MECH
      WRITE( EXUNIT_RXCM, 1043 )
1043  FORMAT( /'C', 1X, 'The following are reserved symbols declared in this',
     &              1X, 'INCLUDE file:'
     &        /'C', 4X, 'MECHNAME       = Mechanism name'
     &        /'C', 4X, 'N_GAS_CHEM_SPC = Total number of species in gas phase chemical mechanism'
     &        /'C', 4X, 'N_ACT_SP       = Number of active (determined by ODE solver) species in mechanism'
     &        /'C', 4X, 'NRXNS          = Number of mechanism reactions'
     &        /'C', 4X, 'KUNITS         = Units of mechanism reactions'
     &        /'C', 4X, 'KTYPE          = Reaction type'
     &        /'C', 4X, 'IRXBITS        = Bit test mask vector for selected reactions'
     &        /'C', 4X, 'IORDER         = Order of the reaction'
     &        /'C', 4X, 'KTN1           = Number of type 1 reactions'
     &        /'C', 4X, 'KRX1           = Reactions list pointer to type 1 reactions'
     &        /'C', 4X, 'KTN2           = Number of type 2 reactions'
     &        /'C', 4X, 'KRX2           = Reactions list pointer to type 2 reactions'
     &        /'C', 4X, 'KTN3           = Number of type 3 reactions'
     &        /'C', 4X, 'KRX3           = Reactions list pointer to type 3 reactions'
     &        /'C', 4X, 'KTN4           = Number of type 4 reactions'
     &        /'C', 4X, 'KRX4           = Reactions list pointer to type 4 reactions'
     &        /'C', 4X, 'KTN5           = Number of type 5 reactions'
     &        /'C', 4X, 'KRX5           = Reactions list pointer to type 5 reactions'
     &        /'C', 4X, 'KTN6           = Number of type 6 reactions'
     &        /'C', 4X, 'KRX6           = Reactions list pointer to type 6 reactions'
     &        /'C', 4X, 'KTN7           = Number of type 7 reactions'
     &        /'C', 4X, 'KRX7           = Reactions list pointer to type 7 reactions' )
!    &        /'C', 4X, 'KCNV           = Number of reactions for possible PPM units',
!    &              1X, 'conversion',
!    &        /'C', 4X, 'KRXCNV         = Reactions list pointer to units conversion',
!    &              1X, 'reactions' )

      IF ( HAS_CONSTS ) THEN
         WRITE( EXUNIT_RXCM, 1045 )
1045     FORMAT( /'C', 4X, 'NWM       = Number of air 3-body reactions'
     &           /'C', 4X, 'NRXWM     = Reactions list pointer to air 3-body',
     &                 1X, 'reactions'
     &           /'C', 4X, 'ATM_AIR   = air 3-body reactions concentration'
     &           /'C', 4X, 'NWW       = Number of H2O 3-body reactions'
     &           /'C', 4X, 'NRXWW     = Reactions list pointer to H2O 3-body',
     &                 1X, 'reactions'
     &           /'C', 4X, 'NWO2      = Number of reactions with O2'
     &           /'C', 4X, 'NRXWO2    = Reactions list pointer to O2 reactions'
     &           /'C', 4X, 'ATM_O2    = Oxygen reactions concentration'
     &           /'C', 4X, 'NWN2      = Number of N2 3-body reactions'
     &           /'C', 4X, 'NRXWN2    = Reactions list pointer to N2 3-body',
     &                 1X, 'reactions'
     &           /'C', 4X, 'ATM_N2    = Nitrogen 3-body reactions concentration'
     &           /'C', 4X, 'NWCH4     = Number of reactions with CH4'
     &           /'C', 4X, 'NRXWCH4   = Reactions list pointer to CH4 reactions'
     &           /'C', 4X, 'ATM_CH4   = Methane reactions concentration'
     &           /'C', 4X, 'NWH2      = Number of reactions with H2'
     &           /'C', 4X, 'NRXWH2    = Reactions list pointer to H2 reactions'
     &           /'C', 4X, 'ATM_H2    = Hydrogen reactions concentration' )

      ELSE

         WRITE( EXUNIT_RXCM, 1047 )
1047     FORMAT( /'C', 4X, 'NWM       = Number of air 3-body reactions'
     &           /'C', 4X, 'NRXWM     = Reactions list pointer to air 3-body',
     &                 1X, 'reactions'
     &           /'C', 4X, 'NWW       = Number of H2O 3-body reactions'
     &           /'C', 4X, 'NRXWW     = Reactions list pointer to H2O 3-body',
     &                 1X, 'reactions'
     &           /'C', 4X, 'NWO2      = Number of reactions with O2'
     &           /'C', 4X, 'NRXWO2    = Reactions list pointer to O2 reactions'
     &           /'C', 4X, 'NWN2      = Number of N2 3-body reactions'
     &           /'C', 4X, 'NRXWN2    = Reactions list pointer to N2 3-body',
     &                 1X, 'reactions'
     &           /'C', 4X, 'NWCH4     = Number of reactions with CH4'
     &           /'C', 4X, 'NRXWCH4   = Reactions list pointer to CH4 reactions'
     &           /'C', 4X, 'NWH2      = Number of reactions with H2'
     &           /'C', 4X, 'NRXWH2    = Reactions list pointer to H2 reactions' )

      END IF

      WRITE( EXUNIT_RXCM, 1049 )
1049  FORMAT( /'C', 4X, 'MXPRD     = Maximum number of mechanism reaction',
     &              1X, 'products'
     &        /'C', 4X, 'IRR       = Reactions list pointer to reactants',
     &              1X, 'and products'
     &        /'C', 4X, 'RTDAT     = Kinetic reaction rates expressions',
     &              1X, 'components'
     &        /'C', 4X, 'NFALLOFFF = Number of falloff reactions'
     &        /'C', 4X, 'IRRFALL   = Reactions list pointer to falloff reactions'
     &        /'C', 4X, 'RFDAT     = Falloff reaction rates expressions',
     &              1X, 'components'
     &        /'C', 4X, 'SC        = Stoichiometric coefficients'
     &        /'C', 4X, 'NREACT    = Number of reactants in each mechanism',
     &              1X, 'reaction'
     &        /'C', 4X, 'NPRDCT    = Number of products in each mechanism',
     &              1X, 'reaction'
     &        /'C', 4X, 'RXLABEL   = Character label list for mechanism',
     &              1X, 'reactions'
     &        /'C', 4X, 'NMPHOT    = Number of mechanism photolytic reactions'
     &        /'C', 4X, 'NPHOTAB   = Number of photolytic reactions tables', 
     &        /'C', 4X, 'IPH       = Reactions list pointer to photolytic',
     &              1X, 'reactions and tables' )


c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     NS
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      WRITE( EXUNIT_SPCS, 1053 ) NS + N_SS_SPC
1053  FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: NSPCS =', I4 )

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     CTMSPC and SPC1RX
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

!     WRITE( EXUNIT_SPCS, 1055 )
!055  FORMAT( /6X, 'INTEGER    ISPCS' )

      WRITE( EXUNIT_SPCS, 1057 )
!057  FORMAT( /6X, 'CHARACTER*16 CTMSPC(NSPCS )'
1057  FORMAT( /6X, 'CHARACTER( 16 ) :: SPCNAMES( NSPCS )'
     &        /6X, 'INTEGER         :: SPC1RX( NSPCS )' / )

      DO ISPC = 1, NS
         WRITE( EXUNIT_SPCS, 1059 ) ISPC, ISPC, SPCLIS( ISPC ), SPC1RX( ISPC )
!059     FORMAT( 6X, 'DATA', 1X, 'CTMSPC(', I3, '),', 1X, 'SPC1RX(', I3, ')',
1059     FORMAT( 6X, 'DATA', 1X, 'SPCNAMES(', I3, '),', 1X, 'SPC1RX(', I3, ')',
     &           2X, '/ ''', A16, ''',', I4, ' /' )
      END DO

      DO ISPC = 1, N_SS_SPC
         WRITE( EXUNIT_SPCS, 1059 ) ISPC + NS, ISPC + NS, SS_SPC( ISPC ), SS1RX( ISPC )
      END DO

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     MECHNAME and GAS_CHEM_SPC list
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      WRITE( EXUNIT_RXCM, 1061 ) DESCRP_MECH(1:LEN_TRIM(DESCRP_MECH))
1061  FORMAT( /6X, 'CHARACTER( 32 ), PARAMETER ::',
     &         1X, 'MECHNAME = ''', A, '''' )


      WRITE( EXUNIT_RXCM, 2053 ) NS + N_SS_SPC
2053  FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: N_GAS_CHEM_SPC =', I4 )

      WRITE( EXUNIT_RXDT, 2057 )
2057  FORMAT( /6X, 'CHARACTER( 16 ) :: GAS_CHEM_SPC( N_GAS_CHEM_SPC )')

      WRITE( EXUNIT_RXDT, 2052 )
2052  FORMAT(/, '! The below character array lists the species names used in the ',
     &       /, '! gas phase chemistry. The names and their order MUST agree with ',
     &       /, '! the GC_SPC array for the gas phase chemistry to work correctly. ')

      DO ISPC = 1, NS
         WRITE( EXUNIT_RXDT, 2059 ) ISPC, SPCLIS( ISPC )
2059     FORMAT( 6X, 'DATA', 1X, 'GAS_CHEM_SPC(', I3, ' ) / ''', A16, ''' /')
      END DO

      DO ISPC = 1, N_SS_SPC
         WRITE( EXUNIT_RXDT, 2059 ) ISPC + NS, SS_SPC( ISPC )
      END DO


c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     NR, KUNITS
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      WRITE( EXUNIT_RXCM, 1075 ) NS
1075  FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: N_ACT_SP =', I4 )

      WRITE( EXUNIT_RXCM, 1076 ) NR
1076  FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: NRXNS =', I4 )

      WRITE( EXUNIT_RXCM, 1077 )
1077  FORMAT( /6X, 'INTEGER', 12X, ':: KUNITS' )

      WRITE( EXUNIT_RXDT, 1078 ) KUNITS
1078  FORMAT( /6X, 'DATA  KUNITS /', I4, ' /' )

      WRITE( EXUNIT_RXDT, 1079 )
1079  FORMAT( /6X, 'INTEGER IRXXN' )

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     KTYPE
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
 
      WRITE( EXUNIT_RXCM, 1101 )
1101  FORMAT( /6X, 'INTEGER', 12X, ':: KTYPE( NRXNS )' )

      WRITE( EXUNIT_RXDT, 1103 )
1103  FORMAT( /6X, 'DATA ( KTYPE( IRXXN ), IRXXN = 1, NRXNS ) /' )

      CALL WRBF6 ( EXUNIT_RXDT, 10, NR, KTYPE )
   
c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     IRXBITS
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      WRITE( EXUNIT_RXCM, 1105 )
1105  FORMAT( /6X, 'INTEGER', 12X, ':: IRXBITS( NRXNS )' )

      WRITE( EXUNIT_RXDT, 1107 )
1107  FORMAT( /6X, 'DATA ( IRXBITS( IRXXN ), IRXXN = 1, NRXNS ) /' )

      CALL WRBF6 ( EXUNIT_RXDT, 10, NR, IRXBITS )
   
c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     IORDER
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      WRITE( EXUNIT_RXCM, 1109 )
1109  FORMAT( /6X, 'INTEGER', 12X, ':: IORDER( NRXNS )' )

      WRITE( EXUNIT_RXDT, 1111 )
1111  FORMAT( /6X, 'DATA ( IORDER( IRXXN ), IRXXN = 1, NRXNS ) /' )

      CALL WRBF6 (EXUNIT_RXDT, 10, NR, IORDER )
   
c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     KTN1,KRX1, KTN2,KRX2, KTN3,KRX3, KTN4,KRX4,
c     KTN5,KRX5, KTN6,KRX6, KTN7,KRX7 
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      VARA4 = 'KTN1'
      VARB4 = 'KRX1'
      WRITE( EXUNIT_RXCM, 1201 ) VARA4, KTN1
1201  FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: ', A4, ' =', I4 )
      IF ( KTN1 .NE. 0 ) THEN
         WRITE( EXUNIT_RXCM, 1203 ) VARB4, VARA4
1203     FORMAT(  6X, 'INTEGER', 12X, ':: ', A4, '( ', A4, ' )' )
         WRITE( EXUNIT_RXDT, 1205 ) VARB4, VARA4
1205     FORMAT( /6X, 'DATA ( ', A4, '( IRXXN ), IRXXN = 1, ', A4, ' ) /' )

         CALL WRBF6 ( EXUNIT_RXDT, 10, KTN1, KRX1 )
   
      ELSE
         WRITE( EXUNIT_RXCM, 1207 ) VARB4
1207     FORMAT(  6X, 'INTEGER', 12X, ':: ', A4, '( 1 )' )
         WRITE( EXUNIT_RXDT, 1209 ) VARB4
1209     FORMAT( /6X, 'DATA   ', A4, '( 1 )', ' / 0 /' )
      END IF

      VARA4 = 'KTN2'
      VARB4 = 'KRX2'
      WRITE( EXUNIT_RXCM, 1201 ) VARA4, KTN2
      IF ( KTN2 .NE. 0 ) THEN
         WRITE( EXUNIT_RXCM, 1203 ) VARB4, VARA4
         WRITE( EXUNIT_RXDT, 1205 ) VARB4, VARA4

         CALL WRBF6 ( EXUNIT_RXDT, 10, KTN2, KRX2 )
   
      ELSE
         WRITE( EXUNIT_RXCM, 1207 ) VARB4
         WRITE( EXUNIT_RXDT, 1209 ) VARB4
      END IF

      VARA4 = 'KTN3'
      VARB4 = 'KRX3'
      WRITE( EXUNIT_RXCM, 1201 ) VARA4, KTN3
      IF ( KTN3 .NE. 0 ) THEN
         WRITE( EXUNIT_RXCM, 1203 ) VARB4, VARA4
         WRITE( EXUNIT_RXDT, 1205 ) VARB4, VARA4

         CALL WRBF6 ( EXUNIT_RXDT, 10, KTN3, KRX3 )

      ELSE
         WRITE( EXUNIT_RXCM, 1207 ) VARB4
         WRITE( EXUNIT_RXDT, 1209 ) VARB4
      END IF

      VARA4 = 'KTN4'
      VARB4 = 'KRX4'
      WRITE( EXUNIT_RXCM, 1201 ) VARA4, KTN4
      IF ( KTN4 .NE. 0 ) THEN
         WRITE( EXUNIT_RXCM, 1203 ) VARB4, VARA4
         WRITE( EXUNIT_RXDT, 1205 ) VARB4, VARA4

         CALL WRBF6 ( EXUNIT_RXDT, 10, KTN4, KRX4 )

      ELSE
         WRITE( EXUNIT_RXCM, 1207 ) VARB4
         WRITE( EXUNIT_RXDT, 1209 ) VARB4
      END IF

      VARA4 = 'KTN5'
      VARB4 = 'KRX5'
      WRITE( EXUNIT_RXCM, 1201 ) VARA4, KTN5
      IF ( KTN5 .NE. 0 ) THEN
         WRITE( EXUNIT_RXCM, 1203 ) VARB4, VARA4
         WRITE( EXUNIT_RXDT, 1205 ) VARB4, VARA4

         CALL WRBF6 ( EXUNIT_RXDT, 10, KTN5, KRX5 )

      ELSE
         WRITE( EXUNIT_RXCM, 1207 ) VARB4
         WRITE( EXUNIT_RXDT, 1209 ) VARB4
      END IF

      VARA4 = 'KTN6'
      VARB4 = 'KRX6'
      WRITE( EXUNIT_RXCM, 1201 ) VARA4, KTN6
      IF ( KTN6 .NE. 0 ) THEN
         WRITE( EXUNIT_RXCM, 1203 ) VARB4, VARA4
         WRITE( EXUNIT_RXDT, 1205 ) VARB4, VARA4

         CALL WRBF6 ( EXUNIT_RXDT, 10, KTN6, KRX6 )
      ELSE
         WRITE( EXUNIT_RXCM, 1207 ) VARB4
         WRITE( EXUNIT_RXDT, 1209 ) VARB4
      END IF

      VARA4 = 'KTN7'
      VARB4 = 'KRX7'
      WRITE( EXUNIT_RXCM, 1201 ) VARA4, KTN7
      IF ( KTN7 .NE. 0 ) THEN
         WRITE( EXUNIT_RXCM, 1203 ) VARB4, VARA4
         WRITE( EXUNIT_RXDT, 1205 ) VARB4, VARA4

         CALL WRBF6 ( EXUNIT_RXDT, 10, KTN7, KRX7 )

      ELSE
         WRITE( EXUNIT_RXCM, 1207 ) VARB4
         WRITE( EXUNIT_RXDT, 1209 ) VARB4
      END IF



c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     KCNV, KRXCNV
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

!     WRITE( EXUNIT_RXCM, 1221 ) KCNV
!221  FORMAT( /6X, 'INTEGER, PARAMETER', 3X, ':: KCNV =', I4 )
!     WRITE( EXUNIT_RXCM, 1223 )
!223  FORMAT(  6X, 'INTEGER', 14X, ':: KRXCNV(KCNV)' )

!     WRITE( EXUNIT_RXDT, 1225 )
!225  FORMAT( /6X, 'DATA (KRXCNV(IRXXN), IRXXN = 1, KCNV) /' )

!     CALL WRBF6 ( EXUNIT_RXDT, 10, KCNV, KRXCNV )

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     NWM,NRXWM, NWW,NRXWW, NWO2,NRXWO2, NWN2,NRXWN,, NWCH4,NRXWCH4, NWH2,NRXWH2
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      WRITE( EXUNIT_RXCM, 1301 ) NWM
1301  FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: NWM =', I4  )
      IF ( NWM .NE. 0 ) THEN
         WRITE( EXUNIT_RXCM, 1303 )
1303     FORMAT(  6X, 'INTEGER', 12X, ':: NRXWM( NWM )' )
         WRITE( EXUNIT_RXDT, 1305 )
1305     FORMAT( /6X, 'DATA ( NRXWM( IRXXN ), IRXXN = 1, NWM ) /' )
         CALL WRBF6 ( EXUNIT_RXDT, 10, NWM, NRXWM )
      ELSE
         WRITE( EXUNIT_RXCM, 1307 )
1307     FORMAT(  6X, 'INTEGER', 12X, ':: NRXWM( 1 )' )
         WRITE( EXUNIT_RXDT, 1309 )
1309     FORMAT( /6X, 'DATA   NRXWM( 1 )', ' / 0 /' )
      END IF
      IF ( HAS_CONSTS ) THEN
         ISPC = INDEX1 ( 'ATM_AIR', MAXCONSTS, NAMCONSTS )
         WRITE( EXUNIT_RXCM, 1310 ) CVAL( ISPC )
1310     FORMAT(  6X, 'REAL,    PARAMETER ::', 1X, 'ATM_AIR =', 1PE12.5  )
      END IF

      WRITE( EXUNIT_RXCM, 1311) NWW
1311  FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: NWW =', I4 )
      IF ( NWW .NE. 0 ) THEN
         WRITE( EXUNIT_RXCM, 1313 )
1313     FORMAT(  6X, 'INTEGER', 12X, ':: NRXWW( NWW )' )
         WRITE( EXUNIT_RXDT, 1315 )
1315     FORMAT( /6X, 'DATA ( NRXWW( IRXXN ), IRXXN = 1, NWW ) /' )
         CALL WRBF6 ( EXUNIT_RXDT, 10, NWW, NRXWW )
      ELSE
         WRITE( EXUNIT_RXCM, 1317 )
1317     FORMAT(  6X, 'INTEGER', 12X, ':: NRXWW( 1 )' )
         WRITE( EXUNIT_RXDT, 1319 )
1319     FORMAT( /6X, 'DATA   NRXWW( 1 )', ' / 0 /' )
      END IF

      WRITE( EXUNIT_RXCM, 1321 ) NWO2
1321  FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: NWO2 =', I4 )
      IF ( NWO2 .NE. 0 ) THEN
         WRITE( EXUNIT_RXCM, 1323 )
1323     FORMAT(  6X, 'INTEGER', 12X, ':: NRXWO2( NWO2 )' )
         WRITE( EXUNIT_RXDT, 1325 )
1325     FORMAT( /6X, 'DATA ( NRXWO2( IRXXN ), IRXXN = 1, NWO2 ) /' )
         CALL WRBF6 ( EXUNIT_RXDT, 10, NWO2, NRXWO2 )
      ELSE
         WRITE( EXUNIT_RXCM, 1327 )
1327     FORMAT(  6X, 'INTEGER', 12X, ':: NRXWO2( 1 )' )
         WRITE( EXUNIT_RXDT, 1329 )
1329     FORMAT( /6X, 'DATA   NRXWO2( 1 )', ' / 0 /' )
      END IF
      IF ( HAS_CONSTS ) THEN
         ISPC = INDEX1 ( 'ATM_O2', MAXCONSTS, NAMCONSTS )
         WRITE( EXUNIT_RXCM, 1330 ) CVAL( ISPC )
1330     FORMAT(  6X, 'REAL,    PARAMETER ::', 1X, 'ATM_O2 =', 1PE12.5  )
      END IF

      WRITE( EXUNIT_RXCM, 1331 ) NWN2
1331  FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: NWN2 =', I4 )
      IF ( NWN2 .NE. 0 ) THEN
         WRITE( EXUNIT_RXCM, 1333 )
1333     FORMAT(  6X, 'INTEGER', 12X, ':: NRXWN2( NWN2 )' )
         WRITE( EXUNIT_RXDT, 1335 )
1335     FORMAT( /6X, 'DATA ( NRXWN2( IRXXN ), IRXXN = 1, NWN2 ) /' )
         CALL WRBF6 ( EXUNIT_RXDT, 10, NWN2, NRXWN2 )
      ELSE
         WRITE( EXUNIT_RXCM, 1337 )
1337     FORMAT(  6X, 'INTEGER', 12X, ':: NRXWN2( 1 )' )
         WRITE( EXUNIT_RXDT, 1339 )
1339     FORMAT( /6X, 'DATA   NRXWN2( 1 )', ' / 0 /' )
      END IF
      IF ( HAS_CONSTS ) THEN
         ISPC = INDEX1 ( 'ATM_N2', MAXCONSTS, NAMCONSTS )
         WRITE( EXUNIT_RXCM, 1340 ) CVAL( ISPC )
1340     FORMAT(  6X, 'REAL,    PARAMETER ::', 1X, 'ATM_N2 =', 1PE12.5  )
      END IF

      WRITE( EXUNIT_RXCM, 1341 ) NWCH4
1341  FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: NWCH4 =', I4 )
      IF ( NWCH4 .NE. 0 ) THEN
         WRITE( EXUNIT_RXCM, 1343 )
1343     FORMAT(  6X, 'INTEGER', 12X, ':: NRXWCH4( NWCH4 )' )
         WRITE( EXUNIT_RXDT, 1345 )
1345     FORMAT( /6X, 'DATA ( NRXWCH4( IRXXN ), IRXXN = 1, NWCH4 ) /' )
         CALL WRBF6 ( EXUNIT_RXDT, 10, NWCH4, NRXWCH4 )
      ELSE
         WRITE( EXUNIT_RXCM, 1347 )
1347     FORMAT(  6X, 'INTEGER', 12X, ':: NRXWCH4( 1 )' )
         WRITE( EXUNIT_RXDT, 1349 )
1349     FORMAT( /6X, 'DATA   NRXWCH4( 1 )', ' / 0 /' )
      END IF
      IF ( HAS_CONSTS ) THEN
         ISPC = INDEX1 ( 'ATM_CH4', MAXCONSTS, NAMCONSTS )
         WRITE( EXUNIT_RXCM, 1350 ) CVAL( ISPC )
1350     FORMAT(  6X, 'REAL,    PARAMETER ::', 1X, 'ATM_CH4 =', 1PE12.5  )
      END IF

      WRITE( EXUNIT_RXCM, 1351 ) NWH2
1351  FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: NWH2 =', I4 )
      IF ( NWH2 .NE. 0 ) THEN
         WRITE( EXUNIT_RXCM, 1353 )
1353     FORMAT(  6X, 'INTEGER', 12X, ':: NRXWH2( NWH2 )' )
         WRITE( EXUNIT_RXDT, 1355 )
1355     FORMAT( /6X, 'DATA ( NRXWH2( IRXXN ), IRXXN = 1, NWH2 ) /' )
         CALL WRBF6 ( EXUNIT_RXDT, 10, NWH2, NRXWH2 )
      ELSE
         WRITE( EXUNIT_RXCM, 1357 )
1357     FORMAT(  6X, 'INTEGER', 12X, ':: NRXWH2( 1 )' )
         WRITE( EXUNIT_RXDT, 1359 )
1359     FORMAT( /6X, 'DATA   NRXWH2( 1 )', ' / 0 /' )
      END IF
      IF ( HAS_CONSTS ) THEN
         ISPC = INDEX1 ( 'ATM_H2', MAXCONSTS, NAMCONSTS )
         WRITE( EXUNIT_RXCM, 1360 ) CVAL( ISPC )
1360     FORMAT(  6X, 'REAL,    PARAMETER ::', 1X, 'ATM_H2 =', 1PE12.5  )
      END IF

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     IRR
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      WRITE( EXUNIT_RXCM, 1401 ) MXPRD
1401  FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: MXPRD =', I4 )
      
      WRITE( EXUNIT_RXCM, 1403 )
1403  FORMAT(  6X, 'INTEGER', 12X, ':: IRR( NRXNS,MXPRD+3 )' )
      
      DO 701 ISPC = 1, MXPRD+3

      WRITE( EXUNIT_RXDT, 1405 ) ISPC
1405  FORMAT( /6X, 'DATA ( IRR( IRXXN,', I3, ' ), IRXXN = 1, NRXNS ) /' )

      CALL WRBF6 ( EXUNIT_RXDT, 10, NR, IRR( 1,ISPC ) )

701   CONTINUE

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     RTDAT
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      WRITE( EXUNIT_RXDT, 1413 ) '1'
1413  FORMAT( /6X, 'DATA ( RTDAT( ', A, ',IRXXN ), IRXXN = 1, NRXNS ) /' )

      DO IRX = 1, NR
         DBUFF( IRX ) = RTDAT( 1,IRX )
      END DO
      CALL WRBF12D ( EXUNIT_RXDT, 5, NR, DBUFF, 'D' )
   
      WRITE( EXUNIT_RXDT, 1413) '2'

      DO IRX = 1, NR
         DBUFF( IRX ) = RTDAT( 2,IRX )
      END DO
      CALL WRBF12D ( EXUNIT_RXDT, 5, NR, DBUFF, 'D' )

      WRITE( EXUNIT_RXDT, 1413 ) '3'

      DO IRX = 1, NR
         DBUFF( IRX ) = RTDAT( 3,IRX )
      END DO
      CALL WRBF12D ( EXUNIT_RXDT, 5, NR, DBUFF, 'D' )

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     NFALLOFF, IRRFALL
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      WRITE( EXUNIT_RXCM, 1501 ) NFALLOFF
1501  FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: NFALLOFF =', I4 )

      IF ( NFALLOFF .NE. 0 ) THEN
         WRITE( EXUNIT_RXCM, 1503 )
1503     FORMAT(  6X, 'INTEGER', 12X, ':: IRRFALL( NFALLOFF )' )
         WRITE( EXUNIT_RXDT, 1505 )
1505     FORMAT( /6X, 'DATA ( IRRFALL( IRXXN ), IRXXN = 1, NFALLOFF ) /' )

         CALL WRBF6 ( EXUNIT_RXDT, 10, NFALLOFF, IRRFALL )
 
      ELSE
         WRITE( EXUNIT_RXCM, 1507 )
1507     FORMAT(  6X, 'INTEGER', 12X, ':: IRRFALL( 1 )' )
      END IF

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     RFDAT
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      IF ( NFALLOFF .NE. 0 ) THEN

         WRITE( EXUNIT_RXDT, 1523 ) '1'
1523     FORMAT( /6X, 'DATA ( RFDAT( ', A, ',IRXXN ), IRXXN = 1, NFALLOFF ) /' )
         DO IRX = 1, NFALLOFF
            DBUFF( IRX ) = RFDAT( 1,IRX )
         END DO
         CALL WRBF12D ( EXUNIT_RXDT, 5, NFALLOFF, DBUFF, 'D' )

         WRITE( EXUNIT_RXDT, 1523 ) '2'
         DO IRX = 1, NFALLOFF
            DBUFF( IRX ) = RFDAT( 2,IRX )
         END DO
         CALL WRBF12D ( EXUNIT_RXDT, 5, NFALLOFF, DBUFF, 'D' )

         WRITE( EXUNIT_RXDT, 1523 ) '3'
         DO IRX = 1, NFALLOFF
            DBUFF( IRX ) = RFDAT( 3,IRX )
         END DO
         CALL WRBF12D ( EXUNIT_RXDT, 5, NFALLOFF, DBUFF, 'D' )

         WRITE( EXUNIT_RXDT, 1523 ) '4'
         DO IRX = 1, NFALLOFF
            DBUFF( IRX ) = RFDAT( 4,IRX )
         END DO
         CALL WRBF12D ( EXUNIT_RXDT, 5, NFALLOFF, DBUFF, 'D' )

         WRITE( EXUNIT_RXDT, 1523 ) '5'
         DO IRX = 1, NFALLOFF
            DBUFF( IRX ) = RFDAT( 5,IRX )
         END DO
         CALL WRBF12D ( EXUNIT_RXDT, 5, NFALLOFF, DBUFF, 'D' )

      END IF

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     SC
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      WRITE( EXUNIT_RXCM, 1551 )
1551  FORMAT( /6X, 'REAL', 15X, ':: SC( NRXNS,MXPRD )' )

      DO 801 ISPC = 1, MXPRD

      WRITE( EXUNIT_RXDT, 1553 ) ISPC
1553  FORMAT( /6X, 'DATA ( SC( IRXXN,', I3, ' ), IRXXN = 1, NRXNS ) /' )

      DO IRX = 1, NR
         SBUFF( IRX ) = SC( IRX,ISPC )
      END DO
      CALL WRBF12S ( EXUNIT_RXDT, 5, NR, SBUFF, 'F' )

801   CONTINUE

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     NREACT, NPRDCT
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      WRITE( EXUNIT_RXCM, 1601 )
1601  FORMAT( /6X, 'INTEGER', 12X, ':: NREACT( NRXNS )' )

      WRITE( EXUNIT_RXDT, 1603 )
1603  FORMAT( /6X, 'DATA ( NREACT( IRXXN ), IRXXN = 1, NRXNS ) /' )

      CALL WRBF6 ( EXUNIT_RXDT, 10, NR, NREACT )

      WRITE( EXUNIT_RXCM, 1605 )
1605  FORMAT(  6X, 'INTEGER', 12X, ':: NPRDCT( NRXNS )' )

      WRITE( EXUNIT_RXDT, 1607 )
1607  FORMAT( /6X, 'DATA ( NPRDCT( IRXXN ), IRXXN = 1, NRXNS ) /' )

      CALL WRBF6 ( EXUNIT_RXDT, 10, NR, NPRDCT )

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     IP, IPH
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      IF ( IP .NE. 0 ) THEN

         WRITE( EXUNIT_RXCM, 1701 ) IP
1701     FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: NMPHOT =', I4 )

         WRITE( EXUNIT_RXCM, 1703 )
1703     FORMAT(  6X, 'INTEGER', 12X, ':: IPH( NMPHOT,3 )' )

         WRITE( EXUNIT_RXDT, 1705 ) '1'
1705     FORMAT( /6X, 'DATA ( IPH( IRXXN,', A, ' ), IRXXN = 1, NMPHOT ) /' )

         CALL WRBF6 ( EXUNIT_RXDT, 10, IP, IPH( 1,1 ) )

         WRITE( EXUNIT_RXDT, 1705 ) '2'

         CALL WRBF6 ( EXUNIT_RXDT, 10, IP, IPH( 1,2 ) )

         WRITE( EXUNIT_RXDT, 1705 ) '3'

         CALL WRBF6 ( EXUNIT_RXDT, 10, IP, IPH( 1,3 ) )

      ELSE

         WRITE( EXUNIT_RXCM, 1707 )
1707     FORMAT( /'C Photolysis reactions information not available ...'
     &           /6X, 'INTEGER, PARAMETER', 1X, ':: NMPHOT = 0' )

         WRITE( EXUNIT_RXCM, 1709 )
1709     FORMAT( /6X, 'INTEGER', 12X, ':: IPH( 1,3 )' )

      END IF

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     1st Common Block
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      WRITE( EXUNIT_RXCM, 1901 )
1901  FORMAT( /5X, ' COMMON     / MECHRX1 /'
     &        /5X, '&             KUNITS,'
     &        /5X, '&             KTYPE,'
     &        /5X, '&             IRXBITS,'
     &        /5X, '&             IORDER,'
     &        /5X, '&             KRX1,'
     &        /5X, '&             KRX2,'
     &        /5X, '&             KRX3,'
     &        /5X, '&             KRX4,'
     &        /5X, '&             KRX7,'
     &        /5X, '&             KRX5,'
     &        /5X, '&             KRX6,'
!    &        /5X, '&             KRXCNV,'
     &        /5X, '&             NRXWM,'
     &        /5X, '&             NRXWW,'
     &        /5X, '&             NRXWO2,'
     &        /5X, '&             NRXWN2,'
     &        /5X, '&             NRXWCH4,'
     &        /5X, '&             NRXWH2,'
     &        /5X, '&             IRR,'
     &        /5X, '&             IRRFALL,'
     &        /5X, '&             SC,'
     &        /5X, '&             NREACT,'
     &        /5X, '&             NPRDCT,'
     &        /5X, '&             IPH' )

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     2nd Common Block (8-byte reals)
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      WRITE( EXUNIT_RXCM, 1411 )
1411  FORMAT( /6X, 'REAL( 8 )', 10X, ':: RTDAT( 3,NRXNS )' )

      IF ( NFALLOFF .NE. 0 ) THEN
         WRITE( EXUNIT_RXCM, 1521 )
1521     FORMAT(  6X, 'REAL( 8 )', 10X, ':: RFDAT( 5,NFALLOFF )' )
         WRITE( EXUNIT_RXCM, 1903 )
1903     FORMAT( /5X, ' COMMON     / MECHRX2 /'
     &           /5X, '&             RTDAT,'
     &           /5X, '&             RFDAT' )
      ELSE
         WRITE( EXUNIT_RXCM, 1525 )
1525     FORMAT(  6X, 'REAL( 8 )', 10X, ':: RFDAT( 1,1 )' )
         WRITE( EXUNIT_RXCM, 1905 )
1905     FORMAT( /5X, ' COMMON     / MECHRX2 /'
     &           /5X, '&             RTDAT' )
      END IF

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     3rd Common Block (character)
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c        PHOTAB
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      IF ( NPHOTAB .NE. 0 ) THEN

         WRITE( EXUNIT_RXCM, 1711 ) NPHOTAB
1711     FORMAT( /6X, 'INTEGER, PARAMETER', 1X, ':: NPHOTAB =', I4 )

         WRITE( EXUNIT_RXCM, 1713 )
1713     FORMAT(  6X, 'CHARACTER( 16 )', 4X, ':: PHOTAB( NPHOTAB )' )

         WRITE( EXUNIT_RXDT, 1715 )
1715     FORMAT( /6X, 'DATA ( PHOTAB( IRXXN ), IRXXN = 1, NPHOTAB ) /' )

         DO IRX = 1, NPHOTAB - 1
            WRITE( BUFF20( IRX ), '(1X, "''", A16, "''", ",") )' ) PHOTAB( IRX )
         END DO
         WRITE( BUFF20( NPHOTAB ), '(1X, "''", A16, "''", "/") )' ) PHOTAB( NPHOTAB )

         IFLD1 = 1
         DO IFLD0 = 1, NPHOTAB / 3
            IFLD2 = IFLD1 + 2
            WRITE( EXUNIT_RXDT, 1759 ) ( BUFF20( IRX ), IRX = IFLD1, IFLD2 )
1759        FORMAT(5X, '&', 2X, 4A20 )
            IFLD1 = IFLD2 + 1
         END DO
         IF ( IFLD1 .LE. NPHOTAB )
     &      WRITE( EXUNIT_RXDT, 1759 ) ( BUFF20( IRX ), IRX = IFLD1, NPHOTAB )

         WRITE( EXUNIT_RXCM, 1801 )
1801     FORMAT( /6X, 'CHARACTER( 16 )', 4X, ':: RXLABEL( NRXNS )' )

         WRITE( EXUNIT_RXCM, 1807 )
1807     FORMAT( /5X, ' COMMON     / MECHRX3 /'
     &           /5X, '&             PHOTAB,',
     &           /5X, '&             RXLABEL' )

      ELSE

         WRITE( EXUNIT_RXCM, 1717 )
1717     FORMAT( /'C Photolysis table information not available ...'
     &           /6X, 'INTEGER, PARAMETER', 1X, ':: NPHOTAB = 0' )
                                                
         WRITE( EXUNIT_RXCM, 1719 )
1719     FORMAT( /'C Photolysis table information not available ...'
     &           /6X, 'CHARACTER( 16 )', 4X, ':: PHOTAB( 1 )' )

         WRITE( EXUNIT_RXCM, 1801 )

         WRITE( EXUNIT_RXCM, 1809 )
1809     FORMAT( /5X, ' COMMON     / MECHRX3 /'
     &           /5X, '&             RXLABEL' )

      END IF
c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c        RXLABEL
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      WRITE( EXUNIT_RXDT, 1803 )
1803  FORMAT( /6X, 'DATA ( RXLABEL( IRXXN ), IRXXN = 1, NRXNS ) /' )

!     DO IRX = 1, NR - 1
!        WRITE( BUFF20( IRX ), '(1X, "''", A16, "''", ",") )' ) RXLABEL( IRX )
!     END DO
!     WRITE( BUFF20( NR ), '(1X, "''", A16, "''", "/") )' ) RXLABEL( NR )

!     IFLD1 = 1
!     DO IFLD0 = 1, NR / 3
!        IFLD2 = IFLD1 + 2
!        WRITE( EXUNIT_RXDT, 1059 ) ( BUFF20( IRX ), IRX = IFLD1, IFLD2 )
!        IFLD1 = IFLD2 + 1
!     END DO
!     IF ( IFLD1 .LE. NR )
!    &   WRITE( EXUNIT_RXDT, 1059 ) ( BUFF20( IRX ), IRX = IFLD1, NR )

      CALL WRBF16C ( EXUNIT_RXDT, 3, NR, RXLABEL )



c_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
c     Fini
c-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

      WRITE( EXUNIT_SPCS, 2013 ) EXHEAD_SPCS
2013  FORMAT( /'C', 1X, 'End of ', A47, 1X, 12('-') )
      WRITE( EXUNIT_RXDT, 2017 ) EXHEAD_RXDT
2017  FORMAT( /'C', 1X, 'End of ', A50, 1X, 12('-') )
      WRITE( EXUNIT_RXCM, 2020 ) EXHEAD_RXCM
2020  FORMAT( /'C', 1X, 'End of ', A52, 1X, 10('-') )

      CLOSE( EXUNIT_SPCS )
      CLOSE( EXUNIT_RXDT )
      CLOSE( EXUNIT_RXCM )
      RETURN
      END
