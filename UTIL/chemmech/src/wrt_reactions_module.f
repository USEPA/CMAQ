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
C $Header$

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)CHEMMECH.F 1.1 /project/mod3/MECH/src/driver/mech/SCCS/s.CHEMMECH.F 02 Jan 1997 15:26:41

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE WRT_RATE_CONSTANT( NR, IP, NS, SPCLIS, LABEL  )


      USE MECHANISM_DATA
      USE GET_ENV_VARS
      USE KPP_DATA
      
      IMPLICIT NONE

      INTEGER,         INTENT( IN ) :: NR ! number of reactions
      INTEGER,         INTENT( IN ) :: IP ! number of photolysis reaction
      INTEGER,         INTENT( IN ) :: NS ! number of species
      CHARACTER( 16 ), INTENT( IN ) :: SPCLIS( : )
      CHARACTER( 16 ), INTENT( IN ) :: LABEL( :,: ) ! LABEL(NXX,1) 1st label found in rx NXX

c..local Variables for steady-state species

       
      CHARACTER(  1 ) :: CHR
      CHARACTER( 16 ) :: WORD
      CHARACTER( 50 ) :: PHRASE
      CHARACTER( 81 ) :: INBUF
      CHARACTER( 16 ) :: RXNS_MODULE_DATA = 'RXNS_MODULE_DATA'
      CHARACTER(  3 ) :: ENDD

      INTEGER, EXTERNAL :: INDEX1
      INTEGER, EXTERNAL :: INDEXES
      INTEGER            :: LPOINT, IEOL
      INTEGER            :: ICOL, ISPC, ISPCNEW, IRX, IDX
      INTEGER            :: NXX, IPR, IPHOTAB, NC
      INTEGER            :: DUMMY_COEF( MAXRXNUM )        ! Yields for the DUMMY variable in each reaction
      INTEGER            :: SS1RX( MAXNLIST )             ! First reaction occurrence for each SS species
      
c..Variables for species to be dropped from mechanism
      INTEGER         :: N_DROP_SPC = 0
      CHARACTER( 16 ) :: DROP_SPC( MAXNLIST )
      LOGICAL         :: LERROR
      LOGICAL         :: KPP_DUMMY   = .FALSE.
      LOGICAL         :: FIRST_TERM  = .TRUE.
      LOGICAL         :: EXISTING
      REAL( 8 )       :: WREXT_COEFFS( MAXSPECTERMS)
      INTEGER         :: WREXT_INDEX(  MAXSPECTERMS)

      INTEGER SPC1RX( MAXSPEC )              ! rx index of 1st occurence of species
                                             ! in mechanism table
      CHARACTER( 120 ) :: EQN_MECH_KPP
      CHARACTER( 120 ) :: SPC_MECH_KPP
      CHARACTER( 891 ) :: REACTION_STR(  MAXRXNUM )
      CHARACTER(  16 ) :: COEFF_STR
      CHARACTER(  16 ) :: NAMCONSTS( MAXCONSTS ) = (/
     &                    'ATM_AIR         ',
     &                    'ATM_O2          ',
     &                    'ATM_N2          ',
     &                    'ATM_H2          ',
     &                    'ATM_CH4         ' /)

      CHARACTER(  16 )    :: CLABEL                  ! mechanism constants label
      REAL( 8 )           :: CONSTVAL                ! retrieved constant
      REAL( 8 )            :: CVAL( MAXCONSTS )       ! mechanism constants value
      INTEGER, PARAMETER  :: LUNOUT = 6


      CHARACTER(  12 ) :: EXFLNM_SPCS = 'SPCSDATX'
      CHARACTER(  12 ) :: EXFLNM_RXDT = 'RXNSDATX'
      CHARACTER(  12 ) :: EXFLNM_RXCM = 'RXNSCOMX'
      CHARACTER( 140 ) :: FILE_LINE

      INTEGER, EXTERNAL :: JUNIT
      INTEGER            :: ICOUNT, IPRODUCT, ISP
      
      CHARACTER( 120 )   :: MSG, XMSG
      INTEGER            :: STATUS

      INTEGER, SAVE :: IFNEVER = 0     ! Flag for counter initialization
      INTEGER, SAVE :: NDLMAX  = 0     ! Max # of PD loss terms in any reaction
      INTEGER, SAVE :: NDPMAX  = 0     ! Max # of PD prod terms in any reaction

   
      INTEGER :: ICLO( NCS2 )        ! Pointer to # of ops in decomp loop 1
      INTEGER :: JCLO( NCS2 )        ! Pointer to # of ops in decomp loop 2
      INTEGER :: NSPECT( NCS )       ! Number of species in mechanism ncs

      INTEGER, ALLOCATABLE, SAVE :: ISAPORL( : )  ! Count of PD terms for each species

      INTEGER, ALLOCATABLE, SAVE :: ISPARDER( :,: )  ! Indicator of a PD term in the 
                                                     ! Jacobian matrix
      INTEGER, ALLOCATABLE, SAVE :: IZILCH  ( :,: )  ! # of nonzero calcs in decomp
                                                     ! loop 1
      INTEGER, ALLOCATABLE, SAVE :: JZILCH  ( :,: )  ! # of nonzero calcs in decomp
                                                     ! loop 2
      INTEGER, ALLOCATABLE, SAVE :: LZERO   ( :,: )  ! Symbolic Jacobian matrix

      INTEGER, ALLOCATABLE, SAVE :: IZEROI  ( : )  ! Pointer to decomp loop 1 i index
      INTEGER, ALLOCATABLE, SAVE :: IZEROK  ( : )  ! Pointer to decomp loop 1 k index
      INTEGER, ALLOCATABLE, SAVE :: JZERO   ( : )  ! Pointer to decomp loop 2 i index
      INTEGER IOS                  ! status
   

      INTEGER I,J,K,I1,J1,I2       ! Matrix loop indices
      INTEGER IA, IB               ! I,J index holders for decomp loop 2
      INTEGER INEW, JNEW           ! Index for sorted species number
      INTEGER IOLD, JOLD           ! Index for old species number
      INTEGER IPA, KPA             ! I,K index holders for decomp loop 1
      INTEGER IPB, KPB             ! I,K index holders for decomp loop 1
      INTEGER IPROD, JP            ! Species number of a product
      INTEGER IREACT, IR, JR       ! Species number of a reactant
      INTEGER ISP2                 ! Species loop indices
      INTEGER JRE, JPR, IRE        ! Indices for nonzero Jacobian entries 
      INTEGER JZ3, JZ4             ! Counter for calcs in backsub groupings
      INTEGER NP, IAP              ! Product loop indices
      INTEGER IAL, JAL             ! Reactant loop indices
      INTEGER IAR                  ! Pointer to location of PD term
      INTEGER IARRAY2              ! Final # of matrix entries w/ Sp. Mat
      INTEGER ICB                  ! Counter for # of terms in decomp loop 1
      INTEGER ICBSUM               ! Running count of calcs for j index 
                                   ! in decomp loop 1
      INTEGER ICCOUNT              ! Two term op count for decomp loop 1
      INTEGER ICNT                 ! Total op counter for decomp loop 1
      INTEGER ICNTA                ! op. counter for decomp loop 1 w/ Sp Mat 
      INTEGER ICNTB                ! op. counter for decomp loop 1 w/ Sp Mat
      INTEGER IFSUN                ! Day/night loop index
      INTEGER IJSTEP               ! Number of terms to calc in decomp loops
      INTEGER IMINNEW              ! Index holder for sort routine
      INTEGER IMINOLD              ! Index holder for sort routine
      INTEGER IPORR                ! Species number of a product or reactant
      INTEGER JCB                  ! Counter for # of terms in decomp loop 2
      INTEGER JCCOUNT              ! Two term op count for decomp loop 2
      INTEGER JCNT                 ! Total op counter for decomp loop 2 
      INTEGER JCNTA                ! op. counter for decomp loop 2 w/o Sp Mat
      INTEGER JCNTB                ! op. counter for decomp loop 2 w/ Sp Mat
      INTEGER JZ                   ! Loop index for backsub loops
      INTEGER KA                   ! Loop index for decomposition loops
      INTEGER KCNT                 ! op. counter for bksub loop 1 w/ Sp. Mat.
      INTEGER KCNTA                ! op. counter for bksub loop 1 w/o Sp Mat
      INTEGER KNTARRAY             ! Final # of matrix entries w/o Sp. Mat
      INTEGER KOUNT0               ! Initial # of matrix entries w/ Sp. Mat
      INTEGER KOUNT0A              ! Initial # of matrix entries w/o Sp. Mat
      INTEGER KZ                   ! # of nonzero calcs in backsub loop 1
      INTEGER NCSP                 ! Mechanism number NCS+1=day NCS+2=night
      INTEGER NK, NRT              ! Rate and Reactant number 
      INTEGER NLS                  ! Number of loss PD terms
      INTEGER NOCHANG              ! Count of number of species not reacting
      INTEGER NPR                  ! Number of prod PD terms
      INTEGER NQQ                  ! Loop index for Gear order      
      INTEGER NRPP                 ! Reactant plus product loop index
      INTEGER NRX                  ! Reaction loop index
      INTEGER NU                   ! Active reaction count holder
      INTEGER MCNT                 ! op. counter for bksub loop 2 w/ Sp. Mat.
      INTEGER MCNTA                ! op. counter for bksub loop 2 w/o Sp. Mat.
      INTEGER MINVALU              ! Current number of PD terms in sort
      INTEGER MXIARRAY              ! maximum # of components is sparse Jacobain vector
      INTEGER MZ                   ! # of nonzero calcs in backsub loop 2
      INTEGER SPECIAL_TERMS         ! Total # of terms in special rate
      INTEGER COUNT_TERMS           ! Active count of terms in a special rate
      INTEGER TEMPLATE_UNIT         ! IO unit # for mapping subroutine
      INTEGER IDIFF_ORDER           ! difference between order of two separate reactions

      CHARACTER(  32 ) :: MAPPING_ROUTINE = 'MAPPING_ROUTINE'
      CHARACTER( 256 ) :: EQNAME



      LOGICAL LITE               ! option to omitted specific write statements
      LOGICAL FALLOFF_RATE       ! whether a reaction is a falloff type
  
      INTERFACE 
       SUBROUTINE GETRCTNT ( IMECH, INBUF, IEOL, LPOINT, CHR, WORD,
     &                      NXX, NS, SPCLIS, SPC1RX,
     &                      ICOL, LABEL, N_DROP_SPC, DROP_SPC )
         INTEGER,         INTENT(   IN  ) :: IMECH
         CHARACTER( 81 ), INTENT( INOUT ) :: INBUF
         INTEGER,         INTENT( INOUT ) :: LPOINT
         INTEGER,         INTENT( INOUT ) :: IEOL
         CHARACTER(  1 ), INTENT( INOUT ) :: CHR
         CHARACTER( 16 ), INTENT( INOUT ) :: WORD
         INTEGER,         INTENT(   IN  ) :: NXX
         INTEGER,         INTENT( INOUT ) :: NS
         CHARACTER( 16 ), INTENT( INOUT ) :: SPCLIS( : )
         INTEGER,         INTENT( INOUT ) :: SPC1RX( : )
         INTEGER,         INTENT( INOUT ) :: ICOL
         CHARACTER( 16 ), INTENT(   IN  ) :: LABEL( :, : )
         INTEGER,         INTENT(   IN  ) :: N_DROP_SPC
         CHARACTER( 16 ), INTENT(   IN  ) :: DROP_SPC( : )
        END SUBROUTINE GETRCTNT
        SUBROUTINE GETPRDCT ( IMECH, INBUF, LPOINT, IEOL, CHR, WORD,
     &                      NXX, NS, SPCLIS, SPC1RX,
     &                      ICOL, N_DROP_SPC, DROP_SPC )
          INTEGER,         INTENT(   IN  ) :: IMECH
          CHARACTER( 81 ), INTENT( INOUT ) :: INBUF
          INTEGER,         INTENT( INOUT ) :: LPOINT
          INTEGER,         INTENT( INOUT ) :: IEOL
          CHARACTER(  1 ), INTENT( INOUT ) :: CHR
          CHARACTER( 16 ), INTENT( INOUT ) :: WORD
          INTEGER,         INTENT(   IN  ) :: NXX
          INTEGER,         INTENT( INOUT ) :: NS
          CHARACTER( 16 ), INTENT( INOUT ) :: SPCLIS( : )
          INTEGER,         INTENT( INOUT ) :: SPC1RX( : )
          INTEGER,         INTENT( INOUT ) :: ICOL
          INTEGER,         INTENT(   IN  ) :: N_DROP_SPC
          CHARACTER( 16 ), INTENT(   IN  ) :: DROP_SPC( : )
         END SUBROUTINE GETPRDCT
         SUBROUTINE GETRATE ( IMECH, INBUF, LPOINT, IEOL, CHR,
     &                         NXX, LABEL, IP )
           CHARACTER(  1 ), INTENT( INOUT ) :: CHR
           CHARACTER( 81 ), INTENT( INOUT ) :: INBUF
           INTEGER,         INTENT( IN )    :: IMECH
           INTEGER,         INTENT( INOUT ) :: LPOINT
           INTEGER,         INTENT( INOUT ) :: IEOL
           INTEGER,         INTENT( INOUT ) :: IP
           INTEGER,         INTENT( IN )    :: NXX
           CHARACTER( 16 ), INTENT( INOUT ) :: LABEL( :,: )
        END SUBROUTINE GETRATE
        SUBROUTINE WREXTS_FORTRAN90 (WRUNIT, EQNAME_MECH, DESCRP_MECH, NS, 
     &                      SPCLIS, SPC1RX, NR, IP,  NAMCONSTS, CVAL, SS1RX, LITE  ) 
          INTEGER,          INTENT( IN )  ::  WRUNIT     ! logical write unit no.
          CHARACTER( 120 ), INTENT ( IN ) :: EQNAME_MECH
          CHARACTER(  32 ), INTENT ( IN ) :: DESCRP_MECH
          INTEGER,          INTENT ( IN ) :: NS                ! no. of species found in mechanism table
          CHARACTER(  16 ), INTENT ( IN ) :: SPCLIS( : ) ! species list from mechanism table
          INTEGER,          INTENT ( IN ) :: NR
          INTEGER,          INTENT ( IN ) :: SPC1RX( : ) ! rx index of 1st occurence of species in mechanism table
          INTEGER,          INTENT ( IN ) :: IP
          CHARACTER( 16 ),  INTENT ( IN ) :: NAMCONSTS( : )
          REAL( 8 ),        INTENT ( IN ) :: CVAL( : )
          INTEGER,          INTENT ( IN ) :: SS1RX( : )
          LOGICAL,          INTENT ( IN ) :: LITE               ! option to omit specific write statements
        END SUBROUTINE WREXTS_FORTRAN90
        SUBROUTINE GET_SS_DATA ( LUNOUT, NR ) 
          INTEGER, INTENT ( IN )         :: LUNOUT   ! Output unit number
          INTEGER, INTENT ( IN )         :: NR       ! No. of reactions
        END SUBROUTINE GET_SS_DATA
        SUBROUTINE CHECK_SS_SPC ( LUNOUT, NS, SPCLIS, NR, LABEL, SS1RX )
         INTEGER, INTENT ( IN )         :: LUNOUT               ! Output unit number
         INTEGER, INTENT ( IN )         ::  NS                  ! No. of species in mechanism
         CHARACTER( 16 ), INTENT ( IN ) ::  SPCLIS( : )   ! List of mechanism species
         INTEGER, INTENT ( IN )         ::  NR                  ! No. of reactions
         CHARACTER( 16 ), INTENT ( IN ) ::  LABEL( :,: ) ! Reaction labels
         INTEGER, INTENT ( INOUT )      ::  SS1RX( : )
       END SUBROUTINE CHECK_SS_SPC
       SUBROUTINE WRSS_EXT_FORTRAN90( WRUNIT, NR ) 
         INTEGER, INTENT( IN )    ::  WRUNIT     ! logical write unit no.
         INTEGER, INTENT ( IN )   :: NR   ! No. of reactions
       END SUBROUTINE WRSS_EXT_FORTRAN90
       SUBROUTINE WRT_RATES( IOUNIT )
         INTEGER, INTENT( IN ) :: IOUNIT
       END SUBROUTINE WRT_RATES
      END INTERFACE 
  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Initialize module and local mechanism array variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      IF( .NOT. ALLOCATED( INDEX_FIXED_SPECIES ) )THEN
          ALLOCATE( INDEX_FIXED_SPECIES( MAXRXNUM, MAXRCTNTS ) )
      END IF
      
   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Find names for output module file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      OPEN ( UNIT = MODULE_UNIT, FILE = FNAME_DATA_MODULE, STATUS = 'UNKNOWN' )

      WRITE( MODULE_UNIT,'(7X,"MODULE RXNS_DATA", 3/ 7X, "IMPLICIT NONE" 3/ )')
       
      LITE = OMIT_RCONST_DATA
      
      CALL WREXTS_FORTRAN90 ( MODULE_UNIT, EQUATIONS_MECHFILE,
     &              MECHNAME,
     &              NS, SPCLIS, SPC1RX,
     &              NR,
     &              IP,
     &              NAMCONSTS,
     &              CONST, SS1RX, LITE ) 
      

      CALL WRSPECIAL_EXT_FORTRAN90( MODULE_UNIT )

      IF( N_SS_SPC .GT. 0 ) CALL GET_SS_DATA( LUNOUT, NR ) 


      CALL WRSS_EXT_FORTRAN90( MODULE_UNIT, NR ) 
C Error-check phot tables and report to log
!      WRITE( LUNOUT, * ) ' '
!      IPHOTAB = 0
!      NMPHOT  = IP
!      DO IPR = 1, IP
!         IF ( IPH( IPR,3 ) .NE. 0 ) THEN ! table
!            IPHOTAB = IPHOTAB + 1
!            IRX = IPH( IPR,1 )
!            NXX = IPH( IPR,2 )
!            WRITE( 6, 1009 ) IRX, PHOTAB( NXX ), RTDAT( 1,IRX ) 
!         END IF
!      END DO

C Error-check heteorogeneous tables and report to log
!      WRITE( LUNOUT, * ) ' '
!      IPHOTAB = 0
!      DO IPR = 1, MHETERO
!         IPHOTAB = IPHOTAB + 1
!         IRX = IHETERO(IPR,1)
!         IF( IRX .LT. 1 .OR. IRX .GT. NR )THEN
!            WRITE(6,'(A,I4,A,I4)')
!     &      '*** ERROR IHETERO(MHETERO,1) < 1 or > # of Reactions, i.e.,',NR,
!     &      ' IHETERO(MHETERO,1) = ', IRX
!            STOP
!         END IF
!         NXX = IHETERO(IPR,2)
!         IF( NXX .LT. 1 .OR. NXX .GT. NHETERO )THEN
!            WRITE(6,'(A,I4,A,I4)')
!     &      '*** ERROR IHETERO(MHETERO,2) < 1 or > NHETERO, i.e.,',NHETERO,
!     &      ' IHETERO(MHETERO,1) = ', NXX
!            STOP
!         END IF
!         WRITE( 6, 1109 ) IRX, HETERO( NXX ), RTDAT( 1,IRX ) 
!1109     FORMAT(  3X, 'Reaction', I4,
!     &            1X, 'uses heterogeneous rate table: ', A16,
!     &            1X, 'scaled by:', 1PG13.5 )
!
!      END DO
      
!      WRITE( 6, 1012 ) IPHOTAB, MHETERO
!1012  FORMAT(/ 5X, 'There are', I3,
!     &         1X, 'heterogeneous table references out of', I3,
!     &         1X, 'tables' / )


! set up variables equal to the rate constant of type 10 fall off reactions      
!      DO NXX = 1, NR
!         IF( KTYPE( NXX ) .EQ. 10 )THEN
!             WRITE(MODULE_UNIT, 4505)LABEL(NXX,1)
!         END IF
!      END DO       
! set up variables equal to the rate constant of type 11 reactions      
!      WRITE(MODULE_UNIT,4749)
      IF( NSPECIAL .GT. 0 )THEN
         WRITE(MODULE_UNIT,4750)
      ELSE
         WRITE(MODULE_UNIT,4751)
      END IF
! set up pointers and names for photolysis rate array
      WRITE(MODULE_UNIT,4502)
C Error-check phot tables and report to log
!      WRITE( LUNOUT, * ) ' '
!      IPHOTAB = 0
!      NMPHOT  = IP
!      DO IPR = 1, IP
!         IF ( IPH( IPR,3 ) .NE. 0 ) THEN ! table
!            IPHOTAB = IPHOTAB + 1
!            IRX = IPH( IPR,1 )
!            NXX = IPH( IPR,2 )
!            WRITE( 6, 1009 ) IRX, PHOTAB( NXX ), RTDAT( 1,IRX ) 
!         END IF
!      END DO
      DO IPR = 1, NPHOTAB
         WRITE(MODULE_UNIT,4503),PHOTAB(IPR),IPR
      END DO
!      DO IPR = 1, NPHOTAB
!         WRITE(MODULE_UNIT,4557)IPR, PHOTAB(IPR)
!      END DO
      IF( NHETERO .GT. 0 )THEN
 !         WRITE(MODULE_UNIT,5023)NHETERO
          DO IPR = 1, NHETERO
             WRITE(MODULE_UNIT,5024)HETERO(IPR),IPR
          END DO
!          DO IPR = 1, NHETERO
!             WRITE(MODULE_UNIT,5025)IPR, HETERO(IPR)
!          END DO
      ELSE 
!          WRITE(MODULE_UNIT,5026)NHETERO
      END IF

      WRITE( MODULE_UNIT,'(7X, "END MODULE RXNS_DATA")')
     
      CLOSE( MODULE_UNIT )

      OPEN ( UNIT = MODULE_UNIT, FILE = FNAME_FUNC_MODULE, STATUS = 'UNKNOWN' )

      WRITE( MODULE_UNIT,'(7X,"MODULE RXNS_FUNCTION", 3/ 7X, "IMPLICIT NONE" 3/ )')

      WRITE( MODULE_UNIT, 4611 )TRIM( MECHNAME )

      IF( NFUNCTIONS .GT. 0 )THEN
         WRITE( MODULE_UNIT, '(/ "! User Defined rate functions " )' )
         DO NXX = 1, NFUNCTIONS
            PHRASE = TRIM( FUNCTIONS( NXX ) )
            CALL CONVERT_CASE ( PHRASE, .TRUE. )
            WRITE( MODULE_UNIT, 4612 )PHRASE
         END DO 
      END IF
4612  FORMAT(9X,'REAL( 8 ) :: ', A )

      WRITE( MODULE_UNIT, 4510) 
      
       ISPC = INDEX(EQN_MECH_KPP,'/mech', BACK= .TRUE.) + 1
       NXX  = INDEX(EQN_MECH_KPP,'.eqn', BACK= .TRUE.)  - 1
       PHRASE = MECHNAME
       CALL CONVERT_CASE ( PHRASE, .FALSE. )
       WRITE(MODULE_UNIT,95050)
       DO ISPC = 1, NSPECIAL
          WRITE(MODULE_UNIT, 4506)SPECIAL(ISPC)
       END DO
       IF( KUNITS .EQ. 2 )THEN
         WRITE(MODULE_UNIT,95051)
       ELSE
         WRITE(MODULE_UNIT,95052)
       END IF
      
!      WRITE( 6, 1011 ) IPHOTAB, NPHOTAB

       DO NXX = 1, NSPECIAL
! count total number of terms in special rates
         SPECIAL_TERMS = 0
         DO IREACT = 1, MAXSPECTERMS
            IF( KC_COEFFS( NXX, IREACT ) .EQ. 0.0 )CYCLE
            IF( INDEX_KTERM( NXX, IREACT ) .GT. -1 )THEN
                SPECIAL_TERMS = SPECIAL_TERMS + 1
            END IF
         END DO         
         DO IREACT = 1, MAXSPECTERMS
            IF( OPERATORS( NXX, IREACT ) .GT. 0 )THEN
                SPECIAL_TERMS = SPECIAL_TERMS + 1
            END IF
         END DO         
         WRITE(MODULE_UNIT,'(11X, A16)', ADVANCE = 'NO' )SPECIAL( NXX )
         WRITE(MODULE_UNIT,'(A3)', ADVANCE = 'NO' )' = '
         FIRST_TERM = .TRUE.
! first write standard rate constants time concentrations
         COUNT_TERMS = 0
         DO IREACT = 1, MAXSPECTERMS
             IRX  = INDEX_KTERM( NXX, IREACT )
             IF( IRX .LT. 0 .OR. KC_COEFFS( NXX, IREACT ) .EQ. 0.0 )CYCLE
!              WRITE(6,'(11X, A16, 1X, ES12.4)' )SPECIAL( NXX ),KC_COEFFS( NXX, IREACT )
             COUNT_TERMS = COUNT_TERMS + 1 
             IF( FIRST_TERM )THEN
                PHRASE = ' '
                IF(KC_COEFFS( NXX, IREACT ) .LT. 0.0 )PHRASE = ' - '
!                FIRST_TERM = .FALSE.
             ELSE
                WRITE(MODULE_UNIT, 4711, ADVANCE = 'NO' )
                PHRASE = ' +  '
                IF(KC_COEFFS( NXX, IREACT ) .LT. 0.0 )PHRASE = '  -  '
             END IF
             IF( KC_COEFFS( NXX, IREACT ) .NE. 1.0 )THEN
                IF( IRX .GT. 0 )THEN
                    WRITE(MODULE_UNIT, 4708, ADVANCE = 'NO')TRIM(PHRASE),
     &              REAL( ABS( KC_COEFFS( NXX, IREACT ) ), 8), IRX
                ELSE
                    WRITE(MODULE_UNIT, 4718, ADVANCE = 'NO')TRIM(PHRASE),
     &              REAL( ABS( KC_COEFFS( NXX, IREACT ) ), 8)
                END IF
             ELSE
                IF( IRX .GT. 0 )THEN
                  IF( FIRST_TERM )THEN
                      FIRST_TERM = .FALSE.
                      WRITE(MODULE_UNIT, 4706, ADVANCE = 'NO')TRIM(PHRASE),IRX
                 ELSE
                      WRITE(MODULE_UNIT, 4706, ADVANCE = 'NO')TRIM(PHRASE) // ' ',IRX
                 END IF
                ELSE
                  IF( FIRST_TERM )THEN
                      FIRST_TERM = .FALSE.
                      WRITE(MODULE_UNIT, 4726, ADVANCE = 'NO')TRIM(PHRASE)
                 ELSE
                      WRITE(MODULE_UNIT, 4726, ADVANCE = 'NO')TRIM(PHRASE) // ' '
                 END IF
                END IF
             END IF
             ISPC = INDEX_CTERM( NXX, IREACT )
             IF( ISPC .LT. 1 )CYCLE
             IF( REORDER_SPECIES )THEN
                ISPCNEW = IOLD2NEW( ISPC )
             ELSE
                ISPCNEW = ISPC
             END IF
!              ISPC    = IRM2SP( IREACT, NXX )
!             WRITE(PHRASE,'(A,I4,A)')' * Y( NCELL, ', IOLD2NEW(ISPC,NCS) , ' ) '
!             IF( IRX .GT. 0 .AND. KC_COEFFS( NXX, IREACT ) .NE. 1.0 )THEN
             IF( IRX .GT. 0 )THEN
!                WRITE(PHRASE,'(A,I4,A)')'* Y( NCELL, IOLD2NEW( ', ISPC, ', NCS) ) '
                WRITE(PHRASE,'(A,A,A)')'* Y( NCELL, INDEX_',
     &          MECHANISM_SPC( ISPC )(1:MAXLEN_SPECIES), ' ) '
             ELSE
!                WRITE(PHRASE,'(A,I4,A)')'Y( NCELL, IOLD2NEW( ', ISPC, ', NCS) ) '
                IF( KUNITS .EQ. 2 )THEN
                   WRITE(PHRASE,'(A,A,A)')'CFACT * Y( NCELL, INDEX_',
     &             MECHANISM_SPC( ISPC )(1:MAXLEN_SPECIES), ' ) '
                ELSE
                   WRITE(PHRASE,'(A,A,A)')'Y( NCELL, INDEX_',
     &             MECHANISM_SPC( ISPC )(1:MAXLEN_SPECIES), ' ) '
                END IF
             END IF
             WRITE(MODULE_UNIT, 4709, ADVANCE = 'NO')TRIM( PHRASE )
             IF( IREACT .LT. MAXSPECTERMS )THEN
                 IF( COUNT_TERMS .LT. SPECIAL_TERMS )THEN
                     WRITE(MODULE_UNIT, 75006, ADVANCE = 'NO')
                 END IF
             END IF
         END DO
! next write defined operators         
         DO IREACT = 1, MAXSPECTERMS
            IDX = OPERATORS( NXX, IREACT )
            IF( IDX .LT. 1 )CYCLE
!              WRITE(6,'(11X, A16, 1X, ES12.4)' )SPECIAL( NXX ),OPERATOR_COEFFS( NXX, IREACT )
             COUNT_TERMS = COUNT_TERMS + 1 
             IF( FIRST_TERM )THEN
                PHRASE = ''
                IF(OPERATOR_COEFFS( NXX, IREACT ) .LT. 0.0 )PHRASE = '-'
!                FIRST_TERM = .FALSE.
             ELSE
                WRITE(MODULE_UNIT, 4711, ADVANCE = 'NO' )
                PHRASE = ' +  '
                IF(OPERATOR_COEFFS( NXX, IREACT ) .LT. 0.0 )PHRASE = ' -  '
             END IF
             IF( OPERATOR_COEFFS( NXX, IREACT ) .NE. 1.0 )THEN
                 WRITE(MODULE_UNIT, 4710, ADVANCE = 'NO')TRIM(PHRASE),
     &           REAL( ABS( OPERATOR_COEFFS( NXX, IREACT ) ), 8), TRIM( SPECIAL( IDX ) )
             ELSE
                IF( FIRST_TERM )THEN
                   FIRST_TERM = .FALSE.
                   WRITE(MODULE_UNIT, 4712, ADVANCE = 'NO')TRIM(PHRASE),TRIM( SPECIAL( IDX ) )
                ELSE
                   WRITE(MODULE_UNIT, 4712, ADVANCE = 'NO')TRIM(PHRASE) // ' ',TRIM( SPECIAL( IDX ) )
                END IF
             END IF
             IF( IREACT .LT. MAXSPECTERMS )THEN
                 IF( COUNT_TERMS .LT. SPECIAL_TERMS )THEN
                     WRITE(MODULE_UNIT, 75006, ADVANCE = 'NO')
                 END IF
             END IF
         END DO 
         WRITE(MODULE_UNIT, '(/)')
      END DO
75006 FORMAT(2X, "&")      
      WRITE(MODULE_UNIT,95701)
95701 FORMAT(/ '! define rate constants in terms of special rate operators ' /)
      DO NXX = 1, NSPECIAL_RXN
         IDX = ISPECIAL( NXX,1 )
         IF( RTDAT( 1, IDX ) .NE. 1.0 .AND. RTDAT( 1, IDX ) .GE. 0.0 )THEN
             IF( RTDAT( 3, IDX ) .EQ. 0.0 )THEN
                 WRITE(MODULE_UNIT, 95068 )IDX,REAL(RTDAT( 1, IDX ), 8),
     &           SPECIAL( ISPECIAL( NXX,2 ) ),TRIM( LABEL( IDX,1 ) )
             END IF
             IF( RTDAT( 3, IDX ) .LT. 0.0 )THEN
                 WRITE(MODULE_UNIT, 95067 )IDX,REAL(RTDAT( 1, IDX ), 8),REAL(ABS(RTDAT( 3, IDX )), 8),
     &           SPECIAL( ISPECIAL( NXX,2 ) ),TRIM( LABEL( IDX,1 ) )
             END IF
             IF( RTDAT( 3, IDX ) .GT. 0.0 )THEN
                 WRITE(MODULE_UNIT, 95077 )IDX,REAL(RTDAT( 1, IDX ), 8),REAL(RTDAT( 3, IDX ), 8),
     &           SPECIAL( ISPECIAL( NXX,2 ) ),TRIM( LABEL( IDX,1 ) )
             END IF
         END IF
         IF( RTDAT( 1, IDX ) .LT. 0.0 )THEN
             IF( RTDAT( 3, IDX ) .EQ. 0.0 )THEN
                 WRITE(MODULE_UNIT, 95088 )IDX,REAL(ABS(RTDAT( 1, IDX )), 8),
     &           SPECIAL( ISPECIAL( NXX,2 ) ),TRIM( LABEL( IDX,1 ) )
             END IF
             IF( RTDAT( 3, IDX ) .LT. 0.0 )THEN
                 WRITE(MODULE_UNIT, 95087 )IDX,REAL(ABS(RTDAT( 1, IDX )), 8),REAL(ABS(RTDAT( 3, IDX )), 8),
     &           SPECIAL( ISPECIAL( NXX,2 ) ),TRIM( LABEL( IDX,1 ) )
             END IF
             IF( RTDAT( 3, IDX ) .GT. 0.0 )THEN
                 WRITE(MODULE_UNIT, 95086 )IDX,REAL(ABS(RTDAT( 1, IDX )), 8),REAL(RTDAT( 3, IDX ), 8),
     &           SPECIAL( ISPECIAL( NXX,2 ) ),TRIM( LABEL( IDX,1 ) )
             END IF
         END IF
         IF( RTDAT( 1, IDX ) .EQ. 1.0 )THEN
            IF( RTDAT( 3, IDX ) .LT. 0.0 )THEN
                 WRITE(MODULE_UNIT, 95066 )IDX,
     &           REAL(ABS(RTDAT( 3, IDX )), 8),SPECIAL( ISPECIAL( NXX,2 ) ),TRIM( LABEL( IDX,1 ) )
            END IF
            IF( RTDAT( 3, IDX ) .GT. 0.0 )THEN
                 WRITE(MODULE_UNIT, 95076 )IDX,
     &           REAL(RTDAT( 3, IDX ), 8),SPECIAL( ISPECIAL( NXX,2 ) ),TRIM( LABEL( IDX,1 ) )
            END IF
            IF( RTDAT( 3, IDX ) .EQ. 0.0 )THEN         
!                 WRITE(*,*)IDX,SPECIAL( ISPECIAL( NXX,2 ) ) !,LABEL( IDX,1 )
                 WRITE(MODULE_UNIT,95070)IDX,SPECIAL( ISPECIAL( NXX,2 ) ),
     &           TRIM( LABEL( IDX,1 ) )
            END IF
         END IF
      END DO
      WRITE(MODULE_UNIT,95060)
      WRITE(MODULE_UNIT,4504)

! start writing the subroutine for rate constants 

!!!!   IF( HALOGEN_PARAMETER )THEN
          WRITE(MODULE_UNIT,99870)
!!!!   ELSE
!!!!      WRITE(MODULE_UNIT,99880)
!!!!   END IF
      
      IF( KUNITS .EQ. 2 )THEN
          WRITE(MODULE_UNIT,'(3A)')'! All rate constants converted from  molec/cm3 to ppm'
          WRITE(MODULE_UNIT,'(3A)')'! and 1/sec to 1/min'
      ELSE
          WRITE(MODULE_UNIT,'(3A)')'! Only fall off rate constants converted from  molec/cm3 '
          WRITE(MODULE_UNIT,'(3A)')'! and 1/sec to 1/min'
          WRITE(MODULE_UNIT,'(3A)')'! Remainder use ppm and 1/min '
      END IF

C Error-check phot tables and report to log
!      WRITE( LUNOUT, * ) ' '
!      IPHOTAB = 0
!      NMPHOT  = IP
!      DO IPR = 1, IP
!         IF ( IPH( IPR,3 ) .NE. 0 ) THEN ! table
!            IPHOTAB = IPHOTAB + 1
!            IRX = IPH( IPR,1 )
!            NXX = IPH( IPR,2 )
!            WRITE( 6, 1009 ) IRX, PHOTAB( NXX ), RTDAT( 1,IRX ) 
!1009        FORMAT(  3X, 'Reaction', I4,
!     &               1X, 'uses photolysis table: ', A16,
!     &               1X, 'scaled by:', 1PG13.5 )
!         END IF
!      END DO
      
!      WRITE( 6, 1011 ) IPHOTAB, NPHOTAB
!1011  FORMAT(/ 5X, 'There are', I3,
!     &         1X, 'photolysis table references out of', I3,
!     &         1X, 'tables' / )

! write IF block for photolysis rates

      
      IF( IP .GT. 0 )THEN
         WRITE(MODULE_UNIT,99879)
          DO IPR = 1, IP
             NXX = IPH( IPR,1 )
             IF( NXX .LE. 0 )CYCLE
             WRITE(MODULE_UNIT, 5117, ADVANCE= 'NO')LABEL(NXX,1), NXX
             IF ( IPH( IPR,3 ) .NE. 0 )THEN
                IDX = IPH( IPR, 2 )
                IF( RTDAT(1, NXX) .NE. 1.0 )THEN
                   WRITE(MODULE_UNIT,5000, ADVANCE = 'NO')REAL(RTDAT(1, NXX),8),TRIM( PHOTAB(IDX) )
                ELSE
                   WRITE(MODULE_UNIT,5001, ADVANCE = 'NO')TRIM( PHOTAB(IDX) )
                END IF
             ELSE IF( IPH( NXX,3 ) .EQ. 0 )THEN
                IDX = IPH(IPH( NXX,2 ), 2)
                IF( RTDAT(1, NXX) .NE. 1.0 )THEN
                   WRITE(MODULE_UNIT,5100, ADVANCE = 'NO')REAL(RTDAT(1, NXX),8), IDX
                ELSE
                   WRITE(MODULE_UNIT,5101, ADVANCE = 'NO')IDX
                END IF
             END IF
         END DO
	 IF( HALOGEN_PARAMETER )THEN
!	     WRITE(MODULE_UNIT,'(2/ 16X, A)')'IF( .NOT. PRESENT( LAND ) )CYCLE'
	     WRITE(MODULE_UNIT,'(2/ 16X, A)')'IF( .NOT. LAND( NCELL ) )THEN'
             DO NXX = 1, NR
	        IF( KTYPE( NXX ) .NE. 12 )CYCLE
	        WRITE(MODULE_UNIT, 5118, ADVANCE= 'NO') LABEL(NXX,1), NXX
                DO IDX = 1, NFALLOFF
                   IF( IRRFALL( IDX ) .EQ. NXX )EXIT
                END DO
                CALL WRITE_RATE_CONVERT_BEFORE(MODULE_UNIT, IORDER(NXX))
                WRITE( MODULE_UNIT, 5120 )RTDAT(1, NXX ),RFDAT(1, IDX),RTDAT(2, NXX ),RFDAT(2, IDX) ! ,PHOTAB(HAL_PHOTAB(NXX))
             END DO
	     WRITE(MODULE_UNIT,'(16X, A)')'END IF'
	 END IF
         WRITE(MODULE_UNIT,99881)
      END IF

5117  FORMAT(/    '!  Reaction Label ', A / 16X, 'RKI( NCELL, ', I4, ') = ')
5118  FORMAT(     '!  Reaction Label ', A / 19X, 'RKI( NCELL, ', I4, ') = ')
      WRITE(MODULE_UNIT,99882)
      IF( LINES_CAPTURED .GT. 0 )WRITE(MODULE_UNIT,99884)
      IF( ( KTN5 + KTN6 ) .GT. 0 )WRITE(MODULE_UNIT,99883)
    
      IF( LINES_CAPTURED .GT. 0 )THEN ! then write call to subroutine with user defined functions
         WRITE(MODULE_UNIT,'(/,"! call subroutine with user defined functions" )')
         WRITE(MODULE_UNIT,'(13X,"CALL RATE_FUNCTIONS( CAIR, TEMP, PRESS, H2O )", /)')
      END IF
! write loop for remaining rates
      DO NXX = 1, NR
!         WRITE(6,'(A,I4,3A,I4)')'Writing Reaction #',NXX,': ',TRIM(RXLABEL(NXX)),' out of Reaction:',NXX

         IF( KTYPE( NXX ) .NE. 11 .AND. KTYPE( NXX ) .NE. 0 )THEN
!          WRITE(MODULE_UNIT, 1498 )TRIM(LABEL(NXX,1))
!            CYCLE
!         ELSE
!            WRITE(MODULE_UNIT, 1501, ADVANCE= 'NO')LABEL(NXX,1), NXX
         END IF 
         
         SELECT CASE( KTYPE( NXX ) )
          CASE( -1 )
!             IF( KUNITS .EQ. 2 )CALL WRITE_RATE_CONVERT_TIME(MODULE_UNIT, IORDER(NXX))
             WRITE(MODULE_UNIT, 1501, ADVANCE= 'NO')LABEL(NXX,1), NXX
             DO IPR = 1, MHETERO
                IF ( IHETERO( IPR,1 ) .EQ. NXX )EXIT
             END DO
             IDX = IHETERO( IPR, 2 )
             IF( RTDAT(1, NXX) .NE. 1.0 )THEN
                 WRITE(MODULE_UNIT,5027, ADVANCE = 'NO')REAL(RTDAT(1, NXX),8),TRIM( HETERO(IDX) )
             ELSE
                 WRITE(MODULE_UNIT,5128, ADVANCE = 'NO')TRIM( HETERO(IDX) )
             END IF
!          CASE(  0 )
!             DO IPR = 1, IP
!                IF ( IPH( IPR,1 ) .EQ. NXX )EXIT
!             END DO
!             IF ( IPH( IPR,3 ) .NE. 0 )THEN
!                IDX = IPH( IPR, 2 )
!                IF( RTDAT(1, NXX) .NE. 1.0 )THEN
!                   WRITE(MODULE_UNIT,5000, ADVANCE = 'NO')REAL(RTDAT(1, NXX),8),TRIM( PHOTAB(IDX) )
!                ELSE
!                   WRITE(MODULE_UNIT,5001, ADVANCE = 'NO')TRIM( PHOTAB(IDX) )
!                END IF
!             ELSE IF( IPH( NXX,3 ) .EQ. 0 )THEN
!                IDX = IPH(IPH( NXX,2 ), 2)
!                IF( RTDAT(1, NXX) .NE. 1.0 )THEN
!                   WRITE(MODULE_UNIT,5100, ADVANCE = 'NO')REAL(RTDAT(1, NXX),8), IDX
!                ELSE
!                   WRITE(MODULE_UNIT,5101, ADVANCE = 'NO')IDX
!                END IF
!             END IF
          CASE( 1 )
             WRITE(MODULE_UNIT, 1501, ADVANCE= 'NO')LABEL(NXX,1), NXX
             WRITE(MODULE_UNIT,5111, ADVANCE = 'NO')REAL(RTDAT(1, NXX), 8)
             CALL WRITE_RATE_CONVERT_AFTER(MODULE_UNIT, IORDER(NXX))
          CASE( 2 )
             WRITE(MODULE_UNIT, 1501, ADVANCE= 'NO')LABEL(NXX,1), NXX
             IF( KUNITS .EQ. 2 )CALL WRITE_RATE_CONVERT_BEFORE(MODULE_UNIT, IORDER(NXX))
             WRITE(MODULE_UNIT,5129, ADVANCE = 'NO')RTDAT(1, NXX), RTDAT(2, NXX)
          CASE( 3 )
             WRITE(MODULE_UNIT, 1501, ADVANCE= 'NO')LABEL(NXX,1), NXX
             IF( KUNITS .EQ. 2 )CALL WRITE_RATE_CONVERT_BEFORE(MODULE_UNIT, IORDER(NXX))
             WRITE(MODULE_UNIT,5103, ADVANCE = 'NO')RTDAT(1, NXX), RTDAT(3, NXX)
          CASE( 4 )
             WRITE(MODULE_UNIT, 1501, ADVANCE= 'NO')LABEL(NXX,1), NXX
             IF( KUNITS .EQ. 2 )CALL WRITE_RATE_CONVERT_BEFORE(MODULE_UNIT, IORDER(NXX))
             WRITE(MODULE_UNIT,5104, ADVANCE = 'NO')RTDAT(1, NXX), RTDAT(3, NXX), RTDAT(2, NXX)
          CASE( 5 )
             IRX = INT( RTDAT( 3, NXX) )
             IF( IRX .GT. NXX )CYCLE
             WRITE(MODULE_UNIT, 1501, ADVANCE= 'NO')LABEL(NXX,1), NXX
             IDIFF_ORDER = IORDER(NXX) - IORDER(IRX)
             IF( IDIFF_ORDER .NE. 0 )THEN
                 FALLOFF_RATE = ( KTYPE(IRX) .GT. 7 .AND. KTYPE(IRX) .LT. 11 )
                 IF( KUNITS .EQ. 2 .OR. FALLOFF_RATE )THEN
                   CALL WRITE_RATE_CONVERT_BEFORE(MODULE_UNIT, IDIFF_ORDER )
                 END IF
             END IF
             WRITE(MODULE_UNIT,5115, ADVANCE = 'NO')IRX, 1.0D0/RTDAT( 1, NXX ), -RTDAT(2, NXX )
          CASE( 6 )
             IRX = INT( RTDAT( 2, NXX) )
             IF( IRX .GT. NXX )CYCLE
             WRITE(MODULE_UNIT, 1501, ADVANCE= 'NO')LABEL(NXX,1), NXX
             
             IDIFF_ORDER = IORDER(NXX) - IORDER(IRX)
             IF( IDIFF_ORDER .NE. 0 )THEN
                 FALLOFF_RATE = ( KTYPE(IRX) .GT. 7 .AND. KTYPE(IRX) .LT. 11 )
                 IF( KUNITS .EQ. 2 .OR. FALLOFF_RATE )THEN
                   CALL WRITE_RATE_CONVERT_BEFORE(MODULE_UNIT, IDIFF_ORDER )
                 END IF
             END IF
             IF( RTDAT( 1, NXX ) .NE. 1.0 )THEN
                 WRITE(MODULE_UNIT, 5006, ADVANCE = 'NO')REAL(RTDAT( 1, NXX ), 8), IRX
             ELSE
                 WRITE(MODULE_UNIT, 4706, ADVANCE = 'NO')' ', IRX
             END IF
          CASE( 7 )
             WRITE(MODULE_UNIT, 1501, ADVANCE= 'NO')LABEL(NXX,1), NXX
             IF( RTDAT(2, NXX) .NE. 0.0 )THEN
                 WRITE(MODULE_UNIT,5114, ADVANCE = 'NO')REAL(RTDAT(1, NXX), 8),REAL(RTDAT(2, NXX), 8)
             ELSE
                 WRITE(MODULE_UNIT,5007, ADVANCE = 'NO')REAL(RTDAT(1, NXX), 8)
             END IF
          CASE( 8 )
             WRITE(MODULE_UNIT, 1501, ADVANCE= 'NO')LABEL(NXX,1), NXX
             DO IDX = 1, NFALLOFF
                IF( IRRFALL( IDX ) .EQ. NXX )EXIT
             END DO
             CALL WRITE_RATE_CONVERT_BEFORE(MODULE_UNIT, IORDER(NXX))
             WRITE(MODULE_UNIT,5108, ADVANCE = 'NO')RTDAT(1,NXX),(1.0*RTDAT(2,NXX)),RTDAT(3,NXX),
     &      (1.0*RFDAT(1,IDX)),RFDAT(2,IDX),(1.0*RFDAT(3,IDX))
          CASE( 9 )
             WRITE(MODULE_UNIT, 1501, ADVANCE= 'NO')LABEL(NXX,1), NXX
             DO IDX = 1, NFALLOFF
                IF( IRRFALL( IDX ) .EQ. NXX )EXIT
             END DO
             CALL WRITE_RATE_CONVERT_BEFORE(MODULE_UNIT, IORDER(NXX))
             IF( RFDAT( 2, IDX ) .EQ. 0.0 .AND. RFDAT( 3, IDX ) .EQ. 0.0 )THEN
                 WRITE(MODULE_UNIT,5109, ADVANCE = 'NO')RTDAT(1,NXX),RTDAT(2,NXX),
     &           RTDAT(3,NXX),1.0*RFDAT(1,IDX)
             ELSE 
                 WRITE(MODULE_UNIT,5119, ADVANCE = 'NO')RTDAT(1,NXX),RFDAT(2, IDX),RTDAT(2,NXX),
     &           RTDAT(3,NXX),RFDAT(3, IDX),1.0*RFDAT(1,IDX),RFDAT(4, IDX),RFDAT(5, IDX)
              END IF 
          CASE( 10 )
             WRITE(MODULE_UNIT, 1501, ADVANCE= 'NO')LABEL(NXX,1), NXX
             DO IDX = 1, NFALLOFF
                IF( IRRFALL( IDX ) .EQ. NXX )EXIT
             END DO
             CALL WRITE_RATE_CONVERT_BEFORE(MODULE_UNIT, IORDER(NXX))
             WRITE(MODULE_UNIT, 5110, ADVANCE = 'NO')RTDAT(1,NXX),RTDAT(3,NXX),RTDAT(2,NXX),
     &      RFDAT(1,IDX),RFDAT(3,IDX),RFDAT(2,IDX),RFDAT(5,IDX),RFDAT(4,IDX)
          CASE( 11 )
	      DO IDX = 1, NSPECIAL_RXN
	         IF( ISPECIAL( IDX, 1) .EQ. NXX )EXIT
              END DO       
             IF( ORDER_SPECIAL( ISPECIAL( IDX, 2 ) ) .EQ. 0 )THEN
                  IDIFF_ORDER = IORDER(NXX) - 1
             ELSE IF( ORDER_SPECIAL( ISPECIAL( IDX, 2 ) ) .GT. 0 )THEN
                  IDIFF_ORDER = IORDER(NXX) - ORDER_SPECIAL( ISPECIAL( IDX, 2 ))
             END IF
             IF( IDIFF_ORDER .NE. 0 )THEN
                IF( KUNITS .EQ. 2 )THEN
                    WRITE(MODULE_UNIT,95069,ADVANCE = 'NO')ISPECIAL( IDX,1 )
                    CALL WRITE_RATE_CONVERT_BEFORE(MODULE_UNIT, IDIFF_ORDER )
                    WRITE(MODULE_UNIT,95071)ISPECIAL( IDX,1 )
                END IF
             END IF
             WRITE(MODULE_UNIT, 1498 )TRIM(LABEL(NXX,1))
!             DO IDX = 1, NSPECIAL_RXN
!                IF( ISPECIAL( IDX, 1 ) .EQ. NXX )EXIT
!             END DO
!             I   = ISPECIAL( IDX, 1)
!             IRX = ISPECIAL( IDX, 2)
!             IF( RTDAT( 1, I) .NE. 1.0 )THEN
!                WRITE(MODULE_UNIT,5011, ADVANCE = 'NO')REAL(RTDAT( 1, I),8), TRIM( SPECIAL( IRX ) )
!             ELSE
!                WRITE(MODULE_UNIT,5012, ADVANCE = 'NO')TRIM( SPECIAL( IRX ) )
!             END IF
          CASE( 13 )
             WRITE(MODULE_UNIT, 1501, ADVANCE= 'NO')LABEL(NXX,1), NXX
             DO IDX = 1, NRATE_STRING
                IF( KSTRING( IDX ) .EQ. NXX )EXIT
             END DO
             CALL WRITE_RATE_CONVERT_BEFORE(MODULE_UNIT, IORDER(NXX))
             WRITE(MODULE_UNIT,'(A)')TRIM( RATE_STRING( IDX ) )
          END SELECT
!          WRITE( MODULE_UNIT,'(/)')
      END DO
      DO NXX = 1, NR
         IF( KTYPE( NXX ) .NE. 5 .OR. KTYPE( NXX ) .NE. 6 )CYCLE
         SELECT CASE( KTYPE( NXX ) )
          CASE( 5 )
             IRX = INT( RTDAT( 3, NXX) )
             IF( IRX .GT. NXX )CYCLE
             WRITE(MODULE_UNIT, 1501, ADVANCE= 'NO')LABEL(NXX,1), NXX
             IDIFF_ORDER = IORDER(NXX) - IORDER(IRX)
             IF( IDIFF_ORDER .NE. 0 )THEN
                 FALLOFF_RATE = ( KTYPE(IRX) .GT. 7 .AND. KTYPE(IRX) .LT. 11 )
                 IF( KUNITS .EQ. 2 .OR. FALLOFF_RATE )THEN
                   CALL WRITE_RATE_CONVERT_BEFORE(MODULE_UNIT, IDIFF_ORDER )
                 END IF
             END IF
             WRITE(MODULE_UNIT,5115, ADVANCE = 'NO')IRX, 1.0D0/RTDAT( 1, NXX ), -RTDAT(2, NXX )
          CASE( 6 )
             IRX = INT( RTDAT( 2, NXX) )
             IF( IRX .GT. NXX )CYCLE
             IDIFF_ORDER = IORDER(NXX) - IORDER(IRX)
             WRITE(MODULE_UNIT, 1501, ADVANCE= 'NO')LABEL(NXX,1), NXX
             IF( IDIFF_ORDER .NE. 0 )THEN
                 FALLOFF_RATE = ( KTYPE(IRX) .GT. 7 .AND. KTYPE(IRX) .LT. 11 )
                 IF( KUNITS .EQ. 2 .OR. FALLOFF_RATE )THEN
                   CALL WRITE_RATE_CONVERT_BEFORE(MODULE_UNIT, IDIFF_ORDER )
                 END IF
             END IF
             IF( RTDAT( 1, NXX ) .NE. 1.0 )THEN
                 WRITE(MODULE_UNIT, 5006, ADVANCE = 'NO')REAL(RTDAT( 1, NXX ), 8), IRX
             ELSE
                 WRITE(MODULE_UNIT, 4706, ADVANCE = 'NO')' ', IRX
             END IF
          END SELECT
      END DO

      WRITE(MODULE_UNIT,99991)

!...write reaction rates routine to module 
      CALL WRT_RATES( MODULE_UNIT )
      
      TEMPLATE_UNIT = JUNIT()
      CALL VALUE_NAME( MAPPING_ROUTINE, EQNAME )
    
      INQUIRE( FILE = TRIM( EQNAME ), EXIST = EXISTING )
      
      IF( .NOT. EXISTING )THEN
         WRITE(6,*)'ERROR: CANNOT LOCATE FILE: ' // TRIM(EQNAME)
         STOP
      END IF

      OPEN( UNIT = TEMPLATE_UNIT, FILE = TRIM( EQNAME ), STATUS = 'OLD', ERR = 40000)

      NC = 0
      DO 
        NC = NC + 1
        READ (TEMPLATE_UNIT,'(A)',END=39999)FILE_LINE
        WRITE( MODULE_UNIT,'(A)')TRIM( FILE_LINE )
        IF( NC .GT. 100000 )EXIT
      END DO
      
39999 IF( NC .LT. 3)THEN
         WRITE(6,*)'ERROR: ' // TRIM( EQNAME ) // ' is empty file. '
         WRITE(6,*)'Check run script for variable MAPPING_ROUTINE'
         STOP
      END IF
      CLOSE( TEMPLATE_UNIT )

       WRITE( MODULE_UNIT,2260)
2260   FORMAT(10X, 'SUBROUTINE RESET_SPECIES_POINTERS( IOLD2NEW )',
     &      2/13X,'USE RXNS_DATA',
     &       /13X,'IMPLICIT NONE',
     &       /13X,'INTEGER, INTENT( IN ) :: IOLD2NEW( :,: ) ', 2/ )
          DO ISPC = 1, NS + N_SS_SPC
              ISPCNEW = INEW2OLD( ISPC )
              WRITE( MODULE_UNIT, 2261 ) MECHANISM_SPC( ISPCNEW )(1:MAXLEN_SPECIES), 
     &        MECHANISM_SPC( ISPCNEW )(1:MAXLEN_SPECIES)
          END DO
2261   FORMAT( 13X, 'INDEX_', A, ' = IOLD2NEW( INDEX_', A, ', 1 )' ) 
       WRITE( MODULE_UNIT,2262 )
2262  FORMAT(10X,'END SUBROUTINE RESET_SPECIES_POINTERS')

      IF( LINES_CAPTURED .GT. 0 )THEN
          INQUIRE( FILE = TRIM( FUNCTIONS_CAPTURED ), EXIST = EXISTING )
          IF( .NOT. EXISTING )THEN
             WRITE(6,*)'ERROR: CANNOT LOCATE FILE: ' // TRIM(FUNCTIONS_CAPTURED)
             STOP
          END IF
          OPEN( UNIT = UNIT_FUNCTIONS, FILE = TRIM( FUNCTIONS_CAPTURED ), STATUS = 'OLD' )
          WRITE( MODULE_UNIT,'(9X,"SUBROUTINE RATE_FUNCTIONS( M, TEMP, PRESS, H2O )")')
          WRITE( MODULE_UNIT,'(11X,"USE RXNS_DATA" )')
          WRITE( MODULE_UNIT,'(11X,"IMPLICIT NONE", //)')
          WRITE( MODULE_UNIT,'(11X,"! Arguments:")')
          WRITE( MODULE_UNIT,'(11X,"REAL( 8 ), INTENT( IN ) :: M     ! air number density (wet) [molec/cm^3] ")')
          WRITE( MODULE_UNIT,'(11X,"REAL( 8 ), INTENT( IN ) :: TEMP  ! air temperature, K ")')
          WRITE( MODULE_UNIT,'(11X,"REAL( 8 ), INTENT( IN ) :: PRESS ! pressure [Atm]  ")')
          WRITE( MODULE_UNIT,'(11X,"REAL( 8 ), INTENT( IN ) :: H2O   ! concentration [molec/cm^3] ")')
          WRITE( MODULE_UNIT,'(/ 11X,"! Local:")')
          WRITE( MODULE_UNIT,'(11X,"REAL( 8 ), PARAMETER :: O2_FRACTION  = ATM_O2 / ATM_AIR")')
          WRITE( MODULE_UNIT,'(11X,"REAL( 8 ), PARAMETER :: N2_FRACTION  = ATM_N2 / ATM_AIR")')
          WRITE( MODULE_UNIT,'(11X,"REAL( 8 ), PARAMETER :: H2_FRACTION  = ATM_H2 / ATM_AIR")')
          WRITE( MODULE_UNIT,'(11X,"REAL( 8 ), PARAMETER :: CH4_FRACTION = ATM_CH4 / ATM_AIR")')
          WRITE( MODULE_UNIT,'(/ 11X,"REAL( 8 ) :: O2  ! concentration [molec/cm^3] ")')
          WRITE( MODULE_UNIT,'(11X,"REAL( 8 ) :: N2  ! concentration [molec/cm^3] ")')
          WRITE( MODULE_UNIT,'(11X,"REAL( 8 ) :: H2  ! concentration [molec/cm^3] ")')
          WRITE( MODULE_UNIT,'(11X,"REAL( 8 ) :: CH4 ! concentration [molec/cm^3] ")')
          WRITE( MODULE_UNIT,'(/ "! Calculate constant atmospheric species " )')
            WRITE( MODULE_UNIT, '(11X,"O2  = O2_FRACTION * M" )')
            WRITE( MODULE_UNIT, '(11X,"N2  = N2_FRACTION * M" )')
            WRITE( MODULE_UNIT, '(11X,"H2  = H2_FRACTION * M" )')
            WRITE( MODULE_UNIT, '(11X,"CH4 = CH4_FRACTION * M",/ )')
          DO NC = 1, LINES_CAPTURED
            READ (UNIT_FUNCTIONS,'(A)')FILE_LINE
            WRITE( MODULE_UNIT,'(11X,A)')TRIM( FILE_LINE )
          END DO
          WRITE( MODULE_UNIT,'(/9X,"END SUBROUTINE RATE_FUNCTIONS" )')
          CLOSE( UNIT_FUNCTIONS )
      END IF

      WRITE( MODULE_UNIT,'(7X,"END MODULE RXNS_FUNCTION")')
      CLOSE( MODULE_UNIT )
      RETURN

40000 WRITE(6,*)'Unable to open below file for cgrid mapping subroutine:'
      WRITE(6,*)TRIM( EQNAME )
      WRITE(6,*)'IO UNIT = ',TEMPLATE_UNIT
      STOP
      
1498  FORMAT(/ '! RKI for Reaction ', A,' set in SPECIAL_RATES Routine' )

1501  FORMAT(/    '!  Reaction Label ', A / 13X, 'RKI( NCELL, ', I4, ') = ')
1993  FORMAT( / 5X, '*** ERROR: Special label already used'
     &        / 5X, 'Processing for special label number:', I6 )
1994  FORMAT( / 5X, '*** ERROR: Equal sign expected after special label'
     &        / 5X, 'Last line read was:' / A81 )
2003  FORMAT( / 5X, '*** ERROR: Units must be either cm, CM, PPM, or ppm'
     &        / 5X, 'Last line read was:' / A81 )
2005  FORMAT( / 5X, '*** ERROR: End bracket, ], missing for units code'
     &        / 5X, 'Last line read was:' / A81 )
2007  FORMAT( / 5X, '*** ERROR: First word of reaction input must be REAC'
     &        / 5X, 'Last line read was:' / A81 )
2009  FORMAT( / 5X, '*** ERROR: Equal sign expected after REACTIONS'
     &        / 5X, 'Last line read was:' / A81 )
2011  FORMAT( / 5X, '*** ERROR: Maximum number of reactions exceeded'
     &        / 5X, 'Last line read was:' / A81 )
2013  FORMAT( / 5X, '*** ERROR: Equal sign expected after reactants'
     &        / 5X, 'Last line read was:' / A81 )
!013  FORMAT( / 5X, '*** ERROR: Rate constant data must begin with a # or %'
!    &        / 5X, 'Last line read was:' / A81 )
2015  FORMAT( / 5X, '*** ERROR: Reactions line must end with a ;'
     &        / 5X, 'Last line read was:' / A81 )
2017  FORMAT( / 5X, '*** ERROR: Linear dependency photolysis reaction label',
     &          1X, 'points to undefined reaction'
     &        / 5X, 'Processing for reaction number:', I6 )
2019  FORMAT( / 5X, '*** ERROR: Reaction label refers to undefined reaction type'
     &        / 5X, 'Processing for reaction number:', I6, 1X, A )
2021  FORMAT( / 5X, '*** ERROR: Label points to currently undefined reaction'
     &        / 5X, 'Processing for reaction number:', I6 )
2031  FORMAT( / 5X, '*** ERROR: Special Rate Coefficient ', A16,
     &              ' uses the unlisted reaction label ', A16 )
2032  FORMAT( / 5X, '*** ERROR: Special Rate Coefficient ', A16,
     &              ' incorrectly uses the reaction ', A16,'.',
     &              ' The reaction order is misinterpreted as 1st or 2nd')
2033  FORMAT( / 5X, '*** ERROR: Special Rate Coefficient ', A16,
     &              ' uses the unlisted species ', A16 )
2034  FORMAT( / 5X, '*** ERROR: Special Rate Coefficient ', A16,
     &              ' incorrectly uses the reaction ', A16,'.',
     &              ' The reaction order is not 2nd.')

3010  FORMAT( / 5X, '*** ERROR: The following steady-state species is also in the ',
     &              'ELIMINATE list' )
3011  FORMAT( 16X, A )

4001  FORMAT( / 5X, '*** ERROR: Number of Steady-state species exceeds max allowable;',
     &              ' increase MAXNLIST' )

4002  FORMAT( / 5X, '*** ERROR: Number of ELIMINATE species exceeds max allowable;',
     &              ' increase MAXNLIST' )
4505  FORMAT('REAL( 8 )  :: RKI_RXN_', A16,' ! rate constant for stated reaction label')        
4506  FORMAT( 7X, 'REAL( 8 )  :: ', A16)        

4500  FORMAT(/7X,'CONTAINS'
     &      2/ 7X,'REAL( 8 ) FUNCTION FALL_T10 ( A0,B0,C0,A1,B1,C1,CE,CF)'
     &      / 9X,'IMPLICIT NONE'
     &      / '! rate constant for CMAQ fall off reaction type 10'
     &      / '! Arguements:'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: A0'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: B0'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: C0'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: A1'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: B1'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: C1'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: CE'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: CF'
     &      / 9X,'! Local:'
     &      / 9X,'REAL( 8 ) K0'
     &      / 9X,'REAL( 8 ) K1'
     &      / 9X,'REAL( 8 ) KEND'
     &      / 9X,'K0 = A0 * CAIR * DEXP(B0*INV_TEMP)* TEMPOT300**C0'
     &      / 9X,'K1 = A1 * DEXP(B1*INV_TEMP) * TEMPOT300**C1'
     &      / 9X,'KEND = ( ( 1.0D0 + ( ( 1.0D0 / CE ) * DLOG10( K0 / K1 ) ) ** 2.0D0 ) )'
     &      / 9X,'KEND = 1.0D0 / KEND'
     &      / 9X,'FALL_T10 = ( K0 / ( 1.0D0 + K0/K1 ) ) * CF ** KEND'
     &      / 9X,'RETURN'
     &      / 7X,'END FUNCTION FALL_T10' 
     &      / 7X,'REAL( 8 ) FUNCTION POWE_T02( A0,B0 )'
     &      / 9X,'IMPLICIT NONE'
     &      / '! rate constant for CMAQ Arrhenius reaction type 2'
     &      / '! Arguements:'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: A0'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: B0'
     &      / 9X,'! Local: None'
     &      / 9X,'POWE_T02 =  A0 * TEMPOT300**B0'
     &      / 9X,'RETURN'
     &      / 7X,'END FUNCTION POWE_T02'
     &      / 7X,'REAL( 8 ) FUNCTION ARRE_T04( A0,B0,C0 )'
     &      / 9X,'IMPLICIT NONE'
     &      / '! rate constant for CMAQ Arrhenius reaction type 4'
     &      / '! Arguements:'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: A0'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: B0'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: C0'
     &      / 9X,'! Local:'
     &      / 9X,'INTRINSIC DEXP'
     &      / 9X,'ARRE_T04 =  A0 * DEXP( B0 * INV_TEMP ) * TEMPOT300**C0'
     &      / 9X,'RETURN'
     &      / 7X,'END FUNCTION ARRE_T04'
     &      / 7X,'REAL( 8 ) FUNCTION ARRE_T03( A0,B0 )'
     &      / '! rate constant for CMAQ Arrhenius reaction type 3'
     &      / 9X,'IMPLICIT NONE'
     &      / '! Arguements:'
     &      / 9X,'REAL( 8 ),     INTENT(IN) ::  A0'
     &      / 9X,'REAL( 8 ),     INTENT(IN) ::  B0'
     &      / 9X,'! Local:'
     &      / 9X,'INTRINSIC DEXP'
     &      / 9X,'ARRE_T03 =  A0 * DEXP( B0 * INV_TEMP )'
     &      / 9X,'RETURN'
     &      / 7X,'END FUNCTION ARRE_T03 '
     &      / 7X,'REAL( 8 ) FUNCTION FALL_T08(A0,C0,A2,C2,A3,C3)'
     &      / '! rate constant for CMAQ fall off reaction type 8'
     &      / 9X,'IMPLICIT NONE'
     &      / '! Arguements:'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: A0'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: C0'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: A2'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: C2'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: A3'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: C3'
     &      / 9X,'! Local:'
     &      / 9X,'REAL( 8 ) K0'
     &      / 9X,'REAL( 8 ) K2'
     &      / 9X,'REAL( 8 ) K3'
     &      / 9X,'INTRINSIC DEXP'
     &      / 9X,'K0 = A0 * DEXP( C0 * INV_TEMP )'
     &      / 9X,'K2 = A2 * DEXP( C2 * INV_TEMP )'
     &      / 9X,'K3 = A3 * DEXP( C3 * INV_TEMP )'
     &      / 9X,'K3 = K3 * CAIR'
     &      / 9X,'FALL_T08 = K0 + K3/( 1.0D0 + K3/K2 )'
     &      / 9X,'RETURN'
     &     2/ 7X,'END FUNCTION FALL_T08'
     &     2/ 7X,'REAL( 8 ) FUNCTION FALL_T11(A1,B1,C1,A2, B2, C2)'
     &      / '! rate constant for CMAQ fall off reaction type 11'
     &      / '! actually expanded form of type 9'
     &      / 9X,'IMPLICIT NONE'
     &      / '! Arguements:'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: A1'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: B1'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: C1'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: A2'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: B2'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: C2'
     &      /9X,'!  Local:'
     &      / 9X,'REAL( 8 ) K1'
     &      / 9X,'REAL( 8 ) K2'
     &      / 9X,'INTRINSIC DEXP'
     &      / 9X,'K1 = A1 * DEXP( C1 * INV_TEMP ) * TEMPOT300**B1'
     &      / 9X,'K2 = A2 * DEXP( C2 * INV_TEMP ) * TEMPOT300**B2'
     &      / 9X,'FALL_T11 = K1 + K2 * CAIR'
     &      / 9X,'RETURN'
     &     2/ 7X,'END FUNCTION FALL_T11'     
     &     2/ 7X,'REAL( 8 ) FUNCTION FALL_T09(A1,C1,A2,C2)'
     &      / '! rate constant for CMAQ fall off reaction type 9'
     &      / 9X,'IMPLICIT NONE'
     &      / '! Arguements:'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: A1'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: C1'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: A2'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: C2'
     &      /9X,'!  Local:'
     &      / 9X,'REAL( 8 ) K1'
     &      / 9X,'REAL( 8 ) K2'
     &      / 9X,'INTRINSIC DEXP'
     &      / 9X,'K1 = A1 * DEXP( C1 * INV_TEMP )'
     &      / 9X,'K2 = A2 * DEXP( C2 * INV_TEMP )'
     &      / 9X,'FALL_T09 = K1 + K2 * CAIR'
     &      / 9X,'RETURN'
     &     2/ 7X,'END FUNCTION FALL_T09'       )

4510  FORMAT(/7X,'CONTAINS'
     &      2/ 
     &      / 7X,'REAL( 8 ) FUNCTION POWER_T02( TEMPOT300,A0,B0 )'
     &      / 9X,'IMPLICIT NONE'
     &      / '! rate constant for CMAQ Arrhenuis reaction type 2'
     &      / '! Arguements:'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: TEMPOT300'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: A0'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: B0'
     &      / 9X,'! Local: None'
     &      / 9X,'POWER_T02 =  A0 * TEMPOT300**B0'
     &      / 9X,'RETURN'
     &      / 7X,'END FUNCTION POWER_T02'
     &      / 7X,'REAL( 8 ) FUNCTION ARRHENUIS_T04( INV_TEMP,TEMPOT300,A0,B0,C0 )'
     &      / 9X,'IMPLICIT NONE'
     &      / '! rate constant for CMAQ Arrhenuis reaction type 4'
     &      / '! Arguements:'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: INV_TEMP'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: TEMPOT300'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: A0'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: B0'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: C0'
     &      / 9X,'! Local:'
     &      / 9X,'INTRINSIC DEXP'
     &      / 9X,'ARRHENUIS_T04 =  A0 * DEXP( B0 * INV_TEMP ) * TEMPOT300**C0'
     &      / 9X,'RETURN'
     &      / 7X,'END FUNCTION ARRHENUIS_T04'
     &      / 7X,'REAL( 8 ) FUNCTION ARRHENUIS_T03( INV_TEMP,A0,B0 )'
     &      / '! rate constant for CMAQ Arrhenuis reaction type 3'
     &      / 9X,'IMPLICIT NONE'
     &      / '! Arguements:'
     &      / 9X,'REAL( 8 ),   INTENT( IN ) ::  INV_TEMP'
     &      / 9X,'REAL( 8 ),     INTENT(IN) ::  A0'
     &      / 9X,'REAL( 8 ),     INTENT(IN) ::  B0'
     &      / 9X,'! Local:'
     &      / 9X,'INTRINSIC DEXP'
     &      / 9X,'ARRHENUIS_T03 =  A0 * DEXP( B0 * INV_TEMP )'
     &      / 9X,'RETURN'
     &      / 7X,'END FUNCTION ARRHENUIS_T03 '
     &      / 7X,'REAL( 8 ) FUNCTION FALLOFF_T08(INV_TEMP,CAIR,A0,C0,A2,C2,A3,C3)'
     &      / '! rate constant for CMAQ fall off reaction type 8'
     &      / 9X,'IMPLICIT NONE'
     &      / '! Arguements:'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: INV_TEMP'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: CAIR'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: A0'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: C0'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: A2'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: C2'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: A3'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: C3'
     &      / 9X,'! Local:'
     &      / 9X,'REAL( 8 ) K0'
     &      / 9X,'REAL( 8 ) K2'
     &      / 9X,'REAL( 8 ) K3'
     &      / 9X,'INTRINSIC DEXP'
     &      / 9X,'K0 = A0 * DEXP( C0 * INV_TEMP )'
     &      / 9X,'K2 = A2 * DEXP( C2 * INV_TEMP )'
     &      / 9X,'K3 = A3 * DEXP( C3 * INV_TEMP )'
     &      / 9X,'K3 = K3 * CAIR'
     &      / 9X,'FALLOFF_T08 = K0 + K3/( 1.0D0 + K3/K2 )'
     &      / 9X,'RETURN'
     &      / 7X,'END FUNCTION FALLOFF_T08'
     &      / 7X,'REAL( 8 ) FUNCTION FALLOFF_T09(INV_TEMP,CAIR,A1,C1,A2,C2)'
     &      / '! rate constant for CMAQ fall off reaction type 9'
     &      / 9X,'IMPLICIT NONE'
     &      / '! Arguements:'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: INV_TEMP'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: CAIR'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: A1'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: C1'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: A2'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: C2'
     &      / 9X,'!  Local:'
     &      / 9X,'REAL( 8 ) K1'
     &      / 9X,'REAL( 8 ) K2'
     &      / 9X,'INTRINSIC DEXP'
     &      / 9X,'K1 = A1 * DEXP( C1 * INV_TEMP )'
     &      / 9X,'K2 = A2 * DEXP( C2 * INV_TEMP )'
     &      / 9X,'FALLOFF_T09 = K1 + K2 * CAIR'
     &      / 9X,'RETURN'
     &      / 7X,'END FUNCTION FALLOFF_T09'
     &      / 7X,'REAL( 8 ) FUNCTION FALLOFF_T10(INV_TEMP,TEMPOT300,CAIR,A0,B0,C0,A1,B1,C1,CE,CF)'
     &      / 9X,'IMPLICIT NONE'
     &      / '! rate constant for CMAQ fall off reaction type 10'
     &      / '! Arguements:'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: INV_TEMP'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: TEMPOT300'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: CAIR'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: A0'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: B0'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: C0'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: A1'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: B1'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: C1'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: CE'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: CF'
     &      / 9X,'! Local:'
     &      / 9X,'REAL( 8 ) K0'
     &      / 9X,'REAL( 8 ) K1'
     &      / 9X,'REAL( 8 ) KEND'
     &      / 9X,'K0 = A0 * CAIR * DEXP(B0*INV_TEMP)* TEMPOT300**C0'
     &      / 9X,'K1 = A1 * DEXP(B1*INV_TEMP) * TEMPOT300**C1'
     &      / 9X,'KEND = ( ( 1.0D0 + ( ( 1.0D0 / CE ) * DLOG10( K0 / K1 ) ) ** 2.0D0 ) )'
     &      / 9X,'KEND = 1.0D0 / KEND'
     &      / 9X,'FALLOFF_T10 = ( K0 / ( 1.0D0 + K0/K1 ) ) * CF ** KEND'
     &      / 9X,'RETURN'
     &      / 7X,'END FUNCTION FALLOFF_T10' 
     &      / 7X,'REAL( 8 ) FUNCTION FALLOFF_T11(INV_TEMP,TEMPOT300,CAIR,A1,B1,C1,A2, B2, C2, D1, D2)'
     &      / '! rate constant for CMAQ fall off reaction type 11'
     &      / '! actually expanded form of type 9'
     &      / 9X,'IMPLICIT NONE'
     &      / '! Arguements:'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: INV_TEMP'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: TEMPOT300'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: CAIR'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: A1'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: B1'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: C1'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: A2'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: B2'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: C2'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: D1'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: D2'
     &      /9X,'!  Local:'
     &      / 9X,'REAL( 8 ) K1'
     &      / 9X,'REAL( 8 ) K2'
     &      / 9X,'REAL( 8 ) K3'
     &      / 9X,'INTRINSIC DEXP'
     &      / 9X,'K1 = A1 * DEXP( C1 * INV_TEMP ) * TEMPOT300**B1'
     &      / 9X,'K2 = A2 * DEXP( C2 * INV_TEMP ) * TEMPOT300**B2'
     &      / 9X,'K3 = D1 * DEXP( D2 * INV_TEMP )'
     &      / 9X,'FALLOFF_T11 = K1 + K2 * CAIR + K3'
     &      / 9X,'RETURN'
     &      / 7X,'END FUNCTION FALLOFF_T11' 
     &      / 7X,'REAL( 8 ) FUNCTION HALOGEN_FALLOFF(PRESS,A1,B1,A2,B2)'
     &      / 9X,'IMPLICIT NONE'
     &      / 9X,'REAL( 8 ), PARAMETER    :: MAX_RATE = 2.6750D-06  ! Maximum loss rate (1/sec)'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: PRESS'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: A1'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: B1'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: A2'
     &      / 9X,'REAL( 8 ), INTENT( IN ) :: B2'
     &      / 9X,'INTRINSIC DEXP'
     &      / 9X,'HALOGEN_FALLOFF = A1 * DEXP( B1 * PRESS ) + A2 * DEXP( B2 * PRESS )'
     &      / 9X,'HALOGEN_FALLOFF = DMIN1 (MAX_RATE, HALOGEN_FALLOFF )'
     &      / 9X,'RETURN'
     &      / 7X,'END FUNCTION HALOGEN_FALLOFF' 
     &      /    )
    
    
4501   FORMAT( '! Name of Mechanism ', A
     &       2/ 7X,'PUBLIC             :: CALC_RCONST, SPECIAL_RATES'
     &       2/ 7X,'REAL( 8 ), PRIVATE :: CAIR          ! air number density (wet) [molec/cm^3]'
     &        / 7X,'REAL( 8 ), PRIVATE :: CFACT         ! molec/cc to ppm conversion factor   '/
     &        / 7X,'REAL( 8 ), PRIVATE :: CFACT_SQU     ! molec/cc to ppm conversion factor squared  '/
     &        / 7X,'REAL( 8 ), PRIVATE :: INV_CFACT     ! Reciprocal of molec/cc to ppm conversion factor   '/
     &        / 7X,'REAL( 8 ), PRIVATE :: TEMPOT300     ! temperature divided by 300 K, dimensionaless '/
     &        / 7X,'REAL( 8 ), PRIVATE :: INV_TEMP      ! reciprocal of air temperature, K-1'
     &        / 7X,'REAL( 8 ), PRIVATE :: TEMP          ! air temperature, K'
     &        / 7X,'REAL( 8 ), PRIVATE :: PRESS         ! pressure [Atm] '
     &        / 7X,'REAL( 8 ), PRIVATE :: INV_RFACTOR   ! Convertor: ppm/min to molec/(cm^3*sec)'
     &        / 7X,'REAL( 8 ), PRIVATE :: RFACTOR_SQU   ! Convertor cm^6/(molec^2*sec) to 1/(ppm^2*min)'
     &        / 7X,'REAL( 8 ), PRIVATE :: RFACTOR       ! Convertor cm^3/(molec*sec) to 1/(ppm*min)'
     &        / 7X,'REAL,      PRIVATE :: H2O           ! Cell H2O mixing ratio (ppmV)')

4601   FORMAT( '! Name of Mechanism ', A32
     &        / 7X,'PUBLIC             :: CALC_RCONST, SPECIAL_RATES'
     &       2/ 7X,'REAL( 8 ), PRIVATE :: CAIR          ! air number density (wet) [molec/cm^3]'
     &        / 7X,'REAL( 8 ), PRIVATE :: CFACT         ! Convertor cm^3/(molec*sec) to 1/(ppm*min)'/
     &        / 7X,'REAL( 8 ), PRIVATE :: CFACT_SQU     ! Convertor cm^6/(molec^2*sec) to 1/(ppm^2*min)'/
     &        / 7X,'REAL( 8 ), PRIVATE :: INV_CFACT     ! ppm/min to molec/(cm^3*sec)'/
     &        / 7X,'REAL( 8 ), PRIVATE :: TEMPOT300     ! temperature divided by 300 K, dimensionaless '/
     &        / 7X,'REAL( 8 ), PRIVATE :: INV_TEMP      ! reciprocal of air temperature, K-1'
     &        / 7X,'REAL( 8 ), PRIVATE :: TEMP          ! air temperature, K'
     &        / 7X,'REAL( 8 ), PRIVATE :: PRESS         ! pressure [Atm] '
     &        / 7X,'REAL( 8 ), PRIVATE :: INV_RFACT     ! ppm/min to molec/(cm^3*min)'
     &        / 7X,'REAL( 8 ), PRIVATE :: RFACT_SQU     ! cm^6/(molec^2*min) to 1/(ppm^2*min)'
     &        / 7X,'REAL( 8 ), PRIVATE :: RFACT         ! cm^3/(molec*min) to 1/(ppm*min)'
     &        / 7X,'REAL       PRIVATE :: H2O           ! Cell H2O mixing ratio (ppmV)')

4611   FORMAT( '! Name of Mechanism ', A
     &        // 7X,'PUBLIC             :: CALC_RCONST, SPECIAL_RATES, MAP_CHEMISTRY_SPECIES' )
          
4502   FORMAT(  '! pointers and names to specific photolysis rates' )
4503   FORMAT(  7X,'INTEGER, PARAMETER  :: IJ_',A16,' = ', I3 )
4504   FORMAT(' ' )
4555   FORMAT(' ')
4556   FORMAT( 'RFACTOR       = 6.0D-5  * CAIR'
     &       / 'INV_RFACTOR   = 6.0D+7  / CAIR'
     &       / 'RFACTOR_SQU   = 6.0D-11 * CAIR * CAIR'
     &       / 'CFACTOR       = 1.0D0'
     &       / 'INV_TEMP      = 1.0D0 / TEMP'
     &       / 'COEFF_FALLOFF = CAIR ' )
4557   FORMAT('DATA PHOTAB(', I3,' ) / ''',A16,''' /')
4507  FORMAT('RKI_RXN_', A16,A4)        
4706  FORMAT(A,1X,'RKI( NCELL, ', I4,' ) ')
4726  FORMAT(A,1X)
4708  FORMAT(A,1PD12.4,' * RKI( NCELL, ', I4,' ) ')
4718  FORMAT(A,1PD12.4,' * ')
4709  FORMAT( A )     
4710  FORMAT(A,1PD12.4,' * ', A)
4711  FORMAT( / 5X, '&' 21X)
4712  FORMAT(A, 1X, A)
4713  FORMAT( '!If( .Not. CALC_RCONST )Then'
     &      / '!   Return'
     &      / '!Else'
     &      / '!   CALC_RCONST = .False.'
     &      / '!End If' 
     &      / '! Rate Constant Units produce reaction rates in ppm/min' )
4714  FORMAT('! number mixing ratios of constant atmospheric species, ppmV')     
4749   FORMAT('!Flag to call SPECIAL_RATES rountine in Integrator ')
4750   FORMAT(7X, 'LOGICAL,  PARAMETER :: USE_SPECIAL_RATES = .TRUE. ')
4751   FORMAT(7X, 'LOGICAL,  PARAMETER :: USE_SPECIAL_RATES = .FALSE.')
5000   FORMAT(1PD12.4,' * RJBLK( NCELL, IJ_',A,' )')
5001   FORMAT( 1X, 'RJBLK( NCELL, IJ_',A, ' )' )
5100   FORMAT(1PD12.4,' * RKI( NCELL, ',I4,' )')
5101   FORMAT(  'RKI( NCELL, ',I4,' )')
5029   FORMAT('POWE_T02( ',1PD12.4,', ', 1PD12.4,' )')
5002   FORMAT('ARRE_T04( ',1PD12.4,', 0.0000D+0,', 1PD12.4,' )')
5003   FORMAT('ARRE_T03( ',1PD12.4,', ', 1PD12.4,' )')
5004   FORMAT('ARRE_T04( ', 1PD12.4,', ', 1PD12.4,', ', 1PD12.4,' )')
5014   FORMAT('ARRE_T04( ',1PD12.4,', 0.0000D+0,', 1PD12.4,' )  * PRESS ')             
5008   FORMAT('FALL_T08( ', 3(1PD12.4,', '), ' & ' / 5X, '&', 47X, 2(1PD12.4,', '), 1PD12.4, ' )' )
5009   FORMAT('FALL_T09( ', 3(1PD12.4,', '), ' & ' / 5X, '&', 47X, 1PD12.4, ' )' )
5010   FORMAT('FALL_T10( ', 3(1PD12.4,', '), ' & ' / 5X,'&', 47X, 3(1PD12.4,', '),  ' & '
     &        / 5X, '&', 47X, 1PD12.4,', ', 1PD12.4,' )')
5019   FORMAT('FALL_T11( ', 3(1PD12.4,', '), '&', / 5X,'&', 47X,  3(1PD12.4,', '), ' & ',
     &                   / 5X,'&', 47X,  1PD12.4,', ', 1PD12.4,' )')

!format statements for calling rate constant functions

5111   FORMAT(1PD12.4) 
5129   FORMAT('POWER_T02( TEMPOT300, ',1PD12.4,', ', 1PD12.4,' )')
5102   FORMAT('ARRHENUIS_T04( INV_TEMP,  TEMPOT300,',1PD12.4,', 0.0000D+0,', 1PD12.4,' )')
5103   FORMAT('ARRHENUIS_T03( INV_TEMP,',1PD12.4,', ', 1PD12.4,' )')
5104   FORMAT('ARRHENUIS_T04( INV_TEMP,  TEMPOT300,',  ' & ' / 5X, '&', 49X, 1PD12.4,', ', 1PD12.4,
     &        ', ', 1PD12.4,' )')
5114   FORMAT('ARRHENUIS_T04( INV_TEMP,  TEMPOT300,',  ' & ' / 5X, '&', 49X, 1PD12.4,', 0.0000D+0,',
     &        1PD12.4,' )  * PRESS ')             
5108   FORMAT('FALLOFF_T08( INV_TEMP,  CAIR,', ' & ' / 5X, '&', 47X, 3(1PD12.4,', '), ' & ' / 5X, '&',
     &         47X, 2(1PD12.4,', '), 1PD12.4, ' )' )
5109   FORMAT('FALLOFF_T09( INV_TEMP,  CAIR,', ' & ' / 5X, '&', 47X, 3(1PD12.4,', '), ' & ' / 5X, '&',
     &         47X, 1PD12.4, ' )' )
5110   FORMAT('FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR,', ' & ' / 5X, '&', 47X, 3(1PD12.4,', '), ' & ' 
     &        / 5X,'&', 47X, 3(1PD12.4,', '),  ' & '
     &        / 5X, '&', 47X, 1PD12.4,', ', 1PD12.4,' )')
5119   FORMAT('FALLOFF_T11( INV_TEMP,TEMPOT300,CAIR,', ' & ' / 5X, '&', 47X, 3(1PD12.4,', '), '&', / 5X,'&',
     &         47X,  3(1PD12.4,', '),' & ',
     &        / 5X,'&', 47X,  1PD12.4,', ', 1PD12.4,' )')
5120   FORMAT('HALOGEN_FALLOFF( BLKPRES( NCELL ), ' 2(1PD12.4,', '), ' & ' / 5X, 
     &        '&', 57X, (1PD12.4,', '), 6X, 1PD12.4, ' )')

5005   FORMAT('RKI( NCELL, ' I4, ' ) / ARR2( ',1PD12.4,', ',1PD12.4,' )')            
5115   FORMAT('RKI( NCELL, ' I4, ' )' , ' & ' 
     &        / 5X, '&', 25X, '* (' 1PD12.4,' * DEXP(', 1PD12.4,' * INV_TEMP) ) ')
5125   FORMAT('RKI( NCELL, ' I4, ' ) * ARRHENUIS_T03( INV_TEMP,',1PD12.4,', ',1PD12.4,' )')            
5006   FORMAT(1PD12.4,' * RKI( NCELL, ' I4, ' ) ')   
5007   FORMAT(1PD12.4,' *( 1.0D0 + 0.6D0 * PRESS )')             
5011   FORMAT(1PD12.4,' * ',A)             
5012   FORMAT(A)
5027   FORMAT(1PD12.4,' * BLKHET( NCELL, IK_',A,' )')
5028   FORMAT( 1X, 'KHETERO( NCELL, IK_',A, ' )' )
5128   FORMAT( 1X, 'BLKHET(  NCELL, IK_',A, ' )' )
5023   FORMAT(
     &        / 'INTEGER, PARAMETER  :: NHETERO  = ', I3,'  ! number of heterogeneous rates '
     &        / 'CHARACTER(16), SAVE :: HETERO(  NHETERO )  ! Names of  heterogeneous '
     &        / 'REAL( 8 )           :: KHETERO( NHETERO )  ! grid cell heterogeneous rates ,[min-1]')
5024   FORMAT(7X,'INTEGER, PARAMETER  :: IK_',A16,' = ', I3 )
5025   FORMAT('DATA HETERO(', I3,' ) / ''',A16,''' /')
5026   FORMAT(7X,'INTEGER, PARAMETER  :: NHETERO  = ', I3,'  ! number of heterogeneous rates ')
94000 FORMAT( 1X,'One of the dimensions below is too small:')
94020 FORMAT( 1X,'DIMENSION: MXCOUNT2 = ',I6,' VARIABLE: ICNT    = ',I6)  
94040 FORMAT( 1X,'DIMENSION: MXCOUNT1 = ',I6,' VARIABLE: JCNT    = ',I6)  
94060 FORMAT( 1X,'DIMENSION: MXARRAY  = ',I6,' VARIABLE: IARRAY2 = ',I6)  
94080 FORMAT( 1X,'DIMENSION: MXARRAY  = ',I6,' VARIABLE: ICCOUNT = ',I6)  
94100 FORMAT( 1X,'DIMENSION: MXARRAY  = ',I6,' VARIABLE: JCCOUNT = ',I6)
94200 FORMAT( 1X,'DIMENSION: MXRP     = ',I6,' VARIABLE: NDPMAX  = ',I6)
94220 FORMAT( 1X,'DIMENSION: MXRR     = ',I6,' VARIABLE: NDLMAX  = ',I6)

95050  FORMAT( 7X,'SUBROUTINE SPECIAL_RATES( NUMCELLS, Y, TEMP, DENS, RKI )'
     &       /  '! Purpose: calculate special rate operators and update'
     &       /  '!         appropriate rate constants'
     &      //  7X,'USE RXNS_DATA'
     &      /   7X,'IMPLICIT NONE'
     &      //  '! Arguments:'
     &      /   7X,'INTEGER,      INTENT( IN  )   :: NUMCELLS        ! Number of cells in block '
     &      /   7X,'REAL( 8 ),    INTENT( IN )    :: Y( :, : )       ! species concs'
     &      /   7X,'REAL( 8 ),    INTENT( IN )    :: TEMP( : )       ! air temperature, K '
     &      /   7X,'REAL( 8 ),    INTENT( IN )    :: DENS( : )       ! air density, Kg/m3'
     &      /   7X,'REAL( 8 ),    INTENT( INOUT ) :: RKI( :, : )     ! reaction rate constant, ppm/min '
     &      /   '! Local:'
     &      /   7X,'REAL( 8 ), PARAMETER :: DENSITY_TO_NUMBER = 2.07930D+19 ! Kg/m3 to molecules/cm3' /
     &      /   7X,'INTEGER   :: NCELL'
     &      /   7X,'REAL( 8 ) :: INV_TEMP'
     &      /   7X,'REAL( 8 ) :: CAIR'
     &      /   7X,'REAL( 8 ) :: CFACT         ! scales operator if not multiplied by RKI, cm^3/(molecule) to 1/(ppm)'
     &      /   7X,'REAL( 8 ) :: CFACT_SQU     ! scales operator if not multiplied by RKI, cm^6/(molec^2) to 1/(ppm^2)'
     &      /   '! special rate operators listed below' //)
     
     
95051  FORMAT(/ 7X,'DO NCELL = 1, NUMCELLS'
     &      /   7X,'   INV_TEMP  = 1.0D0 / TEMP( NCELL )'
     &      /   7X,'   CAIR      = DENSITY_TO_NUMBER * DENS( NCELL )'
     &      /   7X,'   CFACT     = 1.0D-06 * CAIR'  
     &      /   7X,'   CFACT_SQU = 1.0D-12 * CAIR * CAIR' /
     &      //  '! define special rate operators' / )
95052  FORMAT(/ 7X,'DO NCELL = 1, NUMCELLS'
     &      /   7X,'   INV_TEMP  = 1.0D0 / TEMP( NCELL )'
     &      /   7X,'   CAIR      = 1.0D+6'
     &      /   7X,'   CFACT     = 1.0D0'  
     &      /   7X,'   CFACT_SQU = 1.0D0' /
     &      //  '! define special rate operators' / )
95060  FORMAT(  7X,'END DO'
     &         // 7X,'RETURN'
     &          / 7X,'END SUBROUTINE SPECIAL_RATES')
95100  FORMAT(2X,A16,' = 0.0D0')        

95066 FORMAT(11X,'RKI( NCELL,',I4,' ) = DEXP( -',1PD10.4,'*INV_TEMP ) * ', A16,' ! reaction: ',A)
95067 FORMAT(11X,'RKI( NCELL,',I4,' ) = ',1PD10.4,' * DEXP( -',1PD10.4,'*INV_TEMP ) * ', A16,' ! reaction: ',A)
95068 FORMAT(11X,'RKI( NCELL,',I4,' ) = ',1PD10.4,' * ', A16,' ! reaction: ',A)
95069 FORMAT(11X,'RKI( NCELL,',I4,' ) = ')
95076 FORMAT(11X,'RKI( NCELL,',I4,' ) = DEXP( ',1PD10.4,'*INV_TEMP ) * ', A16,' ! reaction: ',A)
95077 FORMAT(11X,'RKI( NCELL,',I4,' ) = ',1PD10.4,' * DEXP( ',1PD10.4,'*INV_TEMP ) * ', A16,' ! reaction: ',A)
95087 FORMAT(11X,'RKI( NCELL,',I4,' ) = -',1PD10.4,' * DEXP( -',1PD10.4,'*INV_TEMP ) * ', A16,' ! reaction: ',A)
95086 FORMAT(11X,'RKI( NCELL,',I4,' ) = -',1PD10.4,' * DEXP( ',1PD10.4,'*INV_TEMP ) * ', A16,' ! reaction: ',A)
95088 FORMAT(11X,'RKI( NCELL,',I4,' ) = -',1PD10.4,' * ', A16,' ! reaction: ',A)
95070 FORMAT(11X,'RKI( NCELL,',I4,' ) = ',A16,13X,' ! reaction: ',A)
95071 FORMAT('RKI( NCELL,',I4,' ) ')


99870 FORMAT(7X,'SUBROUTINE CALC_RCONST( BLKTEMP, BLKPRES, BLKH2O, RJBLK, BLKHET, LSUNLIGHT, LAND, RKI, NUMCELLS )' //
     & '!**********************************************************************' //
     & '!  Function: To compute thermal and photolytic reaction rate' /
     & '!            coefficients for each reaction.' //
     & '!  Preconditions: Photolysis rates for individual species must have' /
     & '!                 been calculated and stored in RJPHOT. Expects' /
     & '!                 temperature in deg K, pressure in atm., water' /
     & '!                 vapor in ppmV, and J-values in /min.' /
     & '!  Key Subroutines/Functions Called: POWER_02, ARRHRENUIS_T0*, FALLOFF_T*, HALOGEN_FALLOFF ' /
     & '!***********************************************************************'///
     &      //  7X,'USE RXNS_DATA'  //
     & '        IMPLICIT NONE  ' //
     & '!  Arguements: None ' //
     & '        REAL( 8 ),           INTENT( IN  ) :: BLKTEMP( : )      ! temperature, deg K '/
     & '        REAL( 8 ),           INTENT( IN  ) :: BLKPRES( : )      ! pressure, Atm'/
     & '        REAL( 8 ),           INTENT( IN  ) :: BLKH2O ( : )      ! water mixing ratio, ppm '/
     & '        REAL( 8 ),           INTENT( IN  ) :: RJBLK  ( :, : )   ! photolysis rates, 1/min '/ 
     & '        REAL( 8 ),           INTENT( IN  ) :: BLKHET ( :, : )   ! heterogeneous rate constants, ???/min'/
     & '        INTEGER,             INTENT( IN  ) :: NUMCELLS          ! Number of cells in block ' /
     & '        LOGICAL,             INTENT( IN  ) :: LSUNLIGHT         ! Is there sunlight? ' /
     & '        LOGICAL,             INTENT( IN  ) :: LAND( : )         ! Is the surface totally land? ' /
     & '        REAL( 8 ),           INTENT( OUT ) :: RKI ( :, : )      ! reaction rate constant, ppm/min '/
!     & '        LOGICAL,   OPTIONAL, INTENT( IN  ) :: LAND( : )         ! Is the surface totally land? ' /
     & '!..Parameters: ' //
     & '        REAL( 8 ), PARAMETER :: COEF1  = 7.33981D+15     ! Molec/cc to ppm conv factor ' /
     & '        REAL( 8 ), PARAMETER :: CONSTC = 0.6D+0          ! Constant for reaction type 7' /
     & '        REAL( 8 ), PARAMETER :: TI300  = 1.0D+0/300.0D+0 ! reciprocal of 300 deg K' /
     & '        REAL( 8 ), PARAMETER :: SFACT  = 60.D+0          ! seconds per minute ' /
     & '!..External Functions: None' //
     & '!..Local Variables:' //
     & '        INTEGER   :: NRT           ! Loop index for reaction types '/
     & '        INTEGER   :: IRXN          ! Reaction number'/
     & '        INTEGER   :: JNUM          ! J-value species # from PHOT)'/
     & '        INTEGER   :: KNUM          ! Reaction # for a relative rate coeff.'/
     & '        INTEGER   :: N             ! Loop index for reactions'/
     & '        INTEGER   :: NCELL         ! Loop index for # of cells in the block' /
     & '        REAL( 8 ) :: CAIR          ! air number density (wet) [molec/cm^3]' /
     & '        REAL( 8 ) :: CFACT         ! Convertor cm^3/(molec*sec) to 1/(ppm*min)'/
     & '        REAL( 8 ) :: CFACT_SQU     ! Convertor cm^6/(molec^2*sec) to 1/(ppm^2*min)'/
     & '        REAL( 8 ) :: INV_CFACT     ! ppm/min to molec/(cm^3*sec)'/
     & '        REAL( 8 ) :: TEMPOT300     ! temperature divided by 300 K, dimensionaless '/
     & '        REAL( 8 ) :: INV_TEMP      ! reciprocal of air temperature, K-1' /
     & '        REAL( 8 ) :: INV_CAIR      ! reciprocal of air number density (wet), [cm^3/molec]' /
     & '        REAL( 8 ) :: TEMP          ! air temperature, K' /
     & '        REAL( 8 ) :: PRESS         ! pressure [Atm] ' /
     & '        REAL( 8 ) :: INV_RFACT     ! ppm/min to molec/(cm^3*min)' /
     & '        REAL( 8 ) :: RFACT_SQU     ! cm^6/(molec^2*min) to 1/(ppm^2*min)' /
     & '        REAL( 8 ) :: RFACT         ! cm^3/(molec*min) to 1/(ppm*min)' /
     & '        REAL( 8 ) :: H2O           ! concentration, [molec/cm^3] '  //
     & '        RKI = 0.0D0 ' / )

99880 FORMAT(7X,'SUBROUTINE CALC_RCONST( BLKTEMP, BLKPRES, BLKH2O, RJBLK, BLKHET, LSUNLIGHT, RKI, NUMCELLS )' //
     & '!**********************************************************************' //
     & '!  Function: To compute thermal and photolytic reaction rate' /
     & '!            coefficients for each reaction.' //
     & '!  Preconditions: Photolysis rates for individual species must have' /
     & '!                 been calculated and stored in RJPHOT. Expects' /
     & '!                 temperature in deg K, pressure in atm., water' /
     & '!                 vapor in ppmV, and J-values in /min.' /
     & '!  Key Subroutines/Functions Called: POWER_02, ARRHRENUIS_T0*, FALLOFF_T* ' /
     & '!***********************************************************************'///
     &      //  7X,'USE RXNS_DATA'  //
     & '        IMPLICIT NONE  ' //
     & '!  Arguements: None ' //
     & '        REAL( 8 ), INTENT( IN  ) :: BLKTEMP( : )      ! temperature, deg K '/
     & '        REAL( 8 ), INTENT( IN  ) :: BLKPRES( : )      ! pressure, Atm'/
     & '        REAL( 8 ), INTENT( IN  ) :: BLKH2O ( : )      ! water mixing ratio, ppm '/
     & '        REAL( 8 ), INTENT( IN  ) :: RJBLK  ( :, : )   ! photolysis rates, 1/min '/ 
     & '        REAL( 8 ), INTENT( IN  ) :: BLKHET ( :, : )   ! heterogeneous rate constants, ???/min'/
     & '        INTEGER,   INTENT( IN  ) :: NUMCELLS          ! Number of cells in block ' /
     & '        LOGICAL,   INTENT( IN  ) :: LSUNLIGHT         ! Is there sunlight? ' /
     & '        REAL( 8 ), INTENT( OUT ) :: RKI ( :, : )   ! reaction rate constant, ppm/min '/
     & '!..Parameters: ' //
     & '        REAL( 8 ), PARAMETER :: COEF1  = 7.33981D+15     ! Molec/cc to ppm conv factor ' /
     & '        REAL( 8 ), PARAMETER :: CONSTC = 0.6D+0          ! Constant for reaction type 7' /
     & '        REAL( 8 ), PARAMETER :: TI300  = 1.0D+0/300.0D+0 ! reciprocal of 300 deg K' /
     & '        REAL( 8 ), PARAMETER :: SFACT  = 60.D+0          ! seconds per minute ' /
     & '!..External Functions: None' //
     & '!..Local Variables:' //
     & '        INTEGER   :: NRT           ! Loop index for reaction types '/
     & '        INTEGER   :: IRXN          ! Reaction number'/
     & '        INTEGER   :: JNUM          ! J-value species # from PHOT)'/
     & '        INTEGER   :: KNUM          ! Reaction # for a relative rate coeff.'/
     & '        INTEGER   :: N             ! Loop index for reactions'/
     & '        INTEGER   :: NCELL         ! Loop index for # of cells in the block' /
     & '        REAL( 8 ) :: CAIR          ! air number density (wet) [molec/cm^3]' /
     & '        REAL( 8 ) :: CFACT         ! Convertor cm^3/(molec*sec) to 1/(ppm*min)'/
     & '        REAL( 8 ) :: CFACT_SQU     ! Convertor cm^6/(molec^2*sec) to 1/(ppm^2*min)'/
     & '        REAL( 8 ) :: INV_CFACT     ! ppm/min to molec/(cm^3*sec)'/
     & '        REAL( 8 ) :: TEMPOT300     ! temperature divided by 300 K, dimensionaless '/
     & '        REAL( 8 ) :: INV_TEMP      ! reciprocal of air temperature, K-1' /
     & '        REAL( 8 ) :: INV_CAIR      ! reciprocal of air number density (wet), [cm^3/molec]' /
     & '        REAL( 8 ) :: TEMP          ! air temperature, K' /
     & '        REAL( 8 ) :: PRESS         ! pressure [Atm] ' /
     & '        REAL( 8 ) :: INV_RFACT     ! ppm/min to molec/(cm^3*min)' /
     & '        REAL( 8 ) :: RFACT_SQU     ! cm^6/(molec^2*min) to 1/(ppm^2*min)' /
     & '        REAL( 8 ) :: RFACT         ! cm^3/(molec*min) to 1/(ppm*min)' /
     & '        REAL( 8 ) :: H2O           ! concentration, [molec/cm^3] '  //
     & '        RKI = 0.0D0 ' / )
99879   FORMAT(/'        IF( LSUNLIGHT )THEN ' /
     &          '            DO NCELL = 1, NUMCELLS ' )
99881   FORMAT(/'            END DO '
     &         / '       END IF ' )
!     &         / '       ELSE '
!     &         / '         DO N = 1, NSUNLIGHT_RXNS '
!     &         / '            DO NCELL = 1, NUMCELLS '
!     &         / '               RKI(NCELL, N ) = 0.0D0 '
!     &         / '            END DO'
!     &         / '         END DO'
!     &         / '       END IF ' )
99882   FORMAT(/
     & '        DO NCELL = 1, NUMCELLS ' /
     & '!  Set-up conversion factors '/
     & '             INV_TEMP  = 1.0D+00 / BLKTEMP( NCELL ) '/
     & '             CAIR      = 1.0D+06 * COEF1 * BLKPRES( NCELL ) * INV_TEMP '/
     & '             CFACT     = 6.0D-05 * CAIR' / 
     & '             CFACT_SQU = 6.0D-11 * CAIR * CAIR '/
     & '             INV_CAIR  = 1.0D0 / CAIR ' /
     & '             INV_CFACT = 6.0D+07 * INV_CAIR '/     
     & '             TEMP      = BLKTEMP( NCELL ) '/
     & '             TEMPOT300 = BLKTEMP( NCELL ) * TI300 '  )
99884  FORMAT('             H2O       = CAIR * BLKH2O( NCELL ) ')
99883  FORMAT(
     & '             RFACT     = 1.0D+06 * INV_CAIR ' / 
     & '             RFACT_SQU = 1.0D+12 * INV_CAIR * INV_CAIR ')
99991  FORMAT(7X // 7X, ' END DO  ' 
     & / '!  Multiply rate constants by [M], [O2], [N2], [H2O], [H2], or [CH4]'
     & / '!  where needed and return'
     & / 7X,'IF ( NWM .GT. 0 ) THEN'
     & / 7X,'   DO NRT = 1, NWM'
     & / 7X,'      IRXN = NRXWM( NRT )'
     & / 7X,'      DO NCELL = 1, NUMCELLS'
     & / 7X,'         RKI( NCELL,IRXN ) = RKI( NCELL,IRXN ) * ATM_AIR'
     & / 7X,'      END DO'
     & / 7X,'   END DO'
     & / 7X,'END IF' 
     & / 7X,'IF ( NWO2 .GT. 0 ) THEN'
     & / 7X,'   DO NRT = 1, NWO2'
     & / 7X,'      IRXN = NRXWO2( NRT )'
     & / 7X,'      DO NCELL = 1, NUMCELLS'
     & / 7X,'         RKI( NCELL,IRXN ) = RKI( NCELL,IRXN ) * ATM_O2'
     & / 7X,'      END DO'
     & / 7X,'   END DO'
     & / 7X,'END IF' 
     & / 7X,'IF ( NWN2 .GT. 0 ) THEN'
     & / 7X,'   DO NRT = 1, NWN2'
     & / 7X,'      IRXN = NRXWN2( NRT )'
     & / 7X,'      DO NCELL = 1, NUMCELLS'
     & / 7X,'         RKI( NCELL,IRXN ) = RKI( NCELL,IRXN ) * ATM_N2'
     & / 7X,'      END DO'
     & / 7X,'   END DO'
     & / 7X,'END IF' 
     & / 7X,'IF ( NWW .GT. 0 ) THEN'
     & / 7X,'   DO NRT = 1, NWW'
     & / 7X,'      IRXN = NRXWW( NRT )'
     & / 7X,'      DO NCELL = 1, NUMCELLS'
     & / 7X,'         RKI( NCELL,IRXN ) = RKI( NCELL,IRXN ) * BLKH2O( NCELL )'
     & / 7X,'      END DO'
     & / 7X,'   END DO'
     & / 7X,'END IF' 
     & / 7X,'IF ( NWH2 .GT. 0 ) THEN'
     & / 7X,'   DO NRT = 1, NWH2'
     & / 7X,'      IRXN = NRXWH2( NRT )'
     & / 7X,'      DO NCELL = 1, NUMCELLS'
     & / 7X,'         RKI( NCELL,IRXN ) = RKI( NCELL,IRXN ) * ATM_H2'
     & / 7X,'      END DO'
     & / 7X,'   END DO'
     & / 7X,'END IF' 
     & / 7X,'IF ( NWCH4 .GT. 0 ) THEN'
     & / 7X,'   DO NRT = 1, NWCH4'
     & / 7X,'      IRXN = NRXWCH4( NRT )'
     & / 7X,'      DO NCELL = 1, NUMCELLS'
     & / 7X,'         RKI( NCELL,IRXN ) = RKI( NCELL,IRXN ) * ATM_CH4'
     & / 7X,'      END DO'
     & / 7X,'   END DO'
     & / 7X,'END IF' 
     & / 7X, 'RETURN' 
     & / 7X,'END SUBROUTINE CALC_RCONST')

99890 FORMAT(7X,'SUBROUTINE CALC_RCONST( BLKTEMP, BLKPRES, BLKH2O, RJBLK, BLKHET, LSUNLIGHT, RKI, NUMCELLS )' //
     & '!**********************************************************************' //
     & '!  Function: To compute thermal and photolytic reaction rate' /
     & '!            coefficients for each reaction.' //
     & '!  Preconditions: Photolysis and heteorogeneous rate must have' /
     & '!                 been calculated and stored in RJPHOT. Expects' /
     & '!                 temperature in deg K, pressure in atm., water' /
     & '!                 vapor in ppmV, and J-values in /min.' /
     & '!  Key Subroutines/Functions Called: None ' /
     & '!***********************************************************************'///
     &      //  7X,'USE RXNS_DATA' //
     & '        IMPLICIT NONE  ' //
     & '!  Arguements: None ' //
     & '        REAL( 8 ), INTENT( IN  ) :: BLKTEMP( : )      ! temperature, deg K '/
     & '        REAL( 8 ), INTENT( IN  ) :: BLKPRES( : )      ! pressure, Atm '/
     & '        REAL( 8 ), INTENT( IN  ) :: BLKH2O ( : )      ! water mixing ratio, ppm '/
     & '        REAL( 8 ), INTENT( IN  ) :: RJBLK  ( :, : )   ! photolysis rates, 1/min '/ 
     & '        REAL( 8 ), INTENT( IN  ) :: BLKHET ( :, : )   ! heterogeneous rate constants, ???/min'/
     & '        INTEGER,   INTENT( IN  ) :: NUMCELLS          ! Number of cells in block ' /
     & '        LOGICAL,   INTENT( IN  ) :: LSUNLIGHT         ! Is there sunlight? ' /
     & '        REAL( 8 ), INTENT( OUT ) :: RKI ( :, : )      ! reaction rate constant, ppm/min '/
     & '!..Parameters: ' //
     & '        REAL( 8 ), PARAMETER :: COEF1      = 7.33981D+15     ! Molec/cc to ppm conv factor ' /
     & '        REAL( 8 ), PARAMETER :: CONSTC     = 0.6D+0          ! Constant for reaction type 7' /
     & '        REAL( 8 ), PARAMETER :: TI300      = 1.0D+0/300.0D+0 ! reciprocal of 300 deg K' /
     & '!..External Functions: None' //
     & '!..Local Variables:' //
     & '        INTEGER NRT                  ! Loop index for reaction types '/
     & '        INTEGER IRXN                 ! Reaction number'/
     & '        INTEGER JNUM                 ! J-value species # from PHOT)'/
     & '        INTEGER KNUM                 ! Reaction # for a relative rate coeff.'/
     & '        INTEGER N                    ! Loop index for reactions'/
     & '        INTEGER NCELL                ! Loop index for # of cells in the block' 
     & //
     & '          RKI = 0.0 ' /
     & '          DO NCELL = 1, NUMCELLS ' /
     & '!  Set-up conversion factors '/
     & '             INV_TEMP  = 1.0D+00 / BLKTEMP( NCELL ) '/
     & '             CAIR      = 1.0D+06 * COEF1 * BLKPRES( NCELL ) * INV_TEMP '/
     & '!             CFACT     = 6.0D-05 * CAIR' /
     & '!             CFACT_SQU = 6.0D-11 * CAIR * CAIR '/
     & '!             INV_CFACT = 6.0D+07 / CAIR '/     
     & '             RFACT     = 1.0D-06 * CAIR' / 
     & '             RFACT_SQU = 1.0D-12 * CAIR * CAIR '/
     & '             INV_RFACT = 1.0D+06 / CAIR '/     
     & '             CFACT     = 60.0D0  * RFACT' /
     & '             CFACT_SQU = 60.0D0  * RFACT_SQU '/
     & '             INV_CFACT = 60.0D0  * INV_RFACT '/     
     & '             TEMP      = BLKTEMP( NCELL ) '/
     & '             TEMPOT300 = BLKTEMP( NCELL ) * TI300 ' // )

       RETURN
       
       END SUBROUTINE WRT_RATE_CONSTANT
          
       SUBROUTINE  CONVERT_CASE_LOCAL ( BUFFER, UPPER )
C***********************************************************************

C  subroutine body starts at line  41
C
C  FUNCTION:  converts to upcase or lower the text in BUFFER
C             based on values of logic flag UPPER
C
C  PRECONDITIONS REQUIRED:  text is ASCII
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:  prototype 1/91 by CJC
C
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*)   BUFFER
        LOGICAL         UPPER


C...........   PARAMETER:  ASCII for 'a', 'z', 'A'

        INTEGER       IA, IZ, AADIF

        PARAMETER   ( IA    = 97,
     &                IZ    = 122,
     &                AADIF = 32 )


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER       I, L
        INTEGER       C
        INTEGER       FACTOR
        INTEGER       STRT, FINI
        


C***********************************************************************
C   begin body of subroutine  UPCASE

        L  =  LEN ( BUFFER )
        IF( UPPER )THEN
            FACTOR =  - AADIF
            STRT   =    IA
            FINI   =    IZ
        ELSE
            FACTOR =    AADIF
            STRT   =    IA - AADIF
            FINI   =    IZ - AADIF
        END IF
        
        DO  111  I = 1 , L
            C = ICHAR ( BUFFER ( I:I ) )
            IF ( C .GE. STRT  .AND.  C .LE. FINI ) THEN
                BUFFER ( I:I ) = CHAR ( C + FACTOR )
            END IF
111     CONTINUE        !  end loop on I

        RETURN
        END SUBROUTINE CONVERT_CASE_LOCAL

      SUBROUTINE WRITE_RATE_CONVERT_BEFORE(OUT_UNIT, RXN_ORDER)
        IMPLICIT NONE
        INTEGER, INTENT( IN ) :: OUT_UNIT
        INTEGER, INTENT( IN ) :: RXN_ORDER
        
         SELECT CASE( RXN_ORDER )
           CASE( 0 )
             WRITE(OUT_UNIT, 95000, ADVANCE = 'NO')
           CASE( 1 )
             WRITE(OUT_UNIT, 95001, ADVANCE = 'NO')
           CASE( 2 )
             WRITE(OUT_UNIT, 95002, ADVANCE = 'NO')
           CASE( 3 )
             WRITE(OUT_UNIT, 95003, ADVANCE = 'NO')
           CASE( -2 )
             WRITE(OUT_UNIT, 95005, ADVANCE = 'NO')
           CASE( -1 )
             WRITE(OUT_UNIT, 95004, ADVANCE = 'NO')
        END SELECT
95000   FORMAT(' INV_CFACT * ')                
95001   FORMAT(' SFACT * ')                
95002   FORMAT(' CFACT * ')                
95003   FORMAT(' CFACT_SQU * ')                
95004   FORMAT(' RFACT * ')                
95005   FORMAT(' RFACT_SQU * ')                
        RETURN
      END SUBROUTINE WRITE_RATE_CONVERT_BEFORE
      SUBROUTINE WRITE_RATE_CONVERT_AFTER(OUT_UNIT, RXN_ORDER)
        IMPLICIT NONE
        INTEGER, INTENT( IN ) :: OUT_UNIT
        INTEGER, INTENT( IN ) :: RXN_ORDER
        
         SELECT CASE( RXN_ORDER )
           CASE( 0 )
             WRITE(OUT_UNIT, 95000, ADVANCE = 'NO')
           CASE( 1 )
             WRITE(OUT_UNIT, 95001, ADVANCE = 'NO')
           CASE( 2 )
             WRITE(OUT_UNIT, 95002, ADVANCE = 'NO')
           CASE( 3 )
             WRITE(OUT_UNIT, 95003, ADVANCE = 'NO')
           CASE( -2 )
             WRITE(OUT_UNIT, 95005, ADVANCE = 'NO')
           CASE( -1 )
             WRITE(OUT_UNIT, 95004, ADVANCE = 'NO')
        END SELECT
95000   FORMAT(' * INV_CFACT ')                
95001   FORMAT(' * SFACT ')                
95002   FORMAT(' * CFACT ')                
95003   FORMAT(' * CFACT_SQU ')                
95004   FORMAT(' * RFACT ')                
95005   FORMAT(' * RFACT_SQU ')                
        RETURN
      END SUBROUTINE WRITE_RATE_CONVERT_AFTER
      SUBROUTINE WRITE_RATE_CONVERT_TIME(OUT_UNIT, RXN_ORDER)
        IMPLICIT NONE
        INTEGER, INTENT( IN ) :: OUT_UNIT
        INTEGER, INTENT( IN ) :: RXN_ORDER
        
         SELECT CASE( RXN_ORDER )
           CASE( 0 )
             WRITE(OUT_UNIT, 95000, ADVANCE = 'NO')
           CASE( 1 )
             WRITE(OUT_UNIT, 95001, ADVANCE = 'NO')
           CASE( 2 )
             WRITE(OUT_UNIT, 95002, ADVANCE = 'NO')
           CASE( 3 )
             WRITE(OUT_UNIT, 95003, ADVANCE = 'NO')
        END SELECT
95000   FORMAT(' INV_RFACT * ')                
95001   FORMAT(' ')                
95002   FORMAT(' RFACT * ')                
95003   FORMAT(' RFACT_SQU * ')                
        RETURN
      END SUBROUTINE WRITE_RATE_CONVERT_TIME
