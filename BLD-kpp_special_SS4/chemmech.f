
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
      PROGRAM CHEMMECH

      USE KPP_DATA

      IMPLICIT NONE
      INCLUDE 'PARMS.e'
      INCLUDE 'CHMECH.e'
      CHARACTER(  1 ) :: CHR
      CHARACTER( 16 ) :: WORD
      CHARACTER( 37 ) :: PHRASE
      CHARACTER( 81 ) :: INBUF
      CHARACTER( 12 ) :: MECHNAME      = 'MECHDEF'
      CHARACTER( 16 ) :: EQNS_KPP_FILE = 'EQNS_KPP_FILE'
      CHARACTER( 16 ) :: SPCS_KPP_FILE = 'SPCS_KPP_FILE'
      CHARACTER(  3 ) :: END
      CHARACTER( 16 ) :: SPCLIS( MAXSPEC )
      INTEGER, EXTERNAL :: INDEX1
      INTEGER, EXTERNAL :: INDEXES
      INTEGER IMECH, LPOINT, IEOL
      INTEGER I, ICOL, ISPC, IRX, IDX

      INTEGER NR, IP, NXX, NS, IPR, IPHOTAB, NC
      INTEGER MXPRD                            ! max no. products
      INTEGER NPRDCT( MAXRXNUM )               ! no. of products for rx j
      INTEGER NREACT( MAXRXNUM )               ! no. of reactants for rx j
      INTEGER IPH( MAXPHOTRXNS,3 )
      INTEGER NPHOTAB                          ! no. of photolysis tables
      CHARACTER( 16 ) :: PHOTAB( MAXPHOTRXNS ) ! photolysis table label

      INTEGER NSPECIAL_RXN
      INTEGER ISPECIAL( MAXSPECRXNS,2 )
      INTEGER NSPECIAL
      CHARACTER( 16 ) :: SPECIAL( MAXSPECRXNS )

      INTEGER         :: NKC_TERMS(  MAXSPECRXNS )
      CHARACTER( 16 ) :: KC_TERMS(   MAXSPECRXNS,  MAXSPECTERMS, 2)
      INTEGER         :: INDEX_KTERM( MAXSPECRXNS, MAXSPECTERMS)
      INTEGER         :: INDEX_CTERM( MAXSPECRXNS, MAXSPECTERMS)
      REAL( 8 )       :: KC_COEFFS(  MAXSPECRXNS,  MAXSPECTERMS)
      INTEGER         :: N_OPERATORS( MAXSPECRXNS )
      INTEGER         :: OPERATORS( MAXSPECRXNS, MAXSPECTERMS )
      REAL( 8 )       :: OPERATOR_COEFFS( MAXSPECRXNS, MAXSPECTERMS)
      INTEGER         :: NUSING_SPECIAL, IUSING_SPECIAL( MAXSPEC )
      CHARACTER( 16 ) :: USING_SPECIAL( MAXSPEC )

c..Variables for steady-state species
      INTEGER         :: N_SS_SPC = 0                         ! No. of SS species
      CHARACTER( 16 ) :: SS_SPC( MAXNLIST )                   ! List of SS pecies names
      INTEGER         :: SS_RCT_COEF( MAXNLIST, MAXRXNUM )    ! Reactant coeffs for each SS species
      REAL            :: SS_PRD_COEF( MAXNLIST, MAXRXNUM )    ! Product coeffs for each SS species
      INTEGER         :: SS1RX( MAXNLIST )                    ! First reaction occurrence for each SS species
      INTEGER         :: MAX_SS_LOSS = 0                      ! Max no of reactions for which 1 SS species
                                                              ! appears as a reactant
      INTEGER         :: MAX_SS_PROD = 0                      ! Max no of reactions for which 1 SS species
                                                              ! appears as a product
      INTEGER         :: N_LOSS_RXNS( MAXNLIST )              ! No. of loss rxns for each SS species
      INTEGER         :: N_PROD_RXNS( MAXNLIST )              ! No. of prod rxns for each SS species
      INTEGER         :: SS_LOSS_RXNS( MAXNLIST, MAXRXNUM )   ! List of rxns in which SS species is a reactant
      INTEGER         :: SS_PROD_RXNS( MAXNLIST, MAXRXNUM )   ! List of rxns in which SS species is a product
      INTEGER         :: SS_RCT_IND( MAXRXNUM )               ! SS spc ind that reacts w/ a non-SS spc
      REAL            :: SS_PROD_COEF( MAXNLIST, MAXRXNUM )   ! Yields for rxns producing a SS species
      INTEGER         :: DUMMY_COEF( MAXRXNUM )               ! Yields for the DUMMY variable in each reaction
      
c..Variables for species to be dropped from mechanism
      INTEGER         :: N_DROP_SPC = 0
      CHARACTER( 16 ) :: DROP_SPC( MAXNLIST )
      LOGICAL         :: LERROR
      LOGICAL         :: KPP_DUMMY   = .FALSE.
      LOGICAL         :: FIRST_TERM  = .TRUE.
      REAL( 8 )       :: WREXT_COEFFS( MAXSPECTERMS)
      INTEGER         :: WREXT_INDEX(  MAXSPECTERMS)

      INTEGER IRR( MAXRXNUM,MAXPRODS+3 )
      REAL    SC ( MAXRXNUM,MAXPRODS )
      CHARACTER( 16 ) :: LABEL( MAXRXNUM,2 ) ! LABEL(NXX,1) 1st label found in rx NXX
                                             ! LABEL(NXX,2) 2nd label found in rx NXX
      INTEGER SPC1RX( MAXSPEC )              ! rx index of 1st occurence of species
                                             ! in mechanism table
      CHARACTER( 120 ) :: EQNAME_MECH
      CHARACTER( 120 ) :: EQN_MECH_KPP
      CHARACTER( 120 ) :: SPC_MECH_KPP
      CHARACTER( 891 ) :: REACTION_STR(  MAXRXNUM )
      CHARACTER(  16 ) :: COEFF_STR
      CHARACTER(  32 ) :: DESCRP_MECH
      CHARACTER(  16 ) :: NAMCONSTS( MAXCONSTS ) = (/
     &                    'ATM_AIR',
     &                    'ATM_O2',
     &                    'ATM_N2',
     &                    'ATM_H2',
     &                    'ATM_CH4' /)
      CHARACTER(  16 ) :: CLABEL                  ! mechanism constants label
      REAL( 8 )        :: CONSTVAL                ! retrieved constant
      REAL( 8 )        :: CVAL( MAXCONSTS )       ! mechanism constants value
      INTEGER, PARAMETER :: LUNOUT = 6

      INTEGER, EXTERNAL :: JUNIT
      INTEGER            :: ICOUNT, IREACT, IPRODUCT
      EXTERNAL NAMEVAL

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Initialize mechanism array variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO 101 IRX = 1, MAXRXNUM
         DO ISPC = 1, MAXPRODS+3
            IRR( IRX,ISPC ) = 0
         END DO
         DO ISPC = 1, MAXPRODS
            SC( IRX,ISPC ) = 0.0
         END DO
         DO ISPC = 1, 3
            RTDAT( ISPC,IRX ) = 0.0
         END DO
         KTYPE( IRX ) = 0
         IORDER( IRX )  = 0
         IRXBITS( IRX ) = 0
         LABEL( IRX,1) = '<<<<<<<<<<<<<<<<'
         LABEL( IRX,2) = '>>>>>>>>>>>>>>>>'
         KRX1( IRX ) = 0
         KRX2( IRX ) = 0
         KRX3( IRX ) = 0
         KRX4( IRX ) = 0
         KRX5( IRX ) = 0
         KRX6( IRX ) = 0
         KRX7( IRX ) = 0
101   CONTINUE
      NFALLOFF = 0
      DO 103 IRX = 1, MAXFALLOFF
         IRRFALL( IRX ) = 0   
         DO ISPC = 1, 5
            RFDAT( ISPC,IRX ) = 0.0
         END DO
103   CONTINUE
      DO 105 IRX = 1, MAX3BODIES
         NRXWM( IRX )   = 0
         NRXWW( IRX )   = 0
         NRXWO2( IRX )  = 0
         NRXWN2( IRX )  = 0
         NRXWCH4( IRX ) = 0
         NRXWH2( IRX )  = 0
105   CONTINUE
      KTN1 = 0
      KTN2 = 0
      KTN3 = 0
      KTN4 = 0
      KTN5 = 0
      KTN6 = 0
      KTN7 = 0
!     KCNV = 0
      NWM  = 0
      NWW  = 0
      NWO2 = 0
      NWN2 = 0
      NWCH4 = 0
      NWH2 = 0
      DO ISPC = 1, MAXSPEC
         SPCLIS( ISPC ) = ' '
         SPC1RX( ISPC ) = 0
      END DO
      IP = 0
      NPHOTAB = 0
      DO ISPC = 1, MAXPHOTRXNS
         IPH( ISPC,1 ) = 0
         IPH( ISPC,2 ) = 0
         IPH( ISPC,3 ) = 0
         PHOTAB( ISPC ) = ' '
      END DO

      NSPECIAL     = 0
      NSPECIAL_RXN = 0
      NUSING_SPECIAL = 0
      DO ISPC = 1, MAXSPECRXNS
         ISPECIAL( ISPC,1 ) = 0
         ISPECIAL( ISPC,2 ) = 0
         SPECIAL( ISPC )    = ' '
         NKC_TERMS( ISPC )  = 0
         KC_COEFFS( ISPC,  1:MAXSPECTERMS) = 0.0
         KC_TERMS(  ISPC,  1:MAXSPECTERMS, 1) = ' '
         KC_TERMS(  ISPC,  1:MAXSPECTERMS, 2) = ' '
         INDEX_KTERM(MAXSPECRXNS,  1:MAXSPECTERMS) = 0
         INDEX_CTERM(MAXSPECRXNS,  1:MAXSPECTERMS) = 0
         N_OPERATORS( ISPC )  = 0
         OPERATORS(   ISPC, 1:MAXSPECTERMS)  = 0
         OPERATOR_COEFFS( ISPC, 1:MAXSPECTERMS) = 0.0
      END DO

      IUSING_SPECIAL = 0
      USING_SPECIAL  = ' '

      SS_RCT_COEF  = 0                 ! Array initialization
      SS_PRD_COEF  = 0.0               ! Array initialization
      SS_RCT_IND = 0                   ! Array initialization

      ALLOCATE( INDEX_FIXED_SPECIES( MAXRXNUM, MAXRCTNTS ) )
      INDEX_FIXED_SPECIES = 0
      NRXN_FIXED_SPECIES  = 0
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Open mechanism input file and get the first non-comment line
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      NS = 0
      NXX = 0
      MXPRD = 0
      IMECH = JUNIT()
      PRINT*,'IMECH = ',IMECH 
C symbolic link locates "MECHNAME"; setenv requires INQUIRE:
!     OPEN ( UNIT = IMECH, FILE = MECHNAME, STATUS = 'UNKNOWN' )
      CALL NAMEVAL ( MECHNAME, EQNAME_MECH )
      OPEN ( UNIT = IMECH, FILE = EQNAME_MECH, STATUS = 'UNKNOWN' )
!Open output file for conversion to KPP Equations Format
!      KPPEQN_UNIT = JUNIT()
      PRINT*,'KPPEQN_UNIT = ',KPPEQN_UNIT
      CALL NAMEVAL ( EQNS_KPP_FILE, EQN_MECH_KPP )
      CALL NAMEVAL ( SPCS_KPP_FILE, SPC_MECH_KPP )
      OPEN ( UNIT = KPPEQN_UNIT, FILE = EQN_MECH_KPP, STATUS = 'UNKNOWN' )
!
      CALL RDLINE ( IMECH, INBUF, LPOINT, IEOL )
      CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
      WORD( 1:4 ) = '    '
      IF ( CHR .EQ. 'R' .OR. CHR .EQ. 'r' )
     &   WORD( 1:4 ) = INBUF( LPOINT:LPOINT+3 )
      IF ( WORD( 1:4 ) .NE. 'REAC' ) THEN
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C The first string is the mechanism descriptive name 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         DESCRP_MECH = INBUF( LPOINT:LPOINT+31 )
         CALL RDLINE ( IMECH, INBUF, LPOINT, IEOL )
         CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
         CALL GETWORD ( IMECH, INBUF, LPOINT, IEOL, CHR, WORD )
      ELSE
         DESCRP_MECH = '00000000'
         CALL GETWORD ( IMECH, INBUF, LPOINT, IEOL, CHR, WORD )
!        CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
      END IF
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Read Special Block for reaction coefficients
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         IF ( WORD( 1:4 ) .EQ. 'SPEC' ) THEN
198         CALL RDLINE ( IMECH, INBUF, LPOINT, IEOL )

            CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
            IF( CHR .EQ. 'R')THEN
                CALL GETWORD ( IMECH, INBUF, LPOINT, IEOL, CHR, WORD )

                IPR = INDEX1 ( WORD, NSPECIAL, SPECIAL )
                NSPECIAL = NSPECIAL + 1
                IF(IPR .EQ. 0)THEN
                   SPECIAL(NSPECIAL) = WORD
                ELSE
                   WRITE( LUNOUT, 1993 ) 
                   STOP ' *** CHEMMECH ERROR ***'
                ENDIF
C brake down expression for special rate coefficients
                IF ( CHR .EQ. '=' )THEN
                    CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
199                 CALL GET_OPERATOR ( IMECH, INBUF, LPOINT, IEOL, CHR, WORD,
     &                          NSPECIAL, SPECIAL, NKC_TERMS, KC_TERMS, 
     &                          KC_COEFFS, N_OPERATORS, OPERATORS, 
     &                          OPERATOR_COEFFS  )
                    IF(CHR .EQ. ';')THEN
                       GO TO 198
                    ELSE
C                       CALL RDLINE ( IMECH, INBUF, LPOINT, IEOL ) 
C                       CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR)
                       GO TO 199
                    ENDIF
                ELSE
                    WRITE( LUNOUT, 1994 ) INBUF( 1:IEOL )
                    STOP ' *** CHEMMECH ERROR ***'
                ENDIF
            ELSEIF( CHR .EQ. 'E' .OR. CHR .EQ. 'e' )THEN
                END = INBUF( LPOINT:LPOINT+2 )
                IF( END .NE. 'END' .AND. END .NE. 'end' )GO TO 198
            ENDIF
         ELSE

           GO TO 210

         ENDIF

         CALL RDLINE ( IMECH, INBUF, LPOINT, IEOL )
         CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
         CALL GETWORD ( IMECH, INBUF, LPOINT, IEOL, CHR, WORD )


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Read block to get steady-state species
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

210      IF ( WORD( 1:4 ) .EQ. 'STEA' ) THEN
           
211         CALL RDLINE( IMECH, INBUF, LPOINT, IEOL )
            CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
            CALL GETWORD ( IMECH, INBUF, LPOINT, IEOL, CHR, WORD )

            IF( WORD( 1 : 3 ) .EQ. 'END' .OR. WORD( 1 : 3 ) .EQ. 'end' ) THEN

               IF( N_SS_SPC .GT. 0)THEN
                   WRITE( LUNOUT, 3001 )
2999               FORMAT( /5X, 'ERROR: Number of Steady State Species> 0, this utility ',
     &                     /5X, 'is not available in this version of CHEMMECH')
                   STOP
               ENDIF

               WRITE( LUNOUT, 3001 )
3001           FORMAT( /5X, 'The following species will be put in steady-state:' )
               DO ISPC = 1, N_SS_SPC
                  WRITE( LUNOUT, 3002 ) SS_SPC( ISPC )
3002              FORMAT( 10X, A )
               ENDDO
               WRITE( LUNOUT, 3003 )
3003           FORMAT( 1X )

               CALL RDLINE ( IMECH, INBUF, LPOINT, IEOL )
               CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
               CALL GETWORD ( IMECH, INBUF, LPOINT, IEOL, CHR, WORD )
               GO TO 215
            ELSE
               N_SS_SPC = N_SS_SPC + 1
               IF( N_SS_SPC .GT. MAXNLIST ) THEN
                   WRITE( LUNOUT, 4001 )
                   STOP ' *** CHEMMECH ERROR ***'
               ENDIF
               SS_SPC( N_SS_SPC ) = WORD
               GO TO 211
            ENDIF

         ENDIF
 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Read block to get species to be dropped from mechanism
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
215      IF ( WORD( 1:4 ) .EQ. 'ELIM' ) THEN
           
216         CALL RDLINE( IMECH, INBUF, LPOINT, IEOL )
            CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
            CALL GETWORD ( IMECH, INBUF, LPOINT, IEOL, CHR, WORD )

            IF( WORD( 1 : 3 ) .EQ. 'END' .OR. WORD( 1 : 3 ) .EQ. 'end' ) THEN

               WRITE( LUNOUT, 3004 )
3004           FORMAT( /5X, 'The following species will be eliminated from the mechanism:' )
               DO ISPC = 1, N_DROP_SPC
                  WRITE( LUNOUT, 3002 ) DROP_SPC( ISPC )
               ENDDO
               WRITE( LUNOUT, 3003 )
            
               CALL RDLINE ( IMECH, INBUF, LPOINT, IEOL )
               CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
               CALL GETWORD ( IMECH, INBUF, LPOINT, IEOL, CHR, WORD )
               GO TO 224
            ELSE
               N_DROP_SPC = N_DROP_SPC + 1
               IF( N_DROP_SPC .GT. MAXNLIST ) THEN
                   WRITE( LUNOUT, 4002 )
                   STOP ' *** CHEMMECH ERROR ***'
               ENDIF

               DROP_SPC( N_DROP_SPC ) = WORD
               GO TO 216
            ENDIF

         ENDIF
           
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Check that no species is in both the ELIMNATE and STEADY-STATE lists
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
224     IF( N_SS_SPC .GT. 0 .AND. N_DROP_SPC .GT. 0 ) THEN

           LERROR = .FALSE.
           DO ISPC = 1, N_SS_SPC
              IF( INDEX1( SS_SPC( ISPC ), N_DROP_SPC, DROP_SPC ) .NE. 0 ) THEN
                 IF( .NOT. LERROR ) THEN
                     WRITE( LUNOUT, 3010 )
                     LERROR = .TRUE.
                 ENDIF
                 WRITE( LUNOUT, 3011 ) SS_SPC( ISPC )
               ENDIF
           ENDDO
           IF( LERROR ) STOP ' *** CHEMMECH ERROR ***'
                         
        ENDIF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Make sure this word is REAC and then check for ppm or cm units
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
225      IF ( WORD( 1:4 ) .EQ. 'REAC' ) THEN
         IF ( CHR .EQ. '[' ) THEN
            CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
            CALL GETWORD ( IMECH, INBUF, LPOINT, IEOL, CHR, WORD )
            IF ( WORD( 1:2 ) .EQ. 'PP' .OR. WORD( 1:2 ) .EQ. 'pp' ) THEN
               KUNITS = 1
            ELSE IF ( WORD( 1:2 ) .EQ. 'CM' .OR. WORD( 1:2 ) .EQ. 'cm' ) THEN
               KUNITS = 2
            ELSE
               WRITE( LUNOUT, 2003 ) INBUF
               STOP ' *** CHEMMECH ERROR ***'
            END IF
            IF ( CHR .NE. ']' ) THEN
               WRITE( LUNOUT, 2005 ) INBUF
               STOP ' *** CHEMMECH ERROR ***'
            END IF
            CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
         END IF   ! CHR .EQ. '['
      ELSE
         WRITE( LUNOUT, 2007 ) INBUF
         STOP ' *** CHEMMECH ERROR ***'
      END IF      ! word .EQ. 'REAC'
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Make sure an equal sign is present processing any reactions 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( CHR .EQ. '=' ) THEN
         CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
      ELSE
         WRITE( LUNOUT, 2009 ) INBUF
         STOP ' *** CHEMMECH ERROR ***'
      END IF
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Start of reaction processing
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
201   CONTINUE
      NXX = NXX + 1
      IF ( NXX .GT. MAXRXNUM ) THEN
         WRITE( LUNOUT, 2011 ) INBUF
         STOP ' *** CHEMMECH ERROR ***'
      END IF 
      IF ( CHR .EQ. '<' ) THEN   ! label for this reaction
         CALL GETLABEL ( IMECH, INBUF, LPOINT, IEOL, CHR, LABEL( NXX,1 ) )
      END IF
      ICOL = 0
      IORDER( NXX ) = 0
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Get the reactants
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO 301 ISPC = 1, 3
         CALL GETRCTNT ( IMECH, INBUF, IEOL, LPOINT, CHR, WORD,
     &                   NXX, NS, SPCLIS, SPC1RX,
     &                   NWM, NRXWM, NWW, NRXWW,
     &                   NWO2, NRXWO2, NWN2, NRXWN2,
     &                   NWCH4, NRXWCH4, NWH2, NRXWH2,
     &                   ICOL, IORDER, IRXBITS, IRR,
     &                   LABEL, N_DROP_SPC, DROP_SPC,
     &                   N_SS_SPC, SS_SPC, SS_RCT_COEF )
         IF ( CHR .EQ. '+' ) THEN
            CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
         ELSE
            GO TO 303
         END IF
301   CONTINUE
303   CONTINUE
      NREACT( NXX ) = ICOL


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Check for equal sign after all reactants read
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( CHR .EQ. '=' ) THEN
         CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
         WRITE(KPPEQN_UNIT, '(A)', ADVANCE = 'NO' )'='
      ELSE
         WRITE( LUNOUT, 2013 ) INBUF( 1:IEOL )
         STOP ' *** CHEMMECH ERROR ***'
      END IF
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Get the products
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ICOL = 3
      IF ( CHR .NE. '#' .AND. CHR .NE. '%' ) THEN
401      CONTINUE
         CALL GETPRDCT ( IMECH, INBUF, LPOINT, IEOL, CHR, WORD,
     &                   NXX, NS, SPCLIS, SPC1RX,
     &                   ICOL, IRXBITS, IRR, SC,
     &                   N_DROP_SPC, DROP_SPC,
     &                   N_SS_SPC, SS_SPC, SS_PRD_COEF )
!        IF ( CHR .EQ. '+' .OR. CHR .EQ. '-' ) THEN
!           CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
         IF ( CHR .NE. '#' .AND. CHR .NE. '%' ) THEN
            GO TO 401
         END IF
      END IF
      ICOL = ICOL - 3
      NPRDCT( NXX ) = ICOL
      IF( NPRDCT( NXX ) .LT. 1)KPP_DUMMY = .TRUE.
      IF( NPRDCT( NXX ) .LT. 1 )THEN
          WRITE(KPPEQN_UNIT,'(A)', ADVANCE = 'NO')' DUMMY   : '
      ELSE
          WRITE(KPPEQN_UNIT,'(A)', ADVANCE = 'NO')' : '
      END IF
      MXPRD = MAX ( MXPRD, ICOL )
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Check for start of rate constant after all products read
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc      
!     IF ( CHR .NE. '#' .AND. CHR .NE. '%' ) THEN
!        WRITE( LUNOUT, 2013 ) INBUF( 1:IEOL )
!        STOP ' *** CHEMMECH ERROR ***'
!     END IF
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Get rate constants and check for end of reaction symbol
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       CALL GETRATE ( IMECH, INBUF, LPOINT, IEOL, CHR,
     &               NXX, LABEL,
     &               KTYPE, IRXBITS, NFALLOFF, IRRFALL,
     &               IPH, IP, NPHOTAB, PHOTAB,
     &               ISPECIAL, NSPECIAL_RXN, NSPECIAL, SPECIAL,
     &               KTN1, KTN2, KTN3, KTN4, KTN5, KTN6, KTN7, 
     &               KRX1, KRX2, KRX3, KRX4, KRX5, KRX6, KRX7, 
     &               RTDAT, RFDAT )
      IF ( CHR .NE. ';' ) THEN
         WRITE( LUNOUT, 2015 ) INBUF
         STOP ' *** CHEMMECH ERROR ***'
      END IF
      WRITE(KPPEQN_UNIT,'(A)' )' ;'
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Check for end of mechanism; if not, go back to 201 and get the
C next reaction
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
      END = '   '
      IF ( CHR .EQ. 'E' .OR. CHR .EQ. 'e' ) END = INBUF( LPOINT:LPOINT+2 )
      IF ( END .NE. 'END' .AND. END .NE. 'end' ) GO TO 201
      NR = NXX
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Get mechanism constant values for NRXWM, NRXWO2, NRXWN2, NRXWCH4, and NRXWH2 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CVAL = 0.0D0   ! array
      NC = 0
      CALL RDLINE ( IMECH, INBUF, LPOINT, IEOL )
      IF ( IEOL .GE. 0 ) THEN   ! not end of mechanism
         WORD( 1:9 ) = INBUF( IEOL-8:IEOL )
         IF ( WORD( 1:9 ) .EQ. 'CONSTANTS' .OR.
     &        WORD( 1:9 ) .EQ. 'constants' .OR.
     &        WORD( 1:9 ) .EQ. 'Constants' ) THEN
         CALL RDLINE ( IMECH, INBUF, LPOINT, IEOL )
         CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
421         CONTINUE
            NC = NC + 1
            IF ( CHR .EQ. '<' ) THEN   ! label for this constant
               CALL GETLABEL ( IMECH, INBUF, LPOINT, IEOL, CHR, CLABEL )
            END IF
            CALL GETWORD ( IMECH, INBUF, LPOINT, IEOL, CHR, CLABEL )
            IF ( CHR .EQ. '=' ) THEN
               CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
               CALL GETREAL ( IMECH, INBUF, LPOINT, IEOL, CHR, CONSTVAL )
            END IF
            IPR = INDEX1 ( CLABEL, MAXCONSTS, NAMCONSTS )
            CVAL( IPR ) = CONSTVAL
            END = '   '
            IF ( CHR .EQ. 'E' .OR. CHR .EQ. 'e' ) END = INBUF( LPOINT:LPOINT+2 )
            IF ( END .NE. 'END' .AND. END .NE. 'end' ) GO TO 421
         END IF
      END IF   ! not end of mechanism

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Resolve all reactions label references
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      WRITE( LUNOUT, * ) ' '
      DO 501 IRX = 1, NR
         IF ( LABEL( IRX,2 ) .NE. '>>>>>>>>>>>>>>>>' ) THEN
C search all rx's for LABEL(,1) match ...
            DO NXX = 1, NR
               IF ( LABEL( NXX,1 ) .EQ. LABEL( IRX,2 ) ) THEN
Could be a linear dependency photolysis reaction ...
                  IF ( KTYPE( IRX ) .EQ. 0 ) THEN
                     DO IPR = 1, IP
                        IF ( IPH( IPR,3 ) .EQ. 0 ) THEN
                           IPH( IPR,2 ) = NXX
                           WRITE( LUNOUT, 1003 ) IRX, IPH( IPR,2 ), RTDAT( 1,IRX ) 
1003                       FORMAT(  3X, 'Reaction', I4,
     &                              1X, 'is proportional to photolysis',
     &                              1X, 'reaction', I4, ','
     &                            /T51, 'scaled by:', 1PG13.5 )
                           GO TO 501
                        END IF
                     END DO
                     WRITE( LUNOUT, 2017 ) IRX
                     STOP ' *** CHEMMECH ERROR ***'
                  END IF
Could be a reverse equilibrium reaction ...
                  IF ( KTYPE( IRX ) .EQ. 5 ) THEN
                     RTDAT( 3,IRX ) = FLOAT ( NXX )
                     WRITE( LUNOUT, 1005 ) IRX, NXX
1005                 FORMAT( 3X, 'Reaction', I4,
     &                       1X, 'is a reverse equilibrium reaction:',
     &                       1X, 'The forward reaction is', I4 )
                     GO TO 501
Could a linear dependency reaction ...
                  ELSE IF ( KTYPE( IRX ) .EQ. 6 ) THEN
                     RTDAT( 2,IRX ) = FLOAT ( NXX )
                     WRITE( LUNOUT, 1007 ) IRX, NXX, RTDAT( 1,IRX )
1007                 FORMAT( 3X, 'Reaction', I4,
     &                       1X, 'is proportional to reaction', I4, ',',
     &                       2X, 'scaled by:', 1PG13.5 )
                     GO TO 501
                  ELSE
                     WRITE( LUNOUT, 2019 ) IRX
                     STOP ' *** CHEMMECH ERROR ***'
                  END IF
               END IF  ! LABEL(NXX,1) .EQ. LABEL( IRX,2)
            END DO
C if we get here, LABEL(,1) match not found
            WRITE( LUNOUT, 2021 ) IRX
            STOP ' *** CHEMMECH ERROR ***'
         END IF  ! LABEL .NE.  ...
501   CONTINUE

C Error-check phot tables and report to log
      WRITE( LUNOUT, * ) ' '
      IPHOTAB = 0
      DO IPR = 1, IP
         IF ( IPH( IPR,3 ) .NE. 0 ) THEN ! table
            IPHOTAB = IPHOTAB + 1
            IRX = IPH( IPR,1 )
            NXX = IPH( IPR,2 )
            WRITE( LUNOUT, 1009 ) IRX, PHOTAB( NXX ), RTDAT( 1,IRX ) 
1009        FORMAT(  3X, 'Reaction', I4,
     &               1X, 'uses photolysis table: ', A16,
     &               1X, 'scaled by:', 1PG13.5 )
         END IF
      END DO
      WRITE( LUNOUT, 1011 ) IPHOTAB, NPHOTAB
1011  FORMAT(/ 5X, 'There are', I3,
     &         1X, 'photolysis table references out of', I3,
     &         1X, 'tables' / )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Determine pointers to rate coefficients and species listed in the
C KC_TERMS array
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       DO IRX = 1, NSPECIAL
          DO IPR = 1, NKC_TERMS(IRX)
             NR_LOOP : DO ISPC = 1, NR
                IF( KC_TERMS(IRX, IPR, 1) .EQ. LABEL( ISPC, 1))THEN
                    INDEX_KTERM(IRX, IPR ) = ISPC
                    EXIT NR_LOOP
                ENDIF
             ENDDO NR_LOOP
             IF( INDEX_KTERM(IRX, IPR ) .EQ. 0)THEN
               WRITE( LUNOUT, 2031)SPECIAL( IRX ),KC_TERMS(IRX, IPR, 1)
               STOP ' *** CHEMMECH ERROR ***'
             ENDIF

             IF( KC_TERMS(IRX, IPR, 2) .EQ. 'FIRST_ORDER' )THEN
                 IF( IORDER( ISPC ) .NE. 1 )THEN
                    WRITE(LUNOUT, 2032)SPECIAL(IRX),LABEL(ISPC,1)
                    STOP ' *** CHEMMECH ERROR ***'
                 ENDIF
                 CYCLE
             ENDIF

             IF( IORDER( ISPC ) .EQ. 2 )THEN
                 ISPC = INDEX1( KC_TERMS(IRX, IPR, 2), NS, SPCLIS)
                 IF( ISPC .GT. 0 )THEN
                    INDEX_CTERM(IRX, IPR ) = ISPC
                 ELSE
                    WRITE( LUNOUT, 2033)SPECIAL( IRX ),KC_TERMS(IRX, IPR, 2)
                    STOP ' *** CHEMMECH ERROR ***'
                 ENDIF
             ELSE
                 WRITE(LUNOUT, 2034)SPECIAL(IRX),KC_TERMS(IRX, IPR, 1)
                 STOP ' *** CHEMMECH ERROR ***'
             ENDIF
          ENDDO
       ENDDO 

       DO IRX = 1, NSPECIAL_RXN
          ISPC = ISPECIAL( IRX, 1)
          DO IPR = 1, NREACT( ISPC )
             I = IRR(ISPC, IPR)
             IDX = INDEX1(SPCLIS(I),NUSING_SPECIAL,USING_SPECIAL)
        
             IF(IDX .EQ. 0)THEN
                NUSING_SPECIAL = NUSING_SPECIAL + 1
                USING_SPECIAL(NUSING_SPECIAL) = SPCLIS(I)
                IUSING_SPECIAL(NUSING_SPECIAL) = I
c             print*,ispc,IUSING_SPECIAL(NUSING_SPECIAL),
c     &                    USING_SPECIAL(NUSING_SPECIAL)

             ENDIF 

          ENDDO
       ENDDO

       IF( N_SS_SPC .GT. 0 ) CALL CHECK_SS_SPC( LUNOUT, N_SS_SPC, SS_SPC, NS,
     &                       SPCLIS, IRR, NR, LABEL, SS_RCT_COEF, SS_PRD_COEF,
     &                       SS1RX )


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Create the Fortran INCLUDE files for the reactions data and the
C interim species include file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL WREXTS ( EQNAME_MECH,
     &              DESCRP_MECH,
     &              NS, SPCLIS, SPC1RX,
     &              NR,
     &              MXPRD,
     &              IRR,
     &              RTDAT,
     &              SC,
     &              NPRDCT,
     &              NREACT,
     &              KUNITS,
     &              KTYPE,
     &              IRXBITS,
     &              IORDER,
     &              KTN1, KRX1,
     &              KTN2, KRX2,
     &              KTN3, KRX3,
     &              KTN4, KRX4,
     &              KTN5, KRX5,
     &              KTN6, KRX6,
     &              KTN7, KRX7,
     &              NFALLOFF,
     &              IRRFALL,
     &              RFDAT,
     &              NWM,   NRXWM,
     &              NWW,   NRXWW,
     &              NWO2,  NRXWO2,
     &              NWN2,  NRXWN2,
     &              NWCH4, NRXWCH4,
     &              NWH2,  NRXWH2,
     &              LABEL( 1,1 ),
     &              IP,
     &              IPH,
     &              NPHOTAB,
     &              PHOTAB,
     &              ISPECIAL, 
     &              NSPECIAL_RXN, 
     &              NSPECIAL,
     &              SPECIAL,
     &              NAMCONSTS,
     &              CVAL, 
     &              N_SS_SPC,
     &              SS_SPC,
     &              SS1RX )
      

      CALL WRSPECIAL_EXT( NSPECIAL, NSPECIAL_RXN, SPECIAL,
     &                     ISPECIAL, INDEX_KTERM, INDEX_CTERM,
     &                     KC_COEFFS, OPERATORS, OPERATOR_COEFFS)

      IF( N_SS_SPC .GT. 0 ) CALL GET_SS_DATA( LUNOUT, 
     &                                        NR, 
     &                                        NREACT,
     &                                        N_SS_SPC, 
     &                                        SS_SPC, 
     &                                        SS_RCT_COEF, 
     &                                        SS_PRD_COEF, 
     &                                        MAX_SS_LOSS, 
     &                                        MAX_SS_PROD, 
     &                                        N_LOSS_RXNS, 
     &                                        N_PROD_RXNS, 
     &                                        SS_LOSS_RXNS, 
     &                                        SS_PROD_RXNS, 
     &                                        SS_PROD_COEF,
     &                                        SS_RCT_IND )


      CALL WRSS_EXT( NR,
     &               N_SS_SPC,
     &               SS_SPC,
     &               MAX_SS_LOSS, 
     &               MAX_SS_PROD, 
     &               N_LOSS_RXNS, 
     &               N_PROD_RXNS, 
     &               SS_LOSS_RXNS, 
     &               SS_PROD_RXNS, 
     &               SS_PROD_COEF,
     &               SS_RCT_IND )

      CLOSE( KPPEQN_UNIT )
! write out reactions strings to determine KPP information
       DO NXX = 1, NR
         DO IREACT = 1, NREACT( NXX )
            ISPC = IRR( NXX, IREACT )
               IF( IREACT .LE. 1 )THEN
                  REACTION_STR( NXX ) =  TRIM(SPCLIS( ISPC )) // ' '
               ELSE
                  REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) //  ' + ' // TRIM(SPCLIS( ISPC )) // ' '
               END IF
         END DO
         DO I = 1, MAXRCTNTS
         IF( INDEX_FIXED_SPECIES( NXX, I ) .GT. 0 .AND. INDEX_FIXED_SPECIES( NXX, I ) .LT. 7 )THEN
              ISPC = INDEX_FIXED_SPECIES( NXX, I )
              REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX ))
     &                           //  ' + ' //  TRIM( FIXED_SPECIES( ISPC ) ) // ' '
!              REACTION_STR( NXX ) = ' + ' //  TRIM( FIXED_SPECIES( ISPC ) ) // ' '
         ELSE 
              IF( INDEX_FIXED_SPECIES( NXX, I ) .GE. 7 )THEN
                  WRITE(*,*)'WARNING: INDEX_FIXED_SPECIES( ', NXX,',', I, ') = ',INDEX_FIXED_SPECIES( NXX, I )
              END IF
         END IF         
         END DO
         REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) //  ' = '
! write out products
         DO IPRODUCT = 1, NPRDCT( NXX )
            ISPC = IRR( NXX, IPRODUCT + 3 )
            IF ( ABS( SC( NXX,IPRODUCT ) ) .NE. 1.0 ) THEN
               IF ( SC( NXX,IPRODUCT ) .LT. 0 ) THEN
                  WRITE(COEFF_STR,'(A,F8.5)')' - ',ABS(SC( NXX,IPRODUCT ))
                  REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) // TRIM(COEFF_STR) 
     &                        // ' * '  // TRIM(SPCLIS( ISPC ))
               ELSE
                  WRITE(COEFF_STR,'(F8.5)')SC( NXX,IPRODUCT )
                  IF( IPRODUCT .EQ. 1 )THEN
                     REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) // ' ' // TRIM(COEFF_STR) 
     &                           // ' * '  // TRIM(SPCLIS( ISPC ))
                  ELSE
                     REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) // ' + ' // TRIM(COEFF_STR) 
     &                           // ' * '  // TRIM(SPCLIS( ISPC ))
                  END IF
               END IF
            ELSE 
               IF ( SC( NXX,IPRODUCT ) .LT. 0.0 ) THEN
                   REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) //  ' - ' // TRIM(SPCLIS( ISPC ))
               ELSE
                  IF( IPRODUCT .EQ. 1 )THEN
                    REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) // ' ' //  TRIM(SPCLIS( ISPC ))
                  ELSE
                    REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) //  ' + ' // TRIM(SPCLIS( ISPC ))
                  END IF
               END IF
            END IF
         END DO
! determine conditions whether reaction string needs a non-zero multiple of dummy
! variable added to reaction 
         DUMMY_COEF( NXX ) = INDEXES(REACTION_STR( NXX ),(NXX-1),REACTION_STR )
         IF( NPRDCT( NXX ) .LT. 1 )DUMMY_COEF( NXX ) = DUMMY_COEF( NXX ) + 1 
         IF(  KPP_DUMMY )KPP_DUMMY = .TRUE.
         
       END DO
! create species list
      OPEN ( UNIT = KPPEQN_UNIT, FILE = SPC_MECH_KPP, STATUS = 'UNKNOWN' )
      WRITE(KPPEQN_UNIT,'(3A)')'#INCLUDE atoms'
      WRITE(KPPEQN_UNIT,'(3A)')'#DEFVAR'
      DO ISPC = 1, NS
         WRITE(KPPEQN_UNIT,6001)SPCLIS( ISPC )
      END DO
      WRITE(KPPEQN_UNIT,'(3A)')'#DEFFIX'
      DO ISPC = 1, NFIXED_SPECIES
         WRITE(KPPEQN_UNIT,6001)FIXED_SPECIES( ISPC )
      END DO
      IF( KPP_DUMMY )WRITE(KPPEQN_UNIT,6001)'DUMMY'
6001  FORMAT(8X,A18,' =  IGNORE ;')      
      WRITE(KPPEQN_UNIT,4555) ! '#INLINE F90_INIT'
      WRITE(KPPEQN_UNIT,4556)
      DO ISPC = 1, NFIXED_SPECIES
         IF( NRXN_FIXED_SPECIES( ISPC  ) .LT. 1 )CYCLE
         DO IPR = 1, MAXCONSTS
            IF( NAMCONSTS( IPR )( 5:LEN_TRIM(NAMCONSTS( IPR )) ) .EQ. FIXED_SPECIES( ISPC ) )THEN
                WORD = 'indf_' // TRIM( FIXED_SPECIES( ISPC ) ) 
                WRITE( KPPEQN_UNIT, 1309 ) WORD, TRIM( NAMCONSTS( IPR ))
            END IF
            IF( NAMCONSTS( IPR ) .EQ. 'ATM_AIR' .AND. FIXED_SPECIES( ISPC ) .EQ. 'M' )THEN
                WORD = 'indf_' // TRIM( FIXED_SPECIES( ISPC ) ) 
                WRITE( KPPEQN_UNIT, 1309 ) WORD, TRIM( NAMCONSTS( IPR ))
            END IF
         END DO
      END DO
1309  FORMAT('FIX( ', A16,' ) = ', A9,  ' * CAIR '  )
      WRITE(KPPEQN_UNIT,4504)
      CLOSE( KPPEQN_UNIT )
! create equations file      
      OPEN ( UNIT = KPPEQN_UNIT, FILE = EQN_MECH_KPP, STATUS = 'UNKNOWN' )
! define inline function for rate constant of type 10 fall off reactions 
      WRITE(KPPEQN_UNIT,4500)
! mechanism parameters
      WRITE(KPPEQN_UNIT,'(A)')'#INLINE F90_GLOBAL'
      WRITE(KPPEQN_UNIT,4501)TRIM( DESCRP_MECH ), NPHOTAB
! create pointers for unused fixed species
      DO IPR = 1, NFIXED_SPECIES
         IF( NRXN_FIXED_SPECIES( IPR  ) .LT. 1 )THEN
             WRITE(KPPEQN_UNIT,4603)'indf_' // TRIM( FIXED_SPECIES( IPR ) )
         END IF
      END DO
4603  FORMAT('INTEGER,  SAVE       :: ', A16,' = 0 ' )
4604  FORMAT('REAL(dp), PARAMETER  :: ', A16,' = ', 1PD12.4 )
! write inline data for constant species
      WRITE(KPPEQN_UNIT, 4714 )
      IF ( MAXVAL( CVAL ) .GT. 0.0D0 ) THEN
         DO IPR = 1, MAXCONSTS
            ISPC = INDEX1 ( TRIM( NAMCONSTS( IPR ) ), MAXCONSTS, NAMCONSTS )
            IF( ISPC .LT. 1 )CYCLE
            WRITE( KPPEQN_UNIT, 1310 )  NAMCONSTS( IPR ), REAL(CVAL( ISPC ), 8)
         END DO
1310     FORMAT('REAL(dp), PARAMETER ::', 1X, A16,' =', 1PD12.5  )
      END IF
! set up variables equal to the rate constant of type 10 fall off reactions      
      DO NXX = 1, NR
         IF( KTYPE( NXX ) .EQ. 10 )THEN
             WRITE(KPPEQN_UNIT, 4505)LABEL(NXX,1)
         END IF
      END DO       
! set up variables equal to the rate constant of type 11 reactions      
      WRITE(KPPEQN_UNIT,4749)
      IF( NSPECIAL .GT. 0 )THEN
         WRITE(KPPEQN_UNIT,4750)
         DO ISPC = 1, NSPECIAL
             WRITE(KPPEQN_UNIT, 4506)SPECIAL(ISPC)
         END DO
      ELSE
         WRITE(KPPEQN_UNIT,4751)
      END IF
! set up pointers and names for photolysis rate array
      WRITE(KPPEQN_UNIT,4502)
      DO IPR = 1, NPHOTAB
         WRITE(KPPEQN_UNIT,4503),PHOTAB(IPR),IPR
      END DO
      DO IPR = 1, NPHOTAB
         WRITE(KPPEQN_UNIT,4557)IPR, PHOTAB(IPR)
      END DO
      WRITE(KPPEQN_UNIT,4504)
!      WRITE(KPPEQN_UNIT,'(A)')'#INLINE F90_UTIL'
!      WRITE(KPPEQN_UNIT,4504)
! write formulas for specail rate expressions
      WRITE(KPPEQN_UNIT,'(A)')'#INLINE F90_UTIL'
      ISPC = INDEX(EQN_MECH_KPP,'/mech', BACK= .TRUE.) + 1
      NXX  = INDEX(EQN_MECH_KPP,'.eqn', BACK= .TRUE.)  - 1
      PHRASE = EQN_MECH_KPP(ISPC:NXX)
!      PHRASE =  'mech' // DESCRP_MECH
!      CALL CONVERT_CASE ( PHRASE, .FALSE. )
      WRITE(KPPEQN_UNIT,95050)TRIM( PHRASE )
      DO NXX = 1, NSPECIAL
         WRITE(KPPEQN_UNIT,'(5X, A16)', ADVANCE = 'NO' )SPECIAL( NXX ) 
         FIRST_TERM = .TRUE.
! first write standard rate constants time concentrations
         DO IREACT = 1, MAXSPECTERMS
             IRX  = INDEX_KTERM( NXX, IREACT )
             IF( IRX .LT. 1 )CYCLE
             IF( FIRST_TERM )THEN
                PHRASE = ' = '
                FIRST_TERM = .FALSE.
                IF(KC_COEFFS( NXX, IREACT ) .LT. 0.0 )PHRASE = ' = ' // ' - '
             ELSE
                WRITE(KPPEQN_UNIT, 4711, ADVANCE = 'NO' )
                PHRASE = ' + '
                IF(KC_COEFFS( NXX, IREACT ) .LT. 0.0 )PHRASE = ' - '
             END IF
             IF( KC_COEFFS( NXX, IREACT ) .NE. 1.0 )THEN
                 WRITE(KPPEQN_UNIT, 4708, ADVANCE = 'NO')TRIM(PHRASE),
     &          REAL( ABS( KC_COEFFS( NXX, IREACT ) ), 8), IRX
             ELSE
                 WRITE(KPPEQN_UNIT, 4706, ADVANCE = 'NO')TRIM(PHRASE),IRX
             END IF
             ISPC = INDEX_CTERM( NXX, IREACT )
             IF( ISPC .LT. 1 )CYCLE
             PHRASE = ' * Y( ind_' // TRIM( SPCLIS( ISPC ) ) // ' ) '
             WRITE(KPPEQN_UNIT, 4709, ADVANCE = 'NO')TRIM( PHRASE )
!             IF( ISPC .LT. 1 )THEN
!                WRITE(KPPEQN_UNIT, 4708, ADVANCE = 'NO')TRIM(PHRASE),
!     &          REAL( ABS( KC_COEFFS( NXX, IREACT ) ), 8), IRX
!             ELSE
!                WRITE(KPPEQN_UNIT, 4711, ADVANCE = 'NO')TRIM(PHRASE),
!     &          REAL( ABS( KC_COEFFS( NXX, IREACT ) ), 8), IRX, TRIM( SPCLIS( ISPC ) )
!             END IF
         END DO
! next write defined operators         
         DO IREACT = 1, MAXSPECTERMS
            IDX = OPERATORS( NXX, IREACT )
            IF( IDX .LT. 1 )CYCLE
             IF( FIRST_TERM )THEN
                PHRASE = ' = '
                IF(OPERATOR_COEFFS( NXX, IREACT ) .LT. 0.0 )PHRASE = ' = ' // ' - '
                FIRST_TERM = .FALSE.
             ELSE
                WRITE(KPPEQN_UNIT, 4711, ADVANCE = 'NO' )
                PHRASE = ' + '
                IF(OPERATOR_COEFFS( NXX, IREACT ) .LT. 0.0 )PHRASE = ' - '
             END IF
             IF( OPERATOR_COEFFS( NXX, IREACT ) .NE. 1.0 )THEN
                 WRITE(KPPEQN_UNIT, 4710, ADVANCE = 'NO')TRIM(PHRASE),
     &           REAL( ABS( OPERATOR_COEFFS( NXX, IREACT ) ), 8), TRIM( SPECIAL( IDX ) )
             ELSE
                 WRITE(KPPEQN_UNIT, 4712, ADVANCE = 'NO')TRIM(PHRASE),TRIM( SPECIAL( IDX ) )
             END IF
         END DO 
         WRITE(KPPEQN_UNIT, * )' '
      END DO
      DO NXX = 1, NSPECIAL_RXN 
! define rate constants interms of special rate operators
         WRITE(KPPEQN_UNIT,95070)ISPECIAL( NXX,1 ),SPECIAL( ISPECIAL( NXX,2 ) ),
     &   TRIM( LABEL( ISPECIAL( NXX,1 ),1 ) )
      END DO
95070 FORMAT(5X,'RCONST(',I4,' ) = ',A16,' ! reaction: ',A)
      WRITE(KPPEQN_UNIT,95060)
      WRITE(KPPEQN_UNIT,4504)
      WRITE(KPPEQN_UNIT,'(A)')'#INLINE F90_RCONST'
! initialize special rate expressions in Update_Rconst subroutine
      DO NXX = 1, NSPECIAL
         WRITE(KPPEQN_UNIT,95100)SPECIAL( NXX ) 
      END DO
!    
      WRITE(KPPEQN_UNIT, 4713)
! write functions define CMAQ specific fall off rate constant
      DO NXX = 1, NR
         IF( KTYPE( NXX ) .EQ. 10 )THEN
             DO IDX = 1, NFALLOFF
                IF( IRRFALL( IDX ) .EQ. NXX )EXIT
             END DO
             WRITE(KPPEQN_UNIT, 4507, ADVANCE = 'NO')LABEL(NXX,1),' = ' 
             WRITE(KPPEQN_UNIT, 5010)RTDAT(1,NXX),RTDAT(3,NXX),RTDAT(2,NXX),
     &      RFDAT(1,IDX),RFDAT(3,IDX),RFDAT(2,IDX),RFDAT(5,IDX),RFDAT(4,IDX)
         END IF
      END DO       
      WRITE(KPPEQN_UNIT,4504)
! write equations for mechanism
      WRITE(KPPEQN_UNIT,'(3A)')'#EQNTAGS ON'
      WRITE(KPPEQN_UNIT,'(3A)')'#EQUATIONS'
      IF( KUNITS .EQ. 2 )THEN
          WRITE(KPPEQN_UNIT,'(3A)')'// All rate constants converted from  molec/cm3 to ppm'
          WRITE(KPPEQN_UNIT,'(3A)')'// and 1/sec to 1/min'
      ELSE
          WRITE(KPPEQN_UNIT,'(3A)')'// Only fall off rate constants converted from  molec/cm3 '
          WRITE(KPPEQN_UNIT,'(3A)')'// and 1/sec to 1/min'
          WRITE(KPPEQN_UNIT,'(3A)')'// Remainder use ppm and 1/min '
      END IF
      DO NXX = 1, NR
         WRITE(KPPEQN_UNIT,'(3A)', ADVANCE= 'NO')' <',TRIM(LABEL( NXX,1 )),'> '
         DO IREACT = 1, NREACT( NXX )
            ISPC = IRR( NXX, IREACT )
               IF( IREACT .LT. 2 )THEN
                  WRITE(KPPEQN_UNIT,'(A, A)', ADVANCE = 'NO')TRIM(SPCLIS( ISPC )),' '
                  ICOUNT = 1 + LEN( SPCLIS( ISPC ) )
               ELSE
                  WRITE(KPPEQN_UNIT,'(3A)', ADVANCE = 'NO')'+ ',TRIM(SPCLIS( ISPC )),' '
                  ICOUNT = 1 + LEN( SPCLIS( ISPC ) )
                  ICOUNT = 3 + LEN( SPCLIS( ISPC ) )                  
               END IF
         END DO
         DO I = 1, MAXRCTNTS
         IF( INDEX_FIXED_SPECIES( NXX, I ) .GT. 0 .AND. INDEX_FIXED_SPECIES( NXX, I ) .LT. 7 )THEN
              ISPC = INDEX_FIXED_SPECIES( NXX, I  )
              WRITE(KPPEQN_UNIT,'(3A)', ADVANCE = 'NO')'+ ',TRIM(FIXED_SPECIES( ISPC )),' '
              ICOUNT = 3 + LEN( FIXED_SPECIES( ISPC ) )
         ELSE 
              IF( INDEX_FIXED_SPECIES( NXX, I ) .GE. 7 )THEN
                  WRITE(*,*)'WARNING: INDEX_FIXED_SPECIES( ', NXX,',', I, ') = ',INDEX_FIXED_SPECIES( NXX, I )
              END IF
         END IF    
         END DO     
         WRITE(KPPEQN_UNIT, '(A)', ADVANCE = 'NO' )'='
! write out products
         DO IPRODUCT = 1, NPRDCT( NXX )
            ISPC = IRR( NXX, IPRODUCT + 3 )
            IF ( ABS( SC( NXX,IPRODUCT ) ) .NE. 1.0 ) THEN
               IF ( SC( NXX,IPRODUCT ) .LT. 0 ) THEN
                  WRITE(KPPEQN_UNIT,'(A,F8.5,3A)', ADVANCE = 'NO')
     &           '- ',ABS(SC( NXX,IPRODUCT )),'*',TRIM(SPCLIS( ISPC )),' '
                  ICOUNT = ICOUNT + 12 + LEN( SPCLIS( ISPC ) )
               ELSE
                  IF( IPRODUCT .EQ. 1 )THEN
                     WRITE(KPPEQN_UNIT,'(F8.5, 3A)', ADVANCE = 'NO')
     &               SC( NXX,IPRODUCT ),'*',TRIM(SPCLIS( ISPC )),' '
                     ICOUNT = ICOUNT + 10 + LEN( SPCLIS( ISPC ) )
                  ELSE
                     WRITE(KPPEQN_UNIT,'(A,F8.5,3A)', ADVANCE = 'NO')
     &               '+ ',SC( NXX,IPRODUCT ),'*',TRIM(SPCLIS( ISPC )),' '
                     ICOUNT = ICOUNT + 12 + LEN( SPCLIS( ISPC ) )
                  END IF
               END IF
            ELSE IF ( SC( NXX,IPRODUCT ) .LT. 0.0 ) THEN
               WRITE(KPPEQN_UNIT,'(3A)', ADVANCE = 'NO')
     &               '- ',TRIM(SPCLIS( ISPC )),' '
               ICOUNT = ICOUNT + 3 + LEN( SPCLIS( ISPC ) )
            ELSE
               IF( IPRODUCT .EQ. 1 )THEN
                  WRITE(KPPEQN_UNIT,'(3A)', ADVANCE = 'NO')
     &           ' ',TRIM(SPCLIS( ISPC )),' '
                  ICOUNT = ICOUNT + 2 + LEN( SPCLIS( ISPC ) )
               ELSE
                  WRITE(KPPEQN_UNIT,'(3A)', ADVANCE = 'NO')
     &             '+ ',TRIM(SPCLIS( ISPC )),' '
                  ICOUNT = ICOUNT + 3 + LEN( SPCLIS( ISPC ) )
               END IF
            END IF
            IF( ICOUNT .GT. 132 .AND. IPRODUCT .LT.  NPRDCT( NXX ) )THEN
                ICOUNT = 0
                WRITE(KPPEQN_UNIT, * )' '
                WRITE(KPPEQN_UNIT,'(A16)', ADVANCE = 'NO')' '
            END IF
         END DO 
! add dummy variable to reaction with no production or reaction that are identical
! to previous reactions but with different rate constants         
         IF( DUMMY_COEF( NXX ) .GT. 1 )THEN
             IF( NPRDCT( NXX ) .LT. 1 )THEN
                 WRITE(KPPEQN_UNIT,'(F8.5,A)', ADVANCE = 'NO')REAL(DUMMY_COEF(NXX)),'*DUMMY   : '
             ELSE
                 WRITE(KPPEQN_UNIT,'(A,F8.5,A)', ADVANCE = 'NO')' + ',REAL(DUMMY_COEF(NXX)),'*DUMMY   : '
             END IF
         ELSE IF( DUMMY_COEF( NXX ) .EQ. 1 )THEN
             IF( NPRDCT( NXX ) .LT. 1 )THEN
                 WRITE(KPPEQN_UNIT,'(A)', ADVANCE = 'NO')' DUMMY   : '
             ELSE!
                 WRITE(KPPEQN_UNIT,'(A)', ADVANCE = 'NO')' + DUMMY   : '
             END IF
         ELSE IF (DUMMY_COEF( NXX ) .EQ. 0 )THEN
             WRITE(KPPEQN_UNIT,'(A)', ADVANCE = 'NO')' : '
         END IF
         SELECT CASE( KTYPE( NXX ) )
          CASE( 0 )
             DO IPR = 1, IP
                IF ( IPH( IPR,1 ) .EQ. NXX )EXIT
             END DO
             IF ( IPH( IPR,3 ) .NE. 0 )THEN
                IDX = IPH( IPR, 2 )
                IF( RTDAT(1, NXX) .NE. 1.0 )THEN
                   WRITE(KPPEQN_UNIT,5000, ADVANCE = 'NO')REAL(RTDAT(1, NXX),8),TRIM( PHOTAB(IDX) )
                ELSE
                   WRITE(KPPEQN_UNIT,5001, ADVANCE = 'NO')TRIM( PHOTAB(IDX) )
                END IF
             ELSE IF( IPH( NXX,3 ) .NE. 0 )THEN
                IDX = IPH(IPH( NXX,2 ), 2)
                IF( RTDAT(1, NXX) .NE. 1.0 )THEN
                   WRITE(KPPEQN_UNIT,5000, ADVANCE = 'NO')REAL(RTDAT(1, NXX),8),TRIM( PHOTAB(IDX) )
                ELSE
                   WRITE(KPPEQN_UNIT,5001, ADVANCE = 'NO')TRIM( PHOTAB(IDX) )
                END IF
             END IF
          CASE( 1 )
             IF( KUNITS .EQ. 2 )CALL WRITE_RATE_CONVERT(KPPEQN_UNIT, IORDER(NXX))
             WRITE(KPPEQN_UNIT,'(1PD12.4)', ADVANCE = 'NO')(RTDAT(1, NXX), 8)
          CASE( 2 )
             IF( KUNITS .EQ. 2 )CALL WRITE_RATE_CONVERT(KPPEQN_UNIT, IORDER(NXX))
             WRITE(KPPEQN_UNIT,5002, ADVANCE = 'NO')RTDAT(1, NXX), RTDAT(2, NXX)
          CASE( 3 )
             IF( KUNITS .EQ. 2 )CALL WRITE_RATE_CONVERT(KPPEQN_UNIT, IORDER(NXX))
             WRITE(KPPEQN_UNIT,5003, ADVANCE = 'NO')RTDAT(1, NXX), RTDAT(3, NXX)
          CASE( 4 )
             IF( KUNITS .EQ. 2 )CALL WRITE_RATE_CONVERT(KPPEQN_UNIT, IORDER(NXX))
             WRITE(KPPEQN_UNIT,5004, ADVANCE = 'NO')RTDAT(1, NXX), RTDAT(3, NXX), RTDAT(2, NXX)
          CASE( 5 )
!             DO IDX = 1, KTN5
!                IF( KRX5( IDX ) .EQ. NXX )EXIT
!             END DO         
             IRX = INT( RTDAT( 3, NXX) )
!             IF( KUNITS .EQ. 2 )CALL WRITE_RATE_CONVERT(KPPEQN_UNIT, IORDER(NXX))
             WRITE(KPPEQN_UNIT,5005, ADVANCE = 'NO')IRX,RTDAT( 1, NXX ), RTDAT(2, NXX )
          CASE( 6 )
!             DO IDX = 1, KTN6
!                IF( KRX6( IDX ) .EQ. NXX )EXIT
!             END DO         
             IRX = INT( RTDAT( 2, NXX) )
!            IF( KUNITS .EQ. 2 )CALL WRITE_RATE_CONVERT(KPPEQN_UNIT, IORDER(NXX))
             IF( RTDAT( 1, NXX ) .NE. 1.0 )THEN
                 WRITE(KPPEQN_UNIT, 5006, ADVANCE = 'NO')REAL(RTDAT( 1, NXX ), 8), IRX
             ELSE
                 WRITE(KPPEQN_UNIT, 4706, ADVANCE = 'NO')' ', IRX
             END IF
          CASE( 7 )
             IF( RTDAT(2, NXX) .NE. 0.0 )THEN
                 WRITE(KPPEQN_UNIT,5014, ADVANCE = 'NO')REAL(RTDAT(1, NXX), 8),REAL(RTDAT(2, NXX), 8)
             ELSE
                 WRITE(KPPEQN_UNIT,5007, ADVANCE = 'NO')REAL(RTDAT(1, NXX), 8)
             END IF
          CASE( 8 )
             DO IDX = 1, NFALLOFF
                IF( IRRFALL( IDX ) .EQ. NXX )EXIT
             END DO
             CALL WRITE_RATE_CONVERT(KPPEQN_UNIT, IORDER(NXX))
             WRITE(KPPEQN_UNIT,5008, ADVANCE = 'NO')RTDAT(1,NXX),(1.0*RTDAT(2,NXX)),RTDAT(3,NXX),
     &      (1.0*RFDAT(1,IDX)),RFDAT(2,IDX),(1.0*RFDAT(3,IDX))
          CASE( 9 )
             DO IDX = 1, NFALLOFF
                IF( IRRFALL( IDX ) .EQ. NXX )EXIT
             END DO
             CALL WRITE_RATE_CONVERT(KPPEQN_UNIT, IORDER(NXX))
             WRITE(KPPEQN_UNIT,5009, ADVANCE = 'NO')RTDAT(1,NXX),RTDAT(2,NXX),RTDAT(3,NXX),
     &       1.0*RFDAT(1,IDX)
          CASE( 10 )
             DO IDX = 1, NFALLOFF
                IF( IRRFALL( IDX ) .EQ. NXX )EXIT
             END DO
             CALL WRITE_RATE_CONVERT(KPPEQN_UNIT, IORDER(NXX))
             WRITE(KPPEQN_UNIT, 4507, ADVANCE = 'NO')LABEL(NXX,1)
          CASE( 11 )
             DO IDX = 1, NSPECIAL_RXN
                IF( ISPECIAL( IDX, 1 ) .EQ. NXX )EXIT
             END DO
             I   = ISPECIAL( IDX, 1)
             IRX = ISPECIAL( IDX, 2)
             IF( RTDAT( 1, I) .NE. 1.0 )THEN
                WRITE(KPPEQN_UNIT,5011, ADVANCE = 'NO')REAL(RTDAT( 1, I),8), TRIM( SPECIAL( IRX ) )
             ELSE
                WRITE(KPPEQN_UNIT,5012, ADVANCE = 'NO')TRIM( SPECIAL( IRX ) )
             END IF
          END SELECT
         WRITE(KPPEQN_UNIT,'(A)')' ;'
      END DO

      CLOSE( KPPEQN_UNIT )

      WRITE( LUNOUT, * ) '   Normal Completion of CHEMMECH'

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
     &        / 5X, 'Processing for reaction number:', I6 )
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
4505  FORMAT('REAL(dp)  :: RKI_RXN_', A16,' ! rate constant for stated reaction label')        
4506  FORMAT('REAL(dp)  :: ', A16,'         ! time dependent rate econstant ')        
4500  FORMAT('#INLINE F90_RATES'
     &      / 'REAL(kind=dp) FUNCTION FALL_OFF ( A0,B0,C0,A1,B1,C1,CE,CF)'
     &      / '  IMPLICIT NONE'
     &      / '  REAL(kind=dp), INTENT( IN ) :: A0,B0,C0,A1,B1,C1,CE,CF'
     &      / '  REAL(kind=dp) K0, K1, KEND'
     &      / '! K0 = A0 * COEFF_FALLOFF * DEXP(B0/TEMP)* (TEMP/300.0_dp)**C0'
     &      / '! K1 = A1 * DEXP(B1/TEMP) * (TEMP/300.0_dp)**C1'
     &      / '  K0 = A0 * COEFF_FALLOFF * DEXP(B0*INV_TEMP)* (TEMP/300.0_dp)**C0'
     &      / '  K1 = A1 * DEXP(B1*INV_TEMP) * (TEMP/300.0_dp)**C1'
     &      / '  KEND = ( ( 1.0_dp + ( ( 1.0_dp / CE ) * DLOG10( K0 / K1 ) ) ** 2.0_dp ) )'
     &      / '  KEND = 1.0_dp / KEND'
     &      / '  FALL_OFF = ( K0 / ( 1.0_dp + K0/K1 ) ) * CF ** KEND'
     &      / 'END FUNCTION FALL_OFF'
     &      / '#ENDINLINE' )
    
4501   FORMAT( '! Name of Mechanism '
     &        / 'CHARACTER(32), PARAMETER :: MECHNAME = ''', A, '''' / '!' / '!'
     &        / 'REAL(dp), PARAMETER :: INV_T300 = 1.0D0 / 300.0D0 ! reciprocal 300K'
     &        / 'REAL(dp)            :: CAIR          ! air number density (wet) [molec/cm^3]'
     &        / 'REAL(dp)            :: INV_TEMP      ! reciprocal of air temperature, K-1'
     &        / 'REAL(dp)            :: PRESS         ! pressure [Atm] '
     &        / 'REAL(dp)            :: INV_RFACTOR   ! Convertor: ppm/min to molec/(cm^3*sec)'
     &        / 'REAL(dp)            :: RFACTOR_SQU   ! Convertor cm^6/(molec^2*sec) to 1/(ppm^2*min)'
     &        / 'REAL(dp)            :: RFACTOR       ! Convertor cm^3/(molec*sec) to 1/(ppm*min)'
     &        / 'REAL(dp)            :: COEFF_FALLOFF ! Factor in pressure limiting rate constants, [molec/cm^3] '
     &        / 'REAL                :: H2O                ! Cell H2O mixing ratio (ppmV)'
     &        / 'INTEGER, PARAMETER  :: NPHOTAB  = ', I3,'     ! number of photolysis rates '
     &        / 'CHARACTER(16), SAVE :: PHOTAB( NPHOTAB )  ! Names of  photolysis '
     &        / 'REAL(dp)            :: RJCELL( NPHOTAB )  ! grid cell photolysis rates ,[min-1]'
     &        / 'LOGICAL             :: CALC_RCONST        ! compute temp and dens dependent rate constants')
4502   FORMAT(  '! pointers and names to specific photolysis rates' )
4503   FORMAT(  'INTEGER, PARAMETER  :: IJ_',A16,' = ', I3 )
4504   FORMAT('#ENDINLINE' )
4555   FORMAT('#INLINE F90_INIT')
4556   FORMAT( 'RFACTOR       = 6.0D-5  * CAIR'
     &       / 'INV_RFACTOR   = 6.0D+7  / CAIR'
     &       / 'RFACTOR_SQU   = 6.0D-11 * CAIR * CAIR'
     &       / 'CFACTOR       = 1.0D0'
     &       / 'INV_TEMP      = 1.0D0 / TEMP'
     &       / 'COEFF_FALLOFF = CAIR ' )
4557   FORMAT('DATA PHOTAB(', I3,' ) / ''',A16,''' /')
4507  FORMAT('RKI_RXN_', A16,A4)        
4706  FORMAT(A,1X,'RCONST( ', I4,' ) ')
4708  FORMAT(A,1X,1PD12.4,' * RCONST( ', I4,' ) ')
4709  FORMAT( A )     
4710  FORMAT(A,1X,1PD12.4,' * ', A)
4711  FORMAT(' & ' / ' & ' 18X)
4712  FORMAT(A, 1X, A)
4713  FORMAT( '!If( .Not. CALC_RCONST )Then'
     &      / '!   Return'
     &      / '!Else'
     &      / '!   CALC_RCONST = .False.'
     &      / '!End If' 
     &      / '! Rate Constant Units produce reaction rates in ppm/min' )
4714  FORMAT('! number mixing ratios of constant atmospheric species, ppmV')     
4749   FORMAT('!Flag to call SPECIAL_RATES rountine in Integrator ')
4750   FORMAT('  LOGICAL,  PARAMETER :: USE_SPECIAL_RATES = .TRUE. ')
4751   FORMAT('  LOGICAL,  PARAMETER :: USE_SPECIAL_RATES = .FALSE.')
5000   FORMAT(1PD12.4,' * RJCELL( IJ_',A,' )')
5001   FORMAT(  'RJCELL( IJ_',A, ' )' )
5002   FORMAT('ARRD( ',1PD12.4,', 0.0000D+0,', 1PD12.4,' )')
5003   FORMAT('ARR2D( ',1PD12.4,', ', 1PD12.4,' )')
5004   FORMAT('ARRD( ', 1PD12.4,', ', 1PD12.4,', ', 1PD12.4,' )')
5005   FORMAT('RCONST( ' I4, ' ) / ARR2( ',1PD12.4,', ',1PD12.4,' )')            
5006   FORMAT(1PD12.4,' * RCONST( ' I4, ' ) ')   
5007   FORMAT(1PD12.4,' *( 1.0D0 + 0.6D0 * PRESS )')             
5008   FORMAT('EP2D( ', 5(1PD12.4,', '), 1PD12.4, ' )' )
5009   FORMAT('EP3D( ', 3(1PD12.4,', '), 1PD12.4,' )')
5010   FORMAT('FALL_OFF( ', 2(1PD12.4,', '),' & ' / ' &', 5(1PD12.4,', '),' & ' / ' &', 1PD12.4,' )')
5011   FORMAT(1PD12.4,' * ',A)             
5012   FORMAT(A)
5014   FORMAT('ARRD( ',1PD12.4,', 0.0000D+0,', 1PD12.4,' ) * PRESS )')             
95050  FORMAT( 'SUBROUTINE SPECIAL_RATES( N, Y)'
     &       /  '!Purpose: calculate special rate operators and update'
     &       /  '!         appropriate rate constants'
     &      //  '  USE ', A,'_Global'
     &      /   '  IMPLICIT NONE'
     &      //  '!Arguments:'
     &      //  '   INTEGER,       INTENT( IN ) :: N      ! number of species'
     &      /   '   REAL(kind=dp), INTENT( IN ) :: Y( N ) ! species concs'
     &      // )
95060  FORMAT( 'RETURN'
     &      /  'END SUBROUTINE SPECIAL_RATES')
95100  FORMAT(2X,A16,' = 0.0D0')        
       STOP
       END     
       SUBROUTINE  CONVERT_CASE ( BUFFER, UPPER )
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
        END SUBROUTINE CONVERT_CASE

      SUBROUTINE WRITE_RATE_CONVERT(OUT_UNIT, RXN_ORDER)
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
95000   FORMAT(' INV_RFACTOR * ')                
95001   FORMAT(' 60.0D0 * ')                
95002   FORMAT(' RFACTOR * ')                
95003   FORMAT(' RFACTOR_SQU * ')                
        RETURN
      END SUBROUTINE WRITE_RATE_CONVERT
