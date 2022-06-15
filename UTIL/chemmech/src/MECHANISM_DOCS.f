
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

      MODULE MECHANISM_DOCS

        IMPLICIT NONE

          PUBLIC                 :: WRT_WIKI_TABLE, WRT_MD_TABLE, WRT_CSV_TABLE, 
     &                              CALCULATE_RATES, WRT_HTML_TABLE, WRT_MD_SUBTABLE,
     &                              WRT_FOAM_TABLES, ECHO_MECH
          PRIVATE

! standard atmosphere ar alt = 0 and 2 km
          INTEGER, PARAMETER   :: NUMB_POINTS  = 2
          REAL( 8 ), PARAMETER :: TEMP( NUMB_POINTS ) = (/  298.15D0,  275.15D0/) ! air temperature , K
          REAL( 8 ), PARAMETER :: CAIR( NUMB_POINTS ) = (/ 2.4615D19, 2.0936D19/) ! approximate air number density [molec/cm^3]
          REAL( 8 ), PARAMETER :: PRES( NUMB_POINTS ) = (/   1.000D0,  7.846D-1/) ! air [Atm]

          REAL( 8 ), ALLOCATABLE :: RATE_CONSTANT( :,: )
          REAL( 8 ), ALLOCATABLE :: STRING_CONSTANT( :,: )

          LOGICAL, PARAMETER :: WRITEOUT_RCONST = .TRUE.

          INTEGER            :: LOGDEV  = 6     ! Logical unit number for log file
          INTEGER            :: IOS             ! status
          INTEGER, EXTERNAL  :: JUNIT

          CHARACTER( 80 )    :: MSG             ! Mesaage text for output log

          Integer, Parameter, Private   :: EXP_LEN     = 1024         

        CONTAINS
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE WRT_WIKI_TABLE( NR, IP, LABEL, NS  )

 
      USE GET_ENV_VARS
      USE MECHANISM_DATA
      
      IMPLICIT NONE

      INTEGER,         INTENT( IN ) :: NR ! number of reactions
      INTEGER,         INTENT( IN ) :: IP ! number of photolysis reaction
      CHARACTER( 16 ), INTENT( IN ) :: LABEL( :,: ) ! LABEL(NXX,1) 1st label found in rx NXX
                                                            ! LABEL(NXX,2) 2nd label found in rx NXX
      INTEGER,         INTENT( IN ) :: NS ! number of species

c..local Variables for steady-state species

       
      CHARACTER(  1 ) :: CHR
      CHARACTER( 16 ) :: WORD
      CHARACTER( 37 ) :: PHRASE
      CHARACTER( 81 ) :: INBUF
      CHARACTER( 16 )  :: WIKI_OUT_FILE = 'WIKI_OUT_FILE'
      CHARACTER( 627 ) :: FWIKI_OUT_FILE

      INTEGER, EXTERNAL :: INDEX1
      INTEGER            :: LPOINT, IEOL
      INTEGER            :: I, ICOL, ISPC, IRX, IDX
      INTEGER            :: NXX, IPR, IPHOTAB, NC
      INTEGER            :: DUMMY_COEF( MAXRXNUM )               ! Yields for the DUMMY variable in each reaction
      INTEGER            :: SS1RX( MAXNLIST )                    ! First reaction occurrence for each SS species
      
c..Variables for species to be dropped from mechanism
      INTEGER         :: N_DROP_SPC = 0
      CHARACTER( 16 ) :: DROP_SPC( MAXNLIST )
      LOGICAL         :: LERROR
      LOGICAL         :: KPP_DUMMY   = .FALSE.
      LOGICAL         :: FIRST_TERM  = .TRUE.
      REAL( 8 )       :: WREXT_COEFFS( MAXSPECTERMS)
      INTEGER         :: WREXT_INDEX(  MAXSPECTERMS)
      INTEGER         :: TABLE_UNIT

      INTEGER SPC1RX( MAXSPEC )              ! rx index of 1st occurence of species
                                             ! in mechanism table
      CHARACTER( 120 ) :: WIKI_TABLE
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
      INTEGER             :: IDIFF_ORDER           ! difference between order of two separate reactions
      LOGICAL             :: FALLOFF_RATE       ! whether a reaction is a falloff type


      CHARACTER(  12 ) :: EXFLNM_SPCS = 'SPCSDATX'
      CHARACTER(  12 ) :: EXFLNM_RXDT = 'RXNSDATX'
      CHARACTER(  12 ) :: EXFLNM_RXCM = 'RXNSCOMX'

      INTEGER, EXTERNAL :: JUNIT
      INTEGER            :: ICOUNT, IREACT, IPRODUCT


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Create name for output file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  
      FWIKI_OUT_FILE = TRIM( OUTDIR ) // '/wiki_' // 
     &                 TRIM( MECHNAME_LOWER_CASE )  // '.txt'
      CALL CALCULATE_RATES( NR )
      
      IF( .NOT. ALLOCATED( IOLD2NEW ) )THEN
         ALLOCATE( IOLD2NEW( NUMB_MECH_SPCS ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = 'ERROR IOLD2NEW'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF
         DO I = 1, NUMB_MECH_SPCS
            IOLD2NEW( I ) = I
         END DO
      END IF
! write out reactions strings to determine mechanism information

       DO NXX = 1, NR
         DO IREACT = 1, NREACT( NXX )
            ISPC = IRR( NXX, IREACT )
               IF( IREACT .LE. 1 )THEN
                  REACTION_STR( NXX ) =  TRIM(SPARSE_SPECIES( ISPC )) // ' '
               ELSE
                  REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) //  ' + ' // TRIM(SPARSE_SPECIES( ISPC )) // ' '
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
         REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) //  ' ----> '
! write out products
         DO IPRODUCT = 1, NPRDCT( NXX )
            ISPC = IRR( NXX, IPRODUCT + 3 )
            IF ( ABS( SC( NXX,IPRODUCT ) ) .NE. 1.0 ) THEN
               IF ( SC( NXX,IPRODUCT ) .LT. 0 ) THEN
                  WRITE(COEFF_STR,'(A,F8.5)')' - ',ABS(SC( NXX,IPRODUCT ))
                  REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) // TRIM(COEFF_STR) 
     &                        // ' * '  // TRIM(SPARSE_SPECIES( ISPC ))
               ELSE
                  WRITE(COEFF_STR,'(F8.5)')SC( NXX,IPRODUCT )
                  IF( IPRODUCT .EQ. 1 )THEN
                     REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) // ' ' // TRIM(COEFF_STR) 
     &                           // ' * '  // TRIM(SPARSE_SPECIES( ISPC ))
                  ELSE
                     REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) // ' + ' // TRIM(COEFF_STR) 
     &                           // ' * '  // TRIM(SPARSE_SPECIES( ISPC ))
                  END IF
               END IF
            ELSE 
               IF ( SC( NXX,IPRODUCT ) .LT. 0.0 ) THEN
                   REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) //  ' - ' // TRIM(SPARSE_SPECIES( ISPC ))
               ELSE
                  IF( IPRODUCT .EQ. 1 )THEN
                    REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) // ' ' //  TRIM(SPARSE_SPECIES( ISPC ))
                  ELSE
                    REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) //  ' + ' // TRIM(SPARSE_SPECIES( ISPC ))
                  END IF
               END IF
            END IF
         END DO
         
       END DO
      
! create wiki table file      
      TABLE_UNIT = JUNIT()
      OPEN ( UNIT = TABLE_UNIT, FILE = FWIKI_OUT_FILE, STATUS = 'UNKNOWN'  )
! 
      WRITE(TABLE_UNIT, 69099) 
      PHRASE = ' '
      PHRASE(1:32) = MECHNAME(1:32)
      CALL CONVERT_CASE(PHRASE, .FALSE.)
      IF( NSPECIAL .GT. 0 )WRITE(TABLE_UNIT,69100)TRIM(PHRASE)

      DO NXX = 1, NSPECIAL
         WRITE(TABLE_UNIT,'(A,A)', ADVANCE = 'NO' )'**',TRIM(SPECIAL( NXX ) )
         FIRST_TERM = .TRUE.
! first write standard rate constants time concentrations
         DO IREACT = 1, MAXSPECTERMS
             IRX  = INDEX_KTERM( NXX, IREACT )
             IF( IRX .LT. 1 .AND. INDEX_CTERM( NXX, IREACT ) .LT. 1 )CYCLE
             IF( FIRST_TERM )THEN
                PHRASE = ' = '
                FIRST_TERM = .FALSE.
                IF(KC_COEFFS( NXX, IREACT ) .LT. 0.0 )PHRASE = ' = ' // ' - '
             ELSE
!                WRITE(TABLE_UNIT, 4711, ADVANCE = 'NO' )
                PHRASE = ' + '
                IF(KC_COEFFS( NXX, IREACT ) .LT. 0.0 )PHRASE = ' - '
             END IF
             IF( KC_COEFFS( NXX, IREACT ) .NE. 1.0 )THEN
                 IF( IRX .GT. 0 )THEN
                    WRITE(TABLE_UNIT, 4708, ADVANCE = 'NO')TRIM(PHRASE),
     &              REAL( ABS( KC_COEFFS( NXX, IREACT ) ), 8),TRIM(LABEL(IRX,1))
                 ELSE
                    WRITE(TABLE_UNIT, 5708, ADVANCE = 'NO')TRIM(PHRASE),
     &              REAL( ABS( KC_COEFFS( NXX, IREACT ) ), 8)
                 END IF
             ELSE
                 IF( IRX .GT. 0 )THEN
                    WRITE(TABLE_UNIT, 4706, ADVANCE = 'NO')TRIM(PHRASE),TRIM(LABEL(IRX,1))
                 ELSE
                    WRITE(TABLE_UNIT, 4709, ADVANCE = 'NO')TRIM(PHRASE)
                 END IF
             END IF
             IF( INDEX_CTERM( NXX, IREACT ) .LT. 1  )CYCLE
             ISPC = IOLD2NEW( INDEX_CTERM( NXX, IREACT ) )
             IF( IRX .GT. 0 )THEN
                PHRASE = '[' // TRIM( SPARSE_SPECIES( ISPC ) ) // ']'
             ELSE
                PHRASE = ' [' // TRIM( SPARSE_SPECIES( ISPC ) ) // ']'
             END IF
             WRITE(TABLE_UNIT, 4709, ADVANCE = 'NO')TRIM( PHRASE )
         END DO
         IF( MAXVAL( OPERATORS( NXX, 1:MAXSPECTERMS ) ) .LT. 1 )THEN
            WRITE(TABLE_UNIT, * )' '
            CYCLE
         END IF
! next write defined operators         
         DO IREACT = 1, MAXSPECTERMS
            IDX = OPERATORS( NXX, IREACT )
            IF( IDX .LT. 1 .AND. IREACT .LT. MAXSPECTERMS )THEN
                CYCLE
            ELSE IF( IDX .LT. 1 .AND. IREACT .GE. MAXSPECTERMS )THEN
                WRITE(TABLE_UNIT, * )' '
                CYCLE
            END IF
             IF( FIRST_TERM )THEN
                PHRASE = ' = '
                IF(OPERATOR_COEFFS( NXX, IREACT ) .LT. 0.0 )PHRASE = ' = ' // ' - '
                FIRST_TERM = .FALSE.
             ELSE
                WRITE(TABLE_UNIT, 4711, ADVANCE = 'NO' )
                PHRASE = ' + '
                IF(OPERATOR_COEFFS( NXX, IREACT ) .LT. 0.0 )PHRASE = ' - '
             END IF
             IF( OPERATOR_COEFFS( NXX, IREACT ) .NE. 1.0 )THEN
                 WRITE(TABLE_UNIT, 4710, ADVANCE = 'NO')TRIM(PHRASE),
     &           REAL( ABS( OPERATOR_COEFFS( NXX, IREACT ) ), 8), TRIM( SPECIAL( IDX ) )
             ELSE
                 WRITE(TABLE_UNIT, 4712, ADVANCE = 'NO')TRIM(PHRASE),TRIM( SPECIAL( IDX ) )
             END IF
             IF( IREACT .GE. MAXSPECTERMS )WRITE(TABLE_UNIT, * )' '
         END DO 
      END DO
      IF( NSPECIAL .GT. 0 )WRITE(TABLE_UNIT, 69101)

!    
      WRITE(TABLE_UNIT, 5121)(TEMP(LPOINT),CAIR(LPOINT),PRES(LPOINT),LPOINT=1,NUMB_POINTS)
      DO NXX = 1, NR
         WRITE(TABLE_UNIT,'(A, I5, 3A)', ADVANCE= 'NO')'|| ', NXX, ' || <',TRIM(LABEL( NXX,1 )),'>   || '
         DO IREACT = 1, NREACT( NXX )
            ISPC = IRR( NXX, IREACT )
               IF( IREACT .LT. 2 )THEN
                  WRITE(TABLE_UNIT,'(A, A)', ADVANCE = 'NO')TRIM(SPARSE_SPECIES( ISPC )),' '
                  ICOUNT = 1 + LEN( SPARSE_SPECIES( ISPC ) )
               ELSE
                  WRITE(TABLE_UNIT,'(3A)', ADVANCE = 'NO')'+ ',TRIM(SPARSE_SPECIES( ISPC )),' '
                  ICOUNT = 1 + LEN( SPARSE_SPECIES( ISPC ) )
                  ICOUNT = 3 + LEN( SPARSE_SPECIES( ISPC ) )                  
               END IF
         END DO
         DO I = 1, MAXRCTNTS
         IF( INDEX_FIXED_SPECIES( NXX, I ) .GT. 0 .AND. INDEX_FIXED_SPECIES( NXX, I ) .LT. 7 )THEN
              ISPC = INDEX_FIXED_SPECIES( NXX, I  )
              WRITE(TABLE_UNIT,'(3A)', ADVANCE = 'NO')'+ ',TRIM(FIXED_SPECIES( ISPC )),' '
              ICOUNT = 3 + LEN( FIXED_SPECIES( ISPC ) )
         ELSE 
              IF( INDEX_FIXED_SPECIES( NXX, I ) .GE. 7 )THEN
                  WRITE(*,*)'WARNING: INDEX_FIXED_SPECIES( ', NXX,',', I, ') = ',INDEX_FIXED_SPECIES( NXX, I )
              END IF
         END IF    
         END DO     
         WRITE(TABLE_UNIT, '(A)', ADVANCE = 'NO' )'---->'
! write out products
         DO IPRODUCT = 1, NPRDCT( NXX )
            ISPC = IRR( NXX, IPRODUCT + 3 )
            IF ( ABS( SC( NXX,IPRODUCT ) ) .NE. 1.0 ) THEN
               IF ( SC( NXX,IPRODUCT ) .LT. 0 ) THEN
                  WRITE(TABLE_UNIT,'(A,F8.5,3A)', ADVANCE = 'NO')
     &           '- ',ABS(SC( NXX,IPRODUCT )),'*',TRIM(SPARSE_SPECIES( ISPC )),' '
                  ICOUNT = ICOUNT + 12 + LEN( SPARSE_SPECIES( ISPC ) )
               ELSE
                  IF( IPRODUCT .EQ. 1 )THEN
                     WRITE(TABLE_UNIT,'(F8.5, 3A)', ADVANCE = 'NO')
     &               SC( NXX,IPRODUCT ),'*',TRIM(SPARSE_SPECIES( ISPC )),' '
                     ICOUNT = ICOUNT + 10 + LEN( SPARSE_SPECIES( ISPC ) )
                  ELSE
                     WRITE(TABLE_UNIT,'(A,F8.5,3A)', ADVANCE = 'NO')
     &               '+ ',SC( NXX,IPRODUCT ),'*',TRIM(SPARSE_SPECIES( ISPC )),' '
                     ICOUNT = ICOUNT + 12 + LEN( SPARSE_SPECIES( ISPC ) )
                  END IF
               END IF
            ELSE IF ( SC( NXX,IPRODUCT ) .LT. 0.0 ) THEN
               WRITE(TABLE_UNIT,'(3A)', ADVANCE = 'NO')
     &               '- ',TRIM(SPARSE_SPECIES( ISPC )),' '
               ICOUNT = ICOUNT + 3 + LEN( SPARSE_SPECIES( ISPC ) )
            ELSE
               IF( IPRODUCT .EQ. 1 )THEN
                  WRITE(TABLE_UNIT,'(3A)', ADVANCE = 'NO')
     &           ' ',TRIM(SPARSE_SPECIES( ISPC )),' '
                  ICOUNT = ICOUNT + 2 + LEN( SPARSE_SPECIES( ISPC ) )
               ELSE
                  WRITE(TABLE_UNIT,'(3A)', ADVANCE = 'NO')
     &             '+ ',TRIM(SPARSE_SPECIES( ISPC )),' '
                  ICOUNT = ICOUNT + 3 + LEN( SPARSE_SPECIES( ISPC ) )
               END IF
            END IF
!            IF( ICOUNT .GT. 132 .AND. IPRODUCT .LT.  NPRDCT( NXX ) )THEN
!            IF( IPRODUCT .LT.  NPRDCT( NXX ) )THEN
!                ICOUNT = 0
!                WRITE(TABLE_UNIT, * )' '
!                WRITE(TABLE_UNIT,'(A16)', ADVANCE = 'NO')' '
!            END IF
         END DO 
         WRITE(TABLE_UNIT,'(A)', ADVANCE = 'NO')' || '

         SELECT CASE( KTYPE( NXX ) )
          CASE( -1 )
             DO IPR = 1, NHETERO
                IF ( IHETERO( IPR,1 ) .EQ. NXX )EXIT
             END DO
             IDX = IHETERO( IPR, 2 )
             IF( RTDAT(1, NXX) .NE. 1.0 )THEN
                 WRITE(TABLE_UNIT,5027, ADVANCE = 'NO')REAL(RTDAT(1, NXX),8),TRIM( HETERO(IDX) )
             ELSE
                 WRITE(TABLE_UNIT,5028, ADVANCE = 'NO')TRIM( HETERO(IDX) )
             END IF
          CASE(  0 )
             DO IPR = 1, IP
                IF ( IPH( IPR,1 ) .EQ. NXX )EXIT
             END DO
             IF ( IPH( IPR,3 ) .NE. 0 )THEN
                IDX = IPH( IPR, 2 )
                IF( RTDAT(1, NXX) .NE. 1.0 )THEN
                   WRITE(TABLE_UNIT,5000, ADVANCE = 'NO')REAL(RTDAT(1, NXX),8),TRIM( PHOTAB(IDX) )
                ELSE
                   WRITE(TABLE_UNIT,5001, ADVANCE = 'NO')TRIM( PHOTAB(IDX) )
                END IF
             ELSE IF( IPH( NXX,3 ) .EQ. 0 )THEN
                IDX = IPH(IPH( NXX,2 ), 2)
                IF( RTDAT(1, NXX) .NE. 1.0 )THEN
                   WRITE(TABLE_UNIT,5100, ADVANCE = 'NO')REAL(RTDAT(1, NXX),8),TRIM(LABEL(IDX,1))
                ELSE
                   WRITE(TABLE_UNIT,5101, ADVANCE = 'NO')TRIM(LABEL(IDX,1))
                END IF
             END IF
          CASE( 1 )
             WRITE(TABLE_UNIT,'(ES12.4)', ADVANCE = 'NO')REAL(RTDAT(1, NXX), 8)
          CASE( 2 )
             WRITE(TABLE_UNIT,5129, ADVANCE = 'NO')RTDAT(1, NXX), RTDAT(2, NXX)
          CASE( 3 )
             WRITE(TABLE_UNIT,5103, ADVANCE = 'NO')RTDAT(1, NXX), RTDAT(3, NXX)
          CASE( 4 )
             WRITE(TABLE_UNIT,5104, ADVANCE = 'NO')RTDAT(1, NXX), RTDAT(3, NXX), RTDAT(2, NXX)
          CASE( 5 )
             IRX = INT( RTDAT( 3, NXX) )
             IF( IRX .GT. NXX )CYCLE
             IDIFF_ORDER = IORDER(NXX) - IORDER(IRX)
             IF( IDIFF_ORDER .NE. 0 )THEN
                 FALLOFF_RATE = ( KTYPE(IRX) .GT. 7 .AND. KTYPE(IRX) .LT. 11 )
!                 IF( KUNITS .EQ. 2 .OR. FALLOFF_RATE )THEN
!                   CALL WRITE_RATE_CONVERT_BEFORE(MODULE_UNIT, IDIFF_ORDER )
!                 END IF
             END IF
             WRITE(TABLE_UNIT,5115, ADVANCE = 'NO')1.0D0/RTDAT( 1, NXX ), -RTDAT(2, NXX ),TRIM(LABEL(IRX,1))
          CASE( 6 )
!             DO IDX = 1, KTN6
!                IF( KRX6( IDX ) .EQ. NXX )EXIT
!             END DO         
             IRX = INT( RTDAT( 2, NXX) )
	     IDIFF_ORDER = IORDER(NXX) - IORDER(IRX)
	     IF( IDIFF_ORDER .NE. 0 )THEN
	         FALLOFF_RATE = ( KTYPE(IRX) .GT. 7 .AND. KTYPE(IRX) .LT. 11 )
	     END IF
             IF( RTDAT( 1, NXX ) .NE. 1.0 )THEN
                 WRITE(TABLE_UNIT, 5006, ADVANCE = 'NO')REAL(RTDAT( 1, NXX ), 8),TRIM(LABEL(IRX,1))
             ELSE
                 WRITE(TABLE_UNIT, 4706, ADVANCE = 'NO')' ', TRIM(LABEL(IRX,1))
             END IF
          CASE( 7 )
             IF( RTDAT(1, NXX) .NE. 0.0 )THEN
                 WRITE(TABLE_UNIT,5114, ADVANCE = 'NO')REAL(RTDAT(1, NXX), 8),REAL(RTDAT(2, NXX), 8)
             ELSE
                 WRITE(TABLE_UNIT,5007, ADVANCE = 'NO')REAL(RTDAT(1, NXX), 8)
             END IF
          CASE( 8 )
             DO IDX = 1, NFALLOFF
                IF( IRRFALL( IDX ) .EQ. NXX )EXIT
             END DO
             WRITE(TABLE_UNIT,5108, ADVANCE = 'NO')RTDAT(1,NXX),(1.0*RTDAT(2,NXX)),RTDAT(3,NXX),
     &      (1.0*RFDAT(1,IDX)),RFDAT(2,IDX),(1.0*RFDAT(3,IDX))
          CASE( 9 )
             DO IDX = 1, NFALLOFF
                IF( IRRFALL( IDX ) .EQ. NXX )EXIT
             END DO
             IF( RFDAT( 2, IDX ) .EQ. 0.0 .AND. RFDAT( 3, IDX ) .EQ. 0.0 )THEN
                 WRITE(TABLE_UNIT,5109, ADVANCE = 'NO')RTDAT(1,NXX),RTDAT(2,NXX),
     &           RTDAT(3,NXX),1.0*RFDAT(1,IDX)
             ELSE 
                 WRITE(TABLE_UNIT,5119, ADVANCE = 'NO')RTDAT(1,NXX),RTDAT(2,NXX),RFDAT(2, IDX),
     &           RTDAT(3,NXX),1.0*RFDAT(1,IDX),RFDAT(3, IDX),RFDAT(4, IDX),RFDAT(5, IDX)
              END IF 
          CASE( 10 )
             DO IDX = 1, NFALLOFF
                IF( IRRFALL( IDX ) .EQ. NXX )EXIT
             END DO
             WRITE(TABLE_UNIT, 5110, ADVANCE = 'NO')RTDAT(1,NXX),RTDAT(3,NXX),RTDAT(2,NXX),
     &       RFDAT(1,IDX),RFDAT(3,IDX),RFDAT(2,IDX),RFDAT(5,IDX),RFDAT(4,IDX)
          CASE( 11 )
             DO IDX = 1, NSPECIAL_RXN
                IF( ISPECIAL( IDX, 1 ) .EQ. NXX )EXIT
             END DO
             I   = ISPECIAL( IDX, 1)
             IRX = ISPECIAL( IDX, 2)
             IF( RTDAT( 1, I) .NE. 1.0 .AND. ABS( RTDAT( 3, I ) ) .LT. 1.0E-8 )THEN
                WRITE(TABLE_UNIT,5011, ADVANCE = 'NO')REAL(RTDAT( 1, I),8), TRIM( SPECIAL( IRX ) )
             ELSE IF( RTDAT( 1, I) .NE. 1.0 .AND. ABS( RTDAT( 3, I ) ) .GE. 1.0E-8 )THEN
                WRITE(TABLE_UNIT,5013, ADVANCE = 'NO')REAL(RTDAT( 1, I ), 8),REAL(RTDAT( 3, I ), 8),
     &          TRIM( SPECIAL( IRX) )
             ELSE IF( RTDAT( 1, I) .EQ. 1.0 .AND. ABS( RTDAT( 3, I ) ) .GE. 1.0E-8 )THEN
                WRITE(TABLE_UNIT,5014, ADVANCE = 'NO')REAL(RTDAT( 3, I ), 8),
     &          TRIM( SPECIAL( IRX) )
             ELSE
                WRITE(TABLE_UNIT,5012, ADVANCE = 'NO')TRIM( SPECIAL( IRX ) )
             END IF
           CASE( 12 )
             DO IDX = 1, NFALLOFF
                IF( IRRFALL( IDX ) .EQ. NXX )EXIT
             END DO
             WRITE(TABLE_UNIT,5120, ADVANCE = 'NO')RTDAT(1, NXX ),RFDAT(1, IDX),RTDAT(2, NXX ),
     &       RFDAT(2, IDX),RTDAT(3, NXX )
          CASE( 13 )
             DO IDX = 1, NRATE_STRING
                IF( KSTRING( IDX ) .EQ. NXX )EXIT
             END DO
             WRITE(TABLE_UNIT,'(A)', ADVANCE = 'NO')TRIM( RATE_STRING( IDX ) )
          END SELECT
! write estimated rate constant 
          SELECT CASE( KTYPE( NXX ) )
             CASE( 0 )
                 WRITE(TABLE_UNIT,'(A,  A, A, A, A / A)')' || ', 'Not Available',
     &           ' || ', 'Not Available', ' || Photolysis Reaction;depends on radiation and predicted concentrations || ',  
     &           '|-'
             CASE( -1 )
                 WRITE(TABLE_UNIT,'(A,  A, A, A, A / A)')' || ', 'Not Available',
     &           ' || ', 'Not Available', ' || Heteorogeneous Reaction;Depends predicted concentrations || ',  '|-'
             CASE( 11 )
                 WRITE(TABLE_UNIT,'(A,  A, A, A, A / A)')' || ', 'Not Available',
     &           ' || ', 'Not Available', ' || Rate constant an Operator;Depends predicted concentrations || ',  '|-'
             CASE( 12 )
                 WRITE(TABLE_UNIT,'(A,  ES12.4, A, ES12.4, 2A / A)')' || ', RATE_CONSTANT( 1, NXX),
     &           ' || ', RATE_CONSTANT( 2, NXX), 
     &           ' || Set to zero if sun is below the horizon. SEAWATER equals surface fraction covered by ice'
     &           // ' free open ocean plus surf zones.',
     &           ' P equals air pressure in atmospheres || ',  '|-'
             CASE( 13 )
                IF( RATE_CONSTANT( 1, NXX) .LT. 0.0 .OR. RATE_CONSTANT( 1, NXX) .LT. 0.0 )THEN
                 WRITE(TABLE_UNIT,'(1X, A,  A, A, A, A / A)')'|| ', 'Not Available',
     &           ' || ', 'Not Available', ' || Rate constant entered as a character string;' 
     &           // 'CHEMMECH evaluator routine failed to compute value. || |-'
               ELSE
                 WRITE(TABLE_UNIT,'(A,  ES12.4, A, ES12.4, A / A)')' || ', RATE_CONSTANT( 1, NXX),
     &          ' || ', RATE_CONSTANT( 2, NXX), ' || Rate constant entered as a character string || ',  '|-'
               END IF
             CASE( 6 )
                 WRITE(TABLE_UNIT,'(A,  ES12.4, A, ES12.4, A / A)')' || ', RATE_CONSTANT( 1, NXX),
     &          ' || ', RATE_CONSTANT( 2, NXX), ' || Rate constant multiple of constant for listed reaction || ',  '|-'
             CASE( 5 )
                 WRITE(TABLE_UNIT,'(A,  ES12.4, A, ES12.4, A / A)')' || ', RATE_CONSTANT( 1, NXX),
     &          ' || ', RATE_CONSTANT( 2, NXX), 
     &          ' || Rate constant scaled as reverse equilibrium to constant for listed reaction || ',  '|-'
             CASE DEFAULT
                 WRITE(TABLE_UNIT,'(A,  ES12.4, A, ES12.4, A / A)')' || ', RATE_CONSTANT( 1, NXX),
     &          ' || ', RATE_CONSTANT( 2, NXX), ' || || ',  '|-'
         END SELECT
      END DO
      WRITE(TABLE_UNIT,'(A)')'|}'

      IF( NFUNCTIONS .GT. 0 )THEN
        WRITE(TABLE_UNIT,*)
        WRITE(TABLE_UNIT,*)
        WRITE(TABLE_UNIT,'(A)')'Functions Table.'
        WRITE(TABLE_UNIT,5122)
        DO IDX = 1, NFUNCTIONS
           WRITE(TABLE_UNIT,'(A1,A16,A1,A,A1)')'||',TRIM(FUNCTIONS(IDX)),'||',TRIM(FORMULA(IDX)),'||'
           WRITE(TABLE_UNIT,'(A)')'|-'
        END DO
        WRITE(TABLE_UNIT,'(A)')'|}'
      END IF

      CLOSE( TABLE_UNIT )

      RETURN
      
    
69099 FORMAT("Information is based on the mech.def file." /
     &   "*Fall-off/pressure dependent reaction rate constants ([M] equals air number density):" /
     &   "**For rate constants with k<sub>o</sub>, k<sub>inf</sub>, n, F values: ",
     &       "k = [ k<sub>o</sub>[M]/(1+k<sub>o</sub>[M]/k<sub>inf</sub>)]F<sup>G</sup>, ",
     &       "where G=(1+(log<sub>10</sub>(k<sub>o</sub>[M]/k<sub>inf</sub>)/n)<sup>2</sup>))<sup>-1</sup> " /
     &   "**For rate constants with k<sub>1</sub>, k<sub>2</sub>: k = k<sub>1</sub> + k<sub>2</sub> [M]" / 
     &   "**For rate constants with k<sub>0</sub>, k<sub>2</sub>, k<sub>3</sub>: ",
     &       "k = k<sub>0</sub> + k<sub>3</sub>[M]/(1+k<sub>3</sub>[M]/k<sub>2</sub>)" /
     &   "**For rate constants with k<sub>1</sub>, k<sub>2</sub>, k<sub>3</sub>: ",
     &       "k = k<sub>1</sub> + k<sub>2</sub>[M] + k<sub>3</sub> " /
     & / "*For rate constants with the form A<''Reference''>, k equals A times a reference that represents photolysis rate, ", 
     &   "a heteorogeneous rate constant, rate constant for the given reaction or an operator. A equals one if not given." /
     & / "*In the mechanism definition file, the rate is formatted as" 
     & / "**A~<''HETEOROGENEOUS''>"
     & / "**A*K<''REACTION''>"
     & / "**A/<''PHOTOLYSIS''>"
     & / "**A?<'OPERATOR''>" /)
69100 FORMAT("*For the ", A, " mechanism, the operators are defined  below.")
69101 FORMAT( / "where <''REACTION''> is the rate constant for the given ''REACTION'' and [''species''] ",
     &          "equals the concentration of a mechanism ''species'' at the beginning of ",
     &          "the integration time-step for the chemistry's numerical solver.")

4706   FORMAT(A,1X,"<''", A,"''>")
4708   FORMAT(A,1X,ES9.2,"<''", A,"''>")
5708   FORMAT(A,1X,ES9.2)
4709   FORMAT( A )     
4710   FORMAT(A,1X,ES9.2,'*', A)
4711   FORMAT(1X)
4712   FORMAT(A, 1X, A)
5000   FORMAT(ES12.4,"<''",A,"''>")
5001   FORMAT(  "<''",A, "''>" )
5100   FORMAT(ES12.4,"<''",I4,"''>")
5101   FORMAT(  "<''",I4,"''>")
5006   FORMAT(ES12.4,"<''", A, "''>")   
5007   FORMAT(ES12.4,' *( 1.0D0 + 0.6D0 * PRESS )')             
5011   FORMAT(ES12.4,"<''",A,"''>")             
5012   FORMAT("<''",A,"''>")
5013   FORMAT(ES12.4,"exp(-",ES12.4,"/T)<''",A,"''>")             
5014   FORMAT("exp(-",ES12.4,"/T)*<''",A,"''>")             
5027   FORMAT(ES12.4,"<''",A,"''>")
5028   FORMAT( "<''",A, "''>" )

5111   FORMAT(ES12.4) 
!5129   FORMAT('POWER_T02( TEMPOT300, ',ES12.4,', ', ES12.4,' )')
5129   FORMAT(ES12.4,'*(T/300)<sup>(', ES12.4,')</sup>')
!5102   FORMAT('ARRHENUIS_T04( INV_TEMP,  TEMPOT300,',ES12.4,', 0.0000D+0,', ES12.4,' )')
5102   FORMAT(ES12.4,'*(T/300)<sup>(', ES12.4,')</sup>')
!5103   FORMAT('ARRHENUIS_T03( INV_TEMP,',ES12.4,', ', ES12.4,' )')
5103   FORMAT(ES12.4,'*exp<sup>(', ES12.4,'/T)</sup>')
5104   FORMAT(ES12.4,'*exp<sup>(',ES12.4,'/T)</sup>*(T/300)<sup>('ES12.4,' )</sup>')
5114   FORMAT(ES12.4,'P*(T/300)<sup>(', ES12.4,' )</sup>')
!5114   FORMAT('ARRHENUIS_T04( INV_TEMP,  TEMPOT300,',  ES12.4,', 0.0000D+0,',
!     &        ES12.4,' )  * PRESS ')             
5115   FORMAT( ES12.4,'*exp<sup>(', ES12.4,'/T)</sup> \* ', A )
!5108   FORMAT('FALLOFF_T08( INV_TEMP,  CAIR, ', 3(ES12.4,', '),2(ES12.4,', '), ES12.4, ' )' )
!5109   FORMAT('FALLOFF_T09( INV_TEMP,  CAIR,', 3(ES12.4,', '), ES12.4, ' )' )
!5110   FORMAT('FALLOFF_T90( INV_TEMP,  TEMPOT300,  CAIR,', 3(ES12.4,', '), 3(ES12.4,', '), ES12.4,
!     &         ', ', ES12.4,' )')
!5119   FORMAT('FALLOFF_T91( INV_TEMP,TEMPOT300,CAIR,', 3(ES12.4,', '), 
!     &         3(ES12.4,', '), ES12.4,', ', ES12.4,' )')
!FALL 8
5108    FORMAT(' k<sub>0</sub> = ', ES12.4,'*exp<sup>(',ES12.4,'/T)</sup>;',
     &         ' k<sub>1</sub> = ', ES12.4,'*exp<sup>(',ES12.4,'/T)</sup>;',
     &         ' k<sub>3</sub> = ', ES12.4,'*exp<sup>(',ES12.4,'/T)</sup>')
!FALL 9
5109    FORMAT(' k<sub>0</sub> = ', ES12.4,'*exp<sup>(',ES12.4,'/T)</sup>;',
     &         ' k<sub>1</sub> = ', ES12.4,'*exp<sup>(',ES12.4,'/T)</sup>')
! FALL 10
5110    FORMAT(' k<sub>o</sub> = ', ES12.4,'*exp<sup>(',ES12.4,'/T)</sup>*(T/300)<sup>',ES12.4,'</sup>;',
     &         ' k<sub>inf</sub> = ', ES12.4,'*exp<sup>(',ES12.4,'/T)</sup>*(T/300)<sup>',ES12.4,'</sup>;',
     &         ' n = ', ES12.4,'; F = ', ES12.4 )
!FALL 11
5119    FORMAT( ' k<sub>0</sub> = ', ES12.4,'*exp<sup>(',ES12.4,'/T)</sup>*(T/300)<sup>',ES12.4,'</sup>;',
     &          ' k<sub>2</sub> = ', ES12.4,'*exp<sup>(',ES12.4,'/T)</sup>*(T/300)<sup>',ES12.4,'</sup>;',
     &          ' k<sub>3</sub> = ', ES12.4,'*exp<sup>(',ES12.4,'/T)</sup>')
5120   FORMAT('SEAWATER*min(', ES10.3,'*exp<sup>(',ES10.3'*P),</sup> +', ES10.3,'*exp<sup>(',ES10.3'*P),</sup>, ',ES10.3')')

5121   FORMAT('{|class="wikitable"',
     &        / '|-',
     &        / '!Reaction Number',
     &        / '!Reaction Label',
     &        / '!Reaction',
     &        / '!Rate Constant Formula',
     &        / '!Value at ',F6.2,' K; ',ES12.4,' molec/cm<sup>3</sup>; ', F6.2,' Atm.',
     &        / '!Value at ',F6.2,' K; ',ES12.4,' molec/cm<sup>3</sup>; ', F6.2,' Atm.',
     &        / '!Notes',
     &        / '!References',
     &        / '|-' )

5122   FORMAT('{|class="wikitable"',
     &        / '|-',
     &        / '!Name',
     &        / '!Formula'
     &        / '|-' )

95100  FORMAT(2X,A16,' = 0.0D0')        


       END SUBROUTINE WRT_WIKI_TABLE

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE WRT_MD_TABLE( NR, IP, LABEL, NS  )

 
      USE GET_ENV_VARS
      USE MECHANISM_DATA
      
      IMPLICIT NONE

      INTEGER,         INTENT( IN ) :: NR ! number of reactions
      INTEGER,         INTENT( IN ) :: IP ! number of photolysis reaction
      CHARACTER( 16 ), INTENT( IN ) :: LABEL( :,: ) ! LABEL(NXX,1) 1st label found in rx NXX
                                                            ! LABEL(NXX,2) 2nd label found in rx NXX
      INTEGER,         INTENT( IN ) :: NS ! number of species

c..local Variables for steady-state species

       
      CHARACTER(  1 ) :: CHR
      CHARACTER( 16 ) :: WORD
      CHARACTER( 37 ) :: PHRASE
      CHARACTER( 81 ) :: INBUF
      CHARACTER( 16 )  :: WIKI_OUT_FILE = 'WIKI_OUT_FILE'
      CHARACTER( 627 ) :: FWIKI_OUT_FILE

      INTEGER, EXTERNAL :: INDEX1
      INTEGER            :: LPOINT, IEOL
      INTEGER            :: I, ICOL, ISPC, IRX, IDX
      INTEGER            :: NXX, IPR, IPHOTAB, NC
      INTEGER            :: DUMMY_COEF( MAXRXNUM )               ! Yields for the DUMMY variable in each reaction
      INTEGER            :: SS1RX( MAXNLIST )                    ! First reaction occurrence for each SS species
      
c..Variables for species to be dropped from mechanism
      INTEGER         :: N_DROP_SPC = 0
      CHARACTER( 16 ) :: DROP_SPC( MAXNLIST )
      LOGICAL         :: LERROR
      LOGICAL         :: KPP_DUMMY   = .FALSE.
      LOGICAL         :: FIRST_TERM  = .TRUE.
      REAL( 8 )       :: WREXT_COEFFS( MAXSPECTERMS)
      INTEGER         :: WREXT_INDEX(  MAXSPECTERMS)
      INTEGER         :: TABLE_UNIT

      INTEGER SPC1RX( MAXSPEC )              ! rx index of 1st occurence of species
                                             ! in mechanism table
      CHARACTER( 120 ) :: WIKI_TABLE
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
      INTEGER             :: IDIFF_ORDER           ! difference between order of two separate reactions
      LOGICAL             :: FALLOFF_RATE       ! whether a reaction is a falloff type


      CHARACTER(  12 ) :: EXFLNM_SPCS = 'SPCSDATX'
      CHARACTER(  12 ) :: EXFLNM_RXDT = 'RXNSDATX'
      CHARACTER(  12 ) :: EXFLNM_RXCM = 'RXNSCOMX'

      INTEGER, EXTERNAL :: JUNIT
      INTEGER            :: ICOUNT, IREACT, IPRODUCT


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Create name for output file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  
      FWIKI_OUT_FILE = TRIM( OUTDIR ) // '/mech_' // 
     &                 TRIM( MECHNAME_LOWER_CASE )  // '.md'

      CALL CALCULATE_RATES( NR )

      IF( .NOT. ALLOCATED( IOLD2NEW ) )THEN
         ALLOCATE( IOLD2NEW( NUMB_MECH_SPCS ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = 'ERROR IOLD2NEW'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF
         DO I = 1, NUMB_MECH_SPCS
            IOLD2NEW( I ) = I
         END DO
      END IF
! write out reactions strings to determine mechanism information

       DO NXX = 1, NR
         DO IREACT = 1, NREACT( NXX )
            ISPC = IRR( NXX, IREACT )
               IF( IREACT .LE. 1 )THEN
                  REACTION_STR( NXX ) =  TRIM(SPARSE_SPECIES( ISPC )) // ' '
               ELSE
                  REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) //  ' + ' // TRIM(SPARSE_SPECIES( ISPC )) // ' '
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
         REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) //  ' ----> '
! write out products
         DO IPRODUCT = 1, NPRDCT( NXX )
            ISPC = IRR( NXX, IPRODUCT + 3 )
            IF ( ABS( SC( NXX,IPRODUCT ) ) .NE. 1.0 ) THEN
               IF ( SC( NXX,IPRODUCT ) .LT. 0 ) THEN
                  WRITE(COEFF_STR,'(A,F8.5)')' - ',ABS(SC( NXX,IPRODUCT ))
                  REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) // TRIM(COEFF_STR) 
     &                        // ' * '  // TRIM(SPARSE_SPECIES( ISPC ))
               ELSE
                  WRITE(COEFF_STR,'(F8.5)')SC( NXX,IPRODUCT )
                  IF( IPRODUCT .EQ. 1 )THEN
                     REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) // ' ' // TRIM(COEFF_STR) 
     &                           // ' * '  // TRIM(SPARSE_SPECIES( ISPC ))
                  ELSE
                     REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) // ' + ' // TRIM(COEFF_STR) 
     &                           // ' * '  // TRIM(SPARSE_SPECIES( ISPC ))
                  END IF
               END IF
            ELSE 
               IF ( SC( NXX,IPRODUCT ) .LT. 0.0 ) THEN
                   REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) //  ' - ' // TRIM(SPARSE_SPECIES( ISPC ))
               ELSE
                  IF( IPRODUCT .EQ. 1 )THEN
                    REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) // ' ' //  TRIM(SPARSE_SPECIES( ISPC ))
                  ELSE
                    REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) //  ' + ' // TRIM(SPARSE_SPECIES( ISPC ))
                  END IF
               END IF
            END IF
         END DO
         
       END DO

! create wiki table file      
      TABLE_UNIT = JUNIT()
      OPEN ( UNIT = TABLE_UNIT, FILE = FWIKI_OUT_FILE, STATUS = 'UNKNOWN'  )
! 
      WRITE(TABLE_UNIT, 69099) 
      PHRASE = ' '
      PHRASE(1:32) = MECHNAME(1:32)
      CALL CONVERT_CASE(PHRASE, .FALSE.)
      IF( NSPECIAL .GT. 0 )WRITE(TABLE_UNIT,69100)TRIM(PHRASE)

      DO NXX = 1, NSPECIAL
         WRITE(TABLE_UNIT,'(A,A)', ADVANCE = 'NO' )' * ',TRIM(SPECIAL( NXX ) )
         FIRST_TERM = .TRUE.
! first write standard rate constants time concentrations
         DO IREACT = 1, MAXSPECTERMS
             IRX  = INDEX_KTERM( NXX, IREACT )
             IF( IRX .LT. 1 .AND. INDEX_CTERM( NXX, IREACT ) .LT. 1 )CYCLE
             IF( FIRST_TERM )THEN
                PHRASE = ' = '
                FIRST_TERM = .FALSE.
                IF(KC_COEFFS( NXX, IREACT ) .LT. 0.0 )PHRASE = ' = ' // ' - '
             ELSE
!                WRITE(TABLE_UNIT, 4711, ADVANCE = 'NO' )
                PHRASE = ' + '
                IF(KC_COEFFS( NXX, IREACT ) .LT. 0.0 )PHRASE = ' - '
             END IF
             IF( KC_COEFFS( NXX, IREACT ) .NE. 1.0 )THEN
                 IF( IRX .GT. 0 )THEN
                    WRITE(TABLE_UNIT, 4708, ADVANCE = 'NO')TRIM(PHRASE),
     &              REAL( ABS( KC_COEFFS( NXX, IREACT ) ), 8),TRIM(LABEL(IRX,1))
                 ELSE
                    WRITE(TABLE_UNIT, 5708, ADVANCE = 'NO')TRIM(PHRASE),
     &              REAL( ABS( KC_COEFFS( NXX, IREACT ) ), 8)
                 END IF
             ELSE
                 IF( IRX .GT. 0 )THEN
                    WRITE(TABLE_UNIT, 4706, ADVANCE = 'NO')TRIM(PHRASE),TRIM(LABEL(IRX,1))
                 ELSE
                    WRITE(TABLE_UNIT, 4709, ADVANCE = 'NO')TRIM(PHRASE)
                 END IF
             END IF
             IF( INDEX_CTERM( NXX, IREACT ) .LT. 1  )CYCLE
             ISPC = IOLD2NEW( INDEX_CTERM( NXX, IREACT ) )
             IF( IRX .GT. 0 )THEN
                PHRASE = '[' // TRIM( SPARSE_SPECIES( ISPC ) ) // ']'
             ELSE
                PHRASE = ' [' // TRIM( SPARSE_SPECIES( ISPC ) ) // ']'
             END IF
             WRITE(TABLE_UNIT, 4709, ADVANCE = 'NO')TRIM( PHRASE )
         END DO
         IF( MAXVAL( OPERATORS( NXX, 1:MAXSPECTERMS ) ) .LT. 1 )THEN
            WRITE(TABLE_UNIT, * )' '
            CYCLE
         END IF
! next write defined operators         
         DO IREACT = 1, MAXSPECTERMS
            IDX = OPERATORS( NXX, IREACT )
            IF( IDX .LT. 1 .AND. IREACT .LT. MAXSPECTERMS )THEN
                CYCLE
            ELSE IF( IDX .LT. 1 .AND. IREACT .GE. MAXSPECTERMS )THEN
                WRITE(TABLE_UNIT, * )' '
                CYCLE
            END IF
             IF( FIRST_TERM )THEN
                PHRASE = ' = '
                IF(OPERATOR_COEFFS( NXX, IREACT ) .LT. 0.0 )PHRASE = ' = ' // ' - '
                FIRST_TERM = .FALSE.
             ELSE
                WRITE(TABLE_UNIT, 4711, ADVANCE = 'NO' )
                PHRASE = ' + '
                IF(OPERATOR_COEFFS( NXX, IREACT ) .LT. 0.0 )PHRASE = ' - '
             END IF
             IF( OPERATOR_COEFFS( NXX, IREACT ) .NE. 1.0 )THEN
                 WRITE(TABLE_UNIT, 4710, ADVANCE = 'NO')TRIM(PHRASE),
     &           REAL( ABS( OPERATOR_COEFFS( NXX, IREACT ) ), 8), TRIM( SPECIAL( IDX ) )
             ELSE
                 WRITE(TABLE_UNIT, 4712, ADVANCE = 'NO')TRIM(PHRASE),TRIM( SPECIAL( IDX ) )
             END IF
             IF( IREACT .GE. MAXSPECTERMS )WRITE(TABLE_UNIT, * )' '
         END DO 
      END DO
      IF( NSPECIAL .GT. 0 )WRITE(TABLE_UNIT, 69101)

!    
      WRITE(TABLE_UNIT, 5121)(TEMP(LPOINT),CAIR(LPOINT),PRES(LPOINT),LPOINT=1,NUMB_POINTS)
      DO NXX = 1, NR
         WRITE(TABLE_UNIT,'(A, I5, 3A)', ADVANCE= 'NO')'| ', NXX, ' | <_',TRIM(LABEL( NXX,1 )),'_>   | '
         DO IREACT = 1, NREACT( NXX )
            ISPC = IRR( NXX, IREACT )
               IF( IREACT .LT. 2 )THEN
                  WRITE(TABLE_UNIT,'(A, A)', ADVANCE = 'NO')TRIM(SPARSE_SPECIES( ISPC )),' '
                  ICOUNT = 1 + LEN( SPARSE_SPECIES( ISPC ) )
               ELSE
                  WRITE(TABLE_UNIT,'(3A)', ADVANCE = 'NO')'+ ',TRIM(SPARSE_SPECIES( ISPC )),' '
                  ICOUNT = 1 + LEN( SPARSE_SPECIES( ISPC ) )
                  ICOUNT = 3 + LEN( SPARSE_SPECIES( ISPC ) )                  
               END IF
         END DO
         DO I = 1, MAXRCTNTS
         IF( INDEX_FIXED_SPECIES( NXX, I ) .GT. 0 .AND. INDEX_FIXED_SPECIES( NXX, I ) .LT. 7 )THEN
              ISPC = INDEX_FIXED_SPECIES( NXX, I  )
              WRITE(TABLE_UNIT,'(3A)', ADVANCE = 'NO')'+ ',TRIM(FIXED_SPECIES( ISPC )),' '
              ICOUNT = 3 + LEN( FIXED_SPECIES( ISPC ) )
         ELSE 
              IF( INDEX_FIXED_SPECIES( NXX, I ) .GE. 7 )THEN
                  WRITE(*,*)'WARNING: INDEX_FIXED_SPECIES( ', NXX,',', I, ') = ',INDEX_FIXED_SPECIES( NXX, I )
              END IF
         END IF    
         END DO     
         WRITE(TABLE_UNIT, '(A)', ADVANCE = 'NO' )'---->'
! write out products
         DO IPRODUCT = 1, NPRDCT( NXX )
            ISPC = IRR( NXX, IPRODUCT + 3 )
            IF ( ABS( SC( NXX,IPRODUCT ) ) .NE. 1.0 ) THEN
               IF ( SC( NXX,IPRODUCT ) .LT. 0 ) THEN
                  WRITE(TABLE_UNIT,'(A,F8.5,3A)', ADVANCE = 'NO')
     &           '- ',ABS(SC( NXX,IPRODUCT )),'*',TRIM(SPARSE_SPECIES( ISPC )),' '
                  ICOUNT = ICOUNT + 12 + LEN( SPARSE_SPECIES( ISPC ) )
               ELSE
                  IF( IPRODUCT .EQ. 1 )THEN
                     WRITE(TABLE_UNIT,'(F8.5, 3A)', ADVANCE = 'NO')
     &               SC( NXX,IPRODUCT ),'*',TRIM(SPARSE_SPECIES( ISPC )),' '
                     ICOUNT = ICOUNT + 10 + LEN( SPARSE_SPECIES( ISPC ) )
                  ELSE
                     WRITE(TABLE_UNIT,'(A,F8.5,3A)', ADVANCE = 'NO')
     &               '+ ',SC( NXX,IPRODUCT ),'*',TRIM(SPARSE_SPECIES( ISPC )),' '
                     ICOUNT = ICOUNT + 12 + LEN( SPARSE_SPECIES( ISPC ) )
                  END IF
               END IF
            ELSE IF ( SC( NXX,IPRODUCT ) .LT. 0.0 ) THEN
               WRITE(TABLE_UNIT,'(3A)', ADVANCE = 'NO')
     &               '- ',TRIM(SPARSE_SPECIES( ISPC )),' '
               ICOUNT = ICOUNT + 3 + LEN( SPARSE_SPECIES( ISPC ) )
            ELSE
               IF( IPRODUCT .EQ. 1 )THEN
                  WRITE(TABLE_UNIT,'(3A)', ADVANCE = 'NO')
     &           ' ',TRIM(SPARSE_SPECIES( ISPC )),' '
                  ICOUNT = ICOUNT + 2 + LEN( SPARSE_SPECIES( ISPC ) )
               ELSE
                  WRITE(TABLE_UNIT,'(3A)', ADVANCE = 'NO')
     &             '+ ',TRIM(SPARSE_SPECIES( ISPC )),' '
                  ICOUNT = ICOUNT + 3 + LEN( SPARSE_SPECIES( ISPC ) )
               END IF
            END IF
!            IF( ICOUNT .GT. 132 .AND. IPRODUCT .LT.  NPRDCT( NXX ) )THEN
!            IF( IPRODUCT .LT.  NPRDCT( NXX ) )THEN
!                ICOUNT = 0
!                WRITE(TABLE_UNIT, * )' '
!                WRITE(TABLE_UNIT,'(A16)', ADVANCE = 'NO')' '
!            END IF
         END DO 
         WRITE(TABLE_UNIT,'(A)', ADVANCE = 'NO')' | '

         SELECT CASE( KTYPE( NXX ) )
          CASE( -1 )
             DO IPR = 1, NHETERO
                IF ( IHETERO( IPR,1 ) .EQ. NXX )EXIT
             END DO
             IDX = IHETERO( IPR, 2 )
             IF( RTDAT(1, NXX) .NE. 1.0 )THEN
                 WRITE(TABLE_UNIT,5027, ADVANCE = 'NO')REAL(RTDAT(1, NXX),8),TRIM( HETERO(IDX) )
             ELSE
                 WRITE(TABLE_UNIT,5028, ADVANCE = 'NO')TRIM( HETERO(IDX) )
             END IF
          CASE(  0 )
             DO IPR = 1, IP
                IF ( IPH( IPR,1 ) .EQ. NXX )EXIT
             END DO
             IF ( IPH( IPR,3 ) .NE. 0 )THEN
                IDX = IPH( IPR, 2 )
                IF( RTDAT(1, NXX) .NE. 1.0 )THEN
                   WRITE(TABLE_UNIT,5000, ADVANCE = 'NO')REAL(RTDAT(1, NXX),8),TRIM( PHOTAB(IDX) )
                ELSE
                   WRITE(TABLE_UNIT,5001, ADVANCE = 'NO')TRIM( PHOTAB(IDX) )
                END IF
             ELSE IF( IPH( NXX,3 ) .EQ. 0 )THEN
                IDX = IPH(IPH( NXX,2 ), 2)
                IF( RTDAT(1, NXX) .NE. 1.0 )THEN
                   WRITE(TABLE_UNIT,5100, ADVANCE = 'NO')REAL(RTDAT(1, NXX),8),TRIM(LABEL(IDX,1))
                ELSE
                   WRITE(TABLE_UNIT,5101, ADVANCE = 'NO')TRIM(LABEL(IDX,1))
                END IF
             END IF
          CASE( 1 )
             WRITE(TABLE_UNIT,'(ES12.4)', ADVANCE = 'NO')REAL(RTDAT(1, NXX), 8)
          CASE( 2 )
             WRITE(TABLE_UNIT,5129, ADVANCE = 'NO')RTDAT(1, NXX), RTDAT(2, NXX)
          CASE( 3 )
             WRITE(TABLE_UNIT,5103, ADVANCE = 'NO')RTDAT(1, NXX), RTDAT(3, NXX)
          CASE( 4 )
             WRITE(TABLE_UNIT,5104, ADVANCE = 'NO')RTDAT(1, NXX), RTDAT(3, NXX), RTDAT(2, NXX)
          CASE( 5 )
             IRX = INT( RTDAT( 3, NXX) )
             IF( IRX .GT. NXX )CYCLE
             IDIFF_ORDER = IORDER(NXX) - IORDER(IRX)
             IF( IDIFF_ORDER .NE. 0 )THEN
                 FALLOFF_RATE = ( KTYPE(IRX) .GT. 7 .AND. KTYPE(IRX) .LT. 11 )
!                 IF( KUNITS .EQ. 2 .OR. FALLOFF_RATE )THEN
!                   CALL WRITE_RATE_CONVERT_BEFORE(MODULE_UNIT, IDIFF_ORDER )
!                 END IF
             END IF
             WRITE(TABLE_UNIT,5115, ADVANCE = 'NO')1.0D0/RTDAT( 1, NXX ), -RTDAT(2, NXX ),TRIM(LABEL(IRX,1))
          CASE( 6 )
!             DO IDX = 1, KTN6
!                IF( KRX6( IDX ) .EQ. NXX )EXIT
!             END DO         
             IRX = INT( RTDAT( 2, NXX) )
	     IDIFF_ORDER = IORDER(NXX) - IORDER(IRX)
	     IF( IDIFF_ORDER .NE. 0 )THEN
	         FALLOFF_RATE = ( KTYPE(IRX) .GT. 7 .AND. KTYPE(IRX) .LT. 11 )
	     END IF
             IF( RTDAT( 1, NXX ) .NE. 1.0 )THEN
                 WRITE(TABLE_UNIT, 5006, ADVANCE = 'NO')REAL(RTDAT( 1, NXX ), 8),TRIM(LABEL(IRX,1))
             ELSE
                 WRITE(TABLE_UNIT, 4706, ADVANCE = 'NO')' ', TRIM(LABEL(IRX,1))
             END IF
          CASE( 7 )
             IF( RTDAT(1, NXX) .NE. 0.0 )THEN
                 WRITE(TABLE_UNIT,5114, ADVANCE = 'NO')REAL(RTDAT(1, NXX), 8),REAL(RTDAT(2, NXX), 8)
             ELSE
                 WRITE(TABLE_UNIT,5007, ADVANCE = 'NO')REAL(RTDAT(1, NXX), 8)
             END IF
          CASE( 8 )
             DO IDX = 1, NFALLOFF
                IF( IRRFALL( IDX ) .EQ. NXX )EXIT
             END DO
             WRITE(TABLE_UNIT,5108, ADVANCE = 'NO')RTDAT(1,NXX),(1.0*RTDAT(2,NXX)),RTDAT(3,NXX),
     &      (1.0*RFDAT(1,IDX)),RFDAT(2,IDX),(1.0*RFDAT(3,IDX))
          CASE( 9 )
             DO IDX = 1, NFALLOFF
                IF( IRRFALL( IDX ) .EQ. NXX )EXIT
             END DO
             IF( RFDAT( 2, IDX ) .EQ. 0.0 .AND. RFDAT( 3, IDX ) .EQ. 0.0 )THEN
                 WRITE(TABLE_UNIT,5109, ADVANCE = 'NO')RTDAT(1,NXX),RTDAT(2,NXX),
     &           RTDAT(3,NXX),1.0*RFDAT(1,IDX)
             ELSE 
                  WRITE(TABLE_UNIT,5119, ADVANCE = 'NO')RTDAT(1,NXX),RTDAT(2,NXX),RFDAT(2, IDX),
     &           RTDAT(3,NXX),1.0*RFDAT(1,IDX),RFDAT(3, IDX),RFDAT(4, IDX),RFDAT(5, IDX)
              END IF 
          CASE( 10 )
             DO IDX = 1, NFALLOFF
                IF( IRRFALL( IDX ) .EQ. NXX )EXIT
             END DO
             WRITE(TABLE_UNIT, 5110, ADVANCE = 'NO')RTDAT(1,NXX),RTDAT(3,NXX),RTDAT(2,NXX),
     &      RFDAT(1,IDX),RFDAT(3,IDX),RFDAT(2,IDX),RFDAT(5,IDX),RFDAT(4,IDX)
          CASE( 11 )
             DO IDX = 1, NSPECIAL_RXN
                IF( ISPECIAL( IDX, 1 ) .EQ. NXX )EXIT
             END DO
             I   = ISPECIAL( IDX, 1)
             IRX = ISPECIAL( IDX, 2)
             IF( RTDAT( 1, I) .NE. 1.0 .AND. ABS( RTDAT( 3, I ) ) .LT. 1.0E-8 )THEN
                WRITE(TABLE_UNIT,5011, ADVANCE = 'NO')REAL(RTDAT( 1, I),8), TRIM( SPECIAL( IRX ) )
             ELSE IF( RTDAT( 1, I) .NE. 1.0 .AND. ABS( RTDAT( 3, I ) ) .GT. 1.0E-8 )THEN
                WRITE(TABLE_UNIT,5013, ADVANCE = 'NO')REAL(RTDAT( 1, I ), 8),REAL(RTDAT( 3, I ), 8),
     &          TRIM( SPECIAL( IRX) )
             ELSE IF( RTDAT( 1, I) .EQ. 1.0 .AND. ABS( RTDAT( 3, I ) ) .GT. 1.0E-8 )THEN
                WRITE(TABLE_UNIT,5014, ADVANCE = 'NO')REAL(RTDAT( 3, I ), 8),
     &          TRIM( SPECIAL( IRX) )
             ELSE
                WRITE(TABLE_UNIT,5012, ADVANCE = 'NO')TRIM( SPECIAL( IRX ) )
             END IF
           CASE( 12 )
             DO IDX = 1, NFALLOFF
                IF( IRRFALL( IDX ) .EQ. NXX )EXIT
             END DO
             WRITE(TABLE_UNIT,5120, ADVANCE = 'NO')RTDAT(1, NXX ),RFDAT(1, IDX),RTDAT(2, NXX ),
     &       RFDAT(2, IDX),RTDAT(3, NXX)
          CASE( 13 )
             DO IDX = 1, NRATE_STRING
                IF( KSTRING( IDX ) .EQ. NXX )EXIT
             END DO
             WRITE(TABLE_UNIT,'(A)', ADVANCE = 'NO')TRIM( RATE_STRING( IDX ) )
          END SELECT
! write estimated rate constant 
          SELECT CASE( KTYPE( NXX ) )
             CASE( 0 )
                 WRITE(TABLE_UNIT,'(A,  A, A, A, A , A)')' | ', 'Not Available',
     &           ' | ', 'Not Available', ' | Photolysis Reaction;depends on radiation and predicted concentrations | |'
             CASE( -1 )
                 WRITE(TABLE_UNIT,'(A,  A, A, A, A , A)')' | ', 'Not Available',
     &           ' | ', 'Not Available', ' | Heteorogeneous Reaction;Depends predicted concentrations | |'
             CASE( 11 )
                 WRITE(TABLE_UNIT,'(A,  A, A, A, A , A)')' | ', 'Not Available',
     &           ' | ', 'Not Available', ' | Rate constant an Operator;Depends predicted concentrations | |'
             CASE( 12 )
                 WRITE(TABLE_UNIT,'(A,  ES12.4, A, ES12.4, 2A , A)')' | ', RATE_CONSTANT( 1, NXX),
     &           ' | ', RATE_CONSTANT( 2, NXX), 
     &           ' | Set to zero if sun is below the horizon. SEAWATER equals surface fraction covered'
     &           // ' by ice free open ocean plus surf zones.;',
     &           ' P equals air pressure in atmospheres | | '
             CASE( 13 )
                IF( RATE_CONSTANT( 1, NXX) .LT. 0.0 .OR. RATE_CONSTANT( 1, NXX) .LT. 0.0 )THEN
                 WRITE(TABLE_UNIT,'(1X, A,  A, A, A, A , A)')'| ', 'Not Available',
     &           ' | ', 'Not Available', ' | Rate constant entered as a character string;'  
     &           // 'CHEMMECH evaluator routine failed to compute value. | |'
               ELSE
                 WRITE(TABLE_UNIT,'(A,  ES12.4, A, ES12.4, A , A)')' | ', RATE_CONSTANT( 1, NXX),
     &          ' | ', RATE_CONSTANT( 2, NXX), ' | Rate constant entered as a character string | | '
               END IF
             CASE( 6 )
                 WRITE(TABLE_UNIT,'(A,  ES12.4, A, ES12.4, A , A)')' | ', RATE_CONSTANT( 1, NXX),
     &          ' | ', RATE_CONSTANT( 2, NXX), ' | Rate constant multiple of constant for listed reaction | | '
             CASE( 5 )
                 WRITE(TABLE_UNIT,'(A,  ES12.4, A, ES12.4, A , A)')' | ', RATE_CONSTANT( 1, NXX),
     &          ' | ', RATE_CONSTANT( 2, NXX), 
     &          ' | Rate constant scaled as reverse equilibrium to constant for listed reaction | |'
             CASE DEFAULT
                 WRITE(TABLE_UNIT,'(A,  ES12.4, A, ES12.4, A, A)')' | ', RATE_CONSTANT( 1, NXX),
     &          ' | ', RATE_CONSTANT( 2, NXX), ' | |'
         END SELECT
      END DO

      IF( NFUNCTIONS .GT. 0 )THEN
        WRITE(TABLE_UNIT,*)
        WRITE(TABLE_UNIT,*)
        WRITE(TABLE_UNIT,'(A)')'Functions Table.'
        WRITE(TABLE_UNIT,'(A)')'|+...........+|+......................|'
        WRITE(TABLE_UNIT,'(A)')'|     Name    |         Formula       |'
        DO IDX = 1, NFUNCTIONS
           WRITE(TABLE_UNIT,'(A1,A16,A1,A,A1)')'|',TRIM(FUNCTIONS(IDX)),'|',TRIM(FORMULA(IDX)),'|'
        END DO
      END IF

      CLOSE(TABLE_UNIT)


      RETURN
      
    
69099 FORMAT("Information is based on the mech.def file." /
     &   "* Fall-off or pressure dependent reaction rate constants (M equals air number density):" /
     &   " * For rate constants with k<sub>o</sub>, k<sub>inf</sub>, n, F values: ",
     &       "k = [ k<sub>o</sub>M/(1+k<sub>o</sub>M/k<sub>inf</sub>)]F<sup>G</sup>, ",
     &       "where G=(1+(log<sub>10</sub>(k<sub>o</sub>M/k<sub>inf</sub>)/n)<sup>2</sup>))<sup>-1</sup> " /
     &   " * For rate constants with k<sub>1</sub>, k<sub>2</sub>: k = k<sub>1</sub> + k<sub>2</sub>M" / 
     &   " * For rate constants with k<sub>0</sub>, k<sub>2</sub>, k<sub>3</sub>: ",
     &       "k = k<sub>0</sub> + k<sub>3</sub>M/(1+k<sub>3</sub>M/k<sub>2</sub>)" /
     &   " * For rate constants with k<sub>1</sub>, k<sub>2</sub>, k<sub>3</sub>: ",
     &       "k = k<sub>1</sub> + k<sub>2</sub>M + k<sub>3</sub> " /
     & / "* For rate constants with the form A<_Reference_>, k equals A times a reference that represents photolysis rate, ", 
     &   "a heteorogeneous rate constant, rate constant for the given reaction or an operator. A equals one if not given." /
     & / "* In the mechanism definition file, the rate is formatted as" 
     & / " * A~<_HETEOROGENEOUS_>"
     & / " * A*K<_REACTION_>"
     & / " * A/<_PHOTOLYSIS_>"
     & / " * A?<_OPERATOR_>" /)
69100 FORMAT("* For the ", A, " mechanism, the operators are defined  below.")
69101 FORMAT( / "where <_REACTION_> is the rate constant for the given _REACTION_ and [_species_] ",
     &          "equals the concentration of a mechanism _species_ at the beginning of ",
     &          "the integration time-step for the chemistry's numerical solver." /
     &          "###   Reactions Table." /)

4706   FORMAT(A,1X,"<_", A,"_>")
4708   FORMAT(A,1X,ES9.2,"<_", A,"_>")
5708   FORMAT(A,1X,ES9.2)
4709   FORMAT( A )     
4710   FORMAT(A,1X,ES9.2,'*', A)
4711   FORMAT(1X)
4712   FORMAT(A, 1X, A)
5000   FORMAT(ES12.4,"<_",A,"_>")
5001   FORMAT(  "<_",A, "_>" )
5100   FORMAT(ES12.4,"<_",I4,"_>")
5101   FORMAT(  "<_",I4,"_>")
5006   FORMAT(ES12.4,"<_", A, "_>")   
5007   FORMAT(ES12.4,' ( 1.0D0 + 0.6P )')             
5011   FORMAT(ES12.4,"<_",A,"_>")             
5012   FORMAT("<_",A,"_>")
5013   FORMAT(ES12.4,"exp(",ES12.4,"/T)<_",A,"_>")             
5014   FORMAT("exp(",ES12.4,"/T)*<_",A,"_>")             
5027   FORMAT(ES12.4,"<_",A,"_>")
5028   FORMAT( "<_",A, "_>" )

5111   FORMAT(ES12.4) 
!5129   FORMAT('POWER_T02( TEMPOT300, ',ES12.4,', ', ES12.4,' )')
5129   FORMAT(ES12.4,'(T/300)<sup>(', ES12.4,')</sup>')
!5102   FORMAT('ARRHENUIS_T04( INV_TEMP,  TEMPOT300,',ES12.4,', 0.0000D+0,', ES12.4,' )')
5102   FORMAT(ES12.4,'(T/300)<sup>(', ES12.4,')</sup>')
!5103   FORMAT('ARRHENUIS_T03( INV_TEMP,',ES12.4,', ', ES12.4,' )')
5103   FORMAT(ES12.4,'e<sup>(', ES12.4,'/T)</sup>')
5104   FORMAT(ES12.4,'e<sup>(',ES12.4,'/T)</sup>(T/300)<sup>('ES12.4,' )</sup>')
5114   FORMAT(ES12.4,'P(T/300)<sup>(', ES12.4,' )</sup>')
!5114   FORMAT('ARRHENUIS_T04( INV_TEMP,  TEMPOT300,',  ES12.4,', 0.0000D+0,',
!     &        ES12.4,' )  * PRESS ')             
5115   FORMAT( ES12.4,'e<sup>(', ES12.4,'/T)</sup><', A, '>')
!5108   FORMAT('FALLOFF_T08( INV_TEMP,  CAIR, ', 3(ES12.4,', '),2(ES12.4,', '), ES12.4, ' )' )
!5109   FORMAT('FALLOFF_T09( INV_TEMP,  CAIR,', 3(ES12.4,', '), ES12.4, ' )' )
!5110   FORMAT('FALLOFF_T90( INV_TEMP,  TEMPOT300,  CAIR,', 3(ES12.4,', '), 3(ES12.4,', '), ES12.4,
!     &         ', ', ES12.4,' )')
!5119   FORMAT('FALLOFF_T91( INV_TEMP,TEMPOT300,CAIR,', 3(ES12.4,', '), 
!     &         3(ES12.4,', '), ES12.4,', ', ES12.4,' )')
!FALL 8
5108    FORMAT(' k<sub>0</sub> = ', ES12.4,'e<sup>(',ES12.4,'/T)</sup>;',
     &         ' k<sub>1</sub> = ', ES12.4,'e<sup>(',ES12.4,'/T)</sup>;',
     &         ' k<sub>3</sub> = ', ES12.4,'e<sup>(',ES12.4,'/T)</sup>')
!FALL 9
5109    FORMAT(' k<sub>0</sub> = ', ES12.4,'e<sup>(',ES12.4,'/T)</sup>;',
     &         ' k<sub>1</sub> = ', ES12.4,'e<sup>(',ES12.4,'/T)</sup>')
! FALL 10
5110    FORMAT(' k<sub>o</sub> = ', ES12.4,'e<sup>(',ES12.4,'/T)</sup>(T/300)<sup>',ES12.4,'</sup>;',
     &         ' k<sub>inf</sub> = ', ES12.4,'e<sup>(',ES12.4,'/T)</sup>(T/300)<sup>',ES12.4,'</sup>;',
     &         ' n = ', ES12.4,'; F = ', ES12.4 )
!FALL 11
5119    FORMAT( ' k<sub>0</sub> = ', ES12.4,'e<sup>(',ES12.4,'/T)</sup>(T/300)<sup>',ES12.4,'</sup>;',
     &          ' k<sub>2</sub> = ', ES12.4,'e<sup>(',ES12.4,'/T)</sup>(T/300)<sup>',ES12.4,'</sup>;',
     &          ' k<sub>3</sub> = ', ES12.4,'e<sup>(',ES12.4,'/T)</sup>')
5120   FORMAT('SEAWATER*min(', ES10.3,'e<sup>(',ES10.3'P),</sup> +', ES10.3,'e<sup>(',ES10.3'P),</sup>, ',ES10.3')')

5121   FORMAT(  '|Reaction Number|Reaction Label|Reaction|Rate Constant Formula|Value at ',F6.2,' K; ',
     &           ES12.4,' molec/cm<sup>3</sup>; ', F6.2,' Atm.',
     &          '|Value at ',F6.2,' K; ',ES12.4,' molec/cm<sup>3</sup>; ', F6.2,' Atm.|Notes|References|',
     &        / '|:--------------|:------------:|:------------:|:------------:|:------------:',
     &          '|:-------------:|:-------------|:-------------|')

95100  FORMAT(2X,A16,' = 0.0D0')        


       END SUBROUTINE WRT_MD_TABLE          
       SUBROUTINE CALCULATE_RATES( NREACTIONS )

         USE MECHANISM_DATA

         IMPLICIT NONE

         INTEGER,         INTENT( IN ) :: NREACTIONS ! number of reactions

         REAL( 8 ), PARAMETER :: ONE_OVER_300 = 1.0D0/300.0D0
         
         CHARACTER( 80 ) :: MSG     ! Mesaage text for output log

         INTEGER         :: IOS         ! status
         INTEGER         :: IDX,NXX, N  ! loop counters

         REAL( 8 )       :: ONE_OTEMP( NUMB_POINTS )
         REAL( 8 )       :: TEMPOT300( NUMB_POINTS )
         
         LOGICAL, SAVE   :: CALCULATED = .FALSE.

          IF( CALCULATED )RETURN
          
          CALCULATED = .TRUE.

          IF( .NOT. ALLOCATED( RATE_CONSTANT ) )THEN
              ALLOCATE( RATE_CONSTANT( NUMB_POINTS, NREACTIONS ), STAT = IOS )
              IF ( IOS .NE. 0 ) THEN
                  MSG = 'In CALCULATE_RATES: ERROR allocating RATE_CONSTANT'
                  WRITE(6,'(A)')MSG 
                  STOP
              END IF
              RATE_CONSTANT = 0.0D0
          END IF

          ONE_OTEMP = 1.0D0 / TEMP
          TEMPOT300 = TEMP * ONE_OVER_300

          IF( NRATE_STRING .GT. 0 )CALL EVALUATE_STRING_RATES()


         DO NXX = 1, NREACTIONS  
            DO N = 1, NUMB_POINTS
          	SELECT CASE( KTYPE( NXX ) )
          	 CASE( -1 )  ! set heteorogeneous rate constants to zero
                      RATE_CONSTANT( N,NXX ) = 0.0D0
          	 CASE(  0 )  ! set photolysis rate constants to zero
                      RATE_CONSTANT( N,NXX ) = 0.0D0
          	 CASE( 1 )
          	      RATE_CONSTANT( N,NXX ) = RTDAT(1, NXX)
          	 CASE( 2 )
          	      RATE_CONSTANT( N,NXX ) = POWER_T02( TEMPOT300( N ), RTDAT(1, NXX), RTDAT(2, NXX))
          	 CASE( 3 )
         	      RATE_CONSTANT( N,NXX ) = ARRHENUIS_T03( ONE_OTEMP( N ), RTDAT(1, NXX), RTDAT(3, NXX) )
          	 CASE( 4 )
         	    RATE_CONSTANT( N,NXX ) = ARRHENUIS_T04( ONE_OTEMP( N ),  TEMPOT300( N ), 
     &                                       RTDAT(1, NXX), RTDAT(3, NXX), RTDAT(2, NXX) )
          	 CASE( 7 )
          	    IF( RTDAT(1, NXX) .NE. 0.0 )THEN
            	        RATE_CONSTANT( N,NXX ) = ARRHENUIS_T04( ONE_OTEMP( N ),  TEMPOT300( N ),
     &                                           RTDAT(1, NXX),0.0D+0,RTDAT(2, NXX))
     &                                         * PRES( N )                        
          	    ELSE
            	        RATE_CONSTANT( N,NXX ) = RTDAT(1, NXX)*( 1.0D0 + 0.6D0*PRES( N ) )
          	    END IF
          	 CASE( 8 )
          	    DO IDX = 1, NFALLOFF
          	       IF( IRRFALL( IDX ) .EQ. NXX )EXIT
          	    END DO
            	    RATE_CONSTANT( N,NXX ) = FALLOFF_T08( ONE_OTEMP( N ),CAIR( N ), 
     &                                       RTDAT(1,NXX),RTDAT(2,NXX), RTDAT(3,NXX),
     &    	                             RFDAT(1,IDX),RFDAT(2,IDX),RFDAT(3,IDX))
          	 CASE( 9 )
          	    DO IDX = 1, NFALLOFF
          	       IF( IRRFALL( IDX ) .EQ. NXX )EXIT
          	    END DO
          	    IF( RFDAT( 2, IDX ) .EQ. 0.0 .AND. RFDAT( 3, IDX ) .EQ. 0.0 )THEN
            	        RATE_CONSTANT( N,NXX ) = FALLOFF_T09( ONE_OTEMP( N ),CAIR( N ), 
     &                                           RTDAT(1,NXX),RTDAT(2,NXX),
     &    		                         RTDAT(3,NXX),RFDAT(1,IDX))
          	    ELSE 
            	       RATE_CONSTANT( N,NXX ) = FALLOFF_T91( ONE_OTEMP( N ),TEMPOT300( N ),CAIR( N ), 
     &                                          RTDAT(1,NXX),RFDAT(2, IDX),RTDAT(2,NXX),
     &                                          RTDAT(3,NXX),RFDAT(3, IDX),RFDAT(1,IDX),
     &                                          RFDAT(4, IDX),RFDAT(5, IDX))
          	     END IF 
          	 CASE( 10 )
          	    DO IDX = 1, NFALLOFF
          	       IF( IRRFALL( IDX ) .EQ. NXX )EXIT
          	    END DO
            	    RATE_CONSTANT( N,NXX ) = FALLOFF_T90( ONE_OTEMP( N ),TEMPOT300( N ),CAIR( N ), 
     &                                       RTDAT(1,NXX),RTDAT(3,NXX),RTDAT(2,NXX),
     &    	                             RFDAT(1,IDX),RFDAT(3,IDX),RFDAT(2,IDX),
     &                                       RFDAT(5,IDX),RFDAT(4,IDX) )
          	 CASE( 11 ) ! set special rate expressions to zero
                      RATE_CONSTANT( N,NXX ) = 0.0D0
          	 CASE( 12 )
          	    DO IDX = 1, NFALLOFF
          	       IF( IRRFALL( IDX ) .EQ. NXX )EXIT
          	    END DO
            	    RATE_CONSTANT( N,NXX ) = HALOGEN_FALLOFF( PRES( N ), RTDAT(1, NXX ),
     &                                       RFDAT(1, IDX), RTDAT(2, NXX ), RFDAT(2, IDX), RTDAT(3, NXX))
                CASE( 13 )
                    DO IDX = 1, NRATE_STRING
                       IF( KSTRING( IDX ) .EQ. NXX )EXIT
                    END DO
                    IF( IDX .GT. 0 .AND. IDX .LE. NRATE_STRING )THEN
                        RATE_CONSTANT( N,NXX ) = STRING_CONSTANT( IDX, N )
                    ELSE
                        WRITE(6,'(A, /, A)' )'ERROR: ' // TRIM( RXLABEL(NXX) ) 
     &                  // ' does not use any function defined in the FUNCTIONS block, ',
     &                     ' but its rate constant is type 13. Try moving its string to the block'
                        STOP
                    END IF
                    IF( RATE_CONSTANT( N,NXX ) .LT. 0.0D0 )THEN
                        WRITE(6,99953)RXLABEL(NXX)
                    END IF
          	 END SELECT
            END DO 
         END DO 
! calculate rate constant that reference other rate constants
         DO NXX = 1, NREACTIONS  
            DO N = 1, NUMB_POINTS
          	SELECT CASE( KTYPE( NXX ) )
          	 CASE( 5 )
          	    IDX = INT( RTDAT( 3, NXX) )
         	    RATE_CONSTANT( N,NXX ) = RATE_CONSTANT( N,IDX ) 
     &                                     * EXP( - RTDAT(2, NXX ) * ONE_OTEMP( N ) )
     &                                     /  RTDAT( 1, NXX )
          	 CASE( 6 )
          	    IDX = INT( RTDAT( 2, NXX) )
          	    IF( RTDAT( 1, NXX ) .NE. 1.0 )THEN
              	        RATE_CONSTANT( N,NXX ) = RTDAT( 1, NXX )*RATE_CONSTANT( N,IDX )
          	    ELSE
            	        RATE_CONSTANT( N,NXX ) = RATE_CONSTANT( N,IDX )
         	    END IF
          	 END SELECT
            END DO 
         END DO 
! write out values
         IF( .NOT. WRITEOUT_RCONST ) RETURN
          
         WRITE(6,'(A)',ADVANCE='NO')'Rate constants, '
         IF( KUNITS .EQ. 1 )THEN
             WRITE(6,'(A)')'Units correspond to ppmV and seconds '
         ELSE IF( KUNITS .EQ. 2 )THEN
             WRITE(6,'(A)')'Units correspond to molecules/cm3 and seconds'
         END IF
         WRITE(6,99949)
         DO N = 1, NUMB_POINTS
           WRITE(6,99950)TEMP( N ),PRES( N ),CAIR( N )
         END DO
         WRITE(6,99951)
         WRITE(6,99952)
         DO NXX = 1, NREACTIONS
            WRITE(6,'(A16,1X,2(ES12.3,1X))')RXLABEL(NXX),RATE_CONSTANT( 1,NXX ),RATE_CONSTANT( 2,NXX )
         END DO  
99949    FORMAT('Value at ')
99950    FORMAT('T = ',F9.4,' K; P = ',F6.3,' Atm.; [M] = ',ES12.3,' molecules/cm3')
99951    FORMAT(/ 'Note that values are set to zero for photolysis, heteorogeneous and species reactions ')
99952    FORMAT('because they depend on atmospheric state, radiation or predicted species concentration' /)
99953    FORMAT('Warning: CHEMMECH failed to evaluate rate constant for reaction: ', A16,'.',
     &           /, 'Check Mechanism Definition for possbile FORTRAN syntax errors.' )
        END SUBROUTINE CALCULATE_RATES
        SUBROUTINE EVALUATE_STRING_RATES()

           USE MECHANISM_DATA
           USE EVALUATOR

           IMPLICIT NONE

           ! Local:
           CHARACTER(  16 ) :: NAMCONSTS( MAXCONSTS ) = (/
     &                        'ATM_AIR         ',
     &                        'ATM_O2          ',
     &                        'ATM_N2          ',
     &                        'ATM_H2          ',
     &                        'ATM_CH4         ' /)

           CHARACTER( 80 ) :: MSG     ! Mesaage text for output log

           REAL( 8 ) :: O2  ! concentration [molec/cm^3] 
           REAL( 8 ) :: N2  ! concentration [molec/cm^3] 
           REAL( 8 ) :: H2  ! concentration [molec/cm^3] 
           REAL( 8 ) :: CH4 ! concentration [molec/cm^3] 

           INTEGER         :: IOS         ! status
           INTEGER         :: IDX,NXX, N  ! loop counters
           INTEGER         :: NVARIABLES
           INTEGER         :: NOPERATIONS
           INTEGER         :: INDX_AIR
           INTEGER         :: INDX_O2
           INTEGER         :: INDX_N2
           INTEGER         :: INDX_H2
           INTEGER         :: INDX_CH4


           LOGICAL, SAVE :: CALCULATED

           CHARACTER( 16), ALLOCATABLE :: VARIABLE( : )
           CHARACTER(500), ALLOCATABLE :: EXPRESSION( : )
           CHARACTER(500)              :: EXPRESSCP

           REAL( 8 ) :: O2_FRACTION  = 0.0D0
           REAL( 8 ) :: N2_FRACTION  = 0.0D0
           REAL( 8 ) :: H2_FRACTION  = 0.0D0
           REAL( 8 ) :: CH4_FRACTION = 0.0D0 

           REAL( 8 ),     ALLOCATABLE :: VALUE( : )
           REAL( 8 )                  :: RESULT( 1 )

!Function: 
           INTEGER, EXTERNAL :: INDEX1

          IF( CALCULATED )RETURN
          
          CALCULATED = .TRUE.

          IF( .NOT. ALLOCATED( STRING_CONSTANT ) )THEN
              ALLOCATE( STRING_CONSTANT( NRATE_STRING,NUMB_POINTS ), STAT = IOS )
              IF ( IOS .NE. 0 ) THEN
                  MSG = 'In EVALUATE_STRING_RATES: ERROR allocating STRING_CONSTANT'
                  WRITE(6,'(A)')MSG 
                  STOP
              END IF
              STRING_CONSTANT = 0.0D0
          END IF
         
         NVARIABLES  = 8 + NFUNCTIONS + NRATE_STRING
         NOPERATIONS = NFUNCTIONS + NRATE_STRING

         ALLOCATE( VARIABLE( NVARIABLES ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
             MSG = 'In EVALUATE_STRING_RATES: ERROR allocating VARIABLE'
             WRITE(6,'(A)')MSG 
             STOP
             VARIABLE = ' '
         END IF

         ALLOCATE( EXPRESSION( NOPERATIONS ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
             MSG = 'In EVALUATE_STRING_RATES: ERROR allocating EXPRESSION'
             WRITE(6,'(A)')MSG 
             STOP
             EXPRESSION = ' '
         END IF

         ALLOCATE( VALUE( NVARIABLES ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
             MSG = 'In EVALUATE_STRING_RATES: ERROR allocating VALUE'
             WRITE(6,'(A)')MSG 
             STOP
         END IF

         VARIABLE( 1 ) = 'Temp'
         VARIABLE( 2 ) = 'Pres'
         VARIABLE( 3 ) = 'M'
         VARIABLE( 4 ) = 'O2'
         VARIABLE( 5 ) = 'N2'
         VARIABLE( 6 ) = 'H2'
         VARIABLE( 7 ) = 'CH4'
         VARIABLE( 8 ) = 'H2O'



         NXX  = 0
         IDX  = 8
         DO N = 1, NFUNCTIONS
            IDX = IDX + 1
            VARIABLE( IDX )  = FUNCTIONS( N )
            NXX = NXX + 1
            EXPRESSION( NXX ) = FORMULA( N )
         END DO

         DO N = 1, NRATE_STRING
            IDX = IDX + 1
            WRITE(VARIABLE(IDX),'(A,I5.5)')'SR', N
            NXX = NXX + 1
            EXPRESSION( NXX )  = RATE_STRING( N )
         END DO

         INDX_AIR  = INDEX1 ( 'ATM_AIR', MAXCONSTS, NAMCONSTS )
         INDX_H2   = INDEX1 ( 'ATM_H2', MAXCONSTS, NAMCONSTS )
         INDX_N2   = INDEX1 ( 'ATM_N2', MAXCONSTS, NAMCONSTS )
         INDX_O2   = INDEX1 ( 'ATM_O2', MAXCONSTS, NAMCONSTS )
         INDX_CH4  = INDEX1 ( 'ATM_CH4', MAXCONSTS, NAMCONSTS )
        
         O2_FRACTION  = CONST( INDX_O2 ) / CONST( INDX_AIR )
         N2_FRACTION  = CONST( INDX_N2 ) / CONST( INDX_AIR )
         H2_FRACTION  = CONST( INDX_H2 ) / CONST( INDX_AIR )
         CH4_FRACTION = CONST( INDX_CH4 ) / CONST( INDX_AIR )
   
         DO N = 1, NUMB_POINTS           
! Calculate constant atmospheric species 
            VALUE( 1 )  = TEMP( N )
            VALUE( 2 )  = PRES( N )
            VALUE( 3 )  = CAIR( N )
            VALUE( 4 )  = O2_FRACTION * CAIR( N )
            VALUE( 5 )  = N2_FRACTION * CAIR( N )
            VALUE( 6 )  = H2_FRACTION * CAIR( N )
            VALUE( 7 )  = CH4_FRACTION * CAIR( N )
            VALUE( 8 )  = 0.0D0 ! H2O
            VALUE(9:NOPERATIONS) = 0.0D0
!            DO IDX = 1, 8
!               WRITE(6,'(I5,A16,ES12.4, A)')IDX,
!     &         TRIM(VARIABLE(IDX)) // ' = ', VALUE(IDX) 
!            END DO
            IDX = NVARIABLES - NOPERATIONS
            DO NXX = 1, NOPERATIONS
               EXPRESSCP = EXPRESSION(NXX)
               EXPRESSCP = REPLACE_TEXT(EXPRESSCP,'TEMP','Temp')
               EXPRESSCP = REPLACE_TEXT(EXPRESSCP,'PRES','Pres')
               CALL EVALUATE(EXPRESSCP,NVARIABLES,VARIABLE,VALUE,RESULT)
               IDX = IDX + 1
               VALUE(IDX) = RESULT(1) 
               WRITE(6,'(I5,A16,ES12.4, A)')IDX,
     &         TRIM(VARIABLE(NXX+8)) // ' = ', RESULT(1), TRIM(EXPRESSCP) 
            END DO
            DO NXX = 1, NRATE_STRING
               IDX = NFUNCTIONS + 8
               STRING_CONSTANT( NXX, N ) = VALUE( NXX + IDX ) 
!               WRITE(6,'(I5,A16,ES12.4, A)')NXX + IDX,
!     &         TRIM(VARIABLE(NXX + IDX)) // ' = ', VALUE(NXX + IDX) 
            END DO 
          END DO 

        END SUBROUTINE EVALUATE_STRING_RATES 
        FUNCTION Replace_Text (s,text,rep)  RESULT(outs)
           Implicit None

           CHARACTER*(*), Intent( In ) :: s,text,rep
           CHARACTER(LEN(S))          :: outs     ! provide outs with extra 100 char len
           INTEGER                    :: i, nt, nr

           outs = s
           nt = LEN_TRIM(text)
           nr = LEN_TRIM(rep)
           DO
              i = INDEX(outs,text(:nt)) 
              IF (i .Eq. 0) EXIT
              outs = outs(:i-1) // rep(:nr) // outs(i+nt:)
           END DO
        END FUNCTION Replace_Text
       REAL( 8 ) FUNCTION POWER_T02( TEMPOT300,A0,B0 )
         IMPLICIT NONE
! rate constant for CMAQ Arrhenuis reaction type 2
! Arguements:
         REAL( 8 ), INTENT( IN ) :: TEMPOT300
         REAL( 8 ), INTENT( IN ) :: A0
         REAL( 8 ), INTENT( IN ) :: B0
         ! Local: None
         POWER_T02 =  A0 * TEMPOT300**B0
         RETURN
       END FUNCTION POWER_T02
       REAL( 8 ) FUNCTION ARRHENUIS_T04( INV_TEMP,TEMPOT300,A0,B0,C0 )
         IMPLICIT NONE
! rate constant for CMAQ Arrhenuis reaction type 4
! Arguements:
         REAL( 8 ), INTENT( IN ) :: INV_TEMP
         REAL( 8 ), INTENT( IN ) :: TEMPOT300
         REAL( 8 ), INTENT( IN ) :: A0
         REAL( 8 ), INTENT( IN ) :: B0
         REAL( 8 ), INTENT( IN ) :: C0
         ! Local:
         INTRINSIC DEXP
         ARRHENUIS_T04 =  A0 * DEXP( B0 * INV_TEMP ) * TEMPOT300**C0
         RETURN
       END FUNCTION ARRHENUIS_T04
       REAL( 8 ) FUNCTION ARRHENUIS_T03( INV_TEMP,A0,B0 )
! rate constant for CMAQ Arrhenuis reaction type 3
         IMPLICIT NONE
! Arguements:
         REAL( 8 ),   INTENT( IN ) ::  INV_TEMP
         REAL( 8 ),     INTENT(IN) ::  A0
         REAL( 8 ),     INTENT(IN) ::  B0
         ! Local:
         INTRINSIC DEXP
         ARRHENUIS_T03 =  A0 * DEXP( B0 * INV_TEMP )
         RETURN
       END FUNCTION ARRHENUIS_T03 
       REAL( 8 ) FUNCTION FALLOFF_T08(INV_TEMP,CAIR,A0,C0,A2,C2,A3,C3)
! rate constant for CMAQ fall off reaction type 8
         IMPLICIT NONE
! Arguements:
         REAL( 8 ), INTENT( IN ) :: INV_TEMP
         REAL( 8 ), INTENT( IN ) :: CAIR
         REAL( 8 ), INTENT( IN ) :: A0
         REAL( 8 ), INTENT( IN ) :: C0
         REAL( 8 ), INTENT( IN ) :: A2
         REAL( 8 ), INTENT( IN ) :: C2
         REAL( 8 ), INTENT( IN ) :: A3
         REAL( 8 ), INTENT( IN ) :: C3
         ! Local:
         REAL( 8 ) K0
         REAL( 8 ) K2
         REAL( 8 ) K3
         INTRINSIC DEXP
         K0 = A0 * DEXP( C0 * INV_TEMP )
         K2 = A2 * DEXP( C2 * INV_TEMP )
         K3 = A3 * DEXP( C3 * INV_TEMP )
         K3 = K3 * CAIR
         FALLOFF_T08 = K0 + K3/( 1.0D0 + K3/K2 )
         RETURN
       END FUNCTION FALLOFF_T08
       REAL( 8 ) FUNCTION FALLOFF_T09(INV_TEMP,CAIR,A1,C1,A2,C2)
! rate constant for CMAQ fall off reaction type 9
         IMPLICIT NONE
! Arguements:
         REAL( 8 ), INTENT( IN ) :: INV_TEMP
         REAL( 8 ), INTENT( IN ) :: CAIR
         REAL( 8 ), INTENT( IN ) :: A1
         REAL( 8 ), INTENT( IN ) :: C1
         REAL( 8 ), INTENT( IN ) :: A2
         REAL( 8 ), INTENT( IN ) :: C2
         !  Local:
         REAL( 8 ) K1
         REAL( 8 ) K2
         INTRINSIC DEXP
         K1 = A1 * DEXP( C1 * INV_TEMP )
         K2 = A2 * DEXP( C2 * INV_TEMP )
         FALLOFF_T09 = K1 + K2 * CAIR
         RETURN
       END FUNCTION FALLOFF_T09
       REAL( 8 ) FUNCTION FALLOFF_T90(INV_TEMP,TEMPOT300,CAIR,A0,B0,C0,A1,B1,C1,CE,CF)
         IMPLICIT NONE
! rate constant for CMAQ fall off reaction type 10
! Arguements:
         REAL( 8 ), INTENT( IN ) :: INV_TEMP
         REAL( 8 ), INTENT( IN ) :: TEMPOT300
         REAL( 8 ), INTENT( IN ) :: CAIR
         REAL( 8 ), INTENT( IN ) :: A0
         REAL( 8 ), INTENT( IN ) :: B0
         REAL( 8 ), INTENT( IN ) :: C0
         REAL( 8 ), INTENT( IN ) :: A1
         REAL( 8 ), INTENT( IN ) :: B1
         REAL( 8 ), INTENT( IN ) :: C1
         REAL( 8 ), INTENT( IN ) :: CE
         REAL( 8 ), INTENT( IN ) :: CF
         ! Local:
         REAL( 8 ) K0
         REAL( 8 ) K1
         REAL( 8 ) KEND
         K0 = A0 * CAIR * DEXP(B0*INV_TEMP)* TEMPOT300**C0
         K1 = A1 * DEXP(B1*INV_TEMP) * TEMPOT300**C1
         KEND = ( ( 1.0D0 + ( ( 1.0D0 / CE ) * DLOG10( K0 / K1 ) ) ** 2.0D0 ) )
         KEND = 1.0D0 / KEND
         FALLOFF_T90 = ( K0 / ( 1.0D0 + K0/K1 ) ) * CF ** KEND
         RETURN
       END FUNCTION FALLOFF_T90
       REAL( 8 ) FUNCTION FALLOFF_T91(INV_TEMP,TEMPOT300,CAIR,A1,B1,C1,A2, B2, C2, D1, D2)
! rate constant for CMAQ fall off reaction type 11
! actually expanded form of type 9
         IMPLICIT NONE
! Arguements:
         REAL( 8 ), INTENT( IN ) :: INV_TEMP
         REAL( 8 ), INTENT( IN ) :: TEMPOT300
         REAL( 8 ), INTENT( IN ) :: CAIR
         REAL( 8 ), INTENT( IN ) :: A1
         REAL( 8 ), INTENT( IN ) :: B1
         REAL( 8 ), INTENT( IN ) :: C1
         REAL( 8 ), INTENT( IN ) :: A2
         REAL( 8 ), INTENT( IN ) :: B2
         REAL( 8 ), INTENT( IN ) :: C2
         REAL( 8 ), INTENT( IN ) :: D1
         REAL( 8 ), INTENT( IN ) :: D2
         !  Local:
         REAL( 8 ) K1
         REAL( 8 ) K2
         REAL( 8 ) K3
         INTRINSIC DEXP
         K1 = A1 * DEXP( C1 * INV_TEMP ) * TEMPOT300**B1
         K2 = A2 * DEXP( C2 * INV_TEMP ) * TEMPOT300**B2
         K3 = D1 * DEXP( D2 * INV_TEMP )
         FALLOFF_T91 = K1 + K2 * CAIR + K3
         RETURN
       END FUNCTION FALLOFF_T91
       REAL( 8 ) FUNCTION HALOGEN_FALLOFF(PRESS,A1,B1,A2,B2,A3)
         IMPLICIT NONE
         REAL( 8 ), INTENT( IN ) :: PRESS
         REAL( 8 ), INTENT( IN ) :: A1
         REAL( 8 ), INTENT( IN ) :: B1
         REAL( 8 ), INTENT( IN ) :: A2
         REAL( 8 ), INTENT( IN ) :: B2
         REAL( 8 ), INTENT( IN ) :: A3 ! Maximum loss rate (1/sec)

         INTRINSIC DEXP
         HALOGEN_FALLOFF = A1 * DEXP( B1 * PRESS ) + A2 * DEXP( B2 * PRESS )
         HALOGEN_FALLOFF = DMIN1 (A3, HALOGEN_FALLOFF )
         RETURN
       END FUNCTION HALOGEN_FALLOFF

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE WRT_CSV_TABLE( NR, IP, LABEL, NS  )

      USE GET_ENV_VARS
      USE MECHANISM_DATA
      
      IMPLICIT NONE

      INTEGER,         INTENT( IN ) :: NR ! number of reactions
      INTEGER,         INTENT( IN ) :: IP ! number of photolysis reaction
      CHARACTER( 16 ), INTENT( IN ) :: LABEL( :,: ) ! LABEL(NXX,1) 1st label found in rx NXX
                                                            ! LABEL(NXX,2) 2nd label found in rx NXX
      INTEGER,         INTENT( IN ) :: NS ! number of species

c..local Variables for steady-state species

       
      CHARACTER(  1 ) :: CHR
      CHARACTER( 16 ) :: WORD
      CHARACTER( 37 ) :: PHRASE
      CHARACTER( 81 ) :: INBUF
      CHARACTER( 16 )  :: WIKI_OUT_FILE = 'WIKI_OUT_FILE'
      CHARACTER( 627 ) :: FWIKI_OUT_FILE

      INTEGER, EXTERNAL :: INDEX1
      INTEGER            :: LPOINT, IEOL
      INTEGER            :: I, ICOL, ISPC, IRX, IDX
      INTEGER            :: NXX, IPR, IPHOTAB, NC
      INTEGER            :: DUMMY_COEF( MAXRXNUM )               ! Yields for the DUMMY variable in each reaction
      INTEGER            :: SS1RX( MAXNLIST )                    ! First reaction occurrence for each SS species
      
c..Variables for species to be dropped from mechanism
      INTEGER         :: N_DROP_SPC = 0
      CHARACTER( 16 ) :: DROP_SPC( MAXNLIST )
      LOGICAL         :: LERROR
      LOGICAL         :: KPP_DUMMY   = .FALSE.
      LOGICAL         :: FIRST_TERM  = .TRUE.
      REAL( 8 )       :: WREXT_COEFFS( MAXSPECTERMS)
      INTEGER         :: WREXT_INDEX(  MAXSPECTERMS)
      INTEGER         :: TABLE_UNIT

      INTEGER SPC1RX( MAXSPEC )              ! rx index of 1st occurence of species
                                             ! in mechanism table
      CHARACTER( 120 ) :: WIKI_TABLE
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
      INTEGER             :: IDIFF_ORDER           ! difference between order of two separate reactions
      LOGICAL             :: FALLOFF_RATE       ! whether a reaction is a falloff type


      CHARACTER(  12 ) :: EXFLNM_SPCS = 'SPCSDATX'
      CHARACTER(  12 ) :: EXFLNM_RXDT = 'RXNSDATX'
      CHARACTER(  12 ) :: EXFLNM_RXCM = 'RXNSCOMX'

      INTEGER, EXTERNAL :: JUNIT
      INTEGER            :: ICOUNT, IREACT, IPRODUCT



cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Create name for output file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  
      FWIKI_OUT_FILE = TRIM( OUTDIR ) // '/mech_' // 
     &                 TRIM( MECHNAME_LOWER_CASE )  // '.csv'

      CALL CALCULATE_RATES( NR )

      IF( .NOT. ALLOCATED( IOLD2NEW ) )THEN
         ALLOCATE( IOLD2NEW( NUMB_MECH_SPCS ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = 'ERROR IOLD2NEW'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF
         DO I = 1, NUMB_MECH_SPCS
            IOLD2NEW( I ) = I
         END DO
      END IF
! write out reactions strings to determine mechanism information

       DO NXX = 1, NR
         DO IREACT = 1, NREACT( NXX )
            ISPC = IRR( NXX, IREACT )
               IF( IREACT .LE. 1 )THEN
                  REACTION_STR( NXX ) =  TRIM(SPARSE_SPECIES( ISPC )) // ' '
               ELSE
                  REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) //  ' + ' // TRIM(SPARSE_SPECIES( ISPC )) // ' '
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
         REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) //  ' ----> '
! write out products
         DO IPRODUCT = 1, NPRDCT( NXX )
            ISPC = IRR( NXX, IPRODUCT + 3 )
            IF ( ABS( SC( NXX,IPRODUCT ) ) .NE. 1.0 ) THEN
               IF ( SC( NXX,IPRODUCT ) .LT. 0 ) THEN
                  WRITE(COEFF_STR,'(A,F8.5)')' - ',ABS(SC( NXX,IPRODUCT ))
                  REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) // TRIM(COEFF_STR) 
     &                        // ' * '  // TRIM(SPARSE_SPECIES( ISPC ))
               ELSE
                  WRITE(COEFF_STR,'(F8.5)')SC( NXX,IPRODUCT )
                  IF( IPRODUCT .EQ. 1 )THEN
                     REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) // ' ' // TRIM(COEFF_STR) 
     &                           // ' * '  // TRIM(SPARSE_SPECIES( ISPC ))
                  ELSE
                     REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) // ' + ' // TRIM(COEFF_STR) 
     &                           // ' * '  // TRIM(SPARSE_SPECIES( ISPC ))
                  END IF
               END IF
            ELSE 
               IF ( SC( NXX,IPRODUCT ) .LT. 0.0 ) THEN
                   REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) //  ' - ' // TRIM(SPARSE_SPECIES( ISPC ))
               ELSE
                  IF( IPRODUCT .EQ. 1 )THEN
                    REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) // ' ' //  TRIM(SPARSE_SPECIES( ISPC ))
                  ELSE
                    REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) //  ' + ' // TRIM(SPARSE_SPECIES( ISPC ))
                  END IF
               END IF
            END IF
         END DO
         
       END DO

! create wiki table file      
      TABLE_UNIT = JUNIT()
      OPEN ( UNIT = TABLE_UNIT, FILE = FWIKI_OUT_FILE, STATUS = 'UNKNOWN'  )
! 
      WRITE(TABLE_UNIT, 69099) 
      PHRASE = ' '
      PHRASE(1:32) = MECHNAME(1:32)
      CALL CONVERT_CASE(PHRASE, .FALSE.)
      IF( NSPECIAL .GT. 0 )WRITE(TABLE_UNIT,69100)TRIM(PHRASE)

      DO NXX = 1, NSPECIAL
         WRITE(TABLE_UNIT,'(A,A)', ADVANCE = 'NO' )' * ',TRIM(SPECIAL( NXX ) )
         FIRST_TERM = .TRUE.
! first write standard rate constants time concentrations
         DO IREACT = 1, MAXSPECTERMS
             IRX  = INDEX_KTERM( NXX, IREACT )
             IF( IRX .LT. 1 .AND. INDEX_CTERM( NXX, IREACT ) .LT. 1 )CYCLE
             IF( FIRST_TERM )THEN
                PHRASE = ' = '
                FIRST_TERM = .FALSE.
                IF(KC_COEFFS( NXX, IREACT ) .LT. 0.0 )PHRASE = ' = ' // ' - '
             ELSE
!                WRITE(TABLE_UNIT, 4711, ADVANCE = 'NO' )
                PHRASE = ' + '
                IF(KC_COEFFS( NXX, IREACT ) .LT. 0.0 )PHRASE = ' - '
             END IF
             IF( KC_COEFFS( NXX, IREACT ) .NE. 1.0 )THEN
                 IF( IRX .GT. 0 )THEN
                    WRITE(TABLE_UNIT, 4708, ADVANCE = 'NO')TRIM(PHRASE),
     &              REAL( ABS( KC_COEFFS( NXX, IREACT ) ), 8),TRIM(LABEL(IRX,1))
                 ELSE
                    WRITE(TABLE_UNIT, 5708, ADVANCE = 'NO')TRIM(PHRASE),
     &              REAL( ABS( KC_COEFFS( NXX, IREACT ) ), 8)
                 END IF
             ELSE
                 IF( IRX .GT. 0 )THEN
                    WRITE(TABLE_UNIT, 4706, ADVANCE = 'NO')TRIM(PHRASE),TRIM(LABEL(IRX,1))
                 ELSE
                    WRITE(TABLE_UNIT, 4709, ADVANCE = 'NO')TRIM(PHRASE)
                 END IF
             END IF
             IF( INDEX_CTERM( NXX, IREACT ) .LT. 1  )CYCLE
             ISPC = IOLD2NEW( INDEX_CTERM( NXX, IREACT ) )
             IF( IRX .GT. 0 )THEN
                PHRASE = '[' // TRIM( SPARSE_SPECIES( ISPC ) ) // ']'
             ELSE
                PHRASE = ' [' // TRIM( SPARSE_SPECIES( ISPC ) ) // ']'
             END IF
             WRITE(TABLE_UNIT, 4709, ADVANCE = 'NO')TRIM( PHRASE )
         END DO
         IF( MAXVAL( OPERATORS( NXX, 1:MAXSPECTERMS ) ) .LT. 1 )THEN
            WRITE(TABLE_UNIT, * )' '
            CYCLE
         END IF
! next write defined operators         
         DO IREACT = 1, MAXSPECTERMS
            IDX = OPERATORS( NXX, IREACT )
            IF( IDX .LT. 1 .AND. IREACT .LT. MAXSPECTERMS )THEN
                CYCLE
            ELSE IF( IDX .LT. 1 .AND. IREACT .GE. MAXSPECTERMS )THEN
                WRITE(TABLE_UNIT, * )' '
                CYCLE
            END IF
             IF( FIRST_TERM )THEN
                PHRASE = ' = '
                IF(OPERATOR_COEFFS( NXX, IREACT ) .LT. 0.0 )PHRASE = ' = ' // ' - '
                FIRST_TERM = .FALSE.
             ELSE
                WRITE(TABLE_UNIT, 4711, ADVANCE = 'NO' )
                PHRASE = ' + '
                IF(OPERATOR_COEFFS( NXX, IREACT ) .LT. 0.0 )PHRASE = ' - '
             END IF
             IF( OPERATOR_COEFFS( NXX, IREACT ) .NE. 1.0 )THEN
                 WRITE(TABLE_UNIT, 4710, ADVANCE = 'NO')TRIM(PHRASE),
     &           REAL( ABS( OPERATOR_COEFFS( NXX, IREACT ) ), 8), TRIM( SPECIAL( IDX ) )
             ELSE
                 WRITE(TABLE_UNIT, 4712, ADVANCE = 'NO')TRIM(PHRASE),TRIM( SPECIAL( IDX ) )
             END IF
             IF( IREACT .GE. MAXSPECTERMS )WRITE(TABLE_UNIT, * )' '
         END DO 
      END DO
      IF( NSPECIAL .GT. 0 )WRITE(TABLE_UNIT, 69101)

!    
      WRITE(TABLE_UNIT, 5121)(TEMP(LPOINT),CAIR(LPOINT),PRES(LPOINT),LPOINT=1,NUMB_POINTS)
      DO NXX = 1, NR
         WRITE(TABLE_UNIT,'(I5, 3A)', ADVANCE= 'NO')NXX, ', <',TRIM(LABEL( NXX,1 )),'>, '
         DO IREACT = 1, NREACT( NXX )
            ISPC = IRR( NXX, IREACT )
               IF( IREACT .LT. 2 )THEN
                  WRITE(TABLE_UNIT,'(A, A)', ADVANCE = 'NO')TRIM(SPARSE_SPECIES( ISPC )),' '
                  ICOUNT = 1 + LEN( SPARSE_SPECIES( ISPC ) )
               ELSE
                  WRITE(TABLE_UNIT,'(3A)', ADVANCE = 'NO')'+ ',TRIM(SPARSE_SPECIES( ISPC )),' '
                  ICOUNT = 1 + LEN( SPARSE_SPECIES( ISPC ) )
                  ICOUNT = 3 + LEN( SPARSE_SPECIES( ISPC ) )                  
               END IF
         END DO
         DO I = 1, MAXRCTNTS
         IF( INDEX_FIXED_SPECIES( NXX, I ) .GT. 0 .AND. INDEX_FIXED_SPECIES( NXX, I ) .LT. 7 )THEN
              ISPC = INDEX_FIXED_SPECIES( NXX, I  )
              WRITE(TABLE_UNIT,'(3A)', ADVANCE = 'NO')'+ ',TRIM(FIXED_SPECIES( ISPC )),' '
              ICOUNT = 3 + LEN( FIXED_SPECIES( ISPC ) )
         ELSE 
              IF( INDEX_FIXED_SPECIES( NXX, I ) .GE. 7 )THEN
                  WRITE(*,*)'WARNING: INDEX_FIXED_SPECIES( ', NXX,',', I, ') = ',INDEX_FIXED_SPECIES( NXX, I )
              END IF
         END IF    
         END DO     
         WRITE(TABLE_UNIT, '(A)', ADVANCE = 'NO' )'---->'
! write out products
         DO IPRODUCT = 1, NPRDCT( NXX )
            ISPC = IRR( NXX, IPRODUCT + 3 )
            IF ( ABS( SC( NXX,IPRODUCT ) ) .NE. 1.0 ) THEN
               IF ( SC( NXX,IPRODUCT ) .LT. 0 ) THEN
                  WRITE(TABLE_UNIT,'(A,F8.5,3A)', ADVANCE = 'NO')
     &           '- ',ABS(SC( NXX,IPRODUCT )),'*',TRIM(SPARSE_SPECIES( ISPC )),' '
                  ICOUNT = ICOUNT + 12 + LEN( SPARSE_SPECIES( ISPC ) )
               ELSE
                  IF( IPRODUCT .EQ. 1 )THEN
                     WRITE(TABLE_UNIT,'(F8.5, 3A)', ADVANCE = 'NO')
     &               SC( NXX,IPRODUCT ),'*',TRIM(SPARSE_SPECIES( ISPC )),' '
                     ICOUNT = ICOUNT + 10 + LEN( SPARSE_SPECIES( ISPC ) )
                  ELSE
                     WRITE(TABLE_UNIT,'(A,F8.5,3A)', ADVANCE = 'NO')
     &               '+ ',SC( NXX,IPRODUCT ),'*',TRIM(SPARSE_SPECIES( ISPC )),' '
                     ICOUNT = ICOUNT + 12 + LEN( SPARSE_SPECIES( ISPC ) )
                  END IF
               END IF
            ELSE IF ( SC( NXX,IPRODUCT ) .LT. 0.0 ) THEN
               WRITE(TABLE_UNIT,'(3A)', ADVANCE = 'NO')
     &               '- ',TRIM(SPARSE_SPECIES( ISPC )),' '
               ICOUNT = ICOUNT + 3 + LEN( SPARSE_SPECIES( ISPC ) )
            ELSE
               IF( IPRODUCT .EQ. 1 )THEN
                  WRITE(TABLE_UNIT,'(3A)', ADVANCE = 'NO')
     &           ' ',TRIM(SPARSE_SPECIES( ISPC )),' '
                  ICOUNT = ICOUNT + 2 + LEN( SPARSE_SPECIES( ISPC ) )
               ELSE
                  WRITE(TABLE_UNIT,'(3A)', ADVANCE = 'NO')
     &             '+ ',TRIM(SPARSE_SPECIES( ISPC )),' '
                  ICOUNT = ICOUNT + 3 + LEN( SPARSE_SPECIES( ISPC ) )
               END IF
            END IF
!            IF( ICOUNT .GT. 132 .AND. IPRODUCT .LT.  NPRDCT( NXX ) )THEN
!            IF( IPRODUCT .LT.  NPRDCT( NXX ) )THEN
!                ICOUNT = 0
!                WRITE(TABLE_UNIT, * )' '
!                WRITE(TABLE_UNIT,'(A16)', ADVANCE = 'NO')' '
!            END IF
         END DO 
         WRITE(TABLE_UNIT,'(A)', ADVANCE = 'NO')', '

         SELECT CASE( KTYPE( NXX ) )
          CASE( -1 )
             DO IPR = 1, NHETERO
                IF ( IHETERO( IPR,1 ) .EQ. NXX )EXIT
             END DO
             IDX = IHETERO( IPR, 2 )
             IF( RTDAT(1, NXX) .NE. 1.0 )THEN
                 WRITE(TABLE_UNIT,5027, ADVANCE = 'NO')REAL(RTDAT(1, NXX),8),TRIM( HETERO(IDX) )
             ELSE
                 WRITE(TABLE_UNIT,5028, ADVANCE = 'NO')TRIM( HETERO(IDX) )
             END IF
          CASE(  0 )
             DO IPR = 1, IP
                IF ( IPH( IPR,1 ) .EQ. NXX )EXIT
             END DO
             IF ( IPH( IPR,3 ) .NE. 0 )THEN
                IDX = IPH( IPR, 2 )
                IF( RTDAT(1, NXX) .NE. 1.0 )THEN
                   WRITE(TABLE_UNIT,5000, ADVANCE = 'NO')REAL(RTDAT(1, NXX),8),TRIM( PHOTAB(IDX) )
                ELSE
                   WRITE(TABLE_UNIT,5001, ADVANCE = 'NO')TRIM( PHOTAB(IDX) )
                END IF
             ELSE IF( IPH( NXX,3 ) .EQ. 0 )THEN
                IDX = IPH(IPH( NXX,2 ), 2)
                IF( RTDAT(1, NXX) .NE. 1.0 )THEN
                   WRITE(TABLE_UNIT,5100, ADVANCE = 'NO')REAL(RTDAT(1, NXX),8),TRIM(LABEL(IDX,1))
                ELSE
                   WRITE(TABLE_UNIT,5101, ADVANCE = 'NO')TRIM(LABEL(IDX,1))
                END IF
             END IF
          CASE( 1 )
             WRITE(TABLE_UNIT,'(ES12.4)', ADVANCE = 'NO')REAL(RTDAT(1, NXX), 8)
          CASE( 2 )
             WRITE(TABLE_UNIT,5129, ADVANCE = 'NO')RTDAT(1, NXX), RTDAT(2, NXX)
          CASE( 3 )
             WRITE(TABLE_UNIT,5103, ADVANCE = 'NO')RTDAT(1, NXX), RTDAT(3, NXX)
          CASE( 4 )
             WRITE(TABLE_UNIT,5104, ADVANCE = 'NO')RTDAT(1, NXX), RTDAT(3, NXX), RTDAT(2, NXX)
          CASE( 5 )
             IRX = INT( RTDAT( 3, NXX) )
             IF( IRX .GT. NXX )CYCLE
             IDIFF_ORDER = IORDER(NXX) - IORDER(IRX)
             IF( IDIFF_ORDER .NE. 0 )THEN
                 FALLOFF_RATE = ( KTYPE(IRX) .GT. 7 .AND. KTYPE(IRX) .LT. 11 )
!                 IF( KUNITS .EQ. 2 .OR. FALLOFF_RATE )THEN
!                   CALL WRITE_RATE_CONVERT_BEFORE(MODULE_UNIT, IDIFF_ORDER )
!                 END IF
             END IF
             WRITE(TABLE_UNIT,5115, ADVANCE = 'NO')1.0D0/RTDAT( 1, NXX ), -RTDAT(2, NXX ),TRIM(LABEL(IRX,1))
          CASE( 6 )
!             DO IDX = 1, KTN6
!                IF( KRX6( IDX ) .EQ. NXX )EXIT
!             END DO         
             IRX = INT( RTDAT( 2, NXX) )
	     IDIFF_ORDER = IORDER(NXX) - IORDER(IRX)
	     IF( IDIFF_ORDER .NE. 0 )THEN
	         FALLOFF_RATE = ( KTYPE(IRX) .GT. 7 .AND. KTYPE(IRX) .LT. 11 )
	     END IF
             IF( RTDAT( 1, NXX ) .NE. 1.0 )THEN
                 WRITE(TABLE_UNIT, 5006, ADVANCE = 'NO')REAL(RTDAT( 1, NXX ), 8),TRIM(LABEL(IRX,1))
             ELSE
                 WRITE(TABLE_UNIT, 4706, ADVANCE = 'NO')' ', TRIM(LABEL(IRX,1))
             END IF
          CASE( 7 )
             IF( RTDAT(1, NXX) .NE. 0.0 )THEN
                 WRITE(TABLE_UNIT,5114, ADVANCE = 'NO')REAL(RTDAT(1, NXX), 8),REAL(RTDAT(2, NXX), 8)
             ELSE
                 WRITE(TABLE_UNIT,5007, ADVANCE = 'NO')REAL(RTDAT(1, NXX), 8)
             END IF
          CASE( 8 )
             DO IDX = 1, NFALLOFF
                IF( IRRFALL( IDX ) .EQ. NXX )EXIT
             END DO
             WRITE(TABLE_UNIT,5108, ADVANCE = 'NO')RTDAT(1,NXX),(1.0*RTDAT(2,NXX)),RTDAT(3,NXX),
     &      (1.0*RFDAT(1,IDX)),RFDAT(2,IDX),(1.0*RFDAT(3,IDX))
          CASE( 9 )
             DO IDX = 1, NFALLOFF
                IF( IRRFALL( IDX ) .EQ. NXX )EXIT
             END DO
             IF( RFDAT( 2, IDX ) .EQ. 0.0 .AND. RFDAT( 3, IDX ) .EQ. 0.0 )THEN
                 WRITE(TABLE_UNIT,5109, ADVANCE = 'NO')RTDAT(1,NXX),RTDAT(2,NXX),
     &           RTDAT(3,NXX),1.0*RFDAT(1,IDX)
             ELSE 
                 WRITE(TABLE_UNIT,5119, ADVANCE = 'NO')RTDAT(1,NXX),RTDAT(2,NXX),RFDAT(2, IDX),
     &           RTDAT(3,NXX),1.0*RFDAT(1,IDX),RFDAT(3, IDX),RFDAT(4, IDX),RFDAT(5, IDX)
              END IF 
          CASE( 10 )
             DO IDX = 1, NFALLOFF
                IF( IRRFALL( IDX ) .EQ. NXX )EXIT
             END DO
             WRITE(TABLE_UNIT, 5110, ADVANCE = 'NO')RTDAT(1,NXX),RTDAT(3,NXX),RTDAT(2,NXX),
     &      RFDAT(1,IDX),RFDAT(3,IDX),RFDAT(2,IDX),RFDAT(5,IDX),RFDAT(4,IDX)
          CASE( 11 )
             DO IDX = 1, NSPECIAL_RXN
                IF( ISPECIAL( IDX, 1 ) .EQ. NXX )EXIT
             END DO
             I   = ISPECIAL( IDX, 1)
             IRX = ISPECIAL( IDX, 2)
             IF( RTDAT( 1, I) .NE. 1.0 .AND. ABS( RTDAT( 3, I ) ) .EQ. 0.0 )THEN
                WRITE(TABLE_UNIT,5011, ADVANCE = 'NO')REAL(RTDAT( 1, I),8), TRIM( SPECIAL( IRX ) )
             ELSE IF( RTDAT( 1, I) .NE. 1.0 .AND. ABS( RTDAT( 3, I ) ) .GT. 0.0 )THEN
                WRITE(TABLE_UNIT,5013, ADVANCE = 'NO')REAL(RTDAT( 1, I ), 8),REAL(RTDAT( 3, I ), 8),
     &          TRIM( SPECIAL( IRX) )
             ELSE IF( RTDAT( 1, I) .EQ. 1.0 .AND. ABS( RTDAT( 3, I ) ) .GT. 0.0 )THEN
                WRITE(TABLE_UNIT,5014, ADVANCE = 'NO')REAL(RTDAT( 3, I ), 8),
     &          TRIM( SPECIAL( IRX) )
             ELSE
                WRITE(TABLE_UNIT,5012, ADVANCE = 'NO')TRIM( SPECIAL( IRX ) )
             END IF
           CASE( 12 )
             DO IDX = 1, NFALLOFF
                IF( IRRFALL( IDX ) .EQ. NXX )EXIT
             END DO
             WRITE(TABLE_UNIT,5120, ADVANCE = 'NO')RTDAT(1, NXX ),RFDAT(1, IDX),RTDAT(2, NXX ),
     &       RFDAT(2, IDX),RTDAT(3, NXX)
          CASE( 13 )
             DO IDX = 1, NRATE_STRING
                IF( KSTRING( IDX ) .EQ. NXX )EXIT
             END DO
             WRITE(TABLE_UNIT,'(A)', ADVANCE = 'NO')TRIM( RATE_STRING( IDX ) )
          END SELECT
! write estimated rate constant 
          SELECT CASE( KTYPE( NXX ) )
             CASE( 0 )
                 WRITE(TABLE_UNIT,'(A,  A, A, A, A , A)')', ', 'Not Available',
     &           ', ', 'Not Available', ', Photolysis Reaction;depends on radiation and predicted concentrations , ,'
             CASE( -1 )
                 WRITE(TABLE_UNIT,'(A,  A, A, A, A , A)')', ', 'Not Available',
     &           ', ', 'Not Available', ', Heteorogeneous Reaction;Depends predicted concentrations , ,'
             CASE( 11 )
                 WRITE(TABLE_UNIT,'(A,  A, A, A, A , A)')', ', 'Not Available',
     &           ', ', 'Not Available', ', Rate constant an Operator;Depends predicted concentrations , ,'
             CASE( 12 )
                 WRITE(TABLE_UNIT,'(A,  ES12.4, A, ES12.4, 2A , A)')', ', RATE_CONSTANT( 1, NXX),
     &           ', ', RATE_CONSTANT( 2, NXX), 
     &           ',  Set to zero if sun is below the horizon. ',
     &           ' SEAWATER equals surface fraction covered by ice free open ocean plus surf zones.;',
     &           ' P equals air pressure in atmospheres, ,  '
             CASE( 13 )
                IF( RATE_CONSTANT( 1, NXX) .LT. 0.0 .OR. RATE_CONSTANT( 1, NXX) .LT. 0.0 )THEN
                 WRITE(TABLE_UNIT,'(A,  A, A, A, A , A)')', ', 'Not Available',
     &           ', ', 'Not Available', ', Rate constant entered as a character string;' 
     &           // 'CHEMMECH evaluator routine failed to compute value., ,'
               ELSE
                 WRITE(TABLE_UNIT,'(A,  ES12.4, A, ES12.4, A , A)')',  ', RATE_CONSTANT( 1, NXX),
     &          ',  ', RATE_CONSTANT( 2, NXX), ',  Rate constant entered as a character string, ,  '
               END IF
             CASE( 6 )
                 WRITE(TABLE_UNIT,'(A,  ES12.4, A, ES12.4, A , A)')',  ', RATE_CONSTANT( 1, NXX),
     &          ',  ', RATE_CONSTANT( 2, NXX), ',  Rate constant multiple of constant for listed reaction, ,  '
             CASE( 5 )
                 WRITE(TABLE_UNIT,'(A,  ES12.4, A, ES12.4, A , A)')',  ', RATE_CONSTANT( 1, NXX),
     &          ',  ', RATE_CONSTANT( 2, NXX), 
     &          ',  Rate constant scaled as reverse equilibrium to constant for listed reaction, , '
             CASE DEFAULT
                 WRITE(TABLE_UNIT,'(A,  ES12.4, A, ES12.4, A, A)')',  ', RATE_CONSTANT( 1, NXX),
     &          ',  ', RATE_CONSTANT( 2, NXX), ', , '
         END SELECT
      END DO

      IF( NFUNCTIONS .GT. 0 )THEN
        WRITE(TABLE_UNIT,*)
        WRITE(TABLE_UNIT,*)
        WRITE(TABLE_UNIT,'(A)')'Functions Table.'
        WRITE(TABLE_UNIT,'(3A)')'Name',',','Formula'
        DO IDX = 1, NFUNCTIONS
           WRITE(TABLE_UNIT,'(3A)')TRIM(FUNCTIONS(IDX)),',',TRIM(FORMULA(IDX))
        END DO
      END IF

      CLOSE(TABLE_UNIT)


      RETURN
      
    
69099 FORMAT("Information is based on the mech.def file." /
     &   "* Fall-off or pressure dependent reaction rate constants (M equals air number density):" /
     &   " * For rate constants with ko, kinf, n, F values: ",
     &       "k = [ ko*M/(1+ko*M/kinf)]F^G, ",
     &       "where G=1/[1+(log10(k0*M/kinf)/n)^2)] " /
     &   " * For rate constants with k1, k2: k = k1 + k2*M" / 
     &   " * For rate constants with k0, k2, k3: ",
     &       "k = k0 + k3*M/(1+k3*M/k2)" /
     &   " * For rate constants with k1, k2, k3: ",
     &       "k = k1 + k2*M + k3 " /
     & / "* For rate constants with the form A<Reference>, k equals A times a reference that represents photolysis rate, ", 
     &   "a heteorogeneous rate constant, rate constant for the given reaction or an operator. A equals one if not given." /
     & / "* In the mechanism definition file, the rate is formatted as" 
     & / " ** A~<HETEOROGENEOUS>"
     & / " ** A*K<REACTION>"
     & / " ** A/<PHOTOLYSIS>"
     & / " ** A?<OPERATOR>" /)
69100 FORMAT("* For the ", A, " mechanism, the operators are defined  below.")
69101 FORMAT( / "*where <REACTION> is the rate constant for the given REACTION and [species] ",
     &          "equals the concentration of a mechanism species at the beginning of ",
     &          "the integration time-step for the chemistry's numerical solver." /)

4706   FORMAT(A,1X,"<", A,">")
4708   FORMAT(A,1X,ES9.2,"<", A,">")
5708   FORMAT(A,1X,ES9.2,"<", A,">")
4709   FORMAT( A )     
4710   FORMAT(A,1X,ES9.2,'*', A)
4711   FORMAT(1X)
4712   FORMAT(A, 1X, A)
5000   FORMAT(ES12.4,"<",A,">")
5001   FORMAT(  "<",A, ">" )
5100   FORMAT(ES12.4,"<",I4,">")
5101   FORMAT(  "<",I4,">")
5006   FORMAT(ES12.4,"<", A, ">")   
5007   FORMAT(ES12.4,' *( 1.0D0 + 0.6D0 * P )')             
5011   FORMAT(ES12.4,"<",A,">")             
5012   FORMAT("<",A,">")
5013   FORMAT(ES12.4,"exp(",ES12.4,"/T)<",A,">")             
5014   FORMAT("exp(",ES12.4,"/T)<",A,">")             
5027   FORMAT(ES12.4,"<",A,">")
5028   FORMAT( "<",A, ">" )

5111   FORMAT(ES12.4) 
!5129   FORMAT('POWER_T02( TEMPOT300, ',ES12.4,', ', ES12.4,' )')
5129   FORMAT(ES12.4,'*(T/300)^(', ES12.4,')')
!5102   FORMAT('ARRHENUIS_T04( INV_TEMP,  TEMPOT300,',ES12.4,', 0.0000D+0,', ES12.4,' )')
5102   FORMAT(ES12.4,'*(T/300)^(', ES12.4,')')
!5103   FORMAT('ARRHENUIS_T03( INV_TEMP,',ES12.4,', ', ES12.4,' )')
5103   FORMAT(ES12.4,'*exp(', ES12.4,'/T)')
5104   FORMAT(ES12.4,'*exp(',ES12.4,'/T)*(T/300)^('ES12.4,' )')
5114   FORMAT(ES12.4,'P*(T/300)^(', ES12.4,' )')
!5114   FORMAT('ARRHENUIS_T04( INV_TEMP,  TEMPOT300,',  ES12.4,', 0.0000D+0,',
!     &        ES12.4,' )  * PRESS ')             
5115   FORMAT( ES12.4,'*exp(', ES12.4,'/T) \* ', A)
!5108   FORMAT('FALLOFF_T08( INV_TEMP,  CAIR, ', 3(ES12.4,', '),2(ES12.4,', '), ES12.4, ' )' )
!5109   FORMAT('FALLOFF_T09( INV_TEMP,  CAIR,', 3(ES12.4,', '), ES12.4, ' )' )
!5110   FORMAT('FALLOFF_T90( INV_TEMP,  TEMPOT300,  CAIR,', 3(ES12.4,', '), 3(ES12.4,', '), ES12.4,
!     &         ', ', ES12.4,' )')
!5119   FORMAT('FALLOFF_T91( INV_TEMP,TEMPOT300,CAIR,', 3(ES12.4,', '), 
!     &         3(ES12.4,', '), ES12.4,', ', ES12.4,' )')
!FALL 8
5108    FORMAT(' k0 = ', ES12.4,'*exp(',ES12.4,'/T);',
     &         ' k1 = ', ES12.4,'*exp(',ES12.4,'/T);',
     &         ' k3 = ', ES12.4,'*exp(',ES12.4,'/T)')
!FALL 9
5109    FORMAT(' k0 = ', ES12.4,'*exp(',ES12.4,'/T);',
     &         ' k1 = ', ES12.4,'*exp(',ES12.4,'/T)')
! FALL 10
5110    FORMAT(' ko = ', ES12.4,'*exp(',ES12.4,'/T)*(T/300)^',ES12.4,';',
     &         ' kinf = ', ES12.4,'*exp(',ES12.4,'/T)*(T/300)^',ES12.4,';',
     &         ' n = ', ES12.4,'; F = ', ES12.4 )
!FALL 11
5119    FORMAT( ' k0 = ', ES12.4,'*exp(',ES12.4,'/T)*(T/300)^',ES12.4,';',
     &          ' k2 = ', ES12.4,'*exp(',ES12.4,'/T)*(T/300)^',ES12.4,';',
     &          ' k3 = ', ES12.4,'*exp(',ES12.4,'/T)')
5120   FORMAT('SEAWATER*min of ', ES10.3,'*exp(',ES10.3'*P), +', ES10.3,'*exp(',ES10.3'*P) and ',ES10.3'')

5121   FORMAT(  'Reaction Number,Reaction Label,Reaction,Rate Constant Formula,Value at ',F6.2,' K; ',
     &           ES12.4,' molec/cm^3; ', F6.2,' Atm.',
     &          ',Value at ',F6.2,' K; ',ES12.4,' molec/cm^3; ', F6.2,' Atm.,Notes,References')

95100  FORMAT(2X,A16,' = 0.0D0')        


       END SUBROUTINE WRT_CSV_TABLE
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE WRT_HTML_TABLE( NR, IP, LABEL, NS  )

      USE GET_ENV_VARS
      USE MECHANISM_DATA
      
      IMPLICIT NONE

      INTEGER,         INTENT( IN ) :: NR ! number of reactions
      INTEGER,         INTENT( IN ) :: IP ! number of photolysis reaction
      CHARACTER( 16 ), INTENT( IN ) :: LABEL( :,: ) ! LABEL(NXX,1) 1st label found in rx NXX
                                                            ! LABEL(NXX,2) 2nd label found in rx NXX
      INTEGER,         INTENT( IN ) :: NS ! number of species

c..local Variables for steady-state species

       
      CHARACTER(  1 ) :: CHR
      CHARACTER( 16 ) :: WORD
      CHARACTER( 37 ) :: PHRASE
      CHARACTER( 81 ) :: INBUF
      CHARACTER( 16 )  :: WIKI_OUT_FILE = 'WIKI_OUT_FILE'
      CHARACTER( 627 ) :: FWIKI_OUT_FILE

      INTEGER, EXTERNAL :: INDEX1
      INTEGER            :: LPOINT, IEOL
      INTEGER            :: I, ICOL, ISPC, IRX, IDX
      INTEGER            :: NXX, IPR, IPHOTAB, NC
      INTEGER            :: DUMMY_COEF( MAXRXNUM )               ! Yields for the DUMMY variable in each reaction
      INTEGER            :: SS1RX( MAXNLIST )                    ! First reaction occurrence for each SS species
      
c..Variables for species to be dropped from mechanism
      INTEGER         :: N_DROP_SPC = 0
      CHARACTER( 16 ) :: DROP_SPC( MAXNLIST )
      LOGICAL         :: LERROR
      LOGICAL         :: KPP_DUMMY   = .FALSE.
      LOGICAL         :: FIRST_TERM  = .TRUE.
      REAL( 8 )       :: WREXT_COEFFS( MAXSPECTERMS)
      INTEGER         :: WREXT_INDEX(  MAXSPECTERMS)
      INTEGER         :: TABLE_UNIT

      INTEGER SPC1RX( MAXSPEC )              ! rx index of 1st occurence of species
                                             ! in mechanism table
      CHARACTER( 120 ) :: WIKI_TABLE
      CHARACTER( 120 ) :: SPC_MECH_KPP
      CHARACTER( 891 ) :: REACTION_STR(  MAXRXNUM )
      CHARACTER( 1891 ) :: STRING
      CHARACTER(  16 ) :: COEFF_STR
      CHARACTER(  16 ) :: NAMCONSTS( MAXCONSTS ) = (/
     &                    'ATM_AIR         ',
     &                    'ATM_O2          ',
     &                    'ATM_N2          ',
     &                    'ATM_H2          ',
     &                    'ATM_CH4         ' /)

      Character(EXP_LEN)  :: Output_Formula          ! output friendly formula
      CHARACTER(  16 )    :: CLABEL                  ! mechanism constants label
      REAL( 8 )           :: CONSTVAL                ! retrieved constant
      REAL( 8 )            :: CVAL( MAXCONSTS )       ! mechanism constants value
      INTEGER, PARAMETER  :: LUNOUT = 6
      INTEGER             :: IDIFF_ORDER           ! difference between order of two separate reactions
      LOGICAL             :: FALLOFF_RATE       ! whether a reaction is a falloff type


      CHARACTER(  12 ) :: EXFLNM_SPCS = 'SPCSDATX'
      CHARACTER(  12 ) :: EXFLNM_RXDT = 'RXNSDATX'
      CHARACTER(  12 ) :: EXFLNM_RXCM = 'RXNSCOMX'

      INTEGER, EXTERNAL :: JUNIT
      INTEGER            :: ICOUNT, IREACT, IPRODUCT


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Create name for output file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  
      FWIKI_OUT_FILE = TRIM( OUTDIR ) // '/mech_' // 
     &                 TRIM( MECHNAME_LOWER_CASE )  // '.html'

      CALL CALCULATE_RATES( NR )

      IF( .NOT. ALLOCATED( IOLD2NEW ) )THEN
         ALLOCATE( IOLD2NEW( NUMB_MECH_SPCS ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = 'ERROR IOLD2NEW'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF
         DO I = 1, NUMB_MECH_SPCS
            IOLD2NEW( I ) = I
         END DO
      END IF

! create reactions strings to determine
       DO NXX = 1, NR
         DO IREACT = 1, NREACT( NXX )
            ISPC = IRR( NXX, IREACT )
               IF( IREACT .LE. 1 )THEN
                  REACTION_STR( NXX ) =  TRIM(SPARSE_SPECIES( ISPC )) // ' '
               ELSE
                  REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) //  ' + ' // TRIM(SPARSE_SPECIES( ISPC )) // ' '
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
         REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) //  ' &xrarr; ' ! ' ----&gt; '
! write out products
         DO IPRODUCT = 1, NPRDCT( NXX )
            ISPC = IRR( NXX, IPRODUCT + 3 )
            IF ( ABS( SC( NXX,IPRODUCT ) ) .NE. 1.0 ) THEN
               IF ( SC( NXX,IPRODUCT ) .LT. 0 ) THEN
                  WRITE(COEFF_STR,'(A,F8.5)')' - ',ABS(SC( NXX,IPRODUCT ))
                  REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) // TRIM(COEFF_STR) 
     &                        // ' * '  // TRIM(SPARSE_SPECIES( ISPC ))
               ELSE
                  WRITE(COEFF_STR,'(F8.5)')SC( NXX,IPRODUCT )
                  IF( IPRODUCT .EQ. 1 )THEN
                     REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) // ' ' // TRIM(COEFF_STR) 
     &                           // ' * '  // TRIM(SPARSE_SPECIES( ISPC ))
                  ELSE
                     REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) // ' + ' // TRIM(COEFF_STR) 
     &                           // ' * '  // TRIM(SPARSE_SPECIES( ISPC ))
                  END IF
               END IF
            ELSE 
               IF ( SC( NXX,IPRODUCT ) .LT. 0.0 ) THEN
                   REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) //  ' - ' // TRIM(SPARSE_SPECIES( ISPC ))
               ELSE
                  IF( IPRODUCT .EQ. 1 )THEN
                    REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) // ' ' //  TRIM(SPARSE_SPECIES( ISPC ))
                  ELSE
                    REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) //  ' + ' // TRIM(SPARSE_SPECIES( ISPC ))
                  END IF
               END IF
            END IF
         END DO
       END DO

! create table file      
      TABLE_UNIT = JUNIT()
      OPEN ( UNIT = TABLE_UNIT, FILE = FWIKI_OUT_FILE, STATUS = 'UNKNOWN'  )
! 
      WRITE(TABLE_UNIT,2001)Trim(AUTHOR),TRIM(AUTHOR)
      WRITE(TABLE_UNIT, 69099) 
      WRITE(TABLE_UNIT, 69102) 
      WRITE(TABLE_UNIT, 69103) 
      WRITE(TABLE_UNIT, 69104) 
      WRITE(TABLE_UNIT, 69105) 
      WRITE(TABLE_UNIT, 69106) 
      WRITE(TABLE_UNIT, 69107) 
      WRITE(TABLE_UNIT, 69108) 
      WRITE(TABLE_UNIT, 69109) 
      PHRASE = ' '
      PHRASE(1:32) = MECHNAME(1:32)
      CALL CONVERT_CASE(PHRASE, .FALSE.)
      IF( NSPECIAL .GT. 0 )WRITE(TABLE_UNIT,69100)TRIM(PHRASE)

      IEOL = LEN( STRING )
      DO NXX = 1, NSPECIAL
         STRING = TRIM(SPECIAL( NXX ) )
         FIRST_TERM = .TRUE.
         LPOINT = LEN_TRIM( STRING ) + 1
! first write standard rate constants time concentrations
         DO IREACT = 1, MAXSPECTERMS
             IRX  = INDEX_KTERM( NXX, IREACT )
             IF( IRX .LT. 1 .AND. INDEX_CTERM( NXX, IREACT ) .LT. 1 )CYCLE
             IF( FIRST_TERM )THEN
                PHRASE = ' = '
                FIRST_TERM = .FALSE.
                IF(KC_COEFFS( NXX, IREACT ) .LT. 0.0 )PHRASE = ' = ' // ' - '
             ELSE
!                WRITE(TABLE_UNIT, 4711, ADVANCE = 'NO' )
                PHRASE = ' + '
                IF(KC_COEFFS( NXX, IREACT ) .LT. 0.0 )PHRASE = ' - '
             END IF
             IF( KC_COEFFS( NXX, IREACT ) .NE. 1.0 )THEN
                 IF( IRX .GT. 0 )THEN
                    WRITE(STRING(LPOINT:IEOL), 4708)TRIM(PHRASE),
     &              REAL( ABS( KC_COEFFS( NXX, IREACT ) ), 8),TRIM(LABEL(IRX,1))
                 ELSE
                    WRITE(STRING(LPOINT:IEOL), 5708)TRIM(PHRASE),
     &              REAL( ABS( KC_COEFFS( NXX, IREACT ) ), 8)
                 END IF
             ELSE
                 IF( IRX .GT. 0 )THEN
                    WRITE(STRING(LPOINT:IEOL), 4706)TRIM(PHRASE),TRIM(LABEL(IRX,1))
                 ELSE
                    WRITE(STRING(LPOINT:IEOL), 4709)TRIM(PHRASE)
                 END IF
             END IF
             LPOINT = LEN_TRIM( STRING ) + 1
             IF( INDEX_CTERM( NXX, IREACT ) .LT. 1  )CYCLE
             ISPC = IOLD2NEW( INDEX_CTERM( NXX, IREACT ) )
             IF( IRX .GT. 0 )THEN
                PHRASE = '[' // TRIM( SPARSE_SPECIES( ISPC ) ) // ']'
             ELSE
                PHRASE = ' [' // TRIM( SPARSE_SPECIES( ISPC ) ) // ']'
             END IF
             WRITE(STRING(LPOINT:IEOL), 4709)TRIM( PHRASE )
             LPOINT = LEN_TRIM( STRING ) + 1
         END DO
         IF( MAXVAL( OPERATORS( NXX, 1:MAXSPECTERMS ) ) .LT. 1 )THEN
            WRITE(STRING(LPOINT:IEOL), * )' '
            LPOINT = LEN_TRIM( STRING )
            WRITE(TABLE_UNIT,69112)TRIM( STRING )
            CYCLE
         END IF
! next write defined operators         
         DO IREACT = 1, MAXSPECTERMS
            IDX = OPERATORS( NXX, IREACT )
            IF( IDX .LT. 1 .AND. IREACT .LT. MAXSPECTERMS )THEN
                CYCLE
            ELSE IF( IDX .LT. 1 .AND. IREACT .GE. MAXSPECTERMS )THEN
                WRITE(STRING(LPOINT:IEOL), * )' '
                LPOINT = LEN_TRIM( STRING )
                CYCLE
            END IF
             IF( FIRST_TERM )THEN
                PHRASE = ' = '
                IF(OPERATOR_COEFFS( NXX, IREACT ) .LT. 0.0 )PHRASE = ' = ' // ' - '
                FIRST_TERM = .FALSE.
             ELSE
                WRITE(STRING(LPOINT:IEOL), 4711)
                LPOINT = LEN_TRIM( STRING )
                PHRASE = ' + '
                IF(OPERATOR_COEFFS( NXX, IREACT ) .LT. 0.0 )PHRASE = ' - '
             END IF
             IF( OPERATOR_COEFFS( NXX, IREACT ) .NE. 1.0 )THEN
                 WRITE(STRING(LPOINT:IEOL), 4710)TRIM(PHRASE),
     &           REAL( ABS( OPERATOR_COEFFS( NXX, IREACT ) ), 8), TRIM( SPECIAL( IDX ) )
                 LPOINT = LEN_TRIM( STRING )
             ELSE
                 WRITE(STRING(LPOINT:IEOL), 4712)TRIM(PHRASE),TRIM( SPECIAL( IDX ) )
                 LPOINT = LEN_TRIM( STRING )
             END IF
             IF( IREACT .GE. MAXSPECTERMS )WRITE(STRING, * )' '
             LPOINT = LEN_TRIM( STRING )
         END DO 
         WRITE(TABLE_UNIT,69112)TRIM( STRING )
      END DO

      IF( NSPECIAL .GT. 0 )WRITE(TABLE_UNIT, 69101)



         WRITE(TABLE_UNIT,2002)
         WRITE(TABLE_UNIT,2003)
         WRITE(TABLE_UNIT,2004)
         WRITE(TABLE_UNIT,2005)        
         WRITE(TABLE_UNIT,2006)

         DO LPOINT = 1, NUMB_POINTS
            WRITE(STRING, 5121)TEMP(LPOINT),CAIR(LPOINT),PRES(LPOINT)
            WRITE(TABLE_UNIT,1004)TRIM( STRING )         
         END DO

         WRITE(TABLE_UNIT,2009)
         WRITE(TABLE_UNIT,2010)
         WRITE(TABLE_UNIT,2011)


      DO NXX = 1, NR

         WRITE(TABLE_UNIT,1001)
1001     FORMAT(T9,'<tr>')
         WRITE(TABLE_UNIT,1003)NXX,NXX

!         WRITE(TABLE_UNIT,1004)'&lt;' // TRIM(LABEL( NXX,1 )) // '&gt;'
          WRITE(TABLE_UNIT,1004)TRIM(LABEL( NXX,1 ))


         WRITE(TABLE_UNIT,1004)TRIM( REACTION_STR( NXX ) )

         SELECT CASE( KTYPE( NXX ) )
          CASE( -1 )
             DO IPR = 1, NHETERO
                IF ( IHETERO( IPR,1 ) .EQ. NXX )EXIT
             END DO
             IDX = IHETERO( IPR, 2 )
             IF( RTDAT(1, NXX) .NE. 1.0 )THEN
                 WRITE(STRING,5027)REAL(RTDAT(1, NXX),8),TRIM( HETERO(IDX) )
             ELSE
                 WRITE(STRING,5028)TRIM( HETERO(IDX) )
             END IF
          CASE(  0 )
             DO IPR = 1, IP
                IF ( IPH( IPR,1 ) .EQ. NXX )EXIT
             END DO
             IF ( IPH( IPR,3 ) .NE. 0 )THEN
                IDX = IPH( IPR, 2 )
                IF( RTDAT(1, NXX) .NE. 1.0 )THEN
                   WRITE(STRING,5000)REAL(RTDAT(1, NXX),8),TRIM( PHOTAB(IDX) )
                ELSE
                   WRITE(STRING,5001)TRIM( PHOTAB(IDX) )
                END IF
             ELSE IF( IPH( NXX,3 ) .EQ. 0 )THEN
                IDX = IPH(IPH( NXX,2 ), 2)
                IF( RTDAT(1, NXX) .NE. 1.0 )THEN
                   WRITE(STRING,5100)REAL(RTDAT(1, NXX),8),TRIM(LABEL(IDX,1))
                ELSE
                   WRITE(STRING,5101)TRIM(LABEL(IDX,1))
                END IF
             END IF
          CASE( 1 )
             WRITE(STRING,'(ES12.4)')REAL(RTDAT(1, NXX), 8)
          CASE( 2 )
             WRITE(STRING,5129)RTDAT(1, NXX), RTDAT(2, NXX)
          CASE( 3 )
             WRITE(STRING,5103)RTDAT(1, NXX), RTDAT(3, NXX)
          CASE( 4 )
             WRITE(STRING,5104)RTDAT(1, NXX), RTDAT(3, NXX), RTDAT(2, NXX)
          CASE( 5 )
             IRX = INT( RTDAT( 3, NXX) )
             IF( IRX .GT. NXX )CYCLE
             IDIFF_ORDER = IORDER(NXX) - IORDER(IRX)
             IF( IDIFF_ORDER .NE. 0 )THEN
                 FALLOFF_RATE = ( KTYPE(IRX) .GT. 7 .AND. KTYPE(IRX) .LT. 11 )
!                 IF( KUNITS .EQ. 2 .OR. FALLOFF_RATE )THEN
!                   CALL WRITE_RATE_CONVERT_BEFORE(MODULE_UNIT, IDIFF_ORDER )
!                 END IF
             END IF
             WRITE(STRING,5115)1.0D0/RTDAT( 1, NXX ), -RTDAT(2, NXX ),TRIM(LABEL(IRX,1))
!            WRITE(6,5115)1.0D0/RTDAT( 1, NXX ), -RTDAT(2, NXX ),TRIM(LABEL(IRX,1))
          CASE( 6 )
!             DO IDX = 1, KTN6
!                IF( KRX6( IDX ) .EQ. NXX )EXIT
!             END DO         
             IRX = INT( RTDAT( 2, NXX) )
          IDIFF_ORDER = IORDER(NXX) - IORDER(IRX)
           IF( IDIFF_ORDER .NE. 0 )THEN
              FALLOFF_RATE = ( KTYPE(IRX) .GT. 7 .AND. KTYPE(IRX) .LT. 11 )
           END IF
             IF( RTDAT( 1, NXX ) .NE. 1.0 )THEN
                 WRITE(STRING, 5006)REAL(RTDAT( 1, NXX ), 8),TRIM(LABEL(IRX,1))
             ELSE
                 WRITE(STRING, 4706)' ', TRIM(LABEL(IRX,1))
             END IF
          CASE( 7 )
             IF( RTDAT(1, NXX) .NE. 0.0 )THEN
                 WRITE(STRING,5114)REAL(RTDAT(1, NXX), 8),REAL(RTDAT(2, NXX), 8)
             ELSE
                 WRITE(STRING,5007)REAL(RTDAT(1, NXX), 8)
             END IF
          CASE( 8 )
             DO IDX = 1, NFALLOFF
                IF( IRRFALL( IDX ) .EQ. NXX )EXIT
             END DO
             WRITE(STRING,5108)RTDAT(1,NXX),(1.0*RTDAT(2,NXX)),RTDAT(3,NXX),
     &      (1.0*RFDAT(1,IDX)),RFDAT(2,IDX),(1.0*RFDAT(3,IDX))
          CASE( 9 )
             DO IDX = 1, NFALLOFF
                IF( IRRFALL( IDX ) .EQ. NXX )EXIT
             END DO
             IF( RFDAT( 2, IDX ) .EQ. 0.0 .AND. RFDAT( 3, IDX ) .EQ. 0.0 )THEN
                 WRITE(STRING,5109)RTDAT(1,NXX),RTDAT(2,NXX),
     &           RTDAT(3,NXX),1.0*RFDAT(1,IDX)
             ELSE 
                 WRITE(STRING,5119)RTDAT(1,NXX),RTDAT(2,NXX),RFDAT(2, IDX),
     &           RTDAT(3,NXX),1.0*RFDAT(1,IDX),RFDAT(3, IDX),RFDAT(4, IDX),RFDAT(5, IDX)
              END IF 
          CASE( 10 )
             DO IDX = 1, NFALLOFF
                IF( IRRFALL( IDX ) .EQ. NXX )EXIT
             END DO
             WRITE(STRING, 5110)RTDAT(1,NXX),RTDAT(3,NXX),RTDAT(2,NXX),
     &      RFDAT(1,IDX),RFDAT(3,IDX),RFDAT(2,IDX),RFDAT(5,IDX),RFDAT(4,IDX)
          CASE( 11 )
             DO IDX = 1, NSPECIAL_RXN
                IF( ISPECIAL( IDX, 1 ) .EQ. NXX )EXIT
             END DO
             I   = ISPECIAL( IDX, 1)
             IRX = ISPECIAL( IDX, 2)
             IF( RTDAT( 1, I) .NE. 1.0 .AND. ABS(RTDAT( 3, I )) .LT. 1.0E-8 )THEN
                WRITE(STRING,5011)REAL(RTDAT( 1, I),8), TRIM( SPECIAL( IRX ) )
             ELSE IF( RTDAT( 1, I) .NE. 1.0 .AND. ABS( RTDAT( 3, I ) ) .GE. 1.0E-8 )THEN
                WRITE(STRING,5013)REAL(RTDAT( 1, I ), 8),REAL(RTDAT( 3, I ), 8),
     &          TRIM( SPECIAL( IRX) )
             ELSE IF( RTDAT( 1, I) .EQ. 1.0 .AND. ABS( RTDAT( 3, I ) ).GE. 1.0E-8 )THEN
                WRITE(STRING,5014)REAL(RTDAT( 3, I ), 8),
     &          TRIM( SPECIAL( IRX) )
             ELSE
                WRITE(STRING,5012)TRIM( SPECIAL( IRX ) )
             END IF
           CASE( 12 )
             DO IDX = 1, NFALLOFF
                IF( IRRFALL( IDX ) .EQ. NXX )EXIT
             END DO
             WRITE(STRING,5120)RTDAT(1, NXX ),RFDAT(1, IDX),RTDAT(2, NXX ),
     &       RFDAT(2, IDX),RTDAT(3, NXX )
          CASE( 13 )
             DO IDX = 1, NRATE_STRING
                IF( KSTRING( IDX ) .EQ. NXX )EXIT
             END DO
             WRITE(STRING,'(A)')TRIM( RATE_STRING( IDX ) )
          END SELECT
! write rate constant formula
          WRITE(TABLE_UNIT,1004)TRIM( STRING )
! write estimated rate constant, notes and reference 
          SELECT CASE( KTYPE( NXX ) )
             CASE( 0 )
                WRITE(TABLE_UNIT,1004)'Not Available'
                WRITE(TABLE_UNIT,1004)'Not Available'
                WRITE(TABLE_UNIT,1004)'Photolysis Reaction;depends on radiation and predicted concentrations'
                WRITE(TABLE_UNIT,1004)'Check UTILS/inline_phot_preproc for cross-section and quantum data'
             CASE( -1 )
                WRITE(TABLE_UNIT,1004)'Not Available'
                WRITE(TABLE_UNIT,1004)'Not Available'
                WRITE(TABLE_UNIT,1004)'Heteorogeneous Reaction;Depends predicted concentrations'
                WRITE(TABLE_UNIT,1004)'Check routine AEROSOL_CHEMISTRY.F for calculation'
             CASE( 11 )
                WRITE(TABLE_UNIT,1004)'Not Available'
                WRITE(TABLE_UNIT,1004)'Not Available'
                WRITE(TABLE_UNIT,1004)'Rate constant an Operator;Depends predicted concentrations'
                WRITE(TABLE_UNIT,1004)' '
             CASE( 13 )
                IF( RATE_CONSTANT( 1, NXX) .LT. 0.0 .OR. RATE_CONSTANT( 1, NXX) .LT. 0.0 )THEN
                   WRITE(TABLE_UNIT,1004)'Not Available'
                   WRITE(TABLE_UNIT,1004)'Not Available'
                   WRITE(TABLE_UNIT,1004)'Rate constant entered as a character string;' 
     &             //                    'CHEMMECH evaluator routine failed to compute value.'
                   WRITE(TABLE_UNIT,1004)' '
               ELSE
                   WRITE(TABLE_UNIT,1005)RATE_CONSTANT( 1, NXX),RATE_CONSTANT( 1, NXX)
                   WRITE(TABLE_UNIT,1005)RATE_CONSTANT( 2, NXX),RATE_CONSTANT( 2, NXX)
                   WRITE(TABLE_UNIT,1004)' '
                   WRITE(TABLE_UNIT,1004)' '
               END IF
             CASE( 12 )
                WRITE(TABLE_UNIT,1005)RATE_CONSTANT( 1, NXX),RATE_CONSTANT( 1, NXX)
                WRITE(TABLE_UNIT,1005)RATE_CONSTANT( 2, NXX),RATE_CONSTANT( 2, NXX)
                WRITE(TABLE_UNIT,1004)'Set to zero if sun is below the horizon.'
     &                            // ' SEAWATER equals surface fraction covered by ice free open ocean plus surf zones.'
     &                            // ' P equals air pressure in atmospheres'
                WRITE(TABLE_UNIT,1004)' '
             CASE( 6 )
                WRITE(TABLE_UNIT,1005)RATE_CONSTANT( 1, NXX),RATE_CONSTANT( 1, NXX)
                WRITE(TABLE_UNIT,1005)RATE_CONSTANT( 2, NXX),RATE_CONSTANT( 2, NXX)
                WRITE(TABLE_UNIT,1004)'Rate constant multiple of constant for listed reaction'
                WRITE(TABLE_UNIT,1004)' '
             CASE( 5 )
                WRITE(TABLE_UNIT,1005)RATE_CONSTANT( 1, NXX),RATE_CONSTANT( 1, NXX)
                WRITE(TABLE_UNIT,1005)RATE_CONSTANT( 2, NXX),RATE_CONSTANT( 2, NXX)
                WRITE(TABLE_UNIT,1004)'Rate constant scaled as reverse equilibrium to constant for listed reaction'
                WRITE(TABLE_UNIT,1004)' '
             CASE DEFAULT
                WRITE(TABLE_UNIT,1005)RATE_CONSTANT( 1, NXX),RATE_CONSTANT( 1, NXX)
                WRITE(TABLE_UNIT,1005)RATE_CONSTANT( 2, NXX),RATE_CONSTANT( 2, NXX)
                WRITE(TABLE_UNIT,1004)' '
                WRITE(TABLE_UNIT,1004)' '
         END SELECT
         WRITE(TABLE_UNIT,1002)
         WRITE(TABLE_UNIT,1000)
      END DO
      IF( NFUNCTIONS .GT. 0 )THEN
        WRITE(TABLE_UNIT,2999)
        WRITE(TABLE_UNIT,2500)
        WRITE(TABLE_UNIT,2501)
        WRITE(TABLE_UNIT,2502)
        WRITE(TABLE_UNIT,2503)
        WRITE(TABLE_UNIT,2504)
        WRITE(TABLE_UNIT,2505)
        WRITE(TABLE_UNIT,2506)
        WRITE(TABLE_UNIT,1002)
        WRITE(TABLE_UNIT,1000)
        DO IDX = 1, NFUNCTIONS
           WRITE(TABLE_UNIT,2507)TRIM(FUNCTIONS(IDX))
           OUTPUT_FORMULA = REPLACE_TEXT(FORMULA(IDX),"TEMP","T")
           OUTPUT_FORMULA = REPLACE_TEXT(OUTPUT_FORMULA,"PRES","P")
           OUTPUT_FORMULA = REPLACE_TEXT(OUTPUT_FORMULA,"EXP(","exp(")
           OUTPUT_FORMULA = REPLACE_TEXT(OUTPUT_FORMULA,"TEMP","T")
           OUTPUT_FORMULA = REPLACE_TEXT(OUTPUT_FORMULA,"PRES","P")
           OUTPUT_FORMULA = REPLACE_TEXT(OUTPUT_FORMULA,"LOG10(","log<sub>10</sub>(")
           OUTPUT_FORMULA = REPLACE_TEXT(OUTPUT_FORMULA,"LOG(","log(")
           WRITE(TABLE_UNIT,1004)TRIM(OUTPUT_FORMULA)
           WRITE(TABLE_UNIT,1002)
           WRITE(TABLE_UNIT,1000)
        END DO
      END IF
      WRITE(TABLE_UNIT,3001)
      CLOSE(TABLE_UNIT)

      RETURN
      
1002    FORMAT( T17,'<td align="left"><br></td>'
     &         /T17,'<td align="left"><br></td>'
     &         /T17,'<td align="left"><br></td>'
     &         /T17,'<td align="left"><br></td>')
1000    FORMAT(T9,'</tr>')
1003    FORMAT( T17,'<td style="border-top: 1px solid #000000; border-bottom: ',
     &             '1px solid #000000; border-left: 1px solid #000000; border-right: ',
     &             '1px solid #000000" height="17" align="left" sdval="', I4,'" sdnum="1033;">', I4,'</td>')
1004    FORMAT( T17,'<td style="border-top: 1px solid #000000; border-bottom: ',
     &             '1px solid #000000; border-left: 1px solid #000000; border-right: ',
     &             '1px solid #000000" align="left">', A,'</td>')
1005    FORMAT( T17,'<td style="border-top: 1px solid #000000; border-bottom: ',
     &            '1px solid #000000; border-left: 1px solid #000000; border-right: ',
     &            '1px solid #000000" align="right" sdval="', ES12.4,'" sdnum="1033;">', ES12.4,'</td>')

2001    FORMAT('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">',
     &         // '<html>' / '<head>',
     &         / T9,'<meta http-equiv="content-type" content="text/html; charset=utf-8"/>',
     &         / T9,'<title></title>',
     &         / T9,'<meta name="generator" content="CMAQ CHEMMECH PROCESSOR"/>',
     &         / T9,'<meta name="author" content="', A,'"/>',
     &         / T9,'<meta name="created" content="2017-04-14T97:05:52.892979325"/>',
     &         / T9,'<meta name="changedby" content="', A, '"/>',
     &         / T9,'<meta name="changed" content="2017-04-14T97:14:44.403452689"/>',
     &        // T9,'<style type="text/css">',
     &         / T17,'body,div,table,thead,tbody,tfoot,tr,th,td,p ',
     &              '{ font-family:"Liberation Sans"; font-size:x-small }',
     &         / T9,'</style>',
     &        //       '</head>' // '<body>')
2002     FORMAT('<table cellspacing="0" border="0">',
     &         / T9,'<colgroup width="85"></colgroup>',
     &         / T9,'<colgroup width="85"></colgroup>',
     &         / T9,'<colgroup width="900"></colgroup>',
     &         / T9,'<colgroup width="350"></colgroup>',
     &         / T9,'<colgroup span="2" width="120"></colgroup>',
     &         / T9,'<colgroup width="256"></colgroup>',
     &         / T17,'<colgroup width="241"></colgroup>',
     &         / T9,'<colgroup span="4" width="85"></colgroup>')
2003     FORMAT(T9,'<tr>',
     &         / T17,'<td style="border-top: 1px solid #000000; border-bottom: ',
     &               '1px solid #000000; border-left: 1px solid #000000; ',
     &               'border-right: 1px solid #000000" height="74" align="left">',
     &               'Reaction Number</td>')
2004     FORMAT(T17,'<td style="border-top: 1px solid #000000; border-bottom: ',
     &              '1px solid #000000; border-left: 1px solid #000000; ',
     &              'border-right: 1px solid #000000" align="left">',
     &              'Reaction Label</td>')
2005     FORMAT(T17,'<td style="border-top: 1px solid #000000; border-bottom: ',
     &              '1px solid #000000; border-left: 1px solid #000000; ',
     &              'border-right: 1px solid #000000" align="left">',
     &              'Reaction</td>')
2006     FORMAT(T17,'<td style="border-top: 1px solid #000000; border-bottom: ',
     &              '1px solid #000000; border-left: 1px solid #000000; ',
     &              'border-right: 1px solid #000000" align="left">',
     &              'Rate Constant Formula</td>')
2009     FORMAT(T17,'<td style="border-top: 1px solid #000000; border-bottom: ',
     &              '1px solid #000000; border-left: 1px solid #000000; ',
     &              'border-right: 1px solid #000000" align="left">Notes</td>')
2010     FORMAT(T17,'<td style="border-top: 1px solid #000000; border-bottom: ',
     &              '1px solid #000000; border-left: 1px solid #000000; ',
     &              'border-right: 1px solid #000000" align="left">References</td>')
2011     FORMAT(T17,'<td align="left"><br></td>',
     &         / T17,'<td align="left"><br></td>',
     &         / T17,'<td align="left"><br></td>',
     &         / T17,'<td align="left"><br></td>',
     &         / T9,'</tr>')
2500     FORMAT('<table>')
2501     FORMAT('<p style="text-align:center"><font size="5" style="font-size: 12pt">',
     &          'Functions Table.</p>')
2502     FORMAT('<table cellspacing="0" border="0" align="center">')
2503     FORMAT('<colgroup width="118"></colgroup>')
2504     FORMAT('<colgroup width="256"></colgroup>')
2505     FORMAT(T9,'<tr>',
     &         / T17,'<td style="border-top: 1px solid #000000; border-bottom: ',
     &               '1px solid #000000; border-left: 1px solid #000000; ',
     &               'border-right: 1px solid #000000" height="45" align="center">',
     &               'Name</td>')
2506     FORMAT(T17,'<td style="border-top: 1px solid #000000; border-bottom: ',
     &              '1px solid #000000; border-left: 1px solid #000000; ',
     &              'border-right: 1px solid #000000" align="center">',
     &              'Formula</td>')
2507    FORMAT( T17,'<td style="border-top: 1px solid #000000; border-bottom: ',
     &             '1px solid #000000; border-left: 1px solid #000000; border-right: ',
     &             '1px solid #000000" align="center">', A,'</td>')
2999    FORMAT('</table>') 
3001    FORMAT('</table>',
     &        /'<!-- ************************************************************************** -->'
     &        /'</body>'  // '</html>')
    
69099 FORMAT('<p><font size="3" style="font-size: 12pt">Information is based on the mechanism definitions file.</p>' /
     &   '<p><font size="3" style="font-size: 12pt">Fall-off or pressure dependent reaction rate constants ',
     &   '(M equals air number density):</p>')
69102 FORMAT('<ul>'
     &      /'<li><font size="3" style="font-size: 12pt">For rate constants with k<sub>o</sub>, k<sub>inf</sub>, n, F values: ',
     &       'k = [ k<sub>o</sub>M/(1+k<sub>o</sub>M/k<sub>inf</sub>)]F<sup>G</sup>, ',
     &       'where G=(1+(log<sub>10</sub>(k<sub>o</sub>M/k<sub>inf</sub>)/n)<sup>2</sup>)<sup>-1</sub> </li>' )
69103 FORMAT('<li><font size="3" style="font-size: 12pt">For rate constants with k<sub>1</sub>, k<sub>2</sub>: ',
     &       'k = k<sub>1</sub> + k<sub>2</sub>M</li>' )
69104 FORMAT('<li><font size="3" style="font-size: 12pt">For rate constants with k<sub>0</sub>, k<sub>2</sub>, k<sub>3</sub>: ',
     &       'k = k<sub>0</sub> + k<sub>3</sub>M/(1+k<sub>3</sub>M/k<sub>2</sub>)</li>' )
69105 FORMAT('<li><font size="3" style="font-size: 12pt">For rate constants with k<sub>1</sub>, k<sub>2</sub>, k<sub>3</sub>: ',
     &       'k = k<sub>1</sub> + k<sub>2</sub>M + k<sub>3</sub> </li>' 
     &  /'</ul>')
69106 FORMAT( '<p><font size="3" style="font-size: 12pt">For rate constants with the form A&lt;Reference&gt;, ',
     & /      'k equals A times a reference that represents photolysis rate, ', 
     &        'a heteorogeneous rate constant, rate constant for the given reaction ',
     &        'or an operator. A equals one if not given.')
69107 FORMAT( 'In the mechanism definition file, the rate is formatted as</p>')
69108 FORMAT( '<ul>'
     & / '<li><font size="3" style="font-size: 12pt">A~&lt;HETEOROGENEOUS&gt;</li>',
     & / '<li><font size="3" style="font-size: 12pt">A*K&lt;REACTION&gt;</li>',
     & / '<li><font size="3" style="font-size: 12pt">A/&lt;PHOTOLYSIS&gt;</li>',
     & / '<li><font size="3" style="font-size: 12pt">A?&lt;OPERATOR&gt;</li>',
     & /'</ul>')
69109 FORMAT( '<p> </p>')
69100 FORMAT('<p><font size="3" style="font-size: 12pt">For the ', A, ' mechanism, the operators are defined  below.</p>',
     &     / '<ul>')
69112 FORMAT('<li><font size="3" style="font-size: 12pt">', A,'</li>')
69101 FORMAT('</ul>'
     &     / '<p><font size="3" style="font-size: 12pt">where &lt;REACTION&gt; is the rate constant for the given ',
     &       'REACTION and [species] equals the concentration of a mechanism species at the beginning of ',
     &       'the integration time-step for the chemistry numerical solver.</p>' 
     &    // '<p style="text-align:center"><font size="3" style="font-size: 12pt">Reactions Table.</p>')

4706   FORMAT(A,1X,"&lt;", A,"&gt;")
5706   FORMAT(A)
4708   FORMAT(A,1X,ES9.2,"&lt;", A,"&gt;")
5708   FORMAT(A,1X,ES9.2)
4709   FORMAT( A )     
4710   FORMAT(A,1X,ES9.2,'*', A)
4711   FORMAT(1X)
4712   FORMAT(A, 1X, A)
5000   FORMAT(ES12.4,"&lt;",A,"&gt;")
5001   FORMAT(  "&lt;",A, "&gt;" )
5100   FORMAT(ES12.4,"&lt;",I4,"&gt;")
5101   FORMAT(  "&lt;",I4,"&gt;")
5006   FORMAT(ES12.4,"&lt;", A, "&gt;")   
5007   FORMAT(ES12.4,' *( 1.0D0 + 0.6D0 * P )')             
5011   FORMAT(ES12.4,"&lt;",A,"&gt;")             
5012   FORMAT("&lt;",A,"&gt;")
5013   FORMAT(ES12.4,"exp(",ES12.4,"/T)&lt;",A,"&gt;")           
5014   FORMAT("exp(",ES12.4,"/T)&lt;",A,"&gt;")          
5027   FORMAT(ES12.4,"&lt;",A,"&gt;")
5028   FORMAT( "&lt;",A, "&gt;" )

5111   FORMAT(ES12.4) 
5129   FORMAT(ES12.4,'(T/300)<sup>', F6.2,'</sup>')
5102   FORMAT(ES12.4,'(T/300)<sup>', F6.2,'</sup>')
5103   FORMAT(ES12.4,'exp(', ES12.4,'/T)')
5104   FORMAT(ES12.4,'exp(',ES12.4,'/T)(T/300)<sup>',F6.2,'</sup>')
5114   FORMAT(ES12.4,'P(T/300)</sup>', F6.2,'</sup>')
5115   FORMAT( ES12.4,'exp(', ES12.4,'/T)&lt;', A, '&gt;')
5108    FORMAT(' k<sub>0</sub> = ', ES12.4,'exp(',F9.2,'/T)<br>',
     &         ' k<sub>1</sub> = ', ES12.4,'exp(',F9.2,'/T)<br>',
     &         ' k<sub>3</sub> = ', ES12.4,'exp(',F9.2,'/T)')
5109    FORMAT(' k<sub>0</sub> = ', ES12.4,'exp(',F9.2,'/T)<br>',
     &         ' k<sub>1</sub> = ', ES12.4,'exp(',F9.2,'/T)')
5110    FORMAT(' k<sub>o</sub> = ', ES12.4,'exp(',F9.2,'/T)(T/300)<sup>',F6.2'</sup><br>',
     &         ' k<sub>inf</sub> = ', ES12.4,'exp(',F9.2,'/T)(T/300)<sup>',F6.2,'</sup><br>',
     &         ' n = ', F6.2,'; F = ', F6.2 )
5119    FORMAT( ' k0 = ', ES12.4,'exp(',F9.2,'/T)(T/300)<sup>',F6.2,'</sup><br>',
     &          ' k2 = ', ES12.4,'exp(',F9.2,'/T)(T/300)<sup>',F6.2,'</sup><br>',
     &          ' k3 = ', ES12.4,'exp(',F9.2,'/T)')
5120   FORMAT('SEAWATER*min(', ES10.3,'exp(',ES10.3'P) +' ES10.3,'exp(',ES10.3'P), ',ES10.3')')
5121   FORMAT( 'Value (molecules/(sec*cm<sup>3</sup>)) <br> at ',
     &         F6.2,' K<br> ', ES12.4,' molec/cm<sup>3</sup><br> ', F6.2,' Atm.')

95100  FORMAT(2X,A16,' = 0.0D0')        


       END SUBROUTINE WRT_HTML_TABLE
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE WRT_MD_SUBTABLE( NR, IP, LABEL, NS  )

      USE GET_ENV_VARS
      USE MECHANISM_DATA
      
      IMPLICIT NONE

      INTEGER,         INTENT( IN ) :: NR ! number of reactions
      INTEGER,         INTENT( IN ) :: IP ! number of photolysis reaction
      CHARACTER( 16 ), INTENT( IN ) :: LABEL( :,: ) ! LABEL(NXX,1) 1st label found in rx NXX
                                                            ! LABEL(NXX,2) 2nd label found in rx NXX
      INTEGER,         INTENT( IN ) :: NS ! number of species

c..local Variables for steady-state species

       
      CHARACTER(  1 ) :: CHR
      CHARACTER( 16 ) :: WORD
      CHARACTER( 37 ) :: PHRASE
      CHARACTER( 81 ) :: INBUF
      CHARACTER( 16 )  :: WIKI_OUT_FILE = 'WIKI_OUT_FILE'
      CHARACTER( 627 ) :: FWIKI_OUT_FILE

      INTEGER, EXTERNAL :: INDEX1
      INTEGER            :: LPOINT, IEOL
      INTEGER            :: I, ICOL, ISPC, IRX, IDX
      INTEGER            :: NXX, IPR, IPHOTAB, NC
      INTEGER            :: DUMMY_COEF( MAXRXNUM )               ! Yields for the DUMMY variable in each reaction
      INTEGER            :: SS1RX( MAXNLIST )                    ! First reaction occurrence for each SS species
      
c..Variables for species to be dropped from mechanism
      INTEGER         :: N_DROP_SPC = 0
      CHARACTER( 16 ) :: DROP_SPC( MAXNLIST )
      LOGICAL         :: LERROR
      LOGICAL         :: KPP_DUMMY   = .FALSE.
      LOGICAL         :: FIRST_TERM  = .TRUE.
      REAL( 8 )       :: WREXT_COEFFS( MAXSPECTERMS)
      INTEGER         :: WREXT_INDEX(  MAXSPECTERMS)
      INTEGER         :: TABLE_UNIT

      INTEGER SPC1RX( MAXSPEC )              ! rx index of 1st occurence of species
                                             ! in mechanism table
      CHARACTER( 120 ) :: WIKI_TABLE
      CHARACTER( 120 ) :: SPC_MECH_KPP
      CHARACTER( 891 ) :: REACTION_STR(  MAXRXNUM )
      CHARACTER(  16 ) :: COEFF_STR
      CHARACTER(  16 ) :: NAMCONSTS( MAXCONSTS ) = (/
     &                    'ATM_AIR         ',
     &                    'ATM_O2          ',
     &                    'ATM_N2          ',
     &                    'ATM_H2          ',
     &                    'ATM_CH4         ' /)

      CHARACTER(  18 )    :: CLABEL                  ! function name
      CHARACTER(  18 )    :: OLABEL                  ! output friendly function name
      Character(EXP_LEN)  :: Output_Formula          ! output friendly formula

      REAL( 8 )           :: CONSTVAL                ! retrieved constant
      REAL( 8 )            :: CVAL( MAXCONSTS )       ! mechanism constants value
      INTEGER, PARAMETER  :: LUNOUT = 6
      INTEGER             :: IDIFF_ORDER           ! difference between order of two separate reactions
      LOGICAL             :: FALLOFF_RATE       ! whether a reaction is a falloff type


      CHARACTER(   2 ) :: MD_MULTIPLE
      CHARACTER(  12 ) :: EXFLNM_SPCS = 'SPCSDATX'
      CHARACTER(  12 ) :: EXFLNM_RXDT = 'RXNSDATX'
      CHARACTER(  12 ) :: EXFLNM_RXCM = 'RXNSCOMX'

      INTEGER, EXTERNAL :: JUNIT
      INTEGER            :: ICOUNT, IREACT, IPRODUCT


      LOGICAL :: PHOT_NOTE   = .FALSE.
      LOGICAL :: HETEOR_NOTE = .FALSE.
      LOGICAL :: OPERAT_NOTE = .FALSE.
      LOGICAL :: HALO_NOTE   = .FALSE.
      LOGICAL :: EVALU_NOTE  = .FALSE.
      LOGICAL :: STRING_NOTE = .FALSE.
      LOGICAL :: MULTI_NOTE  = .FALSE.
      LOGICAL :: EQUIL_NOTE  = .FALSE.

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Set flags to write table footnotes
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      PHOT_NOTE   = .FALSE.
      HETEOR_NOTE = .FALSE.
      OPERAT_NOTE = .FALSE.
      HALO_NOTE   = .FALSE.
      EVALU_NOTE  = .FALSE.
      STRING_NOTE = .FALSE.
      MULTI_NOTE  = .FALSE.
      EQUIL_NOTE  = .FALSE.

C Set markdown symbol for multiple sign

      MD_MULTIPLE = ACHAR( 92 ) // ACHAR( 42 )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Create name for output file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  
      FWIKI_OUT_FILE = TRIM( OUTDIR ) // '/mech_' // 
     &                 TRIM( MECHNAME_LOWER_CASE )  // '.md'

      CALL CALCULATE_RATES( NR )

      IF( .NOT. ALLOCATED( IOLD2NEW ) )THEN
         ALLOCATE( IOLD2NEW( NUMB_MECH_SPCS ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = 'ERROR IOLD2NEW'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF
         DO I = 1, NUMB_MECH_SPCS
            IOLD2NEW( I ) = I
         END DO
      END IF
! write out reactions strings to determine mechanism information

       DO NXX = 1, NR
         DO IREACT = 1, NREACT( NXX )
            ISPC = IRR( NXX, IREACT )
               IF( IREACT .LE. 1 )THEN
                  REACTION_STR( NXX ) =  TRIM(SPARSE_SPECIES( ISPC )) // ' '
               ELSE
                  REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) //  ' + ' // TRIM(SPARSE_SPECIES( ISPC )) // ' '
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
         REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) //  ' ----> '
! write out products
         DO IPRODUCT = 1, NPRDCT( NXX )
            ISPC = IRR( NXX, IPRODUCT + 3 )
            IF ( ABS( SC( NXX,IPRODUCT ) ) .NE. 1.0 ) THEN
               IF ( SC( NXX,IPRODUCT ) .LT. 0 ) THEN
                  WRITE(COEFF_STR,'(A,F8.5)')' - ',ABS(SC( NXX,IPRODUCT ))
                  REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) // TRIM(COEFF_STR) 
     &                        // ' \* '  // TRIM(SPARSE_SPECIES( ISPC ))
               ELSE
                  WRITE(COEFF_STR,'(F8.5)')SC( NXX,IPRODUCT )
                  IF( IPRODUCT .EQ. 1 )THEN
                     REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) // ' ' // TRIM(COEFF_STR) 
     &                           // ' \* '  // TRIM(SPARSE_SPECIES( ISPC ))
                  ELSE
                     REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) // ' + ' // TRIM(COEFF_STR) 
     &                           // ' \* '  // TRIM(SPARSE_SPECIES( ISPC ))
                  END IF
               END IF
            ELSE 
               IF ( SC( NXX,IPRODUCT ) .LT. 0.0 ) THEN
                   REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) //  ' - ' // TRIM(SPARSE_SPECIES( ISPC ))
               ELSE
                  IF( IPRODUCT .EQ. 1 )THEN
                    REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) // ' ' //  TRIM(SPARSE_SPECIES( ISPC ))
                  ELSE
                    REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) //  ' + ' // TRIM(SPARSE_SPECIES( ISPC ))
                  END IF
               END IF
            END IF
         END DO
         
       END DO

! create wiki table file      
      TABLE_UNIT = JUNIT()
      OPEN ( UNIT = TABLE_UNIT, FILE = FWIKI_OUT_FILE, STATUS = 'UNKNOWN'  )
! 
      WRITE(TABLE_UNIT, 69099) 
      PHRASE = ' '
      PHRASE(1:32) = MECHNAME(1:32)
      CALL CONVERT_CASE(PHRASE, .FALSE.)
      IF( NSPECIAL .GT. 0 )WRITE(TABLE_UNIT,69100)TRIM(PHRASE)

      DO NXX = 1, NSPECIAL
         WRITE(TABLE_UNIT,'(A,A)', ADVANCE = 'NO' )' * ',TRIM(SPECIAL( NXX ) )
         FIRST_TERM = .TRUE.
! first write standard rate constants time concentrations
         DO IREACT = 1, MAXSPECTERMS
             IRX  = INDEX_KTERM( NXX, IREACT )
             IF( IRX .LT. 1 .AND. INDEX_CTERM( NXX, IREACT ) .LT. 1 )CYCLE
             IF( FIRST_TERM )THEN
                PHRASE = ' = '
                FIRST_TERM = .FALSE.
                IF(KC_COEFFS( NXX, IREACT ) .LT. 0.0 )PHRASE = ' = ' // ' - '
             ELSE
!                WRITE(TABLE_UNIT, 4711, ADVANCE = 'NO' )
                PHRASE = ' + '
                IF(KC_COEFFS( NXX, IREACT ) .LT. 0.0 )PHRASE = ' - '
             END IF
             IF( KC_COEFFS( NXX, IREACT ) .NE. 1.0 )THEN
                 IF( IRX .GT. 0 )THEN
                    WRITE(TABLE_UNIT, 4708, ADVANCE = 'NO')TRIM(PHRASE),
     &              REAL( ABS( KC_COEFFS( NXX, IREACT ) ), 8),TRIM(LABEL(IRX,1))
                 ELSE
                    WRITE(TABLE_UNIT, 5708, ADVANCE = 'NO')TRIM(PHRASE),
     &              REAL( ABS( KC_COEFFS( NXX, IREACT ) ), 8)
                 END IF
             ELSE
                 IF( IRX .GT. 0 )THEN
                    WRITE(TABLE_UNIT, 4706, ADVANCE = 'NO')TRIM(PHRASE),TRIM(LABEL(IRX,1))
                 ELSE
                    WRITE(TABLE_UNIT, 4709, ADVANCE = 'NO')TRIM(PHRASE)
                 END IF
             END IF
             IF( INDEX_CTERM( NXX, IREACT ) .LT. 1  )CYCLE
             ISPC = IOLD2NEW( INDEX_CTERM( NXX, IREACT ) )
             IF( IRX .GT. 0 )THEN
                PHRASE = '[' // TRIM( SPARSE_SPECIES( ISPC ) ) // ']'
             ELSE
                PHRASE = ' [' // TRIM( SPARSE_SPECIES( ISPC ) ) // ']'
             END IF
             WRITE(TABLE_UNIT, 4709, ADVANCE = 'NO')TRIM( PHRASE )
         END DO
         IF( MAXVAL( OPERATORS( NXX, 1:MAXSPECTERMS ) ) .LT. 1 )THEN
            WRITE(TABLE_UNIT, * )' '
            CYCLE
         END IF
! next write defined operators         
         DO IREACT = 1, MAXSPECTERMS
            IDX = OPERATORS( NXX, IREACT )
            IF( IDX .LT. 1 .AND. IREACT .LT. MAXSPECTERMS )THEN
                CYCLE
            ELSE IF( IDX .LT. 1 .AND. IREACT .GE. MAXSPECTERMS )THEN
                WRITE(TABLE_UNIT, * )' '
                CYCLE
            END IF
             IF( FIRST_TERM )THEN
                PHRASE = ' = '
                IF(OPERATOR_COEFFS( NXX, IREACT ) .LT. 0.0 )PHRASE = ' = ' // ' - '
                FIRST_TERM = .FALSE.
             ELSE
                WRITE(TABLE_UNIT, 4711, ADVANCE = 'NO' )
                PHRASE = ' + '
                IF(OPERATOR_COEFFS( NXX, IREACT ) .LT. 0.0 )PHRASE = ' - '
             END IF
             IF( OPERATOR_COEFFS( NXX, IREACT ) .NE. 1.0 )THEN
                 WRITE(TABLE_UNIT, 4710, ADVANCE = 'NO')TRIM(PHRASE),
     &           REAL( ABS( OPERATOR_COEFFS( NXX, IREACT ) ), 8), TRIM( SPECIAL( IDX ) )
             ELSE
                 WRITE(TABLE_UNIT, 4712, ADVANCE = 'NO')TRIM(PHRASE),TRIM( SPECIAL( IDX ) )
             END IF
             IF( IREACT .GE. MAXSPECTERMS )WRITE(TABLE_UNIT, * )' '
         END DO 
      END DO
      IF( NSPECIAL .GT. 0 )WRITE(TABLE_UNIT, 69101)

!    
      WRITE(TABLE_UNIT, 5121)
      DO NXX = 1, NR
         WRITE(TABLE_UNIT,'(3A)', ADVANCE= 'NO')'| ',TRIM(LABEL( NXX,1 )),'   | '
         DO IREACT = 1, NREACT( NXX )
            ISPC = IRR( NXX, IREACT )
               IF( IREACT .LT. 2 )THEN
                  WRITE(TABLE_UNIT,'(A, A)', ADVANCE = 'NO')TRIM(SPARSE_SPECIES( ISPC )),' '
                  ICOUNT = 1 + LEN( SPARSE_SPECIES( ISPC ) )
               ELSE
                  WRITE(TABLE_UNIT,'(3A)', ADVANCE = 'NO')'+ ',TRIM(SPARSE_SPECIES( ISPC )),' '
                  ICOUNT = 1 + LEN( SPARSE_SPECIES( ISPC ) )
                  ICOUNT = 3 + LEN( SPARSE_SPECIES( ISPC ) )                  
               END IF
         END DO
         DO I = 1, MAXRCTNTS
         IF( INDEX_FIXED_SPECIES( NXX, I ) .GT. 0 .AND. INDEX_FIXED_SPECIES( NXX, I ) .LT. 7 )THEN
              ISPC = INDEX_FIXED_SPECIES( NXX, I  )
              WRITE(TABLE_UNIT,'(3A)', ADVANCE = 'NO')'+ ',TRIM(FIXED_SPECIES( ISPC )),' '
              ICOUNT = 3 + LEN( FIXED_SPECIES( ISPC ) )
         ELSE 
              IF( INDEX_FIXED_SPECIES( NXX, I ) .GE. 7 )THEN
                  WRITE(*,*)'WARNING: INDEX_FIXED_SPECIES( ', NXX,',', I, ') = ',INDEX_FIXED_SPECIES( NXX, I )
              END IF
         END IF    
         END DO     
         WRITE(TABLE_UNIT, '(A)', ADVANCE = 'NO' )'---->'
! write out products
         DO IPRODUCT = 1, NPRDCT( NXX )
            ISPC = IRR( NXX, IPRODUCT + 3 )
            IF ( ABS( SC( NXX,IPRODUCT ) ) .NE. 1.0 ) THEN
               IF ( SC( NXX,IPRODUCT ) .LT. 0 ) THEN
                  WRITE(TABLE_UNIT,'(A,F9.4,3A)', ADVANCE = 'NO')
     &           '- ',ABS(SC( NXX,IPRODUCT )),MD_MULTIPLE,TRIM(SPARSE_SPECIES( ISPC )),' '
                  ICOUNT = ICOUNT + 12 + LEN( SPARSE_SPECIES( ISPC ) )
               ELSE
                  IF( IPRODUCT .EQ. 1 )THEN
                     WRITE(TABLE_UNIT,'(F9.4, 3A)', ADVANCE = 'NO')
     &               SC( NXX,IPRODUCT ),MD_MULTIPLE,TRIM(SPARSE_SPECIES( ISPC )),' '
                     ICOUNT = ICOUNT + 10 + LEN( SPARSE_SPECIES( ISPC ) )
                  ELSE
                     WRITE(TABLE_UNIT,'(A,F9.4,3A)', ADVANCE = 'NO')
     &               '+ ',SC( NXX,IPRODUCT ),MD_MULTIPLE,TRIM(SPARSE_SPECIES( ISPC )),' '
                     ICOUNT = ICOUNT + 12 + LEN( SPARSE_SPECIES( ISPC ) )
                  END IF
               END IF
            ELSE IF ( SC( NXX,IPRODUCT ) .LT. 0.0 ) THEN
               WRITE(TABLE_UNIT,'(3A)', ADVANCE = 'NO')
     &               '- ',TRIM(SPARSE_SPECIES( ISPC )),' '
               ICOUNT = ICOUNT + 3 + LEN( SPARSE_SPECIES( ISPC ) )
            ELSE
               IF( IPRODUCT .EQ. 1 )THEN
                  WRITE(TABLE_UNIT,'(3A)', ADVANCE = 'NO')
     &           ' ',TRIM(SPARSE_SPECIES( ISPC )),' '
                  ICOUNT = ICOUNT + 2 + LEN( SPARSE_SPECIES( ISPC ) )
               ELSE
                  WRITE(TABLE_UNIT,'(3A)', ADVANCE = 'NO')
     &             '+ ',TRIM(SPARSE_SPECIES( ISPC )),' '
                  ICOUNT = ICOUNT + 3 + LEN( SPARSE_SPECIES( ISPC ) )
               END IF
            END IF
!            IF( ICOUNT .GT. 132 .AND. IPRODUCT .LT.  NPRDCT( NXX ) )THEN
!            IF( IPRODUCT .LT.  NPRDCT( NXX ) )THEN
!                ICOUNT = 0
!                WRITE(TABLE_UNIT, * )' '
!                WRITE(TABLE_UNIT,'(A16)', ADVANCE = 'NO')' '
!            END IF
         END DO 
         WRITE(TABLE_UNIT,'(A)', ADVANCE = 'NO')' | '

         SELECT CASE( KTYPE( NXX ) )
          CASE( -1 )
             DO IPR = 1, NHETERO
                IF ( IHETERO( IPR,1 ) .EQ. NXX )EXIT
             END DO
             IDX = IHETERO( IPR, 2 )
             IF( RTDAT(1, NXX) .NE. 1.0 )THEN
                 WRITE(TABLE_UNIT,5027, ADVANCE = 'NO')REAL(RTDAT(1, NXX),8),TRIM( HETERO(IDX) )
             ELSE
                 WRITE(TABLE_UNIT,5028, ADVANCE = 'NO')TRIM( HETERO(IDX) )
             END IF
          CASE(  0 )
             DO IPR = 1, IP
                IF ( IPH( IPR,1 ) .EQ. NXX )EXIT
             END DO
             IF ( IPH( IPR,3 ) .NE. 0 )THEN
                IDX = IPH( IPR, 2 )
                IF( RTDAT(1, NXX) .NE. 1.0 )THEN
                   WRITE(TABLE_UNIT,5000, ADVANCE = 'NO')REAL(RTDAT(1, NXX),8),TRIM( PHOTAB(IDX) )
                ELSE
                   WRITE(TABLE_UNIT,5001, ADVANCE = 'NO')TRIM( PHOTAB(IDX) )
                END IF
             ELSE IF( IPH( NXX,3 ) .EQ. 0 )THEN
                IDX = IPH(IPH( NXX,2 ), 2)
                IF( RTDAT(1, NXX) .NE. 1.0 )THEN
                   WRITE(TABLE_UNIT,5100, ADVANCE = 'NO')REAL(RTDAT(1, NXX),8),TRIM(LABEL(IDX,1))
                ELSE
                   WRITE(TABLE_UNIT,5101, ADVANCE = 'NO')TRIM(LABEL(IDX,1))
                END IF
             END IF
          CASE( 1 )
             WRITE(TABLE_UNIT,'(ES12.4)', ADVANCE = 'NO')REAL(RTDAT(1, NXX), 8)
          CASE( 2 )
             WRITE(TABLE_UNIT,5129, ADVANCE = 'NO')RTDAT(1, NXX), RTDAT(2, NXX)
          CASE( 3 )
             WRITE(TABLE_UNIT,5103, ADVANCE = 'NO')RTDAT(1, NXX), RTDAT(3, NXX)
          CASE( 4 )
             WRITE(TABLE_UNIT,5104, ADVANCE = 'NO')RTDAT(1, NXX), RTDAT(3, NXX), RTDAT(2, NXX)
          CASE( 5 )
             IRX = INT( RTDAT( 3, NXX) )
             IF( IRX .GT. NXX )CYCLE
             IDIFF_ORDER = IORDER(NXX) - IORDER(IRX)
             IF( IDIFF_ORDER .NE. 0 )THEN
                 FALLOFF_RATE = ( KTYPE(IRX) .GT. 7 .AND. KTYPE(IRX) .LT. 11 )
!                 IF( KUNITS .EQ. 2 .OR. FALLOFF_RATE )THEN
!                   CALL WRITE_RATE_CONVERT_BEFORE(MODULE_UNIT, IDIFF_ORDER )
!                 END IF
             END IF
             WRITE(TABLE_UNIT,5115, ADVANCE = 'NO')1.0D0/RTDAT( 1, NXX ), -RTDAT(2, NXX ),TRIM(LABEL(IRX,1))
!            WRITE(6,5115)1.0D0/RTDAT( 1, NXX ), -RTDAT(2, NXX ),TRIM(LABEL(IRX,1))
          CASE( 6 )
!             DO IDX = 1, KTN6
!                IF( KRX6( IDX ) .EQ. NXX )EXIT
!             END DO         
             IRX = INT( RTDAT( 2, NXX) )
	     IDIFF_ORDER = IORDER(NXX) - IORDER(IRX)
	     IF( IDIFF_ORDER .NE. 0 )THEN
	         FALLOFF_RATE = ( KTYPE(IRX) .GT. 7 .AND. KTYPE(IRX) .LT. 11 )
	     END IF
             IF( RTDAT( 1, NXX ) .NE. 1.0 )THEN
                 WRITE(TABLE_UNIT, 5006, ADVANCE = 'NO')REAL(RTDAT( 1, NXX ), 8),TRIM(LABEL(IRX,1))
             ELSE
                 WRITE(TABLE_UNIT, 4706, ADVANCE = 'NO')' ', TRIM(LABEL(IRX,1))
             END IF
          CASE( 7 )
             IF( RTDAT(1, NXX) .NE. 0.0 )THEN
                 WRITE(TABLE_UNIT,5114, ADVANCE = 'NO')REAL(RTDAT(1, NXX), 8),REAL(RTDAT(2, NXX), 8)
             ELSE
                 WRITE(TABLE_UNIT,5007, ADVANCE = 'NO')REAL(RTDAT(1, NXX), 8)
             END IF
          CASE( 8 )
             DO IDX = 1, NFALLOFF
                IF( IRRFALL( IDX ) .EQ. NXX )EXIT
             END DO
             WRITE(TABLE_UNIT,5108, ADVANCE = 'NO')RTDAT(1,NXX),(1.0*RTDAT(2,NXX)),RTDAT(3,NXX),
     &      (1.0*RFDAT(1,IDX)),RFDAT(2,IDX),(1.0*RFDAT(3,IDX))
          CASE( 9 )
             DO IDX = 1, NFALLOFF
                IF( IRRFALL( IDX ) .EQ. NXX )EXIT
             END DO
             IF( RFDAT( 2, IDX ) .EQ. 0.0 .AND. RFDAT( 3, IDX ) .EQ. 0.0 )THEN
                 WRITE(TABLE_UNIT,5109, ADVANCE = 'NO')RTDAT(1,NXX),RTDAT(2,NXX),
     &           RTDAT(3,NXX),1.0*RFDAT(1,IDX)
             ELSE 
                 WRITE(TABLE_UNIT,5119, ADVANCE = 'NO')RTDAT(1,NXX),RTDAT(2,NXX),RFDAT(2, IDX),
     &           RTDAT(3,NXX),1.0*RFDAT(1,IDX),RFDAT(3, IDX),RFDAT(4, IDX),RFDAT(5, IDX)
              END IF 
          CASE( 10 )
             DO IDX = 1, NFALLOFF
                IF( IRRFALL( IDX ) .EQ. NXX )EXIT
             END DO
             WRITE(TABLE_UNIT, 5110, ADVANCE = 'NO')RTDAT(1,NXX),RTDAT(3,NXX),RTDAT(2,NXX),
     &      RFDAT(1,IDX),RFDAT(3,IDX),RFDAT(2,IDX),RFDAT(5,IDX),RFDAT(4,IDX)
          CASE( 11 )
             DO IDX = 1, NSPECIAL_RXN
                IF( ISPECIAL( IDX, 1 ) .EQ. NXX )EXIT
             END DO
             I   = ISPECIAL( IDX, 1)
             IRX = ISPECIAL( IDX, 2)
             IF( RTDAT( 1, I) .NE. 1.0 .AND. ABS( RTDAT( 3, I ) ) .LT. 1.0E-8 )THEN
                WRITE(TABLE_UNIT,5011, ADVANCE = 'NO')REAL(RTDAT( 1, I),8), TRIM( SPECIAL( IRX ) )
             ELSE IF( RTDAT( 1, I) .NE. 1.0 .AND. ABS( RTDAT( 3, I ) ) .GE. 1.0E-8 )THEN
                WRITE(TABLE_UNIT,5013, ADVANCE = 'NO')REAL(RTDAT( 1, I ), 8),REAL(RTDAT( 3, I ), 8),
     &          TRIM( SPECIAL( IRX) )
             ELSE IF( RTDAT( 1, I) .EQ. 1.0 .AND. ABS( RTDAT( 3, I ) ) .GE. 1.0E-8 )THEN
                WRITE(TABLE_UNIT,5014, ADVANCE = 'NO')REAL(RTDAT( 3, I ), 8),
     &          TRIM( SPECIAL( IRX) )
             ELSE
                WRITE(TABLE_UNIT,5012, ADVANCE = 'NO')TRIM( SPECIAL( IRX ) )
             END IF
           CASE( 12 )
             DO IDX = 1, NFALLOFF
                IF( IRRFALL( IDX ) .EQ. NXX )EXIT
             END DO
             WRITE(TABLE_UNIT,5120, ADVANCE = 'NO')RTDAT(1, NXX ),RFDAT(1, IDX),RTDAT(2, NXX ),
     &       RFDAT(2, IDX),RTDAT(3, NXX)
          CASE( 13 )
             DO IDX = 1, NRATE_STRING
                IF( KSTRING( IDX ) .EQ. NXX )EXIT
             END DO
             OUTPUT_FORMULA = REPLACE_TEXT(RATE_STRING( IDX ),"*","Times")
             OUTPUT_FORMULA = REPLACE_TEXT(OUTPUT_FORMULA,"Times","\*")
             OUTPUT_FORMULA = REPLACE_TEXT(OUTPUT_FORMULA,"TEMP","T")
             OUTPUT_FORMULA = REPLACE_TEXT(OUTPUT_FORMULA,"PRES","P")
             OUTPUT_FORMULA = REPLACE_TEXT(OUTPUT_FORMULA,"EXP(","exp(")
             OUTPUT_FORMULA = REPLACE_TEXT(OUTPUT_FORMULA,"TEMP","T")
             OUTPUT_FORMULA = REPLACE_TEXT(OUTPUT_FORMULA,"PRES","P")
             OUTPUT_FORMULA = REPLACE_TEXT(OUTPUT_FORMULA,"LOG10(","log<sub>10</sub>(")
             OUTPUT_FORMULA = REPLACE_TEXT(OUTPUT_FORMULA,"LOG(","log(")
             WRITE(TABLE_UNIT,'(A)', ADVANCE = 'NO')TRIM( OUTPUT_FORMULA )
          END SELECT
! write estimated rate constant 
          SELECT CASE( KTYPE( NXX ) )
             CASE( 0 )
                 WRITE(TABLE_UNIT,'(A,  A, A, A, A , A)')' | ', 'Not Available<sup>1</sup>',
     &           ' | '
                  PHOT_NOTE = .TRUE.
             CASE( -1 )
                 WRITE(TABLE_UNIT,'(A,  A, A, A, A , A)')' | ', 'Not Available<sup>2</sup>',
     &           ' | '
                  HETEOR_NOTE = .TRUE.
             CASE( 11 )
                 WRITE(TABLE_UNIT,'(A,  A, A, A, A , A)')' | ', 'Not Available<sup>3</sup>',
     &           ' | '
                  OPERAT_NOTE = .TRUE.
             CASE( 12 )
                 WRITE(TABLE_UNIT,'(A,  ES12.4, A, ES12.4, 2A , A)')' | ', RATE_CONSTANT( 1, NXX),
     &           '<sup>4</sup>| '
                  HALO_NOTE = .TRUE.
             CASE( 13 )
                IF( RATE_CONSTANT( 1, NXX) .LT. 0.0 .OR. RATE_CONSTANT( 1, NXX) .LT. 0.0 )THEN
                 WRITE(TABLE_UNIT,'(1X, A,  A, A, A, A , A)')'| ', 'Not Available<sup>5</sup>',
     &           ' | '
                  EVALU_NOTE = .TRUE.
               ELSE
                 WRITE(TABLE_UNIT,'(A,  ES12.4, A, ES12.4, A , A)')' | ', RATE_CONSTANT( 1, NXX),
     &          '<sup>6</sup>| '
                  STRING_NOTE = .TRUE.
               END IF
             CASE( 6 )
                 WRITE(TABLE_UNIT,'(A,  ES12.4, A, ES12.4, A , A)')' | ', RATE_CONSTANT( 1, NXX),
     &           '<sup>7</sup>| '
                  MULTI_NOTE = .TRUE.
             CASE( 5 )
                 WRITE(TABLE_UNIT,'(A,  ES12.4, A, ES12.4, A , A)')' | ', RATE_CONSTANT( 1, NXX), 
     &            '<sup>8</sup>| '
                  EQUIL_NOTE = .TRUE.
             CASE DEFAULT
                 WRITE(TABLE_UNIT,'(A,  ES12.4, A, ES12.4, A, A)')' | ', RATE_CONSTANT( 1, NXX), ' |'
         END SELECT
      END DO

      WRITE(TABLE_UNIT,95109)(TEMP(LPOINT),CAIR(LPOINT),PRES(LPOINT),LPOINT=1,1)
      IF(  PHOT_NOTE   )WRITE(TABLE_UNIT,95101)
      IF(  HETEOR_NOTE )WRITE(TABLE_UNIT,95102)
      IF(  OPERAT_NOTE )WRITE(TABLE_UNIT,95103)
      IF(  HALO_NOTE   )WRITE(TABLE_UNIT,95104)
      IF(  EVALU_NOTE  )WRITE(TABLE_UNIT,95105)
      IF(  STRING_NOTE )WRITE(TABLE_UNIT,95106)
      IF(  MULTI_NOTE  )WRITE(TABLE_UNIT,95107)
      IF(  EQUIL_NOTE  )WRITE(TABLE_UNIT,95108)
      
      IF( NFUNCTIONS .GT. 0 )THEN
        WRITE(TABLE_UNIT,*)
        WRITE(TABLE_UNIT,*)
        WRITE(TABLE_UNIT,'(A, /)')'####  Functions Table.'
        WRITE(TABLE_UNIT,'(A)')'|     Name    |         Formula       |     '
        WRITE(TABLE_UNIT,'(A)')'|:------------|----------------------:|     '
        DO IDX = 1, NFUNCTIONS
           OUTPUT_FORMULA = REPLACE_TEXT(FORMULA(IDX),"*M*","\*M\*")
           OUTPUT_FORMULA = REPLACE_TEXT(OUTPUT_FORMULA,"*O2*","\*O2\*")
           OUTPUT_FORMULA = REPLACE_TEXT(OUTPUT_FORMULA,"*N2*","\*N2\*")
           OUTPUT_FORMULA = REPLACE_TEXT(OUTPUT_FORMULA,"TEMP","T")
           OUTPUT_FORMULA = REPLACE_TEXT(OUTPUT_FORMULA,"PRES","P")
           OUTPUT_FORMULA = REPLACE_TEXT(OUTPUT_FORMULA,"EXP(","exp(")
           DO NXX = 1, NFUNCTIONS
              CLABEL = '*' // TRIM( FUNCTIONS(NXX) ) // '*'
              OLABEL = '\*' // TRIM( FUNCTIONS(NXX) ) // MD_MULTIPLE
              OUTPUT_FORMULA = REPLACE_TEXT(OUTPUT_FORMULA,CLABEL,OLABEL)
           END DO
           OUTPUT_FORMULA = REPLACE_TEXT(FORMULA(IDX),"*","Times")
           OUTPUT_FORMULA = REPLACE_TEXT(OUTPUT_FORMULA,"Times","\*")
           OUTPUT_FORMULA = REPLACE_TEXT(OUTPUT_FORMULA,"TEMP","T")
           OUTPUT_FORMULA = REPLACE_TEXT(OUTPUT_FORMULA,"PRES","P")
           OUTPUT_FORMULA = REPLACE_TEXT(OUTPUT_FORMULA,"EXP(","exp(")
           OUTPUT_FORMULA = REPLACE_TEXT(OUTPUT_FORMULA,"TEMP","T")
           OUTPUT_FORMULA = REPLACE_TEXT(OUTPUT_FORMULA,"PRES","P")
           OUTPUT_FORMULA = REPLACE_TEXT(OUTPUT_FORMULA,"LOG10(","log<sub>10</sub>(")
           OUTPUT_FORMULA = REPLACE_TEXT(OUTPUT_FORMULA,"LOG(","log(")
           WRITE(TABLE_UNIT,'(A1,A16,A1,A,A)')'|',TRIM(FUNCTIONS(IDX)),'|', TRIM(OUTPUT_FORMULA),'|     '
        END DO
        WRITE(TABLE_UNIT,95110)
95110   FORMAT(  /, '<sup>0</sup>M, O2, N2 equal the number densities of air, nitrogen and oxygen.    ')
      END IF

      CLOSE(TABLE_UNIT)


      RETURN
      
    
69099 FORMAT("Information is based on the mech.def file." /
     &   "* Fall-off or pressure dependent reaction rate constants (M equals air number density):" /
     &   " * For rate constants with k<sub>o</sub>, k<sub>i</sub>, n, F values: ",
     &       "k = [ k<sub>o</sub>M/(1+k<sub>o</sub>M/k<sub>i</sub>)]F<sup>G</sup>, ",
     &       "where G=(1+(log<sub>10</sub>(k<sub>o</sub>M/k<sub>i</sub>)/n)<sup>2</sup>))<sup>-1</sup> " /
     &   " * For rate constants with k<sub>1</sub>, k<sub>2</sub>: k = k<sub>1</sub> + k<sub>2</sub>M" / 
     &   " * For rate constants with k<sub>0</sub>, k<sub>2</sub>, k<sub>3</sub>: ",
     &       "k = k<sub>0</sub> + k<sub>3</sub>M/(1+k<sub>3</sub>M/k<sub>2</sub>)" /
     &   " * For rate constants with k<sub>1</sub>, k<sub>2</sub>, k<sub>3</sub>: ",
     &       "k = k<sub>1</sub> + k<sub>2</sub>M + k<sub>3</sub> " /
     & / "* For rate constants with the form A<_Reference_>, k equals A times a reference that represents photolysis rate, ", 
     &   "a heteorogeneous rate constant, rate constant for the given reaction or an operator. A equals one if not given." /
     & / "* In the mechanism definition file, the rate is formatted as" 
     & / " * A~<_HETEOROGENEOUS_>"
     & / " * A*K<_REACTION_>"
     & / " * A/<_PHOTOLYSIS_>"
     & / " * A?<_OPERATOR_>" /)
69100 FORMAT("* For the ", A, " mechanism, the operators are defined  below.")
69101 FORMAT( / "where <_REACTION_> is the rate constant for the given _REACTION_ and [_species_] ",
     &          "equals the concentration of a mechanism _species_ at the beginning of ",
     &          "the integration time-step for the chemistry's numerical solver." /
     &          "###   Reactions Table." /)

4706   FORMAT(A,1X,A)
4708   FORMAT(A,1X,ES9.2,"\*", A)
5708   FORMAT(A,1X,ES9.2)
4709   FORMAT( A )     
4710   FORMAT(A,1X,ES9.2,'\*', A)
4711   FORMAT(1X)
4712   FORMAT(A, 1X, A)
5000   FORMAT(ES12.4,"\*",A)
5001   FORMAT(  A )
5100   FORMAT(ES12.4,"\*",I4)
5101   FORMAT(  I4 )
5006   FORMAT(ES12.4,"\*", A )   
5007   FORMAT(ES12.4,' ( 1.0D0 + 0.6\*P )')             
5011   FORMAT(ES12.4,"\*",A)             
5012   FORMAT(A)
5013   FORMAT(ES12.4,"\*exp(",F9.2,"/T)\*",A)             
5014   FORMAT("exp(",F9.2,"/T)\*",A)             
5027   FORMAT(ES12.4,"\*",A)
5028   FORMAT( A )

5111   FORMAT(ES10.2) 
5129   FORMAT(ES10.2,'(T/300)<sup>', F6.2,'</sup>')
5102   FORMAT(ES10.2,'(T/300)<sup>', F6.2,'</sup>')
5103   FORMAT(ES10.2,'e<sup>', F9.2,'/T</sup>')
5104   FORMAT(ES10.2,'e<sup>',F9.2,'/T</sup>(T/300)<sup>',F6.2,' </sup>')
5114   FORMAT(ES10.2,'P(T/300)<sup>', F9.2,'</sup>')
5115   FORMAT( ES10.2,'e<sup>', F9.2,'/T</sup> \*', A )
5108    FORMAT('k<sub>0</sub>=', ES10.2,'e<sup>',F8.1,'/T</sup><br>',
     &         'k<sub>1</sub>=', ES10.2,'e<sup>',F8.1,'/T</sup><br>',
     &         'k<sub>3</sub>=', ES10.2,'e<sup>',F8.1,'/T</sup>')
5109    FORMAT('k<sub>0</sub>=', ES10.2,'e<sup>',F8.1,'/T</sup><br>',
     &         'k<sub>1</sub>=', ES10.2,'e<sup>',F8.1,'/T</sup>')
5110    FORMAT('k<sub>o</sub>=', ES10.2,'e<sup>',F8.1,'/T</sup>(T/300)<sup>',F6.2,'</sup><br>',
     &         'k<sub>i</sub> = ', ES10.2,'e<sup>',F8.1,'/T</sup>(T/300)<sup>',F6.2,'</sup><br>',
     &         'n=', F9.2,';F=', F9.2 )
5119    FORMAT( 'k<sub>0</sub>=', ES12.4,'e<sup>',F8.1,'/T</sup>(T/300)<sup>',F6.2,'</sup><br>',
     &          'k<sub>2</sub>=', ES12.4,'e<sup>',F8.1,'/T</sup>(T/300)<sup>',F6.2,'</sup><br>',
     &          'k<sub>3</sub>=', ES12.4,'e<sup>',F9.2,'/T</sup>')
5120   FORMAT('SEAWATER*min(', ES10.3,'e<sup>',ES10.3'P</sup>+', ES10.3,'e<sup>',ES10.3'P</sup>, <br>',ES10.3')')

5121   FORMAT(  '|Label|Reaction            |Rate Constant Formula| Value<br> molecules/(sec*cm<sup>3</sup>)|   ',
     &        / '|:---|:-------------------|:--------------------|:----:|   ')
95109   FORMAT(  /, "<sup>0</sup>Units molecules/(sec*cm<sup>3</sup>); Value at ",F6.2," K; ", ES12.4,
     &           " molcules/cm<sup>3</sup>; ", F6.2," Atm.     ")

95100  FORMAT(2X,A16,' = 0.0D0')        

95101     FORMAT("<sup>1</sup>Photolysis Reaction;depends on radiation and predicted concentrations     ")
95102     FORMAT("<sup>2</sup>Heteorogeneous Reaction;Depends predicted concentrations                ")
95103     FORMAT("<sup>3</sup>Rate constant an Operator;Depends predicted concentrations              ")
95104     FORMAT("<sup>4</sup>Set to zero if sun is below the horizon. SEAWATER equals surface fraction covered by ",
     &           "ice free open ocean plus surf zones. P equals air pressure in atmospheres.         ")
95105     FORMAT("<sup>5</sup>Rate constant entered as a character string;",
     &           " CHEMMECH evaluator routine failed to compute value.",
     &           " Check Functions Table if formula use an entry.    ")
95106     FORMAT("<sup>6</sup>Rate constant entered as a character string;",
     &           " Check Functions Table if formula use an entry.     ")
95107     FORMAT("<sup>7</sup>Rate constant multiple of constant for listed reaction   ")
95108     FORMAT("<sup>8</sup>Rate constant scaled as reverse equilibrium to constant ",
     &           "for listed reaction    ")
      

       END SUBROUTINE WRT_MD_SUBTABLE

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE WRT_FOAM_TABLES( NR, IP, LABEL, NS  )

      USE GET_ENV_VARS
      USE MECHANISM_DATA
      USE SPECIES_ATOMS_DATA
      
      IMPLICIT NONE

      INTEGER,         INTENT( IN ) :: NR ! number of reactions
      INTEGER,         INTENT( IN ) :: IP ! number of photolysis reaction
      CHARACTER( 16 ), INTENT( IN ) :: LABEL( :,: ) ! LABEL(NXX,1) 1st label found in rx NXX
                                                            ! LABEL(NXX,2) 2nd label found in rx NXX
      INTEGER,         INTENT( IN ) :: NS ! number of species

c..local Variables for steady-state species

       
      CHARACTER(  1 ) :: CHR
      CHARACTER( 16 ) :: WORD
      CHARACTER( 37 ) :: PHRASE
      CHARACTER( 81 ) :: INBUF
      CHARACTER( 16 )  :: WIKI_OUT_FILE = 'WIKI_OUT_FILE'
      CHARACTER( 627 ) :: FWIKI_OUT_FILE
      CHARACTER( 627 ) :: KFILE_OUT_FILE
      CHARACTER( 627 ) :: BOTTOM_UP_FILE
      CHARACTER( 627 ) :: JFILE_OUT_FILE

      INTEGER, EXTERNAL :: INDEX1
      INTEGER            :: LPOINT, IEOL
      INTEGER            :: I, ICOL, ISPC, IRX, IDX
      INTEGER            :: IATOM
      INTEGER            :: NXX, IPR, IPHOTAB, NC
      INTEGER            :: DUMMY_COEF( MAXRXNUM )               ! Yields for the DUMMY variable in each reaction
      INTEGER            :: SS1RX( MAXNLIST )                    ! First reaction occurrence for each SS species
      
c..Variables for species to be dropped from mechanism
      INTEGER         :: N_DROP_SPC = 0
      CHARACTER( 16 ) :: DROP_SPC( MAXNLIST )
      LOGICAL         :: LERROR
      LOGICAL         :: KPP_DUMMY   = .FALSE.
      LOGICAL         :: FIRST_TERM  = .TRUE.
      REAL( 8 )       :: WREXT_COEFFS( MAXSPECTERMS)
      INTEGER         :: WREXT_INDEX(  MAXSPECTERMS)
      INTEGER         :: TABLE_UNIT
      INTEGER         :: KTABLE_UNIT
      INTEGER         :: JTABLE_UNIT
      INTEGER         :: BOTTOM_UP_UNIT

      INTEGER SPC1RX( MAXSPEC )              ! rx index of 1st occurence of species
                                             ! in mechanism table
      CHARACTER( 120 ) :: WIKI_TABLE
      CHARACTER( 120 ) :: SPC_MECH_KPP
      CHARACTER( 891 ) :: REACTION_STR(  MAXRXNUM )
      CHARACTER( 891 ), ALLOCATABLE :: PHOTAB_STR( : )
      CHARACTER(  16 ) :: COEFF_STR
      CHARACTER(  16 ) :: NAMCONSTS( MAXCONSTS ) = (/
     &                    'ATM_AIR         ',
     &                    'ATM_O2          ',
     &                    'ATM_N2          ',
     &                    'ATM_H2          ',
     &                    'ATM_CH4         ' /)

       INTEGER   :: INDX_AIR  = 0
       INTEGER   :: INDX_H2   = 0
       INTEGER   :: INDX_N2   = 0
       INTEGER   :: INDX_O2   = 0
       INTEGER   :: INDX_CH4  = 0

       REAL( 8 ) :: O2_FRACTION  = 0.0D0
       REAL( 8 ) :: N2_FRACTION  = 0.0D0
       REAL( 8 ) :: H2_FRACTION  = 0.0D0
       REAL( 8 ) :: CH4_FRACTION = 0.0D0 

      CHARACTER(  16 )    :: CLABEL                  ! mechanism constants label
      REAL( 8 )           :: CONSTVAL                ! retrieved constant
      REAL( 8 )            :: CVAL( MAXCONSTS )       ! mechanism constants value
      INTEGER, PARAMETER  :: LUNOUT = 6
      INTEGER             :: IDIFF_ORDER           ! difference between order of two separate reactions
      LOGICAL             :: FALLOFF_RATE       ! whether a reaction is a falloff type


      CHARACTER(  12 ) :: EXFLNM_SPCS = 'SPCSDATX'
      CHARACTER(  12 ) :: EXFLNM_RXDT = 'RXNSDATX'
      CHARACTER(  12 ) :: EXFLNM_RXCM = 'RXNSCOMX'

      INTEGER, EXTERNAL :: JUNIT
      INTEGER            :: ICOUNT, IREACT, IPRODUCT
      
      LOGICAL, SAVE     :: WRITE_DELTA_ATOMS = .FALSE.

     

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Create name for output file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  
      FWIKI_OUT_FILE = TRIM( OUTDIR ) // '/' // 
     &                 TRIM( MECHNAME_LOWER_CASE )  // 'AllRxns.m'
      KFILE_OUT_FILE = TRIM( OUTDIR ) // '/'
     &               // TRIM( MECHNAME_LOWER_CASE )  // '_K.m'
      JFILE_OUT_FILE = TRIM( OUTDIR ) // '/' 
     &              // TRIM( MECHNAME_LOWER_CASE )  // '_J.m'
      BOTTOM_UP_FILE = TRIM( OUTDIR ) // '/J_BottomUp_insert_'  
     &              //  TRIM( MECHNAME_LOWER_CASE )  // '.m'

! create wiki table file      
      TABLE_UNIT = JUNIT()
      OPEN ( UNIT = TABLE_UNIT, FILE = FWIKI_OUT_FILE, STATUS = 'UNKNOWN'  )
      KTABLE_UNIT = JUNIT()
      OPEN ( UNIT = KTABLE_UNIT, FILE = KFILE_OUT_FILE, STATUS = 'UNKNOWN'  )
      JTABLE_UNIT = JUNIT()
      OPEN ( UNIT = JTABLE_UNIT, FILE = JFILE_OUT_FILE, STATUS = 'UNKNOWN'  )
      BOTTOM_UP_UNIT = JUNIT()
      OPEN ( UNIT = BOTTOM_UP_UNIT, FILE = BOTTOM_UP_FILE, STATUS = 'UNKNOWN'  )
! 
      IF( ALLOCATED( DELTA_ATOMS ) )THEN
          WRITE_DELTA_ATOMS = .TRUE.
          ICOUNT = COUNT(ATOM_FOUND)
      ELSE
          ICOUNT = 0
      END IF 

      WRITE(TABLE_UNIT, 69099)TRIM(MECHNAME_LOWER_CASE),(NUMB_MECH_SPCS+ICOUNT),NR,
     &                        TRIM( AUTHOR )
      WRITE(JTABLE_UNIT,95200)TRIM(MECHNAME),TRIM(MECHNAME_LOWER_CASE),TRIM(MECHNAME_LOWER_CASE)
      WRITE(KTABLE_UNIT,95300)TRIM(MECHNAME),TRIM(MECHNAME_LOWER_CASE),NHETERO
      CALL CALCULATE_RATES( NR )
      

      IF( .NOT. ALLOCATED( IOLD2NEW ) )THEN
         ALLOCATE( IOLD2NEW( NUMB_MECH_SPCS ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            MSG = 'ERROR IOLD2NEW'
            WRITE(LOGDEV,'(A)')MSG 
            STOP
         END IF
         DO I = 1, NUMB_MECH_SPCS
            IOLD2NEW( I ) = I
         END DO
      END IF
      
      IF( .NOT. ALLOCATED( PHOTAB_STR ) )THEN 
         ALLOCATE( PHOTAB_STR( NPHOTAB ), STAT = IOS )
         PHOTAB_STR = ' '
      END IF

       INDX_AIR  = INDEX1 ( 'ATM_AIR', MAXCONSTS, NAMCONSTS )
       INDX_H2   = INDEX1 ( 'ATM_H2', MAXCONSTS, NAMCONSTS )
       INDX_N2   = INDEX1 ( 'ATM_N2', MAXCONSTS, NAMCONSTS )
       INDX_O2   = INDEX1 ( 'ATM_O2', MAXCONSTS, NAMCONSTS )
       INDX_CH4  = INDEX1 ( 'ATM_CH4', MAXCONSTS, NAMCONSTS )
        
       O2_FRACTION  = CONST( INDX_O2 ) / CONST( INDX_AIR )
       N2_FRACTION  = CONST( INDX_N2 ) / CONST( INDX_AIR )
       H2_FRACTION  = CONST( INDX_H2 ) / CONST( INDX_AIR )
       CH4_FRACTION = CONST( INDX_CH4 ) / CONST( INDX_AIR )
! write out reactions strings to determine mechanism information

       DO NXX = 1, NR
         DO IREACT = 1, NREACT( NXX )
            ISPC = IRR( NXX, IREACT )
               IF( IREACT .LE. 1 )THEN
                  REACTION_STR( NXX ) =  TRIM(SPARSE_SPECIES( ISPC )) // ' '
               ELSE
                  REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) //  ' + ' // TRIM(SPARSE_SPECIES( ISPC )) // ' '
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
         REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) //  ' ----> '
! write out products
         DO IPRODUCT = 1, NPRDCT( NXX )
            ISPC = IRR( NXX, IPRODUCT + 3 )
            IF ( ABS( SC( NXX,IPRODUCT ) ) .NE. 1.0 ) THEN
               IF ( SC( NXX,IPRODUCT ) .LT. 0 ) THEN
                  WRITE(COEFF_STR,'(A,F8.5)')' - ',ABS(SC( NXX,IPRODUCT ))
                  REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) // TRIM(COEFF_STR) 
     &                        // ' * '  // TRIM(SPARSE_SPECIES( ISPC ))
               ELSE
                  WRITE(COEFF_STR,'(F8.5)')SC( NXX,IPRODUCT )
                  IF( IPRODUCT .EQ. 1 )THEN
                     REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) // ' ' // TRIM(COEFF_STR) 
     &                           // ' * '  // TRIM(SPARSE_SPECIES( ISPC ))
                  ELSE
                     REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) // ' + ' // TRIM(COEFF_STR) 
     &                           // ' * '  // TRIM(SPARSE_SPECIES( ISPC ))
                  END IF
               END IF
            ELSE 
               IF ( SC( NXX,IPRODUCT ) .LT. 0.0 ) THEN
                   REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) //  ' - ' // TRIM(SPARSE_SPECIES( ISPC ))
               ELSE
                  IF( IPRODUCT .EQ. 1 )THEN
                    REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) // ' ' //  TRIM(SPARSE_SPECIES( ISPC ))
                  ELSE
                    REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) //  ' + ' // TRIM(SPARSE_SPECIES( ISPC ))
                  END IF
               END IF
            END IF
         END DO
         
       END DO

      PHRASE = ' '
      PHRASE(1:32) = MECHNAME(1:32)
      CALL CONVERT_CASE(PHRASE, .FALSE.)
      IF( NSPECIAL .GT. 0 )WRITE(TABLE_UNIT,69100)TRIM(PHRASE)
      
      
      WRITE(TABLE_UNIT,'(A,F12.9,A)')'N2  = ',N2_FRACTION,'.*M;'
      WRITE(TABLE_UNIT,'(A,F12.9,A)')'O2  = ',O2_FRACTION,'.*M;'
      WRITE(TABLE_UNIT,'(A,F12.9,A)')'H2  = ',H2_FRACTION,'.*M;'
      WRITE(TABLE_UNIT,'(A,F12.9,A,//)')'CH4 = ',CH4_FRACTION,'.*M;'
      
      IF( NFUNCTIONS .GT. 0 )THEN
        WRITE(TABLE_UNIT,*)
        WRITE(TABLE_UNIT,*)
        WRITE(TABLE_UNIT,'(A)')'%Functions Table.'
        WRITE(TABLE_UNIT,'(3A)')'%Name',',','Formula'
        DO IDX = 1, NFUNCTIONS
           WRITE(TABLE_UNIT,'(3A)')TRIM(FUNCTIONS(IDX)),' = ',TRIM(FORMULA(IDX))
        END DO
      END IF

      WRITE(TABLE_UNIT,'(A)')'SpeciesToAdd = {...'
      ISPC = 0
      DO I = 1, (NUMB_MECH_SPCS/5)
         DO IDX = 1, 5
            ISPC = ISPC + 1
            WRITE(TABLE_UNIT,'(A)',ADVANCE='NO')"'" // TRIM(MECHANISM_SPC(ISPC)) // "'; "
         END DO
         WRITE(TABLE_UNIT,'(A)')'...'
      END DO
      ISPC=ISPC+1
      DO IDX = ISPC, NUMB_MECH_SPCS 
         WRITE(TABLE_UNIT,'(A)',ADVANCE='NO')"'" // TRIM(MECHANISM_SPC(IDX)) // "'; "
      END DO
!      ISPC = 0
!      DO I = 1, (N_DROP_SPC/5)
!         DO IDX = 1, 5
!            ISPC = ISPC + 1
!            WRITE(TABLE_UNIT,'(A)',ADVANCE='NO')"'" // TRIM(DROP_SPC(ISPC)) // "'; "
!         END DO
!         WRITE(TABLE_UNIT,'(A)')'...'
!      END DO
!      ISPC=ISPC+1
!      DO IDX = ISPC, N_DROP_SPC
!         WRITE(TABLE_UNIT,'(A)',ADVANCE='NO')"'" // TRIM(DROP_SPC(IDX)) // "'; "
!      END DO
       IF( WRITE_DELTA_ATOMS )THEN
          ISPC = 0
          ICOL = 0
          WRITE(TABLE_UNIT,'(A)')'...'
          DO IATOM = 1,MAX(1,N_ATOMS/5)
             DO IDX = 1, MIN(1,N_ATOMS)
                ISPC = ISPC + 1
                IF( ATOM_FOUND( ISPC ) )THEN
                  ICOL = ICOL + 1
                  WRITE(TABLE_UNIT,'(A,"; ")',ADVANCE='NO')TRIM( "'DELTA_" // TRIM( ATOMS(ISPC)) )// "'"
                  IF( MOD( ICOL,5 ) .EQ. 0 )THEN
                     WRITE(TABLE_UNIT,'(A)')'...'
                     CYCLE
                  END IF
                ELSE
                END IF
             END DO
             IF( ISPC .EQ. N_ATOMS )EXIT
          END DO
          ISPC=ISPC+1
          DO IDX = ISPC, N_ATOMS 
             IF( .NOT. ATOM_FOUND( IDX ) )CYCLE
             WRITE(TABLE_UNIT,'(A,"; ")',ADVANCE='NO')TRIM( "'DELTA_" // TRIM( ATOMS(IDX)) )// "'"
          END DO
      END IF

      WRITE(TABLE_UNIT,'(A,//)')'};'
      WRITE(TABLE_UNIT,'(A,//)')'AddSpecies'
            

      DO NXX = 1, NSPECIAL
         WRITE(TABLE_UNIT,'(A,A)', ADVANCE = 'NO' )' * ',TRIM(SPECIAL( NXX ) )
         FIRST_TERM = .TRUE.
! first write standard rate constants time concentrations
         DO IREACT = 1, MAXSPECTERMS
             IRX  = INDEX_KTERM( NXX, IREACT )
             IF( IRX .LT. 1 .AND. INDEX_CTERM( NXX, IREACT ) .LT. 1 )CYCLE
             IF( FIRST_TERM )THEN
                PHRASE = ' = '
                FIRST_TERM = .FALSE.
                IF(KC_COEFFS( NXX, IREACT ) .LT. 0.0 )PHRASE = ' = ' // ' - '
             ELSE
!                WRITE(TABLE_UNIT, 4711, ADVANCE = 'NO' )
                PHRASE = ' + '
                IF(KC_COEFFS( NXX, IREACT ) .LT. 0.0 )PHRASE = ' - '
             END IF
             IF( KC_COEFFS( NXX, IREACT ) .NE. 1.0 )THEN
                 IF( IRX .GT. 0 )THEN
                    WRITE(TABLE_UNIT, 4708, ADVANCE = 'NO')TRIM(PHRASE),
     &              REAL( ABS( KC_COEFFS( NXX, IREACT ) ), 8),TRIM(LABEL(IRX,1))
                 ELSE
                    WRITE(TABLE_UNIT, 5708, ADVANCE = 'NO')TRIM(PHRASE),
     &              REAL( ABS( KC_COEFFS( NXX, IREACT ) ), 8)
                 END IF
             ELSE
                 IF( IRX .GT. 0 )THEN
                    WRITE(TABLE_UNIT, 3708, ADVANCE = 'NO')TRIM(PHRASE),TRIM(LABEL(IRX,1))
                 ELSE
                    WRITE(TABLE_UNIT, 4709, ADVANCE = 'NO')TRIM(PHRASE)
                 END IF
             END IF
             IF( INDEX_CTERM( NXX, IREACT ) .LT. 1  )CYCLE
             ISPC = IOLD2NEW( INDEX_CTERM( NXX, IREACT ) )
             IF( IRX .GT. 0 )THEN
                PHRASE = '[' // TRIM( SPARSE_SPECIES( ISPC ) ) // ']'
             ELSE
                PHRASE = ' [' // TRIM( SPARSE_SPECIES( ISPC ) ) // ']'
             END IF
             WRITE(TABLE_UNIT, 4709, ADVANCE = 'NO')TRIM( PHRASE )
         END DO
         IF( MAXVAL( OPERATORS( NXX, 1:MAXSPECTERMS ) ) .LT. 1 )THEN
            WRITE(TABLE_UNIT, * )' '
            CYCLE
         END IF
! next write defined operators         
         DO IREACT = 1, MAXSPECTERMS
            IDX = OPERATORS( NXX, IREACT )
            IF( IDX .LT. 1 .AND. IREACT .LT. MAXSPECTERMS )THEN
                CYCLE
            ELSE IF( IDX .LT. 1 .AND. IREACT .GE. MAXSPECTERMS )THEN
                WRITE(TABLE_UNIT, * )' '
                CYCLE
            END IF
             IF( FIRST_TERM )THEN
                PHRASE = ' = '
                IF(OPERATOR_COEFFS( NXX, IREACT ) .LT. 0.0 )PHRASE = ' = ' // ' - '
                FIRST_TERM = .FALSE.
             ELSE
                WRITE(TABLE_UNIT, 4711, ADVANCE = 'NO' )
                PHRASE = ' + '
                IF(OPERATOR_COEFFS( NXX, IREACT ) .LT. 0.0 )PHRASE = ' - '
             END IF
             IF( OPERATOR_COEFFS( NXX, IREACT ) .NE. 1.0 )THEN
                 WRITE(TABLE_UNIT, 4710, ADVANCE = 'NO')TRIM(PHRASE),
     &           REAL( ABS( OPERATOR_COEFFS( NXX, IREACT ) ), 8), TRIM( SPECIAL( IDX ) )
             ELSE
                 WRITE(TABLE_UNIT, 4712, ADVANCE = 'NO')TRIM(PHRASE),TRIM( SPECIAL( IDX ) )
             END IF
             IF( IREACT .GE. MAXSPECTERMS )WRITE(TABLE_UNIT, * )' '
         END DO 
      END DO
      IF( NSPECIAL .GT. 0 )WRITE(TABLE_UNIT, 69101)

      DO NXX = 1, NR
         WRITE(TABLE_UNIT,'(A,I4,A)')'%',NXX,', <' // TRIM( RXLABEL( NXX) ) // '>' 
         WRITE(TABLE_UNIT,'(A)')'i=i+1;'
         WRITE(TABLE_UNIT,'(A,I4,A)', ADVANCE= 'NO')'Rnames{', NXX, "} = '"
         DO IREACT = 1, NREACT( NXX )
            ISPC = IRR( NXX, IREACT )
               IF( IREACT .LT. 2 )THEN
                  WRITE(TABLE_UNIT,'(A, A)', ADVANCE = 'NO')TRIM(SPARSE_SPECIES( ISPC )),' '
                  ICOUNT = 1 + LEN( SPARSE_SPECIES( ISPC ) )
               ELSE
                  WRITE(TABLE_UNIT,'(3A)', ADVANCE = 'NO')'+ ',TRIM(SPARSE_SPECIES( ISPC )),' '
                  ICOUNT = 1 + LEN( SPARSE_SPECIES( ISPC ) )
                  ICOUNT = 3 + LEN( SPARSE_SPECIES( ISPC ) )                  
               END IF
         END DO
         DO I = 1, MAXRCTNTS
             IF( INDEX_FIXED_SPECIES( NXX, I ) .GT. 0 .AND. INDEX_FIXED_SPECIES( NXX, I ) .LT. 7 )THEN
                 ISPC = INDEX_FIXED_SPECIES( NXX, I )
                  WRITE(TABLE_UNIT,'(3A)', ADVANCE = 'NO')'+ ',TRIM( FIXED_SPECIES( ISPC ) ),' '
                  ICOUNT = 1 + LEN( FIXED_SPECIES( ISPC ) )
                  ICOUNT = 3 + LEN( FIXED_SPECIES( ISPC ) )                  
             ELSE 
                  IF( INDEX_FIXED_SPECIES( NXX, I ) .GE. 7 )THEN
                     WRITE(*,*)'WARNING: INDEX_FIXED_SPECIES( ', NXX,',', I, ') = ',INDEX_FIXED_SPECIES( NXX, I )
                  END IF
             END IF         
         END DO
         WRITE(TABLE_UNIT, '(A)', ADVANCE = 'NO' )'='
! write out products
         DO IPRODUCT = 1, NPRDCT( NXX )
            ISPC = IRR( NXX, IPRODUCT + 3 )
            IF ( ABS( SC( NXX,IPRODUCT ) ) .NE. 1.0 ) THEN
               IF ( SC( NXX,IPRODUCT ) .LT. 0 ) THEN
                  WRITE(TABLE_UNIT,'(A,F8.5,3A)', ADVANCE = 'NO')
     &           '- ',ABS(SC( NXX,IPRODUCT )),'*',TRIM(SPARSE_SPECIES( ISPC )),' '
                  ICOUNT = ICOUNT + 12 + LEN( SPARSE_SPECIES( ISPC ) )
               ELSE
                  IF( IPRODUCT .EQ. 1 )THEN
                     WRITE(TABLE_UNIT,'(F8.5, 3A)', ADVANCE = 'NO')
     &               SC( NXX,IPRODUCT ),'*',TRIM(SPARSE_SPECIES( ISPC )),' '
                     ICOUNT = ICOUNT + 10 + LEN( SPARSE_SPECIES( ISPC ) )
                  ELSE
                     WRITE(TABLE_UNIT,'(A,F8.5,3A)', ADVANCE = 'NO')
     &               '+ ',SC( NXX,IPRODUCT ),'*',TRIM(SPARSE_SPECIES( ISPC )),' '
                     ICOUNT = ICOUNT + 12 + LEN( SPARSE_SPECIES( ISPC ) )
                  END IF
               END IF
            ELSE IF ( SC( NXX,IPRODUCT ) .LT. 0.0 ) THEN
               WRITE(TABLE_UNIT,'(3A)', ADVANCE = 'NO')
     &               '- ',TRIM(SPARSE_SPECIES( ISPC )),' '
               ICOUNT = ICOUNT + 3 + LEN( SPARSE_SPECIES( ISPC ) )
            ELSE
               IF( IPRODUCT .EQ. 1 )THEN
                  WRITE(TABLE_UNIT,'(3A)', ADVANCE = 'NO')
     &           ' ',TRIM(SPARSE_SPECIES( ISPC )),' '
                  ICOUNT = ICOUNT + 2 + LEN( SPARSE_SPECIES( ISPC ) )
               ELSE
                  WRITE(TABLE_UNIT,'(3A)', ADVANCE = 'NO')
     &             '+ ',TRIM(SPARSE_SPECIES( ISPC )),' '
                  ICOUNT = ICOUNT + 3 + LEN( SPARSE_SPECIES( ISPC ) )
               END IF
            END IF
!            IF( ICOUNT .GT. 132 .AND. IPRODUCT .LT.  NPRDCT( NXX ) )THEN
!            IF( IPRODUCT .LT.  NPRDCT( NXX ) )THEN
!                ICOUNT = 0
!                WRITE(TABLE_UNIT, * )' '
!                WRITE(TABLE_UNIT,'(A16)', ADVANCE = 'NO')' '
!            END IF
         END DO
         IF( WRITE_DELTA_ATOMS )THEN
             WRITE(TABLE_UNIT,'(3A)', ADVANCE = 'NO')TRIM( DELTA_ATOMS(NXX) )
             ICOUNT = ICOUNT + LEN_TRIM( DELTA_ATOMS(NXX) ) 
         END IF
         WRITE(TABLE_UNIT,'(A)')"';"


         IF( KTYPE( NXX ) .LE. 7 .OR. KTYPE( NXX ) .GE. 13 )THEN
            WRITE(TABLE_UNIT,'(A,I4,A)', ADVANCE= 'NO')'k(:,i) = ('
         END IF
         
         SELECT CASE( KTYPE( NXX ) )
          CASE( -1 )
             DO IPR = 1, NHETERO
                IF ( IHETERO( IPR,1 ) .EQ. NXX )EXIT
             END DO
             IDX = IHETERO( IPR, 2 )
             IF( RTDAT(1, NXX) .NE. 1.0 )THEN
                 WRITE(TABLE_UNIT,5027, ADVANCE = 'NO')REAL(RTDAT(1, NXX),8),TRIM( HETERO(IDX) )
             ELSE
                 WRITE(TABLE_UNIT,5028, ADVANCE = 'NO')TRIM( HETERO(IDX) )
             END IF
         CASE(  0 )
             DO IPR = 1, IP
                IF ( IPH( IPR,1 ) .EQ. NXX )EXIT
             END DO
             IF ( IPH( IPR,3 ) .NE. 0 )THEN
                IDX = IPH( IPR, 2 )
                IF( RTDAT(1, NXX) .NE. 1.0 )THEN
                   WRITE(TABLE_UNIT,5000, ADVANCE = 'NO')REAL(RTDAT(1, NXX),8),TRIM( PHOTAB(IDX) )
                ELSE
                   WRITE(TABLE_UNIT,5001, ADVANCE = 'NO')TRIM( PHOTAB(IDX) )
                END IF
                PHOTAB_STR(  IDX ) = TRIM( REACTION_STR( NXX )  )
             ELSE IF( IPH( NXX,3 ) .EQ. 0 )THEN
                IDX = IPH(IPH( NXX,2 ), 2)
                IF( RTDAT(1, NXX) .NE. 1.0 )THEN
                   WRITE(TABLE_UNIT,5100, ADVANCE = 'NO')REAL(RTDAT(1, NXX),8),TRIM(LABEL(IDX,1))
                ELSE
                   WRITE(TABLE_UNIT,5101, ADVANCE = 'NO')TRIM(LABEL(IDX,1))
                END IF
             END IF
          CASE( 1 )
             WRITE(TABLE_UNIT,'(ES12.4)', ADVANCE = 'NO')REAL(RTDAT(1, NXX), 8)
          CASE( 2 )
             WRITE(TABLE_UNIT,5129, ADVANCE = 'NO')RTDAT(1, NXX), RTDAT(2, NXX)
          CASE( 3 )
             WRITE(TABLE_UNIT,5103, ADVANCE = 'NO')RTDAT(1, NXX), RTDAT(3, NXX)
          CASE( 4 )
             WRITE(TABLE_UNIT,5104, ADVANCE = 'NO')RTDAT(1, NXX), RTDAT(3, NXX), RTDAT(2, NXX)
          CASE( 5 )
             IRX = INT( RTDAT( 3, NXX) )
             IF( IRX .GT. NXX )CYCLE
             IDIFF_ORDER = IORDER(NXX) - IORDER(IRX)
             IF( IDIFF_ORDER .NE. 0 )THEN
                 FALLOFF_RATE = ( KTYPE(IRX) .GT. 7 .AND. KTYPE(IRX) .LT. 11 )
!                 IF( KUNITS .EQ. 2 .OR. FALLOFF_RATE )THEN
!                   CALL WRITE_RATE_CONVERT_BEFORE(MODULE_UNIT, IDIFF_ORDER )
!                 END IF
             END IF
             WRITE(TABLE_UNIT,5115, ADVANCE = 'NO')1.0D0/RTDAT( 1, NXX ), -RTDAT(2, NXX ),IRX
          CASE( 6 )
!             DO IDX = 1, KTN6
!                IF( KRX6( IDX ) .EQ. NXX )EXIT
!             END DO         
             IRX = INT( RTDAT( 2, NXX) )
	     IDIFF_ORDER = IORDER(NXX) - IORDER(IRX)
	     IF( IDIFF_ORDER .NE. 0 )THEN
	         FALLOFF_RATE = ( KTYPE(IRX) .GT. 7 .AND. KTYPE(IRX) .LT. 11 )
	     END IF
             IF( RTDAT( 1, NXX ) .NE. 1.0 )THEN
                 WRITE(TABLE_UNIT, 5006, ADVANCE = 'NO')REAL(RTDAT( 1, NXX ), 8),IRX
             ELSE
                 WRITE(TABLE_UNIT, 4706, ADVANCE = 'NO')IRX
             END IF
          CASE( 7 )
             IF( RTDAT(1, NXX) .NE. 0.0 )THEN
                 WRITE(TABLE_UNIT,5114, ADVANCE = 'NO')REAL(RTDAT(1, NXX), 8),REAL(RTDAT(2, NXX), 8)
             ELSE
                 WRITE(TABLE_UNIT,5007, ADVANCE = 'NO')REAL(RTDAT(1, NXX), 8)
             END IF
          CASE( 8 )
             DO IDX = 1, NFALLOFF
                IF( IRRFALL( IDX ) .EQ. NXX )EXIT
             END DO
             WRITE(TABLE_UNIT,5108, ADVANCE = 'NO')RTDAT(1,NXX),(1.0*RTDAT(2,NXX)),RTDAT(3,NXX),
     &      (1.0*RFDAT(1,IDX)),RFDAT(2,IDX),(1.0*RFDAT(3,IDX))
             WRITE(TABLE_UNIT,7108, ADVANCE= 'NO')'k(:,i) = ('
          CASE( 9 )
             DO IDX = 1, NFALLOFF
                IF( IRRFALL( IDX ) .EQ. NXX )EXIT
             END DO
            IF( RFDAT( 2, IDX ) .EQ. 0.0 .AND. RFDAT( 3, IDX ) .EQ. 0.0 )THEN
                 WRITE(TABLE_UNIT,5109, ADVANCE = 'NO')RTDAT(1,NXX),RTDAT(2,NXX),
     &           RTDAT(3,NXX),1.0*RFDAT(1,IDX)
                 WRITE(TABLE_UNIT,7109, ADVANCE= 'NO')'k(:,i) = ('
             ELSE 
                 WRITE(TABLE_UNIT,5119, ADVANCE = 'NO')RTDAT(1,NXX),RTDAT(2,NXX),RFDAT(2, IDX),
     &           RTDAT(3,NXX),1.0*RFDAT(1,IDX),RFDAT(3, IDX),RFDAT(4, IDX),RFDAT(5, IDX)
                 WRITE(TABLE_UNIT,7119, ADVANCE= 'NO')'k(:,i) = ('
             END IF 
          CASE( 10 )
             DO IDX = 1, NFALLOFF
                IF( IRRFALL( IDX ) .EQ. NXX )EXIT
             END DO
            WRITE(TABLE_UNIT, 5110, ADVANCE = 'NO')RTDAT(1,NXX),RTDAT(3,NXX),RTDAT(2,NXX),
     &      RFDAT(1,IDX),RFDAT(3,IDX),RFDAT(2,IDX),RFDAT(5,IDX),RFDAT(4,IDX)
             WRITE(TABLE_UNIT,7110, ADVANCE= 'NO')'k(:,i) = ('
          CASE( 11 )
             DO IDX = 1, NSPECIAL_RXN
                IF( ISPECIAL( IDX, 1 ) .EQ. NXX )EXIT
             END DO
             I   = ISPECIAL( IDX, 1)
             IRX = ISPECIAL( IDX, 2)
             IF( RTDAT( 1, I) .NE. 1.0 .AND. ABS( RTDAT( 3, I ) ) .EQ. 0.0 )THEN
                WRITE(TABLE_UNIT,5011, ADVANCE = 'NO')REAL(RTDAT( 1, I),8), TRIM( SPECIAL( IRX ) )
             ELSE IF( RTDAT( 1, I) .NE. 1.0 .AND. ABS( RTDAT( 3, I ) ) .GT. 0.0 )THEN
                WRITE(TABLE_UNIT,5013, ADVANCE = 'NO')REAL(RTDAT( 1, I ), 8),REAL(RTDAT( 3, I ), 8),
     &          TRIM( SPECIAL( IRX) )
             ELSE IF( RTDAT( 1, I) .EQ. 1.0 .AND. ABS( RTDAT( 3, I ) ) .GT. 0.0 )THEN
                WRITE(TABLE_UNIT,5014, ADVANCE = 'NO')REAL(RTDAT( 3, I ), 8),
     &          TRIM( SPECIAL( IRX) )
             ELSE
                WRITE(TABLE_UNIT,5012, ADVANCE = 'NO')TRIM( SPECIAL( IRX ) )
             END IF
           CASE( 12 )
             DO IDX = 1, NFALLOFF
                IF( IRRFALL( IDX ) .EQ. NXX )EXIT
             END DO
             WRITE(TABLE_UNIT,5120, ADVANCE = 'NO')RTDAT(1, NXX ),RFDAT(1, IDX),RTDAT(2, NXX ),
     &       RFDAT(2, IDX),RTDAT(3, NXX)
          CASE( 13 )
             DO IDX = 1, NRATE_STRING
                IF( KSTRING( IDX ) .EQ. NXX )EXIT
             END DO
             WRITE(TABLE_UNIT,'(A)', ADVANCE = 'NO')TRIM( RATE_STRING( IDX ) )
          END SELECT
          IF( KTYPE( NXX ) .NE. 12 )THEN
              WRITE(TABLE_UNIT,'(A)', ADVANCE = 'NO')' )'
          END IF
          DO I = 1, MAXRCTNTS
             IF( INDEX_FIXED_SPECIES( NXX, I ) .GT. 0 .AND. INDEX_FIXED_SPECIES( NXX, I ) .LT. 7 )THEN
                  ISPC = INDEX_FIXED_SPECIES( NXX, I  )
                  IF( KTYPE( NXX ) .NE. 12 )THEN
                     WRITE(TABLE_UNIT,'(2A)', ADVANCE = 'NO')'.*',TRIM(FIXED_SPECIES( ISPC ))
                  ELSE
                     WRITE(TABLE_UNIT,'(A)', ADVANCE = 'NO')'k(:,i) = k(:,i).*'
                     WRITE(TABLE_UNIT,'(A)')'; '
                  END IF
                  ICOUNT = 3 + LEN( FIXED_SPECIES( ISPC ) )
             ELSE 
                  IF( INDEX_FIXED_SPECIES( NXX, I ) .GE. 7 )THEN
                      WRITE(*,*)'WARNING: INDEX_FIXED_SPECIES( ', NXX,',', I, ') = ',INDEX_FIXED_SPECIES( NXX, I )
                  END IF
             END IF    
          END DO     
          IF( KTYPE( NXX ) .NE. 12 )WRITE(TABLE_UNIT,'(A)')'; '
         DO IREACT = 1, NREACT( NXX )
            ISPC = IRR( NXX, IREACT )
            WRITE(TABLE_UNIT,4705, ADVANCE = 'NO')IREACT,TRIM(SPARSE_SPECIES( ISPC ))
         END DO
4705     FORMAT("Gstr{i,",I4,"}='",A,"';")            
         WRITE(TABLE_UNIT,*)
         DO IREACT = 1, NREACT( NXX )
            ISPC = IRR( NXX, IREACT )
            WRITE(TABLE_UNIT,'(A, A)', ADVANCE = 'NO')
     &      "f" // TRIM(SPARSE_SPECIES( ISPC )) // "(i)=f" // TRIM(SPARSE_SPECIES( ISPC )) // "(i)-1.0;"
         END DO
         WRITE(TABLE_UNIT,*)
         DO IPRODUCT = 1, NPRDCT( NXX )
            ISPC = IRR( NXX, IPRODUCT + 3 )
            IF(SC(NXX,IPRODUCT) .GT. 0.0)THEN
                WRITE(TABLE_UNIT,'(A,F8.4,A)', ADVANCE = 'NO')
     &          "f" // TRIM(SPARSE_SPECIES( ISPC )) // "(i)=f" // TRIM(SPARSE_SPECIES( ISPC )) // "(i)+",
     &           SC(NXX,IPRODUCT),";"
            ELSE
                WRITE(TABLE_UNIT,'(A,F8.4,A)', ADVANCE = 'NO')
     &          "f" // TRIM(SPARSE_SPECIES( ISPC )) // "(i)=f" // TRIM(SPARSE_SPECIES( ISPC )) // "(i)-",
     &           ABS(SC(NXX,IPRODUCT)),";"
            END IF
         END DO
         IF( WRITE_DELTA_ATOMS )THEN
             DO IATOM = 1, N_ATOMS
                IF( NONZERO_DELTA(NXX,IATOM) )THEN
                   IF( REACTION_DELTA(NXX,IATOM) .GT. 0.0)THEN
                       WRITE(TABLE_UNIT,'(A,F8.4,A)', ADVANCE = 'NO')
     &                    "fDELTA_" // TRIM(ATOMS(IATOM)) // "(i)=fDELTA_" // TRIM(ATOMS(IATOM)) // "(i)+",
     &                     REACTION_DELTA(NXX,IATOM),";"
                   ELSE
                       WRITE(TABLE_UNIT,'(A,F8.4,A)', ADVANCE = 'NO')
     &                    "fDELTA_" // TRIM(ATOMS(IATOM)) // "(i)=fDELTA_" // TRIM(ATOMS(IATOM)) // "(i)-",
     &                     ABS(REACTION_DELTA(NXX,IATOM)),";"
                   END IF
                END IF
             END DO
         END IF         
         WRITE(TABLE_UNIT,'(/)')
      END DO
      DO IDX = 1,NPHOTAB
         WRITE(JTABLE_UNIT,95210)TRIM( PHOTAB(IDX) ),TRIM( PHOTAB(IDX) )
         WRITE(BOTTOM_UP_UNIT,'(/,A)')  "i = i + 1"
         WRITE(BOTTOM_UP_UNIT,'(A,A)')  "Jnames{i}   = 'J_" // TRIM( PHOTAB(IDX) ) // "';",
     &   " % " // TRIM( PHOTAB_STR( IDX ) )               
         WRITE(BOTTOM_UP_UNIT,'(A)')    "CS{i} = 'Cross_Section_" // TRIM( PHOTAB(IDX) ) // ".csv';"
         WRITE(BOTTOM_UP_UNIT,'(A,/)')  "QY{i} = 'Quantum_Yield_" // TRIM( PHOTAB(IDX) ) // ".csv';"
      END DO
      DO IDX = 1,NHETERO
         WRITE(KTABLE_UNIT,'(/,A)')"i = i + 1"
         WRITE(KTABLE_UNIT,'(A)')  "Knames{i}   = 'K_" // TRIM( HETERO(IDX) ) // "';"
         WRITE(KTABLE_UNIT,'(A,/)')"krx(:,i)    =  1.0E-60;"
      END DO
      WRITE(KTABLE_UNIT,95310)
      
      CLOSE(TABLE_UNIT)
      CLOSE(KTABLE_UNIT)
      CLOSE(JTABLE_UNIT)
      CLOSE(BOTTOM_UP_UNIT)


      RETURN
      
    
69099 FORMAT("%Foam Reactions File based on the mech.def file for the ",A," mechanism.", /,
     &   "% # of species   = ",I4, /,
     &   "% # of reactions = ",I4,/,
     &   "% file created by ",A,//,
     &   "% Set constant species by scaling to air number density")

69100 FORMAT("% For the ", A, " mechanism, the operators are defined  below.")
69101 FORMAT( / "%where <REACTION> is the rate constant for the given REACTION and [species] ",
     &          "equals the concentration of a mechanism species at the beginning of ",
     &          "the integration time-step for the chemistry's numerical solver." /)

4706   FORMAT("k(:,", I4,")")
3708   FORMAT(A,1X,".*", A)
4708   FORMAT(A,1X,ES9.2,".*", A)
5708   FORMAT(A,1X,ES9.2,".*", A)
4709   FORMAT( A )     
4710   FORMAT(A,1X,ES9.2,'.*', A)
4711   FORMAT(1X)
4712   FORMAT(A, 1X, A)
5000   FORMAT(ES12.4,".*J",A)
5001   FORMAT(  "J",A )
5100   FORMAT(ES12.4,'.*',I4)
5101   FORMAT(I4)
5006   FORMAT(ES12.4,".*k(:,", I4, ")")   
5007   FORMAT(ES12.4,' .*( 1.0D0 + 0.6D0 .* P )')             
5011   FORMAT(ES12.4,".*",A)             
5012   FORMAT(A)
5013   FORMAT(ES12.4,"exp(",ES12.4,"./T).*",A)             
5014   FORMAT("exp(",ES12.4,"./T)",A)             
5027   FORMAT(ES12.4,".*K_",A)
5028   FORMAT( "K_",A )

5111   FORMAT(ES12.4) 
5129   FORMAT(ES12.4,'.*(T./300).^(', ES12.4,')')
5102   FORMAT(ES12.4,'.*(T./300).^(', ES12.4,')')
5103   FORMAT(ES12.4,'.*exp(', ES12.4,'./T)')
5104   FORMAT(ES12.4,'.*exp(',ES12.4,'./T).*(T./300).^('ES12.4,' )')
5114   FORMAT(ES12.4,'P.*(T./300).^(', ES12.4,' )')
5115   FORMAT( ES12.4,'.*exp(', ES12.4,'./T).*k(:,', I4, ')')
5108    FORMAT(' xk0 = ', ES12.4,'.*exp(',ES12.4,'./T);',/
     &         ' xk2 = ', ES12.4,'.*exp(',ES12.4,'./T);',/,
     &         ' xk3 = ', ES12.4,'.*exp(',ES12.4,'./T);',/)
7108    FORMAT(A,'xk0+xk3.*M./(1.0+xk3.*M./xk2)')     
5109    FORMAT('xk0 = ', ES12.4,'.*exp(',ES12.4,'./T);',/,
     &         'xk1 = ', ES12.4,'.*exp(',ES12.4,'./T);',/)
7109    FORMAT(A,'xk0+xk1.*M')     
! FALL 10
5110    FORMAT('xko = ', ES12.4,'.*M.*exp(',ES12.4,'./T).*(T./300).^',ES12.4,';',/,
     &         'xkinf = ', ES12.4,'.*exp(',ES12.4,'./T).*(T./300).^',ES12.4,';',/,
     &         'xn = ', ES12.4,';',/,
     &         'F = ', ES12.4,';'/,
     &         'G=1.0./(1.0+(log10(xko./xkinf)./xn).^2);',/)
7110    FORMAT(A,'xko./( 1.0+xko./xkinf).*F.^G')
!FALL 11
5119    FORMAT( 'xk0 = ', ES12.4,'.*exp(',ES12.4,'./T).*(T./300).^',ES12.4,';',/,
     &          'xk1 = ', ES12.4,'.*exp(',ES12.4,'./T).*(T./300).^',ES12.4,';',/,
     &          'xk2 = ', ES12.4,'.*exp(',ES12.4,'./T);',/)
7119   FORMAT(A,'xk0+xk1.*M+xk2')     
5120   FORMAT('ILLUMINATED =  ( SZA > 0.0 );',/,
     &        'OPEN_OCEAN  = 0.0;',/,
     &        'SURF_ZONE   = 0.0;',/,
     &        'SEA_ICE     = 0.0;',/,
     &        'SEAWATER    = (OPEN_OCEAN+SURF_ZONE)-SEA_ICE;',/,
     &        'SEAWATER(SEAWATER<0.0)    = 0.0;',/,
     &        'Patm = 0.001.*P;',/,
     &        'a = ',  ES10.3,'.*exp(',ES10.3'.*Patm) +', ES10.3,'.*exp(',ES10.3'.*Patm);',/,
     &        'b = ',ES10.3,';',/,
     &        'a(a>b) = b;',/,
     &        'k(:,i) = a.*ILLUMINATED.*SEAWATER;',/)

95100  FORMAT(2X,A16,' = 0.0D0')        


95200  FORMAT("function J = ",A,"_J(Met,Jmethod)",/,
     &        "% Calculates photolysis frequencies for the ",A," mechanism in the CMAQ model",/,
     &        "% Met: structure containing required meteorological constraints. Required vars depend on Jmethod.",/,
     &        "%       Met.SZA: solar zenith angle in degrees",/,
     &        "%       Met.ALT: altitude, meters",/,
     &        "%       Met.O3col: overhead ozone column, DU",/,
     &        "%       Met.albedo: surface reflectance, 0-1 (unitless)",/,
     &        "%       Met.T: temperature, T",/,
     &        "%       Met.P: pressure, mbar",/,
     &        "%       Met.LFlux: name of a text file containing an actinic flux spectrum",/,
     &        "%",/,
     &        "% Jmethod: numeric flag or string specifying how to calculate J-values. Default is 'MCM'.",/,
     &        "%       0 or 'MCM':      use MCMv3.3.1 parameterization.",/,
     &        "%                         Some reactions are not included in MCM. For these, 'HYBRID' values are used.",/,
     &        "%                         Required Met fields: SZA",/,
     &        "%       1 or 'BOTTOMUP': bottom-up integration of cross sections/quantum yields.",/,
     &        "%                         See J_BottomUp.m for more info.",/,
     &        "%                         Required Met fields: LFlux, T, P",/,
     &        "%       2 or 'HYBRID':   Interpolation of hybrid J-values from TUV solar spectra.",/,
     &        "%                         See J_TUVhybrid.m for more info.",/,
     &        "%                         Required Met fields: SZA, ALT, O3col, albedo",/,
     &        "%",/,
     &        "% OUTPUTS:",/,
     &        "% J: structure of J-values.",/,
     &        "%",/,
     &        "% INPUTS",/,
     &        "struct2var(Met)",//,
     &        "if nargin<2",/,
     &        "    Jmethod = 'MCM';",/,
     &        "elseif ischar(Jmethod)",/,
     &        "    Jmethod = upper(Jmethod);",/,
     &        "end",//,
     &        "% J-Values",/,
     &        "switch Jmethod",/,
     &        "    case {0,'MCM'}",/,
     &        "        error(['MCM option not functional for ",A," mechanism.'])",//,
     &        "    case {1,'BOTTOMUP'}",/,
     &        "        Jmcm = J_BottomUp(LFlux,T,P);",//,
     &        "    case {2,'HYBRID'}",/,
     &        "        Jmcm = J_Hybrid(SZA,ALT,O3col,albedo);",//,
     &        "    otherwise",/,
     &        "        fprintf('Jmethod = %f\n',Jmethod);",/,
     &        "        error(['MCMv331_J: invalid Jmethod option selected'])",//,
     &        "end",/,
     &        "%rename",/,
     &        "J=struct;")
95210   FORMAT("J.J",A," = Jmcm.J_",A,";")

95300  FORMAT("function K = ",A,"_K(Met,Jmethod)",/,
     &        "% Calculate heteorogeneous and other rate constants for use with the ",A," chemical mechanism.",/,
     &        "% Met: structure containing required meteorological constraints. Required vars depend on Jmethod.",/,
     &        "%       Met.T: temperature, T",/,
     &        "%       Met.P: pressure, mbar",/,
     &        "%",/,
     &        "%",/,
     &        "% OUTPUTS:",/,
     &        "% J: structure of J-values.",/,
     &        "%",/,
     &        "% INPUTS",/,
     &        "struct2var(Met)",//,
     &        "nk = ",I3,"; %number of rate constants",/,
     &        "krx = nan(length(T),nk);",/,
     &        "krx = 1.0E-60;",/,
     &        "Knames = cell(nk,1);",/,
     &        "i=0;",/)

95310   FORMAT("%% accumulate",/,
     &        "K = struct;",/,
     &        "for i=1:length(Knames)",/,
     &        "    K.(Knames{i}) = krx(:,i);",/,
     &        "end")

       END SUBROUTINE WRT_FOAM_TABLES
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
       SUBROUTINE ECHO_MECH( IUNIT,N_DROP_SPC,DROP_SPC  )
 
  
          USE GET_ENV_VARS
          USE MECHANISM_DATA, MECHANISM => MECHNAME
          USE GET_MECHDEF_DATA
          USE SPECIES_ATOMS_DATA
           
          
          IMPLICIT NONE
          
          INTEGER,         INTENT( IN     ) :: IUNIT  ! unit number for in mechanism definitions files
          INTEGER,         INTENT( INOUT  ) :: N_DROP_SPC
          CHARACTER( 16 ), INTENT( INOUT  ) :: DROP_SPC( : )
          
          
          
          
c..local Variables for steady-state species

         CHARACTER(  1 ) :: CHR
         CHARACTER( 16 ) :: WORD
         CHARACTER( 37 ) :: PHRASE
         CHARACTER( 181 ) :: INBUF
         CHARACTER( 181 ) :: INBUF2
         
         CHARACTER(  3 )   :: END
         INTEGER, EXTERNAL :: INDEX1
         INTEGER           :: LPOINT, IEOL
         INTEGER           :: I, ICOL, ISPC, JSPC, IRX, IDX      
         INTEGER           :: NR, IPX, NXX, NS, IPR, IPHOTAB, NC
         
         CHARACTER( 12 )  :: MECHNAME      = 'MECHDEF'
         CHARACTER( 586 ) :: EQNAME_MECH
         CHARACTER( 600 ) :: COPY_MECH
         CHARACTER( 891 ) :: RATE_FORMULA
         CHARACTER(  16 ) :: COEFF_STRING
         CHARACTER(  32 ) :: DESCRP_MECH
         
         
         CHARACTER(  16 ) :: CLABEL                  ! mechanism constants label
         REAL( 8 )        :: CONSTVAL                ! retrieved constant
         
         INTEGER, PARAMETER :: LUNOUT = 6
         
         
         REAL             :: REACTION_BALANCE 
         
         CHARACTER(   16 ) :: OUT_DIR          = 'OUTDIR'
         CHARACTER( 1000 ) :: REACTION_STRING
         CHARACTER(  100 ) :: SUB_STRING
         CHARACTER(   16 ) :: NAME_PRODUCT
         
         
         INTEGER, EXTERNAL  :: JUNIT
         INTEGER            :: ICOUNT, IREACT, IPRODUCT, IATOM
         INTEGER            :: POUND 
         INTEGER            :: PERCENT
         INTEGER            :: SEMICOLON
         INTEGER            :: IPOS, IPOS_DOT
         
         INTEGER            :: STATUS
         
         LOGICAL            :: PAST_REACTIONS
         LOGICAL            :: SAVE_RATE
         LOGICAL            :: ECHO_TAIL
         LOGICAL            :: ECHO_HEAD
         
         LOGICAL            :: ELIMINATE_DONE
           
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Initialize module and local mechanism array variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

! check if atoms_species is a mechanisms and set atoms in mechanism species

         CALL SET_ATOMS_MECHANISM_SPC()
         IF( ATOMS_IN_NAMELISTS )THEN
            CALL ARE_THEY_ATOM_SPECIES()
         END IF
                  
! determine how reactions change atoms
     
         CALL REACTION_DELTA_ATOMS()

! determine whether to write out CMAQ CGRID species name and indices to output

         REWIND( IUNIT )

         CALL VALUE_NAME ( MECHNAME, EQNAME_MECH )
         CALL VALUE_NAME ( OUT_DIR, OUTDIR )
         COPY_MECHANISM = JUNIT()
         ICOL = LEN_TRIM( EQNAME_MECH )
         IDX  = INDEX( EQNAME_MECH,'/',.TRUE.)
         COPY_MECH = TRIM( OUTDIR )  // EQNAME_MECH(IDX:ICOL)  ! // "_deltas"
         OPEN ( UNIT = COPY_MECHANISM, FILE = COPY_MECH, STATUS = 'UNKNOWN' )
         ECHO_TAIL = .FALSE.
         ECHO_HEAD = .TRUE.
         EXUNIT_ATOMS_REPORT = JUNIT()
         EQNAME_ATOMS_REPORT = TRIM( OUTDIR ) // '/' // TRIM( MECHNAME_LOWER_CASE ) 
     &                       // '_reaction_deltas.txt'
         OPEN( FILE=TRIM(EQNAME_ATOMS_REPORT),UNIT=EXUNIT_ATOMS_REPORT,STATUS='UNKNOWN' )                                      
         NXX = 0
! scan through file 

         WRITE( EXUNIT_ATOMS_REPORT, '("!For the mechanism, ",A,",")')TRIM(MECHANISM)
         WRITE( EXUNIT_ATOMS_REPORT, 95001)
95001    FORMAT( "! The information is given as comments below each reaction.",/,
     &           "! the revised mechanism definitions files.",/,
     &           "! The information is given as comments below each reaction.",/,
     &           "! Math is between the equal signs.",/,
     &           "!   For reactants, the format is minus the number of atoms times the element.",/,
     &           "!   For products, the format is number of atoms times the stiochiometric coefficienct times the element.")
     
         ELIMINATE_DONE = .FALSE.
         READ_MECHDEF: DO      
         
           READ(IUNIT,'(A)',END=999)INBUF
           INBUF2  = INBUF
           CALL CONVERT_CASE ( INBUF2, .TRUE. )
           WORD(1:4) = INBUF2(1:4)
         
           IF( WORD(1:4) .NE. 'ELIM' .AND. WORD(1:4) .NE. 'REAC' )THEN
           
              IF( LEN_TRIM(INBUF2) .LT. 1 .OR. INBUF2(1:1) .EQ. "!")THEN
                  WRITE(COPY_MECHANISM,'(A)')TRIM( INBUF )
                  CYCLE
              ELSE IF( ECHO_TAIL )THEN
                  WRITE(COPY_MECHANISM,'(A)')TRIM(INBUF)
              ELSE IF( ECHO_HEAD )THEN
                  WRITE(COPY_MECHANISM,'(A)')TRIM(INBUF)
              END IF
         
           END IF 
           
           IF( WORD(1:4) .EQ. 'ELIM' )THEN
! skip original elimnate block and rewrite      
               DO
                  READ(IUNIT,'(A)',END=999)INBUF
                  INBUF2  = ADJUSTL(INBUF) 
                  CALL CONVERT_CASE ( INBUF2, .TRUE. )
                  IF( INBUF2(1:3) .EQ. 'END' )EXIT
               END DO
           END IF
           IF( NONZERO_ATOMS .AND. .NOT. ELIMINATE_DONE )THEN
! reset drop_spc
              WRITE(COPY_MECHANISM,'(/,"ELIMINATE = ")')
               N_DROP_SPC = 0
               DROP_SPC   = ''
               DO IATOM = 1,N_ATOMS
                  IF( ATOM_FOUND( IATOM ) )THEN
                      N_DROP_SPC = N_DROP_SPC + 1
                      DROP_SPC( N_DROP_SPC ) = "DELTA_" // TRIM( ATOMS(IATOM))
                      WRITE(COPY_MECHANISM,'(A,";")')TRIM( DROP_SPC(N_DROP_SPC) )
                  END IF
               END DO
               WRITE(COPY_MECHANISM,'("END ELIMINATE",/)')
               ELIMINATE_DONE = .TRUE.
!               CYCLE
           END IF 

                
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C check if reactions block encountered
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
225        IF ( WORD( 1:4 ) .EQ. 'REAC' ) THEN
           
                ECHO_HEAD = .FALSE.
                WRITE(COPY_MECHANISM,'(A)')INBUF
                
                READ_REACTIONS: DO
                  READ(IUNIT,'(A)',END=999)INBUF
                  INBUF2  = ADJUSTL(INBUF)               
                  CALL CONVERT_CASE ( INBUF2, .TRUE. )
                  IF( LEN_TRIM(INBUF2) .LT. 1 .OR. INBUF2(1:1) .EQ. "!")THEN
                     WRITE(COPY_MECHANISM,'(A)')TRIM( INBUF )
                     CYCLE
                  ELSE IF( INBUF2(1:1) .EQ. '<' )THEN
                     NXX = NXX + 1 
! find start of substring with rate constant formula               
                     SAVE_RATE = .FALSE.
                     RATE_FORMULA(:) = ' '
                     FIND_RATE: DO 
                        POUND     = INDEX(INBUF,'#')
                        PERCENT   = INDEX(INBUF,'%')
                        IF( PERCENT .GT. 0 )THEN
                           SAVE_RATE = .TRUE.
                           ICOL = PERCENT
                           EXIT FIND_RATE
                        ELSE IF( POUND .GT. 0 )THEN
                           SAVE_RATE = .TRUE.
                           ICOL = POUND
                           EXIT FIND_RATE
                        END IF
                        READ(IUNIT,'(A)',END=999)INBUF
                     END DO FIND_RATE
! save substring with rate constant formula               
                     IF( SAVE_RATE )THEN
                        IDX = LEN_TRIM(INBUF)
                        IPX  = IDX - ICOL + 1   
!                        RATE_FORMULA(1:) = INBUF(ICOL:IDX)
                        
                        BUILD_FORMULA: DO                    
! append formula until semicolon found
                           SEMICOLON = INDEX(INBUF,';')
                           IPR = LEN_TRIM( RATE_FORMULA ) + 1
                           IF( SEMICOLON .GT. 0 )THEN
                               IF( IPR .GT. 1 )THEN
                                  RATE_FORMULA = TRIM(RATE_FORMULA) 
     &                                        // " " // TRIM(ADJUSTL(INBUF))
                               ELSE
                                  RATE_FORMULA(1:IPX) = INBUF(ICOL:IDX)
                               END IF
                              EXIT                           
                           END IF
                           RATE_FORMULA(IPR:) = " " // TRIM( INBUF(ICOL:) )
                           READ(IUNIT,'(A)',END=999)INBUF
                        END DO BUILD_FORMULA                       
                     END IF
! write reaction and rate formula                
!                    WRITE(COPY_MECHANISM,'(A)',ADVANCE='NO')'<' // TRIM( RXLABEL( NXX) ) // '> '
                     REACTION_STRING(:) = ''
                     REACTION_STRING(1:) = '<' // TRIM( RXLABEL( NXX) ) // '> '
                     WRITE_REACTANTS: DO IREACT = 1, NREACT( NXX )
                           SUB_STRING(:)      = ' '
                           ISPC = IRR( NXX, IREACT )
                           IF( IREACT .LT. 2 )THEN
                              ICOUNT = 1 + LEN( SPARSE_SPECIES( ISPC ) )
                              WRITE(SUB_STRING,'(A, A)')TRIM(SPARSE_SPECIES( ISPC )),' '
                              ICOUNT = 1 + LEN( SPARSE_SPECIES( ISPC ) )
                           ELSE
                              WRITE(SUB_STRING,'(3A)')'+ ',TRIM(SPARSE_SPECIES( ISPC )),' '
                              ICOUNT = 1 + LEN( SPARSE_SPECIES( ISPC ) )
                              ICOUNT = 3 + LEN( SPARSE_SPECIES( ISPC ) )                  
                           END IF
                           REACTION_STRING = TRIM( REACTION_STRING ) // ' ' // TRIM( SUB_STRING )
                     END DO WRITE_REACTANTS
                     DO I = 1, MAXRCTNTS
                         SUB_STRING(:)      = ''
                         IF( INDEX_FIXED_SPECIES( NXX, I ) .GT. 0 .AND. INDEX_FIXED_SPECIES( NXX, I ) .LT. 7 )THEN
                             ISPC = INDEX_FIXED_SPECIES( NXX, I )
                              WRITE(SUB_STRING,'(3A)')'+ ',TRIM( FIXED_SPECIES( ISPC ) ),' '
                              ICOUNT = 1 + LEN( FIXED_SPECIES( ISPC ) )
                              ICOUNT = 3 + LEN( FIXED_SPECIES( ISPC ) )                  
                         ELSE 
                              IF( INDEX_FIXED_SPECIES( NXX, I ) .GE. 7 )THEN
                                 WRITE(*,*)'WARNING: INDEX_FIXED_SPECIES( ', NXX,',', I, ') = ',INDEX_FIXED_SPECIES( NXX, I )
                              END IF
                         END IF         
                         REACTION_STRING = TRIM( REACTION_STRING ) // ' ' // TRIM( SUB_STRING )
                     END DO
                     REACTION_STRING = TRIM( REACTION_STRING ) // ' ='
! write out products, both constant and mechanism species
                     WRITE_PRODUCTS: DO IPRODUCT = 1, N_ALL_PRODUCTS(NXX) ! NPRDCT( NXX )
                        JSPC = INDEX_PRODUCT( NXX,IPRODUCT )
                        IF( JSPC .GT. 0 )THEN ! product is mechanism species
                           NAME_PRODUCT  = SPARSE_SPECIES( JSPC )
                        ELSE IF( JSPC .LT. 0 ) THEN ! product is a constant species
                           NAME_PRODUCT  = FIXED_SPECIES( ABS( JSPC ) )
                        ELSE
                           NAME_PRODUCT  = 'UNDEFINED'
                        END IF
                        SUB_STRING(:) = ''
! convert stoichemtric cofficient to character string
                        IF( STOICHIOMETRIC_COEFF( NXX,IPRODUCT ) .NE. 0.0 )THEN
                           WRITE(COEFF_STRING,'(F14.9)')ABS( STOICHIOMETRIC_COEFF( NXX,IPRODUCT ) )
                           COEFF_STRING = ADJUSTL( COEFF_STRING )
                           IPOS_DOT = INDEX(COEFF_STRING,".",BACK=.TRUE.)+2
                           DO
                             IPOS = LEN_TRIM( COEFF_STRING )
                             IF( COEFF_STRING(IPOS:IPOS) .NE. '0')EXIT
                             IPOS = IPOS-1
                             IF( IPOS .LE. IPOS_DOT )EXIT
                             COEFF_STRING = COEFF_STRING(1:IPOS)
                           END DO
                        ELSE
                           COEFF_STRING = "0.000"
                        END IF
                        IF ( ABS( STOICHIOMETRIC_COEFF( NXX,IPRODUCT ) ) .NE. 1.0 ) THEN
                           IF (  STOICHIOMETRIC_COEFF( NXX,IPRODUCT ) .LT. 0.0 ) THEN
                              WRITE(SUB_STRING,'(A,A,3A)')
     &                       '- ',TRIM(COEFF_STRING),'*',TRIM(NAME_PRODUCT),' '
                              ICOUNT = ICOUNT + 12 + LEN_TRIM( NAME_PRODUCT )
                           ELSE
                              IF( IPRODUCT .EQ. 1 )THEN
                                 WRITE(SUB_STRING,'(A, 3A)')
     &                           TRIM(COEFF_STRING),'*',TRIM(NAME_PRODUCT),' '
                                 ICOUNT = ICOUNT + 10 + LEN_TRIM(NAME_PRODUCT)
                              ELSE
                                 WRITE(SUB_STRING,'(A,A,3A)')
     &                           '+ ',TRIM(COEFF_STRING),'*',TRIM(NAME_PRODUCT),' '
                                 ICOUNT = ICOUNT + 12 + LEN_TRIM(NAME_PRODUCT)
                              END IF
                           END IF
                        ELSE IF (  STOICHIOMETRIC_COEFF( NXX,IPRODUCT ) .LT. 0.0 ) THEN
                           WRITE(SUB_STRING,'(3A)')
     &                           '- ',TRIM(NAME_PRODUCT),' '
                           ICOUNT = ICOUNT + 3 + LEN_TRIM(NAME_PRODUCT)
                        ELSE
                           IF( IPRODUCT .EQ. 1 )THEN
                              WRITE(SUB_STRING,'(3A)')
     &                        TRIM(NAME_PRODUCT),' '
                              ICOUNT = ICOUNT + 2 + LEN_TRIM(NAME_PRODUCT)
                           ELSE
                              WRITE(SUB_STRING,'(3A)')
     &                         '+ ',TRIM(NAME_PRODUCT),' '
                              ICOUNT = ICOUNT + 3 + LEN_TRIM(NAME_PRODUCT)
                           END IF
                        END IF
                        REACTION_STRING = TRIM( REACTION_STRING ) // ' ' // TRIM( SUB_STRING )
                     END DO  WRITE_PRODUCTS
                     REACTION_STRING = TRIM( REACTION_STRING ) // ' ' // TRIM(DELTA_ATOMS(NXX))
                     REACTION_STRING = TRIM( REACTION_STRING ) // ' ' // TRIM(RATE_FORMULA)
                     CALL WRITE_REACTION (iunit=COPY_MECHANISM,text=REACTION_STRING,width=72)
! write reaction and math for delta atoms for inspection                   
                      WRITE(EXUNIT_ATOMS_REPORT,'(2A)')TRIM(REACTION_STRING)
                      CALL WRITE_DELTA_MATH(EXUNIT_ATOMS_REPORT,NXX)
                   ELSE IF( INBUF2(1:3) .EQ. 'END' )THEN
                     WRITE(COPY_MECHANISM,'(A)')TRIM(INBUF)
                     PAST_REACTIONS   = .TRUE.
                     ECHO_TAIL        = .TRUE.
                     EXIT READ_REACTIONS
                   END IF                
             END DO READ_REACTIONS    
        END IF
         
      END DO READ_MECHDEF
      
      CLOSE(EXUNIT_ATOMS_REPORT)
999   CLOSE(COPY_MECHANISM) 

1013  FORMAT(  3X, 'Reaction: ',A16,' uses ',A16,' defined in the functions block' )
                       
      
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
2014  FORMAT( / 5X, '*** ERROR: Reactants cannot have negative coefficients'
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
2023  FORMAT( / 5X, '*** ERROR: KTYPE 12 reaction: Number, Label: ', I6, 1X, A,
     &        / 5X, 'points to nonexistant photolysis rate: ', A )
2021  FORMAT( / 5X, '*** ERROR: Label points to currently undefined reaction'
     &        / 5X, 'Processing for reaction #:', I6,' Label: ', A16,' references label:', A16)
2024  FORMAT( / 5X, '*** ERROR: Reaction label refers to reference itself for rate constant '
     &        / 5X, 'Processing for reaction number:', I6, 1X, A )
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
5100   FORMAT(1PD12.4,' * RCONST( ',I4,' )')
5101   FORMAT(  'RCONST( ',I4,' )')
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
5014   FORMAT('ARRD( ',1PD12.4,', 0.0000D+0,', 1PD12.4,' )  * PRESS ')             
5019   FORMAT('EP4D( ', 5(1PD12.4,', '), 1PD12.4,' )')
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


 
       RETURN 
       END SUBROUTINE ECHO_MECH

       END MODULE MECHANISM_DOCS
