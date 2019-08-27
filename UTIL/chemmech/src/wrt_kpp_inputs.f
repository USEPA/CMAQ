
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
      SUBROUTINE WRT_KPP_INPUTS( NR, IP, LABEL, NS  )

 
      USE KPP_DATA
      USE GET_ENV_VARS
      USE MECHANISM_DATA
      
      IMPLICIT NONE

      INTEGER,         INTENT( IN ) :: NR ! number of reactions
      INTEGER,         INTENT( IN ) :: IP ! number of photolysis reaction
      CHARACTER( 16 ), INTENT( IN ) :: LABEL( :,: ) ! LABEL(NXX,1) 1st label found in rx NXX
                                                            ! LABEL(NXX,2) 2nd label found in rx NXX
      INTEGER,         INTENT( IN ) :: NS ! number of species

c..local Variables for steady-state species

       
      CHARACTER(  1 )  :: CHR
      CHARACTER( 16 )  :: WORD
      CHARACTER( 37 )  :: PHRASE
      CHARACTER( 81 )  :: INBUF
      CHARACTER( 16 )  :: EQNS_KPP_FILE = 'EQNS_KPP_FILE'
      CHARACTER( 16 )  :: SPCS_KPP_FILE = 'SPCS_KPP_FILE'
      CHARACTER(  3 )  :: END
      CHARACTER( 140 ) :: FILE_LINE

      INTEGER, EXTERNAL :: INDEX1
      INTEGER, EXTERNAL :: INDEXES
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

      INTEGER SPC1RX( MAXSPEC )              ! rx index of 1st occurence of species
                                             ! in mechanism table
      CHARACTER( 586 ) :: EQN_MECH_KPP
      CHARACTER( 586 ) :: SPC_MECH_KPP
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
      REAL( 8 )            :: CVAL( MAXCONSTS )      ! mechanism constants value
      INTEGER, PARAMETER  :: LUNOUT = 6
      INTEGER             :: IDIFF_ORDER             ! difference between order of two separate reactions
      LOGICAL             :: FALLOFF_RATE            ! whether a reaction is a falloff type
      LOGICAL             :: EXISTING


      CHARACTER(  12 ) :: EXFLNM_SPCS = 'SPCSDATX'
      CHARACTER(  12 ) :: EXFLNM_RXDT = 'RXNSDATX'
      CHARACTER(  12 ) :: EXFLNM_RXCM = 'RXNSCOMX'

      INTEGER, EXTERNAL :: JUNIT
      INTEGER            :: ICOUNT, IREACT, IPRODUCT

      INTERFACE 
       SUBROUTINE GETRCTNT ( IMECH, INBUF, IEOL, LPOINT, CHR, WORD,
     &                      NXX, NS, SPARSE_SPECIES, SPC1RX,
     &                      ICOL, LABEL, N_DROP_SPC, DROP_SPC )
         INTEGER,         INTENT(   IN  ) :: IMECH
         CHARACTER( 81 ), INTENT( INOUT ) :: INBUF
         INTEGER,         INTENT( INOUT ) :: LPOINT
         INTEGER,         INTENT( INOUT ) :: IEOL
         CHARACTER(  1 ), INTENT( INOUT ) :: CHR
         CHARACTER( 16 ), INTENT( INOUT ) :: WORD
         INTEGER,         INTENT(   IN  ) :: NXX
         INTEGER,         INTENT( INOUT ) :: NS
         CHARACTER( 16 ), INTENT( INOUT ) :: SPARSE_SPECIES( : )
         INTEGER,         INTENT( INOUT ) :: SPC1RX( : )
         INTEGER,         INTENT( INOUT ) :: ICOL
         CHARACTER( 16 ), INTENT(   IN  ) :: LABEL( :, : )
         INTEGER,         INTENT(   IN  ) :: N_DROP_SPC
         CHARACTER( 16 ), INTENT(   IN  ) :: DROP_SPC( : )
        END SUBROUTINE GETRCTNT
        SUBROUTINE GETPRDCT ( IMECH, INBUF, LPOINT, IEOL, CHR, WORD,
     &                      NXX, NS, SPARSE_SPECIES, SPC1RX,
     &                      ICOL, N_DROP_SPC, DROP_SPC )
          INTEGER,         INTENT(   IN  ) :: IMECH
          CHARACTER( 81 ), INTENT( INOUT ) :: INBUF
          INTEGER,         INTENT( INOUT ) :: LPOINT
          INTEGER,         INTENT( INOUT ) :: IEOL
          CHARACTER(  1 ), INTENT( INOUT ) :: CHR
          CHARACTER( 16 ), INTENT( INOUT ) :: WORD
          INTEGER,         INTENT(   IN  ) :: NXX
          INTEGER,         INTENT( INOUT ) :: NS
          CHARACTER( 16 ), INTENT( INOUT ) :: SPARSE_SPECIES( : )
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
        SUBROUTINE WREXTS (EQNAME_MECH, DESCRP_MECH, NS, SPARSE_SPECIES, SPC1RX, NR,
     &                      IP,  NAMCONSTS, CVAL, SS1RX  ) 
          CHARACTER( 120 ), INTENT ( IN ) :: EQNAME_MECH
          CHARACTER(  32 ), INTENT ( IN ) :: DESCRP_MECH
          INTEGER,          INTENT ( IN ) :: NS                ! no. of species found in mechanism table
          CHARACTER(  16 ), INTENT ( IN ) :: SPARSE_SPECIES( : ) ! species list from mechanism table
          INTEGER,          INTENT ( IN ) :: NR
          INTEGER,          INTENT ( IN ) :: SPC1RX( : ) ! rx index of 1st occurence of species in mechanism table
          INTEGER,          INTENT ( IN ) :: IP
          CHARACTER( 16 ),  INTENT ( IN ) :: NAMCONSTS( : )
          REAL( 8 ),        INTENT ( IN ) :: CVAL( : )
          INTEGER,          INTENT ( IN ) :: SS1RX( : )
        END SUBROUTINE WREXTS
        SUBROUTINE GET_SS_DATA ( LUNOUT, NR ) 
          INTEGER, INTENT ( IN )         :: LUNOUT   ! Output unit number
          INTEGER, INTENT ( IN )         :: NR       ! No. of reactions
        END SUBROUTINE GET_SS_DATA
        SUBROUTINE CHECK_SS_SPC ( LUNOUT, NS, SPARSE_SPECIES, NR, LABEL, SS1RX )
         INTEGER, INTENT ( IN )         :: LUNOUT               ! Output unit number
         INTEGER, INTENT ( IN )         ::  NS                  ! No. of species in mechanism
         CHARACTER( 16 ), INTENT ( IN ) ::  SPARSE_SPECIES( : )   ! List of mechanism species
         INTEGER, INTENT ( IN )         ::  NR                  ! No. of reactions
         CHARACTER( 16 ), INTENT ( IN ) ::  LABEL( :,: ) ! Reaction labels
         INTEGER, INTENT ( INOUT )      ::  SS1RX( : )
       END SUBROUTINE CHECK_SS_SPC
       SUBROUTINE WRSS_EXT( NR ) 
         INTEGER, INTENT ( IN )         :: NR   ! No. of reactions
       END SUBROUTINE WRSS_EXT
      END INTERFACE 
  

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Find names for KPP output files
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL VALUE_NAME ( EQNS_KPP_FILE, EQN_MECH_KPP )
      CALL VALUE_NAME ( SPCS_KPP_FILE, SPC_MECH_KPP )


! write out reactions strings to determine KPP information

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
         REACTION_STR( NXX ) = TRIM(REACTION_STR( NXX )) //  ' = '
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
         WRITE(KPPEQN_UNIT,6001)SPARSE_SPECIES( ISPC )
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
      WRITE(KPPEQN_UNIT,4501)TRIM( MECHNAME ), NPHOTAB
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
      IF ( MAXVAL( CONST ) .GT. 0.0D0 ) THEN
         DO IPR = 1, MAXCONSTS
            ISPC = INDEX1 ( TRIM( NAMCONSTS( IPR ) ), MAXCONSTS, NAMCONSTS )
            IF( ISPC .LT. 1 )CYCLE
            WRITE( KPPEQN_UNIT, 1310 )  NAMCONSTS( IPR ), REAL(CONST( ISPC ), 8)
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
      IF( NFUNCTIONS .GT. 0 )THEN
         WRITE( KPPEQN_UNIT, '(/ "! User Defined rate functions " )' )
         DO ISPC = 1, NFUNCTIONS
            PHRASE = TRIM( FUNCTIONS( ISPC ) )
            CALL CONVERT_CASE ( PHRASE, .TRUE. )
            WRITE( KPPEQN_UNIT, 4612 )PHRASE
         END DO 
4612  FORMAT(9X,'REAL( Kind = dp ) :: ', A )
      END IF
! set up pointers and names for photolysis rate array
      WRITE(KPPEQN_UNIT,4502)
      DO IPR = 1, NPHOTAB
         WRITE(KPPEQN_UNIT,4503),PHOTAB(IPR),IPR
      END DO
      DO IPR = 1, NPHOTAB
         WRITE(KPPEQN_UNIT,4557)IPR, PHOTAB(IPR)
      END DO
      IF( NHETERO .GT. 0 )THEN
          WRITE(KPPEQN_UNIT,5023)NHETERO
          DO IPR = 1, NHETERO
             WRITE(KPPEQN_UNIT,5024)HETERO(IPR),IPR
          END DO
          DO IPR = 1, NHETERO
             WRITE(KPPEQN_UNIT,5025)IPR, HETERO(IPR)
          END DO
      ELSE 
          WRITE(KPPEQN_UNIT,5026)NHETERO
      END IF

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
             IF( REORDER_SPECIES )THEN
                ISPC = IOLD2NEW( INDEX_CTERM( NXX, IREACT ) )
             ELSE
                ISPC = INDEX_CTERM( NXX, IREACT )
             END IF
             IF( ISPC .LT. 1 )CYCLE
             PHRASE = ' * Y( ind_' // TRIM( SPARSE_SPECIES( ISPC ) ) // ' ) '
             WRITE(KPPEQN_UNIT, 4709, ADVANCE = 'NO')TRIM( PHRASE )
!             IF( ISPC .LT. 1 )THEN
!                WRITE(KPPEQN_UNIT, 4708, ADVANCE = 'NO')TRIM(PHRASE),
!     &          REAL( ABS( KC_COEFFS( NXX, IREACT ) ), 8), IRX
!             ELSE
!                WRITE(KPPEQN_UNIT, 4711, ADVANCE = 'NO')TRIM(PHRASE),
!     &          REAL( ABS( KC_COEFFS( NXX, IREACT ) ), 8), IRX, TRIM( SPARSE_SPECIES( ISPC ) )
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
! write function block from mech.def file
      IF( LINES_CAPTURED .GT. 0 )THEN
          INQUIRE( FILE = TRIM( FUNCTIONS_CAPTURED ), EXIST = EXISTING )
          IF( .NOT. EXISTING )THEN
             WRITE(6,*)'ERROR: CANNOT LOCATE FILE: ' // TRIM(FUNCTIONS_CAPTURED)
             STOP
          END IF
          OPEN( UNIT = UNIT_FUNCTIONS, FILE = TRIM( FUNCTIONS_CAPTURED ), STATUS = 'OLD' )
          WRITE( KPPEQN_UNIT,'("! Lines capture from MECH_DEF FUNCTIONS blocks." )')
          DO NC = 1, LINES_CAPTURED
            READ (UNIT_FUNCTIONS,'(A)')FILE_LINE
            WRITE( KPPEQN_UNIT,'(11X,A)')TRIM( FILE_LINE )
          END DO
          CLOSE( UNIT_FUNCTIONS )
      END IF

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
                  WRITE(KPPEQN_UNIT,'(A, A)', ADVANCE = 'NO')TRIM(SPARSE_SPECIES( ISPC )),' '
                  ICOUNT = 1 + LEN( SPARSE_SPECIES( ISPC ) )
               ELSE
                  WRITE(KPPEQN_UNIT,'(3A)', ADVANCE = 'NO')'+ ',TRIM(SPARSE_SPECIES( ISPC )),' '
                  ICOUNT = 1 + LEN( SPARSE_SPECIES( ISPC ) )
                  ICOUNT = 3 + LEN( SPARSE_SPECIES( ISPC ) )                  
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
     &           '- ',ABS(SC( NXX,IPRODUCT )),'*',TRIM(SPARSE_SPECIES( ISPC )),' '
                  ICOUNT = ICOUNT + 12 + LEN( SPARSE_SPECIES( ISPC ) )
               ELSE
                  IF( IPRODUCT .EQ. 1 )THEN
                     WRITE(KPPEQN_UNIT,'(F8.5, 3A)', ADVANCE = 'NO')
     &               SC( NXX,IPRODUCT ),'*',TRIM(SPARSE_SPECIES( ISPC )),' '
                     ICOUNT = ICOUNT + 10 + LEN( SPARSE_SPECIES( ISPC ) )
                  ELSE
                     WRITE(KPPEQN_UNIT,'(A,F8.5,3A)', ADVANCE = 'NO')
     &               '+ ',SC( NXX,IPRODUCT ),'*',TRIM(SPARSE_SPECIES( ISPC )),' '
                     ICOUNT = ICOUNT + 12 + LEN( SPARSE_SPECIES( ISPC ) )
                  END IF
               END IF
            ELSE IF ( SC( NXX,IPRODUCT ) .LT. 0.0 ) THEN
               WRITE(KPPEQN_UNIT,'(3A)', ADVANCE = 'NO')
     &               '- ',TRIM(SPARSE_SPECIES( ISPC )),' '
               ICOUNT = ICOUNT + 3 + LEN( SPARSE_SPECIES( ISPC ) )
            ELSE
               IF( IPRODUCT .EQ. 1 )THEN
                  WRITE(KPPEQN_UNIT,'(3A)', ADVANCE = 'NO')
     &           ' ',TRIM(SPARSE_SPECIES( ISPC )),' '
                  ICOUNT = ICOUNT + 2 + LEN( SPARSE_SPECIES( ISPC ) )
               ELSE
                  WRITE(KPPEQN_UNIT,'(3A)', ADVANCE = 'NO')
     &             '+ ',TRIM(SPARSE_SPECIES( ISPC )),' '
                  ICOUNT = ICOUNT + 3 + LEN( SPARSE_SPECIES( ISPC ) )
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
          CASE( -1 )
             DO IPR = 1, NHETERO
                IF ( IHETERO( IPR,1 ) .EQ. NXX )EXIT
             END DO
             IDX = IHETERO( IPR, 2 )
             IF( RTDAT(1, NXX) .NE. 1.0 )THEN
                 WRITE(KPPEQN_UNIT,5027, ADVANCE = 'NO')REAL(RTDAT(1, NXX),8),TRIM( HETERO(IDX) )
                 PRINT*,REAL(RTDAT(1, NXX),8),TRIM( HETERO(IDX) )
             ELSE
                 WRITE(KPPEQN_UNIT,5028, ADVANCE = 'NO')TRIM( HETERO(IDX) )
             END IF
          CASE(  0 )
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
             ELSE IF( IPH( NXX,3 ) .EQ. 0 )THEN
                IDX = IPH(IPH( NXX,2 ), 2)
                IF( RTDAT(1, NXX) .NE. 1.0 )THEN
                   WRITE(KPPEQN_UNIT,5100, ADVANCE = 'NO')REAL(RTDAT(1, NXX),8), IDX
                ELSE
                   WRITE(KPPEQN_UNIT,5101, ADVANCE = 'NO')IDX
                END IF
             END IF
          CASE( 1 )
             IF( KUNITS .EQ. 2 )CALL WRITE_RATE_CONVERT(KPPEQN_UNIT, IORDER(NXX))
             WRITE(KPPEQN_UNIT,'(1PD12.4)', ADVANCE = 'NO')REAL(RTDAT(1, NXX), 8)
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
	     IDIFF_ORDER = IORDER(NXX) - IORDER(IRX)
	     IF( IDIFF_ORDER .NE. 0 )THEN
	         FALLOFF_RATE = ( KTYPE(IRX) .GT. 7 .AND. KTYPE(IRX) .LT. 11 )
                 IF( KUNITS .EQ. 2 .OR. FALLOFF_RATE )THEN
	           CALL WRITE_RATE_CONVERT(KPPEQN_UNIT, IDIFF_ORDER )
		 END IF
	     END IF
             WRITE(KPPEQN_UNIT,5005, ADVANCE = 'NO')IRX,RTDAT( 1, NXX ), RTDAT(2, NXX )
          CASE( 6 )
!             DO IDX = 1, KTN6
!                IF( KRX6( IDX ) .EQ. NXX )EXIT
!             END DO         
             IRX = INT( RTDAT( 2, NXX) )
	     IDIFF_ORDER = IORDER(NXX) - IORDER(IRX)
	     IF( IDIFF_ORDER .NE. 0 )THEN
	         FALLOFF_RATE = ( KTYPE(IRX) .GT. 7 .AND. KTYPE(IRX) .LT. 11 )
                 IF( KUNITS .EQ. 2 .OR. FALLOFF_RATE )THEN
	           CALL WRITE_RATE_CONVERT(KPPEQN_UNIT, IDIFF_ORDER )
		 END IF
	     END IF
             IF( RTDAT( 1, NXX ) .NE. 1.0 )THEN
                 WRITE(KPPEQN_UNIT, 5006, ADVANCE = 'NO')REAL(RTDAT( 1, NXX ), 8), IRX
             ELSE
                 WRITE(KPPEQN_UNIT, 4706, ADVANCE = 'NO')' ', IRX
             END IF
          CASE( 7 )
             IF( RTDAT(1, NXX) .NE. 0.0 )THEN
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
             IF( RFDAT( 2, IDX ) .EQ. 0.0 .AND. RFDAT( 3, IDX ) .EQ. 0.0 )THEN
                 WRITE(KPPEQN_UNIT,5009, ADVANCE = 'NO')RTDAT(1,NXX),RTDAT(2,NXX),
     &           RTDAT(3,NXX),1.0*RFDAT(1,IDX)
             ELSE 
                 WRITE(KPPEQN_UNIT,5019, ADVANCE = 'NO')RTDAT(1,NXX),RFDAT(2, IDX),RTDAT(2,NXX),
     &           RTDAT(3,NXX),RFDAT(3, IDX),1.0*RFDAT(1,IDX),RFDAT(4, IDX),RFDAT(5, IDX)
              END IF 
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
             ELSE IF( RTDAT( 1, I) .NE. 1.0 .AND. ABS( RTDAT( 3, I ) ) .GT. 0.0 )THEN
                WRITE(KPPEQN_UNIT,5013, ADVANCE = 'NO')REAL(RTDAT( 1, I ), 8),REAL(RTDAT( 3, I ), 8),
     &          TRIM( SPECIAL( IRX) )
             ELSE IF( RTDAT( 1, I) .EQ. 1.0 .AND. ABS( RTDAT( 3, I ) ) .GT. 0.0 )THEN
                WRITE(KPPEQN_UNIT,5015, ADVANCE = 'NO')REAL(RTDAT( 3, I ), 8),
     &          TRIM( SPECIAL( IRX) )
             ELSE
                WRITE(KPPEQN_UNIT,5012, ADVANCE = 'NO')TRIM( SPECIAL( IRX ) )
             END IF
           CASE( 12 )
             DO IDX = 1, NFALLOFF
                IF( IRRFALL( IDX ) .EQ. NXX )EXIT
             END DO
             CALL WRITE_RATE_CONVERT(KPPEQN_UNIT, IORDER(NXX))
             WRITE(KPPEQN_UNIT,5020, ADVANCE = 'NO')RTDAT(1, NXX ),RFDAT(1, IDX),RTDAT(2, NXX ),
     &       RFDAT(2, IDX)
          CASE( 13 )
             DO IDX = 1, NRATE_STRING
                IF( KSTRING( IDX ) .EQ. NXX )EXIT
             END DO
             CALL WRITE_RATE_CONVERT_BEFORE(KPPEQN_UNIT, IORDER(NXX))
             WRITE(KPPEQN_UNIT,'(A)', ADVANCE = 'NO')TRIM( RATE_STRING( IDX ) )
          END SELECT
         WRITE(KPPEQN_UNIT,'(A)')' ;'
      END DO

      CLOSE( KPPEQN_UNIT )
      RETURN
      
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
     &      / 'REAL( kind=dp ) FUNCTION HALOGEN_FALLOFF(A1,B1,A2,B2)'
     &      / '   IMPLICIT NONE'
     &      / '   REAL( kind=dp ), INTENT( IN ) :: A1'
     &      / '   REAL( kind=dp ), INTENT( IN ) :: B1'
     &      / '   REAL( kind=dp ), INTENT( IN ) :: A2'
     &      / '   REAL( kind=dp ), INTENT( IN ) :: B2'
     &      / '   INTRINSIC DEXP'
     &      / '   IF( OPEN_WATER )THEN'
     &      / '       HALOGEN_FALLOFF = A1 * DEXP( B1 * PRESS ) + A2 * DEXP( B2 * PRESS )'
     &      / '   ELSE'
     &      / '       HALOGEN_FALLOFF = 0.0_dp'
     &      / '   END IF'
     &      / '   RETURN'
     &      / 'END FUNCTION HALOGEN_FALLOFF' 
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
     &        / 'LOGICAL             :: OPEN_WATER         ! Is land category ice free open water?'
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
5013   FORMAT(ES12.4,"*exp(",ES12.4,"*INV_TEMP)*",A,"")             
5015   FORMAT("exp(",ES12.4,"*INV_TEMP)*",A,"")             
5014   FORMAT('ARRD( ',1PD12.4,', 0.0000D+0,', 1PD12.4,' )  * PRESS ')             
5019   FORMAT('EP4D( ', 7(1PD12.4,', '), 1PD12.4,' )')
5020   FORMAT('HALOGEN_FALLOFF( ', 3(1PD12.4,', '), 1PD12.4,' )')
5027   FORMAT(1PD12.4,' * KHETERO( IK_',A,' )')
5028   FORMAT(  'KHETERO( IK_',A, ' )' )
5023   FORMAT(
     &        / 'INTEGER, PARAMETER  :: NHETERO  = ', I3,'  ! number of heterogeneous rates '
     &        / 'CHARACTER(16), SAVE :: HETERO(  NHETERO )  ! Names of  heterogeneous '
     &        / 'REAL(dp)            :: KHETERO( NHETERO )  ! grid cell heterogeneous rates ,[min-1]')
5024   FORMAT(  'INTEGER, PARAMETER  :: IK_',A16,' = ', I3 )
5025   FORMAT('DATA HETERO(', I3,' ) / ''',A16,''' /')
5026   FORMAT('INTEGER, PARAMETER  :: NHETERO  = ', I3,'  ! number of heterogeneous rates ')

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


       END SUBROUTINE WRT_KPP_INPUTS
          
       SUBROUTINE  CONVERT_CASE_BAK ( BUFFER, UPPER )
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
        END SUBROUTINE CONVERT_CASE_BAK

      SUBROUTINE WRITE_RATE_CONVERT_BAK(OUT_UNIT, RXN_ORDER)
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
      END SUBROUTINE WRITE_RATE_CONVERT_BAK
