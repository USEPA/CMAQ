
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
      USE MECHANISM_DATA, MECHANISM => MECHNAME
      USE CGRID_SPCS     ! CGRID mechanism species    
      
      IMPLICIT NONE

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


!      INTEGER NPRDCT( MAXRXNUM )               ! no. of products for rx j
!      INTEGER NREACT( MAXRXNUM )               ! no. of reactants for rx j
!      INTEGER IRR( MAXRXNUM,MAXPRODS+3 )
!      REAL    SC ( MAXRXNUM,MAXPRODS )


      INTEGER         :: NUSING_SPECIAL, IUSING_SPECIAL( MAXSPEC )
      CHARACTER( 16 ) :: USING_SPECIAL( MAXSPEC )


c..local Variables for steady-state species

      INTEGER         :: DUMMY_COEF( MAXRXNUM )               ! Yields for the DUMMY variable in each reaction
      INTEGER         :: SS1RX( MAXNLIST )                    ! First reaction occurrence for each SS species
      
c..Variables for species to be dropped from mechanism
      INTEGER         :: N_DROP_SPC = 0
      CHARACTER( 16 ) :: DROP_SPC( MAXNLIST )
      LOGICAL         :: LERROR
      LOGICAL         :: KPP_DUMMY   = .FALSE.
      LOGICAL         :: FIRST_TERM  = .TRUE.
      REAL( 8 )       :: WREXT_COEFFS( MAXSPECTERMS)
      INTEGER         :: WREXT_INDEX(  MAXSPECTERMS)

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
     &                    'ATM_AIR         ',
     &                    'ATM_O2          ',
     &                    'ATM_N2          ',
     &                    'ATM_H2          ',
     &                    'ATM_CH4         ' /)
      CHARACTER(  16 ) :: CLABEL                  ! mechanism constants label
      REAL( 8 )        :: CONSTVAL                ! retrieved constant
      REAL( 8 )        :: CVAL( MAXCONSTS )
      INTEGER, PARAMETER :: LUNOUT = 6

!         INTEGER IRR( MAXRXNUM,MAXPRODS+3 )
!         REAL    SC ( MAXRXNUM,MAXPRODS )


      CHARACTER(  12 ) :: EXFLNM_SPCS = 'SPCSDATX'
      CHARACTER(  12 ) :: EXFLNM_RXDT = 'RXNSDATX'
      CHARACTER(  12 ) :: EXFLNM_RXCM = 'RXNSCOMX'
      CHARACTER(  12 ) :: RXNS_MODULE = 'RXNS_MODULE'
      
      CHARACTER(  16 ) :: RXNS_DATA_MODULE = 'RXNS_DATA_MODULE'
      CHARACTER(  16 ) :: RXNS_FUNC_MODULE = 'RXNS_FUNC_MODULE'
      CHARACTER(  16 ) :: OUT_DIR          = 'OUTDIR'

      CHARACTER(  5 )    :: CGRID_DATA
      CHARACTER( 32 )    :: CGRID_NMLS = 'USE_SPCS_NAMELISTS'

      INTEGER, EXTERNAL :: JUNIT
      INTEGER            :: ICOUNT, IREACT, IPRODUCT
      EXTERNAL NAMEVAL

      INTERFACE 
       SUBROUTINE GETRCTNT ( IMECH, INBUF, IEOL, LPOINT, CHR, WORD,
     &                      NXX, NS, SPCLIS, SPC1RX,
     &                      ICOL, LABEL, N_DROP_SPC, DROP_SPC )
         USE KPP_DATA
         USE MECHANISM_DATA
         IMPLICIT NONE
         INTEGER,         INTENT(   IN  ) :: IMECH
         CHARACTER( 81 ), INTENT( INOUT ) :: INBUF
         INTEGER,         INTENT( INOUT ) :: LPOINT
         INTEGER,         INTENT( INOUT ) :: IEOL
         CHARACTER(  1 ), INTENT( INOUT ) :: CHR
         CHARACTER( 16 ), INTENT( INOUT ) :: WORD
         INTEGER,         INTENT(   IN  ) :: NXX
         INTEGER,         INTENT( INOUT ) :: NS
         CHARACTER( 16 ), INTENT( INOUT ) :: SPCLIS( MAXSPEC )
         INTEGER,         INTENT( INOUT ) :: SPC1RX( MAXSPEC )
         INTEGER,         INTENT( INOUT ) :: ICOL
         CHARACTER( 16 ), INTENT(   IN  ) :: LABEL( MAXRXNUM, 2 )
         INTEGER,         INTENT(   IN  ) :: N_DROP_SPC
         CHARACTER( 16 ), INTENT(   IN  ) :: DROP_SPC( MAXNLIST )
        END SUBROUTINE GETRCTNT
        SUBROUTINE GETPRDCT ( IMECH, INBUF, LPOINT, IEOL, CHR, WORD,
     &                      NXX, NS, SPCLIS, SPC1RX,
     &                      ICOL, N_DROP_SPC, DROP_SPC )
          USE MECHANISM_DATA
          IMPLICIT NONE
          INTEGER,         INTENT(   IN  ) :: IMECH
          CHARACTER( 81 ), INTENT( INOUT ) :: INBUF
          INTEGER,         INTENT( INOUT ) :: LPOINT
          INTEGER,         INTENT( INOUT ) :: IEOL
          CHARACTER(  1 ), INTENT( INOUT ) :: CHR
          CHARACTER( 16 ), INTENT( INOUT ) :: WORD
          INTEGER,         INTENT(   IN  ) :: NXX
          INTEGER,         INTENT( INOUT ) :: NS
          CHARACTER( 16 ), INTENT( INOUT ) :: SPCLIS( MAXSPEC )
          INTEGER,         INTENT( INOUT ) :: SPC1RX( MAXSPEC )
          INTEGER,         INTENT( INOUT ) :: ICOL
          INTEGER,         INTENT(   IN  ) :: N_DROP_SPC
          CHARACTER( 16 ), INTENT(   IN  ) :: DROP_SPC( MAXNLIST )
         END SUBROUTINE GETPRDCT
         SUBROUTINE GETRATE ( IMECH, INBUF, LPOINT, IEOL, CHR,
     &                         NXX, LABEL, IP )
           USE MECHANISM_DATA
           IMPLICIT NONE
           CHARACTER(  1 ), INTENT( INOUT ) :: CHR
           CHARACTER( 81 ), INTENT( INOUT ) :: INBUF
           INTEGER,         INTENT( IN )    :: IMECH
           INTEGER,         INTENT( INOUT ) :: LPOINT
           INTEGER,         INTENT( INOUT ) :: IEOL
           INTEGER,         INTENT( INOUT ) :: IP
           INTEGER,         INTENT( IN )    :: NXX
           CHARACTER( 16 ), INTENT( INOUT ) :: LABEL( MAXRXNUM,2 )
        END SUBROUTINE GETRATE
        SUBROUTINE WREXTS (EQNAME_MECH, DESCRP_MECH, NS, SPCLIS, SPC1RX, NR,
     &                      IP,  NAMCONSTS, CVAL, SS1RX  ) 
          USE MECHANISM_DATA
          IMPLICIT NONE
          CHARACTER( 120 ), INTENT ( IN ) :: EQNAME_MECH
          CHARACTER(  32 ), INTENT ( IN ) :: DESCRP_MECH
          INTEGER,          INTENT ( IN ) :: NS                ! no. of species found in mechanism table
          CHARACTER(  16 ), INTENT ( IN ) :: SPCLIS( MAXSPEC ) ! species list from mechanism table
          INTEGER,          INTENT ( IN ) :: NR
          INTEGER,          INTENT ( IN ) :: SPC1RX( MAXSPEC ) ! rx index of 1st occurence of species in mechanism table
          INTEGER,          INTENT ( IN ) :: IP
          CHARACTER( 16 ),  INTENT ( IN ) :: NAMCONSTS( MAXCONSTS )
          REAL( 8 ),        INTENT ( IN ) :: CVAL( MAXCONSTS )
          INTEGER,          INTENT ( IN ) :: SS1RX( MAXNLIST )
        END SUBROUTINE WREXTS
        SUBROUTINE GET_SS_DATA ( LUNOUT, NR ) 
          USE MECHANISM_DATA
          IMPLICIT NONE
          INTEGER, INTENT ( IN )         :: LUNOUT   ! Output unit number
          INTEGER, INTENT ( IN )         :: NR       ! No. of reactions
        END SUBROUTINE GET_SS_DATA
        SUBROUTINE CHECK_SS_SPC ( LUNOUT, NS, SPCLIS, NR, LABEL, SS1RX )
         USE MECHANISM_DATA
         IMPLICIT NONE
         INTEGER, INTENT ( IN )         :: LUNOUT               ! Output unit number
         INTEGER, INTENT ( IN )         ::  NS                  ! No. of species in mechanism
         CHARACTER( 16 ), INTENT ( IN ) ::  SPCLIS( MAXSPEC )   ! List of mechanism species
         INTEGER, INTENT ( IN )         ::  NR                  ! No. of reactions
         CHARACTER( 16 ), INTENT ( IN ) ::  LABEL( MAXRXNUM,2 ) ! Reaction labels
         INTEGER, INTENT ( INOUT )      ::  SS1RX( MAXNLIST )
       END SUBROUTINE CHECK_SS_SPC
       SUBROUTINE WRSS_EXT( NR ) 
         USE MECHANISM_DATA
         IMPLICIT NONE
         INTEGER, INTENT ( IN )         :: NR   ! No. of reactions
       END SUBROUTINE WRSS_EXT
       SUBROUTINE WRT_KPP_INPUTS( NR, IP, LABEL, NS, SPCLIS  )
         USE KPP_DATA
         USE MECHANISM_DATA
         IMPLICIT NONE
         INTEGER,         INTENT( IN ) :: NR ! number of reactions
         INTEGER,         INTENT( IN ) :: IP ! number of photolysis reaction
         CHARACTER( 16 ), INTENT( IN ) :: LABEL( MAXRXNUM,2 ) ! LABEL(NXX,1) 1st label found in rx NXX
         INTEGER,         INTENT( IN ) :: NS ! number of species
         CHARACTER( 16 ), INTENT( IN ) :: SPCLIS( MAXSPEC )
       END SUBROUTINE WRT_KPP_INPUTS
      END INTERFACE 
  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Initialize module and local mechanism array variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      CALL INIT_MECH_DATA

      LABEL( 1:MAXRXNUM, 1) = '<<<<<<<<<<<<<<<<'
      LABEL( 1:MAXRXNUM, 2) = '>>>>>>>>>>>>>>>>'
     
      SPCLIS = ' '
      SPC1RX = 0

      NS = 0
      NXX = 0
      MXPRD = 0
      IP = 0
      NUSING_SPECIAL = 0
      IUSING_SPECIAL = 0
      USING_SPECIAL  = ' '
      
      
      ALLOCATE( INDEX_FIXED_SPECIES( MAXRXNUM, MAXRCTNTS ) )
      INDEX_FIXED_SPECIES = 0
      NRXN_FIXED_SPECIES  = 0

! determine whether to write out CMAQ CGRID species name and indices to output

         CALL NAMEVAL ( CGRID_NMLS, CGRID_DATA )

         CALL CONVERT_CASE( CGRID_DATA, .TRUE.)

         IF( CGRID_DATA(1:1) .EQ. 'T' .OR. CGRID_DATA(1:1) .EQ. 'Y' )THEN
             USE_SPCS_NAMELISTS = .TRUE.
             WRITE(6,'(A)')'Environment Variable WRITE_CGRID_DATA set to '
     &       // TRIM( CGRID_DATA ) // ' and adding CMAQ CGRID data to output '
         ELSE IF(  CGRID_DATA(1:1) .EQ. 'F' .OR. CGRID_DATA(1:1) .EQ. 'N' )THEN
             USE_SPCS_NAMELISTS = .FALSE.
             WRITE(6,'(A)')'Environment Variable WRITE_CGRID_DATA set to '
     &      // TRIM( CGRID_DATA ) // ' and not writing CMAQ CGRID data to output '
         ELSE
             WRITE(6,' (A)')'Environment Variable WRITE_CGRID_DATA set to '
     &       // TRIM( CGRID_DATA ) // ' and must equal T, Y, F, or N.'
     &       // ' Using default value of F'
             USE_SPCS_NAMELISTS = .FALSE.
         END IF

      print*,'CHEMMECH: USE_SPCS_NAMELISTS = ',USE_SPCS_NAMELISTS

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Open mechanism input file and get the first non-comment line
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMECH = JUNIT()
      PRINT*,'IMECH = ',IMECH 
      CALL NAMEVAL ( MECHNAME, EQNAME_MECH )
      OPEN ( UNIT = IMECH, FILE = EQNAME_MECH, STATUS = 'UNKNOWN' )
!Open output file for conversion to KPP Equations Format
      PRINT*,'KPPEQN_UNIT = ',KPPEQN_UNIT
      CALL NAMEVAL ( EQNS_KPP_FILE, EQN_MECH_KPP )
      CALL NAMEVAL ( SPCS_KPP_FILE, SPC_MECH_KPP )
      OPEN ( UNIT = KPPEQN_UNIT, FILE = EQN_MECH_KPP, STATUS = 'UNKNOWN' )
      EXUNIT_SPCS = JUNIT()
      EXUNIT_RXDT = JUNIT()
      EXUNIT_RXCM = JUNIT()
c symbolic link locates "EXFLNM_..."; setenv requires INQUIRE (NAMEVAL):
      CALL NAMEVAL ( EXFLNM_SPCS, EQNAME_SPCS )
      CALL NAMEVAL ( EXFLNM_RXDT, EQNAME_RXDT )
      CALL NAMEVAL ( EXFLNM_RXCM, EQNAME_RXCM )
      CALL NAMEVAL ( RXNS_MODULE, FNAME_MODULE )

      CALL NAMEVAL ( RXNS_DATA_MODULE, FNAME_DATA_MODULE )
      CALL NAMEVAL ( RXNS_FUNC_MODULE, FNAME_FUNC_MODULE )
      CALL NAMEVAL ( OUT_DIR, OUTDIR )

      OPEN ( UNIT = EXUNIT_SPCS, FILE = EQNAME_SPCS, STATUS = 'UNKNOWN' )
      OPEN ( UNIT = EXUNIT_RXDT, FILE = EQNAME_RXDT, STATUS = 'UNKNOWN' )
      OPEN ( UNIT = EXUNIT_RXCM, FILE = EQNAME_RXCM, STATUS = 'UNKNOWN' )

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
199                 CALL GET_OPERATOR ( IMECH, INBUF, LPOINT, IEOL, CHR, WORD )

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
         RXLABEL( NXX ) = LABEL( NXX,1 )
      END IF
      ICOL = 0
      IORDER( NXX ) = 0
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Get the reactants
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO 301 ISPC = 1, 3
         CALL GETRCTNT ( IMECH, INBUF, IEOL, LPOINT, CHR, WORD,
     &                   NXX, NS, SPCLIS, SPC1RX,
     &                   ICOL, 
     &                   LABEL, N_DROP_SPC, DROP_SPC ) ! ,
!     &                   N_SS_SPC, SS_SPC, SS_RCT_COEF ) ! , IRR, SC )
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
     &                   ICOL,
     &                   N_DROP_SPC, DROP_SPC ) ! ,
!     &                   N_SS_SPC, SS_SPC, SS_PRD_COEF ) ! , IRR, SC )
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
     &                  NXX, LABEL, IP )

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
                     WRITE( LUNOUT, 2019 ) IRX, LABEL(NXX,1)
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
      NMPHOT  = IP
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
             ENDIF 

          ENDDO
       ENDDO

       IF( N_SS_SPC .GT. 0 ) THEN
           CALL CHECK_SS_SPC( LUNOUT, NS, SPCLIS, NR, LABEL, SS1RX )
       END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Create the Fortran INCLUDE files for the reactions data and the
C interim species include file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      RXLABEL( 1:MAXRXNUM ) = LABEL( 1:MAXRXNUM,1 )
      
      NUMB_MECH_SPCS = NS + N_SS_SPC
      
       ALLOCATE( MECHANISM_INDEX( NUMB_MECH_SPCS ), MECHANISM_SPC( NUMB_MECH_SPCS ),
     &          CGRID_INDEX( NUMB_MECH_SPCS ), SPECIES_TYPE( NUMB_MECH_SPCS )  )

       
       DO I = 1, NUMB_MECH_SPCS
         MECHANISM_INDEX( I ) = I
         MECHANISM_SPC( I )   = SPCLIS( I )
       END DO

C Set CGRID mechanism

       IF( USE_SPCS_NAMELISTS )THEN
           IF ( .NOT. CGRID_SPCS_INIT() ) THEN
               STOP 'Error in CGRID_SPCS:CGRID_SPCS_INIT'
           ELSE
               PRINT*,'Computed CGRID species and indices'
           END IF
       ELSE
           SPECIES_TYPE = 'GC'
           CGRID_INDEX(1:NUMB_MECH_SPCS)  =  MECHANISM_INDEX(1:NUMB_MECH_SPCS)
       END IF

        N_GAS_CHEM_SPC = 0 
        DO ISPC = 1, NUMB_MECH_SPCS
           IF( SPECIES_TYPE( ISPC ) .NE. 'GC' )CYCLE
           N_GAS_CHEM_SPC =  N_GAS_CHEM_SPC + 1
       END DO

      NRXNS = NR
      
      CALL WREXTS ( EQNAME_MECH,
     &              DESCRP_MECH,
     &              NS, SPCLIS, SPC1RX,
     &              NR,
     &              IP,
     &              NAMCONSTS,
     &              CVAL, SS1RX ) 
     
      

      CALL WRSPECIAL_EXT( )

      IF( N_SS_SPC .GT. 0 ) CALL GET_SS_DATA( LUNOUT, NR ) 


      CALL WRSS_EXT( NR ) 

      CLOSE( KPPEQN_UNIT )

      MECHANISM = DESCRP_MECH
      CONST( 1:MAXCONSTS ) = CVAL( 1:MAXCONSTS )
      
      
!     CALL WRT_CALCKS( )

      print*,'calling WRT_RATE_CONSTANT '
      
      EQUATIONS_MECHFILE = EQNAME_MECH
      
      CALL WRT_RATE_CONSTANT( NR, IP, LABEL, NS, SPCLIS  )

!     print*,'calling WRT_FEVAL ' 

!     CALL WRT_FEVAL(  )
      
!     print*,'called WRT_FEVAL ' 
      
!     CALL WRT_JACOB_BLK( )

!      CALL WRT_JACOB( 1 )         
!      CALL WRT_JACOB( 2 )         
      
      CLOSE( EXUNIT_SPCS )
      CLOSE( EXUNIT_RXDT )
      CLOSE( EXUNIT_RXCM )

      CALL WRT_KPP_INPUTS( NR, IP, LABEL, NS, SPCLIS  )
      CLOSE( KPPEQN_UNIT )


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


       WRITE( LUNOUT, * ) '   Normal Completion of CHEMMECH'

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
