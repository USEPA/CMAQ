
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

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE AVG_CONC

      IMPLICIT NONE

C Revision History
C   10 May 2016 D. Wong Modified code to provide flexibility to handle
C                       environment variable ACONC_FILE_SPCS is set to 'ALL' 
C                       or not set.
C 
C   09 Sep 19 F. Sidi: Replaced L_CONC_WVEL & L_ACONC_WVEL (depreciated) with W_VEL &
C                      added W_VEL to species aconc_species list depending on
C                      if the user requests the vertical velocity to be output
C
C
C Function: species, layer pointers and definitions for integral average
C CONC calculations
C
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      INTEGER, SAVE :: N_ASPCS = 0

      INTEGER, SAVE, ALLOCATABLE :: AVG_CONC_MAP( : ) ! pointer into CGRID
      CHARACTER( 16 ), SAVE, ALLOCATABLE :: A_GC_SPC( : ) ! pointer into GC_SPC
      CHARACTER( 16 ), SAVE, ALLOCATABLE :: A_AE_SPC( : ) ! pointer into AE_SPC
      CHARACTER( 16 ), SAVE, ALLOCATABLE :: A_NR_SPC( : ) ! pointer into NR_SPC
      CHARACTER( 16 ), SAVE, ALLOCATABLE :: A_TR_SPC( : ) ! pointer into TR_SPC

C species classes configuration for average CONC 
      INTEGER, SAVE :: A_GC_STRT
      INTEGER, SAVE :: N_A_GC_SPC
      INTEGER, SAVE :: A_AE_STRT
      INTEGER, SAVE :: N_A_AE_SPC
      INTEGER, SAVE :: A_NR_STRT
      INTEGER, SAVE :: N_A_NR_SPC
      INTEGER, SAVE :: A_TR_STRT
      INTEGER, SAVE :: N_A_TR_SPC

      INTEGER, SAVE :: N_A_AE_SPCD
                                
C Number of Average Concentration Layers                                
      INTEGER, SAVE :: A_NLYS = 1

C Average Arrays for Met variables
      REAL, SAVE, ALLOCATABLE :: AVG_WVEL( :,:,: )
      REAL, SAVE, ALLOCATABLE :: AVG_RH( :,:,: )
      REAL, SAVE, ALLOCATABLE :: AVG_TA( :,:,: )
      REAL, SAVE, ALLOCATABLE :: AVG_PRES( :,:,: )

C Logical to test whether met variables are included
      LOGICAL, SAVE :: L_ACONC_RH = .FALSE.
      LOGICAL, SAVE :: L_ACONC_TA = .FALSE.
      LOGICAL, SAVE :: L_ACONC_PRES = .FALSE.

      CONTAINS

         SUBROUTINE A_CONC_DEFN ()

         USE RUNTIME_VARS
         USE GRID_CONF             ! Grid Configuration
         USE CGRID_SPCS            ! CGRID mechanism species
         USE UTILIO_DEFN

#ifdef sens
         Use DDM3D_DEFN, Only: N_ASENS_VARS
#endif

         CHARACTER( 16 ) :: PNAME = 'A_CONC_DEFN'
         CHARACTER(200 ) :: XMSG = ' '

         INTEGER OFF, VAR, V, NV, LVL
         INTEGER ALLOCSTAT, STATUS
         INTEGER :: JDATE = 0
         INTEGER :: JTIME = 0

         INTEGER, ALLOCATABLE :: GC_MAP( : )
         INTEGER, ALLOCATABLE :: AE_MAP( : )
         INTEGER, ALLOCATABLE :: NR_MAP( : )
         INTEGER, ALLOCATABLE :: TR_MAP( : )


C-----------------------------------------------------------------------

C RETRIEVE THE SPECIES SAVED TO INTEGRAL AVERAGE CONCENTRATION FILE

         IF ( N_ACONC_VARS .EQ. 0 .OR. ACONC_FILE_SPCS( 1 ) .EQ. 'ALL' ) THEN
            V = 0
            DO VAR = 1, N_GC_CONC
               V = V + 1
               ACONC_FILE_SPCS( V ) = GC_CONC( VAR )
            END DO
            DO VAR = 1, N_AE_CONC
               V = V + 1
               ACONC_FILE_SPCS( V ) = AE_CONC( VAR )
            END DO
            DO VAR = 1, N_NR_CONC
               V = V + 1
               ACONC_FILE_SPCS( V ) = NR_CONC( VAR )
            END DO
            DO VAR = 1, N_TR_SPC
               V = V + 1
               ACONC_FILE_SPCS( V ) = TR_SPC( VAR )
            END DO
            ACONC_FILE_SPCS( V + 1 ) = 'RH'
            ACONC_FILE_SPCS( V + 2 ) = 'TA'
            ACONC_FILE_SPCS( V + 3 ) = 'PRES'
            N_ACONC_VARS = V + 3
#ifdef sens
            N_ASENS_VARS = V
#endif
         END IF
C  Retrieve CTM_WVEL environmental variable to see if user wants diagnosed vertical velocity
         IF ( W_VEL ) THEN 
            ACONC_FILE_SPCS( N_ACONC_VARS + 1 ) = 'WVEL'
            N_ACONC_VARS = N_ACONC_VARS + 1
         END IF
C Retrieve the layer range used in integral average concentration file
         IF ( ACONC_ELEV .EQ. -1 ) ACONC_ELEV = NLAYS

         IF ( ACONC_BLEV .LE. 0 .OR. ACONC_ELEV .GT. NLAYS ) THEN
            WRITE( XMSG,'( "Layer range", 2I4, " invalid for this model" )' )
     &      ACONC_BLEV, ACONC_ELEV
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         END IF

         A_NLYS = ACONC_ELEV - ACONC_BLEV + 1

C Check species names against include files and create AVG_CONC_MAP, and
C get starting index in AGRID and total count for each species class

         ALLOCATE ( A_GC_SPC    ( N_ACONC_VARS ),
     &              A_AE_SPC    ( N_ACONC_VARS ),
     &              A_NR_SPC    ( N_ACONC_VARS ),
     &              A_TR_SPC    ( N_ACONC_VARS ),
     &              GC_MAP      ( N_ACONC_VARS ),
     &              AE_MAP      ( N_ACONC_VARS ),
     &              NR_MAP      ( N_ACONC_VARS ),
     &              TR_MAP      ( N_ACONC_VARS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'AVG_CONC_MAP or A_GC_SPC or A_AE_SPC or A_NR_SPC'
     &           // ' or A_TR_SPC or GC_MAP or AE_MAP or NR_MAP or TR_MAP'
     &           // ' memory allocation failed'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         END IF

C The selected species MUST be contiguous with the classes,
C and the classes must be in the sequence: GC, AE, NR, TR

         N_A_GC_SPC = 0
         N_A_AE_SPC = 0
         N_A_NR_SPC = 0
         N_A_TR_SPC = 0

         DO VAR = 1, N_ACONC_VARS
            V = INDEX1 ( ACONC_FILE_SPCS( VAR ), N_GC_SPC, GC_SPC )
            IF ( V .GT. 0 ) THEN
               N_ASPCS = N_ASPCS + 1
               N_A_GC_SPC = N_A_GC_SPC + 1
               A_GC_SPC( N_A_GC_SPC ) = ACONC_FILE_SPCS( VAR )
               OFF = 0
               GC_MAP( N_A_GC_SPC ) = V + OFF
            ELSE
               V = INDEX1 ( ACONC_FILE_SPCS( VAR ), N_AE_SPC, AE_SPC )
               IF ( V .GT. 0 ) THEN
                  N_ASPCS = N_ASPCS + 1
                  N_A_AE_SPC = N_A_AE_SPC + 1
                  OFF = N_GC_SPC + 1   ! accounts for advected density
                  A_AE_SPC( N_A_AE_SPC ) = ACONC_FILE_SPCS( VAR )
                  AE_MAP( N_A_AE_SPC ) = V + OFF
               ELSE
                  V = INDEX1 ( ACONC_FILE_SPCS( VAR ), N_NR_SPC, NR_SPC )
                  IF ( V .GT. 0 ) THEN
                     N_ASPCS = N_ASPCS + 1
                     N_A_NR_SPC = N_A_NR_SPC + 1
                     OFF = N_GC_SPC + 1 + N_AE_SPC
                     A_NR_SPC( N_A_NR_SPC ) = ACONC_FILE_SPCS( VAR )
                     NR_MAP( N_A_NR_SPC ) = V + OFF
                  ELSE
                     V = INDEX1 ( ACONC_FILE_SPCS( VAR ), N_TR_SPC, TR_SPC )
                     IF ( V .GT. 0 ) THEN
                        N_ASPCS = N_ASPCS + 1
                        N_A_TR_SPC = N_A_TR_SPC + 1
                        OFF = N_GC_SPC + 1 + N_AE_SPC + N_NR_SPC
                        A_TR_SPC( N_A_TR_SPC ) = ACONC_FILE_SPCS( VAR )
                        TR_MAP( N_A_TR_SPC ) = V + OFF
                     ELSE IF ( ACONC_FILE_SPCS( VAR ) .EQ. 'WVEL' ) THEN
                        ALLOCATE( AVG_WVEL( NCOLS,NROWS,A_NLYS ) )
                     ELSE IF ( ACONC_FILE_SPCS( VAR ) .EQ. 'RH' ) THEN
                        L_ACONC_RH = .TRUE.
                        ALLOCATE( AVG_RH( NCOLS,NROWS,A_NLYS ) )
                     ELSE IF ( ACONC_FILE_SPCS( VAR ) .EQ. 'TA' ) THEN
                        L_ACONC_TA = .TRUE.
                        ALLOCATE( AVG_TA( NCOLS,NROWS,A_NLYS ) )
                     ELSE IF ( ACONC_FILE_SPCS( VAR ) .EQ. 'PRES' ) THEN
                        L_ACONC_PRES = .TRUE.
                        ALLOCATE( AVG_PRES( NCOLS,NROWS,A_NLYS ) )
                     ELSE
                        XMSG = 'Variable ' // ACONC_FILE_SPCS( VAR )
     &                  // ' was requested for average concentration '
     &                  // ' output, but it is not available from any '
     &                  // ' species namelist.'
                        CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
                     END IF
                  END IF
               END IF
            END IF
         END DO

         A_GC_STRT = 1
         A_AE_STRT = A_GC_STRT + N_A_GC_SPC
         A_NR_STRT = A_AE_STRT + N_A_AE_SPC
         A_TR_STRT = A_NR_STRT + N_A_NR_SPC

         IF ( N_A_AE_SPC .GT. 0 ) THEN
            N_A_AE_SPCD = N_A_AE_SPC
         ELSE
            N_A_AE_SPCD = 1
         END IF

         ! Map the CGRID indices to the AGRID indices 
         !    i.e.: AVG_CONC_MAP( AGRID_IND ) = CGRID_IND
         ALLOCATE( AVG_CONC_MAP( N_ASPCS ) )
         VAR = 0
         DO V = 1, N_A_GC_SPC
            VAR = VAR + 1
            AVG_CONC_MAP( VAR ) = GC_MAP( V )
         END DO

         DO V = 1, N_A_AE_SPC
            VAR = VAR + 1
            AVG_CONC_MAP( VAR ) = AE_MAP( V )
         END DO

         DO V = 1, N_A_NR_SPC
            VAR = VAR + 1
            AVG_CONC_MAP( VAR ) = NR_MAP( V )
         END DO

         DO V = 1, N_A_TR_SPC
            VAR = VAR + 1
            AVG_CONC_MAP( VAR ) = TR_MAP( V )
         END DO

         ! Deallocate arrays if they are unused
         DEALLOCATE( GC_MAP, AE_MAP, NR_MAP, TR_MAP )
         IF ( N_A_GC_SPC .EQ. 0 ) DEALLOCATE ( A_GC_SPC )
         IF ( N_A_AE_SPC .EQ. 0 ) DEALLOCATE ( A_AE_SPC )
         IF ( N_A_NR_SPC .EQ. 0 ) DEALLOCATE ( A_NR_SPC )
         IF ( N_A_TR_SPC .EQ. 0 ) DEALLOCATE ( A_TR_SPC )

         END SUBROUTINE A_CONC_DEFN

      END MODULE AVG_CONC
