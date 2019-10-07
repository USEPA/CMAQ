
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
      MODULE STD_CONC

      IMPLICIT NONE

C Function: species, layer pointers and definitions for standard CONC
C calculations
C Revision: J.Young 13Sep2011: Increase size of CONC_FILE_SPCS dimension -
C           previous revision used dynamic allocation, but GET_ENV_LIST
C           was called with the CONC_FILE_SPCS argument before it was allocated
C           D.Wong 11 May 2016: Modified code to provide flexibility to handle
C                environment variable CONC_FILE_SPCS is set to 'ALL' or not set.
C           F.Sidi 09 Sep 2019: Replaced L_CONC_WVEL & L_ACONC_WVEL (depreciated) with W_VEL &
C           added W_VEL to species conc_species list depending on
C           if the user requests the vertical velocity to be output
C
C
C
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      INTEGER, SAVE :: N_CSPCS = 0 ! Number of chemical species saved to conc file
      INTEGER, SAVE :: C_NLAYS = 1 ! Number of layers saved to conc file

      INTEGER, SAVE, ALLOCATABLE :: CONC_MAP( : ) ! pointer into CGRID
      CHARACTER( 16 ), SAVE, ALLOCATABLE :: C_GC_SPC( : ) ! pointer into GC_SPC
      CHARACTER( 16 ), SAVE, ALLOCATABLE :: C_AE_SPC( : ) ! pointer into AE_SPC
      CHARACTER( 16 ), SAVE, ALLOCATABLE :: C_NR_SPC( : ) ! pointer into NR_SPC
      CHARACTER( 16 ), SAVE, ALLOCATABLE :: C_TR_SPC( : ) ! pointer into TR_SPC

      REAL, SAVE, ALLOCATABLE :: SGRID( :,:,:,: ) ! conc subset

C species classes configuration for CONC 
      INTEGER, SAVE :: C_GC_STRT
      INTEGER, SAVE :: N_C_GC_SPC
      INTEGER, SAVE :: C_AE_STRT
      INTEGER, SAVE :: N_C_AE_SPC
      INTEGER, SAVE :: C_NR_STRT
      INTEGER, SAVE :: N_C_NR_SPC
      INTEGER, SAVE :: C_TR_STRT
      INTEGER, SAVE :: N_C_TR_SPC

      INTEGER, SAVE :: N_C_AE_SPCD

C Logical to test whether met variables are included
      LOGICAL, SAVE :: L_CONC_RH = .FALSE.
      LOGICAL, SAVE :: L_CONC_TA = .FALSE.
      LOGICAL, SAVE :: L_CONC_PRES = .FALSE.

      CONTAINS

         SUBROUTINE CONC_DEFN ()

         USE RUNTIME_VARS
         USE HGRD_DEFN             ! horizontal domain specifications
         USE VGRD_DEFN             ! vertical layer specifications
         USE CGRID_SPCS            ! CGRID mechanism species
         USE UTILIO_DEFN

         CHARACTER( 16 ) :: PNAME = 'CONC_DEFN'
         CHARACTER( 96 ) :: XMSG = ' '

         INTEGER OFF, VAR, V, NV, LVL
         INTEGER ALLOCSTAT, STATUS
         INTEGER :: JDATE = 0
         INTEGER :: JTIME = 0

         INTEGER, ALLOCATABLE :: GC_MAP( : )
         INTEGER, ALLOCATABLE :: AE_MAP( : )
         INTEGER, ALLOCATABLE :: NR_MAP( : )
         INTEGER, ALLOCATABLE :: TR_MAP( : )

C-----------------------------------------------------------------------

C Retrieve the species saved to the concentration file

         IF ( N_CONC_VARS .EQ. 0 .OR. CONC_FILE_SPCS( 1 ) .EQ. 'ALL' ) THEN
            V = 0
            DO VAR = 1, N_GC_CONC
               V = V + 1
               CONC_FILE_SPCS( V ) = GC_CONC( VAR )
            END DO
            DO VAR = 1, N_AE_CONC
               V = V + 1
               CONC_FILE_SPCS( V ) = AE_CONC( VAR )
            END DO
            DO VAR = 1, N_NR_CONC
               V = V + 1
               CONC_FILE_SPCS( V ) = NR_CONC( VAR )
            END DO
            DO VAR = 1, N_TR_SPC
               V = V + 1
               CONC_FILE_SPCS( V ) = TR_SPC( VAR )
            END DO
            CONC_FILE_SPCS( V + 1 ) = 'RH'
            CONC_FILE_SPCS( V + 2 ) = 'TA'
            CONC_FILE_SPCS( V + 3 ) = 'PRES'
            N_CONC_VARS = V + 3 
         END IF

C  Retrieve CTM_WVEL environmental variable to see if user wants diagnosed vertical velocity
         IF ( W_VEL ) THEN
            CONC_FILE_SPCS( N_CONC_VARS + 1 ) = 'WVEL'
            N_CONC_VARS = N_CONC_VARS + 1
         END IF

C Retrieve the layer range used in the concentration file
        IF ( CONC_ELEV .EQ. -1 ) CONC_ELEV = NLAYS

        IF ( CONC_BLEV .LE. 0 .OR. CONC_ELEV .GT. NLAYS ) THEN
               WRITE( XMSG,'( "Layer range", 2I4, " invalid for this model" )' )
     &         CONC_BLEV, CONC_ELEV
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         END IF
         IF ( CONC_BLEV .NE. 1 ) THEN
               WRITE( XMSG,'( "Layer", I3, " Not 1st layer in CGRID" )' )
     &         CONC_BLEV
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
         END IF

         C_NLAYS = CONC_ELEV - CONC_BLEV + 1

C Create conc subset array

         ALLOCATE ( SGRID( NCOLS,NROWS,C_NLAYS,N_CONC_VARS ),
     &              STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'SGRID memory allocation failed'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         SGRID = 0.0

C Check species names against include files and create CONC_MAP, and
C get starting index in CGRID and total count for each species class

         ALLOCATE ( C_GC_SPC( N_CONC_VARS ),
     &              C_AE_SPC( N_CONC_VARS ),
     &              C_NR_SPC( N_CONC_VARS ),
     &              C_TR_SPC( N_CONC_VARS ),
     &              GC_MAP( N_CONC_VARS ),
     &              AE_MAP( N_CONC_VARS ),
     &              NR_MAP( N_CONC_VARS ),
     &              TR_MAP( N_CONC_VARS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'CONC species mapping  memory allocation failed'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         END IF

         N_C_GC_SPC = 0
         N_C_AE_SPC = 0
         N_C_NR_SPC = 0
         N_C_TR_SPC = 0

         DO VAR = 1, N_CONC_VARS
            V = INDEX1 ( CONC_FILE_SPCS( VAR ), N_GC_SPC, GC_SPC )
            IF ( V .GT. 0 ) THEN
               N_CSPCS = N_CSPCS + 1
               N_C_GC_SPC = N_C_GC_SPC + 1
               C_GC_SPC( N_C_GC_SPC ) = CONC_FILE_SPCS( VAR )
               OFF = 0
               GC_MAP( N_C_GC_SPC ) = V + OFF
            ELSE
               V = INDEX1 ( CONC_FILE_SPCS( VAR ), N_AE_SPC, AE_SPC )
               IF ( V .GT. 0 ) THEN
                  N_CSPCS = N_CSPCS + 1
                  N_C_AE_SPC = N_C_AE_SPC + 1
                  OFF = N_GC_SPC + 1   ! accounts for advected density
                  C_AE_SPC( N_C_AE_SPC ) = CONC_FILE_SPCS( VAR )
                  AE_MAP( N_C_AE_SPC ) = V + OFF
               ELSE
                  V = INDEX1 ( CONC_FILE_SPCS( VAR ), N_NR_SPC, NR_SPC )
                  IF ( V .GT. 0 ) THEN
                     N_CSPCS = N_CSPCS + 1
                     N_C_NR_SPC = N_C_NR_SPC + 1
                     OFF = N_GC_SPC + 1 + N_AE_SPC
                     C_NR_SPC( N_C_NR_SPC ) = CONC_FILE_SPCS( VAR )
                     NR_MAP( N_C_NR_SPC ) = V + OFF
                  ELSE
                     V = INDEX1 ( CONC_FILE_SPCS( VAR ), N_TR_SPC, TR_SPC )
                     IF ( V .GT. 0 ) THEN
                        N_CSPCS = N_CSPCS + 1
                        N_C_TR_SPC = N_C_TR_SPC + 1
                        OFF = N_GC_SPC + 1 + N_AE_SPC + N_NR_SPC
                        C_TR_SPC( N_C_TR_SPC ) = CONC_FILE_SPCS( VAR )
                        TR_MAP( N_C_TR_SPC ) = V + OFF
                     ELSE IF ( CONC_FILE_SPCS( VAR ) .EQ. 'RH' ) THEN
                        L_CONC_RH = .TRUE.
                     ELSE IF ( CONC_FILE_SPCS( VAR ) .EQ. 'TA' ) THEN
                        L_CONC_TA = .TRUE.
                     ELSE IF ( CONC_FILE_SPCS( VAR ) .EQ. 'PRES' ) THEN
                        L_CONC_PRES = .TRUE.
                     ELSE IF ( CONC_FILE_SPCS( VAR ) .NE. 'WVEL' ) THEN
                        XMSG = 'Variable ' // CONC_FILE_SPCS( VAR )
     &                  // ' incorrect for this model'
                        CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
                     END IF
                  END IF
               END IF
            END IF
         END DO

         C_GC_STRT = 1
         C_AE_STRT = C_GC_STRT + N_C_GC_SPC
         C_NR_STRT = C_AE_STRT + N_C_AE_SPC
         C_TR_STRT = C_NR_STRT + N_C_NR_SPC

         IF ( N_C_AE_SPC .GT. 0 ) THEN
            N_C_AE_SPCD = N_C_AE_SPC
         ELSE
            N_C_AE_SPCD = 1
         END IF

         ! Populate CONC species maps
         ALLOCATE ( CONC_MAP( N_CSPCS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'CONC_MAP memory allocation failed'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         END IF

         VAR = 0
         DO V = 1, N_C_GC_SPC
            VAR = VAR + 1
            CONC_MAP( VAR ) = GC_MAP( V )
         END DO

         DO V = 1, N_C_AE_SPC
            VAR = VAR + 1
            CONC_MAP( VAR ) = AE_MAP( V )
         END DO

         DO V = 1, N_C_NR_SPC
            VAR = VAR + 1
            CONC_MAP( VAR ) = NR_MAP( V )
         END DO

         DO V = 1, N_C_TR_SPC
            VAR = VAR + 1
            CONC_MAP( VAR ) = TR_MAP( V )
         END DO

         ! Deallocate Arrays if Possible
         IF ( N_C_GC_SPC .EQ. 0 ) DEALLOCATE ( C_GC_SPC )
         IF ( N_C_AE_SPC .EQ. 0 ) DEALLOCATE ( C_AE_SPC )
         IF ( N_C_NR_SPC .EQ. 0 ) DEALLOCATE ( C_NR_SPC )
         IF ( N_C_TR_SPC .EQ. 0 ) DEALLOCATE ( C_TR_SPC )

         END SUBROUTINE CONC_DEFN

      END MODULE STD_CONC
