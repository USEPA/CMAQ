
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

C Function: species, layer pointers and definitions for integral average
C CONC calculations

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

      CONTAINS

         SUBROUTINE A_CONC_DEFN ()

         USE RUNTIME_VARS
         USE VGRD_DEFN             ! vertical layer specifications
         USE CGRID_SPCS            ! CGRID mechanism species
         USE UTILIO_DEFN

         CHARACTER( 16 ) :: PNAME = 'A_CONC_DEFN'
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

C RETRIEVE THE SPECIES SAVED TO INTEGRAL AVERAGE CONCENTRATION FILE

         IF ( N_ASPCS .EQ. 0 .OR. ACONC_FILE_SPCS( 1 ) .EQ. 'ALL' ) THEN
            N_ASPCS = N_GC_SPC + N_AE_SPC + N_NR_SPC + N_TR_SPC
            DO VAR = 1, N_GC_SPC
               V = VAR
               ACONC_FILE_SPCS( V ) = GC_SPC( VAR )
            END DO
            DO VAR = 1, N_AE_SPC
               V = N_GC_SPC + VAR
               ACONC_FILE_SPCS( V ) = AE_SPC( VAR )
            END DO
            DO VAR = 1, N_NR_SPC
               V = N_GC_SPC + N_AE_SPC + VAR
               ACONC_FILE_SPCS( V ) = NR_SPC( VAR )
            END DO
            DO VAR = 1, N_TR_SPC
               V = N_GC_SPC + N_AE_SPC + N_NR_SPC + VAR
               ACONC_FILE_SPCS( V ) = TR_SPC( VAR )
            END DO
         END IF

C Retrieve the layer range used in integral average concentration file
         IF ( ACONC_ELEV .EQ. -1 ) ACONC_ELEV = NLAYS

         IF ( ACONC_BLEV .LE. 0 .OR. ACONC_ELEV .GT. NLAYS ) THEN
            WRITE( XMSG,'( "Layer range", 2I4, " invalid for this model" )' )
     &      ACONC_BLEV, ACONC_ELEV
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         END IF

C Check species names against include files and create AVG_CONC_MAP, and
C get starting index in AGRID and total count for each species class

         ALLOCATE ( AVG_CONC_MAP( N_ASPCS ),
     &              A_GC_SPC    ( N_ASPCS ),
     &              A_AE_SPC    ( N_ASPCS ),
     &              A_NR_SPC    ( N_ASPCS ),
     &              A_TR_SPC    ( N_ASPCS ),
     &              GC_MAP      ( N_ASPCS ),
     &              AE_MAP      ( N_ASPCS ),
     &              NR_MAP      ( N_ASPCS ),
     &              TR_MAP      ( N_ASPCS ), STAT = ALLOCSTAT )
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

         DO VAR = 1, N_ASPCS
            V = INDEX1 ( ACONC_FILE_SPCS( VAR ), N_GC_SPC, GC_SPC )
            IF ( V .GT. 0 ) THEN
               N_A_GC_SPC = N_A_GC_SPC + 1
               A_GC_SPC( N_A_GC_SPC ) = ACONC_FILE_SPCS( VAR )
               OFF = 0
               GC_MAP( N_A_GC_SPC ) = V + OFF
            ELSE
               V = INDEX1 ( ACONC_FILE_SPCS( VAR ), N_AE_SPC, AE_SPC )
               IF ( V .GT. 0 ) THEN
                  N_A_AE_SPC = N_A_AE_SPC + 1
                  OFF = N_GC_SPC + 1   ! accounts for advected density
                  A_AE_SPC( N_A_AE_SPC ) = ACONC_FILE_SPCS( VAR )
                  AE_MAP( N_A_AE_SPC ) = V + OFF
               ELSE
                  V = INDEX1 ( ACONC_FILE_SPCS( VAR ), N_NR_SPC, NR_SPC )
                  IF ( V .GT. 0 ) THEN
                     N_A_NR_SPC = N_A_NR_SPC + 1
                     OFF = N_GC_SPC + 1 + N_AE_SPC
                     A_NR_SPC( N_A_NR_SPC ) = ACONC_FILE_SPCS( VAR )
                     NR_MAP( N_A_NR_SPC ) = V + OFF
                  ELSE
                     V = INDEX1 ( ACONC_FILE_SPCS( VAR ), N_TR_SPC, TR_SPC )
                     IF ( V .GT. 0 ) THEN
                        N_A_TR_SPC = N_A_TR_SPC + 1
                        OFF = N_GC_SPC + 1 + N_AE_SPC + N_NR_SPC
                        A_TR_SPC( N_A_TR_SPC ) = ACONC_FILE_SPCS( VAR )
                        TR_MAP( N_A_TR_SPC ) = V + OFF
                     ELSE
                        XMSG = 'Variable ' // ACONC_FILE_SPCS( VAR )
     &                  // ' incorrect for this model'
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

         VAR = 0
         DO V = 1, N_A_GC_SPC
            VAR = VAR + 1
            AVG_CONC_MAP( VAR ) = GC_MAP( V )
#ifdef Verbose
            write( logdev,* ) '<<>>var, AVG_CONC_MAP: ', VAR, AVG_CONC_MAP( VAR )
#endif
         END DO

         DO V = 1, N_A_AE_SPC
            VAR = VAR + 1
            AVG_CONC_MAP( VAR ) = AE_MAP( V )
#ifdef Verbose
            write( logdev,* ) '<<>>var, AVG_CONC_MAP: ', VAR, AVG_CONC_MAP( VAR )
#endif
         END DO

         DO V = 1, N_A_NR_SPC
            VAR = VAR + 1
            AVG_CONC_MAP( VAR ) = NR_MAP( V )
#ifdef Verbose
            write( logdev,* ) '<<>>var, AVG_CONC_MAP: ', VAR, AVG_CONC_MAP( VAR )
#endif
         END DO

         DO V = 1, N_A_TR_SPC
            VAR = VAR + 1
            AVG_CONC_MAP( VAR ) = TR_MAP( V )
#ifdef Verbose
            write( logdev,* ) '<<>>var, AVG_CONC_MAP: ', VAR, AVG_CONC_MAP( VAR )
#endif
         END DO

         IF ( N_A_GC_SPC .EQ. 0 ) THEN
            DEALLOCATE ( A_GC_SPC )
            DEALLOCATE ( GC_MAP )
         END IF
         IF ( N_A_AE_SPC .EQ. 0 ) THEN
            DEALLOCATE ( A_AE_SPC )
            DEALLOCATE ( AE_MAP )
         END IF
         IF ( N_A_NR_SPC .EQ. 0 ) THEN
            DEALLOCATE ( A_NR_SPC )
            DEALLOCATE ( NR_MAP )
         END IF
         IF ( N_A_TR_SPC .EQ. 0 ) THEN
            DEALLOCATE ( A_TR_SPC )
            DEALLOCATE ( TR_MAP )
         END IF

         END SUBROUTINE A_CONC_DEFN

      END MODULE AVG_CONC
