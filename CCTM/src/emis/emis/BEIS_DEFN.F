
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

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/emis/emis/BEIS_DEFN.F,v 1.5 2011/10/21 16:10:45 yoj Exp $


C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE BEIS_DEFN

C-----------------------------------------------------------------------
C Function: biogenics emissions interface to the chemistry-transport model

C Revision History:
C     18 Aug 2007 J.Young: initial implementation
C     16 Feb 2011 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C     10 May 2011 D.Wong: incorporated twoway model implementation

C-----------------------------------------------------------------------

      IMPLICIT NONE

      LOGICAL,              SAVE :: BIOGEMIS   ! flag to in-line beis
      INTEGER,              SAVE :: N_SPC_BIOG ! biogenics emissions species
      REAL,    ALLOCATABLE, SAVE :: VDEMIS_BI( :,:,: ) ! beis emis
      INTEGER, ALLOCATABLE, SAVE :: BEIS_MAP( : ) ! map to GC

      CONTAINS

C=======================================================================

         FUNCTION BEIS_INIT ( JDATE, JTIME, TSTEP ) RESULT ( SUCCESS )

         USE HGRD_DEFN
         USE CGRID_SPCS          ! CGRID mechanism species
         USE BIOG_EMIS           ! from beis
         USE UTILIO_DEFN

         IMPLICIT NONE

         INCLUDE SUBST_CONST     ! constants

         INTEGER JDATE, JTIME, TSTEP
         LOGICAL SUCCESS

         REAL, PARAMETER :: CMLMR = 1.0E+06       ! ppmV/Molar Mixing Ratio

         CHARACTER( 16 ), SAVE :: CTM_BIOGEMIS = 'CTM_BIOGEMIS'! env var for
                                                               ! in-line beis
         CHARACTER( 16 )       :: PNAME = 'BEIS_INIT'
         CHARACTER( 80 )       :: VARDESC   ! env variable description
         CHARACTER( 120 )      :: XMSG = ' '

         INTEGER N_SPC
         REAL    CONVEM               ! conversion for emissions rates to Kg/s
         INTEGER V, K, LOGDEV, STATUS

C-----------------------------------------------------------------------

         SUCCESS = .TRUE.

         LOGDEV = INIT3()

         N_SPC = N_GC_EMIS + N_NR_EMIS + N_TR_EMIS

!        N_SPC_BIOG = MSPCS

C In-line biogenic emissions?

         BIOGEMIS = .FALSE.
         VARDESC = 'Flag for in-line biogenic emissions'
         BIOGEMIS = ENVYN( CTM_BIOGEMIS, VARDESC, BIOGEMIS, STATUS )
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 1 ) THEN
            XMSG = 'Environment variable improperly formatted'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         ELSE IF ( STATUS .EQ. -1 ) THEN
            XMSG = 'Environment variable set, but empty ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
         ELSE IF ( STATUS .EQ. -2 ) THEN
            XMSG = 'Environment variable not set ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
         END IF

         IF ( BIOGEMIS ) THEN

            XMSG = 'Using in-line biogenic emissions option'
            CALL M3MSG2( XMSG )

            IF ( .NOT. BIOG_INIT() ) THEN
               XMSG = 'Could not initialize biogenic emissions variables'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

C biogenics to gas-phase species map
            ALLOCATE( BEIS_MAP( MSPCS ), STAT = STATUS )
            IF ( STATUS .NE. 0 ) THEN
              XMSG = 'BEIS_MAP memory allocation failed'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            N_SPC_BIOG = 0
            DO V = 1, MSPCS
               K = INDEX1( EMSPC( V ), N_GC_EMIS, GC_EMIS )
               IF ( K .GT. 0 ) THEN
                  N_SPC_BIOG = N_SPC_BIOG + 1
                  BEIS_MAP( V ) = K
               ELSE
                  WRITE( LOGDEV,'(/5X,A)' ) TRIM( EMSPC( V ) ) //
     &                              ' not found in GC_EMIS'
                  K = INDEX1( EMSPC( V ), N_NR_EMIS, NR_EMIS )
                  IF ( K .GT. 0 ) THEN
                     N_SPC_BIOG = N_SPC_BIOG + 1
                     BEIS_MAP( V ) = N_GC_EMIS + K
                     WRITE( LOGDEV,* ) ' '
                  ELSE
                     WRITE( LOGDEV,'(5X,A/)' ) TRIM( EMSPC( V ) ) //
     &                                 ' not found in NR_EMIS either'
                     BEIS_MAP( V ) = 0
                  END IF
               END IF
            END DO

#ifdef Verbose
            WRITE( LOGDEV,* ) ' '
            WRITE( LOGDEV,* ) '    Spc INDEX1 BEIS_MAP EMSPC'
            DO V = 1, MSPCS
               K = INDEX1( EMSPC( V ), N_GC_EMIS, GC_EMIS )
               IF ( K .LE. 0 ) THEN
                  K = INDEX1( EMSPC( V ), N_NR_EMIS, NR_EMIS )
               END IF
               WRITE( LOGDEV,'( 3X, 2I5, I8, 4X, A16 )' )
     &                        V, K, BEIS_MAP( V ), EMSPC( V )
            END DO
#endif

            ALLOCATE( VDEMIS_BI( N_SPC_BIOG,MY_NCOLS,MY_NROWS ), STAT = STATUS )
            IF ( STATUS .NE. 0 ) THEN
              XMSG = 'VDEMIS_BI memory allocation failed'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

         END IF

         SUCCESS = .TRUE.; RETURN

         END FUNCTION BEIS_INIT

C=======================================================================

         SUBROUTINE GET_BEIS ( JDATE, JTIME, TSTEP )

         IMPLICIT NONE

         INTEGER JDATE, JTIME, TSTEP( 3 )

         INTERFACE
            SUBROUTINE TMPBEIS312 ( MDATE, MTIME, TSTEP, N_SPC_BIOG, BEIS_MAP, BEMIS )
               INTEGER, INTENT(  IN ) :: MDATE, MTIME, TSTEP( 3 )
               INTEGER, INTENT(  IN ) :: N_SPC_BIOG
               INTEGER, INTENT(  IN ) :: BEIS_MAP( : )
               REAL,    INTENT( OUT ) :: BEMIS( :,:,: )
            END SUBROUTINE TMPBEIS312
         END INTERFACE

C-----------------------------------------------------------------------

         CALL TMPBEIS312( JDATE, JTIME, TSTEP, N_SPC_BIOG, BEIS_MAP, VDEMIS_BI )

         RETURN

         END SUBROUTINE GET_BEIS

      END MODULE BEIS_DEFN

