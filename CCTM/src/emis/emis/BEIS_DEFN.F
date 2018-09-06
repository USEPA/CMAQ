
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
      MODULE BEIS_DEFN

C-----------------------------------------------------------------------
C Function: biogenics emissions interface to the chemistry-transport model

C Revision History:
C     18 Aug 2007 J.Young: initial implementation
C     16 Feb 2011 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C     10 May 2011 D.Wong: incorporated twoway model implementation

C-----------------------------------------------------------------------
      USE RUNTIME_VARS

      IMPLICIT NONE

      INTEGER,              SAVE :: N_SPC_BIOG ! biogenics emissions species
      REAL,    ALLOCATABLE, SAVE :: VDEMIS_BI( :,:,: ) ! beis emis
      INTEGER, ALLOCATABLE, SAVE :: BEIS_MAP( : ) ! map to GC
      CHARACTER( 16 ),      SAVE :: CTM_BIOGEMIS = 'CTM_BIOGEMIS'! env var for

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

         CHARACTER( 16 )       :: PNAME = 'BEIS_INIT'
         CHARACTER( 80 )       :: VARDESC   ! env variable description
         CHARACTER( 120 )      :: XMSG = ' '

         INTEGER N_SPC
         REAL    CONVEM               ! conversion for emissions rates to Kg/s
         INTEGER V, K, STATUS

C-----------------------------------------------------------------------

         SUCCESS = .TRUE.

C In-line biogenic emissions?
         CALL LOG_SUBHEADING( LOGDEV, 'Initialize Online Biogenic VOC Emissions Module (BEIS)' )

C biogenics to gas-phase species map
         IF ( BIOGEMIS ) THEN

            XMSG = 'Using in-line biogenic emissions option'
            CALL M3MSG2( XMSG )

            IF ( .NOT. BIOG_INIT() ) THEN
                XMSG = 'Could not initialize biogenic emissions variables'
                CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
                SUCCESS = .FALSE.; RETURN
            END IF

            ALLOCATE( VDEMIS_BI( MSPCS,MY_NCOLS,MY_NROWS ), STAT = STATUS )
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

         USE EMIS_VARS, ONLY : N_EMIS_ISTR, IBIOSRM,
     &                         MAP_EMtoSURR, MAP_EMtoDIFF
         USE BIOG_EMIS, ONLY : MSPCS
             
         IMPLICIT NONE

         INTEGER JDATE, JTIME, TSTEP( 3 ), ISTR
         LOGICAL, SAVE :: FIRST_TIME = .TRUE.

         INTERFACE
            SUBROUTINE TMPBEIS312 ( MDATE, MTIME, TSTEP, BEIS_MAP, VDEMIS_BI )
               INTEGER, INTENT(  IN ) :: MDATE, MTIME, TSTEP( 3 )
               INTEGER, INTENT(  IN ) :: BEIS_MAP( : )
               REAL,    INTENT( OUT ) :: VDEMIS_BI( :,:,: )
            END SUBROUTINE TMPBEIS312
         END INTERFACE

C-----------------------------------------------------------------------

         IF ( FIRST_TIME ) THEN
             ! Map out BEIS_MAP using MAP_EMtoSURR Array
             ALLOCATE( BEIS_MAP( MSPCS ) )
             BEIS_MAP = 0
             DO ISTR = 1,N_EMIS_ISTR
                 IF ( MAP_EMtoSURR( ISTR,IBIOSRM ) .GT. 0 ) 
     &              BEIS_MAP( MAP_EMtoSURR( ISTR,IBIOSRM ) ) = 
     &                     MAP_EMtoDIFF( ISTR )
             END DO

             FIRST_TIME = .FALSE.
         END IF    

         CALL TMPBEIS312( JDATE, JTIME, TSTEP, BEIS_MAP, VDEMIS_BI )

         RETURN

         END SUBROUTINE GET_BEIS

      END MODULE BEIS_DEFN

