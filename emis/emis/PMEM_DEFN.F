
C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/emis/emis/Attic/PMEM_DEFN.F,v 1.2 2010/12/06 13:00:02 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE PMEM_DEFN

C Set up PM species mapping from emissions file(s) for use in GET_AERO_EMIS

C Revision History:
C     24 Aug 2007 J.Young: initial
C     2 Mar 2010 S.Howard, J.Young: aerosol code reengineering
C-----------------------------------------------------------------------

      USE AERO_DATA

      IMPLICIT NONE

C Number of PM species in emission input file
      INTEGER, PARAMETER :: N_EMIS_PM = 6

!     TYPE, PRIVATE :: PM_TYPE
      TYPE          :: PM_TYPE
         CHARACTER( 16 ) :: NAME                  ! name of PM emission species
         REAL            :: EMIS_SPLIT( N_MODE )  ! split fraction for each mode
      END TYPE PM_TYPE

      TYPE( PM_TYPE ), PARAMETER :: PM_EMIS( N_EMIS_PM ) = (/
C                 Name        Aitken   Accum  Coarse
C               ---------     ------  ------  ------
     & PM_TYPE( 'POC    ', (/  0.001,  0.999,  0.000 /) ),
     & PM_TYPE( 'PSO4   ', (/  0.000,  1.000,  0.000 /) ),
     & PM_TYPE( 'PNO3   ', (/  0.000,  1.000,  0.000 /) ),
     & PM_TYPE( 'PEC    ', (/  0.001,  0.999,  0.000 /) ),
     & PM_TYPE( 'PMFINE ', (/  0.000,  1.000,  0.000 /) ),
     & PM_TYPE( 'PMC    ', (/  0.000,  0.000,  1.000 /) ) /)

C Search function for aerospc name array
!     INTEGER :: FINDPMEMIS

      CHARACTER( 16 ) :: UNITSPM( N_EMIS_PM ) ! units of all PM species

C Indices in the PM_EM array
      INTEGER :: VPOC  = 1   ! primary fine organic carbon
      INTEGER :: VPSO4 = 2   ! primary fine sulfate
      INTEGER :: VPNO3 = 3   ! primary fine nitrate
      INTEGER :: VPEC  = 4   ! primary fine elemental carbon
      INTEGER :: VPMF  = 5   ! unspeciated fine aerosol
      INTEGER :: VPMC  = 6   ! primary coarse aerosol

      INTEGER :: PMEMIS_MAP( N_EMIS_PM )      !pointers of aero # variable to CGRID

      INTEGER :: INDX10 = 0  ! flag when PMC is calc'd from PM10

C-----------------------------------------------------------------------

      CONTAINS

         SUBROUTINE GET_PMEM_MAP ( JDATE, JTIME )

            USE HGRD_DEFN

            INCLUDE SUBST_IOPARMS   ! I/O parameters definitions
#include            SUBST_IODECL
            INCLUDE SUBST_IOFDESC   ! file header data structure
            INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:
            INTEGER, INTENT( IN ) :: JDATE, JTIME

C External Functions:
            INTEGER, EXTERNAL :: INDEX1, TRIMLEN, SETUP_LOGDEV

C Local Variables:
            CHARACTER( 16 ), SAVE :: PNAME = 'GET_PMEM_MAP'
            CHARACTER( 96 ) :: XMSG = ' '

            INTEGER, SAVE :: LOGDEV

            INTEGER INDX, V

C-----------------------------------------------------------------------

            LOGDEV = SETUP_LOGDEV()

C Open the gridded emissions file, which contains gas, aerosol, and non-reactive
C species

            IF ( .NOT. OPEN3( EMIS_1, FSREAD3, PNAME ) ) THEN
               XMSG = 'Could not open '// EMIS_1 // ' file'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            IF ( .NOT. DESC3( EMIS_1 ) ) THEN
               XMSG = 'Could not get '// EMIS_1 // ' file description'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF

C Search emissions file for certain species names. For each species found,

            DO V = 1, N_EMIS_PM
               INDX = INDEX1( PM_EMIS( V )%NAME, NVARS3D, VNAME3D )
               IF ( INDX .LE. 0 ) THEN
                  XMSG = 'Could not find ' // TRIM( PM_EMIS( v )%NAME )
     &                 // ' in emissions file'
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
               END IF
               PMEMIS_MAP( V ) = INDX
               UNITSPM( V ) = UNITS3D( INDX )
            END DO

C Confirm that all aerosol species in the emission input file have the same units.
C If not, print error message and halt.

            DO V = 2, N_EMIS_PM
               IF ( UNITSPM( V ) .NE. UNITSPM( 1 ) ) THEN
                  XMSG = 'PM Units not uniform on ' // EMIS_1
                  CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
               END IF
            END DO

            RETURN

         END SUBROUTINE GET_PMEM_MAP

C-----------------------------------------------------------------------
         FUNCTION FINDPMEMIS( VNAME ) RESULT( NDX )

         IMPLICIT NONE

C arguments:
         CHARACTER( * ) :: VNAME
         INTEGER NDX

C includes:
         INCLUDE SUBST_IOPARMS   ! I/O parameters definitions

C local variables:
         INTEGER         :: SPC
         CHARACTER( 80 ) :: XMSG

         DO SPC = 1, N_EMIS_PM
            IF ( PM_EMIS( SPC )%NAME .EQ. VNAME ) THEN
               NDX = SPC
               RETURN
            END IF
         END DO

         XMSG = 'Required Species ' // TRIM( VNAME )
     &        // ' Not found in PM_EMIS names array'
         CALL M3EXIT( 'PMEM_DEFN', 0, 0, XMSG, XSTAT3 )

         RETURN
         END FUNCTION FINDPMEMIS

      END MODULE PMEM_DEFN

