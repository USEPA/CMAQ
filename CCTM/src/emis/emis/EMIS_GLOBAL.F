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
C $Header: /project/work/rep/arc/CCTM/src/emis/emis/EMIS_DEFN.F,v 1.10 2011/10/21 16:10:46 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE EMIS_GLOBAL

C-----------------------------------------------------------------------
C Function: Define global variables used for emissions processor

C Revision History:
C     09 Nov 2017 B.Murphy: Extracted these variables from EMIS_DEFN
C-----------------------------------------------------------------------
      USE UDTYPES, ONLY: CARRY1

      IMPLICIT NONE

      REAL,    ALLOCATABLE, SAVE :: VDEMIS( :,:,:,: ) ! total emissions array

      INTEGER,              SAVE :: N_EM_SRC   ! global number of emissions sources
      INTEGER,              SAVE :: N_EMIS_SPC ! global emissions species
      INTEGER,              SAVE :: EMLAYS     ! no. of emis integration layers
                                               ! EMLAYS must be .LE. NLAYS

      CHARACTER( 16 ), ALLOCATABLE, SAVE :: TRACSPC( : )  ! Variables from Tracer Emission File
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: AREASPC( : )  ! Variables straight from the Area Emission File
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: EM_SPEC  ( : )! Vector of Internal CMAQ Species names for 
                                                          ! each emission operation
      TYPE( CARRY1 ),  ALLOCATABLE, SAVE :: EM_FILE_SPEC( : )! Species to be read from each source file
      CHARACTER( 200 ),ALLOCATABLE, SAVE :: EM_FILE_NAME( : )! Filename of each emissions file
      CHARACTER( 30  ),ALLOCATABLE, SAVE :: EM_FILE_LAB ( : )! Nickname of each emissions file
      CHARACTER( 16  ),ALLOCATABLE, SAVE :: EM_FILE_TYPE( : )! shortname for the type of each emission
      CHARACTER( 100 ),ALLOCATABLE, SAVE :: EM_FILE_DESC( : )! Description of each emissions file

      REAL(8), ALLOCATABLE, SAVE :: EM_FAC ( :,: ) ! Emissions Scale Factors ( species, source )

      INTEGER, SAVE :: IBIOSRC, IMGSRC, ILTSRC, ISEASRC, IDUSTSRC
      INTEGER, ALLOCATABLE, SAVE :: IGSRC( : ), IPSRC( : ), ITSRC( : )

      INTEGER, ALLOCATABLE, SAVE :: MAP_AEtoSEA( : ), MAP_AEtoDUST( : )
      INTEGER, ALLOCATABLE, SAVE :: MAP_EMtoSEA( : ), MAP_EMtoDUST( : )

      END MODULE EMIS_GLOBAL
