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



!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE CHECK_CSQY_DATA

      IMPLICIT NONE

      CHARACTER( 32 ), SAVE :: JTABLE_REF

      INTEGER, SAVE :: NPHOT_REF  ! # ref phot reactions 
      INTEGER, SAVE :: NTEMP_REF  ! # ref temperatures 
      INTEGER, SAVE :: NWL_REF    ! # ref wavelengths 

!...Names of the mapped photolysis reactions (available to chemical)
!... mechanisms) and their pointers to the reference photolysis rxn

      CHARACTER( 16 ), ALLOCATABLE, SAVE :: PNAME_REF( : )

!...Setup the Mapping from CMAQ chemical reactions to the reference data

      INTEGER, SAVE :: NPHOT_MAP  ! #  phot mapped reactions 

      CHARACTER( 16 ), ALLOCATABLE, SAVE :: PNAME_MAP( : )
      INTEGER, ALLOCATABLE,         SAVE :: PHOT_MAP ( : )
      
      REAL, SAVE, ALLOCATABLE :: STWL_REF ( : ) 
      REAL, SAVE, ALLOCATABLE :: EFFWL_REF( : ) 
      REAL, SAVE, ALLOCATABLE :: ENDWL_REF( : ) 

      REAL, ALLOCATABLE, SAVE :: CLD_BETA_REF    ( : )  ! cloud extinction coef divided by LWC
      REAL, ALLOCATABLE, SAVE :: CLD_COALBEDO_REF( : )  ! cloud coalbedo
      REAL, ALLOCATABLE, SAVE :: CLD_G_REF       ( : )  ! cloud asymmetry factor

      REAL, ALLOCATABLE, SAVE :: FSOLAR_REF( : )        ! initial solar flux [photons*cm-2*s-1]

      REAL, ALLOCATABLE, SAVE :: TEMP_BASE ( : )        ! reference temperatures
      REAL, ALLOCATABLE, SAVE :: TEMP_REF( :,: )        ! reference temperatures

      REAL, ALLOCATABLE, SAVE :: CS_REF ( :,:,: )       ! effective cross sections
      REAL, ALLOCATABLE, SAVE :: QY_REF ( :,:,: )       ! effective quantum yields
      REAL, ALLOCATABLE, SAVE :: ECS_REF( :,:,: )       ! CS*QY averaged UCI Solar Flux

      INTEGER,           SAVE :: NTEMP_STRAT_REF        ! number of stratos temperatures
      REAL, ALLOCATABLE, SAVE :: TEMP_STRAT_REF( : )    ! temperature for stratos O3 xcross, K
      REAL, ALLOCATABLE, SAVE :: O3_CS_STRAT_REF( :,: ) ! ozone xcross at stratos temperatures, cm2

!...    effective quantum yields were computed by performing separate
!...    interval integrations for the cross sections and for the
!...    effective cross sections (cs*qy) (calculated on the finer
!...    wavelength grid.  The effective quantum yield values
!...    were then calculated for the 7 wavelength intervals by 
!...    dividing the effective cross sections by the interval average
!...    cross sections (eQY=eCS/CS).

      REAL, ALLOCATABLE, SAVE :: EQY_REF( :,:,: ) ! eCS/CS averaged 77 bins in UCI Model

      INTEGER, PARAMETER :: NUM_REFRACTIVE = 5
      TYPE MODAL_COMPLEX
         CHARACTER( 16 ) :: NAME                           ! name of complex property
         REAL, ALLOCATABLE, DIMENSION( :, : ) :: REAL_PART ! real part
         REAL, ALLOCATABLE, DIMENSION( :, : ) :: IMAG_PART ! imaginary part
      END TYPE MODAL_COMPLEX

      TYPE( MODAL_COMPLEX ), SAVE :: REFRACTIVE_INDEX( NUM_REFRACTIVE )

      INTEGER  :: IWLR  ! wavelength loop variable
      INTEGER  :: ITTR  ! temperature loop variable
      
! arrays for the size and optical properties of liquid droplets. The latter
! is a function of radius and wavelength
      INTEGER  :: NRADIUS_LIQUID
      
      REAL, ALLOCATABLE ::  RADIUS_LIQUID( : )       ! droplet radius, um
      
      REAL, ALLOCATABLE ::  LIQUID_EXTINCT( :, : )   ! extinction coefficient, m**3/g
      REAL, ALLOCATABLE :: LIQUID_ASYMFACT( :, : )   ! asymmetery factor, dimensionaless
      REAL, ALLOCATABLE :: LIQUID_COALBEDO( :, : )   ! One minus single scattering albebo, dimensionaless
      
! arrays for the size and optical properties of ice particles. The latter
! is a function of effective diameter and wavelength

      INTEGER  :: NDIAMETER_ICE

      REAL, ALLOCATABLE ::  DIAMETER_ICE( : )     ! particle effective diameter, um
      
      REAL, ALLOCATABLE ::  ICE_EXTINCT( :, : )   ! extinction coefficient, m**3/g
      REAL, ALLOCATABLE :: ICE_ASYMFACT( :, : )   ! asymmetery factor, dimensionaless
      REAL, ALLOCATABLE :: ICE_COALBEDO( :, : )   ! One minus single scattering albebo, dimensionaless

!***Information for photolysis

      INTEGER, SAVE :: NWL     ! number of wavelengths
!     INTEGER, PARAMETER  :: NWL_INLINE_METHOD = 7

      INTEGER IWL               ! index use for wavelength
      INTEGER ITT               ! index for temperature
      INTEGER IRRXN

      REAL, ALLOCATABLE, SAVE :: WAVELENGTH( : )  ! effective wavelengths [nm ]

      REAL, ALLOCATABLE, SAVE :: FEXT( : )   ! downward solar direct flux at the top of
                                             ! of the Atmosphere.  [ photons / ( cm **2 s) ]

!***surface albedo

      REAL, ALLOCATABLE, SAVE :: ALB( : )  ! set in subroutine PHOT

!**Cloud albedo values from JPROC

      REAL, ALLOCATABLE, SAVE :: CLOUD_BETA_LWC( : ) ! cloud extinction coef divided by LWC
      REAL, ALLOCATABLE, SAVE :: CLOUD_COALBEDO( : ) ! cloud coalbedo
      REAL, ALLOCATABLE, SAVE :: CLOUD_G( : )        ! cloud asymmetry factor

      INTEGER, SAVE :: NTEMP_STRAT
      REAL, ALLOCATABLE, SAVE :: XO3CS( :,: )       !
      REAL, ALLOCATABLE, SAVE :: TEMP_O3_STRAT( : ) ! temperature for XO3CS, K

!***arrays for reference data for needed photolysis rates

      REAL, ALLOCATABLE, SAVE :: XXCS( :,:,: )  ! absorption cross sections
      REAL, ALLOCATABLE, SAVE :: XXQY( :,:,: )  ! quantum yield

      REAL, ALLOCATABLE, SAVE :: RTEMP_S( :,: )

      CHARACTER(16), ALLOCATABLE, SAVE  :: PHOTOLYSIS_RATE( : ) ! subset of photolysis rates from CSQY DATA


!***Indices for special case photolysis cross sections

      INTEGER, SAVE :: LNO2
      INTEGER, SAVE :: LO3O1D
      INTEGER, SAVE :: LO3O3P
      INTEGER, SAVE :: LACETONE
      INTEGER, SAVE :: LKETONE
      INTEGER, SAVE :: LMGLY_ADJ
      INTEGER, SAVE :: LMGLY_ABS
      INTEGER, SAVE :: LHCHOR_06

      INTEGER, SAVE :: IREFTEMPS  ! number of ref. temperatures

      INTEGER, SAVE :: NUMB_LANDUSE_REF
      INTEGER, SAVE :: INDEX_GRASSLAND_REF
      INTEGER, SAVE :: INDEX_OCEAN_REF
      INTEGER, SAVE :: INDEX_SEA_ICE

      CHARACTER(30), ALLOCATABLE, SAVE :: LANDUSE_REF( : )
      REAL,          ALLOCATABLE, SAVE :: ZENITH_COEFF_REF( : )
      REAL,          ALLOCATABLE, SAVE :: SEASON_COEFF_REF( : )
      REAL,          ALLOCATABLE, SAVE :: SNOW_COEFF_REF( : )
      REAL,          ALLOCATABLE, SAVE :: SPECTRAL_ALBEDO_REF( :,: )

      INTEGER, PARAMETER :: NUMB_EXPECT_NLCD50  = 50
      INTEGER, SAVE      :: NUMB_LANDUSE_NLCD50
      CHARACTER(60), ALLOCATABLE, SAVE :: LANDUSE_NLCD50( : )
      INTEGER,       ALLOCATABLE, SAVE :: ALBMAP_REF2NLCD50( : )
      REAL,          ALLOCATABLE, SAVE :: ALBFAC_REF2NLCD50( : )

      INTEGER, PARAMETER :: NUMB_EXPECT_NLCD40  = 40
      INTEGER, SAVE      :: NUMB_LANDUSE_NLCD40
      CHARACTER(60), ALLOCATABLE, SAVE :: LANDUSE_NLCD40( : )
      INTEGER,       ALLOCATABLE, SAVE :: ALBMAP_REF2NLCD40( : )
      REAL,          ALLOCATABLE, SAVE :: ALBFAC_REF2NLCD40( : )

      INTEGER, PARAMETER :: NUMB_EXPECT_USGS  = 24
      INTEGER, SAVE      :: NUMB_LANDUSE_USGS
      CHARACTER(60), ALLOCATABLE, SAVE :: LANDUSE_USGS( : )
      INTEGER,       ALLOCATABLE, SAVE :: ALBMAP_REF2USGS( : )
      REAL,          ALLOCATABLE, SAVE :: ALBFAC_REF2USGS( : )

      INTEGER, PARAMETER :: NUMB_EXPECT_MODIS = 33
      INTEGER, SAVE      :: NUMB_LANDUSE_MODIS
      CHARACTER(60), ALLOCATABLE, SAVE :: LANDUSE_MODIS( : )
      INTEGER,       ALLOCATABLE, SAVE :: ALBMAP_REF2MODIS( : )
      REAL,          ALLOCATABLE, SAVE :: ALBFAC_REF2MODIS( : )

      LOGICAL, SAVE      :: NO_NLCD40
      LOGICAL, SAVE      :: WRITE_CELL

!***special information for acetone
!***  Reference:
!***     Cameron-Smith, P., Incorporation of non-linear
!***     effective cross section parameterization into a
!***     fast photolysis computation  code (Fast-J)
!***     Journal of Atmospheric Chemistry, Vol. 37,
!***     pp 283-297, 2000.

      INTEGER, PARAMETER :: NWL_ACETONE_FJX = 7

      REAL, SAVE :: OP0( 2, NWL_ACETONE_FJX ) ! variable needed for acetone

      DATA ( OP0( 1, IWL ), IWL = 1, NWL_ACETONE_FJX ) /
     &     2.982E-20, 1.301E-20, 4.321E-21, 1.038E-21,
     &     5.878E-23, 1.529E-25, 0.0/

      DATA ( OP0( 2, IWL ), IWL = 1, NWL_ACETONE_FJX ) /
     &     3.255E-20, 1.476E-20, 5.179E-21, 1.304E-21,
     &     9.619E-23, 2.671E-25, 0.0 /

      REAL, SAVE :: YY30( NWL_ACETONE_FJX )   ! variable needed for acetone

      DATA YY30 / 5.651E-20, 1.595E-19, 2.134E-19,
     &     1.262E-19, 1.306E-19, 1.548E-19, 0.0 /

      REAL :: OPTT                ! variable needed for acetone
      
      INTEGER, PARAMETER :: N_MODE = 3
      
      CONTAINS
      
     
      subroutine m3exit ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      
      implicit none
      character*(*), intent( in ) :: pname
      character*(*), intent( in ) :: xmsg
      integer,       intent( in ) :: jdate, jtime, xstat1
      
      if(XSTAT1 .EQ. 0)then
         print *, ' *******program ',pname,' terminated normally********'
      elseif(XSTAT1 .EQ. 1)then
         print*,  ' IO error in ',pname
      elseif(XSTAT1 .GE. 2)then
         print*, ' Programing Error in',pname
      endif
      print *, ' at date/time ', jdate, jtime
      print *, xmsg
      stop
      end subroutine m3exit
      
      subroutine m3warn ( PNAME, JDATE, JTIME, XMSG )
      
      implicit none
      character*(*), intent( in ) :: pname
      character*(*), intent( in ) :: xmsg
      integer,       intent( in ) :: jdate, jtime
      
      print *, ' !!!warning in program ',pname,' !!!!!!!!!!'
      print *, ' at date/time ', jdate, jtime
      print *, xmsg
      return
      end subroutine m3warn
      
      subroutine m3MESG (  XMSG )
      
      implicit none
      character*(*), intent( in ) :: xmsg
      
      print *, ' !!!M3MESG!!!!!!!!!!'
      print *, xmsg
      return
      end subroutine m3MESG

      integer function init3 ( )
      
      implicit none
     
      init3 = 6
      return
      end function init3

      INTEGER FUNCTION  JUNIT()

C***********************************************************************
C  function body starts at line 60
C
C  FUNCTION:
C
C    Routine returns next available FORTRAN unit number
C
C  REVISION HISTORY:
C
C    3/88  Maximum number of I/O unit numbers was increased from 50
C          to 75 due to increased file I/O requirements.
C    5/88  Modified for ROMNET
C    7/90  Modified for ROM 2.2 -- uses EXWST for error abort.
C    8/90  Algorithm simplification:  replaced IF-GOTO loop by DO loop.
C    8/90  Algorithm simplification:  counting algorithm instead of table
C          of flags; uses POSIX standards-approved unit numbers 11-99
C    3/92  Models-3 Prototype version (eliminate EXWST)
C    8/96  Modified by CJC -- On counting-algorithm failure, performs
C          INQUIREs to find available unit.
C    2/97  conditional definition of EXIT under AIX
C
C  ARGUMENT LIST DESCRIPTION:  empty argument list
C
C  RETURN VALUE:  JUNIT        Unit number selected
C
C  LOCAL VARIABLE DESCRIPTION:
C
C    IUNIT    state variable:  counts through available units
C    BOT      parameter:  first POSIX-approved unit number for FORTRAN I/O
C    TOP      parameter:  last   ...
C
C***********************************************************************


      IMPLICIT NONE

C...........   PARAMETERS and their descriptions:

      INTEGER      BOT          !  1 less than initial unit number
      INTEGER      TOP          !  final unit number
      PARAMETER  ( BOT = 10, TOP = 99 )


C...........   SAVED LOCAL VARIABLES and their descriptions:

      INTEGER      IUNIT, J
      LOGICAL      FLAG
      DATA         IUNIT / TOP /      !  current highest unit number
      SAVE         IUNIT

C............................................................................
C.......   begin body of JUNIT:

      IF ( IUNIT .GE. BOT ) THEN
          JUNIT = IUNIT
          IUNIT = IUNIT - 1
      ELSE
          DO  11  J = BOT, TOP
              INQUIRE( UNIT=J, OPENED=FLAG )
              IF ( .NOT. FLAG ) THEN
                  JUNIT = J
                  RETURN
              END IF 
11        CONTINUE
  
C.........   If you get to here: failure

          WRITE (*,91001) BOT, TOP, IUNIT
          CALL EXIT( 2 )
      END IF
      RETURN
91001 FORMAT (///, 1X, '*** ERROR ABORT IN ROUTINE JUNIT ***',
     &          /, 5X, 'NO MORE UNIT NUMBERS AVAILABLE FOR I/O',
     &          /, 5X, 'First POSIX-approved unit:', I4 ,
     &          /, 5X, 'Last  POSIX-approved unit:', I4 ,
     &          /, 5X, 'Current unit:             ', I4 ,
     &          //)
      END FUNCTION  JUNIT

      INTEGER FUNCTION GETEFILE( LNAME, RDONLY, FMTFLAG, CALLER )

C***********************************************************************
C Version "@(#)$Header$"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  79
C
C  DESCRIPTION:
C    Gets value of logical name LNAME from the environment, checks for
C    existence of a file whose file name is that value, then opens the
C    file as a sequential file on unit IUNIT according to the flags RDONLY
C    (open for read-only iff TRUE, read/write if FALSE) and FMTFLAG
C    (formatted iff TRUE, else unformatted).
C    Logs the file-opening, together with the CALLER version, and
C    returns the unit number (or -1 for failure)
C
C    RETURNS:   unit number, (or -1 for failure)
C
C  PRECONDITIONS REQUIRED:
C
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       TRIMLEN, JUNIT
C
C  REVISION  HISTORY:
C       Prototype  2/1995 by CJC.
C
C       Modified  9/1999 by CJC:  portability/standardization changes
C
C       Modified 7/2003 by CJC:  OMP thread safety -- critical sections
C       associated with INIT3()
C
C       Modified 11/2004 by CJC:  IOSTAT check for INQUIRE
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************
       USE GET_ENV_VARS
       
       IMPLICIT NONE


C...........   ARGUMENTS and their descriptions:


       CHARACTER*(*), INTENT(IN   ) :: LNAME          !  logical file name
       LOGICAL      , INTENT(IN   ) :: RDONLY         !  TRUE iff file is input-only
       LOGICAL      , INTENT(IN   ) :: FMTFLAG        !  TRUE iff file should be formatted
       CHARACTER*(*), INTENT(IN   ) :: CALLER         !  caller-name for logging

C...........   EXTERNAL FUNCTIONS and their descriptions:

!        INTEGER, EXTERNAL :: INIT3
!        INTEGER, EXTERNAL :: JUNIT


C...........   LOCAL VARIABLES and their descriptions:


        INTEGER         IUNIT
        INTEGER         LLEN, PLEN
        INTEGER		ISTAT, JSTAT

        LOGICAL         LCHECK

        CHARACTER*512   PNAME
        CHARACTER*512   MESG
        CHARACTER*16    FMTSTRING

        INTEGER , SAVE :: LOGDEV = -1

C***********************************************************************
C   begin body of GETEFILE()

!$OMP   CRITICAL( S_INIT )
        IF ( LOGDEV .LT. 0 ) THEN
            LOGDEV = INIT3()
        END IF
!$OMP   END CRITICAL( S_INIT )

C...........   Read filename from environment ---

      CALL VALUE_NAME(  LNAME, PNAME )

      LLEN = LEN_TRIM( LNAME )
      PLEN = LEN_TRIM( PNAME )

      IF ( FMTFLAG ) THEN
          FMTSTRING = 'FORMATTED'
      ELSE
          FMTSTRING = 'UNFORMATTED'
      END IF

C.......   Check for existence of files ---

      INQUIRE( FILE=PNAME( 1:PLEN ) , EXIST=LCHECK, IOSTAT=JSTAT )

      IF ( JSTAT .NE. 0 ) THEN
          WRITE( MESG, '( A, I11, 2X, A, 1X, A)' )
     &       'Error', JSTAT, 'inquiring about', LNAME
          CALL M3MESG( MESG )
          MESG = 'Path-name: ' // PNAME
          CALL M3MESG( MESG )
          GETEFILE = -1
          RETURN
      END IF

      IF ( RDONLY ) THEN

          IF ( .NOT. LCHECK ) THEN
              WRITE( LOGDEV, 9000, IOSTAT=JSTAT, ERR=9999 )
     &            'ERROR: input file not found: ',
     &            LNAME( 1:LLEN ),
     &            PNAME( 1:PLEN )
              GETEFILE = -1
              RETURN
          END IF

          IUNIT = JUNIT()

!          OPEN( UNIT   = IUNIT,
!     &          FILE   = PNAME,
!     &          FORM   = FMTSTRING,
!     &          ACTION = 'READ',
!     &          IOSTAT = ISTAT )
          OPEN( UNIT   = IUNIT,
     &          FILE   = PNAME,
     &          FORM   = FMTSTRING,
     &          STATUS = 'OLD',
     &          READONLY,
     &          IOSTAT = ISTAT )

          IF ( ISTAT .NE. 0 ) THEN
              WRITE(LOGDEV,*)'ERROR: file not opened successfully'
              WRITE( LOGDEV, 9100, IOSTAT=JSTAT, ERR=9999 )
     &        LNAME( 1:LLEN ),
     &        PNAME( 1:PLEN ),
     &        'I/O status:  ', ISTAT
              GETEFILE = -1
              STOP
          END IF

          WRITE( LOGDEV, 9001, IOSTAT=JSTAT, ERR=9999 )
     &        'File "', LNAME( 1:LLEN ),
     &        '" opened for input on unit:', IUNIT,
     &        PNAME(  1:PLEN  )

      ELSE      !  not read-only:  open for write

          IF ( LCHECK ) THEN
              WRITE( LOGDEV, 9000, IOSTAT=JSTAT, ERR=9999 )
     &            'WARNING: output file already exists: ',
     &            LNAME( 1:LLEN ),
     &            PNAME( 1:PLEN )
          END IF

          IUNIT = JUNIT()

           OPEN( UNIT   = IUNIT,
     &          FILE   = PNAME,
     &          FORM   = FMTSTRING,
     &          IOSTAT = ISTAT )

          IF ( ISTAT .NE. 0 ) THEN
              WRITE(LOGDEV,*)'ERROR: file not opened successfully'
              WRITE( LOGDEV, 9100, IOSTAT=JSTAT, ERR=9999 )
     &        LNAME( 1:LLEN ),
     &        PNAME( 1:PLEN ),
     &        'I/O status:  ', ISTAT
              GETEFILE = -1
              STOP
          END IF

          WRITE( LOGDEV, 9001, IOSTAT=JSTAT, ERR=9999 )
     &        'File "', LNAME( 1:LLEN ),
     &        '" opened for output on unit:', IUNIT,
     &        PNAME(  1:PLEN  )

      END IF

      GETEFILE = IUNIT
      RETURN

C----------------------------------------------------------------------
C     handle messaging errors:

9999  CONTINUE

      IF ( JSTAT .NE. 0 ) THEN
           WRITE(LOGDEV,*)'ERROR writing log message.'
           WRITE( LOGDEV, 9200, IOSTAT=ISTAT )
     &         'I/O status:', JSTAT
           GETEFILE = -1
           RETURN
      END IF

C-----------------------------------------------------------------------
C    Format statements:

8000   FORMAT( A )
9000   FORMAT( /, 1X, 2A, /, :, 1X, A, / )
9001   FORMAT( /, 5X, 3A, I4,
     &         /, 5X,  A, /)
9100   FORMAT( /, 1X,  A, ':', A,
     &         /, 1X,  A, I7 , / )
9200   FORMAT( /, 1X,  A, I7 , / )

       END FUNCTION GETEFILE


       INTEGER FUNCTION INDEXR ( NAME1, N, NAME2 )
C-----------------------------------------------------------------------
C
C  FUNCTION:
C     This routine searches for NAME1 in list NAME2
C
C  REVISION HISTORY:
C     5/88   Modified for ROMNET
C     July 29, 2005 by FSB
C     Changed name to avoid conflict FSB
C     copied from CMAQ routine INDEX2 to allow internal use
C
C  ARGUMENT LIST DESCRIPTION:
C
C  Input arguments:
C     NAME1       Character string being searched for
C     N           Length of array to be searched
C     NAME2       Character array to be searched
C
C  Output arguments:
C     INDEX1      The position within the NAME2 array that NAME1
C                 found.  If string was not found, INDEX1 = 0
C
C  LOCAL VARIABLE DESCRIPTION:
C     None
C
C-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: N

      CHARACTER*(*), INTENT(IN) :: NAME1
      CHARACTER*(*), INTENT(IN) :: NAME2(*)

      INTEGER I

!***Assume NAME1 is not in list NAME2

      INDEXR = 0

      DO I = 1, N
         IF ( INDEX( NAME2( I ), NAME1 ) .EQ. 1 ) THEN
            INDEXR = I
            RETURN
         END IF
      END DO

      RETURN
      END FUNCTION INDEXR
      
      SUBROUTINE LOAD_CSQY_DATA ( )
!-----------------------------------------------------------------------
!  Purpose: read input file for 
!           -wavelength bin and temperature structure.
!           -photolysis cross-sections and quantum
!
!  Revision History:
!   31 Jan 2014 B.Hutzell: Initial Version based on LOAD_REF_DATA in
!   CMAQ version 5.0
!-----------------------------------------------------------------------

!      USE UTILIO_DEFN

      IMPLICIT NONE

      INCLUDE SUBST_RXCMMN      ! chemical mechamism reactions COMMON

C***arguments

C     NONE

C***local

      LOGICAL :: WRITE_LOG = .TRUE.

      CHARACTER(  32 ) :: PNAME = 'LOAD_CSQY_DATA'
      CHARACTER(  16 ) :: CSQY_FILE = 'CSQY_DATA' ! CSQY_DATA i/o logical name
      CHARACTER(  16 ) :: PHOT_EXPECT
      CHARACTER(  30 ) :: LAND_EXPECT
      CHARACTER( 120 ) :: MSG                     ! buffer for messages to output
      CHARACTER( 240 ) :: FILE_LINE

      CHARACTER(  16 ),  ALLOCATABLE :: AE_RERACT_REF( : )

!     INTEGER, INTENT(OUT) :: NWL_PHOT    ! # of wavelengths used in PHOT_MOD.F
      INTEGER :: NWL_PHOT    ! # of wavelengths used in PHOT_MOD.F
      INTEGER :: IOST        ! IOST returned from OPEN function
      INTEGER :: JDATE = 0
      INTEGER :: LOG_UNIT
      INTEGER :: PHOTAB_UNIT
      INTEGER :: IPHOT, IPHOT_LOAD ! loop indices
      INTEGER :: ITT, ITT_LOAD     ! loop indices 
      INTEGER :: IP_MAP, IP_REF    ! photolysis reaction indicies
      INTEGER :: IWL_LOAD
      INTEGER :: STRT, FINI


      INTEGER :: NAE_REFRACT_REF 

      REAL,       ALLOCATABLE :: AE_IMAG_REFRACT( :, : )
      REAL,       ALLOCATABLE :: AE_REAL_REFRACT( :, : )

      LOGICAL                  :: ERROR_FLAG = .FALSE.

C***external functions: none

      LOG_UNIT = INIT3()

      PHOTAB_UNIT = GETEFILE( CSQY_FILE, .TRUE., .TRUE., PNAME )

      IF ( PHOTAB_UNIT .LT. 0 ) THEN
         MSG = 'Error opening the CSQY data file: ' // TRIM( CSQY_FILE )
         CALL M3EXIT ( PNAME, 0, 0, MSG, -1 )
      END IF

C...begin read

      READ( PHOTAB_UNIT,'(22X,A32)' ) JTABLE_REF

      IF ( JTABLE_REF .NE. MECHNAME ) THEN
         MSG =  'WARNING: JTABLE mechanism is for ' // JTABLE_REF
     &       // ' but gas chemistry name is '       // MECHNAME
         CALL M3WARN( PNAME, 0, 0, MSG )
      END IF

      READ( PHOTAB_UNIT,'(10X,I4)' ) NPHOT_MAP

#ifdef verbose_phot
      write( log_unit,'(22x,a32)' ) jtable_ref
      write( log_unit,'(10x,i4)' ) nphot_map
#endif

      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE

      ALLOCATE( PNAME_MAP( NPHOT_MAP ) )
      ALLOCATE( PNAME_REF( NPHOT_MAP ) )
      ALLOCATE( PHOT_MAP ( NPHOT_MAP ) )

      DO IPHOT_LOAD = 1, NPHOT_MAP
         READ( PHOTAB_UNIT,'(A16)' ) PNAME_REF( IPHOT_LOAD )

#ifdef verbose_phot
         write( log_unit,'(i3,1x,a16)' ) iphot_load, pname_ref( iphot_load )
#endif

         PNAME_MAP( IPHOT_LOAD ) = PNAME_REF( IPHOT_LOAD )
         PHOT_MAP ( IPHOT_LOAD ) = IPHOT_LOAD
      END DO

      READ( PHOTAB_UNIT,'(10X,I3)' ) NTEMP_REF

#ifdef verbose_phot
      write( log_unit,'(10x,i3)' ) ntemp_ref
#endif

      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE

#ifdef verbose_phot
      write( log_unit,* ) trim( file_line )
#endif

      IREFTEMPS = NTEMP_REF

      ALLOCATE( TEMP_BASE( NTEMP_REF ) )

      DO ITT_LOAD = 1, NTEMP_REF
         READ( PHOTAB_UNIT,'(A)' ) FILE_LINE

#ifdef verbose_phot
         write( log_unit,* ) trim( file_line )
#endif

         READ( FILE_LINE,* ) IPHOT_LOAD, TEMP_BASE( ITT_LOAD )

#ifdef verbose_phot
         write( log_unit,'(4x,f6.2)' ) temp_base( itt_load )
#endif

      END DO

      ALLOCATE( TEMP_REF( NTEMP_REF, NPHOT_MAP) )

      DO ITT_LOAD = 1, 15 ! skip next 15 lines
         READ( PHOTAB_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
         write( log_unit, '(I2,1X,A)' )ITT_LOAD,TRIM(FILE_LINE)
#endif
      END DO

      READ( FILE_LINE, 4999) NWL_REF

      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE
      
#ifdef verbose_phot
      write( log_unit, * )TRIM(FILE_LINE)
#endif

4999  FORMAT(17X,I3,2X,17X,I3)

     
      
      NWL       = NWL_REF
      NWL_PHOT  = NWL

#ifdef verbose_phot
      write( log_unit,'(17x,i3)' ) nwl_ref
#endif

      IF ( NWL_REF .NE. NWL_PHOT ) THEN
         WRITE( LOG_UNIT,* ) 'NWL_PHOT = ', NWL_PHOT
         WRITE( LOG_UNIT,* ) 'NWL_REF  = ', NWL_REF
         MSG = 'NWL_REF used in ' // CSQY_FILE
     &       // ' does equal NWL in PHOT_MOD.F file. '
         CALL M3EXIT( PNAME, 0, 0, MSG, -1 )
      END IF

      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
      write( log_unit, * )FILE_LINE
#endif

      ALLOCATE( STWL_REF        ( NWL_REF ) )
      ALLOCATE( EFFWL_REF       ( NWL_REF ) )
      ALLOCATE( ENDWL_REF       ( NWL_REF ) )
      ALLOCATE( FSOLAR_REF      ( NWL_REF ) )
      ALLOCATE( CLD_BETA_REF    ( NWL_REF ) )
      ALLOCATE( CLD_COALBEDO_REF( NWL_REF ) )
      ALLOCATE( CLD_G_REF       ( NWL_REF ) )

      DO IWL_LOAD = 1, NWL_REF
!         READ( PHOTAB_UNIT,'(4X,3(F8.3,2X),2X,ES12.4,2X,2(F8.3,2X),ES12.4,2X)' )
         READ( PHOTAB_UNIT, * )iphot_load,
     &         STWL_REF( IWL_LOAD ), EFFWL_REF( IWL_LOAD ),
     &         ENDWL_REF( IWL_LOAD ), FSOLAR_REF( IWL_LOAD )
     
#ifdef verbose_phot
         write( log_unit,'(4x,3(f8.3,2x),2x,2(es12.4,2x),f8.3,2x,12(es12.4,2x))' )
     &          stwl_ref( iwl_load ), effwl_ref( iwl_load ),
     &          endwl_ref( iwl_load ),fsolar_ref( iwl_load )
#endif

      END DO


      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE
      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE
      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE
      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE

      ALLOCATE( CS_REF ( NPHOT_MAP, NTEMP_REF, NWL_REF ) )
      ALLOCATE( QY_REF ( NPHOT_MAP, NTEMP_REF, NWL_REF ) )
      ALLOCATE( EQY_REF( NPHOT_MAP, NTEMP_REF, NWL_REF ) )
      ALLOCATE( ECS_REF( NPHOT_MAP, NTEMP_REF, NWL_REF ) )

      CS_REF = 0.0
      QY_REF  = 0.0
      EQY_REF = 0.0
      ECS_REF = 0.0

      DO IPHOT_LOAD = 1, NPHOT_MAP
         DO ITT_LOAD = 1, NTEMP_REF
            READ( PHOTAB_UNIT,'(A16,7X,F8.3,1X,40(1PE12.6,2X))' )
     &            PHOT_EXPECT, TEMP_REF( ITT_LOAD, IPHOT_LOAD),
     &            ( CS_REF( IPHOT_LOAD, ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_REF )

#ifdef verbose_phot
            write( log_unit,'(a16,7x,f8.3,1x,40(1pe12.6,2x))' )
     &             phot_expect, temp_ref( itt_load, iphot_load),
     &             ( cs_ref( iphot_load, itt_load, iwl_load), iwl_load = 1, nwl_ref )
#endif

            IF ( PHOT_EXPECT .NE. PNAME_REF( IPHOT_LOAD ) ) THEN
                MSG =  'CS for ' // TRIM( PHOT_EXPECT )
     &              // ' does match the order the PHOT_MAP array.'
                CALL M3EXIT( PNAME, 0, 0, MSG, -1 )
            END IF

            READ( PHOTAB_UNIT,'(A16,7X,F8.3,1X,40(1PE12.6,2X))' )
     &            PHOT_EXPECT, TEMP_REF( ITT_LOAD, IPHOT_LOAD),
     &            ( EQY_REF( IPHOT_LOAD, ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_REF )

            QY_REF( IPHOT_LOAD, ITT_LOAD, 1:NWL_REF) = EQY_REF( IPHOT_LOAD, ITT_LOAD, 1:NWL_REF)

#ifdef verbose_phot
            write( log_unit,'(a16,7x,f8.3,1x,40(1pe12.6,2x))' )
     &             phot_expect, temp_ref( itt_load, iphot_load),
     &             ( qy_ref( iphot_load, itt_load, iwl_load), iwl_load = 1, nwl_ref )
#endif

            IF ( PHOT_EXPECT .NE. PNAME_REF(IPHOT_LOAD) ) THEN
               MSG =  'EQY for ' // TRIM( PHOT_EXPECT )
     &             // ' does match the order the PHOT_MAP array.'
               CALL M3WARN( PNAME, 0, 0, MSG )
               ERROR_FLAG = .TRUE.
            END IF
         END DO
      END DO

      DO ITT_LOAD = 1, 3 ! skip next 3 lines
         READ( PHOTAB_UNIT,'(A)' ) FILE_LINE
      END DO

      READ( PHOTAB_UNIT,'(15X,I3)' ) NTEMP_STRAT_REF

#ifdef verbose_phot
      write( log_unit,'(16x,i3)' ) ntemp_strat_ref
#endif

      ALLOCATE( TEMP_STRAT_REF ( NTEMP_STRAT_REF ) )
      ALLOCATE( O3_CS_STRAT_REF( NTEMP_STRAT_REF, NWL_REF ) )

      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE

      DO ITT_LOAD = 1, NTEMP_STRAT_REF
         READ( PHOTAB_UNIT,'(A16,7X,F8.3,1X,40(1PE12.6,2X))' )
     &         PHOT_EXPECT, TEMP_STRAT_REF( ITT_LOAD ),
     &         ( O3_CS_STRAT_REF( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_REF )

#ifdef verbose_phot
         write( log_unit,'(a16,7x,f8.3,1x,40(1pe12.6,2x))' )
     &          phot_expect, temp_strat_ref( itt_load ),
     &          ( o3_cs_strat_ref( itt_load, iwl_load), iwl_load = 1, nwl_ref )
#endif

         IF ( PHOT_EXPECT .NE. 'O3_STRAT' ) THEN
            MSG = 'O3_STRAT not found at expected location in CSQY_FILE. ' //
     &            TRIM( PHOT_EXPECT ) // ' found.'
            CALL M3WARN( PNAME, 0, 0, MSG )
            ERROR_FLAG = .TRUE.
         END IF
      END DO


      NTEMP_STRAT = NTEMP_STRAT_REF
      ALLOCATE( TEMP_O3_STRAT( NTEMP_STRAT_REF ) )
      ALLOCATE( XO3CS        ( NTEMP_STRAT_REF, NWL_PHOT ) )

      DO ITT_LOAD = 1, NTEMP_STRAT_REF
         TEMP_O3_STRAT( ITT_LOAD ) = TEMP_STRAT_REF( ITT_LOAD )
         DO IWL_LOAD = 1, NWL_PHOT
            XO3CS( ITT_LOAD, IWL_LOAD ) = O3_CS_STRAT_REF( ITT_LOAD, IWL_LOAD )
         END DO
      END DO

C***initialize pointers for mandatory photolysis rates

      LNO2      = 0
      LO3O1D    = 0
      LO3O3P    = 0
      LACETONE  = 0
      LKETONE   = 0
      LMGLY_ADJ = 0
      LMGLY_ABS = 0

C***get needed photolysis data for the model chemistry from the
C***CSQY_DATA

       ALLOCATE( PHOTOLYSIS_RATE ( NPHOTAB ) )
       ALLOCATE( XXCS( NPHOTAB, IREFTEMPS, NWL ) )
       ALLOCATE( XXQY( NPHOTAB, IREFTEMPS, NWL ) )
       ALLOCATE( RTEMP_S( IREFTEMPS, NPHOTAB ) )

       DO IPHOT = 1, NPHOTAB
          IP_MAP = INDEXR( PHOTAB( IPHOT ), NPHOT_MAP, PNAME_MAP )
          IF ( IP_MAP .LE. 0 ) THEN
             MSG = 'FATAL ERROR: photolysis reaction ' // TRIM( PHOTAB( IPHOT ) )
     &          // ' not found in ' //
     &             'the reference data! '
             ERROR_FLAG = .TRUE.
             CALL M3WARN ( PNAME, 0, 0, MSG )
          END IF
          IP_REF = PHOT_MAP( IP_MAP )
          PHOTOLYSIS_RATE( IPHOT ) = PNAME_MAP( IP_MAP )

C***check to see if this photolysis reaction is a special case that
C***  is referenced in other sections of the code.  if so, then set
C***  the appropriate pointers for later processing

           SELECT CASE ( TRIM( PHOTOLYSIS_RATE( IPHOT ) ) )
              CASE( 'O3O3P', 'O3O3P_SAPRC99', 'O3O3P_06', 'O3_O3P_IUPAC04', 'O3O3P_NASA06' )
                    LO3O3P = IPHOT
              CASE( 'NO2', 'NO2_SAPRC99', 'NO2_06', 'NO2_RACM2' )
                    LNO2 = IPHOT
              CASE( 'O3O1D',  'O3O1D_SAPRC99' , 'O3O1D_06', 'O3_O1D_IUPAC04', 'O3O1D_NASA06' )
                    LO3O1D = IPHOT
              CASE( 'KETONE', 'KET_RACM2' )
                    LKETONE   = IPHOT
              CASE( 'MGLY_ADJ' )
                    LMGLY_ADJ = IPHOT
              CASE(  'MGLY_ABS' )
                    LMGLY_ABS = IPHOT
              CASE( 'ACETONE', 'CH3COCH3_RACM2' )
                    LACETONE  = IPHOT
              CASE( 'HCHOR_06', 'HCHO_RAD_RACM2')
                    LHCHOR_06 = IPHOT
           END SELECT


C***load the local cross section & quantum yield data from the reference
C***  dataset for this photolysis reaction

            DO ITT = 1, IREFTEMPS
               DO IWL = 1, NWL
                  XXCS( IPHOT, ITT, IWL ) = CS_REF( IP_REF, ITT, IWL )
                  XXQY( IPHOT, ITT, IWL ) = QY_REF( IP_REF, ITT, IWL )
                  RTEMP_S( ITT, IPHOT ) = TEMP_REF( ITT, IP_REF )
               END DO   ! iwl
            END DO   ! itt

       END DO   ! iphot

       IF ( LNO2   .EQ. 0 ) THEN
          MSG = 'NO2 cross-section not found in the CSQY data! '
          ERROR_FLAG = .TRUE.
          CALL M3WARN ( PNAME, 0, 0, MSG )
       END IF
       IF ( LO3O1D .EQ. 0 ) THEN
          MSG = 'O3(1D) production not found in the CSQY data! '
          CALL M3WARN ( 'NEW_OPTICS', 0, 0, MSG )
       END IF
       IF ( LO3O3P .EQ. 0 ) THEN
          MSG = 'O3 cross-section not found in the CSQY data! '
          ERROR_FLAG = .TRUE.
          CALL M3WARN ( PNAME, 0, 0, MSG )
       END IF

       IF( ERROR_FLAG )THEN
         MSG = 'The above fatal error(s) found in CSQY data! '
         CALL M3EXIT( PNAME, 0, 0, MSG, -1 )
       END IF

      WRITE( LOG_UNIT,* ) 'Sucessfully Loaded CSQY_DATA file'
      
      CLOSE(LOG_UNIT)
      CLOSE(PHOTAB_UNIT)

5012  FORMAT( 4X,A30,1X,3(F8.3,2X) )
5013  FORMAT( 22X,I3 )
5016  FORMAT( 4X,A60,1X,I3,2X,3(F8.3,2X) )

#ifdef verbose_phot
6009  format( a3,', ',8(a,', ') )
6013  format( a22,1x,i3 )
6016  format( i3,1x,a60,1x,i3,2x,3(f8.3,2x) )
#endif

      RETURN
      END SUBROUTINE LOAD_CSQY_DATA



      SUBROUTINE LOAD_OPTICS_DATA()
!-----------------------------------------------------------------------
!  Purpose: read input file for 
!           -wavelength bin for cross check against
!           -size dependent optical data for liquid droplets and ice 
!            ice particles
!           -landuse type data for surface alebdo
!
!  Revision History:
!   31 Jan 2014 B.Hutzell: Initial Version based on LOAD_REF_DATA in
!   CMAQ version 5.0
!-----------------------------------------------------------------------

!      USE UTILIO_DEFN

      IMPLICIT NONE

!***arguments

!     NONE

!***local

      LOGICAL :: WRITE_LOG = .TRUE.

      CHARACTER(  32 ) :: PNAME         = 'LOAD_OPTICS_DATA'
      CHARACTER(  16 ) :: OPTICS_FILE   =  'OPTICS_DATA'      ! OPTICS_DATA i/o logical name
      CHARACTER(  16 ) :: OPTICS_EXPECT
      CHARACTER(  16 ) :: QUANTITY
      CHARACTER(  30 ) :: LAND_EXPECT
      CHARACTER( 120 ) :: MSG                               ! buffer for messages to output
      CHARACTER( 240 ) :: FILE_LINE

      CHARACTER(  16 ),  ALLOCATABLE :: AE_RERACT_REF( : )

!     INTEGER, INTENT(OUT) :: NWL_OPTICS    ! # of wavelengths used in PHOT_MOD.F
      INTEGER :: NWL_OPTICS    ! # of wavelengths used in PHOT_MOD.F
      INTEGER :: IOST        ! IOST returned from OPEN function
      INTEGER :: JDATE = 0
      INTEGER :: LOG_UNIT
      INTEGER :: OPTICS_UNIT
      INTEGER :: IPHOT, IPHOT_LOAD ! loop indices
      INTEGER :: ITT, ITT_LOAD     ! loop indices 
      INTEGER :: IP_MAP, IP_REF    ! photolysis reaction indicies
      INTEGER :: IWL_LOAD
      INTEGER :: STRT, FINI

      INTEGER :: NAE_REFRACT_REF 
      
      REAL,       ALLOCATABLE :: AE_IMAG_REFRACT( :, : )
      REAL,       ALLOCATABLE :: AE_REAL_REFRACT( :, : )

      LOGICAL                  :: ERROR_FLAG = .FALSE.

C***external functions: none

      LOG_UNIT = INIT3()

      OPTICS_UNIT = GETEFILE( OPTICS_FILE, .TRUE., .TRUE., PNAME )


      READ( OPTICS_UNIT,'(A)' ) FILE_LINE
      
#ifdef verbose_phot
      write( log_unit, * )TRIM(FILE_LINE)
#endif

      READ( FILE_LINE, 4999) NWL_REF
      
      NWL_OPTICS = NWL_REF

      DO ITT_LOAD = 1, 14 ! skip next 14 lines
         READ( OPTICS_UNIT,'(A)' ) FILE_LINE
      END DO

      DO IWL_LOAD = 1, NWL_REF
         READ( OPTICS_UNIT, * )iphot_load,
     &         STWL_REF( IWL_LOAD ), EFFWL_REF( IWL_LOAD ),
     &         ENDWL_REF( IWL_LOAD ) 
     
#ifdef verbose_phot
         write( log_unit, 99946 )
     &          stwl_ref( iwl_load ), effwl_ref( iwl_load ),
     &          endwl_ref( iwl_load )
#endif

      END DO
      
      DO ITT_LOAD = 1, 6 ! skip next 6 lines
         READ( OPTICS_UNIT,'(A)' ) FILE_LINE
      END DO

      READ( FILE_LINE, 4999)NAE_REFRACT_REF


      IF( NAE_REFRACT_REF .NE. NUM_REFRACTIVE )THEN
         WRITE( LOG_UNIT,* ) 'NAE_REFRACT_REF  = ', NAE_REFRACT_REF
         MSG = 'NAERO_REFRACT used in ' // OPTICS_FILE
     &       // ' does not equal NUM_REFRACTIVE in CSQY_DATA.F file. '
         CALL M3WARN( PNAME, 0, 0, MSG )
         ERROR_FLAG = .TRUE.
      END IF
      
      ALLOCATE( AE_RERACT_REF   ( NAE_REFRACT_REF ) )

      READ( OPTICS_UNIT,'(A)' ) FILE_LINE
                
#ifdef verbose_phot
      write( log_unit, '(a)')TRIM(FILE_LINE)
#endif

      STRT = SCAN(FILE_LINE, '=', BACK = .TRUE.) + 1
      FINI =  LEN(FILE_LINE)

      READ( FILE_LINE( STRT:FINI ), * )( AE_RERACT_REF( ITT_LOAD ), 
     &                                   ITT_LOAD = 1, NAE_REFRACT_REF )

#ifdef verbose_phot
      write( log_unit, 99947)'REFRACTIVE_INDICES'
      write( log_unit, 99948 )(AE_RERACT_REF( ITT_LOAD ),ITT_LOAD = 1, 
     &                                 NAE_REFRACT_REF )
#endif

      DO ITT_LOAD = 1, NAE_REFRACT_REF 
! set up refractive indices used by aero_photdata routine

          REFRACTIVE_INDEX( ITT_LOAD )%NAME = AE_RERACT_REF( ITT_LOAD )
          ALLOCATE( REFRACTIVE_INDEX( ITT_LOAD )%REAL_PART( N_MODE, NWL_REF ) )         
          ALLOCATE( REFRACTIVE_INDEX( ITT_LOAD )%IMAG_PART( N_MODE, NWL_REF )  ) 
           
#ifdef verbose_phot
          write( log_unit, '(i3, 1x, a16)')itt_load, refractive_index( itt_load )%name
#endif

      END DO

      DO IWL_LOAD = 1, NWL_REF
         READ( OPTICS_UNIT, * )iphot_load,
     &         STWL_REF( IWL_LOAD ), EFFWL_REF( IWL_LOAD ),
     &         ENDWL_REF( IWL_LOAD ), FSOLAR_REF( IWL_LOAD ),
     &         ( AE_REAL_REFRACT( ITT_LOAD, IWL_LOAD ), 
     &           AE_IMAG_REFRACT( ITT_LOAD, IWL_LOAD ),
     &           ITT_LOAD = 1, NAE_REFRACT_REF )

               DO ITT_LOAD = 1, NAE_REFRACT_REF
                  REFRACTIVE_INDEX( ITT_LOAD )%REAL_PART( 1:N_MODE, IWL_LOAD ) 
     &                                      = AE_REAL_REFRACT( ITT_LOAD, IWL_LOAD )
                  REFRACTIVE_INDEX( ITT_LOAD )%IMAG_PART( 1:N_MODE, IWL_LOAD ) 
     &                                      = AE_IMAG_REFRACT( ITT_LOAD, IWL_LOAD )
               END DO
#ifdef verbose_phot
         write( log_unit, 99949 )
     &          stwl_ref( iwl_load ), effwl_ref( iwl_load ),
     &          endwl_ref( iwl_load ),fsolar_ref( iwl_load ),
     &          ( ae_real_refract( itt_load, iwl_load ),
     &            ae_imag_refract( itt_load, iwl_load ), itt_load = 1, nae_refract_ref )
#endif

      END DO

      DO ITT_LOAD = 1, 6 ! skip next 6 lines
         READ( OPTICS_UNIT,'(A)' ) FILE_LINE
      END DO

! read optical data for liquid droplets

      READ( FILE_LINE, 4999)NRADIUS_LIQUID

      READ( OPTICS_UNIT,'(A)' ) FILE_LINE

      ALLOCATE(RADIUS_LIQUID( NRADIUS_LIQUID ))
      
      ALLOCATE( LIQUID_EXTINCT(NRADIUS_LIQUID, NWL_OPTICS),
     &         LIQUID_ASYMFACT(NRADIUS_LIQUID, NWL_OPTICS),
     &         LIQUID_COALBEDO(NRADIUS_LIQUID, NWL_OPTICS))

      READ( OPTICS_UNIT,'(A)' ) FILE_LINE

      QUANTITY = 'LIQ_EXT'
      
      DO ITT_LOAD = 1, NRADIUS_LIQUID
         READ( OPTICS_UNIT, 99950 )
     &         OPTICS_EXPECT, RADIUS_LIQUID( ITT_LOAD ),
     &         ( LIQUID_EXTINCT( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )

#ifdef verbose_phot
          write( log_unit, 99950 )
     &         OPTICS_EXPECT, RADIUS_LIQUID( ITT_LOAD ),
     &         ( LIQUID_EXTINCT( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )
#endif
            IF ( TRIM( OPTICS_EXPECT ) .NE. TRIM( QUANTITY ) ) THEN
               MSG =  'Optical quantity read ' // TRIM( OPTICS_EXPECT )
     &             // ' does match expected quantity, ' // TRIM( QUANTITY )
               CALL M3WARN( PNAME, 0, 0, MSG )
               ERROR_FLAG = .TRUE.
            END IF
      END DO

      READ( OPTICS_UNIT,'(A)' ) FILE_LINE

      QUANTITY = 'LIQ_ASY'
      
      DO ITT_LOAD = 1, NRADIUS_LIQUID
         READ( OPTICS_UNIT, 99950 )
     &         OPTICS_EXPECT, RADIUS_LIQUID( ITT_LOAD ),
     &         ( LIQUID_ASYMFACT( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )

#ifdef verbose_phot
          write( log_unit, 99950 )
     &         OPTICS_EXPECT, RADIUS_LIQUID( ITT_LOAD ),
     &         ( LIQUID_ASYMFACT( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )
#endif
            IF ( TRIM( OPTICS_EXPECT ) .NE. TRIM( QUANTITY ) ) THEN
               MSG =  'Optical quantity read ' // TRIM( OPTICS_EXPECT )
     &             // ' does match expected quantity, ' // TRIM( QUANTITY )
               CALL M3WARN( PNAME, 0, 0, MSG )
               ERROR_FLAG = .TRUE.
            END IF
      END DO

      READ( OPTICS_UNIT,'(A)' ) FILE_LINE

      QUANTITY = 'LIQ_COA'
      
      DO ITT_LOAD = 1, NRADIUS_LIQUID
         READ( OPTICS_UNIT, 99950 )
     &         OPTICS_EXPECT, RADIUS_LIQUID( ITT_LOAD ),
     &         ( LIQUID_COALBEDO( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )

#ifdef verbose_phot
          write( log_unit, 99950 )
     &         OPTICS_EXPECT, RADIUS_LIQUID( ITT_LOAD ),
     &         ( LIQUID_COALBEDO( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )
#endif
            IF ( TRIM( OPTICS_EXPECT ) .NE. TRIM( QUANTITY ) ) THEN
               MSG =  'Optical quantity read ' // TRIM( OPTICS_EXPECT )
     &             // ' does match expected quantity, ' // TRIM( QUANTITY )
               CALL M3WARN( PNAME, 0, 0, MSG )
               ERROR_FLAG = .TRUE.
            END IF
      END DO


      DO ITT_LOAD = 1, 6 ! skip next 6 lines
         READ( OPTICS_UNIT,'(A)' ) FILE_LINE
      END DO

! read optical data for liquid droplets

      READ( FILE_LINE, 4999)NDIAMETER_ICE

      READ( OPTICS_UNIT,'(A)' ) FILE_LINE

      ALLOCATE(DIAMETER_ICE( NDIAMETER_ICE ))
      
      ALLOCATE( ICE_EXTINCT(NDIAMETER_ICE, NWL_OPTICS),
     &         ICE_ASYMFACT(NDIAMETER_ICE, NWL_OPTICS),
     &         ICE_COALBEDO(NDIAMETER_ICE, NWL_OPTICS))

      READ( OPTICS_UNIT,'(A)' ) FILE_LINE

      QUANTITY = 'ICE_EXT'
      
      DO ITT_LOAD = 1, NDIAMETER_ICE
         READ( OPTICS_UNIT, 99950 )
     &         OPTICS_EXPECT, DIAMETER_ICE( ITT_LOAD ),
     &         ( ICE_EXTINCT( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )

#ifdef verbose_phot
          write( log_unit, 99950 )
     &         OPTICS_EXPECT, DIAMETER_ICE( ITT_LOAD ),
     &         ( ICE_EXTINCT( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )
#endif
            IF ( TRIM( OPTICS_EXPECT ) .NE. TRIM( QUANTITY ) ) THEN
               MSG =  'Optical quantity read ' // TRIM( OPTICS_EXPECT )
     &             // ' does match expected quantity, ' // TRIM( QUANTITY )
               CALL M3WARN( PNAME, 0, 0, MSG )
               ERROR_FLAG = .TRUE.
            END IF
      END DO
 
      READ( OPTICS_UNIT,'(A)' ) FILE_LINE

      QUANTITY = 'ICE_ASY'
      
      DO ITT_LOAD = 1, NDIAMETER_ICE
         READ( OPTICS_UNIT, 99950 )
     &         OPTICS_EXPECT, DIAMETER_ICE( ITT_LOAD ),
     &         ( ICE_ASYMFACT( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )

#ifdef verbose_phot
          write( log_unit, 99950 )
     &         OPTICS_EXPECT, DIAMETER_ICE( ITT_LOAD ),
     &         ( ICE_ASYMFACT( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )
#endif
            IF ( TRIM( OPTICS_EXPECT ) .NE. TRIM( QUANTITY ) ) THEN
               MSG =  'Optical quantity read ' // TRIM( OPTICS_EXPECT )
     &             // ' does match expected quantity, ' // TRIM( QUANTITY )
               CALL M3WARN( PNAME, 0, 0, MSG )
               ERROR_FLAG = .TRUE.
            END IF
      END DO

      READ( OPTICS_UNIT,'(A)' ) FILE_LINE

      QUANTITY = 'ICE_COA'
      
      DO ITT_LOAD = 1, NDIAMETER_ICE
         READ( OPTICS_UNIT, 99950 )
     &         OPTICS_EXPECT, DIAMETER_ICE( ITT_LOAD ),
     &         ( ICE_COALBEDO( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )

#ifdef verbose_phot
          write( log_unit, 99950 )
     &         OPTICS_EXPECT, DIAMETER_ICE( ITT_LOAD ),
     &         ( ICE_COALBEDO( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )
#endif
            IF ( TRIM( OPTICS_EXPECT ) .NE. TRIM( QUANTITY ) ) THEN
               MSG =  'Optical quantity read ' // TRIM( OPTICS_EXPECT )
     &             // ' does match expected quantity, ' // TRIM( QUANTITY )
               CALL M3WARN( PNAME, 0, 0, MSG )
               ERROR_FLAG = .TRUE.
            END IF
      END DO

!  read data for calculating surface     

      DO ITT_LOAD = 1, 5 ! skip next 5 lines
         READ( OPTICS_UNIT,'(A)' ) FILE_LINE
      END DO

      READ( OPTICS_UNIT,5013 ) NUMB_LANDUSE_REF

      DO ITT_LOAD = 1, 3 ! skip next 3 lines
         READ( OPTICS_UNIT,'(A)' ) FILE_LINE
      END DO

      READ( OPTICS_UNIT,5013 ) INDEX_GRASSLAND_REF
      READ( OPTICS_UNIT,5013 ) INDEX_OCEAN_REF
      READ( OPTICS_UNIT,5013 ) INDEX_SEA_ICE

#ifdef verbose_phot
      write( log_unit,6013 )'NUMB_LANDUSE_REF    = ', numb_landuse_ref
      write( log_unit,6013 )'INDEX_GRASSLAND_REF = ', index_grassland_ref
      write( log_unit,6013 )'INDEX_OCEAN_REF     = ', index_ocean_ref
      write( log_unit,6013 )'INDEX_SEA_ICE       = ', index_sea_ice
#endif

      ALLOCATE( LANDUSE_REF     ( NUMB_LANDUSE_REF ) )
      ALLOCATE( ZENITH_COEFF_REF( NUMB_LANDUSE_REF ) )
      ALLOCATE( SEASON_COEFF_REF( NUMB_LANDUSE_REF ) )
      ALLOCATE( SNOW_COEFF_REF  ( NUMB_LANDUSE_REF ) )
      ALLOCATE( SPECTRAL_ALBEDO_REF( NWL_OPTICS, NUMB_LANDUSE_REF ) )

      READ( OPTICS_UNIT,'(A)' ) FILE_LINE ! skip line

      DO ITT_LOAD = 1, NUMB_LANDUSE_REF
         READ( OPTICS_UNIT,5012 ) LANDUSE_REF( ITT_LOAD ),
     &                            ZENITH_COEFF_REF( ITT_LOAD ),
     &                            SEASON_COEFF_REF( ITT_LOAD ),
     &                            SNOW_COEFF_REF( ITT_LOAD )
#ifdef verbose_phot
         write( log_unit,5012 ) landuse_ref( itt_load ),
     &                          zenith_coeff_ref( itt_load ),
     &                          season_coeff_ref( itt_load ),
     &                          snow_coeff_ref( itt_load )
#endif
      END DO

      READ( OPTICS_UNIT,'(A)' ) FILE_LINE ! skip line

      DO ITT_LOAD = 1, NUMB_LANDUSE_REF
         READ( OPTICS_UNIT,'(A30,1X,40(1PE12.6,2X))' ) LAND_EXPECT,
     &        ( SPECTRAL_ALBEDO_REF(IWL_LOAD, ITT_LOAD), IWL_LOAD = 1, NWL_REF )

#ifdef verbose_phot
         write( log_unit,'(a30,1x,40(1pe12.6,2x))' ) trim( land_expect ),
     &        ( spectral_albedo_ref(iwl_load, itt_load), iwl_load = 1, nwl_ref )
#endif

      END DO

      DO ITT_LOAD = 1, 3 ! skip next 3 lines
         READ( OPTICS_UNIT,'(A)' ) FILE_LINE
      END DO

      READ( OPTICS_UNIT,5013 ) NUMB_LANDUSE_NLCD50
      READ( OPTICS_UNIT,'(A)' ) FILE_LINE ! skip line

#ifdef verbose_phot
      write( log_unit,6013 ) 'NUMB_NLCD50_MODIS = ', numb_landuse_NLCD50
      write( log_unit,6009 ) '! I', 'LANDUSE_NLCD50-MODIS', 'INDEX_ALBREF',
     &                       'FAC_ALBREF'
#endif

      ALLOCATE( LANDUSE_NLCD50( NUMB_LANDUSE_NLCD50 )  )
      ALLOCATE( ALBMAP_REF2NLCD50( NUMB_LANDUSE_NLCD50 )  )
      ALLOCATE( ALBFAC_REF2NLCD50( NUMB_LANDUSE_NLCD50 )  )

      DO ITT_LOAD = 1, NUMB_LANDUSE_NLCD50
         READ( OPTICS_UNIT,5016 ) LANDUSE_NLCD50( ITT_LOAD ),
     &                            ALBMAP_REF2NLCD50( ITT_LOAD ),
     &                            ALBFAC_REF2NLCD50( ITT_LOAD )

#ifdef verbose_phot
         write( log_unit,6016 ) itt_load, landuse_NLCD50( itt_load ),
     &                          albmap_ref2NLCD50( itt_load ),
     &                          albfac_ref2NLCD50( itt_load )
#endif

      END DO

      READ( OPTICS_UNIT,5013 ) NUMB_LANDUSE_USGS
      READ( OPTICS_UNIT,'(A)' ) FILE_LINE ! skip line

#ifdef verbose_phot
      write( log_unit,6013 ) 'NUMB_USGS = ', numb_landuse_usgs
      write( log_unit,6009 ) '! I','LANDUSE_USGS', 'INDEX_ALBREF', 'FAC_ALBREF'
#endif

      ALLOCATE( LANDUSE_USGS   ( NUMB_LANDUSE_USGS ) )
      ALLOCATE( ALBMAP_REF2USGS( NUMB_LANDUSE_USGS ) )
      ALLOCATE( ALBFAC_REF2USGS( NUMB_LANDUSE_USGS ) )

      DO ITT_LOAD = 1, NUMB_LANDUSE_USGS
         READ( OPTICS_UNIT,5016 ) LANDUSE_USGS( ITT_LOAD ),
     &                            ALBMAP_REF2USGS( ITT_LOAD ),
     &                            ALBFAC_REF2USGS( ITT_LOAD )

#ifdef verbose_phot
         write( log_unit,6016 ) itt_load, landuse_usgs( itt_load ),
     &                          albmap_ref2usgs( itt_load ),
     &                          albfac_ref2usgs( itt_load )
#endif

      END DO

      READ( OPTICS_UNIT,5013 ) NUMB_LANDUSE_MODIS
      READ( OPTICS_UNIT,'(A)' ) FILE_LINE ! skip line

#ifdef verbose_phot
      write( log_unit,6013 ) 'NUMB_MODIS = ', numb_landuse_modis
      write( log_unit,6009 ) '! I','LANDUSE_MODIS', 'INDEX_ALBREF', 'FAC_ALBREF'
#endif

      ALLOCATE( LANDUSE_MODIS   ( NUMB_LANDUSE_MODIS ) )
      ALLOCATE( ALBMAP_REF2MODIS( NUMB_LANDUSE_MODIS ) )
      ALLOCATE( ALBFAC_REF2MODIS( NUMB_LANDUSE_MODIS ) )

      DO ITT_LOAD = 1, NUMB_LANDUSE_MODIS
         READ( OPTICS_UNIT,5016 ) LANDUSE_MODIS( ITT_LOAD ),
     &                            ALBMAP_REF2MODIS( ITT_LOAD ),
     &                            ALBFAC_REF2MODIS( ITT_LOAD )

#ifdef verbose_phot
         write( log_unit,6016 ) itt_load, landuse_modis( itt_load ),
     &                          albmap_ref2modis( itt_load ),
     &                          albfac_ref2modis( itt_load )
#endif

      END DO

      NO_NLCD40 = .TRUE.  ! default condition that file does not contain NLCD40 Landuse data
      
      READ( OPTICS_UNIT,5013, END = 101 ) NUMB_LANDUSE_NLCD40
      READ( OPTICS_UNIT,'(A)' ) FILE_LINE ! skip line

#ifdef verbose_phot
      write( log_unit,6013 ) 'NUMB_NLCD40_MODIS = ', numb_landuse_NLCD40
      write( log_unit,6009 ) '! I', 'LANDUSE_NLCD40-MODIS', 'INDEX_ALBREF',
     &                       'FAC_ALBREF'
#endif

      ALLOCATE( LANDUSE_NLCD40( NUMB_LANDUSE_NLCD40 )  )
      ALLOCATE( ALBMAP_REF2NLCD40( NUMB_LANDUSE_NLCD40 )  )
      ALLOCATE( ALBFAC_REF2NLCD40( NUMB_LANDUSE_NLCD40 )  )

      DO ITT_LOAD = 1, NUMB_LANDUSE_NLCD40
         READ( OPTICS_UNIT,5016 ) LANDUSE_NLCD40( ITT_LOAD ),
     &                            ALBMAP_REF2NLCD40( ITT_LOAD ),
     &                            ALBFAC_REF2NLCD40( ITT_LOAD )

#ifdef verbose_phot
         write( log_unit,6016 ) itt_load, landuse_NLCD40( itt_load ),
     &                          albmap_ref2NLCD40( itt_load ),
     &                          albfac_ref2NLCD40( itt_load )
#endif

      END DO
      
      NO_NLCD40 = .FALSE.

101   IF( NO_NLCD40 )THEN
          MSG = TRIM( PNAME ) // ':'
     &       // TRIM( OPTICS_FILE )
     &       // ' does not contain data for NLCD40 land use and'
     &       // ' corresponds to CMAQ version 5.01.'
          CALL M3MESG( MSG )
      END IF

! set the default values for surface albedo

      DO IWL_LOAD = 1, NWL_OPTICS
         IF ( WAVELENGTH( IWL_LOAD ) .LE. 380.1 ) THEN
            ALB( IWL_LOAD ) = 0.05
         ELSE
            ALB( IWL_LOAD ) = 0.10
         END IF
      END DO


      WRITE( LOG_UNIT,* ) 'Sucessfully Loaded OPTICS_DATA file'
            
      CLOSE(LOG_UNIT)
      CLOSE(OPTICS_UNIT)

4999  FORMAT(17X,I3,2X,17X,I3)
5012  FORMAT( 4X,A30,1X,3(F8.3,2X) )
5013  FORMAT( 22X,I3 )
5016  FORMAT( 4X,A60,1X,I3,2X,3(F8.3,2X) )
99946 FORMAT(4x,3(f8.3,2x),2x,2(es12.4,2x),f8.3,2x,12(es12.4,2x))
99947 FORMAT(a3, 1x, a16)
99948 FORMAT(10(a16,1x))
99949 FORMAT(4x,3(f8.3,2x),2x,2(es12.4,2x),f8.3,2x,12(es12.4,2x))
99950 FORMAT(a8,1x,f10.3,1x,40(1pe12.6,2x))

#ifdef verbose_phot
6009  format( a3,', ',8(a,', ') )
6013  format( a22,1x,i3 )
6016  format( i3,1x,a60,1x,i3,2x,3(f8.3,2x) )
#endif

      RETURN
      END SUBROUTINE LOAD_OPTICS_DATA

            
      END MODULE CHECK_CSQY_DATA
