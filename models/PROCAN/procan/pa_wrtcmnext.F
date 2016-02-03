
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
C $Header: /project/yoj/arc/PROCAN/src/driver/pa/pa_wrtcmnext.F,v 1.9 2011/11/02 14:34:00 yoj Exp $

      SUBROUTINE PA_WRTCMNEXT

C***********************************************************************
 
C Function: Write the Models-3 Process Analysis Commons Include File 
 
C Preconditions: Requires mechanism to have been processed
  
C Key Subroutines/Functions Called: None
 
C Revision History:
C  Prototype created by Jerry Gipson, August, 1996
C  Modified May, 1997 by Jerry Gipson to be consistent with beta CTM
C  Modified Sept, 1997 by Jerry Gipson to be consistent with targeted CTM
C  Updated May 2002 by Jerry Gipson for dynamic allocation version of CCTM
C  Modified Jan, 2006 by J. Young: NPROCS -> NPRCS to avoid CCTM name conflict
C  with multiprocessing variable I couldn't resist the temptation to F90-ize
C  the output
C  Aug 2011 Jeff Young: Replaced I/O API include files with IOAPI's M3UTILIO
 
C***********************************************************************
      USE M3UTILIO      ! IOAPI module

      IMPLICIT NONE
      
C Includes:
      INCLUDE 'PA_IPRDEF.EXT'
      INCLUDE 'PA_GLOBAL.EXT'
      INCLUDE 'PA_PARSE.EXT'
      INCLUDE 'PA_VARS.EXT'
      INCLUDE 'PA_INCDATA.EXT'
      
C Arguments: None
                                        
C Parameters: None

C External Functions: None

C Local Variables:
      CHARACTER(  1 ) :: DOT = '.'  ! Dot character
      CHARACTER( 80 ) :: DELIM      ! Delimiter line output
      CHARACTER( 80 ) :: CEES       ! String of cees
      CHARACTER( 78 ) :: LINOUT     ! Output line

      INTEGER IOUT             ! Unit number for include file
      INTEGER :: LINLEN = 78   ! Length of output line
      INTEGER MXCGRIDD         ! Dimension for number of familys for PA
      INTEGER MXNIRRRXNSD      ! Dimension parameter for MXNIRRRXNS
      INTEGER MXTEMPTRMSD      ! Dimension parameter for MXTEMPTRMS
      INTEGER N                ! Loop index
      INTEGER NCSAVED          ! Dimension for NCSAVE
      INTEGER NIRRVARD         ! Dimension of IRR output variables
      INTEGER N_IPR_SPCD       ! Dimension statement for no. of PA species
      INTEGER NIPRVARD         ! Dimension of PA output variables
      INTEGER NUMTEMPSD        ! Dimension for number of temp IRRs
      INTEGER NUMOUTPOSD       ! Dimension parameter for NUMOUTPOS
      INTEGER NUMOUTNEGD       ! Dimension parameter for NUMOUTNEG
      INTEGER NUMOUTINDD       ! Dimension parameter for NUMOUTIND
      INTEGER PACOLS           ! Number of output columns
      INTEGER PAROWS           ! Number of output rows
      INTEGER PALEVS           ! Number of output levels

      LOGICAL LPROC            ! Flag for processes to be monitoring
      LOGICAL LIRR             ! Flag for IRR computations
         
C***********************************************************************

      DO N = 1, 78
         DELIM( N:N ) = DOT
         CEES(  N:N ) = 'c'
      END DO     
      DELIM( 1:1 ) = 'C'

      IF ( LFULLIRR ) THEN
         LIRR    = .TRUE.
         NIRRVAR = NRXNS
      ELSE IF ( LPARTIRR ) THEN
         LIRR    = .TRUE.
         NIRRVAR = NIRROUT
      ELSE
         LIRR    = .FALSE.     
      END IF

      IF ( N_IPR_SPC .GT. 0 ) THEN
         LPROC = .TRUE.
      ELSE
         LPROC = .FALSE.
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Open and Control Include file and write the header data
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IOUT = JUNIT( )
      OPEN( UNIT = IOUT,  FILE = 'PA_CMN.EXT' )
      WRITE( IOUT, 94000 ) DELIM( 1:72 )
      LINOUT = 'C    INCLUDE FILE   PA_CMN.EXT'
      WRITE( IOUT, 94000 ) LINOUT
      WRITE( IOUT, 94000 ) LINOUT( 1:1 )
      LINOUT = 'C    CONTAINS: Common variables for Process Analysis'
      WRITE( IOUT, 94000 ) LINOUT
      WRITE( IOUT, 94000 ) LINOUT( 1:1 )
      LINOUT = 'C    DEPENDENT UPON: Mechanism Input file'
      WRITE( IOUT, 94000 ) LINOUT
      WRITE( IOUT, 94000 ) LINOUT( 1:1 )
      LINOUT = 'C    CREATED BY: Process Analysis Control Program'
      WRITE( IOUT, 94000 ) LINOUT
      LINOUT = 'C                ' // TDATOUT
      WRITE( IOUT, 94000 ) LINOUT
      WRITE( IOUT, 94000 ) LINOUT( 1:1 )
      WRITE( IOUT, 94000 ) DELIM( 1:72 )
      WRITE( IOUT, 94000 ) 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write messages about array dimensioning if needed
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( .NOT. LIRR ) THEN
         WRITE( IOUT, 94000 ) CEES( 1:72 )
         WRITE( IOUT, 94020 )
         WRITE( IOUT, 94000 ) CEES( 1:72 )
         WRITE( IOUT, 94000 )
         wriTE( IOUT, 94000 )
      END IF

      IF ( .NOT. LPROC ) THEN
         WRITE( IOUT, 94000 ) CEES( 1:72 )
         WRITE( IOUT, 94040 )
         WRITE( IOUT, 94000 ) CEES( 1:72 )
         WRITE( IOUT, 94000 ) 
         WRITE( IOUT, 94000 ) 
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write output file variables 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      WRITE( IOUT, 94000 ) CEES( 1:72 )
      WRITE( IOUT, 94120 ) 
      WRITE( IOUT, 94000 ) CEES( 1:72 )
      WRITE( IOUT, 94140 ) 
      WRITE( IOUT, 94000 ) CEES( 1:72 )
      NIPRVARD  = MAX( NIPRVAR, 1 )
      NIRRVARD  = MAX( NIRRVAR, 1 )
      WRITE( IOUT, 94160 ) NIPRVAR, NIPRVARD
      WRITE( IOUT, 94180 ) NIRRVAR, NIRRVARD
      WRITE( IOUT, 94200 )
 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write integrated process rate variables 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      WRITE( IOUT, 94000 ) CEES( 1:72 )
      WRITE( IOUT, 94260 ) 
      WRITE( IOUT, 94000 ) CEES( 1:72 )
      WRITE( IOUT, 94280 ) 
      WRITE( IOUT, 94000 ) CEES( 1:72 )

      N_IPR_SPCD  = MAX( N_IPR_SPC, 1 )
      NCSAVED     = MAX( NCSAVE, 1 )
      MXCGRIDD    = MAX( MXCGRID, 1 )

      WRITE( IOUT, 94300 ) NPROCS,
     &                     N_IPR_SPC, N_IPR_SPCD,
     &                     NCSAVE, NCSAVED,
     &                     MXCGRID, MXCGRIDD
      WRITE( IOUT, 94320 )
      WRITE( IOUT, 94322 )
 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write integrated reaction rate variables 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      WRITE( IOUT, 94000 ) CEES( 1:72 )
      WRITE( IOUT, 94360 ) 
      WRITE( IOUT, 94000 ) CEES( 1:72 )
      WRITE( IOUT, 94380 ) 
      WRITE( IOUT, 94000 ) CEES( 1:72 )

      IF ( LFULLIRR ) THEN
         WRITE( IOUT, 94420 )
      ELSE
         WRITE( IOUT, 94440 )
      END IF

      IF ( LPARTIRR ) THEN
         WRITE( IOUT, 94480 )
      ELSE
         WRITE( IOUT, 94500 )
      END IF

      IF ( .NOT. LPARTIRR ) THEN
         NUMTEMPS    = 0
         NUMOUTPOS   = 0
         NUMOUTNEG   = 0
         NUMOUTIND   = 0
         MXNIRRRXNS  = 0
         MXTEMPTRMS  = 0
      END IF

      NUMTEMPSD    = MAX(  NUMTEMPS,   1 )
      NUMOUTPOSD   = MAX(  NUMOUTPOS,  1 )
      NUMOUTNEGD   = MAX(  NUMOUTNEG,  1 )
      NUMOUTINDD   = MAX(  NUMOUTIND,  1 )
      MXNIRRRXNSD  = MAX(  MXNIRRRXNS, 1 )
      MXTEMPTRMSD  = MAX(  MXTEMPTRMS, 1 )

      WRITE( IOUT, 94520 ) NUMTEMPS, NUMTEMPSD,
     &                     NUMOUTPOS, NUMOUTPOSD,
     &                     NUMOUTNEG, NUMOUTNEGD,
     &                     NUMOUTIND, NUMOUTINDD,
     &                     MXNIRRRXNS, MXNIRRRXNSD,
     &                     MXTEMPTRMS, MXTEMPTRMSD

      IF ( NIRRVAR .NE. 0 ) THEN
         WRITE( IOUT, 94542 )
      ELSE 
         WRITE( IOUT, 94544 )
      END IF

      WRITE( IOUT, 94580 )
      WRITE( IOUT, 94322)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write common blocks
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      WRITE( IOUT, 94000 ) CEES( 1:72 )
      WRITE( IOUT, 94700 ) 
      WRITE( IOUT, 94000 ) CEES( 1:72 )
      WRITE( IOUT, 94322 ) 
      WRITE( IOUT, 94720 ) 
      WRITE( IOUT, 94740 ) 
      WRITE( IOUT, 94760 )
      WRITE( IOUT, 94781 )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write monocode extensions (declarations and common block) and end
c  line and return
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LINOUT = DELIM
      LINOUT( 14 : 58 ) = ' End of Process Analysis COMMON Include File '
      WRITE( IOUT, 94000 ) LINOUT
      CLOSE( IOUT )
      RETURN

C*********************** FORMAT Statements *****************************

94000 FORMAT( A )

94020 FORMAT( 
     & 'c  Integrated Reaction Rates are not being computed.  All',
     &     ' associated'/
     & 'c  arrays will be set to the smallest possible dimension.' )

94040 FORMAT( 
     & 'c  Integrated Process Rates are not being computed.  All',
     &     ' associated'/
     & 'c  arrays will be set to the smallest possible dimension.' )

94120 FORMAT( 
     &'c  The following variables are used in the output file ', 
     &      'descriptions' )
94140 FORMAT( 
     &'c     NIPRVAR   = Number of IPR output variables'/
     &'c     NIRRVAR   = Number of IRR output variables'/
     &'c     NIPRVARD  = Dimension for IPR output variables'/
     &'c     NIRRVARD  = Dimension for IRR output variables'/
     &'c     IPRNAME   = Name of IPR output variable'/
     &'c     IRRNAME   = Name of IRR output variable'/
     &'c     IPRDESC   = Extended name of IPR output variable'/
     &'c     IRRDESC   = Extended name of IRR output variable' )

94160 FORMAT( /
     & 6X, 'INTEGER, PARAMETER :: NIPRVAR =', I4, ',',
     & 2X,                       'NIPRVARD =', I4 )

94180 FORMAT( /
     & 6X, 'INTEGER, PARAMETER :: NIRRVAR =', I4, ',',
     & 2X,                       'NIRRVARD =', I4 / )

94200 FORMAT( 
     & 6X, 'CHARACTER( 16 ) :: IPRNAME( NIPRVARD )' /
     & 6X, 'CHARACTER( 60 ) :: IPRDESC( NIPRVARD )' /
     & 6X, 'CHARACTER( 16 ) :: IRRNAME( NIRRVARD )' /
     & 6X, 'CHARACTER( 60 ) :: IRRDESC( NIRRVARD )' / )

94260 FORMAT( 
     & 'c  The following variables are used in the integrated process', 
     &     ' rate '/'c  computations.' )

94280 FORMAT( 
     & 'c    NPRCS      = No. of science processes that can be monitored' /
     & 'c    N_IPR_SPC  = Number of families for IPR' /
     & 'c    NCSAVE     = Number of CTM species concs to save for IPR' /
     & 'c    MXCGRID    = Max number of CTM species in an IPR family' /
     & 'c    N_IPR_SPCD = Dimension for number of families for IPR' /
     & 'c    NCSAVED    = Dimension for NCSAVE' /
     & 'c    MXCGRIDD   = Dimension for MXCGRID' /
     & 'c    NCGRID     = Array of no. of CTM species in an IPR family' /
     & 'c    IPROUT     = Array of IPR output variable indices' /
     & 'c    IPR2GRD    = Array of CTM species indices for IPR families' /
     & 'c    IPR2SAV    = Array of saved species indices for IPR families' /
     & 'c    SV2GRD     = Array of CTM indicies for saved concs' /
     & 'c    SPCOEF     = Array of coefficients for IPR familys' ) 

94300 FORMAT( /
     & 6X, 'INTEGER, PARAMETER :: NPRCS      =', I4 /
     & 6X, 'INTEGER, PARAMETER :: N_IPR_SPC  =', I4, ',',
     & 2X,                       'N_IPR_SPCD =', I4 /
     & 6X, 'INTEGER, PARAMETER :: NCSAVE     =', I4, ',',
     & 2X,                       'NCSAVED    =', I4 /
     & 6X, 'INTEGER, PARAMETER :: MXCGRID    =', I4, ',',
     & 2X,                       'MXCGRIDD   =', I4 / )

94320 FORMAT( 
     & 6X, 'CHARACTER( 16 ) :: PROCNAME( NPRCS )' /
     & 6X, 'LOGICAL            LPROCOUT( NPRCS )' //
     & 6X, 'INTEGER NCGRID ( N_IPR_SPCD )' /
     & 6X, 'INTEGER SV2GRD ( NCSAVED )' /
     & 6X, 'INTEGER IPROUT ( N_IPR_SPCD,NPRCS )' /
     & 6X, 'INTEGER IPR2GRD( N_IPR_SPCD,MXCGRIDD )' /
     & 6X, 'INTEGER IPR2SAV( N_IPR_SPCD,MXCGRIDD )' //
     & 6X, 'REAL    SPCOEF ( N_IPR_SPCD,MXCGRIDD )' )

94321 FORMAT(
     & 6X, 'REAL DELC( PACOLSD,PAROWSD,PALEVSD,NIPRVARD )' /
     & 6X, 'REAL CSAV( PACOLSD,PAROWSD,PALEVSD,NCSAVED )' // )

94322 FORMAT( / )

94360 FORMAT( 
     & 'c  The following variables are used in the integrated reaction', 
     &     ' rate' / 'c  computations.' )

94380 FORMAT( 
     & 'c    LFULLIRR     = Flag for computing IRRs for every reaction' /
     & 'c    LPARTIRR     = Flag for computing IRRs for selected', 
     &                    ' reactions' /
     & 'c    NUMTEMPS     = No. of temporary intermediate IRRs to', 
     &                    ' compute' /
     & 'c    NUMOUTPOS    = No. of temp. IRR terms to add iff the', 
     &                   ' IRR > 0' /
     & 'c    NUMOUTNEG    = No. of temp. IRR terms to add iff the', 
     &                   ' IRR < 0' /
     & 'c    NUMOUTIND    = No. of temp. IRR terms to add regardless', 
     &                   ' of sign' /
     & 'c    MXNIRRRXNS   = Max no. of reaction terms in an output IRR' /
     & 'c    MXTEMPTRMS   = Max no. of reaction terms in a temp. IRR' /
     & 'c    NUMTEMPSD    = Dimension for number of temp IRRs' /
     & 'c    NUMOUTPOSD   = Dimension parameter for NUMOUTPOS' /
     & 'c    NUMOUTNEGD   = Dimension parameter for NUMOUTNEG' /
     & 'c    NUMOUTINDD   = Dimension parameter for NUMOUTIND' /
     & 'c    MXNIRRRXNSD  = Dimension parameter for MXNIRRRXNS' /
     & 'c    MXTEMPTRMSD  = Dimension parameter for MXTEMPTRMS' /
     & 'c    NIRRRXNS     = Vector of no. of reaction terms in an', 
     &                    ' output IRR' /
     & 'c    TEMPTERMS    = Vector of no. of terms in a temporary IRR' /
     & 'c    TEMPOUTPOS   = Vector of no. of IRR terms to add if', 
     &                    ' IRR > 0' /
     & 'c    TEMPOUTNEG   = Vector of no. of IRR terms to add if', 
     &                    ' IRR < 0' /
     & 'c    TEMPOUTIND   = Vector of no. of IRR terms to add', 
     &                    ' regardless of sign' /
     & 'c    INDXOUTPOS   = Vector of indices pointing to IRR output', 
     &                    ' variable' /
     & 'c    INDXOUTNEG   = Vector of indices pointing to IRR output', 
     &                    ' variable' /
     & 'c    INDXOUTIND   = Vector of indices pointing to IRR output', 
     &                    ' variable' /
     & 'c    COEFOUTPOS   = Vector of temp IRR coefficients' /
     & 'c    COEFOUTNEG   = Vector of temp IRR coefficients' /
     & 'c    COEFOUTIND   = Vector of temp IRR coefficients' /
     & 'c    TEMPRXN      = Array of reaction nos. for a temp. IRR' /
     & 'c    IRRRXN       = Array of reaction nos. for an output IRR' /
     & 'c    TEMPCOEF     = Array of reaction coefficients for a', 
     &                    ' temp IRR' /
     & 'c    IRRCOEF      = Array of reaction coefficients for an', 
     &                   ' output IRR' /
     & 'c    IRRSTEP      = Array of output IRRs for one time step' /
     & 'c    IRRBLK       = Array of output IRRs for a block of cells' /
     & 'c    IRROUT       = Array of IRRs to output' ) 

94420 FORMAT( / 6X,  'LOGICAL, PARAMETER :: LFULLIRR = .TRUE.' )
94440 FORMAT( / 6X,  'LOGICAL, PARAMETER :: LFULLIRR = .FALSE.' )  
94480 FORMAT( / 6X,  'LOGICAL, PARAMETER :: LPARTIRR = .TRUE.' )
94500 FORMAT( / 6X,  'LOGICAL, PARAMETER :: LPARTIRR = .FALSE.' ) 

94520 FORMAT( /
     & 6X, 'INTEGER, PARAMETER :: NUMTEMPS   =', I4, ',',
     & 2X,                       'NUMTEMPSD   =', I4 /
     & 6X, 'INTEGER, PARAMETER :: NUMOUTPOS  =', I4, ',',
     & 2X,                       'NUMOUTPOSD  =', I4 /
     & 6X, 'INTEGER, PARAMETER :: NUMOUTNEG  =', I4, ',',
     & 2X,                       'NUMOUTNEGD  =', I4 /
     & 6X, 'INTEGER, PARAMETER :: NUMOUTIND  =', I4, ',',
     & 2X,                       'NUMOUTINDD  =', I4 /
     & 6X, 'INTEGER, PARAMETER :: MXNIRRRXNS =', I4, ',',
     & 2X,                       'MXNIRRRXNSD =', I4 /
     & 6X, 'INTEGER, PARAMETER :: MXTEMPTRMS =', I4, ',',
     & 2X,                       'MXTEMPTRMSD =', I4 )

94542 FORMAT( /
     & 6X, 'INTEGER, PARAMETER :: BLKSIZED = BLKSIZE' )

94544 FORMAT( /
     & 6X, 'INTEGER, PARAMETER :: BLKSIZED = 1' )

94580 FORMAT( /
     & 6X, 'INTEGER NIRRRXNS  ( NIRRVARD )' /
     & 6X, 'INTEGER TEMPTERMS ( NUMTEMPSD )' /
     & 6X, 'INTEGER TEMPOUTPOS( NUMOUTPOSD )' /
     & 6X, 'INTEGER INDXOUTPOS( NUMOUTPOSD )' /
     & 6X, 'INTEGER TEMPOUTNEG( NUMOUTNEGD )' /
     & 6X, 'INTEGER INDXOUTNEG( NUMOUTNEGD )' /
     & 6X, 'INTEGER TEMPOUTIND( NUMOUTINDD )' /
     & 6X, 'INTEGER INDXOUTIND( NUMOUTINDD )' /
     & 6X, 'INTEGER IRRRXN    ( NIRRVARD,MXNIRRRXNSD )' /
     & 6X, 'INTEGER TEMPRXN   ( NUMTEMPSD,MXTEMPTRMSD )' //
     & 6X, 'REAL    COEFOUTPOS( NUMOUTPOSD )' /
     & 6X, 'REAL    COEFOUTNEG( NUMOUTNEGD )' /
     & 6X, 'REAL    COEFOUTIND( NUMOUTINDD )' /
     & 6X, 'REAL    TEMPCOEF  ( NUMTEMPSD,MXTEMPTRMSD )' /
     & 6X, 'REAL    IRRCOEF   ( NIRRVARD,MXNIRRRXNSD )' /
     & 6X, 'REAL    IRRSTEP   ( BLKSIZED,NIRRVARD )' /
     & 6X, 'REAL    IRRBLK    ( BLKSIZED,NIRRVARD )' )

94581 FORMAT(
     & 6X, 'REAL    IRROUT( IRCOLSD,IRROWSD,IRLEVSD,NIRRVARD )' / )

94700 FORMAT( 'c  Common Blocks:' )

94720 FORMAT( 
     & 5X, ' COMMON /PACHAR/ IPRNAME,' /
     & 5X, '&                IPRDESC,' /
     & 5X, '&                IRRNAME,' /
     & 5X, '&                IRRDESC,' /
     & 5X, '&                PROCNAME' / )
     
94740 FORMAT( 
     & 5X, ' COMMON /PALGCL/ LPROCOUT' / )

94760 FORMAT( 
     & 5X, ' COMMON /PAINTG/ NCGRID,' /
     & 5X, '&                SV2GRD,' /
     & 5X, '&                IPROUT,' /
     & 5X, '&                IPR2GRD,' /
     & 5X, '&                IPR2SAV,' /
     & 5X, '&                NIRRRXNS,' /
     & 5X, '&                TEMPTERMS,' /
     & 5X, '&                TEMPOUTPOS,' /
     & 5X, '&                TEMPOUTNEG,' /
     & 5X, '&                TEMPOUTIND,' /
     & 5X, '&                INDXOUTPOS,' /
     & 5X, '&                INDXOUTNEG,' /
     & 5X, '&                INDXOUTIND,' /
     & 5X, '&                TEMPRXN,' /
     & 5X, '&                IRRRXN' / )

94780 FORMAT( 
     & 5X, ' COMMON /PAREAL/ SPCOEF,' /
     & 5X, '&                DELC,' /
     & 5X, '&                CSAV,' /
     & 5X, '&                COEFOUTPOS,' /
     & 5X, '&                COEFOUTNEG,' /
     & 5X, '&                COEFOUTIND,' /
     & 5X, '&                TEMPCOEF,' /
     & 5X, '&                IRRCOEF,' /
     & 5X, '&                IRRSTEP,' /
     & 5X, '&                IRRBLK,' /
     & 5X, '&                IRROUT' / )

94781 FORMAT( 
     & 5X, ' COMMON /PAREAL/ SPCOEF,' /
     & 5X, '&                COEFOUTPOS,' /
     & 5X, '&                COEFOUTNEG,' /
     & 5X, '&                COEFOUTIND,' /
     & 5X, '&                TEMPCOEF,' /
     & 5X, '&                IRRCOEF,' /
     & 5X, '&                IRRSTEP,' /
     & 5X, '&                IRRBLK' / )
 
      END
