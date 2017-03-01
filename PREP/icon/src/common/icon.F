
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
C $Header: /project/yoj/arc/ICON/src/common/icon.F,v 1.3 2011/11/01 16:07:15 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

      PROGRAM ICON   

C***********************************************************************
 
C Function: To produce a Models-3 CTM Initial Concentration file.
C           Valid input options include: 
C           1) a Models-3 CTM output file,
C           2) a user specified ASCII file of vertical profiles,
C           3) a RADM model output conc file,
C           4) subroutines that produce catalogued tracer species concs.
              
C Preconditions: None
  
C Key Subroutines/Functions Called: INIC
 
C Revision History:
C    Created by Jerry Gipson, January, 1998; based on the original Models-3
C                                            prototype.
C    Modified 4/22/99 by JG to provide default treatment for missing SDATE
C                      and STIME
C    12/13/99 David Wong, LM -- parallelelized the code
C    01/24/02 Steve Howard (Jeff Young) - dynamic allocation
C    12/13/04 J.Young: vert dyn alloc - Use VGRD_DEFN
C    24 May 06 J.Young: par_init/pio_init col/row order check
C    06 Jun 11 J.Young: Replaced I/O API include files with UTILIO_DEFN
C    13 Jun 11 J.Young: Convert for Namelist redesign
C    21 May 12 J.Young: Replaced IC_PARMS include file with an F90 module
                   
C***********************************************************************

      USE HGRD_DEFN     ! Module to store and load the horizontal grid variables
      USE VGRD_DEFN     ! vertical layer specifications
      USE UTILIO_DEFN
      USE CGRID_SPCS    ! CGRID mechanism species
      USE IC_PARMS      ! ICON parameters

      IMPLICIT NONE       

C Include Files:

C Arguments: None

C Parameters: None

C External Procedures:
      INTEGER, EXTERNAL :: SETUP_LOGDEV

C Local Variables:
      CHARACTER(  2 ) :: COLROW = 'CR'          ! PARIO col/row arg list order
!     CHARACTER( 16 ) :: NML_IC_NAME( NSPCSD )  ! Names of species in *.NMLs
      CHARACTER( 16 ), ALLOCATABLE :: NML_IC_NAME( : )  ! Names of species in CGRID
      CHARACTER( 16 ) :: PNAME = 'ICON'         ! Procedure name
      CHARACTER( 80 ) :: XMSG                   ! Log message 

      INTEGER    LOGUNIT      ! Unit number for output log
      INTEGER    ISP          ! Species counter
      INTEGER    N            ! Loop index
      INTEGER :: SDATE = 0    ! Start date 
      INTEGER :: STIME = 0    ! Start time 
      INTEGER    STATUS       ! Status code
      INTEGER    ALST         ! Allocate status

      INTEGER NPROCS          ! number of processors
      REAL    :: CLOCK = 0.0  ! Wall-clock time (sec) at initialization.
      INTEGER :: PAR_ERR = 0  ! Error code from parallel initialization.

C***********************************************************************

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Initialize IO/API
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#ifdef parallel
C Start up processor communication
      CALL MPCOMM_INIT( NPROCS, MYPE, CLOCK, PAR_ERR )
#else
      NPROCS = 1; MYPE = 0
#endif

      IF ( PAR_ERR .NE. 0 ) THEN
         XMSG = 'Error in initialization program PAR_INIT.'
         CALL M3EXIT( PNAME, SDATE, STIME, XMSG, XSTAT2 )
      END IF

!     LOGUNIT = INIT3( )
      LOGUNIT = SETUP_LOGDEV( )

C Set up horizontal domain, define vertical layer structure and
C calculate processor-to-subdomain maps
      IF ( .NOT. HGRD_INIT ( NPROCS, MYPE ) ) THEN
         XMSG = '*** Failure defining horizontal domain'
         CALL M3EXIT ( PNAME, SDATE, STIME, XMSG, XSTAT2 )
      END IF

      IF ( .NOT. VGRD_INIT() ) THEN
         XMSG = '*** Failure defining vertical structure'
         CALL M3EXIT ( PNAME, SDATE, STIME, XMSG, XSTAT2 )
      END IF

C Set CMAQ CGRID mechanism species
      IF ( .NOT. CGRID_SPCS_INIT() ) THEN
         XMSG = 'Error in CGRID_SPCS:CGRID_SPCS_INIT'
         CALL M3EXIT ( PNAME, SDATE, STIME, XMSG, XSTAT2 )
      END IF

#ifdef parallel
C Initialize parallel I/O
      IF ( .NOT. PIO_INIT( COLROW, GL_NCOLS, GL_NROWS, NLAYS, NTHIK,
     &                     NCOLS, NROWS, NPCOL, NPROW, NPROCS,
     &                     MYPE ) ) THEN
         XMSG = 'Failed to initialize parallel I/O library.'
         CALL M3EXIT ( PNAME, SDATE, STIME, XMSG, XSTAT2 )
      END IF
#endif

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  get environment variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( NSPCSD .LE. 0 ) THEN
         XMSG = 'Number of species in *.NML files must be greater than zero'
         CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

      SDATE = ENVINT( 'SDATE', 'START DATE', 0, STATUS )
      IF ( STATUS .NE. 0 ) THEN
         SDATE = 0
      END IF

      STIME = ENVINT( 'STIME', 'START TIME', 0, STATUS )
      IF ( STATUS .NE. 0 ) THEN
         STIME = 0
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Build one array containing the names of the species in *.NML files 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ALLOCATE ( NML_IC_NAME( NSPCSD ), STAT = ALST ) 
      IF ( ALST .NE. 0 ) THEN
         XMSG = 'Error allocating NML_IC_NAME'
         CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ISP = 0
      DO N = 1, N_GC_SPC
         ISP = ISP + 1
         NML_IC_NAME( ISP ) = GC_SPC( N )
      END DO

      DO N = 1, N_AE_SPC
         ISP = ISP + 1
         NML_IC_NAME( ISP ) = AE_SPC( N )
      END DO

      DO N = 1, N_NR_SPC
         ISP = ISP + 1
         NML_IC_NAME( ISP ) = NR_SPC( N )
      END DO

      DO N = 1, N_TR_SPC
         ISP = ISP + 1
         NML_IC_NAME( ISP ) = TR_SPC( N )
      END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Generate the IC output file and quit
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL INIC( LOGUNIT, SDATE, STIME, NML_IC_NAME )

#ifdef parallel
      CALL PAR_TERM ( PNAME, SDATE, STIME, CLOCK )
#endif

      IF ( SHUT3() ) THEN
         WRITE ( LOGUNIT,92999 )
     &  '>>---->  Program  ICON completed successfully  <----<< '
      ELSE
         WRITE ( LOGUNIT,91000 )
     &      'FATAL ERROR shutting down Models-3 I/O'
      END IF

      STOP

C************************* FORMAT STATEMENTS ***************************

92999 FORMAT ( //5X, A, // )

91000 FORMAT ( //5X , '*** ERROR ABORT in program ICON ***',
     &            /5X , A , // )        !  generic error message format

      END
