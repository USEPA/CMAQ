
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
C $Header: /project/yoj/arc/CCTM/src/driver/wrf/sciproc.F,v 1.2 2011/10/21 16:10:40 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#ifdef isam
      SUBROUTINE SCIPROC ( CGRID, JDATE, JTIME, TSTEP, ASTEP,
     &                     ISAM, MFRC_P, CBFOR, ISAMB4 )
#else
      SUBROUTINE SCIPROC ( CGRID, JDATE, JTIME, TSTEP, ASTEP )
#endif


C-----------------------------------------------------------------------
C Function:
C    Controls all of the physical and chemical processes for a grid
C    Operator splitting symmetric around chemistry
 
C Preconditions:
C    Dates and times represented YYYYDDD:HHMMSS.
C    No "skipped" dates and times.  All boundary input variables (layered or
C    non-layered) have the same perimeter structure with a thickness of NTHIK
 
C Subroutines and functions called:
C    All physical and chemical subroutines, 
C    DECOUPLE_WRF, COUPLE_WRF
 
C Revision History:
C    Oct. 24, 1995 by M. Talat Odman and Clint L. Ingram at NCSC: created
C    13 Dec 97 - Jeff - uncouple diffusion processes
C    27 Jun 98 - Jeff - sync step = chem step
C     7 Jul 01 - Shawn - mv cloud processing before chem
C       Jan 02 - Jeff - dyn alloc; remove PCGRID argument to ping
C    23 Jun 03 J.Young: for layer dependent advection tstep
C    18 Aug 03 J. Pleim - move vdiff before advection
C    29 Aug 03 J.Young: eliminate symmetric processing option and unused
C                       SUBST_GRID_ID string
C    30 May 05 J.Young: mass-conserving advection (yamo)
C     7 Jul 07 J.Young: No cksummer call after HADV - delay possible truncation of
C                       HADV negative conc's until after ZADV
C    25 Jul 08 S.Roselle: removed PING from modeled processes
C    16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C    11 May 11 D.Wong: incorporated twoway model implementation
C    19 Jul 13 R.Kwok: isam
C-----------------------------------------------------------------------

      USE UTILIO_DEFN
#ifdef isam
      USE GRID_CONF
      USE SA_DEFN
      USE PCGRID_DEFN   !KRT20121212
      USE PCBFOR_DEFN   !KRT20121212
      USE PISAM_DEFN    !KRT20121212
      USE PISAMB4_DEFN  !KRT20121212
#endif

      IMPLICIT NONE   

C Include files:

      INCLUDE SUBST_PACTL_ID    ! PA control parameters

C Arguments:
      REAL, POINTER :: CGRID( :,:,:,: )

      INTEGER      JDATE        ! current model date, coded YYYYDDD
      INTEGER      JTIME        ! current model time, coded HHMMSS
      INTEGER      TSTEP( 3 )   ! time step vector (HHMMSS)
                                ! TSTEP(1) = local output step
                                ! TSTEP(2) = sciproc sync. step (chem)
                                ! TSTEP(3) = twoway model time step w.r.t. wrf time
                                !            step and wrf/cmaq call frequency

!     INTEGER      ASTEP( NLAYS )  ! layer advection time step
      INTEGER      ASTEP( : )      ! layer advection time step
#ifdef isam
      REAL, POINTER :: ISAM( :,:,:,:,: )
      REAL, POINTER :: MFRC_P( :,:,: )
      REAL, POINTER :: CBFOR( :,:,:,: )
      REAL, POINTER :: ISAMB4( :,:,:,:,: )
#endif

C Parameters:

C External Functions:

C Local Variables:

      CHARACTER( 16 ) :: PNAME = 'SCIPROC'

      LOGICAL, SAVE :: FIRSTIME = .TRUE.

      CHARACTER( 120 ) :: XMSG = ' '
      CHARACTER( 36 ) :: NMSG = 'After NEXTIME: returned JDATE, JTIME'

      INTEGER, SAVE :: LOGDEV

      INTEGER      SDATE        ! current science process date, coded YYYYDDD
      INTEGER      STIME        ! current science process time, coded HHMMSS
 
      INTEGER      ALLOCSTAT
      INTEGER      STATUS          ! ENV... status
      CHARACTER( 80 ) :: VARDESC   ! environment variable description
      CHARACTER( 80 ) :: MSG = ' '
      CHARACTER( 16 ) :: CTM_CKSUM = 'CTM_CKSUM'     ! env var for cksum on
      LOGICAL, SAVE   :: CKSUM     ! flag for cksum on, default = [T]

#ifdef isam
      LOGICAL         :: LOUTPUT
      INTEGER         :: JSPC, ITAG, K
      INTEGER         :: IERR
      LOGICAL         :: LSA_CHKSUM
      LOGICAL         :: LSA_RMNGTV
      !below just to check if there's any difference in cgrid before and after chem
!20130627      INTEGER, SAVE   :: JO3V
#endif

      INTERFACE
         SUBROUTINE CKSUMMER ( PNAME, CGRID, JDATE, JTIME )
            IMPLICIT NONE
            CHARACTER( * ), INTENT( IN ) :: PNAME
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
         END SUBROUTINE CKSUMMER
         SUBROUTINE PA_UPDATE ( PNAME, CGRID, JDATE, JTIME, TSTEP )
            IMPLICIT NONE
            CHARACTER( * ), INTENT( IN ) :: PNAME
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE PA_UPDATE
#ifdef isam
         SUBROUTINE HADV ( CGRID, JDATE, JTIME, TSTEP, ASTEP, ISAM )
#else
         SUBROUTINE HADV ( CGRID, JDATE, JTIME, TSTEP, ASTEP )
#endif
            IMPLICIT NONE
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
            INTEGER, INTENT( IN )     :: ASTEP( : )
#ifdef isam
            REAL, POINTER             :: ISAM( :,:,:,:,: )
#endif
         END SUBROUTINE HADV
#ifdef isam
         SUBROUTINE ZADV ( CGRID, JDATE, JTIME, TSTEP, ISAM )
#else
         SUBROUTINE ZADV ( CGRID, JDATE, JTIME, TSTEP )
#endif
            IMPLICIT NONE
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
#ifdef isam
            REAL, POINTER             :: ISAM( :,:,:,:,: )
#endif
         END SUBROUTINE ZADV
#ifdef isam
         SUBROUTINE COUPLE_WRF ( CGRID, JDATE, JTIME, TSTEP, ISAM )
#else
         SUBROUTINE COUPLE_WRF ( CGRID, JDATE, JTIME, TSTEP )
#endif
            IMPLICIT NONE
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
#ifdef isam
            REAL, POINTER             :: ISAM( :,:,:,:,: )
#endif
         END SUBROUTINE COUPLE_WRF
#ifdef isam
         SUBROUTINE DECOUPLE_WRF ( CGRID, JDATE, JTIME, TSTEP, ISAM )
#else
         SUBROUTINE DECOUPLE_WRF ( CGRID, JDATE, JTIME, TSTEP )
#endif
            IMPLICIT NONE
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
#ifdef isam
            REAL, POINTER             :: ISAM( :,:,:,:,: )
#endif
         END SUBROUTINE DECOUPLE_WRF
#ifdef isam
         SUBROUTINE HDIFF ( CGRID, JDATE, JTIME, TSTEP, ISAM )
#else
         SUBROUTINE HDIFF ( CGRID, JDATE, JTIME, TSTEP )
#endif
            IMPLICIT NONE
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
#ifdef isam
            REAL, POINTER             :: ISAM( :,:,:,:,: )
#endif
         END SUBROUTINE HDIFF
#ifdef isam
         SUBROUTINE VDIFF ( CGRID, JDATE, JTIME, TSTEP, ISAM, MFRC_P )
#else
         SUBROUTINE VDIFF ( CGRID, JDATE, JTIME, TSTEP )
#endif
            IMPLICIT NONE
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
#ifdef isam
            REAL, POINTER             :: ISAM( :,:,:,:,: )
            REAL, POINTER             :: MFRC_P( :,:,: )
#endif
         END SUBROUTINE VDIFF
#ifdef isam
         SUBROUTINE CLDPROC ( CGRID, JDATE, JTIME, TSTEP,
     &                        CBFOR, ISAM, ISAMB4 )
#else
         SUBROUTINE CLDPROC ( CGRID, JDATE, JTIME, TSTEP )
#endif
            IMPLICIT NONE
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
#ifdef isam
            REAL, POINTER             :: CBFOR( :,:,:,: )
            REAL, POINTER             :: ISAM( :,:,:,:,: )
            REAL, POINTER             :: ISAMB4( :,:,:,:,: )
#endif
         END SUBROUTINE CLDPROC
#ifdef isam
         SUBROUTINE CHEM ( CGRID, JDATE, JTIME, TSTEP, ISAM, CBFOR, ISAMB4 )
#else
         SUBROUTINE CHEM ( CGRID, JDATE, JTIME, TSTEP )
#endif
            IMPLICIT NONE
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
#ifdef isam
            REAL, POINTER             :: ISAM( :,:,:,:,: )
            REAL, POINTER             :: CBFOR( :,:,:,: )
            REAL, POINTER             :: ISAMB4( :,:,:,:,: )
#endif
         END SUBROUTINE CHEM
#ifdef isam
         SUBROUTINE SA_RNORM( JDATE, JTIME, TSTEP, CGRID, ISAM )
            IMPLICIT NONE
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
            REAL, POINTER             :: CGRID( :,:,:,: )
            REAL, POINTER             :: ISAM( :,:,:,:,: )
         END SUBROUTINE SA_RNORM
#endif
#ifdef isam
         SUBROUTINE SA_WRAP_OZ( CGRID, CBFOR, ISAM, ISAMB4, JDATE, JTIME, TSTEP )
            IMPLICIT NONE
            REAL, POINTER             :: CGRID( :,:,:,: )
            REAL, POINTER             :: CBFOR( :,:,:,: )
            REAL, POINTER             :: ISAM( :,:,:,:,: )
            REAL, POINTER             :: ISAMB4( :,:,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE SA_WRAP_OZ
#endif
         SUBROUTINE AERO ( CGRID, JDATE, JTIME, TSTEP )
            IMPLICIT NONE
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE AERO
#ifdef isam
Ckrt...aero wrapper
         SUBROUTINE SA_WRAP_AE( CGRID, CBFOR, ISAM, ISAMB4, JDATE, JTIME, TSTEP )
            IMPLICIT NONE
            REAL, POINTER             :: CGRID( :,:,:,: )
            REAL, POINTER             :: CBFOR( :,:,:,: )
            REAL, POINTER             :: ISAM( :,:,:,:,: )
            REAL, POINTER             :: ISAMB4( :,:,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE
            INTEGER, INTENT( IN )     :: JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE SA_WRAP_AE
#endif
      END INTERFACE
  
C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         LOGDEV = INIT3 ()

         CKSUM = .TRUE.         ! default
         VARDESC = 'Cksum on flag'
         CKSUM = ENVYN( CTM_CKSUM, VARDESC, CKSUM, STATUS )
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 1 ) THEN
            XMSG = 'Environment variable improperly formatted'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         ELSE IF ( STATUS .EQ. -1 ) THEN
            MSG = 'Environment variable set, but empty ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
         ELSE IF ( STATUS .EQ. -2 ) THEN
            MSG = 'Environment variable not set ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) MSG, JTIME
         END IF

      END IF       ! if firstime

#ifdef isam
      LSA_CHKSUM = .FALSE.  ! do checksum over all tags ?
      LSA_RMNGTV = .FALSE.  ! reset any -ve tags ?
! 20130529 reinitialize SULRXN if any
      JSPC = INDEX1( 'SULRXN', NSPC_SA, SPC_NAME( :,OTHRTAG ) )
      IF ( JSPC .GT. 0 ) ISAM( :,:,:,JSPC,: ) = 0.0
#endif

C Synchronization Step Physical Processes Sequence

#ifdef isam
      CALL VDIFF ( CGRID, JDATE, JTIME, TSTEP, ISAM, MFRC_P )
#else
      CALL VDIFF ( CGRID, JDATE, JTIME, TSTEP )
#endif
      IF ( CKSUM ) CALL CKSUMMER ( 'VDIFF', CGRID, JDATE, JTIME )
      IF ( LIPR ) CALL PA_UPDATE ( 'VDIF', CGRID, JDATE, JTIME, TSTEP )

C couple CGRID for advection and horizontal diffusion

#ifdef isam
      CALL COUPLE_WRF ( CGRID, JDATE, JTIME, TSTEP, ISAM )
#else
      CALL COUPLE_WRF ( CGRID, JDATE, JTIME, TSTEP )
#endif
      IF ( CKSUM ) CALL CKSUMMER ( 'COUPLE_WRF', CGRID, JDATE, JTIME )

#ifdef isam
      CALL HADV ( CGRID, JDATE, JTIME, TSTEP, ASTEP, ISAM )
#else
      CALL HADV ( CGRID, JDATE, JTIME, TSTEP, ASTEP )
#endif
!     IF ( CKSUM ) CALL CKSUMMER ( 'HADV', CGRID, JDATE, JTIME )
      IF ( LIPR ) CALL PA_UPDATE ( 'HADV', CGRID, JDATE, JTIME, TSTEP )

#ifdef isam
      CALL ZADV ( CGRID, JDATE, JTIME, TSTEP, ISAM )
#else
      CALL ZADV ( CGRID, JDATE, JTIME, TSTEP )
#endif
      IF ( CKSUM ) CALL CKSUMMER ( 'ADV', CGRID, JDATE, JTIME )
      IF ( LIPR ) CALL PA_UPDATE ( 'ZADV', CGRID, JDATE, JTIME, TSTEP )

#ifdef isam
      CALL HDIFF ( CGRID, JDATE, JTIME, TSTEP, ISAM )
#else
      CALL HDIFF ( CGRID, JDATE, JTIME, TSTEP )
#endif
      IF ( CKSUM ) CALL CKSUMMER ( 'HDIFF', CGRID, JDATE, JTIME )
      IF ( LIPR ) CALL PA_UPDATE ( 'HDIF', CGRID, JDATE, JTIME, TSTEP )

C decouple CGRID for cloud and chemistry

      SDATE = JDATE
      STIME = JTIME
      CALL NEXTIME ( SDATE, STIME, TSTEP( 2 ) )

#ifdef isam
      CALL DECOUPLE_WRF ( CGRID, SDATE, STIME, TSTEP, ISAM )
#else
      CALL DECOUPLE_WRF ( CGRID, SDATE, STIME, TSTEP )
#endif
      IF ( CKSUM ) CALL CKSUMMER ( 'DECOUPLE_WRF', CGRID, JDATE, JTIME )

#ifdef isam
      PCBFOR = PCGRID
      PISAMB4 = PISAM
      CALL CLDPROC ( CGRID, JDATE, JTIME, TSTEP,
     &               CBFOR, ISAM, ISAMB4 )
#else
      CALL CLDPROC ( CGRID, JDATE, JTIME, TSTEP )
#endif
      IF ( CKSUM ) CALL CKSUMMER ( 'CLDPROC', CGRID, JDATE, JTIME )
      IF ( LIPR ) CALL PA_UPDATE ( 'CLDS', CGRID, JDATE, JTIME, TSTEP )

#ifdef isam
      IF ( YES_RENORM ) CALL SA_RNORM( JDATE, JTIME, TSTEP, CGRID, ISAM )
      PCBFOR = PCGRID
      PISAMB4 = PISAM
      CALL CHEM ( CGRID, JDATE, JTIME, TSTEP, ISAM, CBFOR, ISAMB4 )
      IF ( YES_RENORM ) CALL SA_RNORM( JDATE, JTIME, TSTEP, CGRID, ISAM )
#else
      CALL CHEM ( CGRID, JDATE, JTIME, TSTEP )
#endif
      IF ( CKSUM ) CALL CKSUMMER ( 'CHEM', CGRID, JDATE, JTIME )
      IF ( LIPR ) CALL PA_UPDATE ( 'CHEM', CGRID, JDATE, JTIME, TSTEP ) 

#ifdef isam
      CALL SA_WRAP_OZ( CGRID, CBFOR, ISAM, ISAMB4, JDATE, JTIME, TSTEP )
#endif

#ifdef isam
      PCBFOR = PCGRID
      PISAMB4 = PISAM
#endif
      CALL AERO ( CGRID, JDATE, JTIME, TSTEP )
      IF ( CKSUM ) CALL CKSUMMER ( 'AERO', CGRID, JDATE, JTIME )
      IF ( LIPR ) CALL PA_UPDATE ( 'AERO', CGRID, JDATE, JTIME, TSTEP )

#ifdef isam
      CALL SA_WRAP_AE( CGRID, CBFOR, ISAM, ISAMB4, JDATE, JTIME, TSTEP )
#endif

      CALL NEXTIME ( JDATE, JTIME, TSTEP( 2 ) )
      WRITE( LOGDEV,'(/ 5X, A, I8, I7.6)' ) NMSG, JDATE, JTIME

      RETURN
      END
