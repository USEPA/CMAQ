
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
      SUBROUTINE WR_ACONC ( AGRID, JDATE, JTIME, TSTEP )

C Revision History:
C   Jeff - July 01
C   Note: If previous A_CONC exists, check that user hasn't changed what
C         species/layers to save (or domain).
C   30 Mar 01 J.Young: dyn alloc - Use HGRD_DEFN
C   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN;
C                        removed deprecated TRIMLEN
C   20 Jul 11 J.Young: added option for ending time timestamp
C   12 Aug 15 D.Wong:  Extracted section of code that deals with creating
C                        A_CONC_1 and put it in opaconc.F
C                      Added a section of code to allow non IO processors
C                        to open A_CONC_1
C   19 May 16 D.Wong: renamed ACONC_END_TIME to AVG_FILE_ENDTIME
C   27 Mar 17 D.Wong: removed redundant parallel_io ifdef construct
C   09 Sep 19 F. Sidi: Replaced L_ACONC_WVEL(depreciated) with W_VEL
C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE AVG_CONC              ! integral average CONC
      USE UTILIO_DEFN
      USE RUNTIME_VARS
#ifndef mpas
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE)
#endif
#endif

      IMPLICIT NONE

C Include Files:

      INCLUDE SUBST_FILES_ID    ! file name parameters

      REAL      :: AGRID( :,:,:,: )
      INTEGER      JDATE        ! current model date, coded YYYYDDD
      INTEGER      JTIME        ! current model time, coded HHMMSS
      INTEGER      TSTEP        ! output timestep (HHMMSS)

C Local variables:

      INTEGER      MDATE        ! modified model date, coded YYYYDDD
      INTEGER      MTIME        ! modified model time, coded HHMMSS

      CHARACTER( 16 ) :: PNAME = 'WR_ACONC'
      CHARACTER( 96 ) :: XMSG = ' '

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      LOGICAL OK

      INTEGER      L, K, VAR, SPC ! loop counters
      INTEGER      STATUS

C-----------------------------------------------------------------------

C Change output date/time to starting date/time - e.g. timestamp 1995196:090000
C represents data computed from time 1995196:090000 to 1995196:100000

#ifndef mpas
      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.

C Override default beginning time timestamp for ACONC?
         IF ( .NOT. IO_PE_INCLUSIVE ) THEN
            IF ( .NOT. OPEN3( A_CONC_1, FSREAD3, PNAME ) ) THEN
               XMSG = 'Could not open ' // A_CONC_1 // ' file for update'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
         END IF

      END IF

      IF ( END_TIME ) THEN   ! ending time timestamp
         MDATE = JDATE; MTIME = JTIME
      ELSE                   ! beginning time timestamp
         MDATE = JDATE; MTIME = JTIME
         CALL NEXTIME ( MDATE, MTIME, -TSTEP )
      END IF

      VAR = 0
    
      DO SPC = 1, N_A_GC_SPC
         VAR = VAR + 1

         IF ( .NOT. WRITE3( A_CONC_1, A_GC_SPC( SPC ),
     &      MDATE, MTIME, AGRID( :,:,:,VAR ) ) ) THEN
            XMSG = 'Could not write '
     &           // TRIM( A_GC_SPC( SPC ) )
     &           // ' to ' // A_CONC_1
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF
      END DO
 
      DO SPC = 1, N_A_AE_SPC
         VAR = VAR + 1
         IF ( .NOT. WRITE3( A_CONC_1, A_AE_SPC( SPC ),
     &      MDATE, MTIME, AGRID( :,:,:,VAR ) ) ) THEN
            XMSG = 'Could not write '
     &           // TRIM( A_AE_SPC( SPC ) )
     &           // ' to ' // A_CONC_1
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF
      END DO
 
      DO SPC = 1, N_A_NR_SPC
         VAR = VAR + 1
         IF ( .NOT. WRITE3( A_CONC_1, A_NR_SPC( SPC ),
     &      MDATE, MTIME, AGRID( :,:,:,VAR ) ) ) THEN
            XMSG = 'Could not write '
     &           // TRIM( A_NR_SPC( SPC ) )
     &           // ' to ' // A_CONC_1
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF
      END DO
 
      ! Write Tracer Species Concentrations to Average Concentration File
      DO SPC = 1, N_A_TR_SPC
         VAR = VAR + 1
         IF ( .NOT. WRITE3( A_CONC_1, A_TR_SPC( SPC ),
     &      MDATE, MTIME, AGRID( :,:,:,VAR ) ) ) THEN
            XMSG = 'Could not write '
     &           // TRIM( A_TR_SPC( SPC ) )
     &           // ' to ' // A_CONC_1
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF
      END DO
 
      ! Write Vertical Velocity to Average Concentration File
      IF ( W_VEL ) THEN
        IF ( .NOT. WRITE3( A_CONC_1, 'W_VEL',
     &        MDATE, MTIME, AVG_WVEL ) ) THEN
              XMSG = 'Could not write Vertical Wind Velocity'
     &             // ' to ' // A_CONC_1
              CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
        END IF 
      END IF
 
      ! Write Relative Humidity to Average Concentration File
      IF ( L_ACONC_RH ) THEN
        IF ( .NOT. WRITE3( A_CONC_1, 'RH',
     &        MDATE, MTIME, AVG_RH ) ) THEN
              XMSG = 'Could not write Relative Humidity'
     &             // ' to ' // A_CONC_1
              CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
        END IF 
      END IF
      
      ! Write Temperature to Average Concentration File
      IF ( L_ACONC_TA ) THEN
        IF ( .NOT. WRITE3( A_CONC_1, 'TA',
     &        MDATE, MTIME, AVG_TA ) ) THEN
              XMSG = 'Could not write Temperature'
     &             // ' to ' // A_CONC_1
              CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
        END IF 
      END IF
 

      ! Write Pressure to Average Concentration File
      IF (L_ACONC_PRES ) THEN
        IF ( .NOT. WRITE3( A_CONC_1, 'PRES',
     &        MDATE, MTIME, AVG_PRES ) ) THEN
              XMSG = 'Could not write Pressure'
     &             // ' to ' // A_CONC_1
              CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
        END IF 
      END IF
 

      WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &      'Timestep written to', A_CONC_1,
     &      'for date and time', MDATE, MTIME
#endif

      RETURN 
      END
