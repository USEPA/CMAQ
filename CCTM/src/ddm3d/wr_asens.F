
C***********************************************************************
C   Portions of Models-3/CMAQ software were developed or based on      *
C   information from various groups: Federal Government employees,     *
C   contractors working on a United States Government contract, and    *
C   non-Federal sources (including research institutions).  These      *
C   research institutions have given the Government permission to      *
C   use, prepare derivative works, and distribute copies of their      *
C   work in Models-3/CMAQ to the public and to permit others to do     *
C   so.  EPA therefore grants similar permissions for use of the       *
C   Models-3/CMAQ software, but users are requested to provide copies  *
C   of derivative works to the Government without restrictions as to   *
C   use by others.  Users are responsible for acquiring their own      *
C   copies of commercial software associated with Models-3/CMAQ and    *
C   for complying with vendor requirements.  Software copyrights by    *
C   the MCNC Environmental Modeling Center are used with their         *
C   permissions subject to the above restrictions.                     *
C***********************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/work/rep/CCTM/src/driver/yamo_ddm3d/wr_asens.F,v 1.1 2009/09/29 13:53:30 sjr Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE WR_ASENS ( JDATE, JTIME, TSTEP )

C Revision History:
C   Daniel Cohan - Jan. 04 - A modified version of wr_aconc to be
C       used for writing out hourly-averaged sensitivities.
C       A_SENS_1 is written on same levels and for the same species
C       as A_CONC_1.
C   Jeff - July 01
C   Note: If previous A_SENS exists, check that user hasn't changed what
C         species/layers to save (or domain).
C   30 Mar 01 J.Young: dyn alloc - Use HGRD_DEFN
C   Jul 06 D.Cohan: modify for following change in wr_aconc:
C   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module
C   09 Nov 12 S.L.Napelenok: update for cmaq 5.0
C   14 Oct 15 S.L.Napelenok: update for cmaq 5.1.1
C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE AVG_CONC              ! integral average CONC
      USE UTILIO_DEFN

#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE)
#endif

      USE DDM3D_DEFN

      IMPLICIT NONE

C Include Files:

      INCLUDE SUBST_FILES_ID    ! file name parameters

      INTEGER      JDATE        ! current model date, coded YYYYDDD
      INTEGER      JTIME        ! current model time, coded HHMMSS
      INTEGER      TSTEP        ! output timestep (HHMMSS)

C Local variables:

      INTEGER      MDATE        ! modified model date, coded YYYYDDD
      INTEGER      MTIME        ! modified model time, coded HHMMSS

      CHARACTER( 16 ) :: PNAME = 'WR_ASENS'
      CHARACTER( 16 ) :: ACONC_END_TIME = 'ACONC_END_TIME'
      CHARACTER( 80 ) :: VARDESC = ' '
      CHARACTER( 96 ) :: XMSG = ' '

c     CHARACTER( 8 )  :: SPCNAME

      ! Names of sensitivity output variables
      CHARACTER( 16 ) :: SENNAME( NPMAX, N_ASPCS )

      INTEGER, SAVE :: LOGDEV       ! FORTRAN unit number for log file
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      LOGICAL OK
      LOGICAL, SAVE :: END_TIME = .FALSE.

      INTEGER      L, K, KD, VAR, SPC ! loop counters
      INTEGER      STATUS

C-----------------------------------------------------------------------

C Change output date/time to starting date/time - e.g. timestamp 1995196:090000
C represents data computed from time 1995196:090000 to 1995196:100000

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         LOGDEV = INIT3 ()

C Override default beginning time timestamp for ACONC?
         VARDESC = 'Flag to Override default beginning time ' //
     &             'timestamp for ACONC'
         END_TIME = ENVYN( ACONC_END_TIME, VARDESC, END_TIME, STATUS )
         IF ( STATUS .EQ. 1 ) THEN
            XMSG = 'Environment variable improperly formatted'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         ELSE IF ( STATUS .NE. 0 ) THEN
            WRITE( LOGDEV, '(5X, A)' ) VARDESC
            XMSG = '... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
         END IF

         IF ( .not. IO_PE_INCLUSIVE ) THEN
#ifdef parallel_io
            IF ( .NOT. OPEN3( A_SENS_1, FSNONIO, PNAME ) ) THEN
#else
            IF ( .NOT. OPEN3( A_SENS_1, FSREAD3, PNAME ) ) THEN
#endif
               XMSG = 'Could not open ' // A_SENS_1 // ' file for update'
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
         SPCNAME = A_GC_SPC( SPC )( 1:12 )
         VAR = VAR + 1
         DO NP = 1, NPMAX
            SENNAME( NP,VAR ) = TRIM(SPCNAME) // '_' // SEN_PAR( NP )
            IF ( .NOT. WRITE3( A_SENS_1, SENNAME( NP,VAR ),
     &         MDATE, MTIME, SAGRID( :,:,:,NP,VAR ) ) ) THEN
               XMSG = 'Could not write '// SENNAME( NP,VAR ) // ' to ' // A_SENS_1
               CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF
         END DO
      END DO

      DO SPC = 1, N_A_AE_SPC
         SPCNAME = A_AE_SPC( SPC )( 1:12 )
         VAR = VAR + 1
         DO NP = 1, NPMAX
            SENNAME( NP,VAR ) = TRIM(SPCNAME) // '_' // SEN_PAR( NP )
            IF ( .NOT. WRITE3( A_SENS_1, SENNAME( NP,VAR ),
     &         MDATE, MTIME, SAGRID( :,:,:,NP,VAR ) ) ) THEN
               XMSG = 'Could not write '// SENNAME( NP,VAR ) // ' to ' // A_SENS_1
               CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF
         END DO
      END DO

      DO SPC = 1, N_A_NR_SPC
         SPCNAME = A_NR_SPC( SPC )( 1:12 )
         VAR = VAR + 1
         DO NP = 1, NPMAX
            SENNAME( NP,VAR ) = TRIM(SPCNAME) // '_' // SEN_PAR( NP )
            IF ( .NOT. WRITE3( A_SENS_1, SENNAME( NP,VAR ),
     &         MDATE, MTIME, SAGRID( :,:,:,NP,VAR ) ) ) THEN
               XMSG = 'Could not write '// SENNAME( NP,VAR ) // ' to ' // A_SENS_1
               CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF
         END DO
      END DO

      DO SPC = 1, N_A_TR_SPC
         SPCNAME = A_TR_SPC( SPC )( 1:12 )
         VAR = VAR + 1
         DO NP = 1, NPMAX
            SENNAME( NP,VAR ) = TRIM(SPCNAME) // '_' // SEN_PAR( NP )
            IF ( .NOT. WRITE3( A_SENS_1, SENNAME( NP,VAR ),
     &         MDATE, MTIME, SAGRID( :,:,:,NP,VAR ) ) ) THEN
               XMSG = 'Could not write '// SENNAME( NP,VAR ) // ' to ' // A_SENS_1
               CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF
         END DO
      END DO

      WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &      'Timestep written to', A_SENS_1,
     &      'for date and time', MDATE, MTIME

      RETURN 
      END
