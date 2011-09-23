
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
C $Header: /project/yoj/arc/CCTM/src/util/util/cksummer.F,v 1.3 2011/09/23 17:18:16 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)cksummer.F        1.1 /project/mod3/CMAQ/src/util/util/SCCS/s.cksummer.F 03 Jun 1997 12:08:53

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE CKSUMMER ( SCIPROC, CGRID, JDATE, JTIME )

C Function:
C     Sum concentrations over entire grid.

C Revision History:
C   Original version ???
C   2 October, 1998 by Al Bourgeois at LM: parallel implementation
C         and fix bug by SAVEing DEVNAME.

C   1/22/99 David Wong at LM: compute global sum for variables: GC_CKSUM,
C                             AE_CKSUM, NR_CKSUM, and TR_CKSUM

C   1/28/99 David Wong at LM: compute global sum for GCELLS

C   15 Dec 00 J.Young: move CGRID_MAP into f90 module
C                      GLOBAL_RSUM -> Dave Wong's f90 stenex GLOBAL_SUM
C   Jeff - Feb 01 - assumed shape arrays
C   23 Mar 01 J.Young: Use HGRD_DEFN
C   31 May 02 J.Young: REAL*8 reduction accumulator (avoid 32 bit roundoff)
C   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module
C    4 Feb 08 J.Young: checksums based on local processor
C   21 Jun 10 J.Young: convert for Namelist redesign
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN;
C                        removed deprecated TRIMLEN
C   10 Aug 11 David Wong: reset output format 1PEw.d to meet the standard
C                         w-d > 6
C-----------------------------------------------------------------------

      USE GRID_CONF           ! horizontal & vertical domain specifications
      USE CGRID_SPCS          ! CGRID mechanism species
      USE UTILIO_DEFN
      USE SUBST_MODULES              ! stenex
!     USE SUBST_GLOBAL_SUM_MODULE    ! stenex

      IMPLICIT NONE

      CHARACTER( * ) SCIPROC    ! science process name
!     REAL      :: CGRID( :,:,:,: )
      REAL, POINTER :: CGRID( :,:,:,: )
      INTEGER      JDATE        ! current model date, coded YYYYDDD
      INTEGER      JTIME        ! current model time, coded HHMMSS

C Parameters:

      REAL, PARAMETER :: CMIN = 0.0

C Local variables:
 
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      INTEGER, SAVE :: LOGDEV         ! FORTRAN unit number for log file

      CHARACTER( 16 ), SAVE :: PNAME = 'CKSUMMER'
      CHARACTER( 96 ) :: XMSG = ' '
!     CHARACTER( 16 ), SAVE :: OUTNAME = 'FLOOR_FILE'
      CHARACTER( 16 ) :: APPL = 'CTM_APPL'
      CHARACTER( 48 ) :: EQNAME
      CHARACTER(  6 ) :: PRESTR = 'FLOOR_'
      CHARACTER(  3 ) :: CMYPE
      CHARACTER( 96 ), SAVE :: DEVNAME ! Name of output file.

      INTEGER, SAVE :: OUTDEV    ! FORTRAN unit number for neg conc ascii file

      LOGICAL ::  RDONLY = .FALSE.
      LOGICAL ::  FMTTED = .TRUE.
      INTEGER     STATUS       !  ENVSTR status

      INTEGER     S, V, L, C, R
      REAL( 8 ) :: DBL_CKSUM
      REAL         GC_CKSUM, AE_CKSUM, NR_CKSUM, TR_CKSUM
!     REAL, SAVE :: GCELLS
      REAL, SAVE :: LCELLS

      LOGICAL, SAVE :: OPFLG = .TRUE.               ! open file flag
      LOGICAL     EXFLG                             ! write header flag

C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         LOGDEV = INIT3()

!        GCELLS = SUBST_GLOBAL_SUM ( FLOAT( MY_NCOLS * MY_NROWS * NLAYS ) )
         LCELLS = FLOAT( MY_NCOLS * MY_NROWS * NLAYS )

!        CALL ENVSTR( OUTNAME, 'Floor file', PNAME, DEVNAME, STATUS )
!        IF ( STATUS .NE. 0 ) WRITE( LOGDEV,'(5X, A)' ) 'Floor file'
!        IF ( STATUS .EQ. 1 ) THEN
!           XMSG = 'Environment variable improperly formatted'
!           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
!        ELSE IF ( STATUS .EQ. -1 ) THEN
!           XMSG = 'Environment variable set, but empty ... Using default:'
!           WRITE( LOGDEV,'(5X, A, I9)' ) XMSG, JTIME
!        ELSE IF ( STATUS .EQ. -2 ) THEN
!           XMSG = 'Environment variable not set ... Using default:'
!           WRITE( LOGDEV,'(5X, A, I9)' ) XMSG, JTIME
!        END IF
         CALL NAMEVAL ( APPL, EQNAME )
         WRITE( CMYPE,'(I3.3)' ) MYPE
         DEVNAME = PRESTR // CMYPE // '.' // TRIM( EQNAME )

      END IF

      EXFLG = .TRUE.

      DBL_CKSUM = 0.0
      IF ( N_GC_SPC .GT. 0 ) THEN

         V = 0
         DO S = GC_STRT, GC_STRT - 1 + N_GC_SPC
            V = V + 1
            DO L = 1, NLAYS
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     DBL_CKSUM = DBL_CKSUM + CGRID( C,R,L,S )
                     IF ( CGRID( C,R,L,S ) .LT. CMIN ) THEN
                        IF ( EXFLG ) THEN
                           EXFLG = .FALSE.
                           IF ( OPFLG ) THEN   ! open output ASCII file
                              OPFLG = .FALSE.
                              OUTDEV = GETEFILE ( DEVNAME, RDONLY, FMTTED, PNAME )
                           END IF
                           WRITE( OUTDEV,1001 ) CMIN, SCIPROC
                        END IF
                        WRITE( OUTDEV,1003 ) JDATE, JTIME, C, R, L, S,
     &                                       GC_SPC( V ), CGRID( C,R,L,S )
                        CGRID( C,R,L,S ) = CMIN
                     END IF
                  END DO
               END DO
            END DO
         END DO

         GC_CKSUM = DBL_CKSUM
 
!        WRITE( LOGDEV,1005 ) 'Gas chemistry', SCIPROC, CKSUM

      ELSE

         GC_CKSUM = 0.0
 
      END IF
     
      DBL_CKSUM = 0.0
      IF ( N_AE_SPC .GT. 0 ) THEN

         V = 0
         DO S = AE_STRT, AE_STRT - 1 + N_AE_SPC
            V = V + 1
            DO L = 1, NLAYS
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     DBL_CKSUM = DBL_CKSUM + CGRID( C,R,L,S )
                     IF ( CGRID( C,R,L,S ) .LT. CMIN ) THEN
                        IF ( EXFLG ) THEN
                           EXFLG = .FALSE.
                           IF ( OPFLG ) THEN   ! open output ASCII file
                              OPFLG = .FALSE.
                              OUTDEV = GETEFILE ( DEVNAME, RDONLY, FMTTED, PNAME )
                           END IF
                           WRITE( OUTDEV,1001 ) CMIN, SCIPROC
                        END IF
                        WRITE( OUTDEV,1003 ) JDATE, JTIME, C, R, L, S,
     &                                       AE_SPC( V ), CGRID( C,R,L,S )
                        CGRID( C,R,L,S ) = CMIN
                     END IF
                  END DO
               END DO
            END DO
         END DO

         AE_CKSUM = DBL_CKSUM
  
!        WRITE( LOGDEV,1005 ) '      Aerosol', SCIPROC, CKSUM

      ELSE

         AE_CKSUM = 0.0
 
      END IF

      DBL_CKSUM = 0.0
      IF ( N_NR_SPC .GT. 0 ) THEN
 
         V = 0
         DO S = NR_STRT, NR_STRT - 1 + N_NR_SPC
            V = V + 1
            DO L = 1, NLAYS
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     DBL_CKSUM = DBL_CKSUM + CGRID( C,R,L,S )
                     IF ( CGRID( C,R,L,S ) .LT. CMIN ) THEN
                        IF ( EXFLG ) THEN
                           EXFLG = .FALSE.
                           IF ( OPFLG ) THEN   ! open output ASCII file
                              OPFLG = .FALSE.
                              OUTDEV = GETEFILE ( DEVNAME, RDONLY, FMTTED, PNAME )
                           END IF
                           WRITE( OUTDEV,1001 ) CMIN, SCIPROC
                        END IF
                        WRITE( OUTDEV,1003 ) JDATE, JTIME, C, R, L, S,
     &                                       NR_SPC( V ), CGRID( C,R,L,S )
                        CGRID( C,R,L,S ) = CMIN
                     END IF
                  END DO
               END DO
            END DO
         END DO

         NR_CKSUM = DBL_CKSUM
  
!        WRITE( LOGDEV,1005 ) ' Non-reactive', SCIPROC, CKSUM

      ELSE

         NR_CKSUM = 0.0
 
      END IF
     
      DBL_CKSUM = 0.0
      IF ( N_TR_SPC .GT. 0 ) THEN
 
         V = 0
         DO S = TR_STRT, TR_STRT - 1 + N_TR_SPC
            V = V + 1
            DO L = 1, NLAYS
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     DBL_CKSUM = DBL_CKSUM + CGRID( C,R,L,S )
                     IF ( CGRID( C,R,L,S ) .LT. CMIN ) THEN
                        IF ( EXFLG ) THEN
                           EXFLG = .FALSE.
                           IF ( OPFLG ) THEN   ! open output ASCII file
                              OPFLG = .FALSE.
                              OUTDEV = GETEFILE ( DEVNAME, RDONLY, FMTTED, PNAME )
                           END IF
                           WRITE( OUTDEV,1001 ) CMIN, SCIPROC
                        END IF
                        WRITE( OUTDEV,1003 ) JDATE, JTIME, C, R, L, S,
     &                                       TR_SPC( V ), CGRID( C,R,L,S )
                        CGRID( C,R,L,S ) = CMIN
                     END IF
                  END DO
               END DO
            END DO
         END DO

         TR_CKSUM = DBL_CKSUM
  
!        WRITE( LOGDEV,1005 ) '       Tracer', SCIPROC, CKSUM
 
      END IF
     
      IF ( N_TR_SPC .EQ. 0 ) THEN
         WRITE( LOGDEV,1005 ) SCIPROC,
!    &                        SUBST_GLOBAL_SUM( GC_CKSUM ) / GCELLS,
!    &                        SUBST_GLOBAL_SUM( AE_CKSUM ) / GCELLS,
!    &                        SUBST_GLOBAL_SUM( GC_CKSUM ) / GCELLS,
     &                        GC_CKSUM / LCELLS,
     &                        AE_CKSUM / LCELLS,
     &                        NR_CKSUM / LCELLS
      ELSE
         WRITE( LOGDEV,1007 ) SCIPROC,
!    &                        SUBST_GLOBAL_SUM( GC_CKSUM ) / GCELLS,
!    &                        SUBST_GLOBAL_SUM( AE_CKSUM ) / GCELLS,
!    &                        SUBST_GLOBAL_SUM( NR_CKSUM ) / GCELLS,
!    &                        SUBST_GLOBAL_SUM( TR_CKSUM ) / GCELLS
     &                        GC_CKSUM / LCELLS,
     &                        AE_CKSUM / LCELLS,
     &                        NR_CKSUM / LCELLS,
     &                        TR_CKSUM / LCELLS
      END IF

      RETURN

1001  FORMAT(  5X, 'Concentrations less than, but reset to', 1PE11.3,
     &         1X, 'in', A16
     &       / 9X, 'Date:Time',
     &         5X, 'Col', 2X, 'Row', 1X, 'Layer', 1X, 'Species',
     &         13X, 'Value before reset' )

c003  FORMAT( 1X, 4I7, 9X, 1PE11.3)
1003  FORMAT( 5X, I8, ':', I6.6, 4I5, 1X, '(', A16, ')', 1PE12.3)

c005  FORMAT( 3X, A13, 1X, 'Conc field cksum after science process:',
1005  FORMAT( 1X, 'after',
     &        1X, A09, 1X, 'G ', 1PE14.7,
     &                 1X, 'A ', 1PE14.7,
     &                 1X, 'N ', 1PE14.7 )

1007  FORMAT( 1X, 'after',
     &        1X, A09, 1X, 'G ', 1PE14.7,
     &                 1X, 'A ', 1PE14.7,
     &                 1X, 'N ', 1PE14.7,
     &                 1X, 'T ', 1PE14.7 )

      END
