
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
C $Header: /project/yoj/arc/CCTM/src/util/util/cksummer.F,v 1.5 2012/01/19 15:34:21 yoj Exp $ 

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
C                      GLOBAL_RSUM -> Dave Wong`s f90 stenex GLOBAL_SUM
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
C    9 Apr 15 J.Bash: Use F90 MINVAL to speed up execution
C   18 Mar 16 J.Young: Add "INTENT" attribute; remove commented out code;
C                      rearrange logic for efficiency
C-----------------------------------------------------------------------

      USE GRID_CONF           ! horizontal & vertical domain specifications
      USE CGRID_SPCS          ! CGRID mechanism species
      USE UTILIO_DEFN
#ifdef parallel
      USE SE_MODULES          ! stenex (using SE_GLOBAL_SUM_MODULE)
#else
      USE NOOP_MODULES        ! stenex (using NOOP_GLOBAL_SUM_MODULE)
#endif

      IMPLICIT NONE

C Arguments:

      CHARACTER( * ), INTENT( IN ) :: SCIPROC  ! science process name
      REAL, POINTER :: CGRID( :,:,:,: )
      INTEGER, INTENT( IN ) :: JDATE        ! current model date, coded YYYYDDD
      INTEGER, INTENT( IN ) :: JTIME        ! current model time, coded HHMMSS

C Parameters:

      REAL, PARAMETER :: CMIN = 0.0

C Local variables:
 
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      INTEGER, SAVE :: LOGDEV         ! FORTRAN unit number for log file

      CHARACTER( 16 ), SAVE :: PNAME = 'CKSUMMER'
      !CHARACTER( 96 ) :: XMSG = ' '
      CHARACTER(  8 ) :: APPL = 'CTM_APPL'
      CHARACTER( 48 ) :: EQNAME
      CHARACTER(  6 ) :: PRESTR = 'FLOOR_'
      CHARACTER(  3 ) :: CMYPE
      CHARACTER( 96 ), SAVE :: DEVNAME ! Name of output file.

      INTEGER, SAVE :: OUTDEV    ! FORTRAN unit number for neg conc ascii file

      LOGICAL ::  RDONLY = .FALSE.
      LOGICAL ::  FMTTED = .TRUE.
      !INTEGER     STATUS       !  ENVSTR status

      INTEGER     S, V, L, C, R
      REAL( 8 ) :: DBL_CKSUM
      REAL         GC_CKSUM, AE_CKSUM, NR_CKSUM, TR_CKSUM
      REAL, SAVE :: LCELLS

      LOGICAL, SAVE :: OPFLG = .TRUE.               ! open file flag
      LOGICAL     EXFLG                             ! write header flag

C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         LOGDEV = INIT3()

         LCELLS = FLOAT( MY_NCOLS * MY_NROWS * NLAYS )

         CALL NAMEVAL ( APPL, EQNAME )
         WRITE( CMYPE,'(I3.3)' ) MYPE
         DEVNAME = PRESTR // CMYPE // '.' // TRIM( EQNAME )

      END IF

      IF ( MINVAL( CGRID ) .LT. CMIN ) THEN

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
     &                                          GC_SPC( V ), CGRID( C,R,L,S )
                           CGRID( C,R,L,S ) = CMIN
                        END IF
                     END DO
                  END DO
               END DO
            END DO

            GC_CKSUM = REAL( DBL_CKSUM, 4 )
 
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
     &                                          AE_SPC( V ), CGRID( C,R,L,S )
                           CGRID( C,R,L,S ) = CMIN
                        END IF
                     END DO
                  END DO
               END DO
            END DO

            AE_CKSUM = REAL( DBL_CKSUM, 4 )
  
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
     &                                          NR_SPC( V ), CGRID( C,R,L,S )
                           CGRID( C,R,L,S ) = CMIN
                        END IF
                     END DO
                  END DO
               END DO
            END DO

            NR_CKSUM = REAL( DBL_CKSUM, 4 )
  
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
     &                                          TR_SPC( V ), CGRID( C,R,L,S )
                           CGRID( C,R,L,S ) = CMIN
                        END IF
                     END DO
                  END DO
               END DO
            END DO

            TR_CKSUM = REAL( DBL_CKSUM, 4 )
  
         ELSE

            TR_CKSUM = 0.0
 
         END IF

      ELSE   ! MINVAL( CGRID ) .GE. CMIN

         GC_CKSUM = SUM( CGRID( :,:,:,GC_STRT : ( GC_STRT - 1 + N_GC_SPC ) ) )
         AE_CKSUM = SUM( CGRID( :,:,:,AE_STRT : ( AE_STRT - 1 + N_AE_SPC ) ) )
         NR_CKSUM = SUM( CGRID( :,:,:,NR_STRT : ( NR_STRT - 1 + N_NR_SPC ) ) )
         IF ( N_TR_SPC .GT. 0 ) THEN
            TR_CKSUM = SUM( CGRID( :,:,:,TR_STRT : ( TR_STRT - 1 + N_TR_SPC ) ) )
         END IF
     
      END IF

      IF ( N_TR_SPC .EQ. 0 ) THEN
         WRITE( LOGDEV,1005 ) SCIPROC,
     &                        GC_CKSUM / LCELLS,
     &                        AE_CKSUM / LCELLS,
     &                        NR_CKSUM / LCELLS
      ELSE
         WRITE( LOGDEV,1007 ) SCIPROC,
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

1003  FORMAT( 5X, I8, ':', I6.6, 4I5, 1X, '(', A16, ')', 1PE12.3)

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
