
C***************************************************************************
C  Significant portions of Models-3/CMAQ software were developed by        *
C  Government employees and under a United States Government contract.     *
C  Portions of the software were also based on information from non-       *
C  Federal sources, including software developed by research institutions  *
C  through jointly funded cooperative agreements. These research institu-  *
C  tions have given the Government permission to use, prepare derivative   *
C  works, and distribute copies of their work to the public within the     *
C  Models-3/CMAQ software release and to permit others to do so. EPA       *
C  therefore grants similar permissions for use of Models-3/CMAQ software, *
C  but users are requested to provide copies of derivative works to the    *
C  Government without re-strictions as to use by others.  Users are        *
C  responsible for acquiring their own copies of commercial software       *
C  associated with the Models-3/CMAQ release and are also responsible      *
C  to those vendors for complying with any of the vendors' copyright and   *
C  license restrictions. In particular users must obtain a Runtime license *
C  for Orbix from IONA Technologies for each CPU used in Models-3/CMAQ     *
C  applications.                                                           *
C                                                                          *
C  Portions of I/O API, PAVE, and the model builder are Copyrighted        *
C  1993-1997 by MCNC--North Carolina Supercomputing Center and are         *
C  used with their permissions subject to the above restrictions.          *
C***************************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header$

C @(#)GETLABEL.F	1.1 /project/mod3/MECH/src/driver/mech/SCCS/s.GETLABEL.F 02 Jan 1997 15:26:43

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)GETLABEL.F	1.1 /project/mod3/MECH/src/driver/mech/SCCS/s.GETLABEL.F 02 Jan 1997 15:26:43

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE GETLABEL ( IMECH, INBUF, LPOINT, IEOL, CHR, LABEL )

C=======================================================================
C Returns a label that consists of an extended alphanumeric character 
C string no longer than MAXLEN bytes and delimited by "<", ">".
C The label string can span lines, may have embedded comments, and
C may contain up to 81 characters, excluding comments.
C GETLABEL removes embedded blank characters.
C Valid string characters - All printable characters EXCEPT:
C ASCII char range:      contiguous (decimal) range:
C      '<'                    60           label string delimiter
C      '>'                    62           label string delimiter
C      '{'                    123          comment delimiter
C      '}'                    125          comment delimiter
C      '(', ')'               40-41        comment delimiters
C input: IMECH (for RDLINE), INBUF, LPOINT, CHR
C output: WORD
C updates: INBUF, LPOINT, IEOL, CHR
C precondition:  RDLINE and GETCHAR must have been called and the 1st
C                non-comment character found was "<"
C=======================================================================
      USE MECHANISM_DATA
      
      IMPLICIT NONE
! Arguments
      INTEGER,         INTENT( IN )    :: IMECH
      CHARACTER*( * ), INTENT( INOUT ) :: INBUF
      INTEGER,         INTENT( INOUT ) :: IEOL, LPOINT
      CHARACTER*( * ), INTENT( INOUT ) :: CHR
      CHARACTER*( * ), INTENT( INOUT ) :: LABEL

! Local
      INTEGER, PARAMETER   :: MAXLEN = 16
      CHARACTER( MAXLEN )  :: BLANK = ' '
      CHARACTER( 81 )      :: STRBUF 
      INTEGER              :: LENSTR
      LOGICAL              :: VALLABCHR

      INTERFACE 
        SUBROUTINE RDLINE ( IMECH, INBUF, LPOINT, IEOL )
         CHARACTER*( * ), INTENT( INOUT ) :: INBUF
         INTEGER,         INTENT( IN )    :: IMECH
         INTEGER,         INTENT( INOUT ) :: IEOL, LPOINT
        END SUBROUTINE RDLINE
        SUBROUTINE GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
         INTEGER,         INTENT( IN )    :: IMECH
         CHARACTER*( * ), INTENT( INOUT ) :: INBUF
         INTEGER,         INTENT( INOUT ) :: IEOL, LPOINT
         CHARACTER*( * ), INTENT( INOUT ) :: CHR
        END SUBROUTINE GETCHAR
      END INTERFACE

C eat the start-of-string delimiter ('<')
      CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
      LABEL = BLANK
      LENSTR = 0
101   CONTINUE
      IF ( CHR .EQ. '>' ) GO TO 201  ! end-of-string delimiter ('>')
C check for valid character
      IF ( .NOT. VALLABCHR( CHR ) ) THEN
         WRITE( 6,2001 ) INBUF, CHR
         STOP
      END IF
      LENSTR = LENSTR + 1
      IF ( LENSTR .GT. 81 ) THEN
         WRITE( 6,2003 ) INBUF
         STOP
      END IF
C insert into STRBUF
      STRBUF( LENSTR:LENSTR ) = CHR
C get next non-blank character
      CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
      GO TO 101
201   CONTINUE
      IF ( LENSTR .GT. MAXLEN )THEN
         LENSTR = MAXLEN
	 WRITE( 6,2004 )TRIM(STRBUF),STRBUF( 1:LENSTR )
      END IF	 
      LABEL = STRBUF( 1:LENSTR )
      CALL GETCHAR ( IMECH, INBUF, LPOINT, IEOL, CHR )
      RETURN
2001  FORMAT( / 5X, '*** ERROR: Invalid character in a label'
     &        / 5X, 'Line:' / A81,
     &        / 5X, 'Character:', 2X, A1 )
2003  FORMAT( / 5X, '*** ERROR: label buffer cannot exceed 81 characters:'
     &        / 5X, 'Line:'/ A81 )
2004  FORMAT( / 5X, '*** WARNING: label buffer: ' / A, ' exceeds 16 characters:'
     &        / 5X, '*** Trunicating to :'/ A16 )
      END
