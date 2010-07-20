
C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/biog/beis3/getfline.f,v 1.1 2010/07/20 11:26:47 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      INTEGER FUNCTION GETFLINE( IDEV, DESCRIPT )

C----------------------------------------------------------------------
C Description: 
C    Counts the number of lines in an ASCII file
 
C Preconditions:
C    File opened and unit number provided
 
C Subroutines and Functions Called:  M3EXIT
 
C Revision History:
C      prototype 10/98 by M Houyoux
 
C----------------------------------------------------------------------
C Modified from:

C Project Title: EDSS Tools Library
C File: @(#)$Id: getfline.f,v 1.1 2010/07/20 11:26:47 yoj Exp $
C COPYRIGHT (C) 2004, Environmental Modeling for Policy Development
C All Rights Reserved
C Carolina Environmental Program
C University of North Carolina at Chapel Hill
C 137 E. Franklin St., CB# 6116
C Chapel Hill, NC 27599-6116
C smoke@unc.edu
C Pathname: $Source: /project/yoj/arc/CCTM/src/biog/beis3/getfline.f,v $
C Last updated: $Date: 2010/07/20 11:26:47 $ 
C----------------------------------------------------------------------

      IMPLICIT NONE

C Arguments:

      INTEGER       IDEV         ! Unit number for ASCII file
      CHARACTER(*)  DESCRIPT     ! Description of file

C External Functions:
      INTEGER    TRIMLEN

      EXTERNAL   TRIMLEN

C Local Variables:

      INTEGER       L1           ! length of DESCRIPT
      INTEGER       ICNT         ! line counter
      INTEGER       IOS          ! i/o status

      CHARACTER      BUFFER      ! ASCII LINE from X-ref file
      CHARACTER(256) MESG        ! Message buffer

      CHARACTER(16) :: PROGNAME = 'GETFLINE' ! program name

C----------------------------------------------------------------------

      L1 = TRIMLEN( DESCRIPT )

      REWIND( IDEV )

      ICNT = 0

C Loop through lines of file, counting the lines
11    CONTINUE

          READ( IDEV, 93000, IOSTAT=IOS, END=22 ) BUFFER
 
          ICNT = ICNT + 1
 
          IF ( IOS .GT. 0 ) THEN
              WRITE( MESG,94010 )
     &              'I/O error', IOS,
     &              'scanning ' // DESCRIPT( 1:L1 ) // 
     &              ' file at line', ICNT
              CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
          END IF

      GO TO 11

22    CONTINUE

      GETFLINE = ICNT  

      REWIND( IDEV )

      RETURN

93000 FORMAT( A )
94010 FORMAT( 10( A, :, I6, :, 2X ) )

      END
