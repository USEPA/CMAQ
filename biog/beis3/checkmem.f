
C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/biog/beis3/checkmem.f,v 1.2 2011/04/01 15:41:29 sjr Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE CHECKMEM( MSTATUS, AVAR, CALLER )
 
C-----------------------------------------------------------------------
 
C  Description:
C       Reports an error and exits if memory status flag is non-zero.
 
C  Preconditions:
 
C  Subroutines and Functions Called:
 
C  Revision History:
C       Adapted 10/98 by M Houyoux
C       02/11: S.Roselle-Removed deprecated TRIMLEN

C-----------------------------------------------------------------------
C Modified from:
 
C Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling
C             System
C File: @(#)$Id: checkmem.f,v 1.2 2011/04/01 15:41:29 sjr Exp $
C COPYRIGHT (C) 1999, MCNC--North Carolina Supercomputing Center
C All Rights Reserved
C See file COPYRIGHT for conditions of use.
C Environmental Programs Group
C MCNC--North Carolina Supercomputing Center
C P.O. Box 12889
C Research Triangle Park, NC  27709-2889
C env_progs@mcnc.org
C Pathname: $Source: /project/yoj/arc/CCTM/src/biog/beis3/checkmem.f,v $
C Last updated: $Date: 2011/04/01 15:41:29 $ 
 
C-----------------------------------------------------------------------
 
      IMPLICIT NONE

C Arguments:

      INTEGER           MSTATUS  ! ALLOCATE function exit status
      CHARACTER( * ) :: AVAR     ! Variable name of ALLOCATE statement
      CHARACTER( * ) :: CALLER   ! Name of calling procedure

C External Functions:

C Local Variables;
      INTEGER       L1
      INTEGER       L2
      CHARACTER( 256 ) :: MESG = ' '

C-----------------------------------------------------------------------

C Abort if memory status is non-zero

      IF ( MSTATUS .GT. 0 ) THEN           
         L1 = LEN_TRIM( AVAR )
         L2 = LEN_TRIM( CALLER )
         MESG = 'Failure allocating memory for "' // AVAR( 1:L1 )
     &        // '" variable'
         CALL M3EXIT( CALLER( 1:L2 ), 0, 0, MESG, 2 )
      END IF

      RETURN

      END

