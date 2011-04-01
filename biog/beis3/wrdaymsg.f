
C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/biog/beis3/wrdaymsg.f,v 1.2 2011/04/01 15:41:31 sjr Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE WRDAYMSG( JDATE, MESG )

C-----------------------------------------------------------------------
C  Description:
C    Writes a text message to stdout and log file about which day is
C    being processed
C  Preconditions:
C  Subroutines and Functions Called:
C  Revision History:
C    Created 9/99 by M. Houyoux
C    02/11: S.Roselle-Replaced I/O API include files with UTILIO_DEFN
C-----------------------------------------------------------------------
C Modified from:

C Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling System
C File: @(#)$Id: wrdaymsg.f,v 1.2 2011/04/01 15:41:31 sjr Exp $
C COPYRIGHT (C) 2002, MCNC Environmental Modeling Center
C All Rights Reserved
C See file COPYRIGHT for conditions of use.
C Environmental Modeling Center
C MCNC
C P.O. Box 12889
C Research Triangle Park, NC  27709-2889
C smoke@emc.mcnc.org
C Pathname: $Source: /project/yoj/arc/CCTM/src/biog/beis3/wrdaymsg.f,v $
C Last updated: $Date: 2011/04/01 15:41:31 $ 
C-----------------------------------------------------------------------

      USE UTILIO_DEFN

      IMPLICIT NONE

C Includes:

C External Functions:

C Arguments:
      INTEGER       , INTENT(  IN ) :: JDATE    ! Julian date
      CHARACTER( * ), INTENT( OUT ) :: MESG     ! message buffer

C Parameters:
      CHARACTER( 9 ), PARAMETER :: WKDAYS( 7 ) =
     &                 ( / 'Monday   ', 'Tuesday  ', 'Wednesday', 'Thursday ',
     &                     'Friday   ', 'Saturday ', 'Sunday   ' / )

C Local variables:
      INTEGER         DAY          !  day of week number
      INTEGER         L            !  length of day name

      CHARACTER( 16 ) :: PNAME = 'WRDAYMSG' ! procedure name

C-----------------------------------------------------------------------

      DAY = WKDAY( JDATE )

      L = LEN_TRIM( WKDAYS( DAY ) )
      MESG = '>>> Processing ' // WKDAYS( DAY )( 1:L ) // ' ' // MMDDYY( JDATE )
      CALL M3MSG2( MESG )

      RETURN

      END SUBROUTINE WRDAYMSG

