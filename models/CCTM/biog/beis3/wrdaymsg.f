
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
C $Header: /project/yoj/arc/CCTM/src/biog/beis3/wrdaymsg.f,v 1.3 2011/10/21 16:10:18 yoj Exp $

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
C File: @(#)$Id: wrdaymsg.f,v 1.3 2011/10/21 16:10:18 yoj Exp $
C COPYRIGHT (C) 2002, MCNC Environmental Modeling Center
C All Rights Reserved
C See file COPYRIGHT for conditions of use.
C Environmental Modeling Center
C MCNC
C P.O. Box 12889
C Research Triangle Park, NC  27709-2889
C smoke@emc.mcnc.org
C Pathname: $Source: /project/yoj/arc/CCTM/src/biog/beis3/wrdaymsg.f,v $
C Last updated: $Date: 2011/10/21 16:10:18 $ 
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

