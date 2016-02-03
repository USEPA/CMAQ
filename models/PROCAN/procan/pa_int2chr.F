
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
C $Header: /project/yoj/arc/PROCAN/src/driver/pa/pa_int2chr.F,v 1.5 2011/11/02 14:33:59 yoj Exp $

      CHARACTER*10 FUNCTION INT2CHR( NOUT )

C***********************************************************************
 
C Function: Convert an integer number into character representation
C           that is left justified in a string 
 
C Preconditions: None
  
C Key Subroutines/Functions Called: None
 
C Revision History:
C  Prototype created by Jerry Gipson, August, 1996
C  Modified May, 1997 by Jerry Gipson to be consistent with beta CTM
C  Modified Sept, 1997 by Jerry Gipson to be consistent with targeted CTM
C  Aug 2011 Jeff Young: Replaced I/O API include files with IOAPI's M3UTILIO
 
C***********************************************************************
      USE M3UTILIO      ! IOAPI module

      IMPLICIT NONE
      
C Includes: None
      
C Arguments:
      INTEGER NOUT             ! Integer number to convert

C Parameters: None

C External FUNCTIONS: None
 
C Local Variables:
      CHARACTER( 10 ) :: TEMPSTR  ! Temporary string holder

      INTEGER EPOS1         ! Ending positions
      INTEGER SPOS1         ! Starting positions
      INTEGER LEN1          ! Length of string
      INTEGER N             ! Loop index

C***********************************************************************
      WRITE( TEMPSTR, '( I10 )' ) NOUT
      DO N = 1, 10
         INT2CHR( N:N ) = ' '
      END DO

c..Left justify the numeric string
      SPOS1 = LBLANK( TEMPSTR ) + 1
      EPOS1 = LEN_TRIM( TEMPSTR )
      LEN1 = EPOS1 - SPOS1 + 1
      INT2CHR( 1:LEN1 ) = TEMPSTR( SPOS1:EPOS1 )

      RETURN
      END

