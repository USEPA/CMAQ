
!-----------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in    !
!  continuous development by various groups and is based on information !
!  from these groups: Federal Government employees, contractors working !
!  within a United States Government contract, and non-Federal sources  !
!  including research institutions.  These groups give the Government   !
!  permission to use, prepare derivative works of, and distribute copies!
!  of their work in the CMAQ system to the public and to permit others  !
!  to do so.  The United States Environmental Protection Agency         !
!  therefore grants similar permission to use the CMAQ system software, !
!  but users are requested to provide copies of derivative works or     !
!  products designed to operate in the CMAQ system to the United States !
!  Government without restrictions as to use by others.  Software       !
!  that is used with the CMAQ system but distributed under the GNU      !
!  General Public License or the GNU Lesser General Public License is   !
!  subject to their copyright restrictions.                             !
!-----------------------------------------------------------------------!


C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/JPROC/src/driver/jproc_table/index2.f,v 1.4 2011/10/29 01:03:53 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)index2.F	1.1 /project/mod3/JPROC/src/driver/jproc_table/SCCS/s.index2.F 23 May 1997 12:44:17

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      INTEGER FUNCTION INDEX2 (NAME1, N, NAME2)     

C***********************************************************************
C
C  FUNCTION:
C
C    This routine searches for NAME1 in list NAME2    
C
C  REVISION HISTORY:
C
C    5/88   Modified for ROMNET
C
C  ARGUMENT LIST DESCRIPTION:
C
C    Input arguments:
C
C      NAME1       Character string being searched for
C      N           Length of array to be searched
C      NAME2       Character array to be searched
C
C    Output arguments:
C
C      INDEX1      The position within the NAME2 array that NAME1 
C                  found.  If string was not found, INDEX1 = 0
C
C  LOCAL VARIABLE DESCRIPTION:
C
C      None
C
C***********************************************************************

      IMPLICIT NONE

      INTEGER       N
      INTEGER       I

      CHARACTER*(*) NAME1
      CHARACTER*(*) NAME2(*)          

C...Assume NAME1 is not in list NAME2    

      INDEX2 = 0

      DO I = 1, N
        IF ( INDEX( NAME2( I ), NAME1 ) .EQ. 1 ) THEN
          INDEX2 = I
          RETURN     
        ENDIF
      END DO 

      RETURN
      END              
