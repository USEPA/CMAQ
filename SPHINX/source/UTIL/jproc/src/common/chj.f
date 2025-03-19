
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
C $Header: /project/yoj/arc/JPROC/src/driver/jproc_table/chj.f,v 1.5 2011/10/29 01:03:52 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)chj.F	1.1 /project/mod3/JPROC/src/driver/jproc_table/SCCS/s.chj.F 23 May 1997 12:44:16

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      FUNCTION CHJ ( ZETA )
      
C**********************************************************************
C
C  Chapman function is used when the solar zenith angle exceeds
C     75 deg., this is the updated RADM2 version (VERSION 89137)
C     interpolates between values given in, e.g., McCartney (1976).
C
C  Edit history:
C
C     04/08/96 - Moved mods from Sasha's new code to this routine
C     01/03/95 - Function has been completely rewritten for  -SF-
C                readability and efficiency
C
C*********************************************************************

      IMPLICIT NONE

C.......ARGUMENTS and descriptions

      REAL          ZETA        ! zenith angle (deg)
      REAL          CHJ         ! chapman function

C.......LOCAL VARIABLES and descriptions
     
      INTEGER       I           ! angle loop index
      
      REAL          RM          ! zenith angle rounded up (deg)
      
      REAL          Y( 21 )     ! 
      DATA Y /   3.800,  4.055,  4.348,   4.687,   5.083, 
     &           5.551,  6.113,  6.799,   7.650,   8.732,
     &          10.144, 12.051, 14.730,  18.686,  24.905,
     &          35.466, 55.211, 96.753, 197.000, 485.000,
     &         1476.000/
      SAVE          Y

C*********************************************************************
C.......begin body of function CHJ

      I = MAX( INT( ZETA ) + 1, 75 )
      RM = FLOAT( I )

      CHJ = Y( I - 75 ) +
     &    ( Y( I - 74 ) - Y( I - 75 ) ) * ( ZETA - ( RM - 1.0 ) )

      RETURN
      END
