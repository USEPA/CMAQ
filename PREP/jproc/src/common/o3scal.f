
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
C $Header: /project/yoj/arc/JPROC/src/driver/jproc_table/o3scal.f,v 1.5 2011/10/29 01:03:53 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)o3scal.F	1.1 /project/mod3/JPROC/src/driver/jproc_table/SCCS/s.o3scal.F 23 May 1997 12:44:20

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
       SUBROUTINE O3SCAL ( O3, HO3, XLAT, DOBNEW )

C*********************************************************************
C
C  adjustment of O3 profiles to a user-selected dobson value.
C   select value of DOBNEW in main program
C   if don't want to use, don't call this subroutine
C
C*********************************************************************

      IMPLICIT NONE
            
      INCLUDE 'JVALPARMS.EXT'    ! jproc parameters

C...........ARGUMENTS and their descriptions

      REAL         O3( MXLEV )        ! ozone profile
      REAL         HO3                ! ozone scale height
      REAL         XLAT               ! latitudinal band
      REAL         DOBNEW             ! new dobson unit

C...........LOCAL VARIABLES and their descriptions:

      LOGICAL, SAVE :: FIRSTTIME = .TRUE.
     
      INTEGER      I                  ! level index
     
      REAL         DOBSREF            ! reference dobson unit
      REAL         SCALE_FACTOR
               
C*********************************************************************
C     begin body of subroutine O3SCAL2

      IF ( FIRSTTIME ) THEN
        FIRSTTIME = .FALSE.
        WRITE ( 6, 2001 )
      END IF

C...convert O3 value into DU

      DOBSREF = O3( MXLEV ) * 1.0E5 * HO3
      DO I = 1, MXLEV
        DOBSREF = DOBSREF + O3( I ) * 1.0E5
      END DO
      DOBSREF = DOBSREF / 2.687E16

C...apply scale factor to O3 profile if a non-zero TOC value provided

      IF ( DOBNEW .GT. 0.0 ) THEN
        SCALE_FACTOR = DOBNEW / DOBSREF
        WRITE( 6, 2003 ) XLAT, DOBSREF, DOBNEW, SCALE_FACTOR
        DO I = 1, MXLEV
          O3( I ) = O3( I ) * SCALE_FACTOR
        END DO
      ELSE
        WRITE( 6, 2005 ) XLAT, DOBSREF
      END IF

C...formats

2001  FORMAT( 1X, 'Rescaling O3 profile based on TOC data' )
2003  FORMAT( 7X, 'Latitude:', 1X, F5.1, ', oldDOBS=', F9.4, ', newDOBS=',
     &        F9.4, ', scale factor=', F7.4 )
2005  FORMAT( 7X, 'Latitude:', 1X, F5.1, ', DOBS=', F9.4, ', not rescaled' )

      RETURN
      END
