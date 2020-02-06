
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
C $Header: /project/yoj/arc/ICON/src/profile/prof_driver.F,v 1.2 2011/10/21 16:41:56 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%


      SUBROUTINE PROF_INIC( LOGUNIT )   

C***********************************************************************
 
C  Function: Opens the input profile data file, and calls the routine to 
C             compute the ICs and write the output IC file  
              
C  Preconditions: None
  
C  Key Subroutines/Functions Called: ICOUT
 
C  Revision History: Prototype created by Jerry Gipson, January, 1998
C                    Modified April 1998 by JG to remove ppb option for 
C                       input profile file
C  06 Jun 11 J.Young: Replaced I/O API include files with UTILIO_DEFN
C  07 Jun 11 J.Young: Removed RADM2<->CB4 mechanism conversion feature
C  13 Jun 11 J.Young: Convert for Namelist redesign
C  21 May 12 J.Young: Replaced IC_PARMS include file with an F90 module
C  02 Nov 18 S.Roselle: Removed species mapping
C  06 Nov 18 S.Roselle: Replaced UTILIO_DEFN with M3UTILIO

C***********************************************************************

      USE M3UTILIO    ! IOAPI module
      USE IC_PARMS    ! ICON parameters

      IMPLICIT NONE     

C Arguments: 
      INTEGER, INTENT( IN ) :: LOGUNIT   ! Unit number for output log

C Parameters: None

C Local Variables:
      CHARACTER(  16 ) :: ENV_DFLT = ' ' ! Environment variable default value
      CHARACTER(  80 ) :: ENV_DESC       ! Environment variable description
      CHARACTER(  80 ) :: MSG            ! Log message
      CHARACTER(  16 ) :: PNAME = 'PROF_INIC'       ! Procedure name
      CHARACTER( 256 ) :: PROF_FL_NAME   ! Input profile file name
      CHARACTER(  16 ) :: IC_PROFILE = 'IC_PROFILE' ! Logical name of input profile file
  
      INTEGER STATUS       ! Status code

      INTERFACE

         SUBROUTINE PROF_ICOUT ( LOGUNIT, PROF_FL_NAME )
            INTEGER, INTENT( IN ) :: LOGUNIT
            CHARACTER( 256 ), INTENT( IN ) :: PROF_FL_NAME
         END SUBROUTINE PROF_ICOUT

      END INTERFACE

C***********************************************************************

      WRITE( LOGUNIT, 92000 )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Open the input profile file, read the number of species and their 
c  names, and list the file on the output log
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ENV_DESC = 'Input IC Profile file'        
      CALL ENVSTR ( IC_PROFILE, ENV_DESC, ENV_DFLT, PROF_FL_NAME, STATUS )

      IF ( STATUS .NE. 0 ) THEN
         MSG = 'No IC profile file assigned to IC_PROFILE'
         CALL M3ERR ( PNAME, 0, 0, MSG, .TRUE. )
      END IF

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Compute ICs from the input PROFILE and output
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL PROF_ICOUT ( LOGUNIT, PROF_FL_NAME )

      RETURN

C************************* FORMAT STATEMENTS ***************************

92000 FORMAT( // 1X, 79( '#' ) 
     &         / 1X, '#  Input file section '
     &         / 1X, 79( '#' )
     &        // 5X, 'Initial Concentrations from an input IC profile ',
     &               'file.' )

      END
