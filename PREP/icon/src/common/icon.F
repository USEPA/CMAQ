
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
C $Header: /project/yoj/arc/ICON/src/common/icon.F,v 1.3 2011/11/01 16:07:15 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

      PROGRAM ICON   

C***********************************************************************
 
C Function: To produce a Models-3 CTM Initial Concentration file.
C           Valid input options include: 
C           1) a user specified ASCII file of vertical profiles,
C           2) a CMAQ CTM output file,
C           3) subroutines that produce catalogued test pattern species concs.
              
C Preconditions: None
  
C Key Subroutines/Functions Called: INIC
 
C Revision History:
C    Created by Jerry Gipson, January, 1998; based on the original Models-3
C                                            prototype.
C    Modified 4/22/99 by JG to provide default treatment for missing SDATE
C                      and STIME
C    12/13/99 David Wong, LM -- parallelelized the code
C    01/24/02 Steve Howard (Jeff Young) - dynamic allocation
C    12/13/04 J.Young: vert dyn alloc - Use VGRD_DEFN
C    24 May 06 J.Young: par_init/pio_init col/row order check
C    06 Jun 11 J.Young: Replaced I/O API include files with UTILIO_DEFN
C    13 Jun 11 J.Young: Convert for Namelist redesign
C    21 May 12 J.Young: Replaced IC_PARMS include file with an F90 module
C    02 Nov 18 S.Roselle: Removed species mapping; updates to enable runtime
C                     option for profile, regrid, or test patterns BCs
C    07 Nov 18 S.Roselle: Removed parallel processing code; 
C                         replaced UTILIO_DEFN with M3UTILIO
                  
C***********************************************************************

      USE HGRD_DEFN     ! Module to store and load the horizontal grid variables
      USE VGRD_DEFN     ! vertical layer specifications
      USE M3UTILIO      ! IOAPI module
      USE IC_PARMS      ! ICON parameters

      IMPLICIT NONE       

C Include Files: None

C Arguments: None

C Parameters: None

C External Procedures: None

C Local Variables:
      CHARACTER( 16 ) :: PNAME = 'ICON'          ! Procedure name
      CHARACTER( 16 ) :: ICON_TYPE = 'ICON_TYPE' ! ICON type environment variable
      CHARACTER( 16 ) :: IC_MODULE = 'regrid'    ! selected module

      CHARACTER( 80 ) :: MSG                    ! Log message 
      CHARACTER( 80 ) :: VARDESC                ! variable description

      INTEGER    LOGUNIT      ! Unit number for output log
      INTEGER    STATUS       ! Status code

      INTERFACE

         SUBROUTINE M3_INIC ( LOGUNIT )
            INTEGER, INTENT( IN )    :: LOGUNIT
         END SUBROUTINE M3_INIC

         SUBROUTINE PROF_INIC ( LOGUNIT )
            INTEGER, INTENT( IN ) :: LOGUNIT
         END SUBROUTINE PROF_INIC

!        SUBROUTINE TRAC_INIC ( LOGUNIT )
!           INTEGER, INTENT( IN ) :: LOGUNIT
!        END SUBROUTINE TRAC_INIC

      END INTERFACE
C***********************************************************************

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Initialize IO/API and get environment variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      LOGUNIT = INIT3( )

      VARDESC = 'Initial condition type: (regrid, profile)'
      CALL ENVSTR ( ICON_TYPE, VARDESC, IC_MODULE, IC_MODULE, STATUS )
      IF ( STATUS .NE. 0 ) WRITE( LOGUNIT, '(5X, A)' ) VARDESC
      IF ( STATUS .EQ. 1 ) THEN
         MSG = 'Environment variable improperly formatted'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
      ELSE IF ( STATUS .EQ. -1 ) THEN
         MSG = 'Environment variable set, but empty ... Using default:'
         WRITE( LOGUNIT, '(5X, A, A)' ) MSG, IC_MODULE
      ELSE IF ( STATUS .EQ. -2 ) THEN
         MSG = 'Environment variable not set ... Using default:'
         WRITE( LOGUNIT, '(5X, A, A)' ) MSG, IC_MODULE
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Load horizontal grid variables calling GRID_DEF->HGRD_INIT function
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( .NOT. HGRD_INIT() ) THEN
         MSG = '*** Failure defining horizontal domain'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c define vertical layer structure
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( .NOT. VGRD_INIT() ) THEN
         MSG = '*** Failure defining vertical structure'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Generate the IC output file and quit
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( IC_MODULE .EQ. 'regrid' ) THEN
         CALL M3_INIC ( LOGUNIT )
      ELSE IF ( IC_MODULE .EQ. 'profile' ) THEN
         CALL PROF_INIC ( LOGUNIT )
!     ELSE IF ( IC_MODULE .EQ. 'patterns' ) THEN
!        CALL TRAC_INIC ( LOGUNIT )
      ELSE
         MSG = '*** Unknown ICON type requested: ' // IC_MODULE
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
      END IF

      IF ( SHUT3() ) THEN
         WRITE ( LOGUNIT,92999 )
     &  '>>---->  Program  ICON completed successfully  <----<< '
      ELSE
         WRITE ( LOGUNIT,91000 )
     &      'FATAL ERROR shutting down Models-3 I/O'
      END IF

      STOP

C************************* FORMAT STATEMENTS ***************************

92999 FORMAT ( //5X, A, // )

91000 FORMAT ( //5X , '*** ERROR ABORT in program ICON ***',
     &            /5X , A , // )        !  generic error message format

      END
