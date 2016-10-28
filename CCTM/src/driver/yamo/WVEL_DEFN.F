
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
C $Header: /project/yoj/arc/CCTM/src/driver/yamo/WVEL_DEFN.F,v 1.4 2011/10/21 16:10:42 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE WVEL_DEFN

C Function: capture the derived vertical velocity component to write to the
C           conc file

      IMPLICIT NONE

      REAL, ALLOCATABLE, SAVE :: WVEL( :,:,: )
      LOGICAL, SAVE :: W_VEL

      CONTAINS

         FUNCTION WVEL_INIT () RESULT ( SUCCESS )

         USE GRID_CONF             ! horizontal & vertical domain specifications
         USE UTILIO_DEFN

         LOGICAL SUCCESS

         CHARACTER( 16 ) :: PNAME = 'WVEL_INIT'
         CHARACTER( 16 ) :: CTM_WVEL = 'CTM_WVEL'
         CHARACTER( 96 ) :: XMSG = ' '

         INTEGER ALLOCSTAT, IERR
         INTEGER :: JDATE = 0, JTIME = 0

C-----------------------------------------------------------------------

!        logdev = setup_logdev ()

         SUCCESS = .TRUE.

         W_VEL = .FALSE.         ! default
         W_VEL = ENVYN( CTM_WVEL, 'Write vert. vel. flag', W_VEL, IERR )
         IF ( IERR .GT. 0 ) THEN
            XMSG = 'Environment variable improperly formatted'
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
            END IF

         IF ( W_VEL ) THEN

            ALLOCATE ( WVEL( MY_NCOLS,MY_NROWS,NLAYS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'WVEL memory allocation failed'
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
               END IF

            WVEL = 0.0

            END IF

         END FUNCTION WVEL_INIT

      END MODULE WVEL_DEFN
