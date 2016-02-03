
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
C $Header: /project/yoj/arc/PROCAN/src/driver/pa/pa_init.F,v 1.5 2011/11/02 14:33:59 yoj Exp $

      SUBROUTINE PA_INIT

C***********************************************************************
 
C Function: To initialize data for processing Process Analysis commands 
 
C Preconditions: None
  
C Key Subroutines/Functions Called: None
 
C Revision History:
C  Prototype created by Jerry Gipson, May, 1997
C  Modified Sept, 1997 by Jerry Gipson to be consistent with targeted CTM
C  Aug 2011 Jeff Young: Replaced I/O API include files with IOAPI's M3UTILIO
 
C***********************************************************************

      USE M3UTILIO      ! IOAPI module
      USE PA_GLOBAL     ! Mech and Grid data used 

      IMPLICIT NONE
      
C Includes:
      INCLUDE 'PA_VARS.EXT'
      
C Arguments: None
                                        
C Parameters: None

C External Functions: None

C Local Variables:
      CHARACTER(  5 )  :: TZ         ! Time zone label
      CHARACTER( 24 )  :: TIMNDAT    ! Wall clock time and date

      CHARACTER(  16 ) :: PNAME = 'PA_INIT'   ! Procedure name
      CHARACTER( 132 ) :: MSG     ! Error message

      INTEGER          :: IDATE      ! Wall-clock date
      INTEGER          :: ITIME      ! Wall clock time

C***********************************************************************

C Set CGRID mechanism
      IF ( .NOT. CGRID_SPCS_INIT() ) THEN
         MSG = 'Error in CGRID_GC_SPCS:CGRID_SPCS_INIT'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the time and date to be used as the time stamp for all outputs;
c  Convert from GMT to EDT or EST
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL GETDTTIME( IDATE, ITIME )

      IF ( ISDSTIME( IDATE ) ) THEN
         CALL NEXTIME( IDATE, ITIME, -40000 )
         TZ = ' EDT '
      ELSE
         CALL NEXTIME( IDATE, ITIME, -50000 )
         TZ = ' EST '
      END IF

      TIMNDAT = DT2STR( IDATE, ITIME )
      TDATOUT = TIMNDAT( 1:8 ) // TZ // TIMNDAT( 10:24 )   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Initialize some key variables and return
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      NFAMLYS    = 0
      NRXSUMS    = 0
      NCYCLES    = 0
      NIRROUT    = 0
      N_IPR_SPC  = 0

      RETURN 
      END
