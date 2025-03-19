
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
C $Header: /project/yoj/arc/JPROC/src/driver/jproc_table/setalb.f,v 1.5 2011/10/29 01:03:55 sjr Exp $ 

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE SET_ALBEDO ( NWL, MIDWL, ALBEDO )

C*********************************************************************
C
C  Set the albedo of the surface. 
C    Use best estimate albedo of Demerjian et al.,
C    Adv.Env.Sci.Tech.,v.10,p.369, (1980)
C
C*********************************************************************

      IMPLICIT NONE

      INCLUDE 'JVALPARMS.EXT'    ! jproc parameters

C...........ARGUMENTS and their descriptions

      INTEGER      NWL                ! number of wavelength bands

      REAL         MIDWL ( MXWL )     ! wavelength band midpoints
      REAL         ALBEDO( MXWL )     ! ground albedo

C...........LOCAL VARIABLES and their descriptions:

      INTEGER      IWL                ! wavelength index

C*********************************************************************
C     begin body of subroutine SET_ALBEDO

      DO IWL = 1, NWL

        IF ( MIDWL( IWL ) .LT. 400.0 ) THEN
          ALBEDO( IWL ) = 0.05
        ELSE IF (( MIDWL( IWL ) .GE. 400.0 ) .AND.
     &           ( MIDWL( IWL ) .LT. 450.0 )) THEN
          ALBEDO( IWL ) = 0.06
        ELSE IF (( MIDWL( IWL ) .GE. 450.0 ) .AND.
     &           ( MIDWL( IWL ) .LT. 500.0 )) THEN
          ALBEDO( IWL ) = 0.08
        ELSE IF (( MIDWL( IWL ) .GE. 500.0 ) .AND.
     &           ( MIDWL( IWL ) .LT. 550.0 )) THEN
          ALBEDO( IWL ) = 0.10
        ELSE IF (( MIDWL( IWL ) .GE. 550.0 ) .AND.
     &           ( MIDWL( IWL ) .LT. 600.0 )) THEN
          ALBEDO( IWL ) = 0.11
        ELSE IF (( MIDWL( IWL ) .GE. 600.0 ) .AND.
     &           ( MIDWL( IWL ) .LT. 640.0 )) THEN
          ALBEDO( IWL ) = 0.12
        ELSE IF (( MIDWL( IWL ) .GE. 640.0 ) .AND.
     &           ( MIDWL( IWL ) .LT. 660.0 )) THEN
          ALBEDO( IWL ) = 0.135
        ELSE IF ( MIDWL( IWL ) .GE. 660.0 ) THEN
          ALBEDO( IWL ) = 0.15
        END IF

      END DO

      RETURN
      END
