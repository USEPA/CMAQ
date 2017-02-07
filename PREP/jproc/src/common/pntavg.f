
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
C $Header: /project/yoj/arc/JPROC/src/driver/jproc_table/pntavg.f,v 1.4 2011/10/29 01:03:53 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)pntavg.F	1.1 /project/mod3/JPROC/src/driver/jproc_table/SCCS/s.pntavg.F 23 May 1997 12:44:22

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE PNTAVG ( WLIN, CQIN,  NWLIN,
     &                    WLOUT1, WLOUT2, CQOUT, NWLOUT )
     
C*********************************************************************
C
C  This program computes the integrated average data for the ET
C    extra terrestrial irradiance wavelength intervals
C
C  History:
C    S.Roselle  6/5/95  Program created
C
C*********************************************************************

      IMPLICIT NONE      
      
      INCLUDE 'JVALPARMS.EXT'    ! jproc parameters

C...........ARGUMENTS and their descriptions

      INTEGER      NWLOUT              ! number of intervals ETin
      INTEGER      NWLIN               ! number of intervals CQin

      REAL         WLIN( MXWLIN )      ! wl of CQin
      REAL         CQIN( MXWLIN )      ! quantity (CS or QY) as f(WLIN)
      REAL         WLOUT1( MXWL )      ! lower limit on wl int ETin
      REAL         WLOUT2( MXWL )      ! upper limit on wl int ETin
      REAL         CQOUT ( MXWL )      ! quantity (CS or QY) as f(WLOUT)

C...........LOCAL VARIABLES and their descriptions:
      
      INTEGER      I                   ! index var
      INTEGER      J                   ! index var
      INTEGER      MXWLPT              ! pointer
      INTEGER      MNWLPT              ! pointer
     
      REAL         DWLIN               ! wl int for CQin
      REAL         CQA                 ! lower cq value
      REAL         CQB                 ! upper cq value

C*********************************************************************
C...begin body of subroutine INTAVG

C...loop through wavelength intervals for output arrays

      DO I = 1, NWLOUT

C...find lower limit on input array

        MNWLPT = 1
        DO J = 1, NWLIN
          IF ( WLIN( J ) .LE. WLOUT1( I ) ) MNWLPT = J
        END DO

C...find upper limit on input array

        MXWLPT = NWLIN
        DO J = NWLIN, 1, -1
          IF ( WLIN( J ) .GE. WLOUT2( I ) ) MXWLPT = J
        END DO

C...initialize output arrays

        CQOUT( I ) = 0.0

C...loop through the valid wavelength intervals

        DO J = MNWLPT, MXWLPT 

C...determine weighting fraction for the wavelength interval

          IF ( WLOUT1( I ) .LE. WLIN( J ) ) THEN
            
            IF ( WLOUT2( I ) .EQ. WLIN( J+1 ) ) THEN
              DWLIN = WLIN( J+1 ) - WLIN( J )
              CQA = CQIN( J )
              CQB = CQIN( J+1 )
            ELSE IF ( WLOUT2( I ) .LT. WLIN( J+1 ) ) THEN
              DWLIN = WLOUT2( I ) - WLIN( J )
              CQA = CQIN( J )
              CQB = ( CQIN( J+1 ) - CQIN( J ) )
     &            / ( WLIN( J+1 ) - WLIN( J ) )
     &            * ( WLOUT2( I ) - WLIN( J ) ) + CQIN( J )
            ELSE IF ( WLOUT2( I ) .GT. WLIN( J+1 ) ) THEN
              DWLIN = WLIN( J+1 ) - WLIN( J )
              CQA = CQIN( J )
              CQB = CQIN( J+1 )
            END IF
            
          ELSE IF ( WLOUT1( I ) .GT. WLIN( J ) ) THEN

            IF ( WLOUT2( I ) .EQ. WLIN( J+1 ) ) THEN
              DWLIN = WLIN( J+1 ) - WLOUT1( I )
              CQA = ( CQIN( J+1 ) - CQIN( J ) )
     &            / ( WLIN( J+1 ) - WLIN( J ) )
     &            * ( WLOUT1( I ) - WLIN( J ) ) + CQIN( J )
              CQB = CQIN( J+1 )
            ELSE IF ( WLOUT2( I ) .LT. WLIN( J+1 ) ) THEN
              DWLIN = WLOUT2( I ) - WLOUT1( I )
              CQA = ( CQIN( J+1 ) - CQIN( J ) )
     &            / ( WLIN( J+1 ) - WLIN( J ) )
     &            * ( WLOUT1( I ) - WLIN( J ) ) + CQIN( J )
              CQB = ( CQIN( J+1 ) - CQIN( J ) )
     &            / ( WLIN( J+1 ) - WLIN( J ) )
     &            * ( WLOUT2( I ) - WLIN( J ) ) + CQIN( J )
            ELSE IF ( WLOUT2( I ) .GT. WLIN( J+1 ) ) THEN
              DWLIN = WLIN( J+1 ) - WLOUT1( I )
              CQA = ( CQIN( J+1 ) - CQIN( J ) )
     &            / ( WLIN( J+1 ) - WLIN( J ) )
     &            * ( WLOUT1( I ) - WLIN( J ) ) + CQIN( J )
              CQB = CQIN( J+1 )
            END IF
              
          END IF

C...set wavelength interval fraction

          DWLIN = AMAX1 ( DWLIN / ( WLOUT2( I ) - WLOUT1( I ) ), 0.0 )

C...add weighted quantity to output arrays

          CQOUT( I ) = CQOUT( I ) + 0.5 * ( CQA + CQB ) * DWLIN

        END DO

      END DO
     
      RETURN
      END
