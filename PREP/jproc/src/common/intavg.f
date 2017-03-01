
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
C $Header: /project/yoj/arc/JPROC/src/driver/jproc_table/intavg.f,v 1.5 2011/10/29 01:03:53 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)intavg.F	1.1 /project/mod3/JPROC/src/driver/jproc_table/SCCS/s.intavg.F 23 May 1997 12:44:18

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE INTAVG ( WLIN, CQIN, NWLIN, TYPE,
     &                    WLOUT1, WLOUT2, CQOUT, NWLOUT )
     
C*********************************************************************
C
C  This program computes the integrated average data for the ET
C    extra terrestrial irradiance wavelength intervals
C
C  History:
C    S.Roselle  6/05/95  Program created
C    S.Roselle  7/25/96  Revised subroutine to compute interval
C                        average quantities for point, centered,
C                        beginning, and ending data
C
C*********************************************************************

      USE M3UTILIO

      IMPLICIT NONE      
      
      INCLUDE 'JVALPARMS.EXT'         ! jproc parameters

C...........ARGUMENTS and their descriptions

      CHARACTER(1) :: TYPE             ! spectra type

      INTEGER      NWLOUT              ! number of intervals ETin
      INTEGER      NWLIN               ! number of intervals CQin

      REAL         WLIN ( MXWLIN )     ! wl for CQin
      REAL         WLIN1( MXWLIN )     ! lower limit on wl int CQin
      REAL         WLIN2( MXWLIN )     ! upper limit on wl int CQin
      REAL         CQIN( MXWLIN )      ! quantity (CS or QY) as f(WLIN)
      REAL         WLOUT1( MXWL )      ! lower limit on wl int ETin
      REAL         WLOUT2( MXWL )      ! upper limit on wl int ETin
      REAL         CQOUT ( MXWL )      ! quantity (CS or QY) as f(WLOUT)

C...........LOCAL VARIABLES and their descriptions:
      
      CHARACTER(16) :: PNAME = 'INTAVG' ! program name
      CHARACTER(80) :: MSG   = '    '   ! message

      INTEGER      I                   ! index var
      INTEGER      J                   ! index var
      INTEGER      MXWLPT              ! pointer
      INTEGER      MNWLPT              ! pointer
     
      REAL         DWLIN               ! wl int for CQin
      REAL         CQA                 ! lower cq value
      REAL         CQB                 ! upper cq value
      
C*********************************************************************
C...begin body of subroutine INTAVG

C...process point data

      IF ( TYPE .EQ. 'P' ) THEN

C...transform the data to the same wavelength intervals as
C...  the irradiance data.

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
     &              / ( WLIN( J+1 ) - WLIN( J ) )
     &              * ( WLOUT2( I ) - WLIN( J ) ) + CQIN( J )
              ELSE IF ( WLOUT2( I ) .GT. WLIN( J+1 ) ) THEN
                DWLIN = WLIN( J+1 ) - WLIN( J )
                CQA = CQIN( J )
                CQB = CQIN( J+1 )
              END IF
            
            ELSE IF ( WLOUT1( I ) .GT. WLIN( J ) ) THEN

              IF ( WLOUT2( I ) .EQ. WLIN( J+1 ) ) THEN
                DWLIN = WLIN( J+1 ) - WLOUT1( I )
                CQA = ( CQIN( J+1 ) - CQIN( J ) )
     &              / ( WLIN( J+1 ) - WLIN( J ) )
     &              * ( WLOUT1( I ) - WLIN( J ) ) + CQIN( J )
                CQB = CQIN( J+1 )
              ELSE IF ( WLOUT2( I ) .LT. WLIN( J+1 ) ) THEN
                DWLIN = WLOUT2( I ) - WLOUT1( I )
                CQA = ( CQIN( J+1 ) - CQIN( J ) )
     &              / ( WLIN( J+1 ) - WLIN( J ) )
     &              * ( WLOUT1( I ) - WLIN( J ) ) + CQIN( J )
                CQB = ( CQIN( J+1 ) - CQIN( J ) )
     &              / ( WLIN( J+1 ) - WLIN( J ) )
     &              * ( WLOUT2( I ) - WLIN( J ) ) + CQIN( J )
              ELSE IF ( WLOUT2( I ) .GT. WLIN( J+1 ) ) THEN
                DWLIN = WLIN( J+1 ) - WLOUT1( I )
                CQA = ( CQIN( J+1 ) - CQIN( J ) )
     &              / ( WLIN( J+1 ) - WLIN( J ) )
     &              * ( WLOUT1( I ) - WLIN( J ) ) + CQIN( J )
                CQB = CQIN( J+1 )
              END IF
              
            END IF

C...set wavelength interval fraction

            DWLIN = AMAX1 ( DWLIN / ( WLOUT2( I ) - WLOUT1( I ) ), 0.0 )

C...add weighted quantity to output arrays

            CQOUT( I ) = CQOUT( I ) + 0.5 * ( CQA + CQB ) * DWLIN

          END DO

        END DO

C...process interval data

      ELSE

C...determine wl intervals for CENTERED WLBAND data

        IF ( TYPE .EQ. 'C' ) THEN

          WLIN1( 1 ) = 0.5 * (( 3.0 * WLIN( 1 ) ) -  WLIN( 2 ))
          WLIN2( 1 ) = 0.5 * (WLIN( 1 ) + WLIN( 2 ))

          DO J = 2, NWLIN-1
            WLIN1( J ) = 0.5 * (WLIN( J ) + WLIN ( J-1 ))
            WLIN2( J ) = 0.5 * (WLIN( J ) + WLIN ( J+1 ))
          END DO

C...determine wl intervals for BEGINNING WLBAND data

        ELSE IF ( TYPE .EQ. 'B' ) THEN

          DO J = 1, NWLIN-1
            WLIN1( J ) = WLIN( J )
            WLIN2( J ) = WLIN( J+1 )
          END DO

C...determine wl intervals for ENDING WLBAND data

        ELSE IF ( TYPE .EQ. 'E' ) THEN

          DO J = 2, NWLIN
            WLIN1( J-1 ) = WLIN( J-1 )
            WLIN2( J-1 ) = WLIN( J )
          END DO

C...stop program if wavelength data type not found

        ELSE

          MSG = 'Unrecognized spectra type in ' // TRIM( PNAME )
          CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT2 )

        END IF

C...loop through wavelength intervals for output arrays

        DO I = 1, NWLOUT

C...find lower limit on input array
      
          MNWLPT = 1        
          DO J = 1, NWLIN-1
            IF ( WLIN1( J ) .LE. WLOUT1( I ) ) MNWLPT = J
          END DO

C...find upper limit on input array

          MXWLPT = NWLIN-1
          DO J = NWLIN-1, 1, -1
            IF ( WLIN2( J ) .GE. WLOUT2( I ) ) MXWLPT = J
          END DO

C...initialize output arrays

          CQOUT( I ) = 0.0

C...loop through the valid wavelength intervals

          DO J = MNWLPT, MXWLPT 

C...determine weighting fraction for the wavelength interval

            IF ( WLOUT1( I ) .LE. WLIN1( J ) ) THEN
            
              IF ( WLOUT2( I ) .EQ. WLIN2( J ) ) THEN
                DWLIN = WLIN2( J ) - WLIN1( J )
              ELSE IF ( WLOUT2( I ) .LT. WLIN2( J ) ) THEN
                DWLIN = WLOUT2( I ) - WLIN1( J )
              ELSE IF ( WLOUT2( I ) .GT. WLIN2( J ) ) THEN
                DWLIN = WLIN2( J ) - WLIN1( J )
              END IF
            
            ELSE IF ( WLOUT1( I ) .GT. WLIN1( J ) ) THEN

              IF ( WLOUT2( I ) .EQ. WLIN2( J ) ) THEN
                DWLIN = WLIN2( J ) - WLOUT1( I )
              ELSE IF ( WLOUT2( I ) .LT. WLIN2( J ) ) THEN
                DWLIN = WLOUT2( I ) - WLOUT1( I )
              ELSE IF ( WLOUT2( I ) .GT. WLIN2( J ) ) THEN
                DWLIN = WLIN2( J ) - WLOUT1( I )
              END IF
              
            END IF

C...set wavelength interval fraction

            DWLIN = AMAX1 ( DWLIN / ( WLOUT2( I ) - WLOUT1( I ) ), 0.0 )

C...add weighted quantity to output arrays

            CQOUT( I ) = CQOUT( I ) + CQIN( J ) * DWLIN

          END DO

        END DO

      END IF
     
      RETURN
      END
