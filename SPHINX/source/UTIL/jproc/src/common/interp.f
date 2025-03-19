
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
C $Header: /project/yoj/arc/JPROC/src/driver/jproc_table/interp.f,v 1.5 2011/10/29 01:03:53 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)interp.F	1.1 /project/mod3/JPROC/src/driver/jproc_table/SCCS/s.interp.F 23 May 1997 12:44:19

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE INTERP ( DLAT, IDATE, XT, XAIR, XO3, XDOBS, HO3, 
     &                    T, AIR, O3 )

C*********************************************************************
C
C     FUNCTION:  Interpolates data for T, AIR, and O3 to the selected
C     day of the year (from seasonal averages or monthly averages)
C     and to the selected latitude (data at 90N -> 90S by 10 deg incre
C     Note:  latitude dependent data is indexed 1 through 19:
C                 (1 = 90N, 10 = Equator, 19 = 90S).
C            seasonal data is indexed 1 through 4:
c                 1 = winter, 2 = spring, 3 = summer, 4 = fall
C            monthly data is indexed 1=Jan, 2=Feb,...,12=Dec.
c     Output of this subroutine is: Molecular # density, Ozone, and
C     temperature interpolated to date and latitude of interest:
C     AIR(I), T(I), O3.  Also, the O3 data can also be re-scaled to
C     a different value of total ozone (DOBSON).
C     
C     PRECONDITIONS REQUIRED:  
C     
C     REVISION  HISTORY:
C     5/12/95  s.roselle  modified old radm version for new coding std
C
C*********************************************************************

      IMPLICIT NONE

      INCLUDE 'JVALPARMS.EXT'    ! jproc parameters

C...........ARGUMENTS and their descriptions

      INTEGER      IDATE             ! date (yyyymmdd)
      
      REAL         DLAT              ! latitude interval
      REAL         T  ( MXLEV )      ! interpolated temp profile
      REAL         AIR( MXLEV )      ! interpolated air profile
      REAL         O3 ( MXLEV )      ! interpolated ozone profile
      REAL         XT  ( 12, 19, MXLEV ) ! season-lat-vert temp profile
      REAL         XAIR( 12, 19, MXLEV ) ! season-lat-vert air profile
      REAL         XO3 ( 12, 19, MXLEV ) ! season-lat-vert ozone profile
      REAL         XDOBS( 19, 12 )   ! lat-season ozone values
      REAL         HO3               ! ozone scale height

C...........LOCAL VARIABLES and their descriptions:

      INTEGER      IYEAR             ! year (yyyy)
      INTEGER      IDAY              ! day (dd)
      INTEGER      IMONTH            ! month (mm)
      INTEGER      ILEV              ! level index variable
      INTEGER      IMON              ! month index variable
      INTEGER      NS1               ! season number
      INTEGER      JDAYS             ! julian day
      INTEGER      NS2               ! next season number
      INTEGER      NM2               ! next month number
      INTEGER      NM1               ! month number
      INTEGER      L1                ! latitude
      INTEGER      L2                ! next latitude
      INTEGER      JDAY              ! julian date
      INTEGER      JDAYM             ! julian date
      
      INTEGER      IJMN( 12 )        ! julian start day for each month
      DATA         IJMN /   0,  31,  59,  90, 120, 151,
     &                    181, 212, 243, 273, 304, 334 /
      SAVE         IJMN
     
      INTEGER      MIDS( 5 )         ! julian day midpoints of seasons
      DATA         MIDS / 35, 126, 218, 310, 400 /
      SAVE         MIDS
 
      INTEGER      MIDM( 13 )        ! julian day midpoints by month
      DATA         MIDM /  15,  46,  74, 105, 135, 166, 196, 
     &                    227, 258, 288, 319, 349, 380 /
      SAVE         MIDM

      REAL         FS2               ! seasonal weighting factor 1
      REAL         FS1               ! seasonal weighting factor 2
      REAL         FM2               ! monthly weighting factor 1
      REAL         FM1               ! monthly weighting factor 2
      REAL         AL1               !
      REAL         FL1               ! latitude weighting factor 1
      REAL         FL2               ! latitude weighting factor 2
      REAL         A1                ! seasonally interpolated air 1
      REAL         A2                ! seasonally interpolated air 2
      REAL         T1                ! seasonally interpolated temp 1
      REAL         T2                ! seasonally interpolated temp 2
      REAL         O31               ! seasonally interpolated ozone 1
      REAL         O32               ! seasonally interpolated ozone 2
      REAL         D1                ! monthly interpolated dobson 1
      REAL         D2                ! monthly interpolated dobson 2
      REAL         DOBSON            ! lat & monthly interp. dobson
      REAL         DOBSREF           ! interp. O3 profile dobson unit

C*********************************************************************
C     begin body of subroutine INTERP2

C.......assumed julian date of seasonally averaged data, MIDS:
C.......    1 - winter -  35 - 4 feb
C.......    2 - spring - 126 - 6 may
C.......    3 - summer - 218 - 6 aug
C.......    4 - fall   - 310 - 6 nov
C.......    5 - new winter - 35+365 - 4 feb
C.......assumed julian date for monthly averaged data, MIDM:
C.......    15th day of each month

C.......parse date

      IYEAR = INT( IDATE / 10000 )
      IMONTH = INT( (IDATE - IYEAR * 10000) / 100 )
      IDAY   = IDATE - IYEAR * 10000 - IMONTH * 100
      
C.......compute Julian day, adjusting for leap years

      JDAY = IJMN( IMONTH ) + IDAY
      IF ( (MOD( IYEAR, 4 ) .EQ. 0) .AND. (IMONTH .GT. 2) )
     &      JDAY = JDAY + 1

C.......compute season interpolation parameters
C.......  calculate nearest previous season data set index

      NS1 = 4
      IF ( JDAY .GE.  35 ) NS1 = 1
      IF ( JDAY .GE. 126 ) NS1 = 2
      IF ( JDAY .GE. 218 ) NS1 = 3
      IF ( JDAY .GE. 310 ) NS1 = 4

C.......adjust for cyclic year

      JDAYS = JDAY
      IF ( JDAY .LT. 35 ) JDAYS = JDAY + 365
      NS2 = NS1 + 1
      FS2 = FLOAT( JDAYS - MIDS( NS1 ) ) /
     &      FLOAT( MIDS( NS2 ) - MIDS( NS1 ) )
      FS1 = 1.0 - FS2
      IF ( NS2 .EQ. 5 ) NS2 = 1

C.......compute monthly interpolation parameters

      DO IMON = 1, 13
        NM2 = IMON
        IF ( JDAY .LT. MIDM( IMON ) ) GO TO 250
      END DO

250   CONTINUE

C.......adjust for cyclic year

      IF ( NM2 .EQ. 1 ) NM2 = 13
      NM1 = NM2 - 1
      JDAYM = JDAY
      IF ( JDAYM .LT. 15 ) JDAYM = JDAY + 365
      FM2 = FLOAT( JDAYM - MIDM( NM1 ) ) / 
     &      FLOAT( MIDM( NM2 ) - MIDM( NM1 ) )
      FM1 = 1.0 - FM2
      IF ( NM2 .EQ. 13 ) NM2 = 1

C.......compute latitude interpolation parameters

      AL1 = 10.0 - DLAT / 10.0
      L1  = INT( AL1 )
      L2  = L1 + 1
      IF ( DLAT .EQ. -90.0 ) L2  = L1 - 1
      FL2 = AL1 - FLOAT( L1 )
      FL1 = 1.0 - FL2

C.......interpolate

      DO ILEV = 1, MXLEV
        A1  = FS1 * XAIR( NS1, L1, ILEV ) +
     &        FS2 * XAIR( NS2, L1, ILEV )
        A2  = FS1 * XAIR( NS1, L2, ILEV ) +
     &        FS2 * XAIR( NS2, L2, ILEV )
        T1  = FS1 * XT  ( NS1, L1, ILEV ) +
     &        FS2 * XT  ( NS2, L1, ILEV )
        T2  = FS1 * XT  ( NS1, L2, ILEV ) +
     &        FS2 * XT  ( NS2, L2, ILEV )
        O31 = FS1 * XO3 ( NS1, L1, ILEV ) +
     &        FS2 * XO3 ( NS2, L1, ILEV )
        O32 = FS1 * XO3 ( NS1, L2, ILEV ) +
     &        FS2 * XO3 ( NS2, L2, ILEV )
        AIR( ILEV ) = FL1 * A1   + FL2 * A2
        T  ( ILEV ) = FL1 * T1   + FL2 * T2
        O3 ( ILEV ) = FL1 * O31  + FL2 * O32
      END DO

C.......interpolate dobson values:

      D1 = FM1 * XDOBS( L1, NM1 ) + FM2 * XDOBS( L1, NM2 )
      D2 = FM1 * XDOBS( L2, NM1 ) + FM2 * XDOBS( L2, NM2 )
      DOBSON = FL1 * D1 + FL2 * D2

C.......adjustment of O3 profiles to monthly DOBSON data from WMO 1981

      DOBSREF = O3( MXLEV ) * 1.0E5 * HO3

      DO ILEV = 1, MXLEV
        DOBSREF = DOBSREF + O3( ILEV ) * 1.0E5
      END DO

      DOBSREF = DOBSREF / 2.687E16
      
      DO ILEV = 1, MXLEV
        O3( ILEV ) = O3( ILEV ) * DOBSON / DOBSREF
      END DO

      RETURN
      END
