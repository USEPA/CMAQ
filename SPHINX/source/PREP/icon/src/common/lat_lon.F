
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
C $Header: /project/yoj/arc/ICON/src/common/lat_lon.F,v 1.2 2011/10/21 16:41:52 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

      SUBROUTINE LAT_LON ( COL, ROW, GDTYP, XORIG, YORIG, XCELL, YCELL,
     &                     XCENT, YCENT, P_ALP, P_BET, P_GAM, LAT, LON )
 
C***********************************************************************
 
C Function: Computes latitude and longitude of center of grid cells 
 
C Preconditions: None
  
C Key Subroutines/Functions Called: SETLAM
C                                   SETPOL
C                                   SETEQM
C                                   LAM2LL
C                                   POL2LL
C                                   EQM2LL
C                                   UTM2LL
 
C Revision History: Modified form of LAT_LON program originally created
C                      by C. J. Coats  -- Jerry Gipson, January 1998    
C    06 Jun 11 J.Young: Replaced I/O API include files with UTILIO_DEFN
C    28 Aug 18 R.Mathur/S.Roselle: Added polar stereographic projection
C                      grid type option 
C    06 Nov 18 S.Roselle: Replaced UTILIO_DEFN with M3UTILIO
C    21 Apr 21 C. Hogrefe: Added equatorial mercator projection
C                      grid type option 

C***********************************************************************

      USE M3UTILIO   ! IOAPI module

      IMPLICIT NONE     

C Arguments:
      INTEGER, INTENT( IN ) :: GDTYP       ! Grid type
      INTEGER, INTENT( IN ) :: COL         ! column
      INTEGER, INTENT( IN ) :: ROW         ! row

      REAL( 8 ), INTENT( IN ) :: P_ALP  ! 1st map projection parameter
      REAL( 8 ), INTENT( IN ) :: P_BET  ! 2nd map projection parameter
      REAL( 8 ), INTENT( IN ) :: P_GAM  ! 3rd map projection parameter

      REAL( 8 ), INTENT( IN ) :: XCELL  ! X-dimension of cell (m)
      REAL( 8 ), INTENT( IN ) :: XCENT  ! Longitude of coordinate system center
      REAL( 8 ), INTENT( IN ) :: XORIG  ! X-origin of grid
      REAL( 8 ), INTENT( IN ) :: YCELL  ! Y-dimension of cell (m)
      REAL( 8 ), INTENT( IN ) :: YCENT  ! Latitude of coordinate system center
      REAL( 8 ), INTENT( IN ) :: YORIG  ! Y-origin of grid

      REAL, INTENT( OUT ) :: LAT            ! Output latitude
      REAL, INTENT( OUT ) :: LON            ! Output longitude

C Parameters: None

C Local Variables:
      CHARACTER( 16 ) :: PNAME = 'LAT_LON'
      CHARACTER( 80 ) :: MSG = ' '

      INTEGER ZONE           ! UTM zone

      REAL X, X0             ! X-dimension origin
      REAL Y, Y0             ! Y-dimension origin

C***********************************************************************

      X0 = SNGL( XORIG - 0.5D+00 * XCELL )
      Y0 = SNGL( YORIG - 0.5D+00 * YCELL )

      IF ( GDTYP .EQ. LATGRD3 ) THEN      ! LAT_LON Coordinates

         LAT = Y0 + FLOAT( ROW ) * SNGL( YCELL )
         LON = X0 + FLOAT( COL ) * SNGL( XCELL )
  
      ELSE IF ( GDTYP .EQ. LAMGRD3 ) THEN  ! Lambert Coordinates

         IF ( .NOT. SETLAM( SNGL( P_ALP ),        !  first, initialize
     &                      SNGL( P_BET ),        !  for LAM2LL()
     &                      SNGL( P_GAM ),
     &                      SNGL( XCENT ), 
     &                      SNGL( YCENT ) ) ) THEN
            MSG = 'Lambert projection setup error for CTM CONC file'
            CALL M3EXIT ( PNAME, 0, 0, MSG, 2 )
         END IF
      
         X = X0 + FLOAT( COL ) * SNGL( XCELL )
         Y = Y0 + FLOAT( ROW ) * SNGL( YCELL )
         IF ( .NOT. LAM2LL( X, Y, LON, LAT ) ) THEN
            MSG = 'Lambert conversion error for CTM CONC file'
            CALL M3EXIT ( PNAME, 0, 0, MSG, 2 )
         END IF                          

      ELSE IF ( GDTYP .EQ. POLGRD3 ) THEN  ! Polar Stereographic Projection

         IF ( .NOT. SETPOL( SNGL( P_ALP ),        !  first, initialize
     &                      SNGL( P_BET ),        !  for POL2LL()
     &                      SNGL( P_GAM ),
     &                      SNGL( XCENT ),
     &                      SNGL( YCENT ) ) ) THEN
            MSG='Polar stereographic projection setup error for CTM CONC file'
            CALL M3EXIT ( PNAME, 0, 0, MSG, 2 )
         END IF

         X = X0 + FLOAT( COL ) * SNGL( XCELL )
         Y = Y0 + FLOAT( ROW ) * SNGL( YCELL )
         IF ( .NOT. POL2LL( X, Y, LON, LAT ) ) THEN
            MSG = 'Polar stereographic conversion error for CTM CONC file'
            CALL M3EXIT ( PNAME, 0, 0, MSG, 2 )
         END IF

      ELSE IF ( GDTYP .EQ. EQMGRD3 ) THEN  ! Equatorial Mercator Projection

         IF ( .NOT. SETEQM( SNGL( P_ALP ),        !  first, initialize
     &                      SNGL( P_BET ),        !  for EQM2LL()
     &                      SNGL( P_GAM ),
     &                      SNGL( XCENT ),
     &                      SNGL( YCENT ) ) ) THEN
            MSG='Equatorial mercator projection setup error for CTM CONC file'
            CALL M3EXIT ( PNAME, 0, 0, MSG, 2 )
         END IF

         X = X0 + FLOAT( COL ) * SNGL( XCELL )
         Y = Y0 + FLOAT( ROW ) * SNGL( YCELL )
         IF ( .NOT. EQM2LL( X, Y, LON, LAT ) ) THEN
            MSG = 'Equatorial mercator conversion error for CTM CONC file'
            CALL M3EXIT ( PNAME, 0, 0, MSG, 2 )
         END IF

      ELSE IF ( GDTYP .EQ. UTMGRD3 ) THEN   ! UTM Coordinates

         ZONE = NINT( P_ALP )
         X = X0 + FLOAT( COL ) * SNGL( XCELL )
         Y = Y0 + FLOAT( ROW ) * SNGL( YCELL )
         CALL UTM2LL ( X, Y, ZONE, LON, LAT )

      ELSE                                   !  Unsupported Coordinates

         WRITE( MSG, 94000) GDTYP
         CALL M3EXIT ( PNAME, 0, 0, MSG, 2 )

      END IF 

      RETURN         

C************************* FORMAT Statements ***************************

94000 FORMAT( 'LAT/LON calculations for GDTYP3D ',I1, ' not supported' )
    
      END
