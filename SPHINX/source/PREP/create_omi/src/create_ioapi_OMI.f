
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
C $Header: /project/work/rep/arc/CCTM/src/phot/phot_inline/opphot.F,v 1.3 2011/10/21 16:11:28 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE CREATE_IOAPI_OMI ( FILE_NAME, JDATE, NLAT, NLON )

C-----------------------------------------------------------------------
C
C  FUNCTION:  ioapi for OMI ozone column observation
C
C  PRECONDITIONS REQUIRED:
C     None
C
C  REVISION  HISTORY:
C       Date   Who          What
C     -------- ----------   -----------------------------------------
C-----------------------------------------------------------------------

      USE M3UTILIO

      IMPLICIT NONE


C...Arguments

      CHARACTER( 16 ), INTENT( IN ) :: FILE_NAME  ! name of file 
      INTEGER,         INTENT( IN ) :: JDATE      ! Start date of file, YYYYDDD
      INTEGER,         INTENT( IN ) :: NLAT       ! # of latitude points
      INTEGER,         INTENT( IN ) :: NLON       ! # of Longitude points

C...Local variables

      CHARACTER( 32 ), SAVE :: PNAME = 'CREATE_IOAPI_OMI'
      CHARACTER( 20 )       :: COMMAND
      CHARACTER( 96 )       :: XMSG = ' '
      INTEGER               :: TSTEP  ! output time step

      INTEGER               :: N, L, JWL, INCR   ! loop variables
      INTEGER               :: SYSTEM            ! exteral function for line commands
      LOGICAL               :: EXISTS             
C-----------------------------------------------------------------------

C...Set output file characteristics based on COORD.EXT and open
C...  the photolysis diagnostic file

         FTYPE3D = GRDDED3
         SDATE3D = JDATE
         STIME3D = 0
         TSTEP3D = 240000

         NCOLS3D = NLON
         NROWS3D = NLAT
         NLAYS3D = 1
         NTHIK3D = 1
         GDTYP3D = LATGRD3 
         P_ALP3D = 0
         P_BET3D = 0
         P_GAM3D = 0
         XORIG3D = -180.0D0
         YORIG3D =  -90.0D0
         YCELL3D = REAL( 180.0 / REAL( NLAT, 4 ),8 )
         XCELL3D = REAL( 360.0 / REAL( NLON, 4 ),8 )
         VGTYP3D = 7
         VGTOP3D = 5000

         VGLVS3D( 1 ) = 1.0
         VGLVS3D( 2 ) = 0.9975
      

         GDNAM3D = 'OMI_GLOBE' 

C...CSA Variables, Units and Descriptions for FILE_NAME
         N = 1
         VNAME3D( N ) = 'OZONE_COLUMN'
         UNITS3D( N ) = 'DU'
         VDESC3D( N ) = 'Total Ozone Column Density'
         VTYPE3D( N ) = M3REAL

         N = N + 1
         VNAME3D( N ) = 'CLOUD_FRACT'
         UNITS3D( N ) = 'None'
         VDESC3D( N ) = 'Radiative Cloud Fraction'
         VTYPE3D( N ) = M3REAL

         N = N + 1
         VNAME3D( N ) = 'O3_MISSING'
         UNITS3D( N ) = 'None'
         VDESC3D( N ) = 'Ozone Column Density Not Avialable'
         VTYPE3D( N ) = M3REAL

         N = N + 1
         VNAME3D( N ) = 'LATITUDE'
         UNITS3D( N ) = 'DEG'
         VDESC3D( N ) = 'Latitude Coordinate'
         VTYPE3D( N ) = M3REAL

         NVARS3D = N
         FDESC3D( 1 ) = 'OMI Satellite Obseravations'
         DO L = 2, MXDESC3
            FDESC3D( L ) = ' '
         END DO
! Determine if file exists and delete if needed
         INQUIRE( FILE = FILE_NAME, EXIST = EXISTS )
         IF( EXISTS )THEN
             COMMAND = '\rm ' // TRIM( FILE_NAME )
             XMSG    = 'WARNING: ' // Trim( FILE_NAME ) 
     &              // ' exists and deleting '
             WRITE( 6, * )
             N = SYSTEM( COMMAND )
             IF( N .EQ. -1 )THEN
                XMSG = 'Cannot delete '// FILE_NAME // ' file'
                CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
             END IF 
         END IF
! create file
         IF ( .NOT. OPEN3( FILE_NAME, FSCREA3, PNAME ) ) THEN
            XMSG = 'Could not create '// FILE_NAME // ' file'
            CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
         END IF


      END SUBROUTINE CREATE_IOAPI_OMI
