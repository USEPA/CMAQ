      MODULE GRID_DATA

C*************************************************************************
C
C  FUNCTION:  To define a grid class
C             
C  PRECONDITIONS: None
C 
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Prototype created by Jerry Gipson, July, 1999
C                   
C*************************************************************************

      TYPE GRID

         INTEGER  :: NCOLS
         INTEGER  :: NROWS
         INTEGER  :: NLAYS
         INTEGER  :: GDTYP

         REAL*8  ::  P_ALP
         REAL*8  ::  P_BET
         REAL*8  ::  P_GAM
         REAL*8  ::  XCENT
         REAL*8  ::  YCENT
         REAL*8  ::  XORIG
         REAL*8  ::  YORIG
         REAL*8  ::  XCELL
         REAL*8  ::  YCELL

      END TYPE GRID

      TYPE ( GRID ) M3GRID

      CONTAINS

         SUBROUTINE GET_M3GRID
C*************************************************************************
C
C  FUNCTION:  To set grid data
C             
C  PRECONDITIONS: None
C 
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Prototype created by Jerry Gipson, July, 1999
C                    JG corrected setting of M3GRID % NROWS 02/02/00
C                   
C*************************************************************************
         USE M3UTILIO
         USE M3FILES

         IMPLICIT NONE 

      
C..ARGUMENTS: None

C..PARAMETERS: None

C..EXTERNAL FUNCTIONS: None

C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
         CHARACTER*16    PNAME        ! Program Name

         INTEGER   N                  ! Loop index
         INTEGER   STATUS             ! Status code

C**********************************************************************
         DATA  PNAME       / 'GET_GRID'  /

         IF ( .NOT. DESC3( M3_FLNAME( 1 ) ) ) THEN
            CALL M3EXIT( PNAME, 0, 0,
     &                  'Could not get ' // M3_FLNAME( 1 ) //
     &                  ' file description', XSTAT1 )
         ENDIF

         M3GRID % NCOLS = NCOLS3D
         M3GRID % NROWS = NROWS3D
         M3GRID % NLAYS = NLAYS3D
         M3GRID % GDTYP = GDTYP3D

         M3GRID % P_ALP = P_ALP3D
         M3GRID % P_BET = P_BET3D
         M3GRID % P_GAM = P_GAM3D
         M3GRID % XCENT = XCENT3D
         M3GRID % YCENT = YCENT3D
         M3GRID % XORIG = XORIG3D
         M3GRID % YORIG = YORIG3D
         M3GRID % XCELL = XCELL3D
         M3GRID % YCELL = YCELL3D

         RETURN

         END SUBROUTINE GET_M3GRID

      END MODULE GRID_DATA
