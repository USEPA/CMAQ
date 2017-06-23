
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
C $Header: /project/work/rep/PARIO/src/boundary.f,v 1.4 2006/06/05 17:47:51 yoj Exp $

      SUBROUTINE BOUNDARY ( GNBNDY, NBNDY, NLAYS, BTHICK, NEIGHBOR,
     &                      MY_COL1, MY_ROW1, MY_NCOL, MY_NROW,
     &                      GNCOLS, GNROWS, GARRAY, LARRAY )
C ....................................................................
 
C  PURPOSE:   Construct the local processor boundary array from the
C             global boundary array.
 
C  REVISION HISTORY: 
C       Original version 09/01/1998 by Al Bourgeois.
 
C  ARGUMENT LIST DESCRIPTION:
C  IN:
C      INTEGER   GNBNDY      ! Size of global grid boundary
C      INTEGER   NBNDY       ! Dimension for local PE boundary array.
C      INTEGER   NLAYS       ! Layer dimension of variable.
C      INTEGER   BTHICK      ! Cell thickness of grid boundary
C      INTEGER   NEIGHBOR( 8 ) ! Neighboring PEs, first north then clockwise.
C      INTEGER   MY_COL1     ! Starting global column index of local processor.
C      INTEGER   MY_ROW1     ! Starting global row index of local processor.
C      INTEGER   MY_NCOL     ! Local number of grid columns actually used.
C      INTEGER   MY_NROW     ! Local number of grid rows actually used.
C      INTEGER   GNCOLS      ! Number of columns in global grid.
C      INTEGER   GNROWS      ! Number of rows in global grid.
C      REAL      GARRAY( GNBNDY,NLAYS ) ! Boundary values on global grid.
  
C  OUT:
C      REAL      LARRAY( NBNDY,NLAYS )  ! Boundary values on local grid.
 
C  LOCAL VARIABLE DESCRIPTION:  see below
 
C  CALLS: NAMEVAL, TRIMLEN, PUTENV, WRITE
 
C .......................................................................

        IMPLICIT  NONE

C ARGUMENTS:

       INTEGER   GNBNDY      ! Size of global grid boundary
       INTEGER   NBNDY       ! Dimension for local PE boundary array.
       INTEGER   NLAYS       ! Layer dimension of variable.
       INTEGER   BTHICK      ! Cell thickness of grid boundary
       INTEGER   NEIGHBOR( 8 ) ! Neighboring PEs, first north then clockwise.
       INTEGER   MY_COL1     ! Starting global column index of local processor.
       INTEGER   MY_ROW1     ! Starting global row index of local processor.
       INTEGER   MY_NCOL     ! Local number of grid columns actually used.
       INTEGER   MY_NROW     ! Local number of grid rows actually used.
       INTEGER   GNCOLS      ! Number of columns in global grid.
       INTEGER   GNROWS      ! Number of rows in global grid.
       REAL      GARRAY( GNBNDY,NLAYS ) ! Boundary values on global grid.
       REAL      LARRAY( NBNDY,NLAYS )  ! Boundary values on local grid.

C LOCAL VARIABLES:

       INTEGER   IL          ! Loop counter over grid layers.
       INTEGER   IB          ! Loop counter over boundary cells.
       INTEGER   SOUTH_PE    ! ID of neighbor processor to the south.
       INTEGER   EAST_PE     ! ID of neighbor processor to the east.
       INTEGER   NORTH_PE    ! ID of neighbor processor to the north.
       INTEGER   WEST_PE     ! ID of neighbor processor to the west.
       INTEGER   LNS_SIZE    ! Number cells in local north or south boundary
       INTEGER   LEW_SIZE    ! Number cells in local east or west boundary
       INTEGER   GNS_SIZE    ! Number cells in global north or south boundary
       INTEGER   GEW_SIZE    ! Number cells in global east or west boundary
       INTEGER   LS_START    ! Starting index into LARRAY for south boundary 
       INTEGER   LS_END      ! Ending index into LARRAY for south boundary
       INTEGER   LE_START    ! Starting index into LARRAY for east boundary
       INTEGER   LE_END      ! Ending index into LARRAY for east boundary
       INTEGER   LN_START    ! Starting index into LARRAY for north boundary
       INTEGER   LN_END      ! Ending index into LARRAY for north boundary
       INTEGER   LW_START    ! Starting index into LARRAY for west boundary
       INTEGER   LW_END      ! Ending index into LARRAY for west boundary
       INTEGER   GS_SKIP     ! Reference index into GARRAY for south boundary
       INTEGER   GE_SKIP     ! Reference index into GARRAY for east boundary
       INTEGER   GN_SKIP     ! Reference index into GARRAY for north boundary
       INTEGER   GW_SKIP     ! Reference index into GARRAY for west boundary

C .......................................................................

C Define useful values. Names beginning with L refer to
C the local grid, those with G refer to the global grid.

      SOUTH_PE = NEIGHBOR( 5 )
      EAST_PE  = NEIGHBOR( 3 )
      NORTH_PE = NEIGHBOR( 1 )
      WEST_PE  = NEIGHBOR( 7 )

      LNS_SIZE = BTHICK * ( MY_NCOL + BTHICK )
      LEW_SIZE = BTHICK * ( MY_NROW + BTHICK )

      LS_START = 1
      LS_END   = LNS_SIZE
      LE_START = LS_END + 1
      LE_END   = LE_START + LEW_SIZE - 1
      LN_START = LE_END + 1
      LN_END   = LN_START + LNS_SIZE - 1
      LW_START = LN_END + 1
      LW_END   = LW_START + LEW_SIZE - 1

      GNS_SIZE = BTHICK * ( GNCOLS + BTHICK )
      GEW_SIZE = BTHICK * ( GNROWS + BTHICK )

      GS_SKIP = BTHICK*( MY_COL1 - 1 ) - LS_START + 1
      GE_SKIP = GNS_SIZE + BTHICK*( MY_ROW1 - 1 ) - LE_START + 1
      GN_SKIP = GNS_SIZE + GEW_SIZE + BTHICK*( MY_COL1 - 1 ) - LN_START + 1
      GW_SKIP = 2*GNS_SIZE + GEW_SIZE + BTHICK*( MY_ROW1 - 1 ) - LW_START + 1

C Construct SOUTH boundary

!     LARRAY = 0.0   ! array assignment

      IF ( SOUTH_PE .EQ. -1 ) THEN
         DO IL = 1, NLAYS
            DO IB = LS_START, LS_END
               LARRAY( IB,IL ) = GARRAY( GS_SKIP+IB,IL )
            END DO
         END DO
      END IF

C Construct EAST boundary

      IF ( EAST_PE .EQ. -1 ) THEN
         DO IL = 1, NLAYS
            DO IB = LE_START, LE_END
               LARRAY( IB,IL ) = GARRAY( GE_SKIP+IB,IL )
            END DO
         END DO
      END IF

C Construct NORTH boundary

      IF ( NORTH_PE .EQ. -1 ) THEN
         DO IL = 1, NLAYS
            DO IB = LN_START, LN_END
               LARRAY( IB,IL ) = GARRAY( GN_SKIP+IB,IL )
            END DO
         END DO
      END IF

C Construct WEST boundary

      IF ( WEST_PE .EQ. -1 ) THEN
         DO IL = 1, NLAYS
            DO IB = LW_START, LW_END
               LARRAY( IB,IL ) = GARRAY( GW_SKIP+IB,IL )
            END DO
         END DO
      END IF

      RETURN

      END

