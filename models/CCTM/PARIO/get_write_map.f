
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
C $Header: /project/work/rep/PARIO/src/get_write_map.f,v 1.6 2011/10/20 22:49:57 sjr Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

      SUBROUTINE GET_WRITE_MAP( NP, NPC, NPR, NCOLS3D, NROWS3D, NLAYS3D )
C.....................................................................
 
C  PURPOSE:  Determine the processor-to-grid map for the grid
C            to be written.
 
C  REVISION HISTORY: 
C       Original version  1/1999 by Al Bourgeois, to allow pwrite3 to
C              write output on a subgrid.
C       Modified 08/06/1999 by Al Bourgeois to make this a subroutine
C              instead of a function.
C       Modified 10/08/01 by David Wong
C         -- added a missing variable IERR in the SUBDMAP calling arguments
C       Modified 12/31/02 by David Wong
C         -- extended to handle dot file
C       6 May 06 J.Young
C      24 Aug 11 David Wong: extended the implementation to handle window
C                            MET_CRO_3D file
 
C  ARGUMENT LIST DESCRIPTION:
C  E1 in PIOGRID.EXT
C  M2 in PIOMAPS_MODULE
C  IN:
C     INTEGER  NP                    ! Number of processors
C     INTEGER  NPC                   ! Number of processors across grid cols
C     INTEGER  NPR                   ! Number of processors across grid rows
C     INTEGER  GNCOLS            E1  ! Column dimension of global domain
C     INTEGER  GNROWS            E1  ! Row dimension of global domain
C     INTEGER  NCOLS3D               ! Column dimension of file variables
C     INTEGER  NROWS3D               ! Row dimension of file variables
C     INTEGER  NLAYS3D               ! Layer dimension of file variable
C     INTEGER  NCOLS_PE(NP)      M2  ! Number columns in each processor
C     INTEGER  NROWS_PE(NP)      M2  ! Number rows in each processor
C     INTEGER  COLSX_PE(2,NP)    M2  ! Column range for each PE
C     INTEGER  ROWSX_PE(2,NP)    M2  ! Row range for each PE

C  OUT:
C     INTEGER  WR_NCOLS_PE(NP)   M2  ! No. cols of each PE subgrid to write
C     INTEGER  WR_NROWS_PE(NP)   M2  ! No. rows of each PE subgrid to write
C     INTEGER  WR_COLSX_PE(2,NP) M2  ! Col range of each PE subgrid to write
C     INTEGER  WR_ROWSX_PE(2,NP) M2  ! Row range of each PE subgrid to write
 
C  CALLS: SUBDMAP
C........................................................................

      USE PIOMAPS_MODULE

      IMPLICIT  NONE

      INCLUDE 'PIOGRID.EXT'
 
C Arguments:

      INTEGER  NP                    ! Number of processors 
      INTEGER  NPR                   ! Number of processors across grid rows
      INTEGER  NPC                   ! Number of processors across grid cols
      INTEGER  NCOLS3D               ! Column dimension of file variables
      INTEGER  NROWS3D               ! Row dimension of file variables
      INTEGER  NLAYS3D               ! Layer dimension of file variable
 
C Local Variables: 

      INTEGER      I             ! Loop index
      INTEGER      IDUMMY        ! Dummy argument to SUBDMAP, not used
      INTEGER      IERR          ! Return Error code

C........................................................................

C Determine the processor-to-subdomain mapping for the grid to
C be written. If the file variables to be written are defined on
C the entire (global) domain, load the previously defined
C decomposition map. Otherwise, get the new mapping on the subgrid.

      IF (( NCOLS3D .EQ. GNCOLS) .AND. ( NROWS3D .EQ. GNROWS )) THEN

C Set the full-grid processor-to-subdomain mapping

         DO I = 1, NP
            WR_NCOLS_PE( I )   = NCOLS_PE( I )
            WR_NROWS_PE( I )   = NROWS_PE( I )
            WR_COLSX_PE( 1,I ) = COLSX_PE( 1,I )
            WR_COLSX_PE( 2,I ) = COLSX_PE( 2,I )
            WR_ROWSX_PE( 2,I ) = ROWSX_PE( 2,I )
            WR_ROWSX_PE( 1,I ) = ROWSX_PE( 1,I )
         END DO

      ELSE IF (( NCOLS3D .EQ. GNCOLS+1) .AND. ( NROWS3D .EQ. GNROWS+1 )) THEN

C Set the dot full-grid processor-to-subdomain mapping

         DO I = 1, NP
            WR_NCOLS_PE( I )   = NCOLS_PE( I )
            WR_NROWS_PE( I )   = NROWS_PE( I )
            WR_COLSX_PE( 1,I ) = COLSX_PE( 1,I )
            WR_COLSX_PE( 2,I ) = COLSX_PE( 2,I )
            WR_ROWSX_PE( 2,I ) = ROWSX_PE( 2,I )
            WR_ROWSX_PE( 1,I ) = ROWSX_PE( 1,I )
         END DO

         DO I = NPC, NP, NPC
            WR_NCOLS_PE( I )   = NCOLS_PE( I ) + 1
            WR_COLSX_PE( 2,I ) = COLSX_PE( 2,I ) + 1
         END DO

         DO I = NP, NP-NPC+1, -1
            WR_NROWS_PE( I )   = NROWS_PE( I ) + 1
            WR_ROWSX_PE( 2,I ) = ROWSX_PE( 2,I ) + 1
         END DO

      ELSE IF (( NCOLS3D .EQ. GNCOLS+2) .AND. ( NROWS3D .EQ. GNROWS+2 )) THEN

C Set the extended cross full-grid processor-to-subdomain mapping

         DO I = 1, NP
            WR_NCOLS_PE( I )   = NCOLS_PE( I )
            WR_NROWS_PE( I )   = NROWS_PE( I )
            WR_COLSX_PE( 1,I ) = COLSX_PE( 1,I ) + 1
            WR_COLSX_PE( 2,I ) = COLSX_PE( 2,I ) + 1
            WR_ROWSX_PE( 2,I ) = ROWSX_PE( 2,I ) + 1
            WR_ROWSX_PE( 1,I ) = ROWSX_PE( 1,I ) + 1
         END DO

         DO I = 1, NPC    ! south
            WR_NROWS_PE( I )   = NROWS_PE( I ) + 1
            WR_ROWSX_PE( 1,I ) = WR_ROWSX_PE( 1,I ) - 1
         END DO

         DO I = 1, NP, NPC    ! west
            WR_NCOLS_PE( I )   = NCOLS_PE( I ) + 1
            WR_COLSX_PE( 1,I ) = WR_COLSX_PE( 1,I ) - 1
         END DO

         DO I = NPC, NP, NPC  ! east
            WR_NCOLS_PE( I )   = NCOLS_PE( I ) + 1
            WR_COLSX_PE( 2,I ) = WR_COLSX_PE( 2,I ) + 1
         END DO

         DO I = NP, NP-NPC+1, -1  ! north
            WR_NROWS_PE( I )   = NROWS_PE( I ) + 1
            WR_ROWSX_PE( 2,I ) = WR_ROWSX_PE( 2,I ) + 1
         END DO

      ELSE

C Get the subgrid processor-to_subdomain mapping

         CALL SUBDMAP( NP, NCOLS3D, NROWS3D, NLAYS3D, NPC, NPR,
     &                 NCOLS_PE, NROWS_PE, COLSX_PE, ROWSX_PE,
     &                 IDUMMY, IERR )

      END IF
     
      RETURN
      END
