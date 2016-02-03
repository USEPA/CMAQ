
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
C $Header: /project/work/rep/PARIO/src/growbuf.f,v 1.8 2011/03/30 18:13:00 sjr Exp $

      LOGICAL FUNCTION GROWBUF ( PTR_ARRAY, END ) 
C ....................................................................
 
C  PURPOSE:    Grows memory for dynamic memory array
 
C  RETURN VALUE:  The function fails if an error is detected in this
C       subroutine
 
C  REVISION HISTORY: 
C       Original version 07/20/1998 by Al Bourgeois. Moved from PINTERP3,
C          modified for better error handling across processors.
C       Modified 12/07/1998 by Al Bourgeois to add EXTERNAL declarations.
C       Modified 05/15/1999 by Al Bourgeois to change variable names for
C          general use.
C       Modified 06/16/1999 by Al Bourgeois to remove interprocessor
C          synchronization. This removes the guarantee that all processors
C          return the same error code, and a "hang" state can occur if
C          PM3EXIT is not called on the condition that this function fails.
C       Modified 02/06/2003 by David Wong
C          -- use f90 syntax to allocate memory and grow memory so it
C             no longer uses DYNMEM library
C       Modified 11/03/2004 by David Wong
C          -- fixed a bug, which only manifested in Sun system, in 
C             allocating new memory space
C       Modified 02/23/2011 by Shawn Roselle
C          -- Replaced I/O API include files with M3UTILIO

    
C  ARGUMENT LIST DESCRIPTION:
C  IN:
C     INTEGER  PTR_ARRAY      ! Address of dynamic array
C     INTEGER  END            ! Offset for end of array
 
C  OUT:
C     INTEGER  PTR_ARRAY      ! Address of grown array
 
C  LOCAL VARIABLE DESCRIPTION:  see below
 
C  CALLS:  M3WARN
 
C .......................................................................

      USE PINTERPB_MODULE
      USE M3UTILIO              ! i/o api

      IMPLICIT  NONE

C ARGUMENTS:
 
      TYPE( MEM_TYPE )      :: PTR_ARRAY  ! Address of dynamic array
      INTEGER, INTENT( IN ) :: END        ! Offset for end of array

C LOCAL VARIABLES: 

      INTEGER         :: IERROR        ! Error code from allocation subroutine
      CHARACTER( 80 ) :: MSG           ! Message issued from PM3WARN routine

!     REAL, ALLOCATABLE, TARGET :: LARRAY1( : ), LARRAY2( : )
      INTEGER :: DATA_SIZE, I

C .......................................................................

      GROWBUF = .TRUE.

      DATA_SIZE = ( END - BUFFERHD_SIZE ) / 2

!     ALLOCATE( LARRAY1( DATA_SIZE ), STAT=IERROR )
      ALLOCATE( PTR_ARRAY%MEM( 0 )%DATA_PTR( DATA_SIZE ), STAT=IERROR )
      IF ( IERROR .NE. 0 ) THEN
         MSG = 'Error growing array 1.'
         CALL M3WARN( 'GROWBUF', 0, 0, MSG )
         GROWBUF = .FALSE.; RETURN
      END IF

!     ALLOCATE( LARRAY2( DATA_SIZE ), STAT=IERROR )
      ALLOCATE( PTR_ARRAY%MEM( 1 )%DATA_PTR( DATA_SIZE ), STAT=IERROR )
      IF ( IERROR .NE. 0 ) THEN
         MSG = 'Error growing array 2.'
         CALL M3WARN( 'GROWBUF', 0, 0, MSG )
         GROWBUF = .FALSE.; RETURN
      END IF

      PTR_ARRAY%SIZE = DATA_SIZE * 2

!     PTR_ARRAY%MEM( 0 )%DATA_PTR => LARRAY1
!     PTR_ARRAY%MEM( 1 )%DATA_PTR => LARRAY2
      BUFFERHD_SIZE = BUFFERHD_SIZE + DATA_SIZE * 2

      RETURN
      END
