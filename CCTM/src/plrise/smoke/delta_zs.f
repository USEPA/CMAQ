
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
C $Header: /project/yoj/arc/CCTM/src/plrise/smoke/delta_zs.f,v 1.2 2011/10/21 16:11:30 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE DELTA_ZS( EMLAYS, MY_NSRC, SRC_MAP, STKHT, ZF, ZSTK, DDZF )

C-----------------------------------------------------------------------
C Function: Compute ZSTK and DDZF

C Revision History:
C     20 Nov 2007 J.Young: initial implementation
C     Aug 2015 D. Wong:    Used assumed shape array declaration

C-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER, INTENT( IN )  :: EMLAYS
      INTEGER, INTENT( IN )  :: MY_NSRC
      INTEGER, INTENT( IN )  :: SRC_MAP( : )
      REAL,    INTENT( IN )  :: STKHT( : )
      REAL,    INTENT( IN )  :: ZF  ( :,: )
      REAL,    INTENT( OUT ) :: ZSTK( :,: )
      REAL,    INTENT( OUT ) :: DDZF( :,: )

      REAL    ZF0, ZF1
      INTEGER L, S, SRC

      DO S = 1, MY_NSRC
         SRC = SRC_MAP( S )

         ZF0 = ZF( 1,S )
         ZSTK( 1,S ) = ZF0 - STKHT( SRC )
         DDZF( 1,S ) = 1.0 / ZF0

         DO L = 2, EMLAYS
            ZF1 = ZF( L,S )
            ZSTK( L,S ) = ZF1 - STKHT( SRC )
            DDZF( L,S ) = 1.0 / ( ZF1 - ZF0 )
            ZF0 = ZF1
         END DO

      END DO

      RETURN

      END SUBROUTINE DELTA_ZS

