
C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/plrise/smoke/delta_zs.f,v 1.1 2010/07/20 11:30:03 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE DELTA_ZS( EMLAYS, MY_NSRC, SRC_MAP, STKHT, ZF, ZSTK, DDZF )

C-----------------------------------------------------------------------
C Function: Compute ZSTK and DDZF

C Revision History:
C     20 Nov 2007 J.Young: initial implementation

C-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER, INTENT( IN )  :: EMLAYS
      INTEGER, INTENT( IN )  :: MY_NSRC
      INTEGER, INTENT( IN )  :: SRC_MAP( : )
      REAL,    INTENT( IN )  :: STKHT( : )
      REAL,    INTENT( IN )  :: ZF  ( EMLAYS,MY_NSRC )
      REAL,    INTENT( OUT ) :: ZSTK( EMLAYS,MY_NSRC )
      REAL,    INTENT( OUT ) :: DDZF( EMLAYS,MY_NSRC )

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

